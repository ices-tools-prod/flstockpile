## flstock_duckdb.R
##
## Read versioned FLStock records with DuckDB, from either the SQLite
## database (flstock_db_schema.sql) or a Parquet export of it, and rebuild
## them as FLStock objects in an R environment.
## Requires: duckdb, DBI, FLCore, jsonlite

## Install any missing packages (FLCore comes from the FLR r-universe)
.flstock_pkgs <- c("DBI", "duckdb", "FLCore", "jsonlite")
.missing <- .flstock_pkgs[!vapply(.flstock_pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(.missing)) {
  install.packages(.missing,
                   repos = c("https://flr.r-universe.dev", "https://cloud.r-project.org"))
}

library(DBI)
library(duckdb)
library(FLCore)
library(jsonlite)

if (!exists("json_to_flstock")) source("json_to_flstock.R")

## Raw URL of the database in the GitHub repo
flstock_github_url <- "https://raw.githubusercontent.com/neilcampbelll/FLstockpile/main/flstocks.sqlite"

#' Open a DuckDB connection exposing a `flstock_versions` view
#'
#' @param path a .sqlite or .parquet file, local or an http(s) URL
.flstock_duckdb_con <- function(path) {
  is_parquet <- grepl("\\.parquet$", path, ignore.case = TRUE)
  
  ## Download a remote file to a temporary local copy first
  if (grepl("^https?://", path)) {
    tmp <- tempfile(fileext = if (is_parquet) ".parquet" else ".sqlite")
    download.file(path, tmp, mode = "wb", quiet = TRUE)
    path <- tmp
  }
  
  con <- dbConnect(duckdb())
  if (is_parquet) {
    dbExecute(con, sprintf(
      "CREATE VIEW flstock_versions AS SELECT * FROM read_parquet('%s')", path))
  } else {
    dbExecute(con, "INSTALL sqlite")
    dbExecute(con, "LOAD sqlite")
    dbExecute(con, sprintf("ATTACH '%s' AS src (TYPE sqlite, READ_ONLY)", path))
    dbExecute(con, "CREATE VIEW flstock_versions AS SELECT * FROM src.flstock_versions")
  }
  con
}

#' Export the SQLite flstock_versions table to a Parquet file
#'
#' @param db_path path to the SQLite database
#' @param parquet_path output .parquet path
export_flstock_parquet <- function(db_path, parquet_path) {
  con <- .flstock_duckdb_con(db_path)
  on.exit(dbDisconnect(con, shutdown = TRUE))
  dbExecute(con, sprintf(
    "COPY flstock_versions TO '%s' (FORMAT parquet)", parquet_path))
  invisible(parquet_path)
}

#' Load FLStock objects into an environment
#'
#' Each stock is assigned under its stock_id (e.g. `ple.27.4a`). Without
#' `version`, the most recent version (by created_at) of each stock is loaded.
#'
#' @param path a .sqlite or .parquet file
#' @param stock_id optional character vector of stock_ids to load (default: all)
#' @param version optional version to load instead of the latest
#' @param envir environment to assign the stocks into
#' @return an FLStocks object of what was loaded, invisibly
load_flstocks <- function(path, stock_id = NULL, version = NULL, envir = .GlobalEnv) {
  con <- .flstock_duckdb_con(path)
  on.exit(dbDisconnect(con, shutdown = TRUE))
  
  where  <- character(0)
  params <- list()
  if (!is.null(stock_id)) {
    where  <- c(where, sprintf("stock_id IN (%s)",
                               paste(rep("?", length(stock_id)), collapse = ", ")))
    params <- c(params, as.list(stock_id))
  }
  if (!is.null(version)) {
    where  <- c(where, "version = ?")
    params <- c(params, list(version))
  }
  
  sql <- paste(
    "SELECT stock_id, version, json FROM flstock_versions",
    if (length(where)) paste("WHERE", paste(where, collapse = " AND ")) else "",
    if (is.null(version))
      "QUALIFY row_number() OVER (PARTITION BY stock_id ORDER BY created_at DESC) = 1"
    else "",
    "ORDER BY stock_id"
  )
  
  res <- if (length(params)) dbGetQuery(con, sql, params = params) else dbGetQuery(con, sql)
  if (nrow(res) == 0) stop("no matching flstock versions found")
  
  stocks <- lapply(seq_len(nrow(res)), function(i) {
    stk <- json_to_flstock(res$json[[i]])
    assign(res$stock_id[[i]], stk, envir = envir)
    message("loaded ", res$stock_id[[i]], " (", res$version[[i]], ")")
    stk
  })
  names(stocks) <- res$stock_id
  
  invisible(FLStocks(stocks))
}

## Example
# load_flstocks(flstock_github_url)                         # straight from GitHub
# load_flstocks("flstocks.sqlite")                          # latest of every stock
# load_flstocks("flstocks.sqlite", stock_id = "ple.27.4a",
#               version = "WGNSSK 2026 Final")
#
# export_flstock_parquet("flstocks.sqlite", "flstocks.parquet")
# load_flstocks("flstocks.parquet")