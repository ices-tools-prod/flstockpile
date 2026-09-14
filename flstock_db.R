## flstock_db.R
##
## Create, populate, and query a SQLite database of versioned FLStock
## JSON documents (see flstock_db_schema.sql, flstock_schema.json).
## Requires: RSQLite, jsonlite

library(RSQLite)
library(jsonlite)

#' Create the flstock database (and table, if not already present)
#'
#' @param db_path path to the SQLite file (created if it doesn't exist)
init_flstock_db <- function(db_path, schema_path = "flstock_db_schema.sql") {
  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con))
  schema_sql <- paste(readLines(schema_path), collapse = "\n")
  dbExecute(con, schema_sql)
  invisible(db_path)
}

#' Check whether a stock_id + version combination is already registered
#'
#' @param db_path path to the SQLite database
#' @param stock_id the stock_id to check
#' @param version the version to check
#' @return TRUE if that version is already registered, FALSE otherwise
flstock_version_exists <- function(db_path, stock_id, version) {
  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con))

  res <- dbGetQuery(con,
    "SELECT 1 FROM flstock_versions WHERE stock_id = ? AND version = ?",
    params = list(stock_id, version))
  nrow(res) > 0
}

#' Register a flstock JSON file (produced by flstock_to_json()) in the database
#'
#' By default, fails if this stock_id + version combination already exists —
#' versions are immutable once written. Set overwrite = TRUE to replace an
#' existing version instead (e.g. while testing).
#'
#' @param db_path path to the SQLite database
#' @param json_path path to a flstock JSON file
#' @param overwrite if TRUE, replace an existing stock_id + version rather than erroring
.null_to_na <- function(x) if (is.null(x)) NA else x

register_flstock_json <- function(db_path, json_path, overwrite = FALSE) {
  obj <- fromJSON(json_path, simplifyVector = FALSE)
  json_text <- paste(readLines(json_path), collapse = "\n")

  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con))

  existing <- dbGetQuery(con,
    "SELECT 1 FROM flstock_versions WHERE stock_id = ? AND version = ?",
    params = list(obj$stock_id, obj$version))
  if (nrow(existing) > 0) {
    if (!overwrite) {
      stop("stock_id '", obj$stock_id, "' version '", obj$version,
           "' is already registered (use overwrite = TRUE to replace it)")
    }
    dbExecute(con,
      "DELETE FROM flstock_versions WHERE stock_id = ? AND version = ?",
      params = list(obj$stock_id, obj$version))
  }

  dbExecute(con, "
    INSERT INTO flstock_versions
      (stock_id, version, name, description, created_by, created_at,
       source, quant, minyear, maxyear, minfbar, maxfbar, plusgroup, json)
    VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
    params = list(
      obj$stock_id, obj$version, obj$name, .null_to_na(obj$description),
      .null_to_na(obj$created_by), obj$created_at, .null_to_na(obj$source), obj$quant,
      obj$range$minyear, obj$range$maxyear,
      obj$range$minfbar, obj$range$maxfbar,
      .null_to_na(obj$range$plusgroup),
      json_text
    ))

  invisible(TRUE)
}

#' List registered versions, optionally filtered to one stock_id
#'
#' @param db_path path to the SQLite database
#' @param stock_id optional stock_id to filter to
#' @return a data.frame of metadata (no json column)
list_flstock_versions <- function(db_path, stock_id = NULL) {
  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con))

  cols <- "id, stock_id, version, name, created_by, created_at, source,
           quant, minyear, maxyear, minfbar, maxfbar, plusgroup"

  if (is.null(stock_id)) {
    dbGetQuery(con, paste("SELECT", cols, "FROM flstock_versions ORDER BY stock_id, created_at"))
  } else {
    dbGetQuery(con, paste("SELECT", cols, "FROM flstock_versions WHERE stock_id = ? ORDER BY created_at"),
               params = list(stock_id))
  }
}

#' Retrieve the raw JSON for one stock version
#'
#' @param db_path path to the SQLite database
#' @param stock_id the stock_id to retrieve
#' @param version version to retrieve; if NULL, the most recently created_at is returned
#' @return the JSON document as a string (pass to json_to_flstock() to rebuild the FLStock)
get_flstock_json <- function(db_path, stock_id, version = NULL) {
  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con))

  if (is.null(version)) {
    res <- dbGetQuery(con, "
      SELECT json FROM flstock_versions
      WHERE stock_id = ?
      ORDER BY created_at DESC LIMIT 1",
      params = list(stock_id))
  } else {
    res <- dbGetQuery(con, "
      SELECT json FROM flstock_versions
      WHERE stock_id = ? AND version = ?",
      params = list(stock_id, version))
  }

  if (nrow(res) == 0) stop("no matching flstock version found")
  res$json[[1]]
}

