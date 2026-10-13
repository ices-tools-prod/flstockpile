#' Location of the stockpile
#'
#' The stockpile is a set of parquet files in one place, which can be a web
#' address or a folder. The location is taken from the option
#' `FLStockpile.url`, or if that is not set, from the environment variable
#' `FLSTOCKPILE_URL`.
#'
#' @return The location, as a character string.
#'
#' @examples
#' \dontrun{
#' options(FLStockpile.url = "path/to/parquet")
#' stockpile_url()
#' }
#'
#' @export

stockpile_url <- function() {
  url <- getOption("FLStockpile.url", Sys.getenv("FLSTOCKPILE_URL"))
  if (!is.character(url) || length(url) != 1 || !nzchar(url)) {
    stop(
      "the location of the stockpile is not set, ",
      "use options(FLStockpile.url = ) or the environment variable FLSTOCKPILE_URL"
    )
  }
  sub("/+$", "", url)
}

# tables read in this session, by location
cache <- new.env()

# Read one of the stockpile tables.
#
# name    the table: stocks, sources, values or indices
# url     the location of the stockpile
# refresh read the table again, even if it was read earlier in this session
#
# Returns a data frame.

read_table <- function(name, url, refresh = FALSE) {
  key <- paste(url, name)
  if (refresh || is.null(cache[[key]])) {
    file <- paste0(url, "/", name, ".parquet")
    if (grepl("^https?://", url)) {
      local_file <- tempfile(fileext = ".parquet")
      on.exit(unlink(local_file))
      utils::download.file(file, local_file, mode = "wb", quiet = TRUE)
      file <- local_file
    }
    cache[[key]] <- as.data.frame(arrow::read_parquet(file))
  }
  cache[[key]]
}

# The versions asked for, as rows of the stocks table.
#
# See stockpile_stocks() for the arguments.

select_versions <- function(stock_code, assessment_year, version, as_of, url, refresh) {
  stocks <- read_table("stocks", url, refresh)

  if (!is.null(stock_code)) {
    unknown <- setdiff(stock_code, stocks$stock_code)
    if (length(unknown) > 0) {
      stop("no such stock code in the stockpile: ", paste(unknown, collapse = ", "))
    }
    stocks <- stocks[stocks$stock_code %in% stock_code, ]
  }
  if (!is.null(assessment_year)) {
    stocks <- stocks[stocks$assessment_year %in% assessment_year, ]
  }
  if (!is.null(as_of)) {
    stocks <- stocks[stocks$submitted <= as.POSIXct(as_of, tz = "UTC"), ]
  }

  if (identical(version, "latest")) {
    newest <- stats::ave(stocks$version, stocks$stock_code, stocks$assessment_year, FUN = max)
    stocks <- stocks[stocks$version == newest, ]
  } else if (!identical(version, "all")) {
    if (!is.numeric(version)) {
      stop("version must be \"latest\", \"all\" or version numbers")
    }
    stocks <- stocks[stocks$version %in% version, ]
  }

  rownames(stocks) <- NULL
  stocks
}

# The rows of a table for some versions.

for_versions <- function(table, versions) {
  key <- function(x) paste(x$stock_code, x$assessment_year, x$version)
  table <- table[key(table) %in% key(versions), ]
  rownames(table) <- NULL
  table
}
