#' Stock assessment as an FLStock
#'
#' Get the values for one stock assessment as an `FLStock` object. This needs
#' the FLCore package, which is not on CRAN:
#' `install.packages("FLCore", repos = "https://flr.r-universe.dev")`.
#'
#' The `FLStock` has one iteration. Where the submitted object had more, the
#' values are the medians over the iterations.
#'
#' @inheritParams stockpile_stocks
#' @param stock_code a stock code, for example `"sol.27.7d"`.
#' @param assessment_year an assessment year. It can be left out if the stock
#'   has only one in the dataset.
#' @param version `"latest"` for the most recent version, or a version number.
#' @param type `"estimated"` for the model estimates, or `"observed"` for the
#'   observed data.
#'
#' @return An `FLStock` object, with the same values, ranges, name and
#' description as the one submitted, except that numbers are in thousands and
#' weights in kg, whatever units they were submitted in. A slot that was all
#' missing values comes back empty, with the default units of FLCore.
#'
#' A stock from SAG (see the `origin` column of [stockpile_stocks()]) has only
#' the catch, landings and discards, in tonnes, with no ages.
#'
#' @seealso [stockpile_values()] for the same values as a data frame.
#'
#' @examples
#' \dontrun{
#' stock <- stockpile_flstock("sol.27.7d")
#' observed <- stockpile_flstock("sol.27.7d", type = "observed")
#' }
#'
#' @export

stockpile_flstock <- function(stock_code, assessment_year = NULL, version = "latest", as_of = NULL,
                              type = c("estimated", "observed"), url = stockpile_url(), refresh = FALSE) {
  if (!requireNamespace("FLCore", quietly = TRUE)) {
    stop(
      "the FLCore package is needed to make an FLStock, ",
      "install.packages(\"FLCore\", repos = \"https://flr.r-universe.dev\")"
    )
  }
  type <- match.arg(type)
  if (length(stock_code) != 1) {
    stop("stock_code must be one stock code")
  }

  versions <- select_versions(stock_code, assessment_year, version, as_of, url, refresh)
  if (nrow(versions) == 0) {
    stop("no such version of ", stock_code, " in the dataset")
  }
  if (nrow(versions) > 1) {
    stop(
      stock_code, " has more than one version that fits, give one assessment_year and version: ",
      paste0(versions$assessment_year, " version ", versions$version, collapse = ", ")
    )
  }

  source <- for_versions(read_table("sources", url, refresh), versions)
  source <- source[source$class == "FLStock" & source$type == type, ]
  if (nrow(source) == 0) {
    stop(
      stock_code, " ", versions$assessment_year, " version ", versions$version,
      " has no ", type, " object, see stockpile_stocks()"
    )
  }

  values <- for_versions(read_table("values", url, refresh), versions)
  values <- values[values$type %in% c(type, "both"), ]

  # the dimensions of the submitted object
  dimnames <- list(
    # a stock from SAG has no ages
    age = if (is.na(source$min_age)) "all" else as.character(source$min_age:source$max_age),
    year = as.character(source$min_year:source$max_year),
    unit = unique(values$unit),
    season = unique(values$season),
    area = unique(values$area),
    iter = "1"
  )
  names(dimnames)[1] <- source$quant

  quants <- lapply(split(values, values$slot), function(rows) {
    # a slot of totals has no ages
    dimnames[[1]] <- if (all(is.na(rows$age))) "all" else dimnames[[1]]
    data <- array(NA_real_, dim = lengths(dimnames), dimnames = dimnames)
    age <- if (all(is.na(rows$age))) rep("all", nrow(rows)) else as.character(rows$age)
    data[cbind(age, as.character(rows$year), rows$unit, rows$season, rows$area, "1")] <- rows$value
    FLCore::FLQuant(data, units = rows$units[1])
  })

  # slots that were all missing are not in the values, and are left as they are
  stock <- do.call(FLCore::FLStock, c(quants, list(name = source$name, desc = source$desc)))
  stock@range[c("min", "max", "plusgroup", "minyear", "maxyear", "minfbar", "maxfbar")] <- c(
    source$min_age, source$max_age, source$plusgroup, source$min_year, source$max_year,
    source$min_fbar, source$max_fbar
  )
  stock
}
