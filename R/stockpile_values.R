#' Observed data and model estimates
#'
#' Get the values for one or more stock assessments: catches, weights,
#' maturity, natural mortality, stock numbers and fishing mortality, by age
#' and year. By default the latest version of each stock assessment is
#' returned.
#'
#' A value is stored as `"observed"` or `"estimated"`. Where a slot is the same
#' in the observed and the estimated object, for example the maturity, it is
#' stored once with type `"both"`. Asking for `type = "observed"` or
#' `type = "estimated"` returns the values of that type together with those of
#' type `"both"`, which is everything in the submitted object of that type.
#'
#' Where the submitted object had iterations, the values are the medians over
#' the iterations.
#'
#' @inheritParams stockpile_stocks
#' @param type `"observed"` or `"estimated"`. The default is all values.
#' @param slot the slots to return, named as in an `FLStock`, for example
#'   `"catch.n"`. The default is all slots.
#'
#' @return A data frame with one row per value:
#' \describe{
#'   \item{stock_code, assessment_year, version}{identify the version}
#'   \item{slot}{the slot, named as in an `FLStock`}
#'   \item{type}{`"observed"`, `"estimated"` or `"both"`}
#'   \item{age}{the age, or `NA` for a total over ages}
#'   \item{year, unit, season, area}{the year, and the unit, season and area
#'     as named by the submitter}
#'   \item{value}{the value. Missing values are not stored}
#'   \item{units}{the units of measurement given by the submitter}
#' }
#'
#' @seealso [stockpile_flstock()] to get the values as an `FLStock`.
#'
#' @examples
#' \dontrun{
#' stockpile_values("sol.27.7d", slot = "catch")
#' stockpile_values("sol.27.7d", type = "observed", slot = "catch.n")
#' }
#'
#' @export

stockpile_values <- function(stock_code = NULL, assessment_year = NULL, version = "latest", as_of = NULL,
                             type = NULL, slot = NULL, url = stockpile_url(), refresh = FALSE) {
  versions <- select_versions(stock_code, assessment_year, version, as_of, url, refresh)
  values <- for_versions(read_table("values", url, refresh), versions)

  if (!is.null(type)) {
    type <- match.arg(type, c("observed", "estimated"))
    values <- values[values$type %in% c(type, "both"), ]
  }
  if (!is.null(slot)) {
    values <- values[values$slot %in% slot, ]
  }

  rownames(values) <- NULL
  values
}

#' Survey indices
#'
#' Get the survey indices for one or more stock assessments. By default the
#' latest version of each stock assessment is returned.
#'
#' @inheritParams stockpile_stocks
#' @param index the names of the indices to return. The default is all
#'   indices.
#'
#' @return A data frame with one row per value, as in [stockpile_values()],
#' and also:
#' \describe{
#'   \item{index}{the name of the index}
#'   \item{slot}{the slot, named as in an `FLIndex`, for example `"index"`}
#'   \item{startf, endf}{the part of the year the survey covers}
#' }
#'
#' @examples
#' \dontrun{
#' stockpile_indices("whg.27.7b-ce-k")
#' }
#'
#' @export

stockpile_indices <- function(stock_code = NULL, assessment_year = NULL, version = "latest", as_of = NULL,
                              index = NULL, url = stockpile_url(), refresh = FALSE) {
  versions <- select_versions(stock_code, assessment_year, version, as_of, url, refresh)
  indices <- for_versions(read_table("indices", url, refresh), versions)

  if (!is.null(index)) {
    indices <- indices[indices$index %in% index, ]
  }

  rownames(indices) <- NULL
  indices
}
