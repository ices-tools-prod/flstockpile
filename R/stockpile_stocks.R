#' Stock assessments in the dataset
#'
#' List the stock assessments in the dataset. With no arguments, the latest
#' version of every stock assessment is returned.
#'
#' @param stock_code stock codes, for example `"sol.27.7d"`. The default is
#'   all stocks.
#' @param assessment_year assessment years. The default is all years.
#' @param version `"latest"` for the most recent version of each stock
#'   assessment, `"all"` for every version, or version numbers.
#' @param as_of a date or time, to see the dataset as it stood then: only
#'   versions submitted up to that time are used. A date means the start of
#'   that day.
#' @param url the location of the dataset, see [stockpile_url()].
#' @param refresh read the table again, even if it has already been read in
#'   this session.
#'
#' @return A data frame with one row per version:
#' \describe{
#'   \item{stock_code, assessment_year, version}{identify the version}
#'   \item{latest}{is this the most recent version of the stock assessment?}
#'   \item{case_id}{the ICES case number the submissions were made under}
#'   \item{stock_key}{the key of the stock in the ICES Stock Database, as used
#'     by the icesSD package}
#'   \item{expert_group}{the ICES expert group that assesses the stock}
#'   \item{assessment_key}{the key of the published assessment in the ICES
#'     Stock Assessment Graphs database, as used by the icesSAG package. Missing
#'     if the assessment is not published there}
#'   \item{submitted}{when the most recent submission in the version was made}
#'   \item{observed, estimated}{does the version have observed data, and model
#'     estimates?}
#'   \item{indices}{the number of survey indices}
#'   \item{name, desc}{the name and description given by the submitter}
#'   \item{quant}{what the `age` column of the values holds, usually `"age"`}
#'   \item{min_age, max_age, plusgroup, min_year, max_year, min_fbar,
#'     max_fbar}{the ranges of the stock}
#'   \item{n_units, n_seasons, n_areas, n_iters}{the number of units, seasons,
#'     areas and iterations in the submitted object}
#' }
#' The ranges are those of the estimated object, or of the observed object
#' where there are no estimates.
#'
#' @seealso [stockpile_sources()] for the submitted objects behind each
#'   version, [stockpile_values()] for the data.
#'
#' @examples
#' \dontrun{
#' stockpile_stocks()
#' stockpile_stocks("meg.27.7b-k8abd", version = "all")
#' stockpile_stocks(as_of = "2026-06-01")
#' }
#'
#' @export

stockpile_stocks <- function(stock_code = NULL, assessment_year = NULL, version = "latest", as_of = NULL,
                             url = stockpile_url(), refresh = FALSE) {
  select_versions(stock_code, assessment_year, version, as_of, url, refresh)
}

#' Submitted objects behind the stock assessments
#'
#' List the submitted objects that each version of a stock assessment was made
#' from. A version has up to one object with the observed data and one with
#' the model estimates, and any number of survey indices.
#'
#' @inheritParams stockpile_stocks
#'
#' @return A data frame with one row per object:
#' \describe{
#'   \item{stock_code, assessment_year, version}{identify the version}
#'   \item{type}{`"observed"` or `"estimated"`}
#'   \item{class}{the class of the submitted object, `"FLStock"` or
#'     `"FLIndex"`}
#'   \item{submission_id, submitted}{the submission the object came from, and
#'     when it was made}
#'   \item{file, object}{the submitted file, and the name of the object in it}
#'   \item{notes}{the notes given by the submitter}
#'   \item{units_assumed}{were any numbers or weights submitted with no units?
#'     They are then taken to be in thousands and kg}
#'   \item{name, desc, quant, min_age, ..., n_iters}{the name, description,
#'     ranges and dimensions of the object, as in [stockpile_stocks()]}
#' }
#'
#' @examples
#' \dontrun{
#' stockpile_sources("sol.27.7d")
#' }
#'
#' @export

stockpile_sources <- function(stock_code = NULL, assessment_year = NULL, version = "latest", as_of = NULL,
                              url = stockpile_url(), refresh = FALSE) {
  versions <- select_versions(stock_code, assessment_year, version, as_of, url, refresh)
  for_versions(read_table("sources", url, refresh), versions)
}
