#' Access the ICES stockpile of stock assessment results
#'
#' The stockpile holds the stock assessment results submitted to ICES, as four
#' parquet tables. Each stock assessment, a stock code and an assessment year,
#' has one or more versions: a corrected submission becomes a new version, and
#' earlier versions are kept.
#'
#' \describe{
#'   \item{stocks}{one row per version, see [stockpile_stocks()]}
#'   \item{sources}{the submitted objects each version was made from, see
#'     [stockpile_sources()]}
#'   \item{values}{the observed data and model estimates, see
#'     [stockpile_values()]}
#'   \item{indices}{the survey indices, see [stockpile_indices()]}
#' }
#'
#' The location of the tables is set with [stockpile_url()].
#'
#' @keywords internal
"_PACKAGE"
