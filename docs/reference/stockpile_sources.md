# Submitted objects behind the stock assessments

List the submitted objects that each version of a stock assessment was
made from. A version has up to one object with the observed data and one
with the model estimates, and any number of survey indices.

## Usage

``` r
stockpile_sources(
  stock_code = NULL,
  assessment_year = NULL,
  version = "latest",
  as_of = NULL,
  url = stockpile_url(),
  refresh = FALSE
)
```

## Arguments

- stock_code:

  stock codes, for example `"sol.27.7d"`. The default is all stocks.

- assessment_year:

  assessment years. The default is all years.

- version:

  `"latest"` for the most recent version of each stock assessment,
  `"all"` for every version, or version numbers.

- as_of:

  a date or time, to see the dataset as it stood then: only versions
  submitted up to that time are used. A date means the start of that
  day.

- url:

  the location of the dataset, see
  [`stockpile_url()`](https://ices-tools-prod.github.io/flstockpile/reference/stockpile_url.md).

- refresh:

  read the table again, even if it has already been read in this
  session.

## Value

A data frame with one row per object:

- stock_code, assessment_year, version:

  identify the version

- type:

  `"observed"` or `"estimated"`

- class:

  the class of the submitted object, `"FLStock"` or `"FLIndex"`

- submission_id, submitted:

  the submission the object came from, and when it was made

- file, object:

  the submitted file, and the name of the object in it

- notes:

  the notes given by the submitter

- units_assumed:

  were any numbers or weights submitted with no units? They are then
  taken to be in thousands and kg

- name, desc, quant, min_age, ..., n_iters:

  the name, description, ranges and dimensions of the object, as in
  [`stockpile_stocks()`](https://ices-tools-prod.github.io/flstockpile/reference/stockpile_stocks.md)

## Examples

``` r
if (FALSE) { # \dontrun{
stockpile_sources("sol.27.7d")
} # }
```
