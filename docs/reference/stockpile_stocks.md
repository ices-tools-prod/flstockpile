# Stock assessments in the dataset

List the stock assessments in the dataset. With no arguments, the latest
version of every stock assessment is returned.

## Usage

``` r
stockpile_stocks(
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
  [`stockpile_url()`](https://neilcampbelll.github.io/FLstockpile/reference/stockpile_url.md).

- refresh:

  read the table again, even if it has already been read in this
  session.

## Value

A data frame with one row per version:

- stock_code, assessment_year, version:

  identify the version

- latest:

  is this the most recent version of the stock assessment?

- case_id:

  the ICES case number the submissions were made under

- submitted:

  when the most recent submission in the version was made

- observed, estimated:

  does the version have observed data, and model estimates?

- indices:

  the number of survey indices

- name, desc:

  the name and description given by the submitter

- quant:

  what the `age` column of the values holds, usually `"age"`

- min_age, max_age, plusgroup, min_year, max_year, min_fbar, max_fbar:

  the ranges of the stock

- n_units, n_seasons, n_areas, n_iters:

  the number of units, seasons, areas and iterations in the submitted
  object

The ranges are those of the estimated object, or of the observed object
where there are no estimates.

## See also

[`stockpile_sources()`](https://neilcampbelll.github.io/FLstockpile/reference/stockpile_sources.md)
for the submitted objects behind each version,
[`stockpile_values()`](https://neilcampbelll.github.io/FLstockpile/reference/stockpile_values.md)
for the data.

## Examples

``` r
if (FALSE) { # \dontrun{
stockpile_stocks()
stockpile_stocks("meg.27.7b-k8abd", version = "all")
stockpile_stocks(as_of = "2026-06-01")
} # }
```
