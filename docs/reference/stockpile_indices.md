# Survey indices

Get the survey indices for one or more stock assessments. By default the
latest version of each stock assessment is returned.

## Usage

``` r
stockpile_indices(
  stock_code = NULL,
  assessment_year = NULL,
  version = "latest",
  as_of = NULL,
  index = NULL,
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

- index:

  the names of the indices to return. The default is all indices.

- url:

  the location of the dataset, see
  [`stockpile_url()`](https://ices-tools-prod.github.io/flstockpile/reference/stockpile_url.md).

- refresh:

  read the table again, even if it has already been read in this
  session.

## Value

A data frame with one row per value, as in
[`stockpile_values()`](https://ices-tools-prod.github.io/flstockpile/reference/stockpile_values.md),
and also:

- index:

  the name of the index

- slot:

  the slot, named as in an `FLIndex`, for example `"index"`

- startf, endf:

  the part of the year the survey covers

## Examples

``` r
if (FALSE) { # \dontrun{
stockpile_indices("whg.27.7b-ce-k")
} # }
```
