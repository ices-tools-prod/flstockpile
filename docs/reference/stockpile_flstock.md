# Stock assessment as an FLStock

Get the values for one stock assessment as an `FLStock` object. This
needs the FLCore package, which is not on CRAN:
`install.packages("FLCore", repos = "https://flr.r-universe.dev")`.

## Usage

``` r
stockpile_flstock(
  stock_code,
  assessment_year = NULL,
  version = "latest",
  as_of = NULL,
  type = c("estimated", "observed"),
  url = stockpile_url(),
  refresh = FALSE
)
```

## Arguments

- stock_code:

  a stock code, for example `"sol.27.7d"`.

- assessment_year:

  an assessment year. It can be left out if the stock has only one in
  the stockpile.

- version:

  `"latest"` for the most recent version, or a version number.

- as_of:

  a date or time, to see the stockpile as it stood then: only versions
  submitted up to that time are used. A date means the start of that
  day.

- type:

  `"estimated"` for the model estimates, or `"observed"` for the
  observed data.

- url:

  the location of the stockpile, see
  [`stockpile_url()`](https://neilcampbelll.github.io/FLstockpile/reference/stockpile_url.md).

- refresh:

  read the table again, even if it has already been read in this
  session.

## Value

An `FLStock` object, with the same values, units, ranges, name and
description as the one submitted. A slot that was all missing values
comes back empty, with the default units of FLCore.

## Details

The `FLStock` has one iteration. Where the submitted object had more,
the values are the medians over the iterations.

## See also

[`stockpile_values()`](https://neilcampbelll.github.io/FLstockpile/reference/stockpile_values.md)
for the same values as a data frame.

## Examples

``` r
if (FALSE) { # \dontrun{
stock <- stockpile_flstock("sol.27.7d")
observed <- stockpile_flstock("sol.27.7d", type = "observed")
} # }
```
