# Observed data and model estimates

Get the values for one or more stock assessments: catches, weights,
maturity, natural mortality, stock numbers and fishing mortality, by age
and year. By default the latest version of each stock assessment is
returned.

## Usage

``` r
stockpile_values(
  stock_code = NULL,
  assessment_year = NULL,
  version = "latest",
  as_of = NULL,
  type = NULL,
  slot = NULL,
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

- type:

  `"observed"` or `"estimated"`. The default is all values.

- slot:

  the slots to return, named as in an `FLStock`, for example
  `"catch.n"`. The default is all slots.

- url:

  the location of the dataset, see
  [`stockpile_url()`](https://neilcampbelll.github.io/FLstockpile/reference/stockpile_url.md).

- refresh:

  read the table again, even if it has already been read in this
  session.

## Value

A data frame with one row per value:

- stock_code, assessment_year, version:

  identify the version

- slot:

  the slot, named as in an `FLStock`

- type:

  `"observed"`, `"estimated"` or `"both"`

- age:

  the age, or `NA` for a total over ages

- year, unit, season, area:

  the year, and the unit, season and area as named by the submitter

- value:

  the value. Missing values are not stored

- units:

  the units of measurement. Numbers (`catch.n`, `landings.n`,
  `discards.n`, `stock.n`) are always in thousands, `"1000"`, and
  weights (`catch.wt`, `landings.wt`, `discards.wt`, `stock.wt`) in
  `"kg"`, whatever units they were submitted in. The units of the other
  slots are those given by the submitter

## Details

A value is stored as `"observed"` or `"estimated"`. Where a slot is the
same in the observed and the estimated object, for example the maturity,
it is stored once with type `"both"`. Asking for `type = "observed"` or
`type = "estimated"` returns the values of that type together with those
of type `"both"`, which is everything in the submitted object of that
type.

Where the submitted object had iterations, the values are the medians
over the iterations.

## See also

[`stockpile_flstock()`](https://neilcampbelll.github.io/FLstockpile/reference/stockpile_flstock.md)
to get the values as an `FLStock`.

## Examples

``` r
if (FALSE) { # \dontrun{
stockpile_values("sol.27.7d", slot = "catch")
stockpile_values("sol.27.7d", type = "observed", slot = "catch.n")
} # }
```
