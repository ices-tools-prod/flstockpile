# Changelog

## flstockpile 0.1.0

- First version:
  [`stockpile_stocks()`](https://ices-tools-prod.github.io/flstockpile/reference/stockpile_stocks.md),
  [`stockpile_sources()`](https://ices-tools-prod.github.io/flstockpile/reference/stockpile_sources.md),
  [`stockpile_values()`](https://ices-tools-prod.github.io/flstockpile/reference/stockpile_values.md)
  and
  [`stockpile_indices()`](https://ices-tools-prod.github.io/flstockpile/reference/stockpile_indices.md)
  read the dataset tables, and
  [`stockpile_flstock()`](https://ices-tools-prod.github.io/flstockpile/reference/stockpile_flstock.md)
  converts the values for one stock to an `FLStock`.

- The package is renamed from FLStockpile to flstockpile, to match its
  repository, which has moved to ices-tools-prod/flstockpile. The option
  that sets the dataset location is now `flstockpile.url`.

- Stocks with no FLStock submitted, such as those in data category 5,
  can be taken from the catches, landings and discards published in SAG.
  The new `origin` column of
  [`stockpile_stocks()`](https://ices-tools-prod.github.io/flstockpile/reference/stockpile_stocks.md)
  and
  [`stockpile_sources()`](https://ices-tools-prod.github.io/flstockpile/reference/stockpile_sources.md)
  is `"SAG"` for these, and
  [`stockpile_flstock()`](https://ices-tools-prod.github.io/flstockpile/reference/stockpile_flstock.md)
  returns them as an `FLStock` with no ages.
