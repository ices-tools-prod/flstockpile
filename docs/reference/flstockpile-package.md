# Access the ICES dataset of stock assessment results

The dataset holds the stock assessment results submitted to ICES, as
four parquet tables. Each stock assessment, a stock code and an
assessment year, has one or more versions: a corrected submission
becomes a new version, and earlier versions are kept.

## Details

- stocks:

  one row per version, see
  [`stockpile_stocks()`](https://ices-tools-prod.github.io/flstockpile/reference/stockpile_stocks.md)

- sources:

  the submitted objects each version was made from, see
  [`stockpile_sources()`](https://ices-tools-prod.github.io/flstockpile/reference/stockpile_sources.md)

- values:

  the observed data and model estimates, see
  [`stockpile_values()`](https://ices-tools-prod.github.io/flstockpile/reference/stockpile_values.md)

- indices:

  the survey indices, see
  [`stockpile_indices()`](https://ices-tools-prod.github.io/flstockpile/reference/stockpile_indices.md)

The location of the tables is set with
[`stockpile_url()`](https://ices-tools-prod.github.io/flstockpile/reference/stockpile_url.md).

## See also

Useful links:

- <https://ices-tools-prod.github.io/flstockpile/>

- <https://github.com/ices-tools-prod/flstockpile>

- Report bugs at <https://github.com/ices-tools-prod/flstockpile/issues>

## Author

**Maintainer**: Colin Millar <colin.millar@ices.dk>

Authors:

- Neil Campbell
