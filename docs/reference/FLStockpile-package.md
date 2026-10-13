# Access the ICES stockpile of stock assessment results

The stockpile holds the stock assessment results submitted to ICES, as
four parquet tables. Each stock assessment, a stock code and an
assessment year, has one or more versions: a corrected submission
becomes a new version, and earlier versions are kept.

## Details

- stocks:

  one row per version, see
  [`stockpile_stocks()`](https://neilcampbelll.github.io/FLstockpile/reference/stockpile_stocks.md)

- sources:

  the submitted objects each version was made from, see
  [`stockpile_sources()`](https://neilcampbelll.github.io/FLstockpile/reference/stockpile_sources.md)

- values:

  the observed data and model estimates, see
  [`stockpile_values()`](https://neilcampbelll.github.io/FLstockpile/reference/stockpile_values.md)

- indices:

  the survey indices, see
  [`stockpile_indices()`](https://neilcampbelll.github.io/FLstockpile/reference/stockpile_indices.md)

The location of the tables is set with
[`stockpile_url()`](https://neilcampbelll.github.io/FLstockpile/reference/stockpile_url.md).

## See also

Useful links:

- <https://github.com/neilcampbelll/FLstockpile>

- Report bugs at <https://github.com/neilcampbelll/FLstockpile/issues>

## Author

**Maintainer**: Colin Millar <colin.millar@ices.dk>

Authors:

- Neil Campbell
