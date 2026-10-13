# Location of the stockpile

The stockpile is a set of parquet files in one place, which can be a web
address or a folder. The location is taken from the option
`FLStockpile.url`, or if that is not set, from the environment variable
`FLSTOCKPILE_URL`.

## Usage

``` r
stockpile_url()
```

## Value

The location, as a character string.

## Examples

``` r
if (FALSE) { # \dontrun{
options(FLStockpile.url = "path/to/parquet")
stockpile_url()
} # }
```
