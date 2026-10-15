# Location of the dataset

The dataset is a set of parquet files in one place, which can be a web
address or a folder. The location is taken from the option
`flstockpile.url`, or if that is not set, from the environment variable
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
options(flstockpile.url = "path/to/parquet")
stockpile_url()
} # }
```
