
<!-- README.md is generated from README.Rmd. Please edit that file -->

# FLStockpile

Read the stock assessment results submitted to ICES: the observed data
and the model estimates for each stock, by age and year.

The dataset is published as a set of
[parquet](https://parquet.apache.org) files in the
[releases](https://github.com/neilcampbelll/FLstockpile/releases) of
this repository. This package reads them into plain data frames, and can
convert a stock to an `FLStock` object if the
[FLCore](https://flr-project.org) package is installed. The parquet
files can also be read directly, with any parquet reader, in any
language.

The current data release is a test release. The content and layout may
change.

## Installation

``` r
# install.packages("remotes")
remotes::install_github("neilcampbelll/FLstockpile")
```

## Usage

Say where the dataset is. This is a data release, so it will not change:

``` r
library(FLStockpile)

options(FLStockpile.url = "https://github.com/neilcampbelll/FLstockpile/releases/download/v2026.001")
```

List the stock assessments. Each one is a stock code and an assessment
year, and by default the latest version of each is returned:

``` r
stocks <- stockpile_stocks()
nrow(stocks)
#> [1] 47
head(stocks[c("stock_code", "assessment_year", "version", "case_id", "observed", "estimated")])
#>      stock_code assessment_year version          case_id observed estimated
#> 1  ank.27.78abd            2026       1 CAS-88100-V7W8S9    FALSE      TRUE
#> 2   ank.27.8c9a            2026       1 CAS-88489-K0L3G2    FALSE      TRUE
#> 3 bll.27.3a47de            2026       1 CAS-87856-K4Q1M7    FALSE      TRUE
#> 4     caa.27.5a            2026       1 CAS-88823-L1B9G4    FALSE      TRUE
#> 5  cod.21.1.isc            2026       1 CAS-88393-C4D5T8     TRUE     FALSE
#> 6  cod.21.1.osc            2026       1 CAS-88389-H4P0R9     TRUE     FALSE
```

Get the values for a stock, here the total catch in the last two years.
The observed catch and the catch estimated by the model are both stored:

``` r
catch <- stockpile_values("sol.27.7d", slot = "catch")
catch <- catch[catch$year >= 2024, c("stock_code", "slot", "type", "year", "value")]
catch[order(catch$year, catch$type), ]
#>    stock_code  slot      type year    value
#> 87  sol.27.7d catch estimated 2024 1374.940
#> 43  sol.27.7d catch  observed 2024 1574.047
#> 88  sol.27.7d catch estimated 2025 1276.954
#> 44  sol.27.7d catch  observed 2025 1251.492
```

Get the estimates as an `FLStock`, which needs FLCore
(`install.packages("FLCore", repos = "https://flr.r-universe.dev")`):

``` r
library(FLCore)

stock <- stockpile_flstock("sol.27.7d")
summary(stock)
#> An object of class "FLStock"
#> 
#> Name:  
#> Description: FLStock created from SAM model fit 
#> Quant: age 
#> Dims:  age   year    unit    season  area    iter
#>  11  44  1   1   1   1   
#> 
#> Range:  min  max pgroup  minyear maxyear minfbar maxfbar 
#>  1   11  11  1982    2025    3   7   
#> 
#> Metrics: 
#>   rec: 9491 - 49819  (NA) 
#>   ssb: 8875 - 18668  (NA) 
#>   catch: 1277 - 6416  (NA) 
#>   fbar: 0.20 - 0.53  (f)
```

and the observed data with
`stockpile_flstock("sol.27.7d", type = "observed")`.

## Versions

Submissions made under the same ICES case number form one stock
assessment. If a corrected submission arrives, it becomes a new version,
and the earlier versions are kept:

``` r
stockpile_stocks("meg.27.7b-k8abd", version = "all")[c("stock_code", "version", "latest", "submitted", "n_iters")]
#>        stock_code version latest           submitted n_iters
#> 1 meg.27.7b-k8abd       1  FALSE 2026-05-11 09:43:38       1
#> 2 meg.27.7b-k8abd       2  FALSE 2026-05-11 10:31:59       1
#> 3 meg.27.7b-k8abd       3   TRUE 2026-10-04 13:56:04    1000
```

Every function takes a `version`, and an `as_of` date to see the data as
it stood on that date:

``` r
stockpile_stocks("meg.27.7b-k8abd", as_of = "2026-06-01")[c("stock_code", "version", "submitted")]
#>        stock_code version           submitted
#> 1 meg.27.7b-k8abd       2 2026-05-11 10:31:59
```

## The tables

The dataset is four parquet files. All of them have the columns
`stock_code`, `assessment_year` and `version`, which together identify a
version of a stock assessment.

| File | One row per | Read with |
|----|----|----|
| `stocks.parquet` | version | `stockpile_stocks()` |
| `sources.parquet` | submitted object a version was made from | `stockpile_sources()` |
| `values.parquet` | value | `stockpile_values()` |
| `indices.parquet` | survey index value | `stockpile_indices()` |

**stocks** has `latest` (is this the most recent version?), `case_id`,
`submitted`, whether the version has `observed` data and `estimated`
values, the number of survey `indices`, and the `name`, `desc`, ranges
(`min_age`, `max_age`, `plusgroup`, `min_year`, `max_year`, `min_fbar`,
`max_fbar`) and dimensions (`n_units`, `n_seasons`, `n_areas`,
`n_iters`) of the stock.

**sources** has the `type` (`observed` or `estimated`) and `class` of
each submitted object, the `submission_id` and `submitted` time of the
submission it came from, the `file` and `object` name, the submitter’s
`notes`, and the same name, description, ranges and dimensions as in
stocks, for that object.

**values** has

| Column | Contents |
|----|----|
| `slot` | the quantity, named as in an `FLStock`: `catch`, `catch.n`, `catch.wt`, `discards`, `discards.n`, `discards.wt`, `landings`, `landings.n`, `landings.wt`, `stock`, `stock.n`, `stock.wt`, `m`, `mat`, `harvest`, `harvest.spwn`, `m.spwn` |
| `type` | `observed` or `estimated`, or `both` where the observed and estimated objects have the same values for the slot, which are then stored once |
| `age` | the age, or missing for a total over ages |
| `year`, `unit`, `season`, `area` | the year, and the unit, season and area as named by the submitter |
| `value` | the value; missing values are not stored |
| `units` | the units of measurement given by the submitter |

Where a submitted object had iterations, the values are the medians over
the iterations.

**indices** has the same columns as values, with the name of the
`index`, slots named as in an `FLIndex` (`index`, `catch.n`, `effort`,
…), and the part of the year the survey covers (`startf`, `endf`).

A list of `FLStock` objects made from the same tables, `flstocks.rds`,
is published with each release.

## How the data are made

The scripts that make the tables from the submissions are in
[data-raw](data-raw): `get_submissions.R` downloads the submitted files,
`make_parquet.R` makes the tables, and `check_parquet.R` checks them
against the submitted objects. `object_types.csv` records which
submitted objects hold observed data and which hold model estimates.
