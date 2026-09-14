# flstock-json

Convert `FLStock` objects (from the [FLR](http://flr-project.org) framework) to and from a
JSON representation, for storage, versioning, and interchange outside R.

## Why

`FLStock` objects are tied to the FLCore package and its S4 class machinery. This repo
defines a plain JSON schema for a stock assessment snapshot, plus R functions to convert
in both directions, so stored data doesn't depend on any particular version of FLCore (or
R) to remain readable.

## Files

- `flstock_schema.json` — JSON Schema defining a valid stock version record
- `flstock_to_json.R` — convert an `FLStock` object to JSON
- `json_to_flstock.R` — convert JSON back to an `FLStock` object
- `flstock_db_schema.sql` — SQLite table for storing versioned records
- `flstock_db.R` — R functions to create, register, list, and fetch records from that database

## Quick start

```r
source("flstock_to_json.R")
source("json_to_flstock.R")

data(ple4)

flstock_to_json(
  stock      = ple4,
  stock_id   = "ple.27.4a",
  version    = "1.0",
  created_by = "Your Name",
  path       = "ple4_v1.0.json"
)

stock2 <- json_to_flstock("ple4_v1.0.json")
```

To validate a JSON file against the schema:

```r
library(jsonvalidate)
validate <- json_validator("flstock_schema.json", engine = "ajv")
validate("ple4_v1.0.json", verbose = TRUE)
```

## Status

Early development. Currently supports `FLStock` only.

## Licence

MIT
