## flstock_json_convert.R
##
## Convert between FLStock objects and the flstock_schema.json record format.

# install.packages("FLCore", repos = c("https://flr.r-universe.dev", "https://cloud.r-project.org"))
# install.packages("jsonlite")
# install.packages("jsonvalidate")

library(FLCore)
library(jsonlite)
library(jsonvalidate)


source("json_to_flstock.R")
source("flstock_to_json.R")
source("flstock_db.R")

.flstock_slots <- c(
  "catch", "catch.n", "catch.wt",
  "discards", "discards.n", "discards.wt",
  "landings", "landings.n", "landings.wt",
  "stock", "stock.n", "stock.wt",
  "m", "mat",
  "harvest", "harvest.spwn", "m.spwn"
)


data(ple4)

flstock_to_json(
  stock      = ple4,
  stock_id   = "ple.27.4a",
  version    = "WGNSSK 2026 Final",
  created_by = "Neil Campbell",
  source     = NULL,
  path       = "ple4_v1.0.json"
)


validate <- json_validator("flstock_schema.json", engine = "ajv")
validate("ple4_v1.0.json", verbose = TRUE)

register_flstock_json("flstocks.sqlite", "ple4_v1.0.json")

list_flstock_versions("flstocks.sqlite") 


flstock_version_exists("flstocks.sqlite", "ple.27.4a", "WGNSSK 2026 Final")   # TRUE
register_flstock_json("flstocks.sqlite", "ple4_v1.0.json", overwrite = TRUE)  # replaces it

stock2 <- json_to_flstock(get_flstock_json("flstocks.sqlite", "ple.27.4a"))

summary(stock2)
