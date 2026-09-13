## flstock_to_json.R
##
## Convert an FLStock object to the flstock_schema.json record format.
## Requires: FLCore, jsonlite

.flstock_slots <- c(
  "catch", "catch.n", "catch.wt",
  "discards", "discards.n", "discards.wt",
  "landings", "landings.n", "landings.wt",
  "stock", "stock.n", "stock.wt",
  "m", "mat",
  "harvest", "harvest.spwn", "m.spwn"
)

#' Convert a single FLQuant slot (age/len x year) to a {quant, year, value} data.frame
.flquant_to_records <- function(flq) {
  qname <- names(dimnames(flq))[1]
  df <- as.data.frame(flq, cohort = FALSE)
  df <- df[, c(qname, "year", "data")]
  names(df) <- c("quant", "year", "value")
  df$quant <- as.character(df$quant)   ## may be a number (age/len) or "all" for aggregate slots
  df$year  <- as.numeric(as.character(df$year))
  df[order(df$quant, df$year), ]
}

#' Convert an FLStock object to the flstock JSON schema, and optionally write to disk
#'
#' @param stock an FLStock object
#' @param stock_id stable identifier for the stock, e.g. "her-irlw"
#' @param version version tag or hash for this snapshot
#' @param created_by optional author/process name
#' @param source optional provenance note (e.g. originating file)
#' @param path optional file path; if given, the JSON is written there
#' @return the JSON string (invisibly, if written to file)
flstock_to_json <- function(stock, stock_id, version, created_by = NULL,
                            source = NULL, path = NULL) {
  
  quant_name <- names(dimnames(catch.n(stock)))[1]
  rng <- as.list(range(stock))
  
  out <- list(
    stock_id    = stock_id,
    name        = name(stock),
    description = desc(stock),
    version     = version,
    created_by  = created_by,
    created_at  = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    source      = source,
    quant       = quant_name,
    range       = list(
      min       = rng$min,
      max       = rng$max,
      plusgroup = if (is.null(rng$plusgroup) || is.na(rng$plusgroup)) NULL else rng$plusgroup,
      minyear   = rng$minyear,
      maxyear   = rng$maxyear,
      minfbar   = rng$minfbar,
      maxfbar   = rng$maxfbar
    ),
    slots = setNames(
      lapply(.flstock_slots, function(s) {
        flq <- slot(stock, s)
        list(
          units = if (is.na(units(flq))) NULL else units(flq),
          data  = .flquant_to_records(flq)
        )
      }),
      .flstock_slots
    )
  )
  
  json <- toJSON(out, auto_unbox = TRUE, pretty = TRUE, na = "null", null = "null", dataframe = "rows")
  
  if (!is.null(path)) {
    writeLines(json, path)
    return(invisible(json))
  }
  json
}