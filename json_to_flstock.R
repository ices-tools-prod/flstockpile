## json_to_flstock.R
##
## Convert a flstock_schema.json record back into an FLStock object.
## Requires: FLCore, jsonlite

.flstock_slots <- c(
  "catch", "catch.n", "catch.wt",
  "discards", "discards.n", "discards.wt",
  "landings", "landings.n", "landings.wt",
  "stock", "stock.n", "stock.wt",
  "m", "mat",
  "harvest", "harvest.spwn", "m.spwn"
)

#' Rebuild a single FLQuant slot from its {quant, year, value} records
.records_to_flquant <- function(records_df, quant_name, units_value) {
  ages  <- unique(records_df$quant)   ## character: a number (age/len) or "all" for aggregate slots
  years <- sort(unique(records_df$year))

  m <- matrix(NA_real_, nrow = length(ages), ncol = length(years),
              dimnames = list(ages, as.character(years)))
  m[cbind(match(records_df$quant, ages), match(records_df$year, years))] <- records_df$value

  dn <- setNames(list(ages, as.character(years)), c(quant_name, "year"))
  flq <- FLQuant(m, dimnames = dn, quant = quant_name)

  units(flq) <- if (is.null(units_value)) NA_character_ else units_value
  flq
}

#' Convert a flstock JSON record (file path or JSON string) back to an FLStock object
#'
#' @param x path to a JSON file, or a JSON string
#' @return an FLStock object
json_to_flstock <- function(x) {

  obj <- if (file.exists(x)) fromJSON(x, simplifyVector = FALSE) else fromJSON(x, simplifyVector = FALSE)

  quant_name <- obj$quant

  slot_list <- lapply(.flstock_slots, function(s) {
    slotdata <- obj$slots[[s]]
    records_df <- do.call(rbind, lapply(slotdata$data, function(r) {
      data.frame(
        quant = as.character(r$quant),
        year  = as.numeric(r$year),
        value = if (is.null(r$value)) NA_real_ else as.numeric(r$value)
      )
    }))
    .records_to_flquant(records_df, quant_name, slotdata$units)
  })
  names(slot_list) <- .flstock_slots

  stock <- FLStock(
    name = obj$name,
    desc = if (is.null(obj$description)) NA_character_ else obj$description
  )
  for (s in .flstock_slots) slot(stock, s) <- slot_list[[s]]

  rng <- obj$range
  range(stock)["min"]     <- rng$min
  range(stock)["max"]     <- rng$max
  range(stock)["minyear"] <- rng$minyear
  range(stock)["maxyear"] <- rng$maxyear
  range(stock)["minfbar"] <- rng$minfbar
  range(stock)["maxfbar"] <- rng$maxfbar
  if (!is.null(rng$plusgroup)) range(stock)["plusgroup"] <- rng$plusgroup

  stock
}

