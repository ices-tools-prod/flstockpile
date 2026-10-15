# Make an FLStock from a SAG summary table saved by get_sag.R.
#
# SAG has the catches, landings and discards of a stock as totals by year.
# These go into the catch, landings and discards slots, with no ages, and the
# other slots are left empty. Where SAG has no catches for a year, but has the
# landings and the discards, the catch is their sum.
#
# file  a file saved by get_sag.R, named
#       "<stock code>_<assessment year>_<assessment key>_<retrieved>.csv"
#
# Returns the FLStock, named by the stock code and described by the SAG
# assessment key.

sag_flstock <- function(file) {
  table <- read.csv(file)
  key <- sag_file_info(file)

  units <- unique(table$units[!is.na(table$units)])
  if (!identical(units, "tonnes")) {
    stop(basename(file), ": units must be tonnes, not ", paste(units, collapse = ", "))
  }

  catch <- ifelse(
    is.na(table$catches) & !is.na(table$landings) & !is.na(table$discards),
    table$landings + table$discards,
    table$catches
  )
  totals <- function(values) {
    FLQuant(values, dimnames = list(age = "all", year = table$Year), units = "t")
  }

  FLStock(
    catch = totals(catch), landings = totals(table$landings), discards = totals(table$discards),
    name = key$stock_code, desc = paste("SAG assessment key", key$assessment_key)
  )
}

# The stock code, assessment year, assessment key and retrieval time of a file
# saved by get_sag.R, from its name.
#
# Returns a data frame with one row per file.

sag_file_info <- function(file) {
  if (length(file) == 0) {
    return(data.frame(file = character(), stock_code = character(), assessment_year = integer(),
      assessment_key = integer(), retrieved = as.POSIXct(character(), tz = "UTC")))
  }
  parts <- do.call(rbind, strsplit(sub("\\.csv$", "", basename(file)), "_"))
  data.frame(
    file = basename(file),
    stock_code = parts[, 1],
    assessment_year = as.integer(parts[, 2]),
    assessment_key = as.integer(parts[, 3]),
    retrieved = as.POSIXct(parts[, 4], format = "%Y%m%d%H%M%S", tz = "UTC")
  )
}
