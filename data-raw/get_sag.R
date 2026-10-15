# Get the catches, landings and discards of stocks from SAG, the ICES Stock
# Assessment Graphs database.
#
# Some stocks, such as those in data category 5, have no FLStock submitted,
# because what there is to submit is already in SAG. For each stock and
# assessment year in the SAG stocks file, the summary table of its published
# assessment is saved to the SAG dir as
#
#   <stock code>_<assessment year>_<assessment key>_<retrieved>.csv
#
# where retrieved is the time it was got from SAG, as yyyymmddhhmmss in UTC.
# A table is only saved when it differs from the last one saved for the stock
# and assessment year, so the files are a record of how SAG has changed, and
# make_parquet.R makes a new version from each of them.
#
# Where SAG has more than one published assessment for a stock and year, the
# one to use is taken from the SAG keys file, as in make_parquet.R.
#
# Usage: Rscript get_sag.R [SAG stocks file] [SAG dir] [SAG keys file]

library(icesSAG)

args <- commandArgs(trailingOnly = TRUE)
sag_stocks_file <- if (length(args) >= 1) args[1] else "sag_stocks.csv"
sag_dir <- if (length(args) >= 2) args[2] else "sag"
sag_keys_file <- if (length(args) >= 3) args[3] else "sag_keys.csv"

source("sag_flstock.R")

columns <- c("Year", "catches", "landings", "discards", "units")

sag_stocks <- read.csv(sag_stocks_file)
sag_keys <- read.csv(sag_keys_file)
dir.create(sag_dir, showWarnings = FALSE)
saved <- sag_file_info(list.files(sag_dir, "\\.csv$"))
retrieved <- format(Sys.time(), "%Y%m%d%H%M%S", tz = "UTC")

for (year in unique(sag_stocks$assessment_year)) {
  published <- suppressMessages(getListStocks(year))

  for (stock_code in sag_stocks$stock_code[sag_stocks$assessment_year == year]) {
    found <- published$AssessmentKey[published$StockKeyLabel == stock_code]
    chosen <- sag_keys$assessment_key[sag_keys$stock_code == stock_code & sag_keys$assessment_year == year]
    if (length(found) > 1 && any(chosen %in% found)) {
      found <- chosen[chosen %in% found]
    }
    if (length(found) != 1) {
      cat(
        "NO SAG KEY ", stock_code, " ", year, ": ",
        if (length(found) == 0) "not published in SAG" else paste("choose one in", sag_keys_file, "from", paste(found, collapse = ", ")),
        "\n",
        sep = ""
      )
      next
    }

    table <- suppressMessages(getSummaryTable(found))[columns]
    table <- table[order(table$Year), ]
    rownames(table) <- NULL

    file <- file.path(sag_dir, paste0(paste(stock_code, year, found, retrieved, sep = "_"), ".csv"))
    write.csv(table, file, row.names = FALSE)

    # compared as read from the files, so the column types are the same
    mine <- saved[saved$stock_code == stock_code & saved$assessment_year == year, ]
    if (nrow(mine) > 0) {
      last <- mine[which.max(mine$retrieved), ]
      if (last$assessment_key == found && identical(read.csv(file.path(sag_dir, last$file)), read.csv(file))) {
        unlink(file)
        cat("unchanged ", stock_code, " ", year, "\n", sep = "")
        next
      }
    }
    cat("saved ", file, "\n", sep = "")
  }
}
