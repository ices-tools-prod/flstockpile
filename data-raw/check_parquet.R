# Check the parquet tables against the submitted objects.
#
# Every FLStock that a version was made from is read again from the extract
# dir, and compared with the FLStock that the FLStockpile package makes from
# the parquet tables: the values, dimensions and units of every slot, and the
# ranges, name and description. Where the submitted object has iterations, it
# is the medians over the iterations that are compared.
#
# The stockpile can be a folder or a web address, so this also checks a
# published copy of the tables.
#
# Usage: Rscript check_parquet.R [stockpile] [extract dir] [package dir]

library(FLCore)

args <- commandArgs(trailingOnly = TRUE)
stockpile <- if (length(args) >= 1) args[1] else "parquet"
extract_dir <- if (length(args) >= 2) args[2] else "extracted"
package_dir <- if (length(args) >= 3) args[3] else ".."

devtools::load_all(package_dir, quiet = TRUE)
options(FLStockpile.url = stockpile)

stock_slots <- c(
  "catch", "catch.n", "catch.wt",
  "discards", "discards.n", "discards.wt",
  "landings", "landings.n", "landings.wt",
  "stock", "stock.n", "stock.wt",
  "m", "mat",
  "harvest", "harvest.spwn", "m.spwn"
)

# Read a submitted object.
#
# submission_id the submission ID
# file          the file, as in the sources table: inside a zip file it is
#               "<zip file>/<file>"
# object        the name of the object in the file, as in the sources table:
#               "" for an rds file, and "<list>$<name>" inside a list

read_submitted <- function(submission_id, file, object) {
  path <- file.path(extract_dir, submission_id, file)
  if (grepl("\\.zip/", file)) {
    parts <- strsplit(file, "\\.zip/")[[1]]
    zip_dir <- tempfile("zip")
    on.exit(unlink(zip_dir, recursive = TRUE))
    unzip(file.path(extract_dir, submission_id, paste0(parts[1], ".zip")), exdir = zip_dir)
    path <- file.path(zip_dir, parts[2])
  }

  # warnings are about classes from packages that are not installed
  found <- suppressWarnings(try(
    {
      env <- new.env()
      load(path, envir = env)
      as.list(env)
    },
    silent = TRUE
  ))
  if (inherits(found, "try-error")) {
    found <- readRDS(path)
  }

  if (object != "") {
    for (name in strsplit(object, "$", fixed = TRUE)[[1]]) found <- found[[name]]
  }
  found
}

sources <- stockpile_sources(version = "all")
sources <- sources[sources$class == "FLStock", ]

problems <- character()
for (i in seq_len(nrow(sources))) {
  source <- sources[i, ]
  submitted <- read_submitted(source$submission_id, source$file, source$object)
  stored <- stockpile_flstock(source$stock_code, source$assessment_year, source$version, type = source$type)

  problem <- function(...) {
    problems <<- c(problems, paste(source$stock_code, source$assessment_year, "version", source$version, source$type, ...))
  }

  if (!isTRUE(validObject(stored, test = TRUE))) problem("is not a valid FLStock")
  if (!identical(name(submitted), name(stored))) problem("name")
  if (!identical(desc(submitted), desc(stored))) problem("desc")
  if (!isTRUE(all.equal(range(submitted), range(stored)))) problem("range")

  for (slot_name in stock_slots) {
    was <- slot(submitted, slot_name)
    if (dim(was)[6] > 1) was <- iterMedians(was)
    is <- slot(stored, slot_name)
    if (!identical(dimnames(was), dimnames(is))) {
      problem(slot_name, "dimensions")
    } else if (!identical(c(was), c(is))) {
      problem(slot_name, "values")
    } else if (!identical(units(was), units(is)) && !all(is.na(was))) {
      # a slot that is all missing is not stored, so has lost its units
      problem(slot_name, "units")
    }
  }
}

cat(nrow(sources), "FLStock objects checked against", stockpile, "\n")
if (length(problems) > 0) {
  cat(problems, sep = "\n")
  stop(length(problems), " differences from the submitted objects")
}
cat("no differences from the submitted objects\n")
