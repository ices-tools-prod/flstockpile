# Check the parquet tables against the submitted objects.
#
# Every FLStock that a version was made from is read again from the extract
# dir, and compared with the FLStock that the flstockpile package makes from
# the parquet tables: the values, dimensions and units of every slot, and the
# ranges, name and description. Where the submitted object has iterations, it
# is the medians over the iterations that are compared. The numbers and
# weights are stored in thousands and kg, so the submitted values are put in
# those units before they are compared.
#
# The dataset can be a folder or a web address, so this also checks a
# published copy of the tables.
#
# Usage: Rscript check_parquet.R [dataset] [extract dir] [package dir]

library(FLCore)

args <- commandArgs(trailingOnly = TRUE)
dataset <- if (length(args) >= 1) args[1] else "parquet"
extract_dir <- if (length(args) >= 2) args[2] else "extracted"
package_dir <- if (length(args) >= 3) args[3] else ".."

devtools::load_all(package_dir, quiet = TRUE)
options(flstockpile.url = dataset)

stock_slots <- c(
  "catch", "catch.n", "catch.wt",
  "discards", "discards.n", "discards.wt",
  "landings", "landings.n", "landings.wt",
  "stock", "stock.n", "stock.wt",
  "m", "mat",
  "harvest", "harvest.spwn", "m.spwn"
)

number_slots <- c("catch.n", "landings.n", "discards.n", "stock.n")
weight_slots <- c("catch.wt", "landings.wt", "discards.wt", "stock.wt")

# What to multiply the values of a slot by to put them in the standard units,
# thousands for numbers and kg for weights, from the units given by the
# submitter. Returns NA where no units were given, or they are not units of
# numbers or weights that are known.

unit_multiplier <- function(units, slot_name) {
  if (units %in% c("NA", "")) {
    return(NA_real_)
  }
  if (slot_name %in% number_slots) {
    named <- c(thousands = 1e3, millions = 1e6)
    numbers <- if (grepl("^10\\^[0-9]+$", units)) {
      10^as.numeric(sub("10^", "", units, fixed = TRUE))
    } else if (grepl("^[0-9.]+(e[0-9]+)?$", units)) {
      as.numeric(units)
    } else {
      unname(named[units])
    }
    multiplier <- numbers / 1000
  } else {
    multiplier <- unname(c(kg = 1, g = 0.001, t = 1000, tonnes = 1000)[units])
  }
  multiplier
}

# Put the numbers of an FLStock in thousands and its weights in kg.
#
# Where no units were given, or they are not known, the values are taken to
# be in thousands and kg already. Returns the FLStock, with the slots this was
# done for, and the units they had, in the "units_assumed" attribute.

standard_units <- function(stock) {
  assumed <- character()
  for (slot_name in c(number_slots, weight_slots)) {
    quant <- slot(stock, slot_name)
    multiplier <- unit_multiplier(units(quant), slot_name)
    if (is.na(multiplier)) {
      if (!all(is.na(quant))) assumed <- c(assumed, paste0(slot_name, " (", units(quant), ")"))
    } else if (multiplier != 1) {
      quant <- quant * multiplier
    }
    units(quant) <- if (slot_name %in% number_slots) "1000" else "kg"
    slot(stock, slot_name) <- quant
  }
  attr(stock, "units_assumed") <- assumed
  stock
}

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
  submitted <- standard_units(read_submitted(source$submission_id, source$file, source$object))
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
    } else if (!isTRUE(all.equal(c(was), c(is), tolerance = 1e-12))) {
      problem(slot_name, "values")
    } else if (!identical(units(was), units(is)) && !all(is.na(was))) {
      # a slot that is all missing is not stored, so has lost its units
      problem(slot_name, "units")
    }
  }
}

cat(nrow(sources), "FLStock objects checked against", dataset, "\n")
if (length(problems) > 0) {
  cat(problems, sep = "\n")
  stop(length(problems), " differences from the submitted objects")
}
cat("no differences from the submitted objects\n")
