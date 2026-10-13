# Make a list of the unique FLStock objects in the extracted submissions.
#
# Every file below the extract dir is read, in the same way as
# make_inventory.R, and the FLStock objects found are collected with what the
# submissions page says about the submission they came from.
#
# - submissions whose notes mark them as one of Adri's tests are ignored
# - where the same FLStock object was submitted more than once, only the copy
#   from the most recent submission is kept
#
# Each element of the list is a list with
#
#   stock          the FLStock object
#   stock_code     the stock code, e.g. "sol.27.7d"
#   year           the assessment year
#   notes          the notes given by the submitter
#   submitted      when it was submitted, as "yyyy-mm-dd hh:mm:ss"
#   submission_id  the submission ID (a GUID)
#   case_id        the case number, e.g. "CAS-88837-N1Q8F1"
#   file           the file the object was in
#   object         the name of the object in that file ("" for an rds file)
#
# so that it can be passed on to flstock_to_json(), for example
#
#   x <- flstocks[[1]]
#   flstock_to_json(x$stock, stock_id = x$stock_code, version = x$submission_id, source = x$file)
#
# Usage: Rscript make_flstocks.R [html file] [extract dir] [output file]

library(FLCore)
library(rvest)

args <- commandArgs(trailingOnly = TRUE)
html_file <- if (length(args) >= 1) args[1] else "ids.html"
extract_dir <- if (length(args) >= 2) args[2] else "extracted"
flstocks_file <- if (length(args) >= 3) args[3] else "flstocks.rds"

# Read the objects in a file.
#
# Returns a named list of objects, or NULL if the file is not an R data file.

read_objects <- function(file) {
  # warnings are about classes from packages that are not installed
  objects <- suppressWarnings(try(
    {
      env <- new.env()
      load(file, envir = env)
      as.list(env)
    },
    silent = TRUE
  ))

  if (inherits(objects, "try-error")) {
    objects <- suppressWarnings(try(list(readRDS(file)), silent = TRUE))
    names(objects) <- ""
  }

  if (inherits(objects, "try-error")) NULL else objects
}

# is() gives an error for classes from packages that are not installed
is_a <- function(object, class) {
  isTRUE(try(is(object, class), silent = TRUE))
}

# Find the FLStock objects in an object, looking inside lists.
#
# Returns a list of FLStock objects, named by where they were found.

find_flstocks <- function(object, name) {
  if (is_a(object, "FLStock")) {
    return(setNames(list(object), name))
  }

  if ((identical(class(object), "list") || is_a(object, "FLlst")) && length(object) > 0) {
    names <- names(object)
    if (is.null(names)) names <- rep("", length(object))
    names <- ifelse(is.na(names) | names == "", paste0("[[", seq_along(object), "]]"), names)
    if (name != "") names <- paste(name, names, sep = "$")
    found <- lapply(seq_along(object), function(i) find_flstocks(object[[i]], names[i]))
    return(do.call(c, found))
  }

  list()
}

# Find the FLStock objects in a file, and in the files inside it if it is a
# zip.
#
# file  the file to read
# label the file name to use in the list
#
# Returns a list with one element per FLStock object, each a list with the
# stock, the file and the name of the object.

file_flstocks <- function(file, label = basename(file)) {
  if (identical(readBin(file, "raw", 2), charToRaw("PK"))) {
    zip_dir <- tempfile("zip")
    on.exit(unlink(zip_dir, recursive = TRUE))
    files <- unzip(file, exdir = zip_dir)
    labels <- paste(label, substring(files, nchar(zip_dir) + 2), sep = "/")
    found <- lapply(seq_along(files), function(i) file_flstocks(files[i], labels[i]))
    return(do.call(c, found))
  }

  objects <- read_objects(file)
  stocks <- do.call(c, lapply(seq_along(objects), function(i) find_flstocks(objects[[i]], names(objects)[i])))
  lapply(seq_along(stocks), function(i) list(stock = stocks[[i]], file = label, object = names(stocks)[i]))
}

# the submissions table, from the saved copy of
# https://data.ices.dk/submissions/ViewSubmissions/StockAssessmentResultsSubmissions
submissions <- html_table(html_element(read_html(html_file), "table"))
names(submissions) <- trimws(names(submissions))
submissions <- data.frame(
  submission_id = submissions$ID,
  submitted = as.POSIXct(submissions$`Submission datetime`, format = "%d-%m-%Y %H:%M:%S", tz = "UTC"),
  stock_code = submissions$StockCode,
  year = submissions$Year,
  notes = trimws(gsub("\\s+", " ", submissions$Notes)),
  case_id = submissions$`Case Number`
)
submissions <- submissions[!duplicated(submissions$submission_id), ]

# ignore Adri's tests
is_test <- grepl("adri", submissions$notes, ignore.case = TRUE)
cat(sum(is_test), "of", nrow(submissions), "submissions ignored as tests\n")
submissions <- submissions[!is_test, ]

# most recent first, so that the first copy of an object is the one to keep
submissions <- submissions[order(submissions$submitted, decreasing = TRUE), ]

flstocks <- list()
for (i in seq_len(nrow(submissions))) {
  files <- list.files(file.path(extract_dir, submissions$submission_id[i]), recursive = TRUE, full.names = TRUE)
  for (found in do.call(c, lapply(files, file_flstocks))) {
    # the same object in a more recent submission
    copy_of <- Position(function(x) identical(x$stock, found$stock), flstocks)
    if (!is.na(copy_of)) {
      cat(
        "COPY ", submissions$stock_code[i], " ", submissions$submission_id[i], " ", found$file, " ", found$object,
        "\n  kept from ", flstocks[[copy_of]]$submission_id, " ", flstocks[[copy_of]]$file, " ", flstocks[[copy_of]]$object, "\n",
        sep = ""
      )
      next
    }

    flstocks[[length(flstocks) + 1]] <- list(
      stock = found$stock,
      stock_code = submissions$stock_code[i],
      year = submissions$year[i],
      notes = submissions$notes[i],
      submitted = format(submissions$submitted[i], "%Y-%m-%d %H:%M:%S"),
      submission_id = submissions$submission_id[i],
      case_id = submissions$case_id[i],
      file = found$file,
      object = found$object
    )
  }
}

# by stock code, then most recent first
stock_codes <- vapply(flstocks, function(x) x$stock_code, "")
flstocks <- flstocks[order(stock_codes)]
names(flstocks) <- make.unique(sort(stock_codes), sep = "_")

saveRDS(flstocks, flstocks_file)
cat(
  length(flstocks), "FLStock objects for", length(unique(stock_codes)), "stocks from",
  length(unique(vapply(flstocks, function(x) x$submission_id, ""))), "submissions written to", flstocks_file, "\n"
)
