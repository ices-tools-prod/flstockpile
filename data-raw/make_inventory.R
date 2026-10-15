# Make an inventory of the R objects in the extracted submissions.
#
# Every file below the extract dir is read, and each object found in it gets
# one row in the inventory. Lists and FLR lists (FLStocks, FLIndices) are
# opened up, so an FLStock inside a list gets its own row.
#
# The file type is worked out from the contents, not the extension, because
# some R data files were submitted as .txt, .data or with no extension:
#
# - R data files, as saved by save() or saveRDS()
# - tables in delimited text files (.csv, .txt)
# - zip files inside a submission are unzipped and their files read
#
# Files that are not data (R scripts, R Markdown, ...) are skipped.
#
# Usage: Rscript make_inventory.R [extract dir] [inventory file]

library(FLCore)

args <- commandArgs(trailingOnly = TRUE)
extract_dir <- if (length(args) >= 1) args[1] else "extracted"
inventory_file <- if (length(args) >= 2) args[2] else "inventory.csv"

# Read the objects in a file.
#
# Returns a named list of objects, or NULL if the file is not data. The
# "format" attribute says how the file was read.

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
  format <- "RData"

  if (inherits(objects, "try-error")) {
    objects <- suppressWarnings(try(list(readRDS(file)), silent = TRUE))
    names(objects) <- ""
    format <- "rds"
  }

  if (inherits(objects, "try-error") && grepl("\\.(csv|txt)$", file, ignore.case = TRUE)) {
    objects <- try(list(read.csv(file, check.names = FALSE)), silent = TRUE)
    names(objects) <- ""
    format <- "csv"
  }

  if (inherits(objects, "try-error")) {
    return(NULL)
  }

  attr(objects, "format") <- format
  objects
}

# is() gives an error for classes from packages that are not installed
is_a <- function(object, class) {
  isTRUE(try(is(object, class), silent = TRUE))
}

# dim() gives an error for the same reason
dim_of <- function(object) {
  dim <- try(dim(object), silent = TRUE)
  if (inherits(dim, "try-error")) NULL else dim
}

# Describe an object, and the objects inside it if it is a list.
#
# Returns a data frame with one row per object.

describe_object <- function(object, name) {
  row <- data.frame(
    object = name,
    class = class(object)[1],
    dim = paste(dim_of(object), collapse = "x"),
    name = NA, desc = NA,
    min_age = NA, max_age = NA, plusgroup = NA,
    min_year = NA, max_year = NA,
    min_fbar = NA, max_fbar = NA
  )

  if (is_a(object, "FLStock")) {
    range <- range(object)
    row$name <- name(object)
    row$desc <- desc(object)
    row$min_age <- range["min"]
    row$max_age <- range["max"]
    row$plusgroup <- range["plusgroup"]
    row$min_year <- range["minyear"]
    row$max_year <- range["maxyear"]
    row$min_fbar <- range["minfbar"]
    row$max_fbar <- range["maxfbar"]
  }

  # open up plain lists and FLR lists, but not fitted model objects
  if ((identical(class(object), "list") || is_a(object, "FLlst")) && length(object) > 0) {
    names <- names(object)
    if (is.null(names)) names <- rep("", length(object))
    names <- ifelse(is.na(names) | names == "", paste0("[[", seq_along(object), "]]"), names)
    if (name != "") names <- paste(name, names, sep = "$")
    rows <- lapply(seq_along(object), function(i) describe_object(object[[i]], names[i]))
    row <- do.call(rbind, c(list(row), rows))
  }

  row
}

# Describe the objects in a file, and in the files inside it if it is a zip.
#
# file  the file to read
# label the file name to use in the inventory
#
# Returns a data frame with one row per object, or NULL if the file is not
# data.

describe_file <- function(file, label = basename(file)) {
  if (identical(readBin(file, "raw", 2), charToRaw("PK"))) {
    zip_dir <- tempfile("zip")
    on.exit(unlink(zip_dir, recursive = TRUE))
    files <- unzip(file, exdir = zip_dir)
    labels <- paste(label, substring(files, nchar(zip_dir) + 2), sep = "/")
    rows <- lapply(seq_along(files), function(i) describe_file(files[i], labels[i]))
    return(do.call(rbind, rows))
  }

  objects <- read_objects(file)
  if (is.null(objects)) {
    cat("SKIPPED ", label, "\n", sep = "")
    return(NULL)
  }

  rows <- lapply(seq_along(objects), function(i) describe_object(objects[[i]], names(objects)[i]))
  cbind(
    file = label,
    format = attr(objects, "format"),
    size = file.size(file),
    md5 = unname(tools::md5sum(file)),
    do.call(rbind, rows)
  )
}

ids <- list.files(extract_dir)

inventory <- list()
for (id in ids) {
  files <- list.files(file.path(extract_dir, id), recursive = TRUE, full.names = TRUE)
  rows <- do.call(rbind, lapply(files, describe_file))
  if (!is.null(rows)) {
    inventory[[id]] <- cbind(id = id, rows)
  }
}
inventory <- do.call(rbind, inventory)

write.csv(inventory, inventory_file, row.names = FALSE, na = "")
cat(
  nrow(inventory), "objects from", length(unique(inventory$id)), "of", length(ids),
  "submissions written to", inventory_file, "\n"
)
