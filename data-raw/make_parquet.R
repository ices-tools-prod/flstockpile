# Extract the stock assessment results submissions into parquet tables.
#
# Every file below the extract dir is read, in the same way as
# make_inventory.R, and the FLStock and FLIndex objects found are written as
# long tables, with what the submissions page says about the submission they
# came from. Submissions whose notes mark them as one of Adri's tests are
# ignored.
#
# Observed and estimated
#
# A submitted FLStock holds either the observed data or the model estimates.
# Which one is recorded in the types file, one row per object, with type
# "observed", "estimated" or "ignore". An object that is not yet in the types
# file is added to it with a guess, and the basis for the guess; check the
# guesses and correct the file by hand, then run this script again.
#
# Versions
#
# The submissions for a case number are taken in the order they were
# submitted, and make up the versions of that stock and assessment year:
#
# - an observed and an estimated object go into the same version
# - a submission with a new object of a type the version already has starts
#   the next version, which keeps the other type from the version before
# - an identical copy of an object changes nothing, except that the copy from
#   the most recent submission is recorded as the source
#
# Versions are never changed or removed by a later submission, so the data as
# it stood on any date can be found from the submitted date.
#
# Keys to other ICES databases
#
# Each stock assessment is looked up, by stock code and assessment year, in
# the ICES Stock Database (with the icesSD package) for its stock key and
# expert group, and in the Stock Assessment Graphs database, SAG (with the
# icesSAG package) for the key of the published assessment. These are the
# stock_key, expert_group and assessment_key columns of stocks.
#
# Where SAG has more than one published assessment for a stock and year, the
# one to use is taken from the SAG keys file, which is kept by hand. Where
# there is none, or none has been chosen, the key is missing.
#
# Tables, all keyed by stock_code, assessment_year and version
#
#   stocks.parquet   one row per version
#   sources.parquet  the submitted objects each version was made from, with
#                    the name, description and ranges of each
#   values.parquet   the FLStock slots: slot, type, age, year, unit, season,
#                    area, value, units
#   indices.parquet  the FLIndex slots, in the same layout, by index
#
# In values, type is "observed" or "estimated", or "both" where the slot is
# the same in the observed and estimated objects and is stored once. age is
# NA for slots that are totals over ages. Missing values are not stored.
#
# Iterations
#
# Where an object has iterations, which are likely to be simulations from a
# Bayesian posterior, only the median over the iterations is stored, and the
# object is taken to be estimated. The number of iterations it had is in the
# n_iters column of stocks and sources.
#
# FLStock objects
#
# The tables are also written back out as a list of FLStock objects, in
# flstocks.rds in the output dir, made from the parquet tables by the
# FLStockpile package so that the two always agree. There is one element per
# version and type, named "<stock code>_<assessment year>_v<version>_<type>",
# each a list with
#
#   stock            the FLStock object
#   stock_code       the stock code, e.g. "sol.27.7d"
#   assessment_year  the assessment year
#   version          the version of the stock assessment
#   latest           is this the most recent version?
#   type             "observed" or "estimated"
#   case_id          the case number, e.g. "CAS-88837-N1Q8F1"
#   submitted        when it was submitted, as "yyyy-mm-dd hh:mm:ss"
#   submission_id    the submission ID (a GUID)
#   notes            the notes given by the submitter
#   file             the file the object was in
#   object           the name of the object in that file ("" for an rds file)
#
#   stock_key        the key of the stock in the ICES Stock Database
#   expert_group     the expert group that assesses the stock
#   assessment_key   the key of the published assessment in SAG
#
# Usage: Rscript make_parquet.R [html file] [extract dir] [types file] [output dir] [package dir] [SAG keys file]

library(FLCore)
library(rvest)
library(arrow)
library(icesSD)
library(icesSAG)

args <- commandArgs(trailingOnly = TRUE)
html_file <- if (length(args) >= 1) args[1] else "ids.html"
extract_dir <- if (length(args) >= 2) args[2] else "extracted"
types_file <- if (length(args) >= 3) args[3] else "object_types.csv"
output_dir <- if (length(args) >= 4) args[4] else "parquet"
package_dir <- if (length(args) >= 5) args[5] else ".."
sag_keys_file <- if (length(args) >= 6) args[6] else "sag_keys.csv"

stock_slots <- c(
  "catch", "catch.n", "catch.wt",
  "discards", "discards.n", "discards.wt",
  "landings", "landings.n", "landings.wt",
  "stock", "stock.n", "stock.wt",
  "m", "mat",
  "harvest", "harvest.spwn", "m.spwn"
)
index_slots <- c("index", "index.var", "catch.n", "catch.wt", "effort", "sel.pattern", "index.q")

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

# Find the FLStock and FLIndex objects in an object, looking inside lists.
#
# Returns a list of objects, named by where they were found.

find_objects <- function(object, name) {
  if (is_a(object, "FLStock") || is_a(object, "FLIndex")) {
    return(setNames(list(object), name))
  }

  if ((identical(class(object), "list") || is_a(object, "FLlst")) && length(object) > 0) {
    names <- names(object)
    if (is.null(names)) names <- rep("", length(object))
    names <- ifelse(is.na(names) | names == "", paste0("[[", seq_along(object), "]]"), names)
    if (name != "") names <- paste(name, names, sep = "$")
    found <- lapply(seq_along(object), function(i) find_objects(object[[i]], names[i]))
    return(do.call(c, found))
  }

  list()
}

# Find the FLStock and FLIndex objects in a file, and in the files inside it
# if it is a zip.
#
# file  the file to read
# label the file name to use in the tables
#
# Returns a list with one element per object, each a list with the data, the
# file and the name of the object.

file_objects <- function(file, label = basename(file)) {
  if (identical(readBin(file, "raw", 2), charToRaw("PK"))) {
    zip_dir <- tempfile("zip")
    on.exit(unlink(zip_dir, recursive = TRUE))
    files <- unzip(file, exdir = zip_dir)
    labels <- paste(label, substring(files, nchar(zip_dir) + 2), sep = "/")
    found <- lapply(seq_along(files), function(i) file_objects(files[i], labels[i]))
    return(do.call(c, found))
  }

  objects <- read_objects(file)
  found <- do.call(c, lapply(seq_along(objects), function(i) find_objects(objects[[i]], names(objects)[i])))
  lapply(seq_along(found), function(i) list(data = found[[i]], file = label, object = names(found)[i]))
}

# Guess whether an FLStock holds the observed data or the model estimates.
#
# Returns the type and the basis for the guess.

guess_type <- function(stock, object, file) {
  if (dim(stock)[6] > 1) {
    return(c("estimated", "has iterations"))
  }

  if (all(is.na(stock.n(stock)))) {
    return(c("observed", "stock.n is all NA"))
  }

  hints <- c("object name" = object, "file name" = basename(file))
  for (hint in names(hints)) {
    if (grepl("obs|input|stk_dat", hints[hint], ignore.case = TRUE)) {
      return(c("observed", hint))
    }
    if (grepl("est|fit|model", hints[hint], ignore.case = TRUE)) {
      return(c("estimated", hint))
    }
  }

  c("estimated", "default")
}

# The values in an FLQuant as a long table, without the missing values.
#
# Where there are iterations, the values are the medians over them.

quant_table <- function(quant) {
  if (dim(quant)[6] > 1) {
    quant <- iterMedians(quant)
  }
  values <- as.data.frame(quant)
  ages <- as.character(values[[1]])
  if (any(ages != "all" & is.na(suppressWarnings(as.integer(ages))))) {
    stop("ages are not whole numbers: ", paste(unique(ages), collapse = " "))
  }
  values <- data.frame(
    age = suppressWarnings(as.integer(ages)),
    year = as.integer(as.character(values$year)),
    unit = as.character(values$unit),
    season = as.character(values$season),
    area = as.character(values$area),
    value = values$data,
    units = units(quant)
  )
  values[!is.na(values$value), ]
}

# The name, description, ranges and dimensions of an FLStock or FLIndex.
#
# Returns a data frame with one row.

object_info <- function(object) {
  range <- range(object)
  in_range <- function(name) if (name %in% names(range)) as.integer(range[name]) else NA_integer_
  data.frame(
    name = name(object),
    desc = desc(object),
    quant = names(dimnames(object@catch.n))[1],
    min_age = in_range("min"), max_age = in_range("max"), plusgroup = in_range("plusgroup"),
    min_year = in_range("minyear"), max_year = in_range("maxyear"),
    min_fbar = in_range("minfbar"), max_fbar = in_range("maxfbar"),
    n_units = dim(object@catch.n)[3], n_seasons = dim(object@catch.n)[4],
    n_areas = dim(object@catch.n)[5], n_iters = dim(object@catch.n)[6]
  )
}

# the submissions table, from the saved copy of
# https://data.ices.dk/submissions/ViewSubmissions/StockAssessmentResultsSubmissions
# the time zone of the submission times is not stated, they are kept as given
submissions <- html_table(html_element(read_html(html_file), "table"))
names(submissions) <- trimws(names(submissions))
submissions <- data.frame(
  submission_id = submissions$ID,
  submitted = as.POSIXct(submissions$`Submission datetime`, format = "%d-%m-%Y %H:%M:%S", tz = "UTC"),
  stock_code = submissions$StockCode,
  assessment_year = submissions$Year,
  notes = trimws(gsub("\\s+", " ", submissions$Notes)),
  case_id = submissions$`Case Number`
)
submissions <- submissions[!duplicated(submissions$submission_id), ]

# ignore Adri's tests
is_test <- grepl("adri", submissions$notes, ignore.case = TRUE)
cat(sum(is_test), "of", nrow(submissions), "submissions ignored as tests\n")
submissions <- submissions[!is_test, ]

# oldest first, so that versions are made in the order of submission
submissions <- submissions[order(submissions$stock_code, submissions$assessment_year, submissions$submitted), ]

# 1. read the objects in each submission

objects <- list()
for (i in seq_len(nrow(submissions))) {
  files <- list.files(file.path(extract_dir, submissions$submission_id[i]), recursive = TRUE, full.names = TRUE)
  for (found in do.call(c, lapply(files, file_objects))) {
    found$submission <- i
    found$class <- if (is_a(found$data, "FLStock")) "FLStock" else "FLIndex"
    objects[[length(objects) + 1]] <- found
  }
}

# 2. the type of each object, from the types file

key_of <- function(submission_id, file, object) paste(submission_id, file, object, sep = "|")

types <- if (file.exists(types_file)) {
  read.csv(types_file, colClasses = "character")
} else {
  data.frame()
}
known <- key_of(types$submission_id, types$file, types$object)

guessed <- lapply(objects, function(found) {
  submission <- submissions[found$submission, ]
  if (key_of(submission$submission_id, found$file, found$object) %in% known) {
    return(NULL)
  }
  guess <- if (found$class == "FLStock") guess_type(found$data, found$object, found$file) else c("observed", "survey index")
  data.frame(
    stock_code = submission$stock_code,
    assessment_year = as.character(submission$assessment_year),
    case_id = submission$case_id,
    submitted = format(submission$submitted, "%Y-%m-%d %H:%M:%S"),
    submission_id = submission$submission_id,
    file = found$file,
    object = found$object,
    class = found$class,
    type = guess[1],
    basis = paste("guess:", guess[2])
  )
})
guessed <- do.call(rbind, guessed)

if (!is.null(guessed)) {
  types <- rbind(types, guessed)
  write.csv(types, types_file, row.names = FALSE)
  cat(nrow(guessed), "objects added to", types_file, "with a guessed type, check them\n")
}

for (i in seq_along(objects)) {
  submission_id <- submissions$submission_id[objects[[i]]$submission]
  objects[[i]]$type <- types$type[match(key_of(submission_id, objects[[i]]$file, objects[[i]]$object), key_of(types$submission_id, types$file, types$object))]
}
bad <- setdiff(vapply(objects, function(x) x$type, ""), c("observed", "estimated", "ignore"))
if (length(bad) > 0) {
  stop("type in ", types_file, " must be observed, estimated or ignore, not: ", paste(bad, collapse = ", "))
}

# 3. make the versions

# a version is a list with the stock code, assessment year, version and case
# number, the observed and estimated objects (or NULL), and a list of indices
versions <- list()
current <- NULL

for (i in seq_len(nrow(submissions))) {
  submission <- submissions[i, ]
  mine <- Filter(function(x) x$submission == i && x$type != "ignore", objects)
  stocks <- Filter(function(x) x$class == "FLStock", mine)
  indices <- Filter(function(x) x$class == "FLIndex", mine)
  if (length(mine) == 0) {
    next
  }

  stock_types <- vapply(stocks, function(x) x$type, "")
  if (any(duplicated(stock_types))) {
    stop(
      "submission ", submission$submission_id, " (", submission$stock_code, ") has more than one ",
      stock_types[duplicated(stock_types)][1], " FLStock, set all but one to ignore in ", types_file
    )
  }

  same_case <- !is.null(current) && current$case_id == submission$case_id &&
    current$stock_code == submission$stock_code && current$assessment_year == submission$assessment_year
  bad <- vapply(stocks, function(x) x$type == "observed" && dim(x$data)[6] > 1, TRUE)
  if (any(bad)) {
    stop(
      "submission ", submission$submission_id, " (", submission$stock_code, ") has an observed FLStock with ",
      "iterations, set it to estimated in ", types_file
    )
  }

  is_copy <- vapply(stocks, function(x) same_case && identical(x$data, current[[x$type]]$data), TRUE)
  replaces <- vapply(stocks, function(x) same_case && !is.null(current[[x$type]]), TRUE) & !is_copy

  if (!same_case || any(replaces)) {
    same_stock <- !is.null(current) && current$stock_code == submission$stock_code &&
      current$assessment_year == submission$assessment_year
    if (!is.null(current)) versions[[length(versions) + 1]] <- current
    current <- list(
      stock_code = submission$stock_code,
      assessment_year = submission$assessment_year,
      version = if (same_stock) current$version + 1L else 1L,
      case_id = submission$case_id,
      observed = if (same_case) current$observed,
      estimated = if (same_case) current$estimated,
      indices = if (same_case) current$indices else list()
    )
  }

  for (stock in stocks) current[[stock$type]] <- stock
  if (length(indices) > 0) current$indices <- indices
}
versions[[length(versions) + 1]] <- current

# 4. make the tables

version_key <- function(version) {
  data.frame(stock_code = version$stock_code, assessment_year = version$assessment_year, version = version$version)
}

version_sources <- function(version) {
  c(Filter(Negate(is.null), list(version$observed, version$estimated)), version$indices)
}

stocks <- lapply(versions, function(version) {
  # described by the estimated object, or the observed one if that is all there is
  sources <- version_sources(version)
  cbind(version_key(version),
    latest = NA,
    case_id = version$case_id,
    submitted = max(submissions$submitted[vapply(sources, function(x) x$submission, 1L)]),
    observed = !is.null(version$observed),
    estimated = !is.null(version$estimated),
    indices = length(version$indices),
    object_info(sources[[if (is.null(version$estimated)) 1 else 1 + !is.null(version$observed)]]$data)
  )
})
stocks <- do.call(rbind, stocks)
stocks$latest <- stocks$version == ave(stocks$version, stocks$stock_code, stocks$assessment_year, FUN = max)

# the keys to the stock database and to SAG, and the expert group
sag_keys <- read.csv(sag_keys_file)
keys <- lapply(unique(stocks$assessment_year), function(year) {
  stock_database <- suppressMessages(getSD(year = year))
  published <- suppressMessages(getListStocks(year))
  stock_codes <- unique(stocks$stock_code[stocks$assessment_year == year])

  assessment_keys <- vapply(stock_codes, function(stock_code) {
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
      return(NA_integer_)
    }
    as.integer(found)
  }, 1L)

  in_database <- match(stock_codes, stock_database$StockKeyLabel)
  data.frame(
    stock_code = stock_codes,
    assessment_year = year,
    stock_key = as.integer(stock_database$StockKey[in_database]),
    expert_group = stock_database$ExpertGroup[in_database],
    assessment_key = assessment_keys
  )
})
keys <- do.call(rbind, keys)
keys <- keys[match(paste(stocks$stock_code, stocks$assessment_year), paste(keys$stock_code, keys$assessment_year)), ]
stocks <- cbind(
  stocks[1:5],
  keys[c("stock_key", "expert_group", "assessment_key")],
  stocks[-(1:5)]
)
rownames(stocks) <- NULL

sources <- lapply(versions, function(version) {
  rows <- lapply(version_sources(version), function(source) {
    submission <- submissions[source$submission, ]
    cbind(version_key(version),
      type = source$type,
      class = source$class,
      submission_id = submission$submission_id,
      submitted = submission$submitted,
      file = source$file,
      object = source$object,
      notes = submission$notes,
      object_info(source$data)
    )
  })
  do.call(rbind, rows)
})
sources <- do.call(rbind, sources)

values <- lapply(versions, function(version) {
  rows <- lapply(stock_slots, function(slot_name) {
    observed <- if (!is.null(version$observed)) slot(version$observed$data, slot_name)
    estimated <- if (!is.null(version$estimated)) slot(version$estimated$data, slot_name)
    # a slot that is the same in both objects is stored once
    quants <- if (!is.null(observed) && identical(observed, estimated)) {
      list(both = estimated)
    } else {
      Filter(Negate(is.null), list(observed = observed, estimated = estimated))
    }
    rows <- lapply(names(quants), function(type) {
      table <- quant_table(quants[[type]])
      if (nrow(table) > 0) cbind(version_key(version), slot = slot_name, type = type, table)
    })
    do.call(rbind, rows)
  })
  do.call(rbind, rows)
})
values <- do.call(rbind, values)

indices <- lapply(versions, function(version) {
  rows <- lapply(version$indices, function(source) {
    index <- source$data
    rows <- lapply(index_slots, function(slot_name) {
      table <- quant_table(slot(index, slot_name))
      if (nrow(table) > 0) {
        cbind(version_key(version),
          index = name(index), slot = slot_name, type = source$type, table,
          startf = unname(range(index)["startf"]), endf = unname(range(index)["endf"])
        )
      }
    })
    do.call(rbind, rows)
  })
  do.call(rbind, rows)
})
indices <- do.call(rbind, indices)

# 5. write the tables

dir.create(output_dir, showWarnings = FALSE)
tables <- list(stocks = stocks, sources = sources, values = values, indices = indices)
for (name in names(tables)) {
  file <- file.path(output_dir, paste0(name, ".parquet"))
  write_parquet(tables[[name]], file)
  cat(format(nrow(tables[[name]]), big.mark = ","), " rows written to ", file, "\n", sep = "")
}
cat(
  nrow(stocks), "versions of", nrow(unique(stocks[c("stock_code", "assessment_year")])),
  "stock assessments, from", length(unique(sources$submission_id)), "submissions\n"
)

# 6. write the list of FLStock objects, made from the tables

devtools::load_all(package_dir, quiet = TRUE)
options(FLStockpile.url = output_dir)

stock_sources <- stockpile_sources(version = "all", refresh = TRUE)
stock_sources <- stock_sources[stock_sources$class == "FLStock", ]
stock_sources <- merge(
  stock_sources,
  stockpile_stocks(version = "all")[c(
    "stock_code", "assessment_year", "version", "latest", "case_id", "stock_key", "expert_group", "assessment_key"
  )]
)
stock_sources <- stock_sources[order(stock_sources$stock_code, stock_sources$assessment_year, stock_sources$version, stock_sources$type), ]

flstocks <- lapply(seq_len(nrow(stock_sources)), function(i) {
  source <- stock_sources[i, ]
  list(
    stock = stockpile_flstock(source$stock_code, source$assessment_year, source$version, type = source$type, refresh = i == 1),
    stock_code = source$stock_code,
    assessment_year = source$assessment_year,
    version = source$version,
    latest = source$latest,
    type = source$type,
    case_id = source$case_id,
    submitted = format(source$submitted, "%Y-%m-%d %H:%M:%S"),
    submission_id = source$submission_id,
    notes = source$notes,
    file = source$file,
    object = source$object,
    stock_key = source$stock_key,
    expert_group = source$expert_group,
    assessment_key = source$assessment_key
  )
})
names(flstocks) <- paste0(
  stock_sources$stock_code, "_", stock_sources$assessment_year, "_v", stock_sources$version, "_", stock_sources$type
)

flstocks_file <- file.path(output_dir, "flstocks.rds")
saveRDS(flstocks, flstocks_file)
cat(length(flstocks), "FLStock objects written to", flstocks_file, "\n")
