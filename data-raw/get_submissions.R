# Get the stock assessment results submissions from the ICES file archive:
#
# 1. extract the submission IDs from a saved copy of
#    https://data.ices.dk/submissions/ViewSubmissions/StockAssessmentResultsSubmissions
# 2. write the IDs to a text file, one per line
# 3. download the zip file for each ID
# 4. extract each zip file into its own folder
#
# Usage: Rscript get_submissions.R [html file] [ids file] [zip dir] [extract dir]

args <- commandArgs(trailingOnly = TRUE)
html_file <- if (length(args) >= 1) args[1] else "ids.html"
ids_file <- if (length(args) >= 2) args[2] else "ids.txt"
zip_dir <- if (length(args) >= 3) args[3] else "zip"
extract_dir <- if (length(args) >= 4) args[4] else "extracted"

# Download a stock assessment results submission from the ICES file archive.
#
# id       the submission ID (a GUID)
# destfile where to save the zip file
#
# Returns the path to the zip file, invisibly.

download_submission <- function(id, destfile = paste0(id, ".zip")) {
  url <- paste0("https://data.ices.dk/filearchive/GetFileByFileID/", id)
  download.file(url, destfile, mode = "wb", quiet = TRUE)

  # the server can answer with a text message instead of a file
  if (!identical(readBin(destfile, "raw", 2), charToRaw("PK"))) {
    stop("not a zip file: ", paste(readLines(destfile, n = 1, warn = FALSE), collapse = ""))
  }

  invisible(destfile)
}

# 1. extract the IDs

html <- paste(readLines(html_file, warn = FALSE), collapse = "\n")

# the IDs are in links ending handler=Download&amp;id=<GUID>
pattern <- "handler=Download&(amp;)?id=[0-9a-fA-F]{8}(-[0-9a-fA-F]{4}){3}-[0-9a-fA-F]{12}"
links <- regmatches(html, gregexpr(pattern, html))[[1]]
ids <- unique(sub(".*id=", "", links))

# 2. write the IDs

writeLines(ids, ids_file)
cat(length(ids), "ids written to", ids_file, "\n")

# 3. download the zip files, one per ID

dir.create(zip_dir, showWarnings = FALSE)

failed <- character()
for (id in ids) {
  destfile <- file.path(zip_dir, paste0(id, ".zip"))
  result <- try(suppressWarnings(download_submission(id, destfile)), silent = TRUE)
  if (inherits(result, "try-error")) {
    failed <- c(failed, id)
    unlink(destfile)
    cat("FAILED ", id, ": ", conditionMessage(attr(result, "condition")), "\n", sep = "")
  }
}

cat(length(ids) - length(failed), "of", length(ids), "downloaded to", zip_dir, "\n")

# 4. extract the zip files, one folder per ID

dir.create(extract_dir, showWarnings = FALSE)

downloaded <- setdiff(ids, failed)
not_extracted <- character()
for (id in downloaded) {
  zipfile <- file.path(zip_dir, paste0(id, ".zip"))
  result <- try(unzip(zipfile, exdir = file.path(extract_dir, id)), silent = TRUE)
  if (inherits(result, "try-error") || length(result) == 0) {
    not_extracted <- c(not_extracted, id)
    cat("NOT EXTRACTED ", id, "\n", sep = "")
  }
}

cat(length(downloaded) - length(not_extracted), "of", length(downloaded), "extracted to", extract_dir, "\n")
