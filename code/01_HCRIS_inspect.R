source("code/00_setup_file.R")

cat("Project root:\n")
print(here())

cat("\nFiles in source:\n")
source_files <- list.files(source_dir, recursive = TRUE, full.names = TRUE)
print(source_files)

cat("\nFiles in reference docs:\n")
ref_files <- list.files(ref_dir, recursive = TRUE, full.names = TRUE)
print(ref_files)

cat("\nSource file information:\n")
if (length(source_files) > 0) {
  print(file.info(source_files)[, c("size", "isdir")])
}

###going to print the first few lines of each of the source files##

find_first_match <- function(pattern, files) {
  matches <- files[grepl(pattern, basename(files), ignore.case = TRUE)]
  if (length(matches) == 0) return(NA_character_)
  matches[1]
}

rpt_file   <- find_first_match("RPT", source_files)
nmrc_file  <- find_first_match("NMRC", source_files)
alpha_file <- find_first_match("ALPHA", source_files)

cat("\nRPT file:\n")
print(rpt_file)
if (!is.na(rpt_file)) print(readLines(rpt_file, n = 5))

cat("\nNMRC file:\n")
print(nmrc_file)
if (!is.na(nmrc_file)) print(readLines(nmrc_file, n = 5))

cat("\nALPHA file:\n")
print(alpha_file)
if (!is.na(alpha_file)) print(readLines(alpha_file, n = 5))