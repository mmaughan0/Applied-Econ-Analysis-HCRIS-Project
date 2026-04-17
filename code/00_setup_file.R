##if necessary install.packages("data.table") install.packages("here")##

library(data.table)
library(here)

list.files(here("source"), recursive = TRUE)
list.files(here("reference docs"), recursive = TRUE)

library(data.table)
library(here)

# Define main project directories
source_dir <- here("source")
ref_dir <- here("reference docs")
intermediate_dir <- here("intermediate")
output_dir <- here("output")
code_dir <- here("code")

# Create key folders if they do not already exist
dir.create(intermediate_dir, showWarnings = FALSE)
dir.create(output_dir, showWarnings = FALSE)

# Print a quick project check
cat("Project root:", here(), "\n")
cat("Source dir:", source_dir, "\n")
cat("Reference docs dir:", ref_dir, "\n")
cat("Intermediate dir:", intermediate_dir, "\n")
cat("Output dir:", output_dir, "\n")