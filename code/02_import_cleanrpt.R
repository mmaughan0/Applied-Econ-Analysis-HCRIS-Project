## normalized rpt table, plus nmrc and alpha tables
source("code/00_setup_file.R")

library(data.table)

## ---------------------------
## file paths
## ---------------------------
rpt_file   <- file.path(source_dir, "HOSP10_2022_rpt.csv")
nmrc_file  <- file.path(source_dir, "HOSP10_2022_nmrc.csv")
alpha_file <- file.path(source_dir, "HOSP10_2022_alpha.csv")

## ---------------------------
## official column names
## ---------------------------
rpt_names <- c(
  "RPT_REC_NUM",
  "PRVDR_CTRL_TYPE_CD",
  "PRVDR_NUM",
  "NPI",
  "RPT_STUS_CD",
  "FY_BGN_DT",
  "FY_END_DT",
  "PROC_DT",
  "INITIAL_RPT_SW",
  "LAST_RPT_SW",
  "TRNSMTL_NUM",
  "FI_NUM",
  "ADR_VNDR_CD",
  "FI_CREAT_DT",
  "UTIL_CD",
  "NPR_DT",
  "SPEC_IND",
  "FI_RCPT_DT"
)

nmrc_names <- c(
  "RPT_REC_NUM",
  "WKSHT_CD",
  "LINE_NUM",
  "CLMN_NUM",
  "ITM_VAL_NUM"
)

alpha_names <- c(
  "RPT_REC_NUM",
  "WKSHT_CD",
  "LINE_NUM",
  "CLMN_NUM",
  "ALPHNMRC_ITM_TXT"
)

## ---------------------------
## import raw files with no headers
## ---------------------------
rpt   <- fread(rpt_file, header = FALSE)
nmrc  <- fread(nmrc_file, header = FALSE)
alpha <- fread(alpha_file, header = FALSE)

## ---------------------------
## quick checks
## ---------------------------
cat("RPT columns in raw file:   ", ncol(rpt),   "\n")
cat("RPT columns expected:      ", length(rpt_names), "\n\n")

cat("NMRC columns in raw file:  ", ncol(nmrc),  "\n")
cat("NMRC columns expected:     ", length(nmrc_names), "\n\n")

cat("ALPHA columns in raw file: ", ncol(alpha), "\n")
cat("ALPHA columns expected:    ", length(alpha_names), "\n\n")

stopifnot(ncol(rpt)   == length(rpt_names))
stopifnot(ncol(nmrc)  == length(nmrc_names))
stopifnot(ncol(alpha) == length(alpha_names))

## ---------------------------
## assign names
## ---------------------------
setnames(rpt, rpt_names)
setnames(nmrc, nmrc_names)
setnames(alpha, alpha_names)

## ---------------------------
## type conversions: rpt
## ---------------------------
date_vars <- c("FI_CREAT_DT", "FI_RCPT_DT", "FY_BGN_DT", "FY_END_DT", "NPR_DT", "PROC_DT")
for (v in date_vars) {
  rpt[, (v) := as.Date(get(v), format = "%m/%d/%Y")]
}

rpt[, PRVDR_NUM := as.character(PRVDR_NUM)]
rpt[, RPT_REC_NUM := as.numeric(RPT_REC_NUM)]

## ---------------------------
## type conversions: nmrc
## ---------------------------
nmrc[, RPT_REC_NUM := as.numeric(RPT_REC_NUM)]
nmrc[, WKSHT_CD := as.character(WKSHT_CD)]
nmrc[, LINE_NUM := as.character(LINE_NUM)]
nmrc[, CLMN_NUM := as.character(CLMN_NUM)]
nmrc[, ITM_VAL_NUM := as.numeric(ITM_VAL_NUM)]

## ---------------------------
## type conversions: alpha
## ---------------------------
alpha[, RPT_REC_NUM := as.numeric(RPT_REC_NUM)]
alpha[, WKSHT_CD := as.character(WKSHT_CD)]
alpha[, LINE_NUM := as.character(LINE_NUM)]
alpha[, CLMN_NUM := as.character(CLMN_NUM)]
alpha[, ALPHNMRC_ITM_TXT := as.character(ALPHNMRC_ITM_TXT)]

## ---------------------------
## inspect first few rows
## ---------------------------
cat("\nFirst rows of RPT:\n")
print(head(rpt))

cat("\nFirst rows of NMRC:\n")
print(head(nmrc))

cat("\nFirst rows of ALPHA:\n")
print(head(alpha))

## ---------------------------
## save cleaned versions
## ---------------------------
saveRDS(rpt,   file.path(intermediate_dir, "rpt_clean.rds"))
saveRDS(nmrc,  file.path(intermediate_dir, "nmrc_clean.rds"))
saveRDS(alpha, file.path(intermediate_dir, "alpha_clean.rds"))

fwrite(rpt,   file.path(intermediate_dir, "rpt_clean.csv"))
fwrite(nmrc,  file.path(intermediate_dir, "nmrc_clean.csv"))
fwrite(alpha, file.path(intermediate_dir, "alpha_clean.csv"))