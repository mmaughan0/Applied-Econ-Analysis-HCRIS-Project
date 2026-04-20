source("code/00_setup_file.R")

library(data.table)

## ---------------------------
## file paths
## ---------------------------
rpt_file    <- file.path(source_dir, "HOSP10_2022_rpt.csv")
nmrc_file   <- file.path(source_dir, "HOSP10_2022_nmrc.csv")
alpha_file  <- file.path(source_dir, "HOSP10_2022_alpha.csv")
sample_file <- file.path(source_dir, "analytical_sample.csv")

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
## import raw files
## ---------------------------
rpt   <- fread(rpt_file, header = FALSE)
nmrc  <- fread(nmrc_file, header = FALSE)
alpha <- fread(alpha_file, header = FALSE)

## analytical sample should have headers
sample <- fread(sample_file)

## ---------------------------
## quick checks
## ---------------------------
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
## type conversions
## ---------------------------
date_vars <- c("FI_CREAT_DT", "FI_RCPT_DT", "FY_BGN_DT", "FY_END_DT", "NPR_DT", "PROC_DT")
for (v in date_vars) {
  rpt[, (v) := as.Date(get(v), format = "%m/%d/%Y")]
}

rpt[, PRVDR_NUM := as.character(PRVDR_NUM)]
rpt[, RPT_REC_NUM := as.numeric(RPT_REC_NUM)]

nmrc[, `:=`(
  RPT_REC_NUM = as.numeric(RPT_REC_NUM),
  WKSHT_CD = as.character(WKSHT_CD),
  LINE_NUM = as.character(LINE_NUM),
  CLMN_NUM = as.character(CLMN_NUM),
  ITM_VAL_NUM = as.numeric(ITM_VAL_NUM)
)]

alpha[, `:=`(
  RPT_REC_NUM = as.numeric(RPT_REC_NUM),
  WKSHT_CD = as.character(WKSHT_CD),
  LINE_NUM = as.character(LINE_NUM),
  CLMN_NUM = as.character(CLMN_NUM),
  ALPHNMRC_ITM_TXT = as.character(ALPHNMRC_ITM_TXT)
)]

## ---------------------------
## prep analytical sample for join
## ---------------------------
## assumes the analytical sample has a variable named CCN
sample[, CCN := as.character(CCN)]

## optional: if your CCNs should always be 6 digits, pad both sides
rpt[, PRVDR_NUM := sprintf("%06s", PRVDR_NUM)]
sample[, CCN := sprintf("%06s", CCN)]

## keep only sample rows with a nonmissing CCN
sample <- sample[!is.na(CCN) & CCN != ""]

## if sample has duplicate CCNs, keep one row per CCN before merge
sample_unique <- unique(sample, by = "CCN")

## ---------------------------
## inner join rpt to analytical sample
## ---------------------------
rpt <- merge(
  rpt,
  sample_unique,
  by.x = "PRVDR_NUM",
  by.y = "CCN",
  all = FALSE
)

cat("Matched RPT rows after inner join:", nrow(rpt), "\n")
cat("Unique matched report numbers:", uniqueN(rpt$RPT_REC_NUM), "\n")

## ---------------------------
## restrict nmrc and alpha to matched reports only
## ---------------------------
keep_rpt_rec_num <- unique(rpt$RPT_REC_NUM)

nmrc  <- nmrc[RPT_REC_NUM %in% keep_rpt_rec_num]
alpha <- alpha[RPT_REC_NUM %in% keep_rpt_rec_num]

cat("NMRC rows after restriction:", nrow(nmrc), "\n")
cat("ALPHA rows after restriction:", nrow(alpha), "\n")

## ---------------------------
## inspect
## ---------------------------
cat("\nFirst rows of matched RPT:\n")
print(head(rpt))

cat("\nFirst rows of restricted NMRC:\n")
print(head(nmrc))

cat("\nFirst rows of restricted ALPHA:\n")
print(head(alpha))

## ---------------------------
## save cleaned/sample-restricted versions
## ---------------------------
saveRDS(rpt,   file.path(intermediate_dir, "rpt_clean.rds"))
saveRDS(nmrc,  file.path(intermediate_dir, "nmrc_clean.rds"))
saveRDS(alpha, file.path(intermediate_dir, "alpha_clean.rds"))

fwrite(rpt,   file.path(intermediate_dir, "rpt_clean.csv"))
fwrite(nmrc,  file.path(intermediate_dir, "nmrc_clean.csv"))
fwrite(alpha, file.path(intermediate_dir, "alpha_clean.csv"))