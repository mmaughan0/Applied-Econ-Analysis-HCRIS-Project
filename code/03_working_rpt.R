source("code/00_setup_file.R")

library(data.table)

## file paths
rpt_file  <- file.path(intermediate_dir, "rpt_clean.rds")
nmrc_file <- file.path(intermediate_dir, "nmrc_clean.rds")

## load cleaned files
rpt  <- readRDS(rpt_file)
nmrc <- readRDS(nmrc_file)

## make sure line/column are treated consistently as character
nmrc[, WKSHT_CD := as.character(WKSHT_CD)]
nmrc[, LINE_NUM := as.character(LINE_NUM)]
nmrc[, CLMN_NUM := as.character(CLMN_NUM)]

## pull key Worksheet G-3 financial items
g3_financials_long <- nmrc[
  WKSHT_CD == "G300000" &
  CLMN_NUM == "00100" &
  LINE_NUM %in% c("100", "200", "300", "400", "500"),
  .(RPT_REC_NUM, LINE_NUM, ITM_VAL_NUM)
]

cat("Number of matching G-3 rows:", nrow(g3_financials_long), "\n")
print(head(g3_financials_long, 20))

## map line numbers to readable variable names
g3_financials_long[, varname := fifelse(
  LINE_NUM == "100", "total_patient_revenues",
  fifelse(
    LINE_NUM == "200", "contractual_allowances_discounts",
    fifelse(
      LINE_NUM == "300", "net_patient_revenue",
      fifelse(
        LINE_NUM == "400", "total_operating_expenses",
        fifelse(
          LINE_NUM == "500", "net_income_service_to_patients",
          NA_character_
        )
      )
    )
  )
)]

## drop anything unexpected just in case
g3_financials_long <- g3_financials_long[!is.na(varname)]

## check duplicates before reshaping
dup_check <- g3_financials_long[, .N, by = .(RPT_REC_NUM, varname)][N > 1]
print(dup_check)

## reshape wide
g3_financials_wide <- dcast(
  g3_financials_long,
  RPT_REC_NUM ~ varname,
  value.var = "ITM_VAL_NUM"
)

print(names(g3_financials_wide))

## merge onto rpt
rpt <- merge(
  rpt,
  g3_financials_wide,
  by = "RPT_REC_NUM",
  all.x = TRUE
)

## optional audit checks
rpt[, check_net_patient_revenue := total_patient_revenues - contractual_allowances_discounts]
rpt[, check_net_income_service_to_patients := net_patient_revenue - total_operating_expenses]

## mismatch checks with tolerance
mismatch_net_rev <- rpt[
  !is.na(net_patient_revenue) &
  !is.na(check_net_patient_revenue) &
  abs(net_patient_revenue - check_net_patient_revenue) > 1e-6,
  .(
    RPT_REC_NUM,
    PRVDR_NUM,
    total_patient_revenues,
    contractual_allowances_discounts,
    net_patient_revenue,
    check_net_patient_revenue
  )
]

mismatch_net_income <- rpt[
  !is.na(net_income_service_to_patients) &
  !is.na(check_net_income_service_to_patients) &
  abs(net_income_service_to_patients - check_net_income_service_to_patients) > 1e-6,
  .(
    RPT_REC_NUM,
    PRVDR_NUM,
    net_patient_revenue,
    total_operating_expenses,
    net_income_service_to_patients,
    check_net_income_service_to_patients
  )
]

cat("Number of net patient revenue mismatches:", nrow(mismatch_net_rev), "\n")
cat("Number of net income from service mismatches:", nrow(mismatch_net_income), "\n")

## inspect
print(
  rpt[
    , .(
      RPT_REC_NUM,
      PRVDR_NUM,
      total_patient_revenues,
      contractual_allowances_discounts,
      net_patient_revenue,
      total_operating_expenses,
      net_income_service_to_patients,
      check_net_patient_revenue,
      check_net_income_service_to_patients
    )
  ][1:10]
)
source("code/00_setup_file.R")

library(data.table)

## file paths
rpt_file  <- file.path(intermediate_dir, "rpt_clean.rds")
nmrc_file <- file.path(intermediate_dir, "nmrc_clean.rds")

## load cleaned files
rpt  <- readRDS(rpt_file)
nmrc <- readRDS(nmrc_file)

## make sure keys are character
nmrc[, WKSHT_CD := as.character(WKSHT_CD)]
nmrc[, LINE_NUM := as.character(LINE_NUM)]
nmrc[, CLMN_NUM := as.character(CLMN_NUM)]

## -------------------------------------------------
## inspect actual Worksheet C coding in your nmrc
## -------------------------------------------------
cat("Sample Worksheet C rows around lines 200-202:\n")
print(
  nmrc[
    grepl("^C", WKSHT_CD) & LINE_NUM %in% c("200", "201", "202"),
    .(RPT_REC_NUM, WKSHT_CD, LINE_NUM, CLMN_NUM, ITM_VAL_NUM)
  ][1:50]
)

source("code/00_setup_file.R")

library(data.table)

## file paths
rpt_file  <- file.path(intermediate_dir, "rpt_clean.rds")
nmrc_file <- file.path(intermediate_dir, "nmrc_clean.rds")

## load cleaned files
rpt  <- readRDS(rpt_file)
nmrc <- readRDS(nmrc_file)

## make sure keys are character
nmrc[, WKSHT_CD := as.character(WKSHT_CD)]
nmrc[, LINE_NUM := as.character(LINE_NUM)]
nmrc[, CLMN_NUM := as.character(CLMN_NUM)]

## inspect lines near the bottom of Worksheet C
cat("Worksheet C rows around total lines:\n")
print(
  nmrc[
    WKSHT_CD == "C000001" &
    LINE_NUM %in% c("20000", "20100", "20200"),
    .(RPT_REC_NUM, WKSHT_CD, LINE_NUM, CLMN_NUM, ITM_VAL_NUM)
  ][1:50]
)

## pull Worksheet C Part I total cost measures
c_cost_long <- nmrc[
  WKSHT_CD == "C000001" &
  CLMN_NUM == "00500" &
  LINE_NUM %in% c("20000", "20100", "20200"),
  .(RPT_REC_NUM, LINE_NUM, ITM_VAL_NUM)
]

cat("\nNumber of matching Worksheet C rows:", nrow(c_cost_long), "\n")
print(c_cost_long[1:20])

## stop if no matches
if (nrow(c_cost_long) == 0) {
  stop("No matches found for Worksheet C totals. Check LINE_NUM and WKSHT_CD again.")
}

## map names
c_cost_long[, varname := fifelse(
  LINE_NUM == "20000", "c_total_cost_subtotal",
  fifelse(
    LINE_NUM == "20100", "c_total_cost_less_observation",
    fifelse(
      LINE_NUM == "20200", "c_total_cost_final",
      NA_character_
    )
  )
)]

c_cost_long <- c_cost_long[!is.na(varname)]

## duplicate check
dup_check_c <- c_cost_long[, .N, by = .(RPT_REC_NUM, varname)][N > 1]
print(dup_check_c)

## reshape wide
c_cost_wide <- dcast(
  c_cost_long,
  RPT_REC_NUM ~ varname,
  value.var = "ITM_VAL_NUM"
)

## merge onto rpt
rpt <- merge(
  rpt,
  c_cost_wide,
  by = "RPT_REC_NUM",
  all.x = TRUE
)

## check: line 202 = line 200 - line 201
rpt[, check_c_total_cost_final := c_total_cost_subtotal - c_total_cost_less_observation]

mismatch_c <- rpt[
  !is.na(c_total_cost_final) &
  !is.na(check_c_total_cost_final) &
  abs(c_total_cost_final - check_c_total_cost_final) > 1e-6,
  .(
    RPT_REC_NUM,
    PRVDR_NUM,
    c_total_cost_subtotal,
    c_total_cost_less_observation,
    c_total_cost_final,
    check_c_total_cost_final
  )
]

cat("Number of Worksheet C total-cost mismatches:", nrow(mismatch_c), "\n")

## inspect
print(
  rpt[
    , .(
      RPT_REC_NUM,
      PRVDR_NUM,
      c_total_cost_subtotal,
      c_total_cost_less_observation,
      c_total_cost_final
    )
  ][1:20]
)

## save
saveRDS(rpt, file.path(intermediate_dir, "rpt_with_worksheet_c_costs.rds"))
fwrite(rpt, file.path(intermediate_dir, "rpt_with_worksheet_c_costs.csv"))