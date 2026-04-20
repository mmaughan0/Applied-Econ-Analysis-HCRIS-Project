source("code/00_setup_file.R")

library(data.table)

## =========================================================
## LOAD
## =========================================================
rpt  <- readRDS(file.path(intermediate_dir, "rpt_clean.rds"))
nmrc <- readRDS(file.path(intermediate_dir, "nmrc_clean.rds"))

nmrc[, `:=`(
  WKSHT_CD = as.character(WKSHT_CD),
  LINE_NUM = as.character(LINE_NUM),
  CLMN_NUM = as.character(CLMN_NUM)
)]

## =========================================================
## HELPERS
## =========================================================
pull_single_item <- function(dt, wksht, line, col, value_name) {
  out <- dt[
    WKSHT_CD == wksht &
      LINE_NUM == line &
      CLMN_NUM == col,
    .(RPT_REC_NUM, value = ITM_VAL_NUM)
  ]
  
  dup <- out[, .N, by = RPT_REC_NUM][N > 1]
  if (nrow(dup) > 0) {
    stop(paste("Duplicate rows found for", value_name))
  }
  
  setnames(out, "value", value_name)
  out
}

pull_multi_line_one_col <- function(dt, wksht, lines, col, name_map) {
  out <- dt[
    WKSHT_CD == wksht &
      LINE_NUM %in% lines &
      CLMN_NUM == col,
    .(RPT_REC_NUM, LINE_NUM, value = ITM_VAL_NUM)
  ]
  
  out[, varname := name_map[LINE_NUM]]
  out <- out[!is.na(varname)]
  
  dup <- out[, .N, by = .(RPT_REC_NUM, varname)][N > 1]
  if (nrow(dup) > 0) {
    stop("Duplicate rows found in pull_multi_line_one_col()")
  }
  
  dcast(out, RPT_REC_NUM ~ varname, value.var = "value")
}

pull_one_line_multi_col <- function(dt, wksht, line, cols, name_map) {
  out <- dt[
    WKSHT_CD == wksht &
      LINE_NUM == line &
      CLMN_NUM %in% cols,
    .(RPT_REC_NUM, CLMN_NUM, value = ITM_VAL_NUM)
  ]
  
  out[, varname := name_map[CLMN_NUM]]
  out <- out[!is.na(varname)]
  
  dup <- out[, .N, by = .(RPT_REC_NUM, varname)][N > 1]
  if (nrow(dup) > 0) {
    stop("Duplicate rows found in pull_one_line_multi_col()")
  }
  
  dcast(out, RPT_REC_NUM ~ varname, value.var = "value")
}

merge_one <- function(base_dt, add_dt) {
  merge(base_dt, add_dt, by = "RPT_REC_NUM", all.x = TRUE)
}

## =========================================================
## WORKSHEET CODES
## =========================================================
## from your prior work
g3_wksht <- "G300000"
c_wksht  <- "C000001"
s3_wksht <- "S300001"
g2_wksht <- "G200000"

## Medicare worksheets chosen from your candidate lists
d1_wksht <- "D10A181"
e1_wksht <- "E10A181"
d_wksht  <- "D00A185"

## optional D-5 add-on not used for now
use_d5   <- FALSE
d5_wksht <- NA_character_

cat("Using worksheet codes:\n")
cat("G-3:", g3_wksht, "\n")
cat("C  :", c_wksht,  "\n")
cat("S-3:", s3_wksht, "\n")
cat("G-2:", g2_wksht, "\n")
cat("D-1:", d1_wksht, "\n")
cat("E-1:", e1_wksht, "\n")
cat("D  :", d_wksht,  "\n")

## =========================================================
## 1) G-3 FINANCIALS
## =========================================================
g3_name_map <- c(
  "100" = "total_patient_revenues",
  "200" = "contractual_allowances_discounts",
  "300" = "net_patient_revenue",
  "400" = "total_operating_expenses",
  "500" = "net_income_service_to_patients"
)

g3_dt <- pull_multi_line_one_col(
  nmrc,
  wksht = g3_wksht,
  lines = names(g3_name_map),
  col   = "00100",
  name_map = g3_name_map
)

rpt <- merge_one(rpt, g3_dt)

rpt[, check_net_patient_revenue :=
      total_patient_revenues - contractual_allowances_discounts]

rpt[, check_net_income_service_to_patients :=
      net_patient_revenue - total_operating_expenses]

## =========================================================
## 2) WORKSHEET C TOTAL COSTS
## =========================================================
c_name_map <- c(
  "20000" = "c_total_cost_subtotal",
  "20100" = "c_total_cost_less_observation",
  "20200" = "c_total_cost_final"
)

c_dt <- pull_multi_line_one_col(
  nmrc,
  wksht = c_wksht,
  lines = names(c_name_map),
  col   = "00500",
  name_map = c_name_map
)

rpt <- merge_one(rpt, c_dt)

rpt[, check_c_total_cost_final :=
      c_total_cost_subtotal - c_total_cost_less_observation]

## =========================================================
## 3) ADJUSTED DISCHARGES INPUTS
## =========================================================
## S-3 Part I line 14 col 15 = inpatient discharges
s3_disch_dt <- pull_single_item(
  nmrc, s3_wksht, "1400", "01500", "inpatient_discharges"
)

## G-2 line 28 col 1 = inpatient revenue
## G-2 line 28 col 2 = outpatient revenue
g2_name_map <- c(
  "00100" = "inpatient_revenue",
  "00200" = "outpatient_revenue"
)

g2_rev_dt <- pull_one_line_multi_col(
  nmrc,
  wksht = g2_wksht,
  line  = "2800",
  cols  = names(g2_name_map),
  name_map = g2_name_map
)

rpt <- merge_one(rpt, s3_disch_dt)
rpt <- merge_one(rpt, g2_rev_dt)

rpt[, avg_inpatient_revenue_per_discharge := fifelse(
  !is.na(inpatient_revenue) &
    !is.na(inpatient_discharges) &
    inpatient_discharges != 0,
  inpatient_revenue / inpatient_discharges,
  NA_real_
)]

rpt[, adjusted_discharges := fifelse(
  !is.na(outpatient_revenue) &
    !is.na(avg_inpatient_revenue_per_discharge) &
    avg_inpatient_revenue_per_discharge != 0 &
    !is.na(inpatient_discharges),
  inpatient_discharges + (outpatient_revenue / avg_inpatient_revenue_per_discharge),
  NA_real_
)]

## =========================================================
## 4) MEDICARE MARGIN INPUTS
## =========================================================
## D-1 Part II line 53 col 1 = Medicare inpatient operating cost
medicare_inpatient_cost_dt <- pull_single_item(
  nmrc, d1_wksht, "5300", "00100", "medicare_inpatient_cost"
)

## E-1 Part I line 7 col 2 = Medicare Part A payment
medicare_partA_payment_dt <- pull_single_item(
  nmrc, e1_wksht, "700", "00200", "medicare_partA_payment"
)

## E-1 Part I line 7 col 4 = Medicare Part B payment
medicare_partB_payment_dt <- pull_single_item(
  nmrc, e1_wksht, "700", "00400", "medicare_partB_payment"
)

## D Part V line 202 cols 6 and 7 = Medicare outpatient / Part B cost
medicare_partB_cost_dt <- pull_one_line_multi_col(
  nmrc,
  wksht = d_wksht,
  line  = "20200",
  cols  = c("00600", "00700"),
  name_map = c(
    "00600" = "medicare_outpatient_cost_col6",
    "00700" = "medicare_outpatient_cost_col7"
  )
)

## optional D-5 physician Part B cost
if (use_d5) {
  d5_phys_cost_dt <- pull_single_item(
    nmrc, d5_wksht, "2100", "00100", "medicare_partB_physician_cost"
  )
} else {
  d5_phys_cost_dt <- unique(rpt[, .(RPT_REC_NUM)])
  d5_phys_cost_dt[, medicare_partB_physician_cost := NA_real_]
}

rpt <- merge_one(rpt, medicare_inpatient_cost_dt)
rpt <- merge_one(rpt, medicare_partA_payment_dt)
rpt <- merge_one(rpt, medicare_partB_payment_dt)
rpt <- merge_one(rpt, medicare_partB_cost_dt)
rpt <- merge_one(rpt, d5_phys_cost_dt)

## =========================================================
## 5) MEDICARE MARGIN CONSTRUCTION
## =========================================================
rpt[, medicare_outpatient_cost_core :=
      medicare_outpatient_cost_col6 + medicare_outpatient_cost_col7]

rpt[, medicare_total_payment :=
      medicare_partA_payment + medicare_partB_payment]

rpt[, medicare_total_cost_core :=
      medicare_inpatient_cost + medicare_outpatient_cost_core]

rpt[, medicare_total_cost_with_d5 :=
      medicare_total_cost_core + fcoalesce(medicare_partB_physician_cost, 0)]

## inpatient Medicare margin
rpt[, medicare_inpatient_margin := fifelse(
  !is.na(medicare_partA_payment) &
    medicare_partA_payment != 0 &
    !is.na(medicare_inpatient_cost),
  (medicare_partA_payment - medicare_inpatient_cost) / medicare_partA_payment,
  NA_real_
)]

## total Medicare margin (core)
rpt[, medicare_total_margin_core := fifelse(
  !is.na(medicare_total_payment) &
    medicare_total_payment != 0 &
    !is.na(medicare_total_cost_core),
  (medicare_total_payment - medicare_total_cost_core) / medicare_total_payment,
  NA_real_
)]

## total Medicare margin (with optional D-5 add-on)
rpt[, medicare_total_margin_with_d5 := fifelse(
  !is.na(medicare_total_payment) &
    medicare_total_payment != 0 &
    !is.na(medicare_total_cost_with_d5),
  (medicare_total_payment - medicare_total_cost_with_d5) / medicare_total_payment,
  NA_real_
)]

## =========================================================
## 6) HOSPITAL COST PER ADJUSTED DISCHARGE + MEDICARE PRICE
## =========================================================
rpt[, hospital_cost_per_adjusted_discharge := fifelse(
  !is.na(c_total_cost_final) &
    !is.na(adjusted_discharges) &
    adjusted_discharges != 0,
  c_total_cost_final / adjusted_discharges,
  NA_real_
)]

## uses core Medicare total margin
rpt[, mdcr_price_per_ad := fifelse(
  !is.na(hospital_cost_per_adjusted_discharge) &
    !is.na(medicare_total_margin_core) &
    (1 - medicare_total_margin_core) != 0,
  hospital_cost_per_adjusted_discharge / (1 - medicare_total_margin_core),
  NA_real_
)]

## optional alternative using D-5-inclusive total margin
rpt[, mdcr_price_per_ad_with_d5 := fifelse(
  !is.na(hospital_cost_per_adjusted_discharge) &
    !is.na(medicare_total_margin_with_d5) &
    (1 - medicare_total_margin_with_d5) != 0,
  hospital_cost_per_adjusted_discharge / (1 - medicare_total_margin_with_d5),
  NA_real_
)]

## =========================================================
## 7) INSPECT
## =========================================================
print(
  rpt[
    , .(
      RPT_REC_NUM,
      PRVDR_NUM,
      c_total_cost_final,
      adjusted_discharges,
      hospital_cost_per_adjusted_discharge,
      medicare_inpatient_cost,
      medicare_partA_payment,
      medicare_inpatient_margin,
      medicare_outpatient_cost_core,
      medicare_partB_payment,
      medicare_total_payment,
      medicare_total_cost_core,
      medicare_total_margin_core,
      mdcr_price_per_ad,
      mdcr_price_per_ad_with_d5
    )
  ][1:20]
)

cat("\nNon-missing adjusted discharges:",
    sum(!is.na(rpt$adjusted_discharges)), "\n")

cat("Non-missing hospital cost per adjusted discharge:",
    sum(!is.na(rpt$hospital_cost_per_adjusted_discharge)), "\n")

cat("Non-missing Medicare inpatient margin:",
    sum(!is.na(rpt$medicare_inpatient_margin)), "\n")

cat("Non-missing Medicare total margin (core):",
    sum(!is.na(rpt$medicare_total_margin_core)), "\n")

cat("Non-missing mdcr_price_per_ad:",
    sum(!is.na(rpt$mdcr_price_per_ad)), "\n")

## =========================================================
## 8) SAVE MASTER ANALYTICAL FILE
## =========================================================
saveRDS(rpt, file.path(intermediate_dir, "rpt_master_analytic.rds"))
fwrite(rpt, file.path(intermediate_dir, "rpt_master_analytic.csv"))

## =========================================================
## 9) MEDICARE PAYER MIX FROM S-3
## =========================================================
## Best guess from S-3 Part I, line 14:
## col 6  = Medicare days
## col 8  = total days
## col 13 = Medicare discharges
## col 15 = total discharges

medicare_mix_dt <- pull_one_line_multi_col(
  nmrc,
  wksht = s3_wksht,
  line  = "1400",
  cols  = c("00600", "00800", "01300", "01500"),
  name_map = c(
    "00600" = "medicare_days",
    "00800" = "total_days",
    "01300" = "medicare_discharges",
    "01500" = "total_discharges"
  )
)

rpt <- merge_one(rpt, medicare_mix_dt)

rpt[, medicare_day_share := fifelse(
  !is.na(medicare_days) &
    !is.na(total_days) &
    total_days != 0,
  medicare_days / total_days,
  NA_real_
)]

rpt[, medicare_discharge_share := fifelse(
  !is.na(medicare_discharges) &
    !is.na(total_discharges) &
    total_discharges != 0,
  medicare_discharges / total_discharges,
  NA_real_
)]

## =========================================================
## 10) MEDICARE REVENUE-BASED ADJUSTED DISCHARGE METHOD
## =========================================================
## Treat Medicare Part A and Part B payments as Medicare inpatient/outpatient revenue proxies

rpt[, medicare_inpatient_revenue := medicare_partA_payment]
rpt[, medicare_outpatient_revenue := medicare_partB_payment]
rpt[, medicare_total_revenue := medicare_inpatient_revenue + medicare_outpatient_revenue]

## Apply Medicare payer mix to total adjusted discharges
## Best guess: use discharge share rather than day share
rpt[, medicare_adjusted_discharges := fifelse(
  !is.na(adjusted_discharges) &
    !is.na(medicare_discharge_share),
  adjusted_discharges * medicare_discharge_share,
  NA_real_
)]

## Revenue per Medicare-adjusted discharge
rpt[, medicare_revenue_per_adjusted_discharge := fifelse(
  !is.na(medicare_total_revenue) &
    !is.na(medicare_adjusted_discharges) &
    medicare_adjusted_discharges != 0,
  medicare_total_revenue / medicare_adjusted_discharges,
  NA_real_
)]

## Optional: separate inpatient and outpatient shares of total adjusted discharges
## These are rough proxies, included only if useful
rpt[, medicare_inpatient_adjusted_discharges := fifelse(
  !is.na(adjusted_discharges) &
    !is.na(medicare_discharge_share),
  adjusted_discharges * medicare_discharge_share,
  NA_real_
)]

rpt[, medicare_outpatient_adjusted_discharges := fifelse(
  !is.na(medicare_adjusted_discharges) &
    !is.na(medicare_discharges) &
    !is.na(medicare_days),
  NA_real_,
  NA_real_
)]

## inspect
print(
  rpt[
    , .(
      RPT_REC_NUM,
      PRVDR_NUM,
      medicare_days,
      total_days,
      medicare_day_share,
      medicare_discharges,
      total_discharges,
      medicare_discharge_share,
      adjusted_discharges,
      medicare_adjusted_discharges,
      medicare_inpatient_revenue,
      medicare_outpatient_revenue,
      medicare_total_revenue,
      medicare_revenue_per_adjusted_discharge
    )
  ][1:20]
)

cat("\nNon-missing Medicare discharge share:",
    sum(!is.na(rpt$medicare_discharge_share)), "\n")

cat("Non-missing Medicare-adjusted discharges:",
    sum(!is.na(rpt$medicare_adjusted_discharges)), "\n")

cat("Non-missing Medicare revenue per adjusted discharge:",
    sum(!is.na(rpt$medicare_revenue_per_adjusted_discharge)), "\n")

    ## =========================================================
## 10) SAVE MASTER ANALYTICAL FILE
## =========================================================
saveRDS(rpt, file.path(intermediate_dir, "rpt_master_analytic.rds"))
fwrite(rpt, file.path(intermediate_dir, "rpt_master_analytic.csv"))

source("code/00_setup_file.R")

library(data.table)

## =========================================================
## LOAD MASTER FILE + ALPHA
## =========================================================
rpt   <- readRDS(file.path(intermediate_dir, "rpt_master_analytic.rds"))
alpha <- readRDS(file.path(intermediate_dir, "alpha_clean.rds"))

alpha[, `:=`(
  WKSHT_CD = as.character(WKSHT_CD),
  LINE_NUM = as.character(LINE_NUM),
  CLMN_NUM = as.character(CLMN_NUM),
  ALPHNMRC_ITM_TXT = as.character(ALPHNMRC_ITM_TXT)
)]

## =========================================================
## RENAME PRICE VARIABLES
## =========================================================
setnames(
  rpt,
  old = c("mdcr_price_per_ad", "medicare_revenue_per_adjusted_discharge"),
  new = c("Price_Profit_Margin_Method", "Price_Payer_Mix_Method"),
  skip_absent = TRUE
)

## =========================================================
## PULL STATE FROM ALPHA
## Best guess from your location:
## WKSHT_CD = S200001
## LINE_NUM = 14300
## CLMN_NUM = 00200
## =========================================================
state_dt <- alpha[
  WKSHT_CD == "S200001" &
    LINE_NUM == "14300" &
    CLMN_NUM == "200",
  .(RPT_REC_NUM, state = ALPHNMRC_ITM_TXT)
]

## check duplicates
dup_state <- state_dt[, .N, by = RPT_REC_NUM][N > 1]
if (nrow(dup_state) > 0) {
  stop("Duplicate state rows found in alpha.")
}

## merge state onto master analytic file
rpt <- merge(rpt, state_dt, by = "RPT_REC_NUM", all.x = TRUE)

## optional cleanup
rpt[, state := trimws(state)]
rpt[state == "", state := NA_character_]

## quick check
cat("Non-missing state values:", sum(!is.na(rpt$state)), "\n")
print(head(unique(rpt[, .(state)]), 20))

## =========================================================
## KEEP ONLY OBS WITH EVERYTHING NEEDED
## =========================================================
state_analysis <- rpt[
  !is.na(state) &
    !is.na(adjusted_discharges) &
    adjusted_discharges > 0 &
    !is.na(Price_Profit_Margin_Method) &
    !is.na(Price_Payer_Mix_Method)
]

## =========================================================
## STATE-LEVEL ADJUSTED-DISCHARGE-WEIGHTED AVERAGES
## =========================================================
state_summary <- state_analysis[
  ,
  .(
    n_hospitals = .N,
    total_adjusted_discharges = sum(adjusted_discharges, na.rm = TRUE),
    Price_Profit_Margin_Method = weighted.mean(
      Price_Profit_Margin_Method,
      w = adjusted_discharges,
      na.rm = TRUE
    ),
    Price_Payer_Mix_Method = weighted.mean(
      Price_Payer_Mix_Method,
      w = adjusted_discharges,
      na.rm = TRUE
    )
  ),
  by = state
][order(state)]

## =========================================================
## INSPECT
## =========================================================
print(state_summary)

cat("\nNumber of states:", nrow(state_summary), "\n")

## =========================================================
## SAVE OUTPUTS
## =========================================================
saveRDS(rpt, file.path(intermediate_dir, "rpt_master_analytic_with_state.rds"))
fwrite(rpt, file.path(intermediate_dir, "rpt_master_analytic_with_state.csv"))

saveRDS(state_summary, file.path(intermediate_dir, "state_weighted_price_summary.rds"))
fwrite(state_summary, file.path(intermediate_dir, "state_weighted_price_summary.csv"))



## =========================================================
## LOAD RAW HCRIS STATE-LEVEL FILE
## =========================================================
## adjust filename if needed
state_hcris <- fread(file.path(intermediate_dir, "state_weighted_price_summary.csv"))

## inspect names if needed
print(names(state_hcris))

## =========================================================
## REGRESSION WITH INTERCEPT
## =========================================================
reg_with_intercept <- lm(
  Price_Payer_Mix_Method ~ Price_Profit_Margin_Method,
  data = state_hcris
)

reg_sum <- summary(reg_with_intercept)

intercept_hat <- coef(reg_with_intercept)[["(Intercept)"]]
beta_hat      <- coef(reg_with_intercept)[["Price_Profit_Margin_Method"]]

t_stat <- reg_sum$coefficients["Price_Profit_Margin_Method", "t value"]
p_value <- reg_sum$coefficients["Price_Profit_Margin_Method", "Pr(>|t|)"]

subtitle_text <- paste0(
  "Fit: y = ",
  round(intercept_hat, 3),
  ifelse(beta_hat >= 0, " + ", " - "),
  round(abs(beta_hat), 3),
  "x",
  "   |   slope t = ", round(t_stat, 2),
  "   |   slope p = ", format.pval(p_value, digits = 3, eps = 0.001)
)

print(summary(reg_with_intercept))

## =========================================================
## SCATTERPLOT WITH LABELS + BEST FIT LINE
## =========================================================
p <- ggplot(
  state_hcris,
  aes(
    x = Price_Profit_Margin_Method,
    y = Price_Payer_Mix_Method
  )
) +
  geom_point() +
  geom_text(aes(label = state), vjust = -0.6, size = 3) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  labs(
    x = "Profit Margin Based Method",
    y = "Payer Mix Based Method",
    title = "State-Level Medicare Price Estimates (Raw HCRIS)",
    subtitle = subtitle_text
  ) +
  theme_minimal()

print(p)

## =========================================================
## SAVE PLOT TO REPO
## =========================================================
ggsave(
  filename = file.path(output_dir, "state_price_raw_hcris_scatter_intercept_labeled.png"),
  plot = p,
  width = 8,
  height = 6,
  dpi = 300
)
source("code/00_setup_file.R")

library(data.table)
library(ggplot2)

## =========================================================
## LOAD STATE-LEVEL FILE
## =========================================================
state_exante <- fread(file.path(source_dir, "State_Price_Estimation_ExAnte.csv"))

## =========================================================
## RENAME COLUMNS
## =========================================================
setnames(
  state_exante,
  old = c("Profit Margin Price Method", "Payer Mix Price Method"),
  new = c("Price_Profit_Margin_Method", "Price_Payer_Mix_Method"),
  skip_absent = TRUE
)

## =========================================================
## KEEP COMPLETE CASES
## =========================================================
state_exante <- state_exante[
  !is.na(Price_Profit_Margin_Method) &
    !is.na(Price_Payer_Mix_Method)
]

## =========================================================
## NO-INTERCEPT REGRESSION
## =========================================================
reg_no_intercept <- lm(
  Price_Payer_Mix_Method ~ 0 + Price_Profit_Margin_Method,
  data = state_exante
)

reg_sum <- summary(reg_no_intercept)

beta_hat <- coef(reg_no_intercept)[["Price_Profit_Margin_Method"]]
t_stat   <- reg_sum$coefficients["Price_Profit_Margin_Method", "t value"]
p_value  <- reg_sum$coefficients["Price_Profit_Margin_Method", "Pr(>|t|)"]

subtitle_text <- paste0(
  "No-intercept fit: y = ", round(beta_hat, 3), "x",
  "   |   t = ", round(t_stat, 2),
  "   |   p = ", format.pval(p_value, digits = 3, eps = 0.001)
)

print(summary(reg_no_intercept))

## =========================================================
## SCATTERPLOT WITH STATE LABELS
## =========================================================
p <- ggplot(
  state_exante,
  aes(
    x = Price_Profit_Margin_Method,
    y = Price_Payer_Mix_Method
  )
) +
  geom_point() +
  geom_text(aes(label = State), vjust = -0.6, size = 3) +
  geom_abline(
    slope = beta_hat,
    intercept = 0,
    linetype = "dashed"
  ) +
  labs(
    x = "Profit Margin Based Method",
    y = "Payer Mix Based Method",
    title = "State-Level Medicare Price Estimates (Ex Ante)",
    subtitle = subtitle_text
  ) +
  theme_minimal()

print(p)

## =========================================================
## SAVE PLOT TO REPO
## =========================================================
ggsave(
  filename = file.path(output_dir, "state_price_exante_scatter_no_intercept_labeled.png"),
  plot = p,
  width = 8,
  height = 6,
  dpi = 300
)



## =========================================================
## LOAD FILES
## =========================================================
state_given <- fread(file.path(source_dir, "State_Price_Estimation_ExAnte.csv"))
state_hcris <- fread(file.path(intermediate_dir, "state_weighted_price_summary.csv"))

## =========================================================
## STANDARDIZE STATE COLUMN
## =========================================================
setnames(state_given, old = "State", new = "state", skip_absent = TRUE)
setnames(state_hcris, old = "State", new = "state", skip_absent = TRUE)

state_given[, state := trimws(as.character(state))]
state_hcris[, state := trimws(as.character(state))]

## =========================================================
## STANDARDIZE METHOD NAMES
## =========================================================
setnames(
  state_given,
  old = c("Profit Margin Price Method", "Payer Mix Price Method"),
  new = c("Given_Profit_Margin_Method", "Given_Payer_Mix_Method"),
  skip_absent = TRUE
)

setnames(
  state_hcris,
  old = c("Price_Profit_Margin_Method", "Price_Payer_Mix_Method"),
  new = c("HCRIS_Profit_Margin_Method", "HCRIS_Payer_Mix_Method"),
  skip_absent = TRUE
)

## =========================================================
## FORCE NUMERIC
## =========================================================
to_num <- function(x) {
  as.numeric(gsub("[,$% ]", "", as.character(x)))
}

state_given[, `:=`(
  Given_Profit_Margin_Method = to_num(Given_Profit_Margin_Method),
  Given_Payer_Mix_Method     = to_num(Given_Payer_Mix_Method)
)]

state_hcris[, `:=`(
  HCRIS_Profit_Margin_Method = to_num(HCRIS_Profit_Margin_Method),
  HCRIS_Payer_Mix_Method     = to_num(HCRIS_Payer_Mix_Method)
)]

## =========================================================
## MERGE BY STATE
## =========================================================
compare_dt <- merge(
  state_given,
  state_hcris[, .(state, HCRIS_Profit_Margin_Method, HCRIS_Payer_Mix_Method)],
  by = "state",
  all = FALSE
)

cat("Merged states:", nrow(compare_dt), "\n")
print(compare_dt)

## =========================================================
## 1) PROFIT MARGIN METHOD COMPARISON
## =========================================================
profit_dt <- compare_dt[
  !is.na(Given_Profit_Margin_Method) &
    !is.na(HCRIS_Profit_Margin_Method)
]

cat("\nProfit comparison observations:", nrow(profit_dt), "\n")

if (nrow(profit_dt) >= 2) {
  reg_profit <- lm(
    HCRIS_Profit_Margin_Method ~ Given_Profit_Margin_Method,
    data = profit_dt
  )
  
  reg_profit_sum <- summary(reg_profit)
  
  profit_coef <- coef(reg_profit)
  profit_intercept <- unname(profit_coef[1])
  profit_beta      <- unname(profit_coef[2])
  
  profit_t <- reg_profit_sum$coefficients[2, "t value"]
  profit_p <- reg_profit_sum$coefficients[2, "Pr(>|t|)"]
  
  profit_subtitle <- paste0(
    "Fit: y = ",
    round(profit_intercept, 3),
    ifelse(profit_beta >= 0, " + ", " - "),
    round(abs(profit_beta), 3),
    "x",
    "   |   slope t = ", round(profit_t, 2),
    "   |   slope p = ", format.pval(profit_p, digits = 3, eps = 0.001)
  )
  
  print(summary(reg_profit))
  
  p_profit <- ggplot(
    profit_dt,
    aes(
      x = Given_Profit_Margin_Method,
      y = HCRIS_Profit_Margin_Method
    )
  ) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    geom_point() +
    geom_text(aes(label = state), vjust = -0.6, size = 3) +
    geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
    labs(
      x = "Given File: Profit Margin Based Method",
      y = "Raw HCRIS: Profit Margin Based Method",
      title = "Profit Margin Method Comparison",
      subtitle = profit_subtitle
    ) +
    theme_minimal()
  
  print(p_profit)
  
  ggsave(
    filename = file.path(output_dir, "compare_profit_margin_method_given_vs_hcris.png"),
    plot = p_profit,
    width = 8,
    height = 6,
    dpi = 300
  )
}

## =========================================================
## 2) PAYER MIX METHOD COMPARISON
## =========================================================
payermix_dt <- compare_dt[
  !is.na(Given_Payer_Mix_Method) &
    !is.na(HCRIS_Payer_Mix_Method)
]

cat("\nPayer-mix comparison observations:", nrow(payermix_dt), "\n")

if (nrow(payermix_dt) >= 2) {
  reg_payermix <- lm(
    HCRIS_Payer_Mix_Method ~ Given_Payer_Mix_Method,
    data = payermix_dt
  )
  
  reg_payermix_sum <- summary(reg_payermix)
  
  payermix_coef <- coef(reg_payermix)
  payermix_intercept <- unname(payermix_coef[1])
  payermix_beta      <- unname(payermix_coef[2])
  
  payermix_t <- reg_payermix_sum$coefficients[2, "t value"]
  payermix_p <- reg_payermix_sum$coefficients[2, "Pr(>|t|)"]
  
  payermix_subtitle <- paste0(
    "Fit: y = ",
    round(payermix_intercept, 3),
    ifelse(payermix_beta >= 0, " + ", " - "),
    round(abs(payermix_beta), 3),
    "x",
    "   |   slope t = ", round(payermix_t, 2),
    "   |   slope p = ", format.pval(payermix_p, digits = 3, eps = 0.001)
  )
  
  print(summary(reg_payermix))
  
  p_payermix <- ggplot(
    payermix_dt,
    aes(
      x = Given_Payer_Mix_Method,
      y = HCRIS_Payer_Mix_Method
    )
  ) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    geom_point() +
    geom_text(aes(label = state), vjust = -0.6, size = 3) +
    geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
    labs(
      x = "Given File: Payer Mix Based Method",
      y = "Raw HCRIS: Payer Mix Based Method",
      title = "Payer Mix Method Comparison",
      subtitle = payermix_subtitle
    ) +
    theme_minimal()
  
  print(p_payermix)
  
  ggsave(
    filename = file.path(output_dir, "compare_payermix_method_given_vs_hcris.png"),
    plot = p_payermix,
    width = 8,
    height = 6,
    dpi = 300
  )
}

