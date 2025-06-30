################################################################################
# Empirical Economics Final Project - Spring 2025
################################################################################

# --- 0. LOAD REQUIRED PACKAGES ---
library(haven)       # For reading .dta files
library(dplyr)       # Data manipulation
library(plm)         # Panel data methods
library(estimatr)    # For cluster-robust SE and IV
library(AER)         # For IV regression and tests
library(lmtest)      # Hypothesis testing

# --- 1. DATA PREPARATION ---
# Load data and convert to panel format
KALIB <- read_dta("KALIB.dta") %>%
  arrange(ccode, year)
KALIB.p <- pdata.frame(KALIB, index = c("ccode", "year"))

# Create lagged variables for instruments - for all questions
KALIB.p$lngni_lag  <- lag(KALIB.p$lngni, 1)
KALIB.p$gni_gr_lag <- lag(KALIB.p$gni_gr, 1)
KALIB.p$pop_gr_lag <- lag(KALIB.p$pop_gr, 1)
KALIB.p$FD_lag     <- lag(KALIB.p$FD, 1)
KALIB.p$year_factor <- as.factor(KALIB.p$year)

# Remove rows with missing lags (important for valid panel and for IV)
KALIB.p <- KALIB.p[complete.cases(KALIB.p[,c("lngni_lag","gni_gr_lag","pop_gr_lag")]),]

################################################################################
# QUESTION 1: IV ESTIMATION WITH ONE ENDOGENOUS VARIABLE (lngni)
################################################################################

# --- (a) 2SLS with lagged lngni as instrument ---
Q1_formula <- gsav_gni ~ lkalib_ci + lcredit + lngni + lntot + trade +
  urban_pop + agedep_old + asia + latin + oecd + year_factor

Q1_iv <- iv_robust(
  gsav_gni ~ lkalib_ci + lcredit + lngni + lntot + trade +
    urban_pop + agedep_old + asia + latin + oecd + year_factor |
    lkalib_ci + lcredit + lngni_lag + lntot + trade +
    urban_pop + agedep_old + asia + latin + oecd + year_factor,
  data = KALIB.p,
  clusters = KALIB.p$ccode
)
cat("\n========== Q1(a): 2SLS Results (One Endogenous Var) ==========\n")
print(summary(Q1_iv))

# --- (b) Instrument Relevance (First Stage) ---
Q1_fs <- lm_robust(
  lngni ~ lngni_lag + lkalib_ci + lcredit + lntot + trade +
    urban_pop + agedep_old + asia + latin + oecd + year_factor,
  data = KALIB.p,
  clusters = KALIB.p$ccode
)
cat("\n========== Q1(b): First Stage Results ==========\n")
print(summary(Q1_fs))
cat("\nFirst-stage F-statistic (instrument relevance):", summary(Q1_fs)$fstatistic["value"], "\n")

# --- (c) Endogeneity Test (Durbin-Wu-Hausman, via AER::ivreg) ---
Q1_ivreg <- ivreg(
  gsav_gni ~ lkalib_ci + lcredit + lngni + lntot + trade +
    urban_pop + agedep_old + asia + latin + oecd + year_factor |
    lkalib_ci + lcredit + lngni_lag + lntot + trade +
    urban_pop + agedep_old + asia + latin + oecd + year_factor,
  data = KALIB.p
)
Q1_ols <- lm(
  gsav_gni ~ lkalib_ci + lcredit + lngni + lntot + trade +
    urban_pop + agedep_old + asia + latin + oecd + year_factor,
  data = KALIB.p
)
Q1_wuhausman <- summary(Q1_ivreg, diagnostics = TRUE)
cat("\n========== Q1(c): IV Regression Diagnostics (Wu-Hausman) ==========\n")
print(Q1_wuhausman$diagnostics)
wh_pvalue <- Q1_wuhausman$diagnostics["Wu-Hausman", "p-value"]
cat("\nWu-Hausman p-value:", wh_pvalue, "\n")

################################################################################
# QUESTION 2: IV ESTIMATION WITH TWO ENDOGENOUS VARIABLES
################################################################################

# --- (a) 2SLS with multiple instruments (lagged lngni, gni_gr, pop_gr) ---
Q2_iv <- iv_robust(
  gsav_gni ~ lkalib_ci + lcredit + lngni + gni_gr + lntot + trade +
    urban_pop + agedep_old + asia + latin + oecd + year_factor |
    lkalib_ci + lcredit + lngni_lag + gni_gr_lag + pop_gr_lag + lntot + trade +
    urban_pop + agedep_old + asia + latin + oecd + year_factor,
  data = KALIB.p,
  clusters = KALIB.p$ccode
)
cat("\n========== Q2(a): 2SLS Results (Two Endogenous Vars) ==========\n")
print(summary(Q2_iv))

# --- (b) Instrument Relevance (First Stage) ---
Q2_fs_lngni <- lm_robust(
  lngni ~ lngni_lag + gni_gr_lag + pop_gr_lag + lkalib_ci + lcredit + lntot +
    trade + urban_pop + agedep_old + asia + latin + oecd + year_factor,
  data = KALIB.p,
  clusters = KALIB.p$ccode
)
cat("\nFirst-stage F-statistic for lngni:", summary(Q2_fs_lngni)$fstatistic["value"], "\n")

Q2_fs_gnigr <- lm_robust(
  gni_gr ~ lngni_lag + gni_gr_lag + pop_gr_lag + lkalib_ci + lcredit + lntot +
    trade + urban_pop + agedep_old + asia + latin + oecd + year_factor,
  data = KALIB.p,
  clusters = KALIB.p$ccode
)
cat("\nFirst-stage F-statistic for gni_gr:", summary(Q2_fs_gnigr)$fstatistic["value"], "\n")

# --- (c) Endogeneity Test (Durbin-Wu-Hausman, via AER::ivreg) ---
Q2_ivreg <- ivreg(
  gsav_gni ~ lkalib_ci + lcredit + lngni + gni_gr + lntot + trade +
    urban_pop + agedep_old + asia + latin + oecd + year_factor |
    lkalib_ci + lcredit + lngni_lag + gni_gr_lag + pop_gr_lag + lntot + trade +
    urban_pop + agedep_old + asia + latin + oecd + year_factor,
  data = KALIB.p
)
Q2_wuhausman <- summary(Q2_ivreg, diagnostics = TRUE)
cat("\n========== Q2(c): IV Regression Diagnostics (Wu-Hausman) ==========\n")
print(Q2_wuhausman$diagnostics)
wh2_pvalue <- Q2_wuhausman$diagnostics["Wu-Hausman", "p-value"]
cat("\nWu-Hausman p-value:", wh2_pvalue, "\n")

################################################################################
# QUESTION 3: PANEL DATA METHODS
################################################################################

Q3_formula <- gsav_gni ~ lkalib_ci + lcredit + lngni + gni_gr + lntot + trade +
  urban_pop + agedep_old + asia + latin + oecd + year_factor

# --- (a) Estimation: OLS, FD, FE, RE ---
Q3_pool <- plm(Q3_formula, data = KALIB.p, model = "pooling")
Q3_fd   <- plm(Q3_formula, data = KALIB.p, model = "fd")
Q3_fe   <- plm(Q3_formula, data = KALIB.p, model = "within")
Q3_re   <- plm(Q3_formula, data = KALIB.p, model = "random")
cat("\n========== Q3(a): Model Comparison ==========\n")
print(summary(Q3_pool))
print(summary(Q3_fd))
print(summary(Q3_fe))
print(summary(Q3_re))

# --- (b) Hausman Test (FE vs RE) ---
Q3_hausman <- phtest(Q3_fe, Q3_re)
cat("\n========== Q3(b): Hausman Test Results ==========\n")
print(Q3_hausman)

# --- (c) Lagged FD instead of lcredit ---
Q3_formula_FD <- gsav_gni ~ lkalib_ci + FD_lag + lngni + gni_gr + lntot + trade +
  urban_pop + agedep_old + asia + latin + oecd + year_factor
Q3_fe_FD <- plm(Q3_formula_FD, data = KALIB.p, model = "within")
cat("\n========== Q3(c): FE with Lagged FD ==========\n")
print(summary(Q3_fe_FD))

# --- (d) 2SLS for Panels: FE and RE ---
Q3_fe_iv <- plm(
  gsav_gni ~ lkalib_ci + lcredit + lngni + gni_gr + lntot + trade +
    urban_pop + agedep_old + asia + latin + oecd + year_factor,
  data = KALIB.p,
  model = "within",
  inst.method = "baltagi",
  instruments = ~ lkalib_ci + lcredit + lngni_lag + gni_gr_lag + lntot + trade +
    urban_pop + agedep_old + asia + latin + oecd + year_factor
)
cat("\n========== Q3(d): 2SLS Fixed Effects (Panel IV) ==========\n")
print(summary(Q3_fe_iv))

Q3_re_iv <- plm(
  gsav_gni ~ lkalib_ci + lcredit + lngni + gni_gr + lntot + trade +
    urban_pop + agedep_old + asia + latin + oecd + year_factor,
  data = KALIB.p,
  model = "random",
  inst.method = "baltagi",
  instruments = ~ lkalib_ci + lcredit + lngni_lag + gni_gr_lag + lntot + trade +
    urban_pop + agedep_old + asia + latin + oecd + year_factor
)
cat("\n========== Q3(d): 2SLS Random Effects (Panel IV) ==========\n")
print(summary(Q3_re_iv))

################################################################################
# VISUALIZATION SECTION: Exploratory and Results Plot
################################################################################

library(ggplot2)  # For all plotting
library(broom)    # For tidy() data frames from models
library(dplyr)    # For data manipulation

# --- Always use a plain data frame for ggplot2 ---
viz_data <- as.data.frame(KALIB.p)
viz_data$year <- as.numeric(as.character(viz_data$year))
viz_data$oecd <- as.factor(viz_data$oecd)
viz_data$ccode <- as.factor(viz_data$ccode)

# --- 1. Visualize Distributions and Trends ---

# A. Time Trend of Savings Rate by Country Group
ggplot(
  viz_data %>% filter(!is.na(gsav_gni) & !is.na(year) & !is.na(oecd)),
  aes(x = year, y = gsav_gni, group = ccode, color = oecd)
) +
  geom_line(alpha = 0.2) +
  stat_summary(
    aes(group = oecd, color = oecd),
    fun = mean,
    geom = "line",
    linewidth = 1.5
  ) +
  labs(
    title = "Trends in Savings Rate by OECD Status",
    x = "Year", y = "Gross Saving / GNI",
    color = "OECD"
  ) +
  theme_minimal()

# B. Distribution of Key Variables (e.g., lngni, lcredit)
ggplot(viz_data, aes(x = lngni)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  labs(title = "Histogram: log(GNI)", x = "lngni", y = "Count") +
  theme_minimal()

ggplot(viz_data, aes(x = lcredit)) +
  geom_density(fill = "skyblue", alpha = 0.5) +
  labs(title = "Density: log(Credit)", x = "lcredit") +
  theme_minimal()

# --- 2. Coefficient Plots (for IV and Panel Regressions) ---

# IV Regression: Plot coefficients and 95% CI for Q1_iv
iv_tidy <- broom::tidy(Q1_iv) %>%
  filter(!grepl("Intercept|year_factor", term))

ggplot(iv_tidy, aes(x = reorder(term, estimate), y = estimate)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.25) +
  coord_flip() +
  labs(title = "IV Coefficient Estimates (Q1 2SLS)",
       x = "Variable", y = "Coefficient Estimate") +
  theme_minimal()

# Panel FE Model: Coefficient Plot (Q3_fe)
fe_tidy <- broom::tidy(Q3_fe) %>%
  filter(!grepl("Intercept|year_factor", term))

ggplot(fe_tidy, aes(x = reorder(term, estimate), y = estimate)) +
  geom_point(size = 2, color = "darkgreen") +
  geom_errorbar(aes(ymin = estimate - 1.96*std.error, ymax = estimate + 1.96*std.error),
                width = 0.25, color = "darkgreen") +
  coord_flip() +
  labs(title = "Fixed Effects Coefficient Estimates",
       x = "Variable", y = "Estimate") +
  theme_minimal()

# --- 3. Actual vs. Fitted for FE Model (Q3_fe) ---
# Use the data *actually used* in FE estimation
fe_frame <- model.frame(Q3_fe)
Q3_fe_fit <- data.frame(
  actual = fe_frame$gsav_gni,
  fitted = as.numeric(fitted(Q3_fe))
)

ggplot(Q3_fe_fit, aes(x = actual, y = fitted)) +
  geom_point(alpha = 0.3, color = "navy") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(title = "Actual vs Fitted Values (Fixed Effects Model)",
       x = "Actual Savings Rate", y = "Fitted Savings Rate") +
  theme_minimal()

# --- 4. Instrument Strength: First-Stage Scatter (lngni vs lngni_lag) ---

ggplot(viz_data, aes(x = lngni_lag, y = lngni)) +
  geom_point(alpha = 0.4, color = "firebrick") +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(title = "First Stage: lngni vs. lngni_lag",
       x = "Lagged log(GNI)", y = "log(GNI)") +
  theme_minimal()

################################################################################
# SAVE MODEL RESULTS TO TABLES (CSV, Excel, LaTeX, TXT)
################################################################################

# --- 0. Load Required Packages ---
library(broom)      # For tidy regression output
library(dplyr)      # For data manipulation
library(writexl)    # For Excel output

# --- 1. Tidy Model Results ---
Q1_iv_tidy  <- broom::tidy(Q1_iv)
Q3_fe_tidy  <- broom::tidy(Q3_fe)
Q3_re_tidy  <- broom::tidy(Q3_re)
# Add any additional models as needed

# --- 2. Save as CSV ---
write.csv(Q1_iv_tidy, "Q1_IV_results.csv", row.names = FALSE)
write.csv(Q3_fe_tidy, "Q3_FE_results.csv", row.names = FALSE)
write.csv(Q3_re_tidy, "Q3_RE_results.csv", row.names = FALSE)

# --- 3. Save as Excel (.xlsx), each model as a separate sheet ---
writexl::write_xlsx(
  list(
    "Q1_IV" = Q1_iv_tidy,
    "Q3_FE" = Q3_fe_tidy,
    "Q3_RE" = Q3_re_tidy
  ),
  "Regression_Results.xlsx"
)

################################################################################
# END OF SCRIPT: Save session info for reproducibility
################################################################################
cat("\n========== SESSION INFO ==========\n")
print(sessionInfo())
