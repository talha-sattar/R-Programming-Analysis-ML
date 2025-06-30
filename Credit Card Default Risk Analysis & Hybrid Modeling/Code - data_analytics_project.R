# ===========================================
#           CREDIT CARD DATA ANALYSIS
# ===========================================

# ---- Create output directory ----
output_dir <- "Graphs_dir"
if (!dir.exists(output_dir)) dir.create(output_dir)

# ---- Load (and if needed, install) packages ----
pkg_list <- c("tidyverse", "data.table", "mice", "corrplot", "ggplot2", "VIM",
              "gridExtra", "dbscan", "naniar", "patchwork", "forcats", "GGally")
for (pkg in pkg_list) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}
if (!require("missForest")) install.packages("missForest")
library(missForest)
if (!require("ranger")) install.packages("ranger")
library(ranger)
if (!require("pROC")) install.packages("pROC")
library(pROC)
if (!require("randomForest")) install.packages("randomForest")
library(randomForest)

# ---- Utility function for saving and showing plots ----
save_and_print_plot <- function(plot, filename, w = 8, h = 6) {
  ggsave(file.path(output_dir, filename), plot, width = w, height = h)
  print(plot)
}

cat("\n===========================================\n")
cat("Q1. AIMS AND HYPOTHESES\n")
cat("===========================================\n")

# ---- Q1: Load and initial exploration ----
data_file <- "default of credit card clients - Data.csv"
raw_data <- tryCatch({
  fread(data_file, stringsAsFactors = FALSE)
}, error = function(e) {
  stop("Could not read file: ", data_file, "\nError: ", e$message)
})

cat("Raw data loaded. Rows:", nrow(raw_data), "Cols:", ncol(raw_data), "\n")

# Visualize Default by Age and Education
p_q1_age <- ggplot(raw_data, aes(x = AGE, fill = factor(`default payment next month`))) +
  geom_histogram(bins = 30, position = "fill") +
  labs(title = "Default Rate by Age (Raw Data)", fill = "Default Next Month")
save_and_print_plot(p_q1_age, "Q1_default_by_age.png")

p_q1_edu <- ggplot(raw_data, aes(x = factor(EDUCATION), fill = factor(`default payment next month`))) +
  geom_bar(position = "fill") +
  labs(title = "Default Rate by Education (Raw Data)", fill = "Default Next Month") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
save_and_print_plot(p_q1_edu, "Q1_default_by_education.png")

cat("\n===========================================\n")
cat("Q2. DATA GATHERING\n")
cat("===========================================\n")

var_classes <- sapply(raw_data, class)
print(data.frame(Variable = names(var_classes), Class = var_classes))
write.csv(data.frame(Variable = names(var_classes), Class = var_classes),
          file.path(output_dir, "Q2_column_types.csv"), row.names = FALSE)
raw_summary <- summary(raw_data)
capture.output(raw_summa8ry, file = file.path(output_dir, "Q2_raw_summary.csv"))
cat("Summary stats and column types saved/printed above.\n")

cat("\n===========================================\n")
cat("Q3. DATA CHECKING\n")
cat("===========================================\n")

missing_values <- colSums(is.na(raw_data))
print(missing_values)
write.csv(missing_values, file.path(output_dir, "Q3_missing_by_col.csv"))

# Visualize missing data
p_q3_missvar <- gg_miss_var(raw_data) + labs(title = "Missing by Variable (Raw Data)")
save_and_print_plot(p_q3_missvar, "Q3_missing_by_variable.png")
raw_data$missing_pct <- rowMeans(is.na(raw_data)) * 100
p_q3_rowmiss <- ggplot(raw_data, aes(x = missing_pct)) +
  geom_histogram(bins = 20, fill = "orange") +
  labs(title = "Histogram: % Missing Values per Row", x = "Percent Missing", y = "Row Count")
save_and_print_plot(p_q3_rowmiss, "Q3_row_missing_pct.png")

cat("Duplicates in data:", sum(duplicated(raw_data)), "\n")
writeLines(paste("Duplicate rows:", sum(duplicated(raw_data))),
           file.path(output_dir, "Q3_duplicate_info.txt"))

# Show and save education and marriage codes
print(table(raw_data$EDUCATION))
write.csv(table(raw_data$EDUCATION), file.path(output_dir, "Q3_education_levels.csv"))
print(table(raw_data$MARRIAGE))
write.csv(table(raw_data$MARRIAGE), file.path(output_dir, "Q3_marriage_levels.csv"))

cat("\n===========================================\n")
cat("Q4. DATA MANIPULATION AND CLEANING\n")
cat("===========================================\n")

clean_education <- function(x) {
  factor(ifelse(x %in% c(0, 4, 5, 6), 4, x),
         levels = c(1, 2, 3, 4),
         labels = c("Graduate", "University", "High School", "Others"))
}
clean_marriage <- function(x) {
  factor(ifelse(x %in% c(0, 3), 3, x),
         levels = c(1, 2, 3),
         labels = c("Married", "Single", "Others"))
}

cleaned_data <- raw_data %>%
  mutate(
    SEX = factor(SEX, levels = c(1, 2), labels = c("Male", "Female")),
    EDUCATION = clean_education(EDUCATION),
    MARRIAGE = clean_marriage(MARRIAGE),
    `default payment next month` = factor(`default payment next month`, levels = c(0, 1), labels = c("No", "Yes"))
  ) %>%
  filter(AGE >= 18 & AGE <= 100)

# Save before/after tables for evidence
edu_raw_table <- as.data.frame(table(raw_data$EDUCATION), stringsAsFactors = FALSE)
edu_clean_table <- as.data.frame(table(cleaned_data$EDUCATION), stringsAsFactors = FALSE)
colnames(edu_raw_table) <- c("Level", "Raw_Count")
colnames(edu_clean_table) <- c("Level", "Cleaned_Count")
edu_merge <- merge(edu_raw_table, edu_clean_table, by = "Level", all = TRUE)
edu_merge[is.na(edu_merge)] <- 0
print(edu_merge)
write.csv(edu_merge, file.path(output_dir, "Q4_education_before_after.csv"), row.names = FALSE)

mar_raw_table <- as.data.frame(table(raw_data$MARRIAGE), stringsAsFactors = FALSE)
mar_clean_table <- as.data.frame(table(cleaned_data$MARRIAGE), stringsAsFactors = FALSE)
colnames(mar_raw_table) <- c("Level", "Raw_Count")
colnames(mar_clean_table) <- c("Level", "Cleaned_Count")
mar_merge <- merge(mar_raw_table, mar_clean_table, by = "Level", all = TRUE)
mar_merge[is.na(mar_merge)] <- 0
print(mar_merge)
write.csv(mar_merge, file.path(output_dir, "Q4_marriage_before_after.csv"), row.names = FALSE)

# Handle out-of-range values in PAY_0–PAY_6
pay_cols <- paste0("PAY_", c(0, 2:6))
for (col in pay_cols) cleaned_data[[col]][cleaned_data[[col]] < -2 | cleaned_data[[col]] > 9] <- NA

# Visualize missingness after cleaning
p_q4_miss <- gg_miss_var(cleaned_data) + labs(title = "Missing After Cleaning")
save_and_print_plot(p_q4_miss, "Q4_missing_after_cleaning.png")

# Impute missing PAY_x with MICE
if (anyNA(cleaned_data[, ..pay_cols])) {
  mice_imp <- mice(cleaned_data[, ..pay_cols], m = 1, method = "pmm", maxit = 5, seed = 123)
  cleaned_data[, (pay_cols) := complete(mice_imp)]
}
imputed_nas <- sapply(cleaned_data[, ..pay_cols], function(x) sum(is.na(x)))
print(imputed_nas)
write.csv(imputed_nas, file.path(output_dir, "Q4_post_imputation_missing.csv"))

# Remove duplicates
cleaned_data <- distinct(cleaned_data)

# Show a cleaned variable distribution
p_clean_age <- ggplot(cleaned_data, aes(x = AGE)) +
  geom_histogram(bins = 30, fill = "coral", alpha = 0.7) +
  labs(title = "Cleaned Age Distribution")
save_and_print_plot(p_clean_age, "Q4_cleaned_age.png")

cat("\n===========================================\n")
cat("Q5. EXPLORATORY ANALYSIS\n")
cat("===========================================\n")

# ---- 1. DEMOGRAPHIC DISTRIBUTIONS ----

# Age distribution (histogram)
p_age_hist <- ggplot(cleaned_data, aes(x = AGE)) +
  geom_histogram(bins = 30, fill = "steelblue", alpha = 0.7) +
  labs(title = "Age Distribution", x = "Age", y = "Count")
save_and_print_plot(p_age_hist, "EDA_age_hist.png")

# Gender distribution (bar)
p_sex <- ggplot(cleaned_data, aes(x = SEX)) +
  geom_bar(fill = "pink", alpha = 0.7) +
  labs(title = "Gender Distribution", x = "Gender", y = "Count")
save_and_print_plot(p_sex, "EDA_gender_bar.png")

# Education distribution (bar)
p_edu <- ggplot(cleaned_data, aes(x = EDUCATION)) +
  geom_bar(fill = "orange", alpha = 0.7) +
  labs(title = "Education Distribution", x = "Education", y = "Count")
save_and_print_plot(p_edu, "EDA_education_bar.png")

# Marital Status (bar)
p_mar <- ggplot(cleaned_data, aes(x = MARRIAGE)) +
  geom_bar(fill = "green", alpha = 0.7) +
  labs(title = "Marital Status Distribution", x = "Marital Status", y = "Count")
save_and_print_plot(p_mar, "EDA_marital_bar.png")

# ---- 2. DEFAULT RATE BY DEMOGRAPHICS ----

# By Gender
p_def_sex <- ggplot(cleaned_data, aes(x = SEX, fill = `default payment next month`)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Default Rate by Gender", x = "Gender", y = "Proportion", fill = "Default")
save_and_print_plot(p_def_sex, "EDA_default_by_gender.png")

# By Education
p_def_edu <- ggplot(cleaned_data, aes(x = EDUCATION, fill = `default payment next month`)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Default Rate by Education", x = "Education", y = "Proportion", fill = "Default")
save_and_print_plot(p_def_edu, "EDA_default_by_education.png")

# By Marital Status
p_def_mar <- ggplot(cleaned_data, aes(x = MARRIAGE, fill = `default payment next month`)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Default Rate by Marital Status", x = "Marital Status", y = "Proportion", fill = "Default")
save_and_print_plot(p_def_mar, "EDA_default_by_marital.png")

# By Age Group (bin ages)
cleaned_data$AGE_GROUP <- cut(cleaned_data$AGE, breaks=c(18,25,35,45,55,65,100), 
                              labels=c("18-24","25-34","35-44","45-54","55-64","65+"))
p_def_agegrp <- ggplot(cleaned_data, aes(x = AGE_GROUP, fill = `default payment next month`)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Default Rate by Age Group", x = "Age Group", y = "Proportion", fill = "Default")
save_and_print_plot(p_def_agegrp, "EDA_default_by_agegrp.png")

# ---- 3. CREDIT LIMIT AND BILLS ----

p_lim <- ggplot(cleaned_data, aes(x = LIMIT_BAL)) +
  geom_histogram(bins = 40, fill = "purple", alpha = 0.7) +
  labs(title = "Credit Limit Distribution", x = "Credit Limit", y = "Count")
save_and_print_plot(p_lim, "EDA_credit_limit_hist.png")

# Bills & Payments: trends and relationships
p_bill_pay <- ggpairs(cleaned_data, columns = which(names(cleaned_data) %in% c("BILL_AMT1", "BILL_AMT2", "PAY_AMT1", "PAY_AMT2", "LIMIT_BAL")),
                      aes(color = `default payment next month`),
                      upper = list(continuous = wrap("cor", size = 3))) +
  labs(title = "Pairs Plot: Bills, Payments, Credit Limit")
save_and_print_plot(p_bill_pay, "EDA_pairs_bills_pay.png", 12, 12)

# Boxplot of LIMIT_BAL by Education and Default
p_lim_edu <- ggplot(cleaned_data, aes(x = EDUCATION, y = LIMIT_BAL, fill = `default payment next month`)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Credit Limit by Education & Default", x = "Education", y = "Credit Limit")
save_and_print_plot(p_lim_edu, "EDA_limit_by_edu_default.png")

# ---- 4. TIME SERIES-LIKE TRENDS (PAY_x/BILL_AMT_x/PAY_AMT_x) ----

# Melt for line plots per customer not practical (too many rows),
# but we can show mean per month for everyone:
pay_melt <- cleaned_data %>% 
  select(starts_with("PAY_"), `default payment next month`) %>% 
  mutate(id = row_number()) %>% 
  pivot_longer(cols = starts_with("PAY_"), names_to = "Month", values_to = "Status")
pay_trend <- pay_melt %>%
  group_by(Month) %>%
  summarise(mean_status = mean(Status, na.rm=TRUE))
p_paytrend <- ggplot(pay_trend, aes(x = Month, y = mean_status, group=1)) +
  geom_line(color="blue", size=1.2) +
  labs(title = "Mean Payment Status by Month", x = "Month", y = "Mean Payment Status")
save_and_print_plot(p_paytrend, "EDA_mean_pay_status_by_month.png")

# ---- 5. OUTLIER AND SKEWNESS CHECKS ----

# Define num_cols if not already
num_cols <- c("LIMIT_BAL", "AGE", 
              paste0("BILL_AMT", 1:6), 
              paste0("PAY_AMT", 1:6))

if (!require("reshape2")) install.packages("reshape2")
library(reshape2)
cleaned_long <- melt(cleaned_data, measure.vars = num_cols)

p_all_box <- ggplot(cleaned_long, aes(x = variable, y = value)) +
  geom_boxplot(alpha = 0.7, outlier.colour = "red") +
  labs(title = "Boxplots of All Numeric Features") +
  theme(axis.text.x = element_text(angle=45, hjust=1))

save_and_print_plot(p_all_box, "EDA_all_numeric_boxplots.png", 13, 8)
print(p_all_box)


# ---- 6. CLASS IMBALANCE ----

p_def <- ggplot(cleaned_data, aes(x = `default payment next month`, fill = `default payment next month`)) +
  geom_bar() +
  labs(title = "Class Distribution: Default vs No Default", x = "Default Next Month", y = "Count")
save_and_print_plot(p_def, "EDA_class_balance.png")

cat("A Comprehensive exploratory data analysis completed. Check the 'Graphs_dir' folder for plots.\n")

cat("\n===========================================\n")
cat("Q8. STATE-OF-THE-ART: EXTREMELY RANDOMIZED TREES (Islam et al. via ranger)\n")
cat("===========================================\n")

set.seed(123)
q8_sample <- cleaned_data[sample(1:nrow(cleaned_data), min(5000, nrow(cleaned_data))), ]
q8_X <- q8_sample %>%
  select(-ID, -`default payment next month`) %>%
  mutate_if(is.factor, as.numeric)
q8_y <- as.numeric(q8_sample$`default payment next month`) - 1  # 0/1

idx <- sample(1:nrow(q8_X), 0.8 * nrow(q8_X))
X_train <- q8_X[idx, ]
y_train <- q8_y[idx]
X_test <- q8_X[-idx, ]
y_test <- q8_y[-idx]

# Extremely Randomized Trees (via ranger)
train_df <- data.frame(X_train, y = as.factor(y_train))
test_df <- data.frame(X_test)
et_model <- ranger(y ~ ., data = train_df, num.trees = 100, splitrule = "extratrees", probability = TRUE, seed = 123)
et_pred <- predict(et_model, test_df)$predictions[, "1"]
et_pred_class <- as.numeric(et_pred > 0.5)

# Random Forest for comparison
rf_model <- randomForest(x = X_train, y = as.factor(y_train), ntree = 100)
rf_pred <- predict(rf_model, X_test, type = "prob")[,2]
rf_pred_class <- as.numeric(rf_pred > 0.5)

q8_metrics <- function(truth, pred_prob, pred_class) {
  accuracy <- mean(truth == pred_class)
  recall <- sum(truth == 1 & pred_class == 1) / sum(truth == 1)
  precision <- sum(truth == 1 & pred_class == 1) / sum(pred_class == 1)
  f1 <- ifelse((precision + recall) > 0, 2 * (precision * recall) / (precision + recall), NA)
  auc <- pROC::auc(pROC::roc(truth, pred_prob))
  c(Accuracy = accuracy, Recall = recall, Precision = precision, F1 = f1, AUC = as.numeric(auc))
}
et_metrics <- q8_metrics(y_test, et_pred, et_pred_class)
rf_metrics <- q8_metrics(y_test, rf_pred, rf_pred_class)
print(rbind(ExtremelyRandomizedTrees = et_metrics, RandomForest = rf_metrics))
write.csv(rbind(ExtremelyRandomizedTrees = et_metrics, RandomForest = rf_metrics),
          file.path(output_dir, "Q8_ET_vs_RF_metrics.csv"))

et_roc <- roc(y_test, et_pred)
rf_roc <- roc(y_test, rf_pred)
png(file.path(output_dir, "Q8_ET_RF_ROC.png"), width = 900, height = 700)
plot(et_roc, col = "blue", lwd = 2, main = "ROC: Extremely Randomized Trees vs Random Forest")
lines(rf_roc, col = "red", lwd = 2)
legend("bottomright", legend = c("Extremely Randomized Trees", "Random Forest"),
       col = c("blue", "red"), lwd = 2)
dev.off()

cat("\n===========================================\n")
cat("Q9. NEW APPROACH: Hybrid Model-based + Heuristic Risk\n")
cat("===========================================\n")

# Get offline risk for all samples in q8_sample
all_X <- q8_X
all_offline_pred <- predict(et_model, data.frame(all_X))$predictions[, "1"]
q9_offline_risk <- all_offline_pred
q9_online_risk <- with(q8_sample,
                       ifelse((PAY_AMT1 < 0.1 * BILL_AMT1) | (BILL_AMT1 > LIMIT_BAL), 1, 0))
lambda <- 0.5
q9_combined_risk <- lambda * q9_online_risk + (1 - lambda) * q9_offline_risk
q9_pred_class <- as.numeric(q9_combined_risk > 0.5)
combined_metrics <- q8_metrics(q8_y, q9_combined_risk, q9_pred_class)
print(rbind(
  ExtremelyRandomizedTrees = et_metrics,
  RandomForest = rf_metrics,
  HybridCombinedRisk = combined_metrics))
write.csv(rbind(
  ExtremelyRandomizedTrees = et_metrics,
  RandomForest = rf_metrics,
  HybridCombinedRisk = combined_metrics),
  file.path(output_dir, "Q9_hybrid_vs_ET_RF_metrics.csv"))

# Risk distributions
risk_df <- data.frame(
  OfflineRisk = q9_offline_risk,
  OnlineRisk = q9_online_risk,
  CombinedRisk = q9_combined_risk,
  Default = q8_sample$`default payment next month`
)
p_q9_off <- ggplot(risk_df, aes(x = OfflineRisk, fill = Default)) +
  geom_density(alpha = 0.5) +
  labs(title = "Distribution of Offline Risk Scores (Extremely Randomized Trees)", fill = "Default")
save_and_print_plot(p_q9_off, "Q9_offline_risk_dist.png")

p_q9_comb <- ggplot(risk_df, aes(x = CombinedRisk, fill = Default)) +
  geom_density(alpha = 0.5) +
  labs(title = "Distribution of Combined Risk Scores (Hybrid Approach)", fill = "Default")
save_and_print_plot(p_q9_comb, "Q9_combined_risk_dist.png")

cat("\n===========================================\n")
cat("All results are saved in Graphs_dir and printed above.\n")
cat("Paper for Q8/Q9 reference: Islam, S. R., Eberle, W., & Ghafoor, S. K. Credit Default Mining Using Combined Machine Learning and Heuristic Approach.\n")
cat("===========================================\n")

