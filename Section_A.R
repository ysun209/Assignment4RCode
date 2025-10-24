# Section A - Task 1
# Script: Section_A_Task1.R
# Purpose: Perform the analyses requested in Section A Task 1 of Assignment 4
# Notes: Assumes the CSV file "Section A Dataset - Anaemia_final.csv" is in the
# same directory as this script. Outputs summary tables to console and saves
# plots as PNG files in the working directory.

# Load tidyverse for data manipulation and ggplot2 for plotting
if (!requireNamespace("tidyverse", quietly = TRUE)) {
  install.packages("tidyverse", repos = "https://cloud.r-project.org")
}
library(tidyverse)

# Optional: for skewness calculation
if (!requireNamespace("e1071", quietly = TRUE)) {
  install.packages("e1071", repos = "https://cloud.r-project.org")
}
library(e1071)

# Read the data (try a few sensible relative paths)
possible_paths <- c(
  "Section A Dataset - Anaemia_final.csv",
  "Section A Dataset - Anaemia_final .csv",
  "Anaemia_final.csv",
  "Section A Dataset - Anaemia_final.csv"
)
ana_path <- possible_paths[1]
if (!file.exists(ana_path)) {
  stop(paste0("Could not find the anaemia CSV at expected path: ", ana_path,
              "\nPlease place 'Section A Dataset - Anaemia_final.csv' in the script directory."))
}

ana <- read_csv(ana_path, show_col_types = FALSE)

# Quick look
cat("Rows:", nrow(ana), "Columns:", ncol(ana), "\n")

# Ensure column names expected are present
expected_cols <- c("pt_id","age","gender","ethnic_gp","heart_rate","sbp","dbp",
                   "haemoglobin","heart_rate_6m","sbp_6m","dbp_6m","haemoglobin_6m")
missing_cols <- setdiff(expected_cols, names(ana))
if (length(missing_cols) > 0) {
  warning("Missing expected columns: ", paste(missing_cols, collapse = ", "))
}

# Make sure gender and ethnic_gp are in sensible formats
ana <- ana %>%
  mutate(
    gender = toupper(as.character(gender)),
    ethnic_gp = as.integer(ethnic_gp)
  )

# Helper: mean (SD) formatting
mean_sd <- function(x, na.rm = TRUE, digits = 1) {
  m <- mean(x, na.rm = na.rm)
  s <- sd(x, na.rm = na.rm)
  if (is.na(m) & is.na(s)) return(NA_character_)
  sprintf(paste0("%0.", digits, "f (%0.", digits, "f)"), m, s)
}

median_iqr <- function(x, na.rm = TRUE, digits = 1) {
  med <- median(x, na.rm = na.rm)
  iqr <- IQR(x, na.rm = na.rm)
  sprintf(paste0("%0.", digits, "f (IQR %0.", digits, "f)"), med, iqr)
}

# Create change variables
ana <- ana %>%
  mutate(
    change_haemoglobin = haemoglobin - haemoglobin_6m,
    change_heart_rate = heart_rate - heart_rate_6m,
    change_sbp = sbp - sbp_6m
  )

# Summary function to compute Mean (SD) for numeric vars by gender and total
summarise_by_gender <- function(df, var) {
  df %>%
    summarise(
      Total = mean_sd(.data[[var]]),
      Women = mean_sd(.data[[var]][gender == "F"]),
      Men = mean_sd(.data[[var]][gender == "M"])
    ) %>%
    pivot_longer(everything(), names_to = "Group", values_to = var)
}

# Build the cohort characteristics table
cohort_table <- tibble(Item = character(), Total = character(), Women = character(), Men = character())

# n
n_total <- nrow(ana)
n_women <- sum(ana$gender == "F", na.rm = TRUE)
n_men <- sum(ana$gender == "M", na.rm = TRUE)
cohort_table <- cohort_table %>% add_row(Item = "n", Total = as.character(n_total), Women = as.character(n_women), Men = as.character(n_men))

# Age
cohort_table <- cohort_table %>% add_row(Item = "Age, years (mean (SD))",
                                         Total = mean_sd(ana$age),
                                         Women = mean_sd(ana$age[ana$gender=="F"]),
                                         Men = mean_sd(ana$age[ana$gender=="M"]))

# Ethnic group: show n(%) for each group; we'll add rows for each category
eth_tab <- ana %>% count(ethnic_gp) %>% mutate(pct = 100 * n / sum(n))
# Map ethnic codes to labels (as in assignment)
eth_labels <- c('1'='European','2'='Pacific Peoples','3'='Māori','4'='Chinese')
eth_rows <- eth_tab %>% mutate(label = eth_labels[as.character(ethnic_gp)]) %>%
  transmute(Item = paste0('Ethnic: ', label), Total = paste0(n, ' (', round(pct,1), '%)'), Women = "", Men = "")
cohort_table <- bind_rows(cohort_table, eth_rows)

# At baseline: heart_rate, sbp, haemoglobin
cohort_table <- cohort_table %>% add_row(Item = "\n_At baseline_", Total = "", Women = "", Men = "")
cohort_table <- cohort_table %>% add_row(Item = "Heart rate, bpm (mean (SD))",
                                         Total = mean_sd(ana$heart_rate),
                                         Women = mean_sd(ana$heart_rate[ana$gender=="F"]),
                                         Men = mean_sd(ana$heart_rate[ana$gender=="M"]))
cohort_table <- cohort_table %>% add_row(Item = "Systolic blood pressure, mmHg (mean (SD))",
                                         Total = mean_sd(ana$sbp),
                                         Women = mean_sd(ana$sbp[ana$gender=="F"]),
                                         Men = mean_sd(ana$sbp[ana$gender=="M"]))
cohort_table <- cohort_table %>% add_row(Item = "Haemoglobin, g/L (mean (SD))",
                                         Total = mean_sd(ana$haemoglobin),
                                         Women = mean_sd(ana$haemoglobin[ana$gender=="F"]),
                                         Men = mean_sd(ana$haemoglobin[ana$gender=="M"]))

# At 6 months
cohort_table <- cohort_table %>% add_row(Item = "\n_At 6 months post-baseline_", Total = "", Women = "", Men = "")
cohort_table <- cohort_table %>% add_row(Item = "Heart rate, bpm (mean (SD))",
                                         Total = mean_sd(ana$heart_rate_6m),
                                         Women = mean_sd(ana$heart_rate_6m[ana$gender=="F"]),
                                         Men = mean_sd(ana$heart_rate_6m[ana$gender=="M"]))
cohort_table <- cohort_table %>% add_row(Item = "Systolic blood pressure, mmHg (mean (SD))",
                                         Total = mean_sd(ana$sbp_6m),
                                         Women = mean_sd(ana$sbp_6m[ana$gender=="F"]),
                                         Men = mean_sd(ana$sbp_6m[ana$gender=="M"]))
cohort_table <- cohort_table %>% add_row(Item = "Haemoglobin, g/L (mean (SD))",
                                         Total = mean_sd(ana$haemoglobin_6m),
                                         Women = mean_sd(ana$haemoglobin_6m[ana$gender=="F"]),
                                         Men = mean_sd(ana$haemoglobin_6m[ana$gender=="M"]))

# Change over 6 months
cohort_table <- cohort_table %>% add_row(Item = "\n_Change in values over 6 months_", Total = "", Women = "", Men = "")
cohort_table <- cohort_table %>% add_row(Item = "Change Heart rate, bpm (mean (SD))",
                                         Total = mean_sd(ana$change_heart_rate),
                                         Women = mean_sd(ana$change_heart_rate[ana$gender=="F"]),
                                         Men = mean_sd(ana$change_heart_rate[ana$gender=="M"]))
cohort_table <- cohort_table %>% add_row(Item = "Change Systolic blood pressure, mmHg (mean (SD))",
                                         Total = mean_sd(ana$change_sbp),
                                         Women = mean_sd(ana$change_sbp[ana$gender=="F"]),
                                         Men = mean_sd(ana$change_sbp[ana$gender=="M"]))
cohort_table <- cohort_table %>% add_row(Item = "Change Haemoglobin, g/L (mean (SD))",
                                         Total = mean_sd(ana$change_haemoglobin),
                                         Women = mean_sd(ana$change_haemoglobin[ana$gender=="F"]),
                                         Men = mean_sd(ana$change_haemoglobin[ana$gender=="M"]))

# Print the cohort table
cat("\nCohort characteristics table:\n")
print(cohort_table)

# Task 2: What percentage of the cohort are Māori?
pct_maori <- 100 * mean(ana$ethnic_gp == 3, na.rm = TRUE)
cat(sprintf("\nTask 2: Percentage Māori = %0.1f%%\n", pct_maori))

# Task 3: What percentage of Māori are women?
maori <- ana %>% filter(ethnic_gp == 3)
pct_maori_women <- 100 * mean(maori$gender == "F", na.rm = TRUE)
cat(sprintf("Task 3: Percentage of Māori who are women = %0.1f%%\n", pct_maori_women))

# Task 4: Boxplot of age stratified by ethnic group
png("task4_age_by_ethnic_group_boxplot.png", width = 800, height = 600)
boxplot(age ~ factor(ethnic_gp, labels = c('European','Pacific Peoples','Māori','Chinese')), data = ana,
  xlab = "Ethnic group", ylab = "Age (years)", main = "Age by ethnic group",  col = c("lightblue", "lightgreen", "lightcoral", "lightgoldenrod"))
dev.off()
cat("Saved boxplot: task4_age_by_ethnic_group_boxplot.png\n")

# Task 5: Histograms of age stratified by gender (base R hist)
# Prepare age vectors for each gender and define common breaks (5-year bins)
ages_f <- na.omit(ana$age[ana$gender == 'F'])
ages_m <- na.omit(ana$age[ana$gender == 'M'])
min_age <- floor(min(ana$age, na.rm = TRUE))
max_age <- ceiling(max(ana$age, na.rm = TRUE))
breaks_age <- seq(min_age, max_age, by = 5)

png("task5_age_hist_by_gender.png", width = 800, height = 600)
par(mfrow = c(1,2), mar = c(4,4,3,1))
hist(ages_f, breaks = breaks_age, col = 'lightcoral', border = 'white',
  xlab = 'Age (years)', ylab = 'Frequency', main = 'Age Distribution - Women')
hist(ages_m, breaks = breaks_age, col = 'steelblue', border = 'white',
  xlab = 'Age (years)', ylab = 'Frequency', main = 'Age Distribution - Men')
dev.off()
cat("Saved histograms: task5_age_hist_by_gender.png\n")

# Task 6: Skewness for men's age (submission-ready answer)
ages_m <- ana$age[ana$gender == 'M']
age_men_skew <- e1071::skewness(ages_m, na.rm = TRUE)
if (is.na(age_men_skew)) {
  cat("Task 6 (answer): Could not compute men's age skewness (no non-missing male ages).\n")
} else {
  skew_direction <- ifelse(age_men_skew > 0, 'right (positive skew)', ifelse(age_men_skew < 0, 'left (negative skew)', 'approximately symmetric'))
  skew_word <- if (age_men_skew > 0) 'right-skewed' else if (age_men_skew < 0) 'left-skewed' else 'approximately symmetric'
  cat(sprintf("Task 6 (answer): Men's age skewness = %0.3f (%s). Conclusion: The distribution of men's ages is %s.\n", 
              age_men_skew, skew_direction, skew_word))
}

# Task 7: Difference in mean haemoglobin between European (1) and Chinese (4)
hb_eu <- ana %>% filter(ethnic_gp == 1) %>% pull(haemoglobin)
hb_ch <- ana %>% filter(ethnic_gp == 4) %>% pull(haemoglobin)

# Calculate descriptive statistics for each group
n_eu <- length(na.omit(hb_eu))
n_ch <- length(na.omit(hb_ch))
mean_eu <- mean(hb_eu, na.rm = TRUE)
sd_eu <- sd(hb_eu, na.rm = TRUE)
mean_ch <- mean(hb_ch, na.rm = TRUE)
sd_ch <- sd(hb_ch, na.rm = TRUE)
mean_diff <- mean_eu - mean_ch

# Two-sample t-test (Welch)
ttest_hb <- t.test(hb_eu, hb_ch)

cat("\n========== Task 7: Mean Haemoglobin Comparison (European vs Chinese) ==========\n")
cat(sprintf("European (n=%d): Mean = %0.2f g/L (SD = %0.2f)\n", n_eu, mean_eu, sd_eu))
cat(sprintf("Chinese (n=%d): Mean = %0.2f g/L (SD = %0.2f)\n", n_ch, mean_ch, sd_ch))
cat(sprintf("Difference in means: %0.2f g/L\n", mean_diff))
cat(sprintf("\nWelch Two-Sample t-test Results:\n"))
cat(sprintf("  t-statistic = %0.4f\n", ttest_hb$statistic))
cat(sprintf("  Degrees of freedom = %0.2f\n", ttest_hb$parameter))
cat(sprintf("  p-value = %0.4f\n", ttest_hb$p.value))
cat(sprintf("  95%% CI for difference: [%0.2f, %0.2f]\n", ttest_hb$conf.int[1], ttest_hb$conf.int[2]))

cat("\nInterpretation: ")
if (ttest_hb$p.value < 0.05) {
  cat(sprintf("Using a Welch two-sample t-test comparing European (mean = %0.2f g/L, SD = %0.2f) and Chinese (mean = %0.2f g/L, SD = %0.2f) groups, there is a statistically significant difference in mean haemoglobin (t = %0.4f, df = %0.2f, p = %0.4f), with Europeans having %0.2f g/L %s haemoglobin (95%% CI: [%0.2f, %0.2f]).\n", 
              mean_eu, sd_eu, mean_ch, sd_ch, ttest_hb$statistic, ttest_hb$parameter, ttest_hb$p.value, abs(mean_diff), ifelse(mean_diff > 0, "higher", "lower"), ttest_hb$conf.int[1], ttest_hb$conf.int[2]))
} else {
  cat(sprintf("Using a Welch two-sample t-test comparing European (mean = %0.2f g/L, SD = %0.2f) and Chinese (mean = %0.2f g/L, SD = %0.2f) groups, there is no statistically significant difference in mean haemoglobin (t = %0.4f, df = %0.2f, p = %0.4f; 95%% CI: [%0.2f, %0.2f]).\n", 
              mean_eu, sd_eu, mean_ch, sd_ch, ttest_hb$statistic, ttest_hb$parameter, ttest_hb$p.value, ttest_hb$conf.int[1], ttest_hb$conf.int[2]))
}

# Task 8: Change in Hb (baseline - 6 months) and its mean (SD)
mean_change_hb <- mean_sd(ana$change_haemoglobin)
cat(sprintf("\nTask 8: Mean (SD) of change in Hb (baseline - 6m) = %s g/L\n", mean_change_hb))

# Task 9: Was mean change in Hb higher for women or men?
mean_change_women <- mean(ana$change_haemoglobin[ana$gender=='F'], na.rm = TRUE)
mean_change_men <- mean(ana$change_haemoglobin[ana$gender=='M'], na.rm = TRUE)
which_higher <- ifelse(mean_change_women > mean_change_men, 'Women', ifelse(mean_change_men > mean_change_women, 'Men', 'Equal'))
cat(sprintf("Task 9: Mean change in Hb - Women: %0.2f, Men: %0.2f. Higher for: %s\n", mean_change_women, mean_change_men, which_higher))

# Task 10: % of women with decrease in Hb > 15 g/L (i.e., change_haemoglobin > 15)
women <- ana %>% filter(gender == 'F')
pct_women_decrease15 <- 100 * mean(women$change_haemoglobin > 15, na.rm = TRUE)
cat(sprintf("\nTask 10: Percentage of women with decrease in Hb > 15 g/L = %0.1f%%\n", pct_women_decrease15))

# Task 11: Create anaemia variables at baseline and 6 months (Yes/No/Missing)
classify_anaemia <- function(hb, gender) {
  res <- rep(NA_character_, length(hb))
  is_na <- is.na(hb) | is.na(gender)
  res[is_na] <- 'Missing'
  # For females: hb < 115
  fem <- (gender == 'F') & !is_na
  res[fem & hb < 115] <- 'Yes'
  res[fem & hb >= 115] <- 'No'
  # For males: hb < 130
  mal <- (gender == 'M') & !is_na
  res[mal & hb < 130] <- 'Yes'
  res[mal & hb >= 130] <- 'No'
  res
}

ana <- ana %>% mutate(
  anaemia_baseline = classify_anaemia(haemoglobin, gender),
  anaemia_6m = classify_anaemia(haemoglobin_6m, gender)
)

# Tabulate gender against anaemia at baseline
tab_gender_anaemia <- table(ana$gender, ana$anaemia_baseline, useNA = 'ifany')
cat("\nTask 11: Table - gender vs anaemia at baseline:\n")
print(tab_gender_anaemia)

# Task 12: Median and IQR of age at baseline for people with anaemia
anaemic_people <- ana %>% filter(anaemia_baseline == 'Yes')
median_age_anaemic <- median(anaemic_people$age, na.rm = TRUE)
iqr_age_anaemic <- IQR(anaemic_people$age, na.rm = TRUE)
cat(sprintf("\nTask 12: Median age (IQR) for people with anaemia = %0.1f (IQR %0.1f) years\n", median_age_anaemic, iqr_age_anaemic))

# Task 13: Among people with anaemia, is median age higher for women or men?
med_age_by_gender_anaemic <- anaemic_people %>% group_by(gender) %>% summarise(med = median(age, na.rm = TRUE))
cat("Task 13: Median ages among anaemic people by gender:\n")
print(med_age_by_gender_anaemic)
higher_median_gender <- med_age_by_gender_anaemic$gender[which.max(med_age_by_gender_anaemic$med)]
cat(sprintf("Among people with anaemia, median age is higher for: %s\n", higher_median_gender))

# Task 14: % of people with anaemia at baseline who did not still have anaemia at 6 months
ana_baseline_yes <- ana %>% filter(anaemia_baseline == 'Yes')
pct_recovered <- 100 * mean(ana_baseline_yes$anaemia_6m != 'Yes', na.rm = TRUE)
cat(sprintf("\nTask 14: %% of baseline-anaemic who no longer had anaemia at 6 months = %0.1f%%\n", pct_recovered))

# Task 15: % of people who did not have anaemia at baseline who developed anaemia at 6 months
ana_baseline_no <- ana %>% filter(anaemia_baseline == 'No')
pct_new_anaemia <- 100 * mean(ana_baseline_no$anaemia_6m == 'Yes', na.rm = TRUE)
cat(sprintf("Task 15: %% of baseline-non-anaemic who developed anaemia at 6 months = %0.1f%%\n", pct_new_anaemia))

# Task 16: One-sentence on whether there has been an increase in anaemia
prev_baseline <- 100 * mean(ana$anaemia_baseline == 'Yes', na.rm = TRUE)
prev_6m <- 100 * mean(ana$anaemia_6m == 'Yes', na.rm = TRUE)
cat(sprintf("\nTask 16: Prevalence anaemia baseline = %0.1f%%; at 6 months = %0.1f%%.\n", prev_baseline, prev_6m))
if (prev_6m > prev_baseline) {
  cat("There appears to be an increase in anaemia prevalence at 6 months compared with baseline (observational; no formal test performed).\n")
} else if (prev_6m < prev_baseline) {
  cat("There appears to be a decrease in anaemia prevalence at 6 months compared with baseline (observational; no formal test performed).\n")
} else {
  cat("The prevalence of anaemia is unchanged between baseline and 6 months.\n")
}

# Save the cohort table as CSV for inclusion in a report
write_csv(cohort_table, "task1_cohort_characteristics_table_sectionA_task1.csv")
cat("Saved cohort characteristics table to task1_cohort_characteristics_table_sectionA_task1.csv\n")

# Save the gender vs anaemia table
write_csv(as.data.frame(tab_gender_anaemia), "task11_gender_vs_anaemia_baseline_table.csv")
cat("Saved gender vs anaemia table to task11_gender_vs_anaemia_baseline_table.csv\n")

# End of script
cat("\nSection A Task 1 script complete.\n")
