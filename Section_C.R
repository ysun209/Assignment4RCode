# Section C - Primary Biliary Cirrhosis Study
# Script: Section_C.R
# Purpose: Perform the analyses requested in Section C of Assignment 4
# Notes: Assumes the CSV file "Section C Dataset - pbc_final.csv" is in the
# same directory as this script. Outputs summary tables to console and saves
# plots as PNG files in the working directory.

# Load tidyverse for data manipulation and ggplot2 for plotting
if (!requireNamespace("tidyverse", quietly = TRUE)) {
  install.packages("tidyverse", repos = "https://cloud.r-project.org")
}
library(tidyverse)

# Read the data
pbc_path <- "Section C Dataset - pbc_final.csv"
if (!file.exists(pbc_path)) {
  stop(paste0("Could not find the PBC CSV at expected path: ", pbc_path,
              "\nPlease place 'Section C Dataset - pbc_final.csv' in the script directory."))
}

pbc <- read_csv(pbc_path, show_col_types = FALSE)

# Quick look at the data
cat("Rows:", nrow(pbc), "Columns:", ncol(pbc), "\n")
cat("\nColumn names:\n")
print(names(pbc))
cat("\nFirst few rows:\n")
print(head(pbc))

# Display summary statistics
cat("\nSummary of the dataset:\n")
print(summary(pbc))

# ============================================================================
# Task 22 (3 marks): Convert albumin to categorical variable
# ============================================================================
cat("\n========== Task 22: Albumin Categorical Variable ==========\n")

# Step 1: Calculate median albumin
median_albumin <- median(pbc$albumin, na.rm = TRUE)
cat(sprintf("Median albumin = %0.2f g/dl\n", median_albumin))

# Step 2: Create categorical variable
pbc <- pbc %>%
  mutate(
    albumin_group = case_when(
      is.na(albumin) ~ NA_character_,
      albumin <= median_albumin ~ "Group A",
      albumin > median_albumin ~ "Group B"
    )
  )

# Step 3: Calculate median albumin for each group
albumin_by_group <- pbc %>%
  group_by(albumin_group) %>%
  summarise(
    n = n(),
    median_albumin = median(albumin, na.rm = TRUE),
    mean_albumin = mean(albumin, na.rm = TRUE),
    sd_albumin = sd(albumin, na.rm = TRUE)
  )

cat("\n--- Albumin by Group ---\n")
print(albumin_by_group)

cat("\n--- ANSWER ---\n")
cat(sprintf("Group A (≤ median): Median albumin = %0.2f g/dl\n", 
            filter(albumin_by_group, albumin_group == "Group A")$median_albumin))
cat(sprintf("Group B (> median): Median albumin = %0.2f g/dl\n", 
            filter(albumin_by_group, albumin_group == "Group B")$median_albumin))

# ============================================================================
# Task 23 (6 marks): Treatment group vs endpoint status table
# ============================================================================
cat("\n========== Task 23: Treatment Group vs Endpoint Status ==========\n")

# Step 1: Convert treatment group to categorical with labels
pbc <- pbc %>%
  mutate(
    treatment = case_when(
      trt == 1 ~ "D-penicillamine",
      trt == 2 ~ "Placebo",
      is.na(trt) ~ "Not randomised"
    ),
    treatment = factor(treatment, levels = c("D-penicillamine", "Placebo", "Not randomised"))
  )

# Step 2: Convert status to categorical with labels
pbc <- pbc %>%
  mutate(
    endpoint_status = case_when(
      status == 0 ~ "Censored",
      status == 1 ~ "Transplant",
      status == 2 ~ "Dead"
    ),
    endpoint_status = factor(endpoint_status, levels = c("Censored", "Transplant", "Dead"))
  )

# Step 3: Create cross-tabulation
status_by_treatment <- pbc %>%
  count(treatment, endpoint_status) %>%
  pivot_wider(names_from = endpoint_status, values_from = n, values_fill = 0)

cat("\n--- Treatment Group vs Endpoint Status Table ---\n")
print(status_by_treatment)

# Create a more polished table with totals
status_table <- pbc %>%
  group_by(treatment, endpoint_status) %>%
  summarise(n = n(), .groups = 'drop') %>%
  pivot_wider(names_from = endpoint_status, values_from = n, values_fill = 0) %>%
  mutate(Total = Censored + Transplant + Dead)

# Add row totals
totals_row <- status_table %>%
  summarise(
    treatment = "Total",
    Censored = sum(Censored),
    Transplant = sum(Transplant),
    Dead = sum(Dead),
    Total = sum(Total)
  )

status_table_with_totals <- bind_rows(status_table, totals_row)

cat("\n--- Publication-Ready Table: Treatment Group vs Endpoint Status ---\n")
print(status_table_with_totals)

# Save the table as CSV
write_csv(status_table_with_totals, "task23_treatment_vs_status_table.csv")
cat("\nSaved table to: task23_treatment_vs_status_table.csv\n")

# Answer the three questions
cat("\n--- ANSWERS TO QUESTIONS ---\n")

# Question 1: How many people had a transplant?
n_transplant <- sum(pbc$endpoint_status == "Transplant", na.rm = TRUE)
cat(sprintf("1. Number of people who had a transplant: %d\n", n_transplant))

# Question 2: What percentage of transplants were in placebo group?
transplant_data <- pbc %>% filter(endpoint_status == "Transplant")
n_transplant_placebo <- sum(transplant_data$treatment == "Placebo", na.rm = TRUE)
pct_transplant_placebo <- 100 * n_transplant_placebo / n_transplant
cat(sprintf("2. Percentage of transplant patients in placebo group: %0.1f%%\n", pct_transplant_placebo))

# Question 3: What percentage of D-penicillamine group died?
dpen_group <- pbc %>% filter(treatment == "D-penicillamine")
n_dpen_total <- nrow(dpen_group)
n_dpen_died <- sum(dpen_group$endpoint_status == "Dead", na.rm = TRUE)
pct_dpen_died <- 100 * n_dpen_died / n_dpen_total
cat(sprintf("3. Percentage of D-penicillamine group who died: %0.1f%%\n", pct_dpen_died))

# ============================================================================
# Task 24 (5 marks): Merge transplant and death into binary outcome
# ============================================================================
cat("\n========== Task 24: Binary Outcome - Risk Comparison ==========\n")

# Step 1: Create binary outcome variable
pbc <- pbc %>%
  mutate(
    outcome_binary = case_when(
      endpoint_status == "Censored" ~ 0,
      endpoint_status %in% c("Transplant", "Dead") ~ 1
    )
  )

# Step 2: Calculate risk for each treatment group
# Only include randomised participants (exclude "Not randomised")
randomised_pbc <- pbc %>% filter(treatment %in% c("D-penicillamine", "Placebo"))

risk_by_treatment <- randomised_pbc %>%
  group_by(treatment) %>%
  summarise(
    n_total = n(),
    n_outcome = sum(outcome_binary == 1, na.rm = TRUE),
    risk = mean(outcome_binary, na.rm = TRUE),
    risk_pct = 100 * risk
  )

cat("\n--- Risk of Transplant or Death by Treatment Group ---\n")
print(risk_by_treatment)

# Step 3: Calculate risk ratio and risk difference
risk_dpen <- filter(risk_by_treatment, treatment == "D-penicillamine")$risk
risk_placebo <- filter(risk_by_treatment, treatment == "Placebo")$risk
risk_ratio <- risk_dpen / risk_placebo
risk_diff <- risk_dpen - risk_placebo
risk_diff_pct <- 100 * risk_diff

cat("\n--- RISK COMPARISON ---\n")
cat(sprintf("Risk in D-penicillamine group: %0.3f (%0.1f%%)\n", risk_dpen, 100*risk_dpen))
cat(sprintf("Risk in Placebo group: %0.3f (%0.1f%%)\n", risk_placebo, 100*risk_placebo))
cat(sprintf("Risk Ratio (RR): %0.3f\n", risk_ratio))
cat(sprintf("Risk Difference: %0.3f (%0.1f percentage points)\n", risk_diff, risk_diff_pct))

# Perform chi-square test to assess statistical significance
outcome_table <- table(randomised_pbc$treatment, randomised_pbc$outcome_binary)
chi_test <- tryCatch({
  chisq.test(outcome_table, correct = FALSE)
}, warning = function(w) {
  return(NULL)
}, error = function(e) {
  return(NULL)
})

if (!is.null(chi_test) && !is.na(chi_test$p.value)) {
  cat(sprintf("\nChi-square test p-value: %0.4f\n", chi_test$p.value))
} else {
  # Use Fisher's exact test as alternative
  fisher_test <- fisher.test(outcome_table)
  cat(sprintf("\nFisher's exact test p-value: %0.4f\n", fisher_test$p.value))
  chi_test <- fisher_test
}

cat("\n--- INTERPRETATION ---\n")
if (risk_ratio > 1) {
  interpretation <- sprintf(
    "The D-penicillamine group had a %0.1f%% higher risk of transplant or death compared to the placebo group (RR = %0.2f). ",
    abs(risk_diff_pct), risk_ratio
  )
} else {
  interpretation <- sprintf(
    "The D-penicillamine group had a %0.1f%% lower risk of transplant or death compared to the placebo group (RR = %0.2f). ",
    abs(risk_diff_pct), risk_ratio
  )
}

if (!is.null(chi_test) && !is.na(chi_test$p.value)) {
  if (chi_test$p.value < 0.05) {
    interpretation <- paste0(interpretation, 
      sprintf("This difference is statistically significant (p = %0.4f).", chi_test$p.value))
  } else {
    interpretation <- paste0(interpretation, 
      sprintf("However, this difference is not statistically significant (p = %0.4f).", chi_test$p.value))
  }
}

cat(interpretation, "\n")

# ============================================================================
# Task 25 (6 marks): Stage analysis
# ============================================================================
cat("\n========== Task 25: Disease Stage Analysis ==========\n")

# Step 1: Convert stage to categorical variable
pbc <- pbc %>%
  mutate(
    stage_cat = factor(stage, levels = c(1, 2, 3, 4), 
                      labels = c("Stage 1", "Stage 2", "Stage 3", "Stage 4"))
  )

# Question 1: Which stage has the most people?
stage_counts <- pbc %>%
  count(stage_cat) %>%
  arrange(desc(n))

cat("\n--- Question 1: Stage Distribution ---\n")
print(stage_counts)
most_common_stage <- stage_counts$stage_cat[1]
n_most_common <- stage_counts$n[1]
cat(sprintf("\nAnswer: %s has the most people (n = %d)\n", most_common_stage, n_most_common))

# Question 2: Mean (SD) of bilirubin in each stage
bili_by_stage <- pbc %>%
  group_by(stage_cat) %>%
  summarise(
    n = sum(!is.na(bili)),
    mean_bili = mean(bili, na.rm = TRUE),
    sd_bili = sd(bili, na.rm = TRUE),
    mean_sd = sprintf("%0.2f (%0.2f)", mean_bili, sd_bili)
  )

cat("\n--- Question 2: Mean (SD) Bilirubin by Stage ---\n")
print(bili_by_stage %>% select(stage_cat, n, mean_sd))

# Question 3 & 4: ANOVA for age by stage
cat("\n--- Questions 3 & 4: ANOVA - Age by Stage ---\n")

# Prepare data for ANOVA (remove NA values)
pbc_anova <- pbc %>% filter(!is.na(stage_cat) & !is.na(age))

# Perform ANOVA
anova_age_stage <- aov(age ~ stage_cat, data = pbc_anova)
anova_summary <- summary(anova_age_stage)
print(anova_summary)

# Extract F-statistic and p-value
f_stat <- anova_summary[[1]]$`F value`[1]
p_value <- anova_summary[[1]]$`Pr(>F)`[1]

cat(sprintf("\nF-statistic = %0.4f\n", f_stat))
cat(sprintf("p-value = %0.4f\n", p_value))

cat("\n--- Question 3: Interpretation of ANOVA p-value ---\n")
if (p_value < 0.05) {
  cat(sprintf("The p-value (%0.4f) is less than 0.05, indicating that there is a statistically significant difference in mean age among the four disease stages. This suggests that age differs significantly across at least some of the stages.\n", p_value))
} else {
  cat(sprintf("The p-value (%0.4f) is greater than 0.05, indicating that there is no statistically significant difference in mean age among the four disease stages.\n", p_value))
}

# Question 4: Pairwise comparison - Stage 3 vs Stage 4
cat("\n--- Question 4: Pairwise Comparison - Stage 3 vs Stage 4 ---\n")

# Perform post-hoc pairwise t-tests with adjustment for multiple comparisons
pairwise_results <- pairwise.t.test(pbc_anova$age, pbc_anova$stage_cat, 
                                    p.adjust.method = "bonferroni")
cat("\nPairwise t-tests (Bonferroni correction):\n")
print(pairwise_results)

# Extract specific comparison for Stage 3 vs Stage 4
stage3_data <- pbc_anova %>% filter(stage_cat == "Stage 3")
stage4_data <- pbc_anova %>% filter(stage_cat == "Stage 4")

mean_age_stage3 <- mean(stage3_data$age, na.rm = TRUE)
mean_age_stage4 <- mean(stage4_data$age, na.rm = TRUE)
diff_mean_age <- mean_age_stage3 - mean_age_stage4

# Direct t-test for Stage 3 vs Stage 4
ttest_3v4 <- t.test(stage3_data$age, stage4_data$age)

cat(sprintf("\nMean age in Stage 3: %0.2f years (n = %d)\n", mean_age_stage3, nrow(stage3_data)))
cat(sprintf("Mean age in Stage 4: %0.2f years (n = %d)\n", mean_age_stage4, nrow(stage4_data)))
cat(sprintf("Difference in means (Stage 3 - Stage 4): %0.2f years\n", diff_mean_age))
cat(sprintf("t-statistic: %0.4f\n", ttest_3v4$statistic))
cat(sprintf("p-value (unadjusted): %0.4f\n", ttest_3v4$p.value))
cat(sprintf("95%% Confidence Interval: [%0.2f, %0.2f]\n", 
            ttest_3v4$conf.int[1], ttest_3v4$conf.int[2]))

cat("\n--- Question 4: Interpretation ---\n")
if (ttest_3v4$p.value < 0.05) {
  if (diff_mean_age > 0) {
    cat(sprintf("The mean age of people in Stage 3 is significantly higher than those in Stage 4 by %0.2f years (p = %0.4f). This suggests that Stage 3 patients are, on average, older than Stage 4 patients.\n", 
                abs(diff_mean_age), ttest_3v4$p.value))
  } else {
    cat(sprintf("The mean age of people in Stage 3 is significantly lower than those in Stage 4 by %0.2f years (p = %0.4f). This suggests that Stage 4 patients are, on average, older than Stage 3 patients.\n", 
                abs(diff_mean_age), ttest_3v4$p.value))
  }
} else {
  cat(sprintf("There is no statistically significant difference in mean age between Stage 3 and Stage 4 (difference = %0.2f years, p = %0.4f). The observed difference could be due to random variation.\n", 
              diff_mean_age, ttest_3v4$p.value))
}

# ============================================================================
# Additional visualizations
# ============================================================================

# Plot 1: Age distribution by stage
png("task25_age_by_stage_boxplot.png", width = 800, height = 600)
boxplot(age ~ stage_cat, data = pbc,
        xlab = "Disease Stage", ylab = "Age (years)", 
        main = "Age Distribution by Disease Stage",
        col = c("lightblue", "lightgreen", "lightcoral", "lightgoldenrod"))
dev.off()
cat("\nSaved boxplot: task25_age_by_stage_boxplot.png\n")

# Plot 2: Bilirubin by stage
png("task25_bilirubin_by_stage_boxplot.png", width = 800, height = 600)
boxplot(bili ~ stage_cat, data = pbc,
        xlab = "Disease Stage", ylab = "Bilirubin (mg/dl)", 
        main = "Bilirubin Distribution by Disease Stage",
        col = c("lightblue", "lightgreen", "lightcoral", "lightgoldenrod"))
dev.off()
cat("Saved boxplot: task25_bilirubin_by_stage_boxplot.png\n")

# Plot 3: Outcome by treatment
png("task24_outcome_by_treatment.png", width = 800, height = 600)
outcome_prop <- randomised_pbc %>%
  group_by(treatment) %>%
  count(outcome_binary) %>%
  mutate(prop = n / sum(n))

barplot_data <- matrix(c(
  filter(outcome_prop, treatment == "D-penicillamine" & outcome_binary == 0)$n,
  filter(outcome_prop, treatment == "D-penicillamine" & outcome_binary == 1)$n,
  filter(outcome_prop, treatment == "Placebo" & outcome_binary == 0)$n,
  filter(outcome_prop, treatment == "Placebo" & outcome_binary == 1)$n
), nrow = 2, byrow = FALSE)

barplot(barplot_data, beside = TRUE, 
        names.arg = c("D-penicillamine", "Placebo"),
        legend.text = c("Censored", "Transplant/Death"),
        col = c("lightgreen", "coral"),
        xlab = "Treatment Group", ylab = "Number of Patients",
        main = "Treatment Outcome (Binary)")
dev.off()
cat("Saved barplot: task24_outcome_by_treatment.png\n")

# ============================================================================
# Summary
# ============================================================================
cat("\n========== Section C Analysis Complete ==========\n")
cat("All tasks completed successfully!\n")
cat("\nFiles created:\n")
cat("  - task23_treatment_vs_status_table.csv\n")
cat("  - task25_age_by_stage_boxplot.png\n")
cat("  - task25_bilirubin_by_stage_boxplot.png\n")
cat("  - task24_outcome_by_treatment.png\n")

# End of script
