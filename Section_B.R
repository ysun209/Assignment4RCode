# Section B - Tasks 17-21
# Script: Section_B.R
# Purpose: Perform the analyses requested in Section B of Assignment 4
# Notes: Assumes the CSV file "Section B Dataset - Gestation_final.csv" is in the
# same directory as this script. Outputs summary tables to console and saves
# plots as PNG files in the working directory.

# Load tidyverse for data manipulation and ggplot2 for plotting
if (!requireNamespace("tidyverse", quietly = TRUE)) {
  install.packages("tidyverse", repos = "https://cloud.r-project.org")
}
library(tidyverse)

# Read the data
gestation_path <- "Section B Dataset - Gestation_final.csv"
if (!file.exists(gestation_path)) {
  stop(paste0("Could not find the gestation CSV at expected path: ", gestation_path,
              "\nPlease place 'Section B Dataset - Gestation_final.csv' in the script directory."))
}

gest <- read_csv(gestation_path, show_col_types = FALSE)

# Quick look at the data
cat("Rows:", nrow(gest), "Columns:", ncol(gest), "\n")
cat("Column names:", paste(names(gest), collapse = ", "), "\n\n")

# Check the first few rows
cat("First few rows of the dataset:\n")
print(head(gest))

# ==============================================================================
# Task 17: Plot baby's birth weight against mother's pre-pregnancy weight
# ==============================================================================
cat("\n========== Task 17: Scatter plot of baby's birth weight vs mother's pre-pregnancy weight ==========\n")

# Create scatter plot with proper labels
png("task17_birthweight_vs_mother_weight.png", width = 800, height = 600)
plot(gest$wt.1, gest$wt, 
     xlab = "Mother's Pre-pregnancy Weight (lbs)", 
     ylab = "Baby's Birth Weight (oz)",
     main = "Baby's Birth Weight vs Mother's Pre-pregnancy Weight",
     pch = 19,  # solid circle
     col = "steelblue")
dev.off()
cat("Saved scatter plot: task17_birthweight_vs_mother_weight.png\n")

# ==============================================================================
# Task 18: Same plot with different symbol and red trend line
# ==============================================================================
cat("\n========== Task 18: Scatter plot with custom symbol and trend line ==========\n")

png("task18_birthweight_vs_mother_weight_with_trendline.png", width = 800, height = 600)
plot(gest$wt.1, gest$wt, 
     xlab = "Mother's Pre-pregnancy Weight (lbs)", 
     ylab = "Baby's Birth Weight (oz)",
     main = "Baby's Birth Weight vs Mother's Pre-pregnancy Weight (with Trend Line)",
     pch = 17,  # filled triangle symbol
     col = "steelblue")
# Add red linear trend line
abline(lm(wt ~ wt.1, data = gest), col = "red", lwd = 2)
dev.off()
cat("Saved scatter plot with trend line: task18_birthweight_vs_mother_weight_with_trendline.png\n")

# ==============================================================================
# Task 19: Describe the strength of association
# ==============================================================================
cat("\n========== Task 19: Strength of Association ==========\n")

# Calculate Pearson correlation coefficient
cor_result <- cor.test(gest$wt, gest$wt.1, method = "pearson")

cat("Pearson Correlation Test Results:\n")
cat(sprintf("  Correlation coefficient (r) = %0.4f\n", cor_result$estimate))
cat(sprintf("  t-statistic = %0.4f\n", cor_result$statistic))
cat(sprintf("  Degrees of freedom = %d\n", cor_result$parameter))
cat(sprintf("  p-value = %0.6f\n", cor_result$p.value))
cat(sprintf("  95%% Confidence Interval: [%0.4f, %0.4f]\n", cor_result$conf.int[1], cor_result$conf.int[2]))

# Interpret the correlation strength
r_value <- cor_result$estimate
if (abs(r_value) < 0.3) {
  strength <- "weak"
} else if (abs(r_value) < 0.7) {
  strength <- "moderate"
} else {
  strength <- "strong"
}

direction <- ifelse(r_value > 0, "positive", "negative")

cat("\n--- INTERPRETATION ---\n")
cat(sprintf("The Pearson correlation test shows a %s %s association (r = %0.4f, p %s 0.001) between baby's birth weight and mother's pre-pregnancy weight.\n", 
            strength, direction, r_value, 
            ifelse(cor_result$p.value < 0.001, "<", paste("=", round(cor_result$p.value, 4)))))

# ==============================================================================
# Task 20: How many babies weren't included and why?
# ==============================================================================
cat("\n========== Task 20: Missing Data Analysis ==========\n")

# Total number of babies in the dataset
n_total <- nrow(gest)

# Count complete cases for the correlation analysis (both wt and wt.1 non-missing)
complete_wt <- sum(!is.na(gest$wt))
complete_wt1 <- sum(!is.na(gest$wt.1))
complete_both <- sum(!is.na(gest$wt) & !is.na(gest$wt.1))

# Calculate how many were excluded
n_excluded <- n_total - complete_both
n_missing_baby_wt <- sum(is.na(gest$wt))
n_missing_mother_wt <- sum(is.na(gest$wt.1))
n_missing_both <- sum(is.na(gest$wt) & is.na(gest$wt.1))

cat(sprintf("Total babies in dataset: %d\n", n_total))
cat(sprintf("Babies included in correlation analysis: %d\n", complete_both))
cat(sprintf("Babies excluded from correlation analysis: %d\n\n", n_excluded))

cat("Breakdown of missing data:\n")
cat(sprintf("  - Missing baby's birth weight: %d\n", n_missing_baby_wt))
cat(sprintf("  - Missing mother's pre-pregnancy weight: %d\n", n_missing_mother_wt))
cat(sprintf("  - Missing both values: %d\n", n_missing_both))

cat("\n--- ANSWER ---\n")
cat(sprintf("%d babies weren't included in the analysis of the strength of association.\n", n_excluded))
cat("They were excluded because they had missing data for either baby's birth weight, mother's pre-pregnancy weight, or both variables.\n")
cat("Correlation analysis requires complete data for both variables to calculate the association.\n")

# ==============================================================================
# Task 21: Filter data to gestation < 270 days and answer questions
# ==============================================================================
cat("\n========== Task 21: Analysis of Babies with Gestation < 270 Days ==========\n")

# Filter the data to include only gestation < 270 days
gest_filtered <- gest %>% filter(gestation < 270)
n_filtered <- nrow(gest_filtered)

# A. What percentage of the cohort are left in the data?
pct_remaining <- 100 * n_filtered / n_total

cat(sprintf("A. Percentage of cohort with gestation < 270 days: %0.1f%%\n", pct_remaining))
cat(sprintf("   (%d out of %d babies)\n\n", n_filtered, n_total))

# B. Median and IQR of gestation in this subset
median_gest <- median(gest_filtered$gestation, na.rm = TRUE)
iqr_gest <- IQR(gest_filtered$gestation, na.rm = TRUE)

cat(sprintf("B. Median gestation in subset: %0.1f days\n", median_gest))
cat(sprintf("   Interquartile range (IQR): %0.1f days\n\n", iqr_gest))

# C. Mean and SD of mother's age in this subset
mean_age <- mean(gest_filtered$age, na.rm = TRUE)
sd_age <- sd(gest_filtered$age, na.rm = TRUE)

cat(sprintf("C. Mother's age in subset: Mean = %0.1f years, SD = %0.1f years\n", mean_age, sd_age))

# Additional summary statistics for the filtered subset
cat("\n--- ADDITIONAL SUMMARY STATISTICS FOR FILTERED SUBSET ---\n")
cat(sprintf("Number of observations: %d\n", n_filtered))
cat(sprintf("Gestation range: %0.1f to %0.1f days\n", 
            min(gest_filtered$gestation, na.rm = TRUE), 
            max(gest_filtered$gestation, na.rm = TRUE)))
cat(sprintf("Mean gestation: %0.1f days (SD = %0.1f)\n", 
            mean(gest_filtered$gestation, na.rm = TRUE), 
            sd(gest_filtered$gestation, na.rm = TRUE)))

# Summary by sex in the filtered data
cat("\nBreakdown by infant sex in filtered subset:\n")
sex_summary <- gest_filtered %>% 
  group_by(sex) %>% 
  summarise(
    n = n(),
    pct = 100 * n() / nrow(gest_filtered),
    mean_gestation = mean(gestation, na.rm = TRUE),
    mean_birthweight = mean(wt, na.rm = TRUE),
    .groups = "drop"
  )
print(sex_summary)

# Save the filtered dataset for potential further analysis
write_csv(gest_filtered, "task21_gestation_filtered_under270.csv")
cat("\nSaved filtered dataset to: task21_gestation_filtered_under270.csv\n")

# ==============================================================================
# END OF SECTION B SCRIPT
# ==============================================================================
cat("\n=============================================================\n")
cat("Section B script complete.\n")
cat("All plots and tables have been generated and saved.\n")
cat("=============================================================\n")
