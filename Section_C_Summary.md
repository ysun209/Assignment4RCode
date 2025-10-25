# Section C Analysis Summary

## Overview
This document summarizes the Section C analysis for Assignment 4, based on the primary biliary cirrhosis (PBC) dataset. The analysis was conducted using R, following the same structure as Section_A.R.

## Dataset
- **File**: Section C Dataset - pbc_final.csv
- **Sample Size**: 393 patients
- **Study**: Mayo Clinic RCT (1974-1984) on primary biliary cirrhosis
- **Treatment Groups**: D-penicillamine, Placebo, Not randomised

## Tasks Completed

### Task 22 (3 marks): Albumin Categorical Variable
**Question**: Convert albumin to a categorical variable with two groups based on the median.

**Results**:
- **Overall Median Albumin**: 3.55 g/dl
- **Group A** (≤ median): Median = **3.29 g/dl** (n=201)
- **Group B** (> median): Median = **3.80 g/dl** (n=192)

---

### Task 23 (6 marks): Treatment Group vs Endpoint Status

**Publication-Ready Table**:

| Treatment       | Censored | Transplant | Dead | Total |
|----------------|----------|------------|------|-------|
| D-penicillamine | 83       | 9          | 56   | 148   |
| Placebo         | 79       | 9          | 60   | 148   |
| Not randomised  | 58       | 6          | 33   | 97    |
| **Total**       | **220**  | **24**     | **149** | **393** |

**Answers to Questions**:
1. **How many people had a transplant?** 24 people
2. **What percentage of transplants were in placebo group?** 37.5%
3. **What percentage of D-penicillamine group died?** 37.8%

**Output File**: task23_treatment_vs_status_table.csv

---

### Task 24 (5 marks): Binary Outcome Analysis

**Binary Outcome Variable**: outcome_binary
- 0 = Censored
- 1 = Transplant or Death

**Risk Comparison** (Randomised participants only):

| Treatment       | Total (n) | Events (n) | Risk  | Risk % |
|----------------|-----------|------------|-------|--------|
| D-penicillamine | 148       | 65         | 0.439 | 43.9%  |
| Placebo         | 148       | 69         | 0.466 | 46.6%  |

**Statistical Measures**:
- **Risk Ratio (RR)**: 0.94
- **Risk Difference**: -2.7 percentage points
- **Fisher's Exact Test p-value**: 0.7262

**Interpretation**:
The D-penicillamine group had a 2.7% lower risk of transplant or death compared to the placebo group (RR = 0.94). However, this difference is **not statistically significant** (p = 0.73), suggesting that D-penicillamine did not significantly reduce the risk of adverse outcomes compared to placebo in this study.

**Output File**: task24_outcome_by_treatment.png

---

### Task 25 (6 marks): Disease Stage Analysis

#### Question 1: Which stage has the most people?

| Stage   | Count |
|---------|-------|
| Stage 3 | 144   |
| Stage 4 | 136   |
| Stage 2 | 86    |
| Stage 1 | 21    |

**Answer**: **Stage 3** has the most people (n = 144)

---

#### Question 2: Mean (SD) of bilirubin in each stage

| Stage   | n   | Mean (SD) Bilirubin (mg/dl) |
|---------|-----|------------------------------|
| Stage 1 | 21  | 1.36 (1.79)                  |
| Stage 2 | 86  | 2.40 (4.16)                  |
| Stage 3 | 144 | 2.68 (4.19)                  |
| Stage 4 | 136 | 4.44 (4.92)                  |

**Pattern**: Bilirubin levels increase with disease stage, with Stage 4 having the highest mean bilirubin.

**Output File**: task25_bilirubin_by_stage_boxplot.png

---

#### Question 3: ANOVA - Age differences across stages

**ANOVA Results**:
- **F-statistic**: 7.165
- **p-value**: 0.0001

**Interpretation**:
The p-value (0.0001) is less than 0.05, indicating that there is a **statistically significant difference** in mean age among the four disease stages. This suggests that age differs significantly across at least some of the stages.

---

#### Question 4: Pairwise comparison - Stage 3 vs Stage 4

**Mean Ages**:
- **Stage 3**: 49.04 years (n=144)
- **Stage 4**: 53.88 years (n=136)
- **Difference**: -4.84 years (Stage 3 - Stage 4)

**Statistical Test**:
- **t-statistic**: -3.80
- **p-value**: 0.0002
- **95% CI**: [-7.35, -2.34]

**Interpretation**:
The mean age of people in Stage 3 is **significantly lower** than those in Stage 4 by 4.84 years (p = 0.0002). This suggests that Stage 4 patients are, on average, older than Stage 3 patients, which may indicate that disease progression is associated with age, or that older patients present with more advanced disease.

**Output File**: task25_age_by_stage_boxplot.png

---

## Files Generated

### Tables (CSV):
1. `task23_treatment_vs_status_table.csv` - Treatment group vs endpoint status

### Visualizations (PNG):
1. `task24_outcome_by_treatment.png` - Bar plot of binary outcomes by treatment
2. `task25_age_by_stage_boxplot.png` - Age distribution by disease stage
3. `task25_bilirubin_by_stage_boxplot.png` - Bilirubin distribution by stage

---

## Key Findings

1. **Albumin Groups**: Patients were successfully divided into two groups based on median albumin (3.55 g/dl)

2. **Treatment Outcomes**: 
   - Similar outcomes between D-penicillamine and placebo groups
   - No significant difference in risk of transplant or death (p = 0.73)
   - Both groups had ~44-47% event rates

3. **Disease Staging**:
   - Most patients were in Stage 3 (37%)
   - Bilirubin increases with disease stage (marker of liver function)
   - Stage 4 patients are significantly older than Stage 3 patients
   - Age varies significantly across stages (p = 0.0001)

---

## R Script
The complete analysis is in **Section_C.R**, which includes:
- Data loading and exploration
- All statistical tests and calculations
- Publication-ready tables
- Visualization generation
- Detailed interpretations for each task

Run the script using:
```r
source("Section_C.R")
```

Or from terminal:
```powershell
Rscript Section_C.R
```
