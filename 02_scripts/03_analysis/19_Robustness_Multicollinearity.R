# ==============================================================================
# File:          19_Robustness_Multicollinearity.R
# Project:       Immigration, Crime, and Prosperity:The Economic Drivers of Growth in German Districts
# Author:        Ömer Furkan Çoban
#
# University:    Carl von Ossietzky University of Oldenburg
# Department:    Applied Economics and Data Science
# Course:        Applied Economics
# Semester:      WiSe 25/26
# Lecturers:     Prof. Dr. Jürgen Bitzer, Dr. Bernhard Christopher Dannemann, 
#                Abigail Opokua Asare
#
# Category:      Econometric Analysis
#
# Description:   Assesses multicollinearity among model covariates using Variance 
#                Inflation Factors (VIF). Critical for ensuring stable 
#                coefficient estimates across regression specifications.
#
# Inputs:        Final Panel: 01_datasets/final/panel_data_final_2003_2021.csv
# Outputs:       VIF diagnostics results exported to 03_results/tables/txt/vif_analysis.txt
# ==============================================================================

# 1. Setup & Utilities ---------------------------------------------------------
source(here::here("02_scripts", "02_data_preprocessing", "00_import.R"))
source(here::here("02_scripts", "04_tools", "utils.R"))

input_file <- here::here("01_datasets", "final", "panel_data_final_2003_2021.csv")
output_dir <- here::here("03_results", "tables", "txt")
ensure_dir(output_dir)

# 2. Data Preparation ----------------------------------------------------------
df <- prepare_panel_data(input_file)

# 3. VIF Calculation Logic (Manual for Fixed Effects) --------------------------
# For each regressor Xj, we calculate R2_j from:
# Xj ~ X[-j] + district_id + year
# VIFj = 1 / (1 - R2_j)

regressors <- c(
    "log_unemp_l1",
    "log_capital",
    "log_property_damage_l1",
    "FE_share_children",
    "FE_share_youth",
    "foreigner_share",
    "FE_foreign_labor_supply_pressure_l1",
    "FE_log_labor_efficacy_l1"
)

message("--> Calculating VIF scores for the TWFE model...")

vif_results <- data.frame(
    Variable = character(),
    R2_within = numeric(),
    VIF = numeric(),
    stringsAsFactors = FALSE
)

for (x in regressors) {
    # Define formula: x ~ others + FE
    others <- setdiff(regressors, x)
    formula_vif <- as.formula(paste(x, "~", paste(others, collapse = " + "), "| district_id + year"))
    
    # Estimate model
    m <- fixest::feols(formula_vif, data = df)
    
    # Extract Within R2 (The R2 after absorbing Fixed Effects)
    # This is appropriate for evaluating multicollinearity in FE models
    r2_within <- fixest::fitstat(m, "wr2")[[1]]
    
    # Store
    vif_val <- 1 / (1 - r2_within)
    vif_results <- rbind(vif_results, data.frame(
        Variable = x,
        R2_within = round(r2_within, 4),
        VIF = round(vif_val, 4)
    ))
}

# 4. Save and Print ------------------------------------------------------------
sink(file.path(output_dir, "vif_analysis.txt"))
cat("================================================================\n")
cat("      Multicollinearity Analysis: Variance Inflation Factor   \n")
cat("            (Adjusted for Two-Way Fixed Effects)              \n")
cat("================================================================\n\n")
print(vif_results)
cat("\nInterpretation:\n")
cat("- VIF < 5:  Low / Negligible multicollinearity\n")
cat("- 5 < VIF < 10: Moderate multicollinearity (acceptable)\n")
cat("- VIF > 10: Potential concern (high multicollinearity)\n\n")

if (all(vif_results$VIF < 10)) {
    cat("Conclusion: All VIF scores are within acceptable thresholds (< 10).\n")
    cat("Multicollinearity does not significantly bias the parameter estimation.\n")
} else {
    cat("Conclusion: Some variables exhibit high VIF (> 10).\n")
    cat("Standard errors might be inflated for these variables.\n")
}
cat("================================================================\n")
sink()

message("\u2713 VIF analysis complete. Result saved to 03_results/tables/txt/vif_analysis.txt")
print(vif_results)
