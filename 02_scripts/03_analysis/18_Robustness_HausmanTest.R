# ==============================================================================
# File:          18_Robustness_HausmanTest.R
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
# Description:   Conducts the Hausman Specification Test to evaluate whether a Fixed Effects (FE) 
#                or Random Effects (RE) model is more appropriate. This validates the consistency 
#                of the chosen FE estimator against the RE alternative.
#
# Inputs:        Final Panel: 01_datasets/final/panel_data_final_2003_2021.csv
# Outputs:       Hausman test results exported to 03_results/tables/txt/hausman_test.txt
# ==============================================================================

# 1. Setup & Utilities ---------------------------------------------------------
source(here::here("02_scripts", "02_data_preprocessing", "00_import.R"))
source(here::here("02_scripts", "04_tools", "utils.R"))

input_file <- here::here("01_datasets", "final", "panel_data_final_2003_2021.csv")
output_dir <- here::here("03_results", "tables", "txt")
ensure_dir(output_dir)

# 2. Data Preparation ----------------------------------------------------------
df <- prepare_panel_data(input_file)

# Convert to pdata.frame for plm compatibility
pdf <- plm::pdata.frame(df, index = c("district_id", "year"))

# 3. Model Specification -------------------------------------------------------
# Using the Final Specification from mod6
formula_hausman <- log_gdp_pc ~ 
    log_unemp_l1 + 
    log_capital + 
    log_property_damage_l1 + 
    FE_share_children + 
    FE_share_youth + 
    foreigner_share + 
    FE_foreign_labor_supply_pressure_l1 + 
    FE_log_labor_efficacy_l1

# 4. Estimation ----------------------------------------------------------------
message("--> Estimating Fixed Effects (Within) Model...")
fe_model <- plm::plm(formula_hausman, data = pdf, model = "within")

message("--> Estimating Random Effects Model...")
re_model <- plm::plm(formula_hausman, data = pdf, model = "random")

# 5. Hausman Test --------------------------------------------------------------
message("--> Running Hausman Test...")
h_test <- plm::phtest(fe_model, re_model)

print(h_test)

# 6. Save Result ---------------------------------------------------------------
sink(file.path(output_dir, "hausman_test.txt"))
cat("================================================================\n")
cat("      Hausman Test: Fixed Effects vs. Random Effects         \n")
cat("================================================================\n\n")
print(h_test)
cat("\nInterpretation:\n")
cat("H0: Random Effects is consistent and efficient.\n")
cat("H1: Random Effects is inconsistent (Fixed Effects is preferred).\n\n")
if (h_test$p.value < 0.05) {
    cat("Result: REJECT H0 (p < 0.05). Fixed Effects is more appropriate.\n")
} else {
    cat("Result: FAIL TO REJECT H0 (p >= 0.05). Random Effects is appropriate.\n")
}
cat("================================================================\n")
sink()

message("\u2713 Hausman Test complete. Result saved to 03_results/tables/txt/hausman_test.txt")
