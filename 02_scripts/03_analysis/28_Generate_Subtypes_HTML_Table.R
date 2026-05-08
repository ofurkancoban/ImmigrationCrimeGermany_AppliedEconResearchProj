# ==============================================================================
# File:          28_Generate_Subtypes_HTML_Table.R
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
# Description:   Compiles the fragmented output from the exhaustive Subtype batch scripts into a singular, highly stylized HTML representation. Embeds specific CSS styling for integration into Quarto presentation slides reporting robustness summaries.
#
# Inputs:        Output matrices computed by the Subtype regressions
# Outputs:       A unified 'regression_comparison_subtypes.html' stored in 03_results/tables/html/
# ==============================================================================

# 1. Setup & Utilities ---------------------------------------------------------
source(here::here("02_scripts", "02_data_preprocessing", "00_import.R"))
source(here::here("02_scripts", "04_tools", "utils.R"))

# Script-specific configuration
force_process <- TRUE # Set to TRUE to overwrite existing files

input_file <- here::here("01_datasets", "final", "panel_data_final_2003_2021.csv")
output_dir <- here::here("03_results", "tables")

# 2. Skip Logic ----------------------------------------------------------------
target_html <- file.path(output_dir, "html", "crime_subtypes", "regression_comparison_subtypes.html")

if (should_skip(target_html, force = force_process)) {
    message("\u2713 Comparison table already exists, skipping: ", basename(target_html))
} else {
    ensure_dir(dirname(target_html))

    # 3. Data Preparation ------------------------------------------------------
    df <- prepare_panel_data(input_file) %>%
        filter(!is.na(FE_foreign_labor_supply_pressure_l1), !is.na(log_unemp_l1), !is.na(FE_log_labor_efficacy_l1))

    # 4. Model Estimation (Final Stage for each) -------------------------------
    message("Estimating Comparison Models...")

    m_total <- feols(
        log_gdp_pc ~ log_unemp_l1 +
            log_capital +
            log_total_l1 +
            FE_share_children +
            FE_share_youth +
            foreigner_share +
            FE_foreign_labor_supply_pressure_l1 +
            FE_log_labor_efficacy_l1 |
            district_id + year,
        data = df
    )
    m_street <- feols(
        log_gdp_pc ~ log_unemp_l1 +
            log_capital +
            log_street_l1 +
            FE_share_children +
            FE_share_youth +
            foreigner_share +
            FE_foreign_labor_supply_pressure_l1 +
            FE_log_labor_efficacy_l1 |
            district_id + year,
        data = df
    )
    m_drugs <- feols(
        log_gdp_pc ~ log_unemp_l1 +
            log_capital +
            log_drugs_l1 +
            FE_share_children +
            FE_share_youth +
            foreigner_share +
            FE_foreign_labor_supply_pressure_l1 +
            FE_log_labor_efficacy_l1 |
            district_id + year,
        data = df
    )
    m_bodily <- feols(
        log_gdp_pc ~ log_unemp_l1 +
            log_capital +
            log_bodily_l1 +
            FE_share_children +
            FE_share_youth +
            foreigner_share +
            FE_foreign_labor_supply_pressure_l1 +
            FE_log_labor_efficacy_l1 |
            district_id + year,
        data = df
    )
    m_main <- feols(
        log_gdp_pc ~ log_unemp_l1 +
            log_capital +
            log_property_damage_l1 +
            FE_share_children +
            FE_share_youth +
            foreigner_share +
            FE_foreign_labor_supply_pressure_l1 +
            FE_log_labor_efficacy_l1 |
            district_id + year,
        data = df
    )

    models <- list(
        "(1) Total" = m_total,
        "(2) Street" = m_street,
        "(3) Drugs" = m_drugs,
        "(4) Bodily" = m_bodily,
        "(5) Property (Main)" = m_main
    )

    # 5. Table Construction (Stylized GT) --------------------------------------
    coef_map <- c(
        "log_unemp_l1" = "Log Unemployment (t-1)",
        "log_capital" = "Log Capital Stock per Capita",
        "log_total_l1" = "Log Total Crime Rate (t-1)",
        "log_street_l1" = "Log Street Crime Rate (t-1)",
        "log_drugs_l1" = "Log Drug Offences Rate (t-1)",
        "log_bodily_l1" = "Log Bodily Harm Rate (t-1)",
        "log_property_damage_l1" = "Log Property Damage Rate (t-1)",
        "FE_share_children" = "Demographic Share: Children (0-17) (%)",
        "FE_share_youth" = "Demographic Share: Youth (18-24) (%)",
        "foreigner_share" = "Foreigner Share (%)",
        "FE_foreign_labor_supply_pressure_l1" = "Foreign Labor Supply Pressure (t-1) (%)",
        "FE_log_labor_efficacy_l1" = "Log Labor Integration Efficacy (t-1)"
    )

    # Fixed Effects rows
    rows <- data.frame(
        term = c("District FE", "Year FE"),
        "(1) Total" = rep("Yes", 2),
        "(2) Street" = rep("Yes", 2),
        "(3) Drugs" = rep("Yes", 2),
        "(4) Bodily" = rep("Yes", 2),
        "(5) Property (Main)" = rep("Yes", 2),
        check.names = FALSE
    )

    # Export Consolidated TXT
    outfile_txt <- file.path(output_dir, "txt", "regression_comparison_subtypes.txt")
    modelsummary(
        models,
        coef_map = coef_map,
        stars = c('*' = .1, '**' = .05, '***' = .01),
        gof_map = list(
            list("raw" = "nobs", "clean" = "Observations", "fmt" = 0),
            list("raw" = "r2.within", "clean" = "Within R\u00b2", "fmt" = 3)
        ),
        add_rows = rows,
        title = "Robustness: Alternative Crime Definitions Comparison (Final Specification)",
        notes = "Standard errors clustered at the district level. Each column replaces the baseline Log Property Damage Rate with the respective crime category.",
        output = outfile_txt
    )

    rows <- data.frame(
        term = c("District FE", "Year FE", ""),
        "(1) Total" = c("Yes", "Yes", ""),
        "(2) Street" = c("Yes", "Yes", ""),
        "(3) Drugs" = c("Yes", "Yes", ""),
        "(4) Bodily" = c("Yes", "Yes", ""),
        "(5) Property (Main)" = c("Yes", "Yes", ""),
        check.names = FALSE
    )

    gof_mapping <- list(
        list("raw" = "nobs", "clean" = "Observations", "fmt" = 0),
        list("raw" = "r2.within", "clean" = "Within R\u00b2", "fmt" = 3)
    )

    message("Generating consolidated HTML table...")
    table_styled <- modelsummary(
        models,
        coef_map = coef_map,
        stars = c('*' = .1, '**' = .05, '***' = .01),
        gof_map = gof_mapping,
        add_rows = rows,
        output = "gt"
    )

    # Applying theme consistently via utils.R
    table_styled <- apply_academic_theme(
        table_styled,
        title = "Robustness: Comparison of Crime Subtypes",
        subtitle = "Final Model Specifications across different crime categories (2003-2021)",
        source_note = "Standard errors clustered at the district level. All crime rates are lagged (t-1) and logged (rate + 0.1)."
    ) %>%
    tab_options(
        table.font.names = "Inter, -apple-system, sans-serif" # RevealJS preference
    )

    # 6. Save Table ------------------------------------------------------------
    gtsave(table_styled, target_html)
    message("\u2713 Comparison table generated: ", basename(target_html))
}

message("✓ Subtype Table Generation complete.")
