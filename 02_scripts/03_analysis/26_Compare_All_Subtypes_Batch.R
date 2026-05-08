# ==============================================================================
# File:          26_Compare_All_Subtypes_Batch.R
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
# Description:   Operates a batch processing mechanism that loops through all investigated crime subtypes concurrently. Extracts key elasticity estimators and standard errors across disparate crime models to synthesize overarching patterns.
#
# Inputs:        Final Panel: 01_datasets/final/panel_data_final_2003_2021.csv
# Outputs:       Comparative crime sub-model artifacts exported to 03_results/tables/
# ==============================================================================

source(here::here("02_scripts", "02_data_preprocessing", "00_import.R"))
source(here::here("02_scripts", "04_tools", "utils.R"))

# 1. Setup ---------------------------------------------------------------------
input_file <- here("01_datasets", "final", "panel_data_final_2003_2021.csv")
output_dir <- here("03_results", "tables")
if (!dir.exists(file.path(output_dir, "html", "crime_subtypes"))) {
    dir.create(
        file.path(output_dir, "html", "crime_subtypes"),
        recursive = TRUE
    )
}

# 2. Data Preparation ----------------------------------------------------------
df <- prepare_panel_data(input_file) %>%
    filter(!is.na(FE_foreign_labor_supply_pressure_l1), !is.na(log_unemp_l1), !is.na(FE_log_labor_efficacy_l1))

# 3. Model Definition and Estimation Loop --------------------------------------

crime_types <- list(
    list(
        var = "PKS_total_crime_rate",
        label = "Total Crime",
        file = "total_crime"
    ),
    list(
        var = "PKS_street_crime_rate",
        label = "Street Crime",
        file = "street_crime"
    ),
    list(
        var = "PKS_drug_offences_rate",
        label = "Drug Offences",
        file = "drug_offences"
    ),
    list(
        var = "PKS_bodily_harm_rate",
        label = "Bodily Harm",
        file = "bodily_harm"
    ),
    list(
        var = "PKS_property_damage_rate",
        label = "Property Damage",
        file = "property_damage"
    )
)

run_crime_model <- function(crime_var, label, filename) {
    message(glue("Processing: {label}..."))

    # 3.1 Data for specific crime
    df_crime <- df %>%
        rename(log_crime = !!sym(crime_var))

    # 3.2 Main Spec (Final Stage variables)
    m <- feols(
        log_gdp_pc ~ log_unemp_l1 +
            log_capital +
            log_crime +
            FE_share_children +
            FE_share_youth +
            foreigner_share +
            FE_foreign_labor_supply_pressure_l1 +
            FE_log_labor_efficacy_l1 |
            district_id + year,
        data = df_crime
    )

    # 3.3 Table Construction
    base_path <- file.path(output_dir, "html", "crime_subtypes", filename)
    models <- list(m)
    names(models) <- label

    coef_map_sub <- c(
        "log_unemp_l1" = "Log Unemployment (t-1)",
        "log_capital" = "Log Capital Stock per Capita",
        "log_crime" = glue("Log {label} Rate (t-1)"),
        "FE_share_children" = "Share Children (0-17)",
        "FE_share_youth" = "Share Youth (18-24)",
        "foreigner_share" = "Foreigner Share",
        "FE_foreign_labor_supply_pressure_l1" = "Foreign Labor Supply Pressure (t-1) (%)",
        "FE_log_labor_efficacy_l1" = "Labor Integration Efficacy (t-1)"
    )

    # Fixed Effects rows
    rows <- data.frame(
        term = c("District FE", "Year FE"),
        "Model" = c("Yes", "Yes"),
        check.names = FALSE
    )
    names(rows)[2] <- label

    # Custom GOF
    gof_mapping <- list(
        list("raw" = "nobs", "clean" = "Observations", "fmt" = 0),
        list("raw" = "r2.within", "clean" = "Within Rsq", "fmt" = 3)
    )

    table_styled <- modelsummary(
        models,
        coef_map = coef_map_sub,
        stars = c('*' = .1, '**' = .05, '***' = .01),
        gof_map = gof_mapping,
        add_rows = rows,
        output = "gt"
    ) %>%
        apply_academic_theme(
            title = glue("Robustness: {label} Specification"),
            subtitle = "Linear Specification with Two-Way Fixed Effects (2003-2021)"
        )

    gtsave(table_styled, paste0(base_path, ".html"))
}

# 4. Execution -----------------------------------------------------------------
for (crime in crime_types) {
    run_crime_model(crime$var, crime$label, crime$file)
}

message(
    "✓ All Subtype Analysis complete. Results in: 03_results/tables/crime_subtypes/"
)
