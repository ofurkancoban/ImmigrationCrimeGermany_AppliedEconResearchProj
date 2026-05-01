# ==============================================================================
# File:          13_Feature_Engineering.R
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
# Category:      Data Preprocessing
#
# Description:   Engineers the critical analytical covariates serving the research hypotheses. Notably, it computes the 'Labor Integration Efficacy' and the 'Foreign Labor Supply Pressure' ratios, refines dependency metrics (youth/children shares), and finalizes the master panel ready for econometric ingestion.
#
# Inputs:        Unified intermediate panel dataset from 12_Data_Merge
# Outputs:       The finalized analytical dataset: 01_datasets/final/panel_data_final_2003_2021.csv
# ==============================================================================

# 1. SETUP
# ------------------------------------------------------------------------------
# Source global utilities (packages, directory initialization, robustness helpers)
source(here::here("02_scripts", "02_data_preprocessing", "00_import.R"))

# Script-specific configuration
force_process <- TRUE # Set to TRUE to overwrite existing files

# Define Files to Process
files_to_process <- list(
    list(
        input = here::here("01_datasets", "final", "panel_data_2003_2021.csv"),
        output = here::here("01_datasets", "final", "panel_data_final_2003_2021.csv")
    )
)

process_file <- function(input_path, output_path) {
    # Standardized skip logic
    if (should_skip(output_path, force = force_process)) {
        message("✓ Output already exists: ", basename(output_path))
        return(TRUE)
    }

    if (!file.exists(input_path)) {
        warning("Input file not found, skipping: ", input_path)
        return(FALSE)
    }

    message(glue::glue("→ Processing: {basename(input_path)}"))
    df <- readr::read_csv(input_path, show_col_types = FALSE)

    # 2. Feature Engineering -------------------------------------------------------
    df_enriched <- df %>%
        dplyr::mutate(
            # --- Crime Rate Recalibration (Census Correction) ---
            PKS_total_crime_rate = (PKS_total_crime_reported / population) * 100000,
            PKS_street_crime_rate = (PKS_street_crime_reported / population) * 100000,
            PKS_property_damage_rate = (PKS_property_damage_reported / population) * 100000,
            PKS_bodily_harm_rate = (PKS_bodily_harm_reported / population) * 100000,
            PKS_drug_offences_rate = (PKS_drug_offences_reported / population) * 100000,

            # --- Demographic Refinements ---
            FE_children_share = INKAR_OTHER_population_share_0_5 + INKAR_OTHER_population_share_6_17,
            FE_youth_share = INKAR_OTHER_population_share_18_24,

            # --- Integration Metrics ---
            flsp = INKAR_INTEGRATION_foreign_unemployed_share / INKAR_INTEGRATION_foreigner_share,
            labor_efficacy = (100 - INKAR_INTEGRATION_foreign_unemployed_share) * INKAR_INTEGRATION_foreigner_share
        )

    # 3. Save Output ---------------------------------------------------------------
    message(glue::glue("  ✓ Saving final dataset: {basename(output_path)}"))
    readr::write_csv(df_enriched, output_path)
    return(TRUE)
}

# Run for files
for (f in files_to_process) {
    process_file(f$input, f$output)
}

message("✓ Feature Engineering complete.")
