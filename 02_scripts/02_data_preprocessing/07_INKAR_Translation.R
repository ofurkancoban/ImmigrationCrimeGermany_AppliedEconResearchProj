# ==============================================================================
# File:          07_INKAR_Translation.R
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
# Description:   Parses, cleans, and translates the German headers and structural quirks inherent in the INKAR database exports. It unpivots datasets into a strict, tidy Long format, ensuring the population demographics and economic indicators seamlessly integrate with the spatial IDs.
#
# Inputs:        Raw INKAR CSV arrays from 01_datasets/raw/INKAR/
# Outputs:       Tidy, standardized INKAR subset: 01_datasets/processed/INKAR/01-translated/
# ==============================================================================

# 1. SETUP
# ------------------------------------------------------------------------------
# Source global utilities (packages, directory initialization, robustness helpers)
source(here::here("02_scripts", "02_data_preprocessing", "00_import.R"))

# Script-specific configuration
force_process <- FALSE # Set to TRUE to overwrite existing files

# Define Paths
input_dir <- here::here("01_datasets", "raw", "INKAR")
output_dir <- ensure_dir(here::here("01_datasets", "processed", "INKAR", "01-translated"))

# List all CSV files in Input Directory
files <- list.files(input_dir, pattern = "*.csv", full.names = TRUE)

message(glue::glue("Input directory: {input_dir}"))
message(glue::glue("Found {length(files)} files."))

# Helper to clean description same as 06
clean_desc <- function(d) {
    d <- str_replace_all(d, "[^a-zA-Z0-9äöüÄÖÜß]", "_")
    d <- str_replace_all(d, "_+", "_")
    d <- str_remove(d, "_$")
    return(d)
}

mapping_table <- list(
    "Zensuskorrigierte Zahl der Einwohner insgesamt" = "population_census_corrected",
    "Anteil der Ausländer an den Einwohnern in %" = "foreigner_share",
    "Anteil der Arbeitslosen an den zivilen Erwerbspersonen in %" = "unemployment_rate",
    "Anteil der ausländischen Arbeitslosen an den Arbeitslosen in %" = "foreign_unemployed_share",
    "Bruttoinlandsprodukt in 1.000 € je Einwohner" = "gdp_per_capita",
    
    # AGE DEMOGRAPHICS
    "Anteil der Einwohner unter 6 Jahren an den Einwohnern in %" = "population_share_0_5",
    "Anteil der 6- bis unter 18-Jährigen an der Bevölkerung" = "population_share_6_17",
    "Anteil der 18- bis unter 25-Jährigen an der Bevölkerung" = "population_share_18_24"
)

# Convert mapping names to "Cleaned" version to match filenames
cleaned_mapping <- setNames(
    mapping_table,
    sapply(names(mapping_table), clean_desc)
)

for (file in files) {
    # Identify Variable Code and Target filename early for skipping
    filename <- basename(file)
    german_key <- str_remove(filename, "_\\d{4}-\\d{4}\\.csv$")
    years_part <- str_match(filename, "_(\\d{4}-\\d{4})")[, 2]

    if (is.na(years_part)) {
        message(glue::glue("Skipping {filename}: Year pattern mismatch."))
        next
    }

    if (german_key %in% names(cleaned_mapping)) {
        var_code <- cleaned_mapping[[german_key]]
    } else {
        var_code <- tolower(german_key)
    }

    english_filename <- glue::glue("{var_code}_{years_part}.csv")
    out_path <- file.path(output_dir, english_filename)

    # SKIP LOGIC
    if (should_skip(out_path, force = force_process)) next

    # Read CSV
    data <- read_csv(file, show_col_types = FALSE)

    # 1. Rename District Columns
    if ("Raumeinheit" %in% names(data)) {
        data <- data |> rename(district_name = Raumeinheit)
    } else if ("Raumbezug" %in% names(data)) {
        data <- data |> rename(district_name = Raumbezug)
    }

    if ("Kennziffer" %in% names(data)) {
        data <- data |> rename(district_id = Kennziffer)
    }

    if (!("district_id" %in% names(data))) {
        message(glue::glue("Skipping {basename(file)}: ID column missing."))
        next
    }

    # 3. Rename Year Columns
    col_pattern <- glue::glue("^{german_key}_\\d{{4}}$")
    year_cols <- grep(col_pattern, names(data), value = TRUE)

    if (length(year_cols) > 0) {
        new_names <- sapply(year_cols, function(col) {
            year <- str_extract(col, "\\d{4}$")
            return(glue::glue("{var_code}_{year}"))
        })

        rename_vec <- setNames(year_cols, new_names)
        data_translated <- data |> rename(!!!rename_vec)

        write_csv(data_translated, out_path)
        message(glue::glue("Processed: {filename} -> {english_filename}"))
    } else {
        message(glue::glue(
            "No data columns found in {filename} matching '{german_key}'"
        ))
    }
}

message("INKAR Translation completed.")
