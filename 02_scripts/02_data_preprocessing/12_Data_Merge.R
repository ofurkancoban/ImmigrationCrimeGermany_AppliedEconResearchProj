# ==============================================================================
# File:          12_Data_Merge.R
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
# Description:   Validates spatial keys (AGS) and constructs the final master panel by executing complex full joins across the three critical spheres: The Harmonized PKS (Crime), Harmonized INKAR (Economics), and Harmonized Capital subsets.
#
# Inputs:        PKS_Final_Prepared.csv, INKAR_Final_Prepared.csv, and Capital Harmonized datasets
# Outputs:       Unified intermediate panel dataset prior to feature engineering
# ==============================================================================

# 1. SETUP
# ------------------------------------------------------------------------------
# Source global utilities (packages, directory initialization, robustness helpers)
source(here::here("02_scripts", "02_data_preprocessing", "00_import.R"))

# Script-specific configuration
force_process <- TRUE # Set to TRUE to overwrite existing files

# Paths
pks_dir <- here::here("01_datasets", "processed", "PKS", "03-translated")
inkar_dir <- here::here("01_datasets", "processed", "INKAR", "01-translated")
capital_file <- here::here("01_datasets", "processed", "StatistikPortal", "02-harmonized", "german_districts_capital_stock_2003_2021_harmonized.csv")

# Output directory and file
final_output_dir <- ensure_dir(here::here("01_datasets", "final"))
outfile <- file.path(final_output_dir, "panel_data_2003_2021.csv")

# Standardized skip logic
if (should_skip(outfile, force = force_process)) {
    message("✓ Output already exists: ", basename(outfile))
    process_main <- FALSE
} else {
    process_main <- TRUE
}

if (process_main) {
    # -------------------------------------------------------------------------
    # 1. Define Model 15 Whitelist (INKAR)
    # -------------------------------------------------------------------------
    model_15_inkar_vars <- list(
        "gdp_per_capita" = "INCOME",
        "unemployment_rate" = "EMPLOYMENT",
        "foreigner_share" = "INTEGRATION",
        "foreign_unemployed_share" = "INTEGRATION",
        "population_share_0_5" = "OTHER",
        "population_share_6_17" = "OTHER",
        "population_share_18_24" = "OTHER",
        "population_census_corrected" = "DEMOGRAPHICS"
    )

    # -------------------------------------------------------------------------
    # 2. Load and Merge INKAR Data
    # -------------------------------------------------------------------------
    message("Loading INKAR Data (Model 15 Subset)...")
    inkar_panel_list <- list()

    for (var_code in names(model_15_inkar_vars)) {
        f <- list.files(inkar_dir, pattern = paste0("^", var_code, "_.*\\.csv$"), full.names = TRUE)
        if (length(f) == 0) { warning("Missing INKAR file: ", var_code); next }

        d <- readr::read_csv(f[1], show_col_types = FALSE) %>% janitor::clean_names()
        cat_name <- model_15_inkar_vars[[var_code]]
        target_var_name <- if (var_code == "population_census_corrected") "population" else glue::glue("INKAR_{cat_name}_{var_code}")

        d_long <- d %>%
            dplyr::select(district_id, dplyr::any_of("district_name"), dplyr::starts_with(var_code)) %>%
            tidyr::pivot_longer(cols = dplyr::starts_with(var_code), names_to = c("temp_name", "year"), names_pattern = "(.*)_(\\d{4})$") %>%
            dplyr::group_by(district_id, year) %>%
            dplyr::summarise(district_name = dplyr::first(district_name), !!target_var_name := dplyr::first(value), .groups = "drop") %>%
            dplyr::mutate(year = as.integer(year), district_id = as.character(district_id)) %>%
            dplyr::filter(year >= 2003 & year <= 2021)

        inkar_panel_list[[var_code]] <- d_long
    }

    inkar_master <- purrr::reduce(inkar_panel_list, dplyr::full_join, by = c("district_id", "year")) %>%
        dplyr::mutate(district_name = dplyr::coalesce(district_name.x, district_name.y, district_name.x.x)) %>%
        dplyr::select(district_id, district_name, year, dplyr::everything()) %>%
        dplyr::select(-dplyr::starts_with("district_name."))

    # -------------------------------------------------------------------------
    # 3. Load Capital Stock (StatistikPortal)
    # -------------------------------------------------------------------------
    message("Loading Capital Stock (Harmonized 2003-2021)...")
    if (file.exists(capital_file)) {
        capital_data <- readr::read_delim(capital_file, delim = ";", show_col_types = FALSE) %>%
            dplyr::rename(district_id = AGS, year = Year) %>%
            dplyr::mutate(district_id = sprintf("%05s", as.character(district_id))) %>%
            dplyr::select(district_id, year, CAPITAL_stock_per_capita = Capital_Stock_Per_Capita)
    } else {
        stop("Capital Stock file not found: ", capital_file)
    }

    # -------------------------------------------------------------------------
    # 4. Load PKS (Street Crime & Total)
    # -------------------------------------------------------------------------
    message("Loading PKS (Street Crime Subset)...")
    pks_files <- list.files(pks_dir, pattern = "*.csv", full.names = TRUE)
    pks_list <- list()

    for (file in pks_files) {
        year_val <- as.integer(stringr::str_extract(basename(file), "\\d{4}"))
        if (is.na(year_val) || year_val < 2003 || year_val > 2021) next

        temp <- readr::read_csv(file, show_col_types = FALSE) %>%
            dplyr::mutate(year = year_val) %>%
            dplyr::filter(key %in% c("899000", "------", "674000", "890000", "222000", "224000", "730000", "220000")) %>%
            dplyr::select(district_id, year, key, cases_reported, cases_per_100k)
        pks_list[[length(pks_list) + 1]] <- temp
    }

    pks_merged <- dplyr::bind_rows(pks_list) %>%
        dplyr::mutate(key_type = dplyr::case_when(key == "899000" ~ "street_crime", key == "674000" ~ "property_damage", key == "890000" ~ "total_crime_clean", key == "------" ~ "total_crime_raw", key == "220000" ~ "bodily_harm_direct", key == "222000" ~ "bodily_harm_serious", key == "224000" ~ "bodily_harm_simple", key == "730000" ~ "drug_offences", TRUE ~ key))

    pks_totals <- pks_merged %>%
        dplyr::filter(key_type %in% c("total_crime_clean", "total_crime_raw")) %>%
        dplyr::group_by(district_id, year, key_type) %>%
        dplyr::summarise(cases_reported = dplyr::first(cases_reported), cases_per_100k = dplyr::first(cases_per_100k), .groups = "drop") %>%
        tidyr::pivot_wider(id_cols = c(district_id, year), names_from = key_type, values_from = c(cases_reported, cases_per_100k)) %>%
        dplyr::mutate(PKS_total_crime_reported = dplyr::coalesce(cases_reported_total_crime_clean, cases_reported_total_crime_raw), PKS_total_crime_rate = dplyr::coalesce(cases_per_100k_total_crime_clean, cases_per_100k_total_crime_raw)) %>%
        dplyr::select(district_id, year, PKS_total_crime_reported, PKS_total_crime_rate)

    pks_others <- pks_merged %>%
        dplyr::filter(!key_type %in% c("total_crime_clean", "total_crime_raw")) %>%
        dplyr::group_by(district_id, year, key_type) %>%
        dplyr::summarise(cases_reported = dplyr::first(cases_reported), cases_per_100k = dplyr::first(cases_per_100k), .groups = "drop") %>%
        tidyr::pivot_wider(id_cols = c(district_id, year), names_from = key_type, values_from = c(cases_reported, cases_per_100k)) %>%
        dplyr::mutate(temp_sum_reported = rowSums(dplyr::select(., dplyr::matches("cases_reported_bodily_harm_(serious|simple)")), na.rm = TRUE), temp_sum_rate = rowSums(dplyr::select(., dplyr::matches("cases_per_100k_bodily_harm_(serious|simple)")), na.rm = TRUE), PKS_bodily_harm_reported = dplyr::if_else(temp_sum_reported == 0, dplyr::coalesce(cases_reported_bodily_harm_direct, 0), temp_sum_reported), PKS_bodily_harm_rate = dplyr::if_else(temp_sum_rate == 0, dplyr::coalesce(cases_per_100k_bodily_harm_direct, 0), temp_sum_rate)) %>%
        dplyr::rename(PKS_street_crime_reported = cases_reported_street_crime, PKS_street_crime_rate = cases_per_100k_street_crime, PKS_property_damage_reported = cases_reported_property_damage, PKS_property_damage_rate = cases_per_100k_property_damage, PKS_drug_offences_reported = cases_reported_drug_offences, PKS_drug_offences_rate = cases_per_100k_drug_offences) %>%
        dplyr::select(district_id, year, PKS_street_crime_reported, PKS_street_crime_rate, PKS_property_damage_reported, PKS_property_damage_rate, PKS_bodily_harm_reported, PKS_bodily_harm_rate, PKS_drug_offences_reported, PKS_drug_offences_rate)

    pks_wide <- pks_totals %>% dplyr::full_join(pks_others, by = c("district_id", "year"))

    # -------------------------------------------------------------------------
    # 5. Final Merge
    # -------------------------------------------------------------------------
    message("Performing Final Model Merge...")
    final_data <- inkar_master %>%
        dplyr::inner_join(capital_data, by = c("district_id", "year")) %>%
        dplyr::inner_join(pks_wide, by = c("district_id", "year")) %>%
        dplyr::arrange(district_id, year)

    readr::write_csv(final_data, outfile)
    message("✓ Panel Data Merge complete: ", outfile)
}
