# ==============================================================================
# File:          11_Capital_Stock_Harmonization.R
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
# Description:   Applies the population-weighted spatial harmonization matrix (transition keys) specifically to the estimated Capital Stock metrics, ensuring the constructed Capital vectors align perfectly with the synchronized PKS and INKAR district geometries.
#
# Inputs:        Calculated Capital vectors and BBSR Transition Key Tables
# Outputs:       Cleaned Capital subset: 01_datasets/processed/StatistikPortal/02-harmonized/
# ==============================================================================

# 1. SETUP
# ------------------------------------------------------------------------------
# Source global utilities (packages, directory initialization, robustness helpers)
source(here::here("02_scripts", "02_data_preprocessing", "00_import.R"))

# Script-specific configuration
force_process <- FALSE # Set to TRUE to overwrite existing files

# Suppress R CMD check NOTEs for NSE (dplyr column references)
utils::globalVariables(c(
    "old_id", "new_id", "weight", "total_w", "AGS", "Year", "Name",
    "Capital_Stock_Total", "Capital_Stock_Per_Capita", "Sector_Share_Agriculture",
    "Sector_Share_Industry", "Sector_Share_Services", "new_name"
))

options(warn = 1) # Print warnings immediately

input_file <- here::here("01_datasets/processed/StatistikPortal/01-processed/german_districts_capital_stock_2003_2021.csv")
output_file <- here::here("01_datasets/processed/StatistikPortal/02-harmonized/german_districts_capital_stock_2003_2021_harmonized.csv")

# Standardized skip logic
if (should_skip(output_file, force = force_process)) {
    message("✓ Output already exists: ", basename(output_file))
    run_main <- FALSE
} else {
    run_main <- TRUE
    ensure_dir(dirname(output_file))
}

if (run_main) {
    ref_file <- here::here("01_datasets/tools/INKAR_ref-kreise-1990-2023_fixed.xlsx")
    target_year <- 2023

    # --- Helper Functions ---
    .map_cache <- new.env(parent = emptyenv())

    get_transition_map <- function(ref_file, year_t) {
        year_t1 <- year_t + 1
        sheet_name <- paste0(year_t, "-", year_t1)
        if (exists(sheet_name, envir = .map_cache)) return(get(sheet_name, envir = .map_cache))

        sheets <- readxl::excel_sheets(ref_file)
        if (!sheet_name %in% sheets) return(NULL)

        data <- readxl::read_excel(ref_file, sheet = sheet_name)
        colnames(data) <- colnames(data) |> stringr::str_replace_all("[\r\n]+", " ") |> stringr::str_squish()

        col_name_t <- paste0("Kreise 31.12.", year_t)
        col_name_t1 <- paste0("Kreise 31.12.", year_t1)
        idx_t <- which(colnames(data) == col_name_t)
        idx_t1 <- which(colnames(data) == col_name_t1)
        if (length(idx_t) == 0 || length(idx_t1) == 0) return(NULL)

        weight_col_name <- colnames(data)[stringr::str_detect(colnames(data), "bevölkerungs.*Umsteige")][1]
        if (is.na(weight_col_name)) weight_col_name <- colnames(data)[1]

        transition <- data |>
            dplyr::select(old_id = dplyr::all_of(col_name_t), new_id = dplyr::all_of(col_name_t1), weight = dplyr::all_of(weight_col_name), old_name = dplyr::all_of(paste0("Kreisname ", year_t)), new_name = dplyr::all_of(paste0("Kreisname ", year_t1))) |>
            dplyr::mutate(old_id = as.character(as.integer(old_id)), new_id = as.character(as.integer(new_id)), weight = as.numeric(weight)) |>
            dplyr::filter(!is.na(old_id) & !is.na(new_id))

        transition <- transition |>
            group_by(old_id) |>
            mutate(total_w = sum(weight)) |>
            ungroup() |>
            mutate(weight = ifelse(total_w >= 0.95, weight / total_w, weight)) |>
            select(-total_w)

        clean_id <- function(x) { s <- sprintf("%08d", as.numeric(x)); substr(s, 1, 5) }
        transition <- transition |> mutate(old_id = clean_id(old_id), new_id = clean_id(new_id))
        assign(sheet_name, transition, envir = .map_cache)
        return(transition)
    }

    harmonize_capital_stock <- function(data, year_src) {
        effective_start_year <- year_src
        if (year_src == 2008) effective_start_year <- 2007
        if (year_src == 2011) effective_start_year <- 2010
        if (year_src == 2009) effective_start_year <- 2008
        if (year_src == 2016) effective_start_year <- 2015
        if (year_src == 2021) effective_start_year <- 2020

        current_data <- data
        if (effective_start_year < target_year) {
            for (y in effective_start_year:(target_year - 1)) {
                map <- get_transition_map(ref_file, y)
                if (is.null(map)) break
                current_data <- current_data |> mutate(AGS = stringr::str_trim(AGS))
                joined <- current_data |> left_join(map, by = c("AGS" = "old_id"), relationship = "many-to-many") |> filter(!is.na(new_id))

                absolute_cols <- c("Capital_Stock_Total")
                weighted_avg_cols <- c("Capital_Stock_Per_Capita", "Sector_Share_Agriculture", "Sector_Share_Industry", "Sector_Share_Services")

                for (v in absolute_cols) if (v %in% names(joined)) joined[[v]] <- joined[[v]] * joined$weight
                for (v in weighted_avg_cols) if (v %in% names(joined)) joined[[v]] <- joined[[v]] * joined$weight

                current_data <- joined |> group_by(Year, new_id) |>
                    summarise(Capital_Stock_Total = sum(Capital_Stock_Total, na.rm = TRUE), Capital_Stock_Per_Capita = sum(Capital_Stock_Per_Capita, na.rm = TRUE), Sector_Share_Agriculture = sum(Sector_Share_Agriculture, na.rm = TRUE), Sector_Share_Industry = sum(Sector_Share_Industry, na.rm = TRUE), Sector_Share_Services = sum(Sector_Share_Services, na.rm = TRUE), Name = first(new_name), .groups = "drop") |>
                    rename(AGS = new_id)
            }
        }
        return(current_data)
    }

    fill_missing_with_predecessors <- function(harmonized_data, raw_data, year_src) {
        all_districts <- unique(raw_data$AGS)
        present_districts <- harmonized_data |> filter(Year == year_src) |> pull(AGS)
        missing_districts <- setdiff(all_districts, present_districts)
        if (length(missing_districts) == 0) return(harmonized_data)

        filled_rows <- list()
        for (new_dist in missing_districts) {
            found_value <- FALSE
            for (search_year in year_src:min(2022, year_src + 20)) {
                map <- get_transition_map(ref_file, search_year)
                if (!is.null(map)) {
                    predecessors <- map |> filter(new_id == new_dist)
                    if (nrow(predecessors) > 0) {
                        pred_ids <- unique(predecessors$old_id)
                        pred_values <- raw_data |> filter(AGS %in% pred_ids, Year == year_src)
                        if (nrow(pred_values) > 0) {
                            weighted_values <- pred_values |> left_join(predecessors, by = c("AGS" = "old_id")) |> filter(!is.na(weight))
                            if (nrow(weighted_values) > 0) {
                                filled_value <- weighted_values |> summarise(Year = year_src, AGS = new_dist, Capital_Stock_Total = sum(Capital_Stock_Total * weight, na.rm = TRUE), Capital_Stock_Per_Capita = sum(Capital_Stock_Per_Capita * weight, na.rm = TRUE), Sector_Share_Agriculture = sum(Sector_Share_Agriculture * weight, na.rm = TRUE), Sector_Share_Industry = sum(Sector_Share_Industry * weight, na.rm = TRUE), Sector_Share_Services = sum(Sector_Share_Services * weight, na.rm = TRUE), Name = first(new_name), .groups = "drop")
                                filled_rows[[length(filled_rows) + 1]] <- filled_value
                                found_value <- TRUE; break
                            }
                        }
                    }
                }
            }
        }

        if (length(filled_rows) > 0) {
            harmonized_data <- harmonized_data |> bind_rows(bind_rows(filled_rows)) |> arrange(Year, AGS)
        }
        return(harmonized_data)
    }

    # --- Main Processing ---
    message("===== STARTING CAPITAL STOCK HARMONIZATION =====")
    if (!file.exists(ref_file)) stop("Reference file not found: ", ref_file)

    capital_data <- readr::read_delim(input_file, delim = ";", show_col_types = FALSE) %>%
        dplyr::mutate(AGS = sprintf("%05s", as.character(AGS))) %>%
        dplyr::filter(Year >= 2003 & Year <= 2021)

    years <- sort(unique(capital_data$Year))
    harmonized_list <- list()

    for (yr in years) {
        message("\n→ Processing Year: ", yr)
        year_data <- capital_data |> filter(Year == yr)
        harmonized_year <- harmonize_capital_stock(year_data, yr)
        harmonized_year <- fill_missing_with_predecessors(harmonized_year, capital_data, yr)
        harmonized_list[[as.character(yr)]] <- harmonized_year
    }

    harmonized_data <- dplyr::bind_rows(harmonized_list)

    # Post-processing: Fix city-state per capita
    pop_file <- here::here("01_datasets/processed/INKAR/01-translated/population_census_corrected_1995-2023.csv")
    if (file.exists(pop_file)) {
        pop_data <- read.csv(pop_file) |>
            dplyr::select(district_id, dplyr::starts_with("population_census_corrected_")) |>
            tidyr::pivot_longer(cols = dplyr::starts_with("population_census_corrected_"), names_to = "Year", values_to = "Population") |>
            dplyr::mutate(Year = as.integer(stringr::str_replace(Year, "population_census_corrected_", ""))) |>
            dplyr::rename(AGS = district_id) |>
            dplyr::mutate(AGS = stringr::str_pad(as.character(AGS), width = 5, pad = "0")) |>
            dplyr::select(AGS, Year, Population)

        harmonized_data <- harmonized_data |>
            dplyr::left_join(pop_data, by = c("AGS", "Year")) |>
            dplyr::mutate(Capital_Stock_Per_Capita = dplyr::if_else((is.na(Capital_Stock_Per_Capita) | Capital_Stock_Per_Capita == 0) & !is.na(Capital_Stock_Total) & !is.na(Population) & Population > 0, (Capital_Stock_Total * 1e6) / Population, Capital_Stock_Per_Capita)) |>
            dplyr::select(-Population)
    }

    harmonized_data <- harmonized_data |> dplyr::arrange(Year, AGS)
    readr::write_delim(harmonized_data, output_file, delim = ";")
    message("\n===== COMPLETED =====")
}
