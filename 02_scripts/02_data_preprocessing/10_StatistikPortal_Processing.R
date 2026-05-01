# ==============================================================================
# File:          10_StatistikPortal_Processing.R
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
# Description:   Calculates proxy Capital Stock values at the district level. Since fixed assets data is only published at the state level by German authorities, this script employs a sector-specific top-down distribution algorithm, downscaling state capital to districts relative to their Gross Value Added (GVA) contributions.
#
# Inputs:        Raw tables from 01_datasets/raw/StatistikPortal/
# Outputs:       Intermediate Capital Stock metrics for the 19-year timeframe
# ==============================================================================

# 1. SETUP
# ------------------------------------------------------------------------------
# Source global utilities (packages, directory initialization, robustness helpers)
source(here::here("02_scripts", "02_data_preprocessing", "00_import.R"))

# Script-specific configuration
force_process <- FALSE # Set to TRUE to overwrite existing files

# Input Files
state_file <- here::here("01_datasets", "raw", "StatistikPortal", "vgrdl_r1b4_bs2023.xlsx")
district_file <- here::here("01_datasets", "raw", "StatistikPortal", "vgrdl_r2b1_bs2024.xlsx")

# Output File
output_csv <- here::here("01_datasets", "processed", "StatistikPortal", "01-processed", "german_districts_capital_stock_2003_2021.csv")

# Standardized skip logic
if (should_skip(output_csv, force = force_process)) {
    message("✓ Output already exists: ", basename(output_csv))
    # Wrap main execution in a skip logic flag
    run_main <- FALSE
} else {
    run_main <- TRUE
    ensure_dir(dirname(output_csv))
}

if (run_main) {
    message("Starting Capital Stock Calculation...")
    options(scipen = 999)

    # -------------------------------------------------------------------------
    # Helper Functions
    # -------------------------------------------------------------------------

    clean_numeric_column <- function(x) {
        x <- as.character(x)
        x[x %in% c(".", "-", "")] <- NA
        x <- stringr::str_replace_all(x, ",", ".")
        as.numeric(x)
    }

    pad_ags <- function(x) {
        stringr::str_pad(x, width = 5, pad = "0")
    }

    sectors <- list(
        list(state_sheet = "1.4.2", district_sheet = "3.2", label = "Agriculture"),
        list(state_sheet = "1.4.3", district_sheet = "3.3", label = "Industry"),
        list(state_sheet = "1.4.4", district_sheet = "3.4", label = "Services")
    )

    read_state_data <- function(sheet_name, sector_label) {
        header <- readxl::read_excel(state_file, sheet = sheet_name, range = "A3:W3", col_names = FALSE)
        state_names <- as.character(header[1, ])
        state_names <- state_names[!is.na(state_names)]
        state_names <- state_names[!state_names %in% c("Jahr", "Lfd. Nr.")]

        df <- readxl::read_excel(state_file, sheet = sheet_name, skip = 5, col_types = "text", col_names = FALSE)
        colnames(df)[1] <- "Year"
        first_na <- which(is.na(df[[1]]))[1]
        if (!is.na(first_na)) df <- df[1:(first_na - 1), ]

        if ((ncol(df) - 1) < length(state_names)) state_names <- state_names[1:(ncol(df) - 1)]
        colnames(df)[2:(1 + length(state_names))] <- state_names

        df_long <- df %>%
            dplyr::select(Year, dplyr::any_of(state_names)) %>%
            dplyr::filter(!is.na(Year)) %>%
            dplyr::filter(stringr::str_detect(Year, "^[0-9]{4}$")) %>%
            dplyr::mutate(Year = as.integer(Year)) %>%
            dplyr::filter(Year >= 2003) %>%
            tidyr::pivot_longer(cols = -Year, names_to = "Raw_State_Name", values_to = "Capital_Stock_State") %>%
            dplyr::mutate(Capital_Stock_State = clean_numeric_column(Capital_Stock_State), Sector = sector_label)

        map_state_to_ags <- function(name) {
            name <- stringr::str_replace_all(name, "[\r\n]", "")
            name <- stringr::str_remove_all(name, "-")
            name <- stringr::str_squish(name)
            dplyr::case_when(
                stringr::str_detect(name, "Schleswig") ~ "01",
                stringr::str_detect(name, "^Hamburg$") ~ "02",
                stringr::str_detect(name, "Niedersachsen") ~ "03",
                stringr::str_detect(name, "Bremen") ~ "04",
                stringr::str_detect(name, "Nordrhein") ~ "05",
                stringr::str_detect(name, "Hessen") ~ "06",
                stringr::str_detect(name, "RheinlandPfalz") ~ "07",
                stringr::str_detect(name, "Baden") ~ "08",
                stringr::str_detect(name, "Bayern") ~ "09",
                stringr::str_detect(name, "Saarland") ~ "10",
                stringr::str_detect(name, "^Berlin$") ~ "11",
                stringr::str_detect(name, "Brandenburg") ~ "12",
                stringr::str_detect(name, "Mecklenburg") ~ "13",
                stringr::str_detect(name, "SachsenAnhalt") ~ "15",
                stringr::str_detect(name, "Sachsen") & !stringr::str_detect(name, "Anhalt") ~ "14",
                stringr::str_detect(name, "Thüringen") ~ "16",
                TRUE ~ NA_character_
            )
        }

        df_long <- df_long %>%
            dplyr::mutate(AGS_State = map_state_to_ags(Raw_State_Name)) %>%
            dplyr::filter(!is.na(AGS_State))
        return(df_long)
    }

    read_district_data <- function(sheet_name, value_name, is_gva = TRUE) {
        df <- readxl::read_excel(district_file, sheet = sheet_name, skip = 4, col_types = "text", .name_repair = "unique")
        df <- janitor::clean_names(df)
        year_cols <- grep("^x[0-9]{4}$", colnames(df), value = TRUE)
        df_long <- df %>%
            dplyr::rename(AGS = regional_schlussel, Name = gebietseinheit, Land_Name = land) %>%
            dplyr::select(AGS, Name, Land_Name, dplyr::all_of(year_cols)) %>%
            dplyr::filter(!is.na(AGS)) %>%
            tidyr::pivot_longer(cols = dplyr::all_of(year_cols), names_to = "Year", values_to = "Value_District") %>%
            dplyr::mutate(Year = as.integer(stringr::str_remove(Year, "^x")), Value_District = clean_numeric_column(Value_District))
        if (is_gva) df_long <- df_long %>% dplyr::filter(Year >= 2003)
        return(df_long)
    }

    # --- Main Processing ---
    state_data_list <- list()
    for (sec in sectors) state_data_list[[sec$label]] <- read_state_data(sec$state_sheet, sec$label)
    state_data <- dplyr::bind_rows(state_data_list)

    gva_agri <- read_district_data("3.2", "Agriculture") %>% dplyr::mutate(Sector = "Agriculture")
    gva_ind <- read_district_data("3.3", "Industry") %>% dplyr::mutate(Sector = "Industry")
    gva_serv <- read_district_data("3.4", "Services") %>% dplyr::mutate(Sector = "Services")
    dist_gva <- dplyr::bind_rows(gva_agri, gva_ind, gva_serv)

    dist_pop <- read_district_data("5", "Population", is_gva = FALSE) %>%
        dplyr::mutate(Population = Value_District * 1000, AGS = dplyr::case_when(AGS == "02" ~ "02000", AGS == "11" ~ "11000", TRUE ~ pad_ags(AGS))) %>%
        dplyr::select(AGS, Year, Population)

    gva_states <- dist_gva %>% dplyr::filter(nchar(AGS) == 2) %>% dplyr::rename(GVA_State = Value_District, AGS_State = AGS) %>% dplyr::select(AGS_State, Year, Sector, GVA_State)
    gva_districts <- dist_gva %>% dplyr::filter(nchar(AGS) == 5) %>% dplyr::mutate(AGS_State = substr(AGS, 1, 2)) %>% dplyr::select(AGS, Name, Land_Name, AGS_State, Year, Sector, Value_District)

    final_calc <- gva_districts %>%
        dplyr::left_join(gva_states, by = c("AGS_State", "Year", "Sector")) %>%
        dplyr::left_join(state_data, by = c("AGS_State", "Year", "Sector")) %>%
        dplyr::mutate(Weight = dplyr::if_else(GVA_State > 0, Value_District / GVA_State, 0), Capital_Stock_District = Capital_Stock_State * Weight)

    capital_total <- final_calc %>%
        dplyr::group_by(AGS, Name, Land_Name, Year) %>%
        dplyr::summarise(Capital_Stock_Total = sum(Capital_Stock_District, na.rm = TRUE), GVA_Total = sum(Value_District, na.rm = TRUE), Industry_GVA = sum(Value_District[Sector == "Industry"], na.rm = TRUE), Agriculture_GVA = sum(Value_District[Sector == "Agriculture"], na.rm = TRUE), Services_GVA = sum(Value_District[Sector == "Services"], na.rm = TRUE), .groups = "drop")

    city_states <- state_data %>%
        dplyr::filter(AGS_State %in% c("02", "11")) %>%
        dplyr::group_by(AGS_State, Year) %>%
        dplyr::summarise(Capital_Stock_Total = sum(Capital_Stock_State, na.rm = TRUE), .groups = "drop") %>%
        dplyr::rename(AGS = AGS_State) %>%
        dplyr::mutate(AGS = paste0(AGS, "000"), Name = dplyr::case_when(AGS == "02000" ~ "Hamburg", AGS == "11000" ~ "Berlin", TRUE ~ NA_character_), Land_Name = Name)

    city_states_gva <- dist_gva %>%
        dplyr::filter(nchar(AGS) == 2, AGS %in% c("02", "11")) %>%
        dplyr::group_by(AGS, Year) %>%
        dplyr::summarise(GVA_Total = sum(Value_District, na.rm = TRUE), Industry_GVA = sum(Value_District[Sector == "Industry"], na.rm = TRUE), Agriculture_GVA = sum(Value_District[Sector == "Agriculture"], na.rm = TRUE), Services_GVA = sum(Value_District[Sector == "Services"], na.rm = TRUE), .groups = "drop") %>%
        dplyr::mutate(AGS = paste0(AGS, "000"))

    city_states <- city_states %>%
        dplyr::left_join(city_states_gva, by = c("AGS", "Year")) %>%
        dplyr::left_join(dist_pop, by = c("AGS", "Year")) %>%
        dplyr::mutate(Capital_Stock_Per_Capita = dplyr::if_else(!is.na(Population) & Population > 0, (Capital_Stock_Total * 1e6) / Population, NA_real_)) %>%
        dplyr::select(-Population)

    final_dataset <- dplyr::bind_rows(capital_total, city_states) %>%
        dplyr::mutate(AGS = pad_ags(AGS)) %>%
        dplyr::mutate(Sector_Share_Industry = dplyr::if_else(GVA_Total > 0, Industry_GVA / GVA_Total, 0), Sector_Share_Agriculture = dplyr::if_else(GVA_Total > 0, Agriculture_GVA / GVA_Total, 0), Sector_Share_Services = dplyr::if_else(GVA_Total > 0, Services_GVA / GVA_Total, 0)) %>%
        dplyr::left_join(dist_pop, by = c("AGS", "Year")) %>%
        dplyr::mutate(Capital_Stock_Per_Capita = dplyr::if_else(!is.na(Capital_Stock_Per_Capita), Capital_Stock_Per_Capita, dplyr::if_else(Population > 0, (Capital_Stock_Total * 1e6) / Population, NA_real_))) %>%
        dplyr::filter(Year <= 2021) %>%
        dplyr::select(AGS, Name, Year, Capital_Stock_Total, Capital_Stock_Per_Capita, Sector_Share_Agriculture, Sector_Share_Industry, Sector_Share_Services) %>%
        dplyr::arrange(AGS, Year)

    write.table(final_dataset, file = output_csv, sep = ";", dec = ".", row.names = FALSE, quote = FALSE, fileEncoding = "UTF-8")
    message(sprintf("Saved processed file to: %s", output_csv))
}
