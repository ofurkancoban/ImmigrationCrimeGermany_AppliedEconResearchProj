# ==============================================================================
# File:          04_PKS_Spatial_Harmonization.R
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
# Description:   Performs spatial harmonization on the 19-year longitudinal PKS dataset. Because German district borders (Kreise) changed drastically between 2003 and 2021 due to territorial reforms, this script maps historic crime figures to the static 2023 district geometry using BBSR population-weighted transition keys (Umsteigeschlüssel).
#
# Inputs:        Intermediate PKS CSV files and BBSR Transition Key Tables
# Outputs:       Harmonized panel dataset mapping historic crime data to current borders
# ==============================================================================

# 1. SETUP
# ------------------------------------------------------------------------------
# Source global utilities (packages, directory initialization, robustness helpers)
source(here::here("02_scripts", "02_data_preprocessing", "00_import.R"))

# Script-specific configuration
force_process <- FALSE # Set to TRUE to overwrite existing files

# Suppress R CMD check NOTEs for NSE (dplyr column references)
utils::globalVariables(c(
  "old_id", "new_id", "weight", "total_w", "Kreisschluessel", "Kreis",
  "Kreisart", "Schluesselzahl", "Straftat", "new_name", "Einwohner",
  "Erfasste_Faelle", "Aufklaerung_Faelle", "Versuche_Anzahl",
  "Tatverdaechtige_Insgesamt", "Nichtdeutsche_Tatverdaechtige_Anzahl"
))

options(warn = 1) # Print warnings immediately

input_dir <- here::here("01_datasets/processed/PKS/01-converted")
output_dir <- ensure_dir(here::here("01_datasets/processed/PKS/02-harmonized"))
ref_file <- here::here("01_datasets/tools/INKAR_ref-kreise-1990-2023.xlsx")
ref_url <- "https://www.bbsr.bund.de/BBSR/DE/forschung/raumbeobachtung/Raumabgrenzungen/umstiegsschluessel/ref-kreise-1990-2023.xlsx?__blob=publicationFile&v=2"
target_year <- 2023

# --- Download Reference Data (if missing) ---
if (!file.exists(ref_file)) {
    message("ℹ Reference file not found. Downloading...")
    ensure_dir(dirname(ref_file))
    tryCatch({
        download.file(ref_url, ref_file, mode = "wb")
        message("✓ Download complete: ", basename(ref_file))
    }, error = function(e) {
        stop("Failed to download reference file: ", e$message)
    })
}

# --- Fix Reference Data (Create _fixed.xlsx) ---
ref_file_fixed <- file.path(dirname(ref_file), "INKAR_ref-kreise-1990-2023_fixed.xlsx")

if (!file.exists(ref_file_fixed)) {
  message("ℹ Creating fixed reference file: ", basename(ref_file_fixed))
  wb <- openxlsx::loadWorkbook(ref_file)
  sheets_to_fix <- c("2014-2015", "2015-2016")

  for (sheet_name in sheets_to_fix) {
    message("  → Checking sheet: ", sheet_name)
    df <- openxlsx::read.xlsx(wb, sheet = sheet_name, colNames = FALSE, skipEmptyRows = FALSE)
    mk_row_idx <- which(apply(df, 1, function(x) {
      any(grepl("7137", x)) && any(grepl("Mayen-Koblenz", x))
    }))

    if (length(mk_row_idx) == 1) {
      message("    • Found Mayen-Koblenz in sheet '", sheet_name, "' at row ", mk_row_idx)
      target_cols <- c(3, 4, 5)
      for (col in target_cols) {
        openxlsx::writeData(wb, sheet = sheet_name, x = 1.0, startRow = mk_row_idx, startCol = col, colNames = FALSE)
      }
    }
  }
  openxlsx::saveWorkbook(wb, ref_file_fixed, overwrite = TRUE)
}
ref_file <- ref_file_fixed

# --- Helper Functions ---
get_transition_map <- function(ref_file, year_t) {
  year_t1 <- year_t + 1
  sheet_name <- paste0(year_t, "-", year_t1)
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
    dplyr::select(
      old_id = dplyr::all_of(col_name_t),
      new_id = dplyr::all_of(col_name_t1),
      weight = dplyr::all_of(weight_col_name),
      old_name = dplyr::all_of(paste0("Kreisname ", year_t)),
      new_name = dplyr::all_of(paste0("Kreisname ", year_t1))
    ) |>
    dplyr::mutate(
      old_id = as.character(as.integer(old_id)),
      new_id = as.character(as.integer(new_id)),
      weight = as.numeric(weight)
    ) |>
    dplyr::filter(!is.na(old_id) & !is.na(new_id))

  transition <- transition |>
    group_by(old_id) |>
    mutate(total_w = sum(weight)) |>
    ungroup() |>
    mutate(weight = ifelse(total_w >= 0.95, weight / total_w, weight)) |>
    select(-total_w)

  clean_id <- function(x) {
    s <- sprintf("%08d", as.numeric(x))
    substr(s, 1, 5)
  }
  transition <- transition |> mutate(old_id = clean_id(old_id), new_id = clean_id(new_id))
  return(transition)
}

harmonize_dataset <- function(data, year_src) {
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

      current_data <- current_data |> mutate(Kreisschluessel = stringr::str_trim(Kreisschluessel))
      joined <- current_data |> left_join(map, by = c("Kreisschluessel" = "old_id"), relationship = "many-to-many") |> filter(!is.na(new_id))

      exclude_cols <- c("Kreisschluessel", "Schluesselzahl", "Straftat", "Kreis", "Kreisart", "old_id", "new_id", "weight", "old_name", "new_name", "total_w", "Jahr")
      prop_patterns <- "_HZ$|_Prozent$|_quote$|Quote"
      val_cols <- names(joined)[sapply(joined, is.numeric)]
      val_cols <- setdiff(val_cols, exclude_cols)
      val_cols <- val_cols[!stringr::str_detect(val_cols, stringr::regex(prop_patterns, ignore_case = TRUE))]

      for (v in val_cols) joined[[v]] <- joined[[v]] * joined$weight
      current_data <- joined |> group_by(Schluesselzahl, Straftat, new_id) |>
        summarise(across(all_of(val_cols), \(x) if (all(is.na(x))) NA else sum(x, na.rm = TRUE)), Kreis = first(new_name), Kreisart = first(Kreisart), .groups = "drop") |>
        rename(Kreisschluessel = new_id)
    }
  }

  # Recalculate Rates
  if ("Erfasste_Faelle" %in% names(current_data) && "Einwohner" %in% names(current_data)) {
    current_data <- current_data |> mutate(Straftaten_HZ = ifelse(Einwohner > 0, (Erfasste_Faelle / Einwohner * 100000), 0))
  }
  if ("Aufklaerung_Faelle" %in% names(current_data) && "Erfasste_Faelle" %in% names(current_data)) {
    current_data <- current_data |> mutate(Aufklaerungsquote = ifelse(Erfasste_Faelle > 0, (Aufklaerung_Faelle / Erfasste_Faelle * 100), 0))
  }

  standard_col_order <- c("Schluesselzahl", "Straftat", "Kreisschluessel", "Kreisart", "Kreis", "Einwohner", "Erfasste_Faelle", "Straftaten_HZ", "Versuche_Anzahl", "Versuche_Anteil_Prozent", "Mit_Schusswaffe_gedroht", "Mit_Schusswaffe_geschossen", "Aufklaerung_Faelle", "Aufklaerungsquote", "Tatverdaechtige_Insgesamt", "Tatverdaechtige_Maennlich", "Tatverdaechtige_Weiblich", "Nichtdeutsche_Tatverdaechtige_Anzahl", "Nichtdeutsche_Tatverdaechtige_Prozent")
  final_cols <- names(current_data)
  ordered_cols <- c(intersect(standard_col_order, final_cols), setdiff(final_cols, standard_col_order))
  current_data <- current_data |> dplyr::arrange(Schluesselzahl, Kreisschluessel) |> dplyr::select(all_of(ordered_cols))
  return(current_data)
}

# --- Main Processing Loop ---
files <- list.files(input_dir, pattern = "\\.csv$", full.names = TRUE)
files <- files[stringr::str_detect(basename(files), "^20[0-9]{2}")]

message("===== STARTING UNIVERSAL HARMONIZATION =====")

for (f in files) {
  filename <- basename(f)
  year <- as.integer(stringr::str_extract(filename, "^20[0-9]{2}"))
  out_name <- paste0(year, "_PKS_harmonized.csv")
  out_path <- file.path(output_dir, out_name)

  # Standardized skip logic
  if (should_skip(out_path, force = force_process)) next

  message(paste0("\n→ Processing: ", filename, " (Year: ", year, ")"))
  data <- read.csv(f, colClasses = "character")
  if ("Einwohner" %in% names(data)) data$Einwohner <- as.numeric(data$Einwohner)

  exclude_conversion <- c("Kreisschluessel", "Schluesselzahl", "Straftat", "Kreis", "Kreisart", "Jahr", "Schluessel", "Kreis_Type", "old_id", "new_id")
  cols_to_convert <- setdiff(names(data), exclude_conversion)
  for (col in cols_to_convert) data[[col]] <- suppressWarnings(as.numeric(data[[col]]))

  harmonized_data <- harmonize_dataset(data, year)
  old_digits <- getOption("digits"); old_scipen <- getOption("scipen"); options(digits = 22, scipen = 999)
  write.csv(harmonized_data, out_path, row.names = FALSE)
  options(digits = old_digits, scipen = old_scipen)
  message("    ✓ Saved: ", out_name)
}

message("\n===== COMPLETED =====")
