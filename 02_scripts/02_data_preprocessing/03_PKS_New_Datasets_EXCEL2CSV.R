# ==============================================================================
# File:          03_PKS_New_Datasets_EXCEL2CSV.R
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
# Description:   Extracts and cleans district-level crime figures from the modern PKS Excel workbooks (2013-2024). It bypasses varying Excel sheet structures, standardizes column names, isolates district keys (Schlüssel), and concatenates yearly crime metrics into continuous structured representations.
#
# Inputs:        Raw Excel files downloaded to 01_datasets/raw/PKS/
# Outputs:       Standardized intermediate CSV files saved to 01_datasets/processed/PKS/01-converted/
# ==============================================================================

# 1. SETUP
# ------------------------------------------------------------------------------
# Source global utilities (packages, directory initialization, robustness helpers)
source(here::here("02_scripts", "02_data_preprocessing", "00_import.R"))

# Script-specific configuration
force_process <- FALSE # Set to TRUE to overwrite existing files

# Suppress R CMD check NOTEs for NSE (dplyr column references)
utils::globalVariables(c(
    "Kreisart",
    "Kreis",
    "Schluesselzahl",
    "Kreisschluessel"
))

# Directory paths
input_dir <- here::here("01_datasets/raw/PKS")
output_dir <- ensure_dir(here::here("01_datasets/processed/PKS/01-converted/"))

# Get list of Excel files (excluding old_datasets directory)
files <- list.files(input_dir, pattern = "\\.xls[x]?$", full.names = TRUE)
files <- files[!file.info(files)$isdir]

###########################################################################
# Configuration
###########################################################################

# Define standard column names
col_names <- c(
    "Schluesselzahl",
    "Straftat",
    "Kreisschluessel",
    "Kreisart",
    "Kreis",
    "Erfasste_Faelle",
    "Straftaten_HZ", # Standardized from HZ_nach_Zensus_2011
    "Versuche_Anzahl",
    "Versuche_Anteil_Prozent",
    "Mit_Schusswaffe_gedroht",
    "Mit_Schusswaffe_geschossen",
    "Aufklaerung_Faelle",
    "Aufklaerungsquote",
    "Tatverdaechtige_Insgesamt",
    "Tatverdaechtige_Maennlich",
    "Tatverdaechtige_Weiblich",
    "Nichtdeutsche_Tatverdaechtige_Anzahl",
    "Nichtdeutsche_Tatverdaechtige_Prozent"
)

###########################################################################
# Helper Functions
###########################################################################

process_file <- function(file_path) {
    filename <- basename(file_path)
    
    # Construct output path early for skip check
    base_name <- stringr::str_remove(filename, "\\.xls[x]?$")
    output_file <- file.path(output_dir, paste0(base_name, ".csv"))
    
    # Standardized skip logic
    if (should_skip(output_file, force = force_process)) return(TRUE)

    message("→ Processing: ", filename)

    tryCatch(
        {
            # Identify sheet
            sheets <- readxl::excel_sheets(file_path)
            target_sheet <- sheets[1]

            # Read first 30 rows to find header/data start
            header_check <- suppressMessages(readxl::read_excel(
                file_path,
                sheet = target_sheet,
                col_names = FALSE,
                n_max = 30
            ))

            # Find the row index where the first column is EXACTLY "------"
            start_row_idx <- which(header_check[[1]] == "------")[1]

            skip_count <- 9 # Default fallback

            if (!is.na(start_row_idx)) {
                skip_count <- start_row_idx - 1
            } else {
                # Fallback: Try to find "Straftaten insgesamt" in col 2
                start_row_idx <- which(
                    header_check[[2]] == "Straftaten insgesamt"
                )[1]
                if (!is.na(start_row_idx)) {
                    skip_count <- start_row_idx - 1
                } else {
                    message(
                        "  ! Could not detect data start. Defaulting to skip: ",
                        skip_count
                    )
                }
            }

            # Read the full data
            data <- suppressMessages(readxl::read_excel(
                file_path,
                sheet = target_sheet,
                skip = skip_count,
                col_names = FALSE
            ))

            # Handle Column Mismatch / Truncation
            if (ncol(data) > length(col_names)) {
                data <- data[, seq_along(col_names)]
            } else if (ncol(data) < length(col_names)) {
                message(
                    "  ℹ File has fewer columns (",
                    ncol(data),
                    ") than expected (",
                    length(col_names),
                    "). Appending NAs."
                )
                missing_count <- length(col_names) - ncol(data)
                # Append NA columns efficiently
                new_cols <- matrix(NA, nrow = nrow(data), ncol = missing_count)
                colnames(new_cols) <- paste0("Missing_", 1:missing_count) # Temporary names
                data <- cbind(data, as.data.frame(new_cols))
            }

            # Assign temporary names to check content
            colnames(data) <- col_names

            # Check for Column Swapping (Kreis vs Kreisart)
            sample_rows <- head(
                data[!is.na(data$Kreisart) & data$Kreisart != "------", ],
                20
            )

            if (nrow(sample_rows) > 0) {
                col4_vals <- sample_rows$Kreisart
                col5_vals <- sample_rows$Kreis

                type_patterns <- c(
                    "^KfS", "^LK", "^SK", "^K$", "^Region", "^RV"
                )
                col4_is_type <- mean(stringr::str_detect(
                    col4_vals,
                    paste(type_patterns, collapse = "|")
                )) > 0.5
                col5_is_type <- mean(stringr::str_detect(
                    col5_vals,
                    paste(type_patterns, collapse = "|")
                )) > 0.5

                if (!col4_is_type && col5_is_type) {
                    message("  ! Detected swapped Kreis/Kreisart columns. Swapping back.")
                    data <- data |>
                        dplyr::rename(Kreis = Kreisart, Kreisart = Kreis)
                    data <- data |> dplyr::select(dplyr::all_of(col_names))
                }
            }

            # Filter out empty rows or rows where Schluesselzahl is NA
            data <- data |> dplyr::filter(!is.na(Schluesselzahl))

            # --- Data Quality Reporting ---
            na_count <- sum(is.na(data))
            message("  ✓ Stats -> NA: ", na_count)

            # Ensure all standard columns exist (if missing, add NA)
            standard_cols <- c(
                "Schluesselzahl", "Straftat", "Kreisschluessel", "Kreisart", "Kreis",
                "Einwohner", "Erfasste_Faelle", "Straftaten_HZ", "Versuche_Anzahl",
                "Versuche_Anteil_Prozent", "Mit_Schusswaffe_gedroht", "Mit_Schusswaffe_geschossen",
                "Aufklaerung_Faelle", "Aufklaerungsquote", "Tatverdaechtige_Insgesamt",
                "Tatverdaechtige_Maennlich", "Tatverdaechtige_Weiblich",
                "Nichtdeutsche_Tatverdaechtige_Anzahl", "Nichtdeutsche_Tatverdaechtige_Prozent"
            )

            # Identify missing columns
            missing_cols <- setdiff(standard_cols, names(data))
            if (length(missing_cols) > 0) {
                for (col in missing_cols) {
                    data[[col]] <- NA
                }
            }

            # --- Back-calculate Einwohner (Population) ---
            # Modern PKS Excel files often omit the population column, 
            # but provide Counts and Rates (HZ). This back-calculation ensures 
            # downstream spatial harmonization scripts can correctly recalculate 
            # rates after district border shifts.
            # Formula: Population = (Cases / HZ) * 100,000
            data <- data |>
                dplyr::mutate(
                    Erfasste_Faelle = as.numeric(Erfasste_Faelle),
                    Straftaten_HZ = as.numeric(Straftaten_HZ),
                    Einwohner = dplyr::if_else(
                        is.na(Einwohner) & !is.na(Straftaten_HZ) & Straftaten_HZ > 0,
                        (Erfasste_Faelle / Straftaten_HZ) * 100000,
                        as.numeric(Einwohner)
                    )
                )

            # Reorder and Select
            data <- data |>
                dplyr::arrange(Schluesselzahl, Kreisschluessel) |>
                dplyr::select(dplyr::all_of(standard_cols))

            # Save to CSV
            old_digits <- getOption("digits")
            old_scipen <- getOption("scipen")
            options(digits = 22, scipen = 999)
            utils::write.csv(data, output_file, row.names = FALSE, na = "")
            options(digits = old_digits, scipen = old_scipen)

            message("  ✓ Saved: ", basename(output_file))
        },
        error = function(e) {
            message("  × Error processing file: ", e$message)
        }
    )
}

###########################################################################
# Main Processing Loop
###########################################################################

message("\n===== PROCESSING EXCEL FILES (2013–2024) =====")

for (f in files) {
    process_file(f)
}

message("\n===== EXCEL PROCESSING COMPLETED =====\n")
