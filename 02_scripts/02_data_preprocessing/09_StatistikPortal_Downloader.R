# ==============================================================================
# File:          09_StatistikPortal_Downloader.R
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
# Description:   Targeted script to acquire Gross Value Added (GVA) and State-Level Capital Stock aggregates from the Federal Statistical Office (Statistikportal) API/Web portal. These macro-economic fundamentals are necessary to proxy Regional Capital formation.
#
# Inputs:        None (Automated URL scraping)
# Outputs:       Raw national and state-level economic tables saved to 01_datasets/raw/StatistikPortal/
# ==============================================================================

# 1. SETUP
# ------------------------------------------------------------------------------
# Source global utilities (packages, directory initialization, robustness helpers)
source(here::here("02_scripts", "02_data_preprocessing", "00_import.R"))

# Script-specific configuration
force_download <- FALSE # Set to TRUE to overwrite existing files

# Directory paths
output_dir <- ensure_dir(here::here("01_datasets", "raw", "StatistikPortal"))

###########################################################################
# Helper: clean filename
###########################################################################
clean_filename_from_url <- function(url) {
  fname <- basename(url)
  fname <- sub("\\?.*$", "", fname) # remove query string
  return(fname)
}

###########################################################################
# Downloader Function
###########################################################################
download_file <- function(url, dest_path) {
  filename <- basename(dest_path)
  
  # Standardized skip logic
  if (should_skip(dest_path, force = force_download)) {
    message("✓ Already exists: ", filename)
    return(TRUE)
  }

  message("→ Downloading: ", filename)

  # GET request
  resp <- try(httr::GET(url, httr::write_disk(dest_path, overwrite = TRUE), httr::timeout(60)), silent = TRUE)

  if (inherits(resp, "try-error")) {
    message("× Error requesting: ", url)
    return(FALSE)
  }

  if (httr::status_code(resp) != 200) {
    message("× HTTP ", httr::status_code(resp), " for ", filename)
    unlink(dest_path) 
    return(FALSE)
  }

  # File size check
  if (file.size(dest_path) < 1000) {
    message("× File too small (poss. HTML/Error) — skipping: ", filename)
    unlink(dest_path)
    return(FALSE)
  }

  message("✓ Saved: ", filename)
  return(TRUE)
}

###########################################################################
# Execution
###########################################################################

datasets <- list(
  # Capital Stock (State Level)
  list(url = "https://www.statistikportal.de/sites/default/files/2024-06/vgrdl_r1b4_bs2023.xlsx", name = "vgrdl_r1b4_bs2023.xlsx"),
  
  # Gross Value Added (District Level)
  list(url = "https://www.statistikportal.de/sites/default/files/2025-12/vgrdl_r2b1_bs2024.xlsx", name = "vgrdl_r2b1_bs2024.xlsx"),
  
  # Population (District Level, Band 3 usually contains population data)
  list(url = "https://www.statistikportal.de/sites/default/files/2024-08/vgrdl_r2b3_bs2023.xlsx", name = "vgrdl_r2b3_bs2023.xlsx")
)

message("\n===== STARTING STATISTIKPORTAL DOWNLOADS =====\n")

for (ds in datasets) {
  download_file(ds$url, file.path(output_dir, ds$name))
}

message("\n===== ALL DOWNLOADS COMPLETED =====\n")
