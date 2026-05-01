# ==============================================================================
# File:          01_PKS_Dataset_Downloader.R
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
# Description:   This script dynamically downloads raw Police Crime Statistics (Polizeiliche Kriminalstatistik - PKS) data from the official Bundeskriminalamt (BKA) servers. It resolves historic PDF archives (2003-2012) and modern Excel tables (2013-2024), utilizing robust web-scraping to collect longitudinal district-level crime cases.
#
# Inputs:        None (Scrapes dynamically from BKA URLs)
# Outputs:       Raw PDF (.pdf) and raw Excel (.xlsx/.xls) PKS tables archived in 01_datasets/raw/PKS/
# ==============================================================================

# 1. SETUP
# ------------------------------------------------------------------------------
# Source global utilities (packages, directory initialization, robustness helpers)
source(here::here("02_scripts", "02_data_preprocessing", "00_import.R"))

# Script-specific configuration
force_download <- FALSE # Set to TRUE to overwrite existing files

# Folder structure
dir_pdf <- ensure_dir(here::here("01_datasets/raw/PKS/old_datasets"))
dir_xlsx <- ensure_dir(here::here("01_datasets/raw/PKS"))

###########################################################################
# Helper: sanitize filename from URL
###########################################################################

clean_filename_from_url <- function(url) {
  fname <- basename(url)
  fname <- sub("\\?.*$", "", fname) # remove query string
  return(fname)
}

###########################################################################
# Universal downloader
###########################################################################

download_file <- function(url, year) {
  message("→ Downloading ", year)

  # Choose folder based on extension
  filename <- clean_filename_from_url(url)

  # If URL does not contain extension, default guess
  ext <- tools::file_ext(filename)

  if (ext == "pdf") {
    outpath <- file.path(dir_pdf, paste0(year, "_", filename))
  } else {
    outpath <- file.path(dir_xlsx, paste0(year, "_", filename))
  }

  # Standardized skip logic from 00_import.R
  if (should_skip(outpath, force = force_download)) {
    message("✓ Already exists: ", basename(outpath))
    return(TRUE)
  }

  # GET request
  resp <- try(httr::GET(url, httr::timeout(30)), silent = TRUE)

  if (inherits(resp, "try-error")) {
    message("× Error requesting: ", url)
    return(FALSE)
  }

  if (httr::status_code(resp) != 200) {
    message("× HTTP ", httr::status_code(resp), " for year ", year)
    return(FALSE)
  }

  raw <- httr::content(resp, "raw")

  # Protect against HTML files disguised as downloads
  if (length(raw) < 5000) {
    message("× File too small (HTML returned?) — skipping: ", year)
    return(FALSE)
  }

  writeBin(raw, outpath)
  message("✓ Saved: ", basename(outpath))
  return(TRUE)
}

###########################################################################
# PDF DOWNLOAD (2003–2012)
###########################################################################

message("\n===== DOWNLOADING PDF FILES (2003–2012) =====")

# 2011–2003 shared pattern
pdf_years <- 2003:2011

for (year in pdf_years) {
  url <- glue::glue(
    "https://www.bka.de/SharedDocs/Downloads/DE/Publikationen/",
    "PolizeilicheKriminalstatistik/pksJahrbuecherBis2011/pks{year}.pdf?__blob=publicationFile&v=2"
  )
  download_file(url, year)
}

# 2012 special case
url_2012 <- paste0(
  "https://www.bka.de/SharedDocs/Downloads/DE/Publikationen/",
  "PolizeilicheKriminalstatistik/2012/pks2012Jahrbuch.pdf?__blob=publicationFile&v=1"
)
download_file(url_2012, 2012)

###########################################################################
# EXCEL DATA DOWNLOAD (2013–2024)
###########################################################################

# Modern Excel files 2019–2024
url_modern <- paste0(
  "https://www.bka.de/SharedDocs/Downloads/DE/Publikationen/",
  "PolizeilicheKriminalstatistik/{YEAR}/Kreis/Faelle/",
  "KR-F-01-T01-Kreise-Faelle-HZ_xls.xlsx?__blob=publicationFile&v=4"
)

# BKA-LKS Excel files 2016–2018
url_bkalks <- paste0(
  "https://www.bka.de/SharedDocs/Downloads/DE/Publikationen/",
  "PolizeilicheKriminalstatistik/{YEAR}/BKATabellen/FaelleLaenderKreiseStaedte/",
  "BKA-LKS-F-03-T01-Kreise_excel.xlsx?__blob=publicationFile&v=3"
)

# tb01 Excel files 2014–2015
url_tb01 <- paste0(
  "https://www.bka.de/SharedDocs/Downloads/DE/Publikationen/",
  "PolizeilicheKriminalstatistik/{YEAR}/BKATabellen/FaelleLaenderKreiseStaedte/",
  "tb01_FaelleGrundtabelleKreise_excel.xlsx?__blob=publicationFile&v=2"
)

# Special case 2013
url_2013 <- paste0(
  "https://www.bka.de/SharedDocs/Downloads/DE/Publikationen/",
  "PolizeilicheKriminalstatistik/2013/BKATabellen/FaelleLaenderStaedte/",
  "tb01_FaelleKriminalitaetsbetrachtungAufKreisebene_excel.xls?__blob=publicationFile&v=2"
)

excel_years_modern <- 2019:2024
excel_years_bkalks <- 2016:2018
excel_years_tb01 <- 2014:2015

message("\n===== DOWNLOADING EXCEL FILES (2013–2024) =====")

download_file(url_2013, 2013)

for (year in excel_years_tb01) {
  url <- gsub("\\{YEAR\\}", year, url_tb01)
  download_file(url, year)
}

for (year in excel_years_bkalks) {
  url <- gsub("\\{YEAR\\}", year, url_bkalks)
  download_file(url, year)
}

for (year in excel_years_modern) {
  url <- gsub("\\{YEAR\\}", year, url_modern)
  download_file(url, year)
}

message("\n===== ALL DOWNLOADS COMPLETED SUCCESSFULLY =====\n")
