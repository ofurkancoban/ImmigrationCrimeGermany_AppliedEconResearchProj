# ==============================================================================
# File:          01b_Geodata_Downloader.R
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
# Description:   Automates the download and extraction of the official BKG VG250 
#                (Verwaltungsgebiete 1:250 000) spatial boundary data. This 
#                dataset is required for the static mapping stage (Stage 29).
#
# Inputs:        None (Downloads from BKG Open Data Portal)
# Outputs:       Extracted Shapefiles in 01_datasets/geodata/
# ==============================================================================

# 1. SETUP
# ------------------------------------------------------------------------------
source(here::here("02_scripts", "02_data_preprocessing", "00_import.R"))

# Configuration
geodata_url <- "https://daten.gdz.bkg.bund.de/produkte/vg/vg250_ebenen_0101/2024/vg250_01-01.utm32s.shape.ebenen.zip"
geodata_dir <- here::here("01_datasets", "geodata")
ensure_dir(geodata_dir)

# Define the target file that confirms the extraction is complete
# Based on the structure used in 29_Generate_Static_Maps.R
target_shp <- file.path(
    geodata_dir, 
    "vg250_01-01.utm32s.shape.ebenen", 
    "vg250_ebenen_0101", 
    "VG250_KRS.shp"
)

# 2. DOWNLOAD & UNZIP
# ------------------------------------------------------------------------------

if (fs::file_exists(target_shp)) {
    message("✓ BKG Geodata (VG250) already exists. Skipping download.")
} else {
    message("→ Downloading BKG Geodata (VG250)... This may take a minute (~100MB).")
    
    temp_zip <- tempfile(fileext = ".zip")
    
    # Download with a longer timeout for the large file
    download_status <- try(download.file(geodata_url, temp_zip, mode = "wb", quiet = TRUE))
    
    if (inherits(download_status, "try-error") || !fs::file_exists(temp_zip)) {
        stop("× Failed to download BKG Geodata. Please check the URL or your internet connection.")
    }
    
    message("→ Extracting Geodata to: ", geodata_dir)
    utils::unzip(temp_zip, exdir = geodata_dir)
    
    # Cleanup
    unlink(temp_zip)
    
    if (fs::file_exists(target_shp)) {
        message("✓ Geodata successfully downloaded and extracted.")
    } else {
        warning("! Extraction finished, but target SHP not found at expected path: ", target_shp)
    }
}

message("===== GEODATA DOWNLOADER COMPLETED =====\n")
