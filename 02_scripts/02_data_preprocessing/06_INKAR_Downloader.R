# ==============================================================================
# File:          06_INKAR_Downloader.R
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
# Description:
# This script uses the 'inkaR' package(https://cran.r-project.org/web/packages/inkaR/index.html, https://github.com/ofurkancoban/inkaR) to download spatial development indicators 
# from the BBSR INKAR database. It also downloads the official metadata 
# indicator overview from inkar.de for downstream translation and documentation.
#
# Output:
# Individual CSV files in ./01_datasets/raw/INKAR/ and metadata in ./01_datasets/tools/
# ==============================================================================

# 1. SETUP
# ------------------------------------------------------------------------------
# Source global utilities (packages, directory initialization, robustness helpers)
source(here::here("02_scripts", "02_data_preprocessing", "00_import.R"))

# Script-specific configuration
force_download <- FALSE # Set to TRUE to overwrite existing files

print("Starting INKAR Downloader v2 (using inkaR package 0.6.1)...")

# Directory paths
output_dir <- ensure_dir(here("01_datasets", "raw", "INKAR"))

# 2. SELECTION
# ------------------------------------------------------------------------------
# Descriptions must EXACTLY match the keys in 07_INKAR_Translation.R mapping_table
vars <- tibble::tribble(
    ~Category        , ~IndID      , ~Description                                                                                                ,
    # PRIMARY VARIABLES
    "DEMOGRAPHICS"   , "bev_korr"  , "Zensuskorrigierte Zahl der Einwohner insgesamt"                                                            ,
    "INTEGRATION"    , "143"       , "Anteil der Ausländer an den Einwohnern in %"                                                              ,
    "EMPLOYMENT"     , "12"        , "Anteil der Arbeitslosen an den zivilen Erwerbspersonen in %"                                               ,
    "INTEGRATION"    , "20"        , "Anteil der ausländischen Arbeitslosen an den Arbeitslosen in %"                                           ,
    "INCOME"         , "398"       , "Bruttoinlandsprodukt in 1.000 € je Einwohner"                                                            ,
    
    # AGE DEMOGRAPHICS
    "DEMOGRAPHICS"   , "121"       , "Anteil der Einwohner unter 6 Jahren an den Einwohnern in %"                                             ,
    "DEMOGRAPHICS"   , "122"       , "Anteil der 6- bis unter 18-Jährigen an der Bevölkerung"                                                ,
    "DEMOGRAPHICS"   , "123"       , "Anteil der 18- bis unter 25-Jährigen an der Bevölkerung"
)

# 3. DOWNLOAD & SAVE
# ------------------------------------------------------------------------------
print(paste("Downloading", nrow(vars), "indicators for level KRE (Districts)..."))

# Helper function MUST matched 07_INKAR_Translation.R logic
clean_desc <- function(d) {
    d <- stringr::str_replace_all(d, "[^a-zA-Z0-9äöüÄÖÜß]", "_")
    d <- stringr::str_replace_all(d, "_+", "_")
    d <- stringr::str_remove(d, "_$")
    return(d)
}

# Loop through variables
for (i in seq_len(nrow(vars))) {
    v <- vars[i, ]
    
    # Determine file prefix (Cleaned German Description)
    file_prefix <- clean_desc(v$Description)
    
    # Check if ANY file with this prefix exists to potentially skip.
    existing_files <- list.files(output_dir, pattern = paste0("^", file_prefix))
    
    if (length(existing_files) > 0 && !force_download) {
        message("Skipping [", v$Category, "]: ", v$Description, " (File already exists)")
        next
    }
    
    print(paste0("Processing [", v$Category, "]: ID: ", v$IndID))
    
    tryCatch({
        # Download data using inkaR (Long format as base)
        data <- inkaR::get_inkar_data(v$IndID, level = "KRE", lang = "de", format = "long")
        
        if (is.null(data) || nrow(data) == 0) {
            warning(paste("No data found for", v$IndID))
            next
        }
        
        # Determine year range
        years <- range(as.numeric(data$Zeit), na.rm = TRUE)
        year_range_str <- paste0(years[1], "-", years[2])
        
        # PIVOT WIDE (Required by legacy script 07)
        wide_data <- data %>%
            select(
                Kennziffer,
                Raumeinheit,
                Year = Zeit,
                Value = Wert
            ) %>%
            mutate(
                ColumnName = paste0(file_prefix, "_", Year)
            ) %>%
            select(-Year) %>%
            pivot_wider(
                names_from = ColumnName,
                values_from = Value
            )
            
        # Format filename
        file_name <- paste0(file_prefix, "_", year_range_str, ".csv")
        file_path <- file.path(output_dir, file_name)
        
        write_csv(wide_data, file_path)
        print(paste("Successfully saved to:", file_name))
        
    }, error = function(e) {
        message(paste("Error processing", v$IndID, ":", e$message))
    })
}

# 4. METADATA UPDATE & OVERVIEW DOWNLOAD
# ------------------------------------------------------------------------------
# 4a. Simple TXT Overview
metadata_path <- file.path(output_dir, "Metadata_Overview.txt")
metadata_text <- paste(
    "INKAR DOWNLOAD METADATA\n",
    "Generated: ", Sys.time(), "\n",
    "Source: BBSR INKAR Database via inkaR R-Package v0.6.1\n",
    "Geographic Level: KRE (Landkreise)\n",
    "Variables:\n",
    paste(vars$Description, collapse = "\n"),
    sep = ""
)
writeLines(metadata_text, metadata_path)
print("Metadata Overview TXT updated.")

# 4b. Official Excel Overview for 08_Metadata_Translation.R
# This is required by Stage 8 of the pipeline
download_metadata_overview <- function() {
    url <- "https://www.inkar.de/documents/Uebersicht%20der%20Indikatoren.xlsx"
    tools_dir <- here::here("01_datasets", "tools")
    if (!dir.exists(tools_dir)) {
        dir.create(tools_dir, recursive = TRUE)
    }
    dest_file <- file.path(tools_dir, "INKAR_Uebersicht_der_Indikatoren.xlsx")

    if (!file.exists(dest_file) || force_download) {
        message("Downloading official INKAR Metadata Overview...")
        tryCatch(
            {
                download.file(url, dest_file, mode = "wb", quiet = FALSE)
                message("\u2713 Metadata Overview downloaded successfully.")
            },
            error = function(e) {
                message("Error downloading metadata: ", e$message)
            }
        )
    } else {
        message("Metadata Overview already exists.")
    }
}

# Execute Download
download_metadata_overview()

print("INKAR Downloader v2 completed.")
