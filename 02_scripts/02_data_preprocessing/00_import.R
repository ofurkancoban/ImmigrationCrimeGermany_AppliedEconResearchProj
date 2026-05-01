# ==============================================================================
# File:          00_import.R
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
# Description:   A utility script containing foundational functions, common directory definitions, and global library imports required consistently across multiple data cleaning and preprocessing stages. Handles package installation and environment setup.
#
# Inputs:        None
# Outputs:       Loaded libraries, initialized directory structure, and utility functions in the global environment.
# ==============================================================================

packages <- c(
  "tidyverse", # Core data science suite (dplyr, ggplot2, readr, etc.)
  "here", # Robust path management
  "fixest", # Fast Fixed-Effects estimations
  "modelsummary", # Academic table generation
  "gt", # Display tables
  "tinytable", # Minimalist tables
  "glue", # String interpolation
  "httr", # HTTP requests
  "xml2", # XML parsing
  "jsonlite", # JSON processing
  "readxl", # Excel import
  "openxlsx", # Excel export
  "pdftools", # PDF data extraction
  "janitor", # Data cleaning utilities
  "fs", # File system operations
  "sf", # Simple Features (Mapping/Spatial)
  "giscoR", # Eurostat GISCO API
  "leaflet", # Interactive maps
  "htmlwidgets", # HTML widget support
  "zoo", # Time series utilities (rollmean, etc.)
  "patchwork", # Multi-panel plots
  "remotes", # Install from GitHub if needed
  "quarto", # Quarto document rendering
  "tinytex", # LaTeX distribution for PDF rendering
  "plm", # Linear Models for Panel Data (Hausman test)
  "knitr", # Dynamic report generation
  "kableExtra", # Enhanced table formatting
  "ggrepel", # Repel overlapping text labels in ggplot2
  "showtext", # Using fonts more easily in R graphs
  "htmltools", # Tools for HTML generation and output
  "viridis" # Colorblind-friendly color maps
)

# --- Configuration & Auto-Installation ---
if (is.null(getOption("repos")) || getOption("repos")["CRAN"] == "@CRAN@") {
  options(repos = c(CRAN = "https://cloud.r-project.org"))
}

install_and_load <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message("Installing missing package: ", pkg)
    install.packages(pkg)
  }
  # Note: Character.only is TRUE because variable pkg is passed
  suppressPackageStartupMessages(library(pkg, character.only = TRUE))
}

invisible(lapply(packages, install_and_load))

# --- inkaR (v0.6.1 from CRAN Source) ---

# CRAN: https://cran.r-project.org/web/packages/inkaR/index.html
# Github: https://github.com/ofurkancoban/inkaR

# Note: Binaries on CRAN may lag at 0.4.3, so we force installation from source.
if (!requireNamespace("inkaR", quietly = TRUE) || utils::packageVersion("inkaR") < "0.6.1") {
  message("Attempting to install inkaR v0.6.1 from CRAN source...")
  try_cran <- try(install.packages("inkaR", type = "source", quiet = TRUE), silent = TRUE)
  
  if (inherits(try_cran, "try-error") || !requireNamespace("inkaR", quietly = TRUE) || utils::packageVersion("inkaR") < "0.6.1") {
    message("CRAN install failed or version insufficient. Attempting GitHub installation (ofurkancoban/inkaR)...")
    if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
    remotes::install_github("ofurkancoban/inkaR", upgrade = "never", quiet = TRUE)
  }
}
suppressPackageStartupMessages(library(inkaR))

# --- Robustness Helpers ---

#' Ensure a directory exists, creating it if necessary
#' @param path Character. Path to the directory.
ensure_dir <- function(path) {
  if (!fs::dir_exists(path)) {
    message("Creating missing directory: ", path)
    fs::dir_create(path, recurse = TRUE)
  }
  return(path)
}

#' Check if a file already exists and should be skipped
#' @param target_file Character. Path to the file.
#' @param force Logical. If TRUE, never skip.
#' @return Logical. TRUE if skip is advised, FALSE otherwise.
#' @description Check if a file already exists and should be skipped.
should_skip <- function(target_file, force = FALSE) {
  if (force) return(FALSE)
  if (fs::file_exists(target_file)) {
    # message("Skipping: ", basename(target_file), " (already exists)")
    return(TRUE)
  }
  return(FALSE)
}

# --- Project Structure Initialization ---
standard_dirs <- c(
  "01_datasets/raw/PKS",
  "01_datasets/raw/INKAR",
  "01_datasets/geodata",
  "01_datasets/processed/PKS",
  "01_datasets/processed/INKAR/01-translated",
  "03_results/tables/txt",
  "03_results/tables/html",
  "03_results/figures"
)

# Pre-create standard directories to prevent "path does not exist" errors
invisible(lapply(standard_dirs, ensure_dir))

message("✓ All packages loaded and project directories initialized.")
