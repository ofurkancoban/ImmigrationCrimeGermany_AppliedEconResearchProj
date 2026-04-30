# ==============================================================================
# File:          setup_environment.R
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
# Category:      Setup
#
# Description:   This script serves as the master orchestrator for the entire 32-stage research pipeline. It establishes the central directory structure, loads necessary R packages, defines static paths for all datasets, and sequentially executes preprocessing, analysis, and rendering routines. Supports granular execution via command-line arguments.
#
# Inputs:        Command-line arguments (e.g., --stage=preprocess)
# Outputs:       Initialized project environment and sequential execution of data pipeline stages.
# ==============================================================================

if (!requireNamespace("here", quietly = TRUE)) {
  install.packages("here")
}

# --- Pre-flight Check: System Dependencies ---
check_system_dependency <- function(cmd, install_hint) {
  found <- system(paste0("which ", cmd), ignore.stdout = TRUE) == 0
  if (!found) {
    if (.Platform$OS.type == "windows") {
      found <- system(paste0("where ", cmd), ignore.stdout = TRUE) == 0
    }
  }
  if (!found) {
    stop(sprintf("\n[ERROR] System dependency '%s' not found.\n%s", cmd, install_hint))
  }
  message(sprintf("✓ System dependency '%s' detected.", cmd))
}

message("Checking system requirements...")
check_system_dependency("quarto", "Please install Quarto CLI.")
check_system_dependency("curl", "Please install 'curl'.")

source(here::here("02_scripts", "02_data_preprocessing", "00_import.R"))
source(here::here("02_scripts", "04_tools", "utils.R"))

if (!tinytex::is_tinytex()) {
  message("--> Installing TinyTeX...")
  tinytex::install_tinytex()
}

# --- Define Script Paths (01-13 Preprocessing) ---
s01 <- here::here("02_scripts", "02_data_preprocessing", "01_PKS_Dataset_Downloader.R")
s01b <- here::here("02_scripts", "02_data_preprocessing", "01b_Geodata_Downloader.R")
s02 <- here::here("02_scripts", "02_data_preprocessing", "02_PKS_Old_Datasets_PDF2CSV.R")
s03 <- here::here("02_scripts", "02_data_preprocessing", "03_PKS_New_Datasets_EXCEL2CSV.R")
s04 <- here::here("02_scripts", "02_data_preprocessing", "04_PKS_Spatial_Harmonization.R")
s05 <- here::here("02_scripts", "02_data_preprocessing", "05_PKS_Translation.R")
s06 <- here::here("02_scripts", "02_data_preprocessing", "06_INKAR_Downloader.R")
s07 <- here::here("02_scripts", "02_data_preprocessing", "07_INKAR_Translation.R")
s08 <- here::here("02_scripts", "02_data_preprocessing", "08_Metadata_Translation.R")
s09 <- here::here("02_scripts", "02_data_preprocessing", "09_StatistikPortal_Downloader.R")
s10 <- here::here("02_scripts", "02_data_preprocessing", "10_StatistikPortal_Processing.R")
s11 <- here::here("02_scripts", "02_data_preprocessing", "11_Capital_Stock_Harmonization.R")
s12 <- here::here("02_scripts", "02_data_preprocessing", "12_Data_Merge.R")
s13 <- here::here("02_scripts", "02_data_preprocessing", "13_Feature_Engineering.R")

# --- Define Script Paths (14-30 Analysis & Robustness) ---
s14 <- here::here("02_scripts", "03_analysis", "14_Descriptive_Statistics.R")
s15 <- here::here("02_scripts", "03_analysis", "15_Main_Econometric_Analysis.R")
s16 <- here::here("02_scripts", "03_analysis", "16_Analysis_Interaction_Base.R")
s17 <- here::here("02_scripts", "03_analysis", "17_Analysis_Interaction_Extended.R")
s18 <- here::here("02_scripts", "03_analysis", "18_Robustness_HausmanTest.R")
s19 <- here::here("02_scripts", "03_analysis", "19_Robustness_Multicollinearity.R")
s20 <- here::here("02_scripts", "03_analysis", "20_Robustness_LagSensitivity.R")
s21 <- here::here("02_scripts", "03_analysis", "21_Robustness_EastWest.R")
s21b <- here::here("02_scripts", "03_analysis", "21b_Robustness_IV.R")
s22 <- here::here("02_scripts", "03_analysis", "22_Subtype_Total_Crime.R")
s23 <- here::here("02_scripts", "03_analysis", "23_Subtype_Street_Crime.R")
s24 <- here::here("02_scripts", "03_analysis", "24_Subtype_Drugs.R")
s25 <- here::here("02_scripts", "03_analysis", "25_Subtype_Bodily_Harm.R")
s26 <- here::here("02_scripts", "03_analysis", "26_Compare_All_Subtypes_Batch.R")
s27 <- here::here("02_scripts", "03_analysis", "27_Compare_Crime_vs_Property.R")
s28 <- here::here("02_scripts", "03_analysis", "28_Generate_Subtypes_HTML_Table.R")
s29 <- here::here("02_scripts", "03_analysis", "29_Generate_Static_Maps.R")
s30 <- here::here("02_scripts", "03_analysis", "30_Generate_Interactive_Map.R")

# --- Define Script Paths (31-32 Rendering) ---
s31 <- here::here("04_paper", "31_Render_Paper.R")
s32 <- here::here("05_presentation", "32_Render_Presentation.R")

# Pre-create standard directories to prevent "path does not exist" errors
invisible(lapply(standard_dirs, ensure_dir))

# --- Command Line Argument Support (for Makefile) ---
args <- commandArgs(trailingOnly = TRUE)
run_stage <- "all"
if (length(args) > 0) {
  for (arg in args) {
    if (startsWith(arg, "--stage=")) {
      run_stage <- sub("--stage=", "", arg)
    }
  }
}

message(glue::glue("\n===== STARTING PROJECT PIPELINE (Stage: {run_stage}) =====\n"))

# --- STAGE 1-13: Preprocessing ---
if (run_stage %in% c("all", "preprocess")) {
  message("--- [01-13] DATA PREPROCESSING ---")
  scripts_pre <- list(
    "PKS Dataset Downloader" = s01,
    "BKG Geodata Downloader" = s01b,
    "PKS PDF Extraction (2003-2012)" = s02,
    "PKS Excel Processing (2013-2024)" = s03,
    "PKS Spatial Boundary Harmonization" = s04,
    "PKS Variable Translation" = s05,
    "INKAR Data Downloader v2" = s06,
    "INKAR Data Translation" = s07,
    "INKAR Metadata Translation" = s08,
    "StatistikPortal Data Downloader" = s09,
    "District Capital Stock Calculation" = s10,
    "Capital Stock Spatial Harmonization" = s11,
    "Final Panel Dataset Merge" = s12,
    "Analysis Feature Engineering" = s13
  )

  for (i in seq_along(scripts_pre)) {
    message(glue::glue("→ [{i}/32] Running {names(scripts_pre)[i]}..."))
    source(scripts_pre[[i]])
    message("  ✓ Done.\n")
  }
}

# --- STAGE 14-30: Analysis ---
if (run_stage %in% c("all", "analysis")) {
  message("--- [14-30] ECONOMETRIC ANALYSIS ---")
  scripts_ana <- list(
    "14" = "Descriptive Statistics",
    "15" = "Main Econometric Analysis",
    "16" = "Interaction Effects: Base",
    "17" = "Interaction Effects: Extended",
    "18" = "Diagnostic: Hausman Test",
    "19" = "Diagnostic: Multicollinearity (VIF)",
    "20" = "Robustness: Lag Sensitivity",
    "21" = "Robustness: East-West Comparison",
    "21b" = "Robustness: Instrumental Variables (IV)",
    "22" = "Subtype: Total Crime",
    "23" = "Subtype: Street Crime",
    "24" = "Subtype: Drugs",
    "25" = "Subtype: Bodily Harm",
    "26" = "Batch Table: All Subtypes",
    "27" = "Comparison: Crime vs Property",
    "28" = "Generate HTML Table",
    "29" = "Generate Static Maps",
    "30" = "Generate Interactive Map"
  )

  scripts_paths <- list(
    "14" = s14, "15" = s15, "16" = s16, "17" = s17, "18" = s18,
    "19" = s19, "20" = s20, "21" = s21, "21b" = s21b, "22" = s22, "23" = s23,
    "24" = s24, "25" = s25, "26" = s26, "27" = s27, "28" = s28,
    "29" = s29, "30" = s30
  )

  for (stage in names(scripts_ana)) {
    message(glue::glue("→ [{stage}/32] Running {scripts_ana[[stage]]}..."))
    source(scripts_paths[[stage]])
    message("  ✓ Stage {stage} Completed.\n")
  }
}

# --- STAGE 31-32: Rendering ---
if (run_stage %in% c("all", "render")) {
  message("--- [31-32] DOCUMENT RENDERING ---")
  scripts_render <- list(
    "31" = "Rendering Research Paper (PDF/HTML)",
    "32" = "Rendering Presentation (Reveal.js)"
  )

  scripts_render_paths <- list(
    "31" = s31, "32" = s32
  )

  for (stage in names(scripts_render)) {
    message(glue::glue("→ [{stage}/32] {scripts_render[[stage]]}..."))
    source(scripts_render_paths[[stage]])
    message("  ✓ Final Stage {stage} Completed.\n")
  }
}

message("\n==========================================")
message("   FULL PIPELINE COMPLETED SUCCESSFULLY")
message("==========================================\n")
