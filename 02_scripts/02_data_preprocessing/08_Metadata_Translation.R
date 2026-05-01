# ==============================================================================
# File:          08_Metadata_Translation.R
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
# Description:   Extracts metadata dictionaries from the raw INKAR datasets. It translates unit measures, methodologies, and variable definitions from German to English to build a comprehensive data dictionary for the finalized panel variables.
#
# Inputs:        Raw INKAR files containing structural metadata
# Outputs:       A standalone data dictionary mapping short variable names to extensive definitions
# ==============================================================================

# 1. SETUP
# ------------------------------------------------------------------------------
# Source global utilities (packages, directory initialization, robustness helpers)
source(here::here("02_scripts", "02_data_preprocessing", "00_import.R"))

# Script-specific configuration
force_process <- FALSE # Set to TRUE to overwrite existing files

# -------------------------------------------------------------------------
# Configuration
# -------------------------------------------------------------------------
input_file_de <- here::here("01_datasets", "tools", "INKAR_Uebersicht_der_Indikatoren.xlsx")
output_file_en <- here::here("01_datasets", "tools", "INKAR_Overview_of_Indicators.xlsx")
api_key <- "AIzaSyATBXajvzQLTDHEQbcpq0Ihe0vWDHmO520"
endpoint <- "https://translate-pa.googleapis.com/v1/translateHtml"

# Standardized skip logic
if (should_skip(output_file_en, force = force_process)) {
    message("✓ Output already exists: ", basename(output_file_en))
    # We exit the script early if output exists and not forced
    # In a sourced script, we might want to use a guard or just return if in a function.
    # Here we will wrap the main logic in an if block.
    run_main <- FALSE
} else {
    run_main <- TRUE
}

if (run_main) {
    # -------------------------------------------------------------------------
    # Helper: Capitalize First Letter
    # -------------------------------------------------------------------------
    capitalize_first <- function(x) {
      sapply(x, function(s) {
        if (is.na(s) || s == "") {
          return(s)
        }
        paste0(toupper(substr(s, 1, 1)), substr(s, 2, nchar(s)))
      })
    }

    # -------------------------------------------------------------------------
    # Use Curl for Translation (Robust)
    # -------------------------------------------------------------------------
    batch_translate <- function(
      texts,
      source_lang = "de",
      target_lang = "en",
      apply_casing = TRUE
    ) {
      if (length(texts) == 0) return(character(0))
      texts <- as.character(texts)
      unique_texts <- unique(texts[!is.na(texts) & texts != ""])
      if (length(unique_texts) == 0) return(texts)

      chunk_size <- 20
      chunks <- split(unique_texts, ceiling(seq_along(unique_texts) / chunk_size))
      translation_map <- list()

      for (i in seq_along(chunks)) {
        chunk <- chunks[[i]]
        chunk_utf8 <- enc2utf8(as.character(chunk))
        inner_json <- jsonlite::toJSON(chunk_utf8, auto_unbox = FALSE)
        data_raw <- sprintf('[[%s, "%s", "%s"], "te"]', inner_json, source_lang, target_lang)

        payload_file <- tempfile(fileext = ".json")
        writeLines(data_raw, payload_file, useBytes = TRUE)

        cmd <- sprintf('curl -s -X POST "%s?key=%s" -H "content-type: application/json+protobuf" -H "user-agent: Mozilla/5.0" --data-binary @%s', endpoint, api_key, payload_file)

        tryCatch({
            res_content <- system(cmd, intern = TRUE)
            res_content <- paste(res_content, collapse = "")
            trans_vec <- try(jsonlite::fromJSON(res_content, simplifyVector = FALSE), silent = TRUE)
            if (!inherits(trans_vec, "try-error") && is.list(trans_vec) && length(trans_vec) >= 1) {
              values <- unlist(trans_vec[[1]])
              if (length(values) == length(chunk)) {
                for (j in seq_along(chunk)) translation_map[[chunk[j]]] <- values[j]
              }
            }
          }, error = function(e) message("Curl Failed Chunk ", i, ": ", e$message))
        unlink(payload_file)
        Sys.sleep(0.3)
      }

      sapply(texts, function(x) {
        if (is.na(x) || x == "") return(x)
        if (x %in% names(translation_map)) {
          val <- translation_map[[x]]
          if (apply_casing) return(capitalize_first(val))
          return(val)
        }
        return(x)
      })
    }

    # -------------------------------------------------------------------------
    # Main Processing Logic
    # -------------------------------------------------------------------------

    if (!file.exists(input_file_de)) {
      stop(paste("DE File not found:", input_file_de, "\nRun 06_INKAR_Downloader.R first!"))
    }

    file.copy(input_file_de, output_file_en, overwrite = TRUE)
    message("  → Created copy: ", basename(output_file_en))

    wb <- openxlsx::loadWorkbook(output_file_en)
    sheets <- names(wb)
    new_sheet_names <- batch_translate(sheets)
    new_sheet_names <- gsub(" DE$", " EN", new_sheet_names)

    # Rename sheets
    for (i in seq_along(sheets)) {
      if (sheets[i] != new_sheet_names[i]) openxlsx::renameWorksheet(wb, sheets[i], new_sheet_names[i])
    }
    sheets <- names(wb)

    for (i in seq_along(sheets)) {
      sheet <- sheets[i]
      message("  → Translating Sheet ", i, "/", length(sheets), ": ", sheet)

      raw_data <- openxlsx::readWorkbook(wb, sheet, colNames = FALSE, rows = 1:20)
      header_row_idx <- 1
      found <- FALSE
      keywords <- c("Indikator", "Bereich", "Kurzname", "M_ID", "Status", "Raumeinheit", "Zentralität")

      if (nrow(raw_data) > 0) {
        for (r in 1:nrow(raw_data)) {
          row_vals <- as.character(raw_data[r, ])
          matches <- sum(sapply(keywords, function(k) any(grepl(k, row_vals, ignore.case = TRUE))))
          if (matches >= 2 || (any(grepl("Kurzname", row_vals, ignore.case = TRUE)) && matches >= 1)) {
            header_row_idx <- r
            found <- TRUE; break
          }
        }
      }

      header_vals <- as.character(raw_data[header_row_idx, ])
      df_content <- openxlsx::readWorkbook(wb, sheet, startRow = header_row_idx + 1, colNames = FALSE)

      if (nrow(df_content) > 0) {
        for (col_idx in 1:ncol(df_content)) {
          hdr <- header_vals[col_idx]
          if (is.na(hdr)) next
          if (any(grepl("ID|Code|Kennziffer", hdr, ignore.case = TRUE)) && !grepl("Indikator", hdr)) next
          vec <- df_content[[col_idx]]
          non_na <- vec[!is.na(vec)]
          if (length(non_na) > 0) {
            is_num <- suppressWarnings(all(!is.na(as.numeric(non_na))))
            if (!is_num) {
              apply_casing_flag <- !grepl("Kürzel|Abbreviation", hdr, ignore.case = TRUE)
              vec_en <- batch_translate(vec, apply_casing = apply_casing_flag)
              openxlsx::writeData(wb, sheet, vec_en, startCol = col_idx, startRow = header_row_idx + 1, colNames = FALSE)
            }
          }
        }
      }

      header_trans_map <- c(
        "Bereich" = "Domain", "Unterbereich" = "Subdomain", "Indikator" = "Indicator_Name",
        "Kurzname" = "Short_Name", "Einheit" = "Unit", "Anmerkung" = "Note", "Anmerkungen" = "Notes",
        "Name" = "Name", "Algorithmus" = "Algorithm", "Status" = "Status", "Kreis" = "District",
        "Verfügbarkeit" = "Availability", "Raumbezug" = "Spatial_Reference", "Quelle" = "Source",
        "Beschreibung" = "Description", "Statistische Grundlagen" = "Statistical_Basis",
        "Raumeinheit" = "Spatial_Unit", "Zentralität" = "Centrality"
      )
      new_headers <- header_vals
      for (idx in seq_along(new_headers)) {
        if (!is.na(new_headers[idx]) && new_headers[idx] %in% names(header_trans_map)) {
          new_headers[idx] <- header_trans_map[[new_headers[idx]]]
        }
      }
      openxlsx::writeData(wb, sheet, t(new_headers), startCol = 1, startRow = header_row_idx, colNames = FALSE, rowNames = FALSE)
    }
    openxlsx::saveWorkbook(wb, output_file_en, overwrite = TRUE)
    message("  ✓ Translation completed successfully")
}
