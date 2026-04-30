# ==============================================================================
# File:          utils.R
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
# Category:      Tools & Utilities
#
# Description:   Centralized utility functions for data preparation, modeling, and academic table styling (GT). Provides helpers for panel data transformation, significance star formatting, and PDF/HTML table exporting.
#
# Inputs:        Various (Data frames, model objects)
# Outputs:       Formatted tables, styled GT objects, and analysis-ready data structures.
# ==============================================================================

source(here::here("02_scripts", "02_data_preprocessing", "00_import.R"))

# 1. Library Management --------------------------------------------------------
# Libraries are now loaded globally via 00_import.R

# 2. Data Preparation ----------------------------------------------------------
prepare_panel_data <- function(file_path) {
    if (!file.exists(file_path)) {
        stop("Panel dataset not found at: ", file_path)
    }

    readr::read_csv(file_path, show_col_types = FALSE) %>%
        dplyr::arrange(district_id, year) %>%
        dplyr::group_by(district_id) %>%
        dplyr::mutate(
            log_gdp_pc = log(INKAR_INCOME_gdp_per_capita),
            log_capital = log(CAPITAL_stock_per_capita),
            log_property_damage = log(PKS_property_damage_rate),
            log_street_crime = log(PKS_street_crime_rate),
            log_total_crime = log(PKS_total_crime_rate),
            foreigner_share = INKAR_INTEGRATION_foreigner_share,
            
            # --- FE Naming Standard ---
            FE_labor_integration_efficacy = labor_efficacy,
            FE_foreign_labor_supply_pressure = flsp,
            FE_log_labor_efficacy = log(labor_efficacy),
            FE_share_children = FE_children_share,
            FE_share_youth = FE_youth_share,

            # Lagged controls
            log_unemp_l1 = dplyr::lag(log(INKAR_EMPLOYMENT_unemployment_rate), 1),
            FE_foreign_labor_supply_pressure_l1 = dplyr::lag(flsp, 1),
            log_property_damage_l1 = dplyr::lag(log(PKS_property_damage_rate + 0.1), 1),
            log_capital_l1 = dplyr::lag(log(CAPITAL_stock_per_capita), 1),
            FE_log_labor_efficacy_l1 = dplyr::lag(log(labor_efficacy), 1),
            
            # Crime Subtype Lags
            log_total_l1 = dplyr::lag(log(PKS_total_crime_rate + 0.1), 1),
            log_street_l1 = dplyr::lag(log(PKS_street_crime_rate + 0.1), 1),
            log_drugs_l1 = dplyr::lag(log(PKS_drug_offences_rate + 0.1), 1),
            log_bodily_l1 = dplyr::lag(log(PKS_bodily_harm_rate + 0.1), 1)
        ) %>%
        dplyr::ungroup()
}

# 3. GT Table Styling (Academic / Journal Theme) -------------------------------
apply_academic_theme <- function(
    gt_table,
    title = NULL,
    subtitle = NULL,
    source_note = "Standard errors clustered at the district level.",
    width_1st_col = 280,
    width_other_cols = NULL
) {
    # 1. Basic Structure (Headers, Source Notes)
    styled_table <- gt_table %>%
        gt::tab_header(
            title = if (!is.null(title)) gt::md(title) else NULL,
            subtitle = if (!is.null(subtitle)) gt::md(subtitle) else NULL
        ) %>%
        {
          if (is.null(source_note) || source_note == "") .
          else gt::tab_source_note(., source_note = source_note)
        }

    # 2. Bold Labels & Striping (Format-unspecific)
    styled_table <- styled_table %>%
        gt::tab_style(
            style = gt::cell_text(weight = "bold", size = gt::px(15)),
            locations = gt::cells_column_labels()
        ) %>%
        gt::tab_style(
            style = gt::cell_text(weight = "bold", size = gt::px(18)),
            locations = gt::cells_title(groups = "title")
        ) %>%
        gt::opt_row_striping()

    # 3. Consolidated Format-Aware Styling
    if (knitr::is_latex_output()) {
        styled_table <- styled_table %>%
            gt::tab_options(
                table.width = gt::pct(100),
                table.font.size = gt::px(12),
                data_row.padding = gt::px(6),
                column_labels.padding = gt::px(8),
                heading.align = "center",
                table.border.top.width = gt::px(3),
                table.border.top.color = "black",
                table.border.bottom.width = gt::px(3),
                table.border.bottom.color = "black",
                column_labels.border.bottom.width = gt::px(2),
                column_labels.border.bottom.color = "black"
            ) %>%
            gt::opt_table_font(font = "Latin Modern Roman")
    } else {
        styled_table <- styled_table %>%
            gt::tab_options(
                table.width = gt::pct(100),
                table.font.size = gt::px(16),
                data_row.padding = gt::px(4),
                column_labels.padding = gt::px(8),
                heading.align = "center",
                table.border.top.width = gt::px(2),
                table.border.top.color = "black",
                table.border.bottom.width = gt::px(2),
                table.border.bottom.color = "black",
                column_labels.border.bottom.width = gt::px(2),
                column_labels.border.bottom.color = "black"
            ) %>%
            gt::opt_table_font(font = list("EB Garamond", "Cormorant Garamond", "serif")) %>%
            # Sanitize LaTeX characters for HTML browser rendering
            gt::text_transform(
                locations = list(gt::cells_body(), gt::cells_column_labels()),
                fn = function(x) gsub("\\\\%", "%", x)
            )
    }

    # 4. Column Widths
    if (width_1st_col != 280 || !is.null(width_other_cols)) {
        w_list <- list()
        if (!is.null(width_other_cols)) {
            w_list <- c(w_list, list(stats::as.formula(paste0("gt::everything() ~ gt::px(", width_other_cols, ")"))))
        }
        w_list <- c(w_list, list(stats::as.formula(paste0("1 ~ gt::px(", width_1st_col, ")"))))
        
        styled_table <- styled_table %>%
            gt::cols_width(.list = w_list) %>%
            gt::tab_options(table.layout = "fixed")
    }

    # 5. Bold Significance Stars (HTML Injection)
    styled_table <- styled_table %>%
        gt::text_transform(
            locations = gt::cells_body(),
            fn = function(x) {
                sapply(x, function(cell_text) {
                    if (!is.null(cell_text) && !is.na(cell_text) && grepl("\\*", cell_text)) {
                        bolded <- gsub(
                            "(\\*+)",
                            "<span style='font-weight:bold;'>\\1</span>",
                            as.character(cell_text)
                        )
                        return(as.character(gt::html(bolded)))
                    }
                    return(as.character(cell_text))
                }, USE.NAMES = FALSE)
            }
        )

    return(styled_table)
}

# 4. ModelSummary Helpers ------------------------------------------------------
save_standard_table <- function(
    models,
    coef_map,
    gof_map,
    add_rows,
    title,
    subtitle,
    file_path_html,
    file_path_txt = NULL,
    force = FALSE,
    width_1st_col = 280,
    width_other_cols = NULL
) {
    # 0. Check for skipping
    skip_html <- should_skip(file_path_html, force = force)
    skip_txt <- if (!is.null(file_path_txt)) should_skip(file_path_txt, force = force) else TRUE
    
    if (skip_html && skip_txt) {
        message("\u2713 Table already exists, skipping: ", basename(file_path_html))
        return(invisible(NULL))
    }

    # Ensure directories exist
    ensure_dir(dirname(file_path_html))
    if (!is.null(file_path_txt)) ensure_dir(dirname(file_path_txt))

    # 1. TXT Export (Simple)
    if (!is.null(file_path_txt)) {
        modelsummary::modelsummary(
            models,
            coef_map = coef_map,
            stars = c('*' = .1, '**' = .05, '***' = .01),
            gof_map = gof_map,
            add_rows = add_rows,
            title = title,
            output = file_path_txt
        )
    }

    # 2. GT / HTML Export (Styled)
    gt_raw <- modelsummary::modelsummary(
        models,
        coef_map = coef_map,
        stars = c('*' = .1, '**' = .05, '***' = .01),
        gof_map = gof_map,
        add_rows = add_rows,
        output = "gt"
    )

    gt_styled <- apply_academic_theme(
        gt_raw,
        title = title,
        subtitle = subtitle,
        width_1st_col = width_1st_col,
        width_other_cols = width_other_cols
    )

    gt::gtsave(gt_styled, file_path_html)
    message("\u2713 Table successfully exported to: ", file_path_html)
}

# 5. Pipeline Utilities --------------------------------------------------------
# Rendering scripts (31, 32) are now located in their respective folders
# and are orchestrated via setup_environment.R.
