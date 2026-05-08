# ==============================================================================
# File:          27_Compare_Crime_vs_Property.R
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
# Category:      Econometric Analysis
#
# Description:   Executes a targeted contrastive regression architecture comparing the explicit elasticity of standard property crime against broader macroscopic criminality metrics regarding their effect on Integration capability.
#
# Inputs:        Final Panel: 01_datasets/final/panel_data_final_2003_2021.csv
# Outputs:       Comparative discrepancy regression tables exported to 03_results/tables/
# ==============================================================================

source(here::here("02_scripts", "04_tools", "utils.R"))

# Output directory
output_dir <- here::here("03_results", "tables")
if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
}

# 2. Load and Prepare Data -----------------------------------------------------
message("Loading and preparing data using standardized utils...")
df <- prepare_panel_data(here::here("01_datasets", "final", "panel_data_final_2003_2021.csv")) %>%
    dplyr::mutate(
        log_total = log_total_l1,
        log_street = log_street_l1,
        log_property = log_property_damage_l1
    )

# 4. Run Models ----------------------------------------------------------------

# Model 1: Total Crime
mod_total <- feols(
    log_gdp_pc ~ log_unemp_l1 +
        log_capital +
        log_total +
        FE_share_children +
        FE_share_youth +
        foreigner_share +
        FE_foreign_labor_supply_pressure_l1 +
        FE_log_labor_efficacy_l1 |
        district_id + year,
    data = df
)

# Model 2: Street Crime
mod_street <- feols(
    log_gdp_pc ~ log_unemp_l1 +
        log_capital +
        log_street +
        FE_share_children +
        FE_share_youth +
        foreigner_share +
        FE_foreign_labor_supply_pressure_l1 +
        FE_log_labor_efficacy_l1 |
        district_id + year,
    data = df
)

# Model 3: Property Damage
mod_property <- feols(
    log_gdp_pc ~ log_unemp_l1 +
        log_capital +
        log_property +
        FE_share_children +
        FE_share_youth +
        foreigner_share +
        FE_foreign_labor_supply_pressure_l1 +
        FE_log_labor_efficacy_l1 |
        district_id + year,
    data = df
)

models <- list(
    "Total Offenses" = mod_total,
    "Street Crime" = mod_street,
    "Property Damage" = mod_property
)

# 5. Export --------------------------------------------------------------------

# FE Rows
rows <- data.frame(
    term = c("District FE", "Year FE"),
    "Total Offenses" = rep("Yes", 2),
    "Street Crime" = rep("Yes", 2),
    "Property Damage" = rep("Yes", 2),
    check.names = FALSE
)

# Custom GOF Mapping
gof_mapping <- list(
    list("raw" = "nobs", "clean" = "Observations", "fmt" = 0),
    list("raw" = "r2.within", "clean" = "Within R\u00b2", "fmt" = 3)
)

coef_map <- c(
    "log_unemp_l1" = "Log Unemployment (t-1)",
    "log_capital" = "Log Capital Stock per Capita",
    "log_total" = "Log Total Crime Rate",
    "log_street" = "Log Street Crime Rate",
    "log_property" = "Log Property Damage Rate",
    "FE_share_children" = "Share Children (0-17)",
    "FE_share_youth" = "Share Youth (18-24)",
    "foreigner_share" = "Foreigner Share",
    "FE_foreign_labor_supply_pressure_l1" = "Foreign Labor Supply Pressure (t-1)",
    "FE_log_labor_efficacy_l1" = "Labor Integration Efficacy (t-1)"
)

# Export TXT
outfile_txt <- file.path(output_dir, "txt", "regression_crime_type_comparison.txt")
modelsummary(
    models,
    coef_map = coef_map,
    stars = c('*' = .1, '**' = .05, '***' = .01),
    gof_map = gof_mapping,
    add_rows = rows,
    title = "Comparative Crime Analysis: Regional Prosperity (Dep. Var: Log GDP per Capita)",
    notes = "Standard errors clustered at the district level.",
    output = outfile_txt
)

# Export HTML (Stylized)
outfile_html <- file.path(output_dir, "html", "regression_crime_type_comparison.html")

table_styled <- modelsummary(
    models,
    coef_map = coef_map,
    stars = c('*' = .1, '**' = .05, '***' = .01),
    gof_map = gof_mapping,
    add_rows = rows,
    output = "gt"
) %>%
    tab_header(
        title = md("Comparative Crime Analysis: Regional Prosperity"),
        subtitle = md("Separate Comparison of Three Primary Crime Proxies")
    ) %>%
    tab_source_note(
        source_note = "Standard errors clustered at the district level."
    ) %>%
    tab_options(
        table.border.top.style = "solid",
        table.border.top.width = px(3),
        table.border.top.color = "black",
        table.border.bottom.style = "solid",
        table.border.bottom.width = px(3),
        table.border.bottom.color = "black",
        column_labels.border.bottom.style = "solid",
        column_labels.border.bottom.width = px(2),
        column_labels.border.bottom.color = "black",
        table_body.border.bottom.style = "solid",
        table_body.border.bottom.width = px(2),
        table_body.border.bottom.color = "black",
        table.font.names = "Times New Roman",
        heading.align = "center"
    ) %>%
    tab_style(style = cell_text(size = px(14)), locations = cells_body()) %>%
    tab_style(
        style = cell_text(weight = "bold", size = px(15)),
        locations = cells_column_labels()
    ) %>%
    tab_style(
        style = cell_text(weight = "bold", size = px(18)),
        locations = cells_title()
    ) %>%
    tab_style(
        style = cell_text(size = px(12)),
        locations = cells_source_notes()
    ) %>%
    cols_width(1 ~ px(280)) %>%
    text_transform(
        locations = cells_body(),
        fn = function(x) {
            sapply(x, function(cell_text) {
                if (grepl("\\*", cell_text)) {
                    bolded <- gsub(
                        "(\\*+)",
                        "<span style='font-weight:bold;'>\\1</span>",
                        cell_text
                    )
                    return(as.character(html(bolded)))
                }
                return(cell_text)
            })
        }
    )

gtsave(table_styled, outfile_html)

message(
    "\u2713 Comparison results saved to 03_results/tables/regression_crime_type_comparison.txt and .html"
)
