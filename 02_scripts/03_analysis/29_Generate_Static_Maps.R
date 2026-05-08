# ==============================================================================
# File:          29_Generate_Static_Maps.R
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
# Category:      Data Visualization
#
# Description:   Generates high-resolution static choropleth maps for the 
#                Term Paper using official BKG VG250 (2024) spatial boundaries.
#                Includes labels for outliers and log-scales for skewed data.
#
# Inputs:        Final Panel: 01_datasets/final/panel_data_final_2003_2021.csv,
#                BKG Shapefile: 01_datasets/geodata/vg250_*/VG250_KRS.shp
# Outputs:       Static PNG maps in 03_results/plots/
# ==============================================================================

# 1. Setup ---------------------------------------------------------------------
source(here::here("02_scripts", "02_data_preprocessing", "00_import.R"))
source(here::here("02_scripts", "04_tools", "utils.R"))

# Load spatial libraries
library(sf)
library(ggplot2)
library(viridis)
library(ggrepel)
library(dplyr)
library(tidyr)

input_file <- here::here("01_datasets", "final", "panel_data_final_2003_2021.csv")
output_dir <- here::here("03_results", "plots")
ensure_dir(output_dir)

# Path to BKG Shapefile
shp_path <- here::here(
    "01_datasets", "geodata", 
    "vg250_01-01.utm32s.shape.ebenen", 
    "vg250_ebenen_0101", 
    "VG250_KRS.shp"
)

# 2. Load & Prepare Spatial Data -----------------------------------------------
message("Loading BKG VG250 (2024) boundaries...")

germany_sf <- st_read(shp_path, quiet = TRUE) %>%
    filter(GF == 4) %>%
    mutate(district_id = substr(AGS, 1, 5)) %>%
    select(district_id, GEN, geometry)

# 3. Load & Filter Panel Data --------------------------------------------------
message("Loading panel data (2021 snapshot)...")
df_2021 <- prepare_panel_data(input_file) %>%
    filter(year == 2021)

# 4. Helper: Create Map (inkaR Style + Outlier Labels + Log Scale) -------------

create_official_map <- function(shp, data, var_name, title_str, palette_opt = "viridis", n_outliers = 7, use_log = FALSE) {
    
    # Join
    map_data <- shp %>%
        left_join(data, by = "district_id")
    
    # Identify Outliers (Top and Bottom N)
    outliers <- map_data %>%
        filter(!is.na(.data[[var_name]])) %>%
        arrange(desc(.data[[var_name]])) %>%
        slice(c(1:n_outliers, (n() - n_outliers + 1):n())) %>%
        mutate(label = GEN)
    
    # Map with inkaR-inspired styling
    p <- ggplot(map_data) +
        geom_sf(aes(fill = .data[[var_name]]), color = "white", linewidth = 0.05) +
        # Add outlier labels using ggrepel
        geom_text_repel(
            data = outliers,
            aes(label = label, geometry = geometry),
            stat = "sf_coordinates",
            size = 2.5,
            fontface = "bold",
            box.padding = 0.5,
            point.padding = 0.3,
            min.segment.length = 0,
            segment.color = "grey50",
            segment.size = 0.2,
            bg.color = "white",
            bg.r = 0.1
        )
    
    # Handle Scaling (Logarithmic or Linear)
    if (use_log) {
        # Log-scale is excellent for variables with extreme outliers like Crime or GDP
        p <- p + scale_fill_viridis_c(
            option = palette_opt, 
            name = NULL,
            na.value = "grey95",
            trans = "log10", # Log-transform to handle outliers (like Koblenz)
            labels = scales::comma # Keeps original scale numbers in legend
        )
    } else {
        p <- p + scale_fill_viridis_c(
            option = palette_opt, 
            name = NULL, 
            labels = scales::comma,
            na.value = "grey95"
        )
    }
    
    # Final styling
    p + inkaR::theme_inkaR(mode = "light") +
        labs(
            title = title_str,
            subtitle = paste0(
                "Status: 31.12.2021 | Spatial: BKG VG250 (2024)\n",
                if(use_log) "Scale: Logarithmic (to handle outliers) | " else "",
                "Labels: Top/Bottom ", n_outliers, " Districts"
            ),
            caption = "Source: BBSR INKAR, PKS, and BKG (2024)."
        ) +
        theme(
            plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
            plot.subtitle = element_text(size = 10, hjust = 0.5, lineheight = 1.2),
            axis.text = element_blank(),
            axis.title = element_blank(),
            panel.grid = element_blank()
        )
}

# 5. Generate Maps -------------------------------------------------------------

# 5.1 GDP per Capita (Log-scale is common for wealth distributions)
p_gdp <- create_official_map(
    germany_sf, df_2021, 
    "INKAR_INCOME_gdp_per_capita", 
    "Regional Economic Prosperity (GDP per Capita)",
    palette_opt = "viridis",
    use_log = TRUE
)

# 5.2 Labor Integration Efficacy
p_efficacy <- create_official_map(
    germany_sf, df_2021, 
    "labor_efficacy", 
    "Labor Market Integration Performance",
    palette_opt = "plasma",
    use_log = FALSE
)

# 5.3 Property Damage Rate (using raw rate for map usually)
p_crime <- create_official_map(
    germany_sf, df_2021, 
    "PKS_property_damage_rate", 
    "Regional Safety Snapshot (Property Damage)",
    palette_opt = "magma",
    use_log = TRUE
)

# 6. Export --------------------------------------------------------------------
message("Saving final high-res labelled maps...")

ggsave(file.path(output_dir, "map_gdp_2021.png"), p_gdp, width = 8, height = 10, dpi = 300, bg = "white")
ggsave(file.path(output_dir, "map_efficacy_2021.png"), p_efficacy, width = 8, height = 10, dpi = 300, bg = "white")
ggsave(file.path(output_dir, "map_crime_2021.png"), p_crime, width = 8, height = 10, dpi = 300, bg = "white")

message("✓ All Labeled Maps (with Log-Scaling) generated successfully.")
