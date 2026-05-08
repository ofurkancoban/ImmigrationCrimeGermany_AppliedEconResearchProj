# ==============================================================================
# File:          30_Generate_Interactive_Map.R
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
# Description:   Produces a complex, interactive Leaflet leaflet mapping widget plotting the spatial harmonization process or core regional indicators across the German districts, generating directly embeddable HTML assets for the slide deck.
#
# Inputs:        Spatial datasets and Final Panel: 01_datasets/final/panel_data_final_2003_2021.csv
# Outputs:       interactive_map.html exported to 05_presentation/assets/
# ==============================================================================

source(here::here("02_scripts", "02_data_preprocessing", "00_import.R"))

# 1. Fetch High-Resolution Baselines (Resolution 03)
message("Fetching High-Precision Baselines with Descriptive Tooltips...")
options(timeout = 300)

# 2006 (Historical)
de_2006 <- gisco_get_nuts(
  nuts_level = 3,
  year = "2006",
  country = "Germany",
  resolution = "03"
) %>%
  st_transform(4326)

# 2024 (Modern)
de_2024 <- gisco_get_nuts(
  nuts_level = 3,
  year = "2024",
  country = "Germany",
  resolution = "03"
) %>%
  st_transform(4326)

# --- MV STATE SCOPE (NUTS-2: DE80) ---

# Historical MV (2006) - Focus strictly on Rostock and Demmin components
mv_2006 <- de_2006 %>%
  filter(NUTS_ID %in% c("DE803", "DE807", "DE808", "DE809"))

# Modern MV (2024) - Focus strictly on targets of Rostock and Demmin
mv_2024 <- de_2024 %>%
  filter(NUTS_ID %in% c("DE803", "DE80K", "DE80J", "DE80N"))

# Logic for Descriptive Labels (2006 Structure)
mv_2006_labels <- mv_2006 %>%
  mutate(
    Tooltip = case_when(
      NUTS_ID %in% c("DE807", "DE809") ~ paste0(
        "<b>",
        NUTS_NAME,
        "</b><br/>Status: To be Merged into Rostock<br/>Weight: 1.0"
      ),
      NUTS_ID ==
        "DE808" ~ "<b>Demmin</b><br/>Status: To be Split<br/>Destinations: Seenplatte (82%), Vorpommern-GW (18%)",
      NUTS_ID ==
        "DE803" ~ "<b>Rostock City</b><br/>Status: Independent City (No Change)",
      TRUE ~ paste0("<b>", NUTS_NAME, "</b>")
    ),
    FillColor = case_when(
      NUTS_ID %in% c("DE807", "DE809") ~ "#5e72e4", # Merger parts
      NUTS_ID == "DE808" ~ "#663399", # Split source
      NUTS_ID %in% c("DE803", "DE804") ~ "#34495e", # Independent cities
      TRUE ~ "#4292c6" # A vibrant blue instead of gray
    )
  )

# Logic for Descriptive Labels (2024 Structure)
# Note: We need the fragments for Demmin again for precision
demmin_2006 <- de_2006 %>% filter(NUTS_ID == "DE808")
targets_modern <- de_2024 %>% filter(NUTS_ID %in% c("DE80J", "DE80N"))
demmin_fragments <- st_intersection(demmin_2006, targets_modern) %>%
  st_collection_extract("POLYGON") %>%
  mutate(
    Tooltip = ifelse(
      NUTS_ID.1 == "DE80J",
      "<b>Fragment from Demmin</b><br/>Destination: MSE (Seenplatte)<br/>Harmonization Weight: 0.824",
      "<b>Fragment from Demmin</b><br/>Destination: V-GW<br/>Harmonization Weight: 0.176"
    ),
    FillColor = ifelse(NUTS_ID.1 == "DE80J", "#ff8c00", "#ff4500")
  )

mv_2024_labels <- mv_2024 %>%
  mutate(
    Tooltip = case_when(
      NUTS_ID ==
        "DE80K" ~ "<b>Landkreis Rostock</b><br/>Status: Harmonized (Merged)<br/>Components: Bad Doberan + Güstrow",
      NUTS_ID ==
        "DE803" ~ "<b>Rostock City</b><br/>Status: Independent City (Fixed)",
      TRUE ~ paste0("<b>", NUTS_NAME, "</b>")
    ),
    FillColor = case_when(
      NUTS_ID == "DE80K" ~ "#ffcc00", # Rostock region (Merged)
      NUTS_ID == "DE803" ~ "#8e44ad", # Rostock City (distinct purple)
      NUTS_ID == "DE80J" ~ "#27ae60", # Mecklenburgische Seenplatte (emerald green)
      NUTS_ID == "DE80N" ~ "#2980b9", # Vorpommern-Greifswald (distinct strong blue)
      NUTS_ID == "DE804" ~ "#2c3e50", # Other cities (if any)
      TRUE ~ "#4292c6" # Vibrant blue fallback
    )
  )

# 2. Interactive Map (Static/Locked View)
m <- leaflet(
  options = leafletOptions(
    dragging = FALSE,
    zoomControl = FALSE,
    scrollWheelZoom = FALSE,
    doubleClickZoom = FALSE,
    touchZoom = FALSE
  )
) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%

  # MODE 1: Historical Structure
  addPolygons(
    data = mv_2006_labels,
    color = "#fff",
    weight = 2.5,
    fillColor = ~FillColor,
    fillOpacity = 0.8,
    group = "Historical Structure (2006)",
    label = ~ lapply(Tooltip, htmltools::HTML),
    highlightOptions = highlightOptions(
      weight = 4,
      color = "#fff",
      bringToFront = TRUE
    )
  ) %>%
  addLabelOnlyMarkers(
    data = suppressWarnings(st_centroid(mv_2006_labels)),
    label = ~NUTS_NAME,
    labelOptions = labelOptions(
      noHide = TRUE,
      direction = "center",
      textOnly = TRUE,
      style = list(
        "font-size" = "11px",
        "font-weight" = "bold",
        "color" = "#004079",
        "text-shadow" = "0px 0px 3px white, 0px 0px 3px white"
      )
    ),
    group = "Historical Structure (2006)"
  ) %>%

  # MODE 2: Harmonized Structure
  addPolygons(
    data = mv_2024_labels,
    color = "#fff",
    weight = 3,
    fillColor = ~FillColor,
    fillOpacity = 0.8,
    group = "Harmonized Structure (2023)",
    label = ~ lapply(Tooltip, htmltools::HTML),
    highlightOptions = highlightOptions(
      weight = 4,
      color = "#fff",
      bringToFront = TRUE
    )
  ) %>%
  addLabelOnlyMarkers(
    data = suppressWarnings(st_centroid(mv_2024_labels)),
    label = ~NUTS_NAME,
    labelOptions = labelOptions(
      noHide = TRUE,
      direction = "center",
      textOnly = TRUE,
      style = list(
        "font-size" = "11px",
        "font-weight" = "bold",
        "color" = "#004079",
        "text-shadow" = "0px 0px 3px white, 0px 0px 3px white"
      )
    ),
    group = "Harmonized Structure (2023)"
  ) %>%
  # Add the split fragments on top for detail
  addPolygons(
    data = demmin_fragments,
    color = "#fff",
    weight = 2.5,
    fillColor = ~FillColor,
    fillOpacity = 0.9,
    group = "Harmonized Structure (2023)",
    label = ~ lapply(Tooltip, htmltools::HTML)
  ) %>%

  addLayersControl(
    baseGroups = c(
      "Historical Structure (2006)",
      "Harmonized Structure (2023)"
    ),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  setView(lng = 12.85, lat = 53.85, zoom = 8)

saveWidget(
  m,
  here::here("05_presentation", "assets", "interactive_map.html"),
  selfcontained = TRUE
)

# 3. Calculation Tables (Premium & Wide)
t_rostock <- data.frame(
  Unit = c("Bad Doberan", "Güstrow", "**Total**"),
  Weight = c("1.0", "1.0", ""),
  Contribution = c("7,223", "9,354", "**16,577**")
) %>%
  gt() %>%
  tab_header("Ex 1: Rostock (Merger)") %>%
  fmt_markdown(columns = everything()) %>%
  cols_align(align = "center", columns = c(Weight, Contribution)) %>%
  cols_align(align = "left", columns = Unit) %>%
  opt_table_font(font = "Plus Jakarta Sans") %>%
  opt_table_lines("default") %>%
  tab_options(
    heading.title.font.size = px(14),
    heading.title.font.weight = "bold",
    column_labels.font.weight = "bold",
    column_labels.font.size = px(13),
    column_labels.border.bottom.color = "black",
    column_labels.border.bottom.width = px(2),
    column_labels.border.top.color = "black",
    column_labels.border.top.width = px(2),
    table.font.size = px(13),
    data_row.padding = px(6),
    table.width = px(550),
    table_body.border.bottom.color = "black",
    table_body.border.bottom.width = px(2)
  )

t_demmin <- data.frame(
  Unit = c(
    "Demmin to Mecklenburgische Seenplatte",
    "Demmin to Vorpommern-Greifswald",
    "**Total**"
  ),
  Weight = c("0.824", "0.176", ""),
  Contribution = c("5,949", "1,271", "**7,220**")
) %>%
  gt() %>%
  tab_header("Ex 2: Demmin (Split)") %>%
  fmt_markdown(columns = everything()) %>%
  cols_align(align = "center", columns = c(Weight, Contribution)) %>%
  cols_align(align = "left", columns = Unit) %>%
  opt_table_font(font = "Plus Jakarta Sans") %>%
  opt_table_lines("default") %>%
  tab_options(
    heading.title.font.size = px(14),
    heading.title.font.weight = "bold",
    column_labels.font.weight = "bold",
    column_labels.font.size = px(13),
    column_labels.border.bottom.color = "black",
    column_labels.border.bottom.width = px(2),
    column_labels.border.top.color = "black",
    column_labels.border.top.width = px(2),
    table.font.size = px(13),
    data_row.padding = px(6),
    table.width = px(550),
    table_body.border.bottom.color = "black",
    table_body.border.bottom.width = px(2)
  )

gtsave(
  t_rostock,
  here::here("05_presentation", "assets", "calc_table_rostock.html")
)
gtsave(t_demmin, here::here("05_presentation", "assets", "calc_table_demmin.html"))

message("✓ High-Precision Map and Premium Wide Tables Generated.")
