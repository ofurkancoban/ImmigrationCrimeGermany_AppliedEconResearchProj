# ==============================================================================
# File:          05_PKS_Translation.R
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
# Description:   Translates and standardizes German variable names, categorizes crime typologies (e.g., Diebstahl to Theft, Gewaltdelikte to Violent Crime), and structures the final panel structure for the Crime vector. It prepares the PKS data subset for the final merge.
#
# Inputs:        Spatially Harmonized PKS Data Panel
# Outputs:       Cleaned and translated PKS subset: 01_datasets/processed/PKS/03-translated/
# ==============================================================================

# 1. SETUP
# ------------------------------------------------------------------------------
# Source global utilities (packages, directory initialization, robustness helpers)
source(here::here("02_scripts", "02_data_preprocessing", "00_import.R"))

# Script-specific configuration
force_process <- FALSE # Set to TRUE to overwrite existing files

# Define Paths
input_dir <- here::here("01_datasets", "processed", "PKS", "02-harmonized")
output_dir <- ensure_dir(here::here("01_datasets", "processed", "PKS", "03-translated"))

# -------------------------------------------------------------------------
# Translation Dictionaries
# -------------------------------------------------------------------------

# 1. Column Names
col_map <- c(
    "Schluesselzahl" = "key",
    "Straftat" = "offense",
    "Kreisschluessel" = "district_id",
    "Kreisart" = "district_type",
    "Kreis" = "district_name",
    "Einwohner" = "population",
    "Erfasste_Faelle" = "cases_reported",
    "Straftaten_HZ" = "cases_per_100k",
    "Versuche_Anzahl" = "attempts_count",
    "Versuche_Anteil_Prozent" = "attempts_rate",
    "Mit_Schusswaffe_gedroht" = "threatened_with_firearm",
    "Mit_Schusswaffe_geschossen" = "shot_with_firearm",
    "Aufklaerung_Faelle" = "cases_solved",
    "Aufklaerungsquote" = "clearance_rate",
    "Tatverdaechtige_Insgesamt" = "suspects_total",
    "Tatverdaechtige_Maennlich" = "suspects_male",
    "Tatverdaechtige_Weiblich" = "suspects_female",
    "Nichtdeutsche_Tatverdaechtige_Anzahl" = "suspects_non_german_count",
    "Nichtdeutsche_Tatverdaechtige_Prozent" = "suspects_non_german_rate"
)

# 2. Offense Descriptions (Straftat)
offense_map <- c(
    "Straftaten insgesamt" = "Total Offenses",
    "Gewaltkriminalität" = "Violent Crime",
    "Straßenkriminalität" = "Street Crime",
    "Diebstahl insgesamt und zwar:" = "Theft Total (of which:)",
    "Diebstahl ohne erschwerende Umstände §§ 242" = "Theft without aggravating circumstances",
    "Diebstahl unter erschwerenden Umständen §§ 243-244a StGB und zwar:" = "Theft under aggravating circumstances (of which:)",
    "Wohnungseinbruchdiebstahl §§ 244 Abs. 1 Nr. 3 und Abs. 4" = "Residential Burglary",
    "Tageswohnungseinbruchdiebstahl §§ 244 Abs. 1 Nr. 3 und Abs. 4" = "Daytime Residential Burglary",
    "Diebstahl insgesamt an/aus Kraftfahrzeugen" = "Theft from/of Motor Vehicles",
    "Diebstahl insgesamt von Kraftwagen einschl. unbefugte Ingebrauchnahme" = "Theft of Motor Cars incl. unauthorized use",
    "Diebstahl insgesamt von Mopeds und Krafträdern einschl. unbefugte Ingebrauchnahme" = "Theft of Mopeds/Motorcycles incl. unauthorized use",
    "Diebstahl insgesamt von Fahrrädern einschl. unbefugte Ingebrauchnahme" = "Bicycle Theft incl. unauthorized use",
    "Ladendiebstahl insgesamt" = "Shoplifting Total",
    "Einfacher Ladendiebstahl" = "Simple Shoplifting",
    "Taschendiebstahl insgesamt" = "Pickpocketing Total",
    "Raub" = "Robbery",
    "Raubüberfälle in Wohnungen" = "Robbery in Dwellings",
    "Handtaschenraub" = "Handbag Snatching",
    "Sonstige Raubüberfälle auf Straßen" = "Other Street Robbery",
    "Körperverletzung §§ 223-227, 229, 231" = "Bodily Harm",
    "Gefährliche und schwere Körperverletzung" = "Dangerous and Serious Bodily Harm",
    "Vorsätzliche einfache Körperverletzung § 223 StGB" = "Intentional Simple Bodily Harm",
    "Mord" = "Murder",
    "Totschlag" = "Manslaughter",
    "Vergewaltigung" = "Rape",
    "Vergewaltigung, sexuelle Nötigung und sexueller Übergriff im besonders schweren Fall einschl. mit Todesfolge" = "Rape, sexual coercion, and sexual assault in especially serious cases incl. with fatal consequences",
    "Rauschgiftdelikte (soweit nicht bereits mit anderer Schlüsselzahl erfasst)" = "Drug Offenses (if not captured elsewhere)",
    "Betrug §§ 263" = "Fraud",
    "Erschleichen von Leistungen § 265a StGB" = "Obtaining Services by Deception",
    "Unterschlagung §§ 246" = "Embezzlement",
    "Urkundenfälschung §§ 267-271" = "Forgery of Documents",
    "Sachbeschädigung §§ 303-305a StGB" = "Property Damage",
    "Sachbeschädigung durch Graffiti insgesamt" = "Graffiti Property Damage",
    "Brandstiftung und Herbeiführen einer Brandgefahr §§ 306-306d" = "Arson and Causing Explosion/Fire",
    "Widerstand gegen Vollstreckungsbeamte und gleichstehende Personen §§ 113" = "Resistance against Enforcement Officers",
    "Tätlicher Angriff auf Vollstreckungsbeamte und gleichstehende Personen §§ 114" = "Assault on Enforcement Officers",
    "Widerstand gegen und tätlicher Angriff auf Vollstreckungsbeamte und gleichstehende Personen §§ 113-115 StGB" = "Resistance/Assault on Enforcement Officers (Combined)",
    "Beförderungserschleichung" = "Fare Evasion",
    "Begünstigung" = "Assistance after the fact",
    "Cybercrime" = "Cybercrime",
    "Unerlaubt eingereiste/aufhältige Personen (SZ: 725100" = "Unauthorized Entry/Stay",
    "Straftaten gegen das Aufenthalts-" = "Immigration Offenses",
    "Straftaten gegen das Leben" = "Crimes against Life",
    # --- Legacy Codes (2003-2012) ---
    "DiebstahlKfz" = "Theft of Motor Cars incl. unauthorized use",
    "Koerperverletzung" = "Bodily Harm",
    "Wohnungseinbruch" = "Residential Burglary",
    "Sachbeschaedigung" = "Property Damage",
    "Rauschgift" = "Drug Offenses",
    "Strassenkriminalitaet" = "Street Crime",
    "(Vorsätzliche leichte) Körperverletzung § 223 StGB" = "Intentional Simple Bodily Harm",
    "Wohnungseinbruchdiebstahl § 244 Abs. 1 Nr. 3 StGB darunter:" = "Residential Burglary (incl.)",
    "Tageswohnungseinbruch" = "Daytime Residential Burglary",
    "Widerstand gegen Polizeivollzugsbeamte" = "Resistance against Police Officers",
    "Computerkriminalität" = "Computer Crime",
    "IuK-Kriminalität im engeren Sinne (SZ: 517500, 517900, 543000, 674200, 678000)" = "ICT Crime (narrow definition)",
    "IuK-Kriminalität im engeren Sinne" = "ICT Crime (narrow definition)",
    "Wohnungseinbruchdiebstahl § 244 Abs. 1 Nr. 3, § 244a StGB" = "Residential Burglary",
    "Tageswohnungseinbruchdiebstahl § 244 Abs. 1 Nr. 3, § 244a StGB" = "Daytime Residential Burglary",
    "Wohnungseinbruchdiebstahl §§ 244 Abs. 1 Nr. 3, 244a StGB" = "Residential Burglary",
    "Tageswohnungseinbruchdiebstahl §§ 244 Abs. 1 Nr. 3, 244a StGB" = "Daytime Residential Burglary"
)

# Helper function to clean and map offenses
translate_offense <- function(x) {
    x <- gsub('"', '', x)
    x <- stringr::str_trim(x)
    if (x %in% names(offense_map)) return(offense_map[[x]])
    for (german in names(offense_map)) {
        if (grepl(german, x, fixed = TRUE)) return(offense_map[[german]])
    }
    return(x)
}

# --- Main Processing Loop ---

files <- list.files(input_dir, pattern = "\\.csv$", full.names = TRUE)

message("===== STARTING DATA TRANSLATION =====")

for (f in files) {
    filename <- basename(f)
    new_filename <- gsub("_harmonized.csv", "_translated.csv", filename)
    out_path <- file.path(output_dir, new_filename)

    # Standardized skip logic
    if (should_skip(out_path, force = force_process)) next

    message("\n→ Translating: ", filename)

    # Read Data
    data <- read.csv(f, colClasses = "character")

    # 1. Translate Columns
    current_cols <- names(data)
    new_cols <- current_cols
    for (i in seq_along(current_cols)) {
        col <- current_cols[i]
        if (col %in% names(col_map)) new_cols[i] <- col_map[[col]]
    }
    names(data) <- new_cols

    # 2. Translate Content (Optimized)
    if ("offense" %in% names(data)) {
        unique_offenses <- unique(data$offense)
        translated_values <- sapply(unique_offenses, translate_offense)
        lookup_df <- data.frame(
            offense = unique_offenses,
            offense_en = translated_values,
            stringsAsFactors = FALSE
        )
        data <- data |>
            dplyr::left_join(lookup_df, by = "offense") |>
            dplyr::mutate(offense = offense_en) |>
            dplyr::select(-offense_en)
    }

    # 3. Save
    write.csv(data, out_path, row.names = FALSE, quote = TRUE)
    message("    ✓ Saved: ", new_filename)
}

message("\n===== TRANSLATION COMPLETED =====")
