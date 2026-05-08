# ==============================================================================
# File:          14_Descriptive_Statistics.R
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
# Description:   Generates descriptive statistics for the research paper, 
#                including summary tables, correlation matrices, and 
#                temporal trend visualizations (East vs. West).
#
# Inputs:        Final Panel: 01_datasets/final/panel_data_final_2003_2021.csv
# Outputs:       Tables (html/txt) in 03_results/tables/ and Plots in 03_results/plots/
# ==============================================================================

# 1. Setup & Utilities ---------------------------------------------------------
source(here::here("02_scripts", "02_data_preprocessing", "00_import.R"))
source(here::here("02_scripts", "04_tools", "utils.R"))

# Script-specific configuration
force_process <- TRUE 
input_file <- here::here("01_datasets", "final", "panel_data_final_2003_2021.csv")
output_dir_tables <- here::here("03_results", "tables")
output_dir_plots <- here::here("03_results", "plots")

# Ensure directories exist
ensure_dir(file.path(output_dir_tables, "html"))
ensure_dir(file.path(output_dir_tables, "txt"))
ensure_dir(output_dir_plots)

# 2. Data Preparation ----------------------------------------------------------
df <- prepare_panel_data(input_file) %>%
    mutate(
        # Create East/West dummy for comparison
        # AGS starting with 13, 14, 15, 16 (excluding Berlin 11 for simplicity or keeping it separate)
        is_east = ifelse(substr(district_id, 1, 2) %in% c("12", "13", "14", "15", "16"), "East", "West")
    )

message(glue::glue("Data Loaded: {nrow(df)} observations from {length(unique(df$district_id))} districts."))

# 3. Advanced Summary Statistics (Between vs. Within) -------------------------
# Define variables of interest
vars_summary <- c(
    "log_gdp_pc", 
    "FE_log_labor_efficacy_l1", 
    "log_property_damage_l1", 
    "FE_foreign_labor_supply_pressure_l1", 
    "log_unemp_l1", 
    "FE_share_children", 
    "FE_share_youth", 
    "foreigner_share"
)

var_labels <- c(
    "log_gdp_pc" = "Log GDP per Capita",
    "FE_log_labor_efficacy_l1" = "Labor Integration Efficacy (t-1)",
    "log_property_damage_l1" = "Log Property Damage Rate (t-1)",
    "FE_foreign_labor_supply_pressure_l1" = "Foreign Labor Supply Pressure (t-1)",
    "log_unemp_l1" = "Log Unemployment (t-1)",
    "FE_share_children" = "Share Children (0-17)",
    "FE_share_youth" = "Share Youth (18-24)",
    "foreigner_share" = "Foreigner Share (%)"
)

# Function to calculate Between and Within Standard Deviation
calc_panel_sd <- function(data, vars) {
    results <- list()
    for (v in vars) {
        # Overall
        sd_overall <- sd(data[[v]], na.rm = TRUE)
        
        # Between (SD of district means)
        district_means <- data %>%
            group_by(district_id) %>%
            summarise(mean_x = mean(.data[[v]], na.rm = TRUE), .groups = "drop")
        sd_between <- sd(district_means$mean_x, na.rm = TRUE)
        
        # Within (SD of deviation from district mean)
        data_within <- data %>%
            group_by(district_id) %>%
            mutate(mean_x = mean(.data[[v]], na.rm = TRUE)) %>%
            mutate(within_x = .data[[v]] - mean_x + mean(data[[v]], na.rm = TRUE)) %>%
            ungroup()
        sd_within <- sd(data_within$within_x, na.rm = TRUE)
        
        results[[v]] <- data.frame(
            Variable = var_labels[v],
            Overall_SD = sd_overall,
            Between_SD = sd_between,
            Within_SD = sd_within,
            Ratio_B_W = sd_between / sd_within
        )
    }
    do.call(rbind, results)
}

panel_sd_table <- calc_panel_sd(df, vars_summary) %>%
    gt::gt() %>%
    apply_academic_theme(
        title = "Panel Variance Decomposition",
        subtitle = "Comparison of Between-District and Within-District Variation",
        source_note = "Between SD reflects spatial differences; Within SD reflects temporal changes."
    ) %>%
    gt::fmt_number(columns = 2:5, decimals = 3)

gt::gtsave(panel_sd_table, file.path(output_dir_tables, "html", "panel_variance_decomposition.html"))
message("✓ Variance Decomposition Table exported.")

# 4. Enhanced Summary Table (Distribution Metrics) -----------------------------
# Custom skewness and kurtosis functions
skewness <- function(x) {
    x <- x[!is.na(x)]
    n <- length(x)
    res <- sum((x - mean(x))^3) / (n * sd(x)^3)
    return(res)
}

kurtosis <- function(x) {
    x <- x[!is.na(x)]
    n <- length(x)
    res <- n * sum((x - mean(x))^4) / (sum((x - mean(x))^2)^2)
    return(res)
}

# Skim table with more percentiles and distribution metrics
enhanced_summary <- df %>%
    select(all_of(vars_summary)) %>%
    rename(any_of(var_labels)) %>%
    modelsummary::datasummary(
        All(.) ~ Mean + SD + Min + P25 + Median + P75 + Max + skewness + kurtosis,
        data = .,
        output = "gt"
    ) %>%
    apply_academic_theme(
        title = "Detailed Descriptive Statistics",
        subtitle = "Including Distributional Moments and Percentiles",
        source_note = "Metrics calculated across all district-year observations."
    )

gt::gtsave(enhanced_summary, file.path(output_dir_tables, "html", "enhanced_summary_statistics.html"))
message("✓ Enhanced Summary Table exported.")

# 5. Regional Comparison (East vs. West) ---------------------------------------
regional_comp_table <- df %>%
    select(is_east, all_of(vars_summary)) %>%
    rename(any_of(var_labels)) %>%
    modelsummary::datasummary(
        (`Region` = is_east) ~ (`Variable` = All(.)) * (Mean + SD),
        data = .,
        output = "gt"
    ) %>%
    apply_academic_theme(
        title = "Regional Comparison: East vs. West Germany",
        subtitle = "Differences in Averages across New and Old Federal States",
        source_note = "East includes Brandenburg, Mecklenburg-Vorpommern, Saxony, Saxony-Anhalt, and Thuringia."
    )

gt::gtsave(regional_comp_table, file.path(output_dir_tables, "html", "regional_comparison_east_west.html"))
message("✓ Regional Comparison Table exported.")

# 6. Correlation Matrix --------------------------------------------------------
corr_table <- df %>%
    select(all_of(vars_summary)) %>%
    rename(any_of(var_labels)) %>%
    modelsummary::datasummary_correlation(output = "gt") %>%
    apply_academic_theme(
        title = "Correlation Matrix",
        subtitle = "Pearson Correlation coefficients across main variables",
        source_note = ""
    )

gt::gtsave(corr_table, file.path(output_dir_tables, "html", "correlation_matrix.html"))
message("✓ Correlation Matrix exported.")

# 7. Temporal Trends (Plot) ----------------------------------------------------
# Aggregate data by year and region
df_trends <- df %>%
    group_by(year, is_east) %>%
    summarise(
        mean_gdp = mean(INKAR_INCOME_gdp_per_capita, na.rm = TRUE),
        mean_foreign = mean(foreigner_share, na.rm = TRUE),
        mean_crime = mean(PKS_property_damage_rate, na.rm = TRUE),
        .groups = 'drop'
    )

# Plot: GDP per Capita Trend
p_gdp <- ggplot(df_trends, aes(x = year, y = mean_gdp, color = is_east, group = is_east)) +
    geom_line(size = 1.2) +
    geom_point() +
    scale_color_manual(values = c("East" = "#E41A1C", "West" = "#377EB8")) +
    labs(
        title = "Evolution of GDP per Capita",
        subtitle = "Comparison between East and West German Districts",
        x = "Year", y = "Mean GDP per Capita (EUR)",
        color = "Region"
    ) +
    theme_minimal() +
    geom_vline(xintercept = 2015, linetype = "dashed", alpha = 0.5) +
    annotate("text", x = 2015.5, y = max(df_trends$mean_gdp), label = "2015 Migration Surge", angle = 90, vjust = -0.5, size = 3)

ggsave(file.path(output_dir_plots, "trend_gdp_east_west.png"), p_gdp, width = 10, height = 6, dpi = 300)

# Plot: Foreigner Share Trend
p_foreign <- ggplot(df_trends, aes(x = year, y = mean_foreign, color = is_east, group = is_east)) +
    geom_line(size = 1.2) +
    geom_point() +
    scale_color_manual(values = c("East" = "#E41A1C", "West" = "#377EB8")) +
    labs(
        title = "Evolution of Foreigner Share",
        subtitle = "Average Percentage of Foreign Nationals",
        x = "Year", y = "Foreigner Share (%)",
        color = "Region"
    ) +
    theme_minimal() +
    geom_vline(xintercept = 2015, linetype = "dashed", alpha = 0.5)

ggsave(file.path(output_dir_plots, "trend_foreigner_share.png"), p_foreign, width = 10, height = 6, dpi = 300)

message("✓ Temporal Trend Plots exported.")

# 6. High vs Low Integration Group Comparison ----------------------------------
# Define high/low based on labor efficacy 2021
efficacy_2021 <- df %>%
    filter(year == 2021) %>%
    mutate(group = ntile(FE_log_labor_efficacy_l1, 4)) %>%
    mutate(group_label = case_when(
        group == 4 ~ "High Efficacy (Top 25%)",
        group == 1 ~ "Low Efficacy (Bottom 25%)",
        TRUE ~ "Middle"
    )) %>%
    filter(group_label != "Middle")

p_comp <- ggplot(efficacy_2021, aes(x = group_label, y = log_gdp_pc, fill = group_label)) +
    geom_boxplot() +
    scale_fill_brewer(palette = "Set2") +
    labs(
        title = "GDP per Capita: High vs. Low Integration Efficacy",
        subtitle = "Snapshot comparison (2021)",
        x = "Labor Integration Performance Group", y = "Log GDP per Capita",
        fill = ""
    ) +
    theme_minimal() +
    guides(fill = "none")

ggsave(file.path(output_dir_plots, "comparison_efficacy_2021.png"), p_comp, width = 8, height = 6, dpi = 300)

message("✓ High/Low Comparison Plot exported.")
# 8. Professional Summary Table (Term Paper Style) ----------------------------
# Select raw variables as requested by the user
vars_prof <- c(
    # Dependent
    "INKAR_INCOME_gdp_per_capita", 
    # Integration
    "FE_log_labor_efficacy_l1", 
    "FE_foreign_labor_supply_pressure_l1", 
    "INKAR_INTEGRATION_foreigner_share",
    # Economic
    "CAPITAL_stock_per_capita", 
    "INKAR_EMPLOYMENT_unemployment_rate",
    # Control
    "PKS_property_damage_rate", 
    "FE_share_children", 
    "FE_share_youth"
)

prof_labels <- c(
    "INKAR_INCOME_gdp_per_capita" = "GDP per Capita (€)",
    "FE_log_labor_efficacy_l1" = "Labor Integration Efficacy (t-1)",
    "FE_foreign_labor_supply_pressure_l1" = "Foreign Labor Supply Pressure (t-1)",
    "INKAR_INTEGRATION_foreigner_share" = "Foreigner Share (%)",
    "CAPITAL_stock_per_capita" = "Capital Stock per Capita (€)",
    "INKAR_EMPLOYMENT_unemployment_rate" = "Unemployment Rate (%)",
    "PKS_property_damage_rate" = "Property Damage Rate (per 100k)",
    "FE_share_children" = "Share of Children (0-17) (%)",
    "FE_share_youth" = "Share of Youth (18-24) (%)"
)

# Create the table with categorical row groups
summary_prof <- df %>%
    select(all_of(vars_prof)) %>%
    rename(any_of(prof_labels)) %>%
    modelsummary::datasummary(
        All(.) ~ Mean + SD + Min + Max + N,
        data = .,
        output = "gt"
    ) %>%
    # Add Row Groups for professional look
    gt::tab_row_group(label = "Control Variables", rows = 8:9) %>%
    gt::tab_row_group(label = "Economic Foundations", rows = 5:6) %>%
    gt::tab_row_group(label = "Integration Metrics", rows = 2:4) %>%
    gt::tab_row_group(label = "Dependent Variable", rows = 1) %>%
    apply_academic_theme(
        title = "Summary Statistics for Term Paper",
        subtitle = "Raw values of key metrics across 400 German districts (2003-2021)",
        source_note = "Sources: INKAR, PKS, and calculations by the author. N = 7,600 observations count."
    ) %>%
    gt::fmt_number(columns = 2:5, decimals = 2)

gt::gtsave(summary_prof, file.path(output_dir_tables, "html", "summary_statistics_professional.html"))
message("✓ Professional Summary Table exported.")
# 9. Distribution Plots (Visualizing Methodology) ----------------------------
message("--> Generating Distribution Plots...")

# Function to create Histogram + Density plot
plot_dist <- function(data, var, title, is_log = FALSE) {
    p <- ggplot(data, aes(x = .data[[var]])) +
        geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "#377EB8", alpha = 0.5, color = "white") +
        geom_density(color = "#E41A1C", size = 1) +
        labs(title = title, x = if(is_log) paste("Log", var) else var, y = "Density") +
        theme_minimal(base_size = 12) +
        theme(plot.title = element_text(face = "bold", size = 10))
    return(p)
}

# 9.1 GDP: Raw vs Log
p1_gdp_raw <- plot_dist(df, "INKAR_INCOME_gdp_per_capita", "GDP per Capita (Raw)")
p1_gdp_log <- plot_dist(df, "log_gdp_pc", "Log GDP per Capita", is_log = TRUE)

# 9.2 Property Damage: Raw vs Log
p2_crime_raw <- plot_dist(df, "PKS_property_damage_rate", "Property Damage Rate (Raw)")
p2_crime_log <- plot_dist(df, "log_property_damage", "Log Property Damage Rate", is_log = TRUE)

# 9.3 Labor Integration Efficacy
p3_labor_dist <- plot_dist(df, "FE_log_labor_efficacy_l1", "Labor Integration Efficacy (Log, t-1)")
p3_labor_box <- ggplot(df, aes(x = "", y = FE_log_labor_efficacy_l1)) +
    geom_boxplot(fill = "#4DAF4A", alpha = 0.6) +
    labs(title = "Efficacy Boxplot (Outliers)", x = "", y = "Value") +
    theme_minimal(base_size = 10)

# Combine into academic grids using patchwork
# Grid 1: Justification for Log Transformation (GDP & Crime)
layout_log <- (p1_gdp_raw | p1_gdp_log) / (p2_crime_raw | p2_crime_log)
ggsave(file.path(output_dir_plots, "distribution_log_justification.png"), layout_log, width = 12, height = 8, dpi = 300)

# Grid 2: Main Independent Variable (Labor Efficacy)
layout_labor <- (p3_labor_dist | p3_labor_box)
ggsave(file.path(output_dir_plots, "distribution_labor_efficacy.png"), layout_labor, width = 10, height = 5, dpi = 300)

message("✓ Distribution Plots exported.")

message("==============================================================================")
message("   Descriptive Statistics Analysis Finished Successfully.   ")
message("   Files saved in:                                          ")
message("     - 03_results/tables/html/summary_statistics_professional.html")
message("     - 03_results/tables/html/panel_variance_decomposition.html    ")
message("     - 03_results/plots/distribution_log_justification.png        ")
message("     - 03_results/plots/distribution_labor_efficacy.png            ")
message("     - 03_results/plots/trend_gdp_east_west.png                    ")
message("==============================================================================")
