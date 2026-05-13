# ==============================================================================
# File:          31_Render_Paper.R
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
# Category:      Document Rendering
#
# Description:   Automates the compilation of the Quarto research paper into 
#                PDF and HTML formats. Ensures all dynamic results are correctly 
#                embedded.
#
# Inputs:        Quarto Document: 04_paper/paper.qmd
# Outputs:       Rendered Research Paper in 04_paper/
# ==============================================================================

if (!requireNamespace("quarto", quietly = TRUE)) install.packages("quarto")
library(quarto)

old_wd <- getwd()
setwd(here::here("04_paper"))

message("Starting Quarto Render: Research Paper...")

# Render to all formats defined in the YAML (PDF and HTML)
quarto_render(input = "paper.qmd", output_format = "all")

setwd(old_wd)

message("✓ Paper rendering complete. Check the '04_paper/' directory.")
