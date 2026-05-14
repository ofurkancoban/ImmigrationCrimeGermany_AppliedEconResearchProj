# ==============================================================================
# File:          32_Render_Presentation.R
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
# Category:      Presentation Rendering
#
# Description:   Automates the rendering of the Reveal.js presentation (presentation.qmd),
#                generating a high-quality slide deck for the research project.
#
# Inputs:        Quarto Presentation: 05_presentation/presentation.qmd
# Outputs:       Presentation (HTML) in 05_presentation/
# ==============================================================================

if (!requireNamespace("quarto", quietly = TRUE)) install.packages("quarto")
library(quarto)

pres_path <- here::here("05_presentation", "presentation.qmd")

message("Starting Quarto Render: Presentation (Reveal.js)...")

# Render specifically to Reveal.js (Presentation)
quarto_render(input = pres_path, output_format = "revealjs")

# Copy self-contained presentation to 04_paper folder for the HTML paper website link
dest_path <- here::here("04_paper", "presentation.html")
file.copy(from = here::here("05_presentation", "presentation.html"), to = dest_path, overwrite = TRUE)

message("✓ Presentation rendering complete and copied to '04_paper/'.")
