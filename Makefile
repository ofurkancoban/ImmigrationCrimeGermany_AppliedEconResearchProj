# Makefile for Immigration, Crime, and Prosperity Project
# ------------------------------------------------------------------------------
# This Makefile orchestrates the 32-stage research pipeline.
# Requirements: R, Quarto, and 'renv' package.

RSCRIPT = Rscript

.PHONY: help all init preprocess analysis paper presentation clean

help:
	@echo "Available commands:"
	@echo "  make init          : Initialize renv and restore all R packages"
	@echo "  make preprocess    : Run stages 01-13 (Data downloading & cleaning)"
	@echo "  make analysis      : Run stages 14-30 (Econometrics & diagnostics)"
	@echo "  make paper         : Render the final Quarto research paper (PDF/HTML)"
	@echo "  make presentation  : Render the Quarto Reveal.js presentation"
	@echo "  make all           : Run the entire end-to-end pipeline (01-32)"
	@echo "  make clean         : Remove temporary build artifacts and logs"

all: init preprocess analysis paper presentation

init:
	$(RSCRIPT) -e "if (!requireNamespace('renv', quietly = TRUE)) install.packages('renv'); renv::restore()"

preprocess:
	@echo "Running Preprocessing Stages (01-13)..."
	$(RSCRIPT) 02_scripts/01_setup/setup_environment.R --stage=preprocess

analysis:
	@echo "Running Analysis Stages (14-30)..."
	$(RSCRIPT) 02_scripts/01_setup/setup_environment.R --stage=analysis

paper:
	@echo "Rendering Research Paper..."
	$(RSCRIPT) 04_paper/31_Render_Paper.R

presentation:
	@echo "Rendering Presentation..."
	$(RSCRIPT) 05_presentation/32_Render_Presentation.R

clean:
	@echo "Cleaning up temporary files..."
	find . -type d -name "*_cache" -exec rm -rf {} +
	find . -type d -name "*_files" -exec rm -rf {} +
	find . -type f -name "*.log" -delete
	find . -type f -name "*.aux" -delete
	find . -type f -name "*.out" -delete
	@echo "Cleanup complete."
