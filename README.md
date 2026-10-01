# Immigration, Crime, and Prosperity

## The Economic Drivers of Growth in German Districts

**Project Date:** **27.02.2026**

This project investigates the relationship between immigration labor integration, social stability (measured by property damage), and regional economic prosperity (GDP per capita) across 400 German districts from 2003 to 2021.

### Datasets & Variables

The project utilizes a balanced panel dataset of 400 German districts (NUTS-3 level) over a 19-year period (2003–2021), yielding 7,200 observations.

* **BBSR INKAR Database:** Regional socioeconomic indicators including local GDP per capita, general unemployment rates, foreigner shares, and age demographics (shares of children and youth).
* **Polizeiliche Kriminalstatistik (PKS):** District-level police crime statistics, specifically property damage rates used as a proxy for urban capital density and infrastructure activity.
* **Statistikportal & State Fixed Assets:** State-level asset data disaggregated to NUTS-3 level to estimate regional capital stock per capita.
* **Constructed Metrics:**
  * *Labor Integration Efficacy (LIE):* Measures the local efficiency of absorbing the foreign-born population into the active labor market.
  * *Foreign Labor Supply Pressure (FLSP):* Measures the concentration of foreign labor reserves relative to the regional labor force.
* **R Package Contributions:**
  * **[inkaR](https://github.com/ofurkancoban/inkaR):** A fast, modern, and lightweight R package developed for this project to automate the downloading, cleaning, and spatial mapping of development indicators directly from the official German Federal Office for Building and Regional Planning (BBSR) INKAR database.

### Key Empirical Findings

Using a Two-Way Fixed Effects (TWFE) panel specification, the study establishes several core findings:

1. **The "Integration Dividend":** Prosperity is driven by the *quality* of labor integration rather than the *quantity* of migrants. A $1\%$ increase in Labor Integration Efficacy ($LIE_{it-1}$) leads to a $0.028\%$ increase in regional GDP per capita ($p < 0.01$).
2. **Quantity vs. Quality:** A simple increase in the foreign-born population share without active integration displays a slight negative effect on GDP per capita ($\beta = -0.005, p < 0.01$). Conversely, available labor reserves (Foreign Labor Supply Pressure) function as latent productive capacity and show a positive semi-elasticity ($\beta = 0.011, p < 0.05$).
3. **Urban Density & Crime Proxies:** Property damage rates correlate positively with prosperity, indicating they capture the capital-related frictions associated with urban vibrancy and economic density.
4. **Regional Heterogeneity:** Sub-sample analysis reveals that the positive growth effects of foreign labor integration are concentrated in mature, high-capital-density Western German districts. In Eastern Germany, historical and structural labor market differences temporarily decouple integration success from output growth.

---

### Reproducibility Guide

To ensure this project runs identically on any machine, we use `renv` for dependency management and a `Makefile` for pipeline orchestration.

#### Method 1: The One-Command Way (Recommended)

If you have `make` installed (standard on macOS/Linux), run:

```bash
make all
```

*This will automatically initialize the environment, download all data, run the analysis, and render the paper/presentation.*

#### Method 2: The R-Only Way (Orchestrator)

1. Open `AE_Project.Rproj` in RStudio.
2. Run the master script:

   ```r
   source("02_scripts/01_setup/setup_environment.R")
   ```

*This script sequentially executes all 32 stages of the research pipeline.*

#### Method 3: Granular Execution

You can run specific parts of the pipeline:

- `make init` : Restore R packages via `renv`.
- `make preprocess` : Run data downloading and cleaning (Stages 1-13).
- `make analysis` : Run econometric models and diagnostics (Stages 14-30).
- `make paper` : Render the final PDF/HTML paper.
- `make presentation` : Render the Quarto Reveal.js presentation.

---

### Key Deliverables & Formats

- **Research Paper (PDF)**
- **Research Paper (HTML/Web Page)**
- **Presentation Slides (Reveal.js HTML)**

---

### Project Structure

The project follows a modular "Research Pipeline" design:

```text
.
├── 01_datasets/          # Data storage (Raw, Processed, Final)
├── 02_scripts/           # R Codebase
│   ├── 01_setup/         # Environment & Pipeline Orchestration
│   ├── 02_preprocessing/ # Data loaders, scrapers, and cleaning
│   ├── 03_analysis/      # Econometric models (TWFE, Robustness)
│   └── 04_tools/         # Utility functions and global imports
├── 03_results/           # Generated tables, maps, and figures
├── 04_paper/             # Quarto academic paper source
├── 05_presentation/      # Quarto Reveal.js slides source
├── Makefile              # Automation script
├── renv.lock             # Exact R package versions
└── AE_Project.Rproj      # RStudio Project file
```

---

### System Requirements

- **R (4.0+):** [Download R](https://cran.r-project.org/)
- **Quarto CLI:** Essential for rendering documents. [Download Quarto](https://quarto.org/get-started/).
- **TinyTeX:** For PDF compilation (will be auto-installed if missing).
- **System Libraries:**
  - **macOS:** `brew install gdal geos proj poppler`
  - **Linux:** `sudo apt-get install libgdal-dev libgeos-dev libproj-dev`Key Dependencies

Managed via `renv`. Major packages include:

- `fixest`: High-performance TWFE estimation.
- `modelsummary`: Academic table generation.
- `tidyverse`: Data manipulation.
- `sf`: Spatial data analysis.

---

**Author:** Ömer Furkan Çoban
**Course:** Applied Economics, Uni Oldenburg (WiSe 25/26)
