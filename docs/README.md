# Climate Vulnerability Indicators for Salmon (CVIS)

A data-informed, indicator-based framework for evaluating climate change vulnerability of Pacific salmon conservation units (CUs) in the Fraser River basin, British Columbia.

## Overview

This project presents an analytical framework that integrates climate projections, habitat assessments, demographic, and genetics data to characterize relative climate vulnerability for Pacific salmon conservation units. The framework evaluates 17 indicators across 5 categories representing different life stages and mechanisms of climate change vulnerability:

*   **Freshwater Spawning & Rearing**
    *   *Change in ENM favourability* (`favchange`): Relative changes in ecological niche model habitat suitability.
    *   *Standardized cumulative threats* (`cthr`): Integrated land-use and human footprint impact scores.
    *   *August Stream Temperature* (`tw8proj` & `tw8rate`): Projected stream temperatures and decadal rates of change.
    *   *August Stream Flow Change* (`flow8pdelta`): Projected dry-season proportional flow change.
    *   *Winter Stream Flow Change* (`flow18pdelta`): Projected wet-season proportional flow change (Nov-Jan).
    *   *Freshwater residency time* (`fwres`): Species-specific rearing residence duration.
*   **Freshwater Upstream Migration**
    *   *Migration corridor temperature* (`migrTproj`): River temperatures along the migration corridor during migration timing.
    *   *Migration season discharge* (`migrQpdelta`): Proportional change in discharge during migration.
    *   *Length of migration path* (`migrdist`): Absolute distance from ocean entry to NuSEDS spawning sites.
*   **Marine Nearshore**
    *   *Projected SST* (`SSTproj`): Sea surface temperatures during the critical nearshore ocean entry window.
    *   *Rate of change in SST* (`SSTrate`): Decadal SST warming rate.
    *   *Cumulative marine habitat impacts* (`CImpact`): Localized marine stressors and threats.
*   **Demographic Factors**
    *   *Wild Salmon Policy status* (`CUstatus`): Current integrated WSP status assessment.
    *   *Mature spawner abundance* (`CUnmat`): Baseline geometric mean number of mature spawner individuals.
*   **Genetic Factors**
    *   *Genetic heterozygosity* (`hetzyg`): Population genomic diversity levels.
    *   *Genomic offset* (`genoff`): Predicted genetic vulnerability based on genomic-climate mismatch.

The analysis currently covers 50 conservation units throughout the Fraser River watershed, with results for mid-century (2040-2060) and end-of-century (2080-2100) time periods under RCP 4.5 and RCP 8.5 emissions scenarios.

## Technical Report

A full description of methods and results are detailed in a technical report currently in development. Please contact the authors for further information. 

## Project Structure

```
├── code/
│   ├── 0_setup.R                    # Paths registration, configs, and library setup
│   ├── 0a_console.R                 # Workflow runner and console interface
│   │
│   ├── 1a_CU_import.R               # CU crosswalk decoders and spawner site imports
│   ├── 1b_FW_stream_process.R       # Raw stream networks & PCIC spatial processing
│   ├── 1c_FW_PCIC_period_average.R  # PCIC NetCDF period averaging calculations
│   ├── 1d_FW_PCIC_model_averages.R  # Multi-model GCM ensemble averaging
│   ├── 1f_genetics_import.R         # Heterozygosity and genomic offset imports
│   ├── 1x_FW_create_PCIC_grid.R     # PCIC climate grid spatial polygon builder
│   ├── 1z_FWA_query.R               # BC Freshwater Atlas stream database queries
│   │
│   ├── 2_fw_utils.R                 # Utility helpers for freshwater & FWA tracing
│   ├── 2a_FW_boundary_subset.R      # Subsetting stream networks by CU boundary
│   ├── 2b_FW_rearing_stats.R        # Freshwater spawning/rearing indicator metrics
│   ├── 2c_FW_upstream_paths.R       # Tracing upstream migration routes from ocean entry
│   ├── 2d_FW_migration_stats.R      # Migration pathway thermal and flow indicators
│   │
│   ├── 3_marine_utils.R             # Utility helpers for marine SST and grids
│   ├── 3a_marine_data_import.R      # SST and salinity projections spatial import
│   ├── 3b_marine_summarize.R        # SST/SSS Marine Analysis Zone (MAZ) averaging
│   ├── 3c_marine_stats.R            # Nearshore marine indicator metrics
│   ├── 3d_marine_grid_standardize.R # SST grid interpolation and standardization
│   ├── 3e_validating_standardized_grid.R # Spatial interpolation validation checks
│   ├── 3z_compare_GStr_SSTs.R       # Salish Sea SST model comparisons & Z-scores
│   │
│   ├── 4_scoring_utils.R            # Utility helpers for indicator scaling and scoring
│   ├── 4a_CU_scoring.R              # 0-1 normalization and portfolio aggregations
│   ├── 4b_sensitivity_analysis.R    # Quantitative analysis of vulnerability score sensitivity, rank displacements, and jackknife leverage analysis
│   ├── 4c_sensitivity_summary.R     # Consolidating sensitivity metrics, risk drivers, and ANOVA
│   ├── 4d_bootstrap_analysis.R      # Bootstrap uncertainty analysis and robustness classification
│   │
│   ├── 5a_plots_CU.R                # Individual CU lollipop and map plots
│   ├── 5b_plots_compare.R           # Comparative multi-CU indicator plots
│   ├── 5c_plots_sensitivity_indicators.R # Plotting indicator sensitivity ranges
│   ├── 5d_table_summaries.R         # Generates publication-grade HTML summary and sensitivity tables (via gt)
│   ├── 5e_plots_study_area.R        # Generates Fraser Basin study area maps and species facets
│   │
│   ├── 6_CVIS_report.Rmd            # Master CVIS report template
│   ├── 6_figures_manuscript.R       # Generating and exporting manuscript figures
│   ├── 6a_S1_indicators_description.Rmd # Supplement S1 report (Indicator descriptions and baseline outputs)
│   ├── Supplement_S2_data.Rmd       # Supplement S2 report (Input datasets and models)
│   ├── 6b_S2_CU_reports.Rmd         # Supplement S3 report template (Individual CU profiles)
│   ├── 6d_generate_dashboard.R      # Self-contained master HTML dashboard builder
│   ├── deploy_reports.R             # Master script to compile reports and deploy assets to /docs for GitHub Pages
│   └── precompute_cu_lakes.R        # Precomputes lake intersection percentages for Salmon Conservation Units to speed up spatial indicators mapping
│
├── processed_data/
│   ├── CU/                          # Aggregated CU metadata and genetics data
│   ├── freshwater/                  # Processed freshwater indicators
│   ├── marine/                      # Processed marine indicators
│   └── params/                      # Analysis configs and indicators metadata tables
│
├── output/
│   ├── figures/                     # Generated figures and plots (e.g. manuscript)
│   ├── reports/                     # Generated HTML/PDF reports (nested by run)
│   ├── scoring_results.Rdata        # Final indicator scores database
│   └── sensitivity_analysis.Rdata   # Sensitivity database
```

## Data Requirements

This project requires external climate, spatial, and biological data that are not included in the repository due to size and access constraints. Required datasets include:

### Climate Data

-   **PCIC VIC-GL Model**: Stream temperature and flow projections (1980-2099)
-   **Thermalscapes**: August stream temperature projections for BC
-   **Environmental Niche Models**: Habitat suitability projections
-   **SalishSeaCast**: Marine oceanographic model outputs
-   **HOTSSea**: Historical Salish Sea hindcast

### Spatial Data

-   **BC Freshwater Atlas**: Provincial stream network
-   **BC Fishpass**: Accessibility and habitat models
-   **CU Boundaries**: Pacific salmon conservation unit boundaries
-   **Marine Adaptive Zones**: Ocean zone boundaries

### Biological Data

-   **PSF Life History Timing**: CU-specific phenology data
-   **NUSEDS**: Spawning escapement database
-   **WSP Status Assessments**: Population status classifications

## Usage

### General Workflow

The CVIS analysis pipeline can be executed in modular stages using `code/0a_console.R` as the master script runner. This runner coordinates the execution sequence:

1.  **Setup & Initialization** (`code/0_setup.R`): Configures global analysis options (such as target CUs, emission scenarios, and models), registers centralized directories, and loads dependencies. Sibling raw data folders (`0_data_climate`, `0_data_spatial`, `0_data_salmon`) resolve outside the repository to remain Git-clean.
2.  **Freshwater Indicator Analysis** (scripts 1a-2d): Imports CU metadata and traces spawner pathways. Calculates dry-season temperatures, winter flow anomalies, and thermal migration corridor stressors.
3.  **Marine Nearshore Analysis** (scripts 3a-3d): Standardizes ocean sea surface temperature (SST) and salinity datasets to extract entry-window conditions.
4.  **Vulnerability Scoring, Sensitivity & Uncertainty Analysis** (scripts 4a-4d): Normalizes all raw indicators to a standard 0-1 scale. Aggregates scores across lifecycle categories using multiple methods (arithmetic means, extreme-value scaling, and red flags). Performs sensitivity analysis (ANOVA/PCA) and bootstrapping to evaluate score stability and robustness.
5.  **Visualization, Summaries & Dashboard Stitching** (scripts 5a-5e and 6a-6d): Renders lollipop and map plots, compiles publication-grade HTML summary and sensitivity tables (via `gt`), and renders individual CU HTML profiles.
6.  **Report Deployment & Exploration**: Runs `code/deploy_reports.R` to compile and deploy the self-contained reports and master dashboard to the `/docs` folder, facilitating static hosting and easy browsing of the results (e.g., via GitHub Pages).

### Core Dependencies

The framework leverages several categories of R packages:
-   **Spatial Analysis**: `sf`, `terra`, `fwapgr` (Freshwater Atlas integration), `gstat`
-   **Data Processing**: `dplyr`, `tidyr`, `purrr`, `data.table`, `stars` (NetCDF raster manipulation)
-   **Visualizations & Outputs**: `ggplot2`, `gt`, `rmarkdown`

## Output

The framework produces:

-   **Standardized indicator values** (0-1 scale) for each CU
-   **Combined vulnerability scores** using multiple aggregation methods (averages, extreme-value scaling, red flag counts)
-   **Spatial visualizations** of stream-level indicators
-   **Comparative plots** across CUs, species, and scenarios
-   **HTML reports** for individual CUs and watershed-wide summaries (placed in `output/reports/`)
-   **Self-contained HTML Dashboard** (`docs/6b_S2_CU_reports.html`) for searching, filtering, and interactive browsing of individual Conservation Unit profiles on GitHub Pages.

## Contributing

This is a research project with preliminary results. Contributions and feedback are welcome. Please contact the authors before redistributing results or code.

### Authors

-   Cory Lagasse
-   Josie Iacarella
-   Colin Bailey
-   Michael Arbeider
-   Timothy Healy

Fisheries and Oceans Canada

## Citation

If you use this framework or code, please cite:

[Citation information to be added upon publication]

## Acknowledgments

This work builds upon climate projections and models developed by:

-   Pacific Climate Impacts Consortium (PCIC)

-   Pacific Salmon Foundation (PSF)

-   BC Fishpass

-   DFO Freshwater Spatial Ecology Program

-   ECCC high resolution SST projections

See individual data sources for specific attribution requirements.

## Contact

For questions or collaboration inquiries, please contact:

Cory Lagasse - Fisheries and Oceans Canada

------------------------------------------------------------------------

**Note**: Results presented in this framework are preliminary and subject to change as methods are refined. Please do not distribute without authors' permission.
