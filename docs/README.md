# Climate Vulnerability Indicators for Salmon (CVIS)

A data-informed, indicator-based framework for evaluating climate change vulnerability of Pacific salmon conservation units (CUs) in the Fraser River basin, British Columbia.

## Overview

This project presents an analytical framework that integrates climate projections, habitat assessments, demographic, and genetics data to characterize relative climate vulnerability for Pacific salmon conservation units. The framework evaluates \>15 indicators across 5 categories representing different life stages and mechanisms of climate change vulnerability:

-   **Freshwater Spawning & Rearing** - Environmental changes to spawning and rearing streams
-   **Freshwater Upstream Migration** - Changes affecting adult migration to spawning sites
-   **Marine Nearshore** - Sea surface temperature changes during ocean entry
-   **Demographic Factors** - Population status and abundance metrics
-   **Genetic Factors** - Genetic diversity and offset

The analysis currently covers 50 conservation units throughout the Fraser River watershed, with results for mid-century (2040-2060) and end-of-century (2080-2100) time periods under RCP 4.5 and RCP 8.5 emissions scenarios.

## Technical Report

A full description of methods and results are detailed in a technical report currently in development. Please contact the authors for further information. 

## Project Structure

```         
├── code/
│   ├── 0_setup.R                    # Project configuration and paths
│   ├── 0a_console.R                 # Workflow demonstration script
│   ├── 1a_CU_import.R               # Conservation unit data import
│   ├── 1b_FW_stream_process.R       # Freshwater stream data processing
│   ├── 1c_FW_PCIC_period_average.R  # Climate model period averaging
│   ├── 1d_FW_PCIC_model_averages.R  # Climate model ensemble means
│   ├── 2a_FW_boundary_subset.R      # Spatial subsetting by CU boundaries
│   ├── 2b_FW_rearing_stats.R        # Spawning/rearing indicator statistics
│   ├── 2c_FW_upstream_paths.R       # Migration pathway analysis
│   ├── 2d_FW_migration_stats.R      # Migration indicator statistics
│   ├── 3a_marine_data_import.R      # Marine data import
│   ├── 3b_marine_summarize.R        # Marine data summarization
│   ├── 3c_marine_stats.R            # Marine indicator statistics
│   ├── 3d_marine_grid_standardize.R # Marine grid standardization
│   ├── 4a_CU_scoring.R              # Vulnerability indicator scoring
│   ├── 5a_plots_CU.R                # CU-specific plotting functions
│   ├── 5b_plots_compare.R           # Comparative plotting functions
│   ├── 6a_CU_indicator_report.Rmd   # CU-specific report template
│   └── 6b_CVIS_overview.Rmd         # Project overview report template
├── processed_data/
│   ├── freshwater/                  # Processed freshwater indicators
│   └── marine/                      # Processed marine indicators
├── output/
│   └── figures/                     # Generated figures and plots
└── reports/                         # Generated HTML/PDF reports
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

### Basic Workflow

The analysis workflow is demonstrated in `code/0a_console.R`. The typical sequence is:

1.  **Setup and configuration** (`0_setup.R`)

2.  **Data import and processing** (scripts 1a-1d)

3.  **Spatial analysis** (scripts 2a-2d for freshwater, 3a-3e for marine)

4.  **Indicator scoring** (scripts 4a-4d)

5.  **Visualization and reporting** (scripts 5a-6b)

## Output

The framework produces:

-   **Standardized indicator values** (0-1 scale) for each CU
-   **Combined vulnerability scores** using multiple aggregation methods
-   **Spatial visualizations** of stream-level indicators
-   **Comparative plots** across CUs, species, and scenarios
-   **HTML/PDF reports** for individual CUs and watershed-wide summaries

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
