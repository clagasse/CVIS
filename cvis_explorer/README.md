# Climate Vulnerability Indicator System (CVIS) Explorer

An interactive Shiny application for exploring and comparing climate vulnerability indicators across Pacific salmon Conservation Units (CUs).

## Overview

The CVIS Explorer provides an intuitive interface for analyzing standardized climate vulnerability indicators across multiple salmon species and conservation units. The app enables researchers and managers to:

- **Compare** vulnerability indicators across 1-10 Conservation Units
- **Visualize** data through interactive heatmaps, radar charts, and boxplots
- **Filter** by species, DFO area, conservation status, and RCP scenario
- **Export** data and visualizations for reports

## Features

### Interactive Visualizations
- **Heatmap**: Compare standardized indicator values across multiple CUs
- **Radar Chart**: Multi-dimensional comparison of up to 10 indicators
- **Grouped Bar Charts**: Category-based comparisons with faceted views
- **Context Boxplots**: View selected CUs in the context of all CUs with species averages

### Filtering & Selection
- Filter by species (Chinook, Chum, Coho, Pink, Sockeye)
- Filter by DFO management area
- Filter by WSP conservation status (Red, Amber, Green)
- Filter by RCP climate scenario (RCP 4.5, RCP 8.5)
- Search functionality for finding specific CUs
- Select/deselect individual CUs or all at once

### Indicator Categories
- **Spawning & Rearing**: Freshwater habitat indicators (temperature, flow, threats, favourability)
- **Upstream Migration**: Migration conditions (temperature, discharge, distance)
- **Nearshore Marine**: Ocean entry conditions (SST projections and rates, cumulative impacts)
- **Demographics**: Population status and abundance

## Installation

### Prerequisites

Required R packages:
```r
install.packages(c(
  "shiny",
  "bslib",
  "tidyverse",
  "plotly",
  "DT",
  "scales",
  "here"
))
```

### Data Requirements

The app expects a standardized indicators CSV file in the `processed_data/` directory with the naming pattern:
```
processed_data/YYYY-MM-DD_standardized_indicators.csv
```

The CSV should contain:
- Conservation Unit identifiers (`FULL_CU_IN`, `CU_NAME`, `CVIS_NAME`)
- Species information (`SPECIES_NAME`)
- Management information (`DFO_AREA`, `CUstatus`)
- Climate scenarios (`rcp`, `period`, `period_code`)
- Standardized indicators (columns starting with `std_`)

## Usage

### Running the App

1. Ensure your data file is in the `processed_data/` directory
2. Set your working directory to the project root
3. Run the app:

```r
shiny::runApp("app.R")
```

Or from RStudio, open `app.R` and click "Run App"

### Using the Interface

#### 1. Apply Filters (Optional)
Use the sidebar filters to narrow down your dataset:
- Select one or more species
- Choose DFO management areas
- Filter by conservation status
- Select RCP scenario (default: RCP 4.5)

#### 2. Select Conservation Units
- Browse or search for CUs in the checkbox list
- Select 1-10 CUs for detailed comparison
- Leave all unchecked to view all filtered CUs (for table view only)
- Use "Select All" (max 10) or "Clear All" buttons

#### 3. Choose Indicator Categories
Select which categories to include in your analysis:
- Spawning & Rearing
- Upstream Migration
- Nearshore Marine
- Demographics

#### 4. Explore Visualizations

**Overview Table Tab:**
- Sortable, searchable table of all selected CUs
- Color-coded by conservation status
- All indicators in columns

**Comparison Plots Tab:**
- Choose from 4 visualization types
- Interactive plots with hover details
- Best with 2-10 selected CUs

**Category Breakdown Tab:**
- Faceted bar charts by category
- Side-by-side CU comparisons

#### 5. Export Data
- **Download CSV**: Export selected CUs and indicators
- **Download Plot**: (Requires additional setup)

## Data Structure

### Indicator Naming

The app expects standardized indicator columns with the prefix `std_` and suffix `_mean` for most indicators:

```
std_favchange_mean     - ENM Change in Favourability
std_CT_mean            - Cumulative threats to freshwater habitat
std_tw8rate_mean       - Rate of change in August Temperature
std_tw8proj_mean       - Projected August Temperature
std_lowQpdelta_mean    - Change in August flow
std_highQpdelta_mean   - Change in Nov-Jan flow
std_fwres              - Freshwater residency time
std_migrT_mean         - Temperature during upstream migration
std_migrQ_mean         - Change in discharge during migration
std_migrdist           - Length of upstream migration
std_SSTproj_mean       - Nearshore SST during ocean entry
std_SSTrate_mean       - Rate of change in nearshore SST
std_CImpact_mean       - Cumulative impacts to marine habitat
std_CUstatus           - WSP status
std_CUnmat             - Number of mature individuals
```

### Conservation Status Conversion

The app converts text status values to numeric scores:
- Red: 1.0
- Red/Amber: 0.75
- Amber: 0.5
- Amber/Green: 0.25
- Green: 0.0

## Project Structure

```
project/
├── app.R                          # Main Shiny application
├── processed_data/                # Input data directory
│   └── YYYY-MM-DD_standardized_indicators.csv
├── README.md                      # This file
└── [other R scripts]              # Data processing scripts
```

## Troubleshooting

### App doesn't start
- Check that `processed_data/` directory exists
- Verify CSV file is present and properly named
- Ensure all required packages are installed

### No CUs appear in the sidebar
- Check filter settings - try removing all filters
- Verify your data file has the required columns
- Check R console for error messages

### Plots don't update when selecting CUs
- Ensure you're selecting between 1-10 CUs for plots
- Check that indicator categories are selected
- Try refreshing the browser

### Data shows as "Too many CUs to display"
- Apply more specific filters (species, area, status)
- Or select specific CUs (max 10) from the checkbox list

## Technical Details

### Reactive Architecture
- Data filtering is reactive to all sidebar inputs
- CU selection creates individual reactive dependencies for each checkbox
- All plots and tables update automatically when selections change

### Performance
- The app loads the most recent CSV file on startup
- Data is pre-processed and cached in memory
- Suitable for datasets with up to ~200 CUs and ~20 indicators

### Styling
- Uses `bslib` for modern Bootstrap 5 theming
- "Flatly" bootswatch theme with custom primary color
- Responsive layout adapts to different screen sizes

## Version History

### v1.0 (2024-11-20)
- Initial release
- Interactive CU selection with immediate reactivity
- Four visualization types
- Multi-level filtering system
- CSV export functionality

## Contributing

When adding new indicators:
1. Add indicator metadata to the `tbl_indicators` tribble in `prepare_data_from_csv()`
2. Assign appropriate category (`fwR`, `migr`, `mar`, `dem`, `gen`)
3. Ensure column name in CSV matches the `abbrev` field

## License

[Add your license information here]

## Contact

[Add contact information here]

## Acknowledgments

This application was developed to support climate vulnerability assessment of Pacific salmon conservation units in British Columbia, Canada.