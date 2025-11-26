# Willamette Loading Study - Cougar Dam Analysis

![R](https://img.shields.io/badge/R-276DC3?logo=r&logoColor=white)
![GitHub last commit](https://img.shields.io/github/last-commit/danmcgraw94/williamette-loading)
![Status](https://img.shields.io/badge/status-2025%20Report-blue)

## License and Disclaimer

Scripts developed by the U.S. Army Corps of Engineers as part of official duties. As a work of the United States Government, this project is in the public domain within the United States.

### Disclaimer
Scripts are provided "as is" without warranty of any kind. The U.S. Army Corps of Engineers makes no guarantees regarding the accuracy, completeness, or reliability of this software or the results obtained from its use.

The findings and conclusions in this repository are those of the author(s) and do not necessarily represent the views of the U.S. Army Corps of Engineers or the Department of Defense.

## Project Overview
This repository contains hydrological analysis and flood frequency studies for the Willamette project, with a focus on Cougar Dam. 

## Project Structure

### Root Directory
- **Williamette_Loading.Rproj** - R project file for organized workspace management
- **ReadMe.md** - This file

### Main Directories

#### `data/`
Central data storage organized by source and analysis type.

##### `data/Cougar/`
Primary data specific to Cougar Dam analysis:

- **BestFit/** - Input data for RMC Best-Fit theoretical distribution fitting
  - `ss/` - Systematic and historical records
  - `Sensitivities/` - Sensitivity analysis datasets
  - Paleoflood inputs

- **Crit_Duration/** - Critical duration analysis data
  - Historical storm events (Dec-1964, Jan-1995, Jan-1996, Dec-1999)
  - `HMS_Routing/` - HEC-HMS routing results

- **Inflow_Data/** - Systematic inflow records
  - 3-day inflow volumes

- **Operations/** - Reservoir operations data
  - Guide curves
  - Elevation-storage relationships
  - Flood control levels
  - Rule curves (DSS format)
  - Reservoir statistics

- **Peak_to_Vol/** - Peak flow to volume transformation data
  - Vida gage relationships
  - Cougar reservoir relationships
  - BestFit Paleoflood results

- **Precip_Freq/** - Precipitation frequency analysis
  - `RRFT/` - Reservoir Regulation and Flood Routing Tool results
    - `HMS/` - HEC-HMS model files and runs
    - `Precip_Quantiles/` - Bootstrap quantiles at multiple AEPs (0.1%, 1%, 2%, 10%)
    - `Spreadsheet_Calcs/` - Upper/lower bounds and computed values
    - `RRFT_Results/` - Final RRFT volume-frequency results
  - GEV parameter files

- **RFA/** - Regional Frequency Analysis
  - Sensitivity analysis results and tracking

- **Res_Model/** - Reservoir routing models
  - Stage-storage-discharge relationships

- **Stage_Data/** - Historical stage records
  - `AMS/` - Annual Maximum Series
  - Stage duration analysis

##### `data/NWP/`
Data obtained from Portland District (NWP):

- **Reservoir_Data/** - System-wide Willamette project information
  - Elevation-storage curves
  - Flood control levels
  - Operations manuals
  - Location aliases

- **Stage_POR/** - Period of Record stage data
  - Hourly stage (DSS and CSV formats)
  - Daily stage summaries

- **BiOp/** - Biological Opinion operational scenarios
  - Planned operations for fish

##### `data/Climate/`
Climate normal data from PRISM (1991-2020 normals):
- Monthly and annual precipitation summaries
- Temperature normals

##### `data/GIS/`
Geospatial data:
- **PRISM/** - Climate raster data (precipitation and temperature)
  - Monthly normals (800m resolution)
  - Clipped datasets for Willamette Basin
- **Shapefiles** - Basin boundaries (HUC4, HUC8, HUC10)
- **Terrain** - USGS 30m elevation data
- **Dam Locations** - USACE dam locations

##### `data/X_OLD_All_Sites_Stage_data/`
USGS stage data (archived - no longer actively used)

---

#### `R_scripts/`
Active R scripts for analysis and figure generation:

**Core Analysis Scripts:**
- `CGR_Packages.R` - Package loading and dependencies
- `CGR_Systematic Data Sources.R` - Data import and processing
- `CGR_Seasonality.R` - Flood and stage seasonality analysis
- `CGR_peak_to_vol.R` - Peak flow to volume transformations
- `CGR_Crit_Duration.R` - Critical duration analysis
- `CGR_Stage_Duration.R` - Stage duration curve analysis
- `CGR_VFA_Input_Plots.R` - Volume frequency analysis visualizations

**Operations & Infrastructure:**
- `CGR_Guide_Curve.R` - Operational guide curve plotting
- `CGR_Elev_Stor.R` - Elevation-storage relationships
- `CGR_Res_Model.R` - Reservoir routing model visualization

**Precipitation-Frequency:**
- `CGR_PrecipFreq.R` - Precipitation frequency analysis
- `RRFT_Results.Rmd` - RRFT results documentation (R Markdown)

**System-Wide Analysis:**
- `Stage_data_POR_and_BiOps.R` - POR stage processing and BiOp analysis
- `Willamette_Climate_Normals.R` - Climate normal data processing
- `Willamette_Climate_Norms - Precip.R` - Precipitation normals, basin-wide
- `Willamette_Climate_Norms - Temps.R` - Temperature normals, basin-wide

---

#### `archive_scripts/`
Legacy scripts from initial development (Summer 2025). These have been superseded by updated versions in `R_scripts/` with improved functionality, figure quality, and code clarity.

**Archived Files:**
- Original analysis scripts (CGR_*.R)
- `JMD_Examples/` - Reference examples from John Martin Dam
  - Stage duration, seasonality, critical duration
  - HMS routing examples
  - Peak flow regression
  - RFA and BestFit sensitivity analyses

---

#### `outputs/`
Generated outputs from analyses:

##### `outputs/Figures/`
Publication-ready figures organized by analysis type:

- **Critical_Duration/** - Critical duration overlays, facets, and 3-day fraction plots
- **Operations/** - Guide curves and storage-elevation plots
- **Peak_to_Vol/** - Peak to volume relationships (Vida, Cougar, combined)
- **PF/** - Precipitation-frequency curves and RRFT results
- **ResModel/** - Stage-storage-discharge relationships and debris reduction scenarios
- **Seasonality/** - Inflow and stage seasonality plots
- **Stage_Duration/** - Stage duration curves
- **Stage_Frequency/** - Stage frequency analysis (future)
- **Stage_POR/** - Period of record stage visualizations (hourly, daily, truncated)
- **Systematic Inflow/** - AMS time series, ECDF, and combined plots
- **Volume_Frequency/** - Paleo timelines, empirical AEP plots, time series
- **Climate_Normal/** - Precipitation boxplots and ribbon plots

##### `outputs/Stage_POR/`
- Daily stage POR (full and truncated records)

##### `outputs/Climate_Norms/`
- Reserved for climate normal outputs

---

## Key Analyses

### Flood Frequency Analysis
- Integration of paleo records with systematic data
- Volume-frequency relationships

### Precipitation-Frequency
- GEV parameter bootstrapping from Max Stable Process data
- RRFT (Reservoir Regulation and Flood Routing Tool) analysis
- Bootstrap realizations at four AEP values (0.1%, 1%, 2%, 10%)
- Transformation of precipitation depths to 3-day inflow volumes
- Quantile priors for theoretical distribution fitting

### Reservoir Operations
- Guide curve visualization with operational zones
- Stage-storage-discharge rating curves
- Critical duration analysis for multiple historical events
- BiOp operational scenario evaluation

### Data Processing
- Climate normal extraction and basin averaging

---

## Technical Details

### Software Requirements
- R (version 4.0+)
- RStudio (recommended)
- Key R packages:
  - `tidyverse` - Data manipulation and visualization
  - `ggplot2` - Publication-quality graphics
  - `patchwork` - Multi-panel plot composition
  - `purrr` - Functional programming for data processing
  - `fs` - File system operations
  - `here` - Path management

### Data Formats
- CSV - Tabular data
- DSS (HEC-DSS) - Time series data from HEC-HMS/ResSim
- Excel (.xlsx) - Summary tables and metadata
- GeoTIFF - Raster climate data
- Shapefile - Vector GIS data

### Coordinate Systems
- Elevation datums: NAVD29, NAVD88
- Projection: (specify as needed for GIS data)

---

## Usage

1. Open `Williamette_Loading.Rproj` in RStudio
2. Run `CGR_Packages.R` to load required packages
3. Execute desired analysis scripts in `R_scripts/` directory
4. Generated figures save automatically to `outputs/Figures/`

---

## Notes

- Legacy code has been reorganized and improved for maintainability
- Project uses R Projects for reproducible workflow management

---

## Contact

Dan McGraw
daniel.e.mcgraw@usace.army.mil

---

*Last Updated: November 2025*

## R-project Tree
Note recursive branches are shown selectively:

Williamette_Loading/

├── data/

│   ├── Cougar/                      # Cougar Dam-specific data

│   │   ├── BestFit/                 # RMC Best-Fit input data

│   │   ├── Crit_Duration/           # Critical duration storm events

│   │   ├── Inflow_Data/             # Systematic inflow records

│   │   ├── Operations/              # Guide curves, flood levels, reservoir stats

│   │   ├── Peak_to_Vol/             # Peak flow to volume transformations

│   │   ├── Precip_Freq/             # Precipitation-frequency analysis

│   │   │   └── RRFT/                # RRFT results and HMS models

│   │   │       ├── HMS/             # HEC-HMS model files

│   │   │       ├── Precip_Quantiles/  # Bootstrap quantiles (0.1%-10% AEP)

│   │   │       ├── RRFT_Results/    # Final volume-frequency results

│   │   │       └── Spreadsheet_Calcs/  # Upper/lower bounds

│   │   ├── RFA/                     # Regional Frequency Analysis

│   │   ├── Res_Model/               # Reservoir routing models

│   │   └── Stage_Data/              # Historical stage records and AMS

│   │

│   ├── NWP/                         # Portland District data

│   │   ├── BiOp/                    # Biological Opinion operations

│   │   ├── Reservoir_Data/          # System-wide Willamette operations info

│   │   └── Stage_POR/               # Period of record stage (hourly/daily)

│   │

│   ├── Climate/                     # PRISM climate normals (1991-2020)

│   │   └── Normals/

│   │

│   └── GIS/                         # Geospatial data

│       ├── PRISM/                   # Climate rasters (precipitation, temperature)

│       └── *.shp                    # Basin boundaries and dam locations

│

├── R_scripts/                       # Active analysis scripts

│   ├── CGR_Packages.R               # Package dependencies

│   ├── CGR_Systematic Data Sources.R  # Data import

│   ├── CGR_peak_to_vol.R            # Peak-volume transformations

│   ├── CGR_Crit_Duration.R          # Critical duration analysis

│   ├── CGR_Seasonality.R            # Flood seasonality

│   ├── CGR_Stage_Duration.R         # Stage duration curves

│   ├── CGR_VFA_Input_Plots.R        # Volume frequency visualizations

│   ├── CGR_PrecipFreq.R             # Precipitation-frequency analysis

│   ├── CGR_Guide_Curve.R            # Operational guide curves

│   ├── CGR_Elev_Stor.R              # Elevation-storage relationships

│   ├── CGR_Res_Model.R              # Reservoir routing models

│   ├── RRFT_Results.Rmd             # RRFT documentation (R Markdown)

│   ├── Stage_data_POR_and_BiOps.R   # POR stage processing

│   └── Willamette_Climate_Normals.R # Climate normal processing

│

├── archive_scripts/                 # Legacy scripts (Summer 2023)

│   └── JMD_Examples/                # Reference examples from previous analyses

│

├── outputs/

│   ├── Figures/                     # Publication-ready figures

│   │   ├── Critical_Duration/

│   │   ├── Operations/

│   │   ├── Peak_to_Vol/

│   │   ├── PF/                      # Precipitation-frequency

│   │   ├── ResModel/

│   │   ├── Seasonality/

│   │   ├── Stage_Duration/

│   │   ├── Stage_POR/

│   │   ├── Systematic Inflow/

│   │   └── Volume_Frequency/

│   │

│   └── Stage_POR/                   # Processed stage data (CSV)

│

├── Williamette_Loading.Rproj        # R Project file

└── ReadMe.md                        # Project documentation
