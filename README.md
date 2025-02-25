# Replication Package for The Role of Fear in Harm Reduction: Misinformation’s Impact on Naloxone Use

This replication package contains all files necessary to replicate the analyses, tables, and figures presented in the paper, "The Role of Fear in Harm Reduction: Misinformation’s Impact on Naloxone Use". The package is organized into two subdirectories: `original_data` and `scripts`.

## Directory Structure

1. **`original_data/`**
   - `final_ny_data.csv`: A CSV file containing all publicly available data used in the analysis, compiled into a single document.
   - `standing_order.csv`: A CSV file that reflects the count of naloxone standing order-participating pharmacies by county and year. This dataset was hand-coded from archived New York State Department of Health (NYSDOH) webpages.

2. **`scripts/`**
   - `tables_generation.R`: An R script containing all code necessary to:
     - Read in the data from `original_data/`
     - Perform the paper's analysis
     - Generate and format all tables presented in the paper
   - `plot_generation.R`: An R script containing all code necessary to:
     - Read in the data from `original_data/`
     - Perform the analysis required to generate figures
     - Produce all plots and figures included in the paper

## Instructions for Replication

1. **Prerequisites:**
   - R (version [specify version or ">= 4.3.3"]) should be installed on your system.
   - Ensure that the following R packages are installed:
- `tigris`
- `sf`
- `dplyr`
- `ggplot2`
- `readr`
- `patchwork`
- `fixest`
- `stringr`
- `viridis`
- `scales`
- `RWmisc`
- `stargazer`

These packages can be installed using the `install.packages()` function in R. For example:

```R
install.packages(c("tigris", "sf", "dplyr", "ggplot2", "readr", 
                   "patchwork", "fixest", "stringr", "viridis", 
                   "scales", "RWmisc", "stargazer"))

2. **Steps:**
   - Place the `original_data/` and `scripts/` directories in the same parent directory.
   - Open `tables_generation.R` in RStudio or your preferred R environment and run the script to replicate all tables.
   - Open `plot_generation.R` in RStudio or your preferred R environment and run the script to replicate all plots and figures.

3. **Expected Outputs:**
   - Tables and plots will be generated and saved in the respective directories or displayed in the R environment, as specified in the scripts.

## Notes
- The `final_ny_data.csv` file includes only publicly available data sources. Detailed descriptions and sources for this dataset are documented in the paper's supplementary materials.
- The `standing_order.csv` file was manually created and includes hand-coded data from NYSDOH archived webpages. This dataset may require additional clarification if discrepancies arise.
