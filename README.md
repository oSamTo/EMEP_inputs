# EMEP Model Input File Generator

## Overview

This repository automates the creation of emission input files for the **EMEP atmospheric chemistry-transport model**. It processes emissions data from multiple sources (NAEI, EMEP/CEIP, HTAPv3, EDGAR) and generates NetCDF input files for three spatial domains:

- **UK & Ireland (UKEIRE)**: 0.01° resolution
- **Europe (EU)**: 0.1° resolution  
- **Global**: 0.1° resolution

**Supported EMEP model versions**: v4.45, v5.0

---

## Project Purpose

The EMEP model requires gridded emissions data in standardized NetCDF format. This project:

1. **Sources emissions data** from various national and international inventories
2. **Processes and harmonizes** data into common GNFR/SNAP sectors
3. **Grids emissions** to specified spatial resolutions
4. **Generates NetCDF files** compatible with EMEP model input requirements
5. **Provides QA/QC tools** for validation and comparison of outputs

---

## Data Sources and Spatial Coverage

### UK & Ireland (UKEIRE domain)
- **Pollutants**: NOx, NH3, SO2, PM2.5, PMCO, CO, VOC
- **Data source**: NAEI (UK) + MapEire (Ireland) + EMEP (supplementary)
- **Processing**: Handled by [Inventory Processor](https://github.com/oSamTo/inventory_processor) (stored on JASMIN)
- **Sectors**: SNAP categories (mapped to GNFR for EU reporting)
- **Resolution**: 1km (original), 0.01° (resampled for EMEP)
- **Years available**: 2015-2018 (currently configured)

### EU Domain
- **Data source**: HTAPv3 (CAMS) emissions as submitted to EMEP/CEIP
- **Pollutants**: Same as UK domain
- **Sectors**: GNFR (General Nomenclature For Reporting)
- **Resolution**: 0.1°
- **Coverage**: 30°W–90°E, 30°N–82°N
- **Masking**: UK terrestrial cells removed to avoid double-counting

### Global Domain (under development)
- **Data source**: HTAPv3.2 emissions (country-level data mapped to GNFR sectors)
- **Future expansion**: EDGAR inventory integration planned

---

## Repository Structure

```
├── run.R                           # Main execution script
├── README.md                       # This file
│
├── R/                              # Core R functions and setup
│   ├── run_setup.R                 # Configuration file (EDIT THIS FOR RUNS)
│   ├── workspace.R                 # Global objects, packages, domain definitions
│   ├── array_size.R                # Array job size calculation
│   ├── process_ISO_grids.R         # ISO grid processing functions
│   ├── QAQC_html.Rmd               # QA/QC reporting template
│   ├── qaqc_codex_v2/              # QA/QC v2 workflow
│   │   ├── data.R                  # Data loading for QA/QC
│   │   ├── plots.R                 # QA/QC plotting functions
│   │   ├── render.R                # Report rendering
│   │   └── utils.R                 # Utility functions
│   ├── v4.45/                      # EMEP v4.45 specific functions
│   │   ├── UK_v4.45_functions.R
│   │   └── EU_v4.45_functions.R
│   └── v5.0/                       # EMEP v5.0 specific functions
│       ├── UK_v5.0_functions.R     # Main UK processing pipeline
│       ├── EU_v5.0_functions.R
│       └── GLOBAL_v5.0_functions.R
│
├── comparisons/                    # Comparison and validation tools
│   ├── run_compare.R               # Main comparison script
│   ├── compare_functions.R         # General comparison functions
│   ├── compare_functions_EU.R      # EU-specific comparisons
│   ├── compare_functions_GLOBAL.R  # Global comparisons
│   ├── compare_markdown.Rmd        # Comparison report template
│   └── EU/, GLOBAL/, UKEIRE/       # Example comparison outputs
│
├── data/                           # Input data and lookup tables
│   ├── alt_emis/                   # Alternative emissions sources
│   │   ├── EDGAR/
│   │   ├── EMEP4UKv5.0_Mar25/
│   │   ├── EPA_4.36/
│   │   ├── HCL/, HTAP/, NFC/, NFCv2/, SCOTDOM/
│   ├── femis/                      # Temporal factors
│   │   ├── DailyFac.nox
│   │   ├── HourlyFacs.INERIS
│   │   └── MonthlyFac.nox
│   ├── gridded/                    # Pre-gridded emissions (if available)
│   ├── lookups/                    # Reference tables
│   │   ├── EMEP_sectors.csv        # Sector definitions
│   │   ├── SNAP_to_GNFR.csv        # Sector mapping
│   │   ├── pollutant_names.csv     # Pollutant standardization
│   │   └── dt_iso_*.csv, *.rds     # Country/ISO reference data
│   ├── spatial/                    # Spatial boundary files and shapefiles
│   │   ├── UK/, Eire/, EU/, world/ # Domain-specific boundaries
│   │   └── EDGAR_iso_grids/, HTAP_iso_grids/
│   └── temporal/                   # Temporal profiles by EMEP version
│       ├── EMEP4UKv4.34/
│       ├── EMEP4UKv4.36/
│       ├── EMEP4UKv4.45/
│       └── EMEP4UKv5.0/
│
├── outputs/                        # Generated NetCDF files and outputs
│   ├── EMEP4UKv5.0_Mar25/
│   ├── EPA_4.36/
│   ├── HCL/, HTAP/, NFC/, NFCv2/, SCOTDOM/
│   │   └── BASE/ or SGS*/ folders  # Scenario-based outputs
│   └── archive/                    # Previous run archives
│
├── slurm/                          # HPC job submission scripts
│   ├── emep.array                  # Main SLURM array job script
│   ├── comparison.array            # Comparison job script
│   ├── init.job                    # Initialization job
│   └── output/                     # Slurm logs and output
│
├── targets_dump/                   # Targets workflow configuration (experimental)
│   ├── targets_EMEP.R
│   └── PARAMETERS.R
│
├── dump/                           # Diagnostic and temporary files
├── docs/                           # Documentation
└── latex_logs/                     # LaTeX compilation outputs
```

---

## Workflow Overview

### Main Processing Pipeline

```
┌─────────────────────────────────────┐
│  1. CONFIGURE RUN (R/run_setup.R)   │
│  - Select domain (UKEIRE/EU/GLOBAL) │
│  - Choose data source (NAEI/EMEP)   │
│  - Select emissions years           │
│  - Choose EMEP version (v4.45/v5.0) │
│  - Select pollutants                │
│  - Set temporal resolution (annual) │
│  - Define scenarios (BASE/SGS)      │
└────────────┬────────────────────────┘
             │
             ▼
┌─────────────────────────────────────┐
│  2. EXECUTE MAIN RUN (run.R)        │
│  - Load processed inventory data    │
│  - Grid to target resolution        │
│  - Apply temporal factors           │
│  - Create NetCDF files              │
│  - Generate QA/QC reports           │
└────────────┬────────────────────────┘
             │
             ▼
┌─────────────────────────────────────┐
│  3. QUALITY ASSURANCE               │
│  - Verify totals and distributions  │
│  - Check for spatial/temporal gaps  │
│  - Validate against expected ranges │
│  - Generate HTML QA/QC reports      │
└────────────┬────────────────────────┘
             │
             ▼
┌─────────────────────────────────────┐
│  4. OPTIONAL COMPARISON             │
│  (comparisons/run_compare.R)        │
│  - Compare two sets of inputs       │
│  - Relative & absolute differences  │
│  - Identify changes                 │
└─────────────────────────────────────┘
```

---

## Getting Started

### Step 1: Configure Your Run

Edit **`R/run_setup.R`** to specify:

```r
# Domain
run_domain <<- "UKEIRE"  # or "EU", "GLOBAL"
run_source <<- "NAEI"    # or "EMEP", "HTAP", "EDGAR"

# EMEP version
emep_version <<- "v5.0"  # or "v4.45"

# Data years
v_years <- c(2015:2018)  # emissions years to process
v_pollutants <- c("nox", "nh3", "sox", "pm25", "pmco", "co", "voc")

# Output configuration
output_project <- "NFCv2"  # scenario project name
v_scenarios <- "BASE"      # "BASE" or "SGS1", "SGS2", etc.

# Inventory versions
naei_inv <- 2025           # NAEI inventory compilation year
map_yr_uk <- 2023          # UK spatial distribution year
map_yr_ie <- 2019          # Ireland spatial distribution year
```

### Step 2: Execute the Run

**Option A - Interactive (local/small runs):**
```bash
Rscript run.R 1
```
(The `1` is the array index; for sequential runs, increment this)

**Option B - HPC/SLURM (large/multiple runs):**

Create an array job from your run configuration:
```bash
# Estimate number of array jobs needed
Rscript R/array_size.R

# Submit array job (replace N with max array size)
sbatch --array=1-N slurm/emep.array
```

### Step 3: Check QA/QC

After a successful run, QA/QC HTML reports are automatically generated in the output directory for each pollutant. Open these to verify:
- Emission totals vs. expected values
- Spatial distribution reasonableness
- Temporal profiles (if applicable)
- No data gaps or artifacts

### Step 4: Compare Results (Optional)

To compare two different sets of outputs:

1. Edit **`comparisons/run_compare.R`** with folder paths of runs to compare
2. Execute comparison script:
   ```bash
   Rscript comparisons/run_compare.R 1
   ```
   (Or via SLURM: `sbatch slurm/comparison.array`)

---

## Key Configuration Options

### Temporal Resolution
- `time_dim = "annual"` - Annual totals (most common)
- `time_dim = "month"` - Monthly profiles (if available)
- `time_dim = "yday"` - Daily profiles (under development)

### Temporal Schemes
- `tp_scheme = "TPannual"` - Time-period annual (default)
- Custom schemes available for specific EMEP configurations

### Scenarios
- `v_scenarios = "BASE"` - Standard emissions inventory
- `v_scenarios = paste0("SGS", 1:6)` - Sensitivity/scenario simulations
- Alternate emissions defined in `data/alt_emis/alternate_emissions.csv`

### Spatial Domains & Resolutions
| Domain | Resolution | CRS | Extent | Use Case |
|--------|-----------|-----|--------|----------|
| UKEIRE | 0.01° | EPSG:4326 | 13.8°W–4.6°E, 49–61.5°N | UK & Ireland detailed |
| EU | 0.1° | EPSG:4326 | 30°W–90°E, 30–82°N | European regional |
| GLOBAL | 0.1° | EPSG:4326 | Global | Global studies |

---

## Important Notes

### Data Dependencies
- **UK/Ireland data** is pre-processed by the [Inventory Processor](https://github.com/oSamTo/inventory_processor) and stored at:
  ```
  /gws/nopw/j04/ceh_generic/inventory_processor
  ```
- Ensure the correct inventory year folder exists in the expected location

### Masking and Double-Counting
- UK terrestrial cells are removed from EU outputs to prevent double-counting
- This is enforced via spatial masking during processing

### Alternate Emissions
- For scenario runs, alternate emission sources must be defined in:
  ```
  data/alt_emis/alternate_emissions.csv
  ```
- Column structure: `year, source, scenario, ...` (see examples in the file)

### Output Structure
All outputs follow this naming convention:
```
outputs/{project}/{scenario}/{domain}/{source}/{inv_year}/
        EMEP{version}/{time_dim}/{temporal_scheme}/{iso_code}
```

---

## Output Files

Each run generates:

1. **NetCDF Files** (`*.nc`)
   - One file per ISO country code (for compatibility with EMEP model)
   - Variables: Emissions by sector and temporal dimension
   - Metadata: Source, version, creation date

2. **QA/QC Reports** (HTML)
   - Emission totals by sector and country
   - Spatial distribution maps
   - Temporal profiles (if applicable)
   - Comparison to source data

3. **Log Files** (in `slurm/output/`)
   - R console output for debugging

---

## Troubleshooting

### Error: "No such file or directory" for inventory data
- Check `naei_inv`, `emep_inv`, or `htap_inv` versions match available data in Inventory Processor
- Verify paths in `R/workspace.R` and `R/run_setup.R`

### Error: "alternate_emissions.csv not found"
- Create an empty CSV file with minimal headers if not running scenarios:
  ```
  year,source,scenario
  ```

### SLURM array job fails with timeout
- Increase `--time` in `slurm/emep.array` (currently set to 24 hours)
- Reduce number of years or pollutants per run
- Increase `--cpus-per-task` or `--mem` if memory-bound

### QA/QC reports missing or incomplete
- Check that `output_QAQC <<- TRUE` in `R/run_setup.R`
- Ensure `R/qaqc_codex_v2/render.R` executed without errors (check slurm logs)

---

## Development and Extensions

### Adding a New EMEP Version
1. Create `R/v{version}/UK_v{version}_functions.R` and similar for other domains
2. Define version-specific preprocessing steps and output formats
3. Update `R/workspace.R` to source new functions
4. Test with small data subset before full run

### Adding a New Data Source
1. Place processed emissions data in `data/alt_emis/{SOURCE}/`
2. Create mapping functions in version-specific `*_functions.R` file
3. Add conditional logic in `run.R` for data source selection

### Custom Scenarios
1. Define alternate emissions in `data/alt_emis/alternate_emissions.csv`
2. Set `v_scenarios` in `R/run_setup.R`
3. Ensure folder structure matches expected output paths

---

## Related Resources

- **Inventory Processor** (upstream data): https://github.com/oSamTo/inventory_processor
- **EMEP Model**: http://emep.int/
- **GNFR Sectors**: https://www.eea.europa.eu/themes/air/air-quality-standards-and-target-values/gnfr
- **SNAP Sectors**: UN/ECE Air Pollution Classification

---

## Contact & Support

For issues or contributions related to:
- **This workflow**: Contact Sam Tom (samtom@ceh.ac.uk)
- **EMEP model**: See http://emep.int/
- **Inventory data**: See linked Inventory Processor repository

