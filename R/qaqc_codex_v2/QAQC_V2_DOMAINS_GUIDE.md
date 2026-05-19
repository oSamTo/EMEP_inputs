# QAQC v2 Codex - Multi-Domain Support Guide

## Overview
The QAQC v2 routines have been updated to fully support three domains with proper handling of data sources, polygon overlays, and domain-specific file naming conventions.

## Supported Domains

### 1. UKEIRE Domain
**Location:** UK and Ireland
**File Naming Pattern:** `{species}_UKEIRE_{y}emis_{map_yr_uk}map_{inv}inv_{STAGE}.csv`
**Key Features:**
- Requires `map_yr_uk` parameter (mapping year)
- Includes MASKED processing stage
- Domain outline polygon: `data/spatial/UKEire/UKEIRE_LL.shp`
- Sector maps include explicit domain borders

**Example Call:**
```r
create_qaqc_v2_codex(
  project = "MyProject",
  scenario = "BASE",
  y = 2021,
  species = "NOx",
  domain = "UKEIRE",
  folname = "/path/to/UKEIRE/outputs",
  inv = "v5.0",
  data_source = NA,
  map_yr_uk = 2021,
  time_dim = "Annual",
  emep_version = "v5.0",
  v_EMEP_sec = "GNFR",
  render_pdf = TRUE
)
```

### 2. EU Domain
**Location:** Europe
**File Naming Pattern:** `{species}_EU_{y}emis_{inv}inv_{STAGE}.csv` (or with map year)
**Key Features:**
- Supports multiple file naming conventions
- No mapping year required
- Includes MASKED processing stage
- Domain outline polygon: `data/spatial/EU/EU.shp` (if available, falls back to world borders)
- Scales properly to EU extent

**Example Call:**
```r
create_qaqc_v2_codex(
  project = "MyProject",
  scenario = "BASE",
  y = 2021,
  species = "NOx",
  domain = "EU",
  folname = "/path/to/EU/outputs",
  inv = "v5.0",
  data_source = NA,
  time_dim = "Annual",
  emep_version = "v5.0",
  v_EMEP_sec = "GNFR",
  render_pdf = TRUE
)
```

### 3. GLOBAL Domain

#### 3a. EDGAR Source
**Location:** Global with EDGAR grid
**File Naming Pattern:** `{species}_GLOBAL_{y}emis_{inv}inv_{STAGE}.csv` (flexible)
**Key Features:**
- EDGAR-specific grid boundaries: `data/spatial/world/global_iso_edgar.shp`
- Supports multiple file naming conventions
- No MASKED processing stage
- Timeseries data from inventory processor
- Flexible data source detection

**Example Call:**
```r
create_qaqc_v2_codex(
  project = "MyProject",
  scenario = "BASE",
  y = 2021,
  species = "NOx",
  domain = "GLOBAL",
  folname = "/path/to/GLOBAL/EDGAR/outputs",
  inv = "EDGAR_v5.0",
  data_source = "EDGAR",
  time_dim = "Annual",
  emep_version = "v5.0",
  v_EMEP_sec = "GNFR",
  render_pdf = TRUE
)
```

#### 3b. HTAP Source
**Location:** Global with HTAP grid
**File Naming Pattern:** `{species}_GLOBAL_{y}emis_{inv}inv_{STAGE}.csv` (flexible)
**Key Features:**
- HTAP-specific grid boundaries: `data/spatial/world/global_iso_htap.shp`
- Supports multiple file naming conventions
- No MASKED processing stage
- Timeseries data from inventory processor
- Flexible data source detection

**Example Call:**
```r
create_qaqc_v2_codex(
  project = "MyProject",
  scenario = "BASE",
  y = 2021,
  species = "NOx",
  domain = "GLOBAL",
  folname = "/path/to/GLOBAL/HTAP/outputs",
  inv = "HTAPv3.2",
  data_source = "HTAP",
  time_dim = "Annual",
  emep_version = "v5.0",
  v_EMEP_sec = "GNFR",
  render_pdf = TRUE
)
```

## File Structure Requirements

### Input Files
Each domain requires specific CSV tables at processing stages:
```
{folname}/tables/e{y}/
├── {species}_{DOMAIN}_{naming}_{STAGE}.csv
└── [tables for each processing stage]
```

### Raster Files
Precomputed rasters should be located at:
```
{folname}/rast/e{y}/
├── {species}_total_emis_qaqc.tif
└── {species}_sector{N:02d}_emis_qaqc.tif
```

### Output Files
QAQC reports will be generated at:
```
{folname}/qaqc/e{y}/
├── {species}_{DOMAIN}_{y}emis_{inv}inv_QAQC_v2_codex.pdf
├── plots/e{y}/qaqc_v2_codex/
│   ├── {species}_total_quantile_map.png
│   ├── {species}_sector_quantile_maps.png
│   ├── {species}_stage_totals_by_iso.png
│   ├── {species}_top10_iso_sector_totals.png
│   ├── {species}_global_total_timeseries.png [GLOBAL only]
│   └── {species}_global_sector_timeseries.png [GLOBAL only]
└── raster_tables/e{y}/ [if generated]
```

## Key Features by Processing Stage

### Inventory Stage
- Read and validate initial inventory totals
- Available for all domains
- Column name variants supported: `emis_t`, `emis_t_spatial_scaled`, `emis_t_scalar`

### Masked Stage (UKEIRE, EU only)
- Applied domain/spatial masking
- Column variants: `tsum`, `emis_t_tot_masked`
- Skipped for GLOBAL domain

### Processed Stage
- Data processing and aggregation
- Column variants: `emis_t_tot_grouped`, `ann_emis_kt` (GLOBAL uses kilotonnes)
- Available for all domains

### NetCDF Input/Output Stages
- Pre-write and post-write validation
- Checks totals before and after NetCDF file creation
- Available for all domains

## Plotting Features

### Raster Maps
- **Total Emissions Map**: Shows overall domain emissions with quantile-based classification
  - GLOBAL: Includes ISO grid boundary overlay (EDGAR or HTAP specific)
  - UKEIRE: Includes UK/Ireland boundary
  - EU: Includes domain boundary (if available)
  
- **Sector Maps**: Faceted display of sectoral contributions
  - Consistent color scale across sectors
  - Domain outlines for clear boundary definition
  - GNFR sector names when lookup provided

### Summary Plots
- **Stage Totals by Area**: Bar charts showing conservation through processing stages
- **Top 10 ISO Sector Composition**: Stacked bar chart of largest emitting areas
- **Global Timeseries** (GLOBAL domain only):
  - Total emissions trend over 25-year window
  - Sectoral breakdowns with GNFR codes

## Data Source Detection

The QAQC routines auto-detect data source when possible:

1. **Explicit Parameter**: If `data_source` is provided, it's used directly
2. **Directory Path Inference**: Extracted from output folder name (e.g., `.../EDGAR/outputs`)
3. **Flexible File Matching**: Multiple naming patterns checked for compatibility

**Priority for GLOBAL domain:**
- EDGAR → Uses `global_iso_edgar.shp`
- HTAP → Uses `global_iso_htap.shp`
- (Default) → Falls back to HTAP

## Testing Checklist

- [ ] UKEIRE domain: Verify map_yr_uk parameter works correctly
- [ ] UKEIRE domain: Check MASKED stage appears in stage totals plot
- [ ] UKEIRE domain: Verify domain outline visible on sector maps
- [ ] EU domain: Test with multiple naming conventions
- [ ] EU domain: Check shapefile fallback works
- [ ] GLOBAL EDGAR: Verify EDGAR grid boundaries used
- [ ] GLOBAL EDGAR: Check timeseries loads correctly
- [ ] GLOBAL HTAP: Verify HTAP grid boundaries used
- [ ] GLOBAL HTAP: Check timeseries loads correctly
- [ ] All domains: Verify raster quantile classification works
- [ ] All domains: Check top-10 ISO sector table generated
- [ ] All domains: Verify PDF renders without errors

## Troubleshooting

### No polygon overlay appears on maps
- Check shapefile exists at expected location
- Verify spatial library can read shapefile
- For GLOBAL: Confirm data_source is set correctly (EDGAR/HTAP)

### Timeseries plots not appearing (GLOBAL only)
- Verify inventory processor data directory exists
- Check naming of totals CSV files in source directory
- Confirm species name matches inventory data

### Files not found for processing stage
- Check naming conventions match domain expectations
- Verify table files exist in `{folname}/tables/e{y}/`
- Check file permissions are readable

### PDF rendering fails
- Check all required packages are installed
- Verify raster files exist and are readable
- Check LaTeX is installed for PDF generation
