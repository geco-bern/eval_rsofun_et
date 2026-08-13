# Evaluation of simulated ET from rsofun

This repository implements the analysis of Grossi et al. (in prep.).

---

## Table of Contents

- [Repository Structure](#repository-structure)
  - [analysis/](#analysis)
  - [data-raw/](#data-raw)
  - [R/](#r)
  - [fig/](#fig)
  - [processed\_global\_data/](#processed_global_data)
- [Data download](#data-download)
  - [ERA5](#era5)
  - [FLUXCOM](#fluxcom)
  - [PML](#pml)
- [Processing pipeline](#processing-pipeline)

---

## Repository Structure

### analysis/

Contains all analysis scripts. They are organised by spatial extent and must
be run in numerical order within each scale. Scripts ending with `*_plot.R`
read pre-computed data and can be run independently once the upstream steps
are complete.

#### FLUXNET scale

| Script | Description |
|---|---|
| `00_prepare_forcing_fluxnet.R` | Loads the FluxDataKit v3.4 rsofun driver and site metadata. Filters sites by vegetation type (excludes croplands and wetlands), data-quality flags, and minimum time-series length. Splits the retained sites into a training set (one site per Koeppen × IGBP stratum, stratified random sample) and a held-out test set. Saves `data/driver.rds` and `data/sites.csv`. |
| `01_prepare_obs_fluxnet.R` | Reads the filtered driver produced by `00_...` and extracts the observed GPP and LE time series. Aggregates them to daily, x-daily, monthly, and annual resolutions and saves a structured `obs_eval` list used as the evaluation target in subsequent steps. |
| `02_calib_fluxnet.R` | Calibrates the P-model against the FLUXNET training sites using Bayesian parameter estimation (`BayesianTools`). Free parameters are `kphio`, `kphio_par_a`, `kphio_par_b`, `soilm_thetastar`, and `gw_calib`, with separate error terms for GPP and LE. Saves the posterior parameter samples for use in evaluation. |
| `03_fluxnet_eval_plot.R` | Runs the calibrated P-model on the held-out test sites and produces the FLUXNET-scale evaluation figures: observed vs. modelled scatter plots, Taylor diagrams, and drought-response analysis for both GPP and ET. |

#### CAMELS scale

| Script | Description |
|---|---|
| `04_create_camels_driver.R` | Builds the complete rsofun driver for all CAMELS-US catchments in a single pipeline. (1) Ingests spatially averaged climate forcings — temperature, VPD, PPFD, atmospheric pressure, precipitation, CO₂, cloud cover, runoff — from Caravan CSV files and gridded products via `ingestr` and `get_driver_bycatchment.R`. (2) Extracts water-holding capacity from the Stocker et al. (2023) cwdx80 raster and MODIS fAPAR (monthly → daily interpolation). (3) Extracts net radiation as SW + LW downwelling from the WFDEI archive (1990–2010) and fills the `netrad` forcing column. Saves `data/driver_camels.rds`. |
| `05_camels_plot.R` | Runs the P-model on CAMELS catchments and produces the catchment-scale evaluation figures. Filters out regulated catchments (degree of regulation > 0) and basins where annual runoff exceeds annual precipitation. Outputs include spatial maps of model performance (RMSE, R², bias) across the contiguous US. |

#### Global scale

| Script | Description |
|---|---|
| `06_a_process_global_data.R` | Loads the three processed global ET products (ERA5, PML, FLUXCOM) from `processed_global_data/` and the gRSOFUN global P-model CSV output. Validates units (all products must be in mm d⁻¹), filters to the 1997–2011 evaluation period, and assembles site-level and gridded evaluation datasets. Saves `processed_global_data/global_eval_dataset.rds`. |
| `06_b_build_evaluation_dataset.R` | Loads the three processed global ET products (ERA5, PML, FLUXCOM) from `processed_global_data/` and the gRSOFUN global P-model CSV output. Validates units (all products must be in mm d⁻¹), filters to the 1997–2011 evaluation period, and assembles site-level and gridded evaluation datasets. Saves `processed_global_data/global_eval_dataset.rds`. |
| `07_global_plot.R` | Reads the assembled evaluation dataset and produces all global-scale figures: (a) latitudinal profile of annual AET for the P-model vs. the ERA5/PML/FLUXCOM ensemble; (b) global map of P-model mean annual AET; (c) RMSE and R² boxplots at FLUXNET site locations comparing P-model, ERA5, PML, and FLUXCOM against observed ET. Figures are saved to `fig/`. |

---

### data-raw/

Contains scripts used to process raw downloaded data into the formats consumed
by the analysis scripts. These scripts require access to the raw data archives
on the workstation and cannot be run elsewhere.

| Script | Description |
|---|---|
| `download_era5.py` | Script to downoald ERA5 dataset |

---

### R/

Contains helper functions sourced by the analysis scripts.

| File | Description |
|---|---|
| `calc_vpd_td.R` | Functions for computing vapour pressure deficit from dew-point temperature, atmospheric pressure from elevation and temperature, and latent-heat flux to ET conversion (`le_to_et`). |
| `modified_cost_likelihood_pmodel.R` | Custom log-likelihood cost function used in `02_calib_fluxnet.R`; evaluates joint likelihood of GPP and LE residuals with separate error parameters. |
| `eval_sofun.R`, `get_stats.R`, `analyse_modobs2.R` | Evaluation utilities: compute model performance statistics (RMSE, R², bias, slope) and generate observed-vs-modelled diagnostic plots. |
| `align_events.R`, `eval_droughtresponse.R` | Functions for event-centred analysis; align time series around drought events and compute the model's drought-response relative to observations. |
| `create_obs_eval.R` | Constructs the structured observation object (`obs_eval`) from the driver data for use by `rsofun::runread_pmodel_f`. |
| `get_driver_bycatchment.R` | Extracts and assembles climate forcings for a single catchment from gridded archives; called iteratively by `04_create_camels_driver.R`. |
| `extract_fapar_byfile.R`, `interpolate2daily_fapar.R` | Ingest FAPAR from per-file NetCDF archives and interpolate monthly values to daily resolution for the rsofun driver. |
| `create_table_latex.R` | Formats summary statistics as LaTeX tables for the manuscript. |
| `heatscatter_dependencies.R` | Dependencies for heat-scatter density plots used in `05_camels_plot.R`. |
| `global_legend.R` | `plot_discrete_cbar()` helper that draws a discrete colour-bar legend for the global AET map. |
| `main_plus_metrics.R` | Runs the P-model and appends performance metrics to the output; wraps `rsofun::runread_pmodel_f` with a post-processing step. |

---

### fig/

Output figures are saved here (`.svg` and `.pdf`).

---

### processed_global_data/

**Target path:** `~/data/archive/eval_rsofun_et/processed_global_data/`

Stores the processed outputs of `analysis/06_a_process_global_data.R` —
one `.rds` file per product, each a nested tibble with monthly mean ET in mm d⁻¹
at 0.5° × 0.5° resolution:

| File | Period | Source |
|---|---|---|
| `monthly_era5.rds` | 1982–2011 | ERA5 reanalysis |
| `monthly_pml.rds` | 1997–2011 | PML v2 |
| `monthly_fluxcom.rds` | 1997–2011 | FLUXCOM RS+METEO ensemble |
| `monthly_tmp_patm.rds` | 1997–2011 | Monthly mean temperature and atmospheric pressure (required for FLUXCOM LE→ET conversion) |
| `global_eval_dataset.rds` | 1997–2011 | Assembled evaluation dataset produced by `analysis/06_b_build_evaluation_dataset.R` |

---

## Data download

Three global ET products are used to benchmark the P-model output.
Download instructions for each are given below.

---

### ERA5

**Variable:** Surface latent heat flux (`slhf`)  
**Temporal resolution:** Monthly means  
**Native resolution:** ~0.1°  
**Period downloaded:** 1982–2011  
**Format:** NetCDF (single file after unzipping)  
**Target path:** `~/data_scratch/big_data/data_stream-moda.nc`

#### Registration

Create a free account at the [Copernicus Climate Data Store (CDS)](https://cds.climate.copernicus.eu).
Then install and configure the CDS API client:

```bash
pip install cdsapi
```

Create `~/.cdsapirc` with your UID and API key (available on your CDS profile page):

```
url: https://cds.climate.copernicus.eu/api/v2
key: <UID>:<API-key>
```

#### Download script

Save the following as `data-raw/download_era5.py` and run with
`python data-raw/download_era5.py`:

```python
import cdsapi

dataset = "reanalysis-era5-single-levels-monthly-means"
request = {
    "product_type": ["monthly_averaged_reanalysis"],
    "variable": ["surface_latent_heat_flux"],
    "year": [str(y) for y in range(1982, 2012)],
    "month": [f"{m:02d}" for m in range(1, 13)],
    "time": ["00:00"],
    "data_format": "netcdf",
    "download_format": "zip"
}

client = cdsapi.Client()
client.retrieve(dataset, request).download("era5_slhf_1982_2011.zip")
```

Unzip the archive and move the resulting `.nc` file to the target path above.

#### Notes on the raw variable

- `slhf` is an **accumulated** flux [J m⁻²] over the month with an **upward-positive** sign convention (values are negative over land).
- `06_a_process_global_data.R` inverts the sign and converts to mm d⁻¹.
- The native longitude grid runs 0–360°; the processing script reorders it to −180–180°.

---

### FLUXCOM

**Variable:** Latent heat flux (`LE`)  
**Temporal resolution:** Monthly means  
**Native resolution:** 0.5°  
**Period downloaded:** 1997–2011  
**Format:** NetCDF (one file per year)  
**Product used:** RS+METEO ensemble mean  
**Target path:** `/data/archive/eval_rsofun_et/FLUXCOM/ensemble/`

#### Registration and access

FLUXCOM data are distributed by the Max Planck Institute for Biogeochemistry.
Request access at:

<https://www.fluxcom.org/EF-Download/>

If the download links on that page are inactive, try the BGI software portal:

<https://www.bgc-jena.mpg.de/en/bgi/software>

Access is granted after a brief registration; you will receive FTP credentials by email.

#### Download

Once you have FTP credentials, download the RS+METEO ensemble LE files for
1997–2011. Files follow the naming pattern `LE.<YEAR>.4320.720.nc` (or similar,
depending on the version). Place all files in the target directory above.

A minimal `lftp` command to batch-download the relevant years:

```bash
lftp -u <username>,<password> ftp.bgc-jena.mpg.de << 'EOF'
cd /data/FLUXCOM/RS+METEO/ensemble/
mget LE.199*.nc LE.200*.nc LE.201[01].nc
bye
EOF
```

Adjust the remote path as indicated in the access instructions you receive.

#### Notes on the raw variable

- `LE` is in **MJ m⁻² d⁻¹** (mean daily flux for each month).
- Conversion to mm d⁻¹ requires monthly mean air temperature and atmospheric pressure. These are provided by `processed_global_data/monthly_tmp_patm.rds`; if that file is absent, `06_a_process_global_data.R` falls back to fixed physical constants (λ = 2.45 × 10⁶ J kg⁻¹, ρ = 1000 kg m⁻³).

---

### PML

**Variable:** Actual evapotranspiration (`ETa`)  
**Temporal resolution:** Monthly totals  
**Native resolution:** 0.5°  
**Period downloaded:** 1997–2011 (data before 1997 have quality issues)  
**Format:** NetCDF (one file per year)  
**Target path:** `/data/archive/eval_rsofun_et/PML/data/`

#### Reference

Zhang, Y. et al. (2016). Multi-decadal trends in global terrestrial
evapotranspiration and its components. *Scientific Reports*, 6, 19124.
<https://doi.org/10.1038/srep19124>

#### Download

Data are archived by CSIRO:

<https://data.csiro.au/collection/csiro:17375v2>

1. Navigate to the link above and click **Access** (free registration may be required).
2. Download the monthly ETa files for 1997–2011. Files follow the pattern `Monthly_PML_ETa_<YEAR>.nc`.
3. Place all files in the target directory above.

#### Notes on the raw variable

- `ETa` is in **mm month⁻¹** (monthly total, not a daily mean).
- The native raster has transposed x/y axes; `06_a_process_global_data.R` corrects this with `flip(trans(flip(r)))`.
- The processing script converts mm month⁻¹ to mm d⁻¹ by dividing by the number of days in each month.

---

## Processing pipeline

Run scripts in the order below. Within each scale, upstream scripts must
complete before downstream ones.

```
# ── Global ET product processing (workstation only) ──────────────────────

# ── FLUXNET scale ─────────────────────────────────────────────────────────
analysis/00_prepare_forcing_fluxnet.R      # filter sites, build driver → data/driver.rds
analysis/01_prepare_obs_fluxnet.R          # extract observed GPP & ET → data/obs_eval.rds
analysis/02_calib_fluxnet.R                # Bayesian calibration → data/params_calib.rds
analysis/03_fluxnet_eval_plot.R            # evaluation figures → fig/

# ── CAMELS scale ──────────────────────────────────────────────────────────
analysis/04_create_camels_driver.R         # build driver with all forcings → data/driver_camels.rds
analysis/05_camels_plot.R                  # evaluation figures → fig/

# ── Global scale ──────────────────────────────────────────────────────────

analysis/06_a_process_global_data.R   # process each global dataset → processed_global_data/
analysis/06_b_build_evaluation_dataset.R        # assemble evaluation dataset → processed_global_data/global_eval_dataset.rds
analysis/07_global_plot.R                  # evaluation figures → fig/
```

