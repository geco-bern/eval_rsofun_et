# analysis/06_processed_global_data.R
#
# PURPOSE:
#   Load the three processed global ET products (ERA5, PML, FLUXCOM) and the
#   gRSOFUN global P-model output, then build a common evaluation dataset at
#   monthly mean ET (mm d-1) for the period 1997–2011.
#
#   All three reference products are expected to have already been converted to
#   monthly mean ET in mm d-1 by data-raw/05_process_global_et_products.R.
#   The assembled dataset is returned as a list and optionally saved for use by
#   07_global_plot.R.
#
# INPUTS (from processed_global_data/):
#   monthly_era5.rds    — nested tibble {date | data<tibble(lon, lat, aet)>}
#   monthly_pml.rds     — same structure
#   monthly_fluxcom.rds — same structure
#
# INPUTS (gRSOFUN output):
#   final_aet_df_PM-S0_*.csv  — columns: sitename, year, month, aet, lon, lat, fland
#
# OUTPUT (optional):
#   processed_global_data/global_eval_dataset.rds — list with slots:
#     $global_products   — long nested tibble with source column
#     $p_model           — cleaned gRSOFUN output tibble
#     $fluxnet_monthly   — site-level monthly observed AET from FluxDataKit
#
# AUTHORS: Grossi et al. (in prep.)

library(dplyr)
library(tidyr)
library(lubridate)
library(purrr)
library(readr)
library(here)

source(here("R/calc_vpd_td.R"))   # le_to_et() and helpers

# ── Paths ─────────────────────────────────────────────────────────────────

proc_dir <- here("processed_global_data")   # processed ET products
fdk_dir  <- "/data_2/FluxDataKit/v3.4/zenodo_upload"
rsofun_csv <- "/data_2/scratch/akurth/grsofun_output/PM-S0/final_aet_df_PM-S0_20251128_1152.csv"
site_products_dir <- "~/data_scratch/global/dataset_comparison"  # site-level CSVs

# Study period
YEAR_START <- 1997L
YEAR_END   <- 2011L

# =========================================================================
# 1. Load processed global products  -------------------------------------
# =========================================================================

message("Loading processed global ET products (mm d-1) ...")

monthly_era5    <- readRDS(file.path(proc_dir, "monthly_era5.rds"))
monthly_pml     <- readRDS(file.path(proc_dir, "monthly_pml.rds"))
monthly_fluxcom <- readRDS(file.path(proc_dir, "monthly_fluxcom.rds"))

# ── Quick validation: all products should already be in mm d-1 ────────────
# (unit conversion performed in data-raw/05_process_global_et_products.R)
# Sanity check: global median AET should be < 10 mm d-1
check_units <- function(tbl, label) {
  med <- tbl |> unnest(data) |> pull(aet) |> median(na.rm = TRUE)
  if (med > 15) {
    warning(label, ": median AET = ", round(med, 1),
            " — values look too large; expected mm d-1. ",
            "Re-run data-raw/05_process_global_et_products.R.")
  } else {
    message("  ", label, ": median AET = ", round(med, 2), " mm d-1  [OK]")
  }
}

check_units(monthly_era5,    "ERA5")
check_units(monthly_pml,     "PML")
check_units(monthly_fluxcom, "FLUXCOM")

# ── Filter to common evaluation period ───────────────────────────────────
filter_period <- function(tbl) {
  tbl |> filter(year(date) >= YEAR_START, year(date) <= YEAR_END)
}

monthly_era5    <- filter_period(monthly_era5)
monthly_pml     <- filter_period(monthly_pml)
monthly_fluxcom <- filter_period(monthly_fluxcom)

# ── Combine into a single long object with a 'source' column ─────────────
global_products <- bind_rows(
  monthly_era5    |> mutate(source = "ERA5"),
  monthly_pml     |> mutate(source = "PML"),
  monthly_fluxcom |> mutate(source = "FLUXCOM")
) |>
  select(source, date, data)

message("  Combined global products: ",
        nrow(global_products), " source × month combinations")

# =========================================================================
# 2. Load gRSOFUN / P-model output  --------------------------------------
# =========================================================================

message("Loading gRSOFUN P-model output ...")

p_model_raw <- read_csv(rsofun_csv, show_col_types = FALSE)
colnames(p_model_raw) <- c("sitename", "year", "month", "aet", "lon", "lat", "fland")

# ── Remove ocean / mostly-ocean pixels ────────────────────────────────────
# fland < 0.5 means < 50 % land fraction → exclude
p_model <- p_model_raw |>
  filter(fland >= 0.5) |>
  filter(year >= YEAR_START, year <= YEAR_END)

# Build a proper date column for convenience
p_model <- p_model |>
  mutate(date = as.Date(sprintf("%04d-%02d-01", year, month)))

# ── Unit check: gRSOFUN outputs monthly-mean AET in mm d-1 ────────────────
# (No additional conversion needed; the factor 365/12 used in the old script
#  was incorrect — gRSOFUN AET is already a daily mean, not a monthly sum.)
message("  P-model rows after filtering: ", nrow(p_model))

# =========================================================================
# 3. Site-level evaluation dataset (FLUXNET / FluxDataKit)  --------------
# =========================================================================

message("Building site-level evaluation dataset ...")

# ── Site metadata & quality flags ─────────────────────────────────────────
fdk_site_info <- read_csv(file.path(fdk_dir, "fdk_site_info.csv"),
                          show_col_types = FALSE)
fdk_filter    <- read_csv(file.path(fdk_dir, "fdk_site_fullyearsequence.csv"),
                          show_col_types = FALSE)

# Remove croplands, wetlands, and two problematic Canadian sites
fdk_site_info <- fdk_site_info |>
  filter(!igbp_land_use %in% c("CRO", "WET")) |>
  filter(!sitename %in% c("CA-SF1", "CA-SF3"))

# Keep only site-years with good LE data quality
fdk_filter <- fdk_filter |> filter(drop_le == FALSE)

fdk_site_info <- fdk_site_info |> filter(sitename %in% fdk_filter$sitename)
fdk_filter    <- fdk_filter    |> filter(sitename %in% fdk_site_info$sitename)

# ── Driver data ───────────────────────────────────────────────────────────
driver_full <- read_rds(file.path(fdk_dir, "rsofun_driver_data_v3.4.2.rds"))

driver_full <- driver_full |> filter(sitename %in% fdk_site_info$sitename)

# Helper: enthalpy of vaporisation and water density (for LE → ET)
calc_enthalpy_vap <- function(tc) {
  1918460 * ((tc + 273.15) / (tc + 273.15 - 33.91))^2
}

calc_density_h2o <- function(tc, press) {
  po <- 0.99983952 +
    6.78826e-05 * tc - 9.08659e-06 * tc^2 + 1.02213e-07 * tc^3 -
    1.35439e-09 * tc^4 + 1.47115e-11 * tc^5 - 1.11663e-13 * tc^6 +
    5.04407e-16 * tc^7 - 1.00659e-18 * tc^8
  ko <- 19652.17 + 148.183 * tc - 2.29995 * tc^2 + 0.01281 * tc^3 -
    4.91564e-05 * tc^4 + 1.03553e-07 * tc^5
  ca <- 3.26138 + 0.0005223 * tc + 0.0001324 * tc^2 -
    7.655e-07 * tc^3 + 8.584e-10 * tc^4
  cb <- 7.2061e-05 - 5.8948e-06 * tc + 8.699e-08 * tc^2 -
    1.01e-09 * tc^3 + 4.322e-12 * tc^4
  pbar <- 1e-05 * press
  1000 * po * (ko + ca * pbar + cb * pbar^2) /
    (ko + ca * pbar + cb * pbar^2 - pbar)
}

#' LE (W m-2) → ET (mm d-1)
le_to_et <- function(df) {
  1000 * 86400 * df$le / (calc_enthalpy_vap(df$temp) *
                            calc_density_h2o(df$temp, df$patm))
}

# ── Unnest forcing and restrict to quality-flagged year ranges ─────────────
driver_forcing <- driver_full |>
  select(sitename, forcing) |>
  unnest(cols = forcing) |>
  left_join(
    fdk_filter |> select(sitename,
                         year_start = year_start_le,
                         year_end   = year_end_le),
    by = "sitename"
  ) |>
  mutate(year = year(date)) |>
  filter(year >= year_start, year <= year_end, year <= YEAR_END) |>
  select(-year_start, -year_end, -year)

# ── Observed AET from FluxDataKit LE → mm d-1 ─────────────────────────────
# LE in the driver is in W m-2; convert to mm d-1
obs_et_daily <- driver_forcing |>
  select(sitename, date, le, temp, patm) |>
  mutate(obs_aet = le_to_et(pick(le, temp, patm))) |>
  select(sitename, date, obs_aet)

# ── Aggregate to monthly means (mm d-1) ──────────────────────────────────
fluxnet_monthly <- obs_et_daily |>
  mutate(year = year(date), month = month(date)) |>
  group_by(sitename, year, month) |>
  summarise(obs_aet = mean(obs_aet, na.rm = TRUE), .groups = "drop") |>
  mutate(date = as.Date(sprintf("%04d-%02d-01", year, month))) |>
  # Re-apply quality year-range filter at the monthly level
  left_join(
    fdk_filter |> select(sitename,
                         year_start = year_start_le,
                         year_end   = year_end_le),
    by = "sitename"
  ) |>
  filter(year >= year_start, year <= year_end) |>
  select(sitename, date, year, month, obs_aet)

# ── Site-collocated gRSOFUN values ────────────────────────────────────────
# Match each FLUXNET site to the nearest P-model grid cell

nearest_pmodel <- function(lon_site, lat_site, pm) {
  pm_sub <- pm |>
    filter(
      abs(lon - lon_site) == min(abs(lon - lon_site))
    )
  pm_sub |>
    filter(
      abs(lat - lat_site) == min(abs(lat - lat_site))
    )
}

rsofun_site <- map(
  seq_len(nrow(fdk_site_info)),
  function(i) {
    si <- fdk_site_info[i, ]
    nearest_pmodel(si$lon, si$lat, p_model) |>
      transmute(
        sitename = si$sitename,
        date     = date,
        rsofun   = aet          # already mm d-1
      )
  }
) |>
  list_rbind()

# Re-apply site year-range filter
rsofun_site <- rsofun_site |>
  left_join(
    fdk_filter |> select(sitename,
                         year_start = year_start_le,
                         year_end   = year_end_le),
    by = "sitename"
  ) |>
  mutate(year = year(date)) |>
  filter(year >= year_start, year <= year_end) |>
  select(sitename, date, rsofun)

# ── Site-level reference products (ERA5, PML, FLUXCOM point extracts) ─────
# These CSVs were pre-extracted on the workstation (one row per site × month)
# Expected columns: sitename, date, aet  (aet in mm d-1 for ERA5/FLUXCOM;
#                                          mm d-1 after /days for PML)
days_in_month_df <- tibble(
  month = 1:12,
  days  = c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
)

era5_site <- read_csv(
  file.path(site_products_dir, "era5_fluxnet.csv"),
  show_col_types = FALSE
) |>
  rename(era5 = aet) |>
  left_join(
    fdk_filter |> select(sitename,
                         year_start = year_start_le,
                         year_end   = year_end_le),
    by = "sitename"
  ) |>
  filter(year(date) >= year_start, year(date) <= year_end) |>
  select(sitename, date, era5)

pml_site <- read_csv(
  file.path(site_products_dir, "PML_fluxnet.csv"),
  show_col_types = FALSE
) |>
  # PML is in mm month-1; convert to mm d-1
  mutate(month = month(date)) |>
  left_join(days_in_month_df, by = "month") |>
  mutate(pml = aet / days) |>
  left_join(
    fdk_filter |> select(sitename,
                         year_start = year_start_le,
                         year_end   = year_end_le),
    by = "sitename"
  ) |>
  filter(year(date) >= year_start, year(date) <= year_end) |>
  select(sitename, date, pml)

# FLUXCOM: LE [MJ m-2 d-1] → mm d-1 using collocated driver temp/patm
fluxcom_raw_site <- read_csv(
  file.path(site_products_dir, "fluxicom_fluxnet.csv"),
  show_col_types = FALSE
)

fluxcom_site <- fluxcom_raw_site |>
  rename(le = le_MJ_day) |>
  mutate(le = le * 1e6 / 86400) |>   # MJ m-2 d-1 → W m-2
  left_join(
    driver_forcing |>
      select(sitename, date, temp, patm) |>
      mutate(date = as.Date(date)),
    by = c("sitename", "date")
  ) |>
  drop_na(temp, patm) |>
  mutate(fluxcom = le_to_et(pick(le, temp, patm))) |>
  left_join(
    fdk_filter |> select(sitename,
                         year_start = year_start_le,
                         year_end   = year_end_le),
    by = "sitename"
  ) |>
  filter(year(date) >= year_start, year(date) <= year_end) |>
  select(sitename, date, fluxcom)

# ── Merge all site-level data ──────────────────────────────────────────────
monthly_site_df <- fluxnet_monthly |>
  select(sitename, date, obs_aet) |>
  left_join(era5_site,      by = c("sitename", "date")) |>
  left_join(pml_site,       by = c("sitename", "date")) |>
  left_join(fluxcom_site,   by = c("sitename", "date")) |>
  left_join(rsofun_site,    by = c("sitename", "date"))

message("  Site-level monthly dataset: ",
        nrow(monthly_site_df), " rows, ",
        n_distinct(monthly_site_df$sitename), " sites")

# =========================================================================
# 4. Save assembled evaluation dataset  ----------------------------------
# =========================================================================

eval_dataset <- list(
  global_products = global_products,   # global gridded, mm d-1, 1997–2011
  p_model         = p_model,           # global P-model grid, mm d-1, 1997–2011
  fluxnet_monthly = monthly_site_df    # site-level, mm d-1, per-site year range
)

out_path <- file.path(proc_dir, "global_eval_dataset.rds")
saveRDS(eval_dataset, out_path)
message("\nSaved evaluation dataset: ", out_path)

message("\n--- Summary ---")
message("Global products: ", paste(unique(global_products$source), collapse = ", "))
message("P-model grid cells: ", n_distinct(paste(p_model$lon, p_model$lat)))
message("FLUXNET sites: ", n_distinct(monthly_site_df$sitename))
message("Evaluation period: ", YEAR_START, "–", YEAR_END)
message("ET unit throughout: mm d-1 (monthly mean)")
