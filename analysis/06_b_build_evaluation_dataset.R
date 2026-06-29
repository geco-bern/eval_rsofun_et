# analysis/06_processed_global_data.R
#
# PURPOSE:
#   Load the three processed global ET products (ERA5, PML, FLUXCOM) and the
#   gRSOFUN global P-model output, then build a common evaluation dataset at
#   monthly mean ET (mm d-1) for the period 1997-2011.
#
#   All three reference products must have already been converted to monthly
#   mean ET in mm d-1 by data-raw/05_process_global_et_products.R.
#   The assembled dataset is saved for use by analysis/07_global_plot.R.
#
# INPUTS (from processed_global_data/):
#   monthly_era5.rds    -- nested tibble {date | data<tibble(lon, lat, aet)>}
#   monthly_pml.rds     -- same structure
#   monthly_fluxcom.rds -- same structure
#
# INPUTS (gRSOFUN output):
#   data/final_aet_df_PM-S0.csv  -- columns: sitename, year, month, aet, lon, lat, fland
#
# OUTPUT:
#   processed_global_data/global_eval_dataset.rds  -- list with slots:
#     $global_products   -- long nested tibble with source column
#     $p_model           -- cleaned gRSOFUN output tibble
#     $fluxnet_monthly   -- site-level monthly observed and modelled AET
#
# AUTHORS: Grossi et al. (in prep.)

library(dplyr)
library(tidyr)
library(lubridate)
library(purrr)
library(readr)
library(here)

source(here("R/calc_vpd_td.R"))   # provides le_to_et() and physical helpers

# ── Paths ──────────────────────────────────────────────────────────────────
proc_dir     <- here("processed_global_data")
fdk_dir      <- here("data/fluxnet")          # FluxDataKit Zenodo files
rsofun_csv   <- here("data/final_aet_df_PM-S0.csv")
site_csv_dir <- here("data/site_products")    # site-collocated CSVs

# Study period
YEAR_START <- 1997L
YEAR_END   <- 2011L

# =========================================================================
# 1. Load processed global products
# =========================================================================

message("Loading processed global ET products (mm d-1) ...")

monthly_era5    <- readRDS(file.path(proc_dir, "monthly_era5.rds"))
monthly_pml     <- readRDS(file.path(proc_dir, "monthly_pml.rds"))
monthly_fluxcom <- readRDS(file.path(proc_dir, "monthly_fluxcom.rds"))

# Unit validation: global median AET should be in a plausible mm d-1 range
check_units <- function(tbl, label) {
  med <- tbl |> unnest(data) |> pull(aet) |> median(na.rm = TRUE)
  if (med > 15) {
    warning(label, ": median AET = ", round(med, 1),
            " -- values look too large; expected mm d-1. ",
            "Re-run data-raw/05_process_global_et_products.R.")
  } else {
    message("  ", label, ": median AET = ", round(med, 2), " mm d-1  [OK]")
  }
}

check_units(monthly_era5,    "ERA5")
check_units(monthly_pml,     "PML")
check_units(monthly_fluxcom, "FLUXCOM")

# Filter to common evaluation period
filter_period <- function(tbl) {
  tbl |> filter(year(date) >= YEAR_START, year(date) <= YEAR_END)
}

monthly_era5    <- filter_period(monthly_era5)
monthly_pml     <- filter_period(monthly_pml)
monthly_fluxcom <- filter_period(monthly_fluxcom)

# Combine into a single long object with a source column
global_products <- bind_rows(
  monthly_era5    |> mutate(source = "ERA5"),
  monthly_pml     |> mutate(source = "PML"),
  monthly_fluxcom |> mutate(source = "FLUXCOM")
) |>
  select(source, date, data)

message("  Combined: ", nrow(global_products), " source x month combinations")

# =========================================================================
# 2. Load gRSOFUN / P-model output
# =========================================================================

message("Loading gRSOFUN P-model output ...")

p_model_raw <- read_csv(rsofun_csv, show_col_types = FALSE)
colnames(p_model_raw) <- c("sitename", "year", "month", "aet", "lon", "lat", "fland")

# Remove ocean / mostly-ocean pixels (land fraction < 50 %)
p_model <- p_model_raw |>
  filter(fland >= 0.5) |>
  filter(year >= YEAR_START, year <= YEAR_END) |>
  mutate(date = as.Date(sprintf("%04d-%02d-01", year, month)))

message("  P-model rows after filtering: ", nrow(p_model))

# =========================================================================
# 3. Site-level evaluation dataset (FLUXNET / FluxDataKit)
# =========================================================================

message("Building site-level evaluation dataset ...")

# ── Site metadata and quality flags ───────────────────────────────────────
fdk_site_info <- read_csv(here(fdk_dir, "fdk_site_info.csv"),
                          show_col_types = FALSE)
fdk_filter    <- read_csv(here(fdk_dir, "fdk_site_fullyearsequence.csv"),
                          show_col_types = FALSE)

# Remove croplands, wetlands, and two problematic Canadian sites
fdk_site_info <- fdk_site_info |>
  filter(!igbp_land_use %in% c("CRO", "WET")) |>
  filter(!sitename %in% c("CA-SF1", "CA-SF3"))

fdk_filter <- fdk_filter |>
  filter(drop_le == FALSE) |>
  filter(sitename %in% fdk_site_info$sitename)

fdk_site_info <- fdk_site_info |>
  filter(sitename %in% fdk_filter$sitename)

# ── Driver data ───────────────────────────────────────────────────────────
driver_full <- read_rds(here(fdk_dir, "rsofun_driver_data_v3.4.2.rds")) |>
  filter(sitename %in% fdk_site_info$sitename)

# ── Physical helpers for LE -> ET conversion ─────────────────────────────
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

le_to_et <- function(df) {
  1000 * 86400 * df$le /
    (calc_enthalpy_vap(df$temp) * calc_density_h2o(df$temp, df$patm))
}

# ── Unnest forcing and restrict to quality-flagged year ranges ────────────
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

# ── Observed AET: LE (W m-2) -> mm d-1 ───────────────────────────────────
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
  left_join(
    fdk_filter |> select(sitename,
                         year_start = year_start_le,
                         year_end   = year_end_le),
    by = "sitename"
  ) |>
  filter(year >= year_start, year <= year_end) |>
  select(sitename, date, year, month, obs_aet)

# ── Site-collocated P-model values ───────────────────────────────────────
rsofun_site <- map(seq_len(nrow(fdk_site_info)), function(i) {
  si <- fdk_site_info[i, ]
  p_model |>
    filter(
      abs(lon - si$lon) == min(abs(lon - si$lon)),
      abs(lat - si$lat) == min(abs(lat - si$lat))
    ) |>
    transmute(sitename = si$sitename, date = date, rsofun = aet)
}) |>
  list_rbind() |>
  left_join(
    fdk_filter |> select(sitename,
                         year_start = year_start_le,
                         year_end   = year_end_le),
    by = "sitename"
  ) |>
  mutate(year = year(date)) |>
  filter(year >= year_start, year <= year_end) |>
  select(sitename, date, rsofun)

# ── Site-collocated reference products ───────────────────────────────────
# CSVs pre-extracted on the workstation; expected columns:
#   sitename, date, aet  (aet in mm d-1 for ERA5; mm month-1 for PML;
#                          LE in MJ m-2 d-1 for FLUXCOM -- converted below)
days_lut <- tibble(
  month = 1:12,
  days  = c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
)

era5_site <- read_csv(file.path(site_csv_dir, "era5_fluxnet.csv"),
                      show_col_types = FALSE) |>
  rename(era5 = aet) |>
  left_join(fdk_filter |> select(sitename,
                                 year_start = year_start_le,
                                 year_end   = year_end_le),
            by = "sitename") |>
  filter(year(date) >= year_start, year(date) <= year_end) |>
  select(sitename, date, era5)

pml_site <- read_csv(file.path(site_csv_dir, "PML_fluxnet.csv"),
                     show_col_types = FALSE) |>
  mutate(month = month(date)) |>
  left_join(days_lut, by = "month") |>
  mutate(pml = aet / days) |>   # mm month-1 -> mm d-1
  left_join(fdk_filter |> select(sitename,
                                 year_start = year_start_le,
                                 year_end   = year_end_le),
            by = "sitename") |>
  filter(year(date) >= year_start, year(date) <= year_end) |>
  select(sitename, date, pml)

fluxcom_site <- read_csv(file.path(site_csv_dir, "fluxcom_fluxnet.csv"),
                         show_col_types = FALSE) |>
  rename(le = le_MJ_day) |>
  mutate(le = le * 1e6 / 86400) |>   # MJ m-2 d-1 -> W m-2
  left_join(
    driver_forcing |>
      select(sitename, date, temp, patm) |>
      mutate(date = as.Date(date)),
    by = c("sitename", "date")
  ) |>
  drop_na(temp, patm) |>
  mutate(fluxcom = le_to_et(pick(le, temp, patm))) |>
  left_join(fdk_filter |> select(sitename,
                                 year_start = year_start_le,
                                 year_end   = year_end_le),
            by = "sitename") |>
  filter(year(date) >= year_start, year(date) <= year_end) |>
  select(sitename, date, fluxcom)

# ── Merge all site-level columns ──────────────────────────────────────────
monthly_site_df <- fluxnet_monthly |>
  select(sitename, date, obs_aet) |>
  left_join(era5_site,    by = c("sitename", "date")) |>
  left_join(pml_site,     by = c("sitename", "date")) |>
  left_join(fluxcom_site, by = c("sitename", "date")) |>
  left_join(rsofun_site,  by = c("sitename", "date"))

message("  Site-level monthly dataset: ",
        nrow(monthly_site_df), " rows, ",
        n_distinct(monthly_site_df$sitename), " sites")

# =========================================================================
# 4. Save assembled evaluation dataset
# =========================================================================

eval_dataset <- list(
  global_products = global_products,
  p_model         = p_model,
  fluxnet_monthly = monthly_site_df
)

out_path <- file.path(proc_dir, "global_eval_dataset.rds")
saveRDS(eval_dataset, out_path)

message("\nSaved: ", out_path)
message("Global products : ", paste(unique(global_products$source), collapse = ", "))
message("P-model cells   : ", n_distinct(paste(p_model$lon, p_model$lat)))
message("FLUXNET sites   : ", n_distinct(monthly_site_df$sitename))
message("Period          : ", YEAR_START, "-", YEAR_END)
message("ET unit         : mm d-1 (monthly mean throughout)")
