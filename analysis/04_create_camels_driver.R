# analysis/04_create_camels_driver.R
#
# PURPOSE:
#   Build the complete rsofun P-model driver for all CAMELS-US catchments.
#   This script consolidates three previously separate scripts:
#     - 04_create_camels_driver.R    (forcing via ingestr)
#     - add_netrad_to_camels.R       (net radiation from WFDEI)
#     - prepare_driver_from_shapefile_multy_year.R  (prototype manual pipeline)
#
# PIPELINE OVERVIEW:
#   1. Load Caravan-CAMELS shapefiles and catchment attributes
#   2. Build per-catchment forcing via ingestr (temp, vpd, ppfd, patm, rain,
#      co2, ccov, runoff) and extract WHC and fAPAR from gridded archives
#   3. Extract net radiation (SW + LW) from WFDEI and fill the netrad column
#   4. Save final driver to data/driver_camels.rds
#
# INPUTS (workstation only -- see README for download instructions):
#   data-raw/caravan/          Caravan-CAMELS shapefiles, attributes, time series
#   data-raw/whc_stocker_2023/ cwdx80_forcing.nc  (Stocker et al. 2023 WHC)
#   data-raw/fparmodisv6.1/    MODIS MOD15A2 fPAR NetCDF files (Myneni 2021)
#   data-raw/wfdei/SWdown_daily/  WFDEI SW downwelling monthly NetCDFs
#   data-raw/wfdei/LWdown_daily/  WFDEI LW downwelling monthly NetCDFs
#
# OUTPUTS:
#   data/driver_camels.rds          Final driver with all forcing columns filled
#   data/df_fapar_camels.rds        Monthly fAPAR per catchment (intermediate)
#   data/ddf_fapar_camels.rds       Daily fAPAR per catchment (intermediate)
#   data/camels/extraction_sw_lw.csv  Raw SW+LW extraction (cached)
#
# FORCING COLUMN NOTES:
#   netrad  — filled from WFDEI SW + LW (Section 3)
#   snow    — set to 0; Caravan CAMELS CSVs do not provide a snow column
#   ppfd    — ingested via ingestr; rsofun can compute it internally if NA
#   fapar   — MODIS MOD15A2, monthly → daily interpolation (Section 2c)
#
# AUTHORS: Grossi et al. (in prep.)

library(readr)
library(dplyr)
library(tidyr)
library(terra)
library(lubridate)
library(ingestr)   # branch 'shapefile' required
library(stringr)
library(here)
library(purrr)
library(ggplot2)

source(here("R/get_driver_bycatchment.R"))
source(here("R/calc_vpd_td.R"))
source(here("R/extract_fapar_byfile.R"))
source(here("R/interpolate2daily_fapar.R"))

# ── Paths ──────────────────────────────────────────────────────────────────
caravan_dir <- here("data-raw/caravan")   # adjust if stored elsewhere
wfdei_sw    <- here("data-raw/wfdei/SWdown_daily/")
wfdei_lw    <- here("data-raw/wfdei/LWdown_daily/")

dir.create(here("data/camels"), showWarnings = FALSE, recursive = TRUE)

# =========================================================================
# 1. Caravan-CAMELS shapefiles and catchment attributes
# =========================================================================

message("Loading Caravan-CAMELS shapefiles and attributes ...")

basin_shapes <- terra::vect(
  file.path(caravan_dir, "shapefiles/camels/camels_basin_shapes.shp")
)

catchmentinfo <- read_csv(
  file.path(caravan_dir, "attributes/camels/attributes_other_camels.csv"),
  show_col_types = FALSE
) |>
  left_join(
    read_csv(file.path(caravan_dir, "attributes/camels/attributes_caravan_camels.csv"),
             show_col_types = FALSE),
    by = join_by(gauge_id)
  ) |>
  left_join(
    read_csv(file.path(caravan_dir, "attributes/camels/attributes_hydroatlas_camels.csv"),
             show_col_types = FALSE),
    by = join_by(gauge_id)
  ) |>
  filter(gauge_id %in% basin_shapes$gauge_id)

# =========================================================================
# 2a. Per-catchment forcing via ingestr
# =========================================================================
# get_driver_bycatchment() ingests: temp, tmin, tmax, vpd, ppfd, patm, rain,
# co2, ccov, runoff from the Caravan CSV and gridded products via ingestr.
# snow is set to 0 (not in Caravan CAMELS); netrad is NA placeholder.

message("Building per-catchment forcing (ingestr pipeline) ...")

date_start <- lubridate::ymd("2001-01-01")
date_end   <- lubridate::ymd("2020-12-31")

df_co2 <- ingest_bysite(
  sitename   = "dummy",
  source     = "co2_mlo",
  year_start = lubridate::year(date_start),
  year_end   = lubridate::year(date_end),
  verbose    = FALSE
)

file_list <- list.files(
  path       = file.path(caravan_dir, "timeseries/csv/camels/"),
  pattern    = "\\.csv$",
  full.names = TRUE
)

driver_camels <- purrr::map_dfr(
  file_list,
  ~ get_driver_bycatchment(., catchmentinfo)
)

# =========================================================================
# 2b. Water-holding capacity (WHC)
# =========================================================================
# Stocker et al. (2023) cwdx80 raster at 0.05 deg.

message("Extracting WHC ...")

rasta_whc <- rast(here("data-raw/whc_stocker_2023/cwdx80_forcing.nc"))

basin_shapes_whc <- project(basin_shapes, crs(rasta_whc))
rasta_whc        <- crop(rasta_whc, basin_shapes_whc)

vec_whc <- extract(rasta_whc, basin_shapes_whc,
                   fun = mean, weights = TRUE, na.rm = TRUE)

df_whc <- tibble(
  sitename = basin_shapes$gauge_id,
  whc      = vec_whc[, 2]
)

driver_camels <- driver_camels |>
  unnest(site_info) |>
  left_join(df_whc, by = join_by(sitename)) |>
  nest(site_info = c(lat, lon, elv, whc, canopy_height, reference_height))

rm(rasta_whc, basin_shapes_whc, vec_whc, df_whc)

# =========================================================================
# 2c. fAPAR (MODIS MOD15A2, Myneni 2021)
# =========================================================================
# Monthly NetCDF files are extracted per basin and interpolated to daily.

message("Extracting and interpolating fAPAR ...")

file_list_fapar <- list.files(
  path       = here("data-raw/fparmodisv6.1/"),
  pattern    = "\\.nc$",
  full.names = TRUE
)

terraOptions(cores = 8)

df_fapar <- map_dfr(
  file_list_fapar,
  ~ extract_fapar_byfile(., basin_shapes = basin_shapes)
)

write_rds(df_fapar, file = here("data/df_fapar_camels.rds"))

# Interpolate monthly -> daily, dropping 29 Feb
dates_fapar <- unique(df_fapar$date)
ddf_span <- dplyr::tibble(
  date = seq(
    from = lubridate::ymd(paste0(min(lubridate::year(dates_fapar)), "-01-01")),
    to   = lubridate::ymd(paste0(max(lubridate::year(dates_fapar)), "-12-31")),
    by   = "days"
  )
) |>
  filter(!(month(date) == 2 & mday(date) == 29))

ddf_fapar <- df_fapar |>
  ungroup() |>
  group_by(gauge_id) |>
  nest() |>
  dplyr::mutate(data = purrr::map(data, ~ interpolate2daily_fapar(., ddf_span))) |>
  dplyr::mutate(data = purrr::map(data, ~ dplyr::select(., -fapar))) |>
  dplyr::mutate(data = purrr::map(data, ~ dplyr::rename(., fapar = fapar_daily)))

write_rds(ddf_fapar, file = here("data/ddf_fapar_camels.rds"))

# Sanity check: inspect fAPAR time series for first catchment
ddf_fapar$data[[1]] |>
  ggplot(aes(date, fapar)) +
  geom_line() +
  labs(title = paste("fAPAR:", ddf_fapar$gauge_id[[1]]))

# Merge daily fAPAR into driver
driver_camels <- driver_camels |>
  unnest(cols = c(forcing)) |>
  select(-fapar) |>   # remove NA placeholder from ingestr step
  left_join(
    ddf_fapar |>
      rename(sitename = gauge_id) |>
      unnest(cols = c(data)),
    by = join_by(sitename, date)
  ) |>
  nest(forcing = c(date, temp, vpd, ppfd, netrad, patm, snow, rain,
                   tmin, tmax, vwind, fapar, co2, ccov, runoff)) |>
  select(site_info, params_siml, forcing)

rm(df_fapar, ddf_fapar, ddf_span, dates_fapar, file_list_fapar)

# =========================================================================
# 3. Net radiation from WFDEI (SW + LW downwelling)
# =========================================================================
# WFDEI monthly NetCDF files, one per month, daily layers inside each file.
# Files are named e.g. SWdown_daily_WFDEI_CRU_199001.nc  (chars 20-23 = year,
# 24-25 = month). Grids are stored bottom-up and are flipped on read.
#
# net_rad = SW_down + LW_down  [W m-2]
# This is a downwelling approximation; upwelling terms are not available in
# WFDEI. Consistent with the approach in the original add_netrad_to_camels.R.

message("Extracting net radiation from WFDEI ...")

sw_files_all <- list.files(wfdei_sw, full.names = FALSE)
lw_files_all <- list.files(wfdei_lw, full.names = FALSE)

yrs_sw   <- as.integer(substr(sw_files_all, 20, 23))
sw_files <- sw_files_all[yrs_sw >= 1990 & yrs_sw <= 2010]
lw_files <- lw_files_all[yrs_sw >= 1990 & yrs_sw <= 2010]

# Assign sequential IDs matching the order of basin_shapes
shapefile_rad        <- terra::vect(
  file.path(caravan_dir, "shapefiles/camels/camels_basin_shapes.shp")
)
shapefile_rad$ID <- seq_len(nrow(shapefile_rad))

extract_weighted_all <- function(ncfile, poly) {
  r <- rast(ncfile)
  r <- flip(r, "vertical")   # WFDEI grids are stored bottom-up
  r <- crop(r, poly)
  extract(r, poly, fun = mean, na.rm = TRUE, exact = TRUE)
}

final_results_SW <- vector("list", length(sw_files))
final_results_LW <- vector("list", length(lw_files))

for (i in seq_along(sw_files)) {

  year  <- substr(sw_files[i], 20, 23)
  month <- substr(sw_files[i], 24, 25)

  # SW
  out_sw   <- extract_weighted_all(file.path(wfdei_sw, sw_files[i]), shapefile_rad)
  out_long <- out_sw |>
    pivot_longer(starts_with("SW"), names_to = "layer", values_to = "SW")
  n_days <- n_distinct(out_long$layer)
  dates  <- seq(as.Date(paste0(year, "-", month, "-01")),
                length.out = n_days, by = "1 day")
  out_long$date  <- rep(dates, nrow(shapefile_rad))
  out_long$layer <- NULL
  final_results_SW[[i]] <- out_long

  # LW (same date vector as SW)
  out_lw   <- extract_weighted_all(file.path(wfdei_lw, lw_files[i]), shapefile_rad)
  out_long <- out_lw |>
    pivot_longer(starts_with("LW"), names_to = "layer", values_to = "LW")
  out_long$date  <- rep(dates, nrow(shapefile_rad))
  out_long$layer <- NULL
  final_results_LW[[i]] <- out_long

  if (i %% 12 == 0)
    message("  Radiation: processed ", i, " / ", length(sw_files), " months")
}

sw_lw <- bind_rows(final_results_SW) |>
  mutate(LW = bind_rows(final_results_LW)$LW) |>
  arrange(ID)

write_csv(sw_lw, here("data/camels/extraction_sw_lw.csv"))
message("  Raw SW+LW extraction cached to data/camels/extraction_sw_lw.csv")

sw_lw <- sw_lw |>
  mutate(net_rad = SW + LW) |>
  filter(!(month(date) == 2 & mday(date) == 29))   # drop 29 Feb

rm(final_results_SW, final_results_LW, out_sw, out_lw,
   out_long, shapefile_rad, sw_files, lw_files,
   sw_files_all, lw_files_all, yrs_sw)

# ── Merge net radiation into driver ───────────────────────────────────────
driver_camels <- driver_camels |>
  unnest(forcing) |>
  mutate(netrad = sw_lw$net_rad) |>
  nest(forcing = c(date, temp, vpd, ppfd, netrad, patm, snow, rain,
                   tmin, tmax, vwind, fapar, co2, ccov, runoff)) |>
  select(site_info, params_siml, forcing)

rm(sw_lw)

# =========================================================================
# 4. Save final driver
# =========================================================================

write_rds(driver_camels, file = here("data/driver_camels.rds"))

message("\nDone. Final driver saved to data/driver_camels.rds")
message("  Catchments : ", nrow(driver_camels))
message("  Columns    : sitename, site_info, params_siml, forcing")
message("  forcing    : date, temp, tmin, tmax, vpd, ppfd, netrad, patm,")
message("               snow, rain, vwind, fapar, co2, ccov, runoff")

# =========================================================================
# APPENDIX: Manual extraction pipeline (pre-ingestr reference)
# =========================================================================
# The functions below were used in the prototype script
# (data-raw/prepare_driver_from_shapefile_multy_year.R) before the ingestr-
# based pipeline was adopted. They are kept here as a fallback in case
# ingestr is unavailable or a variable needs to be re-extracted manually.
#
# They are NOT called by the main pipeline above.

# ── calculate_patm() ────────────────────────────────────────────────────────
# Barometric formula: atmospheric pressure from elevation and temperature.
# Equivalent to calc_patm() in R/calc_vpd_td.R.
#
# calculate_patm <- function(elevation, temperature_list) {
#   P0 <- 101325; L <- 0.0065; g <- 9.80665
#   m  <- 0.0289644; R <- 8.3144598; T0 <- 273.15
#   for (i in seq_along(temperature_list)) {
#     elv  <- elevation[i]
#     temp <- temperature_list[[i]]$tmean
#     patm <- P0 * (1 - (L * elv) / (T0 + temp))^((g * m) / (R * L))
#     temperature_list[[i]] <- temperature_list[[i]] |>
#       mutate(patm = patm) |>
#       select(date, patm)
#   }
#   temperature_list
# }

# ── calculate_vp() ──────────────────────────────────────────────────────────
# Vapour pressure from specific humidity (Qair) and atmospheric pressure.
#
# calculate_vp <- function(patm_list, vp_list) {
#   kR <- 8.3143; kMv <- 18.02; kMa <- 28.963
#   rv <- kR / kMv; rd <- kR / kMa
#   for (i in seq_along(vp_list)) {
#     tmp  <- left_join(vp_list[[i]], patm_list[[i]], by = "date")
#     wair <- tmp$humidity / (1 - tmp$humidity)
#     tmp$vp <- tmp$patm * wair * rv / (rd + wair * rv)
#     patm_list[[i]] <- tmp |> select(date, vp, coverage_fraction)
#   }
#   patm_list
# }

# ── calculate_vpd() ─────────────────────────────────────────────────────────
# VPD from vapour pressure and min/max temperature (Tetens formula).
#
# calculate_vpd <- function(vp_list, temp_list) {
#   for (i in seq_along(vp_list)) {
#     tmp  <- left_join(vp_list[[i]], temp_list[[i]], by = "date")
#     esat_min <- 611 * exp((17.27 * tmp$tmin) / (tmp$tmin + 273.15))
#     esat_max <- 611 * exp((17.27 * tmp$tmax) / (tmp$tmax + 273.15))
#     tmp$vpd <- ((esat_min - tmp$vp) + (esat_max - tmp$vp)) / 2
#     vp_list[[i]] <- tmp |> select(date, vpd, coverage_fraction)
#   }
#   vp_list
# }

# ── extract_temperatures() ──────────────────────────────────────────────────
# Read temp, tmin, tmax, precipitation, streamflow from Caravan CSV files.
# Superseded by get_driver_bycatchment() which uses ingestr.
#
# extract_temperatures <- function(csv_dir, days) {
#   files <- list.files(csv_dir, full.names = TRUE)
#   map(files, function(f) {
#     csv <- read_csv(f, show_col_types = FALSE)
#     tibble(
#       date          = as_date(csv$date),
#       tmin          = csv$temperature_2m_min,
#       tmax          = csv$temperature_2m_max,
#       tmean         = csv$temperature_2m_mean,
#       precipitation = csv$total_precipitation_sum,
#       streamflow    = csv$streamflow
#     ) |>
#       filter(date %in% days)
#   })
# }
