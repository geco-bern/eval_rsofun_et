# data-raw/05_process_global_et_products.R
#
# PURPOSE:
#   Process the three raw global ET products (ERA5, PML, FLUXCOM) from their
#   originally downloaded formats into a common tidy representation:
#     - monthly mean ET in mm d-1
#     - spatial resolution: 0.5° × 0.5°
#     - period: 1982–2011 (ERA5) / 1997–2011 (PML, FLUXCOM)
#     - format: nested tibble  {date <Date> | data <list<tibble(lon, lat, aet)>>}
#
# OUTPUT:
#   processed_global_data/monthly_era5.rds
#   processed_global_data/monthly_pml.rds
#   processed_global_data/monthly_fluxcom.rds
#
# NOTE:
#   Raw source data live on the workstation only (see README for download
#   instructions).  This script is committed for reproducibility but cannot be
#   run without access to the raw data archives.
#
# AUTHORS: Grossi et al. (in prep.)

library(dplyr)
library(tidyr)
library(lubridate)
library(purrr)
library(ncdf4)
library(terra)
library(here)

# Helper functions --------------------------------------------------------

source(here("R/calc_vpd_td.R"))   # provides le_to_et()

# Latent-heat / density helpers (also used by calc_vpd_td; kept here as
# self-contained fallback so this script can be sourced independently)

calc_enthalpy_vap <- function(tc) {
  1918460 * ((tc + 273.15) / (tc + 273.15 - 33.91))^2
}

calc_density_h2o <- function(tc, press) {
  po <- 0.99983952 +
    6.78826e-05  * tc -
    9.08659e-06  * tc^2 +
    1.02213e-07  * tc^3 -
    1.35439e-09  * tc^4 +
    1.47115e-11  * tc^5 -
    1.11663e-13  * tc^6 +
    5.04407e-16  * tc^7 -
    1.00659e-18  * tc^8
  ko <- 19652.17 +
    148.183  * tc -
    2.29995  * tc^2 +
    0.01281  * tc^3 -
    4.91564e-05 * tc^4 +
    1.03553e-07 * tc^5
  ca <- 3.26138 +
    0.0005223 * tc +
    0.0001324 * tc^2 -
    7.655e-07 * tc^3 +
    8.584e-10 * tc^4
  cb <- 7.2061e-05 -
    5.8948e-06 * tc +
    8.699e-08  * tc^2 -
    1.01e-09   * tc^3 +
    4.322e-12  * tc^4
  pbar <- 1e-05 * press
  1000 * po * (ko + ca * pbar + cb * pbar^2) /
    (ko + ca * pbar + cb * pbar^2 - pbar)
}

#' Convert latent-heat flux (W m-2, i.e. J s-1 m-2) to ET (mm d-1)
#'
#' @param df data.frame with columns `le` (W m-2), `temp` (°C), `patm` (Pa)
le_to_et_mmd <- function(df) {
  lambda <- calc_enthalpy_vap(df$temp)       # J kg-1
  rho_w  <- calc_density_h2o(df$temp, df$patm)  # kg m-3
  # ET [mm d-1] = LE [W m-2] / (lambda [J kg-1] * rho_w [kg m-3]) * 86400 [s d-1] * 1000 [mm m-1]
  1000 * 86400 * df$le / (lambda * rho_w)
}

# Target 0.5° template raster
template_05 <- function(source_rast) {
  rast(ext(source_rast), resolution = 0.5, crs = crs(source_rast))
}

#' Convert a single SpatRaster layer to a tidy tibble (lon, lat, aet)
rast_to_tibble <- function(r) {
  df <- terra::as.data.frame(r, xy = TRUE, na.rm = TRUE)
  colnames(df) <- c("lon", "lat", "aet")
  tibble(df)
}

# Output directory --------------------------------------------------------

out_dir <- here("processed_global_data")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# =========================================================================
# 1. ERA5  ----------------------------------------------------------------
# =========================================================================
#
# Source: ECMWF CDS — "surface latent heat flux", monthly averages
#   Variable: slhf  [J m-2]  — accumulated over the month (negative
#             because the sign convention is surface → atmosphere)
# Resolution: 0.1° (native); resampled to 0.5° here
# Period:     1982-01 to 2011-12  (360 months)
# Download:   see README (cdsapi Python snippet)

message("Processing ERA5 ...")

era5_path <- "~/data_scratch/big_data/data_stream-moda.nc"

era5_raw <- terra::rast(era5_path)

# ── Fix longitude wrapping (grid starts at 0°, not -180°) ─────────────────
# Pixels 181–360° → shift to -179–0°; pixels 0–179° → keep as 0–179°
ext_east <- ext(181, 360, ymin(era5_raw), ymax(era5_raw))
ext_west <- ext(  0, 179.95, ymin(era5_raw), ymax(era5_raw))

era5_east <- shift(crop(era5_raw, ext_east), dx = -360)  # now -179 – 0°
era5_west <- crop(era5_raw, ext_west)                    # 0 – 179.95°
era5_360  <- terra::merge(era5_east, era5_west)

# ── Resample to 0.5° ───────────────────────────────────────────────────────
era5_05 <- resample(era5_360, template_05(era5_360), method = "average")

# ── Date vector ────────────────────────────────────────────────────────────
months_era5 <- seq(as.Date("1982-01-01"), as.Date("2011-12-01"), by = "month")
stopifnot(nlyr(era5_05) == length(months_era5))

# ── Convert units and build nested tibble ─────────────────────────────────
# ERA5 slhf: J m-2 per month (accumulated, negative upward)
# → W m-2 (mean flux): divide by seconds-in-month, multiply by -1
# → mm d-1: apply le_to_et_mmd() requires temp & patm
#
# A simpler approximation used here because no ERA5 temperature / pressure
# layer is extracted in this script:
#   slhf [J m-2 month] / (lambda_water * rho_water) → m month-1
# Using standard values lambda ≈ 2.45e6 J kg-1, rho ≈ 1000 kg m-3:
#   aet [mm d-1] = -slhf * 1000 / (2.45e6 * 1000 * days_in_month)
# For a more accurate conversion (temperature-dependent), supply a climate
# file and uncomment the le_to_et_mmd() pathway below.

days_in_month_vec <- days_in_month(months_era5)   # lubridate helper

monthly_era5 <- map2(
  seq_along(months_era5), months_era5,
  function(i, d) {
    tb <- rast_to_tibble(era5_05[[i]]) |>
      mutate(
        # slhf is negative (upward) J m-2; invert and convert to mm d-1
        aet = (-aet) / (2.45e6 * 1000 * days_in_month_vec[i])  # m d-1
        ,aet = aet * 1000                                        # mm d-1
      )
    tibble(date = d, data = list(tb))
  }
) |>
  list_rbind()

saveRDS(monthly_era5, file.path(out_dir, "monthly_era5.rds"))
message("  Saved: processed_global_data/monthly_era5.rds  (",
        nrow(monthly_era5), " months)")

# =========================================================================
# 2. PML  -----------------------------------------------------------------
# =========================================================================
#
# Source: Zhang et al. (2016) via CSIRO — monthly ETa, 0.5° global
#   Variable: ETa  [mm month-1]
#   Period:   1997–2011 (note: no good-quality data before 1997)
#   Files:    Monthly_PML_ETa_<year>.nc  (one file per year)
#
# Convert mm month-1 → mm d-1 by dividing by days-in-month.

message("Processing PML ...")

pml_dir  <- "/data/archive_projects/eval_rsofun_et/PML/data"
pml_years <- seq(1997, 2011)

# ── Read all yearly files and concatenate ──────────────────────────────────
# NOTE: in the original raster x/y axes are transposed — apply flip/trans/flip
pml_rast <- map(pml_years, function(yr) {
  r <- terra::rast(file.path(pml_dir, paste0("Monthly_PML_ETa_", yr, ".nc")))
  r <- flip(trans(flip(r)))                     # correct axis transposition
  names(r) <- seq(
    as.Date(paste0(yr, "-01-01")),
    as.Date(paste0(yr, "-12-01")),
    by = "month"
  )
  r
}) |>
  reduce(c)   # stack into single SpatRaster

months_pml <- seq(as.Date("1997-01-01"), as.Date("2011-12-01"), by = "month")
stopifnot(nlyr(pml_rast) == length(months_pml))

days_in_month_pml <- days_in_month(months_pml)

monthly_pml <- map2(
  seq_along(months_pml), months_pml,
  function(i, d) {
    tb <- rast_to_tibble(pml_rast[[i]]) |>
      mutate(aet = aet / days_in_month_pml[i])  # mm month-1 → mm d-1
    tibble(date = d, data = list(tb))
  }
) |>
  list_rbind()

saveRDS(monthly_pml, file.path(out_dir, "monthly_pml.rds"))
message("  Saved: processed_global_data/monthly_pml.rds  (",
        nrow(monthly_pml), " months)")

# =========================================================================
# 3. FLUXCOM  -------------------------------------------------------------
# =========================================================================
#
# Source: RS+METEO ensemble (Tramontana et al. / Jung et al.) via MPI-BGC FTP
#   Variable: LE  [MJ m-2 d-1]   (mean daily latent heat flux per month)
#   Period:   1997–2011
#   Files:    one .nc per year, containing LE only for months 1–12
#
# Conversion: LE [MJ m-2 d-1] → LE [W m-2] → ET [mm d-1]
#   Requires monthly mean temperature and atmospheric pressure for the
#   latent-heat / density correction; these are read from a pre-processed
#   climate file (see processed_global_data/monthly_tmp_patm.rds).
#
# If the climate file is not available, fall back to fixed constants
# (lambda = 2.45e6 J kg-1, rho = 1000 kg m-3 → ET ≈ LE * 86400 / 2450).

message("Processing FLUXCOM ...")

fluxcom_dir  <- "/data/archive_projects/eval_rsofun_et/FLUXCOM/ensemble"
fluxcom_yrs  <- seq(1997, 2011)

# Climate file for temperature-dependent LE → ET conversion
climate_path <- file.path(out_dir, "monthly_tmp_patm.rds")
has_climate  <- file.exists(climate_path)

if (has_climate) {
  temp_patm <- readRDS(climate_path)
  # expected structure: nested tibble {date | data<tibble(lon, lat, temp, patm)>}
  message("  Using temperature-dependent LE→ET conversion (monthly_tmp_patm.rds)")
} else {
  warning("monthly_tmp_patm.rds not found; using fixed lambda/rho for LE→ET. ",
          "Results may differ slightly from temperature-corrected values.")
}

# ── List LE files (one per year) ──────────────────────────────────────────
all_files <- list.files(fluxcom_dir, full.names = TRUE)
le_files  <- all_files[grepl("LE", all_files)]

read_fluxcom_year <- function(yr) {
  f <- le_files[grepl(as.character(yr), le_files)]
  if (length(f) == 0) stop("No FLUXCOM LE file found for year ", yr)
  r <- terra::rast(f)
  r[[1:12]]   # keep only the 12 monthly layers
}

fluxcom_rast <- map(fluxcom_yrs, read_fluxcom_year) |> reduce(c)

months_fluxcom <- seq(as.Date("1997-01-01"), as.Date("2011-12-01"), by = "month")
stopifnot(nlyr(fluxcom_rast) == length(months_fluxcom))
names(fluxcom_rast) <- as.character(months_fluxcom)

# ── Convert LE → ET ───────────────────────────────────────────────────────
monthly_fluxcom <- map2(
  seq_along(months_fluxcom), months_fluxcom,
  function(i, d) {
    tb <- rast_to_tibble(fluxcom_rast[[i]]) |>
      # MJ m-2 d-1 → W m-2  (divide by 86400 s d-1, multiply by 1e6 J MJ-1)
      mutate(le = aet * 1e6 / 86400)

    if (has_climate) {
      clim_i <- temp_patm |>
        filter(date == d) |>
        pull(data) |>
        pluck(1)

      tb <- tb |>
        mutate(lonlat = paste0(lon, lat)) |>
        left_join(
          clim_i |> mutate(lonlat = paste0(lon, lat)) |>
            select(lonlat, temp, patm),
          by = "lonlat"
        ) |>
        drop_na(temp, patm) |>
        mutate(aet = le_to_et_mmd(pick(le, temp, patm))) |>
        select(lon, lat, aet)
    } else {
      # Fixed-constant fallback: ET [mm d-1] ≈ LE [W m-2] * 86400 / 2.45e6 * 1
      tb <- tb |>
        mutate(aet = le * 86400 / 2.45e6 * 1000) |>  # mm d-1
        select(lon, lat, aet)
    }

    tibble(date = d, data = list(tb))
  }
) |>
  list_rbind()

saveRDS(monthly_fluxcom, file.path(out_dir, "monthly_fluxcom.rds"))
message("  Saved: processed_global_data/monthly_fluxcom.rds  (",
        nrow(monthly_fluxcom), " months)")

message("\nAll global ET products processed successfully.")
message("Unit: mm d-1 (monthly mean actual evapotranspiration)")
message("Resolution: 0.5° × 0.5°")
