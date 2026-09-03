# PURPOSE:
#   Extract monthly ET from global ET products (PML, GLEAM, FLUXCOM-X)
#   at selected FLUXNET sites for benchmarking against FluxDataKit observations:
#     - site selection and valid years from data/fluxnet/sites_years.csv
#     - site coordinates from data/fluxnet/fdk_site_info.csv
#     - monthly mean ET converted to mm d-1
#     - extraction at the native spatial resolution of each product
#
# OUTPUT:
#   processed_global_data/monthly_pml.rds
#   processed_global_data/monthly_gleam.rds
#   processed_global_data/monthly_fluxcomx.rds
#
# NOTE:
#   Raw global ET products are stored on the workstation under
#   /data/archive/.
#   In contrast to the previous workflow, this script extracts values only
#   at the selected FluxDataKit sites instead of processing the full grids.

library(dplyr)
library(tidyr)
library(lubridate)
library(purrr)
library(readr)
library(stringr)
library(terra)
library(here)


# =========================================================================
# Site information
# =========================================================================

site_info <- read_csv(
  here("data", "fluxnet", "fdk_site_info.csv"),
  show_col_types = FALSE
)

sites_years <- read_csv(
  here("data", "fluxnet", "sites_years.csv"),
  show_col_types = FALSE
)

sites <- sites_years |>
  left_join(
    site_info |>
      select(sitename, lon, lat),
    by = "sitename"
  )

# Check coordinates and keep only sites with valid lon/lat
sites_invalid_coords <- sites |>
  filter(
    is.na(lon) |
      is.na(lat) |
      lon < -180 |
      lon > 180 |
      lat < -90 |
      lat > 90
  )

if (nrow(sites_invalid_coords) > 0) {
  message(
    "Removing ",
    nrow(sites_invalid_coords),
    " site(s) with missing or invalid coordinates:"
  )

  print(
    sites_invalid_coords |>
      select(sitename, lon, lat)
  )
}

sites <- sites |>
  filter(
    !is.na(lon),
    !is.na(lat),
    between(lon, -180, 180),
    between(lat, -90, 90)
  )

message(
  "Using ",
  nrow(sites),
  " sites with valid coordinates."
)

# Spatial points for raster extraction
sites_vect <- terra::vect(
  sites,
  geom = c("lon", "lat"),
  crs = "EPSG:4326"
)


# =========================================================================
# Paths
# =========================================================================

pml_dir <- "/data/archive/eval_rsofun_et/pml_zhang_2016/data"

gleam_dir <- "/data/archive/gleam_miralles_2025/monthly/E"

fluxcomx_dir <- "/data/archive/fluxcomX_nelson_2023"

out_dir <- "/data/archive_projects/MSc_2025_fgrossi_eval_rsofun_et/processed_global_data/"


# =========================================================================
# Helper
# =========================================================================

# Keep only site-months within the valid FluxDataKit period
filter_valid_years <- function(df) {

  df |>
    left_join(
      sites |>
        select(sitename, year_start, year_end),
      by = "sitename"
    ) |>
    filter(
      year(date) >= year_start,
      year(date) <= year_end
    ) |>
    select(-year_start, -year_end)
}


# =========================================================================
# PML
# =========================================================================
#
# Variable: ETa
# Units: mm month-1
# Files: Monthly_PML_ETa_<year>.nc
#
# Conversion:
#   mm month-1 / days in month = mm d-1

message("Processing PML ...")

pml_files <- list.files(
  pml_dir,
  pattern = "^Monthly_PML_ETa_[0-9]{4}\\.nc$",
  recursive = TRUE,
  full.names = TRUE
)

stopifnot(length(pml_files) > 0)

pml_file_info <- tibble(
  file = pml_files,
  year = as.integer(
    str_extract(
      basename(pml_files),
      "(?<=ETa_)[0-9]{4}"
    )
  )
) |>
  arrange(year)


monthly_pml <- map_dfr(
  seq_len(nrow(pml_file_info)),
  function(i) {

    file <- pml_file_info$file[i]
    yr   <- pml_file_info$year[i]

    message("  PML: ", yr)

    r <- terra::rast(file)

    # PML NetCDF is stored as time x longitude x latitude.
    # The original workflow required correcting the spatial axes.
    r <- terra::flip(r)
    r <- terra::trans(r)
    r <- terra::flip(r)

    stopifnot(terra::nlyr(r) == 12) # 12 months for each year, one layer per month

    dates <- seq(
      as.Date(paste0(yr, "-01-01")),
      as.Date(paste0(yr, "-12-01")),
      by = "month"
    )

    vals <- terra::extract(
      r,
      sites_vect
    )

    # First column returned by extract() is the point ID
    vals <- vals[, -1, drop = FALSE]

    as_tibble(vals) |>
      mutate(sitename = sites$sitename) |>
      pivot_longer(
        cols = -sitename,
        names_to = "layer",
        values_to = "aet"
      ) |>
      group_by(sitename) |>
      mutate(
        date = dates[row_number()]
      ) |>
      ungroup() |>
      select(sitename, date, aet) |>
      mutate(
        aet = aet / days_in_month(date)
      )
  }
) |>
  filter_valid_years()


write_rds(
  monthly_pml,
  file.path(out_dir, "monthly_pml.rds")
)


# =========================================================================
# GLEAM
# =========================================================================
#
# Variable: E
# Units: mm month-1
# Files: E_<year>_GLEAM_v4.3a_MO.nc
#
# Conversion:
#   mm month-1 / days in month = mm d-1

message("Processing GLEAM ...")

gleam_files <- list.files(
  gleam_dir,
  pattern = "^E_[0-9]{4}_GLEAM.*_MO\\.nc$",
  recursive = TRUE,
  full.names = TRUE
)

stopifnot(length(gleam_files) > 0)

gleam_file_info <- tibble(
  file = gleam_files,
  year = as.integer(
    str_extract(
      basename(gleam_files),
      "(?<=E_)[0-9]{4}"
    )
  )
) |>
  arrange(year)


monthly_gleam <- map_dfr(
  seq_len(nrow(gleam_file_info)),
  function(i) {

    file <- gleam_file_info$file[i]
    yr   <- gleam_file_info$year[i]

    message("  GLEAM: ", yr)

    # Explicitly select variable E
    r <- terra::rast(
      file,
      subds = "E"
    )

    stopifnot(terra::nlyr(r) == 12)

    dates <- seq(
      as.Date(paste0(yr, "-01-01")),
      as.Date(paste0(yr, "-12-01")),
      by = "month"
    )

    vals <- terra::extract(
      r,
      sites_vect
    )

    vals <- vals[, -1, drop = FALSE]

    as_tibble(vals) |>
      mutate(sitename = sites$sitename) |>
      pivot_longer(
        cols = -sitename,
        names_to = "layer",
        values_to = "aet"
      ) |>
      group_by(sitename) |>
      mutate(
        date = dates[row_number()]
      ) |>
      ungroup() |>
      select(sitename, date, aet) |>
      mutate(
        aet = aet / days_in_month(date)
      )
  }
) |>
  filter_valid_years()


write_rds(
  monthly_gleam,
  file.path(out_dir, "monthly_gleam.rds")
)


# =========================================================================
# FLUXCOM-X
# =========================================================================
#
# Variable: ET
# Units: mm hr-1
# Files: ET_<year>_005_monthly.nc
#
# FLUXCOM-X contains monthly mean hourly ET rates.
#
# Conversion:
#   mm hr-1 * 24 = mm d-1

message("Processing FLUXCOM-X ...")

fluxcomx_files <- list.files(
  fluxcomx_dir,
  pattern = "^ET_[0-9]{4}_005_monthly\\.nc$",
  recursive = TRUE,
  full.names = TRUE
)

stopifnot(length(fluxcomx_files) > 0)

fluxcomx_file_info <- tibble(
  file = fluxcomx_files,
  year = as.integer(
    str_extract(
      basename(fluxcomx_files),
      "(?<=ET_)[0-9]{4}"
    )
  )
) |>
  arrange(year)


monthly_fluxcomx <- map_dfr(
  seq_len(nrow(fluxcomx_file_info)),
  function(i) {

    file <- fluxcomx_file_info$file[i]
    yr   <- fluxcomx_file_info$year[i]

    message("  FLUXCOM-X: ", yr)

    r <- terra::rast(
      file,
      subds = "ET"
    )

    stopifnot(terra::nlyr(r) == 12)

    dates <- seq(
      as.Date(paste0(yr, "-01-01")),
      as.Date(paste0(yr, "-12-01")),
      by = "month"
    )

    vals <- terra::extract(
      r,
      sites_vect
    )

    vals <- vals[, -1, drop = FALSE]

    as_tibble(vals) |>
      mutate(sitename = sites$sitename) |>
      pivot_longer(
        cols = -sitename,
        names_to = "layer",
        values_to = "aet"
      ) |>
      group_by(sitename) |>
      mutate(
        date = dates[row_number()]
      ) |>
      ungroup() |>
      select(sitename, date, aet) |>
      mutate(
        aet = aet * 24
      )
  }
) |>
  filter_valid_years()


write_rds(
  monthly_fluxcomx,
  file.path(out_dir, "monthly_fluxcomx.rds")
)


# =========================================================================
# Checks
# =========================================================================

message("")
message("Finished processing global ET products.")

message(
  "PML:       ",
  n_distinct(monthly_pml$sitename),
  " sites, ",
  nrow(monthly_pml),
  " site-months"
)

message(
  "GLEAM:     ",
  n_distinct(monthly_gleam$sitename),
  " sites, ",
  nrow(monthly_gleam),
  " site-months"
)

message(
  "FLUXCOM-X: ",
  n_distinct(monthly_fluxcomx$sitename),
  " sites, ",
  nrow(monthly_fluxcomx),
  " site-months"
)
