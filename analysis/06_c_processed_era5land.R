# PURPOSE:
#   Extract monthly mean ERA5-Land ET at selected FLUXNET sites.
#   Read daily tot_e NetCDF files (2000-2026) directly from UBELIX,
#   convert accumulated daily evaporation (m water equivalent) to
#   positive ET (mm d-1), and average by calendar month.
#
# INPUT:
#   data/fluxnet/fdk_site_info.csv   (sitename, lon, lat)
#   data/fluxnet/sites_years.csv     (sitename, year_start, year_end)
#   UBELIX: era5-land_UTCDaily.tot_e.YYYY.nc
#
# OUTPUT:
#   data/monthly_era5land.rds
#   Columns: sitename, date (first of month), aet (mm d-1)
#
# ASSUMPTIONS TO VERIFY:
#   Each NetCDF timestep is a complete UTC-day accumulation in metres,
#   with the ERA5-Land sign convention (evaporation negative).
#   The header alone cannot prove how the daily aggregation was made.
#   Incomplete months or site-months with missing daily values are omitted.

library(dplyr)
library(tidyr)
library(lubridate)
library(purrr)
library(readr)
library(stringr)
library(terra)
library(here)
library(ncdf4)

# -------------------------------------------------------------------------
# Sites: same input and filtering as 06_d_processed_global_data.R
# -------------------------------------------------------------------------
site_info <- read_csv(
  here("data", "fluxnet", "fdk_site_info.csv"),
  show_col_types = FALSE
)
sites_years <- read_csv(
  here("data", "fluxnet", "sites_years.csv"),
  show_col_types = FALSE
)

sites <- sites_years |>
  left_join(site_info |> select(sitename, lon, lat), by = "sitename")

sites_invalid_coords <- sites |>
  filter(is.na(lon) | is.na(lat) | lon < -180 | lon > 180 |
           lat < -90 | lat > 90)
if (nrow(sites_invalid_coords) > 0) {
  message("Removing ", nrow(sites_invalid_coords), " invalid site coordinate(s):")
  print(sites_invalid_coords |> select(sitename, lon, lat))
}
sites <- sites |>
  filter(!is.na(lon), !is.na(lat),
         between(lon, -180, 180), between(lat, -90, 90))
stopifnot(nrow(sites) > 0)

# ERA5-Land uses longitudes 0..360, unlike sites (-180..180).
# Convert point longitudes, not the very large raster.
sites_era5 <- sites |>
  mutate(lon_era5 = if_else(lon < 0, lon + 360, lon))
sites_vect <- terra::vect(sites_era5, geom = c("lon_era5", "lat"), crs = "EPSG:4326")

# -------------------------------------------------------------------------
# Paths
# -------------------------------------------------------------------------
era5land_dir <- paste0(
  "/storage/capacity/occr_geco/data_2/scratch/fbernhard/",
  "ERA5Land-T_2000-2026_run2026-07-16/aggregated_UTCDaily"
)
out_dir <- here("data")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

files <- list.files(
  era5land_dir,
  pattern = "^era5-land_UTCDaily\\.tot_e\\.[0-9]{4}\\.nc$",
  full.names = TRUE
)
stopifnot(length(files) > 0)
files <- sort(files)
message("Found ", length(files), " ERA5-Land annual file(s).")

# -------------------------------------------------------------------------
# Read actual CF time axis; do not assume all files have 365/366 days.
# Expected format in supplied header:
#   valid_time:units = "days since 2001-01-01 12:00:00"
# -------------------------------------------------------------------------
read_daily_dates <- function(file) {
  nc <- ncdf4::nc_open(file)
  on.exit(ncdf4::nc_close(nc))
  att <- ncdf4::ncatt_get(nc, "valid_time", "units")
  if (!isTRUE(att$hasatt)) stop("Missing valid_time units in: ", file)
  units <- att$value
  if (!grepl("^days since [0-9]{4}-[0-9]{2}-[0-9]{2}", units)) {
    stop("Unsupported time units in ", basename(file), ": ", units)
  }
  origin <- as.Date(sub("^days since ([0-9]{4}-[0-9]{2}-[0-9]{2}).*$", "\\1", units))
  offsets <- ncdf4::ncvar_get(nc, "valid_time")
  # Date labels are valid for the supplied 12:00 UTC daily axis.
  if (any(abs(offsets - round(offsets)) > 1e-8)) {
    stop("Non-integer daily offsets in ", basename(file), "; inspect timestamps")
  }
  origin + as.integer(round(offsets))
}

filter_valid_years <- function(df) {
  df |>
    left_join(sites |> select(sitename, year_start, year_end), by = "sitename") |>
    filter(year(date) >= year_start, year(date) <= year_end) |>
    select(-year_start, -year_end)
}

# -------------------------------------------------------------------------
# Process one annual file at a time; do not load the global raster into RAM.
# -------------------------------------------------------------------------
process_year <- function(file) {
  message("Processing ERA5-Land: ", basename(file))
  dates <- read_daily_dates(file)

  # Explicitly select tot_e (the NetCDF variable in the supplied header).
  r <- terra::rast(file, subds = "tot_e")
  if (terra::nlyr(r) != length(dates)) {
    stop("Layer/time mismatch in ", basename(file), ": ",
         terra::nlyr(r), " raster layers vs ", length(dates), " dates")
  }
  if (anyDuplicated(dates)) stop("Duplicate dates in ", basename(file))
  if (is.unsorted(dates, strictly = TRUE)) stop("Dates not increasing in ", basename(file))

  # Return one row per site, one column per day (plus ID).
  vals <- terra::extract(r, sites_vect)
  vals <- as.matrix(vals[, -1, drop = FALSE])
  if (ncol(vals) != length(dates) || nrow(vals) != nrow(sites)) {
    stop("Unexpected extraction dimensions in ", basename(file))
  }

  # Matrix -> long format; columns correspond to dates, rows to sites.
  daily <- tibble(
    sitename = rep(sites$sitename, times = length(dates)),
    date = rep(dates, each = nrow(sites)),
    aet = as.vector(vals) * (-1000)
  )

  # Monthly mean daily ET. A complete calendar month must be present in
  # the file and each site's daily values must all be non-missing.
  daily |>
    mutate(date = floor_date(date, unit = "month")) |>
    group_by(sitename, date) |>
    summarise(
      n_days = n(),
      n_valid = sum(!is.na(aet)),
      aet = if (all(!is.na(aet))) mean(aet) else NA_real_,
      .groups = "drop"
    ) |>
    filter(n_days == days_in_month(date), n_valid == n_days) |>
    select(sitename, date, aet)
}

monthly_era5land <- purrr::map_dfr(files, process_year) |>
  filter_valid_years() |>
  arrange(sitename, date)

if (anyDuplicated(monthly_era5land[c("sitename", "date")])) {
  stop("Duplicate site-month rows: check overlapping input files")
}

output_file <- file.path(out_dir, "monthly_era5land.rds")
readr::write_rds(monthly_era5land, output_file)

message("Finished processing ERA5-Land.")
message("Sites: ", dplyr::n_distinct(monthly_era5land$sitename))
message("Site-months: ", nrow(monthly_era5land))
message("Output: ", output_file)
print(dplyr::slice_head(monthly_era5land, n = 10))
