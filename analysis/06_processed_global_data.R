# PURPOSE:
#   Extract monthly ET from global ET products (PML, GLEAM, FLUXCOM-X)
#   at selected FLUXNET sites for benchmarking against FluxDataKit observations:
#     - site selection and valid years from data/fluxnet/sites_years.csv
#     - site coordinates from data/fluxnet/fdk_site_info.csv
#     - monthly mean ET in mm d-1
#     - extraction at the native spatial resolution of each product
#
# OUTPUT:
#   Processed monthly ET at FLUXNET sites for PML, GLEAM, and FLUXCOM-X.
#
# NOTE:
#   Raw global ET products are stored on the workstation under data/archive/eval_rsofun_et/.
#   This script replaces the full-grid processing in 06_a_processed_global_data.R
#   with site-based extraction for the FluxDataKit benchmark.


library(dplyr)
library(tidyr)
library(purrr)
library(lubridate)
library(readr)
library(stringr)
library(terra)
library(here)

# -------------------------------------------------------------------------
# 1. Site information
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
  left_join(
    site_info |>
      select(sitename, lon, lat),
    by = "sitename"
  )

stopifnot(
  !anyNA(sites$lon),
  !anyNA(sites$lat)
)

site_vect <- vect(
  sites,
  geom = c("lon", "lat"),
  crs = "EPSG:4326"
)

# -------------------------------------------------------------------------
# 2. PML
# -------------------------------------------------------------------------

# read yearly files
# extract ETa at sites
# mm month-1 -> mm d-1
# filter to site-specific year_start/year_end

# -------------------------------------------------------------------------
# 3. GLEAM
# -------------------------------------------------------------------------

# read yearly files
# extract E at sites
# mm month-1 -> mm d-1
# filter to site-specific year_start/year_end

# -------------------------------------------------------------------------
# 4. FLUXCOM-X
# -------------------------------------------------------------------------

# read yearly files
# extract ET at sites
# mm h-1 -> mm d-1 (* 24)
# filter to site-specific year_start/year_end

# -------------------------------------------------------------------------
# 5. Save
# -------------------------------------------------------------------------
