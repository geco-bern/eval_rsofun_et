library(readr)
library(dplyr)
library(tidyr)
library(terra)
library(lubridate)
library(ingestr) # uses ingestr branch 'shapefile'
library(stringr)
library(here)
library(purrr)

source(here("R/get_driver_bycatchment.R"))
source(here("R/calc_vpd_td.R"))
source(here("R/extract_fapar_byfile.R"))
source(here("R/interpolate2daily_fapar.R"))

# data preparation, currently only on my folder

# will take at random 10 catchements and see what happens

# vector_path <- "~/data_scratch/shapefiles/camels/camels_basin_shapes.shp" # Francesco
# vector_path <- "~/Downloads/basin_set_full_res/HCDN_nhru_final_671.shp" # from Zenodo https://doi.org/10.5065/D6MW2F4D
vector_path <- "/data/archive_projects/eval_rsofun_et/data/caravan/Caravan-csv/shapefiles/camels/camels_basin_shapes.shp"
basin_shapes <- terra::vect(vector_path)

# catchmentinfo <- read_csv("~/data_scratch/camels_timeseries/camels/attributes_other_camels.csv")
catchmentinfo <- read_csv("/data/archive_projects/eval_rsofun_et/data/caravan/Caravan-csv/attributes/camels/attributes_other_camels.csv")
catchmentinfo_caravan <- read_csv("/data/archive_projects/eval_rsofun_et/data/caravan/Caravan-csv/attributes/camels/attributes_caravan_camels.csv")
catchmentinfo_hydroatlas <- read_csv("/data/archive_projects/eval_rsofun_et/data/caravan/Caravan-csv/attributes/camels/attributes_hydroatlas_camels.csv")

# combine all meta info
catchmentinfo <- catchmentinfo |>
  left_join(
    catchmentinfo_caravan,
    by = join_by(gauge_id)
  ) |>
  left_join(
    catchmentinfo_hydroatlas,
    by = join_by(gauge_id)
  )

# subset to those for which we have a shapefile (in fact keeps all)
catchmentinfo <- catchmentinfo |>
  filter(gauge_id %in% basin_shapes$gauge_id)

## Create driver -------------------------
### Common objects
# Take 20 years, but subset to years for which runoff data is available, done
# in get_driver_bycatchment().
date_start <- lubridate::ymd(paste0(2001, "-01-01"))
date_end <- lubridate::ymd(paste0(2020, "-12-31"))

### CO2 ---------
df_co2 <- ingest_bysite(
  sitename = "dummy",
  source = "co2_mlo",
  year_start = lubridate::year(date_start),
  year_end = lubridate::year(date_end),
  verbose = FALSE
)

### Loop over files (catchments)
# read list of all available CSV files from US-CAMELS
file_list <- list.files(
  path = "/data/archive_projects/eval_rsofun_et/data/caravan/Caravan-csv/timeseries/csv/camels/",
  pattern = "\\.csv$",
  full.names = TRUE
)

driver_camels <- purrr::map_dfr(
  file_list[1:3],
  ~ get_driver_bycatchment(., catchmentinfo)
)

# visdat::vis_miss(driver_camels$forcing[[2]])

## WHC ------------
path <- "/data/archive/whc_stocker_2023/data/cwdx80_forcing.nc"
rasta_whc <- rast(path)

# reproject polygons to raster CRS
basin_shapes <- project(basin_shapes, crs(rasta_whc))

# computational improvement
rasta_whc <- crop(rasta_whc, basin_shapes)

# extract area-weighted means for ALL polygons at once
vec_whc <- extract(
  rasta_whc,
  basin_shapes,
  fun = mean,
  weights = TRUE,
  na.rm = TRUE
)

df_whc <- tibble(
  sitename = basin_shapes$gauge_id,
  whc = vec_whc[, 2]
)

# nest into driver object
driver_camels <- driver_camels |>
  unnest(site_info) |>
  left_join(
    df_whc,
    by = join_by(sitename)
  ) |>
  nest(site_info = c(lat, lon, elv, whc, canopy_height, reference_height))

## fAPAR ------------------
file_list_fapar <- list.files(
  path = "/data/archive/fparmodisv6.1_myneni_2021/data/",
  pattern = "\\.nc$",
  full.names = TRUE
)

# Loop over all available years and extract values for all basins at once
terraOptions(cores = 8) # to speed up, extracting is heavy
df_fapar <- map_dfr(
  file_list_fapar,
  ~ extract_fapar_byfile(., basin_shapes = basin_shapes)
)

write_rds(df_fapar, file = here("data/df_fapar_camels.rds"))

# linearly interpolate monthly fAPAR values to daily (code copied from grsofun_run())
dates <- unique(df_fapar$date)
year_start <- min(lubridate::year(dates))
year_end <- max(lubridate::year(dates))

# create a data frame that spans all dates between start and end of simulation
# consider only complete years
ddf <- dplyr::tibble(
  date = seq(
    from = lubridate::ymd(paste0(year_start, "-01-01")),
    to = lubridate::ymd(paste0(year_end, "-12-31")),
    by = "days"
  )) |>
  filter(!(month(date) == 2 & mday(date) == 29))

# function to linearly interpolate (leaves trailing NAs)
ddf_fapar <- df_fapar |>
  ungroup() |>
  group_by(gauge_id) |>
  nest() |>
  dplyr::mutate(data = purrr::map(data, ~interpolate2daily_fapar(., ddf))) |>
  dplyr::mutate(data = purrr::map(data, ~dplyr::select(., -fapar))) |>
  dplyr::mutate(data = purrr::map(data, ~dplyr::rename(., fapar = fapar_daily)))

write_rds(ddf_fapar, file = here("data/ddf_fapar_camels.rds"))

# test
ddf_fapar$data[[500]] |>
  ggplot(aes(date, fapar)) +
  geom_line()

# nest into driver object
driver_camels <- driver_camels |>
  unnest(cols = c(forcing)) |>
  select(-fapar) |> # was filled with NA before
  left_join(
    ddf_fapar |>
      rename(sitename = gauge_id) |>
      unnest(cols = c(data)),
    by = join_by(sitename, date)
  ) |>
  nest(forcing = c(date, temp, vpd, ppfd, netrad, patm, snow, rain, tmin, tmax, vwind, fapar, co2, ccov, runoff)) |>
  select(site_info, params_siml, forcing)

write_rds(driver_camels, file = here("data/driver_camels.rds"))



# ## Cloud coverage ---------
#
# df_cld <- ingestr:::extract_pointdata_allsites_shp(
#   filename = "/data/archive/cru_harris_2024/data/cru_ts4.08.1901.2023.cld.dat.nc",
#   df_shapefile = shapefile,
#   get_time = T,
#   year_arg = NA_integer_, month_arg = NA_integer_ # only used for WFDEI in combination with get_time
# )
#
# df_cld <- df_cld |>
#   unnest(data) |>
#   filter(varnam == "cld") |>
#   filter(date > date_start & date < date_end) |>
#   mutate(value = value / 100) |>
#   mutate(date = floor_date(date, "month"))
#
#
# df_cld <- df_cld |>
#   group_by(sitename) |>
#   mutate(date = purrr::map(date, ~ seq(.x, ceiling_date(.x, "month") - days(1), by = "day"))) |>
#   unnest(date) |>
#   unnest(sitename) |>
#   select(sitename, date, value) |>
#   rename(ccov = value)
#
# driver_data <- left_join(
#   driver_data |>
#     unnest(forcing),
#   df_cld,
#   by = c("sitename", "date")
# ) |>
#   nest(forcing = c(date, temp, vpd, ppfd, netrad, patm, snow, rain, tmin, tmax, vwind, co2, ccov, runoff))
#
#
# saveRDS(driver_data, "/data/archive_projects/eval_rsofun_et/data/camels_driver.rds")
