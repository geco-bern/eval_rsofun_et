library(readr)
library(dplyr)
library(tidyr)
library(terra)
library(lubridate)
library(ingestr) # uses ingestr branch 'shapefile'
library(stringr)

# data preparation, currently only on my folder

# will take at random 10 catchements and see what happens

# vector_path <- "~/data_scratch/shapefiles/camels/camels_basin_shapes.shp" # Francesco
# vector_path <- "~/Downloads/basin_set_full_res/HCDN_nhru_final_671.shp" # from Zenodo https://doi.org/10.5065/D6MW2F4D
vector_path <- "/data/archive_projects/eval_rsofun_et/data/caravan/Caravan-csv/shapefiles/camels/camels_basin_shapes.shp"

shapefile <- terra::vect(vector_path)

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

# subset to those for which we have a shapefile
catchmentinfo <- catchmentinfo[catchmentinfo$gauge_id %in% shapefile$gauge_id, ]

# # this takes lon and lat of each catchment as its outlet
# catchmentinfo <- catchmentinfo |>
#   dplyr::mutate(date_start = lubridate::ymd(paste0(1990, "-01-01"))) |>
#   dplyr::mutate(date_end = lubridate::ymd(paste0(2020, "-12-31"))) |>
#   rename(
#     lon = gauge_lon,
#     lat = gauge_lat
#   )

## CO2 ---------
df_co2 <- ingest_bysite(
  sitename = "anyone",
  source = "co2_mlo",
  year_start = lubridate::year(date_start),
  year_end = lubridate::year(date_end),
  verbose = FALSE
)

# # remove 29-02
# df_co2 <- df_co2 |>
#   filter(!(month(date) == 2 & day(date) == 29))

# read list of all available CSV files from US-CAMELS
file_list <- list.files(
  path = "/data/archive_projects/eval_rsofun_et/data/caravan/Caravan-csv/timeseries/csv/camels/",
  pattern = "\\.csv$",
  full.names = TRUE
  )

# get gauge id from file name
site_nr <- basename(file_list[1]) |>
  str_remove("camels_") |>
  str_remove(".csv")

sitename <- paste0("camels_", site_nr)

catchmentinfo |>
  dplyr::filter(gauge_id == sitename) |>
  select(lat = gauge_lat, lon = gauge_lon) |>
  mutate(
    canopy_height = 12, # TODO
    reference_height = 10
  ) |> # I select 10 beacuse the wind velocity is measured at 10 m
  nest(site_info = c(lat, lon, canopy_height, reference_height))

date_start <- lubridate::ymd(paste0(1990, "-01-01"))
date_end <- lubridate::ymd(paste0(2020, "-12-31"))

forcing <- read_csv(file_list[1]) |>
  filter(lubridate::date(date) >= date_start, lubridate::date(date) <= date_end) |>
  filter(!(month(date) == 2 & day(date) == 29)) |>
  rename(
    runoff = streamflow,
    tmin = temperature_2m_min,
    tmax = temperature_2m_max,
    temp = temperature_2m_mean
    ) |>
  mutate(
    netrad = surface_net_solar_radiation_mean + surface_net_thermal_radiation_mean,
    vwind = sqrt(u_component_of_wind_10m_mean^2 + v_component_of_wind_10m_mean^2),
    rain = ifelse(temp >= 1, total_precipitation_sum / (24 * 60 * 60), 0),
    snow = ifelse(temp < 1, total_precipitation_sum / (24 * 60 * 60), 0),
    co2 = NA # test
  ) |>
  # left_join(
  #   df_co2,
  #   by = join_by(date)
  # )
  select(date, temp, netrad, snow, rain, tmin, tmax, vwind, co2, runoff) |>
  nest(forcing = c(date, temp, netrad, snow, rain, tmin, tmax, vwind, co2, runoff))


timeseries <- timeseries[substr(timeseries, 60, 74) %in% catchmentinfo$gauge_id]

driver_data <- NULL

for (i in timeseries) {
  sitename <- substr(i, 60, 74)

  params_siml <- rsofun::p_model_drivers$params_siml[[1]] |>
    mutate(use_gs = TRUE, use_phydro = FALSE, use_pml = TRUE) |>
    nest(params_siml = c(
      spinup, spinupyears, recycle, outdt, ltre, ltne, ltrd, ltnd,
      lgr3, lgn3, lgr4,
      use_gs, use_phydro, use_pml
    ))



  tmp <- data.frame(
    sitename = sitename,
    params_siml = tibble(params_siml),
    site_info = tibble(site_info),
    forcing = tibble(forcing)
  )

  driver_data <- rbind(driver_data, tmp)
}



df_out <- tibble()

getvars <- c("vpd", "ppfd")

source <- "watch_wfdei"

dir <- "/data/archive/wfdei_weedon_2014/data/"

timescale <- "d"

df_out <- ingest_globalfields(
  catchmentinfo = catchmentinfo, source = source, getvars = getvars,
  dir = dir, timescale = timescale, is_shapefile = T, shapefile = shapefile
)

driver_data <-
  left_join(
    driver_data |>
      unnest(forcing),
    df_out |>
      dplyr::select(sitename, date, ppfd, patm, vpd),
    by = c("sitename", "date")
  ) |>
  nest(forcing = c(date, temp, vpd, ppfd, netrad, patm, snow, rain, tmin, tmax, vwind, co2, runoff))

## WHC -------

# based on shapefile

df_out <- tibble()

df_out <- ingest_globalfields(
  catchmentinfo = catchmentinfo, source = source, getvars = getvars,
  dir = dir, timescale = timescale, is_shapefile = T, shapefile = shapefile
)

driver_data <-
  left_join(
    driver_data |>
      unnest(site_info),
    df_out,
    by = "sitename"
  ) |>
  nest(site_info = c(lat, lon, whc, canopy_height, reference_height))

print(df_out)


## Elevation --------

df_out <- tibble()

source <- "etopo1"

dir <- "/data/archive/etopo_NA_NA/data/"

df_out <- ingest_globalfields(
  catchmentinfo = catchmentinfo, source = source, getvars = getvars,
  dir = dir, timescale = timescale, is_shapefile = T, shapefile = shapefile
)


driver_data <-
  left_join(
    driver_data |>
      unnest(site_info),
    df_out,
    by = "sitename"
  ) |>
  nest(site_info = c(lat, lon, elv, whc, canopy_height, reference_height))




## Cloud coverage ---------

df_cld <- ingestr:::extract_pointdata_allsites_shp(
  filename = "/data/archive/cru_harris_2024/data/cru_ts4.08.1901.2023.cld.dat.nc",
  df_shapefile = shapefile,
  get_time = T,
  year_arg = NA_integer_, month_arg = NA_integer_ # only used for WFDEI in combination with get_time
)

df_cld <- df_cld |>
  unnest(data) |>
  filter(varnam == "cld") |>
  filter(date > date_start & date < date_end) |>
  mutate(value = value / 100) |>
  mutate(date = floor_date(date, "month"))


df_cld <- df_cld |>
  group_by(sitename) |>
  mutate(date = purrr::map(date, ~ seq(.x, ceiling_date(.x, "month") - days(1), by = "day"))) |>
  unnest(date) |>
  unnest(sitename) |>
  select(sitename, date, value) |>
  rename(ccov = value)

driver_data <- left_join(
  driver_data |>
    unnest(forcing),
  df_cld,
  by = c("sitename", "date")
) |>
  nest(forcing = c(date, temp, vpd, ppfd, netrad, patm, snow, rain, tmin, tmax, vwind, co2, ccov, runoff))


## fAPAR --------

# (ask if is correct) -- No, should take it from MODIS

files <- list.files("/data/archive/fapar3g_zhu_2013/data/")

year_seq <- seq(substr(date_start, 1, 4), substr(date_end, 1, 4), 1)

by_weekly <- c(
  "-01-01", "-01-14", "-02-01", "-02-14", "-03-01", "-03-14",
  "-04-01", "-04-14", "-05-01", "-05-14", "-06-01", "-06-14",
  "-07-01", "-07-14", "-08-01", "-08-14", "-09-01", "-09-14",
  "-10-01", "-10-14", "-11-01", "-11-14", "-12-01", "-12-14"
)


df_tmp <- NULL


for (j in year_seq) {
  file_to_extract <- grep(j, files)

  df_final <- NULL
  # extract yearly fapar

  for (i in 1:24) {
    df_fapar <- ingestr:::extract_pointdata_allsites_shp(
      filename = paste0("/data/archive/fapar3g_zhu_2013/data/", files[file_to_extract[i]]),
      df_shapefile = shapefile,
      get_time = F,
      year_arg = NA_integer_, month_arg = NA_integer_ # only used for WFDEI in combination with get_time
    )

    df_fapar <- df_fapar |>
      unnest(data) |>
      rename(fapar = colnames(df_fapar |> unnest(data))[2])
    df_fapar$date <- lubridate::as_date(paste0(j, by_weekly[i]))

    df_final <- rbind(df_final, df_fapar)
  }
  # linearly extend from biweekly to daily

  for (k in unique(df_final$sitename)) {
    tmp <- df_final[df_final$sitename == k, ]

    tmp <- data.table::setorder(tmp, date)

    linear_fapar <- approx(tmp$date, tmp$fapar, method = "linear", n = 25 * 14 - 2)

    linear_fapar <- data.frame(
      date = lubridate::as_date(linear_fapar$x),
      fapar = linear_fapar$y
    )


    # calculate the fapar from 12-14 to 12-31
    slope <- (tmp$fapar[24] - tmp$fapar[23]) / 14

    step <- seq(1, 17, 1)

    fapar <- tmp$fapar[24] + slope * step

    daily_fapar <- c(linear_fapar$fapar, fapar)

    days <- seq(lubridate::as_date(paste0(j, "-01-01")), lubridate::as_date(paste0(j, "-12-31")), by = "day")

    # remove 29-02

    if (length(grep("02-29", days)) != 0) {
      days <- days[-(grep("02-29", days))]
    }


    tmp_df <- data.frame(sitename = k, date = days, fapar = daily_fapar)


    df_tmp <- rbind(df_tmp, tmp_df)
  }
}


# file_to_extract <- grep(year_seq,files)
#
#
# df_final <- NULL
#
# for(i in 1:24){
#     df_fapar <-  ingestr:::extract_pointdata_allsites_shp(
#   filename = paste0("/data/archive/fapar3g_zhu_2013/data/",files[file_to_extract[i]]),
#   df_shapefile = shapefile,
#   get_time = F,
#   year_arg = NA_integer_, month_arg = NA_integer_ # only used for WFDEI in combination with get_time
#   )
#
#     df_fapar <- df_fapar|> unnest(data) |> rename(fapar = colnames(df_fapar|> unnest(data))[2])
#     df_fapar$date <- lubridate::as_date(paste0(year_seq[1],by_weekly[i]))
#
#     df_final <- rbind(df_final,df_fapar)
# }
#
# tmp <- df_final |> arrange(df_final, sitename)



# df_tmp <- NULL
#
#
# for(i in unique(df_final$sitename)){
#   tmp <- df_final[df_final$sitename == i,]
#
#   tmp <- data.table::setorder(tmp,date)
#
#   linear_fapar <-approx(tmp$date,tmp$fapar,method = 'linear',n = 25*14 - 2)
#
#   linear_fapar <- data.frame(date = lubridate::as_date(linear_fapar$x),
#                            fapar =  linear_fapar$y)
#
#   slope <-(tmp$fapar[24] - tmp$fapar[23]) / 14
#
#   step <- seq(1,17,1)
#
#   fapar <- tmp$fapar[24] + slope * step
#
#   daily_fapar <- c(linear_fapar$fapar,fapar)
#
#   days <- seq(lubridate::as_date("2005-01-01"),lubridate::as_date("2005-12-31"), by = "day")
#
#   tmp_df <- data.frame(sitename = i, date = days, fapar = daily_fapar)
#
#
#   df_tmp <- rbind(df_tmp,tmp_df)
# }


driver_data <- left_join(
  driver_data |>
    unnest(forcing),
  df_tmp,
  by = c("sitename", "date")
) |>
  nest(forcing = c(date, temp, vpd, ppfd, netrad, patm, snow, rain, tmin, tmax, vwind, fapar, co2, ccov, runoff))


saveRDS(driver_data, "/data/archive_projects/eval_rsofun_et/data/camels_driver.rds")
