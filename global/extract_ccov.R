library(readr)
library(terra)
library(lubridate)
library(tictoc)
library(dplyr)
library(tidyr)

global_site_info <- read_csv("~/data_scratch/global/global_site_info.csv")


date_start = lubridate::ymd(paste0(1982, "-01-01"))
date_end = lubridate::ymd(paste0(2011, "-12-31"))

df_lonlat <- global_site_info |>
  dplyr::mutate(date_start = date_start) |>
  dplyr::mutate(date_end = date_end) 

df_vap <- ingestr:::extract_pointdata_allsites(
  filename = "/data/archive/cru_harris_2024/data/cru_ts4.08.1901.2023.vap.dat.nc",
  df_lonlat = df_lonlat,
  get_time = T,
  year_arg = NA_integer_, month_arg = NA_integer_ # only used for WFDEI in combination with get_time
)

df_vap <- df_vap |> unnest(data) |>
  filter(varnam == "vap") |>
  filter(date > date_start & date < date_end) |>
  mutate(value = value /100) |>
  mutate(date = floor_date(date, "month")) |>
  rename(ccov = value) |>
  select(-varnam)


write_csv(df_vap,"../global/df_ccov2.csv")
