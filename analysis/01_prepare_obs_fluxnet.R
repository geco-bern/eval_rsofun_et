# Observational data preparation -----------------------------------------------
# Adopted from sofunCalVal/data-raw/01_generate_rsofun_eval_data.R
#
# Generates obs_eval: a list of data frames aggregated to different temporal
# resolutions (annual "adf", monthly "mdf", x-daily "xdf", daily "ddf"),
# plus a "breaks" element for the x-daily aggregation.
#
# Each data frame has sites along rows, site meta-info columns, and a nested
# "data" column with time series (date, gpp, gpp_qc, le, le_qc).
#
# Outputs:
#   data/obs_eval_fluxnet.rds  – FLUXNET site-level observed GPP and LE
#   data/obs_eval_camels.rds   – CAMELS catchment-level AET

## Library and data loading ----------------------------------------------------
library(tidyverse)

## FLUXNET ---------------------------------------------------------------------
fdk_site_info <- read_csv(here::here("data/fluxnet/fdk_site_info.csv"),
                          show_col_types = FALSE)

# Read rsofun driver data, created with applying additional filters in
# analysis/00_prepare_forcing_fluxnet.R
driver <- read_rds(here::here("data/driver.rds")) |>
  dplyr::select(-params_siml) |>
  tidyr::unnest(site_info) |>
  dplyr::rename(data = forcing) |>
  dplyr::mutate(data = purrr::map(data, ~dplyr::select(., date, gpp, gpp_qc, le, le_qc))) |>

  # complement with additional site meta info and make consistent
  left_join(
    fdk_site_info |>
      dplyr::select(sitename, koeppen_code, igbp_land_use),
    by = "sitename"
  )

### Local functions ------------------------------------------------------------
agg_monthly <- function(df){
  df |>
    mutate(
      year = year(date),
      month = month(date)
    ) |>
    group_by(year, month) |>
    summarise(
      gpp = mean(gpp),
      gpp_qc = mean(gpp_qc),
      le = mean(le),
      le_qc = mean(le_qc),
      .groups = "drop"
    )
}

agg_annual <- function(df){
  df |>
    mutate(
      year = year(date)
    ) |>
    group_by(year) |>
    summarise(
      gpp = mean(gpp),
      gpp_qc = mean(gpp_qc),
      le = mean(le),
      le_qc = mean(le_qc),
      .groups = "drop"
    )
}

agg_xdaily <- function(df, breaks){

  df |>
    mutate(bin = cut(
      date,
      breaks = breaks,
      include.lowest = TRUE,
      right = FALSE)
    ) |>
    group_by(bin) |>
    summarise(
      gpp = mean(gpp),
      gpp_qc = mean(gpp_qc),
      le = mean(le),
      le_qc = mean(le_qc),
      .groups = "drop"
    )
}

binwidth <- 8
breaks <- seq(
  from = lubridate::ymd("1990-01-01"),
  to = lubridate::ymd("2024-12-31"),
  by = paste0(binwidth, " days")
)

### construct object -----------------------------------------------------------
obs_eval <- list(

  # daily - unchanged
  ddf = driver,

  # monthly
  mdf = driver |>
    mutate(data = purrr::map(data, ~agg_monthly(.))),

  # annual
  adf = driver |>
    mutate(data = purrr::map(data, ~agg_annual(.))),

  # x-daily
  xdf = driver |>
    mutate(data = purrr::map(data, ~agg_xdaily(., breaks = breaks))),

  # breaks of x-daily aggregation
  breaks = breaks
)

write_rds(obs_eval, file = here::here("data/obs_eval_fluxnet.rds"))

## CAMELS ----------------------------------------------------------------------
# this code is copied from R/create_obs_eval.R

site_info <- driver |>
  unnest(site_info)

# for now, only aet is evaluated
if (runoff == "daily") {

  adf_tibble_preparation <- driver |>
    unnest(forcing) |>
    mutate(year = lubridate::floor_date(date, "year")) |>
    ungroup() |>
    dplyr::select(c(sitename, year, rain, runoff)) |>
    mutate(aet = (rain * 60 * 60 * 24) - runoff) |> # identical as sum and then subtract
    group_by(sitename, year) |>
    summarise(aet = sum(aet, na.rm = T)) |>
    rename(date = year) |>
    dplyr::select(c(sitename, date, aet)) |>
    nest()

} else if (runoff == "yearly") {

  adf_tibble_preparation <- left_join(
    driver |>
      unnest(forcing) |>
      mutate(year = lubridate::floor_date(date, "year")) |>
      ungroup() |>
      dplyr::select(c(sitename, year, rain)) |>
      mutate(rain = (rain * 60 * 60 * 24)) |> # identical as sum and then subtract
      group_by(sitename, year) |>
      summarise(rain = sum(rain, na.rm = T)),
    attributes_hydroatlas_camels |>
      dplyr::select(sitename, run_mm_syr),
    by = "sitename"
  ) |>
    rename(runoff = run_mm_syr) |>
    mutate(aet = rain - runoff) |>
    rename(date = year) |>
    dplyr::select(c(sitename, date, aet)) |>
    nest()
}

adf_tibble_preparation <- adf_tibble_preparation$data

obs_eval$adf <- tibble(
  sitename = site_info$sitename,
  lon = site_info$lon,
  lat = site_info$lat,
  elv = site_info$elv,
  classid = NA,
  whc = site_info$whc,
  koeppen_code = NA,
  igbp_land_use = NA,
  plant_functioanl_type = NA,
  c4 = FALSE,
  data = adf_tibble_preparation
)

write_rds(obs_eval, file = here::here("data/obs_eval_camels.rds"))
