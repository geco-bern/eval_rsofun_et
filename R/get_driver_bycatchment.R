get_driver_bycatchment <- function(path, catchmentinfo){

  # get gauge id from file name
  site_nr <- basename(path) |>
    str_remove("camels_") |>
    str_remove(".csv")

  sitename <- paste0("camels_", site_nr)

  ### Site info --------
  site_info <- catchmentinfo |>
    dplyr::filter(gauge_id == sitename) |>

    # this takes lon and lat of each catchment as its outlet
    select(
      lat = gauge_lat,
      lon = gauge_lon,
      elv = ele_mt_sav
      ) |>

    # selected 10 because the wind velocity is measured at 10 m
    mutate(
      canopy_height = 12, # TODO
      reference_height = 10
    ) |>

    # nest to make rsofun driver-like
    nest(site_info = c(lat, lon, elv, canopy_height, reference_height))

  ### Forcing time series ---------
  # interpret variables and convert units. CARAVAN variables described in Tab. 1
  # of Kratzert et al. 2023 10.1038/s41597-023-01975-w
  kfFEC <- 2.04  # energy-mass conversion for photons, micro-mol/J (Meek et al., 1984)

  forcing <- read_csv(path) |>
    filter(lubridate::date(date) >= date_start, lubridate::date(date) <= date_end) |>
    filter(!(month(date) == 2 & day(date) == 29)) |>
    rename(
      runoff = streamflow,
      tmin = temperature_2m_min,
      tmax = temperature_2m_max,
      temp = temperature_2m_mean
    ) |>
    mutate(
      vpd = calc_vpd_td(temp, dewpoint_temperature_2m_mean),  # Pa
      patm = surface_pressure_mean * 1e3,  # kPa -> Pa
      ppfd = kfFEC * 1e-6 * surface_net_solar_radiation_mean,  # W m-2 -> mol m-2 s-1
      netrad = surface_net_solar_radiation_mean + surface_net_thermal_radiation_mean,  # W m-2
      vwind = sqrt(u_component_of_wind_10m_mean^2 + v_component_of_wind_10m_mean^2),  # m s-1
      rain = ifelse(temp >= 1, total_precipitation_sum / (24 * 60 * 60), 0),  # mm d-1 -> mm s-1
      snow = ifelse(temp < 1, total_precipitation_sum / (24 * 60 * 60), 0),  # mm d-1 -> mm s-1
      fapar = NA,
      ccov = NA
    ) |>
    left_join(
      df_co2 |>
        select(-sitename),
      by = join_by(date)
    ) |>
    select(date, temp, vpd, ppfd, netrad, patm, snow, rain, tmin, tmax, vwind, fapar, co2, ccov, runoff) |>
    nest(forcing = c(date, temp, vpd, ppfd, netrad, patm, snow, rain, tmin, tmax, vwind, fapar, co2, ccov, runoff))

  # determine years for which runoff data is available (use only complete years)
  years_avl_runoff <- forcing |>
    unnest(forcing) |>
    mutate(year = year(date)) |>
    group_by(year) |>
    summarise(n_runoffdata = sum(!is.na(runoff))) |>
    filter(n_runoffdata >= 365) |>
    pull(year)

  # subset to years will full runoff data availability
  forcing <- forcing |>
    mutate(forcing = purrr::map(forcing, ~dplyr::filter(., year(date) %in% years_avl_runoff)))

  ### Simulation parameters ------------
  params_siml <- rsofun::p_model_drivers$params_siml[[1]] |>
    mutate(use_gs = TRUE, use_phydro = FALSE, use_pml = TRUE) |>
    nest(params_siml = everything())

  # combine to driver-type row
  out <- tibble(sitename = sitename) |>
    bind_cols(site_info, params_siml, forcing)

  return(out)
}
