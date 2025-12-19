create_obs_eval <- function(driver, fdk_site_info, target, scale = "fluxnet", runoff = "daily") {

  obs_eval <- list()

  if (scale == "fluxnet") {

    # daily data
    ddf_tibble_preparation <- driver |>
      unnest(forcing) |>
      ungroup() |>
      group_by(sitename) |>
      dplyr::select(c(date, all_of(target))) |>
      nest()

    ddf_tibble_preparation <- ddf_tibble_preparation$data

    obs_eval$ddf <- tibble(
      sitename = fdk_site_info$sitename,
      lon = fdk_site_info$lon,
      lat = fdk_site_info$lat,
      elv = fdk_site_info$elv,
      classid = fdk_site_info$igbp_land_use,
      whc = fdk_site_info$whc,
      koeppen_code = fdk_site_info$koeppen_code,
      igbp_land_use = fdk_site_info$igbp_land_use,
      plant_functioanl_type = NA,
      c4 = FALSE,
      data = ddf_tibble_preparation
    )

    # weekly data
    xdf_tibble_preparation <- driver |>
      unnest(forcing) |>
      mutate(week = lubridate::floor_date(date, "week")) |>
      ungroup() |>
      dplyr::select(c(sitename, week, all_of(target))) |>
      group_by(sitename, week) |>
      summarise_all(mean) |>
      mutate(inbin = as.factor(week), date = week) |>
      dplyr::select(c(date, inbin, all_of(target))) |>
      nest()

    xdf_tibble_preparation <- xdf_tibble_preparation$data

    obs_eval$xdf <- tibble(
      sitename = fdk_site_info$sitename,
      lon = fdk_site_info$lon,
      lat = fdk_site_info$lat,
      elv = fdk_site_info$elv,
      classid = fdk_site_info$igbp_land_use,
      whc = fdk_site_info$whc,
      koeppen_code = fdk_site_info$koeppen_code,
      igbp_land_use = fdk_site_info$igbp_land_use,
      plant_functioanl_type = NA,
      c4 = FALSE,
      data = xdf_tibble_preparation
    )

    # monthly data
    mdf_tibble_preparation <- driver |>
      unnest(forcing) |>
      mutate(month = lubridate::floor_date(date, "month")) |>
      ungroup() |>
      dplyr::select(c(sitename, month, all_of(target))) |>
      group_by(sitename, month) |>
      summarise_all(mean) |>
      rename(date = month) |>
      dplyr::select(c(sitename, date, all_of(target))) |>
      nest()

    mdf_tibble_preparation <- mdf_tibble_preparation$data

    obs_eval$mdf <- tibble(
      sitename = fdk_site_info$sitename,
      lon = fdk_site_info$lon,
      lat = fdk_site_info$lat,
      elv = fdk_site_info$elv,
      classid = fdk_site_info$igbp_land_use,
      whc = fdk_site_info$whc,
      koeppen_code = fdk_site_info$koeppen_code,
      igbp_land_use = fdk_site_info$igbp_land_use,
      plant_functioanl_type = NA,
      c4 = FALSE,
      data = mdf_tibble_preparation
    )

    # annual data

    adf_tibble_preparation <- driver |>
      unnest(forcing) |>
      mutate(year = lubridate::floor_date(date, "year")) |>
      ungroup() |>
      dplyr::select(c(sitename, year, all_of(target))) |>
      group_by(sitename, year) |>
      summarise_all(sum) |>
      rename(date = year) |>
      dplyr::select(c(sitename, date, all_of(target))) |>
      nest()

    adf_tibble_preparation <- adf_tibble_preparation$data

    obs_eval$adf <- tibble(
      sitename = fdk_site_info$sitename,
      lon = fdk_site_info$lon,
      lat = fdk_site_info$lat,
      elv = fdk_site_info$elv,
      classid = fdk_site_info$igbp_land_use,
      whc = fdk_site_info$whc,
      koeppen_code = fdk_site_info$koeppen_code,
      igbp_land_use = fdk_site_info$igbp_land_use,
      plant_functioanl_type = NA,
      c4 = FALSE,
      data = adf_tibble_preparation
    )

    breaks_preparation <- driver |>
      unnest(forcing) |>
      dplyr::select(date) |>
      arrange(date) |>
      distinct(date) |>
      mutate(breaks = lubridate::floor_date(date, "week"))

    obs_eval$breaks <- unique((breaks_preparation$breaks))[-1]

  } else if (scale == "camels") {

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
  } else {
    warning("no correct scale, select fluxnet or camels")
  }

  return(obs_eval)
}
