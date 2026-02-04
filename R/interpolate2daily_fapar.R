interpolate2daily_fapar <- function(df, ddf){
  ddf <- ddf |>
    dplyr::left_join(
      df,
      by = "date"
    ) |>
    dplyr::mutate(
      fapar_daily = zoo::na.approx(fapar, na.rm = FALSE)
    )

  # fill remaining with mean seasonal cycle
  meandf <- ddf |>
    dplyr::mutate(doy = lubridate::yday(date)) |>
    dplyr::group_by(doy) |>
    dplyr::summarise(fapar_meandoy = mean(fapar_daily, na.rm = TRUE))

  ddf <- ddf |>
    dplyr::mutate(doy = lubridate::yday(date)) |>
    dplyr::left_join(
      meandf,
      by = "doy"
    ) |>
    dplyr::mutate(fapar_daily = ifelse(is.na(fapar_daily), fapar_meandoy, fapar_daily)) |>
    dplyr::select(-fapar_meandoy, -doy)

  return(ddf)
}
