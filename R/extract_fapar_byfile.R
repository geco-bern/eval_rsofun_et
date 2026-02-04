extract_fapar_byfile <- function(path, basin_shapes){

  # read as raster
  rasta <- rast(path)

  # reproject polygons to raster CRS
  basin_shapes <- project(basin_shapes, crs(rasta))

  # for performance
  rasta <- crop(rasta, basin_shapes)

  # extract area-weighted means for ALL polygons at once
  vals <- extract(
    rasta,
    basin_shapes,
    fun = mean,
    weights = TRUE,
    na.rm = TRUE
  ) |>
    mutate(gauge_id = basin_shapes$gauge_id) |>
    select(-ID)

  # Get time values from raster layers
  dates <- time(rasta)

  df <- vals |>
    pivot_longer(
      cols = -gauge_id,
      names_to = "layer",
      values_to = "fapar"
    ) |>
    mutate(date = rep(dates, nrow(basin_shapes))) |>
    select(gauge_id, date, fapar)

  return(df)
}
