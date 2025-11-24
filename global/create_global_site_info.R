
library(terra)



whc <- terra::rast("/data/archive/whc_stocker_2023/data/cwdx80_forcing.nc") 

whc_0.5 <- rast(ext(whc), resolution = 0.5, crs = crs(whc))

# Reproject the raster with bilinear interpolation (continuous data)
whc_0.5 <- resample(whc, whc_0.5, method = "average")


coords <- crds(whc_0.5, na.rm = TRUE) 

whc_arr <- values(whc_0.5, mat = FALSE, na.rm = TRUE)

coords <- tibble(lon = coords[,1], lat = coords[,2], whc = whc_arr)

points <- terra::vect(coords, geom = c("lon", "lat"), crs = "EPSG:4326")


rm(whc,whc_0.5)

gc()

canopy <- terra::rast("~/data_scratch/elevation_lang_005.tif") 

canopy_0.5 <- rast(ext(canopy), resolution = 0.5, crs = crs(canopy))

canopy_0.5 <- resample(canopy, canopy_0.5, method = "average")


vals <- terra::extract(canopy, points, xy = FALSE, ID = TRUE, method = "bilinear")


rm(canopy,canopy_0.5)

gc()

ok_whc <- tibble(canopy = vals$ETH_GlobalCanopyHeight_10m_2020_N00E006_Map)



elevation <- terra::rast("/data/archive/etopo_NA_NA/data/ETOPO1_Bed_g_geotiff.tif") 


elevation[elevation < 0] <- NA # to prevent sea give negative value during average

elevation_0.5 <- rast(ext(elevation), resolution = 0.5, crs = crs(elevation))

elevation_0.5 <- resample(elevation, elevation_0.5, method = "average")


elv <- terra::extract(elevation, points, xy = FALSE, ID = T, method = "bilinear")


rm(elevation,elevation_0.5)

gc()

ok_whc <- cbind(ok_whc,elv$ETOPO1_Bed_g_geotiff)


ok_whc <- cbind(coords,ok_whc)

ok_whc$canopy <- ifelse(is.na(ok_whc$canopy) | ok_whc$canopy <= 0.05,-1,ok_whc$canopy)

ok_whc2 <- ok_whc |>
  rename(elv = 'elv$ETOPO1_Bed_g_geotiff',
         canopy_height = canopy) |>
  mutate(reference_height = canopy_height) |>
  select(lon,lat,whc,elv,canopy_height,reference_height)

ok_whc2$sitename <- paste0(ok_whc2$lon,"_",ok_whc2$lat)

ok_whc2 <- ok_whc2 |> drop_na()

write_csv(ok_whc2,"../global_site_info.csv")

