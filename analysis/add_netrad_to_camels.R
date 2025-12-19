library(dplyr)
library(tidyr)
library(lubridate)
library(sf)
library(rsofun)
library(readr)
library(tictoc)
library(terra)
library(ingestr)

driver_data <- readRDS("./data/camels/camels_driver.rds")

meta_info  <- read_csv("./data/camels/camels_site_info.csv")

vector_path <- "./data/camels/CopyOfcamels/camels_basin_shapes.shp"

shapefile <- terra::vect(vector_path)

net_rad_path <- "/data/archive/wfdei_weedon_2014/data"

SW <- "/data/archive/wfdei_weedon_2014/data/SWdown_daily/"

LW <- "/data/archive/wfdei_weedon_2014/data/LWdown_daily/"

Tari <- "/data/archive/wfdei_weedon_2014/data/Tair_daily/"

SW_file <- list.files(SW)
LW_file <- list.files(LW)


siteinfo <- read.csv("~/data_scratch/big_data/Caravan-csv/Caravan/attributes/camels/attributes_other_camels.csv")

siteinfo <- siteinfo[siteinfo$gauge_id %in% shapefile$gauge_id,]
shapefile$ID <- 1:nrow(shapefile)


yrs <- as.numeric(substr(basename(SW_file), 20, 23))
sw_files <- SW_file[yrs >= 1990 & yrs <= 2010]
lw_files <- LW_file[yrs >= 1990 & yrs <= 2010]


extract_weighted_all <- function(ncfile, poly) {
  r <- rast(ncfile)
  r <- flip(r, "vertical")
  r <- crop(r, poly)
  extract(r, poly, fun = mean, na.rm = TRUE, exact = TRUE)
}

final_results_SW <- NULL
final_results_LW <- NULL


for (i in 1:length(sw_files)){

  file <- sw_files[i]
  year <- substr(file,20,23)
  month <- substr(file,24,25)
  out_list <- lapply(paste0(SW,sw_files[i]), extract_weighted_all, poly=shapefile)
  out <- bind_rows(out_list)
  out_long <- out %>%
    pivot_longer(
      cols = starts_with("SW"),
      names_to = "layer",
      values_to = "SW"
    )
  total_layers <- n_distinct(out_long$layer)
  dates <- seq(as.Date(paste0(year,"-",month,"-01")), length.out = total_layers, by = "1 day")
  out_long$date <- rep(dates,length(shapefile))
  out_long$layer <- NULL
  final_results_SW <- rbind(final_results_SW,out_long)

  # same but for LW
  file <- lw_files[i]
  year <- substr(file,20,23)
  month <- substr(file,24,25)
  out_list <- lapply(paste0(LW,lw_files[i]), extract_weighted_all, poly=shapefile)
  out <- bind_rows(out_list)
  out_long <- out %>%
    pivot_longer(
      cols = starts_with("LW"),
      names_to = "layer",
      values_to = "LW"
    )
  total_layers <- n_distinct(out_long$layer)
  dates <- seq(as.Date(paste0(year,"-",month,"-01")), length.out = total_layers, by = "1 day")
  out_long$date <- rep(dates,length(shapefile))
  out_long$layer <- NULL
  final_results_LW <- rbind(final_results_LW,out_long)

  print(paste0("done ",i))
}

LW <- final_results_LW$LW

sw_lw <- cbind(final_results_SW,LW)

write.table(sw_lw,"extraction_sw_lw.csv", row.names = F, col.names = T, sep = ",")

sw_lw <- read_csv("extraction_sw_lw.csv")


sw_lw <- sw_lw |> arrange(ID)

sw_lw$net_rad <- sw_lw$SW + sw_lw$LW

new_sw_lw <- sw_lw |>
  filter(!(month(date)== 2 & day(date) == 29))

net_rad <- new_sw_lw$net_rad

new_driver_data <- driver_data |>
  unnest(forcing)|>
  mutate(netrad = net_rad) |>
  nest(forcing = c( "date","temp","vpd","ppfd","netrad","patm","snow",
                    "rain","tmin","tmax","vwind","fapar","co2","ccov","runoff"))

saveRDS(new_driver_data,"./data/camels/new_driver_data.rds")

