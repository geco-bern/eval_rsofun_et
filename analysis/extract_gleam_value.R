library(dplyr)
library(tidyr)
library(ggplot2)
library(ncdf4)
library(terra)
library(purrr)
library(readr)


files_path <- "~/data_scratch/monthly_mean_evapotranspiration_GLEAM/E/"

files <- list.files(files_path, full.names = TRUE)


fdk_site_info <- read_csv("./data/fluxnet/fdk_site_info.csv")


# data quality filter info
fdk_filter <-  read_csv("./data/fluxnet/fdk_site_fullyearsequence.csv")

# exclude croplands and wetlands from evaluation
fdk_site_info <- fdk_site_info[fdk_site_info$igbp_land_use != "CRO" &
                                 fdk_site_info$igbp_land_use != "WET",]

# apply good year sequence data quality filter for LE
fdk_filter <- fdk_filter[fdk_filter$drop_le == "FALSE",]

# exclude site outside time range (from 1996 to 2011)
# the first tower starts in 1996, so only crop years after 2011

fdk_filter <- fdk_filter[fdk_filter$year_start_le <= 2011,]

fdk_site_info <- fdk_site_info[fdk_site_info$sitename %in% fdk_filter$sitename,]

final_df <- NULL

for (j in 1:dim(fdk_site_info)[1]){

  lon <- fdk_site_info$lon[j]
  lat <- fdk_site_info$lat[j]

  pt <- cbind(lon, lat)

  df_full <- NULL

  for (i in files){

    r <- rast(i)

    val <- terra::extract(r, pt)

    tvec <- time(r)

    df <- val |>
      pivot_longer(cols = everything(),
                   names_to = "layer",
                   values_to = "aet") |>
      mutate(date = tvec) |>
      select(date, aet)

    df$sitename = fdk_site_info$sitename[j]

    df_full<- rbind(df_full,df)

  }

  print(fdk_site_info$sitename[j])

  final_df <- rbind(final_df,df_full)
}

write_csv(final_df,"./data/gleam_et_at_fluxnet.csv")






