library(readr)
library(terra)
library(dplyr)
library(tidyr)

df_output <- read_csv("~/data_scratch/global/output/combined.csv")



r <- rast(xmin = min(df_output$lon), xmax = max(df_output$lon),
          ymin = min(df_output$lat), ymax = max(df_output$lat),
          ncols = length(unique(df_output$lon))-1, nrows = length(unique(df_output$lat))-1,
          crs = "EPSG:4326")


df_output <- df_output |> rename(month = 'month(date)',
                                 year = 'year(date)')


df_output_1 <- df_output |> filter(month < 10)

df_output_2 <- df_output |> filter(month >= 10)

df_output_1$time <- (paste0(df_output_1$year,"-0",df_output_1$month,"-01"))
df_output_2$time <- (paste0(df_output_2$year,"-",df_output_2$month,"-01"))

df_out <- rbind(df_output_1,df_output_2)

df_points <- df_out |>
  select(lon,lat,time,aet)

points <- terra::vect(df_output, geom = c("lon", "lat"), crs = "EPSG:4326")


r_stack <- list()

for (t in unique(df_points$time)) {
  df_t <- df_points %>% filter(time == t) %>% select(aet, lon, lat)  
  
  points <- vect(df_t, geom = c("lon", "lat"), crs = "EPSG:4326") 
  
  r_t <- rasterize(x = points, y = r, field="aet") 
  
  r_stack[[as.character(t)]] <- r_t 
  
  
  message(paste0("done ",t))
}

r_3D <- rast(r_stack)



writeCDF(r_3D, "../global/dataset_comparison/output_global.nc", overwrite=TRUE,prec = "float")
