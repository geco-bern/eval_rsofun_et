library(readr)
library(terra)
library(dplyr)
library(lubridate)
library(tidyr)
library(tictoc)


global_site_info <- read_csv("~/data_scratch/global/global_site_info.csv")

years <- seq(1982,2011,1)

points <- terra::vect(global_site_info, geom = c("lon", "lat"), crs = "EPSG:4326")

month <- c("01","02","03","04","05","06","07","08","09","10","11","12")


for(year in years){
  
  date_start = lubridate::ymd(paste0(year, "-01-01"))
  date_end = lubridate::ymd(paste0(year, "-12-31"))
  
  df_lonlat <- global_site_info |>
    dplyr::mutate(date_start = date_start) |>
    dplyr::mutate(date_end = date_end) 
  
  
  
  # Tair extraction
  
  
  dir <- paste0("/data/archive/wfdei_weedon_2014/data/Tair_daily/Tair_daily_WFDEI_",year,month[1],".nc")
  
  Tair <- rast(dir)
  
  for(i in 2:12){
    
    dir <- paste0("/data/archive/wfdei_weedon_2014/data/Tair_daily/Tair_daily_WFDEI_",year,month[i],".nc")
    
    Tair2 <- rast(dir)
    
    Tair <- c(Tair,Tair2)
    
    rm(Tair2)
    
    gc()
    
  }
  
  names(Tair) <- seq(1, dim(Tair)[3],1)
  
  
  tic()
  values <- terra::extract(Tair, points, xy = FALSE, ID = T, method = "bilinear")
  toc()
  
  values2 <- values |> 
    pivot_longer(cols = -ID, # Exclude ID column
                 names_to = "doy", # Name for the new column with variable names
                 values_to = "Tair") 
  
  rm(Tair,values)
  
  gc()
  
  
  # Wind extraction
  
  
  dir <- paste0("/data/archive/wfdei_weedon_2014/data/Wind_daily/Wind_daily_WFDEI_",year,month[1],".nc")
  
  wind <- rast(dir)
  
  for(i in 2:12){
    
    dir <- paste0("/data/archive/wfdei_weedon_2014/data/Wind_daily/Wind_daily_WFDEI_",year,month[i],".nc")
    
    wind2 <- rast(dir)
    
   
    
    wind <- c(wind,wind2)
    
    rm(wind2)
    
    gc()
    
  }
  
  names(wind) <- seq(1, dim(wind)[3],1)
  
  
  tic()
  values <- terra::extract(wind, points, xy = FALSE, ID = T, method = "bilinear")
  toc()
  
  wind <- values |> 
    pivot_longer(cols = -ID, # Exclude ID column
                 names_to = "doy", # Name for the new column with variable names
                 values_to = "vwind") 
  
  
  
  driver <- left_join(
    values2, wind, by =c("ID","doy")
  )
  
  rm(wind,values,values2)
  
  gc()
  
  # Ps surf
  
  dir <- paste0("/data/archive/wfdei_weedon_2014/data/PSurf_daily/PSurf_daily_WFDEI_",year,month[1],".nc")
  
  patm <- rast(dir)
  
  for(i in 2:12){
    
    dir <- paste0("/data/archive/wfdei_weedon_2014/data/PSurf_daily/PSurf_daily_WFDEI_",year,month[i],".nc")
    
    patm2 <- rast(dir)
    
   
    
    patm <- c(patm,patm2)
    
    rm(patm2)
    
    gc()
    
  }
  
  names(patm) <- seq(1, dim(patm)[3],1)
  
  
  tic()
  values <- terra::extract(patm, points, xy = FALSE, ID = T, method = "bilinear")
  toc()
  
  patm <- values |> 
    pivot_longer(cols = -ID, # Exclude ID column
                 names_to = "doy", # Name for the new column with variable names
                 values_to = "patm") 
  
  
  
  driver <- left_join(
    driver, patm, by =c("ID","doy")
  )
  
  rm(patm, values)
  
  gc()
  
  # Qair
  
  dir <- paste0("/data/archive/wfdei_weedon_2014/data/Qair_daily/Qair_daily_WFDEI_",year,month[1],".nc")
  
  qair <- rast(dir)
  
  for(i in 2:12){
    
    dir <- paste0("/data/archive/wfdei_weedon_2014/data/Qair_daily/Qair_daily_WFDEI_",year,month[i],".nc")
    
    qair2 <- rast(dir)
    
   
    
    qair <- c(qair,qair2)
    
    rm(qair2)
    
    gc()
    
  }
  
  names(qair) <- seq(1, dim(qair)[3],1)
  
  
  tic()
  values <- terra::extract(qair, points, xy = FALSE, ID = T, method = "bilinear")
  toc()
  
  qair <- values |> 
    pivot_longer(cols = -ID, # Exclude ID column
                 names_to = "doy", # Name for the new column with variable names
                 values_to = "qair") 
  
  
  
  driver <- left_join(
    driver, qair, by =c("ID","doy")
  )
  
  rm(qair, values)
  
  gc()
  
  
  # Rain
  
  dir <- paste0("/data/archive/wfdei_weedon_2014/data/Rainf_daily/Rainf_daily_WFDEI_CRU_",year,month[1],".nc")
  
  rain <- rast(dir)
  
  for(i in 2:12){
    
    dir <- paste0("/data/archive/wfdei_weedon_2014/data/Rainf_daily/Rainf_daily_WFDEI_CRU_",year,month[i],".nc")
    
    rain2 <- rast(dir)
    
   
    
    rain <- c(rain,rain2)
    
    rm(rain2)
    
    gc()
    
  }
  
  names(rain) <- seq(1, dim(rain)[3],1)
  
  
  tic()
  values <- terra::extract(rain, points, xy = FALSE, ID = T, method = "bilinear")
  toc()
  
  rain <- values |> 
    pivot_longer(cols = -ID, # Exclude ID column
                 names_to = "doy", # Name for the new column with variable names
                 values_to = "rain") 
  
  
  
  driver <- left_join(
    driver, rain, by =c("ID","doy")
  )
  
  rm(rain, values)
  
  gc()
  
  # snow  
  
  dir <- paste0("/data/archive/wfdei_weedon_2014/data/Snowf_daily/Snowf_daily_WFDEI_CRU_",year,month[1],".nc")
  
  snow <- rast(dir)
  
  for(i in 2:12){
    
    dir <- paste0("/data/archive/wfdei_weedon_2014/data/Snowf_daily/Snowf_daily_WFDEI_CRU_",year,month[i],".nc")
    
    snow2 <- rast(dir)
    
   
    
    snow <- c(snow,snow2)
    
    rm(snow2)
    
    gc()
    
  }
  
  names(snow) <- seq(1, dim(snow)[3],1)
  
  
  tic()
  values <- terra::extract(snow, points, xy = FALSE, ID = T, method = "bilinear")
  toc()
  
  snow <- values |> 
    pivot_longer(cols = -ID, # Exclude ID column
                 names_to = "doy", # Name for the new column with variable names
                 values_to = "snow") 
  
  
  
  driver <- left_join(
    driver, snow, by =c("ID","doy")
  )
  
  rm(snow, values)
  
  gc()
  
  df_lonlat$ID <- seq(1,dim(df_lonlat)[1],1)
  
  driver2 <- left_join(
    driver,df_lonlat |> select(ID,lon,lat),
    by = "ID"
  )
  
  # shortwave radiation  
  
  dir <- paste0("/data/archive/wfdei_weedon_2014/data/SWdown_daily/SWdown_daily_WFDEI_",year,month[1],".nc")
  
  sw_rad <- rast(dir)
  
  for(i in 2:12){
    
    dir <- paste0("/data/archive/wfdei_weedon_2014/data/SWdown_daily/SWdown_daily_WFDEI_",year,month[i],".nc")
    
    sw_rad2 <- rast(dir)
    
    
    
    sw_rad <- c(sw_rad,sw_rad2)
    
    rm(sw_rad2)
    
    gc()
    
  }
  
  names(sw_rad) <- seq(1, dim(sw_rad)[3],1)
  
  
  tic()
  values <- terra::extract(sw_rad, points, xy = FALSE, ID = T, method = "bilinear")
  toc()
  
  sw_rad <- values |> 
    pivot_longer(cols = -ID, # Exclude ID column
                 names_to = "doy", # Name for the new column with variable names
                 values_to = "sw_rad") 
  
  
  
  driver <- left_join(
    driver, sw_rad, by =c("ID","doy")
  )
  
  rm(sw_rad, values)
  
  gc()
  
  df_lonlat$ID <- seq(1,dim(df_lonlat)[1],1)
  
  driver2 <- left_join(
    driver,df_lonlat |> select(ID,lon,lat),
    by = "ID"
  )
  
  
  
  
  write_csv(driver2,paste0("../global/tmp/global_WFDEI_",year,".csv"))
  
  rm(driver2)
  
  gc()
  
  message(paste0("done ",year))
}



