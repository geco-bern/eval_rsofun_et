

library(readr)
library(dplyr)
library(tidyr)
library(terra)
library(lubridate)
library(ingestr)
# data preparation, currently only on my folder

# will take at random 10 catchements and see what happens

vector_path <- "~/data_scratch/shapefiles/camels/camels_basin_shapes.shp"

shapefile <- terra::vect(vector_path)

#shapefile <- shapefile[300:309,]

siteinfo <- read.csv("~/data_scratch/camels_timeseries/camels/attributes_other_camels.csv")

siteinfo <- siteinfo[siteinfo$gauge_id %in% shapefile$gauge_id,]

# we add the year of interest, for example 2004

date_start = lubridate::ymd(paste0(1990, "-01-01"))
date_end = lubridate::ymd(paste0(2010, "-12-31"))


siteinfo <- siteinfo |>
  dplyr::mutate(date_start = date_start) |>
  dplyr::mutate(date_end = date_end) |>
  mutate(sitename = gauge_id) |>
  rename(lon = gauge_lon,
         lat = gauge_lat)




## CO2



df_co2 <- ingest_bysite(
  sitename  = "anyone",
  source  = "co2_mlo",
  year_start= lubridate::year(date_start),
  year_end  = lubridate::year(date_end),
  verbose = FALSE
)

# remove 29-02

df_co2 <- df_co2 |>
  filter(!(month(date)== 2 & day(date) == 29))





# laod CSV data and driver data preparation


timeseries <- list.files(path = "~/data_scratch/camels_timeseries/camels/csv/", pattern = "\\.csv$", full.names = T)


timeseries <- timeseries[substr(timeseries,60,74) %in% siteinfo$gauge_id]

driver_data <- NULL

for(i in timeseries){
  sitename <- substr(i,60,74)
  
  params_siml <- rsofun::p_model_drivers$params_siml[[1]] |>
    mutate(use_gs = TRUE, use_phydro = FALSE, use_pml= TRUE) |>
    nest(params_siml = c(spinup, spinupyears, recycle, outdt, ltre,  ltne,  ltrd,  ltnd,
                         lgr3,  lgn3,  lgr4 ,
                         use_gs, use_phydro, use_pml))
  
  site_info <- siteinfo[siteinfo$gauge_id == sitename,] |>
    select(lat,lon) |>
    mutate(canopy_height =12, # TODO 
           reference_height = 10) |> # I select 10 beacuse the wind velocity is measured at 10 m
    nest(site_info = c(lat,lon,canopy_height,reference_height))
  
  forcing <- read_csv(i)
  forcing <- forcing |>
    filter(lubridate::date(date) >= date_start, lubridate::date(date) <= date_end) |> 
    filter(!(month(date)== 2 & day(date) == 29)) |>
    rename(rain = total_precipitation_sum,
           runoff = streamflow,
           tmin = temperature_2m_min,
           tmax = temperature_2m_max,
           temp = temperature_2m_mean,
           vwind = u_component_of_wind_10m_max) |>
    mutate(netrad = NA,
           snow = 0,
           co2 = df_co2$co2,
           #fapar = 1, # TODO
           rain = rain / (24*60*60)) |>
    select(date,temp,netrad,snow,rain,tmin,tmax,vwind,co2,runoff) |>
    filter(!(lubridate::month(date) == 2 & lubridate::day(date) == 29)) |>
    nest(forcing = c(date,temp,netrad,snow,rain,tmin,tmax,vwind,co2,runoff))
  
  tmp <- data.frame(sitename = sitename,
                    params_siml = tibble(params_siml),
                    site_info = tibble(site_info),
                    forcing = tibble(forcing))
  
  driver_data <- rbind(driver_data,tmp)
  
}





df_out <- tibble()

getvars <- c("vpd",'ppfd')

source <- "watch_wfdei"

dir <- "/data/archive/wfdei_weedon_2014/data/"

timescale <- "d"

df_out <- ingest_globalfields(siteinfo = siteinfo,source = source,getvars = getvars,
                              dir = dir, timescale= timescale,is_shapefile = T, shapefile = shapefile)


driver_data <- 
  left_join(driver_data |>
              unnest(forcing),
            df_out |>
              dplyr::select(sitename,date,ppfd,patm,vpd),
            by =c("sitename","date")) |>
  nest(forcing = c(date,temp,vpd,ppfd,netrad,patm,snow,rain,tmin,tmax,vwind,co2,runoff))




## extract WHC








# based on shapefile

df_out <- tibble()

df_out <- ingest_globalfields(siteinfo = siteinfo,source = source,getvars = getvars,
                              dir = dir, timescale= timescale,is_shapefile = T, shapefile = shapefile)

driver_data <- 
  left_join(driver_data |>
              unnest(site_info),
            df_out ,
            by ="sitename")|>
  nest(site_info = c(lat,lon,whc,canopy_height,reference_height))

print(df_out)


## Elevation


df_out <- tibble()

source <- "etopo1"

dir <- "/data/archive/etopo_NA_NA/data/"

df_out <- ingest_globalfields(siteinfo = siteinfo,source = source,getvars = getvars,
                              dir = dir, timescale= timescale,is_shapefile = T, shapefile = shapefile)


driver_data <- 
  left_join(driver_data |>
              unnest(site_info),
            df_out ,
            by ="sitename")|>
  nest(site_info = c(lat,lon,elv,whc,canopy_height,reference_height))




## CLoud coverage


df_cld <- ingestr:::extract_pointdata_allsites_shp(
  filename = "/data/archive/cru_harris_2024/data/cru_ts4.08.1901.2023.cld.dat.nc",
  df_shapefile = shapefile,
  get_time = T,
  year_arg = NA_integer_, month_arg = NA_integer_ # only used for WFDEI in combination with get_time
) 

df_cld <- df_cld |> unnest(data) |>
  filter(varnam == "cld") |>
  filter(date > date_start & date < date_end) |>
  mutate(value = value /100) |>
  mutate(date = floor_date(date, "month"))


df_cld <- df_cld |>
  group_by(sitename) |>
  mutate(date = purrr::map(date, ~ seq(.x, ceiling_date(.x, "month") - days(1), by = "day"))) |>
  unnest(date) |>
  unnest(sitename) |>
  select(sitename,date, value) |>
  rename(ccov = value)

driver_data <- left_join(
  driver_data |> 
    unnest(forcing),
  df_cld, by=c("sitename","date")
) |> 
  nest(forcing =c(date,temp,vpd,ppfd,netrad,patm,snow,rain,tmin,tmax,vwind,co2,ccov,runoff))


## fAPAR (ask if is correct)





files <- list.files("/data/archive/fapar3g_zhu_2013/data/")

year_seq <- seq(substr(date_start,1,4),substr(date_end,1,4),1)

by_weekly <- c("-01-01","-01-14","-02-01","-02-14","-03-01","-03-14",
               "-04-01","-04-14","-05-01","-05-14","-06-01","-06-14",
               "-07-01","-07-14","-08-01","-08-14","-09-01","-09-14",
               "-10-01","-10-14","-11-01","-11-14","-12-01","-12-14")


df_tmp <- NULL


for(j in year_seq){
  file_to_extract <- grep(j,files)
  
  df_final <- NULL
  # extract yearly fapar
  
  for(i in 1:24){
    df_fapar <-  ingestr:::extract_pointdata_allsites_shp(
      filename = paste0("/data/archive/fapar3g_zhu_2013/data/",files[file_to_extract[i]]),
      df_shapefile = shapefile,
      get_time = F,
      year_arg = NA_integer_, month_arg = NA_integer_ # only used for WFDEI in combination with get_time
    )
    
    df_fapar <- df_fapar|> unnest(data) |> rename(fapar = colnames(df_fapar|> unnest(data))[2])
    df_fapar$date <- lubridate::as_date(paste0(j,by_weekly[i]))
    
    df_final <- rbind(df_final,df_fapar)
  }
  # linearly extend from biweekly to daily
  
  for(k in unique(df_final$sitename)){
    
    
    tmp <- df_final[df_final$sitename == k,]
    
    tmp <- data.table::setorder(tmp,date)
    
    linear_fapar <-approx(tmp$date,tmp$fapar,method = 'linear',n = 25*14 - 2)
    
    linear_fapar <- data.frame(date = lubridate::as_date(linear_fapar$x),
                               fapar =  linear_fapar$y)
    
    
    # calculate the fapar from 12-14 to 12-31
    slope <-(tmp$fapar[24] - tmp$fapar[23]) / 14
    
    step <- seq(1,17,1)
    
    fapar <- tmp$fapar[24] + slope * step
    
    daily_fapar <- c(linear_fapar$fapar,fapar)
    
    days <- seq(lubridate::as_date(paste0(j,"-01-01")),lubridate::as_date(paste0(j,"-12-31")), by = "day")
    
    # remove 29-02
    
    if(length(grep("02-29",days)) != 0){
      days <- days[-(grep("02-29",days))]
    }
    
    
    tmp_df <- data.frame(sitename = k, date = days, fapar = daily_fapar)
    
    
    df_tmp <- rbind(df_tmp,tmp_df)}
  
}



# 
# file_to_extract <- grep(year_seq,files)
# 
# 
# df_final <- NULL
#   
# for(i in 1:24){
#     df_fapar <-  ingestr:::extract_pointdata_allsites_shp(
#   filename = paste0("/data/archive/fapar3g_zhu_2013/data/",files[file_to_extract[i]]),
#   df_shapefile = shapefile,
#   get_time = F,
#   year_arg = NA_integer_, month_arg = NA_integer_ # only used for WFDEI in combination with get_time
#   )
#     
#     df_fapar <- df_fapar|> unnest(data) |> rename(fapar = colnames(df_fapar|> unnest(data))[2])
#     df_fapar$date <- lubridate::as_date(paste0(year_seq[1],by_weekly[i]))
#     
#     df_final <- rbind(df_final,df_fapar)
# }
# 
# tmp <- df_final |> arrange(df_final, sitename)



# df_tmp <- NULL
# 
# 
# for(i in unique(df_final$sitename)){
#   tmp <- df_final[df_final$sitename == i,]
#   
#   tmp <- data.table::setorder(tmp,date)
#   
#   linear_fapar <-approx(tmp$date,tmp$fapar,method = 'linear',n = 25*14 - 2)
#   
#   linear_fapar <- data.frame(date = lubridate::as_date(linear_fapar$x),
#                            fapar =  linear_fapar$y)
#   
#   slope <-(tmp$fapar[24] - tmp$fapar[23]) / 14
#   
#   step <- seq(1,17,1)
#   
#   fapar <- tmp$fapar[24] + slope * step
# 
#   daily_fapar <- c(linear_fapar$fapar,fapar)
#     
#   days <- seq(lubridate::as_date("2005-01-01"),lubridate::as_date("2005-12-31"), by = "day")
#   
#   tmp_df <- data.frame(sitename = i, date = days, fapar = daily_fapar)
#   
#   
#   df_tmp <- rbind(df_tmp,tmp_df)
# }





driver_data <- left_join(
  driver_data |>
    unnest(forcing),
  df_tmp, by =c("sitename","date")
) |>
  nest(forcing =c(date,temp,vpd,ppfd,netrad,patm,snow,rain,tmin,tmax,vwind,fapar,co2,ccov,runoff))


saveRDS(driver_data,"camels_driver2.rds")

