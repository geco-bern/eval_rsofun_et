library(readr)
library(terra)
library(dplyr)
library(lubridate)
library(tidyr)
library(tictoc)



global_site_info <- read_csv("~/data_scratch/global/global_site_info.csv")

points <- terra::vect(global_site_info, geom = c("lon", "lat"), crs = "EPSG:4326")

values_fin <- NULL
years <- seq(2000,2012,1)

for(year in years){
  
  date_start = lubridate::ymd(paste0(year, "-01-01"))
  date_end = lubridate::ymd(paste0(year, "-12-31"))
  
  df_lonlat <- global_site_info |>
    dplyr::mutate(date_start = date_start) |>
    dplyr::mutate(date_end = date_end) 
  
  
  dir <- paste0("/data/archive/fparmodisv6.1_myneni_2021/fparmodisv6.1_myneni_2021/data/Fpar.average.7200.3600.",year,".nc")
  
  fapar <- terra::rast(dir)

  
  names(fapar) <-  time(fapar)
  
  
  tic()
  values <- terra::extract(fapar, points, xy = FALSE, ID = T, method = "bilinear")
  toc()
  
  values2 <- values |> 
    pivot_longer(cols = -ID, # Exclude ID column
                 names_to = "date", # Name for the new column with variable names
                 values_to = "fapar") 
  
  
  values_fin <- rbind(values_fin,values2)
  
  message(paste0("done "),year)
  
}

write_csv(values_fin,"../global/df_fapar_modis.csv")


values_fin <- read_csv("../global/df_fapar_modis.csv")

full_seq <- seq(as.Date("2001-01-01"), as.Date("2011-12-31"), by = "day")

interpolated <- NULL


mask <- values_fin |> 
  group_by(ID) |> summarise(f = sum(fapar,na.rm=T)) # remove all NA 


mask <- mask |> filter(f > 0)

values_fin <- values_fin[ values_fin$ID %in% mask$ID, ]

extract_fapar_by_year <- function(year){
  
  df_fin <- NULL
  
  tmp <- values_fin |> filter(year(date) >= (year -1) &
                                year(date) <= (year + 1))
  
  full_seq <- seq(as.Date(paste0(year,"-01-01")), as.Date(paste0(year,"-12-31")), by = "day")
  
  j <- 0
  k <- 0
  
  for(i in unique(tmp$ID)){
    
    tmp2 <- tmp |> filter(ID ==i)
    
    lin_seq <- approx(tmp2$date,tmp2$fapar,xout = full_seq)
    
    df <- data.frame(ID = i, date = full_seq, fapar = lin_seq$y)
    
    #df <- df |> nest(data = c(date,fapar))
    
    df_fin <- rbind(df_fin,df)
    
    j <- j+1
    k <- k + 1
    if(j == 1000){
      message(paste0("done ",k))
      j <- 0
    }
  }
  
  return(df_fin)
  
}

df_2001 <- extract_fapar_by_year(2001)

df_2001 <- left_join(df_2001,
                      global_site_info|> mutate(ID = seq(1,nrow(global_site_info),1))|>
                        select(ID,sitename,lon,lat),
                      by = "ID")

write_csv(df_2001,"../global/daily_fapar_modis_2001.csv")

rm(df_2001)

gc()

df_2002 <- extract_fapar_by_year(2002)

df_2002 <- left_join(df_2002,
                     global_site_info|> mutate(ID = seq(1,nrow(global_site_info),1))|>
                       select(ID,sitename,lon,lat),
                     by = "ID")

write_csv(df_2002,"../global/daily_fapar_modis_2002.csv")

rm(df_2002)

gc()

df_2003 <- extract_fapar_by_year(2003)

df_2003 <- left_join(df_2003,
                     global_site_info|> mutate(ID = seq(1,nrow(global_site_info),1))|>
                       select(ID,sitename,lon,lat),
                     by = "ID")

write_csv(df_2003,"../global/daily_fapar_modis_2003.csv")

rm(df_2003)

gc()


df_2004 <- extract_fapar_by_year(2004)

df_2004 <- left_join(df_2004,
                     global_site_info|> mutate(ID = seq(1,nrow(global_site_info),1))|>
                       select(ID,sitename,lon,lat),
                     by = "ID")

write_csv(df_2004,"../global/daily_fapar_modis_2004.csv")

rm(df_2004)

gc()

df_2005 <- extract_fapar_by_year(2005)

df_2005 <- left_join(df_2005,
                     global_site_info|> mutate(ID = seq(1,nrow(global_site_info),1))|>
                       select(ID,sitename,lon,lat),
                     by = "ID")

write_csv(df_2005,"../global/daily_fapar_modis_2005.csv")

rm(df_2005)

gc()

df_2006 <- extract_fapar_by_year(2006)

df_2006 <- left_join(df_2006,
                     global_site_info|> mutate(ID = seq(1,nrow(global_site_info),1))|>
                       select(ID,sitename,lon,lat),
                     by = "ID")

write_csv(df_2006,"../global/daily_fapar_modis_2006.csv")

rm(df_2006)

gc()

df_2007 <- extract_fapar_by_year(2007)

df_2007 <- left_join(df_2007,
                     global_site_info|> mutate(ID = seq(1,nrow(global_site_info),1))|>
                       select(ID,sitename,lon,lat),
                     by = "ID")

write_csv(df_2007,"../global/daily_fapar_modis_2007.csv")

rm(df_2007)

gc()

df_2008 <- extract_fapar_by_year(2008)

df_2008 <- left_join(df_2008,
                     global_site_info|> mutate(ID = seq(1,nrow(global_site_info),1))|>
                       select(ID,sitename,lon,lat),
                     by = "ID")

write_csv(df_2008,"../global/daily_fapar_modis_2008.csv")

rm(df_2008)

gc()

df_2009 <- extract_fapar_by_year(2009)

df_2009 <- left_join(df_2009,
                     global_site_info|> mutate(ID = seq(1,nrow(global_site_info),1))|>
                       select(ID,sitename,lon,lat),
                     by = "ID")

write_csv(df_2009,"../global/daily_fapar_modis_2009.csv")

rm(df_2009)

gc()


df_2010 <- extract_fapar_by_year(2010)

df_2010 <- left_join(df_2010,
                     global_site_info|> mutate(ID = seq(1,nrow(global_site_info),1))|>
                       select(ID,sitename,lon,lat),
                     by = "ID")

write_csv(df_2010,"../global/daily_fapar_modis_2010.csv")

rm(df_2010)

gc()


df_2011 <- extract_fapar_by_year(2011)

df_2011 <- left_join(df_2011,
                     global_site_info|> mutate(ID = seq(1,nrow(global_site_info),1))|>
                       select(ID,sitename,lon,lat),
                     by = "ID")

write_csv(df_2011,"../global/daily_fapar_modis_2011.csv")

rm(df_2011)

gc()











