library(readr)
library(lubridate)
library(tictoc)
library(dplyr)
library(tidyr)

dir <- "~/data_scratch/global/"

dir2 <- "~/data_scratch/global/tmp/"

files <- list.files(dir)

fapar_files <- files[grep("global_fapar",files)]

WFDEI_sites <-  files[grep("global_WFDEI",files)]

df_co2 <- read_csv("../global/df_co2.csv")

global_site_info <- read_csv("~/data_scratch/global/global_site_info.csv")

days_in_month <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)

# the next 2 array are used to replicate the value of ccov

end_month <- c("-01-31","-02-28","-03-31","-04-30","-05-31","-06-30",
               "-07-31","-08-31","-09-30","-10-31","-11-30","-12-31")

start_month <- c("-01-01","-02-01","-03-01","-04-01","-05-01","-06-01",
                 "-07-01","-08-01","-09-01","-10-01","-11-01","-12-01")


df_ccov2 <- read_csv("../global/df_ccov2.csv")

final_driver <- NULL

# select 60000 each time

sitename <- unique(global_site_info$sitename)[36002:42000]

ID_to_sitename <- data.frame(ID = seq(36002,42000,1),
                             sitename = sitename)


global_site_info <- global_site_info[global_site_info$sitename %in% sitename,]

df_ccov2 <- df_ccov2[df_ccov2$sitename %in% sitename,]

years <- seq(1982,2011,1)

# for cycle to do, now just a test with only 1 year

for(year in years){
  
  
  df_fapar <- read_csv(paste0("../global/global_fapar_",year,".csv"))
  
  df_fapar <- df_fapar[df_fapar$ID %in% ID_to_sitename$ID,]
  
  
  df_ccov <- df_ccov2 |> filter(year(date) == year)
  
  
  df_fapar <- left_join(df_fapar,
                        ID_to_sitename, by ="ID") |>
    select(- ID) |> # this change the ID to the sitename
    group_by(sitename) |>
    mutate(doy = seq(date(paste0(year,"-01-01")),date(paste0(year,"-12-31")), by ="days")) |>
    ungroup() |>
    rename(date = doy)
  
  df_WFDEI <- read_csv(paste0("../global/tmp/global_WFDEI_",year,".csv"))
  
  df_WFDEI <- df_WFDEI[df_WFDEI$ID %in% ID_to_sitename$ID,]
  
  df_WFDEI <- left_join(df_WFDEI,
                        ID_to_sitename, by ="ID") |>
    select(- ID) |> # this change the ID to the sitename
    group_by(sitename) |>
    mutate(doy = seq(date(paste0(year,"-01-01")),date(paste0(year,"-12-31")), by ="days")) |>
    ungroup() |>
    rename(date = doy)
  
  df_WFDEI$vpd <-    ingestr::calc_vpd_inst(qair = df_WFDEI$qair,eact = NA,tc = (df_WFDEI$Tair - 273.15),patm = df_WFDEI$patm)
  
  df_WFDEI <- df_WFDEI |>
    rename(temp = Tair) |>
    mutate(temp = temp -  273.15) |>
    mutate(tmin = temp -3,
           tmax = temp + 3,
           ppfd = sw_rad * 2.04 * 1.0e-6, # W m-2 -> mol m-2 s-1
           rain = rain + snow) |> 
    mutate(snow = 0,
           netrad = NA) |>
    select(sitename,date,temp,vpd,ppfd,netrad,patm,snow,rain,tmin,tmax,vwind)
  
  driver <- left_join(df_WFDEI,
                      df_fapar |> select(sitename,date,fapar), 
                      by = c("sitename","date")
  )
  
  driver <- left_join(driver, df_co2 |> select(-sitename), by = "date")
  
  driver <- driver |> 
    filter(!(month(date)== 2 & day(date) == 29))
  
  daily_ccov <- NULL
  
  for(i in 1:12){
    
    tmp <- df_ccov |> filter( month(date) ==i)
    
    tmp <- tmp[rep(seq_len(nrow(tmp)), times = days_in_month[i]), ]
    
    tmp <- tmp |>
      group_by(sitename) |>
      mutate(date = seq(date(paste0(year,start_month[i])),date(paste0(year,end_month[i])),by = "days")) |>
      ungroup() |>
      select(sitename,date,ccov)
    
    daily_ccov <- rbind(daily_ccov,tmp)
    
  }
  
  
  driver <- left_join(driver,
                      daily_ccov, 
                      by = c("sitename","date")
  )
  
  final_driver <- rbind(final_driver,driver)
  
  message(paste0("done ",year))
  
}

rm(df_ccov,df_ccov2,df_fapar,df_WFDEI,driver,tmp,daily_ccov)

gc()

forcing <- final_driver |>
  group_by(sitename) |>
  nest(forcing = c("date", "temp", "vpd", "ppfd", "netrad", "patm", "snow", "rain", "tmin",
                   "tmax","vwind","fapar","co2","ccov" ))

site_info <- global_site_info |>
  group_by(sitename) |> 
  nest(site_info = c("lon", "lat", "elv", "whc", "canopy_height","reference_height"))

params_siml <- rsofun::p_model_drivers$params_siml[[1]] |>
  mutate(use_gs = TRUE, use_phydro = FALSE, use_pml= TRUE) |>
  nest(params_siml = c(spinup, spinupyears, recycle, outdt, ltre,  ltne,  ltrd,  ltnd,
                       lgr3,  lgn3,  lgr4 ,
                       use_gs, use_phydro, use_pml))

driver <- cbind(site_info,params_siml)

driver <- left_join(forcing,driver, by="sitename") |>
  select(sitename,params_siml,site_info,forcing)

saveRDS(driver,"~/data_scratch/global/global_driver_7.rds")

message("done")



