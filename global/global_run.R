library(rsofun)
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(tictoc)
library(lubridate)

siteinfo <- read_csv("../global_site_info.csv")

best_par <- readRDS("~/data_scratch/my_stuff/global_calib_shorter.rds")

params_modl <- list(
  kphio              =  best_par$par[["kphio"]],   
  kphio_par_a        =  best_par$par[["kphio_par_a"]],       
  kphio_par_b        =  best_par$par[["kphio_par_b"]],       
  rd_to_vcmax        =  0.014,  
  soilm_thetastar    =  best_par$par[["soilm_thetastar"]],       
  beta_unitcostratio =  best_par$par[["beta_unitcostratio"]],        
  tau_acclim         =  30,
  kc_jmax            =  0.41,
  gw_calib           = best_par$par[["gw_calib"]]
)

montlhy_aet_pet <- function(driver){
  
  output <- rsofun::runread_pmodel_f(
    driver,
    par = params_modl
  )
  
  to_discard <- output |>
    unnest(data) |>
    group_by(sitename) |>
    summarise(len = n()) |>
    filter(len == 1)
  
  output <- output[!(output$sitename %in% to_discard$sitename),]
  
  if(dim(output)[1] >0){
    output_df <- left_join(output |> unnest(data) |>
                                                 group_by(sitename,year(date), month(date)) |>
                                                 summarise(
                                                   aet = sum(aet),
                                                   gpp = sum(gpp)),
                                               output |> unnest(site_info) |> select(sitename,lat,lon),
                                               by = "sitename")
  } else{
    output_df <- data.frame(nothing = "nothing")
  }

  
  return(output_df)
}



driver <- readRDS("~/data_scratch/global/driver/global_driver_1.rds")

driver <- driver[driver$sitename %in% siteinfo$sitename,]

output <- montlhy_aet_pet(driver)

write_csv(output,"~/data_scratch/global/driver/global_output_1.csv")

rm(driver,output)

gc()

driver <- readRDS("~/data_scratch/global/driver/global_driver_2.rds")

driver <- driver[driver$sitename %in% siteinfo$sitename,]

output <- montlhy_aet_pet(driver)

write_csv(output,"~/data_scratch/global/driver/global_output_2.csv")

rm(driver,output)

gc()

driver <- readRDS("~/data_scratch/global/driver/global_driver_3.rds")

driver <- driver[driver$sitename %in% siteinfo$sitename,]

output <- montlhy_aet_pet(driver)

write_csv(output,"~/data_scratch/global/driver/global_output_3.csv")


rm(driver,output)

gc()

driver <- readRDS("~/data_scratch/global/driver/global_driver_4.rds")

driver <- driver[driver$sitename %in% siteinfo$sitename,]

output <- montlhy_aet_pet(driver)

write_csv(output,"~/data_scratch/global/driver/global_output_4.csv")

rm(driver,output)

gc()

driver <- readRDS("~/data_scratch/global/driver/global_driver_5.rds")

driver <- driver[driver$sitename %in% siteinfo$sitename,]

output <- montlhy_aet_pet(driver)

write_csv(output,"~/data_scratch/global/driver/global_output_5.csv")

rm(driver,output)

gc()

driver <- readRDS("~/data_scratch/global/driver/global_driver_6.rds")

driver <- driver[driver$sitename %in% siteinfo$sitename,]

output <- montlhy_aet_pet(driver)

write_csv(output,"~/data_scratch/global/driver/global_output_6.csv")

rm(driver,output)

gc()

driver <- readRDS("~/data_scratch/global/driver/global_driver_7.rds")

driver <- driver[driver$sitename %in% siteinfo$sitename,]

output <- montlhy_aet_pet(driver)

write_csv(output,"~/data_scratch/global/driver/global_output_7.csv")

rm(driver,output)

gc()

driver <- readRDS("~/data_scratch/global/driver/global_driver_8.rds")

driver <- driver[driver$sitename %in% siteinfo$sitename,]

output <- montlhy_aet_pet(driver)

write_csv(output,"~/data_scratch/global/driver/global_output_8.csv")

rm(driver,output)

gc()

driver <- readRDS("~/data_scratch/global/driver/global_driver_9.rds")

driver <- driver[driver$sitename %in% siteinfo$sitename,]

output <- montlhy_aet_pet(driver)

write_csv(output,"~/data_scratch/global/driver/global_output_9.csv")

rm(driver,output)

gc()

driver <- readRDS("~/data_scratch/global/driver/global_driver_10.rds")

driver <- driver[driver$sitename %in% siteinfo$sitename,]

output <- montlhy_aet_pet(driver)

write_csv(output,"~/data_scratch/global/driver/global_output_10.csv")

rm(driver,output)

gc()

driver <- readRDS("~/data_scratch/global/driver/global_driver_11.rds")

driver <- driver[driver$sitename %in% siteinfo$sitename,]

output <- montlhy_aet_pet(driver)

write_csv(output,"~/data_scratch/global/driver/global_output_11.csv")

rm(driver,output)

gc()


