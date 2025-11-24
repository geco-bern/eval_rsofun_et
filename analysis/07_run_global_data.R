# ERA5 extraction
####---------

# # montlhy era 5 values 0.1 resoultion from 1982 to 2011
# era5 <- terra::rast("~/data_scratch/big_data/data_stream-moda.nc")
#
# # coordinates are shifted (+ 180)
#
# new_extent <- ext(181, 360, ymin(era5), ymax(era5))  # Keeping original latitude
#
# era5_1 <-  crop(era5, new_extent)
#
# era5_1 <- shift(era5_1, dx = -360)
#
# new_extent <- ext(0, 179.95, ymin(era5), ymax(era5))  # Keeping original latitude
#
# era5_2 <-  crop(era5, new_extent)
#
# era5_n <- terra::merge(era5_1,era5_2)
#
# names(era5_n) <- seq(date("1982-01-01"),date("2011-12-01"),by="months")
#
# prova <- rast(ext(era5_n), resolution = 0.5, crs = crs(era5_n))
#
# prova <- resample(era5_n, prova, method = "average")
#
#
# months <- seq(date("1982-01-01"),date("2011-12-01"),by="months")
#
# montlhy_era5 <- NULL
#
# for(i in 1:360){
#
#   prova2 <- prova[[i]]
#
#   prova2 <- terra::as.data.frame(prova2,xy = TRUE, na.rm = TRUE)
#
#   colnames(prova2) <- c("lon","lat","aet")
#
#   prova2 <- prova2 |>  mutate(aet = aet*(-1000))
#
#
#   prova3 <- tibble(date = months[i],nest(prova2))
#
#   montlhy_era5 <- rbind(montlhy_era5,prova3)
#
# }
#
# saveRDS(montlhy_era5,"../big_data/monthly_era5.rds")

####---------
#PML extraction
####---------

# no good quality data before 1997
# years <- seq(1998,2011,1)
#
#
# rast_final <- terra::rast(paste0("~/data_scratch/big_data/PML/data/Monthly_PML_ETa_",1997,".nc"))
#
# names(rast_final) <- seq(date("1997-01-01"),date("1997-12-01"), by = "months")
#
#
# for(i in years){
#
#   tmp <- terra::rast(paste0("~/data_scratch/big_data/PML/data/Monthly_PML_Ec_",i,".nc"))
#
#   names(tmp) <- seq(date(paste0(i,"-01-01")),date(paste0(i,"-12-01")), by = "months")
#
#   rast_final <- c(rast_final,tmp)
#
# }
#
# #check
#
# names(rast_final)
#
# # FRANCESCO all this in necessary, in original rast x and y are inverted
#
# rast_final <- flip(rast_final)
# rast_final <- trans(rast_final)
# rast_final <- flip(rast_final)
#
#
# months <- seq(date("1998-01-01"),date("2011-12-01"),by="months")
#
# montlhy_pml <- NULL
#
# for(i in 1:168){
#
#   prova2 <- rast_final[[i]]
#
#   prova2 <- terra::as.data.frame(prova2,xy = TRUE, na.rm = TRUE)
#
#   colnames(prova2) <- c("lon","lat","aet")
#
#   prova3 <- tibble(date = months[i],nest(prova2))
#
#   montlhy_pml <- rbind(montlhy_pml,prova3)
#
# }

# saveRDS(montlhy_pml,"../big_data/montlhy_pml.rds")
####---------
#FLUXCOM extraction
####---------
#
# dir <- "~/data_scratch/big_data/FLUXCOM/ensemble"
#
# files <- list.files(dir)
#
# files <- files[grep("LE",files)]
#
# # first one outside for cycle
#
# file <- files[grep(1997,files)]
#
# LE_rast_fin <- terra::rast(paste0(dir,"/",file))
#
# LE_rast_fin <-LE_rast_fin[[c(1:12)]]
#
# years <- seq(1998,2011,1)
#
# for(year in years){
#
#   file <- files[grep(year,files)]
#
#   LE_rast <- terra::rast(paste0(dir,"/",file))
#
#   LE_rast <-LE_rast[[c(1:12)]]
#
#   LE_rast_fin <- c(LE_rast_fin,LE_rast)
# }
#
# names(LE_rast_fin) <- seq(date("1997-01-01"),date("2011-12-01"), by = "months")
#
# # FLUXCOM gives the LE in Mj per day, no good
# # I take global temp and patm to convert LE to aet
#
# # STEP1: take mean monthly temp and patm from all global driver
# # done it in extrc ....
#
# temp_patm <- readRDS("~/data_scratch/big_data/montlhy_tmp_patm_for_fluxcom.rds")
#
# temp_patm$date <- seq(date("1997-01-01"),date("2011-12-01"), by = "months")
#
#
# le_to_et <- function(df){
#   1000 * 60 * 60 * 24 * df$le / (cwd::calc_enthalpy_vap(df$temp) * cwd::calc_density_h2o(df$temp, df$patm))
# }
#
# months <- seq(date("1997-01-01"),date("2011-12-01"), by = "months")
#
# montlhy_fluxcom <- NULL
#
# for(i in 1:180){
#
#   prova2 <- LE_rast_fin[[i]]
#
#   prova2 <- terra::as.data.frame(prova2,xy = TRUE, na.rm = TRUE)
#
#   colnames(prova2) <- c("lon","lat","aet")
#
#   tmp <- temp_patm$data[[i]] |>
#     mutate(lonlat = paste0(lon,lat)) |>
#     select(temp,patm,lonlat)
#
#
#   prova2 <- left_join(prova2 |> mutate(lonlat = paste0(lon,lat)),
#                       tmp, by = "lonlat") |>
#     rename(le = aet) |>
#     mutate(le = le *10^6 / (60 * 60 * 24)) |> # transofrm from MJ day to J s (hopefully bith per square meter)
#     select(le,temp,patm,lon,lat) |>
#     drop_na()
#
#   prova2$aet <- le_to_et(prova2)
#
#   prova3 <- tibble(date = months[i],nest(prova2))
#
#   montlhy_fluxcom <- rbind(montlhy_fluxcom,prova3)
#
# }
#
# saveRDS(montlhy_fluxcom,"../big_data/montlhy_fluxcom.rds")
####--------------
