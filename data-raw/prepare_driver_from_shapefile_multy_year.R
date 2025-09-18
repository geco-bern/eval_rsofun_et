# This is (more or less) the polished version on how to create the driver for p model based on shapefile

# The script is divided in 2 parts, the first one is acquiring the data and the second one in creating the driver

# So far, I extract the data just for one year (selected_year) I'll implement the possibility later on

# Now I'm not using the terra package because it gives me some problem but I'll fix

# To work propely, the P model require the following data

# Already present in the files: date, temperature (all of them) and precipitation

# Data to calculate: patm, ppfd, vpd, ccov, CO2

# ccov and CO2 are already present, patm and vpd can be calculated using elevation, air humidity and temperature

# ppfd is not working yet

library(terra)
library(sf)
library(exactextractr)
library(readr)
library(dplyr)
library(lubridate)

# When I extract the value from the gridded file, the package raster gives me a large list

# I adapt the code found in the package ingestr to work with this data structure

year_start <- 2001
year_end <- 2004
year_avaible <- seq(year_start,year_end,by=1)
days <- seq(ymd("2001-01-01"),ymd("2018-12-31"),by="days")

months <- substr(seq(ym("2001-01"),ym("2004-12"),by="months"),1,7)

number_days <- c()

for(m in 1:length(months)){
  index <- grep(months[m],substr(days,1,7))
  number_days <- append(number_days,length(index))
}


calculate_patm <- function(elevation,temperature_list){

  P0 <- 101325
  L <- 0.0065
  g <- 9.80665
  m <- 0.0289644
  R <- 8.3144598
  T0 <- 273.15

  for(i in 1:length(temperature_list)){
    elv <- elevation[i]
    temperature <- temperature_list[[i]]$tmean
    patm <- P0 * (1 - (L*elv)/(T0+temperature))^((g*m)/(R*L))
    temperature_list[[i]]$tmean <- patm
    temperature_list[[i]] <- temperature_list[[i]] |>
      mutate(patm = tmean) |>
      select(date,patm)
  }

  return(temperature_list)
}

calculate_vp <- function(patm_large_list,vp_large_list){

  kR = 8.3143
  kMv = 18.02
  kMa = 28.963
  rv <- kR/kMv
  rd <- kR/kMa

  for(i in 1:length(vp_large_list)){

    tmp <- left_join(vp[[i]],patm_large_list[[i]],by = "date")
    wair <- tmp$humidity/(1 - tmp$humidity)
    tmp$vp <- tmp$patm * wair * rv/(rd + wair * rv)

    tmp <- tmp|> select(date,vp,coverage_fraction)
    patm_large_list[[i]] <- tmp
  }
  return(patm_large_list)
}

calculate_vpd <- function(vp_large_list,temp_df){

  for(i in 1:length(vp_large_list)){

    tmp <- left_join(vp_large_list[[i]],temp_df[[i]],by = "date")

    tmin <- (611 * exp((17.27 * tmp$tmin)/(tmp$tmin + 273.15))) - tmp$vp
    tmax <-(611 * exp((17.27 * tmp$tmax)/(tmp$tmax + 273.15))) - tmp$vp

    vpd <- (tmin + tmax) / 2

    tmp$vpd <- vpd
    tmp <- tmp|> select(date,vpd,coverage_fraction)
    vp_large_list[[i]] <- tmp

  }
  return(vp_large_list)

  }

# the temperature are extracted directly from the CSV files

extract_temperatures <- function(path,days){

  # so far only for 1 year
  large_list <- temp_df
  files <- list.files(path)
  for(i in 1:length(list.files(path))){
    csv = read_csv(paste0(path,list.files(csv_path)[i]))

    # I insert here the year selected_year
    df <- data.frame(date = csv$date, tmin = csv$temperature_2m_min,
                     tmax = csv$temperature_2m_max,
                     tmean = csv$temperature_2m_mean,
                     precipitation = csv$total_precipitation_sum,
                     streamflow = csv$streamflow)
    df$date <- as_date(df$date)
    df <- df[which(df$date %in% days),]
    large_list[[i]] <- df
  }
  return(large_list)
}

# This function is used for some time series that do not cover the whole year (12 files, monthly time series with daily resolution)

rbind_list <- function(list1,list2){
  for(i in 1:length(list1)){
    tmp <- rbind(list1[[i]],list2[[i]])
    list1[[i]] <- tmp
  }
  return(list1)
}

# Each time I extract a data, I use this workflow: get the path and open the data, extract, remove everything  not needed

# Open shapefile (always necessary)

vector_path <- "~/data_scratch/shapefiles/camels/camels_basin_shapes.shp"
polygon_data <- st_read(vector_path)

rm(vector_path)

# Patm data

# load temperature if already available, I select only the date above the 1990
temp_df <- readRDS("~/data_scratch/temp_df.rds")

# otherwise retrive the data (very long)

# csv_path <- "~/data_scratch/camels_timeseries/camels/csv/"
# temp_df <- extract_temperatures(csv_path,days)

# extract elevation
elevation_path <- "/data/archive/etopo_NA_NA/data/ETOPO1_Bed_g_geotiff.tif"
elevation_tiff <- raster(elevation_path)

# set resolution to 0.5 (air humidity for vpd has this value)
# very slow, to check why
elevation_tiff <- aggregate(elevation_tiff,fact = 30)

rm(elevation_path)

polygon_data_elv <- st_transform(polygon_data, crs(elevation_tiff))

elevation <- exact_extract(elevation_tiff, polygon_data_elv,"mean")

patm <- calculate_patm(elevation, temp_df)

rm(elevation_tiff,polygon_data_elv,patm)

# extract WHC

whc_path <- "/data_1/bestocke/data/mct_data/cwdx80.nc"

whc_data <- terra::raster(whc_path, varname = "cwdx80")

whc_data2 <- terra::rast(whc_path)


polygon_data_whc <- st_transform(polygon_data, crs(whc_data))

whc <- exact_extract(whc_data, polygon_data_whc,"mean")

rm(whc_path,whc_data,polygon_data_whc)

# extract VPD, starting from air humidity and patm

#selected year selected_year
path_watch_humidity <- "/data/archive/wfdei_weedon_2014/data/Tair_daily/"

humidity_files = list.files(path_watch_humidity)

# since we assumed to calculate consecutice years, we may take the first index of start year and last of end years

humidity_files <- humidity_files[grep(year_start,humidity_files)[1]:grep(year_end,humidity_files)[12]]

# the first one starts outside the for cycle

humidity_data <- brick(paste0(path_watch_humidity,humidity_files[1]),varname="Qair")

polygon_data_humidity <- st_transform(polygon_data, crs(humidity_data))

humidity_data_list <-  exact_extract(humidity_data, polygon_data_humidity,fun = NULL)

vp <- humidity_data_list

for(i in 1:length(vp)){
  df <- vp[[i]]
  final_df <- NULL
  for(j in 1:(dim(df)[2]-1)){
    tmp <- data.frame(date = rep(days[j],times=dim(df)[1]),humidity = df[,j],coverage_fraction = df$coverage_fraction)
    final_df <- rbind(final_df,tmp)
  }
  vp[[i]] <- final_df
}


vp <- calculate_vp(vp,humidity_data_list)
yearly_results <- calculate_vpd(vp,temp_df)

days_index <- 0

# this will take an eternity, to fix somehow (maybe combine all the file in 1)
# seems that exact_extract is the bottleneck

for(i in 2:length(humidity_files)){

  prec_month <- number_days[i-1]

  days_index <- days_index + prec_month


  humidity_data <- brick(paste0(path_watch_humidity,humidity_files[i]),varname="Qair")

  polygon_data_humidity <- st_transform(polygon_data, crs(humidity_data))

  humidity_data_list <-  exact_extract(humidity_data, polygon_data_humidity,fun = NULL)

  vp <- humidity_data_list

  for(k in 1:length(vp)){
    df <- vp[[k]]
    final_df <- NULL
    for(j in 1:(dim(df)[2]-1)){
      tmp <- data.frame(date = rep(days[days_index + j],times=dim(df)[1]),humidity = df[,j],coverage_fraction = df$coverage_fraction)
      final_df <- rbind(final_df,tmp)
    }
    vp[[k]] <- final_df
  }


  vp <- calculate_vp(patm,humidity_data_list)

  vpd <- calculate_vpd(vp,temp_df)
  yearly_results <- rbind_list(yearly_results,vpd)
  message(paste0("done ",i))
}

# check (should work for all the pther site)

all(unique(yearly_results[[1]]$date) == days)

# to avoid mistake

yearly_results_vpd <- yearly_results

for(i in 1:length(yearly_results_vpd)){
  tmp <- yearly_results_vpd[[i]]
  tmp <- tmp |>
    group_by(date) |>
    summarise(vpd = sum(vpd*(coverage_fraction/sum(coverage_fraction)))) |>
    select(date,vpd)
  yearly_results_vpd[[i]] <- tmp
}

rm(humidity_data,humidity_data_list,humidity_files,polygon_data_humidity,tmp,vp,vpd,yearly_results,
   path_watch_humidity,month_in_year_humidity)

# extract fAPAR
# since data are in monthly resolution, I replicate each measurment by the number of day


fapar_path = "/data/scratch/bstocker/MODIS-C006_MOD15A2_LAI_FPAR_zmaw/MODIS-C006_MOD15A2__LAI_FPAR__LPDAAC__GLOBAL_0.5degree__UHAM-ICDC__2000_2018__MON__fv0.02.nc"

fapar_data <- brick(fapar_path,varname = "fpar")

polygon_data_fapar <- st_transform(polygon_data, crs(fapar_data))

fapar_data <- subset(fapar_data, which(substr(names(fapar_data),2,5) %in% year_avaible))

fapar_data <- brick(fapar_data)

fapar_data <- exact_extract(fapar_data, polygon_data,"mean")

#fapar_data <- fapar_data[,which(substr(colnames(fapar_data),7,10) %in% year_avaible)]

colnames(fapar_data) <- months

# check if days are correct

sum(number_days) == length(days)

fapar_large_list <- temp_df

for(i in 1:length(fapar_large_list)){
  fapar <- rep(fapar_data[i,],number_days)

  df <- data.frame(date = days, fapar = as.numeric(fapar))

  fapar_large_list[[i]] <- df
}

rm(fapar_path,polygon_data_fapar,fapar_data,fapar)

# cloud coverage (to check if path and varname are correct)

# data are monthly starting from 1901 so selected_year
cloud_path <- "/data/archive/cru_NA_2021/data/cru_ts4.05.1901.2020.cld.dat.nc"

cloud_data <- brick(cloud_path,varname = "cld")

polygon_data_cloud <- st_transform(polygon_data, crs(cloud_data))

cloud_data <- subset(cloud_data, which(substr(names(cloud_data),2,5) %in% year_avaible))

cloud_data <- brick(cloud_data)

cloud_coverage <- exact_extract(cloud_data, polygon_data_cloud,"mean")

cloud_coverage_large_list <- temp_df

for(i in 1:length(cloud_coverage_large_list)){

  cloud <- rep(cloud_coverage[i,],number_days)

  df <- data.frame(date = days, cloud = as.numeric(cloud)/100)

  cloud_coverage_large_list[[i]] <- df
}

rm(cloud_path,cloud_data,polygon_data_cloud,cloud_coverage,cloud)

# CO2 data
df_co2 <- read_csv("data_scratch/year_co2.csv")

df_co2 <- df_co2[which(df_co2$Yr %in% year_avaible),]

days_in_years <- NULL

for(i in year_avaible){
  bool <- grep(i, days)
  days_in_years <- append(days_in_years,length(bool))
}

df_co2 <- rep(df_co2$co2, days_in_years)

# part 2 create driver
library(rsofun)
library(readr)
library(dplyr)

coordinate_camels <- read_csv("~/data_scratch/camels_timeseries/camels/attributes_other_camels.csv")
final_driver <- NULL

for(i in 1:671){

  driver <- p_model_drivers

  driver$sitename[[1]] = polygon_data$gauge_id[i]

  driver$site_info[[1]] =
    tibble(
      lon= coordinate_camels$gauge_lon[i],
      lat= coordinate_camels$gauge_lat[i],
      elv = elevation[i],
      whc = whc[i])

  driver$forcing <-
    tibble(
      date = temp_df[[i]]$date,
      temp = temp_df[[i]]$tmean,
      vpd = yearly_results_vpd[[i]]$vpd,
      ppfd = NA, # to change otherwise internally calculated
      netrad = NA,
      patm = patm[[i]]$patm,
      snow = 0,
      rain = temp_df[[i]]$precipitation /(24*60*60), # change to mm day to mm sec
      tmin = temp_df[[i]]$tmin,
      tmax = temp_df[[i]]$tmax,
      fapar = fapar_large_list[[i]]$fapar,
      co2 = df_co2,
      ccov = cloud_coverage_large_list[[i]]$cloud,
      streamflow = temp_df[[i]]$streamflow
    ) |>
    # remove 29-02 othwerise won't work
    filter(!row_number() %in% grep("02-29",date)) |>
    list()

  final_driver <- rbind(final_driver,driver)
}

rm(coordinate_camels,driver)

saveRDS(final_driver,"~/data_scratch/driver_US_stream.rds")
