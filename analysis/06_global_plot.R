library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(readr)
# library(rsofun)
# library(ncdf4)


p_model <-  read_csv("/data_2/scratch/akurth/grsofun_output/PM-S0/final_aet_df_PM-S0_20251128_1152.csv")

colnames(p_model) <- c("sitename","year", "month","aet","lon","lat", "fland" )


p_model <- p_model |>
  filter(fland < 0.5)

# just p_model and the other
#
monthly_era5 <- read_rds("/data/archive_projects/eval_rsofun_et/monthly_era5.rds")

montlhy_pml <- read_rds("/data/archive_projects/eval_rsofun_et/montlhy_pml.rds")

montlhy_fluxcom <- read_rds("/data/archive_projects/eval_rsofun_et/montlhy_fluxcom.rds")

mean_model <- rbind(monthly_era5  |>
                      mutate(year = year(date)) |>
                      filter(year > 1996) |>
                      unnest(data) |>
                      select(year,aet,lat,lon) |>
                      mutate(aet = aet * 365/12) |>
                      filter(lat > -60) |>
                      mutate(
                        lat = ifelse(lat < 0,as.integer(20*lat + 1),as.integer(20*lat +2 ))) |> # so they are aligned
                      mutate(lat = lat / 20),

                    montlhy_pml   |>
                      mutate(year = year(date)) |>
                      filter(year > 1996) |>
                      unnest(data) |>
                      select(year,aet,lat,lon) ,

                    montlhy_fluxcom   |>
                      mutate(year = year(date)) |>
                      filter(year > 1996) |>
                      unnest(data) |>
                      mutate(aet = aet * 365/12) |>
                      select(year,aet,lat,lon)
) |>
  group_by(year,lon,lat) |>
  summarise(aet = sum(aet,na.rm = T)) |>
  group_by(lat) |>
  summarise(
    med =median(aet,na.rm=T),
    q_33 = quantile(aet,0.33,na.rm=T),
    q_66 = quantile(aet,0.66,na.rm=T)) |>
  mutate(Setup = "mean_model")

p_model2 <- p_model |>
  filter(year > 1996) |>
  filter(year < 2012) |>
  filter(lat < 83.75 & lat > -55.75) |>
  group_by(lat)|>
  group_by(year,lon,lat) |>
  summarise(aet = sum(aet,na.rm = T)) |>
  group_by(lat) |>
  summarise(med =  365/ 12 * median(aet,na.rm=T),
            q_33 = 365/ 12 * quantile(aet,0.33,na.rm=T),
            q_66 = 365/12 * quantile(aet,0.66,na.rm=T)) |>
  mutate(Setup = "P_model")

df <- rbind(p_model2,mean_model)



lateral_profile <- ggplot(df) +
  geom_line( aes(x = lat,y = med,color = Setup),size = 0.9) +
  geom_ribbon(aes(x = lat,ymin = q_33,ymax = q_66,fill = Setup ), alpha = 0.3) +
  # geom_line(data = monthly_era5, aes(x = lat,y = med,color = Setup),size = 0.9) +
  # geom_ribbon(data = monthly_era5, aes(x = lat,ymin = q_0.25,ymax = q_0.75,fill = Setup ), alpha = 0.3) +
  scale_color_manual(
    values = c(
      "mean_model" = "black",
      "P_model" = "royalblue"
    ),
    name = "Model"
  ) +
  scale_fill_manual(
    values = c(
      "mean_model" = "black",
      "P_model" = "royalblue"
    ),
    name = "Model") +
  scale_x_continuous(
    expand = c(0,0)
  ) +
  scale_y_continuous(
    expand = c(0,0)
  ) +
  theme_minimal() +
  ylab(expression( paste("Simulated AET (mm yr"^{-1}, ")" ) ))+
  xlab("lat")+
  theme(
    panel.grid.major.y = element_line(),
    panel.grid.major.x = element_line()
  ) + coord_flip() + theme(aspect.ratio = 1.5)

print(lateral_profile)


# timeseries of aet

p_model2 <- p_model |>
  filter(year > 1996) |>
  filter(year < 2012) |>
  filter(lat < 83.75 & lat > -55.75) |>
  mutate(month = ifelse(month < 10,paste0("-0",month),paste0("-",month))) |>
  mutate(year_month = paste0(year,month)) |>
  group_by(year_month) |>
  summarise(med =  365/ 12 * median(aet,na.rm=T),
            q_33 = 365/ 12 * quantile(aet,0.33,na.rm=T),
            q_66 = 365/12 * quantile(aet,0.66,na.rm=T)) |>
  mutate(Setup = "P_model")

p_model2$date <- as.Date(paste0(p_model2$year_month, "-01"))


ggplot(p_model2, aes(x = date, y = med)) + geom_line()


# FDK aet calculation


era5 <- read_csv("~/data_scratch/global/dataset_comparison/era5_fluxnet.csv")

pml <- read_csv("~/data_scratch/global/dataset_comparison/PML_fluxnet.csv")

fluxcom <- read_csv("../global/dataset_comparison/fluxicom_fluxnet.csv")

fdk_site_info <- read_csv("/data_2/FluxDataKit/v3.4/zenodo_upload/fdk_site_info.csv")

# data quality filter info
fdk_filter <-  read_csv("/data_2/FluxDataKit/v3.4/zenodo_upload/fdk_site_fullyearsequence.csv")

# exclude croplands and wetlands from calibration/evaluation
fdk_site_info <- fdk_site_info[fdk_site_info$igbp_land_use != "CRO" &
                                 fdk_site_info$igbp_land_use != "WET",]

fdk_site_info <- fdk_site_info[fdk_site_info$sitename != "CA-SF1" &
                                 fdk_site_info$sitename != "CA-SF3",]

# apply good year sequence data quality filter for LE

fdk_filter <- fdk_filter[fdk_filter$drop_le == "FALSE",]

fdk_site_info <- fdk_site_info[fdk_site_info$sitename %in% fdk_filter$sitename,]

fdk_filter <- fdk_filter[fdk_filter$sitename %in% fdk_site_info$sitename,]

driver <- read_rds("/data_2/FluxDataKit/v3.4/zenodo_upload/rsofun_driver_data_v3.4.2.rds")

driver <- driver[driver$sitename %in% fdk_site_info$sitename,]


driver_forcing <- driver |>
  select(sitename, forcing) |>
  unnest(cols = c(forcing))

driver <- driver_forcing |>
  left_join(
    fdk_filter |>
      select(
        sitename,
        year_start = year_start_le,
        year_end = year_end_le),
    by = join_by(sitename)
  ) |>
  mutate(year = year(date)) |>
  filter(year >= year_start & year <= year_end) |>
  filter(year < 2012) |>
  select(-year_start, -year_end, -year) |>
  group_by(sitename) |>
  nest() |>
  left_join(
    driver |>
      select(
        sitename,
        site_info,
        params_siml
      ),
    by = join_by(sitename)
  ) |>
  rename(forcing = data) |>
  select(sitename, params_siml, site_info, forcing) |>
  ungroup()


fdk_site_info <- fdk_site_info[fdk_site_info$sitename %in% driver$sitename,]

fdk_filter <- fdk_filter[fdk_filter$sitename %in% driver$sitename,]

calc_enthalpy_vap <- function (tc)
{
  enthalpy_vap <- 1918460 * ((tc + 273.15)/(tc + 273.15 -
                                              33.91))^2
  return(enthalpy_vap)
}


calc_density_h2o <- function (tc, press)
{
  po <- 0.99983952
  +6.78826e-05 * tc
  -9.08659e-06 * tc * tc
  +1.02213e-07 * tc * tc * tc
  -1.35439e-09 * tc * tc * tc * tc
  +1.47115e-11 * tc * tc * tc * tc * tc
  -1.11663e-13 * tc * tc * tc * tc * tc * tc
  +5.04407e-16 * tc * tc * tc * tc * tc * tc * tc
  -1.00659e-18 * tc * tc * tc * tc * tc * tc * tc * tc
  ko <- 19652.17
  +148.183 * tc
  -2.29995 * tc * tc
  +0.01281 * tc * tc * tc
  -4.91564e-05 * tc * tc * tc * tc
  +1.03553e-07 * tc * tc * tc * tc * tc
  ca <- 3.26138
  +0.0005223 * tc
  +0.0001324 * tc * tc
  -7.655e-07 * tc * tc * tc
  +8.584e-10 * tc * tc * tc * tc
  cb <- 7.2061e-05
  -5.8948e-06 * tc
  +8.699e-08 * tc * tc
  -1.01e-09 * tc * tc * tc
  +4.322e-12 * tc * tc * tc * tc
  pbar <- (1e-05) * press
  density_h2o <- 1000 * po * (ko + ca * pbar + cb * pbar^2)/(ko +
                                                               ca * pbar + cb * pbar^2 - pbar)
  return(density_h2o)
}

le_to_et <- function(df){
  1000 * 60 * 60 * 24 * df$le / (calc_enthalpy_vap(df$temp) * calc_density_h2o(df$temp, df$patm))
}

et <- le_to_et(driver |>
                 unnest(forcing)
)

et_df <- cbind(driver |>
                 unnest(forcing) |>
                 select(sitename,date),
               et)

montlhy_mean <- et_df |>
  group_by(sitename,year(date),month(date)) |>
  summarise(et = mean(et, na.rm = T))

rm(et,et_df)

montlhy_mean <- left_join(montlhy_mean |>
                            rename(year = 'year(date)',
                                   month = 'month(date)',
                                   obs_aet = et),
                          fdk_filter |>
                            select(sitename,year_start_le,year_end_le),
                          by = "sitename") |>
  filter(year >= year_start_le & year <= year_end_le) |>
  select(-year_start_le, -year_end_le)

# P model out data

# read_csv("/data_2/scratch/akurth/grsofun_output/PM-S0/final_aet_df_PM-S0_20251126_1155.csv")


fdk_filter <- fdk_filter[(year(fdk_filter$start_le )< 2012),]

fdk_site_info <- fdk_site_info[fdk_site_info$sitename %in% fdk_filter$sitename,]

p_model <- p_model[p_model$year > 1996,]

out_rsofun <- NULL

# find the nearest point (not trust netcdf)

for(i in 1:nrow(fdk_site_info)){

  lon_1 <- p_model[
    abs(fdk_site_info$lon[i]- p_model$lon) == min(abs(fdk_site_info$lon[i]- p_model$lon))
    ,]

  lon_1 <- lon_1[
    abs(fdk_site_info$lat[i]- lon_1$lat) == min(abs(fdk_site_info$lat[i]- lon_1$lat))
    ,]

  #lon_1 <- lon_1[1,] to test for correct location


  tmp <- data.frame(sitename = fdk_site_info$sitename[i],
                    date = date(paste0(lon_1$year,
                                       ifelse(lon_1$month < 10,"-0","-"),
                                       lon_1$month,"-01")),
                    rsofun = lon_1$aet)

  out_rsofun <- rbind(out_rsofun,tmp)
}

days_in_month <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)



days_in_month <- data.frame(month = seq(1,12,1),
                            days = c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31))


# out_rsofun <- left_join(
#   out_rsofun |> mutate(month = month(date)),
#   days_in_month,
#   by = "month") |>
#   mutate(rsofun = rsofun / days) |>
#   select(- month, - days)

out_rsofun <- left_join(out_rsofun ,
                        fdk_filter |>
                          select(sitename,year_start_le,year_end_le),
                        by = "sitename") |>
  mutate(year = year(date)) |>
  filter(year >= year_start_le & year <= year_end_le) |>
  select(-year_start_le, -year_end_le, -year)



# FLUXICOM data

# since FLUXIcom gives le in terms of MJ per day need to change to aet


fluxcom_le <- left_join(fluxcom |>
                          rename(le = le_MJ_day) |>
                          mutate(le = le *10^6 / (60 * 60 * 24)), # transofrm from MJ day to J s (hopefully bith per square meter)
                        driver |> unnest(forcing) |>
                          select(sitename,date,temp,patm),
                        by = c("sitename","date")) |>
  drop_na() # filter out NA value

fluxcom_le$aet <- le_to_et(fluxcom_le)

fluxcom_le_mean <- left_join(fluxcom_le |> rename(fluxcom = aet),
                             fdk_filter |>
                               select(sitename,year_start_le,year_end_le),
                             by = "sitename") |>
  filter(year(date) >= year_start_le & year(date) <= year_end_le) |>
  select(-year_start_le, -year_end_le)


# era5 data

era5 <- left_join(era5 |>
                    rename(era5 = aet ),
                  fdk_filter |>
                    select(sitename,year_start_le,year_end_le),
                  by = "sitename") |>
  mutate(year = year(date)) |>
  filter(year >= year_start_le & year <= year_end_le) |>
  select(-year_start_le, -year_end_le, -year)


# PML data

pml <- left_join(pml |>
                   rename(pml = aet ),
                 fdk_filter |>
                   select(sitename,year_start_le,year_end_le),
                 by = "sitename") |>
  mutate(year = year(date)) |>
  filter(year >= year_start_le & year <= year_end_le) |>
  select(-year_start_le, -year_end_le, -year)

# pml is in monthly sum, we want montly mean



pml <- left_join(
  pml |> mutate(month = month(date)),
  days_in_month,
  by = "month") |>
  mutate(pml = pml / days) |>
  select(- month, - days)


montlhy_mean$date <- date(paste0(montlhy_mean$year,
                                 ifelse(montlhy_mean$month < 10,"-0","-"),
                                 montlhy_mean$month,"-01"))


monthly_df <- left_join(
  era5,
  pml,
  by = c("sitename","date")
)

monthly_df <- left_join(
  monthly_df,
  fluxcom_le_mean,
  by = c("sitename","date")
)

monthly_df <- left_join(
  monthly_df,
  montlhy_mean |> select(sitename, date, obs_aet) |>ungroup(),
  by = c("sitename","date")
) |>
  select(-year)

monthly_df <- left_join(
  monthly_df,
  out_rsofun |> select(sitename, date, rsofun) |> mutate(date = date(date))|>ungroup(),
  by = c("sitename","date")
)


rmse <- monthly_df |>
  mutate(#rsofun_global = sqrt((obs_aet-rsofun)^2),
    Pml = sqrt((obs_aet-pml)^2),
    ERA5 = sqrt((obs_aet-era5)^2),
    Fluxcom = sqrt((obs_aet-fluxcom)^2),
    P_model = sqrt((obs_aet - rsofun)^2)) |>
  select(#rsofun_global,
    Pml,ERA5,Fluxcom,P_model) |>
  pivot_longer(
    everything(),
    names_to = "model",
    values_to = "RMSE")

rmse_plot <- ggplot(rmse,aes(x = model, y = RMSE)) + geom_boxplot() +
  ylab( expression( paste("RMSE (mm d"^-1, ")" ))) +
  scale_x_discrete(
    limits = c("P_model","Pml","Fluxcom","ERA5")
  ) + theme(aspect.ratio = 1.5)



corr <- monthly_df |>
  drop_na() |>
  # filter(!(
  #        sitename == "AU-Cow" |
  #        sitename == "AU-Ctr" |
  #        sitename == "FR-Hes" |
  #          sitename == "AR-SLu"  )) |> # dont know doesn't remove french site

  group_by(sitename) |>
  summarise(#rsofun_global = cor(obs_aet, rsofun, use = "complete.obs")^2,
    Pml = cor(obs_aet, pml, use = "complete.obs")^2,
    ERA5 = cor(obs_aet, era5, use = "complete.obs")^2,
    Fluxcom = cor(obs_aet, fluxcom, use = "complete.obs")^2,
    P_model = cor(obs_aet, rsofun, use = "complete.obs")^2) |>
  select(
    Pml,ERA5,Fluxcom,P_model) |>
  pivot_longer(
    everything(),
    names_to = "model",
    values_to = "correlation")

# in case to see the sitename
# corr <- monthly_df |>
#   drop_na() |>
#   group_by(sitename) |>
#   summarise(
#     Pml     = cor(obs_aet, pml,     use = "complete.obs")^2,
#     ERA5    = cor(obs_aet, era5,    use = "complete.obs")^2,
#     Fluxcom = cor(obs_aet, fluxcom, use = "complete.obs")^2,
#     P_model = cor(obs_aet, rsofun,  use = "complete.obs")^2
#   ) |>
#   pivot_longer(
#     cols = c(Pml, ERA5, Fluxcom, P_model),
#     names_to = "model",
#     values_to = "correlation"
#   )


cor_plot <- ggplot(corr,aes(x = model, y = correlation)) + geom_boxplot() +
  scale_x_discrete(
    limits = c("P_model","Pml","Fluxcom","ERA5")
  ) + theme(aspect.ratio = 1.5)

combined <- cowplot::plot_grid(rmse_plot,cor_plot,ncol = 2, labels = letters[1:2])

print(combined)

ggsave(plot = combined, paste0("../my_stuff/global_metrics_aet_a.svg"),device = "svg", dpi = 300, width = 16.5, height = 7)

ggsave(plot = lateral_profile, paste0("../my_stuff/lateral_profile_a.svg"),device = "svg", dpi = 300, width = 16.5, height = 7)


# WORLD MAP AET

map_world <- p_model |>
  group_by(lat,lon) |>
  summarize(aet = 365 *(mean(aet,na.rm = T))) |>
  drop_na()


library(scico)

find_closest_color <- function(value, palette) {
  palette_values <- seq(0, 1, length.out = length(palette))  # Scale palette values
  closest_index <- which.min(abs(palette_values - value))   # Find closest match
  return(palette[closest_index])
}

lapaz <- scico_palette_data("lapaz")

lapaz_i <- NULL

for(i in 256:1){

  tmp <- lapaz[i,]
  lapaz_i <- rbind(lapaz_i,tmp)
}

lapaz_i$hex <- rgb(lapaz_i$r , lapaz_i$g , lapaz_i$b )

map_world$norm <- (map_world$aet - min(map_world$aet))/(max(map_world$aet)-min(map_world$aet))


map_world$hex <-  sapply(map_world$norm, find_closest_color, palette = lapaz_i$hex)

breaks <- seq(0,2750, length.out = 11)

source("../my_stuff/global_legend.R")

# get coast outlines
layer_coast <- rnaturalearth::ne_coastline(
  scale = 110,
  returnclass = "sf"
)

# get ocean layer
layer_ocean <- rnaturalearth::ne_download(
  scale = 110,
  type = "ocean",
  category = "physical",
  returnclass = "sf",
  destdir = here::here("data/")
)


ggmap <- ggplot() +
  geom_raster(data = map_world, aes(x = lon, y = lat, fill = hex)) +
  scale_fill_identity() +
  geom_sf(data = layer_ocean, color = NA, fill = "azure3") +
  geom_sf(data = layer_coast, colour = 'black', linewidth = 0.1) +
  xlab(NULL) +
  ylab(NULL) +
  theme_classic() +
  theme(
    axis.title = element_blank(),
    axis.text  = element_blank(),
    axis.ticks = element_blank(),
    axis.line  = element_blank(),
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "white")
  )


gglegend <- plot_discrete_cbar(
  breaks = breaks,
  colors = lapaz_i$hex[seq(1,256,26)],
  legend_title = expression( paste("Simulated AET (mm yr"^{-1}, ")" ) ),
  legend_direction = "vertical",
  width = 0.03,
  font_size = 3,
  expand_size_y = 0.5,
  spacing = "constant"
)

global_merged <- cowplot::plot_grid(ggmap, gglegend, ncol = 2, rel_widths = c(1, 0.10))

print(global_merged)

ggsave(plot = global_merged, paste0("../my_stuff/global_aet_a.svg"),device = "svg", dpi = 300, width = 16.5, height = 7)

