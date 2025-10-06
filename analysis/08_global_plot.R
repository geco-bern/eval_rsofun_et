library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(readr)
library(terra)
library(scico)
library(geosphere) # use to find the nearest grid to fluxnet
library(sf)
library(cowplot)


# data loading

p_model <-  read_rds("./data/global/global_p_model_simulation.rds")

monthly_era5 <- read_rds("./data/global/monthly_era5.rds")

montlhy_pml <- read_rds("./data/global/montlhy_pml.rds")

montlhy_fluxcom <- read_rds("./data/global/montlhy_fluxcom.rds")


monthly_Pmodel  <- p_model |>
  unnest(data) |>
  mutate(year = year(date)) |>
  filter(year > 1996) |>
  group_by(year,lon,lat) |>
  summarise(aet = sum(aet,na.rm = T)) |>
  group_by(lat) |>
  summarise(med =median(aet,na.rm=T),
            q_33 = quantile(aet,0.33,na.rm=T),
            q_66 = quantile(aet,0.66,na.rm=T)) |>
  mutate(Setup = "P_model")


monthly_era5  <- monthly_era5 |>
  unnest(data) |>
  mutate(year = year(date)) |>
  filter(year > 1996) |>
  group_by(year,lon,lat) |>
  summarise(aet = sum(aet,na.rm = T)* 365/12) |>
  group_by(lat) |>
  summarise(med =median(aet,na.rm=T),
            q_33 = quantile(aet,0.33,na.rm=T),
            q_66 = quantile(aet,0.66,na.rm=T)) |>
  mutate(Setup = "ERA5")

montlhy_pml   <- montlhy_pml   |>
  unnest(data) |>
  mutate(year = year(date)) |>
  filter(year > 1996) |>
  group_by(year,lon,lat) |>
  summarise(aet = sum(aet,na.rm = T)) |>
  group_by(lat) |>
  summarise(med =median(aet,na.rm=T),
            q_33 = quantile(aet,0.33,na.rm=T),
            q_66 = quantile(aet,0.66,na.rm=T)) |>
  mutate(Setup = "PML")


montlhy_fluxcom   <- montlhy_fluxcom   |>
  unnest(data) |>
  mutate(year = year(date)) |>
  filter(year > 1996) |>
  group_by(year,lon,lat) |>
  summarise(aet = sum(aet,na.rm = T)* 365/12) |>
  group_by(lat) |>
  summarise(med =median(aet,na.rm=T),
            q_33 = quantile(aet,0.33,na.rm=T),
            q_66 = quantile(aet,0.66,na.rm=T)) |>
  mutate(Setup = "FLUXCOM")

ggplot(montlhy_fluxcom, aes(x = lat, y = med)) + geom_line()
ggplot(montlhy_pml, aes(x = lat, y = med)) + geom_line()
ggplot(monthly_era5, aes(x = lat, y = med)) + geom_line()
ggplot(monthly_Pmodel, aes(x = lat, y = med)) + geom_line()

df <- rbind(monthly_Pmodel,monthly_era5,montlhy_pml,montlhy_fluxcom)

# plot the median by lat of all models

ggplot(df, aes(x = lat, y = med, colour = Setup)) + geom_line() + coord_flip()


# remove everything (too much memory used)
rm(df,monthly_era5,montlhy_pml,montlhy_fluxcom)
gc()



# plot all the model distribution and the P model

monthly_era5 <- read_rds("./data/global/monthly_era5.rds")

montlhy_pml <- read_rds("./data/global/montlhy_pml.rds")

montlhy_fluxcom <- read_rds("./data/global/montlhy_fluxcom.rds")

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
                      select(year,aet,lat,lon)) |>
  group_by(year,lon,lat) |>
  summarise(aet = sum(aet,na.rm = T)) |>
  group_by(lat) |>
  summarise(
    med =median(aet,na.rm=T),
    q_33 = quantile(aet,0.33,na.rm=T),
    q_66 = quantile(aet,0.66,na.rm=T)) |>
  mutate(Setup = "mean_model")

df <- rbind(monthly_Pmodel,mean_model)



lat_profile <- ggplot(df) +
  geom_line( aes(x = lat,y = med,color = Setup),size = 0.9) +
  geom_ribbon(aes(x = lat,ymin = q_33,ymax = q_66,fill = Setup ), alpha = 0.3) +
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
  theme_classic() +
  theme(
    panel.grid.major.y = element_line(),
    panel.grid.major.x = element_line()
  ) + coord_flip() + theme(aspect.ratio = 1.5)

rm(df, mean_model, monthly_era5, montlhy_fluxcom, montlhy_pml, monthly_Pmodel)
gc()

# world map for P model

map_world <- p_model |>
  unnest(data) |>
  dplyr::select(date,aet,lat,lon) |>
  filter(lat > -60) |>
  group_by(year(date),lon,lat) |>
  summarise(aet = sum(aet,na.rm = T)) |>
  group_by(lon,lat) |>
  summarise(aet = mean(aet,na.rm = T))



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

breaks <- seq(0,1530, 150)

source("./R/global_legend.R")

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
  geom_raster(data =map_world, aes(x = lon, y = lat, fill = hex)) + scale_fill_identity() +
  geom_sf(
    data = layer_ocean,
    color = NA,
    fill = "azure3"
  ) +
  geom_sf(
    data = layer_coast,
    colour = 'black',
    linewidth = 0.1
  ) +
  xlab(NULL) +
  ylab(NULL) +
  theme_classic() +
  theme(axis.ticks.y.right = element_line(),
        axis.ticks.x.top = element_line(),
        panel.grid = element_blank(), plot.background = element_rect(fill = "white")
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

rm(map_world,lapaz,lapaz_i,gglegend,ggmap,global_merged,layer_coast,layer_ocean,tmp,breaks,i,plot_discrete_cbar,find_closest_color)
gc()

# plot performance against fluxnet

driver <- read_rds("data/fluxnet/rsofun_driver_data_v3.4.2.rds")
fdk_site_info <- read_csv("data/fluxnet/fdk_site_info.csv")

# data quality filter info
fdk_filter <-  read_csv("data/fluxnet/fdk_site_fullyearsequence.csv")

# exclude croplands and wetlands from evaluation
fdk_site_info <- fdk_site_info[fdk_site_info$igbp_land_use != "CRO" &
                                 fdk_site_info$igbp_land_use != "WET",]

# apply good year sequence data quality filter for LE
fdk_filter <- fdk_filter[fdk_filter$drop_le == "FALSE",]

# exclude site outside time range (from 1996 to 2011)
# the first tower starts in 1996, so only crop years after 2011

fdk_filter <- fdk_filter[fdk_filter$year_start_le <= 2011,]

fdk_filter$year_end_le <- ifelse(fdk_filter$year_end_le <= 2011, fdk_filter$year_end_le, 2011)


# include only the longest series of consecutive good quality data

driver <- driver[which(driver$sitename %in% fdk_site_info$sitename &
                         driver$sitename %in% fdk_filter$sitename),]

fdk_site_info <- fdk_site_info[which(fdk_site_info$sitename %in% driver$sitename),]

fdk_filter <- fdk_filter[which(fdk_filter$sitename %in% fdk_site_info$sitename),]

fdk_filter <- fdk_filter[duplicated(fdk_filter$sitename) == FALSE,]

driver_forcing <- driver |>
  dplyr::select(sitename, forcing) |>
  unnest(cols = c(forcing))

driver <- driver_forcing |>
  left_join(
    fdk_filter |>
      dplyr::select(
        sitename,
        year_start = year_start_le,
        year_end = year_end_le),
    by = join_by(sitename)
  ) |>
  mutate(year = year(date)) |>
  filter(year >= year_start & year <= year_end) |>
  dplyr::select(-year_start, -year_end, -year) |>
  group_by(sitename) |>
  nest() |>
  left_join(
    driver |>
      dplyr::select(
        sitename,
        site_info,
        params_siml
      ),
    by = join_by(sitename)
  ) |>
  rename(forcing = data) |>
  dplyr::select(sitename, params_siml, site_info, forcing) |>
  ungroup()

# transformation from le to ET

# Weird BUG: CWD don't load the function
# I copy from the package

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

# convert le to ET

le_to_et <- function(df){
  1000 * 60 * 60 * 24 * df$le / (calc_enthalpy_vap(df$temp) * calc_density_h2o(df$temp, df$patm))
}

# convert rain from second to rain
filter <- driver |>
  unnest(forcing) |>
  dplyr::select(sitename,le,temp,patm,rain) |>
  mutate(rain = rain * 60 * 60 * 24 )

filter$et <- le_to_et(filter)

filter <- filter |>
  group_by(sitename) |>
  summarise(rain = sum(rain,na.rm = T),
            et = sum(et,na.rm = T)) |>
  mutate(to_drop = ifelse(et > rain,T,F))

filter <- filter |> filter(to_drop == F)

driver <- driver[driver$sitename %in% filter$sitename,]

fdk_site_info <- fdk_site_info[fdk_site_info$sitename %in% driver$sitename,]


driver <- driver |>
  unnest(forcing) |>
  unnest(site_info) |>
  mutate(aet = 1000 * 60 * 60 * 24 * le / (calc_enthalpy_vap(temp) * calc_density_h2o(temp, patm))) |>
  select(sitename,lat,lon,date,aet)

driver_2 <- driver |>
  group_by(sitename,year(date),month(date)) |>
  summarise(aet = sum(aet, na.rm = T))

driver <- left_join(driver_2,driver[duplicated(driver$sitename) == FALSE,] |> select(sitename,lat,lon), by = "sitename")

rm(driver_2,driver_forcing,filter,calc_density_h2o,calc_enthalpy_vap,le_to_et)

# reframe p_model from date to coordinate (may be used to generate nc file)

p_model <- p_model |>
  filter(year(date) >= 1997) |>
  unnest(data) |>
  group_by(lat, lon) |>
  summarise(data = list(tibble(date = date, aet = aet)), .groups = "drop")


# Convert both datasets to sf objects
p_model_sf <- st_as_sf(p_model, coords = c("lon", "lat"), crs = 4326)
fdk_sf <- st_as_sf(fdk_site_info, coords = c("lon", "lat"), crs = 4326)

# Find index of nearest p_model grid cell for each site
nearest_idx <- st_nearest_feature(fdk_sf, p_model_sf)

model_sites <- fdk_site_info %>%
  mutate(model_data = p_model$data[nearest_idx])

model_long <- model_sites %>%
  select(sitename, lat, lon, model_data) %>%
  unnest(model_data) %>%
  rename(global_p_model_aet = aet)

driver_prepared <- driver %>%
  mutate(
    date = as.Date(sprintf("%d-%02d-01", `year(date)`, `month(date)`))
  ) %>%
  select(sitename, date, aet) |>
  rename(obs_aet = aet)

aet_comparison <- model_long %>%
  inner_join(driver_prepared, by = c("sitename", "date"))

aet_comparison <- aet_comparison %>%
  select(sitename, date, global_p_model_aet, obs_aet, lat, lon)

rm(p_model,p_model_sf)
gc()

print(aet_comparison)

# do the same for the other model


monthly_era5 <- read_rds("./data/global/monthly_era5.rds")

monthly_era5 <- monthly_era5 |>
  filter(year(date) >= 1997) |>
  unnest(data) |>
  group_by(lat, lon) |>
  summarise(data = list(tibble(date = date, aet = aet)), .groups = "drop")

monthly_era5_sf <- st_as_sf(monthly_era5, coords = c("lon", "lat"), crs = 4326)

nearest_idx <- st_nearest_feature(fdk_sf, monthly_era5_sf)

model_sites <- fdk_site_info %>%
  mutate(model_data = monthly_era5$data[nearest_idx])

model_long <- model_sites %>%
  select(sitename, lat, lon, model_data) %>%
  unnest(model_data) %>%
  mutate(aet = aet * 365/12) |>
  rename(era5_aet = aet)

aet_comparison <- left_join(aet_comparison ,
                            model_long |> select(sitename,date,era5_aet),
                            by = c("sitename","date"))

rm(monthly_era5,monthly_era5_sf)
gc()


montlhy_pml <- read_rds("./data/global/montlhy_pml.rds")

montlhy_pml <- montlhy_pml |>
  filter(year(date) >= 1997) |>
  unnest(data) |>
  group_by(lat, lon) |>
  summarise(data = list(tibble(date = date, aet = aet)), .groups = "drop")

montlhy_pml_sf <- st_as_sf(montlhy_pml, coords = c("lon", "lat"), crs = 4326)

nearest_idx <- st_nearest_feature(fdk_sf, montlhy_pml_sf)

model_sites <- fdk_site_info %>%
  mutate(model_data = montlhy_pml$data[nearest_idx])

model_long <- model_sites %>%
  select(sitename, lat, lon, model_data) %>%
  unnest(model_data) %>%
  rename(pml_aet = aet)

aet_comparison <- left_join(aet_comparison ,
                            model_long |> select(sitename,date,pml_aet),
                            by = c("sitename","date"))

rm(montlhy_pml,montlhy_pml_sf)
gc()

montlhy_fluxcom <- read_rds("./data/global/montlhy_fluxcom.rds")

montlhy_fluxcom <- montlhy_fluxcom |>
  filter(year(date) >= 1997) |>
  unnest(data) |>
  group_by(lat, lon) |>
  summarise(data = list(tibble(date = date, aet = aet)), .groups = "drop")

montlhy_fluxcom_sf <- st_as_sf(montlhy_fluxcom, coords = c("lon", "lat"), crs = 4326)

nearest_idx <- st_nearest_feature(fdk_sf, montlhy_fluxcom_sf)

model_sites <- fdk_site_info %>%
  mutate(model_data = montlhy_fluxcom$data[nearest_idx])

model_long <- model_sites %>%
  select(sitename, lat, lon, model_data) %>%
  unnest(model_data) %>%
  mutate(aet = aet * 365/12) |>
  rename(flx_aet = aet)

aet_comparison <- left_join(aet_comparison ,
                            model_long |> select(sitename,date,flx_aet),
                            by = c("sitename","date"))

rm(montlhy_fluxcom,montlhy_fluxcom_sf)
gc()

corr_plot <- aet_comparison |>
  group_by(sitename) |>
  summarise(p_model = cor(global_p_model_aet, obs_aet, use = "complete.obs")^2,
            pml = cor(pml_aet, obs_aet, use = "complete.obs")^2,
            fluxcom = cor(flx_aet, obs_aet, use = "complete.obs")^2,
            era5 = cor(era5_aet, obs_aet, use = "complete.obs")^2
  )

rmse_comparison <- aet_comparison |>
  group_by(sitename) |>
  summarise(p_model = mean(sqrt((global_p_model_aet - obs_aet)^2)),
            pml = mean(sqrt((pml_aet - obs_aet)^2)),
            fluxcom = mean(sqrt((flx_aet - obs_aet)^2)),
            era5 = mean(sqrt((era5_aet - obs_aet)^2)))

rmse_plot <- aet_comparison |>
  mutate(p_model = sqrt((global_p_model_aet - obs_aet)^2),
            pml = sqrt((pml_aet - obs_aet)^2),
            fluxcom = sqrt((flx_aet - obs_aet)^2),
            era5 = sqrt((era5_aet - obs_aet)^2)) |>
  select(sitename,p_model,pml,fluxcom,era5)

# bonus plot, bias across models

plot_1 <- ggplot(rmse_comparison, aes(x = p_model, y = pml)) + geom_point() +
  geom_abline(slope = 1, intercept = 0,  linetype = "dotted") +
  xlim(0,80) + ylim(0,80)

plot_2 <- ggplot(rmse_comparison, aes(x = p_model, y = fluxcom)) + geom_point() +
  geom_abline(slope = 1, intercept = 0,  linetype = "dotted") +
  xlim(0,80) + ylim(0,80)

plot_3 <- ggplot(rmse_comparison, aes(x = p_model, y = era5)) + geom_point() +
  geom_abline(slope = 1, intercept = 0,  linetype = "dotted") +
  xlim(0,80) + ylim(0,80)

plot_4 <- ggplot(corr_plot, aes(x = p_model, y = pml)) + geom_point() +
  geom_abline(slope = 1, intercept = 0,  linetype = "dotted") +
  xlim(0,1) + ylim(0,1)

plot_5 <- ggplot(corr_plot, aes(x = p_model, y = fluxcom)) + geom_point() +
  geom_abline(slope = 1, intercept = 0,  linetype = "dotted") +
  xlim(0,1) + ylim(0,1)

plot_6 <- ggplot(corr_plot, aes(x = p_model, y = era5)) + geom_point() +
  geom_abline(slope = 1, intercept = 0,  linetype = "dotted") +
  xlim(0,1) + ylim(0,1)

plot_grid(plot_1,plot_2,plot_3,
          plot_4,plot_5,plot_6,
          ncol = 3)

rmse_long <- rmse_plot %>%
  select(-sitename) %>%           # remove sitename
  pivot_longer(
    cols = everything(),          # pivot all remaining columns
    names_to = "model",           # new column for model names
    values_to = "rmse"            # new column for RMSE values
  )

rmse_plot <- ggplot(rmse_long,aes(x = model, y = rmse)) + geom_boxplot() +
  ylab( expression( paste("RMSE (mm m"^-1, ")" ))) +
  scale_x_discrete(
    limits = c("p_model","pml","fluxcom","era5")
  ) + theme(aspect.ratio = 1.5)

cor_long <- corr_plot %>%
  select(-sitename) %>%           # remove sitename
  pivot_longer(
    cols = everything(),          # pivot all remaining columns
    names_to = "model",           # new column for model names
    values_to = "corr"            # new column for RMSE values
  )

corr_plot <- ggplot(cor_long,aes(x = model, y = corr)) + geom_boxplot() +
  ylab("correlation") +
  scale_x_discrete(
    limits = c("p_model","pml","fluxcom","era5")
  ) + theme(aspect.ratio = 1.5)

plot_comb <- plot_grid(lat_profile, rmse_plot, corr_plot,ncol = 3, labels = letters[1:3])

scaling_factor = 1.5

ggsave(plot = plot_comb, paste0("./fig/","global_comparison.pdf"),device = "pdf", dpi = 300, width = 21 / scaling_factor, height = 7 / scaling_factor) # reduce size
ggsave(plot = plot_comb, paste0("./fig/","global_comparison.png"), dpi = 300, width = 21 / scaling_factor, height = 7 / scaling_factor) # reduce size
