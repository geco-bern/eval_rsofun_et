library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(readr)
# library(rsofun)
# library(ncdf4)

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

p_model <-  read_csv("~/data_scratch/global/driver/combined_gpp.csv")

colnames(p_model) <- c("sitename","year", "month","aet","gpp","lat","lon" )


monthly_era5 <- read_rds("../big_data/monthly_era5.rds")

montlhy_pml <- read_rds("../big_data/montlhy_pml.rds")

montlhy_fluxcom <- read_rds("../big_data/montlhy_fluxcom.rds")

# transform in yearly values

# 
# monthly_era52 <- monthly_era5 |>
#   unnest(data) |>
#   mutate(year = year(date)) |>
#   group_by(year,lon,lat) |>
#   summarise(aet = sum(aet,na.rm = T)) |>
#   group_by(lat) |>
#   summarise(med =median(aet,na.rm=T),
#             q_33 = quantile(aet,0.33,na.rm=T),
#             q_66 = quantile(aet,0.66,na.rm=T))
#   


# latidudinal profile


# p_model2 <- p_model |>
#   filter(year > 1996) |>
#   group_by(lat)|>
#   group_by(year,lon,lat) |>
#   summarise(aet = sum(aet,na.rm = T)) |>
#   group_by(lat) |>
#   summarise(med =median(aet,na.rm=T),
#             q_33 = quantile(aet,0.33,na.rm=T),
#             q_66 = quantile(aet,0.66,na.rm=T)) |>
#   mutate(Setup = "P_model")


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
  mutate(Setup = "P_model")

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
ggplot(p_model2, aes(x = lat, y = med)) + geom_line()

df <- rbind(p_model2,monthly_era5,montlhy_pml,montlhy_fluxcom)

ggplot(df, aes(x = lat, y = med, colour = Setup)) + geom_line()


# 
# ggplot(df) +
#   geom_line( aes(x = lat,y = med,color = Setup),size = 0.9) +
#   geom_ribbon(aes(x = lat,ymin = q_33,ymax = q_66,fill = Setup ), alpha = 0.3) +
#   # geom_line(data = monthly_era5, aes(x = lat,y = med,color = Setup),size = 0.9) +
#   # geom_ribbon(data = monthly_era5, aes(x = lat,ymin = q_0.25,ymax = q_0.75,fill = Setup ), alpha = 0.3) +
#   scale_color_manual(
#     values = c(
#       "ERA5" = "black",
#       "P_model" = "royalblue",
#       "PML" = "red",
#       "FLUXCOM" = "darkgreen"
#     ),
#     name = "Setup"
#   ) +
#   scale_fill_manual(
#     values = c(
#       "ERA5" = "black",
#       "P_model" = "royalblue",
#       "PML" = "red",
#       "FLUXCOM" = "darkgreen"
#     ),
#     name = "Setup") +
#  scale_x_continuous(
#     expand = c(0,0)
#   ) +
#   scale_y_continuous(
#     expand = c(0,0)
#   ) +
#   theme_classic() +
#   theme(
#     panel.grid.major.y = element_line(),
#     panel.grid.major.x = element_line()
#   ) + coord_flip()


# just p_model and the other
# 
monthly_era5 <- read_rds("../big_data/monthly_era5.rds")

montlhy_pml <- read_rds("../big_data/montlhy_pml.rds")

montlhy_fluxcom <- read_rds("../big_data/montlhy_fluxcom.rds")

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


df <- rbind(p_model2,mean_model)



ggplot(df) +
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
  theme_classic() +
  theme(
    panel.grid.major.y = element_line(),
    panel.grid.major.x = element_line()
  ) + coord_flip() + theme(aspect.ratio = 1.5)


plot_1 <- ggplot(df) +
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
  theme_classic() +
  theme(
    panel.grid.major.y = element_line(),
    panel.grid.major.x = element_line()
  ) + coord_flip() + theme(aspect.ratio = 1.5)

map_world <- rbind(monthly_era5  |>
                      filter(year(date) > 1996) |>
                      unnest(data) |>
                      select(date,aet,lat,lon) |>
                      filter(lat > -60) |>
                      mutate(aet = aet *365/12,
                             lat = ifelse(lat < 0,as.integer(20*lat + 1),as.integer(20*lat +2 )),
                             lon = ifelse(lon < 0,as.integer(20*lon -1),as.integer(20*lon))
                             ) |> # so they are aligned
                      mutate(lat = lat / 20,
                             lon = lon / 20
                             ), 
                    
                    montlhy_pml   |>
                      filter(year(date) > 1996) |>
                      unnest(data) |>
                      select(date,aet,lat,lon) ,
                    
                    montlhy_fluxcom   |>
                      filter(year(date) > 1996) |>
                      unnest(data) |>
                      mutate(aet = aet *365/12)|>
                      select(date,aet,lat,lon) 
                   )|>
  group_by(year(date),lat,lon) |>
  summarise(aet = sum(aet,na.rm=T)/3) |> # 3 is the number of model
  group_by(lat,lon) |>
  summarise(aet = mean(aet,na.rm=T))




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

breaks <- seq(0,1530, 150)

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
  geom_raster(data =multi_year, aes(x = lon, y = lat, fill = hex)) + scale_fill_identity() +
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


ggsave(plot = global_merged, paste0("../my_stuff/global.pdf"),device = "pdf", dpi = 300, width = 16.5, height = 7)
ggsave(plot = global_merged, paste0("../my_stuff/global.png"), dpi = 300, width = 21, height = 7)

