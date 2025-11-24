library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(cowplot)
library(rnaturalearthdata)
# files <- list.files("~/data_scratch/global/")
# 
# 
# files <- files[grep("global_output",files)]
# 
# df_combined <- NULL
# for(i in files){
# 
#   tmp <- read_csv(paste0("~/data_scratch/global/",i))
# 
#   df_combined <- rbind(df_combined,tmp)
# 
#   rm(tmp)
# 
#   gc()
# 
# }
# 
# write_csv(df_combined,"~/data_scratch/global/combined.csv")

global_df <- read_csv("~/data_scratch/global/driver/combined_gpp.csv")

colnames(global_df) <- c("sitename","year", "month", "aet", "pet", "lat","lon"  )

years <- seq(1982,2011,1)

months <- seq(1,12,1)

time_filter <- data.frame(year = rep(years,times = 1, each = 12), month = months)

yearly <- global_df |>
  group_by(sitename,year) |>
  summarise(aet = sum(aet),na.rm = T)

multi_year <- yearly |> 
  group_by(sitename) |>
  summarise(aet = mean(aet,na.rm = T))

# remove outlier for the plot
multi_year$aet <- ifelse(multi_year$aet > 1530,1530,multi_year$aet)

multi_year <- multi_year |> drop_na()


multi_year <- left_join(
  multi_year,
  global_df |> dplyr::select(sitename, lon ,lat) |>
    slice(seq(1, dim(global_df)[1], 360)), # in this way I only keep a single value for each coord
  by = "sitename")

# 
# for(i in 1:360){
#   df_plot <- global_df |>
#     filter(year == time_filter$year[i] & month == time_filter$month[i])
#   
#   
#   plot_1 <- ggplot(df_plot, aes(x = lon, y = lat, color = aet)) + 
#     geom_point(size = 0.5) + scale_colour_viridis_c() +
#     ggtitle(paste0(time_filter$year[i],"-", time_filter$month[i]))
#   
#   
#   if(time_filter$month[i] < 10){
#     filename <- paste0("~/data_scratch/global/global_png/plot",time_filter$year[i],"_0",time_filter$month[i],".png")
#   }else{
#     filename <- paste0("~/data_scratch/global/global_png/plot",time_filter$year[i],"_",time_filter$month[i],".png")
#   }
#   
#    
#   
#   ggsave(plot = plot_1, filename = filename, width = 13*1.5, height = 7*1.5, units = "in", dpi = 300)
#   
#   message(paste0("done ",substr(filename,38,44)))
# }
# 
# 
# library(gifski)
# png_files <- list.files('~/data_scratch/global/global_png', pattern = ".*png$", full.names = TRUE)
# gifski(png_files,width = 1700, height = 998, gif_file = "~/data_scratch/global/global.gif", delay = 1/10)



ggplot(multi_year , aes(x = lon, y = lat, color = aet)) + 
      geom_tile() + scale_colour_viridis_c()


multi_year <- multi_year |> filter(aet < 2500)

lat_profile <- multi_year |>
  group_by(lat) |>
  summarise(aet = median(aet,na.rm=T))

plot_2 <- ggplot(lat_profile, aes(x = lat, y = aet)) + 
  geom_line() + coord_flip() 

final_plot <- plot_grid(plot_1, plot_2, nrow = 1, rel_widths = c(1, 0.3)) +
  ggtitle("multi year average") 

print(final_plot)


year_sum <- yearly |>
  group_by(year) |>
  summarise(aet = (sum(aet,na.rm = T))/64945) # normalized by grid cell

ggplot(year_sum, aes(x = year, y = aet)) + geom_line() +
  ggtitle("mean yearly aet per grid cell (0.5 X 0.5)")


# change color palette

multi_year$aet <- ifelse(multi_year$aet > 1530,1530,multi_year$aet)

multi_year$norm <- (multi_year$aet - min(multi_year$aet))/(max(multi_year$aet)-min(multi_year$aet))

find_closest_color <- function(value, palette) {
  palette_values <- seq(0, 1, length.out = length(palette))  # Scale palette values
  closest_index <- which.min(abs(palette_values - value))   # Find closest match
  return(palette[closest_index])
}

library(scico)


lapaz <- scico_palette_data("lapaz")

lapaz_i <- NULL

for(i in 256:1){
  
  tmp <- lapaz[i,]
  lapaz_i <- rbind(lapaz_i,tmp)
}

lapaz_i$hex <- rgb(lapaz_i$r , lapaz_i$g , lapaz_i$b )

multi_year$hex <-  sapply(multi_year$norm, find_closest_color, palette = lapaz_i$hex)



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




ggplot(multi_year, aes(x = lon, y = lat, fill = hex)) + 
  geom_raster() + scale_fill_identity() + 
  geom_sf(
    data = layer_ocean,
    color = NA,
    fill = "azure3"
  )
  

ggsave("../global_aet.pdf", device = "pdf", dpi = 300,)




davos <- scico_palette_data("davos")

davos_i <- NULL

for(i in 256:1){
  
  tmp <- davos[i,]
  davos_i <- rbind(davos_i,tmp)
}

davos_i$hex <- rgb(davos_i$r , davos_i$g , davos_i$b )


assigned_colors <- sapply(multi_year$norm, find_closest_color, palette = davos_i$hex)

multi_year$hex <- assigned_colors

ggplot(multi_year, aes(x = lon, y = lat, fill = hex)) + 
  geom_tile() + scale_fill_identity() +
  theme(legend.position = "none")


# try 3 differente approach

multi_year <- yearly |> 
  group_by(sitename) |>
  summarise(aet = mean(aet,na.rm = T))

multi_year <- multi_year |> drop_na()


multi_year <- left_join(
  multi_year,
  global_df |> dplyr::select(sitename, lon ,lat) |>
    slice(seq(1, dim(global_df)[1], 360)), # in this way I only keep a single value for each coord
  by = "sitename")

multi_year$norm <- (multi_year$aet - min(multi_year$aet))/(max(multi_year$aet)-min(multi_year$aet))

multi_year$hex <-  sapply(multi_year$norm, find_closest_color, palette = lapaz_i$hex)

ggplot(multi_year, aes(x = lon, y = lat, fill = hex)) + 
  geom_tile() + scale_fill_identity() +
  theme(legend.position = "none") 


multi_year$aet <- ifelse(multi_year$aet > 1905,1905,multi_year$aet)

multi_year$norm <- (multi_year$aet - min(multi_year$aet))/(max(multi_year$aet)-min(multi_year$aet))

multi_year$hex <-  sapply(multi_year$norm, find_closest_color, palette = lapaz_i$hex)

ggplot(multi_year, aes(x = lon, y = lat, fill = hex)) + 
  geom_tile() + scale_fill_identity() +
  theme(legend.position = "none") +
  ggtitle("AET capped at 1905")


ggsave("aet_1905.PNG", width = 1090, height = 771, dpi = 300, units = "px", scale = 3)


multi_year$aet <- ifelse(multi_year$aet > 1530,1530,multi_year$aet)

multi_year$norm <- (multi_year$aet - min(multi_year$aet))/(max(multi_year$aet)-min(multi_year$aet))

multi_year$hex <-  sapply(multi_year$norm, find_closest_color, palette = lapaz_i$hex)

ggplot(multi_year, aes(x = lon, y = lat, fill = hex)) + 
  geom_tile() + scale_fill_identity(
    # guide = "legend",name = "Original Value",labels = c(
    # min(multi_year$aet),
    # median(multi_year$aet),
    # max(multi_year$aet
    #     )
    ) +
#, breaks = 3 ) +
  theme(legend.position = "right")


ggsave("../global_aet.PNG", width = 21, height = 15, dpi = 300,)



multi_year$aet <- ifelse(multi_year$aet > 1300,1300,multi_year$aet)

multi_year$norm <- (multi_year$aet - min(multi_year$aet))/(max(multi_year$aet)-min(multi_year$aet))

multi_year$hex <-  sapply(multi_year$norm, find_closest_color, palette = lapaz_i$hex)

ggplot(multi_year, aes(x = lon, y = lat, fill = hex)) + 
  geom_tile() + scale_fill_identity() +
  theme(legend.position = "none") +
  ggtitle("AET capped at 1300")


ggsave("aet_1300.PNG", width = 1090, height = 771, dpi = 300, units = "px", scale = 3)






# square root

multi_year$norm <- sqrt((multi_year$aet - min(multi_year$aet))/(max(multi_year$aet)-min(multi_year$aet)))

multi_year$hex <-  sapply(multi_year$norm, find_closest_color, palette = lapaz_i$hex)

ggplot(multi_year, aes(x = lon, y = lat, fill = hex)) + 
  geom_tile() + scale_fill_identity() +
  theme(legend.position = "none") +
  ggtitle("AET norm sqrt")

ggsave("aet_sqrt.PNG", width = 1090, height = 771, dpi = 300, units = "px", scale = 3)


# norm only with some intervals

multi_year$norm <- (multi_year$aet - min(multi_year$aet))/(max(multi_year$aet)-min(multi_year$aet))

multi_year$norm <- ifelse(multi_year$norm > 0.2, sqrt(multi_year$norm),multi_year$norm)







find_closest_color <- function(value, palette) {
  palette_values <- seq(0, 1, length.out = length(palette))  # Scale palette values
  closest_index <- which.min(abs(palette_values - value))   # Find closest match
  return(palette[closest_index])
}

library(scico)

fes <- scico_palette_data("bukavu")

fes$hex <- rgb(fes$r , fes$g , fes$b )

fes <- fes[129:256,]


fes_i <- NULL

for(i in 128:1){
  
  tmp <- fes[i,]
  fes_i <- rbind(fes_i,tmp)
}

fes_i$hex <- rgb(fes_i$r , fes_i$g , fes_i$b )


global_df <- read_csv("~/data_scratch/global/driver/combined_gpp.csv")

colnames(global_df) <- c("sitename","year", "month", "aet", "gpp", "lat","lon"  )



yearly <- global_df |>
  group_by(sitename,year) |>
  summarise(gpp = sum(gpp),na.rm = T)

multi_year <- yearly |> 
  group_by(sitename) |>
  summarise(gpp = mean(gpp,na.rm = T))

# remove outlier for the plot
#multi_year$gpp <- ifelse(multi_year$gpp > 1530,1530,multi_year$gpp)

multi_year <- multi_year |> drop_na()


multi_year <- left_join(
  multi_year,
  global_df |> dplyr::select(sitename, lon ,lat) |>
    slice(seq(1, dim(global_df)[1], 360)), # in this way I only keep a single value for each coord
  by = "sitename")

multi_year$gpp <- ifelse(multi_year$gpp > 2100,2100,multi_year$gpp)


multi_year$norm <- (multi_year$gpp - min(multi_year$gpp))/(max(multi_year$gpp)-min(multi_year$gpp))

non_linear <- function(x){
  
  return(x/(1 + exp(-1.5)- exp(-1.5*x)))
}

multi_year$norm <- non_linear(multi_year$norm )

multi_year$hex <-  sapply(multi_year$norm, find_closest_color, palette = fes_i$hex)


ggplot(multi_year, aes(x = lon, y = lat, fill = hex)) + 
  geom_tile() + scale_fill_identity() +
  theme(legend.position = "none")

hist(multi_year$norm, breaks = 100)


# bukavu <- scico_palette_data("bukavu")
# 
# bukavu$hex <- rgb(bukavu$r , bukavu$g , bukavu$b )
# 
# bukavu <- bukavu[150:256,]
# 
# 
# bukavu_i <- NULL
# 
# for(i in 107:1){
#   
#   tmp <- bukavu[i,]
#   bukavu_i <- rbind(bukavu_i,tmp)
# }
# 
# bukavu_i$hex <- rgb(bukavu_i$r , bukavu_i$g , bukavu_i$b )
# 
# 
# multi_year$hex <-  sapply(multi_year$norm, find_closest_color, palette = bukavu_i$hex)
# 
# 
# ggplot(multi_year, aes(x = lon, y = lat, fill = hex)) + 
#   geom_tile() + scale_fill_identity() +
#   theme(legend.position = "none")
