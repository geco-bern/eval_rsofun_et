library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(rsofun)
library(readr)
library(tictoc)
library(terra)
library(cowplot)
library(scico)
library(viridis)
library(maps)
library(sf) # only used to plot


source("./R/main_plus_metrics.R")
source("./R/get_stats.R")
source("./R/analyse_modobs2.R")
source("./R/align_events.R")
source("./R/eval_droughtresponse.R")
source("./R/heatscatter_dependencies.R")
source("./R/create_obs_eval.R")
source("./R/global_legend.R")

scaling_factor <- 1.1

theme_set(
  theme_minimal(base_size = 10) +
    theme(
      axis.title  = element_text(size = 10),
      axis.text   = element_text(size = 10),
      legend.title = element_text(size = 10),
      legend.text  = element_text(size = 10)
    )
)


driver_data <- readRDS("./data/camels/camels_driver.rds")

meta_info  <- read_csv("./data/camels/camels_site_info.csv")

vector_path <- "./data/camels/camels_basin_shapes.shp"

shapefile <- terra::vect(vector_path)


site_info <- read_csv("./data/camels/attributes_hydroatlas_camels.csv")

# filter by degree of regulation (0 = no human influence)
site_info <- site_info[site_info$dor_pc_pva == 0,]

driver_data <- driver_data[driver_data$sitename %in% site_info$gauge_id,]

# filter out site with runoff higher than precipitation
multi_year <- driver_data |>
  unnest(forcing) |>
  select(sitename,date, rain, runoff) |>
  mutate(rain = rain * 24* 60 * 60) |>
  group_by(sitename, year(date)) |>
  summarise(rain = sum(rain),
            runoff = sum(runoff)) |>
  group_by(sitename) |>
  summarise(rain = mean(rain),
            runoff = mean(runoff)) |>
  filter(rain > runoff)

# set reference height equals to canopy height
# otherwise, some simulations fails (see readme)
driver_data <- driver_data |> unnest(site_info) |>
  mutate(reference_height = canopy_height) |> # best for now
  group_by(sitename) |>
  nest(site_info = c(lat,lon,elv,whc,canopy_height,reference_height))

# set vwind to be always positive (TODO: fix the generation of driver data)
driver_data <- driver_data |> unnest(forcing) |>
  mutate(vwind = abs(vwind)) |>
  group_by(sitename) |>
  nest(forcing = c(date, temp, vpd, ppfd, netrad, patm, snow, rain, tmin, tmax, vwind, fapar, co2, ccov, runoff))

driver_data <- driver_data[driver_data$sitename %in% multi_year$sitename,]

best_par <- readRDS("./data/fluxnet/global_calib_PMS0.rds")

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

# run the model for these parameters
output <- rsofun::runread_pmodel_f(
  driver_data,
  par = params_modl
)

# some sites still fails, remove the sites with et less than 200mm (in a 30 years period)

output_filter <- output |>
  unnest(data) |>
  mutate(year = year(date)) %>%
  group_by(year, sitename) %>%
  summarise(mod = sum(aet, na.rm = T))

obs_eval <- create_obs_eval(driver_data, scale = "camels")


settings_eval <- list(
  benchmark = list( aet = c("camels") ),
  sitenames = output$sitename,
  agg       = 8
)

out_eval <- eval_sofun(
  output,
  settings_eval,
  obs_eval = obs_eval,
  overwrite = TRUE,
  light = FALSE
)

plot_1_presentation <- out_eval$aet$camels$plot$gg_modobs_multi_annual

plot_2 <- out_eval$aet$camels$plot$gg_modobs_spatial_annual

performace_site <- out_eval$aet$camels$data$iavdf_stats

# histograms of slopes distribution

plot_3 <- ggplot(performace_site,aes(x = slope)) + geom_histogram(bins = 50, fill = "white", colour = "black") +
  xlab("Slope of the fitted line") + geom_vline(xintercept = 1, color = "red", linetype = "dotted", size = 1)


metrics_plot <- plot_grid(plot_1,plot_2,plot_3,ncol = 3, labels = letters[1:3])
print(metrics_plot)

ggsave(plot = metrics_plot, paste0("./fig/","metrics_plot.pdf"),device = "pdf", dpi = 300, width = 21 * scaling_factor , height = 7 *scaling_factor)

ggsave(plot = metrics_plot, paste0("./fig/","metrics_plot.svg"),device = "svg", dpi = 300, width = 21 * scaling_factor , height = 7 *scaling_factor)

# Budyko relationship

climates <- read_csv("./data/camels/long_koeppen_camels.csv")

climates <- climates[climates$sitename %in% driver_data$sitename,]

max_climates <- climates |>
  group_by(sitename) |>
  filter(fraction == max(fraction))

df_budyko <- left_join(output |>
  unnest(data) |>
  group_by(sitename, year(date)) |>
  summarise(mod_aet = sum(aet),
            pet = sum(pet),
            cond = sum(cond)) |>
  group_by(sitename) |>
  summarise(mod_aet = mean(mod_aet),
            pet = mean(pet),
            cond = mean(cond)),
  multi_year, by = "sitename"
  ) |>
  mutate(rain_2 = rain + cond)

df_budyko <- left_join(df_budyko,
                       out_eval$aet$camels$data$meandf |> select(sitename, obs) |> rename(obs_aet = obs),
                       by = "sitename")

df_budyko <- left_join(df_budyko,
                       max_climates |> select(sitename, koeppen_code),
                       by = "sitename")


mean_climates <- df_budyko |>
  group_by(koeppen_code) |>
  mutate(n = n()) |>
  summarise(
    mod_aet = mean(mod_aet, na.rm = TRUE),
    obs_aet = mean(obs_aet, na.rm = TRUE),
    rain = mean(rain, na.rm = TRUE),
    pet  = mean(pet, na.rm = TRUE),
    n    = mean(n)
  ) |>
  filter(n > 4) |>
  pivot_longer(
    cols = c(mod_aet, obs_aet),
    names_to = "source",
    values_to = "aet"
  )


plot_2_presentation <- ggplot() +
  geom_abline(slope = 1, intercept = 0,  linetype = "dotted") +
  geom_hline(yintercept = 1, , linetype = "dotted") +
  geom_point(data = df_budyko , aes(x = pet / rain, y = mod_aet / rain), size = 1) +
  ylab("AET / precipitation") + xlab("PET / precipitation") + ylim(0,1.2) + xlim(0,10) +
  theme(aspect.ratio = 1)

plot_3_presentation <- ggplot() +
  geom_abline(slope = 1, intercept = 0, , linetype = "dotted") +
  geom_hline(yintercept = 1, , linetype = "dotted") +
  geom_point(data = df_budyko , aes(x = pet / rain, y = obs_aet / rain), size = 1) +
  ylab("AET / precipitation") + xlab("PET / precipitation") + ylim(0,1.2) + xlim(0,10) +
  theme(aspect.ratio = 1)

presentation_plot <- plot_grid(plot_2_presentation,plot_3_presentation,ncol = 2)
ggsave(plot = presentation_plot, paste0("./fig/","budyko_presentation.svg"),device = "svg", dpi = 300, width = 14 * scaling_factor , height = 7 *scaling_factor)
ggsave(plot = plot_1_presentation, paste0("./fig/","camels_metric.svg"),device = "svg", dpi = 300, width = 7 * scaling_factor , height = 7 *scaling_factor)



plot_3 <- ggplot() +
  geom_abline(slope = 1, intercept = 0, linetype = "dotted") +
  geom_hline(yintercept = 1, linetype = "dotted") +
  geom_point(
    data = mean_climates,
    aes(
      x = pet / rain,
      y = aet / rain,
      shape = source,
      color = koeppen_code
    ),
    size = 2
  )  +
  scale_shape_manual(
    name = "Data Type",
    labels = c("Modelled", "Observed"),
    values = c(16, 17)# shapes: 16 = circle, 17 = triangle
  )+
  scale_color_viridis_d(
    option = "C",  # one between C D or E
    name = "Climate"
  ) +
  ylab("AET / precipitation") +
  xlab("PET / precipitation") +
  coord_cartesian(xlim = c(0, 4), ylim = c(0, 1.2)) +
  theme_minimal() +
  theme(
    legend.position = "right",
    aspect.ratio = 1
  )

legend_plot <- cowplot::get_legend(plot_3)

plot_3_nolegend <- plot_3 + theme(legend.position = "none")

budyko_plot <- plot_grid(plot_1,plot_2,plot_3_nolegend,ncol = 3, labels = letters[1:3],label_y = 1)
print(budyko_plot)

# add legend separately

budyko_plot_2 <- plot_grid(budyko_plot,legend_plot,ncol = 2, rel_widths = c(1, 0.05), align = "hv")

print(budyko_plot_2)

ggsave(plot = budyko_plot_2, paste0("./fig/","budyko_merged.pdf"),device = "pdf", dpi = 300, width = 21 * scaling_factor , height = 7 *scaling_factor)

ggsave(plot = budyko_plot_2, paste0("./fig/","budyko_merged.svg"),device = "svg", dpi = 300, width = 21 * scaling_factor , height = 7 *scaling_factor)


# Budyko plot with transpiration instead of AET

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
  1000 * df$le_canopy / (calc_enthalpy_vap(df$temp) * calc_density_h2o(df$temp, df$patm))
}

# * 60 * 60 * 24   * calc_density_h2o(df$temp, df$patm)
transpiration <- output |>
  unnest(data) |>
  select(le_canopy)

driver <- driver_data |>
  unnest(forcing)

driver$le_canopy <- transpiration$le_canopy

et <-  le_to_et(driver)

driver$et <- et

driver <- driver |>
  select(sitename,date,rain,et) |>
  mutate(rain = rain * 24* 60 * 60)

output2 <- output |>
  unnest(data) |>
  select(sitename, date, pet)

df_budyko <- left_join(driver,output2, by = c("sitename","date")) |>
  group_by(sitename, year(date)) |>
  summarise(pet = sum(pet, na.rm = T),
            aet = sum(et, na.rm = T),
            rain = sum(rain, na.rm = T)) |>
  group_by(sitename) |>
  summarise(pet = mean(pet, na.rm = T),
            aet = mean(aet, na.rm = T),
            rain = mean(rain, na.rm = T))# |>
  #mutate(aet = aet / 10000)

df_budyko <- left_join(df_budyko,
                       max_climates |> select(sitename, koeppen_code),
                       by = "sitename")

ggplot() +
  geom_abline(slope = 1, intercept = 0, linetype = "dotted") +
  geom_hline(yintercept = 1, linetype = "dotted") +
  geom_point(
    data = df_budyko,
    aes(
      x = pet / rain,
      y = aet / rain,
      color = koeppen_code
    )) + ylim(0,1) +
  scale_y_log10()


ggplot() +
  geom_abline(slope = 1, intercept = 0, linetype = "dotted") +
  geom_hline(yintercept = 1, linetype = "dotted") +
  geom_point(
    data = df_budyko,
    aes(
      x = pet / rain,
      y = aet / rain,
      color = koeppen_code
    )
  ) +
  scale_y_log10(
    limits = c(0.01, 1),
    breaks = c(0.05,0.1,0.2,0.3),
    labels = c(0.05,0.1,0.2,0.3)
  ) + theme_minimal()+
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(),
    panel.grid.major.y = element_line()
  )




# global map

us_map <- map_data("state")

deltas <- out_eval$aet$camels$data$meandf |>
  mutate(delta = mod - obs)

shapefile <- shapefile[shapefile$gauge_id %in% driver_data$sitename,]

shapefile$delta <- deltas$delta

# to be centered around 0, the legend extent will be - 700, 700

breaks <- seq(-700,700, length.out = 9)

vik <- scico_palette_data("vik")
vik$hex <- rgb(vik$r , vik$g , vik$b )

# this function will create the legend to put near to the US map
gglegend <- plot_discrete_cbar(
  breaks = breaks,
  colors = vik$hex[seq(1,256, length.out =length(breaks) -1)],
  legend_title = expression( paste("Simulated AET (mm yr"^{-1}, ")" ) ),
  legend_direction = "vertical",
  width = 0.03,
  font_size = 3,
  expand_size_y = 0.5,
  spacing = "constant"
)

print(gglegend)

# plot deltas
shapefile_sf  <- st_as_sf(shapefile)

# Define a large rectangle covering your plot region (a bit larger than US)
outer <- data.frame(
  long = c(-126, -126, -65, -65, -126),
  lat  = c(24, 50, 50, 24, 24),
  group = "outer"
)

# Mark US map group as 'us'
us_map$group <- paste0("us_", us_map$group)

# Combine them into one data frame
outside <- bind_rows(
  outer,
  us_map
)


ggmap <- ggplot() +
  geom_polygon(data = outside, aes(x = long, y = lat, group = group),
               fill = "azure3", color = NA) +
  geom_polygon(data = us_map, aes(x = long, y = lat, group = group), fill = "lightgray", color = "black") +
  geom_sf(data = shapefile_sf, aes(fill = delta), colour = "black") +
  scale_fill_scico(palette = "vik",
                   midpoint = 0,
                   limits = c(breaks[1],breaks[length(breaks)]),
                   direction = 1) +
  coord_sf(expand = FALSE) +
  theme_minimal() +
  theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.position = "none" # disable it if you want to check the values
        )

print(ggmap)



global_merged <- cowplot::plot_grid(ggmap, gglegend, ncol = 2, rel_widths = c(1, 0.23),axis = "lrtb")

print(global_merged)

ggsave(plot = global_merged, paste0("./fig/","camels_map.pdf"),device = "pdf",units  = "px", dpi = 300, width = 1707 /0.45, height = 829 /0.45)

ggsave(plot = global_merged, paste0("./fig/","camels_map.svg"),device = "svg",units  = "px", dpi = 300, width = 1707 /0.45, height = 829 /0.45)
