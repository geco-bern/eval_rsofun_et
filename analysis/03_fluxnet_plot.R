
# !!! Run only once if you don't have the correct branch
 # library(devtools)
 # devtools::install_github("geco-bern/rsofun", ref = "phydro")

# Load packages
library(rsofun)
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(lubridate)
library(knitr)
library(ggthemes)
library(here)
library(ncdf4)
library(cowplot)

# Load functions
source("R/main_plus_metrics.R")
source("R/get_stats.R")
source("R/analyse_modobs2.R")
source("R/align_events.R")
source("R/eval_droughtresponse.R")
source("R/heatscatter_dependencies.R")
source("R/create_obs_eval.R")

# used to reduce the image size
scaling_factor <- 1.5

# Set seed
set.seed(42)

# model forcing data
driver <- read_rds("data/fluxnet/rsofun_driver_data_v3.4.2.rds")

# site meta info
fdk_site_info <- read_csv("data/fluxnet/fdk_site_info.csv")

# some climates have the second letter uppercase, it needs to be lowercase

fdk_site_info$koeppen_code <-  ifelse(fdk_site_info$koeppen_code == "BSh","Bsh",fdk_site_info$koeppen_code)
fdk_site_info$koeppen_code <-  ifelse(fdk_site_info$koeppen_code == "BWh","Bwh",fdk_site_info$koeppen_code)
fdk_site_info$koeppen_code <-  ifelse(fdk_site_info$koeppen_code == "BSk","Bsk",fdk_site_info$koeppen_code)

# data quality filter info
fdk_filter <-  read_csv("data/fluxnet/fdk_site_fullyearsequence.csv")

# exclude croplands and wetlands from calibration/evaluation
fdk_site_info <- fdk_site_info[fdk_site_info$igbp_land_use != "CRO" &
                                 fdk_site_info$igbp_land_use != "WET",]


# apply good year sequence data quality filter for GPP
fdk_filter <- fdk_filter[fdk_filter$drop_gpp == "FALSE",]

# include only the longest series of consecutive good quality data

driver <- driver[which(driver$sitename %in% fdk_site_info$sitename &
                         driver$sitename %in% fdk_filter$sitename),]

fdk_site_info <- fdk_site_info[which(fdk_site_info$sitename %in% driver$sitename),]

fdk_filter <- fdk_filter[which(fdk_filter$sitename %in% driver$sitename &
                                 fdk_filter$sitename %in% fdk_site_info$sitename),]

driver_forcing <- driver |>
  dplyr::select(sitename, forcing) |>
  unnest(cols = c(forcing))

driver <- driver_forcing |>
  left_join(
    fdk_filter |>
      dplyr::select(
        sitename,
        year_start = year_start_gpp,
        year_end = year_end_gpp),
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


# apply good year sequence data quality filter for LE
fdk_filter <- fdk_filter[fdk_filter$drop_le == "FALSE",]


# include only the longest series of consecutive good quality data

driver <- driver[which(driver$sitename %in% fdk_site_info$sitename &
                         driver$sitename %in% fdk_filter$sitename),]

fdk_site_info <- fdk_site_info[which(fdk_site_info$sitename %in% driver$sitename),]

fdk_filter <- fdk_filter[which(fdk_filter$sitename %in% driver$sitename &
                                 fdk_filter$sitename %in% fdk_site_info$sitename),]

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




# good-quality sites from analysis of Stocker et al. (2018) New Phyt.
df_flue <- readr::read_csv("data/fluxnet/flue_stocker18nphyt.csv")

df_flue <- df_flue[df_flue$site %in% fdk_filter$sitename,]

df_flue <- left_join(df_flue |>
                       rename(sitename = site), fdk_filter |>
                       dplyr::select(sitename,year_start_gpp,year_end_gpp),
                     by = "sitename") |>
  filter(between(as_date(date),
                 as_date(paste0(year_start_gpp,"-01-01")),
                 as_date(paste0(year_end_gpp,"-12-31"))
  )) |>
  dplyr::select(-c(year_start_gpp,year_end_gpp)) |>
  rename(site = sitename)

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
  mutate(aet = 1000 * 60 * 60 * 24 * le / (calc_enthalpy_vap(temp) * calc_density_h2o(temp, patm))) |>
  nest(forcing = c("date","temp", "vpd", "ppfd", "netrad", "patm", "snow", "rain", "tmin", "tmax", "vwind", "fapar", "co2", "ccov",
                   "gpp", "gpp_qc", "nee", "nee_qc", "le", "le_qc", "aet"))


# construct list of observations used for evaluation
obs_eval <- create_obs_eval(driver ,fdk_site_info, target = c("gpp","aet"))

# this function will perform all  required analysis in SoFunCalVal package

# Its a big function that create all the plot present in the manuscript

# It runs each model setup (PT, PM, PM-S0) separately, after that I merged the plot using cowplot

so_fun_analysis <- function(out_dir, prefix){

  # weekly aggreagates gpp

  dailiy_gpp <- out_eval$gpp$fluxnet$plot$gg_modobs_xdaily + ggtitle(NULL)
  ggsave(plot = dailiy_gpp, paste0(out_dir,prefix,"daily_gpp.png"),dpi = 300)

  ## Mean seasonal cycle


  climates <-  out_eval$gpp$fluxnet$data$meandoydf_byclim_stats  %>%
    dplyr::filter(climatezone != "- north")

  # getting the rsq and correlation for each climate
  metrics_rsq <- climates |>
    group_by(climatezone) |>
    summarise(rsq = mean(rsq),
              rmse = mean(RMSE),# are repeated elements
              x_pos = 365,
              y_pos = 17)

  metrics_rsq <- metrics_rsq %>%
    mutate(label = paste0("italic(R)^2 == ", round(rsq, 2)))


  metrics_rmse <- climates |>
    group_by(climatezone) |>
    summarise(
      rmse = mean(RMSE),# are repeated elements
      x_pos = 365,
      y_pos = 14) |>
    mutate(rmse = paste0("rmse = ", substr(rmse,1,4)))

  # all climates shown (appendix)

  cliamtes_gpp_all <- out_eval$gpp$fluxnet$data$meandoydf_byclim %>%
    dplyr::filter(climatezone %in%  unique(out_eval$gpp$fluxnet$data$meandoydf_byclim$climatezone)

                  # "AM south", "Bsk north", "Cfa north",
                  # "Cfb north", "Cfb south", "ET north",
                  # "Csb north", "Dfb north", "Dfc north"

    ) %>%
    dplyr::filter(climatezone != "- north") %>%
    pivot_longer(
      c(obs_mean, mod_mean),
      names_to = "source",
      values_to = "gpp"
    ) %>%
    ggplot() +
    geom_ribbon(
      aes(x = doy, ymin = obs_min, ymax = obs_max),
      fill = "black",
      alpha = 0.2
    ) +
    geom_line(aes(x = doy, y = gpp, color = source), size = 0.4) +
    labs(y = expression( paste("Simulated GPP (g C m"^-2, " d"^-1, ")" ) ),
         x = "DOY") +
    facet_wrap( ~climatezone, ncol = 4 ) +
    geom_text(
      data = metrics_rsq |> dplyr::filter(climatezone %in% unique(out_eval$gpp$fluxnet$data$meandoydf_byclim$climatezone)),
      aes(x = x_pos, y = y_pos, label = label),
      parse = TRUE,
      size = 4.5,
      hjust = 1
    ) +
    geom_text(data = metrics_rmse |> dplyr::filter(climatezone %in%  unique(out_eval$gpp$fluxnet$data$meandoydf_byclim$climatezone)),
              aes(x = x_pos, y = y_pos, label = rmse), size = 4, hjust = 1) +
    theme_gray() +
    theme(legend.position = "bottom",
          strip.text = element_text(size = 14)) +
    scale_color_manual(
      name = "Setup: ",
      values = c("red", "black")
    )

  ggsave(plot = cliamtes_gpp_all, paste0(out_dir,prefix,"climates_gpp_all.png"),dpi = 300)


  # GPP drought bias

  df_dday_agg <- eval_droughtresponse(
    df = out_eval$gpp$fluxnet$data$ddf %>%
      rename(
        site = sitename
      ),
    df_flue = readr::read_csv("data/fluxnet/flue_stocker18nphyt.csv"),
    before = 20,
    after = 105,
    leng_threshold = 10,
    nbins = 10,
    do_norm = TRUE
  )

  drought_gpp <- df_dday_agg %>%
    ggplot() +
    geom_hline(
      yintercept = 0,
      color = "black",
      linetype = "dotted"
    ) +
    geom_vline(
      xintercept = 0,
      color = "black",
      linetype = "dotted"
    ) +
    geom_line(
      aes(
        x = dday,
        y = median
      ),
      size = 0.9) +
    geom_ribbon(
      aes(
        x = dday,
        ymin = q33,
        ymax = q66
      ),
      alpha = 0.3) +
    scale_color_manual(
      values = c(
        "BRC" = "black",
        "FULL" = "royalblue"
      ),
      name = "Setup"
    ) +
    scale_fill_manual(
      values = c(
        "BRC" = "black",
        "FULL" = "royalblue"
      ),
      name = "Setup") +
    ylim(-1.2, 2.2) +
    xlim(-20, 105) +
    scale_x_continuous(
      expand = c(0,0)
    ) +
    scale_y_continuous(
      expand = c(0,0)
    ) +
    labs(
      x = "Days after drought onset",
      y = expression( paste( "Bias (g C m"^{-1}, " d"^{-1}, ")"))
    ) +
    theme_classic() +
    theme(
      panel.grid.major.y = element_line(),
      panel.grid.major.x = element_line()
    )

  ggsave(plot = drought_gpp, paste0(out_dir,prefix,"drought_gpp.png"),dpi = 300)


  # same structure as GPP

  daily_aet <- out_eval$aet$fluxnet$plot$gg_modobs_monthly + ggtitle(NULL)

  ggsave(plot = daily_aet, paste0(out_dir,prefix,"daily_aet.png"),dpi = 300)


  ## Mean seasonal cycle

  climates <-  out_eval$aet$fluxnet$data$meandoydf_byclim_stats %>%
    dplyr::filter(climatezone != "- north")

  metrics_rsq <- climates |>
    group_by(climatezone) |>
    summarise(rsq = mean(rsq),
              rmse = mean(RMSE),# are repeated elements
              x_pos = 365,
              y_pos = 7.6)
  # |>
  #   mutate(rsq = purrr::map(rsq, ~ bquote(italic(R)^2 == .(round(.x, 2)))))

  metrics_rsq <- metrics_rsq %>%
    mutate(label = paste0("italic(R)^2 == ", round(rsq, 2)))


  metrics_rmse <- climates |>
    group_by(climatezone) |>
    summarise(
      rmse = mean(RMSE),# are repeated elements
      x_pos = 365,
      y_pos = 6.3) |>
    mutate(rmse = paste0("rmse = ", substr(rmse,1,4)))


  climates_aet_all <- out_eval$aet$fluxnet$data$meandoydf_byclim %>%
    dplyr::filter(climatezone %in%  unique(out_eval$aet$fluxnet$data$meandoydf_byclim$climatezone)

                  # "Aw south", "Bsk north", "Cfa north",
                  # "Cfb north", "Cfb south", "ET north",
                  # "Csb north", "Dfb north", "Dfc north"

    ) %>%
    dplyr::filter(koeppen_code != "-") %>%
    pivot_longer(
      c(obs_mean, mod_mean),
      names_to = "source",
      values_to = "aet"
    )%>%
    ggplot() +
    geom_ribbon(
      aes(x = doy, ymin = obs_min, ymax = obs_max),
      fill = "black",
      alpha = 0.2
    ) +
    geom_line(aes(x = doy, y = aet, color = source), size = 0.4) +
    labs(y =  expression( paste("Simulated AET (mm d"^{-1}, ")" ) ),
         x = "DOY") +
    ylim(0,8.5) +
    facet_wrap( ~climatezone, ncol = 4 ) +
    geom_text(
      data = metrics_rsq |> dplyr::filter(climatezone %in% unique(out_eval$gpp$fluxnet$data$meandoydf_byclim$climatezone)),
      aes(x = x_pos, y = y_pos, label = label),
      parse = TRUE,
      size = 4.5,
      hjust = 1
    ) +
    geom_text(data = metrics_rmse |> dplyr::filter(climatezone %in%  unique(out_eval$aet$fluxnet$data$meandoydf_byclim$climatezone)),
              aes(x = x_pos, y = y_pos, label = rmse), size = 4.5, hjust = 1) +
    theme_gray() +
    theme(legend.position = "none",
          strip.text = element_text(size = 14)) +
    scale_color_manual(
      name = "Setup: ",
      values = c("red", "black")
    )

  ggsave(plot = climates_aet_all, paste0(out_dir,prefix,"climates_aet_all.png"), dpi = 300, width = 7, height = 10)


  climates_aet <- out_eval$aet$fluxnet$data$meandoydf_byclim %>%
    dplyr::filter(climatezone %in%
                  c("Csb north", "Csa north", "ET north",
                  "Dfb north", "Dsa north", "Af north",
                  "Aw south", "Cfb north", "Bsh south")

    ) %>%
    dplyr::filter(koeppen_code != "-") %>%
    pivot_longer(
      c(obs_mean, mod_mean),
      names_to = "source",
      values_to = "aet"
    )%>%
    ggplot() +
    geom_ribbon(
      aes(x = doy, ymin = obs_min, ymax = obs_max),
      fill = "black",
      alpha = 0.2
    ) +
    geom_line(aes(x = doy, y = aet, color = source), size = 0.4) +
    labs(y =  expression( paste("Simulated AET (mm d"^{-1}, ")" ) ),
         x = "DOY") +
    ylim(0,8.5) +
    facet_wrap( ~climatezone ) +
    geom_text(
      data = metrics_rsq |> dplyr::filter(
        climatezone %in% c("Csb north", "Csa north", "ET north",
                           "Dfb north", "Dsa north", "Af north",
                           "Aw south", "Cfb north", "Bsh south")
      ),
      aes(x = x_pos, y = y_pos, label = label),
      parse = TRUE,
      size = 4.5,
      hjust = 1
    ) +
    geom_text(data = metrics_rmse |> dplyr::filter(climatezone %in%
                                                     c("Csb north", "Csa north", "ET north",
                                                       "Dfb north", "Dsa north", "Af north",
                                                       "Aw south", "Cfb north", "Bsh south")
                                                   ),
              aes(x = x_pos, y = y_pos, label = rmse), size = 4.5, hjust = 1) +
    theme_gray() +
    theme(legend.position = "bottom",
          strip.text = element_text(size = 14)) +
    scale_color_manual(
      name = "Setup: ",
      values = c("red", "black")
    )

  ggsave(plot = climates_aet, paste0(out_dir,prefix,"climates_aet.png"),dpi = 300)



  ## Drought response


  df_dday_agg <- eval_droughtresponse(
    df = out_eval$aet$fluxnet$data$ddf %>%
      rename(
        site = sitename
      ),
    df_flue = readr::read_csv("data/fluxnet/flue_stocker18nphyt.csv"),
    before = 20,
    after = 105,
    leng_threshold = 10,
    nbins = 10,
    do_norm = TRUE
  )

  drought_aet <- df_dday_agg %>%
    ggplot() +
    geom_hline(
      yintercept = 0,
      color = "black",
      linetype = "dotted"
    ) +
    geom_vline(
      xintercept = 0,
      color = "black",
      linetype = "dotted"
    ) +
    geom_line(
      aes(
        x = dday,
        y = median
      ),
      size = 0.9) +
    geom_ribbon(
      aes(
        x = dday,
        ymin = q33,
        ymax = q66
      ),
      alpha = 0.3) +
    scale_color_manual(
      values = c(
        "BRC" = "black",
        "FULL" = "royalblue"
      ),
      name = "Setup"
    ) +
    scale_fill_manual(
      values = c(
        "BRC" = "black",
        "FULL" = "royalblue"
      ),
      name = "Setup") +
    ylim(-1.2, 2.2) +
    xlim(-20, 105) +
    scale_x_continuous(
      expand = c(0,0)
    ) +
    scale_y_continuous(
      expand = c(0,0)
    ) +
    labs(
      x = "Days after drought onset",
      y = expression( paste( "Bias AET (mm d"^{-1}, ")"))
    ) +
    theme_classic() +
    theme(
      panel.grid.major.y = element_line(),
      panel.grid.major.x = element_line()
    )

  ggsave(plot = drought_aet, paste0(out_dir,prefix,"drought_aet.png"),dpi = 300)

  all_plot <- list(dailiy_gpp,cliamtes_gpp_all,drought_gpp,
                   daily_aet, climates_aet_all, climates_aet, drought_aet)

  return(all_plot)

}


# PM new WHC
#--------------

# Model run

par_calib <-  read_rds("data/fluxnet/global_calib_PM_S0.rds")


params_modl <- list(
  kphio              =  par_calib$par[["kphio"]],
  kphio_par_a        =  par_calib$par[["kphio_par_a"]],
  kphio_par_b        =  par_calib$par[["kphio_par_b"]],
  rd_to_vcmax        =  0.014,
  soilm_thetastar    =  par_calib$par[["soilm_thetastar"]],
  beta_unitcostratio =  par_calib$par[["beta_unitcostratio"]],
  tau_acclim         =  30,
  kc_jmax            =  0.41,
  gw_calib           =  par_calib$par[["gw_calib"]]
)

driver <- driver |> unnest(params_siml) |>
  mutate(use_gs = TRUE,
         use_phydro = FALSE,
         use_pml= TRUE) |>
  group_by(sitename) |>
  nest(params_siml = c(spinup, spinupyears, recycle, outdt, ltre,  ltne,  ltrd,  ltnd,  lgr3,  lgn3,  lgr4 ,
                       use_gs, use_phydro, use_pml))


output <- rsofun::runread_pmodel_f(
  driver,
  par = params_modl
)


# remove sites where it didn't work
output <- output |>
  unnest(data) |>
  mutate(le = le / 86400) |>
  nest(data =c("date", "year_dec", "fapar", "gpp", "aet", "le", "pet", "vcmax", "jmax", "vcmax25","jmax25", "gs_accl", "wscal", "chi", "iwue", "rd", "tsoil", "netrad", "wcont", "snow", "cond", "le_canopy", "le_soil",  "dpsi", "psi_leaf"))

# Run evaluation

settings_eval <- list(
  benchmark = list( gpp = c("fluxnet"), aet = c("fluxnet") ),
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

PM_S0 <- so_fun_analysis("./fig/",prefix = "PM_S0_")

clim_aet <-  PM_S0[[5]]

ggsave(plot = clim_aet, paste0("./fig/","PM_S0_climates_aet_all.pdf"), device = "pdf", dpi = 300, width = 7, height = 10)

##-------
# PM old WHC
##------

nc_file <- nc_open("data/fluxnet/whc_2m.nc")

whc = ncvar_get(nc_file, "whc_2m")
lons = ncvar_get(nc_file, "lon")
lats = ncvar_get(nc_file, "lat")

n <- 1 # parameter to select size of slice to average, it will takes also the neighboring cells, so ti avoids Na

old_whc <- lapply(driver$sitename, function(x){
  tmp <- driver[driver$sitename == x,][[2]][[1]]
  lonid <- which(lons > tmp$lon)[1]
  latid <- which(lats > tmp$lat)[1]
  whc_grid <- whc[(lonid-n):(lonid+n), (latid-n):(latid+n)]
  whc_site <- mean(as.numeric(whc_grid, na.rm=T))
  return(whc_site)
})

old_whc = unlist(old_whc)

old_whc <- ifelse(is.na(old_whc),200,old_whc)

for(i in 1:dim(driver)[1]){
  driver$site_info[i][[1]][4] <- old_whc[i]
}



par_calib <-  read_rds("data/fluxnet/global_calib_PM.rds")


params_modl <- list(
  kphio              =  par_calib$par[["kphio"]],
  kphio_par_a        =  par_calib$par[["kphio_par_a"]],
  kphio_par_b        =  par_calib$par[["kphio_par_b"]],
  rd_to_vcmax        =  0.014,
  soilm_thetastar    =  par_calib$par[["soilm_thetastar"]],
  beta_unitcostratio =  par_calib$par[["beta_unitcostratio"]],
  tau_acclim         =  30,
  kc_jmax            =  0.41,
  gw_calib           =  par_calib$par[["gw_calib"]]
)

driver <- driver |> unnest(params_siml) |>
  mutate(use_gs = TRUE,
         use_phydro = FALSE,
         use_pml= TRUE) |>
  group_by(sitename) |>
  nest(params_siml = c(spinup, spinupyears, recycle, outdt, ltre,  ltne,  ltrd,  ltnd,  lgr3,  lgn3,  lgr4 ,
                       use_gs, use_phydro, use_pml))


output <- rsofun::runread_pmodel_f(
  driver,
  par = params_modl
)


# remove sites where it didn't work
output <- output |>
  unnest(data) |>
  mutate(le = le / 86400) |>
  nest(data =c("date", "year_dec", "fapar", "gpp", "aet", "le", "pet", "vcmax", "jmax", "vcmax25","jmax25", "gs_accl", "wscal", "chi", "iwue", "rd", "tsoil", "netrad", "wcont", "snow", "cond", "le_canopy", "le_soil",  "dpsi", "psi_leaf"))

# Run evaluation

settings_eval <- list(
  benchmark = list( gpp = c("fluxnet"), aet = c("fluxnet") ),
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

PM <- so_fun_analysis("./fig/",prefix = "PM_")

clim_aet <-  PM[[5]]

ggsave(plot = clim_aet, paste0("./fig/","PM_climates_aet_all.pdf"), device = "pdf", dpi = 300, width = 7, height = 10)


##-------
# PT
##------

par_calib <-  read_rds("data/fluxnet/global_calib_PT.rds")


params_modl <- list(
  kphio              =  par_calib$par[["kphio"]],
  kphio_par_a        =  par_calib$par[["kphio_par_a"]],
  kphio_par_b        =  par_calib$par[["kphio_par_b"]],
  rd_to_vcmax        =  0.014,
  soilm_thetastar    =  par_calib$par[["soilm_thetastar"]],
  beta_unitcostratio =  par_calib$par[["beta_unitcostratio"]],
  tau_acclim         =  30,
  kc_jmax            =  0.41,
  gw_calib           =  2 # doesn't change anything
)

driver <- driver |> unnest(params_siml) |>
  mutate(use_gs = FALSE,
         use_phydro = FALSE,
         use_pml= FALSE) |>
  group_by(sitename) |>
  nest(params_siml = c(spinup, spinupyears, recycle, outdt, ltre,  ltne,  ltrd,  ltnd,  lgr3,  lgn3,  lgr4 ,
                       use_gs, use_phydro, use_pml))


output <- rsofun::runread_pmodel_f(
  driver,
  par = params_modl
)


# remove sites where it didn't work
output <- output |>
  unnest(data) |>
  mutate(le = le / 86400) |>
  nest(data =c("date", "year_dec", "fapar", "gpp", "aet", "le", "pet", "vcmax", "jmax", "vcmax25","jmax25", "gs_accl", "wscal", "chi", "iwue", "rd", "tsoil", "netrad", "wcont", "snow", "cond", "le_canopy", "le_soil",  "dpsi", "psi_leaf"))

# Run evaluation

settings_eval <- list(
  benchmark = list( gpp = c("fluxnet"), aet = c("fluxnet") ),
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

PT <- so_fun_analysis("./fig/",prefix = "PT_")

clim_aet <-  PT[[5]]

ggsave(plot = clim_aet, paste0("./fig/","PT_climates_aet_all.pdf"), device = "pdf", dpi = 300, width = 7, height = 10)

# start to merge the figures
####--------

daily_gpp <- plot_grid(PT[[1]],PM[[1]],PM_S0[[1]],ncol = 3, labels = letters[1:3])

ggsave(plot = daily_gpp, paste0("./fig/","daily_gpp_merged.pdf"),device = "pdf", dpi = 300, width = 21 / scaling_factor, height = 7 / scaling_factor) # reduce size
ggsave(plot = daily_gpp, paste0("./fig/","daily_gpp_merged.png"), dpi = 300, width = 21 / scaling_factor, height = 7 / scaling_factor) # reduce size


drought_gpp <- plot_grid(PT[[3]] + ylim(-1.5,1.5),PM[[3]]+ ylim(-1.5,1.5),PM_S0[[3]]+ ylim(-1.5,1.5),ncol = 3, labels = letters[1:3])

ggsave(plot = drought_gpp, paste0("./fig/","drought_gpp_merged.pdf"),device = "pdf", dpi = 300, width = 21 / scaling_factor, height = 7 / scaling_factor)


daily_aet <- plot_grid(PT[[4]],PM[[4]],PM_S0[[4]],ncol = 3, labels = letters[1:3])

ggsave(plot = daily_aet, paste0("./fig/","daily_aet_merged.pdf"),device = "pdf", dpi = 300, width = 21 / scaling_factor, height = 7 / scaling_factor)  # reduce size
ggsave(plot = daily_aet, paste0("./fig/","daily_aet_merged.png"), dpi = 300, width = 21 / scaling_factor, height = 7 / scaling_factor)  # reduce size


legend <- get_legend(PT[[6]])


climates_aet <- plot_grid(PT[[6]] + xlab(NULL) + theme(legend.position = "none"),PM[[6]] + xlab(NULL)+ theme(legend.position = "none"),
                          PM_S0[[6]]+  theme(legend.position = "none",legend),ncol = 1, labels = letters[1:3])

ggsave(plot = climates_aet, paste0("./fig/","climates_aet_merged.pdf"),device = "pdf", dpi = 300, width = 7 / scaling_factor, height = 21 / scaling_factor)


drought_aet <- plot_grid(PT[[7]]+ ylim(-1.5,0.8) ,PM[[7]] + ylim(-1.5,0.8), PM_S0[[7]]+ ylim(-1.5,0.8) ,ncol = 3, labels = letters[1:3])

ggsave(plot = drought_aet, paste0("./fig/","drought_aet_merged.pdf"),device = "pdf", dpi = 300, width = 21 / scaling_factor, height = 7 / scaling_factor)


