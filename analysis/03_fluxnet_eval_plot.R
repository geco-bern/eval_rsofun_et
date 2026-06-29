# !!! Run only once if you don't have the correct branch
# library(devtools)
# devtools::install_github("geco-bern/rsofun", ref = "phydro")

## Load packages, functions, and data ------------------------------------------
library(rsofun)
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(lubridate)
library(knitr)
library(ggthemes)
library(purrr)
library(here)

# Load functions
source(here::here("R/eval_sofun.R"))
source(here::here("R/get_stats.R"))
source(here::here("R/analyse_modobs2.R"))
source(here::here("R/align_events.R"))
source(here::here("R/eval_droughtresponse.R"))
source(here::here("R/create_obs_eval.R"))

# site selection: good-quality sites from analysis of Stocker et al. (2018) New Phyt.
df_flue <- readr::read_csv(here("data/fluxnet/flue_stocker18nphyt.csv"))

# common stuff
params_fix <- list(
  rd_to_vcmax = 0.014,
  beta_unitcostratio = 146,
  kc_jmax = 0.41,
  tau_acclim = 30
)

driver <- read_rds(here("data/driver.rds"))

# remove sites with missing observed GPP or LE in driver data
driver <- driver |>
  mutate(
    nmissing_gpp = map_int(forcing, ~ sum(is.na(.$gpp))),
    nmissing_le = map_int(forcing, ~ sum(is.na(.$le)))
  ) |>
  filter(nmissing_gpp == 0 & nmissing_le == 0) |>
  select(-nmissing_gpp, -nmissing_le)

# subset to testing sites
sites <- read_csv(here("data/sites.csv")) |>
  filter(train == FALSE)

driver <- driver |>
  filter(sitename %in% sites$sitename)

fdk_site_info <- read_csv(here("data/fluxnet/fdk_site_info.csv"),
                          show_col_types = FALSE) |>
  filter(sitename %in% driver$sitename)

# obs_eval <- create_obs_eval(driver, fdk_site_info, target = c("gpp", "le"))
obs_eval <- read_rds(here::here("data/obs_eval_fluxnet.rds"))

# retain only observational data for sites in driver
obs_eval$ddf <- obs_eval$ddf |>
  filter(sitename %in% driver$sitename)
obs_eval$mdf <- obs_eval$mdf |>
  filter(sitename %in% driver$sitename)
obs_eval$adf <- obs_eval$adf |>
  filter(sitename %in% driver$sitename)
obs_eval$xdf <- obs_eval$xdf |>
  filter(sitename %in% driver$sitename)

## Run model and evaluations for all setups ----------------------------------------------------
### PM-S0 ---------------
#### rsofun driver object ---------------
driver_pm_s0 <- driver |>
  mutate(params_siml = map(
    params_siml,
    ~ mutate(
      .,
      use_gs = TRUE,
      use_phydro = FALSE,
      use_pml = TRUE,
      is_global = FALSE
    )
  ))

#### calibrated parameters ---------------
par_calib <- read_rds(here("data/calib_output_pm_s0.rds"))

#### construct all parameters ---------------
params_modl <- c(par_calib$par[c("kphio", "kphio_par_a", "kphio_par_b", "soilm_thetastar", "gw_calib")], params_fix)

#### run model ---------------
output_pm_s0 <- runread_pmodel_f(
  driver_pm_s0,
  par = params_modl
)

#### run evaluation ---------------
settings_eval <- list(
  benchmark = list(gpp = c("fluxnet"), le = c("fluxnet")),
  # sitenames = evalsites,
  agg = 8
)

out_eval_pm_s0 <- eval_sofun(
  output_pm_s0,
  settings_eval,
  obs_eval = obs_eval,
  overwrite = TRUE,
  light = FALSE
)

#### run drought response evaluation --------
df_droughtresponse_pm_s0 <- eval_droughtresponse(
  df = out_eval_pm_s0$le$fluxnet$data$ddf %>%
    rename(site = sitename),
  df_flue = readr::read_csv(here("data/fluxnet/flue_stocker18nphyt.csv")),
  before = 20,
  after = 105,
  leng_threshold = 10,
  nbins = 10,
  do_norm = TRUE
)

### PM ---------------
#### rsofun driver object ---------------
driver_pm <- read_rds(here("data/driver_pm.rds")) # created in 01_calib_fluxnet.R

#### calibrated parameters ---------------
par_calib <- read_rds(here("data/calib_output_pm.rds"))

#### construct all parameters ---------------
params_modl <- c(par_calib$par[c("kphio", "kphio_par_a", "kphio_par_b", "soilm_thetastar", "gw_calib")], params_fix)

#### run model ---------------
output_pm <- runread_pmodel_f(
  driver_pm,
  par = params_modl
)

#### run evaluation ---------------
settings_eval <- list(
  benchmark = list(gpp = c("fluxnet"), le = c("fluxnet")),
  # sitenames = evalsites,
  agg = 8
)

out_eval_pm <- eval_sofun(
  output_pm,
  settings_eval,
  obs_eval = obs_eval,
  overwrite = TRUE,
  light = FALSE
)

#### run drought response evaluation --------
df_droughtresponse_pm <- eval_droughtresponse(
  df = out_eval_pm$le$fluxnet$data$ddf %>%
    rename(site = sitename),
  df_flue = readr::read_csv(here("data/fluxnet/flue_stocker18nphyt.csv")),
  before = 20,
  after = 105,
  leng_threshold = 10,
  nbins = 10,
  do_norm = TRUE
)

### PT ---------------
#### rsofun driver object ---------------
driver_pt <- read_rds(here("data/driver_pt.rds")) # created in 01_calib_fluxnet.R

#### calibrated parameters ---------------
par_calib <- read_rds(here("data/calib_output_pt.rds"))

#### construct all parameters ---------------
params_modl <- c(par_calib$par[c("kphio", "kphio_par_a", "kphio_par_b", "soilm_thetastar", "gw_calib")], params_fix)

#### run model ---------------
output_pt <- runread_pmodel_f(
  driver_pt,
  par = params_modl
)

#### run evaluation ---------------
settings_eval <- list(
  benchmark = list(gpp = c("fluxnet"), le = c("fluxnet")),
  # sitenames = evalsites,
  agg = 8
)

out_eval_pt <- eval_sofun(
  output_pt,
  settings_eval,
  obs_eval = obs_eval,
  overwrite = TRUE,
  light = FALSE
)

#### run drought response evaluation --------
df_droughtresponse_pt <- eval_droughtresponse(
  df = out_eval_pt$le$fluxnet$data$ddf %>%
    rename(site = sitename),
  df_flue = readr::read_csv(here("data/fluxnet/flue_stocker18nphyt.csv")),
  before = 20,
  after = 105,
  leng_threshold = 10,
  nbins = 10,
  do_norm = TRUE
)

## Create plots --------
### Mod vs Obs, x-daily LE ------------
gg1 <- out_eval_pm_s0$le$fluxnet$plot$gg_modobs_monthly +
  labs(title = NULL)

gg2 <- out_eval_pm$le$fluxnet$plot$gg_modobs_monthly +
  labs(title = NULL)

gg3 <- out_eval_pt$le$fluxnet$plot$gg_modobs_monthly +
  labs(title = NULL)

cowplot::plot_grid(gg1, gg2, gg3, labels = letters[1:3], ncol = 3)

### Mean seasonal cycle by climate zone -----------
# number of sites per climate zone
out_eval_pm_s0$le$fluxnet$data$meandoydf_byclim |>
  select(climatezone, nsites) |>
  distinct() |>
  arrange(desc(nsites))

#### LE ----------
out_eval_pm_s0$le$fluxnet$data$meandoydf_byclim %>%
  filter(nsites >= 5) |>
  # dplyr::filter(climatezone %in%
  #   c(
  #     "Csb north", "Csa north", "ET north",
  #     "Dfb north", "Dsa north", "Af north",
  #     "Aw south", "Cfb north", "Bsh south"
  #   )) %>%
  rename_with(~ sub("^mod_", "mod_pm_s0_", .x), starts_with("mod_")) |>
  left_join(
    out_eval_pm$le$fluxnet$data$meandoydf_byclim %>%
      filter(nsites >= 5) |>
      rename_with(~ sub("^mod_", "mod_pm_", .x), starts_with("mod_")) |>
      select(-starts_with("obs")),
    by = join_by(climatezone, doy)
  ) |>
  left_join(
    out_eval_pt$le$fluxnet$data$meandoydf_byclim %>%
      filter(nsites >= 5) |>
      rename_with(~ sub("^mod_", "mod_pt_", .x), starts_with("mod_")) |>
      select(-starts_with("obs")),
    by = join_by(climatezone, doy)
  ) |>
  pivot_longer(
    cols = c(obs_mean, mod_pm_s0_mean, mod_pm_mean, mod_pt_mean),
    names_to = "source",
    values_to = "le"
  ) %>%
  mutate(source = factor(source, levels = c("obs_mean", "mod_pt_mean", "mod_pm_mean", "mod_pm_s0_mean"))) |>
  ggplot() +
  geom_ribbon(
    aes(x = doy, ymin = obs_min, ymax = obs_max),
    fill = "grey70"
  ) +
  geom_line(aes(x = doy, y = le, color = source)) +
  labs(
    y = expression(paste("LE  (W m"^-2, ")")),
    x = "DOY"
  ) +
  facet_wrap(~climatezone, ncol = 4) +
  theme_bw() +
  theme(
    legend.position = "bottom"
    # strip.text = element_text(size = 14)
  ) +
  scale_color_manual(
    name = NULL,
    breaks = c("PM" = "mod_pm_mean", "PM-S0" = "mod_pm_s0_mean", "PT" = "mod_pt_mean", "Observed" = "obs_mean"),
    values = c(unname(see::okabeito_colors()[c(2, 5, 7)]), "black")
  )

#### GPP ---------
out_eval_pm_s0$gpp$fluxnet$data$meandoydf_byclim %>%
  filter(nsites >= 5) |>
  # dplyr::filter(climatezone %in%
  #   c(
  #     "Csb north", "Csa north", "ET north",
  #     "Dfb north", "Dsa north", "Af north",
  #     "Aw south", "Cfb north", "Bsh south"
  #   )) %>%
  rename_with(~ sub("^mod_", "mod_pm_s0_", .x), starts_with("mod_")) |>
  left_join(
    out_eval_pm$gpp$fluxnet$data$meandoydf_byclim %>%
      filter(nsites >= 5) |>
      rename_with(~ sub("^mod_", "mod_pm_", .x), starts_with("mod_")) |>
      select(-starts_with("obs")),
    by = join_by(climatezone, doy)
  ) |>
  left_join(
    out_eval_pt$gpp$fluxnet$data$meandoydf_byclim %>%
      filter(nsites >= 5) |>
      rename_with(~ sub("^mod_", "mod_pt_", .x), starts_with("mod_")) |>
      select(-starts_with("obs")),
    by = join_by(climatezone, doy)
  ) |>
  pivot_longer(
    cols = c(obs_mean, mod_pm_s0_mean, mod_pm_mean, mod_pt_mean),
    names_to = "source",
    values_to = "gpp"
  ) %>%
  mutate(source = factor(source, levels = c("obs_mean", "mod_pt_mean", "mod_pm_mean", "mod_pm_s0_mean"))) |>
  ggplot() +
  geom_ribbon(
    aes(x = doy, ymin = obs_min, ymax = obs_max),
    fill = "grey70"
  ) +
  geom_line(aes(x = doy, y = gpp, color = source)) +
  labs(
    y = expression(paste("GPP  (gC m"^-2, "d"^-1, ")")),
    x = "DOY"
  ) +
  facet_wrap(~climatezone, ncol = 4) +
  theme_bw() +
  theme(
    legend.position = "bottom"
    # strip.text = element_text(size = 14)
  ) +
  scale_color_manual(
    name = NULL,
    breaks = c("PM" = "mod_pm_mean", "PM-S0" = "mod_pm_s0_mean", "PT" = "mod_pt_mean", "Observed" = "obs_mean"),
    values = c(unname(see::okabeito_colors()[c(2, 5, 7)]), "black")
  )

### Drought response ------
gg1 <- df_droughtresponse_pm_s0 %>%
  ggplot(aes(x = dday)) +
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
    aes(y = median),
    size = 0.9
  ) +
  geom_ribbon(
    aes(ymin = q33, ymax = q66),
    alpha = 0.3
  ) +
  scale_x_continuous(
    expand = c(0, 0),
    limits = c(-20, 105)
  ) +
  scale_y_continuous(
    expand = c(0, 0),
    limits = c(-40, 20)
  ) +
  labs(
    title = "PM-S0",
    x = "Days after drought onset",
    y = expression(paste("Bias (W m"^{-2}, ")"))
  ) +
  theme_classic()

gg2 <- df_droughtresponse_pm %>%
  ggplot(aes(x = dday)) +
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
    aes(y = median),
    size = 0.9
  ) +
  geom_ribbon(
    aes(ymin = q33, ymax = q66),
    alpha = 0.3
  ) +
  scale_x_continuous(
    expand = c(0, 0),
    limits = c(-20, 105)
  ) +
  scale_y_continuous(
    expand = c(0, 0),
    limits = c(-40, 20)
  ) +
  labs(
    title = "PM",
    x = "Days after drought onset",
    y = expression(paste("Bias (W m"^{-2}, ")"))
  ) +
  theme_classic()

gg3 <- df_droughtresponse_pt %>%
  ggplot(aes(x = dday)) +
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
    aes(y = median),
    size = 0.9
  ) +
  geom_ribbon(
    aes(ymin = q33, ymax = q66),
    alpha = 0.3
  ) +
  scale_x_continuous(
    expand = c(0, 0),
    limits = c(-20, 105)
  ) +
  scale_y_continuous(
    expand = c(0, 0),
    limits = c(-40, 20)
  ) +
  labs(
    title = "PT",
    x = "Days after drought onset",
    y = expression(paste("Bias (W m"^{-2}, ")"))
  ) +
  theme_classic()

cowplot::plot_grid(
  gg1, gg2, gg3,
  labels = letters[1:3],
  ncol = 3
)
