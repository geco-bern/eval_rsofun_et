# rsofun calibration at FLUXNET sites ------------------------------------------
## Library and data loading ----------------------------------------------------
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(purrr)
# library(remotes)
library(terra)
library(rsofun)
library(here)

# remotes::install_github(
#   "geco-bern/rsofun",
#   ref = "phydro"
# )

source(here("R/modified_cost_likelihood_pmodel.R"))

## Common objects --------------------------------------------------------------
# common for all setups

### Load driver ----------------------------------------------------------------
# File created by 00_prepare_forcing_fluxnet.R
driver <- read_rds(here("data/driver.rds"))

### Parameters fixed for all setups --------------------------------------------
params_fix <- list(
  rd_to_vcmax = 0.014,
  beta_unitcostratio = 146,
  kc_jmax = 0.41,
  tau_acclim = 30
)

### Observational target data --------------------------------------------------
evaluation <- driver |>
  mutate(data = map(forcing, ~select(., date, gpp, gpp_qc, le, le_qc))) |>
  select(-params_siml, -site_info, -forcing)

### Calibration settings  ------------------------------------------------------
settings <- list(
  method = "BayesianTools",
  par = list(
    kphio = list(lower = 0.0001, upper = 0.125, init = 0.05),
    kphio_par_a = list(lower = -0.001, upper = -0.00001, init = -0.0025),
    kphio_par_b = list(lower = 10, upper = 50, init = 20),
    soilm_thetastar = list(lower = 0.5, upper = 400, init = 100),
    gw_calib = list(lower = 0.05, upper = 4, init = 2),
    err_gpp = list(lower = 0.01, upper = 4, init = 2),
    err_le = list(lower = 0.01, upper = 50, init = 30)
  ),
  metric = modified_cost_likelihood_pmodel,
  control = list(
    sampler = "DEzs",
    settings = list(
      nrChains = 3,
      burnin = 10000,
      iterations = 15000
    )
  )
)

## Setup PM S0 -----------------------------------------------------------------
driver_pm_s0 <- driver |>
  mutate(params_siml = map(
    params_siml,
    ~mutate(
      .,
      use_gs = TRUE,
      use_phydro = FALSE,
      use_pml = TRUE,
      is_global = FALSE
      )))

write_rds(driver_pm_s0, file = here("data/driver_pm_s0.rds"))

# Run the calibration for GPP data
calib_output <- calib_sofun(
  drivers = driver_pm_s0,
  obs = evaluation,
  settings = settings,
  # extra arguments for the cost function
  par_fixed = params_fix,
  targets = c("gpp", "le")
)

write_rds(calib_output, here("data/calib_output_pm_s0.rds"))
# write_rds(calib_output, here("data/global_calib_PM_new_whc_no_beta.rds"))

## Setup PM --------------------------------------------------------------------
driver_pm <- driver |>
  mutate(params_siml = map(
    params_siml,
    ~mutate(
      .,
      use_gs = TRUE,
      use_phydro = FALSE,
      use_pml = TRUE,
      is_global = FALSE
    )))

# Get 2 m-WHC for file
rasta <- rast(here("data/fluxnet/whc_2m.nc"))

df <- driver_pm |>
  select(sitename,  site_info) |>
  unnest(site_info)

pts <- vect(
  df,
  geom = c("lon", "lat"),
  crs = "OGC:CRS84"
)

# overwrite whc with extracted value
df <- df |>
  mutate(
    whc = extract(rasta, pts)$whc_2m
  ) |>
  group_by(sitename) |>
  nest() |>
  rename(site_info = data)

# add new site_info to driver object
driver_pm <- driver_pm |>
  select(-site_info) |>
  left_join(
    df,
    by = join_by(sitename)
  ) |>
  select(sitename, params_siml, site_info, forcing)

write_rds(driver_pm, file = here("data/driver_pm.rds"))

# Run the calibration for GPP data
calib_output <- calib_sofun(
  drivers = driver_pm,
  obs = evaluation,
  settings = settings,
  # extra arguments for the cost function
  par_fixed = params_fix,
  targets = c("gpp", "le")
)

write_rds(calib_output, here("data/calib_output_pm.rds"))
# write_rds(calib_output, here("data/global_calib_PM_old_WHC_no_beta.rds"))

## Setup PT --------------------------------------------------------------------
driver_pm <- read_rds(here("data/driver_pm.rds"))

driver_pt <- driver_pm |>
  mutate(params_siml = map(
    params_siml,
    ~mutate(
      .,
      use_gs = FALSE,
      use_phydro = FALSE,
      use_pml= FALSE,
      is_global = FALSE
    )))

write_rds(driver_pt, file = here("data/driver_pt.rds"))

# Run the calibration for GPP data
calib_output <- calib_sofun(
  drivers = driver_pt,
  obs = evaluation,
  settings = settings,
  # extra arguments for the cost function
  par_fixed = params_fix,
  targets = c("gpp", "le")
)

write_rds(calib_output, here("data/calib_output_pt.rds"))
# write_rds(calib_output, "../my_stuff/global_calib_PT_no_beta.rds")
