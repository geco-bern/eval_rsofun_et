# rsofun calibration at FLUXNET sites ------------------------------------------
## library and data loading ----------------------------------------------------
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)

## Load driver -----------------------------------------------------------------
# File created by 00_prepare_forcing_fluxnet.R
driver <- read_rds(here("data/driver.rds"))

## LE and GPP calibration ------------------------------------------------------
evaluation <- driver |>
  unnest(forcing) |>
  select(sitename, date, gpp, gpp_qc, le, le_qc) |>
  group_by(sitename) |>
  nest(data = c(date, gpp, gpp_qc, le, le_qc))

params_fix <- list(
  rd_to_vcmax = 0.014,
  beta_unitcostratio = 146,
  kc_jmax = 0.41,
  tau_acclim = 30
)

# Define calibration settings
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
      burnin = 100000,
      iterations = 15000
    )
  )
)

# Run the calibration for GPP data
calib_output <- rsofun::calib_sofun(
  drivers = driver,
  obs = evaluation,
  settings = settings,
  # extra arguments for the cost function
  par_fixed = params_fix,
  targets = c("gpp", "le")
)

write_rds(calib_output, "../my_stuff/global_calib_PM_new_whc_no_beta.rds")
