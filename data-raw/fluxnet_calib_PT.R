
# library and data loading
library(rsofun)
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(ncdf4)


driver <- read_rds("/data_2/FluxDataKit/v3.4/zenodo_upload/rsofun_driver_data_v3.4.2.rds") 

fdk_site_info <- read_csv("/data_2/FluxDataKit/v3.4/zenodo_upload/fdk_site_info.csv")

# data quality filter info
fdk_filter <-  read_csv("/data_2/FluxDataKit/v3.4/zenodo_upload/fdk_site_fullyearsequence.csv")

# exclude croplands and wetlands from calibration/evaluation
fdk_site_info <- fdk_site_info[fdk_site_info$igbp_land_use != "CRO" &
                                 fdk_site_info$igbp_land_use != "WET",]


# apply good year sequence data quality filter for GPP
fdk_filter <- fdk_filter[fdk_filter$drop_gpp == "FALSE",]

driver <- driver[which(driver$sitename %in% fdk_site_info$sitename &
                         driver$sitename %in% fdk_filter$sitename),]

fdk_site_info <- fdk_site_info[which(fdk_site_info$sitename %in% driver$sitename),]

fdk_filter <- fdk_filter[which(fdk_filter$sitename %in% driver$sitename &
                                 fdk_filter$sitename %in% fdk_site_info$sitename),]

driver_forcing <- driver |> 
  select(sitename, forcing) |> 
  unnest(cols = c(forcing))

driver <- driver_forcing |>
  left_join(
    fdk_filter |> 
      select(
        sitename, 
        year_start = year_start_gpp, 
        year_end = year_end_gpp),
    by = join_by(sitename)
  ) |> 
  mutate(year = year(date)) |> 
  filter(year >= year_start & year <= year_end) |> 
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


# apply good year sequence data quality filter for LE
fdk_filter <- fdk_filter[fdk_filter$drop_le == "FALSE",]

driver <- driver[which(driver$sitename %in% fdk_site_info$sitename &
                         driver$sitename %in% fdk_filter$sitename),]

fdk_site_info <- fdk_site_info[which(fdk_site_info$sitename %in% driver$sitename),]

fdk_filter <- fdk_filter[which(fdk_filter$sitename %in% driver$sitename &
                                 fdk_filter$sitename %in% fdk_site_info$sitename),]

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

good_sites <- readLines("../my_stuff/good_sites.txt")

driver <- driver[driver$sitename %in% good_sites,]

fdk_site_info <- fdk_site_info[which(fdk_site_info$sitename %in% driver$sitename),]

driver <- driver |> unnest(params_siml) |>
  mutate(use_gs = FALSE,
         use_phydro = FALSE,
         use_pml= FALSE,
         is_global = F) |>
  group_by(sitename) |>
  nest(params_siml = c(spinup, spinupyears, recycle, outdt, ltre,  ltne,  ltrd,  ltnd,  lgr3,  lgn3,  lgr4 ,
                       use_gs, use_phydro, use_pml, is_global))



nc_file <-  nc_open("~/data_scratch/my_stuff/whc_2m.nc")

whc = ncvar_get(nc_file, "whc_2m")
lons = ncvar_get(nc_file, "lon")
lats = ncvar_get(nc_file, "lat")

geo <- driver |>
  unnest(site_info) |>
  select(lon  , lat)

geo$sitename <- driver$sitename

n <- 1 # parameter to select size of slice to average

old_whc <- lapply(geo$sitename, function(x){
  tmp <- geo[geo$sitename == x,]
  lonid <- which(lons > tmp$lon)[1]
  latid <- which(lats > tmp$lat)[1]
  whc_grid <- whc[(lonid-n):(lonid+n), (latid-n):(latid+n)]
  whc_site <- mean(as.numeric(whc_grid, na.rm=T))
  return(whc_site)
})

old_whc = unlist(old_whc)

for(i in 1:dim(driver)[1]){
  driver$site_info[i][[1]][4] <- old_whc[i]
}




modified_cost_likelihood_pmodel <- function(
    par,   # model parameters & error terms for each target
    obs,
    drivers,
    targets,
    par_fixed = NULL,   # non-calibrated model parameters
    parallel = FALSE,
    ncores = 2
){
  # NOTE(fabian): These different cost functions share a LOT of code in common. Consider consolidation for maintainability?
  
  # predefine variables for CRAN check compliance
  sitename <- data <- gpp_mod <- NULL
  
  if (!("use_phydro" %in% colnames(drivers$params_siml[[1]]))){
    warning("Parameter use_phydro not set. Assuming FALSE")
    using_phydro = FALSE
  } else {
    using_phydro = drivers$params_siml[[1]]$use_phydro
  }
  
  ## define required parameter set based on model parameters
  if (!using_phydro){
    required_param_names <- rsofun:::required_param_names$p_model
  } else {
    required_param_names <- rsofun:::required_param_names$phydro_model
  }
  
  ## split calibrated parameters into model and error parameters
  par_calibrated_model      <- par[!startsWith(names(par), "err_")] # consider only model parameters for the check
  # par_calibrated_errormodel <- par[   names(par) %in% c("err_gpp", "err_vcmax25") ]
  # par_fixed
  
  ## check parameters
  if (!identical(sort(c(names(par_calibrated_model), names(par_fixed))), required_param_names)){
    stop(sprintf(paste0("Error: Input calibratable and fixed parameters do not ",
                        "match required model parameters:",
                        "\n         par:       c(%s)",
                        "\n         par_fixed: c(%s)",
                        "\n         required:  c(%s)"),
                 paste0(sort(names(par_calibrated_model)), collapse = ", "),
                 paste0(sort(names(par_fixed)), collapse = ", "),
                 paste0(sort(required_param_names), collapse = ", ")))
  }
  
  # Combine fixed and estimated params to result in all the params required to run the model
  # This basically uses all params except those of the error model of the observations
  params_modl <- c(par, par_fixed)[required_param_names]
  
  ## run the model
  df <- runread_pmodel_f(
    drivers,
    par = params_modl,
    makecheck = TRUE,
    parallel = parallel,
    ncores = ncores
  )
  
  ## clean model output and unnest
  df <- df |>
    dplyr::rowwise() |>
    dplyr::reframe(
      cbind(sitename, data[, c('date', unique(c('gpp', targets)))]) |>
        stats::setNames(c('sitename', 'date', paste0(unique(c('gpp', targets)), '_mod')))
    ) # gpp is used to get average trait prediction
  
  # separate validation data into fluxes and traits, site by site
  is_flux <- apply(obs, 1, function(x){ 'date' %in% colnames(x$data)})
  
  if(sum(is_flux) > 0){
    flux_sites <- obs$sitename[is_flux]
    
    # Unnest flux observations for our targets
    obs_flux <- obs[is_flux, ] |>
      dplyr::select(sitename, data) |>
      tidyr::unnest(data) |>
      dplyr::select(any_of(c('sitename', 'date', targets,paste0(targets,"_qc"))))
    
    if(ncol(obs_flux) < 3){
      warning("Dated observations (fluxes) are missing for the chosen targets.")
      df_flux <- data.frame()
    }else{
      # Join P-model output and flux observations
      df_flux <- df |>
        dplyr::filter(sitename %in% flux_sites) |>
        dplyr::left_join(
          obs_flux, 
          by = c('sitename', 'date'))    # observations with missing date are ignored
    }
  }else{
    df_flux <- data.frame()
  }
  
  if(sum(!is_flux) > 0){
    trait_sites <- obs$sitename[!is_flux]
    
    # Unnest trait observations for our targets
    obs_trait <- obs[!is_flux, ] |>
      dplyr::select(sitename, data) |>
      tidyr::unnest(data) |>
      dplyr::select(any_of(c('sitename', targets,paste0(targets,"_qc"))))
    
    if(ncol(obs_trait) < 2){
      warning("Non-dated observations (traits) are missing for the chosen targets.")
      df_trait <- data.frame()
    }else{
      # Join output and trait observations
      df_trait <- df |>
        dplyr::filter(sitename %in% trait_sites) |>
        dplyr::group_by(sitename) |>
        # get growing season average traits
        dplyr::summarise(across(ends_with("_mod") & !starts_with('gpp'),
                                ~ sum(.x * gpp_mod/sum(gpp_mod)),
                                .names = "{.col}")) |>
        dplyr::left_join(
          obs_trait,
          by = c('sitename')        # compare yearly averages rather than daily obs
        )
    }
  }else{
    df_trait <- data.frame()
  }
  
  # loop over targets to compute log-likelihood ll
  ll_df <- data.frame(target = targets, 
                      ll     = NaN)
  for (target in targets){
    # check (needed?):
    if(target %in% colnames(df_flux) & target %in% colnames(df_trait)) {stop(
      sprintf("Target '%s' cannot be simultatneously in df_flux and df_trait.", target))
    }
    
    # get observations and predicted target values, without NA 
    df_target <- if(target %in% colnames(df_flux)){
      df_flux[, c(paste0(target, '_mod'), target ,paste0(target, '_qc'))] |> tidyr::drop_na()
    }else{
      df_trait[, c(paste0(target, '_mod'), target, paste0(target, '_qc'))] |> tidyr::drop_na()
    }
    
    # le in the output is on d^-1 while in input is in s^-1
    if(target == "le"){
      df_target$le_mod <- df_target$le_mod / (24*60*60)
    }
    
    df_target <- df_target |> filter(if_all(ends_with("_qc"), ~ . > 0.8))
    
    # calculate normal log-likelihood
    ll_df[ll_df$target == target, 'll'] <- 
      sum(stats::dnorm(
        x    = df_target[[paste0(target, '_mod')]], # model
        mean = df_target[[target]],                 # obs
        sd   = par[[paste0('err_', target)]],       # error model
        log  = TRUE))
  }
  ll <- sum(ll_df$ll)
  
  # trap boundary conditions
  if(is.nan(ll) | is.na(ll) | ll == 0){ll <- -Inf}
  
  return(ll)
}


# LE and GPP calibration

evaluation <- driver |>
  unnest(forcing) |>
  select(sitename,date,gpp,gpp_qc,le,le_qc) |>
  group_by(sitename) |>
  nest(data = c(date,gpp,gpp_qc,le,le_qc))

params_fix <- list(
  rd_to_vcmax        = 0.014,
  beta_unitcostratio = 146,
  kc_jmax = 0.41,
  tau_acclim         = 30,
  gw_calib = 2)

# Define calibration settings

settings <- list(
  method = "BayesianTools",
  par = list(
    kphio = list(lower=0.0001, upper=0.125, init = 0.05),
    kphio_par_a = list(lower = -0.001, upper = -0.00001, init = -0.0025),
    kphio_par_b = list(lower = 10, upper = 50, init = 20),
    soilm_thetastar = list(lower = 0.5, upper = 400, init=100), 
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

# Run the calibration for GPP data
calib_output <- rsofun::calib_sofun(
  drivers = driver,
  obs = evaluation,
  settings = settings,
  # extra arguments for the cost function
  par_fixed = params_fix,
  targets = c("gpp","le")
)


write_rds(calib_output, "../my_stuff/global_calib_PT_no_beta.rds")
