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
