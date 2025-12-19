#' Evaluates SOFUN model outputs.
#'
#' Calculates a set of perfomance metrics for model outputs, compared against
#' observational data. Currently only evaluations of GPP model outputs, compared
#' agains FLUXNET 2015 data, are implemented.
#'
#' @param mod Object returned by \link{runread_pmodel_f}. This is a nested
#'  dataframe with sites along rows and a nested column \code{"data"} containing
#'  model outputs with columns \code{"date"} (date object created by
#'  \code{lubridate::ymd()}) and \code{"varnam"}.
#'  where \code{"varnam"} corresponds to \code{names(settings$benchmark)}.
#' @param settings A list specifying evaluation settings
#'  (see vignette eval_sofun.pdf for more information and examples)
#' @param obs_eval (Optional) A named list of data frames containing
#'  observational data for each sites. The names of list elements corresponds
#'  to site names. Defaults to \code{NA}
#' @param overwrite (Optional) A logical specifying whether temporary data
#'  stored in \code{./tmpdir} should be overwritten. Defaults to \code{TRUE}.
#' @param doplot (Optional) A logical specifying whether plots should be saved.
#'   Defaults to \code{FALSE}.
#' @param light (Optional) A logical specifying whether reduced data should
#'  saved. Defaults to \code{FALSE}.
#'
#' @return A list containing data frames of modelled and observed values
#'  aggregated to several temporal scales (ddf for daily, xdf for X-daily, mdf
#'  for monthly, adf for annual), data frames of respective performance metrics,
#'  and functions for plotting respective data.
#' @export
#'

eval_sofun <- function(
    mod,
    settings,
    obs_eval = NA,
    overwrite = TRUE,
    doplot = FALSE,
    light = FALSE
    ){

  ## make model output a long flat table
  mod <- mod |>
    # dplyr::rename(id = sitename) |>
    tidyr::unnest(data)

  ## Evaluate daily variables
  out <- purrr::map(
    as.list(names(settings$benchmark)),
    ~eval_sofun_byvar(., dplyr::select(
      mod,
      sitename,
      date,
      mod = {{ . }}
    ),
    settings,
    obs_eval = obs_eval,
    overwrite = TRUE,
    doplot = FALSE,
    light = light
    )
  ) |>
    setNames(names(settings$benchmark))

  return(out)
}

eval_sofun_byvar <- function(
  varnam,
  ddf_mod,
  settings,
  obs_eval = NA,
  overwrite = TRUE,
  doplot = FALSE,
  light = light
  ){

  #!!! select axys label (add others if needed)

  if (varnam == "le"){

    lab = "LE  (J"
    pal = "davos"

    # convert unit to W m-2 (mean rate)
    ddf_mod <- ddf_mod |>
      mutate(mod = mod / (24*60*60))

  }
  if (varnam == "gpp"){
    lab = "GPP (gC"
    pal = "batlowW"
  }

  rlang::inform("-----------------------------------")
  rlang::inform(varnam)
  rlang::inform("-----------------------------------")

  ## initialise
  out <- list()

  ## Interpret benchmarking data specification
  datasource <- unlist(stringr::str_split(settings$benchmark[[varnam]], "_"))

  if ("fluxnet" %in% datasource) {

    # GPP EVALUATION AGAINST FLUXNET 2015 DATA
    # Evaluate model vs. observations for decomposed time series
    # into:
    # - spatial
    # - inter-annual
    # - multi-year trend
    # - seasonal (different time scales: daily/weekly/monthly)
    # - anomalies (different time scales: daily/weekly/monthly)

    ## Initialise lists
    metrics <- list()

    # get sites for which no model output is available and overwrite settings$sitenames
    missing_mod <- purrr::map_lgl(
      ddf_mod,
      ~ identical(., NA)
    ) |>
      which() |>
      names()

    settings$sitenames <- settings$sitenames[which(!(settings$sitenames %in% missing_mod))]


    # Get daily model output ----

    # missing_mod <- purrr::map_lgl( mod$daily, ~identical(., NA ) ) |> which() |> names()
    # missing_mod <- purrr::map_lgl( mod$daily, ~identical(., NULL ) ) |> which() |> names()

    # ddf_mod <- lapply(
    #   as.list(settings$sitenames),
    #   function(x)
    #     dplyr::select( mod$daily[[x]], date, mod = eval(varnam) ) |>
    #     mutate( sitename = x ) ) |>
    #   bind_rows()


    # Get observations for evaluation ----

    if (identical(obs_eval, NA)) {
      rlang::abort(
        "eval_sofun_byvar():
         Object provided by argument 'eval_sofun_byvar'
         could not be identified."
      )
    }

    ## detach
    varnam_obs <- varnam
    varnam_qc <- paste0(varnam, "_qc")

    adf <- obs_eval$adf |>
      tidyr::unnest(data) |>
      dplyr::select(sitename, year, obs = {{ varnam_obs }}, qc = {{ varnam_qc }})
    mdf <- obs_eval$mdf |>
      tidyr::unnest(data) |>
      dplyr::select(sitename, year, month, obs = {{ varnam_obs }}, qc = {{ varnam_qc }})
    ddf <- obs_eval$ddf |>
      tidyr::unnest(data) |>
      dplyr::select(sitename, date, obs = {{ varnam_obs }}, qc = {{ varnam_qc }}, lat, koeppen_code)
    xdf <- obs_eval$xdf |>
      tidyr::unnest(data) |>
      dplyr::select(sitename, bin, obs = {{ varnam_obs }}, qc = {{ varnam_qc }})

    # Aggregate model output data to annual/monthly/weekly ----

    # NOTE: only for dplyr::selected sites,
    # and merge into respective observational data frame

    rlang::inform("Aggregating model outputs...")

    # annual mean
    adf <- adf |>
      left_join(
        ddf_mod |>
          mutate(year = year(date)) |>
          group_by(sitename, year) |>
          summarise(
            mod = mean(mod),
            .groups = "drop"
          ),
        by = c("sitename", "year")
      ) |>
      dplyr::mutate(obs = ifelse(qc < 0.8, NA, obs))

    # monthly mean
    mdf <- mdf |>
      left_join(
        ddf_mod |>
          mutate(year = year(date), month = month(date)) |>
          group_by(sitename, year, month) |>
          summarise(
            mod = mean(mod),
            .groups = "drop"
          ),
        by = c("sitename", "year", "month")
      ) |>
      dplyr::mutate(obs = ifelse(qc < 0.8, NA, obs))

    # x-daily
    xdf <- xdf |>
      left_join(
        ddf_mod |>
          mutate(bin = cut(
            date,
            breaks = obs_eval$breaks,
            include.lowest = TRUE,
            right = FALSE)
          ) |>
          group_by(sitename, bin) |>
          summarise(
            mod = mean(mod),
            .groups = "drop"
          ),
        by = c("sitename", "bin")
      ) |>
      dplyr::mutate(obs = ifelse(qc < 0.8, NA, obs))

    # daily
    ddf <- ddf |>
      left_join(
        ddf_mod,
        by = c("sitename", "date")
      ) |>
      dplyr::mutate(obs = ifelse(qc < 0.8, NA, obs))

    ## metrics for daily and x-daily values, all sites pooled
    metrics$daily_pooled <- with(ddf, get_stats(mod, obs))
    metrics$xdaily_pooled <- with(xdf, get_stats(mod, obs))

    # Evaluate annual values by site ----
    if (sum(!is.na(adf$obs)) > 2) {
      rlang::inform("Evaluate annual values...")
      if (!light) {
        adf_stats <- adf |>
          mutate(
            validpoint = ifelse(is.na(obs) | is.na(mod), FALSE, TRUE)
          ) |>
          group_by(sitename) |>
          nest() |>
          mutate(npoints = purrr::map(data, ~ sum(.$validpoint))) |>
          unnest(npoints) |>
          dplyr::filter(npoints > 2) |>
          mutate(
            linmod = purrr::map(data, ~ lm(obs ~ mod, data = .)),
            stats = purrr::map(data, ~ get_stats(.$mod, .$obs))
          ) |>
          mutate(data = purrr::map(data, ~ add_fitted(.))) |>
          unnest(stats)
      } else {
        adf_stats <- NA
      }

      # metrics for annual values, all sites pooled ----
      metrics$annual_pooled <- with(adf, get_stats(mod, obs))
    } else {
      adf_stats <- NA
      metrics$annual_pooled <- list(rsq = NA, rmse = NA)
    }

    # Evaluate monthly values by site ----
    if (sum(!is.na(mdf$obs)) > 2) {
      rlang::inform("Evaluate monthly values...")
      mdf_stats <- NA

      ## metrics for annual values, all sites pooled
      metrics$monthly_pooled <- with(mdf, get_stats(mod, obs))
    } else {
      mdf_stats <- NA
      metrics$monthly_pooled <- list(rsq = NA, rmse = NA)
    }

    # Get mean annual GPP -> "spatial" data frame and evaluate it ----
    if (sum(!is.na(adf$obs)) > 2) {
      rlang::inform("Evaluate spatial values...")
      meandf <- adf |>
        group_by(sitename) |>
        summarise(
          obs = mean(obs, na.rm = TRUE),
          mod = mean(mod, na.rm = TRUE)
        )

      linmod_meandf <- lm(obs ~ mod, data = meandf)
      metrics$spatial <- with(meandf, get_stats(mod, obs))
    } else {
      meandf <- NA
      metrics$spatial <- list(rsq = NA, rmse = NA)
      linmod_meandf <- NA
    }


    # Get IAV as annual value minus mean by site ----

    if (sum(!is.na(adf$obs)) > 2) {
      rlang::inform("Evaluate interannual variability...")
      iavdf <- adf |>
        left_join(
          dplyr::rename(
            meandf,
            mod_mean = mod,
            obs_mean = obs
          ),
          by = "sitename"
        ) |>
        mutate(
          mod = mod - mod_mean,
          obs = obs - obs_mean
        ) |>
        dplyr::select(-obs_mean, -mod_mean)

      if (!light) {
        iavdf_stats <- iavdf |>
          group_by(sitename) |>
          nest() |>
          mutate(
            nyears_obs = purrr::map(
              data,
              ~ sum(!is.na(.$obs))
            ),
            nyears_mod = purrr::map(data, ~ sum(!is.na(.$mod)))
          ) |>
          unnest(c(nyears_obs, nyears_mod)) |>
          dplyr::filter(nyears_obs > 2 & nyears_mod > 2) |>
          mutate(
            linmod = purrr::map(data, ~ lm(obs ~ mod, data = .)),
            stats = purrr::map(data, ~ get_stats(.$mod, .$obs))
          ) |>
          mutate(data = purrr::map(data, ~ add_fitted(.))) |>
          unnest(stats)
      } else {
        iavdf_stats <- NA
      }

      metrics$anomalies_annual <- with(iavdf, get_stats(mod, obs))
    } else {
      iavdf <- NA
      iavdf_stats <- NA
      metrics$anomalies_annual <- list(rsq = NA, rmse = NA)
    }


    # Get mean seasonal cycle (by day of year) ----

    if (sum(!is.na(ddf$obs)) > 2) {
      rlang::inform("Evaluate mean seasonal cycle...")
      meandoydf <- ddf |>
        mutate(doy = yday(date)) |>
        dplyr::filter(doy != 366) |>
        # NOTE: XXXX this is a dirty fix! better force lubridate to
        # ignore leap years when calculating yday()
        group_by(sitename, doy) |>
        summarise(
          obs_mean = mean(obs, na.rm = TRUE),
          obs_min = min(obs, na.rm = TRUE),
          obs_max = max(obs, na.rm = TRUE),
          mod_mean = mean(mod, na.rm = TRUE),
          mod_min = min(mod, na.rm = TRUE),
          mod_max = max(mod, na.rm = TRUE)
        ) |>
        mutate(
          obs_min = ifelse(is.infinite(obs_min), NA, obs_min),
          obs_max = ifelse(is.infinite(obs_max), NA, obs_max)
        ) |>
        mutate(
          obs_mean_plot = interpol_lin(obs_mean),
          obs_min_plot = interpol_lin(obs_min),
          obs_max_plot = interpol_lin(obs_max),
          site = sitename
        )

      if (!light) {
        meandoydf_stats <- meandoydf |>
          group_by(sitename) |>
          nest()
      } else {
        meandoydf_stats <- NA
      }

      metrics$meandoy <- with(meandoydf, get_stats(mod_mean, obs_mean))

      # aggregate mean seasonal cycle by climate zone (koeppen-geiger) and
      # hemisphere (pooling sites within the same climate zone)
      # XXX This
      if (!light) {
        rlang::inform("Evaluate mean seasonal cycle by climate zones...")
        meandoydf_byclim <- ddf |>
          mutate(doy = yday(date)) |>
          mutate(hemisphere = ifelse(lat > 0, "north", "south")) |>
          dplyr::select(-lat) |>
          dplyr::filter(doy != 366) |>
          group_by(koeppen_code, hemisphere, doy) |>
          summarise(
            obs_mean = median(obs, na.rm = TRUE),
            obs_min = quantile(obs, 0.33, na.rm = TRUE),
            obs_max = quantile(obs, 0.66, na.rm = TRUE),
            mod_mean = median(mod, na.rm = TRUE),
            mod_min = quantile(mod, 0.33, na.rm = TRUE),
            mod_max = quantile(mod, 0.66, na.rm = TRUE),
            nsites = length(unique(sitename))
          ) |>
          mutate(
            obs_min = ifelse(is.infinite(obs_min), NA, obs_min),
            obs_max = ifelse(is.infinite(obs_max), NA, obs_max)
          ) |>
          mutate(
            obs_mean = interpol_lin(obs_mean),
            obs_min = interpol_lin(obs_min),
            obs_max = interpol_lin(obs_max)
          ) |>
          mutate(climatezone = paste(koeppen_code, hemisphere))

        meandoydf_byclim_stats <- NA
      } else {
        meandoydf_byclim_stats <- NA
        metrics$meandoy_byclim <- list(rsq = NA, rmse = NA)
        meandoydf_byclim <- NA
      }
    } else {
      meandoydf <- NA
      meandoydf_stats <- NA
      metrics$meandoy <- list(rsq = NA, rmse = NA)
      meandoydf_byclim <- NA
      meandoydf_byclim_stats <- NA
      metrics$meandoy_byclim <- list(rsq = NA, rmse = NA)
    }

    # IDV (inter-day variability) as daily value minus mean by site and DOY ----
    if (sum(!is.na(ddf$obs)) > 2) {
      rlang::inform("Evaluate inter-day variability...")
      idvdf <- ddf |>
        mutate(doy = yday(date)) |>
        left_join(
          dplyr::rename(meandoydf,
                        mod_mean = mod_mean,
                        obs_mean = obs_mean
          ),
          by = c("sitename", "doy")
        ) |>
        mutate(
          mod = mod - mod_mean,
          obs = obs - obs_mean
        ) |>
        dplyr::select(
          -obs_mean,
          -mod_mean,
          -obs_min,
          -obs_max,
          -mod_min,
          -mod_max
        )

      if (!light) {
        idvdf_stats <- idvdf |>
          group_by(sitename) |>
          nest() |>
          mutate(
            ndays_obs = purrr::map(data, ~ sum(!is.na(.$obs))),
            ndays_mod = purrr::map(data, ~ sum(!is.na(.$mod)))
          ) |>
          unnest(c(ndays_obs, ndays_mod)) |>
          dplyr::filter(ndays_obs > 2 & ndays_mod > 2) |>
          mutate(
            linmod = purrr::map(data, ~ lm(obs ~ mod, data = .)),
            stats = purrr::map(data, ~ get_stats(.$mod, .$obs))
          ) |>
          mutate(data = purrr::map(data, ~ add_fitted(.))) |>
          unnest(stats)
      } else {
        idvdf_stats <- NA
      }

      metrics$anomalies_daily <- with(idvdf, get_stats(mod, obs))
    } else {
      idvdf <- NA
      idvdf_stats <- NA
      metrics$anomalies_daily <- list(rsq = NA, rmse = NA)
    }

    # FLUXNET2015-Plots ----

    if (!light) {

      # Mod. vs. obs. of mean per site -> spatial correlation ----

      plot_modobs_spatial <- function(lab) { # using meandf

        out <- meandf |>
          analyse_modobs2("mod", "obs", type = "points")

        out$gg <- out$gg +
          labs(
            title = "Spatial correlation",
            y = substitute(paste("Observed ", lab, " m"^-2, "s"^-1, ")"), list(lab = lab)),
            x = substitute(paste("Simulated ", lab, " m"^-2, "s"^-1, ")"), list(lab = lab))
            )

        return(out)
      }


      # Combined spatial - IAV correlation ----
      plot_modobs_spatial_annual <- function(lab) {
        get_start_end <- function(df) {
          df_start <- df |>
            arrange(mod) |>
            drop_na(mod, fitted) |>
            slice(1)
          df_end <- df |>
            arrange(desc(mod)) |>
            drop_na(mod, fitted) |>
            slice(1)
          out <- tibble(
            xmin = df_start$mod,
            xmax = df_end$mod,
            ymin = df_start$fitted,
            ymax = df_end$fitted
          )
          return(out)
        }
        df <- adf_stats |>
          mutate(start_end = purrr::map(data, ~ get_start_end(.))) |>
          tidyr::unnest(start_end)

        rsq_lab_annual <- format(metrics$annual_pooled$rsq, digits = 2)
        rmse_lab_annual <- format(metrics$annual_pooled$rmse, digits = 3)

        rsq_lab_spatial <- format(metrics$spatial$rsq, digits = 2)
        rmse_lab_spatial <- format(metrics$spatial$rmse, digits = 3)

        gg <- df |>
          ggplot() +
          geom_segment(
            aes(
              x = xmin,
              y = ymin,
              xend = xmax,
              yend = ymax
            )
          ) +
          geom_line(
            data = fortify(linmod_meandf),
            aes(
              x = mod,
              y = .fitted
            ),
            color = "red"
          ) +
          geom_abline(
            intercept = 0,
            slope = 1,
            linetype = "dotted"
          ) +
          theme_classic() +
          xlim(0, NA) +
          ylim(0, NA) +
          labs(
            subtitle =
              bquote(bold("Annual:") ~ italic(R)^2 == .(rsq_lab_annual) ~ ~
                       RMSE == .(rmse_lab_annual) ~ "\n" ~
                       bold("Spatial:") ~ italic(R)^2 == .(rsq_lab_spatial) ~ ~
                       RMSE == .(rmse_lab_spatial)),
            y = substitute(paste("Observed ", lab, " m"^-2, "s"^-1, ")"), list(lab = lab)),
            x = substitute(paste("Simulated ", lab, " m"^-2, "s"^-1, ")"), list(lab = lab))
          )

        return(gg)
      }

      ## ------------------------------------------------------------
      ## Mod. vs. obs for daily values (absolute)
      ## ------------------------------------------------------------
      ## observed vs. modelled
      plot_modobs_daily <- function(lab) {
        modobs_ddf <- ddf |>
          analyse_modobs2(mod = "mod", obs = "obs", type = "hex", pal = pal)

        gg <- modobs_ddf$gg +
          labs(
            y = substitute(paste("Observed ",lab," m"^-2, "s"^-1, ")"),list(lab=lab)),
            x = substitute(paste("Simulated ",lab," m"^-2, "s"^-1, ")"),list(lab=lab)),
            title = paste("Daily ",substr(lab,1,4))
          )

        return(gg)
      }

      ## ------------------------------------------------------------
      ## Mod. vs. obs for monthly values (absolute)
      ## ------------------------------------------------------------
      ## observed vs. modelled
      plot_modobs_monthly <- function(lab) {
        modobs_mdf <- mdf |>
          analyse_modobs2(mod = "mod", obs = "obs", type = "hex", pal = pal)

        gg <- modobs_mdf$gg +
          labs(
            x = substitute(paste("Observed ",lab," m"^-2, "s"^-1, ")"),list(lab=lab)),
            y = substitute(paste("Simulated ",lab," m"^-2, "s"^-1, ")"),list(lab=lab)),
            title = paste("Monthly ",substr(lab,1,4))
          )

        return(gg)
      }

      ## ------------------------------------------------------------
      ## Mod. vs. obs for annual values (absolute)
      ## ------------------------------------------------------------
      ## observed vs. modelled
      plot_modobs_annual <- function(lab) {
        modobs_adf <- adf |>
          analyse_modobs2(mod = "mod", obs = "obs", type = "points")

        gg <- modobs_adf$gg +
          labs(
            x = substitute(paste("Observed ",lab," m"^-2, "s"^-1, ")"),list(lab=lab)),
            y = substitute(paste("Simulated ",lab," m"^-2, "s"^-1, ")"),list(lab=lab)),
            title = paste("Annual ",substr(lab,1,4))
          )

        return(gg)
      }


      ## ------------------------------------------------------------
      ## Mod. vs. obs. for aggregated values (absolute) aggregated to X-day periods
      ## ------------------------------------------------------------
      ## observed vs. modelled
      plot_modobs_xdaily <- function(lab) {
        modobs_ddf <- xdf |>
          analyse_modobs2(mod = "mod", obs = "obs", type = "hex", pal = pal)

        gg <- modobs_ddf$gg +
          labs(
            y = substitute(paste("Observed ",lab," m"^-2, "s"^-1, ")"),list(lab=lab)),
            x = substitute(paste("Simulated ",lab," m"^-2, "s"^-1, ")"),list(lab=lab)),
            title = paste("X-daily ",substr(lab,1,4))
          )
        return(gg)
      }
    }

    rlang::inform("Done with eval_sofun().")

    ## ------------------------------------------------------------
    ## Construct output lists for FLUXNET2015
    ## ------------------------------------------------------------
    out$fluxnet <- list()

    out$fluxnet$data <- list(
      adf_stats              = adf_stats,
      mdf_stats              = mdf_stats,
      meandf                 = meandf,
      meandf                 = meandf,
      linmod_meandf          = linmod_meandf,
      iavdf                  = iavdf,
      iavdf_stats            = iavdf_stats,
      idvdf                  = idvdf,
      idvdf_stats            = idvdf_stats,
      meandoydf              = meandoydf,
      meandoydf_stats        = meandoydf_stats,
      meandoydf_stats        = meandoydf_stats,
      meandoydf_byclim       = meandoydf_byclim,
      adf                    = adf,
      mdf                    = mdf,
      ddf                    = ddf,
      xdf                    = xdf
    )

    out$fluxnet$metrics <- metrics

    if (!light) {
      out$fluxnet$plot <- list(
        gg_modobs_daily = plot_modobs_daily(lab),
        gg_modobs_xdaily = plot_modobs_xdaily(lab),
        gg_modobs_monthly = plot_modobs_monthly(lab),
        gg_modobs_annual = plot_modobs_annual(lab),
        modobs_spatial = plot_modobs_spatial(lab),
        gg_modobs_spatial_annual = plot_modobs_spatial_annual(lab)
      )
    }
  } # end FLUXNET2015

  return(out)
}

add_fitted <- function(data) {
  linmod <- lm(obs ~ mod, data = data, na.action = "na.exclude")
  data$fitted <- fitted(linmod)
  return(data)
}

add_fitted_alternativenames <- function(data) {
  linmod <- lm(obs_mean ~ mod_mean, data = data, na.action = "na.exclude")
  data$fitted <- fitted(linmod)
  return(data)
}

interpol_lin <- function(vec) {
  out <- try(approx(seq(length(vec)), vec, xout = seq(length(vec)))$y)
  if (class(out) == "try-error") out <- vec * NA
  return(out)
}

extract_koeppen_code <- function(str) {
  out <- stringr::str_split(str, " - ")[[1]][1]
  return(out)
}
