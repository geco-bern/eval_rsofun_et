calc_vpd_td <- function(tair, td){
  # actual vapour pressure from dewpoint temperature is equal to the
  # saturation vapour pressure
  calc_esat(tair) - calc_esat(td)
}

calc_esat <- function(tair){
  #-----------------------------------------------------------------------
  # Ref:      Eq. 5.1, Abtew and Meleese (2013), Ch. 5 Vapor Pressure
  #           Calculation Methods, in Evaporation and Evapotranspiration:
  #           Measurements and Estimations, Springer, London.
  #             vpd = 0.611*exp[ (17.27 tc)/(tc + 237.3) ] - ea
  #             where:
  #                 tc = average daily air temperature, deg C
  #                 eact  = actual vapor pressure, Pa
  #-----------------------------------------------------------------------
  610.8 * exp((17.27 * tair)/(tair + 237.3))
}


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
