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
