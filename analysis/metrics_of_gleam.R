library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(readr)
library(terra)
library(scico)
library(geosphere) # use to find the nearest grid to fluxnet
library(sf)
library(cowplot)
library(grid)


# plot performance against fluxnet

driver <- read_rds("/data_2/FluxDataKit/v3.4/zenodo_upload/rsofun_driver_data_v3.4.2.rds")
fdk_site_info <- read_csv("data/fluxnet/fdk_site_info.csv")

# data quality filter info
fdk_filter <-  read_csv("data/fluxnet/fdk_site_fullyearsequence.csv")

# exclude croplands and wetlands from evaluation
fdk_site_info <- fdk_site_info[fdk_site_info$igbp_land_use != "CRO" &
                                 fdk_site_info$igbp_land_use != "WET",]

# apply good year sequence data quality filter for LE
fdk_filter <- fdk_filter[fdk_filter$drop_le == "FALSE",]

# exclude site outside time range (from 1996 to 2011)
# the first tower starts in 1996, so only crop years after 2011

fdk_filter <- fdk_filter[fdk_filter$year_start_le <= 2011,]

fdk_filter$year_end_le <- ifelse(fdk_filter$year_end_le <= 2011, fdk_filter$year_end_le, 2011)


# include only the longest series of consecutive good quality data

driver <- driver[which(driver$sitename %in% fdk_site_info$sitename &
                         driver$sitename %in% fdk_filter$sitename),]

fdk_site_info <- fdk_site_info[which(fdk_site_info$sitename %in% driver$sitename),]

fdk_filter <- fdk_filter[which(fdk_filter$sitename %in% fdk_site_info$sitename),]

fdk_filter <- fdk_filter[duplicated(fdk_filter$sitename) == FALSE,]

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
  unnest(site_info) |>
  mutate(aet = 1000 * 60 * 60 * 24 * le / (calc_enthalpy_vap(temp) * calc_density_h2o(temp, patm))) |>
  select(sitename,lat,lon,date,aet)

driver_2 <- driver |>
  group_by(sitename,year(date),month(date)) |>
  summarise(aet = sum(aet, na.rm = T))

driver <- left_join(driver_2,driver[duplicated(driver$sitename) == FALSE,] |> select(sitename,lat,lon), by = "sitename")

rm(driver_2,driver_forcing,filter,calc_density_h2o,calc_enthalpy_vap,le_to_et)

# reframe p_model from date to coordinate (may be used to generate nc file)

p_model <-  read_csv("/data_2/scratch/akurth/grsofun_output/PM-S0/final_aet_df_PM-S0_20251126_1734.csv")

year_correct = p_model |>
  filter((year > 1981))

library(dplyr)
library(purrr)

matched_rows <- map_dfr(seq_len(nrow(fdk_site_info)), function(i) {
  site <- fdk_site_info[i, ]
  # Euclidean distance (squared is fine for which.min)
  idx <- which.min( (year_correct$lon - site$lon)^2 + (year_correct$lat - site$lat)^2 )
  year_row <- year_correct[idx, ]
  # keep the grid row and add the flux-site sitename
  year_row %>% mutate(site_sitename = site$sitename)
})

# View result
matched_rows

fdk_with_grid <- fdk_filter %>%
  left_join(matched_rows, by = c("sitename" = "site_sitename"))


filtered_year_correct <- map_dfr(seq_len(nrow(fdk_with_grid)), function(i) {
  site <- fdk_with_grid[i, ]

  year_correct %>%
    filter(
      lon == site$lon,
      lat == site$lat,
      year >= site$year_start_le,
      year <= site$year_end_le
    ) %>%
    mutate(sitename = site$sitename)   # flux site name
})

filtered_year_correct


driver3 <- driver %>%
  rename(
    year = `year(date)`,
    month = `month(date)`
  )

merged_driver <- driver3 %>%
  left_join(filtered_year_correct,
            by = c("sitename", "year", "month"))

df <- merged_driver %>%
  rename(
    aet_driver = aet.x,
    aet_correct = aet.y
  )


rmse_global <- df %>%
  summarise(
    rmse = sqrt(mean((aet_driver - aet_correct)^2, na.rm = TRUE))
  ) %>%
  pull(rmse)

mean(rmse_global,na.rm = T)


cor_by_site <- df %>%
  group_by(sitename) %>%
  summarise(
    correlation = cor(aet_driver, aet_correct, use = "pairwise.complete.obs")
  )

cor_by_site


library(ggplot2)
library(dplyr)

# RMSE data frame
rmse_df <- tibble(
  metric = "RMSE",
  value = rmse_global
)

# Plot
ggplot(rmse_df, aes(x = metric, y = value)) +
  geom_boxplot() +
  theme_minimal(base_size = 14) +
  labs(
    x = NULL,
    y = "RMSE",
    title = "Global RMSE"
  )


# Correlation data frame
corr_df <- cor_by_site %>%
  transmute(value = correlation)

# Plot
ggplot(corr_df, aes(x = "", y = value)) +
  geom_boxplot() +
  theme_minimal(base_size = 14) +
  labs(
    x = NULL,
    y = "Correlation",
    title = "Correlation by Site"
  )


