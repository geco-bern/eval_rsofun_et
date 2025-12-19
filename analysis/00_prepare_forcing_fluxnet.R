# FLUXNET forcing data preparation ---------------------------------------------
## library and data loading ----------------------------------------------------
library(dplyr)
library(tidyr)
library(readr)
library(purrr)
library(lubridate)
library(here)
library(rnaturalearth)
library(ggplot2)

source(here("R/create_table_latex.R"))

## Read external files ---------------------------------------------------------
# These files are all part of the Zenodo release
driver <- read_rds("/data_2/FluxDataKit/v3.4/zenodo_upload/rsofun_driver_data_v3.4.2.rds")
# driver <- read_rds("~/data_2/FluxDataKit/v3.4/zenodo_upload/rsofun_driver_data_v3.4.2.rds")

fdk_site_info <- read_csv("/data_2/FluxDataKit/v3.4/zenodo_upload/fdk_site_info.csv")
# fdk_site_info <- read_csv("~/data_2/FluxDataKit/v3.4/zenodo_upload/fdk_site_info.csv")

# data quality filter info
fdk_filter <- read_csv("/data_2/FluxDataKit/v3.4/zenodo_upload/fdk_site_fullyearsequence.csv")
# fdk_filter <- read_csv("~/data_2/FluxDataKit/v3.4/zenodo_upload/fdk_site_fullyearsequence.csv")

## Select sites ----------------------------------------------------------------
# select sites based on minimum year availability (1), veg type filter, etc.
sites <- fdk_site_info |>
  filter(!(sitename %in% c("MX-Tes", "US-KS3"))) |>  # failed sites
  filter(!(igbp_land_use %in% c("CRO", "WET"))) |>  # exclude croplands and wetlands from calibration/evaluation
  left_join(
    fdk_filter,
    by = "sitename"
  ) |>
  filter(!drop_gpp & !drop_le) |>  # where no full year sequence was found
  filter(nyears_gpp >= 1, nyears_le >= 1)

## Select years ----------------------------------------------------------------
# select years based on good-quality data sequences
driver <- driver |>
  select(sitename, forcing) |>
  unnest(cols = c(forcing)) |>

  # reduce to good-qualilty sequences for GPP
  left_join(
    sites |>
      select(
        sitename,
        year_start = year_start_gpp,
        year_end = year_end_gpp
        ),
    by = join_by(sitename)
  ) |>
  mutate(year = year(date)) |>
  filter(year >= year_start & year <= year_end) |>
  select(-year_start, -year_end, -year) |>

  # reduce to good-quality sequences for LE
  left_join(
    sites |>
      select(
        sitename,
        year_start = year_start_le,
        year_end = year_end_le
      ),
    by = join_by(sitename)
  ) |>
  mutate(year = year(date)) |>
  filter(year >= year_start & year <= year_end) |>
  select(-year_start, -year_end, -year) |>

  # format back to make it a rsofun driver object
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

# write to file
write_rds(
  driver,
  file = here("data/driver.rds")
)

## Create overview table -------------------------------------------------------
# ... of effectively used sites and years
df_sites_metainfo <- driver |>
  mutate(year_start_end = map(
    forcing,
    ~{. |>
        mutate(year = year(date)) |>
        summarise(year_start = min(year), year_end = max(year))
        })) |>
  mutate(
    year_start = map_int(year_start_end, "year_start"),
    year_end = map_int(year_start_end, "year_end")
    ) |>
  select(-params_siml, -site_info, -forcing, -year_start_end) |>
  left_join(
    fdk_site_info |>
      select(-year_start, -year_end),
    by = join_by(sitename)
  )

# write to file
write_rds(
  df_sites_metainfo,
  file = here("data/df_sites_metainfo.rds")
)

### Latex version of table -----------------------------------------------------
create_table_latex(
  df_sites_metainfo |>
    select(
      Site = sitename,
      `Lon.` = lon,
      `Lat.` = lat,
      `Elv.` = elv,
      `Year start` = year_start,
      `Year end` = year_end,
      `Veg. type` = igbp_land_use,
      `Climate` = koeppen_code,
      `Canopy height` = canopy_height,
      `Ref. height` = reference_height,
      `S0` = whc
    ),
  caption = "Selected sites and years.",
  filn = here("data/df_sites_metainfo.tex")
  # align = c("p{0.1cm}", "p{5cm}", "p{7cm}")
)

## Create overview map ---------------------------------------------------------
# get coastline
coast <- rnaturalearth::ne_coastline(scale = 110, returnclass = "sf")

### Site density ----------------------------------------------------------------
world <- ne_countries(scale = "medium", returnclass = "sf")

gg_sitedensity <- ggplot() +
  # world country outlines
  geom_sf(data = world, fill = "gray95", color = "gray70", size = 0.2) +

  # geom_point(
  #   data = df_sites_metainfo,
  #   mapping = aes(x = lon, y = lat),
  #   size = 0.5,
  #   color = "red"
  # ) +

  # hex bin layer: count of points per hex
  stat_bin_hex(
    data = df_sites_metainfo,
    mapping = aes(x = lon, y = lat, fill = after_stat(count)),
    bins = c(100, 70),
    color = NA,
    alpha = 0.9
  ) +

  # # discrete-looking color scale
  # scale_fill_stepsn(
  #   name = "Sites\ncount",
  #   colours = viridis::viridis(5, option = "D"),
  #   breaks  = seq(0, 5),
  #   # limits  = c(0, 100),
  #   na.value = "transparent"
  # ) +

  # color (count) scale
  scale_fill_viridis_c(
    name = "Sites\ncount",
    option = "A",
    # trans = "sqrt",
    na.value = "transparent"
  ) +

  # coordinate system (preserves lat/lon aspect)
  coord_sf(
    ylim = c(-60, 85),
    expand = FALSE # to draw map strictly bounded by the specified extent
  ) +

  # labels and theme
  labs(
    x = "",
    y = ""
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_line(color = "gray90", size = 0.2),
    legend.position = "right"
  )

gg_sitedensity


# xxxxxxxxxxxxxxxx
#
# fdk_site_info <- fdk_site_info[fdk_site_info$igbp_land_use != "CRO" &
#                                  fdk_site_info$igbp_land_use != "WET", ]
#
# ## GPP: Good year sequence filter
# fdk_filter <- fdk_filter[fdk_filter$drop_gpp == "FALSE", ]
#
# driver <- driver[which(driver$sitename %in% fdk_site_info$sitename &
#                          driver$sitename %in% fdk_filter$sitename), ]
#
# fdk_site_info <- fdk_site_info[which(fdk_site_info$sitename %in% driver$sitename), ]
#
# fdk_filter <- fdk_filter[which(fdk_filter$sitename %in% driver$sitename &
#                                  fdk_filter$sitename %in% fdk_site_info$sitename), ]
#
# driver_forcing <- driver |>
#   select(sitename, forcing) |>
#   unnest(cols = c(forcing))
#
# driver <- driver_forcing |>
#   left_join(
#     fdk_filter |>
#       select(
#         sitename,
#         year_start = year_start_gpp,
#         year_end = year_end_gpp
#       ),
#     by = join_by(sitename)
#   ) |>
#   mutate(year = year(date)) |>
#   filter(year >= year_start & year <= year_end) |>
#   select(-year_start, -year_end, -year) |>
#   group_by(sitename) |>
#   nest() |>
#   left_join(
#     driver |>
#       select(
#         sitename,
#         site_info,
#         params_siml
#       ),
#     by = join_by(sitename)
#   ) |>
#   rename(forcing = data) |>
#   select(sitename, params_siml, site_info, forcing) |>
#   ungroup()
#
#
# ## LE: Good year sequence filter
# fdk_filter <- fdk_filter[fdk_filter$drop_le == "FALSE", ]
#
# driver <- driver[which(driver$sitename %in% fdk_site_info$sitename &
#                          driver$sitename %in% fdk_filter$sitename), ]
#
# fdk_site_info <- fdk_site_info[which(fdk_site_info$sitename %in% driver$sitename), ]
#
# fdk_filter <- fdk_filter[which(fdk_filter$sitename %in% driver$sitename &
#                                  fdk_filter$sitename %in% fdk_site_info$sitename), ]
#
# driver_forcing <- driver |>
#   select(sitename, forcing) |>
#   unnest(cols = c(forcing))
#
# driver <- driver_forcing |>
#   left_join(
#     fdk_filter |>
#       select(
#         sitename,
#         year_start = year_start_le,
#         year_end = year_end_le
#       ),
#     by = join_by(sitename)
#   ) |>
#   mutate(year = year(date)) |>
#   filter(year >= year_start & year <= year_end) |>
#   select(-year_start, -year_end, -year) |>
#   group_by(sitename) |>
#   nest() |>
#   left_join(
#     driver |>
#       select(
#         sitename,
#         site_info,
#         params_siml
#       ),
#     by = join_by(sitename)
#   ) |>
#   rename(forcing = data) |>
#   select(sitename, params_siml, site_info, forcing) |>
#   ungroup()
#
# fdk_site_info <- fdk_site_info[which(fdk_site_info$sitename %in% driver$sitename), ]
#
# good_sites <- readLines("../my_stuff/good_sites.txt")
#
# driver <- driver[driver$sitename %in% good_sites, ]
#
# fdk_site_info <- fdk_site_info[which(fdk_site_info$sitename %in% driver$sitename), ]
#
# driver <- driver |>
#   unnest(params_siml) |>
#   mutate(
#     use_gs = TRUE,
#     use_phydro = FALSE,
#     use_pml = TRUE,
#     is_global = F
#   ) |>
#   group_by(sitename) |>
#   nest(params_siml = c(
#     spinup, spinupyears, recycle, outdt, ltre, ltne, ltrd, ltnd, lgr3, lgn3, lgr4,
#     use_gs, use_phydro, use_pml, is_global
#   ))

