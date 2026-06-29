# FLUXNET forcing data preparation ---------------------------------------------
## Library and data loading ----------------------------------------------------
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
# FluxDataKit v3.4 Zenodo release files.
# Place the three files below inside data/fluxnet/ before running this script.

driver <- read_rds(here("data/fluxnet/rsofun_driver_data_v3.4.2.rds"))

fdk_site_info <- read_csv(here("data/fluxnet/fdk_site_info.csv"),
                          show_col_types = FALSE)

# data quality filter info
fdk_filter <- read_csv(here("data/fluxnet/fdk_site_fullyearsequence.csv"),
                       show_col_types = FALSE)

## Select sites ----------------------------------------------------------------
# # remove sites with missing observed GPP or LE in driver data
# driver <- driver |>
#   mutate(
#     nmissing_gpp = map_int(forcing, ~sum(is.na(.$gpp))),
#     nmissing_le = map_int(forcing, ~sum(is.na(.$le)))
#   ) |>
#   filter(nmissing_gpp == 0 & nmissing_le == 0) |>
#   select(-nmissing_gpp, -nmissing_le)

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



sites_train <- sites |>
  mutate(strata = interaction(koeppen_code, igbp_land_use, drop = TRUE)) |>
  filter(nyears_gpp & nyears_le > 12) |>
  group_by(strata) |>
  sample_n(size = 1, replace = FALSE) |>
  ungroup() |>
  pull(sitename)

# add column specifying whether part of training (calibration) set
sites <- sites |>
  mutate(train = ifelse(sitename %in% sites_train, TRUE, FALSE))

# write to file
write_csv(
  sites,
  file = here("data/sites.csv")
)

## Select years ----------------------------------------------------------------
# select years based on good-quality data sequences
driver <- driver |>

  # subset driver to sites retained up to this point: after removing sites without
  # a full year of good-quality data, and after removing croplands and wetlands
  filter(sitename %in% sites$sitename) |>

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
    sites |>
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
      Train = train,
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

  geom_point(
    data = df_sites_metainfo,
    mapping = aes(x = lon, y = lat, color = train),
    size = 0.5
  ) +

  scale_color_manual(
    values = c("grey40", "tomato")
  ) +

  # # hex bin layer: count of points per hex
  # stat_bin_hex(
  #   data = df_sites_metainfo,
  #   mapping = aes(x = lon, y = lat, fill = after_stat(count)),
  #   bins = c(100, 70),
  #   color = NA,
  #   alpha = 0.9
  # ) +

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
    panel.grid = element_line(color = "gray90", linewidth = 0.2),
    legend.position = "right"
  )

gg_sitedensity
