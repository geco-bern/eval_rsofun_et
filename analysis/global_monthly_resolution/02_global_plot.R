# analysis/07_global_plot.R
#
# PURPOSE:
#   Produce all global-scale figures for the evaluation of gRSOFUN ET output:
#     a) Latitudinal profile of annual AET — P-model vs. multi-product ensemble
#     b) Global AET map (P-model annual mean)
#     c) RMSE and R² boxplots (site-level, one box per reference product)
#
#   Requires: analysis/06_processed_global_data.R to have been run first, or
#   the saved evaluation dataset to be present at
#   processed_global_data/global_eval_dataset.rds.
#
# All ET values are in mm d-1 (monthly mean). Annual sums are derived here
# by multiplying monthly means by the number of days in each month and
# summing over the year.
#
# AUTHORS: Grossi et al. (in prep.)

library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(purrr)
library(readr)
library(here)
library(cowplot)
library(scico)
library(rnaturalearth)

# ── Study period ──────────────────────────────────────────────────────────
YEAR_START <- 1997L
YEAR_END   <- 2011L

# ── Days-in-month lookup (ignoring leap years — consistent with prior work) ──
days_in_month_df <- tibble(
  month = 1:12,
  days  = c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
)

# =========================================================================
# 0. Load evaluation dataset  --------------------------------------------
# =========================================================================

eval_path <- here("processed_global_data", "global_eval_dataset.rds")

if (file.exists(eval_path)) {
  message("Loading pre-assembled evaluation dataset ...")
  eval_ds <- readRDS(eval_path)
} else {
  message("Dataset not found — running 06_processed_global_data.R ...")
  source(here("analysis", "06_processed_global_data.R"))
  eval_ds <- readRDS(eval_path)
}

global_products <- eval_ds$global_products   # {source, date, data<lon,lat,aet>}
p_model         <- eval_ds$p_model           # {sitename, year, month, date, aet, lon, lat, fland}
monthly_site_df <- eval_ds$fluxnet_monthly   # {sitename, date, obs_aet, era5, pml, fluxcom, rsofun}

# =========================================================================
# 1. Latitudinal profile  ------------------------------------------------
# =========================================================================
#   For each latitude band, compute median and 33rd/66th percentile of
#   annual AET [mm yr-1] across all longitudes and years (1997–2011).
#   Annual AET = sum(monthly_mean_mm_d * days_in_month) over 12 months.
# =========================================================================

message("Building latitudinal profile ...")

# ── Reference products: annual sums per grid cell × year ─────────────────
annual_refs <- global_products |>
  mutate(
    year  = year(date),
    month = month(date)
  ) |>
  left_join(days_in_month_df, by = "month") |>
  unnest(data) |>                              # lon, lat, aet [mm d-1]
  mutate(
    aet_mm = aet * days,                       # mm month-1 contribution
    lat    = round(lat, 4)                     # avoid floating-point joins
  ) |>
  group_by(source, year, lon, lat) |>
  summarise(ann_aet = sum(aet_mm, na.rm = TRUE), .groups = "drop")

# Snap ERA5 latitudes to 0.05° grid edge (product uses 0.1° grid, some
# cells land between 0.5° PML/FLUXCOM nodes after resampling — align here)
annual_refs <- annual_refs |>
  mutate(lat = ifelse(lat < 0,
                      as.integer(20 * lat + 1) / 20,
                      as.integer(20 * lat + 2) / 20))

# Pool all three products (treat them as an ensemble)
lat_profile_refs <- annual_refs |>
  filter(lat > -60) |>
  group_by(year, lon, lat) |>
  summarise(ann_aet = mean(ann_aet, na.rm = TRUE), .groups = "drop") |>
  group_by(lat) |>
  summarise(
    med  = median(ann_aet,          na.rm = TRUE),
    q_33 = quantile(ann_aet, 0.33,  na.rm = TRUE),
    q_66 = quantile(ann_aet, 0.66,  na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(Setup = "Reference ensemble\n(ERA5 / PML / FLUXCOM)")

# ── P-model: annual sums per grid cell × year ────────────────────────────
lat_profile_pm <- p_model |>
  filter(lat < 83.75, lat > -55.75) |>
  left_join(days_in_month_df, by = "month") |>
  mutate(aet_mm = aet * days) |>              # mm d-1 × days = mm month-1
  group_by(year, lon, lat) |>
  summarise(ann_aet = sum(aet_mm, na.rm = TRUE), .groups = "drop") |>
  group_by(lat) |>
  summarise(
    med  = median(ann_aet,          na.rm = TRUE),
    q_33 = quantile(ann_aet, 0.33,  na.rm = TRUE),
    q_66 = quantile(ann_aet, 0.66,  na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(Setup = "P-model (gRSOFUN)")

df_profile <- bind_rows(lat_profile_pm, lat_profile_refs)

lateral_profile <- ggplot(df_profile) +
  geom_ribbon(
    aes(x = lat, ymin = q_33, ymax = q_66, fill = Setup),
    alpha = 0.25
  ) +
  geom_line(
    aes(x = lat, y = med, colour = Setup),
    linewidth = 0.9
  ) +
  scale_colour_manual(
    values = c(
      "P-model (gRSOFUN)"                        = "royalblue",
      "Reference ensemble\n(ERA5 / PML / FLUXCOM)" = "black"
    ),
    name = NULL
  ) +
  scale_fill_manual(
    values = c(
      "P-model (gRSOFUN)"                        = "royalblue",
      "Reference ensemble\n(ERA5 / PML / FLUXCOM)" = "black"
    ),
    name = NULL
  ) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
  coord_flip() +
  labs(
    x = "Latitude (°)",
    y = expression(paste("Annual AET (mm yr"^{-1}, ")"))
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.major = element_line(colour = "grey90"),
    legend.position  = "bottom",
    aspect.ratio     = 1.5
  )

print(lateral_profile)

# =========================================================================
# 2. Global AET map (P-model)  -------------------------------------------
# =========================================================================

message("Building global AET map ...")

map_world <- p_model |>
  left_join(days_in_month_df, by = "month") |>
  mutate(aet_mm = aet * days) |>               # mm month-1
  group_by(lon, lat, year) |>
  summarise(ann_aet = sum(aet_mm, na.rm = TRUE), .groups = "drop") |>
  group_by(lon, lat) |>
  summarise(aet = mean(ann_aet, na.rm = TRUE), .groups = "drop") |>  # multi-year mean
  drop_na()

# ── Colour palette (lapaz, reversed for water) ───────────────────────────
lapaz_raw <- scico_palette_data("lapaz")
lapaz_rev <- lapaz_raw[nrow(lapaz_raw):1, ]
lapaz_rev$hex <- rgb(lapaz_rev$r, lapaz_rev$g, lapaz_rev$b)

find_closest_color <- function(value, palette) {
  idx <- which.min(abs(seq(0, 1, length.out = length(palette)) - value))
  palette[idx]
}

map_world <- map_world |>
  mutate(
    norm = (aet - min(aet)) / (max(aet) - min(aet)),
    hex  = map_chr(norm, find_closest_color, palette = lapaz_rev$hex)
  )

# ── Spatial context layers ────────────────────────────────────────────────
layer_coast <- ne_coastline(scale = 110, returnclass = "sf")
layer_ocean <- ne_download(
  scale    = 110,
  type     = "ocean",
  category = "physical",
  returnclass = "sf",
  destdir  = here("data")
)

ggmap <- ggplot() +
  geom_raster(data = map_world, aes(x = lon, y = lat, fill = hex)) +
  scale_fill_identity() +
  geom_sf(data = layer_ocean, colour = NA,      fill  = "azure3") +
  geom_sf(data = layer_coast, colour = "black", linewidth = 0.1) +
  labs(x = NULL, y = NULL) +
  theme_classic(base_size = 11) +
  theme(
    axis.title  = element_blank(),
    axis.text   = element_blank(),
    axis.ticks  = element_blank(),
    axis.line   = element_blank(),
    panel.grid  = element_blank(),
    plot.background = element_rect(fill = "white", colour = NA)
  )

# ── Discrete legend ───────────────────────────────────────────────────────
source(here("my_stuff", "global_legend.R"))   # provides plot_discrete_cbar()

breaks     <- seq(0, 2750, length.out = 11)
pal_breaks <- lapaz_rev$hex[round(seq(1, 256, length.out = 11))]

gglegend <- plot_discrete_cbar(
  breaks          = breaks,
  colors          = pal_breaks,
  legend_title    = expression(paste("Annual AET (mm yr"^{-1}, ")")),
  legend_direction = "vertical",
  width           = 0.03,
  font_size       = 3,
  expand_size_y   = 0.5,
  spacing         = "constant"
)

global_merged <- cowplot::plot_grid(
  ggmap, gglegend,
  ncol       = 2,
  rel_widths = c(1, 0.10)
)

print(global_merged)

# =========================================================================
# 3. Site-level evaluation metrics  -------------------------------------
# =========================================================================

message("Computing site-level RMSE and R² ...")

# All values are in mm d-1 — no unit conversion needed
# RMSE: root-mean-squared error per observation
rmse_long <- monthly_site_df |>
  transmute(
    PML     = sqrt((obs_aet - pml)^2),
    ERA5    = sqrt((obs_aet - era5)^2),
    FLUXCOM = sqrt((obs_aet - fluxcom)^2),
    P_model = sqrt((obs_aet - rsofun)^2)
  ) |>
  pivot_longer(everything(), names_to = "model", values_to = "RMSE") |>
  drop_na()

# R²: per-site correlation, then pooled across sites
r2_long <- monthly_site_df |>
  drop_na() |>
  group_by(sitename) |>
  summarise(
    PML     = cor(obs_aet, pml,     use = "complete.obs")^2,
    ERA5    = cor(obs_aet, era5,    use = "complete.obs")^2,
    FLUXCOM = cor(obs_aet, fluxcom, use = "complete.obs")^2,
    P_model = cor(obs_aet, rsofun,  use = "complete.obs")^2,
    .groups = "drop"
  ) |>
  select(-sitename) |>
  pivot_longer(everything(), names_to = "model", values_to = "R2") |>
  drop_na()

model_order <- c("P_model", "PML", "FLUXCOM", "ERA5")

rmse_plot <- ggplot(rmse_long, aes(x = model, y = RMSE)) +
  geom_boxplot(outlier.size = 0.6, fill = "grey92") +
  scale_x_discrete(limits = model_order) +
  labs(
    x = NULL,
    y = expression(paste("RMSE (mm d"^{-1}, ")"))
  ) +
  theme_classic(base_size = 11) +
  theme(aspect.ratio = 1.5)

r2_plot <- ggplot(r2_long, aes(x = model, y = R2)) +
  geom_boxplot(outlier.size = 0.6, fill = "grey92") +
  scale_x_discrete(limits = model_order) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    x = NULL,
    y = expression(R^2)
  ) +
  theme_classic(base_size = 11) +
  theme(aspect.ratio = 1.5)

combined_metrics <- cowplot::plot_grid(
  rmse_plot, r2_plot,
  ncol   = 2,
  labels = letters[1:2]
)

print(combined_metrics)

# =========================================================================
# 4. Save figures  -------------------------------------------------------
# =========================================================================

fig_dir <- here("fig")
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

ggsave(
  filename = file.path(fig_dir, "lateral_profile.svg"),
  plot     = lateral_profile,
  device   = "svg",
  dpi      = 300,
  width    = 8.5,
  height   = 13
)

ggsave(
  filename = file.path(fig_dir, "global_aet_map.svg"),
  plot     = global_merged,
  device   = "svg",
  dpi      = 300,
  width    = 16.5,
  height   = 7
)

ggsave(
  filename = file.path(fig_dir, "global_metrics_aet.svg"),
  plot     = combined_metrics,
  device   = "svg",
  dpi      = 300,
  width    = 16.5,
  height   = 7
)

message("\nFigures saved to ", fig_dir)
