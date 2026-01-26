### ERP penetrologger data
### Plotting script
### J Collins
### 2024-06-22


# plots only depend on saved data, not leftovers in memory
rm(list = ls())
gc()


# ---- Packages ----
source("analyses/01_packages.R")
source(file = "R/analyze_plot_3d_kriging.R")

# ---- Data root (external, not in repo) ----
# Option A: from config file
source("config.R")
# expects: DATA_ROOT <- "path/to/google/drive/..."

# Option B: or environment variable
# DATA_ROOT <- Sys.getenv("PENETRO_DATA")

stopifnot(dir.exists(DATA_ROOT))

# ---- Paths ----
proc_dir <- file.path(DATA_ROOT, "processed")
info_dir <- file.path(DATA_ROOT, "info")

long_dir <- file.path(proc_dir, "long_format_data")
sum_dir  <- file.path(proc_dir, "summary_profiles")
plot_dir <- file.path(DATA_ROOT, "figures")
dir.create(plot_dir, showWarnings = FALSE)


dir.create(plot_dir, showWarnings = FALSE)

# ---- Load data ----
long_data <- read.csv(file.path(long_dir, "long_format_data.csv"))
summary_profiles <- read.csv(file.path(sum_dir, "summary_profiles.csv"))
meta_info <- read.csv(file.path(info_dir, "meta_info.csv"))

local_coords <- read.csv(file.path(info_dir, "local_coords.csv"))

# ---- Basic sanity checks ----
glimpse(long_data)
glimpse(summary_profiles)
glimpse(meta_info)

# Make sure key variables are correct types
long_data <- long_data %>%
  mutate(
    depth = as.numeric(depth),
    penetration_resistance = as.numeric(penetration_resistance),
    treatment = factor(treatment),
    location = factor(location)
  )

summary_profiles <- summary_profiles %>%
  mutate(
    depth = as.numeric(depth),
    mean = as.numeric(mean),
    sem = as.numeric(sem),
    treatment = factor(treatment),
    location = factor(location)
  )

# ---- Global ggplot theme (thesis style) ----
theme_set(
  theme_minimal(base_size = 14) +
    theme(
      panel.grid.minor = element_blank(),
      legend.position = "top"
    )
)





# ---- plots -----

summary_profiles <- long_data %>%
  mutate(
    depth = as.numeric(depth),
    penetration_resistance = as.numeric(penetration_resistance)
  ) %>%
  group_by(id, treatment, location, depth) %>%
  summarise(
    mean  = mean(penetration_resistance, na.rm = TRUE),
    stdev = sd(penetration_resistance, na.rm = TRUE),
    n     = sum(!is.na(penetration_resistance)),
    sem   = stdev / sqrt(n),
    .groups = "drop"
  ) %>%
  arrange(treatment, id, depth)


p_profiles <- ggplot(
  summary_profiles,
  aes(x = mean, y = depth,
      colour = factor(treatment),
      group = interaction(treatment, id))
) +
  geom_errorbarh(
    aes(xmin = mean - sem, xmax = mean + sem),
    height = 0.8,
    alpha = 0.3   # error bars more transparent
  ) +
  geom_path(
    linewidth = 1,
    alpha = 1   # mean lines more visible
  ) +
  scale_y_reverse() +
  facet_wrap(~ location) +
  labs(
    x = "Penetration resistance (MPa)",
    y = "Depth (cm)",
    colour = "Treatment",
    title = "Mean penetration resistance profiles by location"
  ) +
  theme_bw()


p_profiles

ggsave(file.path(plot_dir, "figure_01_mean_profiles.png"),
       plot = p_profiles,
       height = 5, width = 8, dpi = 300)














# ---- add local coordinates ----

long_data <- long_data %>%
  mutate(treatment = as.integer(as.character(treatment)))

local_coords <- local_coords %>%
  mutate(treatment = as.integer(treatment))


long_data <- long_data %>%
  left_join(
    local_coords,
    by = c("treatment", "replicate_number")
  )

long_data %>%
  summarise(
    n_missing_x = sum(is.na(local_x)),
    n_missing_y = sum(is.na(local_y))
  )










plot_id <- 1
id_i <- 26012235

dfp_local <- long_data %>%
  filter(id == id_i, plot_number == plot_id) %>%
  mutate(
    z = -depth
  )


p3d_local <- plot_ly(
  dfp_local,
  x = ~local_x, y = ~local_y, z = ~z,
  color = ~penetration_resistance,
  split = ~replicate_number,
  type = "scatter3d",
  mode = "lines",
  line = list(width = 5),
  text = ~paste(
    "Name:", name,
    "<br>Rep:", replicate_number,
    "<br>Depth:", depth,
    "<br>PR:", penetration_resistance
  )
) %>%
  layout(
    showlegend = FALSE,   # <- THIS removes the line legend
    scene = list(
      xaxis = list(title = "Local X", range = c(0, 500)),
      yaxis = list(title = "Local Y", range = c(0, 500)),
      zaxis = list(title = "Depth (cm)"),
      aspectmode = "manual",
      aspectratio = list(x = 1, y = 1, z = 0.5)
    ),
    title = paste0("3D penetration resistance (local coords) – Plot ", plot_id, " | ID ", id_i)
  )

p3d_local
























# # ---- Spatial interpolation: 3D kriging + stacked 2D surfaces ----
#
# library(dplyr)
# library(sp)
# library(gstat)
# library(akima)
#
# # ---- Parameters ----
# id_i    <- 26012235
# z_scale <- 5
#
# x_range <- c(0, 500)
# y_range <- c(0, 500)
# depth_range <- c(1, 82)
#
# nx <- 30
# ny <- 30
# nz <- 82
#
# cutoff_3d <- 600
# width_3d  <- 30
#
# # ---- Data (one file/id) ----
# dfp <- long_data %>%
#   filter(id == id_i) %>%
#   mutate(
#     depth = as.numeric(depth),
#     penetration_resistance = as.numeric(penetration_resistance),
#     z_scaled = depth * z_scale
#   )
#
# # Convert to SpatialPointsDataFrame (3D coordinates)
# coordinates(dfp) <- ~ local_x + local_y + z_scaled
#
# # ---- 3D variogram + fit ----
# vgm3d <- variogram(
#   penetration_resistance ~ 1,
#   dfp,
#   cutoff = cutoff_3d,
#   width = width_3d
# )
#
# v0 <- var(dfp$penetration_resistance, na.rm = TRUE)
#
# m0 <- vgm(
#   psill  = 0.5 * v0,
#   model  = "Sph",
#   range  = 150,          # tweak based on plot(vgm3d)
#   nugget = 0.1 * v0
# )
#
# fit <- fit.variogram(vgm3d, model = m0)
#
# # Optional diagnostics
# plot(vgm3d, main = "3D empirical variogram")
# plot(vgm3d, fit, main = "3D variogram with fitted model")
#
# # ---- 3D prediction grid (IMPORTANT: use z_scaled) ----
# xg <- seq(x_range[1], x_range[2], length.out = nx)
# yg <- seq(y_range[1], y_range[2], length.out = ny)
# zg <- seq(depth_range[1], depth_range[2], length.out = nz)
#
# grid <- expand.grid(
#   local_x  = xg,
#   local_y  = yg,
#   z_scaled = zg * z_scale
# )
#
# coordinates(grid) <- ~ local_x + local_y + z_scaled
#
# kriged_3d <- krige(
#   penetration_resistance ~ 1,
#   dfp,
#   newdata = grid,
#   model = fit
# )
#
# # kriged_3d is a SpatialPointsDataFrame with var1.pred and var1.var
# # head(kriged_3d@data)
#
# # ---- Stacked 2D interpolation (per depth) ----
# make_depth_surface <- function(depth_i, n = 50) {
#   df_d <- long_data %>%
#     filter(id == id_i, depth == depth_i) %>%
#     mutate(
#       local_x = as.numeric(local_x),
#       local_y = as.numeric(local_y),
#       penetration_resistance = as.numeric(penetration_resistance)
#     )
#
#   akima::interp(
#     x = df_d$local_x,
#     y = df_d$local_y,
#     z = df_d$penetration_resistance,
#     xo = seq(x_range[1], x_range[2], length.out = n),
#     yo = seq(y_range[1], y_range[2], length.out = n),
#     linear = TRUE
#   )
# }
#
# cube_2d <- lapply(seq(depth_range[1], depth_range[2]), make_depth_surface)
#
#
#
# depths <- c(10, 20, 30, 40, 60, 70, 80)
#
# slice_multi <- as.data.frame(kriged_3d) %>%
#   mutate(depth = round(z_scaled / z_scale)) %>%
#   filter(depth %in% depths)
#
# library(viridis)
#
# ggplot(slice_multi,
#        aes(local_x, local_y, fill = var1.pred)) +
#   geom_raster() +
#   coord_equal(xlim = c(0, 500), ylim = c(0, 500), expand = FALSE) +
#   facet_wrap(~ depth, ncol = 3) +
#   scale_fill_viridis_c(option = "inferno", name = "PR (MPa)") +
#   labs(
#     title = "3D kriged penetration resistance slices",
#     x = "Local X",
#     y = "Local Y"
#   ) +
#   theme_minimal() +
#   facet_wrap(
#     ~ depth,
#     ncol = 3,
#     labeller = labeller(
#       depth = function(x) paste0("Depth = ", x, " cm")
#     )
#   ) +
#   theme(
#     strip.text = element_text(face = "bold"),
#     strip.background = element_rect(fill = "grey95", colour = NA)
#   )







# depths <- c(10, 20, 30, 40, 60)
#
# slice_unc_multi <- as.data.frame(kriged_3d) %>%
#   mutate(depth = round(z_scaled / z_scale)) %>%
#   filter(depth %in% depths)
#
#
# slice_unc_multi <- slice_unc_multi %>%
#   mutate(kriging_se = sqrt(var1.var))
#
# ggplot(slice_unc_multi,
#        aes(local_x, local_y, fill = kriging_se)) +
#   geom_raster() +
#   coord_equal(xlim = c(0, 500), ylim = c(0, 500), expand = FALSE) +
#   facet_wrap(~ depth,
#              ncol = 3,
#              labeller = labeller(depth = function(x) paste0(x, " cm"))) +
#   scale_fill_viridis_c(option = "magma", name = "Kriging SE (MPa)") +
#   labs(
#     title = "Standard error of 3D kriged penetration resistance",
#     x = "Local X",
#     y = "Local Y"
#   ) +
#   theme_minimal() +
#   theme(strip.text = element_text(face = "bold"))






# try function

pr_limits <- quantile(
  long_data$penetration_resistance,
  c(0.02, 0.98),
  na.rm = TRUE
)

pr_limits <- c(0, pr_limits[2])  # force min = 0


res <- analyze_plot_3d_kriging(long_data, id_i = 26012232,
                               save_dir = file.path(DATA_ROOT, "figures"), pr_limits = pr_limits)
res$pred_plot
res$se_plot







# force the legend scales to the same for all

ids <- sort(unique(long_data$id))

pr_limits <- c(0, max(long_data$penetration_resistance, na.rm = TRUE))
se_limits <- c(0, max(sqrt(res$kriged_3d$var1.var), na.rm = TRUE))

results <- lapply(ids, function(i) {
  analyze_plot_3d_kriging(
    long_data,
    id_i = i,
    pr_limits = pr_limits,
    save_dir = file.path(DATA_ROOT, "figures")
  )
})







