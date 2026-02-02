### ERP penetrologger data
### Plotting script
### J Collins

rm(list = ls()); gc()

# ---- Packages and functions ----
source("analyses/01_packages.R")
source("config.R")

source("R/analyze_plot_3d_kriging.R")
source("R/prep_long_for_kriging.R")
source("R/add_square_axes.R")

library(dplyr)
library(ggplot2)
library(plotly)

# ---- Config ----
ID_I <- 26020200

DEPTHS <- c(10, 20, 30, 40, 60, 70, 80)

SQUARE_SIZE <- 500     # cm
N_ROWS <- 20
FLIP_ROWS <- FALSE     # IMPORTANT: keep consistent with your field convention

GRID_STEP <- 100       # cm (1 m pixels)
BUFFER <- 250          # cm around points

HECTARE_MIN <- 0
HECTARE_MAX <- N_ROWS * SQUARE_SIZE  # 10000 cm




# ---- Helpers ----

snap_down <- function(v, step) floor(v / step) * step
snap_up   <- function(v, step) ceiling(v / step) * step

make_bbox <- function(df, x = "field_x", y = "field_y",
                      buffer = 250, grid_step = 100,
                      hectare_min = 0, hectare_max = 10000) {

  xr <- range(df[[x]], na.rm = TRUE) + c(-buffer, buffer)
  yr <- range(df[[y]], na.rm = TRUE) + c(-buffer, buffer)

  x_range <- c(
    max(hectare_min, snap_down(xr[1], grid_step)),
    min(hectare_max, snap_up(xr[2], grid_step))
  )

  y_range <- c(
    max(hectare_min, snap_down(yr[1], grid_step)),
    min(hectare_max, snap_up(yr[2], grid_step))
  )

  nx <- as.integer((x_range[2] - x_range[1]) / grid_step) + 1L
  ny <- as.integer((y_range[2] - y_range[1]) / grid_step) + 1L

  list(x_range = x_range, y_range = y_range, nx = nx, ny = ny)
}





# ---- Paths ----
proc_dir <- file.path(DATA_ROOT, "processed")
info_dir <- file.path(DATA_ROOT, "info")
plot_dir <- file.path(DATA_ROOT, "figures")
dir.create(plot_dir, showWarnings = FALSE)

long_dir <- file.path(proc_dir, "long_format_data")
sum_dir  <- file.path(proc_dir, "summary_profiles")
krig_dir <- file.path(proc_dir, "kriging_data")
dir.create(krig_dir, recursive = TRUE, showWarnings = FALSE)

# ---- Load ----
long_data <- read.csv(file.path(long_dir, "long_format_data.csv"))
local_coords <- read.csv(file.path(info_dir, "local_coords2.csv"))

# ---- Type cleanup (only what you actually use downstream) ----
long_data <- long_data %>%
  mutate(
    id = as.integer(id),
    depth = as.numeric(depth),
    penetration_resistance = as.numeric(penetration_resistance),
    plot_number = as.integer(plot_number),
    replicate_number = as.integer(replicate_number),
    treatment = factor(treatment),
    location = factor(location)
  )

local_coords <- local_coords %>%
  mutate(
    id = as.integer(id),
    plot = as.integer(plot),
    replicate_number = as.integer(replicate_number),
    square_number = as.integer(square_number),
    square_letter = toupper(as.character(square_letter)),
    local_x = as.numeric(local_x),
    local_y = as.numeric(local_y)
  )






# ---- Profile plots ----

plot_mean_profiles <- function(long_df, id_i) {
  summary_profiles <- long_df %>%
    filter(id == id_i) %>%
    group_by(id, treatment, location, depth) %>%
    summarise(
      mean  = mean(penetration_resistance, na.rm = TRUE),
      stdev = sd(penetration_resistance, na.rm = TRUE),
      n     = sum(!is.na(penetration_resistance)),
      sem   = stdev / sqrt(n),
      .groups = "drop"
    )

  ggplot(summary_profiles,
         aes(x = mean, y = depth, colour = treatment, group = interaction(treatment, id))) +
    geom_errorbarh(aes(xmin = mean - sem, xmax = mean + sem),
                   height = 0.8, alpha = 0.3) +
    geom_path(linewidth = 1) +
    scale_y_reverse() +
    facet_wrap(~ location) +
    labs(
      x = "Penetration resistance (MPa)",
      y = "Depth (cm)",
      colour = "Treatment",
      title = paste("Mean penetration resistance profiles | id", id_i)
    ) +
    theme_bw()
}

p_profiles <- plot_mean_profiles(long_data, ID_I)
p_profiles







# --- Interactive 3D plot ----


long_ready <- prep_long_for_kriging(
  long_data, local_coords, id_i = ID_I,
  square_size = SQUARE_SIZE,
  n_rows = N_ROWS,
  flip_rows = FLIP_ROWS
)

stopifnot(all(c("local_x","local_y","field_x","field_y") %in% names(long_ready)))

df_all <- long_ready %>%
  mutate(
    z = -depth,
    profile_id = interaction(plot_number, replicate_number, square_letter, square_number, name, drop = TRUE)
  ) %>%
  arrange(profile_id, depth)

p3d <- plot_ly(
  df_all,
  x = ~field_x, y = ~field_y, z = ~z,
  color = ~penetration_resistance,
  split = ~profile_id,
  type = "scatter3d",
  mode = "lines",
  line = list(width = 3),
  text = ~paste(
    "Plot:", plot_number,
    "<br>Square:", square_number, square_letter,
    "<br>Pen:", replicate_number,
    "<br>Depth:", depth,
    "<br>PR:", penetration_resistance
  )
) %>%
  layout(
    showlegend = FALSE,
    scene = list(
      xaxis = list(title = "Field X (cm)"),
      yaxis = list(title = "Field Y (cm)"),
      zaxis = list(title = "Depth (cm)"),
      aspectmode = "data"
    )
  )

p3d





# ---- Krige + save outputs ----

bbox <- make_bbox(
  long_ready,
  buffer = BUFFER,
  grid_step = GRID_STEP,
  hectare_min = HECTARE_MIN,
  hectare_max = HECTARE_MAX
)

res <- analyze_plot_3d_kriging(
  long_data = long_ready,
  id_i = ID_I,
  coord_sys = "field",
  x_range = bbox$x_range,
  y_range = bbox$y_range,
  depth_range = c(min(DEPTHS), max(DEPTHS)),
  depths_pred = DEPTHS,
  depths_se   = DEPTHS,
  nx = bbox$nx,
  ny = bbox$ny,
  cutoff_3d = 1800,
  width_3d  = 70,
  safe_mode = FALSE,
  z_scale = 5,
  vgm_model = "Sph",
  vgm_range = 350
)

# add hectare grid overlay & save plots
pred_plot <- add_square_axes(res$pred_plot, flip_rows = FLIP_ROWS)
se_plot   <- add_square_axes(res$se_plot,   flip_rows = FLIP_ROWS)

ggsave(file.path(plot_dir, paste0("kriged_grid_id_", ID_I, ".png")),
       plot = pred_plot, height = 6, width = 10, dpi = 300)

ggsave(file.path(plot_dir, paste0("kriged_grid_se_id_", ID_I, ".png")),
       plot = se_plot, height = 6, width = 10, dpi = 300)

# tidy kriged output
kr_out <- as.data.frame(res$kriged_3d) %>%
  transmute(
    id = ID_I,
    field_x = .data[[res$x_col]],
    field_y = .data[[res$y_col]],
    depth = z_scaled / res$z_scale_used,
    pr_pred = var1.pred,
    kriging_se = sqrt(pmax(var1.var, 0))
  ) %>%
  filter(depth %in% DEPTHS)

write.csv(
  kr_out,
  file.path(krig_dir, paste0("kriged_field_id_", ID_I, ".csv")),
  row.names = FALSE
)

# quick sanity
range(kr_out$field_x); range(kr_out$field_y)
















# ### ERP penetrologger data
# ### Plotting script
# ### J Collins
# ### 2024-06-22
#
#
# # plots only depend on saved data, not leftovers in memory
# rm(list = ls())
# gc()
#
#
# # ---- Packages and functions ----
# source("analyses/01_packages.R")
# source(file = "R/analyze_plot_3d_kriging.R")
# source("R/prep_long_for_kriging.R")
# source(file = "R/add_square_axes.R")
# library(dplyr)
# library(stringr)
# library(plotly)
#
# # ---- Data root (external, not in repo) ----
# # Option A: from config file
# source("config.R")
# # expects: DATA_ROOT <- "path/to/google/drive/..."
#
# # Option B: or environment variable
# # DATA_ROOT <- Sys.getenv("PENETRO_DATA")
#
# stopifnot(dir.exists(DATA_ROOT))
#
# # ---- Paths ----
# proc_dir <- file.path(DATA_ROOT, "processed")
# info_dir <- file.path(DATA_ROOT, "info")
#
# long_dir <- file.path(proc_dir, "long_format_data")
# sum_dir  <- file.path(proc_dir, "summary_profiles")
# plot_dir <- file.path(DATA_ROOT, "figures")
# dir.create(plot_dir, showWarnings = FALSE)
#
#
# dir.create(plot_dir, showWarnings = FALSE)
#
# # ---- Load data ----
# long_data <- read.csv(file.path(long_dir, "long_format_data.csv"))
# summary_profiles <- read.csv(file.path(sum_dir, "summary_profiles.csv"))
# meta_info <- read.csv(file.path(info_dir, "meta_info.csv"))
#
# local_coords <- read.csv(file.path(info_dir, "local_coords2.csv"))
#
#
#
# # ---- Basic sanity checks ----
# glimpse(long_data)
# glimpse(summary_profiles)
# glimpse(meta_info)
# glimpse(local_coords)
#
# # Make sure key variables are correct types
# long_data <- long_data %>%
#   mutate(
#     depth = as.numeric(depth),
#     penetration_resistance = as.numeric(penetration_resistance),
#     treatment = factor(treatment),
#     location = factor(location),
#     id = as.integer(id),
#     plot_number = as.integer(plot_number),           # "001" -> 1
#     replicate_number = as.integer(replicate_number)  # "1" -> 1
#   )
#
# local_coords <- local_coords %>%
#   mutate(
#     id = as.integer(id),
#     plot = as.integer(plot),
#     replicate_number = as.integer(replicate_number)
#   )
#
# summary_profiles <- summary_profiles %>%
#   mutate(
#     depth = as.numeric(depth),
#     mean = as.numeric(mean),
#     sem = as.numeric(sem),
#     treatment = factor(treatment),
#     location = factor(location)
#   )
#
# # ---- Global ggplot theme (thesis style) ----
# theme_set(
#   theme_minimal(base_size = 14) +
#     theme(
#       panel.grid.minor = element_blank(),
#       legend.position = "top"
#     )
# )
#
#
#
#
# glimpse(long_data)
#
#
#
#
#
# # ---- select ID -----
#
# long_data_ids <- long_data %>%
#   filter(id %in% c(26013000))
#
#
#
#
# # ---- plots -----
#
# summary_profiles <- long_data_ids %>%
#   mutate(
#     depth = as.numeric(depth),
#     penetration_resistance = as.numeric(penetration_resistance)
#   ) %>%
#   group_by(id, treatment, location, depth) %>%
#   summarise(
#     mean  = mean(penetration_resistance, na.rm = TRUE),
#     stdev = sd(penetration_resistance, na.rm = TRUE),
#     n     = sum(!is.na(penetration_resistance)),
#     sem   = stdev / sqrt(n),
#     .groups = "drop"
#   ) %>%
#   arrange(treatment, id, depth)
#
#
# p_profiles <- ggplot(
#   summary_profiles,
#   aes(x = mean, y = depth,
#       colour = factor(treatment),
#       group = interaction(treatment, id))
# ) +
#   geom_errorbarh(
#     aes(xmin = mean - sem, xmax = mean + sem),
#     height = 0.8,
#     alpha = 0.3   # error bars more transparent
#   ) +
#   geom_path(
#     linewidth = 1,
#     alpha = 1   # mean lines more visible
#   ) +
#   scale_y_reverse() +
#   facet_wrap(~ location) +
#   labs(
#     x = "Penetration resistance (MPa)",
#     y = "Depth (cm)",
#     colour = "Treatment",
#     title = "Mean penetration resistance profiles by location"
#   ) +
#   theme_bw()
#
#
# p_profiles
#
# # ggsave(file.path(plot_dir, "figure_01_mean_profiles.png"),
# #        plot = p_profiles,
# #        height = 5, width = 8, dpi = 300)
#
#
#
#
#
#
#
#
#
#
#
#
#
#
# # ---- add local coordinates ----
#
# long_data_ids2 <- long_data %>%
#   filter(id == 26013000) %>%
#   left_join(
#     local_coords %>%
#       select(id, plot, replicate_number, local_x, local_y, square_number, square_letter),
#     by = c("id" = "id",
#            "plot_number" = "plot",
#            "replicate_number" = "replicate_number")
#   )
#
#
# long_data_ids2 %>%
#   summarise(
#     n_missing_x = sum(is.na(local_x)),
#     n_missing_y = sum(is.na(local_y))
#   )
#
#
#
#
# SQUARE_SIZE <- 500  # cm
# origin_x <- 0
# origin_y <- 0
# N_ROWS <- 20
# FLIP_ROWS <- TRUE
#
# long_data_ids2 <- long_data_ids2 %>%
#   mutate(
#     square_letter = toupper(square_letter),
#     square_row = as.integer(square_number),
#
#     # Guardrails: should be hectare letters A..T and rows 1..20
#     # (put outside mutate if you prefer; shown here for clarity)
#     square_col = match(square_letter, LETTERS),   # global A=1 ... Z=26
#     square_row_use = if (FLIP_ROWS) (N_ROWS - square_row + 1) else square_row,
#
#     field_x = origin_x + (square_col - 1) * SQUARE_SIZE + local_x,
#     field_y = origin_y + (square_row_use - 1) * SQUARE_SIZE + local_y,
#
#     z = -depth,
#     profile_id = interaction(plot_number, replicate_number, square_letter, square_number, name, drop = TRUE)
#   ) %>%
#   arrange(profile_id, depth)
#
# # hard-stop if something is off (recommended)
# stopifnot(all(long_data_ids2$square_letter %in% LETTERS[1:20]))
# stopifnot(all(long_data_ids2$square_number %in% 1:20))
#
#
#
#
#
#
#
#
#
# # sanity check
# long_data_ids2 %>%
#   summarise(
#     n_missing_field_x = sum(is.na(field_x)),
#     n_missing_field_y = sum(is.na(field_y))
#   )
#
#
# # check ids are truly only one
# long_data_ids2 %>% count(id)
#
# # detect if join duplicated rows
# nrow(long_data_ids)
# nrow(long_data_ids2)
#
#
#
#
#
#
#
#
# # Plot interactive ####
#
# df_all <- long_data_ids2 %>%
#   filter(id == 26013000) %>%
#   mutate(
#     z = -depth,
#     # pick ONE of these as your profile id (most robust is square + name)
#     profile_id = interaction(plot_number, replicate_number, square_letter, square_number, name, drop = TRUE)
#   ) %>%
#   arrange(profile_id, depth)   # IMPORTANT: ensures lines connect in depth order
#
# plot_ly(
#   long_data_ids2,
#   x = ~field_x, y = ~field_y, z = ~z,
#   color = ~penetration_resistance,
#   split = ~profile_id,
#   type = "scatter3d",
#   mode = "lines",
#   line = list(width = 3),
#   text = ~paste(
#     "Plot:", plot_number,
#     "<br>Square:", square_number, square_letter,
#     "<br>Pen:", replicate_number,
#     "<br>Depth:", depth,
#     "<br>PR:", penetration_resistance
#   )
# ) %>%
#   layout(
#     showlegend = FALSE,
#     scene = list(
#       xaxis = list(title = "Field X (cm)"),
#       yaxis = list(title = "Field Y (cm)"),
#       zaxis = list(title = "Depth (cm)"),
#       aspectmode = "data"
#     )
#   )
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
# # # Plot interpolated slices ####
#
# set.seed(1)
# id_i <- 26013000
# SQUARE_SIZE <- 500
# N_ROWS <- 20
# FLIP_ROWS <- TRUE
# letter_levels <- LETTERS[1:20]   # A:T global
#
#
#
# long_ready <- prep_long_for_kriging(long_data, local_coords, id_i = 26013000,
#                                     square_size = 500, n_rows = 20, flip_rows = FALSE)
#
# pts <- long_ready %>% distinct(square_letter, square_number, field_x, field_y)
#
# ggplot(pts, aes(field_x, field_y, label = paste0(square_letter, square_number))) +
#   geom_point() +
#   geom_text(nudge_y = 80, size = 3) +
#   coord_equal() +
#   theme_bw()
#
# stopifnot(all(c("local_x","local_y","field_x","field_y") %in% names(long_ready)))
#
#
#
#
# # ---- choose a global grid step (cm) ----
# # 100 cm = 1 m pixels (fast), 50 cm = 0.5 m (sharper but slower)
# GRID_STEP <- 100
#
# # ---- choose a buffer around points (cm) to reduce edge artifacts ----
# BUFFER <- 250  # half a square; try 250–500
#
# # ---- block bounding box ----
# xr <- range(long_ready$field_x, na.rm = TRUE) + c(-BUFFER, BUFFER)
# yr <- range(long_ready$field_y, na.rm = TRUE) + c(-BUFFER, BUFFER)
#
# # ---- snap bbox to the global lattice so all blocks align ----
# snap_down <- function(v, step) floor(v / step) * step
# snap_up   <- function(v, step) ceiling(v / step) * step
#
# HECTARE_MIN <- 0
# HECTARE_MAX <- 20 * SQUARE_SIZE  # 10000 cm
#
# x_range <- c(
#   max(HECTARE_MIN, snap_down(xr[1], GRID_STEP)),
#   min(HECTARE_MAX, snap_up(xr[2], GRID_STEP))
# )
#
# y_range <- c(
#   max(HECTARE_MIN, snap_down(yr[1], GRID_STEP)),
#   min(HECTARE_MAX, snap_up(yr[2], GRID_STEP))
# )
#
#
# # ---- convert ranges + step into nx/ny ----
# nx <- as.integer((x_range[2] - x_range[1]) / GRID_STEP) + 1L
# ny <- as.integer((y_range[2] - y_range[1]) / GRID_STEP) + 1L
#
# res <- analyze_plot_3d_kriging(
#   long_data = long_ready,
#   id_i = id_i,
#   coord_sys = "field",
#
#   x_range = x_range,
#   y_range = y_range,
#   depth_range = c(10, 80),
#
#   depths_pred = c(10, 20, 30, 40, 60, 70, 80),
#   depths_se   = c(10, 20, 30, 40, 60, 70, 80),
#
#   nx = nx, ny = ny,
#
#   # variogram settings (keep yours, but see note below)
#   cutoff_3d = 1800,
#   width_3d  = 70,
#
#   safe_mode = FALSE,
#   z_scale = 5,
#   vgm_model = "Sph",
#   vgm_range = 350
# )
#
# res$pred_plot
# res$se_plot
# res$fit
#
#
# names(res$kriged_3d)
# head(as.data.frame(res$kriged_3d))
#
#
#
# ## add the square to the plot ####
#
# add_square_axes(res$pred_plot, flip_rows = FALSE)
#
# ggsave(file.path(plot_dir, paste0("kriged_grid", id_i, ".png")),
#        height = 6, width = 10, dpi = 300)
#
# add_square_axes(res$se_plot,   flip_rows = FALSE)
#
# ggsave(file.path(plot_dir, paste0("kriged_grid_SE", id_i, ".png")),
#        height = 6, width = 10, dpi = 300)
#
#
# kriged_df <- as.data.frame(res$kriged_3d) %>%
#   dplyr::transmute(
#     id = id_i,
#     field_x = .data[[res$x_col]],
#     field_y = .data[[res$y_col]],
#     depth = z_scaled / res$z_scale_used,
#     pr_pred = var1.pred,
#     kriging_se = sqrt(pmax(var1.var, 0))
#   )
#
# range(kriged_df$field_x); range(kriged_df$field_y)
# res$x_col; res$y_col
#
# krig_dir <- file.path(proc_dir, "kriging_data")
#
# # make sure directories exist
# dir.create(krig_dir, recursive = TRUE, showWarnings = FALSE)
#
# kr_out <- as.data.frame(res$kriged_3d) %>%
#   dplyr::transmute(
#     id = id_i,
#     field_x = .data[[res$x_col]],
#     field_y = .data[[res$y_col]],
#     depth = z_scaled / res$z_scale_used,
#     pr_pred = var1.pred,
#     kriging_se = sqrt(pmax(var1.var, 0))
#   ) %>%
#   dplyr::filter(depth %in% c(10, 20, 30, 40, 60, 70, 80))
#
# glimpse(kr_out)
#
# write.csv(
#   kr_out,
#   file.path(krig_dir, paste0("kriged_field_id_", id_i, ".csv")),
#   row.names = FALSE
# )






# # PLOT ALL KRIGE ####
#
#
# P_T_1_5_krige <- read.csv(
#   file.path(krig_dir, "kriged_field_id_26012900.csv")
# )
#
#
# K_O_6_10_krige <- read.csv(
#   file.path(krig_dir, "kriged_field_id_26013000.csv")
# )
#
# glimpse(P_T_1_5_krige)
#
# glimpse(K_O_6_10_krige)
#
#
#
#
# P_T_1_5_krige <- P_T_1_5_krige %>%
#   mutate(treatment = "P:T_1:5")
#
# K_O_6_10_krige <- K_O_6_10_krige %>%
#   mutate(treatment = "K:O_6:10")
#
# krige_all <- dplyr::bind_rows(P_T_1_5_krige, K_O_6_10_krige)
#
#
# krige_all %>%
#   count(treatment, depth)
#
# library(ggplot2)
#
# ggplot(
#   krige_all %>% filter(depth == 10),
#   aes(field_x, field_y, fill = pr_pred)
# ) +
#   geom_raster() +
#   coord_equal(expand = FALSE) +
#   facet_wrap(~ treatment) +
#   scale_fill_viridis_c(name = "PR (MPa)") +
#   labs(
#     title = "Kriged penetration resistance (Depth = 10 cm)",
#     x = "Field X (cm)",
#     y = "Field Y (cm)"
#   ) +
#   theme_bw()
#
#
#
#
#
# library(dplyr)
# library(ggplot2)
#
# d1 <- P_T_1_5_krige %>% filter(depth == 10)
# d2 <- K_O_6_10_krige %>% filter(depth == 10)
#
# dx1 <- min(diff(sort(unique(d1$field_x))), na.rm = TRUE)
# dy1 <- min(diff(sort(unique(d1$field_y))), na.rm = TRUE)
#
# dx2 <- min(diff(sort(unique(d2$field_x))), na.rm = TRUE)
# dy2 <- min(diff(sort(unique(d2$field_y))), na.rm = TRUE)
#
# ggplot() +
#   geom_tile(
#     data = d1,
#     aes(field_x, field_y, fill = pr_pred),
#     width = dx1, height = dy1
#   ) +
#   geom_tile(
#     data = d2,
#     aes(field_x, field_y, fill = pr_pred),
#     width = dx2, height = dy2
#   ) +
#   coord_equal(expand = FALSE) +
#   scale_fill_viridis_c(name = "PR (MPa)") +
#   labs(
#     title = "Kriged penetration resistance (Depth = 10 cm)",
#     subtitle = "Multiple survey days plotted in a single 1 ha field",
#     x = "Field X (cm)",
#     y = "Field Y (cm)"
#   ) +
#   theme_bw()
#
#
#
#
#
#
#
#
#
#
#
#


















