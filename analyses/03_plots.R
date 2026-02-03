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
ID_I <- 26020300
# ID_I <- c(26020200, 26020201, 26020202)   # <- multiple IDs


DEPTHS <- c(10, 20, 30, 40, 50, 60, 70, 80)

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

glimpse()


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





# # ---- Krige + save outputs ----
#
# bbox <- make_bbox(
#   long_ready,
#   buffer = BUFFER,
#   grid_step = GRID_STEP,
#   hectare_min = HECTARE_MIN,
#   hectare_max = HECTARE_MAX
# )
#
# res <- analyze_plot_3d_kriging(
#   long_data = long_ready,
#   id_i = ID_I,
#   coord_sys = "field",
#   x_range = bbox$x_range,
#   y_range = bbox$y_range,
#   depth_range = c(min(DEPTHS), max(DEPTHS)),
#   depths_pred = DEPTHS,
#   depths_se   = DEPTHS,
#   nx = bbox$nx,
#   ny = bbox$ny,
#   cutoff_3d = 1800,
#   width_3d  = 70,
#   safe_mode = FALSE,
#   z_scale = 5,
#   vgm_model = "Sph",
#   vgm_range = 350
# )
#
# # add hectare grid overlay & save plots
# pred_plot <- add_square_axes(res$pred_plot, flip_rows = FLIP_ROWS)
# se_plot   <- add_square_axes(res$se_plot,   flip_rows = FLIP_ROWS)
#
# ggsave(file.path(plot_dir, paste0("kriged_grid_id_", ID_I, ".png")),
#        plot = pred_plot, height = 6, width = 10, dpi = 300)
#
# ggsave(file.path(plot_dir, paste0("kriged_grid_se_id_", ID_I, ".png")),
#        plot = se_plot, height = 6, width = 10, dpi = 300)
#
# # tidy kriged output
# kr_out <- as.data.frame(res$kriged_3d) %>%
#   transmute(
#     id = ID_I,
#     field_x = .data[[res$x_col]],
#     field_y = .data[[res$y_col]],
#     depth = z_scaled / res$z_scale_used,
#     pr_pred = var1.pred,
#     kriging_se = sqrt(pmax(var1.var, 0))
#   ) %>%
#   filter(depth %in% DEPTHS)
#
# write.csv(
#   kr_out,
#   file.path(krig_dir, paste0("kriged_field_id_", ID_I, ".csv")),
#   row.names = FALSE
# )
#
# # quick sanity
# range(kr_out$field_x); range(kr_out$field_y)
#

















