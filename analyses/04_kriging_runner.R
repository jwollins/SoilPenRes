### ERP penetrologger data — Daily kriging runner
### Sets up environment, loads data, sources functions, and runs run_id()
### J Collins

rm(list = ls()); gc()

# ---- Packages + project functions ----
source("analyses/01_packages.R")      # your package loader
source("config.R")                   # defines DATA_ROOT
library(readr)   # faster + safer than read.csv

# Core functions
source("R/analyze_plot_3d_kriging.R")
source("R/prep_long_for_kriging.R")
source("R/add_square_axes.R")
source("R/plot_orthogonal_sections_5m.R")
source("R/plot_orthogonal_sections.R")

# If you saved run_id() in its own file:
source("R/run_id.R")

library(dplyr)
library(ggplot2)
library(purrr)

stopifnot(dir.exists(DATA_ROOT))

# ---- Paths ----
proc_dir <- file.path(DATA_ROOT, "processed")
info_dir <- file.path(DATA_ROOT, "info")

long_dir <- file.path(proc_dir, "long_format_data")
plot_dir <- file.path(DATA_ROOT, "figures")
krig_dir <- file.path(proc_dir, "kriging_data")

dir.create(plot_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(krig_dir, showWarnings = FALSE, recursive = TRUE)

# ---- Load data ----
long_data <- read.csv(file.path(long_dir, "long_format_data.csv"))
local_coords <- read.csv(file.path(info_dir, "local_coords2.csv"))

# ---- Type cleanup (keep this minimal & stable) ----
long_data <- long_data %>%
  mutate(
    id = as.integer(id),
    depth = as.numeric(depth),
    penetration_resistance = as.numeric(penetration_resistance),
    plot_number = as.integer(plot_number),
    replicate_number = as.integer(replicate_number)
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

# ---- Run settings (edit these) ----
DEPTHS <- c(10, 20, 30, 40, 50, 60, 70, 80)

SQUARE_SIZE <- 500
N_ROWS <- 20
FLIP_ROWS <- FALSE          # keep consistent with your field convention

GRID_STEP <- 100            # cm
BUFFER <- 250               # cm

# Option A: run a single ID
ids_to_run <- c(26020200)

# Option B: run all IDs present in long_data (uncomment)
# ids_to_run <- sort(unique(long_data$id))

# Option C: run only IDs not already kriged (uncomment)
# existing <- list.files(krig_dir, pattern = "^kriged_field_id_\\d+\\.csv$", full.names = FALSE)
# done_ids <- as.integer(gsub("^kriged_field_id_(\\d+)\\.csv$", "\\1", existing))
# ids_to_run <- setdiff(sort(unique(long_data$id)), done_ids)

# ---- Run ----
results <- purrr::map(
  ids_to_run,
  run_id,
  long_data = long_data,
  local_coords = local_coords,
  out_dir = krig_dir,
  plot_dir = plot_dir,
  square_size = SQUARE_SIZE,
  n_rows = N_ROWS,
  flip_rows = FLIP_ROWS,
  depths = DEPTHS,
  grid_step = GRID_STEP,
  buffer = BUFFER,
  overwrite = TRUE,   # set TRUE to force re-run
  quiet = FALSE
)


# ---- Quick summary ----
# results is a list of small status lists (id, ok, files, etc.)
print(results)

# Optional: write a simple run log
log_df <- purrr::map_dfr(results, function(x) {
  data.frame(
    id = x$id,
    ok = isTRUE(x$ok),
    skipped = isTRUE(x$skipped),
    n_pred = if (!is.null(x$n_pred)) x$n_pred else NA_integer_,
    csv = if (!is.null(x$files["csv"])) x$files["csv"] else NA_character_,
    pred_png = if (!is.null(x$files["pred"])) x$files["pred"] else NA_character_,
    se_png = if (!is.null(x$files["se"])) x$files["se"] else NA_character_
  )
})

write.csv(log_df, file.path(krig_dir, "kriging_run_log.csv"), row.names = FALSE)





# ---- Plot all Krige DF's ----

krig_files <- list.files(
  krig_dir,
  pattern = "^kriged_field_id_\\d+\\.csv$",
  full.names = TRUE
)

krige_all <- purrr::map_dfr(krig_files, readr::read_csv, show_col_types = FALSE)

glimpse(krige_all)
krige_all %>% count(id)


ids <- c(26012900, 26013000, 26020200)
krige_all <- krige_all %>% filter(id %in% ids)


krige_best <- krige_all %>%
  group_by(field_x, field_y, depth) %>%
  slice_min(order_by = kriging_se, n = 1, with_ties = FALSE) %>%
  ungroup()


depth_i <- 10

p <- ggplot(
  krige_best %>% filter(depth == depth_i),
  aes(field_x, field_y, fill = pr_pred)
) +
  geom_tile() +
  coord_equal(expand = FALSE) +
  scale_fill_viridis_c(
    option = "inferno",
    name = "PR (MPa)"
  ) +
  theme_bw()

p






library(scales)



depths_use <- c(10, 20, 30, 40, 50, 60, 70, 80)
df_plot <- krige_best %>% filter(depth %in% depths_use)

lims <- range(df_plot$pr_pred, na.rm = TRUE)

upper <- ceiling(lims[2] * 10) / 10  # round up to 0.1 MPa

x_breaks <- seq(
  floor(min(df_plot$field_x) / 500) * 500,
  ceiling(max(df_plot$field_x) / 500) * 500,
  by = 1000
)

y_breaks <- seq(
  floor(min(df_plot$field_y) / 500) * 500,
  ceiling(max(df_plot$field_y) / 500) * 500,
  by = 1000
)

lab_m <- function(x) x / 100




p_depths <- ggplot(df_plot, aes(field_x, field_y, fill = pr_pred)) +
  geom_tile() +
  coord_equal(expand = FALSE) +
  facet_wrap(
    ~ depth,
    ncol = 4,
    labeller = labeller(depth = function(d) paste0(d, " cm"))
  ) +
  scale_fill_viridis_c(
    option = "inferno",
    limits = c(0, upper),
    oob = scales::squish,
    name = "PR (MPa)"
  ) +
  scale_x_continuous(
    breaks = x_breaks,
    labels = lab_m
  ) +
  scale_y_continuous(
    breaks = y_breaks,
    labels = lab_m
  ) +
  labs(
    title = "Kriged penetration resistance",
    subtitle = "Depth slices across sampled blocks in the 1 ha field",
    x = "Field X position (m)",
    y = "Field Y position (m)"
  ) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_text(size = 8),
    axis.title = element_text(face = "bold"),
    strip.background = element_rect(fill = "grey95", colour = NA),
    strip.text = element_text(face = "bold"),
    panel.border = element_rect(colour = "grey30", fill = NA, linewidth = 0.3),
    legend.position = "right"
  )


p_depths



ggsave(filename = paste0(plot_dir, "kriged_hectare.png"),
       plot = p_depths, dpi = 300, width = 10, height = 6)


p_depths_sq <- add_square_axes(
  p_depths,
  square_size = 500,
  origin_x = 0,
  origin_y = 0,
  n_cols = 20,
  n_rows = 20,
  flip_rows = FALSE   # IMPORTANT: match how you computed field_y
)

p_depths_sq

ggsave(filename = paste0(plot_dir, "kriged_hectare_grid.png"),
       plot = p_depths_sq, dpi = 300, width = 10, height = 6)




# p_depths %>% add_square_axes(flip_rows = FLIP_ROWS)






# ---- Orthogonal cross-sections ----

plot_orthogonal_sections(
  krige_best,
  x0 = 4250,
  y0 = 6250
)

ggsave(filename = paste0(plot_dir, "orthogonal_sections.png"))




# ---- 5 m × 5 m intersection (band-averaged cross-sections) ----

plot_orthogonal_sections_5m(
  krige_best,
  x0 = 4250,
  y0 = 6250,
  window_m = 5,
  agg = "wmean_se"   # or "mean"
)

ggsave(filename = paste0(plot_dir, "orthogonal_sections_5m.png"))













