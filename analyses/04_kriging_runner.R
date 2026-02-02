### ERP penetrologger data — Daily kriging runner
### Sets up environment, loads data, sources functions, and runs run_id()
### J Collins

rm(list = ls()); gc()

# ---- Packages + project functions ----
source("analyses/01_packages.R")      # your package loader
source("config.R")                   # defines DATA_ROOT

# Core functions
source("R/analyze_plot_3d_kriging.R")
source("R/prep_long_for_kriging.R")
source("R/add_square_axes.R")

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
DEPTHS <- c(10, 20, 30, 40, 60, 70, 80)

SQUARE_SIZE <- 500
N_ROWS <- 20
FLIP_ROWS <- FALSE          # keep consistent with your field convention

GRID_STEP <- 100            # cm
BUFFER <- 250               # cm

# Option A: run a single ID
ids_to_run <- c(26013000)

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
  overwrite = FALSE,   # set TRUE to force re-run
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
