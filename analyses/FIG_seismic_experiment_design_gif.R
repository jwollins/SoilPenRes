source(here::here("analyses", "01_packages.R"))
source(here::here("config.R"))
library(magick)


# ---- plot the experimental design ----

# pick any seismology file from the FieldUI
base_dir <- file.path(shared_drives_root, "DATA/FIELD_DEPLOYMENTS/2025-10-UK_ Harper -1HA_survey/Group2_10M/")


# build ordered list of file paths
png_files <- file.path(
  base_dir,
  sprintf("experiment_%04d", 1:48),
  "layout.png"
)

# sanity check: which are missing?
missing <- png_files[!file.exists(png_files)]
if (length(missing) > 0) {
  stop("Missing files:\n", paste(missing, collapse = "\n"))
}

gif_path <- file.path(FIG_DIR, "experiment_layouts.gif")

image_read(png_files) |>
  image_animate(fps = 2.5) |>
  image_write(gif_path)

gif_path
