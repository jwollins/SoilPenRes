### ERP penetrologger data
### Deployment: June 2024
### J Collins
### 2024-06-22

# ---- Packages + functions ----
source("analyses/01_packages.R")
source("R/dms_to_decimal.R")
source("R/to_long.R")



# ---- Paths (project-relative; no setwd) ----

source("config.R")

stopifnot(dir.exists(DATA_ROOT))

txt_dir   <- file.path(DATA_ROOT, "txt")
info_dir  <- file.path(DATA_ROOT, "info")
proc_dir  <- file.path(DATA_ROOT, "processed")
long_dir  <- file.path(proc_dir, "long_format_data")
sum_dir   <- file.path(proc_dir, "summary_profiles")


dir.create(long_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(sum_dir,  recursive = TRUE, showWarnings = FALSE)

# ---- Inputs ----
treatment_info <- read.csv(file.path(info_dir, "treatment_info.csv")) %>%
  mutate(id = as.integer(id))

# helper: id from filename
id_from_file <- function(path) as.integer(tools::file_path_sans_ext(basename(path)))

# ---- Read ONE file: info + data ----
read_pen_file <- function(path) {

  id_i <- id_from_file(path)

  # info block (top of file)
  info <- read.delim2(
    file = path,
    header = TRUE,
    sep = ":",
    dec = ".",
    col.names = c("name", "data"),
    skip = 2,
    nrows = 9,
    strip.white = TRUE
  )

  # turn into a named list for easy access
  info_vals <- setNames(as.list(info$data), info$name)

  # measurement block (lower part of file)
  dat <- read.delim2(
    file = path,
    header = TRUE,
    sep = "\t",
    dec = ",",
    col.names = c("name", "coordinates", paste0("X", 1:82)),
    skip = 13
  )

  # join treatment/location from lookup table
  hit <- treatment_info %>% filter(id == id_i)
  if (nrow(hit) == 0) {
    warning("No treatment match for id: ", id_i)
    hit <- tibble(id = id_i, treatment = NA, location = NA)
  }

  # attach metadata columns (constant per file)
  dat <- dat %>%
    mutate(
      id = id_i,
      treatment = hit$treatment[1],
      location = hit$location[1],

      project        = info_vals[["Project"]]        %||% info_vals[[1]],
      username       = info_vals[["User name"]]      %||% info_vals[[2]],
      date           = info_vals[["Date"]]           %||% info_vals[[3]],
      pens_per_plot  = info_vals[["Pens per plot"]]  %||% info_vals[[4]],
      nr_of_pens_done= info_vals[["Nr. of pens done"]] %||% info_vals[[5]],
      cone_type      = info_vals[["Cone type"]]      %||% info_vals[[6]],
      pen_speed      = info_vals[["Pen speed"]]      %||% info_vals[[7]],
      depth_unit     = info_vals[["Depth unit"]]     %||% info_vals[[8]],
      pressure_unit  = info_vals[["Pressure unit"]]  %||% info_vals[[9]]
    ) %>%
    relocate(id, treatment, location, name, coordinates, project, username, date,
             pens_per_plot, nr_of_pens_done, cone_type, pen_speed, depth_unit, pressure_unit)

  list(id = id_i, info = info, data = dat, file = path)
}

# small helper: provide fallback if missing (like %||% in rlang)
`%||%` <- function(x, y) if (!is.null(x) && length(x) > 0 && !is.na(x)) x else y

# ---- Read all files ----
txt_files <- list.files(txt_dir, pattern = "\\.txt$", full.names = TRUE)

parsed <- map(txt_files, read_pen_file)

# ---- Combine wide data (all pens/replicates) ----
wide_all <- map_dfr(parsed, "data")

# ---- 1) Meta info (one row per pen/replicate) ----
coord_pat <- "^([NS])(\\d+)\\s+(\\d+\\.?\\d*)\\s+([WE])(\\d+)\\s+(\\d+\\.?\\d*)$"

meta_info <- wide_all %>%
  select(id, treatment, location, name, coordinates, project, username, date,
         pens_per_plot, nr_of_pens_done, cone_type, pen_speed, depth_unit, pressure_unit) %>%
  distinct() %>%
  mutate(
    .m = str_match(coordinates, coord_pat),
    lat_dir = .m[, 2],
    lat_deg = as.numeric(.m[, 3]),
    lat_min = as.numeric(.m[, 4]),
    lon_dir = .m[, 5],
    lon_deg = as.numeric(.m[, 6]),
    lon_min = as.numeric(.m[, 7]),
    latitude  = mapply(dms_to_decimal, lat_deg, lat_min, lat_dir),
    longitude = mapply(dms_to_decimal, lon_deg, lon_min, lon_dir),
    .m = NULL,

    plot_number      = str_match(name, "^PLOT-(\\d{3})\\.")[, 2],
    replicate_number = str_match(name, "^PLOT-\\d{3}\\.(\\d+)")[, 2]
  )

write.csv(meta_info, file.path(info_dir, "meta_info.csv"), row.names = FALSE)

# ---- 2) Long-format data (one row per pen × depth) ----
# Your to_long() should pivot X* to (depth, penetration_resistance) and keep name/coordinates
long_data <- wide_all %>%
  to_long() %>%
  mutate(
    # keep id/treatment/location/etc if your to_long keeps them; if not, join back:
    plot_number      = str_match(name, "^PLOT-(\\d{3})\\.")[, 2],
    replicate_number = str_match(name, "^PLOT-\\d{3}\\.(\\d+)")[, 2]
  )

write.csv(long_data, file.path(long_dir, "long_format_data.csv"), row.names = FALSE)

# ---- 3) Optional: summary depth profile per file (mean/sd/sem across pens) ----
# This summarises across the replicate pens within each plot/file AFTER long-formatting:
summary_profiles <- long_data %>%
  mutate(penetration_resistance = as.numeric(trimws(penetration_resistance))) %>%
  group_by(id, treatment, location, plot_number, depth) %>%
  summarise(
    mean  = mean(penetration_resistance, na.rm = TRUE),
    stdev = sd(penetration_resistance, na.rm = TRUE),
    n     = sum(!is.na(penetration_resistance)),
    sem   = stdev / sqrt(n),
    .groups = "drop"
  )

write.csv(summary_profiles, file.path(sum_dir, "summary_profiles.csv"), row.names = FALSE)

cat("Saved:\n",
    "- ", file.path(info_dir, "meta_info.csv"), "\n",
    "- ", file.path(long_dir, "long_format_data.csv"), "\n",
    "- ", file.path(sum_dir, "summary_profiles.csv"), "\n")

