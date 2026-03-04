### ERP penetrologger data
### Deployment: June 2024
### J Collins
### 2024-06-22

# ---- Packages + functions ----
# source("analyses/01_packages.R")
# source("R/dms_to_decimal.R")
# source("R/to_long.R")
# source(file = "R/read_pen_file.R")

source(here::here("analyses", "01_packages.R"))
source(here::here("R", "dms_to_decimal.R"))
source(here::here("R", "to_long.R"))
source(here::here("R", "read_pen_file.R"))

source(here::here("config.R"))

# Now just use TXT_DIR, INFO_DIR, LONG_DIR, SUM_DIR, FIG_DIR, etc.



# ---- Inputs ----
treatment_info <- read.csv(file.path(INFO_DIR, "treatment_info.csv")) %>%
  mutate(id = as.integer(id))

# helper: id from filename
id_from_file <- function(path) as.integer(tools::file_path_sans_ext(basename(path)))


# small helper: provide fallback if missing (like %||% in rlang)
`%||%` <- function(x, y) if (!is.null(x) && length(x) > 0 && !is.na(x)) x else y



# ---- Read all files ----
txt_files <- list.files(TXT_DIR, pattern = "\\.txt$", full.names = TRUE)

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

write.csv(meta_info, file.path(INFO_DIR, "meta_info.csv"), row.names = FALSE)


# ---- 2) Long-format data (one row per pen × depth) ----
# Your to_long() should pivot X* to (depth, penetration_resistance) and keep name/coordinates
long_data <- wide_all %>%
  to_long() %>%
  mutate(
    # keep id/treatment/location/etc if your to_long keeps them; if not, join back:
    plot_number      = str_match(name, "^PLOT-(\\d{3})\\.")[, 2],
    replicate_number = str_match(name, "^PLOT-\\d{3}\\.(\\d+)")[, 2]
  )

glimpse(long_data)

write.csv(long_data, file.path(LONG_DIR, "long_format_data.csv"), row.names = FALSE)



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

write.csv(summary_profiles, file.path(SUM_DIR, "summary_profiles.csv"), row.names = FALSE)

cat("Saved:\n",
    "- ", file.path(INFO_DIR, "meta_info.csv"), "\n",
    "- ", file.path(LONG_DIR, "long_format_data.csv"), "\n",
    "- ", file.path(SUM_DIR, "summary_profiles.csv"), "\n")

