# config.R ---------------------------------------------------------------
# Purpose: define DATA_ROOT reliably across Google Drive mount variants
#          (CloudStorage vs legacy "… - Google Drive" path), without setwd().


# Packages used by config only
if (!requireNamespace("here", quietly = TRUE)) {
  stop("Please install 'here' (renv::install('here'))", call. = FALSE)
}

# Optional local overrides (not tracked by git)
secrets_path <- here::here("config", "secrets.R")
if (file.exists(secrets_path)) source(secrets_path)

.norm <- function(p) normalizePath(path.expand(p), winslash = "/", mustWork = FALSE)

.home <- Sys.getenv("HOME")

# ---- Candidate Drive roots (allow override via secrets.R) ----
drive_candidates <- unique(.norm(c(
  getOption("soilpenres.drive_root_override", NA_character_),
  file.path(.home, "Library/CloudStorage/GoogleDrive-jcollins@earthroverprogram.org"),
  file.path(.home, "Library/CloudStorage/GoogleDrive-jcollins94@outlook.com"),
  file.path(.home, "jcollins@earthroverprogram.org - Google Drive"),
  file.path(.home, "Google Drive")
)))

drive_candidates <- drive_candidates[!is.na(drive_candidates)]

drive_root <- drive_candidates[dir.exists(drive_candidates)][1]
if (is.na(drive_root)) {
  stop(
    "Could not find Google Drive root.\nTried:\n- ",
    paste(drive_candidates, collapse = "\n- "),
    call. = FALSE
  )
}

shared_drives_root <- file.path(drive_root, "Shared drives")

# If the chosen root doesn't contain Shared drives, search candidates
if (!dir.exists(shared_drives_root)) {
  sd_candidates <- file.path(drive_candidates, "Shared drives")
  shared_drives_root <- sd_candidates[dir.exists(sd_candidates)][1]
  if (is.na(shared_drives_root)) {
    stop(
      "Found a Google Drive root but cannot locate 'Shared drives'.\n",
      "Drive root detected: ", drive_root,
      call. = FALSE
    )
  }
}

# ---- Deployment relative path (only hard-coded piece) ----
DEPLOY_REL <- file.path(
  "DATA", "FIELD_DEPLOYMENTS",
  "2025-10-UK_ Harper -1HA_survey",
  "soil_data", "penetration_res"
)

DATA_ROOT <- .norm(file.path(shared_drives_root, DEPLOY_REL))

# ---- Define standard project output locations (inside repo) ----
# (Keeps generated stuff out of your Drive unless you explicitly want it there.)
OUTPUTS_DIR <- here::here("outputs")
FIG_DIR     <- file.path(OUTPUTS_DIR, "figures")
TABLE_DIR   <- file.path(OUTPUTS_DIR, "tables")
REPORT_DIR  <- file.path(OUTPUTS_DIR, "reports")

# ---- Input subfolders (inside DATA_ROOT on Drive) ----
TXT_DIR  <- file.path(DATA_ROOT, "txt")
INFO_DIR <- file.path(DATA_ROOT, "info")

# ---- Processed data: choose one strategy ----
# Strategy A (recommended): processed lives in repo (reproducible + portable)
PROC_DIR <- here::here("data", "processed")

# If you prefer processed on Drive instead, swap to:
# PROC_DIR <- file.path(DATA_ROOT, "processed")

LONG_DIR <- file.path(PROC_DIR, "long_format_data")
SUM_DIR  <- file.path(PROC_DIR, "summary_profiles")
KRIG_DIR <- file.path(DATA_ROOT, "processed", "kriging_data")
FIELD_GRID_SHP <- file.path(DATA_ROOT, "processed", "hectare_grid_labelled.shp")
stopifnot(file.exists(FIELD_GRID_SHP))


# ---- Validate required inputs ----
req_dirs <- c(TXT_DIR, INFO_DIR)
missing  <- req_dirs[!dir.exists(req_dirs)]
if (length(missing) > 0) {
  stop(
    "Required input directories missing:\n",
    paste0(" - ", missing, collapse = "\n"),
    "\n\nResolved DATA_ROOT:\n  ", DATA_ROOT,
    "\nTip: if this folder is online-only in Google Drive, make it 'Available offline'.",
    call. = FALSE
  )
}

key_file <- file.path(INFO_DIR, "treatment_info.csv")
if (!file.exists(key_file)) {
  stop("Key file missing: ", key_file, call. = FALSE)
}

# ---- Create output folders (safe) ----
dir.create(OUTPUTS_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(FIG_DIR,     recursive = TRUE, showWarnings = FALSE)
dir.create(TABLE_DIR,   recursive = TRUE, showWarnings = FALSE)
dir.create(REPORT_DIR,  recursive = TRUE, showWarnings = FALSE)

dir.create(PROC_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(LONG_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(SUM_DIR,  recursive = TRUE, showWarnings = FALSE)

dir.create(KRIG_DIR, recursive = TRUE, showWarnings = FALSE)


# Optional debug
if (isTRUE(getOption("soilpenres.verbose_config", FALSE))) {
  message("Drive root:    ", drive_root)
  message("Shared drives:", shared_drives_root)
  message("DATA_ROOT:    ", DATA_ROOT)
  message("PROC_DIR:     ", PROC_DIR)
  message("FIG_DIR:      ", FIG_DIR)
}




