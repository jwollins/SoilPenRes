# scripts/00_setup.R
# Run this ONCE per machine

message("Setting up packages for this project...")

pkgs <- c(
  "tidyverse",
  "ggpubr",
  "gridExtra",
  "readxl",
  "plotrix",
  "lmerTest",
  "sf",
  "ggspatial",
  "plotly",
  "here",
  "renv"
)

installed <- rownames(installed.packages())
to_install <- setdiff(pkgs, installed)

if (length(to_install) > 0) {
  install.packages(to_install)
  message("Installed: ", paste(to_install, collapse = ", "))
} else {
  message("All packages already installed.")
}
