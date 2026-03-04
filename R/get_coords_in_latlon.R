# --- get the coords in lat long ----


# ---- Packages and functions ----
source(here::here("analyses", "01_packages.R"))
source(here::here("config.R"))



field_sf <- st_read(FIELD_GRID_SHP, quiet = TRUE)



# 1) pick the column in field_sf that actually contains the grid label (A1..T20)
#    Adjust the candidates if your shapefile uses a different name.
lab_col <- intersect(names(field_sf),
                     c("label","Label","GRID","grid","grid_id","GridID","plot","Plot","id","ID"))[1]

if (is.na(lab_col)) {
  stop("Couldn't find a label column. Run names(field_sf) and set lab_col manually.")
}

# 2) build lon/lat for each grid cell (A..T, 1..20)
grid_lonlat <- field_sf %>%
  mutate(grid_label = as.character(.data[[lab_col]])) %>%
  filter(str_detect(grid_label, "^[A-T][-_]?(?:[1-9]|1\\d|20)$")) %>%  # matches A1, A-1, A_1 ... T20
  mutate(grid_label = str_replace_all(grid_label, "[-_]", "")) %>%     # normalize to A1, B12, etc
  st_make_valid() %>%
  st_point_on_surface() %>%                                           # safer than st_centroid for odd polygons
  st_transform(4326) %>%                                              # WGS84 lon/lat
  mutate(
    lon = st_coordinates(.)[,1],
    lat = st_coordinates(.)[,2],
    row = str_extract(grid_label, "^[A-T]"),
    col = as.integer(str_extract(grid_label, "\\d+$"))
  ) %>%
  st_drop_geometry() %>%
  select(grid_label, row, col, lon, lat) %>%
  arrange(row, col)

grid_lonlat


write.csv(x = grid_lonlat, file = file.path(TABLE_DIR, "hectare_grid_lonlat.csv"))
