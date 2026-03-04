# ---- GPS plot ----



# ---- Packages and functions ----
source(here::here("analyses", "01_packages.R"))
source(here::here("config.R"))
source(here::here("R", "rotate_90_left.R"))

# ---- Load ----
long_data <- read.csv(file.path(LONG_DIR, "long_format_data.csv"))
local_coords <- read.csv(file.path(INFO_DIR, "local_coords2.csv"))
field_sf <- st_read(FIELD_GRID_SHP, quiet = TRUE)


# ---- Type cleanup ----
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



# ---- GPS plot ----

glimpse(long_data)

long_data_coords <- long_data %>%
  distinct(id, coordinates) %>%
  mutate(
    m = str_match(coordinates, "^N(\\d+)\\s+(\\d+\\.?\\d*)\\s+W(\\d+)\\s+(\\d+\\.?\\d*)$"),
    lat_deg = as.numeric(m[, 2]),
    lat_min = as.numeric(m[, 3]),
    lon_deg = as.numeric(m[, 4]),
    lon_min = as.numeric(m[, 5]),
    latitude  = lat_deg + lat_min / 60,
    longitude = -(lon_deg + lon_min / 60)
  ) %>%
  select(-m, -lat_deg, -lat_min, -lon_deg, -lon_min)


ids_to_drop <- c(
  26012230, 26012231, 26012232, 26012233, 26012234,
  26012235, 26012236, 26012237, 26012238
)


coords_clean <- long_data_coords %>%
  filter(!id %in% ids_to_drop) %>%     # ⬅️ drop bad IDs
  filter(!is.na(latitude), !is.na(longitude)) %>%
  filter(
    between(latitude,
            quantile(latitude, 0.01, na.rm = TRUE),
            quantile(latitude, 0.99, na.rm = TRUE)),
    between(longitude,
            quantile(longitude, 0.01, na.rm = TRUE),
            quantile(longitude, 0.99, na.rm = TRUE))
  )



id_labels <- coords_clean %>%
  group_by(id) %>%
  summarise(
    longitude = mean(longitude),
    latitude  = mean(latitude),
    .groups = "drop"
  )









# Points as sf (WGS84 lon/lat)
pts_sf <- coords_clean %>%
  distinct(id, longitude, latitude) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

lab_sf <- id_labels %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

# Transform to the shapefile CRS
pts_sf <- st_transform(pts_sf, st_crs(field_sf))
lab_sf <- st_transform(lab_sf, st_crs(field_sf))

# extract coordinates for ID labels (ggrepel needs X/Y)
lab_xy <- cbind(lab_sf, st_coordinates(lab_sf))




# --- call roation function ----
field_sf_rot <- rotate_90_left(field_sf)
pts_sf_rot   <- rotate_90_left(pts_sf)



bb <- st_bbox(field_sf)

cx <- (bb["xmin"] + bb["xmax"]) / 2
cy <- (bb["ymin"] + bb["ymax"]) / 2

lab_xy_rot <- transform(
  lab_xy,
  X = cx - (Y - cy),
  Y = cy + (X - cx)
)



# --- PLOT IT ----
ggplot() +
  geom_sf(data = field_sf_rot, fill = NA,
          linewidth = 0.3, colour = "grey50") +
  geom_sf_text(data = field_sf_rot,
               aes(label = label),
               size = 2.5, fontface = "bold") +
  geom_sf(data = pts_sf_rot,
          size = 1, alpha = 0.5, colour = "blue") +
  geom_label_repel(data = lab_xy_rot,
                   aes(X, Y, label = id),
                   size = 4, fill = "white",
                   alpha = 0.85, colour = "red") +
  coord_sf() +
  theme_bw()


ggsave(filename = file.path(FIG_DIR, "GPS_location_plot.png"),
       width = 7, height = 7)
