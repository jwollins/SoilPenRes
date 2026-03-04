library(sf)
library(dplyr)
library(stringr)

grid_sf <- st_read("~/jcollins@earthroverprogram.org - Google Drive/Shared drives/DATA/site_data/1_ha_survey/experimental_design/1_ha_survey_plot/shapefiles_1ha_5m_epsg27700/grid_cells.shp")

if (st_is_longlat(grid_sf)) {
  grid_sf <- st_transform(grid_sf, 27700)  # British National Grid (UK). Change if needed.
}

cent <- st_centroid(grid_sf)
xy <- st_coordinates(cent)

grid_lab <- grid_sf %>%
  mutate(X = xy[,1], Y = xy[,2])

# sorted unique centroid coordinates (should be 20 each)
x_levels <- sort(unique(round(grid_lab$X, 6)))
y_levels <- sort(unique(round(grid_lab$Y, 6)))

stopifnot(length(x_levels) == 20, length(y_levels) == 20)

letters_AT <- LETTERS[1:20]  # A:T

grid_lab <- grid_lab %>%
  mutate(
    col = match(round(X, 6), x_levels),         # 1..20
    row = match(round(Y, 6), y_levels),         # 1..20 (bottom..top)
    row_letter = letters_AT[row],
    label = paste0(row_letter, col)
  )


grid_lab <- grid_lab %>%
  mutate(
    row = 21 - match(round(Y, 6), y_levels),
    row_letter = letters_AT[row],
    label = paste0(row_letter, col)
  )


library(ggplot2)

ggplot() +
  geom_sf(data = grid_lab, fill = NA) +
  geom_text(
    data = st_drop_geometry(grid_lab),
    aes(x = X, y = Y, label = label),
    size = 2
  ) +
  coord_sf() +
  theme_minimal()



library(sf)

st_write(
  grid_lab,
  file.path(proc_dir, "hectare_grid_labelled.shp"),
  delete_layer = TRUE
)

