make_bbox <- function(df, x = "field_x", y = "field_y",
                      buffer = 250, grid_step = 100,
                      hectare_min = 0, hectare_max = 10000) {

  xr <- range(df[[x]], na.rm = TRUE) + c(-buffer, buffer)
  yr <- range(df[[y]], na.rm = TRUE) + c(-buffer, buffer)

  x_range <- c(
    max(hectare_min, snap_down(xr[1], grid_step)),
    min(hectare_max, snap_up(xr[2], grid_step))
  )

  y_range <- c(
    max(hectare_min, snap_down(yr[1], grid_step)),
    min(hectare_max, snap_up(yr[2], grid_step))
  )

  nx <- as.integer((x_range[2] - x_range[1]) / grid_step) + 1L
  ny <- as.integer((y_range[2] - y_range[1]) / grid_step) + 1L

  list(x_range = x_range, y_range = y_range, nx = nx, ny = ny)
}
