run_id <- function(
    id_i,
    long_data,
    local_coords,
    out_dir,
    plot_dir,

    # ---- geometry ----
    square_size = 500,
    n_rows = 20,
    flip_rows = FALSE,

    # ---- kriging ----
    depths = c(10, 20, 30, 40, 60, 70, 80),
    grid_step = 100,
    buffer = 250,
    cutoff_3d = 1800,
    width_3d  = 70,
    z_scale   = 5,
    vgm_model = "Sph",
    vgm_range = 350,

    # ---- behaviour ----
    overwrite = FALSE,
    quiet = FALSE
) {

  if (!quiet) message("▶ Running id ", id_i)

  # ---- output paths ----
  csv_out  <- file.path(out_dir,  paste0("kriged_field_id_", id_i, ".csv"))
  pred_png <- file.path(plot_dir, paste0("kriged_grid_id_", id_i, ".png"))
  se_png   <- file.path(plot_dir, paste0("kriged_grid_se_id_", id_i, ".png"))

  if (!overwrite && file.exists(csv_out)) {
    if (!quiet) message("  ⤷ already exists, skipping")
    return(invisible(list(id = id_i, skipped = TRUE)))
  }

  # ---- prep long data (global hectare coords) ----
  long_ready <- prep_long_for_kriging(
    long_data,
    local_coords,
    id_i = id_i,
    square_size = square_size,
    n_rows = n_rows,
    flip_rows = flip_rows
  )

  stopifnot(all(c("field_x", "field_y") %in% names(long_ready)))

  # ---- bounding box snapped to global lattice ----
  snap_down <- function(v, step) floor(v / step) * step
  snap_up   <- function(v, step) ceiling(v / step) * step

  xr <- range(long_ready$field_x, na.rm = TRUE) + c(-buffer, buffer)
  yr <- range(long_ready$field_y, na.rm = TRUE) + c(-buffer, buffer)

  hectare_min <- 0
  hectare_max <- n_rows * square_size

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

  # ---- kriging ----
  res <- analyze_plot_3d_kriging(
    long_data = long_ready,
    id_i = id_i,
    coord_sys = "field",
    x_range = x_range,
    y_range = y_range,
    depth_range = range(depths),
    depths_pred = depths,
    depths_se   = depths,
    nx = nx,
    ny = ny,
    cutoff_3d = cutoff_3d,
    width_3d  = width_3d,
    safe_mode = FALSE,
    z_scale = z_scale,
    vgm_model = vgm_model,
    vgm_range = vgm_range
  )

  if (!res$ok) {
    warning("Kriging failed for id ", id_i)
    return(invisible(list(id = id_i, ok = FALSE)))
  }

  # ---- plots ----
  pred_plot <- add_square_axes(res$pred_plot, flip_rows = flip_rows)
  se_plot   <- add_square_axes(res$se_plot,   flip_rows = flip_rows)

  ggsave(pred_png, pred_plot, width = 10, height = 6, dpi = 300)
  ggsave(se_png,   se_plot,   width = 10, height = 6, dpi = 300)

  # ---- tidy kriged output ----
  kr_out <- as.data.frame(res$kriged_3d) %>%
    dplyr::transmute(
      id = id_i,
      field_x = .data[[res$x_col]],
      field_y = .data[[res$y_col]],
      depth = z_scaled / res$z_scale_used,
      pr_pred = var1.pred,
      kriging_se = sqrt(pmax(var1.var, 0))
    ) %>%
    dplyr::filter(depth %in% depths)

  write.csv(kr_out, csv_out, row.names = FALSE)

  if (!quiet) message("  ✓ finished id ", id_i)

  invisible(list(
    id = id_i,
    ok = TRUE,
    x_range = x_range,
    y_range = y_range,
    n_pred = nrow(kr_out),
    files = c(csv = csv_out, pred = pred_png, se = se_png)
  ))
}
