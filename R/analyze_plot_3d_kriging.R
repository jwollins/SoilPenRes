analyze_plot_3d_kriging <- function(
    long_data,
    id_i,
    depths_pred = c(10, 20, 30, 40, 60, 70, 80),
    depths_se   = c(10, 20, 30, 40, 60, 70, 80),

    # spatial domain
    x_range = c(0, 500),
    y_range = c(0, 500),
    depth_range = c(1, 82),

    # prediction grid resolution
    nx = 30, ny = 30, nz = 82,

    # empirical variogram settings
    cutoff_3d = 600,
    width_3d  = 30,

    # initial model defaults (used if safe_mode=FALSE)
    z_scale = 5,
    vgm_model = "Sph",
    vgm_range = 150,
    psill_frac = 0.5,
    nugget_frac = 0.1,

    # legen limits
    pr_limits = NULL,
    se_limits = NULL,

    # palettes
    palette_pred = "inferno",
    palette_se   = "magma",

    # diagnostics / output
    show_variogram_plots = FALSE,
    save_dir = NULL,      # e.g. file.path(DATA_ROOT, "figures")
    file_prefix = NULL,   # default paste0("id_", id_i)

    # ---- SAFE MODE ----
    safe_mode = TRUE,
    # try these z scalings (vertical anisotropy proxy)
    z_scale_try = c(1, 2, 5, 10),
    # try these model types
    vgm_models_try = c("Exp", "Sph", "Gau"),
    # try these initial ranges
    vgm_ranges_try = c(80, 120, 150, 200, 300),
    # if TRUE: don't stop on failure, return ok=FALSE instead
    quiet_fail = TRUE
) {
  stopifnot(
    requireNamespace("dplyr", quietly = TRUE),
    requireNamespace("sp", quietly = TRUE),
    requireNamespace("gstat", quietly = TRUE),
    requireNamespace("ggplot2", quietly = TRUE),
    requireNamespace("viridis", quietly = TRUE),
    requireNamespace("scales", quietly = TRUE)
  )

  `%>%` <- dplyr::`%>%`

  # ---- Subset data for this id ----
  df0 <- long_data %>%
    dplyr::filter(id == id_i) %>%
    dplyr::mutate(
      depth = as.numeric(depth),
      penetration_resistance = as.numeric(penetration_resistance),
      local_x = as.numeric(local_x),
      local_y = as.numeric(local_y)
    )

  if (nrow(df0) == 0) stop("No rows found for id = ", id_i)

  # labels for titles
  trt <- df0 %>% dplyr::distinct(treatment) %>% dplyr::pull()
  loc <- df0 %>% dplyr::distinct(location)  %>% dplyr::pull()
  trt_lab <- paste(trt, collapse = ", ")
  loc_lab <- paste(loc, collapse = ", ")

  # quick checks
  n_xy <- df0 %>% dplyr::distinct(local_x, local_y) %>% nrow()
  if (n_xy < 3) {
    msg <- paste0("Too few unique (local_x, local_y) points (", n_xy, ") for kriging for id=", id_i)
    if (!quiet_fail) stop(msg)
    return(list(ok = FALSE, id = id_i, reason = msg, treatment = trt, location = loc))
  }

  # ---- Empirical variogram will be computed on a SpatialPointsDataFrame, so we need a helper ----
  fit_one <- function(zs, model, range0) {
    dfp <- df0 %>%
      dplyr::mutate(
        z_scaled = depth * zs
      )

    sp::coordinates(dfp) <- ~ local_x + local_y + z_scaled

    vgm3d <- gstat::variogram(
      penetration_resistance ~ 1,
      dfp,
      cutoff = cutoff_3d,
      width  = width_3d
    )

    v0 <- stats::var(dfp$penetration_resistance, na.rm = TRUE)

    m0 <- gstat::vgm(
      psill  = psill_frac * v0,
      model  = model,
      range  = range0,
      nugget = nugget_frac * v0
    )

    # Try to fit; treat warnings as non-fatal, but catch errors
    fit <- tryCatch(
      suppressWarnings(gstat::fit.variogram(vgm3d, model = m0)),
      error = function(e) NULL
    )

    # gstat returns a variogramModel; if it failed badly it may be NULL or have NAs
    if (is.null(fit)) return(NULL)
    if (anyNA(fit$psill) || anyNA(fit$range)) return(NULL)

    list(dfp = dfp, vgm3d = vgm3d, fit = fit, z_scale = zs, model = model, range0 = range0)
  }

  # ---- Choose candidate tries (baseline first, then fallbacks) ----
  preferred <- data.frame(
    zs = z_scale,
    model = vgm_model,
    range0 = vgm_range,
    stringsAsFactors = FALSE
  )

  fallback <- expand.grid(
    zs = z_scale_try,
    model = vgm_models_try,
    range0 = vgm_ranges_try,
    stringsAsFactors = FALSE
  )

  tries <- dplyr::bind_rows(preferred, fallback) %>%
    dplyr::distinct(zs, model, range0)

  # ---- Run tries until one works ----
  best <- NULL
  tried <- list()

  for (k in seq_len(if (safe_mode) nrow(tries) else length(tries))) {
    if (!safe_mode) {
      zs <- tries[[k]]$zs
      model <- tries[[k]]$model
      range0 <- tries[[k]]$range0
    } else {
      zs <- tries$zs[k]
      model <- tries$model[k]
      range0 <- tries$range0[k]
    }

    out <- fit_one(zs, model, range0)
    tried[[length(tried) + 1]] <- paste0("z_scale=", zs, ", model=", model, ", range0=", range0)

    if (!is.null(out)) {
      best <- out
      break
    }
  }

  if (is.null(best)) {
    msg <- paste0(
      "No variogram fit succeeded for id=", id_i,
      ". Tried: ", paste(tried, collapse = " | ")
    )
    if (!quiet_fail) stop(msg)
    return(list(ok = FALSE, id = id_i, reason = msg, treatment = trt, location = loc))
  }

  # ---- Optional variogram plots ----
  if (show_variogram_plots) {
    graphics::plot(best$vgm3d, main = paste0("3D empirical variogram | id ", id_i))
    graphics::plot(best$vgm3d, best$fit, main = paste0("3D variogram fit | id ", id_i))
  }

  # ---- Prediction grid (uses z_scaled consistent with best z_scale) ----
  xg <- seq(x_range[1], x_range[2], length.out = nx)
  yg <- seq(y_range[1], y_range[2], length.out = ny)
  zg <- seq(depth_range[1], depth_range[2], length.out = nz)

  grid <- expand.grid(
    local_x  = xg,
    local_y  = yg,
    z_scaled = zg * best$z_scale
  )
  sp::coordinates(grid) <- ~ local_x + local_y + z_scaled

  kriged_3d <- gstat::krige(
    penetration_resistance ~ 1,
    best$dfp,
    newdata = grid,
    model = best$fit
  )

  kr_df <- as.data.frame(kriged_3d) %>%
    dplyr::mutate(depth = round(z_scaled / best$z_scale))

  # ---- Build plotting frames ----
  slice_pred <- kr_df %>%
    dplyr::filter(depth %in% depths_pred) %>%
    dplyr::mutate(depth = factor(depth, levels = depths_pred))

  slice_se <- kr_df %>%
    dplyr::filter(depth %in% depths_se) %>%
    dplyr::mutate(
      depth = factor(depth, levels = depths_se),
      kriging_se = sqrt(var1.var)
    )

  # ---- Plots ----
  title_pred <- paste0(
    "3D kriged penetration resistance | id ", id_i,
    " | Treatment: ", trt_lab,
    " | Location: ", loc_lab
  )

  pred_plot <- ggplot2::ggplot(slice_pred, ggplot2::aes(local_x, local_y, fill = var1.pred)) +
    ggplot2::geom_raster() +
    ggplot2::coord_equal(xlim = x_range, ylim = y_range, expand = FALSE) +
    ggplot2::facet_wrap(
      ~ depth, ncol = 4,
      labeller = ggplot2::labeller(depth = function(x) paste0("Depth = ", x, " cm"))
    ) +
    ggplot2::scale_fill_viridis_c(
      option = palette_pred,
      name = "PR (MPa)",
      limits = pr_limits,
      oob = scales::squish
    ) +
    ggplot2::labs(title = title_pred, x = "Local X", y = "Local Y") +
    ggplot2::theme_bw() +
    ggplot2::theme(
      strip.text = ggplot2::element_text(face = "bold"),
      strip.background = ggplot2::element_rect(fill = "grey95", colour = NA)
    )

  title_se <- paste0(
    "Kriging standard error (3D) | id ", id_i,
    " | Treatment: ", trt_lab,
    " | Location: ", loc_lab
  )

  se_plot <- ggplot2::ggplot(slice_se, ggplot2::aes(local_x, local_y, fill = kriging_se)) +
    ggplot2::geom_raster() +
    ggplot2::coord_equal(xlim = x_range, ylim = y_range, expand = FALSE) +
    ggplot2::facet_wrap(
      ~ depth, ncol = 4,
      labeller = ggplot2::labeller(depth = function(x) paste0("Depth = ", x, " cm"))
    ) +
    ggplot2::scale_fill_viridis_c(
      option = palette_se,
      name = "Kriging SE (MPa)",
      limits = se_limits,
      oob = scales::squish
    ) +
    ggplot2::labs(title = title_se, x = "Local X", y = "Local Y") +
    ggplot2::theme_bw() +
    ggplot2::theme(
      strip.text = ggplot2::element_text(face = "bold"),
      strip.background = ggplot2::element_rect(fill = "grey95", colour = NA)
    )

  # ---- Optional saving ----
  if (!is.null(save_dir)) {
    dir.create(save_dir, recursive = TRUE, showWarnings = FALSE)
    if (is.null(file_prefix)) file_prefix <- paste0("id_", id_i)

    ggplot2::ggsave(
      filename = file.path(save_dir, paste0(file_prefix, "_kriged_pred.png")),
      plot = pred_plot, width = 9, height = 7, dpi = 300
    )
    ggplot2::ggsave(
      filename = file.path(save_dir, paste0(file_prefix, "_kriged_se.png")),
      plot = se_plot, width = 9, height = 7, dpi = 300
    )
  }

  list(
    ok = TRUE,
    id = id_i,
    treatment = trt,
    location = loc,
    z_scale_used = best$z_scale,
    vgm_model_used = best$model,
    init_range_used = best$range0,
    vgm3d = best$vgm3d,
    fit = best$fit,
    kriged_3d = kriged_3d,
    pred_plot = pred_plot,
    se_plot = se_plot,
    tried = tried
  )
}
