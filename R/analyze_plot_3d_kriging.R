analyze_plot_3d_kriging <- function(
    long_data,
    id_i,
    depths_pred = c(10, 20, 30, 40, 60, 70, 80),
    depths_se   = c(10, 20, 30, 40, 60, 70, 80),

    # choose coord system for interpolation domain
    coord_sys = c("local", "field"),

    # spatial domain (optional; if NULL computed from data)
    x_range = NULL,
    y_range = NULL,
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

    # legend limits
    pr_limits = NULL,
    se_limits = NULL,

    # palettes
    palette_pred = "inferno",
    palette_se   = "magma",

    # diagnostics / output
    show_variogram_plots = FALSE,
    save_dir = NULL,
    file_prefix = NULL,

    # ---- SAFE MODE ----
    safe_mode = TRUE,
    z_scale_try = c(1, 2, 5, 10),
    vgm_models_try = c("Exp", "Sph", "Gau"),
    vgm_ranges_try = c(80, 120, 150, 200, 300),
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
  coord_sys <- match.arg(coord_sys)

  # ---- Subset data for this id ----
  df0 <- long_data %>%
    dplyr::filter(id == id_i) %>%
    dplyr::mutate(
      depth = as.numeric(depth),
      penetration_resistance = as.numeric(penetration_resistance),
      local_x = as.numeric(local_x),
      local_y = as.numeric(local_y),
      field_x = as.numeric(field_x),
      field_y = as.numeric(field_y)
    )

  if (nrow(df0) == 0) stop("No rows found for id = ", id_i)

  # Choose XY columns based on coord_sys
  xy_cols <- if (coord_sys == "local") c("local_x", "local_y") else c("field_x", "field_y")
  x_col <- xy_cols[1]
  y_col <- xy_cols[2]

  # Drop rows missing chosen coords
  df0 <- df0 %>%
    dplyr::filter(!is.na(.data[[x_col]]), !is.na(.data[[y_col]]), !is.na(depth))

  if (nrow(df0) == 0) {
    msg <- paste0("All rows missing ", x_col, "/", y_col, " for id=", id_i)
    if (!quiet_fail) stop(msg)
    return(list(ok = FALSE, id = id_i, reason = msg))
  }

  # labels for titles
  trt <- df0 %>% dplyr::distinct(treatment) %>% dplyr::pull()
  loc <- df0 %>% dplyr::distinct(location)  %>% dplyr::pull()
  trt_lab <- paste(trt, collapse = ", ")
  loc_lab <- paste(loc, collapse = ", ")

  # auto ranges if not supplied
  if (is.null(x_range)) x_range <- range(df0[[x_col]], na.rm = TRUE)
  if (is.null(y_range)) y_range <- range(df0[[y_col]], na.rm = TRUE)

  # quick checks on unique XY
  n_xy <- df0 %>% dplyr::distinct(.data[[x_col]], .data[[y_col]]) %>% nrow()
  if (n_xy < 3) {
    msg <- paste0("Too few unique (", x_col, ", ", y_col, ") points (", n_xy, ") for kriging for id=", id_i)
    if (!quiet_fail) stop(msg)
    return(list(ok = FALSE, id = id_i, reason = msg, treatment = trt, location = loc))
  }

  # ---- Empirical variogram helper ----
  fit_one <- function(zs, model, range0) {
    dfp <- df0 %>%
      dplyr::mutate(z_scaled = depth * zs)

    # IMPORTANT: set coordinates using chosen XY columns
    sp::coordinates(dfp) <- stats::as.formula(paste0("~", x_col, "+", y_col, "+z_scaled"))

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

    fit <- tryCatch(
      suppressWarnings(gstat::fit.variogram(vgm3d, model = m0)),
      error = function(e) NULL
    )

    if (is.null(fit)) return(NULL)
    if (anyNA(fit$psill) || anyNA(fit$range)) return(NULL)

    list(dfp = dfp, vgm3d = vgm3d, fit = fit, z_scale = zs, model = model, range0 = range0)
  }

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

  best <- NULL
  tried <- list()

  for (k in seq_len(if (safe_mode) nrow(tries) else nrow(preferred))) {
    zs <- tries$zs[k]
    model <- tries$model[k]
    range0 <- tries$range0[k]

    out <- fit_one(zs, model, range0)
    tried[[length(tried) + 1]] <- paste0("z_scale=", zs, ", model=", model, ", range0=", range0)

    if (!is.null(out)) { best <- out; break }
  }

  if (is.null(best)) {
    msg <- paste0("No variogram fit succeeded for id=", id_i, ". Tried: ", paste(tried, collapse = " | "))
    if (!quiet_fail) stop(msg)
    return(list(ok = FALSE, id = id_i, reason = msg, treatment = trt, location = loc))
  }

  if (show_variogram_plots) {
    graphics::plot(best$vgm3d, main = paste0("3D empirical variogram | id ", id_i))
    graphics::plot(best$vgm3d, best$fit, main = paste0("3D variogram fit | id ", id_i))
  }

  # ---- Prediction grid (chosen coord system) ----
  xg <- seq(x_range[1], x_range[2], length.out = nx)
  yg <- seq(y_range[1], y_range[2], length.out = ny)

  # Use *exact* depths requested for plotting (much faster too)
  zg <- sort(unique(c(depths_pred, depths_se)))

  # optional: respect depth_range (keeps things sane if you pass a tighter range)
  zg <- zg[zg >= depth_range[1] & zg <= depth_range[2]]

  # update nz to match
  nz <- length(zg)


  grid <- expand.grid(
    x = xg,
    y = yg,
    z_scaled = zg * best$z_scale
  )

  # Name the grid columns to match the chosen coord columns
  names(grid)[names(grid) == "x"] <- x_col
  names(grid)[names(grid) == "y"] <- y_col

  sp::coordinates(grid) <- stats::as.formula(paste0("~", x_col, "+", y_col, "+z_scaled"))

  kriged_3d <- gstat::krige(
    penetration_resistance ~ 1,
    best$dfp,
    newdata = grid,
    model = best$fit
  )

  kr_df <- as.data.frame(kriged_3d) %>%
    dplyr::mutate(depth = z_scaled / best$z_scale)

  # ---- Build plotting frames ----
  slice_pred <- kr_df %>%
    dplyr::filter(depth %in% depths_pred) %>%
    dplyr::mutate(depth = factor(depth, levels = depths_pred))

  slice_se <- kr_df %>%
    dplyr::filter(depth %in% depths_se) %>%
    dplyr::mutate(
      depth = factor(depth, levels = depths_se),
      kriging_se = sqrt(pmax(var1.var, 0))
    )

  # Axis labels depend on coord system
  x_lab <- if (coord_sys == "local") "Local X" else "Field X"
  y_lab <- if (coord_sys == "local") "Local Y" else "Field Y"

  title_pred <- paste0(
    "3D kriged penetration resistance | id ", id_i,
    " | coords: ", coord_sys,
    " | Treatment: ", trt_lab,
    " | Location: ", loc_lab
  )

  pred_plot <- ggplot2::ggplot(slice_pred, ggplot2::aes(.data[[x_col]], .data[[y_col]], fill = var1.pred)) +
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
    ggplot2::labs(title = title_pred, x = x_lab, y = y_lab) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      strip.text = ggplot2::element_text(face = "bold"),
      strip.background = ggplot2::element_rect(fill = "grey95", colour = NA)
    )

  title_se <- paste0(
    "Kriging standard error (3D) | id ", id_i,
    " | coords: ", coord_sys,
    " | Treatment: ", trt_lab,
    " | Location: ", loc_lab
  )

  se_plot <- ggplot2::ggplot(slice_se, ggplot2::aes(.data[[x_col]], .data[[y_col]], fill = kriging_se)) +
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
    ggplot2::labs(title = title_se, x = x_lab, y = y_lab) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      strip.text = ggplot2::element_text(face = "bold"),
      strip.background = ggplot2::element_rect(fill = "grey95", colour = NA)
    )

  if (!is.null(save_dir)) {
    dir.create(save_dir, recursive = TRUE, showWarnings = FALSE)
    if (is.null(file_prefix)) file_prefix <- paste0("id_", id_i, "_", coord_sys)

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
    coord_sys = coord_sys,
    x_col = x_col,
    y_col = y_col,
    x_range = x_range,
    y_range = y_range,
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


library(gstat)
library(sp)
library(dplyr)

df_cv <- long_preview %>%
  filter(id == 26012900) %>%
  mutate(z_scaled = depth * res$z_scale_used)

coordinates(df_cv) <- ~ field_x + field_y + z_scaled

cv <- gstat::krige.cv(
  penetration_resistance ~ 1,
  df_cv,
  model = res$fit,
  nfold = 5
)


krige_stats <-
    data.frame(
      ME   = mean(cv$residual, na.rm = TRUE),
      MAE  = mean(abs(cv$residual), na.rm = TRUE),
      RMSE = sqrt(mean(cv$residual^2, na.rm = TRUE)),
      R2   = cor(cv$observed, cv$observed - cv$residual, use = "complete.obs")^2
    )

