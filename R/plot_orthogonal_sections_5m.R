plot_orthogonal_sections_5m <- function(df,
                                        x0,
                                        y0,
                                        upper = NULL,
                                        option = "inferno",
                                        window_m = 5,            # size of intersection window (metres)
                                        agg = c("mean", "wmean_se")) {

  agg <- match.arg(agg)

  # --- helper: snap to nearest grid line ---
  nearest_level <- function(val, levels) levels[which.min(abs(levels - val))]

  # --- snap requested slice positions ---
  x_use <- nearest_level(x0, sort(unique(df$field_x)))
  y_use <- nearest_level(y0, sort(unique(df$field_y)))

  # --- window in cm ---
  half_w <- (window_m * 100) / 2  # e.g. 5m -> 250 cm

  # --- filter to 5m bands around the crosshair ---
  band_x <- df %>% dplyr::filter(field_x >= (x_use - half_w), field_x <= (x_use + half_w))
  band_y <- df %>% dplyr::filter(field_y >= (y_use - half_w), field_y <= (y_use + half_w))

  # --- build XZ: aggregate across Y band (so X vs depth) ---
  sec_xz_raw <- band_y %>%
    dplyr::mutate(z = -depth)

  # --- build YZ: aggregate across X band (so Y vs depth) ---
  sec_yz_raw <- band_x %>%
    dplyr::mutate(z = -depth)

  # --- aggregation choice ---
  if (agg == "mean") {
    sec_xz <- sec_xz_raw %>%
      dplyr::group_by(field_x, depth, z) %>%
      dplyr::summarise(pr_pred = mean(pr_pred, na.rm = TRUE), .groups = "drop")

    sec_yz <- sec_yz_raw %>%
      dplyr::group_by(field_y, depth, z) %>%
      dplyr::summarise(pr_pred = mean(pr_pred, na.rm = TRUE), .groups = "drop")
  } else {
    # weighted mean with weights = 1/SE^2 (common in spatial stats)
    # guard against zero/NA SE
    sec_xz <- sec_xz_raw %>%
      dplyr::mutate(w = 1 / pmax(kriging_se, 1e-6)^2) %>%
      dplyr::group_by(field_x, depth, z) %>%
      dplyr::summarise(pr_pred = stats::weighted.mean(pr_pred, w, na.rm = TRUE), .groups = "drop")

    sec_yz <- sec_yz_raw %>%
      dplyr::mutate(w = 1 / pmax(kriging_se, 1e-6)^2) %>%
      dplyr::group_by(field_y, depth, z) %>%
      dplyr::summarise(pr_pred = stats::weighted.mean(pr_pred, w, na.rm = TRUE), .groups = "drop")

    sec_xz <- sec_xz %>%
      dplyr::mutate(
        x_m = (field_x - (x_use - half_w)) / 100
      )

    sec_yz <- sec_yz %>%
      dplyr::mutate(
        y_m = (field_y - (y_use - half_w)) / 100
      )

  }

  # --- colour scale limits ---
  if (is.null(upper)) {
    lims <- range(df$pr_pred, na.rm = TRUE)
    upper <- ceiling(lims[2] * 10) / 10
  }

  # --- map inset (slice location) ---
  depth_map <- min(df$depth, na.rm = TRUE)

  p_map <- ggplot2::ggplot(
    df %>% dplyr::filter(depth == depth_map),
    ggplot2::aes(field_x, field_y)
  ) +
    ggplot2::geom_point(alpha = 0.05, size = 0.4) +
    # 5m x 5m box
    ggplot2::geom_rect(
      xmin = x_use - half_w, xmax = x_use + half_w,
      ymin = y_use - half_w, ymax = y_use + half_w,
      fill = NA, linewidth = 0.7
    ) +
    # centre crosshair lines
    ggplot2::geom_vline(xintercept = x_use, linewidth = 0.5) +
    ggplot2::geom_hline(yintercept = y_use, linewidth = 0.5) +
    ggplot2::coord_equal(expand = FALSE) +
    ggplot2::labs(title = paste0("Slice location (", window_m, " m × ", window_m, " m window)")) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(size = 10, face = "bold")
    )

  # --- X–Z section (band-averaged) ---
  p_xz <- ggplot(sec_xz, aes(x_m, z, fill = pr_pred)) +
    geom_tile() +
    coord_cartesian(xlim = c(0, window_m)) +
    scale_fill_viridis_c(
      option = option,
      limits = c(0, upper),
      oob = scales::squish,
      name = "PR (MPa)"
    ) +
    labs(
      subtitle = paste0("X–Z section (", window_m, " m window)"),
      x = "Distance along X (m)",
      y = "Depth (cm)"
    ) +
    theme_bw() +
    theme(panel.grid = element_blank())

  # --- Y–Z section (band-averaged) ---
  p_yz <- ggplot(sec_yz, aes(y_m, z, fill = pr_pred)) +
    geom_tile() +
    coord_cartesian(xlim = c(0, window_m)) +
    scale_fill_viridis_c(
      option = option,
      limits = c(0, upper),
      oob = scales::squish,
      name = "PR (MPa)"
    ) +
    labs(
      subtitle = paste0("Y–Z section (", window_m, " m window)"),
      x = "Distance along Y (m)",
      y = "Depth (cm)"
    ) +
    theme_bw() +
    theme(panel.grid = element_blank())

  # --- assemble layout ---
  (p_map | p_xz) / p_yz +
    patchwork::plot_layout(widths = c(1, 2), guides = "collect") &
    ggplot2::theme(legend.position = "right")
}
