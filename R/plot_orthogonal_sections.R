plot_orthogonal_sections <- function(df,
                                     x0,
                                     y0,
                                     upper = NULL,
                                     option = "inferno") {

  # --- helper: snap to nearest grid line ---
  nearest_level <- function(val, levels) {
    levels[which.min(abs(levels - val))]
  }

  # --- snap requested slice positions ---
  x_use <- nearest_level(x0, sort(unique(df$field_x)))
  y_use <- nearest_level(y0, sort(unique(df$field_y)))

  # --- build sections ---
  sec_xz <- df %>%
    dplyr::filter(field_y == y_use) %>%
    dplyr::mutate(z = -depth)

  sec_yz <- df %>%
    dplyr::filter(field_x == x_use) %>%
    dplyr::mutate(z = -depth)

  # --- colour scale limits ---
  if (is.null(upper)) {
    lims <- range(df$pr_pred, na.rm = TRUE)
    upper <- ceiling(lims[2] * 10) / 10
  }

  # --- map inset (slice location) ---
  p_map <- ggplot(
    df %>% dplyr::filter(depth == min(depth, na.rm = TRUE)),
    aes(field_x, field_y)
  ) +
    geom_point(alpha = 0.05, size = 0.4) +
    geom_vline(xintercept = x_use, linewidth = 0.6) +
    geom_hline(yintercept = y_use, linewidth = 0.6) +
    coord_equal(expand = FALSE) +
    labs(title = "Slice location") +
    theme_bw() +
    theme(
      panel.grid = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      plot.title = element_text(size = 10, face = "bold")
    )

  # --- X–Z section ---
  p_xz <- ggplot(sec_xz, aes(field_x, z, fill = pr_pred)) +
    geom_tile() +
    scale_fill_viridis_c(
      option = option,
      limits = c(0, upper),
      oob = scales::squish,
      name = "PR (MPa)"
    ) +
    labs(
      subtitle = paste0("X–Z slice at Y = ", y_use, " cm"),
      x = "Field X (cm)",
      y = "Depth (cm)"
    ) +
    theme_bw() +
    theme(panel.grid = element_blank())

  # --- Y–Z section ---
  p_yz <- ggplot(sec_yz, aes(field_y, z, fill = pr_pred)) +
    geom_tile() +
    scale_fill_viridis_c(
      option = option,
      limits = c(0, upper),
      oob = scales::squish,
      name = "PR (MPa)"
    ) +
    labs(
      subtitle = paste0("Y–Z slice at X = ", x_use, " cm"),
      x = "Field Y (cm)",
      y = "Depth (cm)"
    ) +
    theme_bw() +
    theme(panel.grid = element_blank())

  # --- assemble layout ---
  (p_map | p_xz) / p_yz +
    patchwork::plot_layout(widths = c(1, 2), guides = "collect") &
    theme(legend.position = "right")
}

