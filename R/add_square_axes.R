add_square_axes <- function(p,
                            square_size = 500,
                            origin_x = 0, origin_y = 0,
                            n_cols = 20, n_rows = 20,
                            flip_rows = TRUE) {

  # Grid lines for the whole hectare
  x_lines <- origin_x + (0:n_cols) * square_size
  y_lines <- origin_y + (0:n_rows) * square_size

  # Label centers
  x_centers <- origin_x + (1:n_cols - 0.5) * square_size
  x_labels  <- LETTERS[1:n_cols]

  y_centers <- origin_y + (1:n_rows - 0.5) * square_size
  y_labels  <- if (flip_rows) rev(1:n_rows) else 1:n_rows

  p +
    ggplot2::geom_vline(xintercept = x_lines, colour = "white", linewidth = 0.3) +
    ggplot2::geom_hline(yintercept = y_lines, colour = "white", linewidth = 0.3) +
    ggplot2::scale_x_continuous(breaks = x_centers, labels = x_labels) +
    ggplot2::scale_y_continuous(breaks = y_centers, labels = y_labels) +
    ggplot2::labs(x = "Square letter", y = "Square number")
}
