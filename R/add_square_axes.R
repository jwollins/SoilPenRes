add_square_axes <- function(p, data, square_size = 500,
                            x_col = "field_x", y_col = "field_y",
                            x_range = c(0, 2500), y_range = c(0, 2500)) {

  # Grid boundaries (square edges)
  x_lines <- seq(x_range[1], x_range[2], by = square_size)
  y_lines <- seq(y_range[1], y_range[2], by = square_size)

  # Label positions (square centres)
  # Letter order based on what's in your data
  x_sq <- data %>%
    dplyr::distinct(square_letter) %>%
    dplyr::filter(!is.na(square_letter)) %>%
    dplyr::arrange(square_letter) %>%
    dplyr::mutate(i = dplyr::row_number(),
                  x_center = x_range[1] + (i - 0.5) * square_size)

  # Number order based on what's in your data
  y_sq <- data %>%
    dplyr::distinct(square_number) %>%
    dplyr::filter(!is.na(square_number)) %>%
    dplyr::arrange(square_number) %>%
    dplyr::mutate(y_center = y_range[1] + (square_number - 0.5) * square_size)

  p +
    ggplot2::geom_vline(xintercept = x_lines, colour = "white", linewidth = 0.3) +
    ggplot2::geom_hline(yintercept = y_lines, colour = "white", linewidth = 0.3) +
    ggplot2::scale_x_continuous(breaks = x_sq$x_center, labels = x_sq$square_letter) +
    ggplot2::scale_y_continuous(breaks = y_sq$y_center, labels = y_sq$square_number) +
    ggplot2::labs(x = "Square letter", y = "Square number")
}
