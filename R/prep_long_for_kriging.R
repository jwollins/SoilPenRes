prep_long_for_kriging <- function(long_data, local_coords, id_i,
                                  square_size = 500,
                                  n_rows = 20,
                                  flip_rows = TRUE) {

  ld <- long_data %>%
    dplyr::mutate(
      id = as.integer(id),
      depth = as.numeric(depth),
      penetration_resistance = as.numeric(penetration_resistance),
      plot_number = as.integer(plot_number),
      replicate_number = as.integer(replicate_number)
    ) %>%
    dplyr::filter(id == id_i)

  lc <- local_coords %>%
    dplyr::mutate(
      id = as.integer(id),
      plot = as.integer(plot),
      replicate_number = as.integer(replicate_number),
      square_number = as.integer(square_number),
      square_letter = toupper(as.character(square_letter)),
      local_x = as.numeric(local_x),
      local_y = as.numeric(local_y)
    )

  ld2 <- ld %>%
    dplyr::left_join(
      lc %>%
        dplyr::select(id, plot, replicate_number, local_x, local_y, square_number, square_letter),
      by = c("id" = "id", "plot_number" = "plot", "replicate_number" = "replicate_number")
    )

  # ---- GLOBAL hectare mapping ----
  letter_levels <- LETTERS[1:20]  # A:T

  ld2 <- ld2 %>%
    dplyr::mutate(
      square_row = as.integer(square_number),
      square_col = match(square_letter, letter_levels),
      square_row_use = if (flip_rows) (n_rows - square_row + 1) else square_row,
      field_x = (square_col - 1) * square_size + local_x,
      field_y = (square_row_use - 1) * square_size + local_y
    )

  # sanity: no row duplication from join
  stopifnot(nrow(ld2) == nrow(ld))

  # sanity: mapping valid
  stopifnot(all(ld2$square_letter %in% letter_levels))
  stopifnot(all(ld2$square_row %in% 1:n_rows))
  stopifnot(!anyNA(ld2$square_col))

  ld2
}
