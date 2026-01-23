# ---- Helper: wide -> long ----
to_long <- function(df) {
  df %>%
    mutate(across(starts_with("X"), ~ as.numeric(trimws(as.character(.))))) %>%
    pivot_longer(
      cols = starts_with("X"),
      names_to = "depth",
      names_prefix = "X",
      values_to = "penetration_resistance"
    ) %>%
    mutate(depth = as.numeric(depth)) %>%
    arrange(id, name, coordinates, depth)
}
