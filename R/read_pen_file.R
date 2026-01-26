read_pen_file <- function(path) {

  id_i <- id_from_file(path)

  # info block (top of file)
  info <- read.delim2(
    file = path,
    header = TRUE,
    sep = ":",
    dec = ".",
    col.names = c("name", "data"),
    skip = 2,
    nrows = 9,
    strip.white = TRUE
  )

  # turn into a named list for easy access
  info_vals <- setNames(as.list(info$data), info$name)

  # measurement block (lower part of file)
  dat <- read.delim2(
    file = path,
    header = TRUE,
    sep = "\t",
    dec = ",",
    col.names = c("name", "coordinates", paste0("X", 1:82)),
    skip = 13
  )

  # join treatment/location from lookup table
  hit <- treatment_info %>% filter(id == id_i)
  if (nrow(hit) == 0) {
    warning("No treatment match for id: ", id_i)
    hit <- tibble(id = id_i, treatment = NA, location = NA)
  }

  # attach metadata columns (constant per file)
  dat <- dat %>%
    mutate(
      id = id_i,
      treatment = hit$treatment[1],
      location = hit$location[1],

      project        = info_vals[["Project"]]        %||% info_vals[[1]],
      username       = info_vals[["User name"]]      %||% info_vals[[2]],
      date           = info_vals[["Date"]]           %||% info_vals[[3]],
      pens_per_plot  = info_vals[["Pens per plot"]]  %||% info_vals[[4]],
      nr_of_pens_done= info_vals[["Nr. of pens done"]] %||% info_vals[[5]],
      cone_type      = info_vals[["Cone type"]]      %||% info_vals[[6]],
      pen_speed      = info_vals[["Pen speed"]]      %||% info_vals[[7]],
      depth_unit     = info_vals[["Depth unit"]]     %||% info_vals[[8]],
      pressure_unit  = info_vals[["Pressure unit"]]  %||% info_vals[[9]]
    ) %>%
    relocate(id, treatment, location, name, coordinates, project, username, date,
             pens_per_plot, nr_of_pens_done, cone_type, pen_speed, depth_unit, pressure_unit)

  list(id = id_i, info = info, data = dat, file = path)
}
