### ERP penetrologger data
### Plotting script
### J Collins

# ---- Packages and functions ----
source(here::here("analyses", "01_packages.R"))
source(here::here("config.R"))
source(here::here("R", "analyze_plot_3d_kriging.R"))
source(here::here("R", "prep_long_for_kriging.R"))
source(here::here("R", "add_square_axes.R"))
source(here::here("R", "make_bbox.R"))
source(here::here("R", "snap_it.R"))



# ---- Config ----
ID_I <- 26020900
# ID_I <- c(26020200, 26020201, 26020202)   # <- multiple IDs


DEPTHS <- c(10, 20, 30, 40, 50, 60, 70, 80)

SQUARE_SIZE <- 500     # cm
N_ROWS <- 20
FLIP_ROWS <- FALSE     # IMPORTANT: keep consistent with your field convention

GRID_STEP <- 100       # cm (1 m pixels)
BUFFER <- 250          # cm around points

HECTARE_MIN <- 0
HECTARE_MAX <- N_ROWS * SQUARE_SIZE  # 10000 cm




# ---- Load ----
long_data <- read.csv(file.path(LONG_DIR, "long_format_data.csv"))
local_coords <- read.csv(file.path(INFO_DIR, "local_coords2.csv"))



# ---- Type cleanup (only what you actually use downstream) ----
long_data <- long_data %>%
  mutate(
    id = as.integer(id),
    depth = as.numeric(depth),
    penetration_resistance = as.numeric(penetration_resistance),
    plot_number = as.integer(plot_number),
    replicate_number = as.integer(replicate_number),
    treatment = factor(treatment),
    location = factor(location)
  )

local_coords <- local_coords %>%
  mutate(
    id = as.integer(id),
    plot = as.integer(plot),
    replicate_number = as.integer(replicate_number),
    square_number = as.integer(square_number),
    square_letter = toupper(as.character(square_letter)),
    local_x = as.numeric(local_x),
    local_y = as.numeric(local_y)
  )

















# ---- Profile plots ----


ids_to_drop <- c(
  26012230,26012231,26012232,26012233,26012234,
  26012235,26012236,26012237,26012238,260209001,260209002,260209011,260211021
)

library(dplyr)
library(ggplot2)

library(dplyr)

make_profile_summary <- function(long_df, ids_to_drop = NULL) {

  out <- long_df %>%
    filter(!(id %in% ids_to_drop)) %>%
    group_by(id, treatment, location, depth) %>%
    summarise(
      mean  = mean(penetration_resistance, na.rm = TRUE),
      stdev = sd(penetration_resistance, na.rm = TRUE),
      n     = sum(!is.na(penetration_resistance)),
      sem   = ifelse(n >= 2, stdev / sqrt(n), NA_real_),
      .groups = "drop"
    ) %>%
    # drop anything that can't be plotted sensibly
    filter(n > 0) %>%
    mutate(
      mean = ifelse(is.finite(mean), mean, NA_real_),
      sem  = ifelse(is.finite(sem),  sem,  NA_real_)
    ) %>%
    filter(!is.na(mean)) %>%
    arrange(id, treatment, location, depth)

  out
}


summary_profiles <- make_profile_summary(long_data, ids_to_drop)

summary_profiles %>%
  summarise(
    rows = n(),
    any_na_mean = any(is.na(mean)),
    any_nan_mean = any(is.nan(mean)),
    any_inf_mean = any(is.infinite(mean)),
    any_na_depth = any(is.na(depth)),
    any_na_sem = any(is.na(sem))
  )

summary_profiles %>% filter(is.na(mean) | is.nan(mean) | is.infinite(mean) | is.na(depth)) %>% head()


library(ggplot2)

plot_profiles <- function(summary_profiles) {

  ggplot(summary_profiles,
         aes(x = mean, y = depth,
             group = interaction(id, treatment, location),
             colour = treatment)) +

    geom_errorbarh(
      data = dplyr::filter(summary_profiles, !is.na(sem)),
      aes(xmin = mean - sem, xmax = mean + sem),
      height = 0.5, alpha = 0.15, na.rm = TRUE
    ) +

    geom_path(alpha = 0.35, linewidth = 0.7, na.rm = TRUE) +
    scale_y_reverse() +
    labs(
      x = "Penetration resistance (MPa)",
      y = "Depth (cm)",
      colour = "Treatment",
      title = "Mean penetration resistance profiles (all IDs)"
    ) +
    theme_bw()
}



summary_profiles <- make_profile_summary(long_data, ids_to_drop)
p_profiles <- plot_profiles(summary_profiles)
p_profiles


# How many distinct groups are you plotting?
dplyr::n_distinct(interaction(summary_profiles$id,
                              summary_profiles$treatment,
                              summary_profiles$location))

# Check if any group has missing depths (gaps)
summary_profiles %>%
  group_by(id, treatment, location) %>%
  summarise(n_depths = n_distinct(depth),
            min_depth = min(depth),
            max_depth = max(depth),
            .groups="drop") %>%
  arrange(n_depths) %>%
  head(20)












# --- Interactive 3D plot ----


long_ready <- prep_long_for_kriging(
  long_data, local_coords, id_i = ID_I,
  square_size = SQUARE_SIZE,
  n_rows = N_ROWS,
  flip_rows = FLIP_ROWS
)

stopifnot(all(c("local_x","local_y","field_x","field_y") %in% names(long_ready)))

df_all <- long_ready %>%
  mutate(
    z = -depth,
    profile_id = interaction(plot_number, replicate_number, square_letter, square_number, name, drop = TRUE)
  ) %>%
  arrange(profile_id, depth)

p3d <- plot_ly(
  df_all,
  x = ~field_x, y = ~field_y, z = ~z,
  color = ~penetration_resistance,
  split = ~profile_id,
  type = "scatter3d",
  mode = "lines",
  line = list(width = 3),
  text = ~paste(
    "Plot:", plot_number,
    "<br>Square:", square_number, square_letter,
    "<br>Pen:", replicate_number,
    "<br>Depth:", depth,
    "<br>PR:", penetration_resistance
  )
) %>%
  layout(
    showlegend = FALSE,
    scene = list(
      xaxis = list(title = "Field X (cm)"),
      yaxis = list(title = "Field Y (cm)"),
      zaxis = list(title = "Depth (cm)"),
      aspectmode = "data"
    )
  )

p3d




# install.packages(c("htmlwidgets", "webshot2", "magick"))
# webshot2 needs Chrome/Chromium available



# ---- make the GIF ----

out_dir <- file.path(FIG_DIR, "spin_frames")
dir.create(out_dir, showWarnings = FALSE)

png_files <- character(n)

for (i in seq_along(angles)) {
  a <- angles[i]

  p_i <- p3d %>%
    layout(scene = list(
      camera = list(eye = list(
        x = r * cos(a),
        y = r * sin(a),
        z = z
      ))
    ))

  html_file <- tempfile(fileext = ".html")
  saveWidget(p_i, html_file, selfcontained = TRUE)

  png_files[i] <- file.path(out_dir, sprintf("frame_%03d.png", i))
  webshot(
    html_file,
    file = png_files[i],
    vwidth = 900,
    vheight = 700,
    delay = 0.2
  )
}

gif_path <- file.path(FIG_DIR, "penetration_resistance_3d_spin.gif")

image_read(png_files) |>
  image_animate(fps = 10) |>
  image_write(gif_path)

gif_path






# --- P wave velocity ####


## ---- data ----

pvel_dat <- read.csv(file = file.path(INFO_DIR, "20A_1R_profiles.csv"))

summary_profiles <- read.csv(file.path(SUM_DIR, "summary_profiles.csv"))


## ---- filter ----

pr_dat_20A_1R <- dplyr::filter(summary_profiles, id == "26012238" | id == "26012232")


pr_dat_20A_1R$tile <- if_else(condition = pr_dat_20A_1R$id == "26012238",
                              true = "20A",  false = "1R")


glimpse(pvel_dat)
glimpse(pr_dat_20A_1R)


# pvel_dat <- pvel_dat %>%
#   mutate(depth = abs(z) * 100)

# y_limits <- range(
#   c(pr_dat_20A_1R$depth, pvel_dat$depth),
#   na.rm = TRUE
# )




pr_plot <-
  ggplot(pr_dat_20A_1R,
         aes(x = mean, y = depth, group = tile, colour = tile)) +
  geom_errorbarh(aes(xmin = mean - sem, xmax = mean + sem),
                 height = 0.8, alpha = 0.3) +
  geom_path(linewidth = 1) +
  scale_y_reverse(limits = c(90, 0), expand = c(0, 0)) +
  labs(
    x = "Penetration resistance (MPa)",
    y = "Depth (cm)",
    colour = "Treatment",
    # title = "Mean penetration resistance (± SE)"
    title = expression("Mean PR (± SE)")
  ) +
  theme_bw()


pr_plot


names(pvel_dat)

glimpse(pvel_dat)

pvel_dat <- pvel_dat %>%
  mutate(
    depth = abs(z) * 100,
    se = velocity_xy_std / sqrt(16)
  )

pwv_plot <-
  ggplot(pvel_dat,
         aes(x = velocity_xy_mean, y = depth, colour = tile)) +
  geom_errorbarh(
    aes(
      xmin = velocity_xy_mean - se,
      xmax = velocity_xy_mean + se
    ),
    height = 1,
    alpha = 0.3
  ) +
  geom_path(linewidth = 1) +
  scale_y_reverse(limits = c(90, 0), expand = c(0, 0)) +
  labs(
    x = "P Wave Velocity (m/s)",
    y = "Depth (cm)",
    colour = "Treatment",
    # title = "Mean P wave velocity (± SE)"
    title = expression("Mean " * italic(v[p]) * " (± SE)")
  ) +
  theme_bw()


pwv_plot


ggarrange(pr_plot, pwv_plot, common.legend = TRUE, legend = "bottom")








pwv_plot_median <-
  ggplot(pvel_dat,
         aes(x = velocity_xy_median, y = depth, colour = tile)) +
  geom_errorbarh(
    aes(
      xmin = velocity_xy_median - se,
      xmax = velocity_xy_median + se
    ),
    height = 1,
    alpha = 0.3
  ) +
  geom_path(linewidth = 1) +
  scale_y_reverse(limits = c(90, 0), expand = c(0, 0)) +
  labs(
    x = "P Wave Velocity (m/s)",
    y = "Depth (cm)",
    colour = "Treatment",
    title = expression("Median " * italic(v[p]) * " (± SE)")
  ) +
  theme_bw()

pwv_plot_median



# ---- Correlations ----

# Join the datasets
summary_combined <- inner_join(pr_dat_20A_1R, pvel_dat,
                               by = c("tile", "depth")
)


cor_plot <-
ggplot(
  data = summary_combined,
  aes(x = mean, y = velocity_xy_mean, color = tile)
) +
  geom_point(size = 1.5) +
  geom_smooth(method = "lm", se = TRUE) +
  stat_cor(
    data = subset(summary_combined, tile == "1R"),
    label.x = 1.2, label.y = 750, color = "turquoise3", show.legend = FALSE
  ) +
  stat_cor(
    data = subset(summary_combined, tile == "20A"),
    label.x = 1.2, label.y = 800, color = "tomato2", show.legend = FALSE
  ) +
  # ylim(100, 800) +
  labs(
    x = "Penetration resistance (MPa)",
    y = expression("P Wave Velocity (m" ~ s^{
      -1
    } ~ ")"),
    title = expression("Correlation: " * italic(v[p]) * " vs PR")
  ) +
  # scale_color_manual(
  #   name = "Treatment",
  #   values = c("turquoise3", "tomato2"),
  #   labels = c("Conservation", "Conventional")
  # ) +
  theme_bw() +
  theme(legend.position = "bottom")



ggarrange(pr_plot, pwv_plot, cor_plot,
          common.legend = TRUE, legend = "bottom", ncol = 3)


ggsave(filename = file.path(FIG_DIR, "pwave_cor_plot.png"),
       width = 12, height = 6)










cor_plot_median <-
  ggplot(
    data = summary_combined,
    aes(x = mean, y = velocity_xy_median, color = tile)
  ) +
  geom_point(size = 1.5) +
  geom_smooth(method = "lm", se = TRUE) +
  stat_cor(
    data = subset(summary_combined, tile == "1R"),
    label.x = 1.2, label.y = 750, color = "turquoise3", show.legend = FALSE
  ) +
  stat_cor(
    data = subset(summary_combined, tile == "20A"),
    label.x = 1.2, label.y = 800, color = "tomato2", show.legend = FALSE
  ) +
  # ylim(100, 800) +
  labs(
    x = "Penetration resistance (MPa)",
    y = expression("P Wave Velocity (m" ~ s^{
      -1
    } ~ ")"),
    title = expression("Correlation: " * italic(v[p]) * " vs PR")
  ) +
  # scale_color_manual(
  #   name = "Treatment",
  #   values = c("turquoise3", "tomato2"),
  #   labels = c("Conservation", "Conventional")
  # ) +
  theme_bw() +
  theme(legend.position = "bottom")




ggarrange(pr_plot, pwv_plot_median, cor_plot_median,
          common.legend = TRUE, legend = "bottom", ncol = 3)


ggsave(filename = file.path(FIG_DIR, "pwave_cor_plot_median.png"),
       width = 12, height = 6)




















