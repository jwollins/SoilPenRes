### ERP penetrologger data
### Deployment: June 2024
### J Collins 
### 2024-06-22


## 03 - Plots ####

## 3.1 PACKAGES ####

setwd(dir = "~/Documents/ERP/scripts/erpsoiltools/")

source(file = "penetration_res/01_packages.R")

## 3.2 LOAD DATA ####

source("penetration_res/02_data.R")


## 3.3 PLOTS ####

setwd(dir = "~/jcollins@earthroverprogram.org - Google Drive/Shared drives/DATA/FIELD_DEPLOYMENTS/2024-10-Joesfield/soil_data/")



### 3.31 penetration plots ####

# loop through all files and plot
for (i in seq_along(file_data)) {
  # Basic line plot with points
  p <- ggplot(data = file_data[[i]], 
              mapping = aes(x = mean, 
                            y = depth_cm)) + 
    scale_y_reverse() + # reverse the y axis
    geom_ribbon(aes(x = mean, 
                    xmin = mean - sem, 
                    xmax = mean + sem,
                    fill = "stdev"), 
                show.legend = TRUE) + 
    coord_cartesian(xlim = c(0, 4)) + # zoom to specified xlims
    geom_point() +
    geom_path() + # plot in order of data not in order of x axis
    labs(title = paste0(basename(gsub('.txt', '', filelist[[i]])), "_plot"),
         subtitle = "Deployment June 2024",
         x = "Penetrometer Resistance (MPa)",
         y = "Depth (cm)",
         caption = paste(file_info[[i]]$name[3], ":" , file_info[[i]]$data[3], ",",
                                     file_info[[i]]$name[4], ":" , file_info[[i]]$data[4], ",",
                                     file_info[[i]]$name[5], ":" , file_info[[i]]$data[5], ",",
                                     file_info[[i]]$name[6], ":" , file_info[[i]]$data[6], ",",
                                     file_info[[i]]$name[10], ":" , file_info[[i]]$data[10])) +
  theme(plot.title = element_text(size = 14, face = "bold"),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"),
        plot.caption = element_text(color = "black", 
                                    face = "italic", 
                                    size = 8),
        legend.position = "bottom", 
        panel.background = element_rect(fill = "white", 
                                        colour = "black", 
                                        size = 1, 
                                        linetype = 1,
                                        color = "black")) +
    scale_fill_manual(name = element_blank(),
                      values = "grey",
                      labels = c("Standard Error of Mean")) 

# Save the plot
ggsave(filename = paste0(basename(gsub('.txt', '', filelist[[i]])), "_plot", ".png"),
              path = "plots/", 
       plot = p, 
       width = 8, 
       height = 6)
}




### 3.32 location plots ####

library(ggrepel)

location_plot <- ggplot() + 
  geom_sf(data = bound.plots, 
          aes(fill = Treatment)) + 
  scale_fill_manual(values=c(alpha("turquoise3",alpha = 0.25), 
                             alpha("tomato2",alpha = 0.25), 
                             alpha("grey",alpha = 0.1))) +
  geom_point(data = coord_dat, 
             size = 1, 
             stroke = 1,
             aes(x = longitude, 
                 y = latitude,
                 color = project)) + 
  geom_label_repel(data = coord_dat, 
                   size = 2.5,
                   aes(x = longitude, 
                       y = latitude, 
                       label = plot_number),
                   box.padding   = 0.3, 
                   point.padding = 0.1,
                   segment.color = 'black') +
  geom_point(data = stryde_positions, 
             size = 2, 
             shape = 0,
             aes(x = longitude, 
                 y = latitude, 
                 color = "Stryde Positions", 
                 alpha = 0.5)) + 
  scale_color_manual(name = "Points", 
                     values = c("24060703" = "red", 
                                "24060704" = "blue",
                                "24060705" = "darkorange",
                                "24060706" = "darkgreen",
                                "24060707" = "yellow",
                                "Stryde Positions" = "black")) +
  theme(plot.title = element_text(size = 14, face = "bold"),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"),
        legend.position="bottom", 
        legend.box = "vertical") +
  labs(title = "ERP Deployment June 2024",
       subtitle = "Penetrologger and stryde positions", 
       y = "Latitude", 
       x = "Longitude") + 
  annotation_north_arrow(
    location = "tl",
    pad_x = unit(0.5, "in"),
    pad_y = unit(0.25, "in"),
    style = north_arrow_fancy_orienteering) +
  annotation_scale(
    location = "br",
    pad_x = unit(1, "cm"),
    pad_y = unit(1, "cm")) +
  coord_sf(xlim = c(-2.60574, -2.60625), 
           ylim = c(52.91585, 52.91615),
           crs = 4326) 

location_plot

# Save the plot
ggsave(filename = "location_plot.png",
       path = "plots/", 
       plot = location_plot, 
       width = 8, 
       height = 8)





### 3.3 3D plot ####


install.packages("plot3D")
library(plot3D)



long_data <- read.csv(file = "data/processed/long_format_data.csv")

# change crs 
long_data$longitude = long_data$longitude*(-1)

stryde <- read.csv(file = "data/processed/stryde_positions_crs_4326.csv")


# x, y and z coordinates
x <- lon <- long_data$longitude
y <- lat <- long_data$latitude
z <- depth <- long_data$depth
p <- penres <- long_data$penetration_resistance


pr <- penres <- factor(round(long_data$penetration_resistance))



x1 <- long1 <- stryde$longitude
y1 <- lat1 <- stryde$latitude
z1 <- depth1 <- stryde$depth

scatter3D(x1, y1, z1, 
          ticktype = "detailed",
          clab = c("Depth (cm)"), pch = 1, cex = 1, col = "black")

scatter3D(x, y, z, 
          colvar = p, 
          ticktype = "detailed",
          clab = c("Penetration", "resistance", "MPa"), 
          main = "ERP Deployment 06/2024", 
          xlab = "Longitude",
          ylab ="Latitude", 
          zlab = "Depth (cm)")



# Add another point (black color)
scatter3D(x = 2.606, y = 52.916, z = 90, add = TRUE, colkey = FALSE, 
          pch = 18, cex = 3, col = "black")




png(file = "plots/4d_plot.png",
    width = 800, height = 800, res = 100)

s3d <-scatter3D(y, x, -z, 
          colvar = p, 
          ticktype = "detailed",
          clab = c("Penetration", "resistance", "MPa"), 
          main = "ERP Deployment 06/2024", 
          xlab = "Longitude",
          ylab ="Latitude", 
          zlab = "Depth (cm)", 
          bty ="b2", 
          theta = 120,
          zlim = c(-90, 0))


# Add another point (black color)
scatter3D(x = y1, y = x1, z = z1, add = TRUE, colkey = F, labels = "Strydes",
          pch = 18, cex = 1, col = "black")

# Add text
text3D(x = 52.916, y = -2.6065, z = 0,  labels = "Stryde locations",
       add = TRUE, cex = 1)

dev.off()




### 2d plot ####


# Load the data
data <- read.csv(file = "penetrologger_data/processed/long_format_data/long_format_data.csv")

# Create the plot
ggplot(data, aes(x = replicate_number, 
                 y = depth, 
                 color = penetration_resistance)) +
  geom_point() +
  scale_y_reverse() + # Inverting y-axis since depth typically increases downward
  scale_color_brewer(palette = "RdYlGn", direction = -1) + # Discrete colors  facet_wrap(~ project) +
  labs(
    title = "Penetration Resistance by Plot",
    x = "Stryde Position",
    y = "Depth",
    color = "Penetration Resistance"
  ) +
  theme_minimal()



ggsave(filename = "p_res_by_plot.png", path = "plots/")




# Create the plot
ggplot(data, aes(x = replicate_number, 
                 y = Depth_cm, 
                 color = factor(round(Measurement)))) +  # Round Measurement to nearest whole number and convert to factor
  geom_point() +
  scale_y_reverse() + # Inverting y-axis since depth typically increases downward
  scale_color_brewer(palette = "RdYlGn", direction = -1) + # Discrete color scale
  facet_wrap(~ project + treatment) +
  labs(
    title = "Penetration Resistance by Experiment",
    x = "Stryde Position",
    y = "Depth",
    color = "Penetration Resistance (Rounded)") + 
  theme(legend.position="bottom") +
  theme_minimal()

ggsave(filename = "p_res_by_plot.png", path = "plots/")

