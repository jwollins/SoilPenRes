### ERP penetrologger data
### Deployment: June 2024
### J Collins
### 2024-06-22

### 2.1 PACKAGES ####

source(file = "analyses/01_packages.R")

## FUNCTIONS ####

source(file = "R/dms_to_decimal.R")
source(file = "R/to_long.R")


## 02 - Data processing ####

setwd(dir = "~/jcollins@earthroverprogram.org - Google Drive/Shared drives/DATA/FIELD_DEPLOYMENTS/2025-10-UK_ Harper -1HA_survey/soil_data/penetration_res/")





### 2.2 LOAD DATA ####

# Get the file list
filelist <- list.files(pattern = ".*.txt", path = "txt/", full.names = TRUE)

# Create an empty list to store the data frames from each file
file_data <- list()

# Loop through each file in the file list
for (file in filelist) {
  # Read the file using read_delim
  data <- read.delim2(
    file = file,
    header = TRUE,
    sep = "\t",
    dec = ",",
    col.names = c("name", "coordinates", 1:82),
    skip = 13)

  # Store the data in the list
  file_data[[file]] <- data
}

# Now file_data contains the data frames from each file, where the names of the list elements are the file names


### 2.4 SAVE INFO ####

# Create a directory to store the processed files if it doesn't exist
output_dir <- "processed/long_format_data"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

for (i in seq_along(file_data)) {
  csv_file <- file.path(
    output_dir,
    paste0(tools::file_path_sans_ext(basename(filelist[[i]])), "_processed_data.csv")
  )
  write.csv(file_data[[i]], file = csv_file, row.names = FALSE)
  cat("Saved", csv_file, "\n")
}







### 2.3 LOAD INFO ####


# Create an empty list to store the data frames from each file
file_info <- list()

# Loop through each file in the file list
for (file in filelist) {
  # Read the file using read_delim
  info <- read.delim2(
    file = file,
    header = TRUE,
    sep = ":",
    dec = ".",
    col.names = c("name", "data"),
    skip = 2,
    nrows = 9, strip.white = T)

  # Store the data in the list
  file_info[[file]] <- as.data.frame(info)
}



# Now file_data contains the data frames from each file, where the names of the list elements are the file names

# Create a directory to store the processed files if it doesn't exist
output_dir <- "info/"
dir.create(output_dir, showWarnings = FALSE)




### 2.4 SAVE INFO ####

# Loop through the list of dataframes
for (i in seq_along(file_info)) {
  # Save as CSV in specified directory
  csv_file <- paste0(output_dir, basename(gsub('.txt', '', filelist[[i]])), "_processed_info", ".csv")  # Name the CSV file with path
  write.csv(file_info[[i]],
            file = csv_file, row.names = FALSE)  # Save as CSV

  # Print message
  cat("Saved", csv_file, "\n")
}




## add the treatments to the info
# THIS IS WHERE YOU ADD YOUR TREATMENTS

treatment_info <- read.csv("info/treatment_info.csv") %>%
  mutate(id = as.integer(id))

file_data <- map2(file_data, filelist, function(df, fname) {

  id_i <- as.integer(tools::file_path_sans_ext(basename(fname)))
  hit <- treatment_info %>% filter(id == id_i)

  if (nrow(hit) == 0) {
    warning("No treatment match for id: ", id_i)
    return(df)
  }

  df %>%
    mutate(
      id = id_i,
      treatment = hit$treatment[1],
      location = hit$location[1],
      .before = 1
    )
})







### 2.5 DATA PROCESSING ####

# # Create a directory to store the processed files if it doesn't exist
# output_dir <- "penetrologger_data/processed/"
# dir.create(output_dir, showWarnings = FALSE)
#
#
# rbind(file_info[[1]], list("Treatment", "Conservation"))




### 2.51 Coordinates ####

coord_dat <- data.frame()

# Loop through each data frame in data_frames
for (i in seq_along(file_data)) {


  # project name
  file_data[[i]]$project <- file_info[[i]][1,2]

  file_data[[i]] <- file_data[[i]] %>% relocate(project, .after = coordinates)

  # username
  file_data[[i]]$username <- file_info[[i]][2,2]

  file_data[[i]] <- file_data[[i]] %>% relocate(username, .after = project)

  # date
  file_data[[i]]$date <- file_info[[i]][3,2]

  file_data[[i]] <- file_data[[i]] %>% relocate(date, .after = username)

  # pens per plot
  file_data[[i]]$pens_per_plot <- file_info[[i]][4,2]

  file_data[[i]] <- file_data[[i]] %>% relocate(pens_per_plot, .after = date)

  # nr_of_pens_done
  file_data[[i]]$nr_of_pens_done <- file_info[[i]][5,2]

  file_data[[i]] <- file_data[[i]] %>% relocate(nr_of_pens_done, .after = pens_per_plot)

  # cone type
  file_data[[i]]$cone_type <- file_info[[i]][6,2]

  file_data[[i]] <- file_data[[i]] %>% relocate(cone_type, .after = nr_of_pens_done)

  # pen speed
  file_data[[i]]$pen_speed <- file_info[[i]][7,2]

  file_data[[i]] <- file_data[[i]] %>% relocate(pen_speed, .after = cone_type)

  # depth unit
  file_data[[i]]$depth_unit <- file_info[[i]][8,2]

  file_data[[i]] <- file_data[[i]] %>% relocate(depth_unit, .after = pen_speed)

  # pressure unit
  file_data[[i]]$pressure_unit <- file_info[[i]][9,2]

  file_data[[i]] <- file_data[[i]] %>% relocate(pressure_unit, .after = depth_unit)


 #  # bind the name and coordinate rows from the df list
  coord_dat <-  rbind(coord_dat, file_data[[i]][,c(1:11)])

}






# Extract and convert coordinates
coord_dat <- coord_dat %>%
  mutate(
    lat_deg = as.numeric(str_extract(coordinates, "(?<=N|S)\\d+")),
    lat_min = as.numeric(str_extract(coordinates, "(?<=N\\d{2} )\\d+\\.\\d+")),
    lon_deg = as.numeric(str_extract(coordinates, "(?<=W|E)\\d+")),
    lon_min = as.numeric(str_extract(coordinates, "(?<=W\\d{3} )\\d+\\.\\d+")),
    lat_dir = str_extract(coordinates, "^[NS]"),
    lon_dir = str_extract(coordinates, "[WE]$"),
    latitude = mapply(dms_to_decimal, lat_deg, lat_min, lat_dir),
    longitude = mapply(dms_to_decimal, lon_deg, lon_min, lon_dir)
  )



# Extract plot number and create new column
coord_dat <- coord_dat %>%
  mutate(plot_number = as.numeric(str_extract(name, "(?<=PLOT-)\\d+\\.\\d+")))

# change crs
coord_dat$longitude = coord_dat$longitude*(-1)


# Save as CSV in specified directory
write.csv(x =  coord_dat,
          file = "info/meta_info.csv",
          row.names = FALSE)











### DATA ####


glimpse(file_data[1])


# Create a directory to store the processed files if it doesn't exist
output_dir <- "processed/"
#dir.create(output_dir, showWarnings = FALSE)



# Loop through each data frame in data_frames
for (i in seq_along(file_data)) {

  # from X1 onwards
  file_data[[i]] <- file_data[[i]] %>% select(X1:last_col())

  file_data[[i]] <- file_data[[i]] %>% mutate_if(is.character, as.numeric)

  # transpose the df
  file_data[[i]] <- as.data.frame(t(x = file_data[[i]]))

  # add the mean row
  file_data[[i]]$mean <- rowMeans(file_data[[i]], na.rm = FALSE)


  samples <- ncol(file_data[[i]]) - 1

  #calculate standard deviation of each row
  file_data[[i]]$stdev <- apply(file_data[[i]][,1:samples],
                                MARGIN = 1,
                                FUN = sd,
                                na.rm = TRUE)

  # Calculate standard error of the mean for each row
  file_data[[i]]$sem <- apply(file_data[[i]][,1: samples],
                            MARGIN = 1,
                            FUN = function(x) sd(x, na.rm=TRUE) / sqrt(sum(!is.na(x))))


  # create depth df
  depth <- data.frame(depth_cm = seq_len(nrow(file_data[[i]])))


  #bind both df's
  file_data[[i]] <- cbind(depth, file_data[[i]])

  # add project name
  file_data[[i]]$name <- file_info[[i]]$data[1]


  # Save as CSV in specified directory
  csv_file <- paste0(output_dir, basename(gsub('.txt', '', filelist[i])), "_processed", ".csv")  # Name the CSV file with path

  write.csv(file_data[[i]], file = csv_file, row.names = FALSE)  # Save as CSV

  # Print message
  cat("Saved", csv_file, "\n")

}








## 2.7 Long format processing (clean)

# ---- Inputs / outputs ----
in_dir  <- "processed/long_format_data"
out_dir <- "processed/long_format_data"
out_file <- file.path(out_dir, "long_format_data.csv")

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)



# ---- Read all files + convert to long + bind ----
filelist <- list.files(in_dir, pattern = "\\.csv$", full.names = TRUE)

long_data <- filelist %>%
  map(~ read.csv(.x)) %>%
  map(to_long) %>%
  bind_rows()

# ---- Parse coordinates -> decimal degrees ----

coord_pat <- "^([NS])(\\d+)\\s+(\\d+\\.?\\d*)\\s+([WE])(\\d+)\\s+(\\d+\\.?\\d*)$"

long_data <- long_data %>%
  mutate(
    # pull all parts at once
    .m = str_match(coordinates, coord_pat),
    lat_dir = .m[, 2],
    lat_deg = as.numeric(.m[, 3]),
    lat_min = as.numeric(.m[, 4]),
    lon_dir = .m[, 5],
    lon_deg = as.numeric(.m[, 6]),
    lon_min = as.numeric(.m[, 7]),

    latitude  = mapply(dms_to_decimal, lat_deg, lat_min, lat_dir),
    longitude = mapply(dms_to_decimal, lon_deg, lon_min, lon_dir)
  ) %>%
  select(-.m)

# ---- Extract plot + replicate from name ----
long_data <- long_data %>%
  mutate(
    plot_number      = str_match(name, "^PLOT-(\\d{3})\\.")[, 2],
    replicate_number = str_match(name, "^PLOT-\\d{3}\\.(\\d+)")[, 2]
  )

# ---- Save ----
write.csv(long_data, out_file, row.names = FALSE)
cat("Saved", out_file, "\n")






# ## 2.8 Bind data processing ####
#
# # Get the file list
# filelist <- list.files(pattern = ".*.csv",
#                        path = "penetrologger_data/processed/long_format_data/",
#                        full.names = TRUE)
#
# # Create an empty list to store the data frames from each file
# file_data <- list()
#
# # Loop through each file in the file list
# for (file in filelist) {
#   # Read the file using read_delim
#   data <- read.csv(file = file)
#
#   # Store the data in the list
#   file_data[[file]] <- data
# }
#
# # # Column bind all data frames together
#  combined_df <- do.call(rbind, file_data)
#
#
#
# # infodat <- read.csv(file = "penetrologger_data/info/24_10_info.csv")
# #
# # combined_info_dat <- cbind(infodat, combined_df)
# #
# # write_csv(x = combined_info_dat,
# #           file = "data/processed/combined_info_dat.csv")
#
#
#
# ## transpose it
#
#
# # Define the transpose function
# transpose_data <- function(df) {
#
#   # Identify the columns that are not depth-related
#   non_depth_cols <- colnames(df)[!grepl("^X", colnames(df))]
#
#   # Melt the dataframe to convert depth columns into long format
#   df_melted <- pivot_longer(df,
#                             cols = starts_with("X"),  # Select depth columns (X1 to X82)
#                             names_to = "Depth_cm",    # New column for depth
#                             values_to = "Measurement") # New column for measurement values
#
#   # Convert Depth_cm from 'X1', 'X2', ... 'X82' to numeric values (1 to 82)
#   df_melted$Depth_cm <- as.numeric(sub("X", "", df_melted$Depth_cm))
#
#   # Return the long-format dataframe
#   return(df_melted)
# }
#
# # Apply the function to your dataframe
# long_test <- transpose_data(combined_info_dat)
#
# # Replace negative values in the Measurement column with 0
# long_test <- long_test %>%
#   mutate(Measurement = ifelse(Measurement < 0, 0, Measurement))
#
#
# # Extract the plot number and replicate number from the 'name' column
# long_test <- long_test %>%
#   mutate(plot_number = sub("PLOT-(\\d{3})\\..*", "\\1", name),
#          replicate_number = sub("PLOT-\\d{3}\\.(\\d)", "\\1", name))
#
# # add treatment
#
# # Corrected code to create the 'treatment' column
# long_test$treatment <- ifelse(test = long_test$project %in% c(24060705, 24060706, 24060707),
#                               yes = "Conservation",
#                               no = "Conventional")
#
# long_test$treatment <- ifelse(test = long_test$project %in% c(24053102),
#                               yes = "Soil Hall",
#                               no = long_test$treatment)
#
#
#
#
# write.csv(x = long_test, file = "data/processed/long_format_data/combined_data_info.csv")
#

