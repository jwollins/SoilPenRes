### ERP penetrologger data
### Deployment: June 2024
### J Collins
### 2024-06-22


### 01 - Packages required

## 01.1 PACKAGES ####

suppressPackageStartupMessages({
  if (!require(dplyr)) install.packages("dplyr")
  if (!require(ggplot2)) install.packages("ggplot2")
  if (!require(ggpubr)) install.packages("ggpubr")
  if (!require(gridExtra)) install.packages("gridExtra")
  if (!require(readxl)) install.packages("readxl")
  if (!require(readr)) install.packages("readr")
  if (!require(plotrix)) install.packages("plotrix")
  if (!require(lmerTest)) install.packages("lmerTest")
  if (!require(lmerTest)) install.packages("stringr")
  if (!require(lmerTest)) install.packages("tidyverse")
  if (!require(lmerTest)) install.packages("tibble")
  if (!require(lmerTest)) install.packages("sf")
  if (!require(lmerTest)) install.packages("ggspatial")
  if (!require(lmerTest)) install.packages("tidyr")



  library(dplyr) # for "glimpse" and data manipulation
  library(ggplot2) # general plotting
  library(ggpubr) # custom plotting
  library(gridExtra) # grid plotting
  library(readxl) # read .xlsx files
  library(readr) # read .txt files
  library(plotrix) # standard error
  library(lmerTest) # linear mixed effect models
  library(stringr) # subsetting dataframes
  library(tidyverse)
  library(tibble) # add columns in specific positions
  library(sf) # read and plot shapefiles
  library(ggspatial) # annotate north arrow
  library(tidyr) # transpose dataframes
  library(purrr)
})

