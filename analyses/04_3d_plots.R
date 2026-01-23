### ERP penetrologger data
### Deployment: June 2024
### J Collins 
### 2024-06-22


## 3D scatter data ####

info_dat <- read.csv(file = "data/processed/processed_info/2024_06_info.csv")

info_dat <- filter(.data = info_dat, .by =  info_dat$project = 24060703)

# Filter the data based on a criteria of a certain column
info_dat <- subset(info_dat, info_dat$project == 24060703)


p24060703 <- read.csv(file = "data/processed/processed_data/24060703_processed.csv")

p24060703 <- p24060703[,c(2 : 15)]

rep1$sample <- 1
rep1$longitude <- info_dat$longitude[11]
rep1$latitude <- info_dat$latitude[11]


mylist <- list()

for (i in 1:ncol(p24060703)) {
  mylist[i] <- data.frame(p24060703[,i])
  
  mylist[i]$depth <- 1:82
}
