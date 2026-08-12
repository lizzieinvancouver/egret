# The aim of this code is to download climate data from ERA5-Land
# started by Deirdre Loughnan May 2026

# install.packages("ecmwfr")

rm(list=ls()) 
options(stringsAsFactors = FALSE)

# setwd("~/Documents/github/egret/analyses")
setwd("/home/deirdre/egret/era5landDaily")
library(ecmwfr)
library(terra)
library(lubridate)
# library(purrr)
# library(dplyr)

d <- read.csv("..//egretclean.csv")
# d <- read.csv("output/egretclean.csv")

dGeog <- unique(d[,c("datasetID","provenance.lat","provenance.long","continent")])
dGeog <- dGeog[complete.cases(dGeog$provenance.lat),] # n = 419

# wf_set_key(
#   key = "XX",
#   # service = "cds"
# )

dataSet <- sort(unique(dGeog$datasetID))

yrReq <- seq(2010,2020, by = 1)

# yrReq <- seq(2010, 2019, by =1)
mnthReq <- seq(1, 12, by =1)


# for (i in 1:length(contin)) {

# temp <- subset(dGeog, continent == contin[i])

# site_id <- as.character(
#   temp$continent[i]
# )
yrReq <- 2020
target_lat_N <- as.numeric(
  max(dGeog$provenance.lat)
)

target_lat_S <- as.numeric(
  min(dGeog$provenance.lat)
)

target_lon_W <- as.numeric(
  min(dGeog$provenance.long)
)

target_lon_E <- as.numeric(
  max(dGeog$provenance.long)
)

lat_lon_area <- c(
  target_lat_N,  # North
  target_lon_W,  # West
  target_lat_S,  # South
  target_lon_E   # East
)


for (yr in yrReq) {
  #yr <- yrReq[y]
  
  for (mth in mnthReq) {
    # mth <- mnthReq[m]
    
    number_of_days <- days_in_month(
      make_date(
        year = as.integer(yr),
        month = as.integer(mth),
        day = 1
      )
    )
    
    request <- list(
      dataset_short_name = "derived-era5-land-daily-statistics", 
      variable = c("soil_temperature_level_1", "volumetric_soil_water_layer_1"),
      daily_statistic = "daily_mean",
      time_zone       = "utc+00:00",
      year = yr,
      day = seq(1, number_of_days , by =1), 
      month = mth, 
      data_format = "netcdf",
      frequency = "1_hourly",
      area = lat_lon_area
    )
    
    output_filename <- paste0(
      "global_era5_land_daily_", yr,mth, ".zip")
    
    # Clone the base template and inject the specific year, month, and filename
    current_request <- request
    current_request$year   <- yr
    current_request$month  <- mth
    current_request$target <- output_filename
    
    # Execute the request
    tryCatch({
      wf_request(
        request = current_request,
        transfer = TRUE,
        path = ".", 
        time_out = 3600 # 1 hour timeout per monthly chunk is plenty
      )
      
      }
    )
    
  }
}
# }

##############################################################################

# unzip("era5landOutput/global_era5_land_daily_2020.zip")
#   
# era5_rast <- rast("era5landOutput/global_era5_land_daily_2020_2/volumetric_soil_water_layer_1_0_daily-mean.nc")
# target_lat <- 35.62027
# target_lon <-  129.00298
# 
# pt <- data.frame(lon = target_lon, lat = target_lat)
# 
# 
# # Extract temperature values

# extracted_raw <- terra::extract(era5_rast, pt); extracted_raw
# 
