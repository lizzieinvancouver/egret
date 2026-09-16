# The aim of this code is to extract the soil temperature and mositure data from ERA5-Land downloaded data for the bounded lat long of egret
# started by Deirdre Loughnan Aug 2026

# install.packages("ecmwfr")

rm(list=ls()) 
options(stringsAsFactors = FALSE)

setwd("~/Documents/github/egret/analyses/era5landOutput")

library(ecmwfr)
library(terra)
library(lubridate)
library(stringr)
# library(purrr)
# library(dplyr)

d <- read.csv("..//output/egretclean.csv")
# d <- read.csv("output/egretclean.csv")

dGeog <- unique(d[,c("datasetID","provenance.lat","provenance.long","continent")])
dGeog <- dGeog[complete.cases(dGeog$provenance.lat),] # n = 419

# 1. list of files
# 2. unzip folder
# 3. for each .csv file---extract the data
# 4. Complete the data table with the lat/long of interest

# the lat/long values we want to extract---probably easier to extract and then merge back with datasetID

zipped<- list.files("global/", pattern = "\\.zip$")
zip_dir <- "global"
extracted_dir <- file.path("extracted_files")


folder_name <- tools::file_path_sans_ext(basename(zipped))

setwd("~/Documents/github/egret/analyses/era5landOutput/global")


for(i in 1:length(zipped)){
  
  # unzip(zipped[i], exdir = extracted_dir)
  extracted_files <- unzip(zipfile = zipped[1], exdir = extracted_dir)
  new_files <- file.path(dirname(extracted_files), 
    paste0(zipped[i], "_", basename(extracted_files))
  )
  file.rename(from = extracted_files, to = new_files)

}

  smFile <- list.files(path = "extracted_files", pattern = "_volumetric_soil_water_layer_1_0_daily-mean", full.names = TRUE)
  stFile <- list.files(path = "extracted_files", pattern = "_soil_temperature_level_1_0_daily-mean", full.names = TRUE)

  soilMoist <- vector()
  soilTemp <- vector()
  
  
  for (r in 1:length(smFile)){
  smRast <- rotate(rast(smFile[r]))
  stRast <- rast(stFile[r])
  
      for(s in 1:nrow(dGeog)){
        target_lat <- dGeog[s, "provenance.lat"]
        target_lon <- dGeog[s, "provenance.long"]
        
        extSm <- terra::extract(smRast, data.frame(lon = target_lon, lat = target_lat))
        temp <- str_split_fixed(smFile[r], "_", 8)
        extSm$year <- temp[,6]
        extSm$month <- temp[,7]
        extSm$lat <- target_lat
        extSm$long <- target_lon
        
        soilMoist <- rbind(soilMoist, extSm)
        
        # soil temp
        extSt <- terra::extract(smRast, data.frame(lon = target_lon, lat = target_lat))
        temp <- str_split_fixed(smFile[r], "_", 8)
        extSt$year <- temp[,6]
        extSt$month <- temp[,7]
        extSt$lat <- target_lat
        extSt$long <- target_lon

        soilTemp <- rbind(soilTemp, extSt)
      }    
  }

soilMoist$month <- gsub(".zip","", soilMoist$month)
soilTemp$month <- gsub(".zip","", soilTemp$month)

write.csv(soilMoist, "..//..//output/era5LandSoilMoisture.csv", row.names = FALSE)
write.csv(soilTemp, "..//..//output/era5LandSoilTemp.csv", row.names = FALSE)


temp2 <- soilMoist[!complete.cases(soilMoist),]
temp2 <- unique(temp2[,c("lat","long")]) # 18 missing 
names(soilMoist)

tempNA <- unique(temp2)
tempyNA <- unique(temp)
