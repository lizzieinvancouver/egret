# The aim of this code is to download climate data from ERA5-Land
# started by Deirdre Loughnan May 2026

# install.packages("ecmwfr")

rm(list=ls()) 
options(stringsAsFactors = FALSE)

library(ecmwfr)
library(terra)

d <- read.csv("output/egretclean.csv")
dGeog <- unique(d[,c("datasetID","provenance.lat","provenance.long")])
dGeog <- dGeog[complete.cases(dGeog$provenance.lat),] # n = 419
dGeog$iter <- seq(1:nrow(dGeog))

wf_set_key(
  key = "XXXX",
  # service = "cds"
)


yrReq <- seq(2022,2023, by = 1)
mnthReq <- sprintf("%02d", 1:12)

yrReq <- seq(2010, 2019, by =1)
mnthReq <- seq(1, 12, by =1)

for(i in 1:nrow(dGeog)){
  target_lat <- dGeog$provenance.lat[dGeog$iter == i] 
  target_lon <- dGeog$provenance.long[dGeog$iter == i] 
  
  lat_lon_box <- c(
    target_lat + 0.15, # North
    target_lon - 0.15, # West
    target_lat - 0.15, # South
    target_lon + 0.15  # East
  )
  
  for (y in 1:yrReq) {
    yr <- yrReq[y]
    
    for (m in mnthReq) {
      mth <- mnthReq[m]
      
      request <- list(
        dataset_short_name = "derived-era5-land-daily-statistics",
        variable = c("soil_temperature_level_1", "volumetric_soil_water_layer_1"),
        daily_statistic = "daily_mean",
        # time_zone       = "utc+00:00",
        year = yr,
        day = sprintf("%02d", 1:30), 
        month = mth, 
        data_format = "netcdf",
        # Bounding Box: North, West, South, East
        area = lat_lon_area
      )
      
      output_filename <- paste0(dGeog$datasetID[dGeog$iter == i], "era5_land_daily_", yr, "_", mth, ".nc")
     
      if (file.exists(output_filename)) {
        message(paste("Skipping existing file:", output_filename))
        next
      }
      
      message(paste("Requesting data for:", yr, "-", mth))
      
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
        message(paste("Successfully downloaded:", output_filename))
      }, error = function(e) {
        message(paste("Error downloading", yr, "-", mth, ":", e$message))
      })
       
    }
  }
}

######################################################################
######################################################################
# install.packages("terra")
# library(terra)
#unzip(paste0("era5_land_daily_", yr, "_", mo, ".nc"), exdir = "./extracted_nc_files")
nc_files <- list.files(path = ".", pattern = "era5_land_hourly_.*\\.nc$", full.names = TRUE)

dataset <- rast(nc_files[1])

# Extract all timestamps
data_dates <- time(dataset)

# Print the dates to the console
print(data_dates)

# See the date of just the first layer
print(data_dates[1])


nc_files <- list.files(path = ".", pattern = "era5_land_hourly_.*\\.nc$", full.names = TRUE)

yrsStacked <- rast(nc_files)

print(yearsStacked) 

extractData <- extract(yrsStacked, pts, df=TRUE)
write.csv(extractData, "era5_hourly_soilMoisture.csv", row.names = FALSE)