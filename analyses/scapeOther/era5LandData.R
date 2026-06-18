# The aim of this code is to download climate data from ERA5-Land
# started by Deirdre Loughnan May 2026

# install.packages("ecmwfr")
library(ecmwfr)
library(terra)

# wf_set_key(
#   key = "XXXX",
#   # service = "cds"
# )

yrReq <- seq(2000,2023, by = 1)
mnthReq <- sprintf("%02d", 1:12)

request <- list(
  dataset_short_name = "derived-era5-land-daily-statistics",
  variable = c("soil_temperature_level_1", "volumetric_soil_water_layer_1"),
  daily_statistic = "daily_mean",
  frequency       = "1_hourly",
  time_zone       = "utc+00:00",
  day = sprintf("%02d", 1:31), 
  time = sprintf("%02d:00", 0:23),
  data_format = "netcdf",
  # Bounding Box: North, West, South, East
  area = c(51, 2, 49, 7) 
)

for (yr in yrReq) {
  for (mo in mnthReq) {
    
    # Create a unique filename for every single month
    output_filename <- paste0("era5_land_hourly_", yr, "_", mo, ".nc")
    
    # Skip download if the file already exists locally
    if (file.exists(output_filename)) {
      message(paste("Skipping existing file:", output_filename))
      next
    }
    
    message(paste("Requesting data for:", yr, "-", mo))
    
    # Clone the base template and inject the specific year, month, and filename
    current_request <- base_request
    current_request$year   <- yr
    current_request$month  <- mo
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
      message(paste("Error downloading", yr, "-", mo, ":", e$message))
    })
    
  }
}

# install.packages("terra")
# library(terra)
unzip("era5_land_daily_1950_1959.zip", exdir = "./extracted_nc_files")
nc_files <- list.files(path = ".", pattern = "era5_land_hourly_.*\\.nc$", full.names = TRUE)

yrsStacked <- rast(nc_files)

print(yearsStacked) 

extractData <- extract(yrsStacked, pts, df=TRUE)
write.csv(extractData, "era5_hourly_soilMoisture.csv", row.names = FALSE)