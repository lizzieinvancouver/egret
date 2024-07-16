## Started 16 July 2024 ##
## By Lizzie, who is just pulling together misc code in cleaning/ right now ##

##
## taken from cleaning_DL.R 

unique(egret$storage.type) # 75 options
egret$storage.type[which(egret$storage.type == "N/A")] <- "NA"


# Main categories: 
#room temp
egret$storage.type[which(egret$storage.type == "laboratory")] <- "room temp"
egret$storage.type[which(egret$storage.type == "room conditions")] <- "room temp"
egret$storage.type[which(egret$storage.type == "coin envelope (room temperature)")] <- "room temp"
egret$storage.type[which(egret$storage.type == "ambient")] <- "room temp"

egret$storage.type[which(egret$storage.type == "glass bottles, laboratory conditions")] <- "room temp air tight container"

#Air tight container - no temp specified 
egret$storage.type[which(egret$storage.type == "air-tight")] <- "air-tight no temp spec"
egret$storage.type[which(egret$storage.type == "air-tight containers")] <- "air-tight no temp spec"
egret$storage.type[which(egret$storage.type == "airtight plastic bags")] <- "air-tight no temp spec"
egret$storage.type[which(egret$storage.type == "airtight polyethylene bags")] <- "air-tight no temp spec"
egret$storage.type[which(egret$storage.type == "glass container")] <- "air-tight no temp spec"
egret$storage.type[which(egret$storage.type == "plastic bags")] <- "air-tight no temp spec"
egret$storage.type[which(egret$storage.type == "plastic bag")] <- "air-tight no temp spec"
egret$storage.type[which(egret$storage.type == "sealed containers")] <- "air-tight no temp spec"
egret$storage.type[which(egret$storage.type == "sealed glass bottle")] <- "air-tight no temp spec"

#Dry
#Dry - cold
egret$storage.type[which(egret$storage.type == "dry/refrigerated")] <- "dry cold"
egret$storage.type[which(egret$storage.type == "dry refrigerator")] <- "dry cold"
egret$storage.type[which(egret$storage.type == "dry, refrigerator")] <- "dry cold"
egret$storage.type[which(egret$storage.type == "cold dry")] <- "dry cold"
egret$storage.type[which(egret$storage.type == "cold, dry")] <- "dry cold"
egret$storage.type[which(egret$storage.type == "cold/dry")] <- "dry cold"
egret$storage.type[which(egret$storage.type == "dry/cold")] <- "dry cold"
egret$storage.type[which(egret$storage.type == "dry/refrigerated")] <- "dry cold"

# mosit -  cold
egret$storage.type[which(egret$storage.type == "cold/wet")] <- "moist cold"
egret$storage.type[which(egret$storage.type == "cold/moist")] <- "moist cold"

#Dark
egret$storage.type[which(egret$storage.type == "darkness")] <- "dark"
egret$storage.type[which(egret$storage.type == "in darkness")] <- "dark"

#TO CHECK:
# natural as in outdoors?
# paper bags in dark - room temp?
# controlled environment - not a room?
# is wet and undried the same?
# if just says dry or wet, should we assume at room temp?
# Cold start?

unique(egret$storage.humidity)

unique(egret$storage.time)
egret$storage.time[which(egret$storage.time == "didn't mention")] <- "NA"
# TO CHECK:
#"38 5 h" "38 25h" , days + h or missing decimal?

unique(egret$storage.temp)
# many ranges or with variance


## 
## taken from clean_other.R (originally called coordinate_cleaning_JS.R)

unique(egret$storage.type)
egret$storage.type[which(egret$storage.type == "laboratory")] <- "room temp"
egret$storage.type[which(egret$storage.type == "room conditions")] <- "room temp"
egret$storage.type[which(egret$storage.type == "glass bottles, laboratory conditions")] <- "room temp"
egret$storage.type[which(egret$storage.type == "dry/refrigerated")] <- "dry refrigeration"
egret$storage.type[which(egret$storage.type == "dry refrigerator")] <- "dry refrigeration"

#TO CHECK:
# natural as in outdoors? - no "natural" input found in data set - JS

# paper bags in dark - room temp? - yes, at room temp - JS
egret$storage.type[which(egret$storage.type == "paper bags in dark")] <- "room temp"

# controlled environment - not a room? - yes, in a room at 15 degrees - JS
egret$storage.type[which(egret$storage.type == "controlled environment")] <- "room temp"

# is wet and undried the same? - in aldridge96 for Z Palustris, seeds were stored "imbibed at 2 degrees", so wet - JS
# - in aldridge96 for P coarctata, seeds were not stored but transferred to experimental conditions immediately - JS
# if just says dry or wet, should we assume at room temp?  - I think in most cases it is specified in $storage.temp

# only check on papers with dry storage.type and no specified storage.temp
dry <- egret[which(egret$storage.type == "dry" & is.na(egret$storage.temp)),] # 54 papers
unique(dry$datasetID) # 6 papers
#cho18 seeds were stored at 4 +/- 1 degrees
#could not find chen06 
#chichizola18 seeds were stored at 5 degrees 
#kim16 seeds were stored at 5 degrees
#kolodziejek15 seeds were stored at room temp
egret$storage.type[which(egret$datasetID == "kolodziejek15")] <- "dry room temp"
#langlois - storage type included under $treatment (paper, plastic bag, wet sand), unstratified seeds were stored at room temp in paper bags
wet <- egret[which(egret$storage.type == "wet" & is.na(egret$storage.temp)),] # 0 papers, all specify temp in seperate column


unique(egret$storage.humidity)

unique(egret$storage.time)
egret$storage.time[which(egret$storage.time == "didn't mention")] <- "NA"
# TO CHECK:
#"38 5 h" "38 25h" , days + h or missing decimal? - appears to be missing decimal (all values in the paper seem to have a space instead of a decimal) - JS
egret$storage.time[which(egret$storage.time == "38 5 h")] <- "1.604167"  # 38.5 hours is 1.604167 days
egret$storage.time[which(egret$storage.time == "38 25h")] <- "1.59375"  # 38.25 hours is 1.59375 days

unique(egret$storage.temp)
# many ranges or with variance