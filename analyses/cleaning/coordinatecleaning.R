## Updated 30 November 2023 ##
## By Lizzie and Deirdre ##

# updated by Christophe end of May 2024


## Adds lat/lon coordinates when we did not have them (manually looked up by Tolu Amuwo) #
# and does some other cleaning and mapping ##
## Original file called coordinate_cleaning_TA.R ##

# setwd
# if(length(grep("deirdreloughnan", getwd()) > 0)) {
#   setwd("~/Documents/github/egret/analyses")
# } else if(length(grep("lizzie", getwd()) > 0)) {
#   setwd("/Users/lizzie/Documents/git/projects/egret/analyses")
# } else{
#   setwd("boomdittyboom") # for midge
# }
setwd("/Users/christophe_rouleau-desrochers/Documents/github/egret/analyses")
list.files()
# housekeeping
rm(list = ls()) # Clear whatever is already in R's memory
options(stringsAsFactors=FALSE)# Make sure words are read in as characters rather than factors

## load packages ##
library(leaflet)
library(sp)
library(sf)
library(plotly)
# grab the data 
egret <- read.csv("input/egretData.csv")

# Work by Tolu Amuwo where she looked up locations and manually added lat/lon
na.coords <- egret[which(is.na(egret$provenance.lat)),]

### Chuanren04
egret$provenance.lat[which(is.na(egret$provenance.lat) & egret$datasetID == "Chuanren04")] <- "56.13"
egret$provenance.long[which(is.na(egret$provenance.long) & egret$datasetID == "Chuanren04")] <- "-106.347"
### Albrecht20
egret$provenance.lat[which(is.na(egret$provenance.lat) & egret$datasetID == "Albrecht20")] <- "37.838"
egret$provenance.long[which(is.na(egret$provenance.long) & egret$datasetID == "Albrecht20")] <- "83.826"
### Bhatt00
egret$provenance.lat[which(is.na(egret$provenance.lat) & egret$datasetID == "Bhatt00")] <- "29.392"
egret$provenance.long[which(is.na(egret$provenance.long) & egret$datasetID == "Bhatt00")] <- "79.74"
### Boscagli01
egret$provenance.lat[which(is.na(egret$provenance.lat) & egret$datasetID == "Boscagli01")] <- "41.656"
egret$provenance.long[which(is.na(egret$provenance.long) & egret$datasetID == "Boscagli01")] <- "12.99"

# The below only run assuming Aldridge199X gets fixed elsewhere
### Aldridge1992
egret$provenance.lat[which(is.na(egret$provenance.lat) & egret$datasetID == "Aldridge1992")] <- "46.73"
egret$provenance.long[which(is.na(egret$provenance.long) & egret$datasetID == "Aldridge1992")] <- "94.69"
### Amini2018
egret$provenance.lat[which(is.na(egret$provenance.lat) & egret$datasetID == "Amini2018")] <- "36.812"
egret$provenance.long[which(is.na(egret$provenance.long) & egret$datasetID == "Amini2018")] <- "54.945"
### Cho18
egret$provenance.lat[which(is.na(egret$provenance.lat) & egret$datasetID == "Cho18")] <- "37.51"
egret$provenance.long[which(is.na(egret$provenance.long) & egret$datasetID == "Cho18")] <- "128.077"
### Crank92
egret$provenance.lat[which(is.na(egret$provenance.lat) & egret$datasetID == "Crank92")] <- "26.973"
egret$provenance.long[which(is.na(egret$provenance.long) & egret$datasetID == "Crank92")] <- "-99.101"
### Cuena-Lombrana18
egret$provenance.lat[which(is.na(egret$provenance.lat) & egret$datasetID == "Cuena-Lombrana18")] <- "40.02"
egret$provenance.long[which(is.na(egret$provenance.long) & egret$datasetID == "Cuena-Lombrana18")] <- "9.32"
### Dehgan84
egret$provenance.lat[which(is.na(egret$provenance.lat) & egret$datasetID == "Dehgan84")] <- "29.65"
egret$provenance.long[which(is.na(egret$provenance.long) & egret$datasetID == "Dehgan84")] <- "-82.324"
### Gainesville, Flroida
egret$source.population[which(is.na(egret$provenance.lat) & egret$source.population == "Gainesville, Flroida")] <- "Gainesville, Florida"
### cousins10
egret$provenance.lat[which(is.na(egret$provenance.lat) & egret$datasetID == "cousins10")] <- "33.433"
egret$provenance.long[which(is.na(egret$provenance.long) & egret$datasetID == "cousins10")] <- "-79.121"
### Naseri18
egret$provenance.lat[which(is.na(egret$provenance.lat) & egret$datasetID == "Naseri18")] <- "37.023"
egret$provenance.long[which(is.na(egret$provenance.long) & egret$datasetID == "Naseri18")] <- "55.59"
### Naumovski05
egret$source.population[which(egret$datasetID == "Naumovski05" & egret$source.population == "Bile Natural Habitat, Croatia")] <- "Velika kapela mountain, Croatia"
### Naumovski05
egret$provenance.lat[which(is.na(egret$provenance.lat) & egret$datasetID == "Naumovski05" & egret$source.population == "Velika kapela mountain, Croatia")] <- "43.192"
egret$provenance.long[which(is.na(egret$provenance.long) & egret$datasetID == "Naumovski05" & egret$source.population == "Velika kapela mountain, Croatia")] <- "17.194"
### Naumovski05
egret$provenance.lat[which(is.na(egret$provenance.lat) & egret$datasetID == "Naumovski05" & egret$source.population == "University of Zagreb, Croatia")] <- "45.811"
egret$provenance.long[which(is.na(egret$provenance.long) & egret$datasetID == "Naumovski05" & egret$source.population == "University of Zagreb, Croatia")] <- "15.97"
### Necajeva13
egret$provenance.lat[which(is.na(egret$provenance.lat) & egret$datasetID == "Necajeva13")] <- "56.997"
egret$provenance.long[which(is.na(egret$provenance.long) & egret$datasetID == "Necajeva13")] <- "24.023"
### Nin17
egret$provenance.lat[which(is.na(egret$provenance.lat) & egret$datasetID == "Nin17")] <- "42.469"
egret$provenance.long[which(is.na(egret$provenance.long) & egret$datasetID == "Nin17")] <- "13.566"
### Nin17
egret$source.population[which(egret$datasetID == "Nin17" & egret$source.population == "Central Apennine, Tuscasy")] <- "Central Apennine, Tuscany"
### Sacande04
egret$datasetID[which(egret$datasetID == "Sacande05")] <- "Sacande04"
### Sacande04
egret$provenance.lat[which(is.na(egret$provenance.lat) & egret$datasetID == "Sacande04")] <- "0.283"
egret$provenance.long[which(is.na(egret$provenance.long) & egret$datasetID == "Sacande04")] <- "34.752"
### barros12
egret$provenance.lat[which(is.na(egret$provenance.lat) & egret$datasetID == "barros12")] <- "37.78"
egret$provenance.long[which(is.na(egret$provenance.long) & egret$datasetID == "barros12")] <- "-25.497"
### basaran12
egret$provenance.lat[which(is.na(egret$provenance.lat) & egret$datasetID == "basaran12")] <- "41.28"
egret$provenance.long[which(is.na(egret$provenance.long) & egret$datasetID == "basaran12")] <- "36.336"
### jacquemart21
egret$provenance.lat[which(is.na(egret$provenance.lat) & egret$datasetID == "jacquemart21" & egret$source.population == "Aise, France")] <- "47.27"
egret$provenance.long[which(is.na(egret$provenance.long) & egret$datasetID == "jacquemart21" & egret$source.population == "Aise, France")] <- "6.331"
### jacquemart21
egret$provenance.lat[which(is.na(egret$provenance.lat) & egret$datasetID == "jacquemart21" & egret$source.population == "Boton, Belgium")] <- "50.504"
egret$provenance.long[which(is.na(egret$provenance.long) & egret$datasetID == "jacquemart21" & egret$source.population == "Boton, Belgium")] <- "4.470"
### jacquemart21
egret$provenance.lat[which(is.na(egret$provenance.lat) & egret$datasetID == "jacquemart21" & egret$source.population == "Prelleu, France")] <- "48.124"
egret$provenance.long[which(is.na(egret$provenance.long) & egret$datasetID == "jacquemart21" & egret$source.population == "Prelleu, France")] <- "-3.724"
### jacquemart21
egret$provenance.lat[which(is.na(egret$provenance.lat) & egret$datasetID == "jacquemart21" & egret$source.population == "Pairees, Belgium")] <- "50.504"
egret$provenance.long[which(is.na(egret$provenance.long) & egret$datasetID == "jacquemart21" & egret$source.population == "Pairees, Belgium")] <- "4.470"

# egret$provenance.lat[which(is.na(egret$provenance.lat) & egret$datasetID == "javanmard14")] no location given
# egret$provenance.long[which(is.na(egret$provenance.long) & egret$datasetID == "javanmard14")] no location given

### jensen97
egret$provenance.lat[which(is.na(egret$provenance.lat) & egret$datasetID == "jensen97")] <- "42.519"
egret$provenance.long[which(is.na(egret$provenance.long) & egret$datasetID == "jensen97")] <- "43.15"
### 
egret$provenance.lat[which(is.na(egret$provenance.lat) & egret$datasetID == "jusung16")] <- "37.527"
egret$provenance.long[which(is.na(egret$provenance.long) & egret$datasetID == "jusung16")] <- "128.235"
### 
egret$provenance.lat[which(is.na(egret$provenance.lat) & egret$datasetID == "kato11")] <- "34.831"
egret$provenance.long[which(is.na(egret$provenance.long) & egret$datasetID == "kato11")] <- "124.549"
### 
egret$provenance.lat[which(is.na(egret$provenance.lat) & egret$datasetID == "keshtkar08")] <- "33.325"
egret$provenance.long[which(is.na(egret$provenance.long) & egret$datasetID == "keshtkar08")] <- "53.391"
### 
egret$provenance.lat[which(is.na(egret$provenance.lat) & egret$datasetID == "kettenring07")] <- "46.730"
egret$provenance.long[which(is.na(egret$provenance.long) & egret$datasetID == "kettenring07")] <- "-94.690"
### 
egret$provenance.lat[which(is.na(egret$provenance.lat) & egret$datasetID == "mattana16")] <- "39.40"
egret$provenance.long[which(is.na(egret$provenance.long) & egret$datasetID == "mattana16")] <- "8.40"
### 
egret$provenance.lat[which(is.na(egret$provenance.lat) & egret$datasetID == "meena06")] <- "32.139"
egret$provenance.long[which(is.na(egret$provenance.long) & egret$datasetID == "meena06")] <- "77.154"
### meyer94
egret$provenance.lat[which(is.na(egret$provenance.lat) & egret$datasetID == "meyer94" & egret$source.population == "Pinto, Utah, USA")] <- "37.538"
egret$provenance.long[which(is.na(egret$provenance.long) & egret$datasetID == "meyer94" & egret$source.population == "Pinto, Utah, USA")] <- "-113.517"
egret$provenance.lat[which(is.na(egret$provenance.lat) & egret$datasetID == "meyer94" & egret$source.population == "Zion Park, Utah, USA")] <- "37.298"
egret$provenance.long[which(is.na(egret$provenance.long) & egret$datasetID == "meyer94" & egret$source.population == "Zion Park, Utah, USA")] <- "-113.026"
egret$provenance.lat[which(is.na(egret$provenance.lat) & egret$datasetID == "meyer94" & egret$source.population == "Hobble Creek, Utah, USA")] <- "40.213"
egret$provenance.long[which(is.na(egret$provenance.long) & egret$datasetID == "meyer94" & egret$source.population == "Hobble Creek, Utah, USA")] <- "-111.48"
egret$provenance.lat[which(is.na(egret$provenance.lat) & egret$datasetID == "meyer94" & egret$source.population == "Solitude, Utah, USA")] <- "40.621"
egret$provenance.long[which(is.na(egret$provenance.long) & egret$datasetID == "meyer94" & egret$source.population == "Solitude, Utah, USA")] <- "-111.593"
egret$provenance.lat[which(is.na(egret$provenance.lat) & egret$datasetID == "meyer94" & egret$source.population == "Cache Valley, Utah, USA")] <- "38.725"
egret$provenance.long[which(is.na(egret$provenance.long) & egret$datasetID == "meyer94" & egret$source.population == "Cache Valley, Utah, USA")] <- "-109.521"
egret$provenance.lat[which(is.na(egret$provenance.lat) & egret$datasetID == "meyer94" & egret$source.population == "Lava Hot Springs, Idaho, USA")] <- "42.619"
egret$provenance.long[which(is.na(egret$provenance.long) & egret$datasetID == "meyer94" & egret$source.population == "Lava Hot Springs, Idaho, USA")] <- "-112.011"
egret$provenance.lat[which(is.na(egret$provenance.lat) & egret$datasetID == "meyer94" & egret$source.population == "Dewey Mine, Idaho, USA")] <- "45.931"
egret$provenance.long[which(is.na(egret$provenance.long) & egret$datasetID == "meyer94" & egret$source.population == "Dewey Mine, Idaho, USA")] <- "-116.011"
egret$provenance.lat[which(is.na(egret$provenance.lat) & egret$datasetID == "meyer94" & egret$source.population == "South of Price, Utah, USA")] <- "39.601"
egret$provenance.long[which(is.na(egret$provenance.long) & egret$datasetID == "meyer94" & egret$source.population == "South of Price, Utah, USA")] <- "-110.81"
egret$provenance.lat[which(is.na(egret$provenance.lat) & egret$datasetID == "meyer94" & egret$source.population == "Lower LaSal, Utah, USA")] <- "38.302"
egret$provenance.long[which(is.na(egret$provenance.long) & egret$datasetID == "meyer94" & egret$source.population == "Lower LaSal, Utah, USA")] <- "-109.269"
egret$provenance.lat[which(is.na(egret$provenance.lat) & egret$datasetID == "meyer94" & egret$source.population == "Upper LaSal, Utah, USA")] <- "38.302"
egret$provenance.long[which(is.na(egret$provenance.long) & egret$datasetID == "meyer94" & egret$source.population == "Upper LaSal, Utah, USA")] <- "-109.269"
egret$provenance.lat[which(is.na(egret$provenance.lat) & egret$datasetID == "meyer94" & egret$source.population == "West of Vernal, Utah, USA")] <- "40.455"
egret$provenance.long[which(is.na(egret$provenance.long) & egret$datasetID == "meyer94" & egret$source.population == "West of Vernal, Utah, USA")] <- "-109.536"
egret$provenance.lat[which(is.na(egret$provenance.lat) & egret$datasetID == "meyer94" & egret$source.population == "North of Lapointe, Utah, USA")] <- "40.4022"
egret$provenance.long[which(is.na(egret$provenance.long) & egret$datasetID == "meyer94" & egret$source.population == "North of Lapointe, Utah, USA")] <- "-109.7959"
egret$provenance.lat[which(is.na(egret$provenance.lat) & egret$datasetID == "meyer94" & egret$source.population == "Kolob Revervoir, Utah, USA")] <- "37.44"
egret$provenance.long[which(is.na(egret$provenance.long) & egret$datasetID == "meyer94" & egret$source.population == "Kolob Revervoir, Utah, USA")] <- "-113.048"
egret$provenance.lat[which(is.na(egret$provenance.lat) & egret$datasetID == "meyer94" & egret$source.population == "Zion Overlook, Utah, USA")] <- "37.298"
egret$provenance.long[which(is.na(egret$provenance.long) & egret$datasetID == "meyer94" & egret$source.population == "Zion Overlook, Utah, USA")] <- "-113.028"
egret$provenance.lat[which(is.na(egret$provenance.lat) & egret$datasetID == "meyer94" & egret$source.population == "Birds of Prey, Idaho, USA")] <- "43.517"
egret$provenance.long[which(is.na(egret$provenance.long) & egret$datasetID == "meyer94" & egret$source.population == "Birds of Prey, Idaho, USA")] <- "-116.256"
egret$provenance.lat[which(is.na(egret$provenance.lat) & egret$datasetID == "meyer94" & egret$source.population == "Midway, Utah, USA")] <- "40.512"
egret$provenance.long[which(is.na(egret$provenance.long) & egret$datasetID == "meyer94" & egret$source.population == "Midway, Utah, USA")] <- "-111.474"
egret$provenance.lat[which(is.na(egret$provenance.lat) & egret$datasetID == "meyer94" & egret$source.population == "Emigrant Pass, Nevada, USA")] <- "40.655"
egret$provenance.long[which(is.na(egret$provenance.long) & egret$datasetID == "meyer94" & egret$source.population == "Emigrant Pass, Nevada, USA")] <- "-111.274"
egret$provenance.lat[which(is.na(egret$provenance.lat) & egret$datasetID == "meyer94" & egret$source.population == "Soldier Summit, Utah, USA")] <- "39.929"
egret$provenance.long[which(is.na(egret$provenance.long) & egret$datasetID == "meyer94" & egret$source.population == "Soldier Summit, Utah, USA")] <- "-111.078"
egret$provenance.lat[which(is.na(egret$provenance.lat) & egret$datasetID == "meyer94" & egret$source.population == "Fairview Top, Utah, USA")] <- "39.626"
egret$provenance.long[which(is.na(egret$provenance.long) & egret$datasetID == "meyer94" & egret$source.population == "Fairview Top, Utah, USA")] <- "-111.44"
egret$provenance.lat[which(is.na(egret$provenance.lat) & egret$datasetID == "meyer94" & egret$source.population == "Thistle Junction, Utah, USA")] <- "40.00"
egret$provenance.long[which(is.na(egret$provenance.long) & egret$datasetID == "meyer94" & egret$source.population == "Thistle Junction, Utah, USA")] <- "-111.494"
egret$provenance.lat[which(is.na(egret$provenance.lat) & egret$datasetID == "meyer94" & egret$source.population == "Stillwater Campground, Utah, USA")] <- "40.561"
egret$provenance.long[which(is.na(egret$provenance.long) & egret$datasetID == "meyer94" & egret$source.population == "Stillwater Campground, Utah, USA")] <- "-110.70"
### meyer95
egret$provenance.lat[which(is.na(egret$provenance.lat) & egret$datasetID == "meyer95" & egret$source.population == "Valley of Fire, Nevada, USA")] <- "36.480"
egret$provenance.long[which(is.na(egret$provenance.long) & egret$datasetID == "meyer95" & egret$source.population == "Valley of Fire, Nevada, USA")] <- "-114.538"
egret$provenance.lat[which(is.na(egret$provenance.lat) & egret$datasetID == "meyer95" & egret$source.population == "Shivwits, Utah, USA")] <- "37.1811"
egret$provenance.long[which(is.na(egret$provenance.long) & egret$datasetID == "meyer95" & egret$source.population == "Shivwits, Utah, USA")] <- "-113.758"
egret$provenance.lat[which(is.na(egret$provenance.lat) & egret$datasetID == "meyer95" & egret$source.population == "Oak Grove, Utah, USA")] <- "32.861"
egret$provenance.long[which(is.na(egret$provenance.long) & egret$datasetID == "meyer95" & egret$source.population == "Oak Grove, Utah, USA")] <- "-91.388"
egret$provenance.lat[which(is.na(egret$provenance.lat) & egret$datasetID == "meyer95" & egret$source.population == "Pine Valley, Utah, USA")] <- "37.391"
egret$provenance.long[which(is.na(egret$provenance.long) & egret$datasetID == "meyer95" & egret$source.population == "Pine Valley, Utah, USA")] <- "-113.514"
egret$provenance.lat[which(is.na(egret$provenance.lat) & egret$datasetID == "meyer95" & egret$source.population == "Kolob Road, Utah, USA")] <- "37.298"
egret$provenance.long[which(is.na(egret$provenance.long) & egret$datasetID == "meyer95" & egret$source.population == "Kolob Road, Utah, USA")] <- "-113.026"
egret$provenance.lat[which(is.na(egret$provenance.lat) & egret$datasetID == "meyer95" & egret$source.population == "Tridell, Utah, USA")] <- "40.454"
egret$provenance.long[which(is.na(egret$provenance.long) & egret$datasetID == "meyer95" & egret$source.population == "Tridell, Utah, USA")] <- "-109.45"
egret$provenance.lat[which(is.na(egret$provenance.lat) & egret$datasetID == "meyer95" & egret$source.population == "Cisco, Utah, USA")] <- "38.97"
egret$provenance.long[which(is.na(egret$provenance.long) & egret$datasetID == "meyer95" & egret$source.population == "Cisco, Utah, USA")] <- "-109.321"
egret$provenance.lat[which(is.na(egret$provenance.lat) & egret$datasetID == "meyer95" & egret$source.population == "Twin Falls, Idaho, USA")] <- "42.556"
egret$provenance.long[which(is.na(egret$provenance.long) & egret$datasetID == "meyer95" & egret$source.population == "Twin Falls, Idaho, USA")] <- "-114.47"
egret$provenance.lat[which(is.na(egret$provenance.lat) & egret$datasetID == "meyer95" & egret$source.population == "Bruneau Dunes, Idaho, USA")] <- "42.911"
egret$provenance.long[which(is.na(egret$provenance.long) & egret$datasetID == "meyer95" & egret$source.population == "Bruneau Dunes, Idaho, USA")] <- "-115.715"
egret$provenance.lat[which(is.na(egret$provenance.lat) & egret$datasetID == "meyer95" & egret$source.population == "San Rafael Swell, Utah, USA")] <- "38.824"
egret$provenance.long[which(is.na(egret$provenance.long) & egret$datasetID == "meyer95" & egret$source.population == "San Rafael Swell, Utah, USA")] <- "-110.677"
egret$provenance.lat[which(is.na(egret$provenance.lat) & egret$datasetID == "meyer95" & egret$source.population == "Browse Offramp, Utah, USA")] <- "37.236"
egret$provenance.long[which(is.na(egret$provenance.long) & egret$datasetID == "meyer95" & egret$source.population == "Browse Offramp, Utah, USA")] <- "-113.636"
egret$provenance.lat[which(is.na(egret$provenance.lat) & egret$datasetID == "meyer95" & egret$source.population == "Potosi Pass Road, Nevada, USA")] <- "35.989"
egret$provenance.long[which(is.na(egret$provenance.long) & egret$datasetID == "meyer95" & egret$source.population == "Potosi Pass Road, Nevada, USA")] <- "-115.526"
egret$provenance.lat[which(is.na(egret$provenance.lat) & egret$datasetID == "meyer95" & egret$source.population == "Oak Grove Road, Utah, USA")] <- "32.861"
egret$provenance.long[which(is.na(egret$provenance.long) & egret$datasetID == "meyer95" & egret$source.population == "Oak Grove Road, Utah, USA")] <- "-91.388"
egret$provenance.lat[which(is.na(egret$provenance.lat) & egret$datasetID == "meyer95" & egret$source.population == "Duchesne, Utah, USA")] <- "40.163"
egret$provenance.long[which(is.na(egret$provenance.long) & egret$datasetID == "meyer95" & egret$source.population == "Duchesne, Utah, USA")] <- "-110.403"
egret$provenance.lat[which(is.na(egret$provenance.lat) & egret$datasetID == "meyer95" & egret$source.population == "South of Price, Utah, USA")] <- "39.601"
egret$provenance.long[which(is.na(egret$provenance.long) & egret$datasetID == "meyer95" & egret$source.population == "South of Price, Utah, USA")] <- "-110.81"
egret$provenance.lat[which(is.na(egret$provenance.lat) & egret$datasetID == "meyer95" & egret$source.population == "West of Vernal, Utah, USA")] <- "40.455"
egret$provenance.long[which(is.na(egret$provenance.long) & egret$datasetID == "meyer95" & egret$source.population == "West of Vernal, Utah, USA")] <- "-109.536"
egret$provenance.lat[which(is.na(egret$provenance.lat) & egret$datasetID == "meyer95" & egret$source.population == "Central, Utah, USA")] <- "37.417"
egret$provenance.long[which(is.na(egret$provenance.long) & egret$datasetID == "meyer95" & egret$source.population == "Central, Utah, USA")] <- "-113.625"
### martinik14
egret$provenance.lat[which(is.na(egret$provenance.lat) & egret$datasetID == "martinik14")] <- "49.818"
egret$provenance.long[which(is.na(egret$provenance.long) & egret$datasetID == "martinik14")] <- "15.473"
### muller03
egret$provenance.lat[which(is.na(egret$provenance.lat) & egret$datasetID == "muller03")] <- "46.228"
egret$provenance.long[which(is.na(egret$provenance.long) & egret$datasetID == "muller03")] <- "2.213"
### na11
egret$provenance.lat[which(is.na(egret$provenance.lat) & egret$datasetID == "na11")] <- "45.757"
egret$provenance.long[which(is.na(egret$provenance.long) & egret$datasetID == "na11")] <- "124.642"
### ordonez-salanueva15
egret$provenance.lat[which(is.na(egret$provenance.lat) & egret$datasetID == "ordonez-salanueva15")] <- "18.182"
egret$provenance.long[which(is.na(egret$provenance.long) & egret$datasetID == "ordonez-salanueva15")] <- "-97.479"
### parmenter96
egret$provenance.lat[which(is.na(egret$provenance.lat) & egret$datasetID == "parmenter96" & egret$species == "angustifolia")] <- "56.13"
egret$provenance.long[which(is.na(egret$provenance.long) & egret$datasetID == "parmenter96" & egret$species == "angustifolia")] <- "-106.347"
egret$provenance.lat[which(is.na(egret$provenance.lat) & egret$datasetID == "parmenter96" & egret$species == "purpurea")] <- "51.166"
egret$provenance.long[which(is.na(egret$provenance.long) & egret$datasetID == "parmenter96" & egret$species == "purpurea")] <- "10.452"
### picciau17
egret$provenance.lat[which(is.na(egret$provenance.lat) & egret$datasetID == "picciau17")] <- "39.014"
egret$provenance.long[which(is.na(egret$provenance.long) & egret$datasetID == "picciau17")] <- "8.932"
### parvin15
egret$provenance.lat[which(is.na(egret$provenance.lat) & egret$datasetID == "parvin15")] <- "35.844"
egret$provenance.long[which(is.na(egret$provenance.long) & egret$datasetID == "parvin15")] <- "50.972"


# update by christophe -- double checking if we have any of the locations missing
na.coords <- egret[which(is.na(egret$provenance.lat)),]
# select only interesting columns
na.coords.cut <- na.coords[c("datasetID", "genus", "species", "woody", "crop", "source.population", "provenance.lat", "provenance.long", "continent", "Notes")]
head(na.coords.cut)

head(na.coords.cut)
# check how many papers don't have data
temp <- unique(na.coords$datasetID)



# Start a quick map with given locations
### Available locations
nonavalues <-egret[complete.cases(egret["provenance.lat"]),]

# color coding
occurence <- egret %>%
  group_by(continent) %>%
  summarize(continent = first(continent), count = n())

# map
egretlocations <- plot_geo(occurence) %>%
  layout(
    geo = list(
      showframe = TRUE,
      showcoastlines = TRUE,
      showland = TRUE,
      landcolor = toRGB("white"),
      countrycolor = toRGB("darkgrey"),
      coastlinecolor = toRGB("black"),
      coastlinewidth = 0.5,
      lataxis = list(
        range = c(-55, 80),
        showgrid = FALSE
      ),
      lonaxis = list(
        range = c(-130, 160),
        showgrid = FALSE
      )
    )
  ) %>%
  # # Color gradient set to the number of papers in each country
  # add_trace(
  #   z = ~count, color = ~count, colors = 'GnBu',
  #   text = ~continent, locations = ~continent,
  #   marker = list(
  #     line = list(width = 0.5, color = "black")
  #   ),
  #   # Edit the colar bar position --> make the X value negative if I want to set it on the left
  #   colorbar = list(title = "", x = 1, y = 1.1, len = 1.03)
  # ) %>%
  # Dots for which the size is set to the number of rootstocks used in the paper
  add_trace(
    type = "scattergeo",
    lat = ~nonavalues$provenance.lat, 
    lon = ~nonavalues$provenance.long,
   # text = ~paste("Title: ", nonavalues$title, "<br>Number of Rootstocks: ", nonavalues$n_rootstock),
    mode = "markers",
    marker = list(
      size = 1,
      symbol = "circle",
      color = "lightgrey",
      line = list(width = 1.5, color = "black")
    )
  ) %>% 
  layout(title = "")
egretlocations
# save figure
save_image(egretlocations,
           file="/Users/christophe_rouleau-desrochers/Documents/mapegret.html")

# != all but that
temp <- subset(egret, continent == "NA, possibly Asia")

head(temp)
subset()

### sundaramoorthy93
egret[, "sundaramoorthy93"]
egret[, egret$datasetID == "sundaramoorthy93"]
### momonoki79
############################################################################

## fixing continent points ... this needs to BE CHECKED!

egret$continent[which(egret$continent == "USA")] <- "North America"

North_America <- egret[which(egret$continent == "North America"),]

egret$provenance.long[which(egret$continent == "North America" & egret$provenance.long == "83.826")] <- "-83.826"
egret$provenance.long[which(egret$continent == "North America" & egret$provenance.long == "94.69")] <- "-94.69"
egret$provenance.long[which(egret$continent == "North America" & egret$provenance.long == "-10.452")] <- "10.452"
# why's this?
egret$continent[which(egret$continent == "North America" & egret$source.population == "Germany")] <- "Europe"
egret$provenance.long[which(egret$continent == "Europe" & egret$provenance.long < -27 & egret$provenance.lat > 37)] <- "25.497"
egret$provenance.long[which(egret$continent == "Europe" & egret$provenance.long == "-39.5")] <- "39.5"
egret$provenance.long[which(egret$continent == "Europe" & egret$provenance.long == "-36.39")] <- "36.39"
egret$provenance.long[which(egret$continent == "Europe" & egret$provenance.long == "-36.39")] <- "36.39"
egret$provenance.long[which(egret$continent == "Europe" & egret$provenance.long == "15.5474")]  <- "-15.5474"
egret$provenance.long[which(egret$continent == "Europe" & egret$provenance.long == "16.6291")]  <- "-16.6291"

egret$provenance.long[which(egret$continent == "South America" & egret$provenance.long == "60.56")]  <- "-60.56"
egret$provenance.lat[which(egret$continent == "South America" & egret$provenance.lat == "31.26")]  <- "-31.26"

egret$provenance.long[which(egret$continent == "Asia" & egret$provenance.long == "-79.73")] <- "79.73"
egret$provenance.long[which(egret$continent == "Asia" & egret$provenance.long == "-79.05")] <- "79.05"
egret$provenance.long[which(egret$continent == "Asia" & egret$provenance.long == "-121.186944")] <- "121.186944"
egret$provenance.long[which(egret$continent == "Asia" & egret$provenance.long == "-121.117")] <- "121.117"
egret$provenance.long[which(egret$continent == "Asia" & egret$provenance.long == "-50.133333333333297")] <- "50.133333333333297"

egret$provenance.lat[which(egret$continent == "Asia" & egret$provenance.long == "124.549")] <- "133.9198"
egret$provenance.long[which(egret$continent == "Asia" & egret$provenance.lat == "34.831")] <- "34.656"

egret$provenance.lat[which(egret$continent == "Africa" & egret$provenance.lat == "-39.983333")] <- "-33.988"
############################################################################

## projecting coordinates onto a basemap this needs to BE CHECKED!
na.coords <- egret[which(is.na(egret$provenance.lat)),]

unique(egret$provenance.lat)
unique(egret$provenance.long)

egret_filtered <- egret[which(egret$provenance.lat != "38.967 and 37.9" | 
                              egret$provenance.lat != "~34-34.666667" |
                              egret$provenance.lat != "9.4167/9.3833" |
                              egret$provenance.long != "42.0333/42.0167" |
                              egret$provenance.long != "~105.5-106.5"),]

egret_filtered <- egret_filtered[which(!is.na(egret_filtered$provenance.lat)),]
unique(egret_filtered$provenance.lat) # 9.4167/9.3833 still in the dataframe


coords <- egret_filtered[, c("provenance.long", "provenance.lat", "continent")]
unique(coords$continent)
# lon <- egret_filtered$provenance.long
# lat <- egret_filtered$provenance.lat
# coords <- as.data.frame(cbind(lon,lat))
coords$provenance.long <- as.numeric(as.character(coords$provenance.long))
coords$provenance.lat <- as.numeric(as.character(coords$provenance.lat))

coords$provenance.long <- round(coords$provenance.long, 3)
coords$provenance.lat <- round(coords$provenance.lat, 3)
coords <- coords[which(!is.na(coords$provenance.long)),] # NAs likely due to 9.4167/9.3833

#state which columns are lon and lat
coordinates(coords) <- ~provenance.long+provenance.lat

#convert to sf object
coords <- st_as_sf(coords)

#set crs (WSG1984 seems to be used here)
st_crs(coords) <- 4326

#create leaflet
leaflet(coords) %>% addMarkers() %>% addTiles()

#### filtering continents

North_America <- coords[which(coords$continent == "North America"),]
leaflet(North_America) %>% addMarkers() %>% addTiles()

Europe <- coords[which(coords$continent == "Europe"),]
leaflet(Europe) %>% addMarkers() %>% addTiles()

South_America <- coords[which(coords$continent == "South America"),]
leaflet(South_America) %>% addMarkers() %>% addTiles()

Asia <- coords[which(coords$continent == "Asia"),]
leaflet(Asia) %>% addMarkers() %>% addTiles()

Africa <- coords[which(coords$continent == "Africa"),]
leaflet(Africa) %>% addMarkers() %>% addTiles()

Australia <- coords[which(coords$continent == "Australia"),]
leaflet(Australia) %>% addMarkers() %>% addTiles()

