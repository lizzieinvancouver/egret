## Updated 30 November 2023 ##
## By Lizzie and Deirdre ##

## Adds lat/lon coordinates when we did not have them (manually looked up by Tolu Amuwo) #
# and does some other cleaning and mapping ##
## Original file called coordinate_cleaning_TA.R ##

# To convert lat/long from Degree minute seconds to decimals degrees:
# https://www.fcc.gov/media/radio/dms-decimal

#To find the lat/long of a region, please use google earth:
#https://earth.google.com/web/
# once you search a region, use the "add placemark" tool to drop a pin in the regions centre and record the lat long that is shown in the popup box. Note: the unit of the coordinates can be changed to decimal degress by going to "Tools"-->"Settings"--> scroll down to "Formats and Units" and select "decimal" from the drop down menu for the "Latitude Longitude formatting"
## load packages ##
library(leaflet)
library(sp)
library(sf)

# grab the data 
#d <- read.csv("input/dData.csv")

# Read in the data by running cleanall.R 1-3

# Work by Tolu Amuwo where she looked up locations and manually added lat/lon
na.coords <- d[which(is.na(d$provenance.lat)),]
d$lat.long.coarse[which(is.na(d$provenance.lat))] <- "Y"
d$lat.long.coarse[which(is.na(d$lat.long.coarse))] <- "N"
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "Chuanren04")] <- "56.13"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "Chuanren04")] <- "-106.347"
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "Albrecht20")] <- "37.838"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "Albrecht20")] <- "83.826"
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "Bhatt00")] <- "29.392"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "Bhatt00")] <- "79.74"
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "Boscagli01")] <- "41.656"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "Boscagli01")] <- "12.99"

# The below only run assuming Aldridge199X gets fixed elsewhere
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "Aldridge1992")] <- "46.73" #Britany: this doens't look right
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "Aldridge1992")] <- "94.69"

d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "Amini2018")] <- "36.812"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "Amini2018")] <- "54.945"

d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "Cho18")] <- "37.51"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "Cho18")] <- "128.077"

d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "Crank92")] <- "26.973"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "Crank92")] <- "-99.101"

d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "Cuena-Lombrana18")] <- "40.02"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "Cuena-Lombrana18")] <- "9.32"

d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "Dehgan84")] <- "29.65"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "Dehgan84")] <- "-82.324"
d$source.population[which(is.na(d$provenance.lat) & d$source.population == "Gainesville, Flroida")] <- "Gainesville, Florida"

d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "cousins10")] <- "33.433"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "cousins10")] <- "-79.121"

d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "Naseri18")] <- "37.023"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "Naseri18")] <- "55.59"

d$source.population[which(d$datasetID == "Naumovski05" & d$source.population == "Bile Natural Habitat, Croatia")] <- "Velika kapela mountain, Croatia"
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "Naumovski05" & d$source.population == "Velika kapela mountain, Croatia")] <- "43.192"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "Naumovski05" & d$source.population == "Velika kapela mountain, Croatia")] <- "17.194"
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "Naumovski05" & d$source.population == "University of Zagreb, Croatia")] <- "45.811"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "Naumovski05" & d$source.population == "University of Zagreb, Croatia")] <- "15.97"

d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "Necajeva13")] <- "56.997"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "Necajeva13")] <- "24.023"

d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "Nin17")] <- "42.469"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "Nin17")] <- "13.566"
d$source.population[which(d$datasetID == "Nin17" & d$source.population == "Central Apennine, Tuscasy")] <- "Central Apennine, Tuscany"

d$datasetID[which(d$datasetID == "Sacande05")] <- "Sacande04"
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "Sacande04")] <- "0.283"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "Sacande04")] <- "34.752"

d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "barros12")] <- "37.78"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "barros12")] <- "-25.497"

d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "basaran12")] <- "41.28"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "basaran12")] <- "36.336"

d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "jacquemart21" & d$source.population == "Aise, France")] <- "47.27"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "jacquemart21" & d$source.population == "Aise, France")] <- "6.331"
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "jacquemart21" & d$source.population == "Boton, Belgium")] <- "50.504"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "jacquemart21" & d$source.population == "Boton, Belgium")] <- "4.470"
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "jacquemart21" & d$source.population == "Prelleu, France")] <- "48.124"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "jacquemart21" & d$source.population == "Prelleu, France")] <- "-3.724"
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "jacquemart21" & d$source.population == "Pairees, Belgium")] <- "50.504"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "jacquemart21" & d$source.population == "Pairees, Belgium")] <- "4.470"

# d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "javanmard14")] no location given
# d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "javanmard14")] no location given

d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "jensen97")] <- "42.519"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "jensen97")] <- "43.15"

d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "jusung16")] <- "37.527"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "jusung16")] <- "128.235"

d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "kato11")] <- "34.831"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "kato11")] <- "124.549"

d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "keshtkar08")] <- "33.325"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "keshtkar08")] <- "53.391"

d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "kettenring07")] <- "46.730"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "kettenring07")] <- "-94.690"

d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "mattana16")] <- "39.40"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "mattana16")] <- "8.40"

d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "meena06")] <- "32.139"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "meena06")] <- "77.154"

d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "meyer94" & d$source.population == "Pinto, Utah, USA")] <- "37.538"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "meyer94" & d$source.population == "Pinto, Utah, USA")] <- "-113.517"
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "meyer94" & d$source.population == "Zion Park, Utah, USA")] <- "37.298"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "meyer94" & d$source.population == "Zion Park, Utah, USA")] <- "-113.026"
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "meyer94" & d$source.population == "Hobble Creek, Utah, USA")] <- "40.213"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "meyer94" & d$source.population == "Hobble Creek, Utah, USA")] <- "-111.48"
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "meyer94" & d$source.population == "Solitude, Utah, USA")] <- "40.621"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "meyer94" & d$source.population == "Solitude, Utah, USA")] <- "-111.593"
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "meyer94" & d$source.population == "Cache Valley, Utah, USA")] <- "38.725"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "meyer94" & d$source.population == "Cache Valley, Utah, USA")] <- "-109.521"
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "meyer94" & d$source.population == "Lava Hot Springs, Idaho, USA")] <- "42.619"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "meyer94" & d$source.population == "Lava Hot Springs, Idaho, USA")] <- "-112.011"
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "meyer94" & d$source.population == "Dewey Mine, Idaho, USA")] <- "45.931"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "meyer94" & d$source.population == "Dewey Mine, Idaho, USA")] <- "-116.011"
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "meyer94" & d$source.population == "South of Price, Utah, USA")] <- "39.601"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "meyer94" & d$source.population == "South of Price, Utah, USA")] <- "-110.81"
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "meyer94" & d$source.population == "Lower LaSal, Utah, USA")] <- "38.302"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "meyer94" & d$source.population == "Lower LaSal, Utah, USA")] <- "-109.269"
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "meyer94" & d$source.population == "Upper LaSal, Utah, USA")] <- "38.302"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "meyer94" & d$source.population == "Upper LaSal, Utah, USA")] <- "-109.269"
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "meyer94" & d$source.population == "West of Vernal, Utah, USA")] <- "40.455"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "meyer94" & d$source.population == "West of Vernal, Utah, USA")] <- "-109.536"
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "meyer94" & d$source.population == "North of Lapointe, Utah, USA")] <- "40.4022"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "meyer94" & d$source.population == "North of Lapointe, Utah, USA")] <- "-109.7959"
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "meyer94" & d$source.population == "Kolob Revervoir, Utah, USA")] <- "37.44"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "meyer94" & d$source.population == "Kolob Revervoir, Utah, USA")] <- "-113.048"
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "meyer94" & d$source.population == "Zion Overlook, Utah, USA")] <- "37.298"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "meyer94" & d$source.population == "Zion Overlook, Utah, USA")] <- "-113.028"
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "meyer94" & d$source.population == "Birds of Prey, Idaho, USA")] <- "43.517"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "meyer94" & d$source.population == "Birds of Prey, Idaho, USA")] <- "-116.256"
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "meyer94" & d$source.population == "Midway, Utah, USA")] <- "40.512"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "meyer94" & d$source.population == "Midway, Utah, USA")] <- "-111.474"
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "meyer94" & d$source.population == "Emigrant Pass, Nevada, USA")] <- "40.655"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "meyer94" & d$source.population == "Emigrant Pass, Nevada, USA")] <- "-111.274"
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "meyer94" & d$source.population == "Soldier Summit, Utah, USA")] <- "39.929"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "meyer94" & d$source.population == "Soldier Summit, Utah, USA")] <- "-111.078"
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "meyer94" & d$source.population == "Fairview Top, Utah, USA")] <- "39.626"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "meyer94" & d$source.population == "Fairview Top, Utah, USA")] <- "-111.44"
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "meyer94" & d$source.population == "Thistle Junction, Utah, USA")] <- "40.00"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "meyer94" & d$source.population == "Thistle Junction, Utah, USA")] <- "-111.494"
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "meyer94" & d$source.population == "Stillwater Campground, Utah, USA")] <- "40.561"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "meyer94" & d$source.population == "Stillwater Campground, Utah, USA")] <- "-110.70"
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "meyer95" & d$source.population == "Valley of Fire, Nevada, USA")] <- "36.480"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "meyer95" & d$source.population == "Valley of Fire, Nevada, USA")] <- "-114.538"
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "meyer95" & d$source.population == "Shivwits, Utah, USA")] <- "37.1811"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "meyer95" & d$source.population == "Shivwits, Utah, USA")] <- "-113.758"
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "meyer95" & d$source.population == "Oak Grove, Utah, USA")] <- "32.861"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "meyer95" & d$source.population == "Oak Grove, Utah, USA")] <- "-91.388"
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "meyer95" & d$source.population == "Pine Valley, Utah, USA")] <- "37.391"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "meyer95" & d$source.population == "Pine Valley, Utah, USA")] <- "-113.514"
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "meyer95" & d$source.population == "Kolob Road, Utah, USA")] <- "37.298"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "meyer95" & d$source.population == "Kolob Road, Utah, USA")] <- "-113.026"
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "meyer95" & d$source.population == "Tridell, Utah, USA")] <- "40.454"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "meyer95" & d$source.population == "Tridell, Utah, USA")] <- "-109.45"
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "meyer95" & d$source.population == "Cisco, Utah, USA")] <- "38.97"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "meyer95" & d$source.population == "Cisco, Utah, USA")] <- "-109.321"
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "meyer95" & d$source.population == "Twin Falls, Idaho, USA")] <- "42.556"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "meyer95" & d$source.population == "Twin Falls, Idaho, USA")] <- "-114.47"
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "meyer95" & d$source.population == "Bruneau Dunes, Idaho, USA")] <- "42.911"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "meyer95" & d$source.population == "Bruneau Dunes, Idaho, USA")] <- "-115.715"
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "meyer95" & d$source.population == "San Rafael Swell, Utah, USA")] <- "38.824"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "meyer95" & d$source.population == "San Rafael Swell, Utah, USA")] <- "-110.677"
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "meyer95" & d$source.population == "Browse Offramp, Utah, USA")] <- "37.236"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "meyer95" & d$source.population == "Browse Offramp, Utah, USA")] <- "-113.636"
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "meyer95" & d$source.population == "Potosi Pass Road, Nevada, USA")] <- "35.989"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "meyer95" & d$source.population == "Potosi Pass Road, Nevada, USA")] <- "-115.526"
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "meyer95" & d$source.population == "Oak Grove Road, Utah, USA")] <- "32.861"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "meyer95" & d$source.population == "Oak Grove Road, Utah, USA")] <- "-91.388"
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "meyer95" & d$source.population == "Duchesne, Utah, USA")] <- "40.163"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "meyer95" & d$source.population == "Duchesne, Utah, USA")] <- "-110.403"
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "meyer95" & d$source.population == "South of Price, Utah, USA")] <- "39.601"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "meyer95" & d$source.population == "South of Price, Utah, USA")] <- "-110.81"
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "meyer95" & d$source.population == "West of Vernal, Utah, USA")] <- "40.455"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "meyer95" & d$source.population == "West of Vernal, Utah, USA")] <- "-109.536"
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "meyer95" & d$source.population == "Central, Utah, USA")] <- "37.417"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "meyer95" & d$source.population == "Central, Utah, USA")] <- "-113.625"

d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "martinik14")] <- "49.818"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "martinik14")] <- "15.473"

d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "muller03")] <- "46.228"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "muller03")] <- "2.213"

d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "na11")] <- "45.757"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "na11")] <- "124.642"

d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "ordonez-salanueva15")] <- "18.182"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "ordonez-salanueva15")] <- "-97.479"

d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "parmenter96" & d$species == "angustifolia")] <- "56.13"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "parmenter96" & d$species == "angustifolia")] <- "-106.347"
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "parmenter96" & d$species == "purpurea")] <- "51.166"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "parmenter96" & d$species == "purpurea")] <- "10.452"

d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "picciau17")] <- "39.014"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "picciau17")] <- "8.932"

d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "parvin15")] <- "35.844"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "parvin15")] <- "50.972"


## June 15th 2024 trying to finish the missing lat/long by Britany

na.coords.id <- unique(d$datasetID[which(is.na(d$provenance.lat))])
#sundaramoorthy93 didn't provide any location
# d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "sundaramoorthy93")] <- "35.844"
# d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "sundaramoorthy93")] <- "50.972"

#Gotemba city should be in Shizuoka prefecture not Kanagawa prefecture... 
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "momonoki79" & d$source.population == "National Institute of Hygienic Science, Kasukabe City, Saitama Prefecture, Japan
")] <- "35.98"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "momonoki79" & d$source.population == "National Institute of Hygienic Science, Kasukabe City, Saitama Prefecture, Japan
")] <- "139.75"

#seed grown in Umbumbulu but original seed source is from Eastern Cape... for now I will enter Eastern Cape
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "ochuodho08")] <- "32.29"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "ochuodho08")] <- "26.41"

#winstead71 provided latitude only, the longitude of the location found on google earth is used
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "winstead71" & 
                         d$source.population == "Burlington County, New Jersey, USA")] <- "40"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "winstead71" & 
                          d$source.population == "Burlington County, New Jersey, USA")] <- "-74.69"
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "winstead71" & 
                         d$source.population == "Union County, Illinois, USA")] <- "37.5"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "winstead71" & 
                          d$source.population == "Union County, Illinois, USA")] <- "-89.28"
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "winstead71" & 
                         d$source.population == "Harris County, Texas, USA")] <- "29.06"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "winstead71" & 
                          d$source.population == "Harris County, Texas, USA")] <- "-95.31"
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "winstead71" & 
                         d$source.population == "Alachua County, Florida, USA")] <- "29.5"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "winstead71" & 
                          d$source.population == "Alachua County, Florida, USA")] <- "-82.30"
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "winstead71" & 
                         d$source.population == "State of San Luis Potosi, Mexico")] <- "22.5"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "winstead71" & 
                          d$source.population == "State of San Luis Potosi, Mexico")] <- "-100.99"
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "winstead71" & 
                         d$source.population == "State of Hidalgo, Mexico")] <- "21.33"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "winstead71" & 
                          d$source.population == "State of Hidalgo, Mexico")] <- "-98.76"

d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "chakraborty92")] <- "30.56"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "chakraborty92")] <- "77.30"

d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "al-absi10")] <- "30.83"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "al-absi10")] <- "35.62"

#kalimuthu95 population source should be just Coimbatore
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "kalimuthu95")] <- "11.02"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "kalimuthu95")] <- "76.96"

#liu13 figure 4 data is across 5 seed lots (each has different provenance, thus NA for lat long)
# should add "-" to the longitude *confirm with Deirdre first
#edwards96 seeds were mixed

#morozowska02 also mixed up the seeds (has separated data as well)

d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "markovic20")] <- "44.81"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "markovic20")] <- "20.46"

d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "morozowska02" &
                         d$source.population == "Gniezno, Poland")] <- "52.53"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "morozowska02" &
                          d$source.population == "Gniezno, Poland")] <- "17.58"
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "morozowska02" &
                         d$source.population == "Poznań, Poland")] <- "52.31"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "morozowska02" &
                          d$source.population == "Poznań, Poland")] <- "16.92"
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "morozowska02" &
                         d$source.population == "Konin, Poland")] <- "52.22"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "morozowska02" &
                          d$source.population == "Konin, Poland")] <- "18.25"

#javanmard14 didn't provide any info regarding location

d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "Aldridge1993")] <- "46.73"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "Aldridge1993")] <- "94.69"

d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "Aldridge1994")] <- "46.73"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "Aldridge1994")] <- "-94.69"

d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "Aldridge1995")] <- "46.73"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "Aldridge1995")] <- "-94.69"

d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "Aldridge1996")] <- "46.73"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "Aldridge1996")] <- "-94.69"

d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "Aldridge1997")] <- "46.73"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "Aldridge1997")] <- "-94.69"

d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "Aldridge1998")] <- "46.73"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "Aldridge1998")] <- "-94.69"

#nasri14 no location given 

d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "lee06")] <- "37.90"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "lee06")] <- "126.75"

d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "rizwan18")] <- "30.19"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "rizwan18")] <- "71.49"

#grose57 no location
#maithani90 no location
#castro95 no location
#millaku12 albanian alp is located in Albania in Google Earth (the range of mountain extends to Kosovo)

d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "millaku12")] <- "42.45"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "millaku12")] <- "19.80"

#moradi12 no location 
#nkomo09 no location

d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "okay11")] <- "39.80"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "okay11")] <- "32.81"

#olmez17 Sarikum-Sinop are two nearby towns, I suspect the seeds were collected in the mountains around the area (location entered is Sarikum)

d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "olmez17")] <- "42.02"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "olmez17")] <- "34.92"

d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "pritchard93")] <- "51.07"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "pritchard93")] <- "-0.09"

#prknova15 actually has a location Kostelec nad Černými lesy 
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "prknova15")] <- "49.99"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "prknova15")] <- "14.86"

d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "rahnama-ghahfarokhi07")] <- "32.33"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "rahnama-ghahfarokhi07")] <- "50.86"

#raisi13 no location, probably Iran 

d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "ranil15")] <- "39.48"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "ranil15")] <- "-0.34"


unique(d$datasetID[which(d$lat.long.coarse == "Y" & is.na(d$provenance.lat))])
no.location <- c("javanmard14", "nasri14", "edwards96", "sundaramoorthy93", "grose57", 
                 "maithani90", "castro95", "moradi12", "nkomo09", "raisi13", )
############################################################################

## fixing continent points ... this needs to BE CHECKED!

d$continent[which(d$continent == "USA")] <- "North America"

North_America <- d[which(d$continent == "North America"),]

d$provenance.long[which(d$continent == "North America" & d$provenance.long == "83.826")] <- "-83.826"
d$provenance.long[which(d$continent == "North America" & d$provenance.long == "94.69")] <- "-94.69"
d$provenance.long[which(d$continent == "North America" & d$provenance.long == "-10.452")] <- "10.452"

d$continent[which(d$continent == "North America" & d$source.population == "Germany")] <- "Europe"
d$provenance.long[which(d$continent == "Europe" & d$provenance.long < -27 & d$provenance.lat > 37)] <- "25.497"
d$provenance.long[which(d$continent == "Europe" & d$provenance.long == "-39.5")] <- "39.5"
d$provenance.long[which(d$continent == "Europe" & d$provenance.long == "-36.39")] <- "36.39"
d$provenance.long[which(d$continent == "Europe" & d$provenance.long == "-36.39")] <- "36.39"
d$provenance.long[which(d$continent == "Europe" & d$provenance.long == "15.5474")]  <- "-15.5474"
d$provenance.long[which(d$continent == "Europe" & d$provenance.long == "16.6291")]  <- "-16.6291"

d$provenance.long[which(d$continent == "South America" & d$provenance.long == "60.56")]  <- "-60.56"
d$provenance.lat[which(d$continent == "South America" & d$provenance.lat == "31.26")]  <- "-31.26"

d$provenance.long[which(d$continent == "Asia" & d$provenance.long == "-79.73")] <- "79.73"
d$provenance.long[which(d$continent == "Asia" & d$provenance.long == "-79.05")] <- "79.05"
d$provenance.long[which(d$continent == "Asia" & d$provenance.long == "-121.186944")] <- "121.186944"
d$provenance.long[which(d$continent == "Asia" & d$provenance.long == "-121.117")] <- "121.117"
d$provenance.long[which(d$continent == "Asia" & d$provenance.long == "-50.133333333333297")] <- "50.133333333333297"

d$provenance.lat[which(d$continent == "Asia" & d$provenance.long == "124.549")] <- "133.9198"
d$provenance.long[which(d$continent == "Asia" & d$provenance.lat == "34.831")] <- "34.656"

d$provenance.lat[which(d$continent == "Africa" & d$provenance.lat == "-39.983333")] <- "-33.988"
############################################################################

## projecting coordinates onto a basemap this needs to BE CHECKED!

na.coords <- d[which(is.na(d$provenance.lat)),]

unique(d$provenance.lat)
unique(d$provenance.long)

d_filtered <- d[which(d$provenance.lat != "38.967 and 37.9" | 
                              d$provenance.lat != "~34-34.666667" |
                              d$provenance.lat != "9.4167/9.3833" |
                              d$provenance.long != "42.0333/42.0167" |
                              d$provenance.long != "~105.5-106.5"),]

d_filtered <- d_filtered[which(!is.na(d_filtered$provenance.lat)),]
unique(d_filtered$provenance.lat) # 9.4167/9.3833 still in the dataframe


coords <- d_filtered[, c("provenance.long", "provenance.lat", "continent")]
unique(coords$continent)
# lon <- d_filtered$provenance.long
# lat <- d_filtered$provenance.lat
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

