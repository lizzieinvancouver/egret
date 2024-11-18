## Updated 30 November 2023 ##
## By Lizzie and Deirdre ##
## Then mostly by CRD #

# Read in the data by running cleanall.R

### ### ### ### ### ### ### ### ###
# Standardize NA format in lat/long
### ### ### ### ### ### ### ### ###
d$provenance.lat <- gsub("N/A", "NA", d$provenance.lat)
d$provenance.long <- gsub("N/A", "NA", d$provenance.long)

### ### ### ### ### ### ### ### ###
# Fix or add source.population 
### ### ### ### ### ### ### ### ###
# bhatt00: source.population in wrong column
d$source.population[which(d$datasetID == "bhatt00" & d$other.treatment == "Kalika - population")] <- "Kalika, Kumaun, Himalaya"
d$source.population[which(d$datasetID == "bhatt00" & d$other.treatment == "Jalna - population")] <- "Jalna, Kumaun, Himalaya"
d$source.population[which(d$datasetID == "bhatt00" & d$other.treatment == "Binsar - population")] <- "Binsar, Kumaun, Himalaya"
# boscagli01: adding missing source.population
d$source.population[which(d$datasetID == "boscagli01")] <- "Rosellae, Grosseto, Italy"
# parvin15: adding missing source.population
d$source.population[which(d$datasetID == "parvin15")] <- "Kostelec nad Černými lesy"
# kalimuthu95: adding missing source.population
d$source.population[which(d$datasetID == "kalimuthu95")] <- "Coimbatore"
# naseri18: adding missing source.population
d$source.population[which(d$datasetID == "naseri18")] <- "Kord-koy, Golestan, Iran"
# tabatabaeian18: adding missing source.population
d$source.population[which(d$datasetID == "tabatabaeian18")] <- NA
#tylkowski91: typo
d$source.population[which(d$datasetID == "tylkowski91" & d$source.population == "Pozman")] <- "Poznan"
#tylkowski91: removing Pozman as stated in table 1
d$source.population[which(d$datasetID == "tylkowski91" & d$source.population == "Arbor Kornicke, Pozman")] <- "Arbor, Kornicke"
#tylkowski91: removing Pozman as stated in table 1 AND removing ","
d$source.population[which(d$datasetID == "tylkowski91" & d$source.population == "Arbor, Kornicke, Pozman")] <- "Arbor, Kornicke"
#tylkowski91: adding "," to one of the entry that should have one
d$source.population[which(d$datasetID == "tylkowski91" & d$source.population == "Arbor Kornicke")] <- "Arbor, Kornicke"

# barnhill82: adding missing source.population
d$source.population[which(d$datasetID == "barnhill82")] <- "Morgan County, Tennessee, USA"
# lai03: adding missing source.population
d$source.population[which(d$datasetID == "lai03")] <- "Ninshan County, China"

### ### ### ### ### ### ### ### ### ###
# Adding missing lat/long as stated 
# Looked back into the papers       
### ### ### ### ### ### ### ### ### ###
t<-subset(d, datasetID=="chuanren04")

# Work by Tolu Amuwo where she looked up locations and manually added lat/lon
na.coords <- d[which(is.na(d$provenance.lat)),] # ASK LIZZIE: should I keep this line?
# chuanren04: adding missing lat/long
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "chuanren04")] <- "56.13"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "chuanren04")] <- "-106.347"
# albrecht20: adding missing lat/long
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "albrecht20")] <- "37.838"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "albrecht20")] <- "83.826"

# Bhatt00: has three source population with coordinates provided, only figure 1 didn't specify the provenance. coordinates of Kumaun are entered
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "bhatt00" & is.na(d$other.treatment))] <- "29.392"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "bhatt00" & is.na(d$other.treatment))] <- "79.74"
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "bhatt00" & d$other.treatment == "Kalika -population")] <- "29.63"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "bhatt00" & d$other.treatment == "Kalika -population")] <- "79.83"
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "bhatt00" & d$other.treatment == "Jalna -population")] <- "29.57"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "bhatt00" & d$other.treatment == "Jalna -population")] <- "79.68"
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "bhatt00" & d$other.treatment == "Binsar - population")] <- "29.65"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "bhatt00" & d$other.treatment == "Binsar - population")] <- "79.82"

# boscagli01: location should be Rosellae, Grosseto, Italy
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "boscagli01")] <- "42.83"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "boscagli01")] <- "11.16"

# aldridge1992 :The below only run assuming Aldridge199X gets fixed elsewhere
# DOTHIS: the locations are very wrong. I need to go back in the paper
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "aldridge1992")] <- "46.73" 
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "aldridge1992")] <- "94.69"
# amini2018
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "amini2018")] <- "36.812"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "amini2018")] <- "54.945"

#two cho18
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "cho18" 
                       & d$source.population == "Cheogju-si, Korea")] <- "36.63"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "cho18" 
                        & d$source.population == "Cheogju-si, Korea")] <- "127.50"
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "cho18"
                       & d$source.population == "Hoengseong-gun, Gangwon-do, Korea")] <- "37.51"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "cho18"
                        & d$source.population == "Hoengseong-gun, Gangwon-do, Korea")] <- "128.077"

# crank92
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "crank92")] <- "26.973"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "crank92")] <- "-99.101"
#cuena-Lombrana18
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "cuena-Lombrana18")] <- "40.02"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "cuena-Lombrana18")] <- "9.32"
# dehgan84
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "dehgan84")] <- "29.65"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "dehgan84")] <- "-82.324"
d$source.population[which(is.na(d$provenance.lat) & d$source.population == "Gainesville, Flroida")] <- "Gainesville, Florida"
# cousins10
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "cousins10")] <- "33.433"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "cousins10")] <- "-79.121"
# naseri18
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "naseri18")] <- "36.79"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "naseri18")] <- "54.11"
# naumovski05
d$source.population[which(d$datasetID == "naumovski05" & d$source.population == "Bile Natural Habitat, Croatia")] <- "Velika kapela mountain, Croatia"
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "naumovski05" & d$source.population == "Velika kapela mountain, Croatia")] <- "43.192"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "naumovski05" & d$source.population == "Velika kapela mountain, Croatia")] <- "17.194"
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "naumovski05" & d$source.population == "University of Zagreb, Croatia")] <- "45.811"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "naumovski05" & d$source.population == "University of Zagreb, Croatia")] <- "15.97"

d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "necajeva13")] <- "56.997"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "necajeva13")] <- "24.023"

d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "nin17")] <- "42.469"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "nin17")] <- "13.566"
d$source.population[which(d$datasetID == "nin17" & d$source.population == "Central Apennine, Tuscasy")] <- "Central Apennine, Tuscany"

# sacande04
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "sacande04")] <- "0.283"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "sacande04")] <- "34.752"
# barros12
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "barros12")] <- "37.78"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "barros12")] <- "-25.497"

# basaran12
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "basaran12")] <- "41.28"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "basaran12")] <- "36.336"
# jacquemart21
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "jacquemart21" & d$source.population == "Aise, France")] <- "47.27"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "jacquemart21" & d$source.population == "Aise, France")] <- "6.331"
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "jacquemart21" & d$source.population == "Boton, Belgium")] <- "50.504"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "jacquemart21" & d$source.population == "Boton, Belgium")] <- "4.470"
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "jacquemart21" & d$source.population == "Prelleu, France")] <- "48.124"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "jacquemart21" & d$source.population == "Prelleu, France")] <- "-3.724"
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "jacquemart21" & d$source.population == "Pairees, Belgium")] <- "50.504"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "jacquemart21" & d$source.population == "Pairees, Belgium")] <- "4.470"
# jensen97
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "jensen97")] <- "42.519"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "jensen97")] <- "43.15"
# jusung16
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "jusung16")] <- "37.527"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "jusung16")] <- "128.235"
# kato11
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "kato11")] <- "34.831"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "kato11")] <- "124.549"
# keshtkar08
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "keshtkar08")] <- "33.325"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "keshtkar08")] <- "53.391"
# kettenring07
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "kettenring07")] <- "46.730"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "kettenring07")] <- "-94.690"
# mattana16
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "mattana16")] <- "39.40"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "mattana16")] <- "8.40"
# meena06
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "meena06")] <- "32.139"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "meena06")] <- "77.154"
# meyer94
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
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "meyer94" & d$source.population == "oob Revervoir, Utah, USA")] <- "37.44"
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
# meyer95
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
# martinik14
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "martinik14")] <- "49.818" 
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "martinik14")] <- "15.473"
# muller03
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "muller03")] <- "46.228"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "muller03")] <- "2.213"
# na11
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "na11")] <- "45.757"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "na11")] <- "124.642"
# ordonez-salanueva15
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "ordonez-salanueva15")] <- "18.182"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "ordonez-salanueva15")] <- "-97.479"
# parmenter96
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "parmenter96" & d$species == "angustifolia")] <- "56.13"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "parmenter96" & d$species == "angustifolia")] <- "-106.347"
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "parmenter96" & d$species == "purpurea")] <- "51.166"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "parmenter96" & d$species == "purpurea")] <- "10.452"
# picciau17
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "picciau17")] <- "39.014"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "picciau17")] <- "8.932"

#parvin15 seed from BotanicalGarden, College of Agriculture and Natural Resources,Karaj, Iran.
#coordinates not too far from Karaj, Iran 
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "parvin15")] <- "35.844"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "parvin15")] <- "50.972"

#sundaramoorthy93 didn't provide any location DOTHIS
# d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "sundaramoorthy93")] <- "35.844"
# d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "sundaramoorthy93")] <- "50.972"

#Gotemba city should be in Shizuoka prefecture not Kanagawa prefecture... DOTHIS
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "momonoki79" & d$source.population == "National Institute of Hygienic Science, Kasukabe City, Saitama Prefecture, Japan
")] <- "35.98"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "momonoki79" & d$source.population == "National Institute of Hygienic Science, Kasukabe City, Saitama Prefecture, Japan
")] <- "139.75"

# ochuodho08: seed grown in Umbumbulu but original seed source is from Eastern Cape... Eastern Cape was entered
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "ochuodho08")] <- "32.29"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "ochuodho08")] <- "26.41"

# winstead71: provided latitude only, the longitude of the location found on google earth is used. Rounded up to 2 decimals. checked in paper
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "winstead71" & 
                         d$source.population == "Burlington County, New Jersey, USA")] <- "40" 
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "winstead71" & 
                          d$source.population == "Burlington County, New Jersey, USA")] <- "-74.69"
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "winstead71" & 
                         d$source.population == "Union County, Illinois, USA")] <- "37.50" 
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "winstead71" & 
                          d$source.population == "Union County, Illinois, USA")] <- "-89.28"
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "winstead71" & 
                         d$source.population == "Harris County, Texas, USA")] <- "29.06"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "winstead71" & 
                          d$source.population == "Harris County, Texas, USA")] <- "-95.31"
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "winstead71" & 
                         d$source.population == "Alachua County, Florida, USA")] <- "29.50" 
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "winstead71" & 
                          d$source.population == "Alachua County, Florida, USA")] <- "-82.30"
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "winstead71" & 
                         d$source.population == "State of San Luis Potosi, Mexico")] <- "22.50" 
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "winstead71" & 
                          d$source.population == "State of San Luis Potosi, Mexico")] <- "-100.99"
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "winstead71" & 
                         d$source.population == "State of Hidalgo, Mexico")] <- "21.33"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "winstead71" & 
                          d$source.population == "State of Hidalgo, Mexico")] <- "-98.76"
# chakraborty92
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "chakraborty92")] <- "30.56" 
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "chakraborty92")] <- "77.30"
# al-absi10
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "al-absi10")] <- "30.83"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "al-absi10")] <- "35.62"

#kalimuthu95: population source should be just Coimbatore DOTHIS
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "kalimuthu95")] <- "11.02"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "kalimuthu95")] <- "76.96"

#liu13 figure 4 data is across 5 seed lots (each has different provenance, thus NA for lat long) DOTHIS
# should add "-" to the longitude *confirm with Deirdre first
# markovic20
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "markovic20")] <- "44.81"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "markovic20")] <- "20.46"
# morozowska02
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
# aldridge1993
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "aldridge1993")] <- "46.73"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "aldridge1993")] <- "94.69"
# aldridge1994
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "aldridge1994")] <- "46.73"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "aldridge1994")] <- "-94.69"
# aldridge1995
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "aldridge1995")] <- "46.73"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "aldridge1995")] <- "-94.69"
# aldridge1996
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "aldridge1996")] <- "46.73"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "aldridge1996")] <- "-94.69"
# aldridge1997
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "aldridge1997")] <- "46.73"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "aldridge1997")] <- "-94.69"
# aldridge1998
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "aldridge1998")] <- "46.73"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "aldridge1998")] <- "-94.69"
# lee06
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "lee06")] <- "37.90"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "lee06")] <- "126.75"

#rizwan18: seed supplied by a company from Multan Pakistan
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "rizwan18")] <- "30.19"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "rizwan18")] <- "71.49"

#millaku12: albanian alp. Location taken from Google Earth 
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "millaku12")] <- "42.45"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "millaku12")] <- "19.80"
# okay11
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "okay11")] <- "39.80"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "okay11")] <- "32.81"

#olmez17 Sarikum-Sinop are two nearby towns, I suspect the seeds were collected in the mountains around the area (location entered is Sarikum) DOTHIS
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "olmez17")] <- "42.02"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "olmez17")] <- "34.92"
# pritchard93
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "pritchard93")] <- "51.07"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "pritchard93")] <- "-0.09"

# prknova1 location is Kostelec nad Černými lesy 
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "prknova15")] <- "49.99"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "prknova15")] <- "14.86"
# rahnama-ghahfarokhi07
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "rahnama-ghahfarokhi07")] <- "32.33"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "rahnama-ghahfarokhi07")] <- "50.86"
# ranil15
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "ranil15")] <- "39.48"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "ranil15")] <- "-0.34"
# roh08
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "roh08")] <- "47.26"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "roh08")] <- "-11.38"
# rostamipoor20
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "rostamipoor20" &
                       d$source.population == "Semirom, Iran")] <- "31.41"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "rostamipoor20" &
                          d$source.population == "Semirom, Iran")] <- "51.57"
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "rostamipoor20" &
                         d$source.population == "Damavand, Iran")] <- "35.7"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "rostamipoor20" &
                          d$source.population == "Damavand, Iran")] <- "52.06"
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "rostamipoor20" &
                         d$source.population == "Zanjan, Iran")] <- "36.68"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "rostamipoor20" &
                          d$source.population == "Zanjan, Iran")] <- "48.51"
# rouhi13
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "rouhi13")] <- "34.80"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "rouhi13")] <- "48.51"
# rouhi12
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "rouhi12")] <- "34.80"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "rouhi12")] <- "48.51"
# rubin18
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "rubin18")] <- "41.43"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "rubin18")] <- "-81.37"
# thomsen02
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "thomsen02")] <- "54.92"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "thomsen02")] <- "9.58"
# tylkowski91
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "tylkowski91" & 
                   d$source.population == "Arbor, Kornicke")] <- "52.24"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "tylkowski91" &
                          d$source.population == "Arbor, Kornicke")] <- "17.02"
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "tylkowski91" &
                   d$source.population == "Poznan")] <- "52.41"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "tylkowski91" &
                          d$source.population == "Poznan")] <- "16.93" #typo, should be Poznan
# tylkowski09
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "tylkowski09" &
                   d$source.population == "Wieluń Forest District")] <- "51.22"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "tylkowski09" &
                          d$source.population == "Wieluń Forest District")] <- "18.57"
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "tylkowski09" &
                   d$source.population == "Grodziec Forest District")] <- "51.17"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "tylkowski09" &
                          d$source.population == "Grodziec Forest District")] <- "15.79" 

# yuan21
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "yuan21")] <- "34.61" 
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "yuan21")] <- "127.28" # checked long and its wrong. DOTHIS
#yusefi-tanha19: seeds provided by an agriculture company... put the location of the company for now DOTHIS
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "yusefi-tanha19")] <- "32.68"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "yusefi-tanha19")] <- "51.65"
#zadeh15 seed also from company DOTHIS
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "zadeh15")] <- "32.54"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "zadeh15")] <- "51.69"
# carpenter92
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "carpenter92")] <- "27.66"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "carpenter92")] <- "-81.51"
# carpenter92
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "arslan11")] <- "40.07"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "arslan11")] <- "29.22"
# edwards73_1
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "edwards73_1" &
                   d$source.population == "Ucona River, Vancouver Island, British Columbia, Canada")] <- "49.71"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "edwards73_1" &
                          d$source.population == "Ucona River, Vancouver Island, British Columbia, Canada")] <- "-125.10"
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "edwards73_1" &
                         d$source.population == "Adam River, Vancouver Island, British Columbia, Canada")] <- "50.46"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "edwards73_1" &
                          d$source.population == "Adam River, Vancouver Island, British Columbia, Canada")] <- "-126.28"
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "edwards73_1" &
                         d$source.population == "Sheringham Beach, Shirley, British Columbia, Canada")] <- "48.38"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "edwards73_1" &
                          d$source.population == "Sheringham Beach, Shirley, British Columbia, Canada")] <- "-123.92"
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "edwards73_1" &
                         d$source.population == "Gold River, Vancouver Island, British Columbia, Canada")] <- "49.78"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "edwards73_1" &
                          d$source.population == "Gold River, Vancouver Island, British Columbia, Canada")] <- "-126.05"
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "edwards73_1" &
                         d$source.population == "Misery Creek, British Columbia, Canada")] <- "49.67" # not on Google Earth, info from https://mapcarta.com/
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "edwards73_1" &
                          d$source.population == "Misery Creek, British Columbia, Canada")] <- "-123.58"
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "edwards73_1" &
                         d$source.population == "Qualicum Beach, Vancouver Island, British Columbia, Canada")] <- "49.35"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "edwards73_1" &
                          d$source.population == "Qualicum Beach, Vancouver Island, British Columbia, Canada")] <- "-124.44"
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "edwards73_1" &
                         d$source.population == "Mount Todd, British Columbia, Canada")] <- "48.62"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "edwards73_1" &
                          d$source.population == "Mount Todd, British Columbia, Canada")] <- "-123.94"
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "edwards73_1" &
                         d$source.population == "Rupert Inlet, British Columbia, Canada")] <- "50.58" # not on Google Earth, info from https://mapcarta.com/
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "edwards73_1" &
                          d$source.population == "Rupert Inlet, British Columbia, Canada")] <- "-127.51"
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "edwards73_1" &
                         d$source.population == "East Thurlow Island, British Columbia, Canada")] <- "50.38"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "edwards73_1" &
                          d$source.population == "East Thurlow Island, British Columbia, Canada")] <- "-125.44"
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "edwards73_1" &
                         d$source.population == "Kasiks River - Skeena River Junction, British Columbia, Canada")] <- "54.29" #mannually pin the junction 
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "edwards73_1" &
                          d$source.population == "Kasiks River - Skeena River Junction, British Columbia, Canada")] <- "-129.40"
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "edwards73_1" &
                         d$source.population == "Finlayson Arm, Saanich Inlet, Victoria, British Columbia, Canada")] <- "48.50"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "edwards73_1" &
                          d$source.population == "Finlayson Arm, Saanich Inlet, Victoria, British Columbia, Canada")] <- "-123.55"

# edwards73_2
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "edwards73_2" &
                         d$source.population == "Northern end of Nitinat Lake, British Columbia, Canada")] <- "48.82" #Nitnat River because they said "north end"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "edwards73_2" &
                          d$source.population == "Northern end of Nitinat Lake, British Columbia, Canada")] <- "-124.68"
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "edwards73_2" &
                         d$source.population == "Finlayson Arm, Saanich Inlet, Victoria, British Columbia, Canada")] <- "48.50" 
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "edwards73_2" &
                          d$source.population == "Finlayson Arm, Saanich Inlet, Victoria, British Columbia, Canada")] <- "-123.55"
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "edwards73_2" &
                         d$source.population == "Caycuse, Lake Cowichan, British Columbia, Canada")] <- "48.88"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "edwards73_2" &
                          d$source.population == "Caycuse, Lake Cowichan, British Columbia, Canada")] <- "-124.37"
# madeiras07
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "madeiras07")] <- "44.05"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "madeiras07")] <- "-91.66"
# marcello15
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "marcello15")] <- "9.4" # didn't find the location of the farm, here are Tamele coordinates 
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "marcello15")] <- "0.84"
# watanabe22
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "watanabe22")] <- "33.6" 
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "watanabe22")] <- "132.79"
# washitani89 paper not in folder DOTHIS
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "washitani89")] <- "35.72" 
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "washitani89")] <- "139.74"
# scocco98
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "scocco98" & 
                         d$source.population == "Vernasca, Piacenza, Emilia-Romagna, Italy")] <- "44.80" 
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "scocco98" & 
                          d$source.population == "Vernasca, Piacenza, Emilia-Romagna, Italy")] <- "9.83"
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "scocco98" & 
                         d$source.population == " Gossolengo, Piacenza, Emilia-Romagna, Italy")] <- "45" 
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "scocco98" & 
                          d$source.population == " Gossolengo, Piacenza, Emilia-Romagna, Italy")] <- "9.62"
# washitani85
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "washitani85")] <- "35.69" 
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "washitani85")] <- "139.53"
# zhou03
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "zhou03")] <- "24.88" 
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "zhou03")] <- "102.83"
# barnhill82
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "barnhill82")] <- "36.10" 
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "barnhill82")] <- "-84.65"
# li11
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "li11" | d$datasetID == "li11 ")] <- "38.63" 
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "li11" | d$datasetID == "li11 ")] <- "117.23"
# alhelal96
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "alhelal96")] <- "24.71" 
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "alhelal96")] <- "46.68"

#bibby53 paper not in folder DOTHIS
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "bibby53")] <- "40.90" 
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "bibby53")] <- "174.89"

#fulbright86 3 source locations all in southeastern texas so i entered the coordinates of texas (debatable) DOTHIS
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "fulbright86")] <- "31.97" 
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "fulbright86")] <- "-99.90"

#geszprych02
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "geszprych02")] <- "52.23" 
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "geszprych02")] <- "21.01"
# alhelal96
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "gimenez-benavides13")] <- "43.26" 
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "gimenez-benavides13")] <- "-3.50"
# goggans74
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "goggans74")] <- "32.32" 
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "goggans74")] <- "-86.90"
# rehman00
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "rehman00")] <- "35.83" 
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "rehman00")] <- "128.57"
# statton17
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "statton17")] <- "32.03" 
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "statton17")] <- "115.77"
# walck12
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "walck12")] <- "35.77" 
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "walck12")] <- "-86.34"
# kolodziejek19
d$provenance.lat[which(d$datasetID == "kolodziejek19" & d$source.population == "Rze˛dkowice (Cze˛stochowa Upland)")] <- "50.57"

# kolodziejek19 DOTHIS
d$provenance.long[which(d$datasetID == "kolodziejek19" & d$source.population == "Rze˛dkowice (Cze˛stochowa Upland)")] <- "19.48"
# exp0
d$provenance.lat[which(d$datasetID == "kolodziejek19" & d$source.population == "Ornak (Western Tatra Mts)")] <- "49.22" 

# exp0
d$provenance.long[which(d$datasetID == "kolodziejek19" & d$source.population == "Ornak (Western Tatra Mts)")] <- "19.83"

# exp0
d$provenance.lat[which(d$datasetID == "kolodziejek19" & d$source.population == "Kłobuck-Smugi (Wielun´ Upland)")] <- "50.91" 
 
# exp0
d$provenance.long[which(d$datasetID == "kolodziejek19" & d$source.population == "Kłobuck-Smugi (Wielun´ Upland)")] <- "18.97"

# exp0
d$provenance.lat[which(d$datasetID == "kolodziejek19" & d$source.population == "Podde˛bice (Łask Elevation)")] <- "51.89" 
# kulkarni06
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "kulkarni06")] <- "29.61" 
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "kulkarni06")] <- "30.35"
# lee21
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "lee21")] <- "35.62" 
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "lee21")] <- "129.00"
# lai03
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "lai03")] <- "33.31" #the paper is in Mandarin, and instead of Qinling China, the seeds were collected from Ninshan County (near Qinling)
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "lai03")] <- "108.31"
# langlois17
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "langlois17")] <- "45.56" 
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "langlois17")] <- "-73.56"
# amooaghaie09
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "amooaghaie09")] <- "32.65" 
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "amooaghaie09")] <- "51.67"
# harrington09: Olympia and Matlock area (mixed)
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "harrington09")] <- "47.75"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "harrington09")] <- "-120.74"
# herron01
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "herron01")] <- "40.39" 
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "herron01")] <- "175.61"

#irvani12,13,14 couldn't find Fozveh research station in Google Earth, coordinates were in a weird form in the paper: Lat: 361 270 N; Lon: 591 630 E DOTHIS

############################################################################
## fixing continent points ... this needs to BE CHECKED! DOTHIS
############################################################################


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




### ### ### ### ### ### ### ### ###
# Fix locations have lat long in same entry AND entries with N or S in the coordinate
### ### ### ### ### ### ### ### ###

### === ### === ### === ### === 
# For latitude
### === ### === ### === ### === 
# karlsson08 : 9.4167/9.3833. They merged the two locations together, they are only 5km appart, first latitude value was chosen
d$provenance.lat[which(d$datasetID == "karlsson08" & d$provenance.lat == "9.4167/9.3833", "9.4167")] <- "9.4167"
# teimouri13 : 35.5 and 36.3
# the one below, we will need to look into the paper for more info. There is a "and" but it's the same source population DOTHIS
#subset(d, provenance.lat == "35.5 and 36.3")
# zulfiqar15
d$provenance.lat[which(d$datasetID == "zulfiqar15" & d$provenance.lat == "34.464444 N")] <- "34.46"
# zulfiqar15
d$provenance.lat[which(d$datasetID == "zulfiqar15" & d$provenance.lat == "34.358333 N")] <- "34.36"
# zulfiqar15 : " 34.218611 N". R doesn't detect this entry, probably because of the space. Function to force every entry that isn't the problematic one and removing the space
d$provenance.lat <- ifelse(d$datasetID == "zulfiqar15" & !d$provenance.lat %in% c("34.46", "34.36"), "34.22", d$provenance.lat)
# li17 
d$provenance.lat[which(d$datasetID == "li17" & d$provenance.lat == "31.0333 N")] <- "31.0333"
# li21
d$provenance.lat[which(d$datasetID == "li21" & d$provenance.lat == "44.316667 N")] <- "44.316667"
# li21
d$provenance.lat[which(d$datasetID == "li21" & d$provenance.lat == "42.0760 S")] <- "-42.0760"
# yang14
d$provenance.lat[which(d$datasetID == "yang14" & d$provenance.lat == "36.11139 N")] <- "36.11139"
# teimouri13: Chose the first location given, they merged the two locations together
d$provenance.lat[which(d$datasetID == "teimouri13" & d$provenance.lat == "35.5 and 36.3")] <- "35.47"

### === ### === ### === ### === 
# For longitude
### === ### === ### === ### === 
# karlsson08 :42.0333/42.0167. They merged the two locations together, they are only 5km appart, first latitude value was chosen
d$provenance.lat[which(d$datasetID == "karlsson08" & d$provenance.lat == "42.0333/42.0167")] <- "42.0333"
# zulfiqar15
d$provenance.long[which(d$datasetID == "zulfiqar15" & d$provenance.long == "71.625833 E")] <- "71.625833"
# zlesak07
d$provenance.long[which(d$datasetID == "zlesak07" & d$provenance.long == "93.16667 W")] <- "-93.16667"
# li17
d$provenance.long[which(d$datasetID == "li17" & d$provenance.long == "112.26667 E")] <- "112.26667"
# li21
d$provenance.long[which(d$datasetID == "li21" & d$provenance.long == "86.95 E")] <- "86.95"
# li21
d$provenance.long[which(d$datasetID == "li21" & d$provenance.long == "147.0275 E")] <- "147.0275"
# yang14
d$provenance.long[which(d$datasetID == "yang14" & d$provenance.long == " 81.8125 W")] <- "-81.8125"
# teimouri13: Chose the first location given, they merged the two locations together
d$provenance.long[which(d$datasetID == "teimouri13" & d$provenance.long == "59.05 and 59.2")] <- "59.05"

#=== === === === === === === === === === === === === === === === === === === ===
##### STEP 3. Check in the articles for exact coordinates #####
#=== === === === === === === === === === === === === === === === === === === ===
### === ### === ### === ### === 
# For latitude
### === ### === ### === ### === 
### zhou08 wasn't flagged until I saw its longitude. Lat and long were switched.
d$provenance.lat[which(d$datasetID == "zhou08" & d$provenance.lat == "103.42")] <- "32.00"
# brandel05: 54 DOTHIS to DOUBLE CHECK
# nurse08: 42. Different environments were used within Essex Ontario, not a precise location was given. Decided to keep 42 because not a huge climatic difference. No change required

# yeom21: 39
# they give a location that isn't precise and searching the institute doesn't give anything conclusive. It's in North Korea... It's not the source population also, it's a crop research institute. My opinion: exclude these locations from the coordinates. DOTHIS: go back in git issue and see what was decided

# zardari19: 35
d$provenance.lat[which(d$datasetID == "zardari19" & d$provenance.lat == "35")] <- "35.58" # given location had no decimals. A central point in the town indicated in the article was taken. 

# zare11: 3147
d$provenance.lat[which(d$datasetID == "zare11" & d$provenance.lat == "3147")] <- "31.47" # missing a .

# esmaeili09: 48
# Location provided: Marais poitevin which is a 120 000ha wetland. Not sure what to do with this. DOTHIS: go back in git issue and see what was decided

# seng20: 103.3
d$provenance.lat[which(d$datasetID == "seng20" & d$provenance.lat == "103.3")] <- "13.60" # Central location of Forest Restoration and Development “Banteay Srei” Cambodia was taken

# zlesak07: Dragging in Excel. It goes from 46 N to 62 N. Location was confirmed in article.
d$provenance.lat[which(d$datasetID == "zlesak07" & d$provenance.lat == "45 N")] <- "45.00" 
d$provenance.lat[which(d$datasetID == "zlesak07" & d$provenance.lat == "46 N")] <- "45.00" 
d$provenance.lat[which(d$datasetID == "zlesak07" & d$provenance.lat == "47 N")] <- "45.00" 
d$provenance.lat[which(d$datasetID == "zlesak07" & d$provenance.lat == "48 N")] <- "45.00" 
d$provenance.lat[which(d$datasetID == "zlesak07" & d$provenance.lat == "49 N")] <- "45.00" 
d$provenance.lat[which(d$datasetID == "zlesak07" & d$provenance.lat == "50 N")] <- "45.00" 
d$provenance.lat[which(d$datasetID == "zlesak07" & d$provenance.lat == "51 N")] <- "45.00" 
d$provenance.lat[which(d$datasetID == "zlesak07" & d$provenance.lat == "52 N")] <- "45.00" 
d$provenance.lat[which(d$datasetID == "zlesak07" & d$provenance.lat == "53 N")] <- "45.00" 
d$provenance.lat[which(d$datasetID == "zlesak07" & d$provenance.lat == "54 N")] <- "45.00" 
d$provenance.lat[which(d$datasetID == "zlesak07" & d$provenance.lat == "55 N")] <- "45.00" 
d$provenance.lat[which(d$datasetID == "zlesak07" & d$provenance.lat == "56 N")] <- "45.00" 
d$provenance.lat[which(d$datasetID == "zlesak07" & d$provenance.lat == "57 N")] <- "45.00" 
d$provenance.lat[which(d$datasetID == "zlesak07" & d$provenance.lat == "58 N")] <- "45.00" 
d$provenance.lat[which(d$datasetID == "zlesak07" & d$provenance.lat == "59 N")] <- "45.00" 
d$provenance.lat[which(d$datasetID == "zlesak07" & d$provenance.lat == "60 N")] <- "45.00" 
d$provenance.lat[which(d$datasetID == "zlesak07" & d$provenance.lat == "61 N")] <- "45.00" 
d$provenance.lat[which(d$datasetID == "zlesak07" & d$provenance.lat == "62 N")] <- "45.00" 
# skordilis95 : 38.967 and 37.9 DOTHIS
# bibby53: 40.9 DOTHIS
# guo20: Took the central latitude between the two points provided in the article
d$provenance.lat[which(d$datasetID == "guo20" & d$provenance.lat == "~34-34.666667")] <- "34.33" 
# ren15: Took the central latitude betweeen the two points provided in the article
d$provenance.lat[which(d$datasetID == "ren15" & d$provenance.lat == "33.825278 - 34.136389")] <- "33.98" # 
# necajeva13 : 57.233333-56.783333. DOTHIS
# olmez09 DOTHIS: check issue

### === ### === ### === ### === 
# For longitude
### === ### === ### === ### === 
# karlsson08: They merged the two locations together, they are only 5km appart, first latitude value was chosen
d$provenance.long[which(d$datasetID == "karlsson08" & d$provenance.long == "42.0333/42.0167")] <- "42.03" 
# nurse08 : Different environments were used within Essex Ontario, not a precise location was given. Decided to keep 42 because not a huge climatic difference. No change required
# olmez07 :41.83, 41.87, 41.85. # need to spend time redoing that whole scrapping. Very clearly were the locations given in the article with distinct provenance names and coordinates. DOTHIS: CHECK ISSUE IT SHOULD BE SOLVED
# yeom21 : 129. they give location that isn't precise and searching the institute does't give anything conclusive. It's in North Korea... It's not the source population also, it's a crop research institute. My opinion: exclude these locations from the coordinates.DOTHIS
# zardari19: given location had no decimals. I took a central point in the town indicated in the article.
d$provenance.long[which(d$datasetID == "zardari19" & d$provenance.long == "53")] <- "53.39" 
# zare11: missing .
d$provenance.long[which(d$datasetID == "zare11" & d$provenance.long == "5352")] <- "53.52"
# esmaeili09 : Location provided: Marais poitevin which is a 120 000ha wetland. Not sure what to do with this.  DOTHIS
# zhou08: wasn't flagged until I saw its longitude. Lat and long were switched.
d$provenance.long[which(d$datasetID == "zhou08" & d$provenance.long == "32")] <- "103.42"
# yang08 : fixed wrong conversion from hour minute to decimals
d$provenance.long[which(d$datasetID == "yang08" & d$provenance.long == "121.3")] <- "121.50" 
# guo20 : Took the central latitude betweeen the two points provided in the article
d$provenance.long[which(d$datasetID == "guo20" & d$provenance.long == "~105.5-106.5", "106.00")] <- "106.00" 
# ren15: Took the central latitude betweeen the two points provided in the article
d$provenance.long[which(d$datasetID == "ren15" & d$provenance.long == "107.373333 - 107.861389")] <- "107.62"
# zulfiqar15
d$provenance.long[which(d$datasetID == "zulfiqar15" & d$provenance.long == "73.135833 E")] <- "73.135833"
# zulfiqar15
d$provenance.long[which(d$datasetID == "zulfiqar15" & d$provenance.long == "73.472222 E")] <- "73.472222"
# zlesak07
d$provenance.long[which(d$datasetID == "zlesak07" & d$provenance.long == "93.16667 W ")] <- "93.16667"
# harrison14 
d$provenance.long[which(d$datasetID == "harrison14" & d$provenance.long == "147.0275 E")] <- "147.0275"
d$provenance.long[which(d$datasetID == "harrison14" & d$provenance.long == "147.1339 E")] <- "147.1339"
d$provenance.long[which(d$datasetID == "harrison14" & d$provenance.long == "147.7958 E")] <- "147.7958"
d$provenance.long[which(d$datasetID == "harrison14" & d$provenance.long == "148.2293 E")] <- "148.2293"
d$provenance.long[which(d$datasetID == "harrison14" & d$provenance.long == "147.2721 E")] <- "147.2721"
d$provenance.long[which(d$datasetID == "harrison14" & d$provenance.long == "147.2365 E")] <- "147.2365"
d$provenance.long[which(d$datasetID == "harrison14" & d$provenance.long == "146.3807 E")] <- "146.3807"
d$provenance.long[which(d$datasetID == "harrison14" & d$provenance.long == "147.1526 E")] <- "147.1526"
d$provenance.long[which(d$datasetID == "harrison14" & d$provenance.long == "145.6873 E")] <- "145.6873"
d$provenance.long[which(d$datasetID == "harrison14" & d$provenance.long == "145.2676 E")] <- "145.2676"
# yang14
d$provenance.long[which(d$datasetID == "yang14" & d$provenance.long == "81.8125 W")] <- "-81.8125" 

# harrison14
d$provenance.lat[which(d$datasetID == "harrison14" & d$provenance.lat == "42.0760 S")] <- "-42.0760" 
d$provenance.lat[which(d$datasetID == "harrison14" & d$provenance.lat == "41.8888 S")] <- "-41.8888" 
d$provenance.lat[which(d$datasetID == "harrison14" & d$provenance.lat == "43.2397 S")] <- "-43.2397" 
d$provenance.lat[which(d$datasetID == "harrison14" & d$provenance.lat == "41.9405 S")] <- "-41.9405" 
d$provenance.lat[which(d$datasetID == "harrison14" & d$provenance.lat == "41.3684 S ")] <- "-41.3684" # DOTHIS
d$provenance.lat[which(d$datasetID == "harrison14" & d$provenance.lat == "41.0595 S")] <- "-41.0595" 
d$provenance.lat[which(d$datasetID == "harrison14" & d$provenance.lat == "42.9005 S")] <- "-42.9005" 
d$provenance.lat[which(d$datasetID == "harrison14" & d$provenance.lat == "42.0889 S")] <- "-42.0889" 
d$provenance.lat[which(d$datasetID == "harrison14" & d$provenance.lat == "42.0273 S")] <- "-42.0273" 

# LONGITUDE
# zhang21:124.9
d$provenance.long[which(d$datasetID == "zhang21" & d$provenance.long == "124.9")] <- "124.90" 
# brandel05 :10
# d$provenance.long[which(d$datasetID == "brandel05" & d$provenance.long == "10")] <- "10.00"
# to DOUBLE CHECK
# beikmohammadi12 :55.8
d$provenance.long[which(d$datasetID == "beikmohammadi12" & d$provenance.long == "55.8")] <- "55.80"
# alptekin02 : 39.5
d$provenance.long[which(d$datasetID == "alptekin02" & d$provenance.long == "39.5")] <- "39.50"
# mulaudzi09 : 27.5
d$provenance.long[which(d$datasetID == "mulaudzi09" & d$provenance.long == "27.5")] <- "27.50"
# middleton96 : 142.5
d$provenance.long[which(d$datasetID == "middleton96" & d$provenance.long == "142.5")] <- "142.50"
# pliszko18 : 21
d$provenance.long[which(d$datasetID == "pliszko18" & d$provenance.long == "21")] <- "21.00"
# yang18 : 121.5
d$provenance.long[which(d$datasetID == "yang18" & d$provenance.long == "121.5")] <- "121.50"
# yin09 :117.4
d$provenance.long[which(d$datasetID == "yin09" & d$provenance.long == "117.4")] <- "117.40"
# chien09 121.2
d$provenance.long[which(d$datasetID == "chien09" & d$provenance.long == "121.2")] <- "121.20" # checked the article,location is good
# liu13
# fixplease
# edwards96: 124.1
# fixplease
# brandel2005: 10
d$provenance.long[which(d$datasetID == "brandel2005" & d$provenance.long == "10")] <- "10.00" # checked the article, location given isn't precise 
# ma03: -128.4, -124.8, -121.5
# pdf unvailable
# airi2009: 79.5
d$provenance.long[which(d$datasetID == "airi2009" & d$provenance.long == "79.5")] <- "79.50" # checked the article,location is good
# alptekin2002: 39.5
d$provenance.long[which(d$datasetID == "alptekin2002" & d$provenance.long == "39.5")] <- "39.50" # added decimals 
# necajeva13 :21.416667-21.05
#fixplease
# ucler18: 38.9
d$provenance.long[which(d$datasetID == "ucler18" & d$provenance.long == "38.9")] <- "38.90"
# yang18_1: 121.5
d$provenance.long[which(d$datasetID == "yang18_1" & d$provenance.long == "121.5")] <- "121.50" # checked the article,location is good
# yang18_2
d$provenance.long[which(d$datasetID == "yang18_2" & d$provenance.long == "121.5")] <- "121.50" # checked the article,location is good
# yang18_3
d$provenance.long[which(d$datasetID == "yang18_3" & d$provenance.long == "121")] <- "121.00" # checked the article,location is good
# yang10: 120.8
d$provenance.long[which(d$datasetID == "yang10" & d$provenance.long == "120.8")] <- "120.80" # checked the article,location is good
# yeom21 
# see above
# downie91: -67.4, -66.3
d$provenance.long[which(d$datasetID == "downie91" & d$provenance.long == "-67.4")] <- "-67.38" # checked the article,missing decimal when converted
d$provenance.long[which(d$datasetID == "downie91" & d$provenance.long == "-66.3")] <- "-66.30" # checked the article,location is good
# esmaeili09
# see above
# wickens01: 28
d$provenance.long[which(d$datasetID == "wickens01" & d$provenance.long == "28")] <- "28.00" # checked the article,location was estimated and seems precise enough
# joshi03: 77.2
d$provenance.long[which(d$datasetID == "joshi03" & d$provenance.long == "77.2")] <- "77.20" # checked the article,location is good
# tylkowski10: 17.1
d$provenance.long[which(d$datasetID == "tylkowski10" & d$provenance.long == "17.1")] <- "17.10" # checked the article,missing decimal when converted

##### STEP 5 FIXING +/- #####
# edwards96
d$provenance.long[which(d$datasetID == "edwards96" & d$provenance.long == "122.4")] <- "122.40"
#Removing entries in the middle of the ocean
# alptekin02: checked in article. Entry was good, but missing a negative
d$provenance.long[which(d$datasetID == "alptekin02" & d$provenance.long == "-43.74")] <- "43.74" 
# veiga-barbosa16: checked in article. Entry was good, but missing a negative
d$provenance.long[which(d$datasetID == "veiga-barbosa16" & d$provenance.long == "3.5667")] <- "-3.5667"
# ochuodho08: checked in article. Entry was good, but missing a negative
d$provenance.lat[which(d$datasetID == "ochuodho08" & d$provenance.lat == "32.29")] <- "-32.29"
# barros12: checked in article. Entry was good, but missing a negative
d$provenance.long[which(d$datasetID == "barros12" & d$provenance.long == "25.497")] <- "-25.497"
# herron01: checked in article. Entry was good, but missing a negative
d$provenance.lat[which(d$datasetID == "herron01" & d$provenance.lat == "40.39")] <- "-40.39"
# bibby53: checked in article. Entry was good, but missing a negative
d$provenance.lat[which(d$datasetID == "bibby53" & d$provenance.lat == "40.9")] <- "-40.90" 

# Create two new columns DOTHIS Check with Lizzie if I should keep the ones below. 
# d$provenance.Lat <- as.numeric(d$provenance.lat)
# d$provenance.Long <- as.numeric(d$provenance.long)
# d$provLatLon <- paste(d$provenance.lat, d$provenance.long, sep=" ")
# d$provLatLonAlt <- paste(d$provenance.lat, d$provenance.long, d$provenance.altitude, sep=" ")

## Checking on whether we need to include altitude when defining provenance

# checkprovstuff <- subset(d, select=c("provLatLon", "provenance.altitude"))
# checkprovstuff <- checkprovstuff[!duplicated(checkprovstuff), ]
# 
# checkalt <- aggregate(checkprovstuff[c("provenance.altitude")], checkprovstuff[c("provLatLon")], FUN=length)
# checkalt <- checkalt[order(checkalt$provenance.altitude),]

#=== === === === === === === === === === === === === === === === === === === ===
# 
# # #### MAP ####
# # Start a quick map with given locations
# ### Available locations
# no.na.values <- d[complete.cases(d["provenance.lat"]),]
# 
# # Select only 1 entry per provenance
# df.4.map <- no.na.values[!duplicated(no.na.values$provenance.lat), ]
# # clean columns not necessary
# df.4.map <- df.4.map[, c("datasetID", "provenance.lat", "provenance.long", "continent")]
# head(df.4.map)
# color coding
# i need to find another way than using dplyr
# library(dplyr)
# occurence <- df.4.map %>%
#   group_by(continent) %>%
#   summarize(continent = first(continent), count = n())
# head(occurence)
# # map
# plot_geo(occurence) %>%
#   layout(
#     geo = list(
#       showframe = TRUE,
#       showcoastlines = TRUE,
#       showland = TRUE,
#       landcolor = toRGB("white"),
#       countrycolor = toRGB("darkgrey"),
#       coastlinecolor = toRGB("black"),
#       coastlinewidth = 0.5,
#       lataxis = list(
#         range = c(-55, 80),
#         showgrid = FALSE
#       ),
#       lonaxis = list(
#         range = c(-130, 160),
#         showgrid = FALSE
#       )
#     )
#   ) %>%
#   # # Color gradient set to the number of papers in each country
#   # add_trace(
#   #   z = ~count, color = ~count, colors = 'GnBu',
#   #   text = ~continent, locations = ~continent,
#   #   marker = list(
#   #     line = list(width = 0.5, color = "black")
#   #   ),
#   #   # Edit the colar bar position --> make the X value negative if I want to set it on the left
#   #   colorbar = list(title = "", x = 1, y = 1.1, len = 1.03)
#   # ) %>%
# add_trace(
#   type = "scattergeo",
#   lat = ~df.4.map$provenance.lat,
#   lon = ~df.4.map$provenance.long,
#   text = ~paste("ID: ", df.4.map$datasetID),
#   mode = "markers",
#   marker = list(
#     size = 1,
#     symbol = "circle",
#     color = "lightgrey",
#     line = list(width = 1.5, color = "black")
#   )
# ) %>%
#   layout(title = "")
# 
# # save figure
# save_image(egretlocations,
#            file="/Users/christophe_rouleau-desrochers/Documents/mapegret.html")



