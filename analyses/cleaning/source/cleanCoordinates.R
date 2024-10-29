## Updated 30 November 2023 ##
## By Lizzie and Deirdre ##
## Then mostly by CRD #

# Read in the data by running cleanall.R

### ### ### ### ### ### ### ### ###
## Fix or add source.population ##
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
## Adding missing lat/long as stated ## 
## Looked back into the papers ##
### ### ### ### ### ### ### ### ### ###
t<-subset(d, datasetID=="chuanren04")
# Adding
# Work by Tolu Amuwo where she looked up locations and manually added lat/lon
na.coords <- d[which(is.na(d$provenance.lat)),] # ASK LIZZIE: should I keep this line?
# chuanren04: adding missing lat/long
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "chuanren04")] <- "56.13"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "chuanren04")] <- "-106.347"
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "albrecht20")] <- "37.838"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "albrecht20")] <- "83.826"

#Bhatt00 has three source population with coordinates provided, only figure 1 didn't specify the provenance. coordinates of Kumaun are entered
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "bhatt00" & is.na(d$other.treatment))] <- "29.392"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "bhatt00" & is.na(d$other.treatment))] <- "79.74"
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "bhatt00" & d$other.treatment == "Kalika -population")] <- "29.63"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "bhatt00" & d$other.treatment == "Kalika -population")] <- "79.83"
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "bhatt00" & d$other.treatment == "Jalna -population")] <- "29.57"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "bhatt00" & d$other.treatment == "Jalna -population")] <- "79.68"
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "bhatt00" & d$other.treatment == "Binsar - population")] <- "29.65"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "bhatt00" & d$other.treatment == "Binsar - population")] <- "79.82"

#location should be Rosellae, Grosseto, Italy
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "boscagli01")] <- "42.83"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "boscagli01")] <- "11.16"

# The below only run assuming Aldridge199X gets fixed elsewhere
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "aldridge1992")] <- "46.73" #Britany: this doens't look right
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "aldridge1992")] <- "94.69"

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


d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "crank92")] <- "26.973"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "crank92")] <- "-99.101"

d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "cuena-Lombrana18")] <- "40.02"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "cuena-Lombrana18")] <- "9.32"

d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "dehgan84")] <- "29.65"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "dehgan84")] <- "-82.324"
d$source.population[which(is.na(d$provenance.lat) & d$source.population == "Gainesville, Flroida")] <- "Gainesville, Florida"

d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "cousins10")] <- "33.433"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "cousins10")] <- "-79.121"

d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "naseri18")] <- "36.79"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "naseri18")] <- "54.11"

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

d$datasetID[which(d$datasetID == "Sacande05")] <- "sacande04"
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "sacande04")] <- "0.283"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "sacande04")] <- "34.752"

d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "barros12")] <- "37.78"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "barros12")] <- "-25.497"

#basaran12 can't find paper in google drive
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

d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "martinik14")] <- "49.818" #"origin of the stand unknown"
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

#parvin15 seed from BotanicalGarden, College of Agriculture and Natural Resources,Karaj, Iran.
#coordinates not too far from Karaj, Iran (can still specify?)
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
                         d$source.population == "Burlington County, New Jersey, USA")] <- "40" #rounded up to 2 decimals. checked in paper
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "winstead71" & 
                          d$source.population == "Burlington County, New Jersey, USA")] <- "-74.69"
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "winstead71" & 
                         d$source.population == "Union County, Illinois, USA")] <- "37.50" #rounded up to 2 decimals. checked in paper
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "winstead71" & 
                          d$source.population == "Union County, Illinois, USA")] <- "-89.28"
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "winstead71" & 
                         d$source.population == "Harris County, Texas, USA")] <- "29.06"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "winstead71" & 
                          d$source.population == "Harris County, Texas, USA")] <- "-95.31"
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "winstead71" & 
                         d$source.population == "Alachua County, Florida, USA")] <- "29.50" #rounded up to 2 decimals. checked in paper
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "winstead71" & 
                          d$source.population == "Alachua County, Florida, USA")] <- "-82.30"
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "winstead71" & 
                         d$source.population == "State of San Luis Potosi, Mexico")] <- "22.50" #rounded up to 2 decimals. checked in paper
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "winstead71" & 
                          d$source.population == "State of San Luis Potosi, Mexico")] <- "-100.99"
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "winstead71" & 
                         d$source.population == "State of Hidalgo, Mexico")] <- "21.33"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "winstead71" & 
                          d$source.population == "State of Hidalgo, Mexico")] <- "-98.76"

d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "chakraborty92")] <- "30.56" # didn't specify whether seeds were collected from the nursery also 
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

d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "aldridge1993")] <- "46.73"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "aldridge1993")] <- "94.69"

d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "aldridge1994")] <- "46.73"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "aldridge1994")] <- "-94.69"

d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "aldridge1995")] <- "46.73"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "aldridge1995")] <- "-94.69"

d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "aldridge1996")] <- "46.73"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "aldridge1996")] <- "-94.69"

d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "aldridge1997")] <- "46.73"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "aldridge1997")] <- "-94.69"

d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "aldridge1998")] <- "46.73"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "aldridge1998")] <- "-94.69"

#nasri14 no location given 

d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "lee06")] <- "37.90"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "lee06")] <- "126.75"

#rizwan18 seed supplied by a company from Multan Pakistan
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

d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "roh08")] <- "47.26"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "roh08")] <- "-11.38"

#romero05 mixes seeds from 2 locations

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

d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "rouhi13")] <- "34.80"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "rouhi13")] <- "48.51"

d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "rouhi12")] <- "34.80"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "rouhi12")] <- "48.51"

d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "rubin18")] <- "41.43"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "rubin18")] <- "-81.37"

#surya17 no location

#tabatabaeian18 didn't really mention seed source? although they said the species were native to Iran
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "tabatabaeian18")] <- NA
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "tabatabaeian18")] <- NA

d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "thomsen02")] <- "54.92"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "thomsen02")] <- "9.58"

d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "tylkowski91" & 
                   d$source.population == "Arbor, Kornicke")] <- "52.24"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "tylkowski91" &
                          d$source.population == "Arbor, Kornicke")] <- "17.02"
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "tylkowski91" &
                   d$source.population == "Poznan")] <- "52.41"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "tylkowski91" &
                          d$source.population == "Poznan")] <- "16.93" #typo, should be Poznan

d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "tylkowski09" &
                   d$source.population == "Wieluń Forest District")] <- "51.22"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "tylkowski09" &
                          d$source.population == "Wieluń Forest District")] <- "18.57"
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "tylkowski09" &
                   d$source.population == "Grodziec Forest District")] <- "51.17"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "tylkowski09" &
                          d$source.population == "Grodziec Forest District")] <- "15.79" 

#yaqoob17 no location, although "The plants were identified at the Department of Botany, University of Kashmir, Srinagar" <- does that mean seeds were collected there?
# yuan21
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "yuan21")] <- "34.61" 
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "yuan21")] <- "127.28" # checked long and its wrong. 
tmp<-subset(d, datasetID == "yuan21")
#yurteri21 from gene back so no location?

#####  #####  #####  #####  #####  #####  #####  #####  #####  #####  ##### 
##### I AM HERE FOR CHANGING CAP TO SMALL LETTERS 
#####  #####  #####  #####  #####  #####  #####  #####  #####  #####  ##### 

#yusefi-tanha19 seeds provided by an agriculture company... put the location of the company for now
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "yusefi-tanha19")] <- "32.68"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "yusefi-tanha19")] <- "51.65"

#zadeh15 seed also from company 
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "zadeh15")] <- "32.54"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "zadeh15")] <- "51.69"

d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "carpenter92")] <- "27.66"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "carpenter92")] <- "-81.51"

d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "arslan11")] <- "40.07"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "arslan11")] <- "29.22"

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

d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "madeiras07")] <- "44.05"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "madeiras07")] <- "-91.66"

d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "marcello15")] <- "9.4" # didn't find the location of the farm, here are Tamele coordinates 
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "marcello15")] <- "0.84"

#Shahi-gharahlar12 no location (possibly iran?)

#Sharma03 no location 

d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "watanabe22")] <- "33.6" 
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "watanabe22")] <- "132.79"

#Washitani89 paper not in folder
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "washitani89")] <- "35.72" 
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "washitani89")] <- "139.74"

d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "scocco98" & 
                         d$source.population == "Vernasca, Piacenza, Emilia-Romagna, Italy")] <- "44.80" 
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "scocco98" & 
                          d$source.population == "Vernasca, Piacenza, Emilia-Romagna, Italy")] <- "9.83"
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "scocco98" & 
                         d$source.population == " Gossolengo, Piacenza, Emilia-Romagna, Italy")] <- "45" 
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "scocco98" & 
                          d$source.population == " Gossolengo, Piacenza, Emilia-Romagna, Italy")] <- "9.62"

d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "washitani85")] <- "35.69" 
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "washitani85")] <- "139.53"

d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "zhou03")] <- "24.88" 
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "zhou03")] <- "102.83"

#yang16 mixed three seed sources

d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "barnhill82")] <- "36.10" 
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "barnhill82")] <- "-84.65"

d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "li11" | d$datasetID == "li11 ")] <- "38.63" 
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "li11" | d$datasetID == "li11 ")] <- "117.23"

d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "alhelal96")] <- "24.71" 
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "alhelal96")] <- "46.68"

#bibby53 paper not in folder
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "bibby53")] <- "40.90" 
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "bibby53")] <- "174.89"

#fulbright86 3 source locations all in southeastern texas so i entered the coordinates of texas (debatable)

d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "fulbright86")] <- "31.97" 
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "fulbright86")] <- "-99.90"

#geszprych02 paper not in folder
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "geszprych02")] <- "52.23" 
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "geszprych02")] <- "21.01"

d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "gimenez-benavides13")] <- "43.26" 
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "gimenez-benavides13")] <- "-3.50"

d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "goggans74")] <- "32.32" 
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "goggans74")] <- "-86.90"

d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "rehman00")] <- "35.83" 
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "rehman00")] <- "128.57"

d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "statton17")] <- "32.03" 
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "statton17")] <- "115.77"

#vleeshouwers98 no location

d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "walck12")] <- "35.77" 
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "walck12")] <- "-86.34"

#kolodziejek18 no location 
# This codes has warnings that sound bad ... fixplease -- fixed

d$provenance.lat[which(d$datasetID == "kolodziejek19" & d$source.population == "Rze˛dkowice (Cze˛stochowa Upland)")] <- "50.57"

# exp0
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


d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "kulkarni06")] <- "29.61" 
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "kulkarni06")] <- "30.35"

d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "lee21")] <- "35.62" 
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "lee21")] <- "129.00"

d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "lai03")] <- "33.31" #the paper is in Mandarin, and instead of Qinling China, the seeds were collected from Ninshan County (near Qinling)
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "lai03")] <- "108.31"

d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "langlois17")] <- "45.56" 
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "langlois17")] <- "-73.56"

d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "amooaghaie09")] <- "32.65" 
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "amooaghaie09")] <- "51.67"

#ahmad07 no location
#ahola99 no location

d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "harrington09")] <- "47.75" #Olympia and Matlock area (mixed)
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "harrington09")] <- "-120.74"

d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "herron01")] <- "40.39" 
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "herron01")] <- "175.61"

#irvani12,13,14 couldn't find Fozveh research station in Google Earth, coordinates were in a weird form in the paper: Lat: 361 270 N; Lon: 591 630 E

coarse.coords <- subset(d, d$lat.long.coarse == "Y" & !is.na(d$provenance.lat))
na.coords.id <- unique(d$datasetID[which(d$lat.long.coarse == "Y" & is.na(d$provenance.lat))])

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


# 
# # tests with a smaller df. Only take single entries
# dred <- d[!duplicated(d$datasetID), ]
# str(dred)
# # Many entries are still problematic. Some have lat and long in the lat column, some have a letter (e.g. 39N). So I'll separate those:
# # below is a pattern that takes entries that are between 0-9 and have 2 digits, followed by a "." followed by decimals that have between 2 and 16 values. I chose these because some have up to 16 decimals (weird) and 2 because otherwise there is no precise provenance 
# pattern <- "^-?[0-9]{1}\\.\\d{2,16}$|^-?[0-9]{2}\\.\\d{2,16}$|^-?[0-9]{3}\\.\\d{2,16}$"

### COLUMN LATITUDE ###
# # Filter values that have the above pattern
# dred.lat <- dred[!grepl(pattern, dred$provenance.lat), ]
# # Check how many entries are still problematic
# dred.lat$provenance.lat # at this point, there are still 89 entries that either NAs or in a bad format
# # Filter out NAs
# dred.lat2 <- dred.lat[!is.na(dred.lat$provenance.lat), ]
# dred.lat2$provenance.lat # good! now only 29 entries are not in a good format!
# # Create provenance.lat vector entries that need to be changed
# provenance.lat.chg <- dred.lat2$provenance.lat

# ### COLUMN LONGITUDE ###
# # Filter values that have the above pattern
# dred.long <- dred[!grepl(pattern, dred$provenance.long), ]
# # Check how many entries are still problematic
# dred.long$provenance.long # at this point, there are *** still  entries that either NAs or in a bad format
# # Filter out NAs
# dred.long2 <- dred.long[!is.na(dred.long$provenance.long), ]
# dred.long2$provenance.long # good! now only *** entries are not in a good format!
# # Create provenance.long vector entries that need to be changed
# provenance.long.chg <- dred.long2$provenance.long


#=== === === === === === === === === === === === === === === === === === === ===
#### Second cleaning step ####
#=== === === === === === === === === === === === === === === === === === === ===

#=== === === === === === === === === === === === === === === === === === === ===
##### STEP 1. Replace N/A by NA #####
#=== === === === === === === === === === === === === === === === === === === ===
d$provenance.lat <- gsub("N/A", "NA", d$provenance.lat)
d$provenance.long <- gsub("N/A", "NA", d$provenance.long)

#=== === === === === === === === === === === === === === === === === === === ===
##### STEP 2. Select entries that have lat long in same entry #####
#=== === === === === === === === === === === === === === === === === === === ===

# STEP 2.1. in lat
# STEP 2.1.1. karlsson08 : 9.4167/9.3833 
d$provenance.lat[which(d$datasetID == "karlsson08" & d$provenance.lat == "9.4167/9.3833", "9.4167")] <- "9.4167"# in the paper, they merged the two locations together, and since they are only 5km appart, I took the first latitude value
# STEP 2.1.2. teimouri13 : 35.5 and 36.3
# the one below, we will need to look into the paper for more info. There is a "and" but it's the same source population
#subset(d, provenance.lat == "35.5 and 36.3")
# STEP 2.1.3. zulfiqar15 : "34.464444 N "
d$provenance.lat[which(d$datasetID == "zulfiqar15" & d$provenance.lat == "34.464444 N")] <- "34.46"
# STEP 2.1.3.1. zulfiqar15 : "34.358333 N"
d$provenance.lat[which(d$datasetID == "zulfiqar15" & d$provenance.lat == "34.358333 N")] <- "34.36"
# STEP 2.1.3.2. zulfiqar15 : " 34.218611 N"
# R doesn't detect this entry, probably because of the space
# function to force every entry that isn't the problematic one
d$provenance.lat <- ifelse(d$datasetID == "zulfiqar15" & !d$provenance.lat %in% c("34.46", "34.36"), "34.22", d$provenance.lat)
# STEP 2.1.4.li17 : 31.0333 N
d$provenance.lat[which(d$datasetID == "li17" & d$provenance.lat == "31.0333 N")] <- "31.0333"
# STEP 2.1.5. li21 : 44.316667 N
d$provenance.lat[which(d$datasetID == "li21" & d$provenance.lat == "44.316667 N")] <- "44.316667"
# STEP 2.1.6. li21 : 42.0760 S
d$provenance.lat[which(d$datasetID == "li21" & d$provenance.lat == "42.0760 S")] <- "-42.0760"
# STEP 2.1.7. yang14: 36.11139 N
d$provenance.lat[which(d$datasetID == "yang14" & d$provenance.lat == "36.11139 N")] <- "36.11139"


#=== === === === === === === === === === === === === === === === === === === ===
# STEP 2.2. in long
# STEP 2.2.1. karlsson08 :42.0333/42.0167
d$provenance.lat[which(d$datasetID == "karlsson08" & d$provenance.lat == "42.0333/42.0167")] <- "42.0333"# See step 2.1.1.
# STEP 2.2.2. zulfiqar15: 71.625833 E
d$provenance.long[which(d$datasetID == "zulfiqar15" & d$provenance.long == "71.625833 E")] <- "71.625833"
# STEP 2.2.3. zlesak07: 93.16667 W
d$provenance.long[which(d$datasetID == "zlesak07" & d$provenance.long == "93.16667 W")] <- "-93.16667"
# STEP 2.2.4. li17: 112.26667 E
d$provenance.long[which(d$datasetID == "li17" & d$provenance.long == "112.26667 E")] <- "112.26667"
# STEP 2.2.5. li21: 86.95 E
d$provenance.long[which(d$datasetID == "li21" & d$provenance.long == "86.95 E")] <- "86.95"
# STEP 2.2.6. li21 : 147.0275 E
d$provenance.long[which(d$datasetID == "li21" & d$provenance.long == "147.0275 E")] <- "147.0275"
# STEP 2.2.7. yang14 : 81.8125 W
d$provenance.long[which(d$datasetID == "yang14" & d$provenance.long == " 81.8125 W")] <- "-81.8125"
# STEP 2.2.8. gremer20 :  "–121.551"
d$provenance.long[which(d$datasetID == "gremer20" & d$provenance.long == "–121.551")] <- "–121.551"# just a typo 

#=== === === === === === === === === === === === === === === === === === === ===
##### STEP 3. Check in the articles for exact coordinates #####
#=== === === === === === === === === === === === === === === === === === === ===

# STEP 3.1 LATITUDES
###  zhou08 wasn't flagged until I saw it's longitude. Lat and long were switched.
d$provenance.lat[which(d$datasetID == "zhou08" & d$provenance.lat == "103.42")] <- "32.00"
# STEP 3.1.2. brandel05: 54 
# to DOUBLE CHECK
# STEP 3.1.4. karlsson08: 9.4167/9.3833
# to DOUBLE CHECK
# STEP 3.1.10. nurse08: 42
# different environments were used within in Essex Ontario, not a precise location was given. I don't know if we should use this because essex is closer to 42.1 than 42.0 
# Talked with Deirdre on 16 Sept 2024, decided to keep 42 because not a huge climatic difference
# STEP 3.1.12. teimouri13: 35.47 and 36.3 
d$provenance.lat[which(d$datasetID == "teimouri13" & d$provenance.lat == "35.5 and 36.3")] <- "35.47"
#chose the first location given, they did merged the two locations together
# STEP 3.1.15. yeom21: 39
# they give location that isn't precise and searching the institute does't give anything conclusive. It's in North Korea... It's not the source population also, it's a crop research institute. My opinion: exclude these locations from the coordinates.
# STEP 3.1.16. zardari19: 35
d$provenance.lat[which(d$datasetID == "zardari19" & d$provenance.lat == "35")] <- "35.58" #given location had no decimals. I took a central point in the town indicated in the article. 
# STEP 3.1.17. zare11: 3147
d$provenance.lat[which(d$datasetID == "zare11" & d$provenance.lat == "3147")] <- "31.47" #missing a .
# STEP 3.1.18. downie91: 46.9
d$provenance.lat[which(d$datasetID == "downie91" & d$provenance.lat == "46.9")] <- "46.92"#verified location and added the missing decimal
# STEP 3.1.18.1. downie91: 46.6
d$provenance.lat[which(d$datasetID == "downie91" & d$provenance.lat == "46.6")] <- "46.67"#verified location and added the missing decimal
# STEP 3.1.19. esmaeili09: 48
# Location provided: Marais poitevin which is a 120 000ha wetland. Not sure what to do with this.  
# STEP 3.1.20. seng20: 103.3
d$provenance.lat[which(d$datasetID == "seng20" & d$provenance.lat == "103.3")] <- "13.60" # location scrapped doesn't make sens. I took in a central location of Forest Restoration and Development “Banteay Srei”Cambodia
# STEP 3.1.21. zulfiqar15: 34.218611
# that's ok, but there is a space before the number which I don't know why
# STEP 3.1.22. zlesak07: 45 N
d$provenance.lat[which(d$datasetID == "zlesak07" & d$provenance.lat == "45 N")] <- "45.00" #double checked location and added the 0
### below I think the mistake comes from dragging in Excel. It goes from 46 N to 62 N
d$provenance.lat[which(d$datasetID == "zlesak07" & d$provenance.lat == "46 N")] <- "45.00" #double checked location and added the 0
d$provenance.lat[which(d$datasetID == "zlesak07" & d$provenance.lat == "47 N")] <- "45.00" #double checked location and added the 0
d$provenance.lat[which(d$datasetID == "zlesak07" & d$provenance.lat == "48 N")] <- "45.00" #double checked location and added the 0
d$provenance.lat[which(d$datasetID == "zlesak07" & d$provenance.lat == "49 N")] <- "45.00" #double checked location and added the 0
d$provenance.lat[which(d$datasetID == "zlesak07" & d$provenance.lat == "50 N")] <- "45.00" #double checked location and added the 0
d$provenance.lat[which(d$datasetID == "zlesak07" & d$provenance.lat == "51 N")] <- "45.00" #double checked location and added the 0
d$provenance.lat[which(d$datasetID == "zlesak07" & d$provenance.lat == "52 N")] <- "45.00" #double checked location and added the 0
d$provenance.lat[which(d$datasetID == "zlesak07" & d$provenance.lat == "53 N")] <- "45.00" #double checked location and added the 0
d$provenance.lat[which(d$datasetID == "zlesak07" & d$provenance.lat == "54 N")] <- "45.00" #double checked location and added the 0
d$provenance.lat[which(d$datasetID == "zlesak07" & d$provenance.lat == "55 N")] <- "45.00" #double checked location and added the 0
d$provenance.lat[which(d$datasetID == "zlesak07" & d$provenance.lat == "56 N")] <- "45.00" #double checked location and added the 0
d$provenance.lat[which(d$datasetID == "zlesak07" & d$provenance.lat == "57 N")] <- "45.00" #double checked location and added the 0
d$provenance.lat[which(d$datasetID == "zlesak07" & d$provenance.lat == "58 N")] <- "45.00" #double checked location and added the 0
d$provenance.lat[which(d$datasetID == "zlesak07" & d$provenance.lat == "59 N")] <- "45.00" #double checked location and added the 0
d$provenance.lat[which(d$datasetID == "zlesak07" & d$provenance.lat == "60 N")] <- "45.00" #double checked location and added the 0
d$provenance.lat[which(d$datasetID == "zlesak07" & d$provenance.lat == "61 N")] <- "45.00" #double checked location and added the 0
d$provenance.lat[which(d$datasetID == "zlesak07" & d$provenance.lat == "62 N")] <- "45.00" #double checked location and added the 0
# STEP 3.1.22. skordilis95 : 38.967 and 37.9
#fixplease
# STEP 3.1.23. bibby53: 40.9
#missing from the google drive
# STEP 3.1.24. guo20: ~34-34.666667
d$provenance.lat[which(d$datasetID == "guo20" & d$provenance.lat == "~34-34.666667")] <- "34.33" # took the central latitude betweeen the two points provided in the article
# STEP 3.1.25. ren15: 33.825278 - 34.136389
d$provenance.lat[which(d$datasetID == "ren15" & d$provenance.lat == "33.825278 - 34.136389")] <- "33.98" # took the central latitude betweeen the two points provided in the article
# STEP 3.1.25. necajeva13 : 57.233333-56.783333
# fixplease
# STEP 3.1.25. olmez09
# fixplease

# STEP 3.2. LONGITUDE
# STEP 3.2.4. karlsson08 : 42.0333/42.0167
d$provenance.long[which(d$datasetID == "karlsson08" & d$provenance.long == "42.0333/42.0167")] <- "42.02" # mean of the 2 longitudes given
# STEP 3.2.7. lee06 : NA
# STEP 3.2.8. rizwan18 : NA
# STEP 3.2.9. grose57 : NA
# STEP 3.2.10. maithani90 : NA
# STEP 3.2.12. nurse08 : -82
# different environments were used within in Essex Ontario, not a precise location was given. I don't know if we should use this because essex is closer to 42.1 than 42.0
# STEP 3.2.13. olmez07 :41.83, 41.87, 41.85
# need to spend time redoing that whole scrapping. Very clearly were the locations given in the article with distinct provenance names and coordinates.

# STEP 3.2.15. teimouri13 : 59.05 and 59.2
d$provenance.long[which(d$datasetID == "teimouri13" & d$provenance.long == "59.05 and 59.2")] <- "59.05" #chose the first location given, they did merged the two locations together
# STEP 3.2.17. yeom21 : 129
# they give location that isn't precise and searching the institute does't give anything conclusive. It's in North Korea... It's not the source population also, it's a crop research institute. My opinion: exclude these locations from the coordinates.
# STEP 3.2.19. zardari19 :53
d$provenance.long[which(d$datasetID == "zardari19" & d$provenance.long == "53")] <- "53.39" #given location had no decimals. I took a central point in the town indicated in the article.
# STEP 3.2.20. zare11 : 5352
d$provenance.long[which(d$datasetID == "zare11" & d$provenance.long == "5352")] <- "53.52"
# STEP 3.2.21. erken21 : 29.3
d$provenance.long[which(d$datasetID == "erken21" & d$provenance.long == "29.3")] <- "29.26"
# STEP 3.2.22. esmaeili09 :-4.5
# Location provided: Marais poitevin which is a 120 000ha wetland. Not sure what to do with this.  
# STEP 3.2.23. zhou08 : 32
d$provenance.long[which(d$datasetID == "zhou08" & d$provenance.long == "32")] <- "103.42"
# STEP 3.2.24. yang08 : 121.3
d$provenance.long[which(d$datasetID == "yang08" & d$provenance.long == "121.3")] <- "121.50" # wrong conversion from hour minute to decimals
# STEP 3.2.25. guo20 : ~105.5-106.5
d$provenance.long[which(d$datasetID == "guo20" & d$provenance.long == "~105.5-106.5", "106.00")] <- "106.00" # took the central latitude betweeen the two points provided in the article
# STEP 3.2.26. ren15 : 107.373333 - 107.861389
d$provenance.long[which(d$datasetID == "ren15" & d$provenance.long == "107.373333 - 107.861389")] <- "107.62" # took the central latitude betweeen the two points provided in the article
# STEP 3.2.27. zulfiqar15: 73.135833 E
d$provenance.long[which(d$datasetID == "zulfiqar15" & d$provenance.long == "73.135833 E")] <- "73.135833" # removing E
# STEP 3.2.28. zulfiqar15: 73.472222 E
d$provenance.long[which(d$datasetID == "zulfiqar15" & d$provenance.long == "73.472222 E")] <- "73.472222" # removing E
# STEP 3.2.29. zlesak07: 93.16667 W
d$provenance.long[which(d$datasetID == "zlesak07" & d$provenance.long == "93.16667 W ")] <- "93.16667" #removing W
# Step 3.2.30 harrison14 # removing all E
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
# STEP 3.2.31. yang14: 81.8125 W
d$provenance.long[which(d$datasetID == "yang14" & d$provenance.long == "81.8125 W")] <- "-81.8125" #removing W
# STEP 3.2.32. gremer20
# checked and locations seems good
#=== === === === === === === === === === === === === === === === === === === ===
##### STEP 4. Round up to 2 decimals #####
# below are the lat long values that have no decimals, they have been checked and it's because they have been rounded up 
#=== === === === === === === === === === === === === === === === === === === ===
# STEP 4.1. LATITUDES
# STEP 4.1.1. winstead71: 40
d$provenance.lat[which(d$datasetID == "winstead71" & d$provenance.lat == "40")] <- "40.00"
# STEP 4.1.3. borghetti86: 39.9
d$provenance.lat[which(d$datasetID == "borghetti86" & d$provenance.lat == "39.9")] <- "39.90"
# STEP 4.1.5. mulaudzi09: -29.3
d$provenance.lat[which(d$datasetID == "mulaudzi09" & d$provenance.lat == "-29.3")] <- "-29.30"
# STEP 4.1.11. tang21: 27.5
d$provenance.lat[which(d$datasetID == "tang21" & d$provenance.lat == "27.5")] <- "27.50"
# STEP 4.1.13. tylkowski10: 54.2
d$provenance.lat[which(d$datasetID == "tylkowski10" & d$provenance.lat == "54.2")] <- "54.20"
# STEP 4.1.14. tylkowski07: 52.3
d$provenance.lat[which(d$datasetID == "tylkowski07" & d$provenance.lat == "52.3")] <- "52.30"
# STEP 4.1.26. ren08: 27.9
d$provenance.lat[which(d$datasetID == "ren08" & d$provenance.lat == "27.9")] <- "27.90"
# STEP 4.1.27. skordilis95: 41.2
d$provenance.lat[which(d$datasetID == "skordilis95" & d$provenance.lat == "41.2")] <- "41.20"
d$provenance.lat[which(d$datasetID == "skordilis95" & d$provenance.lat == "35.2")] <- "35.20"
# STEP 4.1.28. kolodziejek15: 52
d$provenance.lat[which(d$datasetID == "kolodziejek15" & d$provenance.lat == "52")] <- "52.00"
# STEP 4.1.29
d$provenance.lat[which(d$datasetID == "kolodziejek15" & d$provenance.lat == "52")] <- "52.00"
# STEP 4.2.30 edwards96 : 50.3 49.3 50.4 56.3
d$provenance.lat[which(d$datasetID == "edwards96" & d$provenance.lat == "50.3")] <- "50.30"# checked the article, location is good
d$provenance.lat[which(d$datasetID == "edwards96" & d$provenance.lat == "49.3")] <- "49.30"# checked the article, location is good
d$provenance.lat[which(d$datasetID == "edwards96" & d$provenance.lat == "50.4")] <- "50.40"# checked the article, location is good
d$provenance.lat[which(d$datasetID == "edwards96" & d$provenance.lat == "56.3")] <- "56.30"# checked the article, location is good
# STEP 4.2.31 downie98: 58.3
d$provenance.lat[which(d$datasetID == "downie98" & d$provenance.lat == "58.3")] <- "58.30"# checked the article, location is good
# STEP 4.2.32 brandel2005: 54
d$provenance.lat[which(d$datasetID == "brandel2005" & d$provenance.lat == "54")] <- "54.00" # checked the article, location given isn't precise 
# STEP 4.2.32 ma03 : 49 
# couldnt find the pdf
# STEP 4.2.33 picciau19 : 39.4
d$provenance.lat[which(d$datasetID == "picciau19" & d$provenance.lat == "39.4")] <- "39.40" # checked the article,location is good
# STEP 4.2.33 castro95 : -18.4
d$provenance.lat[which(d$datasetID == "castro95" & d$provenance.lat == "-18.4")] <- "-18.40" # checked the article,location is good
# STEP 4.2.34 middleton96 : -37.3
d$provenance.lat[which(d$datasetID == "middleton96" & d$provenance.lat == "-37.3")] <- "-37.30" # checked the article,location is good
# STEP 4.2.36 rostamipoor20 : 35.7
# STEP 4.2.37 ucler18: 40.7
d$provenance.lat[which(d$datasetID == "ucler18" & d$provenance.lat == "40.7")] <- "40.70" # checked the article,location is good
# STEP 4.2.38 farhadi13: 36.7
d$provenance.lat[which(d$datasetID == "farhadi13" & d$provenance.lat == "36.7")] <- "36.70" # checked the article,location is good
# STEP 4.2.39 harrison14 : 42.0760 S
d$provenance.lat[which(d$datasetID == "harrison14" & d$provenance.lat == "42.0760 S")] <- "-42.0760" # removing N and S to the locatiions. I checked in the article and they are given in decimals
d$provenance.lat[which(d$datasetID == "harrison14" & d$provenance.lat == "41.8888 S")] <- "-41.8888" # removing N and S to the locatiions. I checked in the article and they are given in decimals
d$provenance.lat[which(d$datasetID == "harrison14" & d$provenance.lat == "43.2397 S")] <- "-43.2397" # removing N and S to the locatiions. I checked in the article and they are given in decimals
d$provenance.lat[which(d$datasetID == "harrison14" & d$provenance.lat == "41.9405 S")] <- "-41.9405" # removing N and S to the locatiions. I checked in the article and they are given in decimals
d$provenance.lat[which(d$datasetID == "harrison14" & d$provenance.lat == "41.3684 S ")] <- "-41.3684" # removing N and S to the locatiions. I checked in the article and they are given in decimals
d$provenance.lat[which(d$datasetID == "harrison14" & d$provenance.lat == "41.0595 S")] <- "-41.0595" # removing N and S to the locatiions. I checked in the article and they are given in decimals
d$provenance.lat[which(d$datasetID == "harrison14" & d$provenance.lat == "42.9005 S")] <- "-42.9005" # removing N and S to the locatiions. I checked in the article and they are given in decimals
d$provenance.lat[which(d$datasetID == "harrison14" & d$provenance.lat == "42.0889 S")] <- "-42.0889" # removing N and S to the locatiions. I checked in the article and they are given in decimals
d$provenance.lat[which(d$datasetID == "harrison14" & d$provenance.lat == "42.0273 S")] <- "-42.0273" # removing N and S to the locatiions. I checked in the article and they are given in decimals

# STEP 4.2. LONGITUDE
# STEP 4.2.1. zhang21 :124.9
d$provenance.long[which(d$datasetID == "zhang21" & d$provenance.long == "124.9")] <- "124.90" 
# STEP 4.2.2. brandel05 :10
# d$provenance.long[which(d$datasetID == "brandel05" & d$provenance.long == "10")] <- "10.00"
# to DOUBLE CHECK
# STEP 4.2.3. beikmohammadi12 :55.8
d$provenance.long[which(d$datasetID == "beikmohammadi12" & d$provenance.long == "55.8")] <- "55.80"
# STEP 4.2.5. alptekin02 : 39.5
d$provenance.long[which(d$datasetID == "alptekin02" & d$provenance.long == "39.5")] <- "39.50"
# STEP 4.2.6. mulaudzi09 : 27.5
d$provenance.long[which(d$datasetID == "mulaudzi09" & d$provenance.long == "27.5")] <- "27.50"
# STEP 4.2.11. middleton96 : 142.5
d$provenance.long[which(d$datasetID == "middleton96" & d$provenance.long == "142.5")] <- "142.50"
# STEP 4.2.14. pliszko18 : 21
d$provenance.long[which(d$datasetID == "pliszko18" & d$provenance.long == "21")] <- "21.00"
# STEP 4.2.16. yang18 : 121.5
d$provenance.long[which(d$datasetID == "yang18" & d$provenance.long == "121.5")] <- "121.50"
# STEP 4.2.18. yin09 :117.4
d$provenance.long[which(d$datasetID == "yin09" & d$provenance.long == "117.4")] <- "117.40"
# STEP 4.2.18. chien09 121.2
d$provenance.long[which(d$datasetID == "chien09" & d$provenance.long == "121.2")] <- "121.20" # checked the article,location is good
# STEP 4.2.19. liu13
#fixplease
# STEP 4.2.20. edwards96: 124.1
#fixplease
# STEP 4.2.21. brandel2005: 10
d$provenance.long[which(d$datasetID == "brandel2005" & d$provenance.long == "10")] <- "10.00" # checked the article, location given isn't precise 
# STEP 4.2.22. ma03: -128.4, -124.8, -121.5
# pdf unvailable
# STEP 4.2.23. airi2009: 79.5
d$provenance.long[which(d$datasetID == "airi2009" & d$provenance.long == "79.5")] <- "79.50" # checked the article,location is good
# STEP 4.2.24. alptekin2002: 39.5
d$provenance.long[which(d$datasetID == "alptekin2002" & d$provenance.long == "39.5")] <- "39.50" # added decimals 
# STEP 4.2.25. necajeva13 :21.416667-21.05
#fixplease
# STEP 4.2.26. ucler18: 38.9
d$provenance.long[which(d$datasetID == "ucler18" & d$provenance.long == "38.9")] <- "38.90"
# STEP 4.2.27. yang18_1: 121.5
d$provenance.long[which(d$datasetID == "yang18_1" & d$provenance.long == "121.5")] <- "121.50" # checked the article,location is good
# STEP 4.2.28. yang18_2
d$provenance.long[which(d$datasetID == "yang18_2" & d$provenance.long == "121.5")] <- "121.50" # checked the article,location is good
# STEP 4.2.28. yang18_3
d$provenance.long[which(d$datasetID == "yang18_3" & d$provenance.long == "121")] <- "121.00" # checked the article,location is good
# STEP 4.2.29. yang10: 120.8
d$provenance.long[which(d$datasetID == "yang10" & d$provenance.long == "120.8")] <- "120.80" # checked the article,location is good
# STEP 4.2.30. yeom21 
# see above
# STEP 4.2.31. downie91: -67.4, -66.3
d$provenance.long[which(d$datasetID == "downie91" & d$provenance.long == "-67.4")] <- "-67.38" # checked the article,missing decimal when converted
d$provenance.long[which(d$datasetID == "downie91" & d$provenance.long == "-66.3")] <- "-66.30" # checked the article,location is good
# STEP 4.2.32. esmaeili09
# see above
# STEP 4.2.33. wickens01: 28
d$provenance.long[which(d$datasetID == "wickens01" & d$provenance.long == "28")] <- "28.00" # checked the article,location was estimated and seems precise enough
# STEP 4.2.34. joshi03: 77.2
d$provenance.long[which(d$datasetID == "joshi03" & d$provenance.long == "77.2")] <- "77.20" # checked the article,location is good
# STEP 4.2.35. tylkowski10: 17.1
d$provenance.long[which(d$datasetID == "tylkowski10" & d$provenance.long == "17.1")] <- "17.10" # checked the article,missing decimal when converted
# STEP 4.2.36. 
# STEP 4.2.37. 
# STEP 4.2.38. 
# STEP 4.2.39. 





# tmp<-subset(d, datasetID == "wickens01")

##### STEP 5 FIXING +/- #####
# list(tmp$provenance.long)
# d$provenance.long[which(d$datasetID == "edwards96" & d$provenance.long == "124.1")] <- "124.10" # checked the article,location is good
# d$provenance.long[which(d$datasetID == "edwards96" & d$provenance.long == "122.4")] <- "122.40" # checked the article,location is good
# d$provenance.long[which(d$datasetID == "edwards96" & d$provenance.long == "122.4")] <- "122.40" # checked the article,location is good


##### STEP 6. Miscelleneous fixing #####
d$provenance.long[which(d$datasetID == "edwards96" & d$provenance.long == "122.4")] <- "122.40"
#Removing entries in the middle of the ocean
#alptekin02
d$provenance.long[which(d$datasetID == "alptekin02" & d$provenance.long == "-43.74")] <- "43.74" #entry was good, but just a negative was added 
d$provenance.long[which(d$datasetID == "veiga-barbosa16" & d$provenance.long == "3.5667")] <- "-3.5667" #entry was good, but just a negative was added 
d$provenance.lat[which(d$datasetID == "ochuodho08" & d$provenance.lat == "32.29")] <- "-32.29" #entry was good, but just a negative was added 
d$provenance.long[which(d$datasetID == "barros12" & d$provenance.long == "25.497")] <- "-25.497" #entry was good, but just a negative was added 
d$provenance.lat[which(d$datasetID == "herron01" & d$provenance.lat == "40.39")] <- "-40.39" #entry was good, but just a negative was added 
d$provenance.lat[which(d$datasetID == "bibby53" & d$provenance.lat == "40.9")] <- "-40.90" #entry was good, but just a negative was added 



# #=== === === === === === === === === === === === === === === === === === === ===
# #### CHECKING BAD ENTRIES ####
# #=== === === === === === === === === === === === === === === === === === === ===
d3 <- d[!duplicated(d$datasetID), ]
### COLUMN LATITUDE ###
pattern <- "^-?[0-9]{1}\\.\\d{2,16}$|^-?[0-9]{2}\\.\\d{2,16}$|^-?[0-9]{3}\\.\\d{2,16}$"
# # Filter out NAs
d.latitude <- d[!is.na(d$provenance.lat), ]
# # Filter values that have the above pattern
d.latitude2 <- d.latitude[!grepl(pattern, d.latitude$provenance.lat), ]
# # Check how many entries are still problematic
provenance.lat.chg <- d.latitude2[, c("datasetID", "provenance.lat")]
dlat<-unique(provenance.lat.chg)

# ### COLUMN LONGITUDE ###
d.longitude <- d[!is.na(d$provenance.long), ]
d.longitude2 <- d.longitude[!grepl(pattern, d.longitude$provenance.long), ]
# Check how many entries are still problematic
d.longitude.chg <- d.longitude2[, c("datasetID", "provenance.long")]
dlong <- unique(d.longitude.chg)


# Manually looking at the ones with issues (I could also have done this in a nice loop) ...
# these no longer exists, but did yesterday
# check fixplease once they reappear ... 
# subset(checkprovstuff, provLatLon=="37.11421066 101.5144129")
# subset(checkprovstuff, provLatLon=="36.7 54.35")
# subset(checkprovstuff, provLatLon=="36.51666667 138.35")
# subset(checkprovstuff, provLatLon=="32.3167 77.2") 
# 
# subset(checkprovstuff, provLatLon=="25.17 121.55") # okay, real differences
# subset(checkprovstuff, provLatLon=="25.116667 102.733333") # is the NA correct?

# ## Deleting these out for now, we build them later
# d$provLatLon <- NULL
# d$provLatLonAlt <- NULL

# Create two new columns
d$provenance.Lat <- as.numeric(d$provenance.lat)
d$provenance.Long <- as.numeric(d$provenance.long)
d$provLatLon <- paste(d$provenance.lat, d$provenance.long, sep=" ")
d$provLatLonAlt <- paste(d$provenance.lat, d$provenance.long, d$provenance.altitude, sep=" ")

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



