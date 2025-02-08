## Updated 30 November 2023 ##
## By Lizzie and Deirdre ##
## Then mostly by CRD #

# Read in the data by running cleanall.R
# Don't run the map code for now
mapCode <- FALSE
options(digits=6)
length(unique(d$provenance.lat))
length(unique(as.numeric(d$provenance.lat)))
length(unique(d$provenance.long))
length(unique(as.numeric(d$provenance.long)))
### ### ### ### ### ### ### ### ###
# Standardize NA format in lat/long
### ### ### ### ### ### ### ### ###
d$provenance.lat <- gsub("N/A", NA, d$provenance.lat)
d$provenance.long <- gsub("N/A", NA, d$provenance.long)

### ### ### ### ### ### ### ### ###
# Fix or add source.population 
### ### ### ### ### ### ### ### ###
# Lizzie notes: These first couple of lines don't do anything ...
# but it looks like Deirdre may have updated these data for other reasons, so assuming okay and ...
# Leaving this code just to be safe for now (found a number of instances of code below that does nothing)
# bhatt00: source.population in wrong column
d$source.population[which(d$datasetID == "bhatt00" & d$other.treatment == "Kalika -population")] <- "Kalika, Kumaun, Himalaya"
d$source.population[which(d$datasetID == "bhatt00" & d$other.treatment == "Jalna -population")] <- "Jalna, Kumaun, Himalaya"
d$source.population[which(d$datasetID == "bhatt00" & d$other.treatment == "Binsar - population")] <- "Binsar, Kumaun, Himalaya"
# boscagli01: adding missing source.population
d$source.population[which(d$datasetID == "boscagli01")] <- "Rosellae, Grosseto, Italy"
# parvin15: adding missing source.population
d$source.population[which(d$datasetID == "parvin15")] <- "Kostelec nad Černými lesy"
# naseri18: adding missing source.population
d$source.population[which(d$datasetID == "naseri18")] <- "Kord-koy, Golestan, Iran"
# tabatabaeian18: adding missing source.population
d$source.population[which(d$datasetID == "tabatabaeian18")] <- NA
#tylkowski91: typo
d$source.population[which(d$datasetID == "tylkowski91" & d$source.population == "Pozman")] <- "Poznan"
#tylkowski91: removing Pozman as stated in table 1
d$source.population[which(d$datasetID == "tylkowski91" & d$source.population == "Arbor Kornicke, Pozman")] <- "Arbor, Kornicke"
#tylkowski91: adding "," to one of the entry that should have one
d$source.population[which(d$datasetID == "tylkowski91" & d$source.population == "Arbor Kornicke")] <- "Arbor, Kornicke"
# barnhill82: adding missing source.population
d$source.population[which(d$datasetID == "barnhill82")] <- "Morgan County, Tennessee, USA"
# lai03: adding missing source.population
d$source.population[which(d$datasetID == "lai03")] <- "Ninshan County, China"
# werner13: Changing source.population name that's wrong. Git issue #21
d$source.population[which(d$datasetID == "werner13")] <- "MS Foundation,Maracujá, Serrolandia, Bahia, Brazil"
# ahola99: Adding missing source pop 
d$source.population[which(d$datasetID == "ahola99" & d$species == "pendula")] # missing one species and lat long are probably wrong.TO CHECK
# shahi-gharahlar12: missing source pop
d$source.population[which(d$datasetID == "shahi-gharahlar12")] <- "Department of Horticultural Science, University of Tehran, Iran"
# pritchard93: adding source pop precision
d$source.population[which(d$datasetID == "pritchard93")] <- "Wakehurst Place, Ardingly, West Sussex"
# romero05: source pop provided by species in the paper #TO CHECK
# kolodziejek18: location of where the seeds were grown. Assuming this as provenance
d$source.population[which(d$datasetID == "kolodziejek18")] <- "University of Lodz, Faculty of Biology and Environmental Protection"
# veiga-barbosa14 : source pop taken from general location of Navarra region, north-eastern Spain
d$source.population[which(d$datasetID == "veiga-barbosa14")] <- "Navarra region, north-eastern Spain"

### ### ### ### ### ### ### ### ### ###
# Adding missing lat/long as stated 
# Looked back into the papers       
### ### ### ### ### ### ### ### ### ###
# chuanren04
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "chuanren04")] <- "56.13"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "chuanren04")] <- "-106.347"
# albrecht20
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
# aldridge1992: location taken from Sussex (England) and University of Minesota USA
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "aldridge92" & d$source.population == "Sussex, England" )] <- "50.8679" 
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "aldridge92"& d$source.population == "Sussex, England")] <- "-0.0875"
d$source.population[which(is.na(d$provenance.lat) & d$datasetID == "aldridge92" & d$source.population == "Minnesota, USA" )] <- "University of Minnesota, USA" 
d$provenance.lat[which(is.na(d$provenance.long) & d$datasetID == "aldridge92"& d$source.population == "University of Minnesota, USA")] <- "44.9742"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "aldridge92"& d$source.population == "University of Minnesota, USA")] <- "-93.2268"
# amini18
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "amini18")] <- "36.812"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "amini18")] <- "54.945"
# cho18
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "cho18" & d$source.population == "Cheogju-si, Korea")] <- "36.63"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "cho18" & d$source.population == "Cheogju-si, Korea")] <- "127.50"
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "cho18" & d$source.population == "Hoengseong-gun, Gangwon-do, Korea")] <- "37.51"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "cho18" & d$source.population == "Hoengseong-gun, Gangwon-do, Korea")] <- "128.077"
# crank92
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "crank92")] <- "26.973"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "crank92")] <- "-99.101"
# cuena-Lombrana18: general location of the Gennargentu Massif because the two localities mentioned cannot be found
d$provenance.lat[which(d$datasetID == "cuena-lombrana18")] <-"40.02"
d$provenance.long[which(d$datasetID == "cuena-lombrana18")] <-"9.33"
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
# necajeva13: mixed seeds. Central point of the locations taken and extracted from google earth
d$provenance.lat[which(d$provenance.lat== "57.233333-56.783333" & d$datasetID == "necajeva13")] <- "42.00"
d$provenance.long[which(d$provenance.long == "21.416667-21.05" & d$datasetID == "necajeva13")] <- "21.23"
# nin17
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
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "na11")] <- "45.757" # TO DELETE 
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "na11")] <- "124.642" # TO DELETE
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
#parvin15: lat long extracted from source pop on google earth
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "parvin15")] <- "35.844"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "parvin15")] <- "50.972"
# momonoki79: lat long extracted from source pop on google earth
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "momonoki79" & d$source.population == "National Institute of Hygienic Science, Kasukabe City, Saitama Prefecture, Japan")] <- "35.98"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "momonoki79" & d$source.population == "National Institute of Hygienic Science, Kasukabe City, Saitama Prefecture, Japan")] <- "139.75"
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "momonoki79" & d$source.population == "Gotenba City, Kanagawa Prefecture, Japan")] <- "35.31"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "momonoki79" & d$source.population == "Gotenba City, Kanagawa Prefecture, Japan")] <- "138.89"
# ochuodho08: lat long extracted from source pop on google earth
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "ochuodho08")] <- "-32.29"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "ochuodho08")] <- "26.41"
# winstead71: provided latitude only, the longitude of the location found on google earth is used
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "winstead71" &  d$source.population == "Burlington County, New Jersey, USA")] <- "40" 
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "winstead71" & d$source.population == "Burlington County, New Jersey, USA")] <- "-74.69"
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "winstead71" &  d$source.population == "Union County, Illinois, USA")] <- "37.50" 
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "winstead71" & d$source.population == "Union County, Illinois, USA")] <- "-89.28"
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "winstead71" & d$source.population == "Harris County, Texas, USA")] <- "29.06"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "winstead71" & d$source.population == "Harris County, Texas, USA")] <- "-95.31"
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "winstead71" & d$source.population == "Alachua County, Florida, USA")] <- "29.50" 
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "winstead71" &d$source.population == "Alachua County, Florida, USA")] <- "-82.30"
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "winstead71" & d$source.population == "State of San Luis Potosi, Mexico")] <- "22.50" 
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "winstead71" & d$source.population == "State of San Luis Potosi, Mexico")] <- "-100.99"
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "winstead71" & d$source.population == "State of Hidalgo, Mexico")] <- "21.33"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "winstead71" & d$source.population == "State of Hidalgo, Mexico")] <- "-98.76"
# chakraborty92
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "chakraborty92")] <- "30.56" 
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "chakraborty92")] <- "77.30"
# al-absi10
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "al-absi10")] <- "30.83"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "al-absi10")] <- "35.62"
# kalimuthu95: lat long extracted from source pop on google earth
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "kalimuthu95")] <- "11.02"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "kalimuthu95")] <- "76.96"
# markovic20
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "markovic20")] <- "44.81"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "markovic20")] <- "20.46"
# morozowska02
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "morozowska02" & d$source.population == "Gniezno, Poland")] <- "52.53"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "morozowska02" & d$source.population == "Gniezno, Poland")] <- "17.58"
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "morozowska02" & d$source.population == "Poznań, Poland")] <- "52.31"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "morozowska02" & d$source.population == "Poznań, Poland")] <- "16.92"
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "morozowska02" & d$source.population == "Konin, Poland")] <- "52.22"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "morozowska02" & d$source.population == "Konin, Poland")] <- "18.25"
# aldridge1993
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "aldridge1993")] <- "50.8679"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "aldridge1993")] <- "-0.0875" # TO DELETE
# aldridge1994
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "aldridge1994")] <- "46.73"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "aldridge1994")] <- "-94.69" # TO DELETE
# aldridge1995
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "aldridge1995")] <- "46.73"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "aldridge1995")] <- "-94.69" # TO DELETE
# aldridge1996
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "aldridge1996")] <- "46.73"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "aldridge1996")] <- "-94.69" # TO DELETE
# aldridge1997
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "aldridge1997")] <- "46.73"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "aldridge1997")] <- "-94.69" # TO DELETE
# aldridge1998
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "aldridge1998")] <- "46.73"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "aldridge1998")] <- "-94.69" # TO DELETE
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
#olmez17: lat long extracted from source pop on google earth 
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "olmez17")] <- "42.02"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "olmez17")] <- "34.92"
# pritchard93: lat long extracted from source pop on google earth 
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "pritchard93")] <- "51.07"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "pritchard93")] <- "-0.09"
# prknova1: lat long extracted from source pop on google earth
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "prknova15")] <- "49.99"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "prknova15")] <- "14.86"
# rahnama-ghahfarokhi07
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "rahnama-ghahfarokhi07")] <- "32.33"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "rahnama-ghahfarokhi07")] <- "50.86"
# ranil15
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "ranil15")] <- "39.48"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "ranil15")] <- "-0.34"
# roh08
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "roh08" & d$source.population == "Universita¨t Innsbruck, Austria")] <- "47.26"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "roh08" & d$source.population == "Universita¨t Innsbruck, Austria")] <- "11.38"
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "roh08" & d$source.population == "US National Arboretum, Washington, DC")] <- "38.91"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "roh08" & d$source.population == "US National Arboretum, Washington, DC")] <- "-76.97"
# rostamipoor20
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "rostamipoor20" & d$source.population == "Semirom, Iran")] <- "31.41"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "rostamipoor20" & d$source.population == "Semirom, Iran")] <- "51.57"
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "rostamipoor20" & d$source.population == "Damavand, Iran")] <- "35.7"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "rostamipoor20" & d$source.population == "Damavand, Iran")] <- "52.06"
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "rostamipoor20" & d$source.population == "Zanjan, Iran")] <- "36.68"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "rostamipoor20" & d$source.population == "Zanjan, Iran")] <- "48.51"
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
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "tylkowski91" &  d$source.population == "Arbor, Kornicke")] <- "52.24"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "tylkowski91" & d$source.population == "Arbor, Kornicke")] <- "17.02"
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "tylkowski91" & d$source.population == "Poznan")] <- "52.41"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "tylkowski91" & d$source.population == "Poznan")] <- "16.93" 
# tylkowski09
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "tylkowski09" & d$source.population == "Wieluń Forest District")] <- "51.22"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "tylkowski09" & d$source.population == "Wieluń Forest District")] <- "18.57"
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "tylkowski09" & d$source.population == "Grodziec Forest District")] <- "51.17"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "tylkowski09" & d$source.population == "Grodziec Forest District")] <- "15.79" 
# yuan21: lat long extracted from source pop on google earth
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "yuan21")] <- "34.61" 
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "yuan21")] <- "127.28" 
# yusefi-tanha19
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "yusefi-tanha19")] <- "32.68"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "yusefi-tanha19")] <- "51.65"
# carpenter92
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "carpenter92")] <- "27.66"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "carpenter92")] <- "-81.51"
# carpenter92
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "arslan11")] <- "40.07"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "arslan11")] <- "29.22"
# edwards73_1
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "edwards73_1" & d$source.population == "Ucona River, Vancouver Island, British Columbia, Canada")] <- "49.71"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "edwards73_1" & d$source.population == "Ucona River, Vancouver Island, British Columbia, Canada")] <- "-125.10"
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "edwards73_1" & d$source.population == "Adam River, Vancouver Island, British Columbia, Canada")] <- "50.46"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "edwards73_1" & d$source.population == "Adam River, Vancouver Island, British Columbia, Canada")] <- "-126.28"
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "edwards73_1" & d$source.population == "Sheringham Beach, Shirley, British Columbia, Canada")] <- "48.38"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "edwards73_1" & d$source.population == "Sheringham Beach, Shirley, British Columbia, Canada")] <- "-123.92"
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "edwards73_1" & d$source.population == "Gold River, Vancouver Island, British Columbia, Canada")] <- "49.78"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "edwards73_1" & d$source.population == "Gold River, Vancouver Island, British Columbia, Canada")] <- "-126.05"
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "edwards73_1" & d$source.population == "Misery Creek, British Columbia, Canada")] <- "49.67"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "edwards73_1" & d$source.population == "Misery Creek, British Columbia, Canada")] <- "-123.58"
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "edwards73_1" & d$source.population == "Qualicum Beach, Vancouver Island, British Columbia, Canada")] <- "49.35"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "edwards73_1" & d$source.population == "Qualicum Beach, Vancouver Island, British Columbia, Canada")] <- "-124.44"
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "edwards73_1" & d$source.population == "Mount Todd, British Columbia, Canada")] <- "48.62"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "edwards73_1" & d$source.population == "Mount Todd, British Columbia, Canada")] <- "-123.94"
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "edwards73_1" & d$source.population == "Rupert Inlet, British Columbia, Canada")] <- "50.58" 
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "edwards73_1" & d$source.population == "Rupert Inlet, British Columbia, Canada")] <- "-127.51"
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "edwards73_1" & d$source.population == "East Thurlow Island, British Columbia, Canada")] <- "50.38"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "edwards73_1" & d$source.population == "East Thurlow Island, British Columbia, Canada")] <- "-125.44"
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "edwards73_1" & d$source.population == "Kasiks River - Skeena River Junction, British Columbia, Canada")] <- "54.29"  
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "edwards73_1" & d$source.population == "Kasiks River - Skeena River Junction, British Columbia, Canada")] <- "-129.40"
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "edwards73_1" & d$source.population == "Finlayson Arm, Saanich Inlet, Victoria, British Columbia, Canada")] <- "48.50"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "edwards73_1" & d$source.population == "Finlayson Arm, Saanich Inlet, Victoria, British Columbia, Canada")] <- "-123.55"
# edwards73_2: location taken from Nitnat River because they said "north end"
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "edwards73_2" & d$source.population == "Northern end of Nitinat Lake, British Columbia, Canada")] <- "48.82" 
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "edwards73_2" & d$source.population == "Northern end of Nitinat Lake, British Columbia, Canada")] <- "-124.68"
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "edwards73_2" & d$source.population == "Finlayson Arm, Saanich Inlet, Victoria, British Columbia, Canada")] <- "48.50" 
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "edwards73_2" & d$source.population == "Finlayson Arm, Saanich Inlet, Victoria, British Columbia, Canada")] <- "-123.55"
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "edwards73_2" & d$source.population == "Caycuse, Lake Cowichan, British Columbia, Canada")] <- "48.88"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "edwards73_2" & d$source.population == "Caycuse, Lake Cowichan, British Columbia, Canada")] <- "-124.37"
# madeiras07
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "madeiras07")] <- "44.05"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "madeiras07")] <- "-91.66"
# marcello15: lat long extracted on google earth from town where seeds were collected
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "marcello15")] <- "9.4" 
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "marcello15")] <- "-0.84"
# watanabe22
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "watanabe22")] <- "33.6" 
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "watanabe22")] <- "132.79"
# washitani89: lat long extracted on google earth from Botanical Garden, Tokyo field
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "washitani89")] <- "35.72" 
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "washitani89")] <- "139.74"
# scocco98
d$provenance.lat[which(d$datasetID == "scocco98" & d$source.population == "Vernasca, Piacenza, Emilia-Romagna, Italy")] <- "44.80" 
d$provenance.long[which(d$datasetID == "scocco98" & d$source.population == "Vernasca, Piacenza, Emilia-Romagna, Italy")] <- "9.83"
d$provenance.lat[which(d$datasetID == "scocco98" & d$source.population == " Gossolengo, Piacenza, Emilia-Romagna, Italy")] <- "45" 
d$provenance.long[which(d$datasetID == "scocco98" & d$source.population == " Gossolengo, Piacenza, Emilia-Romagna, Italy")] <- "9.62"
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
# bibby53
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "bibby53")] <- "-40.90" 
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "bibby53")] <- "174.89"
# fulbright86: seeds from 3 locations mixed, a point between these 3 locations was selected on google earth
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "fulbright86")] <- "26.6" 
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "fulbright86")] <- "-97.9"
# geszprych02
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
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "statton17")] <- "-32.03" 
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "statton17")] <- "115.77"
# walck12
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "walck12")] <- "35.77" 
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "walck12")] <- "-86.34"
# kolodziejek19
d$provenance.lat[which(d$datasetID == "kolodziejek19" & d$source.population == "Rze˛dkowice (Cze˛stochowa Upland)")] <- "50.57"
d$provenance.long[which(d$datasetID == "kolodziejek19" & d$source.population == "Rze˛dkowice (Cze˛stochowa Upland)")] <- "19.48"
d$provenance.lat[which(d$datasetID == "kolodziejek19" & d$source.population == "Ornak (Western Tatra Mts)")] <- "49.22" 
d$provenance.long[which(d$datasetID == "kolodziejek19" & d$source.population == "Ornak (Western Tatra Mts)")] <- "19.83"
d$provenance.lat[which(d$datasetID == "kolodziejek19" & d$source.population == "Kłobuck-Smugi (Wielun´ Upland)")] <- "50.91" 
d$provenance.long[which(d$datasetID == "kolodziejek19" & d$source.population == "Kłobuck-Smugi (Wielun´ Upland)")] <- "18.97"
d$provenance.lat[which(d$datasetID == "kolodziejek19" & d$source.population == "Podde˛bice (Łask Elevation)")] <- "51.89" 
# kulkarni06
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "kulkarni06")] <- "-29.61" 
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "kulkarni06")] <- "30.35"
# lai03: location converted to decimals
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "lai03")] <- "33.31" 
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "lai03")] <- "108.31"
# langlois17
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "langlois17")] <- "45.56" 
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "langlois17")] <- "-73.56"
# amooaghaie09
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "amooaghaie09")] <- "32.65" 
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "amooaghaie09")] <- "51.67"
# harrington09: Olympia and Matlock area (seeds were mixed). lat long extracted from middle point between the two areas on google earth
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "harrington09")] <- "47.11"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "harrington09")] <- "-123.13"
# herron01
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "herron01")] <- "-40.39" 
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "herron01")] <- "175.61"
# irvani12 Coordinates were in a weird form in the paper: Lat: 361 270 N; Lon: 591 630 E. Coordinates of the same research station in appropriate format found here: https://www.sciencedirect.com/science/article/pii/S2405844018347832)
d$provenance.lat[which(is.na(d$provenance.lat) & d$datasetID == "irvani12")] <- "32.6"
d$provenance.long[which(is.na(d$provenance.long) & d$datasetID == "irvani12")] <- "51.43"
# dalling99: wrong lat
d$provenance.lat[which(d$datasetID == "dalling99")] <- "-33.98"
# tilki07: missing lat. Central location taken from Artvin on google map
d$provenance.lat[which(d$datasetID == "tilki07")] <- "41.18"
d$provenance.long[which(d$datasetID == "tilki07")] <- "41.82"
# shahi-gharahlar12: Central location taken from university of tehran on google map
d$provenance.lat[which(d$datasetID == "shahi-gharahlar12" & d$source.population == "Department of Horticultural Science, University of Tehran, Iran")] <- "35.76"
d$provenance.long[which(d$datasetID == "shahi-gharahlar12" & d$source.population =="Department of Horticultural Science, University of Tehran, Iran")] <- "51.27"
# kolodziejek18: Central location taken from University of Lodz on google map
d$provenance.lat[which(d$datasetID == "kolodziejek18" & d$source.population == "University of Lodz, Faculty of Biology and Environmental Protection")] <- "51.86"
d$provenance.long[which(d$datasetID == "kolodziejek18" & d$source.population =="University of Lodz, Faculty of Biology and Environmental Protection")] <- "19.47"
# veatch-blohm11: cant find location on googlemap, general coordinates of baltimore were selected
d$provenance.lat[which(d$datasetID == "veatch-blohm11" & d$source.population =="Pilot Preserve, Maryland, USA")] <- "39.26"
d$provenance.long[which(d$datasetID == "veatch-blohm11" & d$source.population =="Pilot Preserve, Maryland, USA")] <- "-76.58"
# veiga-barbosa14
d$provenance.lat[which(d$datasetID == "veiga-barbosa14" & d$source.population =="Navarra region, north-eastern Spain")] <- "42.74"
d$provenance.long[which(d$datasetID == "veiga-barbosa14" & d$source.population =="Navarra region, north-eastern Spain")] <- "-1.73"
# yang16_1: seeds from 5 locations mixed, mean coordinates was calculated
d$provenance.lat[which(d$datasetID == "yang16_1" & d$source.population =="Tienshan Mountains, China")] <- "44.80"
d$provenance.long[which(d$datasetID == "yang16_1" & d$source.population =="Tienshan Mountains, China")] <- "83.18"
# jang22
d$provenance.lat[which(d$datasetID == "jang22" & d$source.population =="Andong National University, South Korea")] <- "36.54"
d$provenance.long[which(d$datasetID == "jang22" & d$source.population =="Andong National University, South Korea")] <- "128.80"

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
##### Fix locations have lat long in same entry AND entries with N or S in the coordinate #####
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
# karlsson08: They merged the two locations together. lat long extracted from middle point between the two areas on google earth
d$provenance.lat[which(d$datasetID == "karlsson08" & d$provenance.lat == "9.4167/9.3833")] <- "9.4"
d$provenance.long[which(d$datasetID == "karlsson08" & d$provenance.long == "42.0333/42.0167")] <- "42.03" 
# teimouri13: They merged the two locations together. lat long extracted from middle point between the two areas on google earth
d$provenance.lat[which(d$datasetID == "teimouri13" & d$provenance.lat == "35.5 and 36.3")] <- "35.9"
d$provenance.long[which(d$datasetID == "teimouri13" & d$provenance.long == "59.05 and 59.2")] <- "59.13"
# zulfiqar15
d$provenance.lat[which(d$datasetID == "zulfiqar15" & d$provenance.lat == "34.464444 N")] <- "34.46"
d$provenance.lat[which(d$datasetID == "zulfiqar15" & d$provenance.lat == "34.358333 N")] <- "34.36"
#" 34.218611 N". R doesn't detect this entry, probably because of the space. Function to force every entry that isn't the problematic one and removing the space
d$provenance.lat <- ifelse(d$datasetID == "zulfiqar15" & !d$provenance.lat %in% c("34.46", "34.36"), "34.22", d$provenance.lat)
d$provenance.long[which(d$datasetID == "zulfiqar15" & d$provenance.long == "71.625833 E")] <- "71.625833"
d$provenance.long[which(d$datasetID == "zulfiqar15" & d$provenance.long == "73.135833 E")] <- "73.135833"
d$provenance.long[which(d$datasetID == "zulfiqar15" & d$provenance.long == "73.472222 E")] <- "73.472222"
# li17 
d$provenance.lat[which(d$datasetID == "li17" & d$provenance.lat == "31.0333 N")] <- "31.0333"
d$provenance.long[which(d$datasetID == "li17" & d$provenance.long == "112.26667 E")] <- "112.26667"
# li21
d$provenance.lat[which(d$datasetID == "li21" & d$provenance.lat == "44.316667 N")] <- "44.316667"
d$provenance.lat[which(d$datasetID == "li21" & d$provenance.lat == "42.0760 S")] <- "-42.0760"
d$provenance.long[which(d$datasetID == "li21" & d$provenance.long == "86.95 E")] <- "86.95"
d$provenance.long[which(d$datasetID == "li21" & d$provenance.long == "147.0275 E")] <- "147.0275"
# yang14
d$provenance.lat[which(d$datasetID == "yang14" & d$provenance.lat == "36.11139 N")] <- "36.11139"
d$provenance.long[which(d$datasetID == "yang14" & d$provenance.long == " 81.8125 W")] <- "-81.8125"
# harrison14
d$provenance.lat[which(d$datasetID == "harrison14" & d$provenance.lat == "42.0760 S")] <- "-42.0760" 
d$provenance.lat[which(d$datasetID == "harrison14" & d$provenance.lat == "41.8888 S")] <- "-41.8888" 
d$provenance.lat[which(d$datasetID == "harrison14" & d$provenance.lat == "43.2397 S")] <- "-43.2397" 
d$provenance.lat[which(d$datasetID == "harrison14" & d$provenance.lat == "41.9405 S")] <- "-41.9405" 
d$provenance.lat[which(d$datasetID == "harrison14" & d$provenance.lat == "41.3684 S ")] <- "-41.3684"
d$provenance.lat[which(d$datasetID == "harrison14" & d$provenance.lat == "41.0595 S")] <- "-41.0595" 
d$provenance.lat[which(d$datasetID == "harrison14" & d$provenance.lat == "42.9005 S")] <- "-42.9005" 
d$provenance.lat[which(d$datasetID == "harrison14" & d$provenance.lat == "42.0889 S")] <- "-42.0889" 
d$provenance.lat[which(d$datasetID == "harrison14" & d$provenance.lat == "42.0273 S")] <- "-42.0273" 
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


### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
##### Fix locations. Changing negative to positive when necessary #####
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
# alptekin02
d$provenance.long[which(d$datasetID == "alptekin02" & d$provenance.long == "-39.5")] <- "39.5" 
d$provenance.long[which(d$datasetID == "alptekin02" & d$provenance.long == "-36.39")] <- "36.39" 
d$provenance.long[which(d$datasetID == "alptekin02" & d$provenance.long == "-43.74")] <- "43.74" 
# veiga-barbosa16: checked in article. Entry was good, but missing a negative
d$provenance.long[which(d$datasetID == "veiga-barbosa16" & d$provenance.long == "3.5667")] <- "-3.5667"
# edwards96: all in BC, location provided in paper. Changed long to negative
d$provenance.long[which(d$datasetID == "edwards96" & d$provenance.long == "124.55")] <- "-124.55"
d$provenance.long[which(d$datasetID == "edwards96" & d$provenance.long == "125.0833333")] <- "-125.0833333"
d$provenance.long[which(d$datasetID == "edwards96" & d$provenance.long == "125.5333333")] <- "-125.5333333"
d$provenance.long[which(d$datasetID == "edwards96" & d$provenance.long == "126.2833333")] <- "-126.2833333"
d$provenance.long[which(d$datasetID == "edwards96" & d$provenance.long == "129.0333333")] <- "-129.0333333"
d$provenance.long[which(d$datasetID == "edwards96" & d$provenance.long == "126.4166667")] <- "-126.4166667"
d$provenance.long[which(d$datasetID == "edwards96" & d$provenance.long == "126.7833333")] <- "-126.7833333"
d$provenance.long[which(d$datasetID == "edwards96" & d$provenance.long == "124.2166667")] <- "-124.2166667"
d$provenance.long[which(d$datasetID == "edwards96" & d$provenance.long == "127.2666667")] <- "-127.2666667"
d$provenance.long[which(d$datasetID == "edwards96" & d$provenance.long == "124.1")] <- "-124.1"
d$provenance.long[which(d$datasetID == "edwards96" & d$provenance.long == "125.8333333")] <- "-125.8333333"
d$provenance.long[which(d$datasetID == "edwards96" & d$provenance.long == "122.4")] <- "-122.4"
d$provenance.long[which(d$datasetID == "edwards96" & d$provenance.long == "122.3666667")] <- "-122.3666667"
d$provenance.long[which(d$datasetID == "edwards96" & d$provenance.long == "123.55")] <- "-123.55"
d$provenance.long[which(d$datasetID == "edwards96" & d$provenance.long == "127.45")] <- "-127.45"
d$provenance.long[which(d$datasetID == "edwards96" & d$provenance.long == "125.5833333")] <- "-125.5833333"
d$provenance.long[which(d$datasetID == "edwards96" & d$provenance.long == "118.1666667")] <- "-118.1666667"
d$provenance.long[which(d$datasetID == "edwards96" & d$provenance.long == "123.9")] <- "-123.9"
d$provenance.long[which(d$datasetID == "edwards96" & d$provenance.long == "118.1666667")] <- "-118.1666667"
d$provenance.long[which(d$datasetID == "edwards96" & d$provenance.long == "129.3333333")] <- "-129.3333333"
# downie98
d$provenance.long[which(d$datasetID == "downie98" & d$provenance.long == "117.61667")] <- "-117.61667"
d$provenance.long[which(d$datasetID == "downie98" & d$provenance.long == "117.26667")] <- "-117.26667"
d$provenance.long[which(d$datasetID == "downie98" & d$provenance.long == "115.78333")] <- "-115.78333"
d$provenance.long[which(d$datasetID == "downie98" & d$provenance.long == "117.88333")] <- "-117.88333"
# chen06: changing lat to negative and long to positive because its in taiwan
d$provenance.lat[which(d$datasetID == "chen06" & d$provenance.lat == "24.083")] <- "-24.083"
d$provenance.long[which(d$datasetID == "chen06" & d$provenance.long == "-121.117")] <- "121.117"
# kamareh12: changing long, lat is ok
d$provenance.long[which(d$datasetID == "kamareh12" & d$provenance.long == "-50.13333333")] <- "50.13333333"
# saeed16
d$provenance.long[which(d$datasetID == "saeed16" & d$source.population == "Gran Canaria, Canary Island")] <- "-15.5474"
d$provenance.long[which(d$datasetID == "saeed16" & d$source.population == "Tenerife, Canary Island")] <- "-16.6291"
# liu13: changing long to negative 
d$provenance.long[which(d$datasetID == "liu13" & d$provenance.long == "118.166667")] <- "-118.166667"
d$provenance.long[which(d$datasetID == "liu13" & d$provenance.long == "118.8")] <- "-128.75"
d$provenance.long[which(d$datasetID == "liu13" & d$provenance.long == "121.7333333")] <- "-128.75"
d$provenance.long[which(d$datasetID == "liu13" & d$provenance.long == "125.0333333")] <- "-128.75"
d$provenance.long[which(d$datasetID == "liu13" & d$provenance.long == "128.75")] <- "-128.75"
# hatzilazarou21
d$provenance.long[which(d$datasetID == "hatzilazarou21" & d$provenance.long == "5.133333")] <- "-5.133333"

### ### ### ### ### ### ### ### ### ### ### ### 
##### Fix locations. Miscellaneous errors #####
### ### ### ### ### ### ### ### ### ### ### ### 
# zardari19: # given location had no decimals. A central point in the town indicated in the article was taken. 
d$provenance.lat[which(d$datasetID == "zardari19" & d$provenance.lat == "35")] <- "35.58" 
d$provenance.long[which(d$datasetID == "zardari19" & d$provenance.long == "53")] <- "53.39" 
# zare11: missing a .
d$provenance.lat[which(d$datasetID == "zare11" & d$provenance.lat == "3147")] <- "31.47" 
d$provenance.long[which(d$datasetID == "zare11" & d$provenance.long == "5352")] <- "53.52"
# seng20: 103.3. # Central location of Forest Restoration and Development “Banteay Srei” Cambodia extracted from google earth
d$provenance.lat[which(d$datasetID == "seng20" & d$provenance.lat == "103.3")] <- "13.60" 
d$provenance.long[which(d$datasetID == "seng20" & d$provenance.long == "13.84")] <- "103.96"
# zlesak07: Dragging error in Excel. It goes from 46 N to 62 N. Location confirmed in article.
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
d$provenance.long[which(d$datasetID == "zlesak07" & d$provenance.long == "93.16667 W ")] <- "-93.16667" 
# skordilis95: long extracted from source pop on google earth. lat middle location taken 38.43
d$provenance.lat[which(d$datasetID == "skordilis95" & d$source.population == "Istiaia, Euboea Island and Attica")] <- "38.43"
d$provenance.long[which(d$datasetID == "skordilis95" & d$source.population == "Soufli, Thrace, Greece")] <- "26.30" 
d$provenance.long[which(d$datasetID == "skordilis95" & d$source.population == "Thasos Island, Greece")] <- "24.65" 
d$provenance.long[which(d$datasetID == "skordilis95" & d$source.population == "Lasithi, Creete, Greece")] <- "25.79"
d$provenance.long[which(d$datasetID == "skordilis95" & d$source.population == "Istiaia, Euboea Island and Attica")] <- "23.15"
# guo20: Took the central latitude between the two points provided in the article
d$provenance.lat[which(d$datasetID == "guo20" & d$provenance.lat == "~34-34.666667")] <- "34.33" 
d$provenance.long[which(d$datasetID == "guo20" & d$provenance.long == "~105.5-106.5", "106.00")] <- "106.00" 
# ren15: Took the central latitude betweeen the two points provided in the article
d$provenance.lat[which(d$datasetID == "ren15" & d$provenance.lat == "33.825278 - 34.136389")] <- "33.98" 
d$provenance.long[which(d$datasetID == "ren15" & d$provenance.long == "107.373333 - 107.861389")] <- "107.62"
# yang08
d$provenance.long[which(d$datasetID == "yang08" & d$provenance.long == "121.3")] <- "121.50" 
# zhang21
d$provenance.long[which(d$datasetID == "zhang21" & d$provenance.long == "124.9")] <- "124.90" 
# mulaudzi09
d$provenance.long[which(d$datasetID == "mulaudzi09" & d$provenance.long == "27.5")] <- "27.50"
# middleton96
d$provenance.long[which(d$datasetID == "middleton96" & d$provenance.long == "142.5")] <- "142.50"
# pliszko18
d$provenance.long[which(d$datasetID == "pliszko18" & d$provenance.long == "21")] <- "21.00"
# yin09:
d$provenance.long[which(d$datasetID == "yin09" & d$provenance.long == "117.4")] <- "117.40"
# olmez09: fixing altitude. 
d$provenance.altitude[which(d$datasetID == "olmez09" & d$source.population == "Derinkoy, Koprubasi, Salkimli")] <- NA
d$provenance.altitude[which(d$datasetID == "olmez09" & d$provenance.altitude == "212, 550, 860" & d$source.population == "Derinkoy")] <- "860"
d$provenance.altitude[which(d$datasetID == "olmez09" & d$provenance.altitude == "212, 550, 860" & d$source.population == "Koprubasi, Artvin, Turkey")] <- "212"
d$provenance.altitude[which(d$datasetID == "olmez09" & d$provenance.altitude == "212, 550, 860" & d$source.population == "Salkimli")] <- "550"
# cuena-Lombrana18: the mean of the altitude range mentionned in the paper was calculated
d$provenance.altitude[which(d$datasetID == "cuena-lombrana18" & d$source.population == "Gennargentu Massif, Sardinia, Trainu Murcunieddu")] <- "1348"
d$provenance.altitude[which(d$datasetID == "cuena-lombrana18" & d$source.population == "Gennargentu Massif, Sardinia, Is Terre Molentes")] <- "1483"
# gremer20: replacing unicode dash to -
d$provenance.long[which(d$datasetID == "gremer20" & d$source.population == "Table Mountain, USA")] <- "-121.551"
d$provenance.long[which(d$datasetID == "gremer20" & d$source.population == "Iowa Hill, USA")] <- "-120.921"
d$provenance.long[which(d$datasetID == "gremer20" & d$source.population == "Drum Powerhouse Road, USA")] <- "-120.815"
d$provenance.long[which(d$datasetID == "gremer20" & d$source.population == "Wrights Lake, USA")] <- "-120.252"
d$provenance.long[which(d$datasetID == "gremer20" & d$source.population == "Yosemite, USA")] <- "-119.566"
d$provenance.long[which(d$datasetID == "gremer20" & d$source.population == "Lassen Volcanic, USA")] <- "-121.505"

############################################################################
##### Fixing Continents Points #####
############################################################################
# dehgan84
d$continent[which(d$continent == "USA" & d$datasetID == "dehgan84")] <- "North America"
# cousins10
d$continent[which(d$continent == "USA" & d$datasetID == "cousins10")] <- "North America"
# roh08
d$continent[which(d$continent == "America" & d$datasetID == "roh08")] <- "North America"
# romero05
d$continent[which(d$continent == "America" & d$datasetID == "romero05")] <- "North America"
# rubin18
d$continent[which(d$continent == "America" & d$datasetID == "rubin18")] <- "North America"
# wytsalucy21
d$continent[which(d$continent == "America" & d$datasetID == "wytsalucy21")] <- "North America"
# yang16_2
d$continent[which(d$continent == "America" & d$datasetID == "yang16_2")] <- "North America"
# barnhill82
d$continent[which(d$continent == "America" & d$datasetID == "barnhill82")] <- "North America"
# shahi-gharahlar12
d$continent[which(d$continent == "NA, possibly Asia" & d$datasetID == "shahi-gharahlar12")] <- "Asia"
# wickens01
d$continent[which(d$continent == "South Africa" & d$datasetID == "wickens01")] <- "Africa"
# ma18
d$continent[which(d$continent == "Australia" & d$datasetID == "ma18")] <- "Oceania"
# middleton96
d$continent[which(d$continent == "Australia" & d$datasetID == "middleton96")] <- "Oceania"
# battaglia97
d$continent[which(d$continent == "Australia" & d$datasetID == "battaglia97")] <- "Oceania"
# statton17
d$continent[which(d$continent == "Australia" & d$datasetID == "statton17")] <- "Oceania"
# bibby53
d$continent[which(d$continent == "Zealandia" & d$datasetID == "bibby53")] <- "Oceania"
# madeiras07
d$continent[which(d$continent == "North Ameria" & d$datasetID == "madeiras07")] <- "North America"
# okay11
d$continent[which(d$continent == "Europe " & d$datasetID == "okay11")] <- "Europe"
# olmez09
d$continent[which(d$continent == "Europe " & d$datasetID == "olmez09")] <- "Europe"
# nkomo09
d$continent[which(d$continent == "Africa " & d$datasetID == "nkomo09")] <- "Africa"

