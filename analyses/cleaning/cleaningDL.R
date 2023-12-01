# aim of this code is to combine the cleaning work of on Dinara and Hoai Huong 

# Also to determine what datasets are still missing
if(length(grep("deirdreloughnan", getwd()) > 0)) {
  setwd("~/Documents/github/egret")
} else if(length(grep("Lizzie", getwd()) > 0)) {
  setwd("~/Documents/git/projects/others/deirdre/Synchrony")
} else{
  setwd("/home/deirdre/Synchrony") # for midge
}


unique(egret$woody) # N, Y, NA, "rhizomatus shrub/bamboo", "succulent" 
temp <- subset(egret, woody == "O") # Magnolia_ingrata aka Magnolia_fulva, yes woody
egret$woody[which(egret$woody == "O")] <- "Y"

sort(unique(egret$source.population)) #410

unique(egret$provenance.lat) # character bc includes 3 ranges and some wild decimals
unique(egret$provenance.long)

unique(egret$continent)
egret$continent[which(egret$continent == "USA")] <- "North America" 
egret$continent[which(egret$continent == "North Ameria")] <- "North America" 
# to check:
#South Africa -- africa or south america?
# Central america
# America --- North america?
# "NA, possibly Asia"

unique(egret$no.indiv.collected) # only 13 reported values
# some ranges -- some NA possibly...
unique(egret$year.collected)
egret$year.collected[which(egret$year.collected == "N/A")] <- "NA"
#character bc 4 values entered as ranges, also from 1951-2019

unique(egret$year.germination)
egret$year.germination[which(egret$year.germination == "n/a")] <- "NA" #only 19 report this
#TO CHECK - TRUE? But what is the value?

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

unique(egret$chill.temp)
# many ranges or with variance
#TO CHECK: 
# 38, seems high
# what does (25/10)/5/0) mean

unique(egret$chill.duration)
egret$chill.duration[which(egret$chill.duration == "unknown")] <- "NA"
#TO CHECK
# what does "90/30/90" mean
#"Continuous cold stratification" kept in cold whole experiment? How long was experiment?
# is 0 a true zero? 

unique(egret$germ.temp)
egret$germ.temp[which(egret$germ.temp == "unknown")] <- "NA"
egret$germ.temp[which(egret$germ.temp == "didn't mention")] <- "NA"
# TO CHECK
# 44696, 44854, 44727,44859
# open air - is this a field study?

unique(egret$other.treatment)

unique(egret$photoperiod)
# TO CHECK
#0.3444444444444445
# 0 a true zero? ie 100% dark?
# 0.25

unique(egret$chemical)
# egret$chemical[which(egret$chemical == "water")] <- "NA"
# is GA, GA3, GA4 all the same thing?
# 36 diff chemicals

unique(egret$chemcial.concent)
# some issues with units, g included in some, ppm, nmol/L

unique(egret$trt.duration)
unique(egret$scarification)
unique(egret$scarif.type)

egret$scarif.type[which(egret$scarif.type == "sandpaper")] <- "mechanical - sandpaper"
egret$scarif.type[which(egret$scarif.type == "sand paper (Np. 150)")] <- "mechanical - sandpaper"

egret$scarif.type[which(egret$scarif.type == "mechanical with razor")] <- "mechanical - razor"
egret$scarif.type[which(egret$scarif.type == "H2SO4")] <- "chemical - H2SO4"

#TO CHECK
# acid - what kinds?
#scapel - removing what
# soaking in water?
#cold

unique(egret$soaking)
unique(egret$soaked.in)
egret$soaked.in[which(egret$soaked.in == "water")] <- "H20"
egret$soaked.in[which(egret$soaked.in == "Water")] <- "H20"

egret$soaked.in[which(egret$soaked.in == "hot H2O - 100C")] <- "H2O 100C"
egret$soaked.in[which(egret$soaked.in == "100C water")] <- "H2O 100C"

egret$soaked.in[which(egret$soaked.in == "water 35ºC")] <- "H20 35C"
egret$soaked.in[which(egret$soaked.in == "35C water")] <- "H20 35C"

egret$soaked.in[which(egret$soaked.in == "60C water")] <- "H20 60C"
egret$soaked.in[which(egret$soaked.in == "80C water")] <- "H20 80C"
egret$soaked.in[which(egret$soaked.in == "5 C water")] <- "H20 5C"

egret$soaked.in[which(egret$soaked.in == "water 35ºC")] <- "H20 35C"

#TO CHECK
# what is N, 0 KNO3 - sounds like a control,
# battaglia93	and 97, what is MPa water and a water solution?

unique(egret$seed.mass.given)
egret$seed.mass.given[which(egret$seed.mass.given == "yes")] <- "Y"
egret$seed.mass.given[which(egret$seed.mass.given == "no")] <- "N"
egret$seed.mass.given[which(egret$seed.mass.given == "Yes")] <- "Y"
egret$seed.mass.given[which(egret$seed.mass.given == "No")] <- "N"
egret$seed.mass.given[which(egret$seed.mass.given == "1200")] <- "Y"
egret$seed.mass.given[which(egret$seed.mass.given == "FALSE")] <- "N"

unique(egret$respvar)
egret$respvar[which(egret$respvar == "germ.speed")] <- "germ.rate"
egret$respvar[which(egret$respvar == "rates.germ")] <- "germ.rate"
egret$respvar[which(egret$respvar == "germ.rate (days)")] <- "germ.rate"
egret$respvar[which(egret$respvar == "germ.proportion")] <- "prop.germ"
egret$respvar[which(egret$respvar == "mtg")] <- "mgt"
egret$respvar[which(egret$respvar == "mean.germ.time")] <- "mgt"
egret$respvar[which(egret$respvar == "MGT")] <- "mgt"

# TO CHECK
# is germ.rt germ rate?
# what is germ.prob
# D50 same as T50

egret$response <- as.numeric(egret$response)
range(egret$response, na.rm =T)

# I assume NG should be NA
egret$response[which(egret$response == "NG")] <- "NA"
egret$response[which(egret$response == "NA*")] <- "NA"

#TO CHECK what is "NA*"

unique(egret$error.type)
egret$error.type[which(egret$error.type == "mean+/-SE")] <- "SE"
egret$error.type[which(egret$error.type == "mean+/-SD")] <- "SD"
egret$error.type[which(egret$error.type == "not specified")] <- "not.specified"

# TO CHECK what is xz

unique(egret$resp.error)

unique(egret$reps)

unique(egret$n.per.rep)
unique(egret$germ.duration)
# TO CHECK
# why are there so many with decimal places? should it not be in days?

unique(egret$germ.tim.zero)
# TO CHECK - "TRUE"

#write.csv(egret, "analyses/output/egretData.csv", row.names = FALSE)

sort(unique(egret$datasetID))

# how many studies do we have that time curves?

curved <- egret[, c("datasetID", "sp.name", "germ.duration")]
curved <- unique(curved)

none <- c("N/A", NA, "NA (<35)", "NA")
curved <- curved[!curved$germ.duration %in% none, ]

curved$count <- 1
noDurations <- aggregate(curved["count"], curved[c("datasetID", "sp.name")], FUN = sum)

temp <- subset(noDurations, count >5)

curvedStudy <- unique(temp$datasetID) #77 studies

curvy <- egret[egret$datasetID %in% curvedStudy, ]
unique(curvy$respvar)

# 61 unique resp var