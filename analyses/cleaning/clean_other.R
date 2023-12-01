## Updated 30 November 2023 ##
## By Lizzie and Deirdre ##

## This contains miscellaneous cleaning of specific entries ##
## Original file called coordinate_cleaning_JS.R ##

# setwd
if(length(grep("deirdreloughnan", getwd()) > 0)) {
  setwd("~/Documents/github/egret/analyses")
} else if(length(grep("lizzie", getwd()) > 0)) {
  setwd("/Users/lizzie/Documents/git/projects/egret/analyses")
} else{
  setwd("boomdittyboom") # for midge
}

# housekeeping
rm(list = ls()) # Clear whatever is already in R's memory
options(stringsAsFactors=FALSE)# Make sure words are read in as characters rather than factors

## load packages ##
library(leaflet)
library(sp)
library(sf)

# grab the data 
egret <- read.csv("input/egretData.csv")

# Checking some columns ... 
unique(egret$study)
egret$study <- gsub(" ","", egret$study)
unique(egret$variety) # only 18
unique(egret$woody) # N, Y, NA, "rhizomatus shrub/bamboo", "succulent" and more
unique(egret$source.population)  
unique(egret$no.indiv.collected) # only 13 reported values
unique(egret$year.germination)
egret$year.germination[which(egret$year.germination == "n/a")] <- NA #only 19 report this
#TO CHECK - TRUE? But what is the value?
germyeartru <- egret[which(egret$year.germination == "TRUE"),] 
#hawkins19 found to have germination year of 2010 
egret$year.germination[which(egret$year.germination == "TRUE")] <- "2010" 
# rechecking all of the NA values
germ_year <- egret[which(is.na(egret$year.germination)),]
unique(germ_year$datasetID) #80 papers where there is no germ year 

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

unique(egret$chill.temp)
# many ranges or with variance
#TO CHECK: 
# 38, seems high - 38 degrees was the temp of the heat stratification treatment. I don't know where that value should be put though- JS
# what does (25/10)/5/0) mean - 25/10 treatment for 3 months, 5 degrees for 1 month, and 0 degrees for 1 month. I don't know how to input - JS

#chill38 <- egret[which(egret$chill.temp == "38"),] 
#chill25 <- egret[which(egret$chill.temp == "(25/10)/5/0"),] 

unique(egret$chill.duration)
egret$chill.duration[which(egret$chill.duration == "unknown")] <- "NA"
#TO CHECK
# what does "90/30/90" mean - 90 days, then 30 days, then 90 days (3 different temperatures) - JS
#"Continuous cold stratification" kept in cold whole experiment? How long was experiment? - could not naseri18 paper on drive - JS
# is 0 a true zero? - there are 0 where they were controls (no chill)

chill9030 <- egret[which(egret$chill.duration == "90/30/90"),]
contcold <- egret[which(egret$chill.duration == "Continuous cold stratification"),]
chilldur0 <- egret[which(egret$chill.duration == "0"),]

unique(egret$germ.temp)
egret$germ.temp[which(egret$germ.temp == "unknown")] <- "NA"
egret$germ.temp[which(egret$germ.temp == "didn't mention")] <- "NA"
# TO CHECK
# 44696, 44854, 44727,44859
# open air - is this a field study?

germ44696 <- egret[which(egret$germ.temp == "44696"),]
germ44854 <- egret[which(egret$germ.temp == "44854"),]
germ44727 <- egret[which(egret$germ.temp == "44727"),]
germ44859 <- egret[which(egret$germ.temp == "44859"),]
# for kolodoziejek19 (labelled 44696 or 44854) the germ temp was either 15/5, 20/10, or 25/15. I don't know which data points correspond to which germ.temp treatment though - JS
# for chen15 (labelled 44696 or 44854), the germ temp was either 30/20, 30/15, 25/15, 20/10, 15/5. As above, I don't know which data points correspond to which germ.temp - JS

#chichizola18 germ temp was 20/10 - JS
egret$germ.temp[which(egret$germ.temp == "44854" & egret$datasetID == "chichizola18")] <- "20/10" 

#koldoziejek18 had germ temps of 15/5, 20/10, 25/15. as above I don't know which data points had which temps - JS
#lee21 (for 44854 and 44727) had germ temps of 15/6, 20/10, and 30/20 but I don't know for which data points - JS
#langois17 (for 44859) germinated seeds in a greenhouse and reports the minimum temperature per month as "germ.temp". How to input? - JS
openair <- egret[which(egret$germ.temp == "open air"),]
#deb17 seeds were sowed in seed beds/poly bags where they germinated, then transferred to the field as seedlings. From "open air" it seems they weren't in a controlled greenhouse - JS

unique(egret$other.treatment)

unique(egret$photoperiod)
# TO CHECK
# 0.25 - I couldn't find studies with "0.25" - JS

#0.3444444444445
photo0.34 <- egret[which(egret$photoperiod == "0.3444444444444445"),]
#borghetti86 photoperiod was 8/16 (8 hours light, 16 dark) - JS
egret$photoperiod[which(egret$photoperiod == "0.3444444444444445")] <- "8/16"


unique(egret$chemical)
# egret$chemical[which(egret$chemical == "water")] <- "NA"
# is GA, GA3, GA4 all the same thing? - they are different types of gibberellic acid I think - JS
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
#scapel - removing what - couldn't find entries with "scalpel" - JS
# soaking in water? - couldn't find entries mentioning soaking in water - JS
#cold - couldn't find entries mentioning "cold" - JS
## have I been looking through the wrong files? there seems to be quite a few things I don't seem to have? 

chemical <- egret[which(egret$scarif.type == "chemical"),]
#jacquemart21 was in H2so4 but was inputted under the chemical column??  - JS
egret$scarif.type[which(egret$scarif.type == "chemical")] <- "chemical - H2SO4"

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
N <- egret[which(egret$soaked.in == "N"),] #Bhatt00 is not on the google drive. Check later - JS
KNO3 <- egret[which(egret$soaked.in == "0 KNO3"),]
#cho18 (seed germination) 0 KNO3 means there was no KNO3 (control) - JS

# battaglia93	and 97, what is MPa water and a water solution? - could not find battaglia 93 and 97 - JS

unique(egret$seed.mass.given)
egret$seed.mass.given[which(egret$seed.mass.given == "yes")] <- "Y"
egret$seed.mass.given[which(egret$seed.mass.given == "no")] <- "N"
egret$seed.mass.given[which(egret$seed.mass.given == "Yes")] <- "Y"
egret$seed.mass.given[which(egret$seed.mass.given == "No")] <- "N"
egret$seed.mass.given[which(egret$seed.mass.given == "1200")] <- "Y"
egret$seed.mass.given[which(egret$seed.mass.given == "FALSE")] <- "N"

unique(egret$error.type)
egret$error.type[which(egret$error.type == "mean+/-SE")] <- "SE"
egret$error.type[which(egret$error.type == "mean+/-SD")] <- "SD"
egret$error.type[which(egret$error.type == "not specified")] <- "not.specified"

# TO CHECK what is xz
xz <- egret[which(egret$error.type == "xz"),]
#I think it should be NA because the error bars on the figure are too small to use - JS

unique(egret$resp.error)
unique(egret$reps)
unique(egret$n.per.rep)
unique(egret$germ.duration)
# TO CHECK
# why are there so many with decimal places? should it not be in days?
unique(egret$germ.tim.zero)
# TO CHECK - "TRUE" 
germtru <- egret[which(egret$germ.tim.zero == "TRUE"),]
#hatzilazou21 started counting germination "at the beginning of the test" (presumably when the treatments started) - JS
