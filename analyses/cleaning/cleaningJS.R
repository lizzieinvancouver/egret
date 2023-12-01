## Updated 30 November 2023 ##
## By Lizzie and Deirdre ##

## ADD Here...
## Original file called coordinate_cleaning_TA.R ##

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

# General chekcs:


unique(egret$study)
egret$study <- gsub(" ","", egret$study)

unique(egret$variety) # only 18

unique(egret$woody) # N, Y, NA, "rhizomatus shrub/bamboo", "succulent" 

unique(egret$source.population) #192 

unique(egret$provenance.lat) # character bc includes 3 ranges and some wild decimals
unique(egret$provenance.long)

unique(egret$continent)
egret$continent[which(egret$continent == "USA")] <- "North America" # 8 and some NA

unique(egret$no.indiv.collected) # only 13 reported values

unique(egret$year.collected)
#character bc 4 values entered as ranges, also from 1951-2019

unique(egret$year.germination)
egret$year.germination[which(egret$year.germination == "n/a")] <- NA #only 19 report this
#TO CHECK - TRUE? But what is the value?
germyeartru <- egret[which(egret$year.germination == "TRUE"),] 
#hawkins19 found to have germination year of 2010 
egret$year.germination[which(egret$year.germination == "TRUE")] <- "2010" 

# rechecking all of the NA values
germ_year <- egret[which(is.na(egret$year.germination)),]
unique(germ_year$datasetID) #80 papers where there is no germ year 

#fix NAs to the correct germ years that I found - JS
egret$year.germination[which(egret$datasetID == "acosta13" & is.na(egret$year.germination))] <- "2009"
egret$year.germination[which(egret$datasetID == "bhatt00" & is.na(egret$year.germination))] <- "1997"
egret$year.germination[which(egret$datasetID == "beikmohammadi12" & is.na(egret$year.germination))] <- "2011"
#could not find paper for aldridge1992,93,94,95,etc (not on source list in spreadsheet, don't know paper title) - JS
#amini18 should be on the google drive but I can't currently access it (obtained by ILL I think) - JS
egret$year.germination[which(egret$datasetID == "chien11" & is.na(egret$year.germination))] <- "2011"
#chien10 gives year collected and that the seeds were stored "temporarily" before germination, but I don't know exactly which year that would have been - JS
#chien11 states that experiment was done ~ a week after collection (2 days drying, 7 days storing) so year germination and year collected should be the same - JS
for(i in unique(egret$year.collected)) {
  egret$year.germination[which(egret$year.collected == i & is.na(egret$year.germination) & egret$datasetID == "chien11")] <- i
}
egret$year.germination[which(egret$datasetID == "cho18" & is.na(egret$year.germination))] <- "2012"
#for chuaren04, germination year is sometime between 2002 and 2004, but they ran multiple experiments and they don't specify each's time period - JS
#for cicek08, gem year was not specified - JS
egret$year.germination[which(egret$datasetID == "conversa09" & is.na(egret$year.germination))] <- "2006"
#for crank92, could not find paper on ubc library - JS
egret$year.germination[which(egret$datasetID == "cuena-lombrana18" & is.na(egret$year.germination))] <- "2014"
#deb17 doesn't give year collected or year germination - JS
#couldn't find dehgan84 on ubclibrary - JS
egret$year.germination[which(egret$datasetID == "cousins10" & is.na(egret$year.germination))] <- "2007"
#for dalling99, year germ is not specified - JS
#could not access naseri18 - JS
#naumovski05 does not specify year germ - JS
#nawrot-chorabik21 no year given - JS
#necajeva13 did not specify, but probably 2008 or 09 - JS
#nin17 did not specify - JS
#sacande05 did not specify - JS
egret$year.germination[which(egret$datasetID == "schutz02" & egret$chill.duration == 0)] <- "1998"
egret$year.germination[which(egret$datasetID == "schutz02" & egret$chill.duration != 0)] <- "2000"
# could not find saeed16 paper
#barros12 did not specify - JS
egret$year.germination[which(egret$datasetID == "jabarzare11" & is.na(egret$year.germination))] <- "2007"
#jacquemart21 did not specify - JS
egret$year.germination[which(egret$datasetID == "jensen97" & is.na(egret$year.germination))] <- "1993/94"
#jiro10 did not specify - JS
egret$year.germination[which(egret$datasetID == "jusung16" & is.na(egret$year.germination))] <- "2011"
#kamareh12 did not specify, but probably 2009 given context (no storage time given) - JS
egret$year.germination[which(egret$datasetID == "kato11" & is.na(egret$year.germination))] <- "2008"
egret$year.germination[which(egret$datasetID == "kazaz10" & is.na(egret$year.germination))] <- "2009"
#keshtkar08 did not specify - JS
egret$year.germination[which(egret$datasetID == "kettenring07" & is.na(egret$year.germination))] <- "2004"
egret$year.germination[which(egret$datasetID == "lo19" & egret$year.collected == 2017)] <- "2017"
egret$year.germination[which(egret$datasetID == "lo19" & egret$year.collected == 2018)] <- "2018"
egret$year.germination[which(egret$datasetID == "ma18")] <- "2013"
#ma03 did not specify - JS
#meyer94 and meyer95 did not specify - JS
#could not find martinik14
#na11 did not specify - JS
#onen20 did not specify
#ordonez-salanueva15 did not specify - JS
#could not find panayotova15 - JS
#could not find parvin16 - JS
#could not find phondani10 - JS
egret$year.germination[which(egret$datasetID == "picciau17" & is.na(egret$year.germination))] <- "2012"
#chen15 germ year was unclear (was between 2012 and 2015, but depended on treatment. Which treatment was when is unclear) - JS
#could not find chen06 - JS
egret$year.germination[which(egret$datasetID == "chichizola18" & is.na(egret$year.germination))] < - "2016"
#kim16 germ year was either 2013 or 2014 depending on length of cold strat treatment - JS
egret$year.germination[which(egret$datasetID == "king12" & is.na(egret$year.germination))] < - "2007"
egret$year.germination[which(egret$datasetID == "kolodziejek18" & is.na(egret$year.germination))] < - "2014/15"
#kolodziejek19 germ year was probably 2015 as storage time after collection was at most 16 weeks - JS
egret$year.germination[which(egret$datasetID == "kulkarni06" & is.na(egret$year.germination))] < - "2003" #collected 2003 only storage mentioned was for a week - JS
#lee21 did not specify - JS
#could not access lai03 - JS
#langlois17 had an experiment conducted in 2007 and one in 2014 - JS
#redondo-gomex11 exp 3 was conducted in 2010. Dates for other experiments not given but were presumably conducted earlier  - JS
# amooghaie09 dd not specify - JS
#could not find ahmad07
#could not find ahola99 
#aiello17 if "subsequently" is inferred to mean directly after collection, germ year is 2015
egret$year.germination[which(egret$datasetID == "aiello17" & is.na(egret$year.germination))] < - "2015"
#could not find budisavljevic21 
#could not find chen08
#could not find harrington09
egret$year.germination[which(egret$datasetID == "hatzilazarou21" & is.na(egret$year.germination))] < - "2021"
egret$year.germination[which(egret$datasetID == "herron01" & is.na(egret$year.germination))] < - "1997"
#could not find irvani12

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

#only check on papers with dry storage.type and no specified storage.temp
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

# 0 a true zero? ie 100% dark?
#check all the 0s
photo0 <- egret[which(egret$photoperiod == "0"),]
# there's lots - go through all later

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
germ.rt <- egret[which(egret$respvar == "germ.rt"),]
#yes, I think it was the abbreviation SC was using when scraping data - JS
egret$respvar[which(egret$respvar == "germ.rt")] <- "germ.rate"

# what is germ.prob - couldn't find - JS
# D50 same as T50?
T50 <- egret[which(egret$respvar == "T50"),]
D50 <- egret[which(egret$respvar == "D50"),]
# in Nin17, D50 is defined as "the time in days required for 50% of the viable seeds to germinate" which seemed to be the definition of T50 in the other papers - JS
egret$respvar[which(egret$respvar == "D50")] <- "T50"

egret$response <- as.numeric(egret$response)
range(egret$response, na.rm =T)

# I assume NG should be NA
egret$response[which(egret$response == "NG")] <- "NA"
egret$response[which(egret$response == "NA*")] <- "NA"

#TO CHECK what is "NA*" - couldn't find (was it already fixed/changed?) - JS


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
