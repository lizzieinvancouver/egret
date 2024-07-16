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


unique(egret$scarification)
unique(egret$scarif.type)

egret$scarif.type[which(egret$scarif.type == "sandpaper")] <- "mechanical - sandpaper"
egret$scarif.type[which(egret$scarif.type == "sand paper (Np. 150)")] <- "mechanical - sandpaper"

egret$scarif.type[which(egret$scarif.type == "mechanical with razor")] <- "mechanical - razor"
egret$scarif.type[which(egret$scarif.type == "H2SO4")] <- "chemical - H2SO4"

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


