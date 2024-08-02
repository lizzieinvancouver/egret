#=== === === === === === === === === === === === === === === === === === === ===
# Created by Dan to add chilling and warm strat infomraion

# housekeeping
rm(list=ls())  
options(stringsAsFactors=FALSE)
library(dplyr)
library(chillR)
library(stringr)
library(ggplot2)

if(length(grep("christophe_rouleau-desrochers", getwd()) > 0)) {
  setwd("~/Documents/github/egret/analyses")
} else if(length(grep("danielbuonaiuto", getwd()) > 0)) {
  setwd("/Users/danielbuonaiuto/Documents/git/egret/analyses/")
} else if(length(grep("lizzie", getwd()) > 0)) {
  setwd("/Users/christophe_rouleau-desrochers/Documents/github/egret/analyses")
}


##need to come up with a way ntonc
unique(d$treatment)


###start with the easy one first remove unscarified
scarf<-dplyr::filter(d, grepl('scarif', treatment))
scarf<-filter(scarf,scarification=="N") #39 obs? further subset to rows that weren't scarified
goo<-dplyr::setdiff(d, scarf) ## remove those rows from the main dataset

####storage
store<-dplyr::filter(d, grepl('storage', treatment))
store<-filter(store,scarification!="dry")
goo<-setdiff(goo,store)

####soaking
soak<-dplyr::filter(d, grepl('soak', treatment))
table(soak$soaking)### one non soaked got in here ###need to fix
unique(soak$soaked.in)
goodsoak<-c("water", "water.distilled", "H20","H20 - 20C", "room temp water",
"cold water","tap water")

soak<-filter(soak,!soaked.in %in% goodsoak)
goo<-setdiff(goo,soak)

#warm strat (come back to this when the column is clearer)
table(d$warmstrat)
wstrat<-filter(d,warmstrat==1)
unique(wstrat$chillTemp)


chem<-dplyr::filter(d,!is.na(chemicalCor))

chemy<-chem %>% group_by(datasetIDstudy,chemicalCor,chemical.concent) %>%count()

#which have water or control
unique(chemy$chemicalCor)
watertreats<-c("Water", "control.water", "water","H2O", "control")

waterstuds<-filter(chemy, chemicalCor %in% watertreats) #these are water only

chemy.nocont<-filter(chemy,!datasetIDstudy %in% waterstuds$datasetIDstudy) #keep these in
chemy.zeros<-filter(chemy.nocont,chemical.concent==0)#these are controls 

d %>% group_by(chemicalCor) %>%count() %>% arrange(-n) #Can we just worry about GA3 and compant?
#NaCl and PEG are salinity/water treatments
#GA3 is a promotor ABA is inhibitor
#H2So4?

###next: figure out how to keep only these ones in the dataset
pops<-dplyr::filter(d, grepl('prov', treatment))
popy<-d %>%group_by(datasetIDstudy,species,provLatLonAlt) %>% count()
popy<-filter(popy,provLatLonAlt!="NA NA NA")
popy<-popy %>%group_by(datasetIDstudy,species) %>% count()
