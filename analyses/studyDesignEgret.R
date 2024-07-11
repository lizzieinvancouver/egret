#=== === === === === === === === === === === === === === === === === === === ===
# created by Lizzie and Christophe on July 8, 2024
# cleaning treatment column of egret

# housekeeping
rm(list=ls())  
options(stringsAsFactors=FALSE)
library(dplyr)

if(length(grep("christophe_rouleau-desrochers", getwd()) > 0)) {
  setwd("~/Documents/github/egret/analyses")
} else if(length(grep("danielbuonaiuto", getwd()) > 0)) {
  setwd("/Users/danielbuonaiuto/Documents/git/egret/analyses/")
} else if(length(grep("lizzie", getwd()) > 0)) {
  setwd("/Users/christophe_rouleau-desrochers/Documents/github/egret/analyses")
}

# read file
d <- read.csv2("output/egretclean.csv", sep=",")

#Create warm strat column
d$warmstrat <- NA
# vector for all treatments that have warm stratification
warmstrat.names <- unique(d$treatment[grep("warm", d$treatment)])
# remove entry that shouldn't be there
warmstrat.names[!warmstrat.names %in% c("cold strat + soak in warm water")]
# add a 1 in warmstrat column whenever "warm" appepeared in the treatment column
d$warmstrat[which(d$treatment %in% warmstrat.names)] <- 1


d$provLatLon<-paste(d$provenance.lat,d$provenance.long ,sep=" ")
# create a vector of columns to check
col2check <- c("germTemp", "chillTemp","chillDuration", "warmstrat", "scarifType", "chemicalCor", "storageType","provLatLon")
##add provinence

##############Dan's (unnecessary) dplyr solution##########################################

treters<-dplyr::select(d,datasetID,study,latbi)
treters<-distinct(treters)

for (i in c(1:length(col2check))) {
  goo<-dplyr::select(d,datasetID,study,latbi,col2check[i])
  goo<-dplyr::distinct(goo)
  goo<-dplyr::filter(goo,!is.na(goo[,4]))
  
goo<-goo %>% dplyr::group_by(datasetID,study,latbi) %>%dplyr::count()
colnames(goo)[4]<-col2check[i]
treters<-dplyr::left_join(treters,goo)
}




treat<-tidyr::gather(treters,"treatment","n",4:11)

write.csv(treat,"output/treatments_manipulated.csv")


treatapplied<-dplyr::filter(treat,!is.na(n))
treat.manipulated<-dplyr::filter(treatapplied,n>1)

manis<-treat.manipulated %>% dplyr::group_by(treatment) %>% dplyr::count() ## this tells us the number of species/exp/study that have multiple levels of treatments

###what gets manipualted together


#####################################################################################

library(xtable)
xtable(treters)

    


# keep only one datasetIDstudy
d.unique <- d[!duplicated(d$datasetIDstudy), ]
# create a vector of datasetIDstudy
unique.studies <- d.unique$datasetIDstudy # paste speces


# create an empty data frame

# vector that will be the new df columns 
colsforstudydesign <- c("datasetIDstudy", col2check)
# fill the right ncol and nrow
studydesign <- data.frame(matrix(ncol = length(colsforstudydesign), nrow=length(unique.studies)))
# set colnames
names(studydesign) <- colsforstudydesign
# add datasetIDstudy names in the first columns
studydesign$datasetIDstudy <- unique.studies

# loop to count unique treatments in each column
# currently treats NAs as a treatment (NAs are not deleted out)
for (i in c(1:length(unique.studies))) { # i = 1
  subby <- d[which(d$datasetIDstudy == unique.studies[i]),]
  for(j in c(1:length(col2check))) { # j = 2
    unique.rows <- length(unique(subby[, c(col2check[j])]))
    studydesign[i, j +1 ] <- unique.rows
  }
} 

# double checking the entries 
# check work above for a random selection of studies
studiestocheck <- c("ochuodho08exp3", "liu13exp2", "downie98exp1")
for(studyhere in studiestocheck){
  dfhere <- subset(d, datasetIDstudy==studyhere)
  ### germ.temp
  print(length(unique(dfhere$germTemp)))
  # germ duration
  print(length(unique(dfhere$germDuration)))
  # chemicalCor
  print(length(unique(dfhere$chemicalCor)))
  print(subset(studydesign, datasetIDstudy == studyhere))
}
