#=== === === === === === === === === === === === === === === === === === === ===
# created by Lizzie and Christophe on 8 July 2024
# cleaning treatment column of egret

# housekeeping

# #Create warm strat column (this code now in analyseSeedCues/dropExtraTreats.R and I think could be DELETED?)
d$warmstrat <- NA
# vector for(d) all treatments that have warm stratification
warmstrat.names <- unique(d$treatment[grep("warm", d$treatment)])
# remove entry that shouldn't be there
warmstrat.names[!warmstrat.names %in% c("cold strat + soak in warm water")]
# add a 1 in warmstrat column whenever "warm" appepeared in the treatment column
d$warmstrat[which(d$treatment %in% warmstrat.names)] <- 1
##add provenance
# d$provLatLon <- paste(d$provenance.lat,d$provenance.long ,sep=" ")



##
## Count up unique treatments for a TON of the columns
# START HERE ... add species, count NA and non-NA separately, then do what columns happen TOGETHER

# keep only one datasetIDstudy
d.unique <- d[!duplicated(d$datasetIDstudy), ]
# create a vector of datasetIDstudy
unique.studies <- d.unique$datasetIDstudy
# create a vector of columns to check
col2check <- c("germTemp", "chillTemp","chillDuration", "warmstrat", "scarifType", 
  "chemicalCor", "storageType","provLatLon")

# creating an empty data frame in a few steps to match col2check
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



if(FALSE){
##
## Dan's dplyr solution
library(dplyr)

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

###How many species are replicated across studies
sps_rep<-d %>% dplyr::group_by(datasetID,latbi) %>% dplyr::count()
sps_rep<-sps_rep %>% dplyr::group_by(latbi) %>% dplyr::count() #24 our of 333 species in more than 1 dataset

length(unique(d$latbi))
length(unique(d$datasetID))
sum(sps_rep$n)

###how much chilling information do we have
table(!is.na(d$chillDuration))#17643 rows
table(!is.na(d$chillTemp))#17141
table(!is.na(d$chillDuration)& !is.na(d$chillTemp))#16462

17643-16462 #1181 rows lost potentially assuming chilling is calculated

d.germ<-dplyr::filter(d,responseVar=="percent.germ")
goober<-d.germ %>% group_by(datasetIDstudy,latbi,tempDay) %>% count()
goober<-goober %>% group_by(datasetIDstudy,latbi) %>% count()
goober<-dplyr::filter(goober,n>1)
d.germ<-dplyr::filter(d.germ, datasetIDstudy %in% goober$datasetIDstudy)
library(ggplot2)
ggplot(d.germ,aes(tempDay,response))+geom_point(aes(color=latbi))+facet_wrap(~datasetIDstudy)
library(xtable)
xtable(treters)
}
## End of Dan's code for now ....
##



