## Started 12 August 2026 ##
## by Lizzie ##

# How big is the effect size of chemical treatments?
# In a hacky way ... 

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

if(length(grep("lizzie", getwd()) > 0)) {
  setwd("/Users/lizzie/Documents/git/projects/egret/analyses")
} else if(length(grep("Xiaomao", getwd()) > 0)) {
  setwd("C:/PhD/Project/egret/analyses")
}

d <- read.csv("output/egretclean.csv")

sort(table(d$chemicalCor))
sort(table(d$chemicalConcent))

# let's make a list of all the colnames that I need to control for!
# doesn't that sound fun? Yes, yes, it does.... 

colztocontrol <- c("latbi", "provLatLon", "provLatLonAlt",
  "chillTemp", "chillDuration", "chillTempUnc",        
  "chillTempCycle", "chillLightCycle", "germTempGen",        
  "germTemp", "germDuration", "germTempClass",    
  "germTempDay", "germTempNight", "germPhotoperiod",     
  "germPhotoperiodDay", "germPhotoperiodNight", 
  "scarifTypeGen", "scarifTypeSpe",         
  "storageType", "storageDetails", "storageTemp", "storageDuration", 
  "photoperiodCor",
  "chemicalCor", "chemicalConcent", "chemicalConcentUnit")

thestudies <- unique(d$datasetIDstudy)
studydesign <- data.frame(matrix(ncol = length(colztocontrol), nrow=length(thestudies)))
names(studydesign) <- colztocontrol

for(astudy in seq_along(thestudies)){
  subby <- d[which(d$datasetIDstudy==thestudies[astudy]),]
  for(colhere in seq_along(colztocontrol)){
    studydesign[astudy, colhere] <- length(unique(subby[,colztocontrol[colhere]]))
  }
}
studydesign$datasetIDstudy <- thestudies

pdf("figures/effectsize/diffTreatsPerStudy.pdf", width=10, height=12)
par(mfrow=c(4,4))
  for(colhere in seq_along(colztocontrol)){
    hist(studydesign[,colztocontrol[colhere]], main="", xlab=colztocontrol[colhere])
  }
dev.off()

# How many are just one?
test <- data.frame(whatcol=character(), 
  howmany1=numeric(), 
  howmany2=numeric(), 
  howmanymodan2=numeric())
for(colhere in seq_along(colztocontrol)){
  test[colhere,"whatcol"] <- colztocontrol[colhere]
  test[colhere,"howmany1"] <- table(studydesign[,colztocontrol[colhere]])[1]
  test[colhere,"howmany2"] <- table(studydesign[,colztocontrol[colhere]])[2]
} 
test$howmanymodan2 <- nrow(studydesign)-test$howmany1-test$howmany2
test[with(test, order(-howmany1)), ]

# START HERE ... 
# Okay, that was a fun and not super important detour ...
# Next, I will subset to the studies that vary 
#   "chemicalCor"         
#   "chemicalConcent"    
# and then get the min and max response for each unique set of ALL possible columns (from above)
which(studydesign$chemicalCor>1)
which(studydesign$chemicalConcent>1)

# things I still care about and have not dealt with ...
if(FALSE){
  "responseVar"         
  "chemicalCor"         
  "chemicalConcent"     
  "chemicalConcentUnit"
}