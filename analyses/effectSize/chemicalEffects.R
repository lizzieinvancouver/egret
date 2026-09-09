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
howmanylevels <- data.frame(whatcol=character(), 
  howmany1=numeric(), 
  howmany2=numeric(), 
  howmanymodan2=numeric())
for(colhere in seq_along(colztocontrol)){
  howmanylevels[colhere,"whatcol"] <- colztocontrol[colhere]
  howmanylevels[colhere,"howmany1"] <- table(studydesign[,colztocontrol[colhere]])[1]
  howmanylevels[colhere,"howmany2"] <- table(studydesign[,colztocontrol[colhere]])[2]
} 
howmanylevels$howmanymodan2 <- nrow(studydesign)-howmanylevels$howmany1-howmanylevels$howmany2
howmanylevels[with(howmanylevels, order(-howmany1)), ]

# START HERE ... 
# Okay, that was a fun and not super important detour ...
# Next, I will subset to the studies that vary 
#   "chemicalCor"         
#   "chemicalConcent" 
# which are ...
studydesign$datasetIDstudy[which(studydesign$chemicalCor>1)]
studydesign$datasetIDstudy[which(studydesign$chemicalConcent>1)]

chemstudiesall <- c(studydesign$datasetIDstudy[which(studydesign$chemicalCor>1)], 
  studydesign$datasetIDstudy[which(studydesign$chemicalConcent>1)])  

chemstudies <- unique(chemstudiesall)
# ... and then get the min and max response for each unique set of ALL possible columns (from above)

# Here I get started on my own...
colztocontrolplusrespvar <- c("responseVar", colztocontrol)
dathere <- d[which(d$datasetIDstudy %in% chemstudies),]

## START HERE! I need to review what the below is doing and make sure that I am happy with it. 

## Below is from chatGPT (9 Sep 2026, Freeversion)
# Find unique combinations of all grouping columns
uniquestuff <- unique(d[colztocontrolplusrespvar])

# For each unique combination, calculate min and max responseValueNum
minmaxlist <- lapply(seq_len(nrow(uniquestuff)), function(i) {
  
  # Identify rows belonging to this combination
  keep <- rep(TRUE, nrow(dathere))
  
  for (j in seq_along(colztocontrolplusrespvar)) {
    x <- dathere[[colztocontrolplusrespvar[j]]]
    val <- uniquestuff[i, colztocontrolplusrespvar[j]]
    
    if (is.na(val)) {
      keep <- keep & is.na(x)
    } else {
      keep <- keep & x == val
    }
  }
  
  y <- dathere$responseValueNum[keep]
  y <- y[!is.na(y)]
  
  if (length(y) == 0) {
    c(min = NA, max = NA)
  } else {
    c(min = min(y), max = max(y))
  }
})

minmax <- do.call(rbind, minmaxlist)

# Create final dataframe
result <- cbind(
  uniquestuff,
  minmax
)

# things I still care about and have not dealt with ...
if(FALSE){
  "responseVar"         
  "chemicalCor"         
  "chemicalConcent"     
  "chemicalConcentUnit"
}