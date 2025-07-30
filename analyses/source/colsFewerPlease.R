## Started 30 July 2025 ##
## By Lizzie for now ##

## This list is a best guess by Lizzie of the columns you could want
# I tried to remove any columns that have a cleaned version you should use instead
fewercols <- c("datasetID", "study", "datasetIDstudy", 
  "genus", "species", "variety", "latbi",
  "woody", "crop", 
  "provenance.lat", "provenance.long", "provenance.altitude",
  "provLatLon",  "provLatLonAlt", "continent",
  "storageType", "storageDetails", "storageTemp", "storageDuration",
  "treatment", "other.treatment", "treatmentOverview",
  "chillTemp", "chillDuration", "chillTempUnc", 
  "chillTempCycle", "chillLightCycle", "germTempGen", 
  "germTemp", "germDuration", "germTempClass",   
  "germTempDay", "germTempNight", "germPhotoperiod",    
  "germPhotoperiodDay", "germPhotoperiodNight", 
  "scarification",  "scarifTypeGen", "scarifTypeSpe",    
  "photoperiod" , "photoperiodCor",    
  "chemicalCor",  "chemicalConcent", "chemicalConcentUnit", "trt.duration", 
  "soaking", "soaked.in", "soaking.duration",       
  "responseVar", "responseValueNum", "responseErrorType", 
  "error.type", "resp.error", 
  "reps", "n.per.rep", "seed.mass.given", 
  "germ.time.zero", "figure", "Notes")    

# Example of how to use this ...
if(FALSE{

  if(length(grep("deirdre", getwd()) > 0)) {
    setwd("~/Documents/github/egret/analyses")
  } else if(length(grep("lizzie", getwd()) > 0)) {
    setwd("/Users/lizzie/Documents/git/projects/egret/analyses")
  } else if(length(grep("Xiaomao", getwd()) > 0)) {
    setwd("C:/PhD/Project/egret/analyses")
  } else if(length(grep("Ken", getwd())) > 0){
    setwd("/Users/Ken Michiko Samson/Documents/Temporal Ecology Lab/egret/analyses")
  } else if(length(grep("christophe_rouleau-desrochers", getwd())) > 0){
    setwd("/Users/christophe_rouleau-desrochers/github/egret/analyses")
  } else if(length(grep("victor", getwd())) > 0){
    setwd('~/projects/egret/analyses')
  } 

  dall <- read.csv("output/egretclean.csv")
  d <- dall[, which(colnames(dall) %in% fewercols)]
  d <- d[,fewercols] # just re-ordering

} 
