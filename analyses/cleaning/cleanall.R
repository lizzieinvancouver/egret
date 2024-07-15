# started Nov 30, 2023 by Deirdre and Lizzie

# aim is read in the csv files from everyone's data that will include both sourced cleaning code and write out a raw file

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

if(length(grep("deirdreloughnan", getwd()) > 0)) {
  setwd("~/Documents/github/egret/analyses")
} else if(length(grep("lizzie", getwd()) > 0)) {
  setwd("/Users/lizzie/Documents/git/projects/egret/analyses")
} else if(length(grep("sapph", getwd()) > 0)) {
  setwd("/Users/sapph/Documents/ubc things/work/egret/analyses")
} else if(length(grep("Xiaomao", getwd()) > 0)) {
  setwd("C:/PhD/Project/egret/analyses")
} else if(length(grep("britanywuuu", getwd()) > 0)) {
  setwd("~/Documents/ubc/year5/TemporalEcologyLab/egret/analyses")
} else if(length(grep("Ken", getwd())) > 0){
  setwd("/Users/Ken Michiko Samson/Documents/Temporal Ecology Lab/egret/analyses")
} else if(length(grep("christophe_rouleau-desrochers", getwd())) > 0){
  setwd("/Users/christophe_rouleau-desrochers/Documents/github/egret/analyses")
} else if(length(grep("frederik", getwd())) > 0){
  setwd("/Users/frederik/github/egret/analyses")
}

# 1. Get the data (reads in a dataframe called `egret')
source("cleaning/source/mergedata.R")

# 2. Clean up datasetID issues
source("cleaning/source/cleandatasetID.R")

# 3. Clean species names
source("cleaning/source/cleanspecies.R")

# 4. Clean chill duration and temperature
source("cleaning/source/cleanChillTempDuration.R")

# 5. Clean germination temperature
source("cleaning/source/cleanGerminationTempDuration.R")

# 6. Clean germination scarification
source("cleaning/source/cleanScarification.R")

# 7. Clean Clean chemical---yes/no column---clean chemcial name col
source("cleaning/source/cleanChemical.R")

# 8. Clean storage type
source("cleaning/source/cleanStorage.R")

# 9. Clean response variables and response
source("cleaning/source/cleanResponseVar.R")

# 10. Clean photoperiod
source("cleaning/source/cleanPhotoperiod.R")

# 11. Clean coordinates seed provinance
# source("cleaning/source/cleanCoordinates.R")

# 12. Clean year of germination (some)
# source("cleaning/source/cleanYearGermination.R")

# 13. Small final cleaning and write out data!

# Cleaning experiment number, if missing a value add "exp1"
d$study[which(is.na(d$study))] <- "exp1"
d$datasetIDstudy <- paste(d$datasetID,d$study, sep = "")

# Get a latin binomial
d$latbi <- paste(d$genus, d$species, sep = "_")

# Get a population or provenance ID ...
# there are a couple studies where altitude is unqiue (review in cleanCoordinates.R) so...
# best to use provLatLonAlt to get unique populations
d$provLatLon <- paste(d$provenance.lat, d$provenance.long, sep=" ")
d$provLatLonAlt <- paste(d$provenance.lat, d$provenance.long, d$provenance.altitude, sep=" ")

# checking sizing ..
dim(d) # dim on 11 July 2024: 70 and 30202 

write.csv(d, "output/egretclean.csv", row.names=FALSE)

