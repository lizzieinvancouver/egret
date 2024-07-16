# aim of this code is to combine the cleaning work of on Dinara and Hoai Huong 

# Also to determine what datasets are still missing
if(length(grep("deirdreloughnan", getwd()) > 0)) {
  setwd("~/Documents/github/egret")
} else if(length(grep("Lizzie", getwd()) > 0)) {
  setwd("~/Documents/git/projects/others/deirdre/Synchrony")
} else{
  setwd("/home/deirdre/Synchrony") # for midge
}







unique(egret$no.indiv.collected) # only 13 reported values
# some ranges -- some NA possibly...
unique(egret$year.collected)
egret$year.collected[which(egret$year.collected == "N/A")] <- "NA"
#character bc 4 values entered as ranges, also from 1951-2019

unique(egret$year.germination)
egret$year.germination[which(egret$year.germination == "n/a")] <- "NA" #only 19 report this
#TO CHECK - TRUE? But what is the value?








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
# what is germ.prob
# D50 same as T50

egret$response <- as.numeric(egret$response)
range(egret$response, na.rm =T)

# I assume NG should be NA
egret$response[which(egret$response == "NG")] <- "NA"
egret$response[which(egret$response == "NA*")] <- "NA"

#TO CHECK what is "NA*"



#write.csv(egret, "analyses/output/egretData.csv", row.names = FALSE)

sort(unique(egret$datasetID))

# how many studies do we have that time curves?

curved <- egret[, c("datasetID", "sp.name", "germ.duration")]
curved <- unique(curved)

none <- c("N/A", NA, "NA (<35)", "NA")
curved <- curved[!curved$germ.duration %in% none, ]

curved$count <- 1
noDurations <- aggregate(curved["count"], curved[c("datasetID", "sp.name")], FUN = sum)

temp <- subset(noDurations, count >5)

curvedStudy <- unique(temp$datasetID) #77 studies

curvy <- egret[egret$datasetID %in% curvedStudy, ]
unique(curvy$respvar)

# 61 unique resp var