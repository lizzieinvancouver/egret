# looking at scarification, warm strat and storage


# housekeeping
rm(list=ls())  
options(stringsAsFactors=FALSE)


if(length(grep("christophe_rouleau-desrochers", getwd()) > 0)) {
  setwd("~/Documents/github/egret/analyses")
} else if(length(grep("lizzie", getwd()) > 0)) {
  setwd("/Users/christophe_rouleau-desrochers/Documents/github/egret/analyses")
}

# read csv
d <- read.csv2("output/egretclean.csv", sep=",")


#Create warm strat column
d$warmstrat <- NA
# vector for all treatments that have warm stratification
warmstrat.names <- unique(d$treatment[grep("warm", d$treatment)])
# remove entry that shouldn't be there
warmstrat.names[!warmstrat.names %in% c("cold strat + soak in warm water")]
# add a 1 in warmstrat column whenever "warm" appepeared in the treatment column
d$warmstrat[which(d$treatment %in% warmstrat.names)] <- 1
       

#### Scarification ####
# select only columns needed
scar <- d[, c("datasetIDstudy", "species", "scarifType", "scarifTypeGen", "scarifTypeSpe")]

# check how many different types of scarification by each study
# by the column scarifType
scarifType_agrstudy <- aggregate(scar[c("datasetIDstudy")], scar["scarifType"] , FUN= length )
scarifType_agrstudy <- scarifType_agrstudy[with(scarifType_agrstudy, order(-datasetIDstudy)), ]
# by the column scarifTypeGen
# double check that, deirdre arrives with a different 
scarifTypeGen_agrstudy <- aggregate(scar[c("datasetIDstudy")], scar["scarifTypeGen"] , FUN= length )
scarifTypeGen_agrstudy <- scarifTypeGen_agrstudy[with(scarifTypeGen_agrstudy, order(-datasetIDstudy)), ]
# by the column scarifTypeSpe
scarifTypeSpe_agrstudy <- aggregate(scar[c("datasetIDstudy")], scar["scarifTypeSpe"] , FUN= length )
scarifTypeSpe_agrstudy <- scarifTypeSpe_agrstudy[with(scarifTypeSpe_agrstudy, order(-datasetIDstudy)), ]

# check how many different types of scarification by each species
# get a smaller df to check by species
scar_unique <- unique(scar)

# by the column scarifType
scarifType_agrspp <- aggregate(scar_unique[c("species")], scar_unique["scarifType"] , FUN= length )
scarifType_agrspp <- scarifType_agrspp[with(scarifType_agrspp, order(-species)), ]
# by the column scarifTypeGen
scarifTypeGen_agrspp <- aggregate(scar_unique[c("species")], scar_unique[c("scarifTypeGen")] , FUN = length )
scarifTypeGen_agrspp <- scarifTypeGen_agrspp[with(scarifTypeGen_agrspp, order(-species)), ]
# by the column scarifTypeSpe
scarifTypeSpe_agrspp <- aggregate(scar_unique[c("species")], scar_unique["scarifTypeSpe"] , FUN= length )
scarifTypeSpe_agrspp <- scarifTypeSpe_agrspp[with(scarifTypeSpe_agrspp, order(-species)), ]
