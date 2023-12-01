## Started 1 Dec 2023 ##
## taken from cleaningDL.R ##

# Needs to be sourced in cleanall.R

# TO DO! We need to fix some of the multiple nameyr IDs to nameyra and nameyrb
egret$datasetID <- tolower(egret$datasetID)
egret$datasetID[which(egret$datasetID == "acosta12")] <- "acosta13"
egret$datasetID[which(egret$datasetID == "brandel2005")] <- "brandel05"
egret$datasetID[which(egret$datasetID == "airi2009")] <- "airi09"
egret$datasetID[which(egret$datasetID == "alptekin2002")] <- "alptekin02"
egret$datasetID[which(egret$datasetID == "amini2018")] <- "amini18"
egret$datasetID[which(egret$datasetID == "pipinus12")] <- "pipinis12"
egret$datasetID[which(egret$datasetID == "picciau18")] <- "picciau19"