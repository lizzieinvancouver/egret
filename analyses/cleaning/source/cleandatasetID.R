## Started 1 Dec 2023 ##
## taken from cleaningDL.R ##

# Needs to be sourced in cleanall.R

# TO DO! We need to fix some of the multiple name-year IDs to nameyra and nameyrb
info <- read.csv("..//data/sourceCSV/egret_source.csv")
subby <- info[,c("Article.Title", "Publication.Year","studyID")]
subby2 <- unique(subby)

subby$dup <- duplicated(subby[,c("Article.Title", "Publication.Year")])
temp <- subset(subby, dup != "FALSE") 
# three detected this way: brandel04, hamala17,	voyiatzis95

subby$dup <- duplicated(subby[,c("studyID")])
temp <- subset(subby, dup != "FALSE") 

#But how many of these are actually scraped?
dupID <- unique(temp$studyID)

# Fix ones with incorrect years
d$datasetID[which(d$datasetID == "acosta12")] <- "acosta13"
d$datasetID[which(d$datasetID == "brandel2005")] <- "brandel05"
d$datasetID[which(d$datasetID == "airi2009")] <- "airi09"
d$datasetID[which(d$datasetID == "alptekin2002")] <- "alptekin02"
d$datasetID[which(d$datasetID == "amini2018")] <- "amini18" 
d$datasetID[which(d$datasetID == "pipinus12")] <- "pipinis12"
d$datasetID[which(d$datasetID == "picciau18")] <- "picciau19"

d$datasetID[which(d$datasetID == "tang10a")] <- "tang10_1"
d$datasetID[which(d$datasetID == "tang10b")] <- "tang10_2"

# Find datasetID's that are duplicated---ie a author published 2 papers in the same year: change to authorYeara and authorYearb
#cho18: gg and HHN

#gremer20 DM scraped one only---why?
#li11 - HHN, julie
#Ren08 --- same DM
#tang10 MN
#Yan16 MN
#Yang20 ---MN
#yang16-- me and mn
#zhang21-- JN

