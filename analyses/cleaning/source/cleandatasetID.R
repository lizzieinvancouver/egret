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
# Most of these were not actually scraped!

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

d$datasetID[which(d$datasetID == "li11 ")] <- "li11"

d$datasetID[which(d$datasetID == "Aldridge1993")] <- "Aldridge1992"
d$datasetID[which(d$datasetID == "Aldridge1994")] <- "Aldridge1992"
d$datasetID[which(d$datasetID == "Aldridge1995")] <- "Aldridge1992"
d$datasetID[which(d$datasetID == "Aldridge1996")] <- "Aldridge1992"
d$datasetID[which(d$datasetID == "Aldridge1997")] <- "Aldridge1992"

d$datasetID[which(d$datasetID == "irvani13")] <- "irvani12"
d$datasetID[which(d$datasetID == "irvani14")] <- "irvani12"

d$datasetID[which(d$datasetID == "Sacande05")] <- "Sacande04"

View(unique(d$datasetID))

Chen06
Chen15
chichizola18


# Checked, these are correct: Kolodziejek18,19, Meyer94,95; Olmez07-09; Rouhi12,13; Tilki06,07; tylkowski09-10
