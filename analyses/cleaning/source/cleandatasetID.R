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

d$datasetID[which(d$datasetID == "Aldridge1993")] <- "Aldridge1992"
d$datasetID[which(d$datasetID == "Aldridge1994")] <- "Aldridge1992"
d$datasetID[which(d$datasetID == "Aldridge1995")] <- "Aldridge1992"
d$datasetID[which(d$datasetID == "Aldridge1996")] <- "Aldridge1992"
d$datasetID[which(d$datasetID == "Aldridge1997")] <- "Aldridge1992"

View(unique(d$datasetID))

Chen06
Chen15
chichizola18

# irvani12,13,14; Kolodziejek18,19, li11 with space; Meyer94,95; Olmez07-09; Rouhi12,13; Sacande04,05; Tilki06,07; tylkowski09-10; NA?
