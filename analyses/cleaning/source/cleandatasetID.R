## Started 1 Dec 2023 ##
## taken from cleaningDL.R ##

# Needs to be sourced in cleanall.R

# Fix ones with incorrect years
d$datasetID[which(d$entered.by == "DMB")] <- "veiga-barbosa14"
d$datasetID[which(d$datasetID == "Acosta12")] <- "acosta13"
d$datasetID[which(d$datasetID == "Brandel2005")] <- "brandel05"
d$datasetID[which(d$datasetID == "airi2009")] <- "airi09"
d$datasetID[which(d$datasetID == "Airi2009")] <- "airi09"
d$datasetID[which(d$datasetID == "Alptekin2002")] <- "alptekin02"
d$datasetID[which(d$datasetID == "Amini2018")] <- "amini18" 
d$datasetID[which(d$datasetID == "pipinus12")] <- "pipinis12"
d$datasetID[which(d$datasetID == "picciau18")] <- "picciau19"
d$datasetID[which(d$datasetID == "li11 ")] <- "li11"
d$datasetID[which(d$datasetID == "Sacande05")] <- "sacande04"

d$datasetID[which(d$datasetID == "tang10a")] <- "tang10_1"
d$datasetID[which(d$datasetID == "tang10b")] <- "tang10_2"

# three yang18
d$datasetID[which(d$datasetID == "yang18" & d$genus == "Maackia")] <- "yang18_1"
d$datasetID[which(d$datasetID == "yang18" & d$genus == "Pasania")] <- "yang18_2"
d$datasetID[which(d$datasetID == "yang18" & d$genus == "Scaevola")] <- "yang18_3"

# two yang16, one already yang16_1
d$datasetID[which(d$datasetID == "yang16")] <- "yang16_2"

# Checked, these are correct: Kolodziejek18,19, Meyer94,95; Olmez07-09; Rouhi12,13; Tilki06,07; tylkowski09-10
d$datasetID[which(d$datasetID == "Aldridge1992")] <- "aldridge92"
d$datasetID[which(d$datasetID == "Aldridge1993")] <- "aldridge92"
d$datasetID[which(d$datasetID == "Aldridge1994")] <- "aldridge92"
d$datasetID[which(d$datasetID == "Aldridge1995")] <- "aldridge92"
d$datasetID[which(d$datasetID == "Aldridge1996")] <- "aldridge92"
d$datasetID[which(d$datasetID == "Aldridge1997")] <- "aldridge92"
d$datasetID[which(d$datasetID == "Aldridge1998")] <- "aldridge92"

d$datasetID[which(d$datasetID == "irvani13")] <- "irvani12"
d$datasetID[which(d$datasetID == "irvani14")] <- "irvani12"

d$datasetID[which(d$datasetID == "Redondo-gomez12")] <- "redondo-gomez11"
d$datasetID[which(d$datasetID == "Redondo-gomez13")] <- "redondo-gomez11"

d$datasetID <- tolower(d$datasetID)

# cleaned entered.by 
d$entered.by[which(d$entered.by == "DL ")] <- "DL"

# temp <- unique(d[,c("datasetID", "entered.by")])
# temp$count <-1
# temp$datasetID <- tolower(temp$datasetID)
# tempy <- aggregate(temp["count"], temp[c("datasetID")], FUN = sum)

# ident <- read.csv("..//data/datasetID.csv")
# ident$dup <- duplicated(ident$studyID)

#I have double checked all the papers that have duplicates
# many have not been entered for various legitamite reasons, others the duplicated is in another language and therefore have not be entered

