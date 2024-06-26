## Started 1 Dec 2023 ##
## taken from cleaningDL.R ##

# Needs to be sourced in cleanall.R

# Fix ones with incorrect years
d$datasetID[which(d$datasetID == "acosta12")] <- "acosta13"
d$datasetID[which(d$datasetID == "brandel2005")] <- "brandel05"
d$datasetID[which(d$datasetID == "airi2009")] <- "airi09"
d$datasetID[which(d$datasetID == "alptekin2002")] <- "alptekin02"
d$datasetID[which(d$datasetID == "amini2018")] <- "amini18" 
d$datasetID[which(d$datasetID == "pipinus12")] <- "pipinis12"
d$datasetID[which(d$datasetID == "picciau18")] <- "picciau19"
d$datasetID[which(d$datasetID == "li11 ")] <- "li11"

d$datasetID[which(d$datasetID == "yang18" & d$species == "taiwanensis")] <- "yang18_1"
d$datasetID[which(d$datasetID == "yang18" & d$species == "glabra")] <- "yang18_2"
d$datasetID[which(d$datasetID == "yang18" & d$species == "taccada")] <- "yang18_3"

# Checked, these are correct: Kolodziejek18,19, Meyer94,95; Olmez07-09; Rouhi12,13; Tilki06,07; tylkowski09-10
d$datasetID[which(d$datasetID == "Aldridge1993")] <- "Aldridge1992"
d$datasetID[which(d$datasetID == "Aldridge1994")] <- "Aldridge1992"
d$datasetID[which(d$datasetID == "Aldridge1995")] <- "Aldridge1992"
d$datasetID[which(d$datasetID == "Aldridge1996")] <- "Aldridge1992"
d$datasetID[which(d$datasetID == "Aldridge1997")] <- "Aldridge1992"

d$datasetID[which(d$datasetID == "irvani13")] <- "irvani12"
d$datasetID[which(d$datasetID == "irvani14")] <- "irvani12"

d$datasetID[which(d$datasetID == "Sacande05")] <- "Sacande04"

# three yang18
d$datasetID[which(d$datasetID == "yang18" & d$genus == "Maackia")] <- "yang18_1"
d$datasetID[which(d$datasetID == "yang18" & d$genus == "Pasania")] <- "yang18_2"
d$datasetID[which(d$datasetID == "yang18" & d$genus == "Scaevola")] <- "yang18_3"

d$datasetID[which(d$datasetID == "tang10a")] <- "tang10_1"
d$datasetID[which(d$datasetID == "tang10b")] <- "tang10_2"

temp <- unique(d[,c("datasetID", "entered.by")])
temp$count <-1
temp$datasetID <- tolower(temp$datasetID)
tempy <- aggregate(temp["count"], temp[c("datasetID")], FUN = sum)

d <- subset(d, datasetID !="al-absi10" | entered.by != "TA")
d <- subset(d, datasetID !="chen06" | entered.by != "AZ")
d <- subset(d, datasetID !="chen15" | entered.by != "TA")

d <- subset(d, datasetID !="chichizola18" | entered.by != "TA") #entered by two people---but different figures/tables
d <- subset(d, datasetID !="han10" | entered.by != "CRD")
d <- subset(d, datasetID !="lee21" | entered.by != "AZ") #entered by two people---but different figures/tables
d <- subset(d, datasetID !="moeini21" | entered.by != "MN")
d <- subset(d, datasetID !="tilki07" | entered.by != "MN") #entered by two people---but different figures/tables
d <- subset(d, datasetID !="wytsalucy21" | entered.by != "DK")
d <- subset(d, datasetID !="yusefi-tanha19" | entered.by != "JS")

ident <- read.csv("..//data/datasetID.csv")
ident$dup <- duplicated(ident$studyID)

#I have double checked all the papers that have duplicates
# many have not been entered for various reasons, others the duplicated is in another language and therefore have not be entered

