## Started 1 Dec 2023 ##
## taken from cleaningDL.R ##

# Needs to be sourced in cleanall.R

# Fix ones with incorrect years
d$datasetID[which(d$entered.by == "DMB")] <- "veiga-barbosa14"
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

d$datasetID[which(d$datasetID == "yang16" & d$species == "taccada")] <- "yang18_3"

# Checked, these are correct: Kolodziejek18,19, Meyer94,95; Olmez07-09; Rouhi12,13; Tilki06,07; tylkowski09-10
d$datasetID[which(d$datasetID == "Aldridge1993")] <- "aldridge1992"
d$datasetID[which(d$datasetID == "Aldridge1994")] <- "aldridge1992"
d$datasetID[which(d$datasetID == "Aldridge1995")] <- "aldridge1992"
d$datasetID[which(d$datasetID == "Aldridge1996")] <- "aldridge1992"
d$datasetID[which(d$datasetID == "Aldridge1997")] <- "aldridge1992"

d$datasetID[which(d$datasetID == "irvani13")] <- "irvani12"
d$datasetID[which(d$datasetID == "irvani14")] <- "irvani12"

d$datasetID[which(d$datasetID == "Sacande05")] <- "sacande04"

# three yang18
d$datasetID[which(d$datasetID == "yang18" & d$genus == "Maackia")] <- "yang18_1"
d$datasetID[which(d$datasetID == "yang18" & d$genus == "Pasania")] <- "yang18_2"
d$datasetID[which(d$datasetID == "yang18" & d$genus == "Scaevola")] <- "yang18_3"

d$datasetID[which(d$datasetID == "tang10a")] <- "tang10_1"
d$datasetID[which(d$datasetID == "tang10b")] <- "tang10_2"
d$datasetID <- tolower(d$datasetID)

temp <- unique(d[,c("datasetID", "entered.by")])
temp$count <-1
temp$datasetID <- tolower(temp$datasetID)
tempy <- aggregate(temp["count"], temp[c("datasetID")], FUN = sum)

d <- d[-which(d$datasetID == "al-absi10" & d$entered.by == "TA"),]  #96 rows to remove
d <- d[-which(d$datasetID == "chen06" & d$entered.by == "AZ"),] #96
d <- d[-which(d$datasetID == "chen15" & d$entered.by == "TA"),] #11
d <- d[-which(d$datasetID == "chichizola18" & d$entered.by == "AZ"),] #45

d <- d[-which(d$datasetID == "han10" & d$entered.by == "CRD"),] #40
d <- d[-which(d$datasetID == "lee21" & d$entered.by == "AZ"),] #30
d <- d[-which(d$datasetID == "moeini21" & d$entered.by == "MN"),] #45
d <- d[-which(d$datasetID == "tilki07" & d$entered.by == "MN"),] #104
d <- d[-which(d$datasetID == "wytsalucy21" & d$entered.by == "DK"),]#92
d <- d[-which(d$datasetID == "yusefi-tanha19" & d$entered.by == "JS"),]#40

d$entered.by[which(d$entered.by == "DL ")] <- "DL"

# ident <- read.csv("..//data/datasetID.csv")
# ident$dup <- duplicated(ident$studyID)

#I have double checked all the papers that have duplicates
# many have not been entered for various reasons, others the duplicated is in another language and therefore have not be entered

