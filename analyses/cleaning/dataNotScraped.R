# trying to figure out what ILL papers to assign:

if(length(grep("deirdreloughnan", getwd()) > 0)) {
  setwd("~/Documents/github/egret")
} else if(length(grep("Lizzie", getwd()) > 0)) {
  setwd("~/Documents/git/projects/others/deirdre/Synchrony")
} else{
  setwd("/Github") # for midge
}

rm(list = ls()) # Clear whatever is already in R's memory
options(stringsAsFactors=FALSE)# Make sure words are read in as characters rather than factors

library("readxl") # To read Excel files
library(taxize) # To clean species names

assigned <-  data.frame(read_xlsx("data/egret.xlsx", sheet = "source"))
head(assigned)
assigned$datasetID <- tolower(assigned$studyID)
assigned$datasetID[which(assigned$datasetID == "acosta12")] <- "acosta13"
assigned$datasetID[which(assigned$datasetID == "brandel2005")] <- "brandel05"
assigned$datasetID[which(assigned$datasetID == "airi2009")] <- "airi09"
assigned$datasetID[which(assigned$datasetID == "alptekin2002")] <- "alptekin02"
assigned$datasetID[which(assigned$datasetID == "amini2018")] <- "amini18"
assigned$datasetID[which(assigned$datasetID == "pipinus12")] <- "pipinis12"
assigned$datasetID[which(assigned$datasetID == "picciau18")] <- "picciau19"

# assigned$dupPap <- duplicated(assigned[,c("datasetID", "Arthurs")])
# assigned$dupID <- duplicated(assigned[,c("datasetID")])
# temp <- subset(assigned, dupPap == "TRUE")

#Deirdre 
egret_DL.xlsx <- read_xlsx("data/egret_DL.xlsx", sheet = "data_detailed")
length(unique(egret_DL.xlsx$datasetID)) #7

# Tolu
egret_TA.xlsx <- read_xlsx(paste("data/", "egret_TA", "/", "egret_TA.xlsx", sep = ""), sheet = "data_detailed")
#colnames(egret_TA.xlsx)[colnames(egret_TA.xlsx) == "germ.tim.zero"] <- "germ.time.zero"
colnames(egret_TA.xlsx)[colnames(egret_TA.xlsx) == "notes"] <- "Notes"
length(unique(egret_TA.xlsx$datasetID)) #38

# Sophia C
egret_SC.xlsx <- read_xlsx(paste("data/", "egret_SC", "/", "egret_SC.xlsx", sep = ""), sheet = "data_detailed")
#colnames(egret_SC.xlsx)[colnames(egret_SC.xlsx) == "germ.tim.zero"] <- "germ.time.zero"
colnames(egret_SC.xlsx)[colnames(egret_SC.xlsx) == "notes"] <- "Notes"
length(unique(egret_SC.xlsx$datasetID)) #35

#Christophe
egret_CRD.xlsx <- read_xlsx("data/egret_CRD.xlsx", sheet = "data_detailed")
#colnames(egret_CRD.xlsx)[colnames(egret_CRD.xlsx) == "germ.tim.zero"] <- "germ.time.zero"
length(unique(egret_CRD.xlsx$datasetID)) #4

# Justin
egret_JN.xlsx <- read_xlsx(paste("data/", "egret_JN", "/", "egret_JN_18.10.2023.xlsx", sep = ""), sheet = "data_detailed")
colnames(egret_JN.xlsx)[colnames(egret_JN.xlsx) == "germ.tim.zero"] <- "germ.time.zero"
length(unique(egret_JN.xlsx$datasetID)) #58

# Britany
egret_BW.xlsx <- read_xlsx("data/egret_BW.xlsx", sheet = "data_detailed")
#colnames(egret_BW.xlsx)[colnames(egret_BW.xlsx) == "germ.tim.zero"] <- "germ.time.zero"
length(unique(egret_BW.xlsx$datasetID)) #7

egret_MN.xlsx <- read_xlsx(paste("data/", "egret_MN", "/", "egret_MN.xlsx", sep = ""), sheet = "data_detailed")
length(unique(egret_MN.xlsx$datasetID)) #50

egret_HHN.xlsx <- read_xlsx(paste("data/", "egret_HHN", "/", "egret_HHN.xlsx", sep = ""), sheet = "data_detailed")
length(unique(egret_HHN.xlsx$datasetID)) #26

egret_DK.xlsx <- read_xlsx(paste("data/", "egret_DK", "/", "egret_DK.xlsx", sep = ""), sheet = "data_detailed")
length(unique(egret_DK.xlsx$datasetID)) #10

egret_DM.xlsx <- read_xlsx(paste("data/", "egret_DM", "/", "egret_DM.xlsx", sep = ""), sheet = "data_detailed")
length(unique(egret_DM.xlsx$datasetID)) #27

egret_GG.xlsx <- read_xlsx(paste("data/", "egret_GG", "/", "egret_GG.xlsx", sep = ""), sheet = "data_detailed")
length(unique(egret_GG.xlsx$datasetID)) #12

egret_AZ.xlsx <- read_xlsx(paste("data/", "egret_AZ", "/", "egret_AZ.xlsx", sep = ""), sheet = "data_detailed")
length(unique(egret_AZ.xlsx$datasetID)) #12

egret_JS.xlsx <- read_xlsx(paste("data/", "egret_JS", "/", "egret_JS.xlsx", sep = ""), sheet = "data_detailed")
length(unique(egret_JS.xlsx$datasetID)) #9

egret1 <- rbind(egret_TA.xlsx, egret_BW.xlsx, egret_CRD.xlsx, egret_MN.xlsx, egret_HHN.xlsx, egret_DK.xlsx,
                egret_JS.xlsx, egret_DM.xlsx, egret_AZ.xlsx, egret_GG.xlsx)
colnames(egret1)[colnames(egret1) == "germ.tim.zero"] <- "germ.time.zero"
egret2 <- rbind(egret_DL.xlsx, egret_SC.xlsx, egret1)

colnames(egret2)[colnames(egret2) == "chemcial.concent"] <- "chemical.concent"
egret <- rbind(egret_JN.xlsx, egret2)

egret$datasetID <- tolower(egret$datasetID)
egret$datasetID[which(egret$datasetID == "acosta12")] <- "acosta13"
egret$datasetID[which(egret$datasetID == "brandel2005")] <- "brandel05"
egret$datasetID[which(egret$datasetID == "airi2009")] <- "airi09"
egret$datasetID[which(egret$datasetID == "alptekin2002")] <- "alptekin02"
egret$datasetID[which(egret$datasetID == "amini2018")] <- "amini18"
egret$datasetID[which(egret$datasetID == "pipinus12")] <- "pipinis12"
egret$datasetID[which(egret$datasetID == "picciau18")] <- "picciau19"

## How many of the initial sourced papers were scraped?
numbAssigned <- nrow(assigned) #487
numbScraped <- length(unique(egret$datasetID)) # 280

numbScraped/numbAssigned # 57%

scrapedPap <- unique(egret$datasetID)
##################################################################
## So why was just under half not scraped?

source_DL.xlsx <- read_xlsx("data/egret_DL.xlsx", sheet = "source")

# Tolu
source_TA.xlsx <- read_xlsx(paste("data/", "egret_TA", "/", "egret_TA.xlsx", sep = ""), sheet = "source")
source_TA.xlsx$language[which(source_TA.xlsx$studyID == "schafer89")] <- "not in english"
source_TA.xlsx$language[which(source_TA.xlsx$studyID == "chung91")] <- "not in english"
source_TA.xlsx$language[which(source_TA.xlsx$studyID == "salehi-eskandari21")] <- "not in english"
source_TA.xlsx$language[which(source_TA.xlsx$studyID == "sato03")] <- "not in english"
source_TA.xlsx$language[which(source_TA.xlsx$studyID == "sayedena18")] <- "not in english"
source_TA.xlsx$language[which(source_TA.xlsx$studyID == "salehi15")] <- "farsi"
source_TA.xlsx$accept_reject[which(source_TA.xlsx$studyID == "cho09")] <- "R"
source_TA.xlsx$reason_reject[which(source_TA.xlsx$studyID == "cho09")] <- "no data"
source_TA.xlsx$accept_reject[which(source_TA.xlsx$studyID == "sajna19")] <- "R"
source_TA.xlsx$reason_reject[which(source_TA.xlsx$studyID == "sajna19")] <- "no data"

# Sophia C
source_SC.xlsx <- read_xlsx(paste("data/", "egret_SC", "/", "egret_SC.xlsx", sep = ""), sheet = "source")
colnames(source_SC.xlsx)[colnames(source_SC.xlsx) == "...20"] <- "scraped.by"

#Christophe
source_CRD.xlsx <- read_xlsx("data/egret_CRD.xlsx", sheet = "source")

# Justin
source_JN.xlsx <- read_xlsx(paste("data/", "egret_JN", "/", "egret_JN_18.10.2023.xlsx", sep = ""), sheet = "source")

# Britany
source_BW.xlsx <- read_xlsx("data/egret_BW.xlsx", sheet = "source_ill")
source_BW2.xlsx <- read_xlsx("data/egret_BW.xlsx", sheet = "source")
colnames(source_BW.xlsx)[colnames(source_BW.xlsx) == "assigned_to"] <- "scraped.by"
colnames(source_BW2.xlsx)[colnames(source_BW2.xlsx) == "assigned_to"] <- "scraped.by"

source_MN.xlsx <- read_xlsx(paste("data/", "egret_MN", "/", "egret_MN.xlsx", sep = ""), sheet = "source")
source_MN.xlsx$accept_reject[which(source_MN.xlsx$studyID == "xiena")] <- "R"
source_MN.xlsx$reason_reject[which(source_MN.xlsx$studyID == "xiena")] <- "insuff source info"
source_MN.xlsx$accept_reject[which(source_MN.xlsx$studyID == "xuna")] <- "R"
source_MN.xlsx$reason_reject[which(source_MN.xlsx$studyID == "xuna")] <- "insuff source info"


source_HHN.xlsx <- read_xlsx(paste("data/", "egret_HHN", "/", "egret_HHN.xlsx", sep = ""), sheet = "source")

source_DK.xlsx <- read_xlsx(paste("data/", "egret_DK", "/", "egret_DK.xlsx", sep = ""), sheet = "source_ill")
colnames(source_DK.xlsx)[colnames(source_DK.xlsx) == "assigned_to"] <- "scraped.by"

source_DM.xlsx <- read_xlsx(paste("data/", "egret_DM", "/", "egret_DM.xlsx", sep = ""), sheet = "source")
source_DM.xlsx$accept_reject[which(source_DM.xlsx$studyID == "gere15")] <- "R"
source_DM.xlsx$reason_reject[which(source_DM.xlsx$studyID == "gere15")] <- "insuff data"

source_GG.xlsx <- read_xlsx(paste("data/", "egret_GG", "/", "egret_GG.xlsx", sep = ""), sheet = "source")
colnames(source_GG.xlsx)[colnames(source_GG.xlsx) == "accept_reject...11"] <- "accept_reject"
colnames(source_GG.xlsx)[colnames(source_GG.xlsx) == "studyID...19"] <- "studyID"
source_GG.xlsx$accept_reject[which(source_GG.xlsx$studyID == "hudson20")] <- "R"


source_AZ.xlsx <- read_xlsx(paste("data/", "egret_AZ", "/", "egret_AZ.xlsx", sep = ""), sheet = "source")
# source_AZ.xlsx$language[which(source_AZ.xlsx$studyID == "koyama08")] <- "Japanese"
# source_AZ.xlsx$language[which(source_AZ.xlsx$studyID == "kwon20")] <- "Korean"
# source_AZ.xlsx$language[which(source_AZ.xlsx$studyID == "lee13")] <- "Korean"

source_JS.xlsx <- read_xlsx(paste("data/", "egret_JS", "/", "egret_JS.xlsx", sep = ""), sheet = "source")

sourceTab <- rbind(source_TA.xlsx[,1:20], source_BW.xlsx[,1:20],source_BW2.xlsx[,1:20], source_CRD.xlsx[,1:20], source_MN.xlsx[,1:20], source_DK.xlsx[,1:20], source_HHN.xlsx[,1:20],
                   source_JS.xlsx[,1:20], source_AZ.xlsx[,1:20],  source_DL.xlsx[,1:20],  source_JN.xlsx[,1:20], source_DM.xlsx[,1:20], source_GG.xlsx[,1:20], source_SC.xlsx[,1:20])

# How many papers were rejected after being looked at?
unique(sourceTab$accept_reject)
temp <- subset(sourceTab, accept_reject == "N")
# N was a crop so R

rejected <- c("N", "R","R*","R?")
rej <- sourceTab[sourceTab$accept_reject %in% rejected, ]

rejectedPap <- unique(rej$studyID)
# 75 rejected papers

# What papers were not in english?
unique(sourceTab$language)

notEng <- c("not english", "not in english'","not in english","Korean","spanish","Japanese","not enlgish","not enligsh" ,"Persian" ,"Spanish","Chinese","Turkish","Arabic", "Not english","Mandarin","Russian","Portuguese","Italian","korean","chinese -english translation avaiilable but can't find it on websie without knowing chinese", "Estonian", "farsi")
notEnglish <- sourceTab[sourceTab$language %in% notEng, ]

nonEngPap <- unique(notEnglish$studyID)
# 108

#######################################################
# How many papers are not accounted for?

missing <- assigned[!assigned$datasetID %in% scrapedPap,] # 250
missing <- missing[!missing$datasetID %in% rejectedPap,] # 141
missing <- missing[!missing$datasetID %in% nonEngPap,] #65

miaPapers <- c("mughal10","mutele15","yahyaoglu06","Tashev08", "racek07","povoa09","morris16", "gere15","xiena","xuna")
missing <- missing[!missing$datasetID %in% miaPapers,] #57

tbd <- c("elisafenko15","maithani90","tanuja20",
         "fetouh14",
         "feurtado05",
         "barnhill82",
         "jensen09",
         "joshi03",
         "chien09",
         "denny04",
         "morozowska02",
         "mughal07","masoomeh09",
         "mulaudzi09",
         "nasri14",
         "santos19",
         "liu09","liu04", # unclear if ILL ever done
         "ikeda01",
         "kazinczi98",
         "baek21","vaghefi09","vahdati06", "vahdati12","veatch-blohm11","veiga-barbosa14", # assigned to Lizzie, Fredi, Dan
         "edwards73" # done, but dup so edwards73_1 and _2
        
         )
missing <- missing[!missing$datasetID %in% tbd,] #57

sort(unique(missing$datasetID)) #33


scrap <- unique(egret$datasetID)

notScraped <- assigned[!assigned$datasetID %in% scrap,]
# Tolu
# eTA <- read_xlsx("data/egret_TA/egret_TA.xlsx", sheet = "data_detailed")
# sTA <- unique(tolower(eTA$datasetID))
# 
# aTA <- subset(assigned, scraped.by == "TA")
# assigned <- assigned[!assigned$studyID %in% sTA,]
# 
# soTA <- read.csv("data/sourceCSV/egret_TA_source.csv")
# rTA <- subset(soTA, accept_reject == "R") # None
# 
# notE <- c("nasiri08","nasiri06","nishio09","chien02","cho05","choi12")
# assigned <- assigned[!assigned$studyID %in% notE,]
# 
# # Grace
# eGG <- read_xlsx("data/egret_GG/egret_GG.xlsx", sheet = "data_detailed")
# 
# sGG <- unique(tolower(eGG$datasetID))
# 
# aGG <- subset(assigned, scraped.by == "GG")
# assigned <- assigned[!assigned$studyID %in% sGG,]
# 
# soGG <- read.csv("data/sourceCSV/egret_GG_source.csv")
# rGG <- subset(soGG, accept_reject == "R") # None
# 
# notG <- c("Ï°∞Ï†ïÍ±¥19","hwang96","hu12","hsieh04","han09","downie91", "derakhshan13", "dalling99","cousins10")
# assigned <- assigned[!assigned$studyID %in% notG,]
# 
# # Dinara
# eDM <- read_xlsx("data/egret_DM/egret_DM.xlsx", sheet = "data_detailed")
# 
# sDM <- unique(tolower(eDM$datasetID))
# 
# aDM <- subset(assigned, scraped.by == "DM")
# assigned <- assigned[!assigned$studyID %in% sDM,]
# 
# soDM <- read.csv("data/sourceCSV/egret_DM_source.csv")
# rDM <- subset(soDM, accept_reject == "R") # None
# assigned <- assigned[!assigned$studyID %in% unique(rDM$studyID),]
# 
# notD <- c("ghanbari18","bochicchio86","bezdeckova12","abreu05","ailin14","acuna03")
# assigned <- assigned[!assigned$studyID %in% notD,]
# 
# # Sophia
# eSC <- read_xlsx("data/egret_SC/egret_SC.xlsx", sheet = "data_detailed")
# 
# sSC <- unique(tolower(eSC$datasetID))
# 
# aSC <- subset(assigned, scraped.by == "SC")
# assigned <- assigned[!assigned$studyID %in% sSC,]
# 
# soSC <- read.csv("data/sourceCSV/egret_SC_source.csv")
# rSC <- subset(soSC, accept_reject == "R") # None
# assigned <- assigned[!assigned$studyID %in% unique(rSC$studyID),]
# 
# notS <- c("boddy13","borghetti86", "boyaci21","braendel04","brandel05", "brenchley98")
# assigned <- assigned[!assigned$studyID %in% notS,]
# 
# # Alina
# eAZ <- read_xlsx("data/egret_AZ/egret_AZ.xlsx", sheet = "data_detailed")
# 
# sAZ <- unique(tolower(eAZ$datasetID))
# 
# aAZ <- subset(assigned, scraped.by == "AZ")
# assigned <- assigned[!assigned$studyID %in% sAZ,]
# 
# soAZ <- read.csv("data/sourceCSV/egret_AZ_source.csv")
# rAZ <- subset(soAZ, accept_reject == "R") # None
# 
# # notG <- c("Ï°∞Ï†ïÍ±¥19","hwang96","hu12","hsieh04","han09","downie91", "derakhshan13", "dalling99","cousins10")
# # assigned <- assigned[!assigned$studyID %in% notG,]
# 
# # Hoai Huong
# eHHN <- read_xlsx("data/egret_HHN/egret_HHN.xlsx", sheet = "data_detailed")
# 
# sHHN <- unique(tolower(eHHN$datasetID))
# 
# aHHN <- subset(assigned, scraped.by == "HHN")
# assigned <- assigned[!assigned$studyID %in% sHHN,]
# 
# soHHN <- read.csv("data/sourceCSV/egret_HHN.csv")
# rHHN <- subset(soHHN, accept_reject == "R") # 14
# assigned <- assigned[!assigned$studyID %in% unique(rHHN$studyID),]
# 
# 
# notG <- c("seo12")
# assigned <- assigned[!assigned$studyID %in% notG,]
# 
# # Monica
# eMN <- read_xlsx("data/egret_MN/egret_MN.xlsx", sheet = "data_detailed")
# 
# sMN <- unique(tolower(eMN$datasetID))
# 
# aMN <- subset(assigned, scraped.by == "MN")
# assigned <- assigned[!assigned$studyID %in% sMN,]
# 
# soMN <- read.csv("data/sourceCSV/egret_MN_source.csv")
# rMN <- subset(soMN, accept_reject == "R") # None
# assigned <- assigned[!assigned$studyID %in% unique(rMN$studyID),]
# 
# notG <- c("mohammadi21","mondani18","moradi18","moreno13","niu12","oliveira20", "qian11", "qiu17","qu11","radsarian17","raeisi21","rajabian07","rostamipoor15", "ryu17","sun98","taghavi18","taghinezad16" ,"takiya06","tuncer17","wu17","yang17","you08","yu15", "zarekia13"    )
# assigned <- assigned[!assigned$studyID %in% notG,]
# 
# # Deirdre
# eDL <- read_xlsx("data/egret_DL.xlsx", sheet = "data_detailed")
# sDL <- unique(tolower(eDL$datasetID))
# 
# aDL <- subset(assigned, scraped.by == "DL")
# assigned <- assigned[!assigned$studyID %in% sDL,]
# 
# soDL <- read.csv("data/sourceCSV/egret_DL_source.csv")
# rDL <- subset(soDL, accept_reject == "R") # None
# assigned <- assigned[!assigned$studyID %in% unique(rDL$studyID),]
# 
# notD <- c("boyaci21","tian05","tamaei01","kim83","wei17","wang21","wang10")
# assigned <- assigned[!assigned$studyID %in% notD,]
# 
# write.csv(assigned, "data/sourceCSV/notDone.csv", row.names = T)
