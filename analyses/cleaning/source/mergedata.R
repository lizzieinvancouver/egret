## Started 1 December 2023 ## 
## By Deirdre and Lizzie (to start)##

## Read in all the datafiles! ##
## Clean them so they rbind ##

#Deirdre 
egret_DL <- read.csv("input/egret_DL.csv")
length(unique(egret_DL$datasetID)) #7
dim(egret_DL) #381  45
# Tolu
egret_TA <- read.csv("input/egret_TA.csv")
#colnames(egret_TA)[colnames(egret_TA) == "germ.tim.zero"] <- "germ.time.zero"
colnames(egret_TA)[colnames(egret_TA) == "notes"] <- "Notes"
length(unique(egret_TA$datasetID)) #38
dim(egret_TA) # 2879   45

# Sophia C
egret_SC <- read.csv("input/egret_SC.csv")
#colnames(egret_SC)[colnames(egret_SC) == "germ.tim.zero"] <- "germ.time.zero"
colnames(egret_SC)[colnames(egret_SC) == "notes"] <- "Notes"
egret_SC <- egret_SC[complete.cases(egret_SC$datasetID),]
length(unique(egret_SC$datasetID)) #35
dim(egret_SC) # 4949   45

#Christophe
egret_CRD <- read.csv("input/egret_CRD.csv")
#colnames(egret_CRD)[colnames(egret_CRD) == "germ.tim.zero"] <- "germ.time.zero"
length(unique(egret_CRD$datasetID)) #4
egret_CRD <- egret_CRD[,1:45]
dim(egret_CRD) #302  45

# Justin
egret_JN <- read.csv("input/egret_JN.csv")
colnames(egret_JN)[colnames(egret_JN) == "germ.tim.zero"] <- "germ.time.zero"
length(unique(egret_JN$datasetID)) #58
dim(egret_JN) # 3256   45

# Britany
egret_BW <- read.csv("input/egret_BW.csv")
#colnames(egret_BW)[colnames(egret_BW) == "germ.tim.zero"] <- "germ.time.zero"
length(unique(egret_BW$datasetID)) #7
dim(egret_BW) #1219   45

egret_MN <- read.csv("input/egret_MN.csv")
length(unique(egret_MN$datasetID)) #50
dim(egret_MN) #7912   45

egret_HHN <- read.csv("input/egret_HHN.csv")
length(unique(egret_HHN$datasetID)) #26
dim(egret_HHN) #2619   45

egret_DK <- read.csv("input/egret_DK.csv")
length(unique(egret_DK$datasetID)) #10
dim(egret_DK) # 838  45
egret_DK <- egret_DK[,1:45]

egret_DM <- read.csv("input/egret_DM.csv")
length(unique(egret_DM$datasetID)) #27
dim(egret_DM) #2944   45

egret_GG <- read.csv("input/egret_GG.csv")
length(unique(egret_GG$datasetID)) #12
dim(egret_GG) #1641   45
egret_GG <- egret_GG[,1:45]

egret_AZ <- read.csv("input/egret_AZ.csv")
length(unique(egret_AZ$datasetID)) #12
dim(egret_AZ) #1071   45

egret_JS <- read.csv("input/egret_JS.csv")
egret_JS <- egret_JS[complete.cases(egret_JS$response),] # tons of empty rows!  # 757 rows actual data
dim(egret_JS) #757  45
length(unique(egret_JS$datasetID)) #9

egret1 <- rbind(egret_TA, egret_BW, egret_CRD, egret_MN, egret_HHN, egret_DK,
                egret_JS, egret_DM, egret_AZ, egret_GG)
colnames(egret1)[colnames(egret1) == "germ.tim.zero"] <- "germ.time.zero"
egret2 <- rbind(egret_DL, egret_SC, egret1)

colnames(egret2)[colnames(egret2) == "chemcial.concent"] <- "chemical.concent"
egret <- rbind(egret_JN, egret2)

#write.csv(egret, "analyses/output/egretData.csv", row.names = F)
