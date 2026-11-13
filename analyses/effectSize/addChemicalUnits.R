###Add Chemical Units to the origianl file ###
###Started by Chuning ###
###Last edit on Nov. 13th 2026 ###

library(tidyverse)
library(repr)
library(dplyr)

#Read the cleaned file including chemical concent and datasetID
original <- read_csv("analyses/effectSize/output/biggestEffectDataGA.csv")

#Add a new empty column to the original database
original[,'chemicalUnit']<-NA

#Add unit 'ppm'
original$chemicalUnit[which(original$datasetID %in%
                       c("al-absi10", 
                         "chakraborty92", 
                         "beikmohammadi12",
                         "kamareh12",
                         "keshtkar08",
                         "meena06",
                         "parvin15",
                         "rahnama-ghahfarokhi07",
                         "rouhi12",
                         "surya17",
                         "wytsalucy21",
                         "yaqoob17",
                         "zardari19",
                         "zare11",
                         "arslan11",
                         "joshi03",
                         "pipinis12",
                         "rezvani14",
                         "irvani12",
                         "bhatt00"))] <- "ppm"
#Add unit 'mg/L'
original$chemicalUnit[which(original$datasetID %in%
                      c("tang10a",
                        "hawkins19",
                        "barros12",
                        "naseri18",
                        "nasri14",
                        "moradi12",
                        "rostamipoor20",
                        "yan16",
                        "zadeh15",
                        "zhou03",
                        "rehman00",
                        "kim16",
                        "han10",
                        "javanmard14",
                        "ren08",
                        "zlesak07",
                        "tang10b"))] <- "mg/L"
#Add unit 'mM'
original$chemicalUnit[which(original$datasetID %in%
                              c("airi09",
                               "necajeva13",
                               "jang22",
                               "scocco98"))] <- "mM"
#Add unit 'ug/L'
original$chemicalUnit[which(original$datasetID =="boscagli01")]<- "ug/L"
#Add unit 'M'
original$chemicalUnit[which(original$datasetID %in%
                              c("aldridge92",
                                "watanabe02",
                                 "morozowska02"))]<- "M"
#Add unit 'mmol/L'
original$chemicalUnit[which(original$datasetID =="mamut20")]<-"mmol/L"
#Add unit 'uM'
original$chemicalUnit[which(original$datasetID %in%
                              c("chien10",
                                "moeini21"))] <- "uM"
#Add unit 'mM/L'
original$chemicalUnit[which(original$datasetID =="kolodziejek15")]<-"mM/L"
#Add unit '%'
original$chemicalUnit[which(original$datasetID =="teimouri13")]<-"%"

#Save the dataframe
write_csv (original, "analyses/effectSize/output/biggestEffectDataGAUnit.csv")
