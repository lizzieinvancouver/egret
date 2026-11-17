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
                               "scocco98",
                               "kolodziejek15",
                               "mamut20"
                               ))] <- "mM"
#Add unit 'ug/L'
original$chemicalUnit[which(original$datasetID =="boscagli01")]<- "ug/L"
#Add unit 'M'
original$chemicalUnit[which(original$datasetID %in%
                              c("aldridge92",
                                "watanabe02",
                                 "morozowska02"))]<- "M"
#Add unit 'uM'
original$chemicalUnit[which(original$datasetID %in%
                              c("chien10",
                                "moeini21"))] <- "uM"

#Add unit '%'
original$chemicalUnit[which(original$datasetID =="teimouri13")]<-"%"

#Change value into mg/L for comparison
original <- original |>
  mutate(convertValue = case_when (
    is.na(chemicalConcent) ~ NA_real_,
    chemicalUnit=="mg/L" ~ chemicalConcent,
    chemicalUnit=="M" ~ chemicalConcent * 1000 * 346.37,
    chemicalUnit=="uM" ~ chemicalConcent / 1000 * 346.37,
    chemicalUnit=="ppm" ~ chemicalConcent,
    chemicalUnit=="ug/L" ~ chemicalConcent /1000,
    chemicalUnit=="mM" ~ chemicalConcent * 346.37
  ))

# Check whether the converted values are comparable
# Issue 1: scocco98 has extremely large values (>10,000) due to concentrations of 350 and 3500 mM.
# Issue 2: boscagli01 has an extremely small value (<0.001) due to a concentration of 0.05 ug/L.
check <- original |>
  filter(convertValue <0.01, convertValue != 0)|>
  select(datasetID, chemicalConcent, chemicalUnit, convertValue)

#Save the dataframe
write_csv (original, "analyses/effectSize/output/biggestEffectDataGAUnit.csv")
