## 26 September 2024 ##
# By Christophe

# Code to clean the treatment categories 

# Create new column of treatment category of cleaned treatments
d$treatmentCor <- d$treatment
d$treatmentDetails <- NA

#unique(d$treatment)
#unique(d$treatmentCor)
#unique(d$treatmentDetails)

# Find data points
idx <- which(d$treatment == unique(d$treatment)[1])
check <- d[idx,]
check_short <- subset(check, select = c("datasetID", "study", "treatment", "other.treatment",
                                        "treatmentCor", "treatmentDetails",
                                        "storageType", "storageDetails", "storageTemp", "storageDuration",
                                        "chillTemp", "chillDuration", "chillTempCycle", "chillLightCycle",
                                        "germTemp", "germDuration", "photoperiodCopy",
                                        "scarification", "scarifType", "scarifTypeGen", "scarifTypeSpe",
                                        "soaking", "soaked.in", "soaking.duration",
                                        "chemicalCor", "trt.duration",
                                        "respvar", "response", "figure"))
idx <- which(d$datasetID == sort(unique(d$datasetID))[179])
check <- d[idx,]
check_short <- subset(check, select = c("datasetID", "study", "species", "treatment", "other.treatment",
                                        "treatmentCor", "treatmentDetails",
                                        "storageType", "storageDetails", "storageTemp", "storageDuration",
                                        "chillTemp", "chillDuration", "chillTempCycle", "chillLightCycle",
                                        "germTemp", "germDuration", "photoperiodCopy",
                                        "scarification", "scarifType", "scarifTypeGen", "scarifTypeSpe",
                                        "soaking", "soaked.in", "soaking.duration",
                                        "chemicalCor", "trt.duration",
                                        "respvar", "response", "figure"))

#acosta13
d$treatmentCor[which(d$datasetID == "acosta13" & d$treatment == "temperature")] <- "germination temperature"
d$treatmentDetails[which(d$datasetID == "acosta13" & d$study == "exp1")] <- "constant"
d$treatmentDetails[which(d$datasetID == "acosta13" & d$study == "exp2")] <- "alternating"

temp <- c(0, -0.3, -0.4, -0.6, -0.9, -1.3)
d$treatmentDetails[which(d$datasetID == "acosta13" & d$treatment == "osmotic potential")] <- paste0(temp, " MPa")

temp <- c("dark", "light")
d$treatmentCor[which(d$datasetID == "acosta13" & d$treatment == "dry storage")] <- "storage x photoperiod"
d$treatmentDetails[which(d$datasetID == "acosta13" & d$treatment == "dry storage")] <- temp

#ahola99
d$treatmentCor[which(d$datasetID == "ahola99")] <- "light x chilling x germination temperature"
d$treatmentDetails[which(d$datasetID == "ahola99")] <- "red / far red x moist x constant"
d$treatmentDetails[which(d$datasetID == "ahola99" & d$treatment == "control")] <- "no chilling"
d$treatmentDetails[which(d$datasetID == "ahola99" & d$treatment == "dark/moist chill")] <- "dark, moist chilling"

#aiello07
#possible missing data on gibberelic acid treatments in Table 3 and Table 4


#airi09
d$treatmentCor[which(d$datasetID == "airi09" & d$treatment %in% c("GA3", "KNO3", "Thiourea"))] <- "chemical"
d$treatmentDetails[which(d$datasetID == "airi09" & d$treatment %in% c("GA3", "KNO3", "Thiourea"))] <- 
  d$treatment[which(d$datasetID == "airi09" & d$treatment %in% c("GA3", "KNO3", "Thiourea"))]

#al-absi10
d$treatmentCor[which(d$datasetID == "al-absi10")] <- "cold stratification x scarification"
d$treatmentDetails[which(d$datasetID == "al-absi10" &
                           d$treatment == "stratification.hotwater")] <-
  "hot water"
d$treatmentDetails[which(d$datasetID == "al-absi10" &
                           d$treatment == "stratification.H2SO4")] <-
  "H2SO4"
d$treatmentDetails[which(d$datasetID == "al-absi10" &
                           d$treatment == "stratification.GA")] <-
  "GA"
d$treatmentDetails[which(d$datasetID == "al-absi10" & is.na(d$scarifType) & is.na(d$chemicalCor))] <- "control"
#soaking duration needs to be cleaned, maybe include soaking in chemical column

#albrecht20, weird mixed data in different figures and discussed as different experiments

#aldridge92, paper pending

#prknova15 - 163
d$treatmentCor[which(d$datasetID == "prknova15")] <- "stratification x seed coat"
d$treatmentCor

#rouhi13 - 179
#wickens01 - 220


#fixing treatment names 
d$treatmentFixed <- d$treatment

# prknova15
# both seed coat removal and soaking
d$other.treatment[which((d$datasetID == "prknova15") & 
                          d$scarifTypeGen == "soaking")] <- d$scarifType[which((d$datasetID == "prknova15") & 
                                                                                 d$scarifTypeGen == "soaking")]
d$treatmentFixed[which((d$datasetID == "prknova15") & d$scarifTypeGen == "soaking")] <- "seed coat removal, soaking in water"

# rouhi13
d$other.treatment[which((d$datasetID == "rouhi13") & 
                          d$scarifTypeGen == "soaking")] <- d$scarifType[which((d$datasetID == "rouhi13") & 
                                                                                 d$scarifTypeGen == "soaking")]
# Wickens01
d$treatmentFixed[which((d$datasetID == "Wickens01") & 
                         d$scarif.type == "hot water 70C")] <- "hot water 70C"
d$treatmentFixed[which((d$datasetID == "Wickens01") & 
                         d$other.treatment == "soaking")] <- "soaking"


print(d$treatmentFixed[which(d$scarifTypeGen == "soaking")])

### === ### === ### === ### === ### === ### === ### === ### === ### === 
#### Easy cleaning of main treatments ####
### === ### === ### === ### === ### === ### === ### === ### === ### === 
subset(d, Treatment == "control, no dormancy breaking treatment")
sub<-subset(d, datasetID == "maithani90")

View(sub)
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><<><><><><><><><>
##### Don #####
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><<><><><><><><><>
#### checking different versions of control
d$Treatment[which(d$treatment == "storage.control")] <- "control"

# nawrot-chorabik21: blue and red colors only in treatment columns. Don't know what to do

# bhatt00: checked and more specific treatments entered in appropriate columns
# momonoki79: checked and more specific treatments entered in appropriate columns
# boscagli01: checked and more specific treatments entered in appropriate columns
# picciau17 : checked and more specific treatments entered in appropriate columns
# maithani90: checked and more specific treatments entered in appropriate columns
# nawrot-chorabik21
d$Treatment[which(d$treatment == "control, no dormancy breaking treatment" & d$datasetID == "nawrot-chorabik21")] <- "control"
# NA (control)
d$Treatment[which(d$treatment == "NA (control)")] <- "control"
d$Treatment[which(d$treatment == "NA(control)")] <- "control"
# control, no treatment
d$Treatment[which(d$treatment == "control, no treatment")] <- "control"
# watanabe02 : change to control
d$Treatment[which(d$treatment == "NA (control strat)")] <- "control"
# basbag09: removed space
d$Treatment[which(d$datasetID =="basbag09" & d$treatment == "control ")] <- "control"
# yan16: removed space
d$Treatment[which(d$datasetID =="yan16" & d$treatment == "control ")] <- "control"



# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><<><><><><><><><>
##### 1.2. Stratification #####
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><<><><><><><><><>
#### Scrap-Subset for double checking -- will be deleted
unique(d$treatment[grep("strat", d$treatment)])

subb<-subset(sub, treatment == "stratification")
unique(subb$datasetID)
cchead(subb)
sub<-subset(d, datasetID == "tilki06")
View(sub)
######## ####### ######## ####### ######## ####### ######## ####### 
# maithani90: maithani90: checked and more specific treatments entered in appropriate columns
# 
# d$Treatment[which(d$datasetID == "maithani90" & d$treatment == "stratified (15days)")] <- "cold.strat"
# d$Treatment[which(d$datasetID == "maithani90" & d$treatment == "stratified (30days)")] <- "cold.strat"
# d$Treatment[which(d$datasetID == "maithani90" & d$treatment == "stratified (15days) (nursery)")] <- "cold.strat"
# d$Treatment[which(d$datasetID == "maithani90" & d$treatment == "stratified (30days) (nursery)")] <- "cold.strat"

# tilki06 : spring and fall sowing are not specified in more precise colums
d$Treatment[which(d$datasetID == "tilki06" & d$treatment == "stratification")] <- "cold.strat"

# tang10_1 : change stratification to cold, 2, 3 and 4 are cold strat #tocheck
d$Treatment[which(d$datasetID == "tang10_1" & d$study =="exp2" & d$treatment == "stratification")] <- "cold.strat"
d$Treatment[which(d$datasetID == "tang10_1" & d$study =="exp3" & d$treatment == "stratification")] <- "cold.strat"
d$Treatment[which(d$datasetID == "tang10_1" & d$study =="exp4" & d$treatment == "stratification")] <- "cold.strat"

# tang10_1 : change stratification to warm strat #tocheck

# tilki07 : change stratification to warm or cold. Has both cold strat and cold+warmstrat
tilki07<-subset(d, datasetID == "tilki07")
d$Treatment[which(d$datasetID == "tilki07" & d$chill.temp =="***" & d$treatment == "stratification")] <- "cold.strat"

# tilki06 : change stratification to warm or cold
# tang10_1 : change stratification to warm or cold
# tilki07 : change stratification to warm or cold
# chien09 : change stratification to warm or cold
# kalimuthu95 : change stratification to warm or cold
# markovic20 : change stratification to warm or cold
# chuanren04 : change stratification to warm or cold
# deb17 : change stratification to warm or cold
# cousins10 : change stratification to warm or cold
# naseri18 : change stratification to warm or cold
# saeed16  : change stratification to warm or cold
# santos19 : change stratification to warm or cold
# mulaudzi09 : change stratification to warm or cold
# rizwan18 : change stratification to warm or cold
# tabatabaeian18 : change stratification to warm or cold
# tang21 : change stratification to warm or cold
# tylkowski10 : change stratification to warm or cold
# wang09  : change stratification to warm or cold
# zhou08 : change stratification to warm or cold
# yang08       : change stratification to warm or cold
# geszprych02  : change stratification to warm or cold
# vahdati12 : change stratification to warm or cold
# yang16_1 : change stratification to warm or cold
# yang20 : change stratification to warm or cold
# airi09 : change stratification to warm or cold


##### 1.3.  #####
##### 1.4.  #####
##### 1.5.  #####

