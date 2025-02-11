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
idx <- which(d$datasetID == sort(unique(d$datasetID))[1])
check <- d[idx,]
check_short <- subset(check, select = c("datasetID", "study", "species", "treatment", "other.treatment",
                                        "treatmentCor", "treatmentDetails",
                                        "storageType", "storageDetails", "storageTemp", "storageDuration",
                                        "chillTemp", "chillDuration", "chillTempCycle", "chillLightCycle",
                                        "germTemp", "germDuration", "photoperiodCor",
                                        "scarification", "scarifType", "scarifTypeGen", "scarifTypeSpe",
                                        "soaking", "soaked.in", "soaking.duration",
                                        "chemicalCor", "trt.duration",
                                        "respvar", "response", "figure"))

#acosta13
d$treatmentCor[which(d$datasetID == "acosta13" & d$treatment == "temperature")] <- "germination temperature"
temp <- c(0, -0.3, -0.4, -0.6, -0.9, -1.3)
d$treatmentDetails[which(d$datasetID == "acosta13" & d$treatment == "osmotic potential")] <- paste0(temp, " MPa")
d$treatmentCor[which(d$datasetID == "acosta13" & d$treatment == "dry storage")] <- "storage x photoperiod"

#ahola99
d$treatmentCor[which(d$datasetID == "ahola99")] <- "r/fr light x moist chilling x germination temperature"

#aiello07
#possible missing data on gibberelic acid treatments in Table 3 and Table 6
d$treatmentCor[which(d$datasetID == "aiello07")] <- "population x cold stratification"

#airi09
temp <- c(rep("", 5), rep(" x cold stratification", 15), rep(" x Thiourea", 15),
          rep(" x GA3", 15), rep(" x KNO3", 15))
d$treatmentCor[which(d$datasetID == "airi09" & d$figure == "Table 2")] <- paste0("population", temp)

#al-absi10
temp <- c(rep("hot water", 16), rep("H2SO4", 16), rep("GA", 16))
d$treatmentCor[which(d$datasetID == "al-absi10")] <- paste0("cold stratification x ", temp)
#soaking duration needs to be cleaned, "120" instead of 0 - 30 minutes used in paper

#albrecht20
d$treatmentCor[which(d$datasetID == "albrecht20")] <- "cold stratification x germination temperature x photoperiod"
#weird shared data between experiments

#aldridge92
d$treatmentCor[which(d$datasetID == "aldridge92")] <- "moisture content x stratification x hormone"

#alhelal96
d$treatmentCor[which(d$datasetID == "alhelal96" & d$study == "exp1")] <- "soaking temperature x hormone x photoperiod"
d$treatmentCor[which(d$datasetID == "alhelal96" & d$study == "exp2")] <- "germination temperature"

#alptekin02
d$treatmentCor[which(d$datasetID == "alptekin")] <- "population x stratification x scarification"

#amini18, paper pending

#amooaghaie09
d$treatmentCor[which(d$datasetID == "amooaghaie09")] <- "moist chilling x hormone"

#arslan11
d$treatmentCor[which(d$datasetID == "arslan11")] <- "moist chilling x hormone x photoperiod x germination temperature"
d$treatmentCor[which(d$datasetID == "arslan11" & d$figure == "table 1")]
# i think the rest of table 1, except the one with scarification, should be removed

#barnhill82
d$treatmentCor[which(d$datasetID == "barnhill82")] <- "stratification x germination temperature x photoperiod"
# add strat temp 3degC

#barros12
d$treatmentCor[which(d$datasetID == "barros12")] <- "stratification x hormone x germination temperature"

#basaran11, paper pending

#basbag09
d$treatmentCor[which(d$datasetID == "basbag09")] <- "germination temperature x germination duration"

#batlla03
d$treatmentCor[which(d$datasetID == "batlla03" & d$study != "exp3")] <- "germination temperature x germination duration"
d$treatmentCor[which(d$datasetID == "batlla03" & d$study == "exp3")] <- "storage"

#battaglia93
d$treatmentCor[which(d$datasetID == "battaglia93" & !(d$figure %in% c("Figure 8", "Figure 9")))] <-
  "seedlot x stratification x germination temperature"
d$treatmentCor[which(d$datasetID == "battaglia93" & d$figure == "Figure 8")] <- "stratification x high temperature"

#battaglia97
d$treatmentCor[which(d$datasetID == "battaglia97")] <- "seedlot x stratification x germination temperature"

#beikmohammadi12, various

#bhatt00, various

#bibby53, supposed to be removed

#borghetti86
d$treatmentCor[which(d$datasetID == "borghetti86")] <- "seedlot x cold-moist stratification"

#boscagli01, various


#brandel05
d$treatmentCor[which(d$datasetID == "brandel05")] <- "dormancy x stratification x germination chemical"

#brenchley98

#eelgrass

#budisavljevic21
d$treatmentCor[which(d$datasetID == "budisavljevic21")] <-
  "stratification x germination temperature x photoperiod"

#chill temp for table 2e should be 23

#bungard97
d$treatmentCor[which(d$datasetID == "bungard97" & d$study == "exp1")] <- "chilling x nitrogen"
d$treatmentCor[which(d$datasetID == "bungard97" & d$study == "exp2")] <- "chilling x nitrogen x photoperiod"
d$treatmentCor[which(d$datasetID == "bungard97" & d$study == "exp3")] <- "chilling x inorganic salt"
d$treatmentCor[which(d$datasetID == "bungard97" & d$study == "exp4")] <- "chemical"

#bytnerowicz14
d$treatmentCor[which(d$datasetID == "bytnerowicz14")] <- "germination temperature x photoperiod"

#carpenter92
d$treatmentCor[which(d$datasetID == "carpenter92" & d$study %in% c("exp1", "exp2"))] <- "germination temperature"
d$treatmentCor[which(d$datasetID == "carpenter92" & d$study == "exp3")] <- "hormone"
d$treatmentCor[which(d$datasetID == "carpenter92" & d$study == "exp4")] <- "storage"

#castro95
d$treatmentCor[which(d$datasetID == "castro95")] <- "cold stratification x substrate moisture x species"

#chakraborty92, various

#chen08
d$treatmentCor[which(d$datasetID == "chen15" & d$figure == "Table1")] <- "cold moist stratification"
d$treatmentCor[which(d$datasetID == "chen15" & d$figure == "figure2")] <- "cold stratification"
#figures 3 and 4 not scraped

#chen15
d$treatmentCor[which(d$datasetID == "chen15" & d$figure == "Fig 1a")] <- "germination temperature"
d$treatmentCor[which(d$datasetID == "chen15" & d$figure == "Fig 1b")] <- "cold stratification"
d$treatmentCor[which(d$datasetID == "chen15" & d$figure == "Table 1")] <- "germination temperature x cold stratification"
d$treatmentCor[which(d$datasetID == "chen15" & d$figure == "Table 2")] <- "hormone"

#chien10
d$treatmentCor[which(d$datasetID == "chien10" & d$study == "exp1")] <- "soaking (hormone)"
d$treatmentCor[which(d$datasetID == "chien10" & d$study == "exp2")] <- "germination temperature x soaking (hormone)"
d$treatmentCor[which(d$datasetID == "chien10" & d$study == "exp4")] <- "moisture content x storage"

#chien11
d$treatmentCor[which(d$datasetID == "chien11")] <- "germination temperature"

#cho18, a or b

#chuanren04
d$treatmentCor[which(d$datasetID == "chuanren04")] <- ""


#cuena-lombrana18
d$treatmentCor[which(d$datasetID == "cuena-lombrana18")] <- "site x stratification x germination temperature"
temp <- c(rep(c(rep("cold", 6), rep("warm and cold", 6)), 4))
d$treatmentDetails[which(d$datasetID == "cuena-lombrana18")] <- paste0(temp, " stratification")

#dalling99
d$treatmentCor[which(d$datasetID == "dalling99")] <- "scarification x germination temperature x photoperiod"

#deb17
d$treatmentCor[which(d$datasetID == "deb17" & d$study == "exp1")] <- "light"
d$treatmentCor[which(d$datasetID == "deb17" & d$figure == "Figure 5")] <- "stratification x soil"

#dehgan84
d$treatmentCor[which(d$datasetID == "dehgan84")] <- "stratification x hormone"
#missing stratification data as chilling

#downie91
#one data point from figure 3 seems to be left in
#what is invigoration...




d$treatmentCor[which(d$datasetID == "")] <- ""

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

