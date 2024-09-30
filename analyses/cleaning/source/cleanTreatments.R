## 26 September 2024 ##
# By Christophe

# Code to clean the treatment categories 

# Create new column of treatment category of cleaned treatments
d$treatmentCat <- d$treatment ### could potentially change that name

# Unique by treatments
subby <- d[!duplicated(d$treatment), ]
# Vector of all unique treatments
vec.treat <- subby$treatment
head(vec.treat)

### checking validity
subby.check <- d[!duplicated(d$treatmentCat), ]
# Vector of all unique treatments
vec.treatcheck <- subby.check$treatmentCat
head(vec.treatcheck)

### === ### === ### === ### === ### === ### === ### === ### === ### === 
#### Step 1. Easy cleaning of main treatments ####
### === ### === ### === ### === ### === ### === ### === ### === ### === 

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><<><><><><><><><>
##### 1.1. Control #####
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><<><><><><><><><>
#### checking different versions of control

#han10: missing control temperature for second experiment. Need to check other treatment names moisture.content.storage.freeze and moisture.content.storage.cold
# d$treatmentCat[which(d$treatment == "moisture.content.control")] <- "control"

# momonoki79: might need to be rescraped. Different types of entries e.g. storage.control, storage.stratif, storage time etc.
# d$treatmentCat[which(d$treatment == "storage.control")] <- "control"

# bhatt00: control scarified and non-scarified, I decided to change this to scarified and control, because it's input GA3 is input in chemical #tocheck
d$treatmentCat[which(d$treatment == "control - scarified")] <- "scarification"
d$treatmentCat[which(d$treatment == "controll - nonscarified")] <- "control"

# boscagli01
d$treatmentCat[which(d$treatment == "control - intact")] <- "control"

# picciau17 : decided to convert light control to light as they manipulated germ duration #tocheck
d$treatmentCat[which(d$treatment == "light control")] <- "light"
d$treatmentCat[which(d$treatment == "dark control")] <- "dark"
# nawrot-chorabik21: need to be investigated. Controls are not controls and there are a lot of different treatments #to check
d$treatmentCat[which(d$treatment == "control, no dormancy breaking treatment")] <- "control"

d$treatmentCat[which(d$treatment == "NA (control)")] <- "control"
# watanabe02 : ok to change to control
d$treatmentCat[which(d$treatment == "NA (control strat)")] <- "control"

# basbag09: removed space
d$treatmentCat[which(d$datasetID =="basbag09" & d$treatment == "control ")] <- "control"

# yan16: removed space
d$treatmentCat[which(d$datasetID =="yan16" & d$treatment == "control ")] <- "control"

# maithani90: a couple of things needs to be changed. Should change stratification to cold strat and change nursery and laboratory to two different experiments
### change to two studies. Check if it's ok if I changed original column
d$study[which(d$datasetID == "maithani90" & d$treatment == "control (nursery)")] <- "exp2"
d$study[which(d$datasetID == "maithani90" & d$treatment == "stratified (30days) (nursery)")] <- "exp2"
### control
d$treatmentCat[which(d$datasetID == "maithani90" & d$treatment == "control (nursery)")] <- "control"

d$treatmentCat[which(d$treatment == "NA(control)")] <- "control"

d$treatmentCat[which(d$treatment == "control, no treatment")] <- "control"
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><<><><><><><><><>
##### 1.2. Stratification #####
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><<><><><><><><><>
#### Scrap-Subset for double checking -- will be deleted
unique(d$treatment[grep("strat", d$treatment)])

sub <- d[, c("datasetID", "study", "entered.by", "species", "provenance.lat", "treatment", "other.treatment", "photoperiod", "chemical", "chemical.concent", "trt.duration", "scarification", "scarif.type", "soaking", "soaked.in", "soaking.duration", "germ.duration")]
subb<-subset(sub, treatment == "stratification")
unique(subb$datasetID)
head(subb)
maithani90<-subset(d, datasetID == "maithani90")
######## ####### ######## ####### ######## ####### ######## ####### 

d$treatmentCat[which(d$datasetID == "maithani90" & d$treatment == "stratified (15days)")] <- "cold.strat"
d$treatmentCat[which(d$datasetID == "maithani90" & d$treatment == "stratified (30days)")] <- "cold.strat"
d$treatmentCat[which(d$datasetID == "maithani90" & d$treatment == "stratified (15days) (nursery)")] <- "cold.strat"
d$treatmentCat[which(d$datasetID == "maithani90" & d$treatment == "stratified (30days) (nursery)")] <- "cold.strat"

# tilki06 : change stratification to warm or cold
d$treatmentCat[which(d$datasetID == "tilki06" & d$treatment == "stratification")] <- "cold.strat"

# tang10_1 : change stratification to cold, 2, 3 and 4 are cold strat #tocheck
d$treatmentCat[which(d$datasetID == "tang10_1" & d$study =="exp2" & d$treatment == "stratification")] <- "cold.strat"
d$treatmentCat[which(d$datasetID == "tang10_1" & d$study =="exp3" & d$treatment == "stratification")] <- "cold.strat"
d$treatmentCat[which(d$datasetID == "tang10_1" & d$study =="exp4" & d$treatment == "stratification")] <- "cold.strat"

# tang10_1 : change stratification to are warm strat #tocheck

# tilki07 : change stratification to warm or cold. Has both cold strat and cold+warmstrat
tilki07<-subset(d, datasetID == "tilki07")
d$treatmentCat[which(d$datasetID == "tilki07" & d$chill.temp =="****" & d$treatment == "stratification")] <- "cold.strat"

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

