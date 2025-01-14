## 26 September 2024 ##
# By Christophe

# Code to clean the treatment categories 

# Create new column of treatment category of cleaned treatments
d$Treatment <- d$treatment ### could potentially change that name

# Unique by treatments
subby <- d[!duplicated(d$treatment), ]
# Vector of all unique treatments
vec.treat <- sort(subby$treatment)
head(vec.treat)

### checking validity
subby.check <- d[!duplicated(d$Treatment), ]
# Vector of all unique treatments
vec.treatcheck <- subby.check$Treatment


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

