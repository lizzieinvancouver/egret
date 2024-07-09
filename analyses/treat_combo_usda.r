# started Jul 8, 2024 by Fredi

# aim is read in the csv files from everyone's data that will include both sourced cleaning code and write out a raw file

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

if(length(grep("deirdreloughnan", getwd()) > 0)) {
  setwd("~/Documents/github/egret/analyses")
} else if(length(grep("lizzie", getwd()) > 0)) {
  setwd("/Users/lizzie/Documents/git/projects/egret/analyses")
} else if(length(grep("sapph", getwd()) > 0)) {
  setwd("/Users/sapph/Documents/ubc things/work/egret/analyses")
} else if(length(grep("Xiaomao", getwd()) > 0)) {
  setwd("C:/PhD/Project/egret/analyses")
} else if(length(grep("britanywuuu", getwd()) > 0)) {
  setwd("~/Documents/ubc/year5/TemporalEcologyLab/egret/analyses")
} else if(length(grep("Ken", getwd())) > 0){
  setwd("/Users/Ken Michiko Samson/Documents/Temporal Ecology Lab/egret/analyses")
} else if(length(grep("christophe_rouleau-desrochers", getwd())) > 0){
  setwd("/Users/christophe_rouleau-desrochers/Documents/github/egret/analyses")
} else if(length(grep("frederik", getwd())) > 0){
  setwd("/Users/frederik/github/egret/analyses")
}

egret <- read.csv("./output/egretclean.csv", sep=",", header=TRUE)

#get an overview of the data
str(egret)
summary(egret)
#total rows
nrow(egret)

#make as factor/as numeric
egret$species <- as.factor(egret$species)
egret$woody <- as.factor(egret$woody)
egret$crop <- as.factor(egret$crop)
egret$continent <- as.factor(egret$continent)
egret$storage.type <- as.factor(egret$storage.type)
egret$storage.time <- as.numeric(egret$storage.time)
summary(egret$storage.time)
egret$storage.temp <- as.numeric(egret$storage.temp)
summary(egret$storage.time)
egret$woody <- as.factor(egret$woody)
egret$woody <- as.factor(egret$woody)


length(unique(egret$species))

names(egret)
unique(egret$photoperiod)



#these are the most cleaned variables:
egret$chill.tempCor
egret$germ.tempCor
str(egret)

#how many species are in each datasetID
datasetID <- aggregate(species ~ datasetID, data = egret, FUN = function(x) length(unique(x)))

#Counting how many species are there in every dormancy class
d.perc$DormancyClass <- as.factor(d.perc$`Dormancy Class`)
d.perc$scarification <- as.factor(d.perc$scarification)
dormclass <- aggregate(species ~ treatment, data = d.perc, FUN = function(x) length(unique(x)))


#################################################################################
#################################################################################
usda <- read.csv("./scrapeUSDAseedmanual/output/usdaGerminationData.csv", sep=",", header=TRUE)

usda$stratification.temp
str(usda)
summary(usda)
names(usda)
germination.per
unique(usda$responseVar_clean)
#
usda$responseVar_clean <- usda$responseVar
#rename level "percent.germ" to "perc.standard"
usda$responseVar_clean[usda$responseVar_clean == "percent.germ"] <- "perc.standard"
usda$responseVar_clean[usda$responseVar_clean == "percent.germ.total"] <- "perc.standard"
usda$responseVar_clean[usda$responseVar_clean == "germ.capacity"] <- "perc.standard"
usda$responseVar_clean[usda$responseVar_clean == "mean.germ.capacity"] <- "perc.standard"
usda$responseVar_clean[usda$responseVar_clean == "percent.germ.15degC.incubated"] <- "perc.standard"
usda$responseVar_clean[usda$responseVar_clean == "mean.percent.germ.energy"] <- "percent.germ.energy"


#aggregate(species ~ usda$responseVar_clean, data = usda, FUN = function(x) length(unique(x)))

#make a new column "spec" to combine genus and species
usda$spec <- paste(usda$genus, usda$species, sep = "_")
summary(is.na(usda$spec))

#these are the interesting exploratory variables 
names(usda)
unique(usda$pretreatment.duration)
unique(usda$pretreatment.hotwater)
unique(usda$pretreatment)
unique(usda$pretreatment.duration)


##forcing
##chilling
##scarification

##create indicator variables
usda$forc_indic <- "no"
usda$chill_indic <- "no"
usda$scar_indic <- "no"

#####forcing
##what are variables related to forcing
unique(usda$germ.dur.Min)
unique(usda$germ.dur.Max)
unique(usda$germ.duration)
#check them
#if row contains a value then put "yes" in usda$forc_indic
usda$forc_indic[!is.na(usda$germ.dur.Min)] <- "yes"
usda$forc_indic[!is.na(usda$germ.dur.Max)] <- "yes"
usda$forc_indic[!is.na(usda$germ.duration)] <- "yes"


#####chilling
##what are variables related to chilling
unique(usda$stratification.temp)
unique(usda$cold.stratification.duration)
unique(usda$chilling)
unique(usda$chill.duraton)
unique(usda$cold.strat.dur.Min)
unique(usda$cold.strat.dur.Max)
unique(usda$cold.strat.dur.Avg)
unique(usda$chill.dur.Avg)

#check them
#if row contains a value then put "yes" in usda$forc_indic
usda$chill_indic[!is.na(usda$stratification.temp)] <- "yes"
usda$chill_indic[!is.na(usda$cold.stratification.duration)] <- "yes"
usda$chill_indic[usda$chilling == "Y"] <- "yes" #if "chilling" contains "Y" then put "yes" in usda$chill_indication
usda$chill_indic[!is.na(usda$chill.duraton)] <- "yes"
usda$chill_indic[!is.na(usda$cold.strat.dur.Min)] <- "yes"
usda$chill_indic[!is.na(usda$cold.strat.dur.Max)] <- "yes"
usda$chill_indic[!is.na(usda$cold.strat.dur.Avg)] <- "yes"
usda$chill_indic[!is.na(usda$chill.dur.Avg)] <- "yes"


#####scarification
##what are variables related to scarification
unique(usda$scarifTypeGen)
unique(usda$scarifType)
#check them
#if row contains a value then put "yes" in usda$forc_indic
usda$scar_indic[!is.na(usda$scarifTypeGen)] <- "yes"



### calculate treatment combinations
#how many species were exposed to forcing only
#how many rows are there with "forc_indic" == "yes"
length(usda$forc_indic[usda$forc_indic == "yes"]) 
#how many unique species with "forc_indic" == "yes"
length(unique(usda$spec[usda$forc_indic == "yes"])) #270 species

# Save each value for every combination
combination_table <- data.frame(forc_indic = c("yes", "yes", "yes", "yes", "no", "no", "no", "no"),
                                chill_indic = c("yes", "yes", "no", "no", "yes", "yes", "no", "no"),
                                scar_indic = c("yes", "no", "yes", "no", "yes", "no", "yes", "no"),
                                number = c(length(unique(usda$spec[usda$forc_indic == "yes" & usda$chill_indic == "yes" & usda$scar_indic == "yes"])),
                                           length(unique(usda$spec[usda$forc_indic == "yes" & usda$chill_indic == "yes" & usda$scar_indic == "no"])),
                                           length(unique(usda$spec[usda$forc_indic == "yes" & usda$chill_indic == "no" & usda$scar_indic == "yes"])),
                                           length(unique(usda$spec[usda$forc_indic == "yes" & usda$chill_indic == "no" & usda$scar_indic == "no"])),
                                           length(unique(usda$spec[usda$forc_indic == "no" & usda$chill_indic == "yes" & usda$scar_indic == "yes"])),
                                           length(unique(usda$spec[usda$forc_indic == "no" & usda$chill_indic == "yes" & usda$scar_indic == "no"])),
                                           length(unique(usda$spec[usda$forc_indic == "no" & usda$chill_indic == "no" & usda$scar_indic == "yes"])),
                                           length(unique(usda$spec[usda$forc_indic == "no" & usda$chill_indic == "no" & usda$scar_indic == "no"]))))

combination_table




#how many species were exposed to chilling only

#how many species were exposed to scarification only



tmp <- aggregate(usda["spec"], by=usda[c("sex","condition")], FUN=length)


tmp <- aggregate(usda["subject"], by=usda[c("sex","condition")], FUN=length)
#new github 


