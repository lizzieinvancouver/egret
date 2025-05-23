# Started Jan 21, 2025
# by Victor

library(dplyr) # oops
library(ggplot2)

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

if(length(grep("deirdre", getwd()) > 0)) {
  setwd("~/Documents/github/egret/analyses")
} else if(length(grep("lizzie", getwd()) > 0)) {
  setwd("/Users/lizzie/Documents/git/projects/egret/analyses")
} else if(length(grep("sapph", getwd()) > 0)) {
  setwd("/Users/sapph/Documents/ubc things/work/egret/analyses")
} else if(length(grep("danielbuonaiuto", getwd()) > 0)) {
  setwd("/Users/danielbuonaiuto/Documents/git/egret/analyses")
} else if(length(grep("Xiaomao", getwd()) > 0)) {
  setwd("C:/PhD/Project/egret/analyses")
} else if(length(grep("britanywuuu", getwd()) > 0)) {
  setwd("/Documents/ubc/year5/TemporalEcologyLab/egret/analyses")
} else if(length(grep("Ken", getwd())) > 0){
  setwd("/Users/Ken Michiko Samson/Documents/Temporal Ecology Lab/egret/analyses")
} else if(length(grep("christophe_rouleau-desrochers", getwd())) > 0){
  setwd("/Users/christophe_rouleau-desrochers/Documents/github/egret/analyses")
} else if(length(grep("victor", getwd())) > 0){
  setwd('~/projects/egret/analyses')
} 

# load some functions
source("studyDesign/functions/timeline_plot.R")

# 0. Get the cleaned data
d <- read.csv('output/egretclean.csv')
cols <- c('datasetID', 
          'genus', 'species', 'variety',
          
          "treatmentFixed", # I guess it's a summary?
          
          # STORAGE-RELATED
          'storageType', 'storageTemp', "storageDuration", "storageDetails",
          
          # SCARIFICATION-RELATED
          "scarifType", "scarifTypeGen", "scarifTypeSpe", 
          "chemicalCor", # is the chemical treatment always related to scarification?
          
          # SOAKING-RELATED (IMBIBITION)
          "soaking", "soaked.in", "soaking.duration", # I guess it's not cleaned?
          
          # STRATIFICATION-RELATED
          "chillTemp", "chillDuration", "chillTempUnc", "chillTempCycle", "chillLightCycle",
          
          # GERMINATION-RELATED
          "germTempGen", "germTemp", "germDuration", "tempClass", "tempDay", "tempNight", "germDurComment",
          "photoperiodNote", "photoperiodCopy", "photoperiodCopyDay", "photoperiodCopyNight", # why variables such as tempDay or photoperiod do not have the prefix germ?
          
          # UNSURE
          "dormancyTemp", "dormancyDuration"
          
          )
d <- d[,cols]


## 1. Wandering across variables ## ----
# 1.1. Look at treatment summary ----

## 1.1.1. Light intensity
unique(d[grepl("intensity", d$treatmentFixed), "treatmentFixed"]) 
## Notes: 
## - case typo? "Light intensity", "light intensity"
## - is the % of intensity saved in another column? e.g. 25%, 50%?

## 1.1.2. Light-related
unique(d[grepl("light", d$treatmentFixed, ignore.case=TRUE), "treatmentFixed"]) 
## Notes: ok, there is a lot here, I guess we're not supposed to use this column?

## 1.1.3. Dormancy (looking at this, as I'm not sure what it is)
unique(d[grepl("dormancy", d$treatmentFixed, ignore.case=TRUE), "treatmentFixed"]) 
unique(d[grepl("dormancy", d$treatmentFixed, ignore.case=TRUE), c("datasetID", "treatmentFixed", "dormancyTemp", "dormancyDuration")])
## Notes: so, the treatment is "dormancy something" but there is nothing in the dormancy-related variables?

# 1.2. Look at storage-related variables ----

## 1.2.1. Cold storage
length(unique(d[grepl("cold", d$storageType), "datasetID"])) # 29 studies
table(d[grepl("cold", d$storageType), "storageDuration"], useNA = "always")
nrow(d[grepl("cold", d$storageType) & is.na(d$storageDuration), ])/nrow(d[grepl("cold", d$storageType), ]) # 58% (of the rows) have no duration specified

## 1.2.2. Dry storage
length(unique(d[grepl("dry", d$storageType), "datasetID"])) # 90 studies
table(d[grepl("dry", d$storageType), "storageDuration"], useNA = "always")
nrow(d[grepl("dry", d$storageType) & is.na(d$storageDuration), ])/nrow(d[grepl("dry", d$storageType), ]) # 38% (of the rows) have no duration specified


## 2. Simplifying variables ## ----
# 2.1.  Storage-related variables ----

## 2.1.1. Storage humidity ----
d %>%
  dplyr::select('datasetID', 'storageType', "storageDuration") %>%
  unique() %>%
  dplyr::mutate(storageHumidity = if_else(grepl("dry", storageType), "dry", if_else(grepl("moist", storageType), "moist", NA))) %>%
  dplyr::filter(is.na(storageHumidity)) %>%
  dplyr::select('storageType') %>%
  unique()
## How can we further simplify?
## We could first hypothesize that {room, ambient} are ~dry? 
d %>%
  dplyr::select('datasetID', 'storageType', "storageDuration") %>%
  unique() %>%
  dplyr::mutate(storageHumidity = if_else(grepl("dry", storageType), "dry", if_else(grepl("moist", storageType), "moist", if_else(grepl("ambient|room", storageType),"dry", NA)))) %>% # first simplification
  dplyr::filter(is.na(storageHumidity)) %>%
  dplyr::select('storageType') %>%
  unique()
## Then, we still have three categories
## A - "storage container"-related: {naked storage, paper, airtight, plastic} => we could hypothesize that ~dry? 
## B - humidity not specified: {cold, dark, controlled environment, airflow, sand, warm} => look more into details:
unique(d[d$storageType %in% c('cold', 'dark', 'controlled environment', 'airflow', 'sand', 'warm'), c('storageType', 'storageTemp', 'storageDuration', 'storageDetails')])
##     => "liquid nitrogen" is made of dry nitrogen gas, so ~ dry?
##     => "dark/paper" => must be ~ dry
##     => "oven" => ~ dry
## C - natural-ish: ground at 3mm, natural environment => hard to tell anything?
d %>%
  dplyr::select('datasetID', 'storageType', 'storageDuration', 'storageDetails') %>%
  unique() %>%
  dplyr::mutate(storageHumidity = if_else(grepl("dry", storageType), "dry", if_else(grepl("moist", storageType), "moist", if_else(grepl("ambient|room", storageType),"dry", NA)))) %>% # first simplification
  dplyr::mutate(storageHumidity = if_else(!is.na(storageHumidity),storageHumidity,if_else(grepl("naked|paper|airtight|plastic", storageType),"dry", NA))) %>% # second simplification (category A)
  dplyr::mutate(storageHumidity = if_else(!is.na(storageHumidity),storageHumidity,if_else(grepl("nitrogen|paper|oven", storageDetails),"dry", NA))) %>% # third simplification (category B)
  dplyr::filter(is.na(storageHumidity)) %>%
  dplyr::select('datasetID', 'storageType') %>% 
  unique() %>% 
  dplyr::group_by(storageType) %>%
  summarise(n_distinct(datasetID))
## Most of the remaining studies do not specify storage conditions at all, BUT sometimes they do specify a storage duration!
## Have a quick look:
# dataplot <-
#   d %>%
#   dplyr::select('datasetID', 'storageType', 'storageDuration', 'storageTemp', 'storageDetails') %>%
#   unique() %>%
#   dplyr::mutate(storageHumidity = if_else(grepl("dry", storageType), "dry", if_else(grepl("moist", storageType), "moist", if_else(grepl("ambient|room", storageType),"dry", NA)))) %>% # first simplification
#   dplyr::mutate(storageHumidity = if_else(!is.na(storageHumidity),storageHumidity,if_else(grepl("naked|paper|airtight|plastic", storageType),"dry", NA))) %>% # second simplification (category A)
#   dplyr::mutate(storageHumidity = if_else(!is.na(storageHumidity),storageHumidity,if_else(grepl("nitrogen|paper|oven", storageDetails),"dry", NA))) %>%
#   dplyr::filter(!is.na(storageDuration) & !is.na(storageType) & !grepl("then", storageDuration)) %>% # quick test where we remove the sample with duration "X then Y" 
#   dplyr::mutate(storageDuration = as.numeric(storageDuration)) %>%
#   dplyr::group_by(datasetID) %>%
#   dplyr::mutate(uniqueID = paste0(datasetID, "_", dplyr::row_number()))
# ggplot() +
#   geom_segment(
#     data = dataplot %>% dplyr::mutate(storageDuration = if_else(storageDuration > 300, 300, storageDuration)),
#     aes(y = uniqueID, yend = uniqueID, x = -storageDuration, xend = 0, 
#         color = storageHumidity), 
#     linewidth = 0.5) +
#   geom_segment(
#     data = dataplot %>% dplyr::filter(storageDuration > 300),
#     aes(y = uniqueID, yend = uniqueID, x = -storageDuration, xend = -300,  color = storageHumidity), 
#     linewidth = 0.5, linetype = "dotted") +
#   coord_cartesian(xlim = c(-350,0)) +
#   scale_x_continuous(breaks = seq(0, -300, -60)) +
#   theme_minimal() + 
#   theme(axis.text.y = element_blank(), panel.grid = element_blank())

## 2.1.2. Storage temperature ----
d %>%
  dplyr::select('datasetID', 'storageType', 'storageDuration', 'storageTemp', 'storageDetails') %>%
  unique() %>%
  dplyr::mutate(storageTempSimplified = if_else(grepl("cold", storageType), "cold", if_else(grepl("warm", storageType), "warm", if_else(grepl("room|ambient", storageType), "ambient", NA)))) %>%
  dplyr::filter(is.na(storageTempSimplified)) %>%
  dplyr::select('storageType', 'storageTemp') %>%
  unique()
## How can we further simplify? Lots of studies have a storageTemp (even though the temperature conditions (warm/cold) is not specified in storageType !)
## But what's cold, what's warm, what's ambient?
d %>%
  dplyr::select('datasetID', 'storageType', 'storageDuration', 'storageTemp', 'storageDetails') %>%
  unique() %>%
  dplyr::mutate(storageTempSimplified = if_else(grepl("cold", storageType), "cold", if_else(grepl("warm", storageType), "warm", if_else(grepl("room|ambient", storageType), "ambient", NA)))) %>%
  dplyr::filter(!is.na(storageTempSimplified)) %>%
  dplyr::select('storageTempSimplified', 'storageTemp') %>%
  dplyr::filter(!grepl("multiple", storageTemp)) %>% 
  dplyr::group_by(storageTempSimplified) %>%
  summarise(minTemp = min(storageTemp, na.rm = TRUE), maxTemp = max(storageTemp, na.rm = TRUE))
## From the studies where we have both the storageTemp and the temperature condition associated (warm/cold/ambient), we get:
## cold = [-10,7], warm = 65, ambient = [17,26]
## We could set a rule based on these? cold < 10, warm > 30 ?
cold_ths <- 10
warm_ths <- 30
d %>%
  dplyr::select('datasetID', 'storageType', 'storageDuration', 'storageTemp', 'storageDetails') %>%
  unique() %>%
  dplyr::mutate(storageTempSimplified = if_else(grepl("cold", storageType), "cold", if_else(grepl("warm", storageType), "warm", if_else(grepl("room|ambient", storageType), "ambient", NA)))) %>% 
  dplyr::filter(!grepl("then|multiple", storageTemp)) %>% # quick test where we remove the sample with temp "X then Y" or "multiple
  dplyr::mutate(storageTemp = as.numeric(storageTemp)) %>%
  dplyr::mutate(storageTempSimplified = if_else(!is.na(storageTempSimplified), storageTempSimplified, if_else(storageTemp > warm_ths,"warm", if_else(storageTemp < cold_ths,"cold", if_else(is.na(storageTemp), NA, "ambient"))))) %>%
  dplyr::filter(is.na(storageTempSimplified)) %>%
  dplyr::select('storageType', 'storageTemp', 'storageDetails') %>%
  unique()
## Let's visualize!
dataplot <-
  d %>%
  dplyr::select('datasetID', 'storageType', 'storageDuration', 'storageTemp', 'storageDetails') %>%
  unique() %>%
  dplyr::mutate(storageTempSimplified = if_else(grepl("cold", storageType), "cold", if_else(grepl("warm", storageType), "warm", if_else(grepl("room|ambient", storageType), "ambient", NA)))) %>% 
  dplyr::filter(!grepl("then|multiple", storageTemp)) %>% # quick test where we remove the sample with temp "X then Y" or "multiple
  dplyr::mutate(storageTemp = as.numeric(storageTemp)) %>%
  dplyr::mutate(storageTempSimplified = if_else(!is.na(storageTempSimplified), storageTempSimplified, if_else(storageTemp > warm_ths,"warm", if_else(storageTemp < cold_ths,"cold", if_else(is.na(storageTemp), NA, "ambient"))))) %>%
  dplyr::filter(!grepl("then", storageDuration)) %>% # quick test where we remove the sample with duration "X then Y" 
  dplyr::mutate(storageDuration = as.numeric(storageDuration)) %>%
  dplyr::group_by(datasetID) %>%
  dplyr::mutate(uniqueID = paste0(datasetID, "_", dplyr::row_number()))
storageTimeline <- ggplot() +
  # storage duration is unknown
  geom_point(
    data = dataplot %>% dplyr::filter(is.na(storageDuration)),
    aes(y = uniqueID, x = -1, color = storageTempSimplified),
    size = 0.1) +
  geom_text(
    data = dataplot %>% dplyr::filter(is.na(storageDuration)),
    aes(y = uniqueID, x = -1, color = storageTempSimplified, label = "?"),
    size = 0.6, color = "white") + 
  # storage duration is 0
  geom_text(
    data = dataplot %>% dplyr::filter(storageDuration == 0),
    aes(y = uniqueID, x = -1, color = storageTempSimplified, label = "0"),
    size = 0.6) +
  # storage duration is known...
  geom_segment(
    data = dataplot %>% dplyr::filter(!is.na(storageDuration)) %>% dplyr::mutate(storageDuration = if_else(storageDuration > 300, 300, storageDuration)),
    aes(y = uniqueID, yend = uniqueID, x = -storageDuration, xend = 0, 
        color = storageTempSimplified), 
    linewidth = 0.55) +
  # ... and longer than 300 days
  geom_segment(
    data = dataplot %>% dplyr::filter(storageDuration > 300),
    aes(y = uniqueID, yend = uniqueID, x = -360, xend = -300,  color = storageTempSimplified), 
    linewidth = 0.4, linetype = "dotted") +
  scale_color_manual(values = c("#3B6790", "#67903b", "#EFB036"), breaks = c("cold", "ambient", "warm"), na.value = "grey80") + 
  coord_cartesian(xlim = c(-350,0)) +
  scale_x_continuous(breaks = seq(0, -270, -90), expand = c(0,2.5),
                     labels = c("0", "3", "6", "9"), name = "Storage duration (months)") +
  scale_y_discrete(expand = c(0.001,-00), position = "right") +
  theme_minimal() + 
  theme(axis.text.y = element_blank(), 
        axis.title.y = element_blank(),
        panel.grid = element_blank(), axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(linewidth= 0.1), axis.line.x = element_line(linewidth= 0.2),
        legend.position = 'top', legend.title = element_blank(),
        legend.text = element_text(size = 3), 
        axis.title.x = element_text(size = 3), axis.text.x = element_text(size = 3),
        legend.key.width =  unit(0.2, 'cm'), legend.margin = margin(b = -15, t = -5),
        plot.margin = margin(t=0, r = 2, b = 1))
# ggsave(storageTimeline, filename = 'figures/storageTimeline_temperature.pdf',
#        width = 40, height = 350, units = "mm")
## Notes: 
## - only two warm storages, including alptekin02 at 105°C (to check?) and 
## - some studies have a storageDuration = 0, but still a lot of details regarding storage conditions? seems odd

# 2.2. Scarification-related variables ----
d %>%
  dplyr::select('datasetID', 'treatmentFixed', 'scarifType', 'scarifTypeGen', 'scarifTypeSpe', 'chemicalCor') %>%
  unique()
## Notes: several issues:
d %>%
  dplyr::filter(datasetID %in% c("prknova15", "airi09", "langlois17", "veiga-barbosa14")) %>%
  dplyr::select('datasetID', 'treatmentFixed', 'scarifType', 'scarifTypeGen', 'scarifTypeSpe', 'chemicalCor') %>%
  unique()
## - prknova15: seed coat was removed, but no "mechanical" scarification specified
## - veiga-barbosa14: scarifTypeGen = "Demucilagation", should be "mechanical"?
## - airi09: scarifType = cold?! I think it's rather stratification
## - langlois17: scarifTypeGen = "mechanical with 220 grit sandpaper", should be "mechanical"?
d[d$datasetID %in% c("prknova15") & d$treatmentFixed %in% c("seed coat removal, soaking in water"), "scarifType"] <- d[d$datasetID %in% c("prknova15") & d$treatmentFixed %in% c("seed coat removal, soaking in water"), "scarifTypeGen"] <- "mechanical"
d[d$datasetID %in% c("veiga-barbosa14") & d$scarifTypeGen %in% c("Demucilagation"), "scarifTypeGen"] <- "mechanical"
d[d$datasetID %in% c("airi09") & d$scarifType %in% c("cold"),c("scarifTypeGen")]  <- NA
d[d$datasetID %in% c("airi09") & d$scarifType %in% c("cold"),c("scarifType")] <- NA
d[d$datasetID %in% c("langlois17") & d$scarifTypeGen %in% c("mechanical with 220 grit sandpaper"), "scarifTypeGen"] <- "mechanical"
d %>%
  dplyr::select('datasetID', 'scarifTypeGen', 'scarifTypeSpe') %>%
  unique() %>%
  dplyr::group_by(scarifTypeGen) %>%
  summarise(n_distinct(datasetID))
## Let's simplify! 
## scarifTypeGen = {"soaking - 70°C hot water", "soaking - 90°C hot water", "soaking"} => "hot water"!
## "partially scarified" => mechanical
d %>%
  dplyr::select('datasetID', 'scarifTypeGen') %>%
  dplyr::mutate(scarifTypeGenSimplified = if_else(grepl("soaking", scarifTypeGen), "hot water", scarifTypeGen)) %>%
  dplyr::mutate(scarifTypeGenSimplified = if_else(scarifTypeGen %in% c("partially scarified"), "mechanical", scarifTypeGenSimplified)) %>% 
  unique() %>%
  dplyr::group_by(scarifTypeGenSimplified) %>%
  summarise(n_distinct(datasetID))


# 2.3. Soaking-related variables (not cleaned?) ----
d %>%
  dplyr::select('datasetID', "soaking", "soaked.in", "soaking.duration") %>%
  unique()
## Notes: 
## - soaking in hot water is sometimes scarification, sometimes imbibition?
## - soaking still looks messy...
# 2.4. Stratification-related variables ----
d %>%
  dplyr::select('datasetID', "treatmentFixed", "chillTemp", "chillDuration", "chillTempUnc", "chillTempCycle", "chillLightCycle") %>%
  unique()
## Notes: why is it called chill?? There are some "heat stratification"
## Quite messy, a simple first exploration could be just to look at chillDuration (that I'm going to rename stratDuration for now)
## Let's define a function to summarize the duration (and in particular to transform all the "X then Y")
sum_duration <- function(x, na.repl = 0){
  xplus <- sapply(strsplit(x, 'then|-'), function(x){
    x <- ifelse(x %in% c(" NA", " NA ", "NA "), na.repl, x)
    paste(x, collapse = '+')}) # transform "X then Y" to "X + Y"
  xnum <- eval(parse(text=xplus)) # compute the sum! i.e. we evaluate the "X + Y"
  return(as.numeric(xnum))
}
sum_duration("15 then NA then 78 then NA")
d %>%
  dplyr::select('datasetID', "chillDuration") %>%
  dplyr::filter(!(chillDuration %in% c("ave"))) %>%
  dplyr::mutate(stratDuration = mapply(sum_duration, chillDuration))
## Let's now look at both chillTemp and chillDuration, and make a function that will simplify plotting things
## The aim is to transform the "then", "and" into different parts
vectorize_chilling <- function(temp, period, duration, na.repl = NA, period.default = 12){
  perioddec <- sapply(strsplit(period, 'then'), function(x){
    x <- ifelse(x %in% c(" NA", " NA ", "NA "), na.repl, x)})
  tempdec <- sapply(strsplit(temp, 'then'), function(x){
    x <- ifelse(x %in% c(" NA", " NA ", "NA "), na.repl, x)})
  durdec <- sapply(strsplit(duration, 'then'), function(x){
    x <- ifelse(x %in% c(" NA", " NA ", "NA "), na.repl, x)})
  ## if there is an "and", that's mean there is an alternating temp (day/night)
  ## but what if we do not know the period?
  for(i in 1:length(tempdec)){
    if(grepl("and", tempdec[i])){
      if(is.na(as.numeric(perioddec[i]))){
        perioddec[i] <- period.default
        cat("Using default period!\n")}
      tempdec[i] <- sum(as.numeric(unlist(strsplit(tempdec[i], 'and')))*c(as.numeric(perioddec[i]), 24-as.numeric(perioddec[i])))/24
    }
  }
  return(list(temp = as.numeric(tempdec), duration = as.numeric(durdec)))
}
## Test of the function
# test <- d %>%
#   dplyr::select('datasetID', "chillDuration", "chillTemp", "chillLightCycle") %>%
#   unique() 
# test$chillTempPer4 <- test$chillTempPer3 <- test$chillTempPer2 <- test$chillTempPer1 <- NA
# test$chillDurPer4 <- test$chillDurPer3 <- test$chillDurPer2 <- test$chillDurPer1 <- NA
# for(i in 1:nrow(test)){
#   vectChilling <- vectorize_chilling(test[i, "chillTemp"], test[i, "chillLightCycle"], test[i, "chillDuration"])
#   test[i, paste0("chillTempPer",1:4)] <- vectChilling$temp[1:4]
#   test[i, paste0("chillDurPer",1:4)] <- vectChilling$duration[1:4]
# }

## 3. Let's visualize! ----
dataplot <-
  d %>%
  dplyr::select('datasetID', 'storageType', 'storageDuration', 'storageTemp', 'scarifTypeGen', "chillTemp", "chillDuration", "chillLightCycle") %>%
  unique() %>%
  # storage-related part
  dplyr::mutate(storageTempSimplified = if_else(grepl("cold", storageType), "cold", if_else(grepl("warm", storageType), "warm", if_else(grepl("room|ambient", storageType), "ambient", NA)))) %>% 
  dplyr::filter(!grepl("then|multiple", storageTemp)) %>% # quick test where we remove the sample with temp "X then Y" or "multiple
  dplyr::mutate(storageTemp = as.numeric(storageTemp)) %>%
  dplyr::mutate(storageTempSimplified = if_else(!is.na(storageTempSimplified), storageTempSimplified, if_else(storageTemp > warm_ths,"warm", if_else(storageTemp < cold_ths,"cold", if_else(is.na(storageTemp), NA, "ambient"))))) %>%
  dplyr::filter(!grepl("then", storageDuration)) %>% # quick test where we remove the sample with duration "X then Y" 
  dplyr::mutate(storageDuration = as.numeric(storageDuration)) %>% 
  # scarification-related part
  dplyr::mutate(scarifTypeGenSimplified = if_else(grepl("soaking", scarifTypeGen), "hot water", scarifTypeGen)) %>%
  dplyr::mutate(scarifTypeGenSimplified = if_else(scarifTypeGen %in% c("partially scarified"), "mechanical", scarifTypeGenSimplified)) %>% 
  # stratification-related part
  dplyr::filter(!(chillDuration %in% c("ave"))) %>%
  dplyr::mutate(stratDuration = mapply(sum_duration, chillDuration)) %>%
  
  dplyr::group_by(datasetID) %>%
  dplyr::mutate(uniqueID = paste0(datasetID, "_", dplyr::row_number())) %>%
  ungroup() %>% as.data.frame()

# transform stratification temperatures/periods
dataplot$chillTempPer4 <- dataplot$chillTempPer3 <- dataplot$chillTempPer2 <- dataplot$chillTempPer1 <- NA
dataplot$chillDurPer4 <- dataplot$chillDurPer3 <- dataplot$chillDurPer2 <- dataplot$chillDurPer1 <- NA
for(i in 1:nrow(dataplot)){
  vectChilling <- vectorize_chilling(dataplot[i, "chillTemp"], dataplot[i, "chillLightCycle"], dataplot[i, "chillDuration"])
  dataplot[i, paste0("chillTempPer",1:4)] <- vectChilling$temp[1:4]
  dataplot[i, paste0("chillDurPer",1:4)] <- vectChilling$duration[1:4]
}

studyTimeline <- timeline_plot(dataplot, add.names = TRUE)

ggsave(studyTimeline, filename = 'figures/studyTimeline.pdf',
       width = 80, height = 1200, units = "mm")


