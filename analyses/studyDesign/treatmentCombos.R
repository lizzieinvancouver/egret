# Started Feb 10, 2025
# by Victor

library(dplyr) # oops
library(ggplot2)
library(ggsankey) # devtools::install_github("davidsjoberg/ggsankey")
library(patchwork)

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



# 0. Get the cleaned data
d <- read.csv('output/egretclean.csv')
treats <- c('datasetID', 'study',
          'genus', 'species', 'variety',
          
          "treatment", # I guess it's a summary?
          
          # STORAGE-RELATED
          'storageType', 'storageTemp', "storageDuration", "storageDetails",
          
          # SCARIFICATION-RELATED
          "scarifType", "scarifTypeGen", "scarifTypeSpe", 
          
          # CHEMICAL-RELATED (warning: concentration not cleaned)
          "chemicalCor", "chemical.concent",
          
          # SOAKING-RELATED (IMBIBITION)
          "soaking", "soaked.in", "soaking.duration", # I guess it's not cleaned?
          
          # STRATIFICATION-RELATED
          "chillTemp", "chillDuration", "chillTempUnc", "chillTempCycle", "chillLightCycle",
          
          # STRAT + STORAGE
          "dormancyTemp", "dormancyDuration",
          
          # GERMINATION-RELATED
          "germTempGen", "germTemp", "germDuration", "tempClass", "tempDay", "tempNight",
          "germPhotoperiod", "germPhotoperiodDay", "germPhotoperiodNight", 
          
          # MISC (e.g. sowing depth)
          "other.treatment"
          
)

# RESPONSE
resp <- c("responseVar", "responseValue")

# unique treatments per study
dtreat <- unique(d[,treats])
dtreat$uniqueID <- 1:nrow(dtreat)

d <- unique(d[, c(treats, resp)])
d <- merge(x = d, y = dtreat, by = treats, all = TRUE)
d$genusspecies <- paste0(d$genus, '_', d$species)

# here we can set which response variable we want to prioritize (if we have multiple options...)
priority <- c("percent.germ") # just as an example
d <- d[order(d$uniqueID, match(d$responseVar, priority)), ]
d <- d[!duplicated(d$uniqueID), ]


# Baskin database
baskin <- read.csv('output/baskinclean.csv')
names(baskin)[2] <- 'genusspecies'
baskin <- baskin[c('Corrected.Genus', 'genusspecies', 'Dormancy.Class')]
# several species have multipe dormancy class
baskin %>%
  dplyr::group_by(genusspecies) %>%
  dplyr::summarise(nclass = n_distinct(Dormancy.Class)) %>%
  dplyr::filter(nclass > 1)

baskin.simplified <- baskin %>%
  dplyr::group_by(genusspecies) %>%
  dplyr::reframe(newclass = paste(Dormancy.Class, collapse=''),
                 physicaldorm = grepl("PY", newclass)) %>%
  dplyr::select(genusspecies, newclass, physicaldorm)

baskin.genus <- baskin %>%
  dplyr::add_count(Corrected.Genus, Dormancy.Class) %>%
  dplyr::group_by(Corrected.Genus) %>%
  summarise(newclass = Dormancy.Class[n == max(n)][1]) %>% # most frequent per Genus
  mutate(physicaldorm = grepl("PY", newclass)) %>%
  dplyr::select(Corrected.Genus, physicaldorm)
names(baskin.genus) <- c('genus', 'genusphysicaldorm')


# 1. First look at scarication treatment
unitreat <- d %>%
  dplyr::group_by(datasetID, study, genus, genusspecies) %>%
  dplyr::reframe(
    nstorage = n_distinct(storageType, storageTemp, storageDuration, storageDetails),
    nscarif = n_distinct(scarifType, scarifTypeGen, scarifTypeSpe),
    nchem = n_distinct(chemicalCor),
    nstrat = n_distinct(chillTemp, chillDuration, chillTempUnc, chillTempCycle, chillLightCycle),
    ngerm = n_distinct(germTempGen, germTemp, germDuration, tempClass, tempDay, tempNight,
                               germPhotoperiod, germPhotoperiodDay, germPhotoperiodNight)) %>%
  # merge with Baskin dormancy classes
  dplyr::left_join(baskin.simplified, by = c('genusspecies')) %>%
  # if we don't have the info for the species, what is the most frequent in the genus?
  dplyr::left_join(baskin.genus, by="genus") %>%
  dplyr::mutate(physicalDormExtrapolate=if_else(is.na(physicaldorm), genusphysicaldorm, physicaldorm)) %>%
  as.data.frame()

facetlabels = labeller(
  physicalDormExtrapolate = 
    c("FALSE" = "No physical dorm.","TRUE" = "Some physical dorm."))

all <- ggplot(data = unitreat) +
  facet_wrap(~ physicalDormExtrapolate, labeller = facetlabels) +
  geom_point(aes(x = ngerm, y = nstrat, size = nstorage, color = nscarif > 1), shape = 1) +
  annotate('rect', xmin = -3, xmax = 30, ymin = -0.5, ymax = 10.5, fill = NA, color = 'black') +
  scale_color_manual(values = c('#67903b', "#903b67")) + 
  theme_bw() +
  labs(x = '', y = 'No of distinct strat. treatments',
       size = 'No. of distinct\nstorage treatments', color = "Varying scarification") +
  theme(panel.grid.minor = element_blank(),
        strip.background = element_blank())

zoom <- ggplot(data = unitreat %>% dplyr::filter(nstrat <= 10 & ngerm <= 25)) +
  facet_wrap(~ physicalDormExtrapolate) +
  geom_point(aes(x = ngerm, y = nstrat, size = nstorage, color = nscarif > 1), shape = 1) +
  scale_color_manual(values = c('#67903b', "#903b67"), guide = FALSE) + 
  theme_bw() +
  labs(x = 'No. of distinct germ. treatments', y = '',
       size = 'No.of distinct\nstorage treatments', color = "Varying scarification") +
  coord_cartesian(ylim = c(0.5, 10.5), xlim = c(0.5, 30), expand = FALSE) +
  theme(panel.grid.minor = element_blank(),
        strip.text = element_blank())

ggsave(all+zoom+plot_layout(ncol=1), filename = 'figures/studyDesign/scarificationDormancy.pdf',
       width = 300, height = 150, units = "mm")

scarifStudies <- unitreat[unitreat$nscarif > 1, c('datasetID', 'study')]

scarifd <- d %>%
  dplyr::filter(datasetID %in% scarifStudies$datasetID & study %in% scarifStudies$study) %>%
  dplyr::group_by(datasetID, study, genusspecies) %>%
  dplyr::reframe(nstorage = n_distinct(storageType, storageTemp, storageDuration, storageDetails),
                 nscarif = n_distinct(scarifType, scarifTypeGen, scarifTypeSpe),
                 nstrat = n_distinct(chillTemp, chillDuration, chillTempUnc, chillTempCycle, chillLightCycle),
                 ngerm = n_distinct(germTempGen, germDuration))
  
