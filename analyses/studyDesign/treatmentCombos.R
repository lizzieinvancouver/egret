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
cols <- c('datasetID', 'study',
          'genus', 'species', 'variety',
          
          "treatment", # I guess it's a summary?
          
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


d$genus <- gsub(" ", "", d$genus, fixed = TRUE)
d$species <- tolower(d$species)
d$genusspecies <- paste0(d$genus, '_', d$species)


baskin <- read.csv('output/baskinclean.csv')
names(baskin)[2] <- 'genusspecies'
# several species have multipe dormancy class
baskin %>%
  dplyr::group_by(genusspecies) %>%
  dplyr::summarise(nclass = n_distinct(Dormancy.Class)) %>%
  dplyr::filter(nclass > 1)
# test <- baskin %>%
#   dplyr::group_by(genusspecies) %>%
#   dplyr::reframe(newclass = paste(Dormancy.Class, collapse=''))


# 1. First look at the number of distinct treatments per study/species
unitreat <- d %>%
  dplyr::group_by(datasetID, study, genusspecies) %>%
  dplyr::reframe(nstorage = n_distinct(c(storageType, storageTemp, storageDuration, storageDetails)),
          nscarif = n_distinct(c(scarifType, scarifTypeGen, scarifTypeSpe, chemicalCor)),
          nstrat = n_distinct(c(chillTemp, chillDuration, chillTempUnc, chillTempCycle, chillLightCycle)),
          ngerm = n_distinct(c(germTempGen, germTemp, germDuration, tempClass, tempDay, tempNight, germDurComment,
                               photoperiodNote, photoperiodCopy, photoperiodCopyDay, photoperiodCopyNight))) 
  dplyr::left_join(baskin[c('Genus_species', 'Dormancy.Class')], by = c('genusspecies' = 'Genus_species'))

all <- ggplot(data = unitreat) +
  facet_wrap(~ nscarif > 1) +
  geom_point(aes(x = ngerm, y = nstrat, size = nstorage, color = nscarif > 1), shape = 1) +
  annotate('rect', xmin = 0, xmax = 30, ymin = 0, ymax = 10, fill = NA, color = 'black') +
  scale_color_manual(values = c('#67903b', "#3B6790")) + 
  theme_bw() +
  labs(x = '', y = 'Number of distinct strat. treatments',
       size = 'Number of distinct\nstorage treatments', color = "Varying scarification") +
  theme(panel.grid.minor = element_blank(),
        strip.text = element_blank())

zoom <- ggplot(data = unitreat %>% dplyr::filter(nstrat <= 10 & ngerm <= 25)) +
  facet_wrap(~ nscarif > 1) +
  geom_point(aes(x = ngerm, y = nstrat, size = nstorage, color = nscarif > 1), shape = 1) +
  scale_color_manual(values = c('#67903b', "#3B6790"), guide = FALSE) + 
  theme_bw() +
  labs(x = 'Number of distinct germ. treatments', y = '',
       size = 'Number of distinct\nstorage treatments', color = "Varying scarification") +
  coord_cartesian(ylim = c(0.5, 10), xlim = c(0.5, 30), expand = FALSE) +
  theme(panel.grid.minor = element_blank(),
        strip.text = element_blank())

all+zoom+plot_layout(ncol=1)
