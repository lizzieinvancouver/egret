# Started Jan 16, 2025
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

# 1. Get the cleaned data
d <- read.csv('output/egretclean.csv')
cols <- c('datasetID', 
          'genus', 'species', 'variety',
          # 'storageType', 'storageTemp', "storageDuration",
          # 'chillTemp', 'chillDuration',
          "tempClass", "germTemp", "tempDay", "tempNight", "germDuration",
          'photoperiodCor', 'photoperiodCopyDay', 'photoperiodCopyNight')
d <- d[,cols]

# 2. Look at germination temperature, first
nrow(d) # 30777
# nrow(d[(is.na(d$germTemp)&is.na(d$tempDay)),])
# nrow(d[(is.na(d$germTemp)&!is.na(d$tempDay)),])
# nrow(d[(!is.na(d$germTemp)&is.na(d$tempDay)),])
d <- d[!is.na(d$germTemp), # keep only studies where germ. temp. is somehow specified
       cols]
nrow(d) # 26850

## 3 different cases (a priori)
#  - constant temperature
group1 <- which((d$germTemp==d$tempDay))
length(group1) # 15078
#  - different temp. between day and night + we know daylength
group2 <- which(!is.na(d$tempDay)&!is.na(d$tempNight)&!(is.na(d$photoperiodCopyDay)|d$photoperiodCopyDay=='ambient'))
length(group2) # 8455
#  - different temp. between day and night + we don't know daylength
group3 <- which(!is.na(d$tempDay)&!is.na(d$tempNight)&(is.na(d$photoperiodCopyDay)|d$photoperiodCopyDay=='ambient'))
length(group3) # 3086

## did we miss something? yes, few strange records... to dig?
nrow(d)-(length(group1)+length(group2)+length(group3)) # 231 lines!
index <- 1:nrow(d)
d.issues <- d[which(!(index %in% c(group1, group2, group3))),]

## for the third group, we will need to compute photoperiod at some point
## for this, we need the latitude where the experiment is done (if they are under natural light?)

## for now, we keep only group1 and 2!
dpr <- d[c(group1, group2),]
nrow(dpr) # 23533

## at this point, we should be able to transform the few columns of interest into numeric
## we first need to make a decision about 'ambient' temperature, let's set it to the average day temp...
mean(as.numeric(dpr[dpr$tempDay != 'ambient','tempDay']))
ambient_temperature <- 20 
dpr$ambientGermTemp <- dpr$tempDay == 'ambient' # save the transformation in a boolean
dpr[dpr$tempDay == 'ambient','tempDay'] <- ambient_temperature

## 3 awful lines...
dpr$tempDay <- as.numeric(dpr$tempDay)
dpr$tempNight <- as.numeric(dpr$tempNight)
dpr$photoperiodCopyDay <- as.numeric(dpr$photoperiodCopyDay)

dpr$germTemp <- dpr$tempDay
## for group2, we can take the weighted mean between day/night conditions
dpr[!is.na(dpr$tempNight), 'germTemp'] <-
  (dpr[!is.na(dpr$tempNight), 'tempDay']*dpr[!is.na(dpr$tempNight), 'photoperiodCopyDay'] +
     dpr[!is.na(dpr$tempNight), 'tempNight']*(24-dpr[!is.na(dpr$tempNight), 'photoperiodCopyDay']))/24
     
## summarize by study
dpr.summ <- dpr %>%
  dplyr::group_by(datasetID, ambientGermTemp) %>%
  dplyr::summarise(meanGermTemp = mean(germTemp),
                   sdGermTemp = sd(germTemp),
                   minGermTemp = min(germTemp),
                   minGermTemp = min(germTemp)
  )

germTemp_bystudy <- ggplot(data = dpr.summ, aes(y=datasetID)) +
  facet_wrap(~sdGermTemp==0, scales = 'free_y',
             labeller = as_labeller(c(`FALSE` = "SD > 0", `TRUE` = "SD = 0"))) +
  geom_pointrange(aes(x = meanGermTemp, col = ambientGermTemp,
                      xmin = meanGermTemp-sdGermTemp, xmax = meanGermTemp+sdGermTemp),
                  size = 0.1) +
  scale_color_manual(values = c('#487575', '#B97C7C')) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_blank(),
        legend.position = 'none') +
  labs(caption = 'Reddish points correspond to "ambient" temperatures')

ggsave(germTemp_bystudy, filename = 'figures/germTempRanges_acrossStudies.pdf',
       width = 210, height = 297, units = "mm")
  
