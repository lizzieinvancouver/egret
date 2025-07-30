# ####source file for cleaning USDA seed manual
# #made by Dan July 9, 2024
### Not able to run as of August 23 2024
rm(list=ls())  
options(stringsAsFactors=FALSE)
library(dplyr)
library(chillR)
library(stringr)
library(ggplot2)
library(tidyverse)
library(readxl)
library(tibble)
library(plyr)
# library(WorldFlora)

if(length(grep("christophe_rouleau-desrochers", getwd()) > 0)) {
  setwd("~/Documents/github/egret/analyses")
} else if(length(grep("danielbuonaiuto", getwd()) > 0)) {
  setwd("/Users/danielbuonaiuto/Documents/git/egret/analyses/")
} else if(length(grep("lizzie", getwd()) > 0)) {
  setwd("/Users/christophe_rouleau-desrochers/Documents/github/egret/analyses")
} else if(length(grep("deirdreloughnan", getwd()) > 0)) {
  setwd("/Users/deirdre/Documents/github/egret/analyses")
} else if(length(grep("sapph", getwd()) > 0)) { # Justin wd
  setwd("/Users/sapph/Documents/ubc things/work/egret/analyses/")
} else if(length(grep("Xiaomao", getwd()) > 0)) {
  setwd("C:/PhD/Project/egret/analyses")
}

# 1. Read in the data
d <- read.csv("scrapeUSDAseedmanual/cleaning/germPreCleanedMasterSheet.csv", na = c("", "NA"))

# 2. Clean miscellaneous
source("scrapeUSDAseedmanual/cleaning/source/cleanGeneralUsda.R")

# 3. Clean species names
source("scrapeUSDAseedmanual/cleaning/source/cleanSpeciesUsda.R")

# 4. Clean seed type
source("scrapeUSDAseedmanual/cleaning/source/cleanSeedTypeUsda.R")

# 5. Clean temperature data
source("scrapeUSDAseedmanual/cleaning/source/cleanTempUsda.R")

# 6. Clean pretreatment
source("scrapeUSDAseedmanual/cleaning/source/cleanPreTrtUsda.R")

# 7. Clean Scarification
source("scrapeUSDAseedmanual/cleaning/source/cleanScarificationUsda.R")

# 8. Clean germination related data
source("scrapeUSDAseedmanual/cleaning/source/cleanGermUsda.R")

# 9. Clean light data
source("scrapeUSDAseedmanual/cleaning/source/cleanLightUsda.R")

# 10. Clean response variables
source("scrapeUSDAseedmanual/cleaning/source/cleanResponseUsda.R")

# 11. Calculate mean of columns with ranges data
source("scrapeUSDAseedmanual/cleaning/source/cleanMeanUsda.R")

# 12. Clean chill data
source("scrapeUSDAseedmanual/cleaning/source/cleanChillUsda.R")

# 13. Change the column names to fit egret dataset
# source("scrapeUSDAseedmanual/cleaning/source/cleanEgretColUsda.R")
d[d==""] <- NA

write.csv(d,"output/usdaGerminationCleaned.csv",row.names = FALSE)
