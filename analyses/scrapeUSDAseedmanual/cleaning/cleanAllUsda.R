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
library(xlsx)
library(tibble)
library(taxize)

if(length(grep("christophe_rouleau-desrochers", getwd()) > 0)) {
  setwd("~/Documents/github/egret/analyses")
} else if(length(grep("danielbuonaiuto", getwd()) > 0)) {
  setwd("/Users/danielbuonaiuto/Documents/git/egret/analyses/")
} else if(length(grep("lizzie", getwd()) > 0)) {
  setwd("/Users/christophe_rouleau-desrochers/Documents/github/egret/analyses")
} else if(length(grep("sapph", getwd()) > 0)) { # Justin wd
  setwd("/Users/sapph/Documents/ubc things/work/egret/analyses/")
} else if(length(grep("Xiaomao", getwd()) > 0)) {
  setwd("C:/PhD/Project/egret/analyses")
}

d <- read_csv("scrapeUSDAseedmanual/cleaning/germPreCleanedMasterSheet.csv", na = c("", "NA"))

source("scrapeUSDAseedmanual/cleaning/source/cleanAllUsda_JNVER.R") ### this is Justin's cleaning code


source("scrapeUSDAseedmanual/cleaning/source/cleaningUSDA_ForDan.R") ### this is Justin's cleaning code

write.csv(usda_new,"input/usdaGerminationCleaned.csv")
