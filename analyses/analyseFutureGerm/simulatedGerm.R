# Started by Deirdre and Ruben on Oct 1, 2026 at the Fall egret retreat!
# aim of this code is to simulate data to test the niche overlap of seed germination

# Our goal is to simulate germination data for ~100 seeds, for a given grid, the species that occur there, simulate their germination rate 
# using the parameter estimates from Victors model, with the soil temp and soil moisture (Y/N sufficient) from era5Land, assuming each year they die
# basing this on Victors seed germination model

# assumptions: 
# what is the min temp chilling accumulation, 
# what is the minim required threshold for moisture 0.139 m3/m3 maintained for 4 days---tropical dry forests (Dantas et al. Oecologia, 2020); wilting point. 0.05 

rm(list=ls())
options(stringsAsFactors = FALSE)
options(mc.cores = parallel::detectCores())
#rstan_options(auto_write = TRUE)
graphics.off()

setwd("~/Documents/github/egret/analyses")

library(rstan)
library(dplyr)
library(ggplot2)
library(phytools)
library(caper)
library(reshape2)

# 1. get the egret+usda data
d <- read.csv("output/egretUsdaData.csv")
d <- d[complete.cases(d),] 


