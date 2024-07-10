####source file for cleaning USDA seed manual 
#made by Dan July 9, 2024

rm(list=ls())
options(stringsAsFactors = FALSE)
options(mc.cores = parallel::detectCores())
#rstan_options(auto_write = TRUE)
graphics.off()

setwd("~/Documents/git/egret/analyses/scrapeUSDAseedmanual/cleaning/")

library(dplyr)
library(reshape2)
library(xlsx)
d <- read_csv("germination_master_spreadsheet.csv", na = c("", "NA"))

source("germination_cleaning.R")

#Note from Justin: the output of this was called germinationCleaned.xlsx, 
#which Selena then went through and manually fixed some issues based on
#weird values from the USDA manual pdf
#This was then saved as "germinationCleaned_official.csv

d<-read.csv("..//output/earlyIterationDataSheets/germinationCleaned_official.csv")


source("germinationCleaningFinal.R")

source("germinationEGRETCorrections.R")
