###Add Chemical Units to origianl file ###
###Started by Chuning ###
###Last edit on Nov. 6th 2026 ###

library(tidyverse)
library(repr)
library(readxl)
library(ggplot2)

#Read the cleaned file including chemical concent and datasetID
original <- read_csv("analyses/effectSize/output/biggestEffectDataGA.csv")
chemical <- original |>
  select(chemical, chemical.concent,figure, datasetID, study, Notes, chemicalConcent)
print(chemical, n=10)

#Read the raw file contain the source of Chemical located in specific figures
raw <- read_csv("analyses/output/egretclean.csv")
figure <- raw|>
  select(datasetID, study, chemical, chemical.concent, figure, Notes, chemicalConcent, chemicalConcentUnit)
figure
