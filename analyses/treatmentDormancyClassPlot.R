# Started July 10, 2024 by mao

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

library(ggplot2)
# Get the datasets
treatment <- read.csv("output/treatments_manipulated.csv")
bb <- read.csv("output/baskinclean.csv")

# Add dormancy class to treatments_manipulated
treatment_dorm <- merge(treatment, bb, by = "latbi", all.x =T)

print(aggregate(treatment_dorm$n, list(treatment_dorm$Dormancy.Class), FUN=sum)) 
summary(treatment_dorm_NAclean$Dormancy.Class)
treatment_dorm$Dormancy.Class <- as.factor(treatment_dorm$Dormancy.Class)
# Delete rows with NAs in Dormancy.Class
treatment_dorm_NAclean <- treatment_dorm[!is.na(treatment_dorm$Dormancy.Class),]

ggplot(treatment_dorm_NAclean, aes(fill=treatment, y=n, x=Dormancy.Class)) + 
  geom_bar(position="stack", stat="identity") + theme_classic() + ggtitle ("No. of different treatments for each dormancy class")

# Seperate plotting for dormancy classes without PD
treatment_dorm_NoPD <- treatment_dorm_NAclean[treatment_dorm_NAclean$Dormancy.Class == c("MD", "MPD", "ND", "PY", "PYPD"),]

ggplot(treatment_dorm_NoPD, aes(fill=treatment, y=n, x=Dormancy.Class)) + 
  geom_bar(position="stack", stat="identity") + theme_classic() + ggtitle ("No. of different treatments for each dormancy class")

