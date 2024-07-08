rm(list=ls())
options(stringsAsFactors = FALSE)
options(mc.cores = parallel::detectCores())
#rstan_options(auto_write = TRUE)
graphics.off()

library(dplyr)

setwd(dir = "~/Documents/git/egret/analyses/output/")

d<-read.csv("egretclean.csv")
bask<-read.csv("..//input/Baskin_Dormancy_Database.csv")

colnames(bask)<-bask[2,]
bask<-bask[3:14256,]

d$Genus_species<-paste(d$genus,d$species,sep="_")


d<-left_join(d,bask)
colnames(d)
d<-filter(d,!is.na(`Dormancy Class`))

table(d$respvar)
d.perc<-filter(d, respvar %in% c("per.germ"))

makefin<-d.perc %>% group_by(datasetID,study,Genus_species) %>% slice(which.max(response))

counter<-d %>% group_by(`Dormancy Class`, Genus_species) %>% count()

library(ggplot2)
ggplot(d.perc,aes(scarification,response))+geom_boxplot(aes(color=`Dormancy Class`,group=`Dormancy Class`))

