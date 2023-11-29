rm(list=ls()) 
options(stringsAsFactors = FALSE)
options(mc.cores = parallel::detectCores())
setwd("/Users/danielbuonaiuto/Documents/git/egret")

d<-read.csv("analyses/output/ospree8studies.csv")

unique(d$sp.name)
colnames(d)
unique(d$chill.temp)
unique(d$chill.duration)
unique(d$respvar)
res<-d%>% group_by(respvar) %>% count()
unique(d$response)

table(d$datasetID)
table(d$treatment)



library(ggplot2)
chill<-dplyr::filter(d,treatment %in% c("moist.chilling","cold stratification","warm/cold stratification"))

table(chill$respvar)
cml<-dplyr::filter(chill,respvar %in% c("per.germ.cumulative","per.germ"))
cml<-filter(cml,germ.duration>20)
unique(cml$chill.duration)
cml$chilling<-NA
cml$chilling[which(cml$chill.duration=="0 days (warm) + 30 days (cold)" )]<-30
cml$chilling[which(cml$chill.duration=="0 days (warm) + 0 days (cold)" )]<- 0
cml$chilling[which(cml$chill.duration=="0 days (warm) + 60 days (cold)" )]<- 60
cml$chilling[which(cml$chill.duration=="0 days (warm) + 90 days (cold)" )]<- 90
cml$chilling[which(cml$chill.duration=="0 days (warm) + 120 days (cold)" )]<- 120

cml$chilling[which(cml$chill.duration=="30 days (warm) + 0 days (cold)" )]<- 0
cml$chilling[which(cml$chill.duration=="30 days (warm) + 30 days (cold)" )]<- 30
cml$chilling[which(cml$chill.duration=="30 days (warm) + 60 days (cold)" )]<- 60
cml$chilling[which(cml$chill.duration=="30 days (warm) + 90 days (cold)" )]<- 90
cml$chilling[which(cml$chill.duration=="30 days (warm) + 120 days (cold)" )]<- 120

cml$chilling[which(cml$chill.duration=="60 days (warm) + 0 days (cold)" )]<- 0
cml$chilling[which(cml$chill.duration=="60 days (warm) + 30 days (cold)" )]<- 30
cml$chilling[which(cml$chill.duration=="60 days (warm) + 60 days (cold)" )]<- 60
cml$chilling[which(cml$chill.duration=="60 days (warm) + 90 days (cold)" )]<- 90
cml$chilling[which(cml$chill.duration=="60 days (warm) + 120 days (cold)" )]<- 120

cml$chilling[which(cml$chill.duration==63 )]<- 63
cml$chilling[which(cml$chill.duration==21 )]<- 21
cml$chilling[which(cml$chill.duration==35 )]<- 35

unique(cml$germ.temp)

cml$forcing<-NA
cml$forcing[which(cml$germ.temp=="20/30" )]<- 25
cml$forcing[which(cml$germ.temp=="15/5" )]<- 10
cml$forcing[which(cml$germ.temp=="20°C (6h dark) + 25°C (18h light)" )]<- 24
cml$forcing[which(cml$germ.temp== 15 )]<- 15

unique(cml$soaked.in)
#cml<-dplyr::filter(cml, soaked.in!="ethephon")
ggplot(cml,aes(chilling,response))+stat_summary(aes(color=genus,shape=as.factor(forcing)))

timy<-filter(d,respvar %in% c("mgt","log10(mgt)"))
timy$resp.use<-ifelse(timy$respvar!="log10(mgt)",log10(timy$response),timy$response)

unique(timy$chill.duration)
unique(timy$chill.temp)
unique(timy$germ.temp)
ggplot(timy,aes(germ.temp,response))+stat_summary(aes(color=genus))
