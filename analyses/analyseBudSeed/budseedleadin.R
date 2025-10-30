#### ospree + egret = Luv4eva
###started by Dan July 10, 2024
### goal run the ospree model with overlapping egret species and extract posteriors

rm(list=ls())
options(stringsAsFactors = FALSE)
options(mc.cores = parallel::detectCores())
#rstan_options(auto_write = TRUE)
graphics.off()

if(length(grep("deirdreloughnan", getwd()) > 0)) {
  setwd("~/Documents/github/egret/analyses")
} else if(length(grep("lizzie", getwd()) > 0)) {
  setwd("/Users/lizzie/Documents/git/projects/egret/analyses")
} else if(length(grep("sapph", getwd()) > 0)) {
  setwd("/Users/sapph/Documents/ubc things/work/egret/analyses")
} else if(length(grep("danielbuonaiuto", getwd()) > 0)) {
  setwd("/Users/dbuona/Documents/git/egret/analyses/")
} else if(length(grep("Xiaomao", getwd()) > 0)) {
  setwd("C:/PhD/Project/egret/analyses")
}
library(rstan)
library(dplyr)
library(ggplot2)
library(phytools)
library(caper)
library(pez)

###read in ospree
osp<-read.csv("input/ospreeforegret.csv")


phylo <- read.tree("input/ospreeforegret.tre")
#todrop<-setdiff(egr$latbi,osp$spps)
#phyloegr<-drop.tip(phylo,todrop)


#plot(phylo, cex=0.7)
VCVPHY <- vcv.phylo(phylo,corr=TRUE)
nspecies <- max(osp$sppnum)





fit <- stan("stan/uber_threeslopeintercept_modified_cholesky_updatedpriors.stan",
            data=list(N=nrow(osp),
                      n_sp=nspecies,
                      sp=osp$sppnum,
                      x1=osp$force.z,
                      x2 = osp$chill.z,
                      x3=osp$photo.z,
                      y=osp$resp,
                      Vphy=vcv(phylo, corr = TRUE)),
            iter = 4000,
            warmup = 2000, # half the iter as warmp is default, but leaving in case we want to change
            chains = 4,
            seed = 117 
)
library(tidybayes)
parameters(fit)
posterior_draws <- fit %>%
  spread_draws( b_chill, r_spnum[spnum, term])

##next steps
###run phylogeny model on ospree and extract posteriors

#####run phylogeny model on egret+ 

egr<-read.csv("input/usdaGerminationCleaned.csv")
table(egr$responseVar)
egr<-dplyr::filter(egr,responseVar=="perc.standard")
sort(unique(egr$latbi))


###
class(egr$tempDayAvg)
class(egr$responseValueAvg)
egr$chillDurationAvg<-as.numeric(egr$chillDurationAvg)

if(FALSE){#there are only 4 rows of data with Min and max values in predictor and response
egr.mm<-filter(egr,!is.na(egr$responseValueMin) & !is.na(egr$responseValueMax) & !is.na(egr$tempDayMin) & !is.na(egr$tempDayMax))
egr.mm2<-filter(egr,!is.na(egr$responseValueMin) & !is.na(egr$responseValueMax) & !is.na(egr$chillDurationMin) & !is.na(egr$chillDurationMax))
egr.mm2<-filter(egr, !is.na(egr$chillDurationMin) & !is.na(egr$chillDurationMax))
}
quantile(egr$tempDayAvg,na.rm=TRUE)
quantile(egr$chillDurationAvg,na.rm=TRUE)

ggplot(egr,aes(tempDayAvg))+geom_histogram()
ggplot(egr,aes(chillDurationAvg))+geom_histogram()



table(is.na(egr$responseValueAvg))
table(is.na(egr$responseValue))
library(ggplot2)

egr$force.z<-(egr$tempDayAvg-mean(egr$tempDayAvg,na.rm=TRUE))/sd(egr$tempDayAvg,na.rm=TRUE)
egr$chill.z<-(egr$chillDurationAvg-mean(egr$chillDurationAvg,na.rm=TRUE))/sd(egr$chillDurationAvg,na.rm=TRUE)

egrt<-filter(egr,!is.na(tempDayAvg))
quantile(egrt$chillDurationAvg,na.rm=TRUE)


ggplot2::ggplot(egrt,aes(force.z,responseValueAvg))+geom_point(aes(color=chill.z))+facet_wrap(~latbi)
 



