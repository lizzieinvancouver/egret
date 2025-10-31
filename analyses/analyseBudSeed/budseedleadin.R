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

posterior_list <- rstan::extract(fit)

# For example, access samples for a parameter 'beta'
sp.ref<-data.frame(latbi=osp$latbi,sppnum=osp$sppnum)
sp.ref<- sp.ref[!duplicated(sp.ref), ]


beta_samples <- posterior_list$b_chill
beta_samples<-as.data.frame(beta_samples)
colnames(beta_samples)<-sp.ref$latbi
beta_samples<-tidyr::gather(beta_samples,"latbi","effect")
chill<- beta_samples %>%group_by(latbi) %>%summarise(mean_chill=mean(effect))

beta_samples <- posterior_list$b_force
beta_samples<-as.data.frame(beta_samples)
colnames(beta_samples)<-sp.ref$latbi
beta_samples<-tidyr::gather(beta_samples,"latbi","effect")
force<- beta_samples %>%group_by(latbi) %>%summarise(mean_force=mean(effect))



##next steps
###run phylogeny model on ospree and extract posteriors

#####run phylogeny model on egret+ 

egr<-read.csv("output//usdaGerminationCleaned.csv")
table(egr$responseVarClean)
library(dplyr)
egr<-dplyr::filter(egr,responseVarClean=="percent.germ")

sort(unique(egr$latbi))

intersect(unique(egr$latbi),unique(chill$latbi))# 18


###
class(egr$tempDayAvg)
class(egr$responseValueAvg)
egr$chillDurationAvg<-as.numeric(egr$chillDurationMin)

if(FALSE){#there are only 4 rows of data with Min and max values in predictor and response
egr.mm<-filter(egr,!is.na(egr$responseValueMin) & !is.na(egr$responseValueMax) & !is.na(egr$tempDayMin) & !is.na(egr$tempDayMax))
egr.mm2<-filter(egr,!is.na(egr$responseValueMin) & !is.na(egr$responseValueMax) & !is.na(egr$chillDurationMin) & !is.na(egr$chillDurationMax))
egr.mm2<-filter(egr, !is.na(egr$chillDurationMin) & !is.na(egr$chillDurationMax))
}
quantile(egr$tempDayAvg,na.rm=TRUE)
quantile(egr$chillDurationAvg,na.rm=TRUE)

ggplot(egr,aes(tempDayAvg))+geom_histogram()
ggplot(egr,aes(chillDurationMin))+geom_histogram()



table(is.na(egr$responseValueAvg))
egr$force.z<-(egr$tempDayAvg-mean(egr$tempDayAvg,na.rm=TRUE))/sd(egr$tempDayAvg,na.rm=TRUE)
egr$chill.z<-(egr$chillDurationMin-mean(egr$chillDurationMin,na.rm=TRUE))/sd(egr$chillDurationMin,na.rm=TRUE)

egr<-filter(egr,!is.na(responseValueAvg))
egr<-filter(egr,!is.na(force.z))
egr<-filter(egr,!is.na(chill.z))
library(ggplot2)





unique(egr$latbi)


# Get names
tree_tips <- phylo$tip.label
data_tips <- egr$latbi

# Check which names are missing
setdiff(tree_tips, data_tips)  # taxa in tree but not in data
setdiff(data_tips, tree_tips) 

drop_tips <- setdiff(phylo$tip.label, egr$latbi)


# Prune tree
seed_tree <- drop.tip(phylo, drop_tips)

egr<-filter(egr,latbi %in% c(seed_tree$tip.label))
usdaref<-data.frame(latbi=unique(egr$latbi))
usdaref$sppnum<-1:nrow(usdaref)
egr<-left_join(egr,usdaref)


nspecies<-length(unique(egr$sppnum))
egr$phylo<-egr$latbi

A <- ape::vcv.phylo(seed_tree)
 mod.goo<- brms::brm(
  responseValueAvg ~ force.z+chill.z + (1|gr(phylo, cov = A)) + (1|latbi),
  data = egr,
  family = gaussian(),
  data2 = list(A = A),
  sample_prior = TRUE, chains = 4, cores = 4,
  iter = 4000, warmup = 3000
)

get_prior(mod.goo)
fit2 <- stan("stan/uber_USDAseedsModish.stan",
            data=list(N=nrow(egr),
                      n_sp=nspecies,
                      sp=egr$sppnum,
                      x1=egr$force.z,
                      x2 = egr$chill.z,
                      y=egr$responseValueAvg,
                      Vphy=vcv(seed_tree, corr = TRUE)),
            iter = 5000,
            warmup = 3000, # half the iter as warmp is default, but leaving in case we want to change
            chains = 4,
            seed = 117 
)

launch_
posterior_list2 <- rstan::extract(fit2)

beta_samples2 <- posterior_list2$b_chill
beta_samples2<-as.data.frame(beta_samples2)
colnames(beta_samples2)<-usdaref$latbi
beta_samples2<-tidyr::gather(beta_samples2,"latbi","effect")
chill2<- beta_samples2 %>%group_by(latbi) %>%summarise(mean_strat=mean(effect))

beta_samples2 <- posterior_list2$b_force
beta_samples2<-as.data.frame(beta_samples2)
colnames(beta_samples2)<-usdaref$latbi
beta_samples2<-tidyr::gather(beta_samples2,"latbi","effect")
force2<- beta_samples2 %>%group_by(latbi) %>%summarise(mean_inc=mean(effect))



compz<-left_join(chill2,chill)
compz2<-left_join(force2,force)

##flip the sign
compz$mean_chill<-compz$mean_chill*-1
compz2$mean_force<-compz2$mean_force*-1

ggplot(compz,aes(mean_chill,mean_strat))+geom_point()+geom_smooth(method="lm")
cor(compz$mean_chill,compz$mean_strat)
range(egr$force.z)

ggplot(compz2,aes(mean_force,mean_inc))+geom_point()+geom_smooth(method="lm")
cor(compz2$mean_force,compz2$mean_inc)
