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
  setwd("/Users/danielbuonaiuto/Documents/git/egret/analyses/")
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
d<-read.csv("input/ospreeforegret.csv")


phylo <- read.tree("input/ospreeforegret.tre")



#plot(phylo, cex=0.7)
VCVPHY <- vcv.phylo(phylo,corr=TRUE)
nspecies <- max(d$sppnum)





fit <- stan("stan/uber_threeslopeintercept_modified_cholesky_updatedpriors.stan",
            data=list(N=nrow(d),
                      n_sp=nspecies,
                      sp=d$sppnum,
                      x1=d$force.z,
                      x2 = d$chill.z,
                      x3=d$photo.z,
                      y=d$resp,
                      Vphy=vcv(phylo, corr = TRUE)),
            iter = 4000,
            warmup = 2000, # half the iter as warmp is default, but leaving in case we want to change
            chains = 4,
            seed = 117 
)




