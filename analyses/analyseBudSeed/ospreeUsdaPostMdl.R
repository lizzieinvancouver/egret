# started Aug 9, 2026 by D. Loughnan
# aim of this code is to model the posterior output of the ospree and egret/usda models to test the relationships between budburst and seed germination cues
# Aug 11: revising this code for a single Stan model that incldues both ospree and egret/usda 
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
} else if(length(grep("dbuona", getwd()) > 0)) {
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
library(cowplot)
library(reshape2)

# 1. get the egret+usda data
# source("analyseBudSeed/prepEgretUsda.R")
d <- read.csv("output/egretUsdaData.csv")
d <- d[complete.cases(d),] 

# 372 spp
phylo <- ape::read.tree("output/usdaEgretFull.tre")
tipsGym <- getDescendants(phylo, node = 1264)
tipsGym <- tipsGym[tipsGym <= Ntip(phylo)]

# Get only angio
angioPhy <- drop.tip(phylo, phylo$tip.label[tipsGym])

# Get only gymno
gymPhy <- keep.tip(phylo, phylo$tip.label[tipsGym])

angio <- d[d$latbi %in% angioPhy$tip.label, ]
gym <- d[d$latbi %in% gymPhy$tip.label, ]

# For angio
da <- angio
phylo <- angioPhy
subby <- unique(da$latbi)

namesphy <- phylo$tip.label
phylo <- phytools::force.ultrametric(phylo, method="extend")
phylo$node.label <- seq(1,length(phylo$node.label),1)
ape::is.ultrametric(phylo)
# plot(phylo, cex=0.7)

phylo <- ape::keep.tip(phylo, subby) # exclude gymnosperms
# plot(phylo, cex=0.7)
cphy <- ape::vcv.phylo(phylo,corr=TRUE)
rm(subby)

cphy <- vcv.phylo(phylo,corr=TRUE)

da$numspp = as.integer(factor(da$latbi, levels = colnames(cphy)))
da$chillDurationS <- scale(da$chillDuration)
da$tempDayS <- scale(da$germTempGen)


# get ospree data:
###read in ospree
osp<-read.csv("input/ospreeforegret.csv")

phyloO <- read.tree("input/ospreeforegret.tre")

#plot(phylo, cex=0.7)
VCVPHY <- vcv.phylo(phylo,corr=TRUE)
nspeciesO <- max(osp$sppnum)
bask<-data.frame(osp_sps=sort(unique(osp$spps)),optimum_chilling=NA,optimum_forcing=NA,baskin_table=NA)

# create a column that is an indicator of whether or not a spp is in both egret+usda and ospree
ospreeSp <- sort(unique(osp$latbi))
egretSp <- sort(unique(da$latbi))

temp <- da[da$latbi %in% ospreeSp,]
sharedSp <- sort(unique(temp$latbi))

egretShared <- unique(da[, c("latbi", "numspp")])
egretShared <- egretShared[egretShared$latbi %in% sharedSp, ]
egretShared <- egretShared[order(egretShared$latbi),]

ospreeShared <- unique(osp[, c("latbi", "sppnum")])
ospreeShared <- ospreeShared[ospreeShared$latbi %in% sharedSp, ]
ospreeShared <- ospreeShared[order(ospreeShared$latbi),]

# combine in data list:
dataEO =list(
          N_degen = sum(da$responseValue %in% c(0,1)),
          N_prop = sum(da$responseValue>0 & da$responseValue<1),
          N_spEgret =  length(unique(da$latbi)),
          sp_degen = array(da$numspp[da$responseValue %in% c(0,1)],
                   dim = sum(da$responseValue%in% c(0,1))),
          sp_prop = array(da$numspp[da$responseValue>0 & da$responseValue<1],
                  dim = sum(da$responseValue>0 & da$responseValue<1)),
          y_degen = array(da$responseValue[da$responseValue %in% c(0,1)],
                  dim = sum(da$responseValue%in% c(0,1))),
          y_prop = array(da$responseValue[da$responseValue>0 & da$responseValue<1],
                 dim = sum(da$responseValue>0 & da$responseValue<1)),
          c_degen = array(da$chillDurationS[da$responseValue %in% c(0,1)],
                  dim = sum(da$responseValue%in% c(0,1))),
          c_prop = array(da$chillDurationS[da$responseValue>0 & da$responseValue<1],
                 dim = sum(da$responseValue>0 & da$responseValue<1)),
          f_degen = array(da$tempDayS[da$responseValue %in% c(0,1)],
                  dim = sum(da$responseValue%in% c(0,1))),
          f_prop = array(da$tempDayS[da$responseValue>0 & da$responseValue<1],
                 dim = sum(da$responseValue>0 & da$responseValue<1)),
          Vphy_egret = cphy,
          N_ospree = nrow(osp),
          N_ospreeSp = nspeciesO,
          spOspree = osp$sppnum,
          x1_ospree = osp$force.z,
          x2_ospree = osp$chill.z,
          x3_ospree = osp$photo.z,
          y_ospree = osp$resp,
          Vphy_ospree = vcv(phyloO, corr = TRUE),
          shared_sp_ospree = ospreeShared$sppnum,
          shared_sp_egret = egretShared$numspp,
          N_shared = length(sharedSp)
            )

fit <- stan("stan/ospreeEgretMdl.stan",
            data = dataEO,
            iter = 2000,
            warmup = 1000, 
            chains = 4
)


# fitAngio <- readRDS("analyseBudSeed/output/fit_full_angio.rds")
# sumAngio <- readRDS("analyseBudSeed/output/summary_full_angio.rds")

# posterior_list <- rstan::extract(fitAngio)
# 
# betaC <- posterior_list$bc
# betaC <- as.data.frame(betaC)
# colnames(betaC) <- sp.angio
# 
# bcMean <- data.frame(colMeans(betaC)); bcMean$latbi <- sp.angio
# # bcMean$dataset <- "usda"
# 
# betaF <- posterior_list$bf
# betaF <- as.data.frame(betaF)
# colnames(betaF) <- sp.angio
# 
# bfMean <- data.frame(colMeans(betaF)); bfMean$latbi <- sp.angio
# # bfMean$dataset <- "usda"
# 
# bEgret <- merge(bfMean, bcMean, by = c("latbi"))
# names(bEgret) <- c("latbi","egretBetaF","egretBetaC")
# 
# ### OSPREE
# osp<-read.csv("input/ospreeforegret.csv")
# sp.ref <- unique(osp$latbi)
# 
# fit <- readRDS("analyseBudSeed/output/fit_ospree.rds")
# # sumOspree <- readRDS("analyseBudSeed/output/summary_full_angio.rds")
# 
# posterior_list <- rstan::extract(fit)
# 
# betaC <- posterior_list$b_chill
# betaC <- data.frame(betaC)
# colnames(betaC) <- sp.ref
# 
# bcMean <- data.frame(colMeans(betaC));
# bcMean$latbi <- sp.ref
# # bcMean$dataset <- "ospree"
# 
# betaF <- posterior_list$b_force
# betaF <- as.data.frame(betaF)
# colnames(betaF) <- sp.ref
# 
# bfMean <- data.frame(colMeans(betaF)); bfMean$latbi <- sp.ref
# # bfMean$dataset <- "ospree"
# 
# bOspree <- merge(bfMean, bcMean, by = c("latbi"))
# names(bOspree) <- c("latbi","ospreeBetaF","ospreeBetaC")
# 
# bEgretSub <- bEgret[bEgret$latbi %in% sp.ref,]
# bOspreeSub <- bOspree[bOspree$latbi %in% sp.angio,]
# dMdl <- merge(bOspreeSub, bEgretSub, by = "latbi")
# 
# # model the relationships between the slopes:
# mdlData <- list(N = nrow(dMdl), 
#                 cue = dMdl$ospreeBetaF, 
#                 ypred = dMdl$egretBetaF)
# 
# fitF <- stan(file = "stan/cueSlopeMdl.stan", 
#                  data = mdlData, warmup = 3000, iter = 4000,
#                  chains = 4, cores = 4 
#                  #,thin = 1, control = list(max_treedepth = 15, adapt_delta = 0.97)
# )
# 
# summ <- data.frame(summary(fitF)[["summary"]])
# 
# # Chilling
# mdlData <- list(N = nrow(dMdl), 
#                 cue = dMdl$ospreeBetaC, 
#                 ypred = dMdl$egretBetaC)
# 
# fitC <- stan(file = "stan/cueSlopeMdl.stan", 
#              data = mdlData, warmup = 3000, iter = 8000,
#              chains = 4, cores = 4 
#              #,thin = 1, control = list(max_treedepth = 15, adapt_delta = 0.97)
# )
# 
# summ <- data.frame(summary(fitC)[["summary"]])

#### do we get the same answer from the full posterior?
# posterior_list <- rstan::extract(fitAngio)
# 
# betaC <- posterior_list$bc
# betaC <- as.data.frame(betaC)
# colnames(betaC) <- sp.angio
# 
# betaC <- betaC[,names(betaC) %in% sp.ref]
# longBetaC <- melt(betaC,   # variables that stay fixed
#                   variable.name = "latbi",  # new column of old column names
#                   value.name = "betaC")  # column of data values
# 
# # bcMean$dataset <- "usda"
# 
# betaF <- posterior_list$bf
# betaF <- as.data.frame(betaF)
# colnames(betaF) <- sp.angio
# 
# betaF <- betaF[,names(betaF) %in% sp.ref]
# longBetaF <- melt(betaF,   # variables that stay fixed
#                   variable.name = "latbi",  # new column of old column names
#                   value.name = "betaF")  # column of data values
# 
# 
# bEgret <- cbind(longBetaF,longBetaC[,"betaC"])
# names(bEgret) <- c("latbi","egretBetaF","egretBetaC")
# 
# ### OSPREE
# posterior_list <- rstan::extract(fit)
# 
# betaC <- posterior_list$b_chill
# betaC <- data.frame(betaC)
# colnames(betaC) <- sp.ref
# 
# betaC <- betaC[,names(betaC) %in% sp.angio]
# longBetaC <- melt(betaC,   # variables that stay fixed
#                   variable.name = "latbi",  # new column of old column names
#                   value.name = "betaC")  # column of data values
# longBetaC <- longBetaC[order(as.factor(as.character(longBetaC$latbi))),]
# bEgret <- bEgret[order(as.factor(as.character(bEgret$latbi))),]
# 
# temp <- cbind(longBetaC, bEgret[,2], bEgret[,3])
# names(temp) <- c("latbi","ospreeBetaF","egretBetaF","egretBetaC")
# 
# mdlData <- list(N = nrow(temp), 
#                 cue = temp$ospreeBetaF, 
#                 ypred = temp$egretBetaF)
# 
# fitPostF <- stan(file = "stan/cueSlopeMdl.stan", 
#              data = mdlData, warmup = 3000, iter = 4000,
#              chains = 4, cores = 4 
#              #,thin = 1, control = list(max_treedepth = 15, adapt_delta = 0.97)
# )  
  
# bOspree <- merge(bfMean, bcMean, by = c("latbi"))
# names(bOspree) <- c("latbi","ospreeBetaF","ospreeBetaC")
# 
# bEgretSub <- bEgret[bEgret$latbi %in% sp.ref,]
# bOspreeSub <- bOspree[bOspree$latbi %in% sp.angio,]
# dMdl <- merge(bEgret, by = "latbi")
# 
# # model the relationships between the slopes:
# mdlData <- list(N = nrow(dMdl), 
#                 cue = dMdl$ospreeBetaF, 
#                 ypred = dMdl$egretBetaF)
# 
# fitF <- stan(file = "stan/cueSlopeMdl.stan", 
#              data = mdlData, warmup = 3000, iter = 4000,
#              chains = 4, cores = 4 
#              #,thin = 1, control = list(max_treedepth = 15, adapt_delta = 0.97)
# )
