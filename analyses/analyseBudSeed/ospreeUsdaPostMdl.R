# started Aug 9, 2026 by D. Loughnan
# aim of this code is to model the posterior output of the ospree and egret/usda models to test the relationships between budburst and seed germination cues

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

d <- read.csv("output/egretUsdaData.csv")
# removing the rows with incomplete data:
d <- d[complete.cases(d),] 

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
sp.angio <- unique(angio$latbi)

fitAngio <- readRDS("analyseBudSeed/output/fit_full_angio.rds")
# sumAngio <- readRDS("analyseBudSeed/output/summary_full_angio.rds")

posterior_list <- rstan::extract(fitAngio)

betaC <- posterior_list$bc
betaC <- as.data.frame(betaC)
colnames(betaC) <- sp.angio

bcMean <- data.frame(colMeans(betaC)); bcMean$latbi <- sp.angio
# bcMean$dataset <- "usda"

betaF <- posterior_list$bf
betaF <- as.data.frame(betaF)
colnames(betaF) <- sp.angio

bfMean <- data.frame(colMeans(betaF)); bfMean$latbi <- sp.angio
# bfMean$dataset <- "usda"

bEgret <- merge(bfMean, bcMean, by = c("latbi"))
names(bEgret) <- c("latbi","egretBetaF","egretBetaC")

### OSPREE
osp<-read.csv("input/ospreeforegret.csv")
sp.ref <- unique(osp$latbi)

fit <- readRDS("analyseBudSeed/output/fit_ospree.rds")
# sumOspree <- readRDS("analyseBudSeed/output/summary_full_angio.rds")

posterior_list <- rstan::extract(fit)

betaC <- posterior_list$b_chill
betaC <- data.frame(betaC)
colnames(betaC) <- sp.ref

bcMean <- data.frame(colMeans(betaC));
bcMean$latbi <- sp.ref
# bcMean$dataset <- "ospree"

betaF <- posterior_list$b_force
betaF <- as.data.frame(betaF)
colnames(betaF) <- sp.ref

bfMean <- data.frame(colMeans(betaF)); bfMean$latbi <- sp.ref
# bfMean$dataset <- "ospree"

bOspree <- merge(bfMean, bcMean, by = c("latbi"))
names(bOspree) <- c("latbi","ospreeBetaF","ospreeBetaC")

bEgretSub <- bEgret[bEgret$latbi %in% sp.ref,]
bOspreeSub <- bOspree[bOspree$latbi %in% sp.angio,]
dMdl <- merge(bOspreeSub, bEgretSub, by = "latbi")

# model the relationships between the slopes:
mdlData <- list(N = nrow(dMdl), 
                cue = dMdl$ospreeBetaF, 
                ypred = dMdl$egretBetaF)

fitF <- stan(file = "stan/cueSlopeMdl.stan", 
                 data = mdlData, warmup = 3000, iter = 4000,
                 chains = 4, cores = 4 
                 #,thin = 1, control = list(max_treedepth = 15, adapt_delta = 0.97)
)

summ <- data.frame(summary(fitF)[["summary"]])

# Chilling
mdlData <- list(N = nrow(dMdl), 
                cue = dMdl$ospreeBetaC, 
                ypred = dMdl$egretBetaC)

fitC <- stan(file = "stan/cueSlopeMdl.stan", 
             data = mdlData, warmup = 3000, iter = 8000,
             chains = 4, cores = 4 
             #,thin = 1, control = list(max_treedepth = 15, adapt_delta = 0.97)
)

summ <- data.frame(summary(fitC)[["summary"]])

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
