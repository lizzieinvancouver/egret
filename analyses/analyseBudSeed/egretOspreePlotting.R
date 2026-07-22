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

quantile2575 <- function(x){
  returnQuanilte <- quantile(x, prob = c(0.05, 0.25, 0.75,0.95))
  return(returnQuanilte)
}

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
da <- angio
phylo <- angioPhy
subby <- unique(da$latbi)

namesphy <- phylo$tip.label
phylo <- phytools::force.ultrametric(phylo, method="extend")
phylo$node.label <- seq(1,length(phylo$node.label),1)
ape::is.ultrametric(phylo)

phylo <- ape::keep.tip(phylo, subby) # exclude gymnosperms
cphy <- ape::vcv.phylo(phylo,corr=TRUE)
rm(subby)
cphy <- vcv.phylo(phylo,corr=TRUE)
da$numspp = as.integer(factor(da$latbi, levels = colnames(cphy)))
da$chillDurationS <- scale(da$chillDuration)
da$tempDayS <- scale(da$germTempGen)

fitAngio <- readRDS("analyseBudSeed/output/fit_full_angio.rds")
sumAngio <- readRDS("analyseBudSeed/output/summary_full_angio.rds")

sp.angio <- data.frame(latbi= da$latbi)
sp.angio <- sp.angio[!duplicated(sp.angio), ]

posterior_list <- rstan::extract(fitAngio)

betaC <- posterior_list$bc
betaC <- as.data.frame(betaC)
colnames(betaC) <- sp.angio

bcMean <- data.frame(colMeans(betaC)); bcMean$latbi <- sp.angio
bcQuan <- t(apply(betaC, 2, quantile2575) )
bcEgret <- cbind(bcMean, bcQuan); bcEgret <- bcEgret[,c("latbi", "colMeans.betaC.","5%","25%","75%","95%")]
names(bcEgret) <- c("latbi","egretMean","egret5","egret25","egret75","egret95")

# forcing
betaF <- posterior_list$bf
betaF <- as.data.frame(betaF)
colnames(betaF) <- sp.angio

bfMean <- data.frame(colMeans(betaF)); bfMean$latbi <- sp.angio
bfQuan <- t(apply(betaF, 2, quantile2575) )
bfEgret <- cbind(bfMean, bfQuan); bfEgret <- bfEgret[,c("latbi", "colMeans.betaF.","5%","25%","75%","95%")]
names(bfEgret) <- c("latbi","egretMean","egret5","egret25","egret75","egret95")

# get ospree data
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

bcQuan <- t(apply(betaC, 2, quantile2575))
bcOspree <- cbind(bcMean, bcQuan); bcOspree <- bcOspree[,c("latbi", "colMeans.betaC.","5%","25%","75%","95%")]
names(bcOspree) <- c("latbi","ospreeMean","ospree5","ospree25","ospree75","ospree95")

# forcing
betaF <- posterior_list$b_force
betaF <- as.data.frame(betaF)
colnames(betaF) <- sp.ref

bfMean <- data.frame(colMeans(betaF)); bfMean$latbi <- sp.ref
bfQuan <- t(apply(betaF, 2, quantile2575) )
bfOspree <- cbind(bfMean, bfQuan); bfOspree <- bfOspree[,c("latbi", "colMeans.betaF.","5%","25%","75%","95%")]
names(bfOspree) <- c("latbi","ospreeMean","ospree5","ospree25","ospree75","ospree95")

# merge and plot:

bsChill <- merge(bcOspree, bcEgret, by = c("latbi"), all = TRUE)
bsChill <- bsChill[complete.cases(bsChill),] 

bsChill$ospreeMeanS <- (bsChill$ospreeMean* -1)/10
bsChill$ospree5S <- (bsChill$ospree5* -1)/10
bsChill$ospree95S <- (bsChill$ospree95* -1)/10

# forcing vs germination 
bsForce <- merge(bfOspree, bfEgret, by = c("latbi"), all = TRUE)
bsForce <- bsForce[complete.cases(bsForce),] 

bsForce$ospreeMeanS <- (bsForce$ospreeMean* -1)/10
bsForce$ospree5S <- (bsForce$ospree5* -1)/10
bsForce$ospree95S <- (bsForce$ospree95* -1)/10

pdf("analyseBudSeed/figures/budSeedCompare.pdf", width = 8, height = 5)
par(mfrow = c(1,2))
plot(bsChill$egretMean ~ bsChill$ospreeMean, xlim = c(-25,5), ylim = c(-2,3), type = 'n',
     xlab = "Ospree chill response", ylab = "Egret chill response" )

x_vals <- seq(-26, 6, length.out = 38)
fit25 <- lm(egret25 ~ ospree25, data = bsChill)
fit75 <- lm(egret75 ~ ospree75, data = bsChill)

y25 <- predict(fit25, newdata = data.frame(ospree25 = x_vals))
y75 <- predict(fit75, newdata = data.frame(ospree75 = x_vals))

# 5. Shade the area between the lines using polygon()
polygon(c(x_vals, rev(x_vals)), c(y25, rev(y75)), col = "lightgrey", border = NA)

arrows(
  bsChill[,"ospreeMean"], # x mean
  bsChill[,"egret5"], # y 25
  bsChill[,"ospreeMean"],
  bsChill[,"egret95"],
  length = 0, col= "cyan4", lwd = 1 
)

arrows(
  bsChill[,"ospree5"], # x mean
  bsChill[,"egretMean"], # y 25
  bsChill[,"ospree95"],
  bsChill[,"egretMean"],
  length = 0, col= "cyan4", lwd = 1 
)

abline(lm(bsChill$egretMean~bsChill$ospreeMean))
text(-25, 2.8, label = expression(bold("a")), cex = 1)
################################################################
plot(bsForce$egretMean ~ bsForce$ospreeMean, xlim = c(-25,5), ylim = c(-2,3), type = 'n',
     xlab = "Ospree force response", ylab = "Egret germination temperature response" )

x_vals <- seq(-26, 6, length.out = 38)
fit25 <- lm(egret25 ~ ospree25, data = bsForce)
fit75 <- lm(egret75 ~ ospree75, data = bsForce)

y25 <- predict(fit25, newdata = data.frame(ospree25 = x_vals))
y75 <- predict(fit75, newdata = data.frame(ospree75 = x_vals))

# 5. Shade the area between the lines using polygon()
polygon(c(x_vals, rev(x_vals)), c(y25, rev(y75)), col = "lightgrey", border = NA)

arrows(
  bsForce[,"ospreeMean"], # x mean
  bsForce[,"egret5"], # y 25
  bsForce[,"ospreeMean"],
  bsForce[,"egret95"],
  length = 0, col= "maroon", lwd = 1 
)

arrows(
  bsForce[,"ospree5"], # x mean
  bsForce[,"egretMean"], # y 25
  bsForce[,"ospree95"],
  bsForce[,"egretMean"],
  length = 0, col= "maroon", lwd = 1 
)

abline(lm(bsForce$egretMean~bsForce$ospreeMean))
text(-25, 2.8, label = expression(bold("b")), cex = 1)

dev.off()


### Just points:
pdf("analyseBudSeed/figures/budSeedComparePoints.pdf", width = 8, height = 5)
par(mfrow = c(1,2))
plot(bsChill$egretMean ~ bsChill$ospreeMean, xlim = c(-25,5), ylim = c(-1,2), type = 'n',
     xlab = "Ospree chill response", ylab = "Egret chill response" )

x_vals <- seq(-26, 6, length.out = 38)
fit25 <- lm(egret25 ~ ospree25, data = bsChill)
fit75 <- lm(egret75 ~ ospree75, data = bsChill)

y25 <- predict(fit25, newdata = data.frame(ospree25 = x_vals))
y75 <- predict(fit75, newdata = data.frame(ospree75 = x_vals))

# 5. Shade the area between the lines using polygon()
polygon(c(x_vals, rev(x_vals)), c(y25, rev(y75)), col = "lightgrey", border = NA)
abline(lm(bsChill$egretMean~bsChill$ospreeMean))

points(bsChill$egretMean ~ bsChill$ospreeMean, col = "black", bg = "cyan4", pch = 21)

text(-25, 2, label = expression(bold("a")), cex = 1)

#############################################################################################
plot(bsForce$egretMean ~ bsForce$ospreeMean, xlim = c(-25,5), ylim = c(-1,2), type = 'n',
     xlab = "Ospree force response", ylab = "Egret germination temperature response" )

x_vals <- seq(-26, 6, length.out = 38)
fit25 <- lm(egret25 ~ ospree25, data = bsForce)
fit75 <- lm(egret75 ~ ospree75, data = bsForce)

y25 <- predict(fit25, newdata = data.frame(ospree25 = x_vals))
y75 <- predict(fit75, newdata = data.frame(ospree75 = x_vals))

# 5. Shade the area between the lines using polygon()
polygon(c(x_vals, rev(x_vals)), c(y25, rev(y75)), col = "lightgrey", border = NA)

points(bsForce$egretMean ~ bsForce$ospreeMean, col = "black", bg = "maroon", pch = 21)

abline(lm(bsForce$egretMean~bsForce$ospreeMean))
text(-25, 2, label = expression(bold("b")), cex = 1)

dev.off()
