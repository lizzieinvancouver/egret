# Started 9 February 2026
# by the provenance model subgroup!

# goal is to plot the model estimates with provenance vs the model without provenance.

library(stringr)
library(ape)
library(phytools)
library(rstan)

options(mc.cores = parallel::detectCores())

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

if(length(grep("deirdre", getwd()) > 0)) {
  setwd("~/Documents/github/egret/analyses")
} else if(length(grep("lizzie", getwd()) > 0)) {
  setwd("/Users/lizzie/Documents/git/projects/egret/analyses")
} else if(length(grep("sapph", getwd()) > 0)) {
  setwd("/Users/sapph/Documents/ubc things/work/egret/analyses")
} else if(length(grep("danielbuonaiuto", getwd()) > 0)) {
  setwd("/Users/danielbuonaiuto/Documents/git/egret/analyses")
} else if(length(grep("Xiaomao", getwd()) > 0)) {
  setwd("C:/PhD/Project/egret/analyses")
} else if(length(grep("britanywuuu", getwd()) > 0)) {
  setwd("/Documents/ubc/year5/TemporalEcologyLab/egret/analyses")
} else if(length(grep("Ken", getwd())) > 0){
  setwd("/Users/Ken Michiko Samson/Documents/Temporal Ecology Lab/egret/analyses")
} else if(length(grep("christophe_rouleau-desrochers", getwd())) > 0){
  setwd("/Users/christophe_rouleau-desrochers/github/egret/analyses")
} else if(length(grep("crouleau", getwd())) > 0){
  setwd("/home/crouleau/egret/analyses")
} else if(length(grep("victor", getwd())) > 0){
  setwd('~/projects/egret/analyses')
} 

# load Mike's diagnostic tools
util <- new.env()
source('mcmc_analysis_tools_rstan.R', local=util)
source('mcmc_visualization_tools.R', local=util)

# load decision rules
source('provenance/decisionRules.R')

# Save the object 'fit_nophy' as an RDS file
fit_withprov <- readRDS("/Users/christophe_rouleau-desrochers/Desktop/UBC/egretLOCAL/fit_nophy.rds")
fit_nophy_noprov <- readRDS("/Users/christophe_rouleau-desrochers/Desktop/UBC/egretLOCAL/fit_nophy_noprov.rds")

# Try a more efficient way to plot the different parameters
df_withprov <- as.data.frame(fit_withprov)

colswithprov <- colnames(df_withprov)
colswithprov <- colswithprov[!grepl("tilde", colswithprov)]
colswithprov <- colswithprov[!grepl("prov", colswithprov)]

dwithprov <- df_withprov[, colnames(df_withprov) %in% colswithprov]

dwithprov2 <- data.frame(
  prmID = character(ncol(dwithprov)),
  fit_mean  = numeric(ncol(dwithprov)),  
  fit_per5  = NA, 
  fit_per25 = NA,
  fit_per75 = NA,
  fit_per95 = NA
)

for (i in 1:ncol(dwithprov)) { # i = 1
  dwithprov2$prmID[i] <- colnames(dwithprov)[i]         
  dwithprov2$fit_mean[i] <- round(mean(dwithprov[[i]]),3)  
  dwithprov2$fit_per5[i] <- round(quantile(dwithprov[[i]], probs = 0.05), 3)
  dwithprov2$fit_per25[i] <- round(quantile(dwithprov[[i]], probs = 0.25), 3)
  dwithprov2$fit_per75[i] <- round(quantile(dwithprov[[i]], probs = 0.75), 3)
  dwithprov2$fit_per95[i] <- round(quantile(dwithprov[[i]], probs = 0.95), 3)
}
dwithprov2

# get a subset for just the slope and intercepts 
vec <- c(paste("a", "[", 1:25, "]", sep = ""),
         paste("bt", "[", 1:25, "]", sep = ""),
         paste("bf", "[", 1:25, "]", sep = ""),
         paste("bcs", "[", 1:25, "]", sep = ""),
         dwithprov2$prmID[grepl("sigma", dwithprov2$prmID)])
prmvec <- c(rep("a", each = 25),
         rep("sigma", each =  4),
         rep("bt", each = 25), 
         rep("bf", each = 25), 
         rep("bcs", each =  25))

dwithprov3 <- subset(dwithprov2, prmID %in% vec)

# No prov
df_noprov <- as.data.frame(fit_nophy_noprov)

colsnoprov <- colnames(df_noprov)
colsnoprov <- colsnoprov[!grepl("tilde", colsnoprov)]
colsnoprov <- colsnoprov[!grepl("prov", colsnoprov)]

dnoprov <- df_noprov[, colnames(df_noprov) %in% colsnoprov]

dnoprov2 <- data.frame(
  prmID = character(ncol(dnoprov)),
  fit_mean  = numeric(ncol(dnoprov)),  
  fit_per5  = NA, 
  fit_per25 = NA,
  fit_per75 = NA,
  fit_per95 = NA
)

for (i in 1:ncol(dnoprov)) { # i = 1
  dnoprov2$prmID[i] <- colnames(dnoprov)[i]         
  dnoprov2$fit_mean[i] <- round(mean(dnoprov[[i]]),3)  
  dnoprov2$fit_per5[i] <- round(quantile(dnoprov[[i]], probs = 0.05), 3)
  dnoprov2$fit_per25[i] <- round(quantile(dnoprov[[i]], probs = 0.25), 3)
  dnoprov2$fit_per75[i] <- round(quantile(dnoprov[[i]], probs = 0.75), 3)
  dnoprov2$fit_per95[i] <- round(quantile(dnoprov[[i]], probs = 0.95), 3)
}
dnoprov2

# get a subset for just the slope and intercepts 
vec <- c(paste("a", "[", 1:27, "]", sep = ""),
         paste("bt", "[", 1:27, "]", sep = ""),
         paste("bf", "[", 1:27, "]", sep = ""),
         paste("bcs", "[", 1:27, "]", sep = ""))
         # dnoprov2$prmID[grepl("sigma", dnoprov2$prmID)])
prmvec <- c(rep("a", each = 27),
            # rep("sigma", each =  4),
            rep("bt", each = 27), 
            rep("bf", each = 27), 
            rep("bcs", each =  27))

dnoprov3 <- subset(dnoprov2, prmID %in% vec)



# Merge!
colnames(dnoprov3)[2:ncol(dnoprov3)] <- paste(
  colnames(dnoprov3)[2:ncol(dnoprov3)], "noprov", sep = "_")

dnoprov3$prm <- prmvec

dforplot <- merge(dwithprov3, dnoprov3, by = "prmID")

# Get the number of provenances per species
# count the number of unique provenance per species
provcounts <- aggregate(provLatLonAlt ~ genusspecies,
          newd,
          function(x) length(unique(x)))

dforplot$numspp <- sub(".*\\[(\\d+)\\]", "\\1", dforplot$prmID)
dforplot$spp <- modeld$genusspecies[match(dforplot$numspp, modeld$numspp)]

dforplot$provperspp <- provcounts$provLatLonAlt[match(dforplot$spp, provcounts$genusspecies)]

# which species are not modeled?
View(subset(newd, genusspecies %in% setdiff(unique(newd$genusspecies), unique(dforplot$spp))))

# Plot!
ggplot(dforplot, aes(x = fit_mean, y = fit_mean_noprov)) +
  geom_errorbar(aes(xmin = fit_per25, xmax = fit_per75), 
                width = 0, linewidth = 0.5, color = "darkgray", alpha=0.7) +
  geom_errorbar(aes(ymin = fit_per25_noprov, ymax = fit_per75_noprov), 
                width = 0, linewidth = 0.5, color = "darkgray", alpha = 0.7) +
  geom_point(aes(color = provperspp), size = 1.5) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "#B40F20", linewidth = 0.8) +
  facet_wrap(~prm, scales = "free") +
  
  labs(x = "with prov", y = "no prov", title = "") +
  theme_minimal()
  