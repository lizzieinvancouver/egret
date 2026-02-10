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

# load models OR run "provenance/modelingCues.R"
getwd()

# Save the object 'fit_nophy' as an RDS file
# fitwithprov <- saveRDS(fit_nophy_noprov, "/Users/christophe_rouleau-desrochers/Desktop/UBC/egretLOCAL/fit_nophy_noprov.rds")

# start with one parameter bt, for each species sp_degen
df_withprov <- as.data.frame(fit_nophy)

# Recover bt with prov ####
btcolsWprov <- colnames(df_withprov)[grepl("bt", colnames(df_withprov))]
btcolsWprov <- grep("tilde", btcolsWprov, invert = TRUE, value = TRUE)
btcolsWprov <- grep("prov", btcolsWprov, invert = TRUE, value = TRUE)
btcolsWprov <- grep("z", btcolsWprov, invert = TRUE, value = TRUE)
btcolsWprov <- grep("sigma", btcolsWprov, invert = TRUE, value = TRUE)

bt_dfWprov <- df_withprov[, colnames(df_withprov) %in% btcolsWprov]

# change their names
# colnames(bt_dfWprov) <- sub("bt\\[(\\d+)\\]", "\\1", colnames(bt_dfWprov))
#empty spp df
bt_dfWprov2 <- data.frame(
  spp = character(ncol(bt_dfWprov)),
  fit_bsp = numeric(ncol(bt_dfWprov)),  
  fit_bsp_per5 = NA, 
  fit_bsp_per25 = NA,
  fit_bsp_per75 = NA,
  fit_bsp_per95 = NA
)

for (i in 1:ncol(bt_dfWprov)) { # i = 1
  bt_dfWprov2$spp[i] <- colnames(bt_dfWprov)[i]         
  bt_dfWprov2$fit_bsp[i] <- round(mean(bt_dfWprov[[i]]),3)  
  bt_dfWprov2$fit_bsp_per5[i] <- round(quantile(bt_dfWprov[[i]], probs = 0.05), 3)
  bt_dfWprov2$fit_bsp_per25[i] <- round(quantile(bt_dfWprov[[i]], probs = 0.25), 3)
  bt_dfWprov2$fit_bsp_per75[i] <- round(quantile(bt_dfWprov[[i]], probs = 0.75), 3)
  bt_dfWprov2$fit_bsp_per95[i] <- round(quantile(bt_dfWprov[[i]], probs = 0.95), 3)
}
  
# Recover bt without prov ####
df_noprov <- as.data.frame(fit_nophy_noprov)

btcolsnoprov <- colnames(df_noprov)[grepl("bt", colnames(df_noprov))]
btcolsnoprov <- grep("tilde", btcolsnoprov, invert = TRUE, value = TRUE)
btcolsnoprov <- grep("z", btcolsnoprov, invert = TRUE, value = TRUE)
btcolsnoprov <- grep("sigma", btcolsnoprov, invert = TRUE, value = TRUE)

bt_dfnoprov <- df_noprov[, colnames(df_noprov) %in% btcolsnoprov]

# change their names
# colnames(bt_dfWprov) <- sub("bt\\[(\\d+)\\]", "\\1", colnames(bt_dfWprov))
#empty spp df
bt_dfnoprov2 <- data.frame(
  spp = character(ncol(bt_dfnoprov)),
  fit_bsp = numeric(ncol(bt_dfnoprov)),  
  fit_bsp_per5 = NA, 
  fit_bsp_per25 = NA,
  fit_bsp_per75 = NA,
  fit_bsp_per95 = NA
)

for (i in 1:ncol(bt_dfnoprov)) { # i = 1
  bt_dfnoprov2$spp[i] <- colnames(bt_dfnoprov)[i]         
  bt_dfnoprov2$fit_bsp[i] <- round(mean(bt_dfnoprov[[i]]),3)  
  bt_dfnoprov2$fit_bsp_per5[i] <- round(quantile(bt_dfnoprov[[i]], probs = 0.05), 3)
  bt_dfnoprov2$fit_bsp_per25[i] <- round(quantile(bt_dfnoprov[[i]], probs = 0.25), 3)
  bt_dfnoprov2$fit_bsp_per75[i] <- round(quantile(bt_dfnoprov[[i]], probs = 0.75), 3)
  bt_dfnoprov2$fit_bsp_per95[i] <- round(quantile(bt_dfnoprov[[i]], probs = 0.95), 3)
}

# prep for plot
colnames(bt_dfnoprov2)[2:ncol(bt_dfnoprov2)] <- paste(colnames(bt_dfnoprov2)[2:ncol(bt_dfnoprov2)], "noprov", sep = "_")

# bind
btforplot <- merge(bt_dfWprov2, bt_dfnoprov2, by = "spp")

ggplot(btforplot, aes(x = fit_bsp, y = fit_bsp_noprov)) +
  geom_errorbar(aes(xmin = fit_bsp_per25, xmax = fit_bsp_per75), 
                width = 0, linewidth = 0.5, color = "darkgray", alpha=0.7) +
  geom_errorbar(aes(ymin = fit_bsp_per25_noprov, ymax = fit_bsp_per75_noprov), 
                width = 0, linewidth = 0.5, color = "darkgray", alpha = 0.7) +
  geom_point(color = "#046C9A", size = 2) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "#B40F20", linewidth = 1) +
  labs(x = "sim bsp", y = "fit bsp", title = "") +
  theme_minimal()
