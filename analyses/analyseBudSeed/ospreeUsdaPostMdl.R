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
# library(pez)
# library(cowplot)
library(reshape2)
 library(shinystan)

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

fitOspree <- stan("stan/ospreeEgretMdl_2for1_v2.stan",
            data = dataEO,
            iter = 2000,
            warmup = 1000, 
            chains = 4
)

summ <- data.frame(summary(fit)[["summary"]])
sampler_params  <- get_sampler_params(fitOspree, inc_warmup = FALSE)
diagnostics <- list(
  max_treedepth= max(sapply(sampler_params, function(x) max(x[, "treedepth__"]))),
  max_divergence = max(sapply(sampler_params, function(x) sum(x[, "divergent__"]))),
  max_rhat = max(summ$Rhat, na.rm = TRUE),
  min_ess = min(summ$n_eff, na.rm = TRUE)
)

saveRDS(fitOspree, file = 'analyseBudSeed/output/fit_combinedMdl_fitOspree.rds')
saveRDS(summ, file = 'analyseBudSeed/output/summary_combinedMdl_fitOspree.rds')
saveRDS(diagnostics, file = 'analyseBudSeed/output/diagnostics_combinedMdl_fitOspree.rds')

fitEgret <- stan("stan/ospreeEgretMdl_egretOnly.stan",
                  data = dataEO,
                  iter = 2000,
                  warmup = 1000, 
                  chains = 4
)

summE <- data.frame(summary(fitEgret)[["summary"]])
sampler_params  <- get_sampler_params(fitEgret, inc_warmup = FALSE)
diagnosticsE <- list(
  max_treedepth= max(sapply(sampler_params, function(x) max(x[, "treedepth__"]))),
  max_divergence = max(sapply(sampler_params, function(x) sum(x[, "divergent__"]))),
  max_rhat = max(summ$Rhat, na.rm = TRUE),
  min_ess = min(summ$n_eff, na.rm = TRUE)
)

saveRDS(fitEgret, file = 'analyseBudSeed/output/fit_combinedMdl_fitEgret.rds')
saveRDS(summE, file = 'analyseBudSeed/output/summary_combinedMdl_fitEgret.rds')
saveRDS(diagnosticsE, file = 'analyseBudSeed/output/diagnostics_combinedMdl_fitEgret.rds')

fit <- readRDS("analyseBudSeed/output/fit_combinedMdl_fitOspree.rds")


ssm <-  as.shinystan(fit)
launch_shinystan(ssm)

util <- new.env()
source('mcmc_analysis_tools_rstan.R', local=util)
source('mcmc_visualization_tools.R', local=util)

## Angiosperm
fit <- readRDS("analyseBudSeed/output/fit_combinedMdl_fitOspree.rds")
summ <- readRDS("analyseBudSeed/output/summary_combinedMdl_fitOspree.rds")
diagnostics <- readRDS("analyseBudSeed/output/diagnostics_combinedMdl_fitOspree.rds")

samples <- util$extract_expectand_vals(fit)

base_samples <- util$filter_expectands(samples,
                                       c("a_z_egret", "lambda_a_egret", "sigma_a_egret", "bc_z_egret","lambda_bc_egret",
                                         "sigma_bc_egret", "bf_z_egret","lambda_bf_egret","sigma_bf_egret", "sigma_y_ospree",
                                         "lam_interceptsa_ospree","lam_interceptsbc_ospree","lam_interceptsbf_ospree",
                                         "lam_interceptsbp_ospree", "sigma_interceptsa_ospree","sigma_interceptsbf_ospree",
                                         "sigma_interceptsbc_ospree","sigma_interceptsbp_ospree","b_zf_ospree","b_zc_ospree",
                                         "b_zp_ospree","a_z_ospree","b_both","a_both","sigma_y_both"),
                                       check_arrays=TRUE)
util$check_all_expectand_diagnostics(base_samples)

# Retrodictive check
par(mfrow=c(1, 1), mar = c(4,4,2,2))
names <- c(sapply(1:dataEO$N_prop, function(n) paste0('y_prop_gen[',n,']')),
           sapply(1:dataEO$N_degen, function(n) paste0('y_degen_gen[',n,']')))
preds <- samples[names]
names(preds) <- sapply(1:length(preds), function(n) paste0('y_gen[',n,']'))
util$plot_hist_quantiles(preds, 'y_gen', 0, 1, 0.1,
                         baseline_values=c(dataEO$y_prop, dataEO$y_degen),
                         xlab="Germination perc.")

# Posterior inference
par(mfrow=c(3, 2), mar = c(4,4,1,1))

util$plot_expectand_pushforward(samples[['a_z_egret']], 20,
                                display_name = "a_z_egret")
util$plot_expectand_pushforward(samples[['bc_z_egret']], 20,
                                display_name = "bc_z")
util$plot_expectand_pushforward(samples[['bf_z_egret']], 20,
                                display_name = "bf_z")
util$plot_expectand_pushforward(samples[['lambda_a_egret']], 20,
                                display_name = "lambda_a_egret")
util$plot_expectand_pushforward(samples[['lambda_bc_egret']], 20,
                                display_name = "lambda_bc_egret")
util$plot_expectand_pushforward(samples[['lambda_bf_egret']], 20,
                                display_name = "lambda_bc_egret")
util$plot_expectand_pushforward(samples[['sigma_a_egret']], 20,
                                flim = c(0,6),
                                display_name="sigma_a_egret")
util$plot_expectand_pushforward(samples[['sigma_bc_egret']], 20,
                                flim = c(0,6),
                                display_name="sigma_bc_egret")
util$plot_expectand_pushforward(samples[['sigma_bf_egret']], 20,
                                flim = c(0,6),
                                display_name="sigma_bf_egret")
util$plot_expectand_pushforward(samples[['lam_interceptsa_ospree']], 20,
                                flim = c(0,6),
                                display_name="lam_interceptsa_ospree")
util$plot_expectand_pushforward(samples[['lam_interceptsbc_ospree']], 20,
                                flim = c(0,6),
                                display_name="lam_interceptsbc_ospree")
util$plot_expectand_pushforward(samples[['lam_interceptsbf_ospree']], 20,
                                flim = c(0,6),
                                display_name="lam_interceptsbf_ospree")
util$plot_expectand_pushforward(samples[['lam_interceptsbp_ospree']], 20,
                                flim = c(0,6),
                                display_name="lam_interceptsbp_ospree")
util$plot_expectand_pushforward(samples[['b_zf_ospree']], 20, # weird
                                flim = c(0,6),
                                display_name="b_zf_ospree")
util$plot_expectand_pushforward(samples[['b_zc_ospree']], 20,
                                flim = c(0,6),
                                display_name="b_zc_ospree")
util$plot_expectand_pushforward(samples[['b_zp_ospree']], 20,
                                flim = c(0,6),
                                display_name="b_zp_ospree")
util$plot_expectand_pushforward(samples[['a_z_ospree']], 20,
                                flim = c(0,6),
                                display_name="a_z_ospree")
util$plot_expectand_pushforward(samples[['sigma_y_ospree']], 20,
                                flim = c(0,6),
                                display_name="sigma_y_ospree")
util$plot_expectand_pushforward(samples[['sigma_interceptsa_ospree']], 20,
                                flim = c(0,6),
                                display_name="sigma_interceptsa_ospree")
util$plot_expectand_pushforward(samples[['sigma_interceptsbf_ospree']], 20,
                                flim = c(0,6),
                                display_name="sigma_interceptsbf_ospree")
util$plot_expectand_pushforward(samples[['sigma_interceptsbc_ospree']], 20,
                                flim = c(0,6),
                                display_name="sigma_interceptsbc_ospree")
util$plot_expectand_pushforward(samples[['sigma_interceptsbp_ospree']], 20,
                                flim = c(0,6),
                                display_name="sigma_interceptsbp_ospree")
util$plot_expectand_pushforward(samples[['b_both']], 20,
                                display_name = "b_both")
util$plot_expectand_pushforward(samples[['a_both']], 20,
                                display_name = "a_both")
util$plot_expectand_pushforward(samples[['sigma_y_both']], 20,
                                display_name = "sigma_y_both")

# species_names <- tapply(da$latbi, da$numspp, unique)
# species_names <- as.character(species_names)
# 
# # Create a data frame that maps the index to the Name
# lookup <- data.frame(
#   index = 1:length(species_names),
#   species_name = species_names
# )
# 
# # Plot for chilling
# parameter_bc <- c("bc_z", names(fit)[grep("^bc\\[", names(fit))])
# stats <- summary(fit, pars = parameter_bc, probs = c(0.25, 0.75))$summary
# 
# df_bc <- as.data.frame(stats)
# df_bc$parameter <- rownames(df_bc)
# 
# colnames(df_bc)[grep("25%", colnames(df_bc))] <- "low"
# colnames(df_bc)[grep("75%", colnames(df_bc))] <- "high"
# 
# 
# df_bc$is_mean <- ifelse(df_bc$parameter == "bc_z", "Global Mean", "Species")
# 
# # making new columns to seperate global mean and sp level mean
# df_bc$index <- as.numeric(gsub("\\D", "", df_bc$parameter))
# df_bc$name <- ifelse(is.na(df_bc$index), 
#                      "Global Mean", 
#                      species_names[df_bc$index])
# df_bc <- df_bc[order(df_bc$index), ]
# df_bc <- rbind(
#   df_bc[df_bc$name == "Global Mean", ],
#   df_bc[df_bc$name != "Global Mean", ]
# )
# df_bc$name <- factor(df_bc$name, levels = rev(df_bc$name))
# 
# pdf("analyseBudSeed/figures/fullChillingAngio.pdf", width = 20, height = 50)
# ggplot(df_bc, aes(x = mean, y = name)) +
#   geom_errorbar(aes(xmin = low, xmax = high, color = is_mean), 
#                 width = 0,
#                 linewidth = 1.5) +
#   geom_point(size = 2.5) +
#   
#   geom_vline(xintercept = 0, linetype = "dashed", color = "black", alpha = 0.5) +
#   
#   scale_color_manual(values = c("Global Mean" = "firebrick", "Species" = "black")) +
#   labs(title = "bc",
#        x = "Posterior Estimate", y = NULL) +
#   theme_minimal()
# dev.off()
# 

# util <- new.env()
# 
# source('modeling/mcmc_analysis_tools_rstan.R', local=util)
# 
# samples <- util$extract_expectand_vals(fit)
# 
# f_names <- sapply(1:dataEO$N_degen,
#                     function(n) paste0('y_degen_gen[', n, ']'))
# da$y_gen_mean <- NA
#   
#   da[da$responseValue %in% c(0,1), 'y_gen_mean'] <-
#     sapply(f_names, function(n){
#       mean(rowMeans(samples[[n]]))})
#   
#   
#   f_names <- sapply(1:dataEO$N_prop,
#                     function(n) paste0('y_prop_gen[', n, ']'))
#   d[d$responseValue>0 & d$responseValue<1, 'y_gen_mean'] <-
#     sapply(f_names, function(n){
#       mean(rowMeans(samples[[n]]))})
#   
#   library(paletteer)
#   nColor <- 100
#   colors = paletteer_c("viridis::inferno", n=nColor)
#   rank <- as.factor( as.numeric( cut(ppcheck$numspp, nColor)))
#   
#   ppcheck <- d[c('numspp', 'responseValue', 'y_gen_mean', 'tempDayS', 'chillDurationS')]
#   plot(da$responseValue ~ ppcheck$y_gen_mean, 
#        col = colors[ rank ], cex = 0.5)
#   abline(a=0, b=1, col = 'white', lwd = 4)
#   abline(a=0, b=1, col = 'black', lwd = 1)
#   
#   
#   
#   s <- sample(unique(ppcheck$numspp), 1)
#   ppchecks <- ppcheck[ppcheck$numspp ==s, ]
#   plot(ppchecks$responseValue ~ ppchecks$time, 
#        col = 'grey', cex = 1)
#   points(ppchecks$y_gen_mean ~ ppchecks$time, cex = 0.5)
#   
#   
#   
#   sp <- 94
#   idxs_degen <- which(dataEO$sp_degen == sp)
#   idxs_prop <- which(dataEO$sp_prop == sp)
#   y_prop_names <- sapply(idxs_prop, function(n) paste0('y_prop_gen[', n, ']'))
#   
#   
#   
#   par(mfrow=c(1, 1))
#   plot.new()
#   xlim=c(0, 1)
#   ylim=c(0, 1)
#   abline(a=0, b=1, col = 'white', lwd = 4)
#   abline(a=0, b=1, col = 'black', lwd = 1)
#   points(x= dataEO$y_prop[idxs_prop], y =  summ[y_prop_names, 'mean'], pch=16, cex=1.0, col="white")
#   points(x= dataEO$y_prop[idxs_prop], y =  summ[y_prop_names, 'mean'], pch=16, cex=0.8, col="black")
#   
#   sub <- da[da$numspp == sp, ]
#   sub <- d[d$genusspecies == 'Clematis_vitalba',]
#   
#   par(mfrow=c(1, 3))
#   dataEO$dataID_prop <- as.integer(factor(da$datasetID[da$responseValue>0 & da$responseValue<1]))
#   idxs_prop <- which(dataEO$sp_prop == sp)
#   
#   for(id in unique(dataEO$dataID_prop[idxs_prop])){
#     
#     idxs_prop_sp <- which(dataEO$dataID_prop[idxs_prop] == id)
#     plot(dataEO$y_prop[idxs_prop_sp] ~ dataEO$t_prop[idxs_prop_sp], pch = 16, cex = 0.5,
#          col = unique(dataEO$dataID_prop[idxs_prop_sp]))
#     
#     
#   }
#   
#   length(unique(da$numspp))
#   
#   