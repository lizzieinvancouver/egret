# January 7, 2026
# started by D. Loughnan with the aim to model the USDA data using the main egret model
# but USDA data just has stratification and germination temps
# library(cmdstanr)
#library(stringr)
library(ape)
library(phytools)
library(pez)
library(rstan)
options(mc.cores = parallel::detectCores())
library(dplyr) # oops
# this preliminary version uses dplyr, will soon take the time to move to a full base R version

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
} else if(length(grep("xiaomao", getwd()) > 0)) {
  setwd("/home/xiaomao/egret/analyses")  
} else if(length(grep("britanywuuu", getwd()) > 0)) {
  setwd("/Documents/ubc/year5/TemporalEcologyLab/egret/analyses")
} else if(length(grep("Ken", getwd())) > 0){
  setwd("/Users/Ken Michiko Samson/Documents/Temporal Ecology Lab/egret/analyses")
} else if(length(grep("christophe_rouleau-desrochers", getwd())) > 0){
  setwd("/Users/christophe_rouleau-desrochers/Documents/github/egret/analyses")
} else if(length(grep("victor", getwd())) > 0){
  setwd('~/projects/egret/analyses')
} 

d <- read.csv("output/usdaChillGermTemp.csv")

d$latbi[which(d$latbi == "Aronia_x prunifolia")] <-"Aronia_x_prunifolia"

# Checking for the unique response values
#result <- d %>%
#  dplyr::group_by(latbi, responseType, chillDuration, chillTemp, tempDay, tempNight, unspecTemp) %>%
#  dplyr::summarise(
#    unique_values = n_distinct(responseValue),
#    .groups = "drop"
#  )

#subset_result <- result %>% filter(unique_values > 1)

#Take only the max value for rows with same chilling and forcing, keep the ones with only one value
d <- d %>%
  dplyr::group_by(latbi,responseType, chillDuration, chillTemp, tempDay, tempNight, unspecTemp) %>%
  dplyr::filter(
    n_distinct(responseValue) == 1 | 
      responseValue == max(responseValue, na.rm = TRUE)
  ) %>%
  ungroup()


phylo <- ape::read.tree("output/usdaPhylogenyFull.tre")
#missing <- c("Quercus_falcata","Quercus_nigra","Quercus_chrysolepis", "Quercus_dumosa", "Quercus_ilicifolia","Quercus_imbricaria", "Quercus_pagoda","Quercus_shumardii,"Quercus_texana")

#d <- d[!d$latbi %in% missing,]
subby <- unique(d$latbi)

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

# numsp
# Prepare data for Stan - chilling hours between -20 and 10
d$numspp = as.integer(factor(d$latbi, levels = colnames(cphy)))
d$responseValueProp <- d$responseValue/100
d$chillDurationS <- scale(d$chillDuration)
d$tempDayS <- scale(d$tempDay)

mdl.dataUSDA <- list(N_degen = sum(d$responseValueProp %in% c(0,1)),
                 N_prop = sum(d$responseValueProp>0 & d$responseValueProp<1),
                 
                 Nsp =  length(unique(d$latbi)),
                 sp_degen = array(d$numspp[d$responseValueProp %in% c(0,1)],
                                  dim = sum(d$responseValueProp%in% c(0,1))),
                 sp_prop = array(d$numspp[d$responseValueProp>0 & d$responseValueProp<1],
                                 dim = sum(d$responseValueProp>0 & d$responseValueProp<1)),
                 
                 y_degen = array(d$responseValueProp[d$responseValueProp %in% c(0,1)],
                                 dim = sum(d$responseValueProp%in% c(0,1))),
                 y_prop = array(d$responseValueProp[d$responseValueProp>0 & d$responseValueProp<1],
                                dim = sum(d$responseValueProp>0 & d$responseValueProp<1)),
                 
                 c_degen = array(d$chillDurationS[d$responseValueProp %in% c(0,1)],
                                 dim = sum(d$responseValueProp%in% c(0,1))),
                 c_prop = array(d$chillDurationS[d$responseValueProp>0 & d$responseValueProp<1],
                                dim = sum(d$responseValueProp>0 & d$responseValueProp<1)),
                 
                 f_degen = array(d$tempDayS[d$responseValueProp %in% c(0,1)],
                                 dim = sum(d$responseValueProp%in% c(0,1))),
                 f_prop = array(d$tempDayS[d$responseValueProp>0 & d$responseValueProp<1],
                                dim = sum(d$responseValueProp>0 & d$responseValueProp<1)),
                 Vphy = cphy)

# Compile and run model
smordbeta <-stan_model("stan/orderedbetalikelihood_2slopes.stan")
fit <- sampling(smordbeta, mdl.dataUSDA, 
                iter = 4000, warmup = 3000,
                chains = 4)

summ <- data.frame(summary(fit)[["summary"]])
sampler_params  <- get_sampler_params(fit, inc_warmup = FALSE)
diagnostics <- list(
  max_treedepth= max(sapply(sampler_params, function(x) max(x[, "treedepth__"]))),
  max_divergence = max(sapply(sampler_params, function(x) sum(x[, "divergent__"]))),
  max_rhat = max(summ$Rhat, na.rm = TRUE),
  min_ess = min(summ$n_eff, na.rm = TRUE)
)

saveRDS(fit, file = 'analyseBudSeed/output/fit_usda.rds')
saveRDS(summ, file = 'analyseBudSeed/output/summary_usda.rds')
saveRDS(diagnostics, file = 'analyseBudSeed/output/diagnostics_usda.rds')

diagnostic <- FALSE
if(diagnostic){
source('modeling/mcmc_analysis_tools_rstan.R', local=util)

samples <- util$extract_expectand_vals(fit)
f_names <- sapply(1:mdl.dataUSDA$N_degen,
                  function(n) paste0('y_degen_gen[', n, ']'))
d$y_gen_mean <- NA

d[d$responseValueProp %in% c(0,1), 'y_gen_mean'] <-
  sapply(f_names, function(n){
    mean(rowMeans(samples[[n]]))})


f_names <- sapply(1:mdl.data$N_prop,
                  function(n) paste0('y_prop_gen[', n, ']'))
modeld[modeld$responseValue>0 & modeld$responseValue<1, 'y_gen_mean'] <-
  sapply(f_names, function(n){
    mean(rowMeans(samples[[n]]))})

library(paletteer)
nColor <- 100
colors = paletteer_c("viridis::inferno", n=nColor)
rank <- as.factor( as.numeric( cut(ppcheck$numspp, nColor)))

ppcheck <- d[c('numspp', 'responseValueProp', 'y_gen_mean', 'tempDayS', 'chillDurationS')]
plot(ppcheck$responseValueProp ~ ppcheck$y_gen_mean, 
     col = colors[ rank ], cex = 0.5)
abline(a=0, b=1, col = 'white', lwd = 4)
abline(a=0, b=1, col = 'black', lwd = 1)



s <- sample(unique(ppcheck$numspp), 1)
ppchecks <- ppcheck[ppcheck$numspp ==s, ]
plot(ppchecks$responseValue ~ ppchecks$time, 
     col = 'grey', cex = 1)
points(ppchecks$y_gen_mean ~ ppchecks$time, cex = 0.5)



sp <- 94
idxs_degen <- which(mdl.data$sp_degen == sp)
idxs_prop <- which(mdl.data$sp_prop == sp)
y_prop_names <- sapply(idxs_prop, function(n) paste0('y_prop_gen[', n, ']'))



par(mfrow=c(1, 1))
plot.new()
xlim=c(0, 1)
ylim=c(0, 1)
abline(a=0, b=1, col = 'white', lwd = 4)
abline(a=0, b=1, col = 'black', lwd = 1)
points(x= mdl.data$y_prop[idxs_prop], y =  summ[y_prop_names, 'mean'], pch=16, cex=1.0, col="white")
points(x= mdl.data$y_prop[idxs_prop], y =  summ[y_prop_names, 'mean'], pch=16, cex=0.8, col="black")

sub <- modeld[modeld$numspp == sp, ]
sub <- d[d$genusspecies == 'Clematis_vitalba',]

par(mfrow=c(1, 3))
mdl.data$dataID_prop <- as.integer(factor(modeld$datasetID[modeld$responseValue>0 & modeld$responseValue<1]))
idxs_prop <- which(mdl.data$sp_prop == sp)

for(id in unique(mdl.data$dataID_prop[idxs_prop])){
  
  idxs_prop_sp <- which(mdl.data$dataID_prop[idxs_prop] == id)
  plot(mdl.data$y_prop[idxs_prop_sp] ~ mdl.data$t_prop[idxs_prop_sp], pch = 16, cex = 0.5,
       col = unique(mdl.data$dataID_prop[idxs_prop_sp]))
  
  
}

length(unique(modeld$numspp))



summ_df <- data.frame( bf.mean = summ[paste0("bf[", 1:mdl.data$Nsp, "]"), "mean"],
                       bf.q2.5 = summ[paste0("bf[", 1:mdl.data$Nsp, "]"), "X2.5."],
                       bf.q97.5 = summ[paste0("bf[", 1:mdl.data$Nsp, "]"), "X97.5."],
                       bf.q25 = summ[paste0("bf[", 1:mdl.data$Nsp, "]"), "X25."],
                       bf.q75 = summ[paste0("bf[", 1:mdl.data$Nsp, "]"), "X75."],
                       bt.mean = summ[paste0("bt[", 1:mdl.data$Nsp, "]"), "mean"],
                       bt.q2.5 = summ[paste0("bt[", 1:mdl.data$Nsp, "]"), "X2.5."],
                       bt.q97.5 = summ[paste0("bt[", 1:mdl.data$Nsp, "]"), "X97.5."],
                       bt.q25 = summ[paste0("bt[", 1:mdl.data$Nsp, "]"), "X25."],
                       bt.q75 = summ[paste0("bt[", 1:mdl.data$Nsp, "]"), "X75."],
                       species = colnames(cphy))

# Add baskin data, at species-level, or at genus-level when required (= the most frequent in the genus)

summ_df <- data.frame( lambda.mean = summ[paste0("lambda_", c('bf',  'bt')), "mean"],
                       lambda.q2.5 = summ[paste0("lambda_", c('bf',  'bt')), "X2.5."],
                       lambda.q97.5 = summ[paste0("lambda_", c('bf',  'bt')), "X97.5."],
                       lambda.q25 = summ[paste0("lambda_", c('bf',  'bt')), "X25."],
                       lambda.q75 = summ[paste0("lambda_", c('bf',  'bt')), "X75."],
                       sigma.mean = summ[paste0("sigma_", c('bf',  'bt')), "mean"],
                       sigma.q2.5 = summ[paste0("sigma_", c('bf',  'bt')), "X2.5."],
                       sigma.q97.5 = summ[paste0("sigma_", c('bf',  'bt')), "X97.5."],
                       sigma.q25 = summ[paste0("sigma_", c('bf',  'bt')), "X25."],
                       sigma.q75 = summ[paste0("sigma_", c('bf',  'bt')), "X75."],
                       broot.mean = summ[paste0("b", c('f',  't'), '_z'), "mean"],
                       broot.q2.5 = summ[paste0("b", c('f',  't'), '_z'), "X2.5."],
                       broot.q97.5 = summ[paste0("b", c('f',  't'), '_z'), "X97.5."],
                       broot.q25 = summ[paste0("b", c('f',  't'), '_z'), "X25."],
                       broot.q75 = summ[paste0("b", c('f',  't'), '_z'), "X75."],
                       aroot.mean = summ['a_z', "mean"],
                       aroot.q2.5 = summ['a_z', "X2.5."],
                       aroot.q97.5 = summ['a_z', "X97.5."],
                       aroot.q25 = summ['a_z', "X25."],
                       aroot.q75 = summ['a_z', "X75."],
                       var = c('forcing',  'chill'))

ggplot(data = summ_df) +
  geom_pointrange(aes(xmin = lambda.q25, xmax = lambda.q75, x = lambda.mean, y = var,
                      col = ifelse(lambda.q25 > 0, 'pos', ifelse(lambda.q97.5 < 0, 'neg', 'nothing'))), 
                  size = 0.07, linewidth = 0.7) +
  geom_pointrange(aes(xmin = lambda.q2.5, xmax = lambda.q97.5, x = lambda.mean, y = var,
                      col = ifelse(lambda.q25 > 0, 'pos', ifelse(lambda.q97.5 < 0, 'neg', 'nothing'))), 
                  size = 0.05, linewidth = 0.5, alpha = 0.5) +
  geom_point(aes(x = lambda.mean, y = var), col = 'white', size = 0.01) +
  theme_bw() +
  scale_color_manual(values = c('#a7495c', 'grey', '#498ba7'), breaks = c('neg', 'nothing', 'pos')) +
  theme(legend.position = 'none', 
        axis.title.y = element_blank(), axis.text.y = element_text(size = 7),
        axis.ticks.y = element_blank(), panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank()) +
  labs(x = "Lambda")

ggplot(data = summ_df) +
  geom_pointrange(aes(xmin = sigma.q25, xmax = sigma.q75, x = sigma.mean, y = var,
                      col = ifelse(sigma.q25 > 0, 'pos', ifelse(sigma.q97.5 < 0, 'neg', 'nothing'))), 
                  size = 0.07, linewidth = 0.7) +
  geom_pointrange(aes(xmin = sigma.q2.5, xmax = sigma.q97.5, x = sigma.mean, y = var,
                      col = ifelse(sigma.q25 > 0, 'pos', ifelse(sigma.q97.5 < 0, 'neg', 'nothing'))), 
                  size = 0.05, linewidth = 0.5, alpha = 0.5) +
  geom_point(aes(x = sigma.mean, y = var), col = 'white', size = 0.01) +
  theme_bw() +
  scale_color_manual(values = c('#a7495c', 'grey', '#498ba7'), breaks = c('neg', 'nothing', 'pos')) +
  theme(legend.position = 'none', 
        axis.title.y = element_blank(), axis.text.y = element_text(size = 7),
        axis.ticks.y = element_blank(), panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank()) +
  labs(x = "Sigma")

ggplot(data = summ_df) +
  geom_pointrange(aes(xmin = broot.q25, xmax = broot.q75, x = broot.mean, y = var,
                      col = ifelse(broot.q25 > 0, 'pos', ifelse(broot.q75 < 0, 'neg', 'nothing'))), 
                  size = 0.07, linewidth = 0.7) +
  geom_pointrange(aes(xmin = broot.q2.5, xmax = broot.q97.5, x = broot.mean, y = var,
                      col = ifelse(broot.q25 > 0, 'pos', ifelse(broot.q75 < 0, 'neg', 'nothing'))), 
                  size = 0.05, linewidth = 0.5, alpha = 0.5) +
  geom_point(aes(x = broot.mean, y = var), col = 'white', size = 0.01) +
  theme_bw() +
  scale_color_manual(values = c('#a7495c', 'grey', '#498ba7'), breaks = c('neg', 'nothing', 'pos')) +
  theme(legend.position = 'none', 
        axis.title.y = element_blank(), axis.text.y = element_text(size = 7),
        axis.ticks.y = element_blank(), panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank()) +
  labs(x = "Root slope")


ggplot(data = summ_df) +
  geom_pointrange(aes(xmin = aroot.q25, xmax = aroot.q75, x = aroot.mean, y = var,
                      col = ifelse(aroot.q25 > 0, 'pos', ifelse(aroot.q75 < 0, 'neg', 'nothing'))), 
                  size = 0.07, linewidth = 0.7) +
  geom_pointrange(aes(xmin = aroot.q2.5, xmax = aroot.q97.5, x = aroot.mean, y = var,
                      col = ifelse(aroot.q25 > 0, 'pos', ifelse(aroot.q75 < 0, 'neg', 'nothing'))), 
                  size = 0.05, linewidth = 0.5, alpha = 0.5) +
  geom_point(aes(x = broot.mean, y = var), col = 'white', size = 0.01) +
  theme_bw() +
  scale_color_manual(values = c('#a7495c', 'grey', '#498ba7'), breaks = c('neg', 'nothing', 'pos')) +
  theme(legend.position = 'none', 
        axis.title.y = element_blank(), axis.text.y = element_text(size = 7),
        axis.ticks.y = element_blank(), panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank()) +
  labs(x = "Root intercept")

}