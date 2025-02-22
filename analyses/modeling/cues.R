# Started Feb 12, 2025
# by Victor

library(stringr)
library(ape)
library(phytools)
library(rstan)

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
  setwd("/Users/christophe_rouleau-desrochers/Documents/github/egret/analyses")
} else if(length(grep("victor", getwd())) > 0){
  setwd('~/projects/egret/analyses')
} 

# Load data
source('studyDesign/decisionRules.R')

# Prepare phylogeny
phylo <- ape::read.tree("output/phylogeny.tre")
gymno <- c('Pseudotsuga_menziesii', 'Pinus_roxburghii','Pinus_sylvestris','Pinus_halepensis',
           'Pinus_brutia','Pinus_canariensis','Pinus_bungeana','Pinus_koraiensis','Pinus_wallichiana',
           'Pinus_strobus','Picea_orientalis','Picea_abies','Picea_sitchensis','Picea_glauca',
           'Abies_amabilis','Abies_procera','Abies_grandis','Abies_nordmanniana','Abies_chensiensis',
           'Abies_lasiocarpa','Tsuga_heterophylla','Tsuga_mertensiana','Ginkgo_biloba', 'Juniperus_oxycedrus',
           'Juniperus_communis')
namesphy <- phylo$tip.label
phylo <- phytools::force.ultrametric(phylo, method="extend")
phylo$node.label <- seq(1,length(phylo$node.label),1)
ape::is.ultrametric(phylo)
plot(phylo, cex=0.7)
phylo <- ape::drop.tip(phylo, gymno) # exclude gymnosperms
plot(phylo, cex=0.7)
cphy <- ape::vcv.phylo(phylo,corr=TRUE)
rm(gymno)

# Process data 
modeld <- newd %>%
  dplyr::filter(!is.na(germDuration) & !is.na(germTempGen) & !is.na(dormancyDuration) & !is.na(dormancyTemp)) %>%
  dplyr::filter(germDuration != 'unknown') %>%
  dplyr::filter(genusspecies %in% phylo$tip.label) %>%
  dplyr::mutate(responseValue = as.numeric(responseValue)/100,
                germDuration = as.numeric(germDuration),
                germTempGen = as.numeric(germTempGen),
                dormancyDuration = as.numeric(dormancyDuration),
                dormancyTemp = as.numeric(dormancyTemp)) %>%
  dplyr::filter(responseValue < 1.05) %>%
  dplyr::mutate(responseValue = if_else(responseValue > 1, 1, responseValue),
                responseValue = if_else(responseValue < 0, 0, responseValue),
                germDuration = if_else(germDuration < 0, 0, germDuration),
                time = scale(germDuration)[,1],
                forcing = scale(germTempGen)[,1],
                chillh = scale(dormancyDuration * 24 * as.numeric(dormancyTemp < 10 & dormancyTemp > -20))[,1]) %>%
  dplyr::filter(!is.na(germDuration) & !is.na(germTempGen) & !is.na(dormancyDuration) & !is.na(dormancyTemp))

# Keep only needed data in phylo tree
spp <-  unique(modeld$genusspecies)
length(spp)
length(phylo$node.label)
phylo2 <- keep.tip(phylo, spp)
cphy <- vcv.phylo(phylo2,corr=TRUE)

# Prepare data for Stan
modeld$numspp = as.integer(factor(modeld$genusspecies, levels = colnames(cphy)))
mdl.data <- list(N_degen = sum(modeld$responseValue %in% c(0,1)),
                 N_prop = sum(modeld$responseValue>0 & modeld$responseValue<1),
                 
                 Nsp =  length(unique(modeld$numspp)),
                 sp_degen = array(modeld$numspp[modeld$responseValue %in% c(0,1)],
                                  dim = sum(modeld$responseValue%in% c(0,1))),
                 sp_prop = array(modeld$numspp[modeld$responseValue>0 & modeld$responseValue<1],
                                 dim = sum(modeld$responseValue>0 & modeld$responseValue<1)),
                 
                 y_degen = array(modeld$responseValue[modeld$responseValue %in% c(0,1)],
                                dim = sum(modeld$responseValue%in% c(0,1))),
                 y_prop = array(modeld$responseValue[modeld$responseValue>0 & modeld$responseValue<1],
                                dim = sum(modeld$responseValue>0 & modeld$responseValue<1)),
                 
                 t_degen = array(modeld$time[modeld$responseValue %in% c(0,1)],
                                 dim = sum(modeld$responseValue%in% c(0,1))),
                 t_prop = array(modeld$time[modeld$responseValue>0 & modeld$responseValue<1],
                              dim = sum(modeld$responseValue>0 & modeld$responseValue<1)),
                 
                 f_degen = array(modeld$forcing[modeld$responseValue %in% c(0,1)],
                                 dim = sum(modeld$responseValue%in% c(0,1))),
                 f_prop = array(modeld$forcing[modeld$responseValue>0 & modeld$responseValue<1],
                                dim = sum(modeld$responseValue>0 & modeld$responseValue<1)),
                 
                 c_degen = array(modeld$chillh[modeld$responseValue %in% c(0,1)],
                                 dim = sum(modeld$responseValue%in% c(0,1))),
                 c_prop = array(modeld$chillh[modeld$responseValue>0 & modeld$responseValue<1],
                                dim = sum(modeld$responseValue>0 & modeld$responseValue<1)),
                 
                 Vphy = cphy)

# Compile and run model
smordbeta <- stan_model("stan/orderedbetalikelihood_3slopes.stan")
fit <- sampling(smordbeta, mdl.data, 
                iter = 4000, warmup = 3000,
                chains = 4, cores = 4)
summ <- data.frame(summary(fit)[["summary"]])
sampler_params  <- get_sampler_params(fit, inc_warmup = FALSE)
diagnostics <- list(
  max_treedepth= max(sapply(sampler_params, function(x) max(x[, "treedepth__"]))),
  max_divergence = max(sapply(sampler_params, function(x) sum(x[, "divergent__"]))),
  max_rhat = max(summ$Rhat, na.rm = TRUE),
  min_ess = min(summ$n_eff, na.rm = TRUE)
)
saveRDS(fit, file = 'modeling/output/3slopes/fit.rds')
saveRDS(summ, file = 'modeling/output/3slopes/summary.rds')
saveRDS(diagnostics, file = 'modeling/output/3slopes/diagnostics.rds')

# Make some plots
summ_df <- data.frame( bf.mean = summ[paste0("bf[", 1:mdl.data$Nsp, "]"), "mean"],
                       bf.q2.5 = summ[paste0("bf[", 1:mdl.data$Nsp, "]"), "X2.5."],
                       bf.q97.5 = summ[paste0("bf[", 1:mdl.data$Nsp, "]"), "X97.5."],
                       bf.q25 = summ[paste0("bf[", 1:mdl.data$Nsp, "]"), "X25."],
                       bf.q75 = summ[paste0("bf[", 1:mdl.data$Nsp, "]"), "X75."],
                       bc.mean = summ[paste0("bc[", 1:mdl.data$Nsp, "]"), "mean"],
                       bc.q2.5 = summ[paste0("bc[", 1:mdl.data$Nsp, "]"), "X2.5."],
                       bc.q97.5 = summ[paste0("bc[", 1:mdl.data$Nsp, "]"), "X97.5."],
                       bc.q25 = summ[paste0("bc[", 1:mdl.data$Nsp, "]"), "X25."],
                       bc.q75 = summ[paste0("bc[", 1:mdl.data$Nsp, "]"), "X75."],
                       bt.mean = summ[paste0("bt[", 1:mdl.data$Nsp, "]"), "mean"],
                       bt.q2.5 = summ[paste0("bt[", 1:mdl.data$Nsp, "]"), "X2.5."],
                       bt.q97.5 = summ[paste0("bt[", 1:mdl.data$Nsp, "]"), "X97.5."],
                       bt.q25 = summ[paste0("bt[", 1:mdl.data$Nsp, "]"), "X25."],
                       bt.q75 = summ[paste0("bt[", 1:mdl.data$Nsp, "]"), "X75."],
                       species = colnames(cphy))

# Add baskin data, at species-level, or at genus-level when required (= the most frequent in the genus)
baskin <- read.csv('output/baskinegretclean.csv')
baskin$species <- gsub(" ", "_", baskin$Genus_species)

baskin.aggr <- baskin %>%
  dplyr::group_by(species) %>%
  dplyr::reframe(newclass = paste(Dormancy.Class, collapse='')) %>%
  dplyr::select(species, newclass)
summ_df <- merge(summ_df, baskin.aggr[c("species", "newclass")], all.x = TRUE)

baskin.genus <- baskin %>%
  dplyr::add_count(Corrected.Genus, Dormancy.Class) %>%
  dplyr::group_by(Corrected.Genus) %>%
  summarise(newclass = Dormancy.Class[n == max(n)][1]) %>% # most frequent per Genus
  dplyr::select(Corrected.Genus, newclass)
names(baskin.genus) <- c('genus', 'newclass.genus')
summ_df$genus <- gsub("_.*$", "", summ_df$species)
summ_df <- merge(summ_df, baskin.genus[c("genus", "newclass.genus")], all.x = TRUE)
summ_df$dormancyclass <- ifelse(!is.na(summ_df$newclass), summ_df$newclass, summ_df$newclass.genus)

summ_df$dormancyclass <- ifelse(summ_df$dormancyclass %in% c("PDPD"), "PD", summ_df$dormancyclass)
summ_df$dormancyclass <- ifelse(summ_df$dormancyclass %in% c("MPDPD"), "MPD", summ_df$dormancyclass)
summ_df$dormancyclass <- ifelse(summ_df$dormancyclass %in% c("PYPDPY"), "PYPD", summ_df$dormancyclass)
summ_df$dormancyclass <- ifelse(!(summ_df$dormancyclass %in% c("ND")) & grepl('ND', summ_df$dormancyclass), "Unclear", summ_df$dormancyclass)
summ_df$dormancyclass <- ifelse(is.na(summ_df$dormancyclass), "Unknown", summ_df$dormancyclass)
unique(summ_df$dormancyclass)

summ_df$dormancyclass.simp <- ifelse(summ_df$dormancyclass %in% c("PD", "MD"), "Endogenous", 
                                     ifelse(summ_df$dormancyclass %in% c("PY"), "Exogenous", 
                                            ifelse(summ_df$dormancyclass %in% c("MPD", "PYPD"), "Mixed", summ_df$dormancyclass))) 
summ_df$dormancyclass.simp <- ifelse(summ_df$dormancyclass %in% c("ND"), "Non-dormant", summ_df$dormancyclass.simp)
summ_df$dormancyclass.simp <- ifelse(summ_df$dormancyclass %in% c("Unknown", "Unclear"), "Unclear, unknown", summ_df$dormancyclass.simp)
unique(summ_df$dormancyclass.simp)



forcingeffect <- ggplot(data = arrange(summ_df,desc(bf.mean))) +
  geom_pointrange(aes(xmin = bf.q25, xmax = bf.q75, x = bf.mean, y = species,
                      col = ifelse(bf.q25 > 0, 'pos', ifelse(bf.q97.5 < 0, 'neg', 'nothing'))), 
                  size = 0.07, linewidth = 0.7) +
  geom_pointrange(aes(xmin = bf.q2.5, xmax = bf.q97.5, x = bf.mean, y = species,
                      col = ifelse(bf.q25 > 0, 'pos', ifelse(bf.q97.5 < 0, 'neg', 'nothing'))), 
                  size = 0.05, linewidth = 0.5, alpha = 0.5) +
  geom_point(aes(x = bf.mean, y = species), col = 'white', size = 0.01) +
  theme_bw() +
  scale_color_manual(values = c('#a7495c', 'grey', '#498ba7'), breaks = c('neg', 'nothing', 'pos')) +
  theme(legend.position = 'none', 
        axis.title.y = element_blank(), axis.text.y = element_text(size = 5),
        axis.ticks.y = element_blank(), panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank()) +
  labs(x = "Forcing effect")

forcingeffect_baskin <- ggplot(data = arrange(summ_df,desc(bf.mean))) +
  geom_vline(aes(xintercept = 0), col = "grey90", linetype = "dashed") +
  geom_pointrange(aes(xmin = bf.q25, xmax = bf.q75, x = bf.mean, y = factor(species,level = arrange(summ_df,desc(bf.mean))$species),
                      col = dormancyclass.simp), 
                  size = 0.07, linewidth = 0.7) +
  geom_pointrange(aes(xmin = bf.q2.5, xmax = bf.q97.5, x = bf.mean, y = factor(species,level = arrange(summ_df,desc(bf.mean))$species),
                      col = dormancyclass.simp), 
                  size = 0.05, linewidth = 0.5, alpha = 0.5) +
  geom_point(aes(x = bf.mean, y = factor(species,level = arrange(summ_df,desc(bf.mean))$species),), col = 'white', size = 0.01) +
  theme_bw() +
  scale_color_manual(values = c('#7fa688', '#ddb166', '#D98B65', "#6B95B2", "grey"), breaks = c('Endogenous', "Mixed", "Exogenous", "Non-dormant", "Unclear, unknown")) +
  scale_x_continuous(position = "top", limits = c(-3.6,3.6)) +
  theme(axis.title.y = element_blank(), axis.text.y = element_text(size = 5),
        axis.ticks.y = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x = element_text(size = 7), axis.text.x = element_text(size = 6),
        panel.grid = element_blank()) +
  theme(
    legend.title = element_blank(),
    legend.key.height  = unit(5, "pt"),
    legend.key.width  = unit(5, "pt"),
    legend.key.spacing.x = unit(1, "pt"),
    legend.text = element_text(size = 6.5, margin = margin(l = 1,  r = 1.5)),
    legend.position = "bottom", legend.margin = margin(t=0,b=0,l=0,r=0))+
  guides(col=guide_legend(nrow=2,byrow=FALSE)) +
  labs(x = "Forcing effect")

ggsave(forcingeffect_baskin, filename = "modeling/figures/cues/forcingeffect_baskin.pdf", height = 9, width = 3.5)

chillingeffect <- ggplot(data = summ_df) +
  geom_pointrange(aes(xmin = bc.q25, xmax = bc.q75, x = bc.mean, y = species,
                      col = ifelse(bc.q25 > 0, 'pos', ifelse(bc.q97.5 < 0, 'neg', 'nothing'))), 
                  size = 0.07, linewidth = 0.7) +
  geom_pointrange(aes(xmin = bc.q2.5, xmax = bc.q97.5, x = bc.mean, y = species,
                      col = ifelse(bc.q25 > 0, 'pos', ifelse(bc.q97.5 < 0, 'neg', 'nothing'))), 
                  size = 0.05, linewidth = 0.5, alpha = 0.5) +
  geom_point(aes(x = bc.mean, y = species), col = 'white', size = 0.01) +
  theme_bw() +
  scale_color_manual(values = c('#a7495c', 'grey', '#498ba7'), breaks = c('neg', 'nothing', 'pos')) +
  theme(legend.position = 'none', 
        axis.title.y = element_blank(), axis.text.y = element_text(size = 5),
        axis.ticks.y = element_blank(), panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank()) +
  labs(x = "Chilling effect")

chillingeffect_baskin <- ggplot(data = arrange(summ_df,desc(bc.mean))) +
  geom_vline(aes(xintercept = 0), col = "grey90", linetype = "dashed") +
  geom_pointrange(aes(xmin = bc.q25, xmax = bc.q75, x = bc.mean, y = factor(species,level = arrange(summ_df,desc(bc.mean))$species),
                      col = dormancyclass.simp), 
                  size = 0.07, linewidth = 0.7) +
  geom_pointrange(aes(xmin = bc.q2.5, xmax = bc.q97.5, x = bc.mean, y = factor(species,level = arrange(summ_df,desc(bc.mean))$species),
                      col = dormancyclass.simp), 
                  size = 0.05, linewidth = 0.5, alpha = 0.5) +
  geom_point(aes(x = bc.mean, y = factor(species,level = arrange(summ_df,desc(bc.mean))$species),), col = 'white', size = 0.01) +
  theme_bw() +
  scale_color_manual(values = c('#7fa688', '#ddb166', '#D98B65', "#6B95B2", "grey"), breaks = c('Endogenous', "Mixed", "Exogenous", "Non-dormant", "Unclear, unknown")) +
  scale_x_continuous(position = "top", limits = c(-6,6)) +
  theme(axis.title.y = element_blank(), axis.text.y = element_text(size = 5),
        axis.ticks.y = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x = element_text(size = 7), axis.text.x = element_text(size = 6),
        panel.grid = element_blank()) +
  theme(
    legend.title = element_blank(),
    legend.key.height  = unit(5, "pt"),
    legend.key.width  = unit(5, "pt"),
    legend.key.spacing.x = unit(1, "pt"),
    legend.text = element_text(size = 6.5, margin = margin(l = 1,  r = 1.5)),
    legend.position = "bottom", legend.margin = margin(t=0,b=0,l=0,r=0))+
  guides(col=guide_legend(nrow=2,byrow=FALSE)) +
  labs(x = "Chilling effect")

ggsave(chillingeffect_baskin, filename = "modeling/figures/cues/chillingeffect_baskin.pdf", height = 9, width = 3.5)

timeeffect <- ggplot(data = summ_df) +
  geom_pointrange(aes(xmin = bt.q25, xmax = bt.q75, x = bt.mean, y = species,
                      col = ifelse(bt.q25 > 0, 'pos', ifelse(bt.q97.5 < 0, 'neg', 'nothing'))), 
                  size = 0.07, linewidth = 0.7) +
  geom_pointrange(aes(xmin = bt.q2.5, xmax = bt.q97.5, x = bt.mean, y = species,
                      col = ifelse(bt.q25 > 0, 'pos', ifelse(bt.q97.5 < 0, 'neg', 'nothing'))), 
                  size = 0.05, linewidth = 0.5, alpha = 0.5) +
  geom_point(aes(x = bt.mean, y = species), col = 'white', size = 0.01) +
  theme_bw() +
  scale_color_manual(values = c('#a7495c', 'grey', '#498ba7'), breaks = c('neg', 'nothing', 'pos')) +
  theme(legend.position = 'none', 
        axis.title.y = element_blank(), axis.text.y = element_text(size = 5),
        axis.ticks.y = element_blank(), panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank()) +
  labs(x = "Chilling effect")

summ_df <- data.frame( lambda.mean = summ[paste0("lambda_", c('bf', 'bc', 'bt')), "mean"],
                       lambda.q2.5 = summ[paste0("lambda_", c('bf', 'bc', 'bt')), "X2.5."],
                       lambda.q97.5 = summ[paste0("lambda_", c('bf', 'bc', 'bt')), "X97.5."],
                       lambda.q25 = summ[paste0("lambda_", c('bf', 'bc', 'bt')), "X25."],
                       lambda.q75 = summ[paste0("lambda_", c('bf', 'bc', 'bt')), "X75."],
                       sigma.mean = summ[paste0("sigma_", c('bf', 'bc', 'bt')), "mean"],
                       sigma.q2.5 = summ[paste0("sigma_", c('bf', 'bc', 'bt')), "X2.5."],
                       sigma.q97.5 = summ[paste0("sigma_", c('bf', 'bc', 'bt')), "X97.5."],
                       sigma.q25 = summ[paste0("sigma_", c('bf', 'bc', 'bt')), "X25."],
                       sigma.q75 = summ[paste0("sigma_", c('bf', 'bc', 'bt')), "X75."],
                       broot.mean = summ[paste0("b", c('f', 'c', 't'), '_z'), "mean"],
                       broot.q2.5 = summ[paste0("b", c('f', 'c', 't'), '_z'), "X2.5."],
                       broot.q97.5 = summ[paste0("b", c('f', 'c', 't'), '_z'), "X97.5."],
                       broot.q25 = summ[paste0("b", c('f', 'c', 't'), '_z'), "X25."],
                       broot.q75 = summ[paste0("b", c('f', 'c', 't'), '_z'), "X75."],
                       aroot.mean = summ['a_z', "mean"],
                       aroot.q2.5 = summ['a_z', "X2.5."],
                       aroot.q97.5 = summ['a_z', "X97.5."],
                       aroot.q25 = summ['a_z', "X25."],
                       aroot.q75 = summ['a_z', "X75."],
                       var = c('forcing', 'chilling', 'time'))

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
