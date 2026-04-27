library(bayesplot)
library(ggplot2)
library(posterior)

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

source("analyseBudSeed/prepEgretUsda.R")
# removing the rows with incomplete data:
d <- d[complete.cases(d),] 

util <- new.env()
source('mcmc_analysis_tools_rstan.R', local=util)
source('mcmc_visualization_tools.R', local=util)

# to better see phylogenetic structure, ordering species by order on phylogeny
phylo <- ape::read.tree("output/usdaEgretFull.tre")
tipsGym <- getDescendants(phylo, node = 1264)
tipsGym <- tipsGym[tipsGym <= Ntip(phylo)]

# Get only angio
angioPhy <- drop.tip(phylo, phylo$tip.label[tipsGym])

# Get only gymno
gymPhy <- keep.tip(phylo, phylo$tip.label[tipsGym])

angio <- d[d$latbi %in% angioPhy$tip.label, ]
gym <- d[d$latbi %in% gymPhy$tip.label, ]


### for full dataset
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

## Angiosperm
fit <- readRDS("analyseBudSeed/output/fit_full_angio.rds")
summ <- readRDS("analyseBudSeed/output/summary_full_angio.rds")
diagnostics <- readRDS("analyseBudSeed/output/diagnostics_full_angio.rds")

samples <- util$extract_expectand_vals(fit)

base_samples <- util$filter_expectands(samples,
                                       c('a_z', 'lambda_a', 'sigma_a', 'a', 'bf_z', 'lambda_bf', 'sigma_bf', 'bf','cutpoints', 'kappa'),
                                       check_arrays=TRUE)
util$check_all_expectand_diagnostics(base_samples)

# Retrodictive check
par(mfrow=c(1, 1), mar = c(4,4,2,2))
names <- c(sapply(1:mdl.dataUSDA$N_prop, function(n) paste0('y_prop_gen[',n,']')),
           sapply(1:mdl.dataUSDA$N_degen, function(n) paste0('y_degen_gen[',n,']')))
preds <- samples[names]
names(preds) <- sapply(1:length(preds), function(n) paste0('y_gen[',n,']'))
util$plot_hist_quantiles(preds, 'y_gen', 0, 1, 0.1,
                         baseline_values=c(mdl.dataUSDA$y_prop, mdl.dataUSDA$y_degen), 
                         xlab="Germination perc.")

# Posterior inference
par(mfrow=c(3, 2), mar = c(4,4,1,1))
util$plot_expectand_pushforward(samples[['sigma_a']], 20,
                                flim = c(0,6),
                                display_name="sigma_a")
util$plot_expectand_pushforward(samples[['sigma_bc']], 20,
                                flim = c(0,6),
                                display_name="sigma_bc")
util$plot_expectand_pushforward(samples[['sigma_bf']], 20,
                                flim = c(0,6),
                                display_name="sigma_bf")
util$plot_expectand_pushforward(samples[['a_z']], 20,
                                display_name = "a_z")
util$plot_expectand_pushforward(samples[['bc_z']], 20,
                                display_name = "bc_z")
util$plot_expectand_pushforward(samples[['bf_z']], 20,
                                display_name = "bf_z")

species_names <- tapply(da$latbi, da$numspp, unique)
species_names <- as.character(species_names)

# Create a data frame that maps the index to the Name
lookup <- data.frame(
  index = 1:length(species_names),
  species_name = species_names
)

# Plot for chilling
parameter_bc <- c("bc_z", names(fit)[grep("^bc\\[", names(fit))])
stats <- summary(fit, pars = parameter_bc, probs = c(0.25, 0.75))$summary

df_bc <- as.data.frame(stats)
df_bc$parameter <- rownames(df_bc)

colnames(df_bc)[grep("25%", colnames(df_bc))] <- "low"
colnames(df_bc)[grep("75%", colnames(df_bc))] <- "high"


df_bc$is_mean <- ifelse(df_bc$parameter == "bc_z", "Global Mean", "Species")

# making new columns to seperate global mean and sp level mean
df_bc$index <- as.numeric(gsub("\\D", "", df_bc$parameter))
df_bc$name <- ifelse(is.na(df_bc$index), 
                     "Global Mean", 
                     species_names[df_bc$index])
df_bc <- df_bc[order(df_bc$index), ]
df_bc <- rbind(
  df_bc[df_bc$name == "Global Mean", ],
  df_bc[df_bc$name != "Global Mean", ]
)
df_bc$name <- factor(df_bc$name, levels = rev(df_bc$name))

pdf("analyseBudSeed/figures/fullChillingAngio.pdf", width = 20, height = 50)
ggplot(df_bc, aes(x = mean, y = name)) +
  geom_errorbar(aes(xmin = low, xmax = high, color = is_mean), 
                width = 0,
                linewidth = 1.5) +
  geom_point(size = 2.5) +
  
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", alpha = 0.5) +
  
  scale_color_manual(values = c("Global Mean" = "firebrick", "Species" = "black")) +
  labs(title = "bc",
       x = "Posterior Estimate", y = NULL) +
  theme_minimal()
dev.off()

# Plot for forcing
parameter_bf <- c("bf_z", names(fit)[grep("^bf\\[", names(fit))])
stats <- summary(fit, pars = parameter_bf, probs = c(0.25, 0.75))$summary

df_bf <- as.data.frame(stats)
df_bf$parameter <- rownames(df_bf)

colnames(df_bf)[grep("25%", colnames(df_bf))] <- "low"
colnames(df_bf)[grep("75%", colnames(df_bf))] <- "high"


df_bf$is_mean <- ifelse(df_bf$parameter == "bf_z", "Global Mean", "Species")

df_bf$index <- as.numeric(gsub("\\D", "", df_bf$parameter))

df_bf$name <- ifelse(is.na(df_bf$index), 
                     "Global Mean", 
                     species_names[df_bf$index])
df_bf <- df_bf[order(df_bf$name), ]
df_bf <- rbind(
  df_bf[df_bf$name == "Global Mean", ],
  df_bf[df_bf$name != "Global Mean", ]
)
df_bf$name <- factor(df_bf$name, levels = rev(df_bf$name))


pdf("C:/PhD/Project/egret/analyses/analyseBudSeed/figures/fullForcingAngio.pdf", width = 20, height = 50)
ggplot(df_bf, aes(x = mean, y = name)) +
  geom_errorbar(aes(xmin = low, xmax = high, color = is_mean), 
                width = 0,
                linewidth = 1.5) +
  geom_point(size = 2.5) +
  
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", alpha = 0.5) +
  
  scale_color_manual(values = c("Global Mean" = "firebrick", "Species" = "black")) +
  labs(title = "bf",
       x = "Posterior Estimate", y = NULL) +
  theme_minimal()
dev.off()

# intercept
parameter_a <- c("a_z", names(fit)[grep("^a\\[", names(fit))])
stats <- summary(fit, pars = parameter_a, probs = c(0.25, 0.75))$summary

df_a <- as.data.frame(stats)
df_a$parameter <- rownames(df_a)

colnames(df_a)[grep("25%", colnames(df_a))] <- "low"
colnames(df_a)[grep("75%", colnames(df_a))] <- "high"


df_a$is_mean <- ifelse(df_a$parameter == "a_z", "Global Mean", "Species")

df_a$index <- as.numeric(gsub("\\D", "", df_a$parameter))

df_a$name <- ifelse(is.na(df_a$index), 
                     "Global Mean", 
                     species_names[df_a$index])
df_a <- df_a[order(df_a$index), ]
df_a <- rbind(
  df_a[df_a$name == "Global Mean", ],
  df_a[df_a$name != "Global Mean", ]
)
df_a$name <- factor(df_a$name, levels = rev(df_a$name))


pdf("analyseBudSeed/figures/fullInterceptAngio.pdf", width = 20, height = 50)
ggplot(df_a, aes(x = mean, y = name)) +
  geom_errorbar(aes(xmin = low, xmax = high, color = is_mean), 
                width = 0,
                linewidth = 1.5) +
  geom_point(size = 2.5) +
  
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", alpha = 0.5) +
  
  scale_color_manual(values = c("Global Mean" = "firebrick", "Species" = "black")) +
  labs(title = "a",
       x = "Posterior Estimate", y = NULL) +
  theme_minimal()
dev.off()

# Lambda
parameter_lambda <- c(names(fit)[grep("lambda", names(fit))])
stats <- summary(fit, pars = parameter_lambda, probs = c(0.25, 0.75))$summary
lambda <- as.data.frame(stats)
lambda$parameter <- rownames(lambda)
colnames(lambda)[grep("25%", colnames(lambda))] <- "low"
colnames(lambda)[grep("75%", colnames(lambda))] <- "high"

pdf("C:/PhD/Project/egret/analyses/analyseBudSeed/figures/lambdaAngio.pdf", width = 20, height = 20)
ggplot(lambda, aes(x = mean, y = parameter)) +
  geom_point(size = 2, alpha = 1) + 
  geom_errorbar(aes(xmin = low, 
                    xmax = high), 
                width = 0, alpha = 1, linewidth = 1) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  labs(y = "", x = "Lambda values") +
  theme(
    axis.text.y = element_blank(), axis.ticks.y = element_blank(),
    legend.title = element_text(size = 12, face = "bold"),  
    legend.text = element_text(size = 10),                  
    legend.key.size = unit(1.5, "lines"), legend.position = "right"             
  ) +
  theme_minimal() +
  scale_y_discrete(limits = rev)  
dev.off()

# visualize with raw data and predicted values from model for angio

y_prop_hat  <- colMeans(rstan::extract(fit)$y_prop_gen)
y_degen_hat <- colMeans(rstan::extract(fit)$y_degen_gen)

df_prop <- data.frame(
  chill     = mdl.dataAngio$c_prop,
  observed  = mdl.dataAngio$y_prop,
  predicted = y_prop_hat,
  species_idx = mdl.dataAngio$sp_prop
)

df_degen <- data.frame(
  chill     = mdl.dataAngio$c_degen,
  observed  = mdl.dataAngio$y_degen,
  predicted = y_degen_hat,
  species_idx = mdl.dataAngio$sp_degen
)

all_data <- rbind(df_prop, df_degen)

all_data$species_name <- species_names[all_data$species_idx]


pdf("analyseBudSeed/figures/predictedRawAngio.pdf", width = 14, height = 11)

par(mfrow = c(4, 5))

unique_spp <- unique(all_data$species_name)

for (i in 1:length(unique_spp)) {
  
  sp_data <- all_data[all_data$species_name == unique_spp[i], ]
  plot(sp_data$chill, sp_data$observed, 
       pch = 16, col = "black",
       ylim = c(0, 1),
       xlab = "Chilling", ylab = "Response",
       main = unique_spp[i],
       cex.main = 0.8)
  points(sp_data$chill, sp_data$predicted, 
         col = "blue", pch = 1, cex = 1.2)
}


dev.off()



## Gymnosperm
dg <- gym
phylo <- gymPhy
subby <- unique(dg$latbi)

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

dg$numspp = as.integer(factor(dg$latbi, levels = colnames(cphy)))

dg$chillDurationS <- scale(dg$chillDuration)
dg$tempDayS <- scale(dg$germTempGen)

fit <- readRDS("analyseBudSeed/output/fit_full_gymno.rds")
summ <- readRDS("analyseBudSeed/output/summary_full_gymno.rds")
diagnostics <- readRDS("analyseBudSeed/output/diagnostics_full_gymno.rds")

samples <- util$extract_expectand_vals(fit)

base_samples <- util$filter_expectands(samples,
                                       c('a_z', 'lambda_a', 'sigma_a', 'a', 'bf_z', 'lambda_bf', 'sigma_bf', 'bf','cutpoints', 'kappa'),
                                       check_arrays=TRUE)
util$check_all_expectand_diagnostics(base_samples)

# Retrodictive check
par(mfrow=c(1, 1), mar = c(4,4,2,2))
names <- c(sapply(1:mdl.dataUSDA$N_prop, function(n) paste0('y_prop_gen[',n,']')),
           sapply(1:mdl.dataUSDA$N_degen, function(n) paste0('y_degen_gen[',n,']')))
preds <- samples[names]
names(preds) <- sapply(1:length(preds), function(n) paste0('y_gen[',n,']'))
util$plot_hist_quantiles(preds, 'y_gen', 0, 1, 0.1,
                         baseline_values=c(mdl.dataUSDA$y_prop, mdl.dataUSDA$y_degen), 
                         xlab="Germination perc.")

# Posterior inference
par(mfrow=c(3, 2), mar = c(4,4,1,1))
util$plot_expectand_pushforward(samples[['sigma_a']], 20,
                                flim = c(0,6),
                                display_name="sigma_a")
util$plot_expectand_pushforward(samples[['sigma_bc']], 20,
                                flim = c(0,6),
                                display_name="sigma_bc")
util$plot_expectand_pushforward(samples[['sigma_bf']], 20,
                                flim = c(0,6),
                                display_name="sigma_bf")
util$plot_expectand_pushforward(samples[['a_z']], 20,
                                display_name = "a_z")
util$plot_expectand_pushforward(samples[['bc_z']], 20,
                                display_name = "bc_z")
util$plot_expectand_pushforward(samples[['bf_z']], 20,
                                display_name = "bf_z")

species_names <- tapply(dg$latbi, dg$numspp, unique)
species_names <- as.character(species_names)

# Create a data frame that maps the index to the Name
lookup <- data.frame(
  index = 1:length(species_names),
  species_name = species_names
)

# Plot for chilling
parameter_bc <- c("bc_z", names(fit)[grep("^bc\\[", names(fit))])
stats <- summary(fit, pars = parameter_bc, probs = c(0.25, 0.75))$summary

df_bc <- as.data.frame(stats)
df_bc$parameter <- rownames(df_bc)

colnames(df_bc)[grep("25%", colnames(df_bc))] <- "low"
colnames(df_bc)[grep("75%", colnames(df_bc))] <- "high"


df_bc$is_mean <- ifelse(df_bc$parameter == "bc_z", "Global Mean", "Species")

# making new columns to seperate global mean and sp level mean
df_bc$index <- as.numeric(gsub("\\D", "", df_bc$parameter))
df_bc$name <- ifelse(is.na(df_bc$index), 
                     "Global Mean", 
                     species_names[df_bc$index])
df_bc <- df_bc[order(df_bc$index), ]
df_bc <- rbind(
  df_bc[df_bc$name == "Global Mean", ],
  df_bc[df_bc$name != "Global Mean", ]
)
df_bc$name <- factor(df_bc$name, levels = rev(df_bc$name))

pdf("analyseBudSeed/figures/fullChillingGymno.pdf", width = 20, height = 50)
ggplot(df_bc, aes(x = mean, y = name)) +
  geom_errorbar(aes(xmin = low, xmax = high, color = is_mean), 
                width = 0,
                linewidth = 1.5) +
  geom_point(size = 2.5) +
  
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", alpha = 0.5) +
  
  scale_color_manual(values = c("Global Mean" = "firebrick", "Species" = "black")) +
  labs(title = "bc",
       x = "Posterior Estimate", y = NULL) +
  theme_minimal()
dev.off()

# Plot for forcing
parameter_bf <- c("bf_z", names(fit)[grep("^bf\\[", names(fit))])
stats <- summary(fit, pars = parameter_bf, probs = c(0.25, 0.75))$summary

df_bf <- as.data.frame(stats)
df_bf$parameter <- rownames(df_bf)

colnames(df_bf)[grep("25%", colnames(df_bf))] <- "low"
colnames(df_bf)[grep("75%", colnames(df_bf))] <- "high"


df_bf$is_mean <- ifelse(df_bf$parameter == "bf_z", "Global Mean", "Species")

df_bf$index <- as.numeric(gsub("\\D", "", df_bf$parameter))

df_bf$name <- ifelse(is.na(df_bf$index), 
                     "Global Mean", 
                     species_names[df_bf$index])
df_bf <- df_bf[order(df_bf$name), ]
df_bf <- rbind(
  df_bf[df_bf$name == "Global Mean", ],
  df_bf[df_bf$name != "Global Mean", ]
)
df_bf$name <- factor(df_bf$name, levels = rev(df_bf$name))

pdf("analyseBudSeed/figures/fullForcingGymno.pdf", width = 20, height = 50)
ggplot(df_bf, aes(x = mean, y = name)) +
  geom_errorbar(aes(xmin = low, xmax = high, color = is_mean), 
                width = 0,
                linewidth = 1.5) +
  geom_point(size = 2.5) +
  
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", alpha = 0.5) +
  
  scale_color_manual(values = c("Global Mean" = "firebrick", "Species" = "black")) +
  labs(title = "bf",
       x = "Posterior Estimate", y = NULL) +
  theme_minimal()
dev.off()

# intercept
parameter_a <- c("a_z", names(fit)[grep("^a\\[", names(fit))])
stats <- summary(fit, pars = parameter_a, probs = c(0.25, 0.75))$summary

df_a <- as.data.frame(stats)
df_a$parameter <- rownames(df_a)

colnames(df_a)[grep("25%", colnames(df_a))] <- "low"
colnames(df_a)[grep("75%", colnames(df_a))] <- "high"


df_a$is_mean <- ifelse(df_a$parameter == "a_z", "Global Mean", "Species")

df_a$index <- as.numeric(gsub("\\D", "", df_a$parameter))

df_a$name <- ifelse(is.na(df_a$index), 
                    "Global Mean", 
                    species_names[df_a$index])
df_a <- df_a[order(df_a$name), ]
df_a <- rbind(
  df_a[df_a$name == "Global Mean", ],
  df_a[df_a$name != "Global Mean", ]
)
df_a$name <- factor(df_a$name, levels = rev(df_a$name))

pdf("analyseBudSeed/figures/fullInterceptGymno.pdf", width = 20, height = 50)
ggplot(df_a, aes(x = mean, y = name)) +
  geom_errorbar(aes(xmin = low, xmax = high, color = is_mean), 
                width = 0,
                linewidth = 1.5) +
  geom_point(size = 2.5) +
  
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", alpha = 0.5) +
  
  scale_color_manual(values = c("Global Mean" = "firebrick", "Species" = "black")) +
  labs(title = "a",
       x = "Posterior Estimate", y = NULL) +
  theme_minimal()
dev.off()

# Lambda
parameter_lambda <- c(names(fit)[grep("lambda", names(fit))])
stats <- summary(fit, pars = parameter_lambda, probs = c(0.25, 0.75))$summary
lambda <- as.data.frame(stats)
lambda$parameter <- rownames(lambda)
colnames(lambda)[grep("25%", colnames(lambda))] <- "low"
colnames(lambda)[grep("75%", colnames(lambda))] <- "high"


ggplot(lambda, aes(x = mean, y = parameter)) +
  geom_point(size = 2, alpha = 1) + 
  geom_errorbar(aes(xmin = low, 
                    xmax = high), 
                width = 0, alpha = 1, linewidth = 1) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  labs(y = "", x = "Lambda values") +
  theme(
    axis.text.y = element_blank(), axis.ticks.y = element_blank(),
    legend.title = element_text(size = 12, face = "bold"),  
    legend.text = element_text(size = 10),                  
    legend.key.size = unit(1.5, "lines"), legend.position = "right"             
  ) +
  theme_minimal() +
  scale_y_discrete(limits = rev)  

# visualize with raw data and predicted values from model for angio

y_prop_hat  <- colMeans(rstan::extract(fit)$y_prop_gen)
y_degen_hat <- colMeans(rstan::extract(fit)$y_degen_gen)

df_prop <- data.frame(
  chill     = mdl.dataGym$c_prop,
  observed  = mdl.dataGym$y_prop,
  predicted = y_prop_hat,
  species_idx = mdl.dataGym$sp_prop
)

df_degen <- data.frame(
  chill     = mdl.dataGym$c_degen,
  observed  = mdl.dataGym$y_degen,
  predicted = y_degen_hat,
  species_idx = mdl.dataGym$sp_degen
)

all_data <- rbind(df_prop, df_degen)

all_data$species_name <- species_names[all_data$species_idx]


pdf("analyseBudSeed/figures/predictedRawGym.pdf", width = 14, height = 11)

par(mfrow = c(4, 5))

unique_spp <- unique(all_data$species_name)

for (i in 1:length(unique_spp)) {
  
  sp_data <- all_data[all_data$species_name == unique_spp[i], ]
  plot(sp_data$chill, sp_data$observed, 
       pch = 16, col = "black",
       ylim = c(0, 1),
       xlab = "Chilling", ylab = "Response",
       main = unique_spp[i],
       cex.main = 0.8)
  points(sp_data$chill, sp_data$predicted, 
         col = "blue", pch = 1, cex = 1.2)
}


dev.off()

# Make the line
draws_a  <- as.matrix(fit, pars = "a")
draws_bc <- as.matrix(fit, pars = "bc")
draws_bf <- as.matrix(fit, pars = "bf")

sp_forcing <- tapply(c(mdl.dataAngio$f_prop, mdl.dataAngio$f_degen),
                     c(mdl.dataAngio$sp_prop, mdl.dataAngio$sp_degen), mean)

df_prop <- data.frame(
  chill     = mdl.dataAngio$c_prop,
  observed  = mdl.dataAngio$y_prop,
  species_idx = mdl.dataAngio$sp_prop
)

df_degen <- data.frame(
  chill     = mdl.dataGym$c_degen,
  observed  = mdl.dataGym$y_degen,
  species_idx = mdl.dataGym$sp_degen
)

all_data <- rbind(df_prop, df_degen)

all_data$species_name <- species_names[all_data$species_idx]
unique_spp <- unique(all_data$species_name)

pdf("C:/PhD/Project/egret/analyses/analyseBudSeed/figures/chillingPredictedAngio.pdf", width = 14, height = 11)

par(mfrow = c(4, 5))

for (i in 1:length(unique_spp)) {

  sp_name <- unique_spp[i]
  sp_idx  <- which(unique_spp == sp_name)
  sp_raw <- all_data[all_data$species_name == sp_name, ]

  chill_seq <- seq(min(sp_raw$chill), max(sp_raw$chill), length.out = 10)
  a_i  <- draws_a[, sp_idx]
  bc_i <- draws_bc[, sp_idx]
  bf_i <- draws_bf[, sp_idx]
  f_i  <- sp_forcing[sp_idx]
  
  mu_mat <- plogis(outer(a_i + (bf_i * f_i), chill_seq, "+") + 
                     outer(bc_i, chill_seq, "*"))
  
  mu_mean <- apply(mu_mat, 2, mean)
  mu_low  <- apply(mu_mat, 2, quantile, 0.05)
  mu_high <- apply(mu_mat, 2, quantile, 0.95)
  

  plot(sp_raw$chill, sp_raw$observed, 
       type = "n",
       ylim = c(0, 1), 
       xlab = "Chilling", ylab = "Response",
       main = sp_name, cex.main = 0.8)
  
  polygon(c(chill_seq, rev(chill_seq)), 
          c(mu_low, rev(mu_high)), 
          col = rgb(0, 0, 1, 0.2), border = NA)
  
  
  lines(chill_seq, mu_mean, col = "blue", lwd = 2)
  
  points(sp_raw$chill, sp_raw$observed, pch = 16, col = rgb(0, 0, 0, 0.5), cex = 0.8)

}

draws_a_z  <- as.matrix(fit, pars = "a_z")
draws_a_z <- as.numeric(draws_a_z)
draws_bc_z <- as.matrix(fit, pars = "bc_z")
draws_bc_z <- as.numeric(draws_bc_z)
draws_bf_z <- as.matrix(fit, pars = "bf_z")
draws_bf_z <- as.numeric(draws_bf_z)
global_forcing <- mean(c(mdl.dataAngio$f_prop, mdl.dataAngio$f_degen))


global_chill_seq <- seq(min(all_data$chill), max(all_data$chill), length.out = 10)

global_mu_mat <- plogis(outer(draws_a_z + (draws_bf_z * global_forcing), global_chill_seq, "+") + outer(draws_bc_z, global_chill_seq, "*"))

global_mu_mean <- apply(global_mu_mat, 2, mean)
global_mu_low  <- apply(global_mu_mat, 2, quantile, 0.05)
global_mu_high <- apply(global_mu_mat, 2, quantile, 0.95)

plot(all_data$chill, all_data$observed, 
     type = "n", 
     ylim = c(0, 1),
     xlab = "Chilling", 
     ylab = "Response")

polygon(c(global_chill_seq, rev(global_chill_seq)), 
        c(global_mu_low, rev(global_mu_high)), 
        col = rgb(0.1, 0.1, 0.1, 0.15), border = NA) 

lines(global_chill_seq, global_mu_mean, col = "blue", lwd = 2)

#points(all_data$chill, all_data$observed, pch = 16, col = rgb(0, 0, 0, 0.1), cex = 0.6)

dev.off()
