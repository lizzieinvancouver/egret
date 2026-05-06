library(bayesplot)
library(ggplot2)
library(posterior)
library(gridExtra)

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

source("analyseBudSeed/betaDisMdl.R")

util <- new.env()
source('mcmc_analysis_tools_rstan.R', local=util)
source('mcmc_visualization_tools.R', local=util)

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
names <- c(sapply(1:mdl.dataAngio$N_prop, function(n) paste0('y_prop_gen[',n,']')),
           sapply(1:mdl.dataAngio$N_degen, function(n) paste0('y_degen_gen[',n,']')))
preds <- samples[names]
names(preds) <- sapply(1:length(preds), function(n) paste0('y_gen[',n,']'))
util$plot_hist_quantiles(preds, 'y_gen', 0, 1, 0.1,
                         baseline_values=c(mdl.dataAngio$y_prop, mdl.dataAngio$y_degen), 
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
y_prop_hat_10 <- apply(rstan::extract(fit)$y_prop_gen, 2, quantile, probs = 0.1, na.rm = TRUE)
y_prop_hat_90 <- apply(rstan::extract(fit)$y_prop_gen, 2, quantile, probs = 0.9, na.rm = TRUE)
y_degen_hat_10 <- apply(rstan::extract(fit)$y_degen_gen, 2, quantile, probs = 0.1, na.rm = TRUE)
y_degen_hat_90 <- apply(rstan::extract(fit)$y_degen_gen, 2, quantile, probs = 0.1, na.rm = TRUE)


df_prop <- data.frame(
  chill     = mdl.dataAngio$c_prop,
  observed  = mdl.dataAngio$y_prop,
  predicted = y_prop_hat,
  quantile10 = y_prop_hat_10,
  quantile90 = y_prop_hat_90,
  species_idx = mdl.dataAngio$sp_prop
)

df_degen <- data.frame(
  chill     = mdl.dataAngio$c_degen,
  observed  = mdl.dataAngio$y_degen,
  predicted = y_degen_hat,
  quantile10 = y_degen_hat_10,
  quantile90 = y_degen_hat_90,
  species_idx = mdl.dataAngio$sp_degen
)


all_data <- rbind(df_prop, df_degen)

all_data$species_name <- species_names[all_data$species_idx]

testing <- FALSE
if(testing){
pdf("analyseBudSeed/figures/predictedRawAngioTest.pdf",
      width = 14, height = 11)
  
  all_data <- arrange(all_data, species_idx)
  unique_spp <- unique(all_data$species_name)
  
  plots <- lapply(unique_spp, function(sp_name) {
    sp_raw <- subset(all_data, species_name == sp_name)
    
    p <- ggplot(sp_raw, aes(x = chill)) +
      ylim(0, 1) +
      labs(title = sp_name, x = "Chilling", y = "Response") +
      theme_bw() +
      theme(plot.title = element_text(size = 8))
    
    if (nrow(sp_raw) > 1) {
      p <- p +
        geom_ribbon(aes(ymin = quantile10, ymax = quantile90),
                    fill = "blue", alpha = 0.2) +
        geom_line(aes(y = predicted),
                  color = "blue", linewidth = 1)
    } else {
      p <- p +
        geom_errorbar(aes(ymin = quantile10, ymax = quantile90),
                      width = 0.05, color = "blue") +
        geom_point(aes(y = predicted),
                   color = "blue", size = 2)
    }
    
    p +
      geom_point(aes(y = observed),
                 color = "black", size = 1.5)
  })
  
  # ---- CHUNK INTO GROUPS OF 20 ----
  chunk_size <- 20
  n_pages <- ceiling(length(plots) / chunk_size)
  
  for (i in seq_len(n_pages)) {
    idx <- ((i - 1) * chunk_size + 1):min(i * chunk_size, length(plots))
    
    grid.arrange(grobs = plots[idx], ncol = 5, nrow = 4)
  }
  
  dev.off()
}
###Hmm this didn't work


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

# Make the line using a fixed mean forcing, which is not retrodictive check
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
all_data <- arrange(all_data,species_idx)
unique_spp <- unique(all_data$species_name)

pdf("analyseBudSeed/figures/chillingPredictedAngio.pdf", width = 14, height = 11)

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
  
  a_i_mean <- mean(a_i)
  bc_i_mean <- mean(bc_i)
  bf_i_mean <- mean(bf_i)
  
  mu_mean <- plogis(a_i_mean + bc_i_mean * chill_seq + bf_i_mean * f_i)
  
  a_i_low <- quantile(a_i, probs = 0.1, na.rm = FALSE)
  bc_i_low <- quantile(bc_i, probs = 0.1, na.rm = FALSE)
  bf_i_low <- quantile(bf_i, probs = 0.1, na.rm = FALSE)
  
  mu_low <- plogis(a_i_low + bc_i_low * chill_seq + bf_i_low * f_i)
  
  a_i_high <- quantile(a_i, probs = 0.9, na.rm = FALSE)
  bc_i_high <- quantile(bc_i, probs = 0.9, na.rm = FALSE)
  bf_i_high <- quantile(bf_i, probs = 0.9, na.rm = FALSE)
  
  mu_high <- plogis(a_i_high + bc_i_high * chill_seq + bf_i_high * f_i)
  
  
  plot(sp_raw$chill, sp_raw$observed, 
       type = "n",
       ylim = c(0, 1), 
       xlab = "Chilling", ylab = "Response",
       main = sp_name, cex.main = 0.8)
  
  polygon(c(chill_seq, rev(chill_seq)), 
          c(mu_low, rev(mu_high)), 
          col = "grey", border = NA)
  
  
  lines(chill_seq, mu_mean, col = "blue", lwd = 2)
  
  points(sp_raw$chill, sp_raw$observed, pch = 16, col = "black", cex = 0.8)

}

draws_a_z  <- as.matrix(fit, pars = "a_z")
draws_a_z <- as.numeric(draws_a_z)
draws_bc_z <- as.matrix(fit, pars = "bc_z")
draws_bc_z <- as.numeric(draws_bc_z)
draws_bf_z <- as.matrix(fit, pars = "bf_z")
draws_bf_z <- as.numeric(draws_bf_z)
global_forcing <- mean(c(mdl.dataAngio$f_prop, mdl.dataAngio$f_degen))


global_chill_seq <- seq(min(all_data$chill), max(all_data$chill), length.out = 10)

draws_a_z_mean <- mean(draws_a_z)
draws_bc_z_mean <- mean(draws_bc_z)
draws_bf_z_mean <- mean(draws_bf_z)

global_mu <- plogis(draws_a_z_mean + draws_bc_z_mean * global_chill_seq + draws_bf_z_mean * f_i)

draws_a_z_low <- quantile(draws_a_z, probs = 0.1, na.rm = FALSE)
draws_bc_z_low <- quantile(draws_bc_z, probs = 0.1, na.rm = FALSE)
draws_bf_z_low <- quantile(draws_bf_z, probs = 0.1, na.rm = FALSE)

global_mu_low <- plogis(draws_a_z_low + draws_bc_z_low * global_chill_seq + draws_bf_z_low * f_i)

draws_a_z_high <- quantile(a_i, probs = 0.9, na.rm = FALSE)
draws_bc_z_high <- quantile(bc_i, probs = 0.9, na.rm = FALSE)
draws_bf_z_high <- quantile(bf_i, probs = 0.9, na.rm = FALSE)

global_mu_high <- plogis(draws_a_z_high + draws_bc_z_high * global_chill_seq + draws_bf_z_high * f_i)


plot(all_data$chill, all_data$observed, 
     type = "n", 
     ylim = c(0, 1),
     xlab = "Chilling", 
     ylab = "Response")

polygon(c(global_chill_seq, rev(global_chill_seq)), 
        c(global_mu_low, rev(global_mu_high)), 
        col = "grey", border = NA) 

lines(global_chill_seq, global_mu_mean, col = "blue", lwd = 2)



dev.off()

# REAL retrodictive check with Mike's functions
pdf("analyseBudSeed/figures/retrodictiveChecksSpp.pdf",
    width = 8, height = 6)

# get a vector of ALL species, both in prop and degen
all_sp <- unique(c(mdl.dataAngio$sp_degen, mdl.dataAngio$sp_prop))
par(mfrow=c(2, 3))

for (sp in all_sp) {
  
  idx_degen_raw  <- which(mdl.dataAngio$sp_degen == sp)
  idx_prop_raw <- which(mdl.dataAngio$sp_prop == sp)
  
  names <- c()
  c_vals <- c()
  f_vals <- c()
  y_obs  <- c()
  # aggregate data for both degen and prop
  if (length(idx_degen_raw) > 0) {
    names  <- c(names, paste0('y_degen_gen[', idx_degen_raw, ']'))
    c_vals <- c(c_vals, mdl.dataAngio$c_degen[idx_degen_raw])
    f_vals <- c(f_vals, mdl.dataAngio$f_degen[idx_degen_raw])
    y_obs  <- c(y_obs, mdl.dataAngio$y_degen[idx_degen_raw])
  }
  
  if (length(idx_prop_raw) > 0) {
    names  <- c(names, paste0('y_prop_gen[', idx_prop_raw, ']'))
    c_vals <- c(c_vals, mdl.dataAngio$c_prop[idx_prop_raw])
    f_vals <- c(f_vals, mdl.dataAngio$f_prop[idx_prop_raw])
    y_obs  <- c(y_obs, mdl.dataAngio$y_prop[idx_prop_raw])
  }
  
  n_unique_c <- length(unique(c_vals))
  n_unique_f <- length(unique(f_vals))
  
  # Case 1: Multiple Chilling levels
  if (n_unique_c > 1){
    # if there are multiple forcing as well, make separate figures for different forcing
    for (f in unique(f_vals)){
      mask <- which(f_vals == f)
    
    orderx <- order(c_vals[mask]) 
    names_ordered <- names[mask][orderx]
    dif_chill <- c_vals[mask][orderx]
    y_ordered <- y_obs[mask][orderx]
    
    util$plot_conn_pushforward_quantiles(
      samples, names_ordered, plot_xs = dif_chill,
      xlab = 'Chilling (scaled)', ylab = 'Germ. perc.',
      display_xlim = c(-1, 5), display_ylim = c(0, 1)
    )
    title(main = paste("Sp:", sp,"| F:", round(f, 2)))
    points(dif_chill, y_ordered, pch=16, cex=1.2, col="white")
    points(dif_chill, y_ordered, pch=16, cex=0.8, col="black")
    }
    # Case 2: Multiple forcing levels
  } else if (n_unique_f > 1) {
    orderx <- order(f_vals)
    dif_forcing <- f_vals[orderx]
    names_ordered <- names[orderx]
    y_ordered <- y_obs[orderx]
    
    util$plot_conn_pushforward_quantiles(
      samples, names_ordered, plot_xs = dif_forcing,
      xlab = 'Forcing (scaled)', ylab = 'Germ. perc.',
      display_xlim = c(-3, 3), display_ylim = c(0, 1)
    )
    title(main = paste("Sp:", sp))
    points(dif_forcing, y_ordered, pch=16, cex=1.2, col="white")
    points(dif_forcing, y_ordered, pch=16, cex=0.8, col="black")
  }
  }
dev.off()
