library(bayesplot)
library(ggplot2)
setwd("C:/PhD/Project/egret/wcvp_Mao/")
util <- new.env()
source('mcmc_analysis_tools_rstan.R', local=util)
source('mcmc_visualization_tools.R', local=util)

###for usda only
## Angiosperm
fit <- readRDS("fit_usda_angio.rds")
summ <- readRDS("summary_usda_angio.rds")
diagnostics <- readRDS("diagnostics_usda_angio.rds")

samples <- util$extract_expectand_vals(fit)

par(mfrow=c(2, 3))
util$plot_expectand_pushforward(samples[['a_z']], 50, display_name = "a_z")
util$plot_expectand_pushforward(samples[['bc_z']], 50, display_name = "bc_z")
util$plot_expectand_pushforward(samples[['bf_z']], 50, display_name = "bf_z")
util$plot_expectand_pushforward(samples[['sigma_a']], 50, display_name = "sigma_a")
util$plot_expectand_pushforward(samples[['sigma_bc']], 50, display_name = "sigma_bc")
util$plot_expectand_pushforward(samples[['sigma_bf']], 50, display_name = "sigma_bf")


species_names <- levels(as.factor(d$latbi))

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

pdf("C:/PhD/Project/egret/analyses/analyseBudSeed/figures/usdaChillingAngio.pdf", width = 20, height = 50)
ggplot(df_bc, aes(x = mean,y = name)) +
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

pdf("C:/PhD/Project/egret/analyses/analyseBudSeed/figures/usdaForcingAngio.pdf", width = 20, height = 50)
ggplot(df_bf, aes(x = mean, y = name, mean)) +
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

pdf("C:/PhD/Project/egret/analyses/analyseBudSeed/figures/usdaInterceptAngio.pdf", width = 20, height = 50)
ggplot(df_a, aes(x = mean, y = name, mean)) +
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

## Gymnosperm
fit <- readRDS("fit_usda_gymno.rds")
summ <- readRDS("summary_usda_gymno.rds")
diagnostics <- readRDS("diagnostics_usda_gymno.rds")

samples <- util$extract_expectand_vals(fit)

par(mfrow=c(2, 3))
util$plot_expectand_pushforward(samples[['a_z']], 50, display_name = "a_z")
util$plot_expectand_pushforward(samples[['bc_z']], 50, display_name = "bc_z")
util$plot_expectand_pushforward(samples[['bf_z']], 50, display_name = "bf_z")
util$plot_expectand_pushforward(samples[['sigma_a']], 50, display_name = "sigma_a")
util$plot_expectand_pushforward(samples[['sigma_bc']], 50, display_name = "sigma_bc")
util$plot_expectand_pushforward(samples[['sigma_bf']], 50, display_name = "sigma_bf")


species_names <- levels(as.factor(d$latbi))

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

pdf("C:/PhD/Project/egret/analyses/analyseBudSeed/figures/usdaChillingGymno.pdf", width = 20, height = 50)
ggplot(df_bc, aes(x = mean,y = name)) +
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

pdf("C:/PhD/Project/egret/analyses/analyseBudSeed/figures/usdaForcingGymno.pdf", width = 20, height = 50)
ggplot(df_bf, aes(x = mean, y = name, mean)) +
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

pdf("C:/PhD/Project/egret/analyses/analyseBudSeed/figures/usdaInterceptGymno.pdf", width = 20, height = 50)
ggplot(df_a, aes(x = mean, y = name, mean)) +
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

### for full dataset
## Angiosperm
fit <- readRDS("fit_full_angio.rds")
summ <- readRDS("summary_full_angio.rds")
diagnostics <- readRDS("diagnostics_full_angio.rds")

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

species_names <- levels(as.factor(d$latbi))

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

pdf("C:/PhD/Project/egret/analyses/analyseBudSeed/figures/fullChillingAngio.pdf", width = 20, height = 50)
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

pdf("C:/PhD/Project/egret/analyses/analyseBudSeed/figures/fullInterceptAngio.pdf", width = 20, height = 50)
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

## Gymnosperm
fit <- readRDS("fit_full_gymno.rds")
summ <- readRDS("summary_full_gymno.rds")
diagnostics <- readRDS("diagnostics_full_gymno.rds")

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

species_names <- levels(as.factor(d$latbi))

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

pdf("C:/PhD/Project/egret/analyses/analyseBudSeed/figures/fullChillingGymno.pdf", width = 20, height = 50)
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

pdf("C:/PhD/Project/egret/analyses/analyseBudSeed/figures/fullForcingGymno.pdf", width = 20, height = 50)
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

pdf("C:/PhD/Project/egret/analyses/analyseBudSeed/figures/fullInterceptGymno.pdf", width = 20, height = 50)
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
