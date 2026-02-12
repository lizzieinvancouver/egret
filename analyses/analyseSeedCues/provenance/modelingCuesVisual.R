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

# load modeling cues (without running the models for now)
source('provenance/modelingCues.R')

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# Compare models with and without provenance ####
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# read the object 'fit_nophy' as an RDS file
fit_withprov <- readRDS("/Users/christophe_rouleau-desrochers/Desktop/UBC/egretLOCAL/fit_nophy.rds")
fit_nophy_noprov <- readRDS("/Users/christophe_rouleau-desrochers/Desktop/UBC/egretLOCAL/fit_nophy_noprov.rds")

# Try a more efficient way to plot the different parameters
df_withprov <- as.data.frame(fit_withprov)

colswithprov <- colnames(df_withprov)
# colswithprov <- colswithprov[grepl("prov", colswithprov)]
# colswithprov <- colswithprov[!grepl("tilde", colswithprov)]

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

# dwithprov2$num <- sub(".*\\[(\\d+)\\]", "\\1", dwithprov2$prmID)

# dwithprov2$numspp <- modeld$numspp[modeld$num]

# get a subset for just the slope and intercepts 
vec <- c(paste("a", "[", 1:27, "]", sep = ""),
         paste("bt", "[", 1:27, "]", sep = ""),
         paste("bf", "[", 1:27, "]", sep = ""),
         paste("bcs", "[", 1:27, "]", sep = ""))
# dwithprov2$prmID[grepl("sigma", dwithprov2$prmID)])

prmvec <- c(rep("a", each = 27),
            # rep("sigma", each =  4),
            rep("bt", each = 27), 
            rep("bf", each = 27), 
            rep("bcs", each =  27))

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
                        newd, function(x) length(unique(x)))

dforplot$numspp <- sub(".*\\[(\\d+)\\]", "\\1", dforplot$prmID)
dforplot$spp <- modeld$genusspecies[match(dforplot$numspp, modeld$numspp)]

dforplot$provperspp <- provcounts$provLatLonAlt[match(dforplot$spp, provcounts$genusspecies)]

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

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# Check sigmas #### 
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
aprovvec <- paste("a_prov", "[", 1:length(unique(modeld$numprov)), "]", sep = "")
da_prov <- subset(dwithprov2, prmID %in% aprovvec)

dsigmas <- subset(dwithprov2, prmID %in% 
                    dwithprov2$prmID[grep("sigma", dwithprov2$prmID)])

ggplot(dsigmas, aes(x = fit_mean, y = prmID)) +
  geom_point(size = 2, alpha = 1) + 
  geom_errorbar(aes(xmin = fit_per5, 
                    xmax = fit_per95), 
                width = 0, alpha = 1, linewidth = 0.5) +
  geom_errorbar(aes(xmin = fit_per25, 
                    xmax = fit_per75), 
                width = 0, alpha = 1, linewidth = 1) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  labs(y = "", x = "Sigma values") +
  # facet_wrap(~ model, nrow =2) +
  theme(
    axis.text.y = element_blank(), axis.ticks.y = element_blank(),
    legend.title = element_text(size = 12, face = "bold"),  
    legend.text = element_text(size = 10),                  
    legend.key.size = unit(1.5, "lines"), legend.position = "right"             
  ) +
  theme_minimal() +
  scale_y_discrete(limits = rev)  
ggsave("provenance/figures/sigmaVals.jpeg", width = 5, height = 5, 
       units = "in", dpi = 300)

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# Plot provenance and color code by spp #### 
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
my_colors <- c("#9E3D22FF", "#AA4422FF", "#B74B22FF",
               "#C35222FF", "#D05921FF", "#D96324FF",
               "#E17028FF", "#E97C2DFF", "#F18832FF",
               "#F69542FF", "#F3A665FF", "#EDB686FF",
               "#E5C5A7FF", "#D9D5C9FF", "#C5CBCCFF",
               "#B0C2CEFF", "#99B8D0FF", "#80AFD2FF",
               "#71A5CDFF", "#689BC5FF", "#5F91BDFF",
               "#5587B4FF", "#4C7EACFF", "#4475A3FF",
               "#3C6D9BFF", "#346492FF", "#2B5C8AFF")

# shapes for woodinessss
my_shapes <- c(
  Y = 15,
  N = 17
)
# add column for woody
dmain <- read.csv("output/egretclean.csv")


# === === === === === === === === === === === === === === === === === === === 
##### a_prov ##### 
# === === === === === === === === === === === === === === === === === === === 
# get a_prov
aprovvec <- paste("a_prov", "[", 1:length(unique(modeld$numprov)), "]", sep = "")
da_prov <- subset(dwithprov2, prmID %in% aprovvec)
da_prov$numprov <- as.character(sub(".*\\[(\\d+)\\]", "\\1", da_prov$prmID))
da_prov$numspp <- modeld$numspp[match(da_prov$numprov, modeld$numprov)]

# get a
avec <- paste("a", "[", 1:length(unique(modeld$numprov)), "]", sep = "")
da <- subset(dwithprov2, prmID %in% avec)
da$numspp <- as.numeric(sub(".*\\[(\\d+)\\]", "\\1", da$prmID))

# add species name to df
da_prov$sppname <- modeld$genusspecies[match(da_prov$numspp, modeld$numspp)]
da$sppname <- modeld$genusspecies[match(da$numspp, modeld$numspp)]

# add woody
da$woody <- dmain$woody[match(da$sppname, dmain$latbi)]

jpeg(
  filename = "provenance/figures/muPlotProv_aprov.jpeg",
  width = 2400,      
  height = 2400,
  res = 300         
)
par(mar = c(4, 6, 4, 5))

# define a gap between species clusters
gap <- 3

# y positions
da_prov$y_pos <- NA
current_y <- 1

species_order <- as.character(1 : max(da_prov$numspp))

da_prov$spp  <- factor(da_prov$numspp, levels = species_order)

da_prov <- da_prov[order(da_prov$spp),]

da_prov$y_pos <- seq_len(nrow(da_prov))

for(sp in species_order){
  idx <- which(da_prov$spp == sp)
  n <- length(idx)
  # assign sequential positions for this species
  da_prov$y_pos[idx] <- current_y:(current_y + n - 1)
  # move cursor down with a gap before next species cluster
  current_y <- current_y + n + gap
}

da_prov$y_pos

# set up empty plot
plot(NA, NA,
     xlim = range(c(da$fit_per5-0.5, da$fit_per95+0.5)),
     ylim = c(0.5, max(da_prov$y_pos) + 0.5),
     xlab = "Days to germinate?",
     ylab = "",
     yaxt = "n",
     main = "a and a_prov"
)

# add error bars
segments(
  x0 = da_prov$fit_per25,
  x1 = da_prov$fit_per75,
  y0 = da_prov$y_pos,
  col = adjustcolor(my_colors[da_prov$spp], alpha.f = 0.7),
  lwd = 1
)

# Add the points
points(
  da_prov$fit_mean,
  da_prov$y_pos,
  cex = 0.7,
  pch = 19,
  col = adjustcolor(my_colors[da_prov$spp], alpha.f = 1)
)

# Add species intervals and mean
da$spp <- da$spp_name
spp_y <- tapply(da_prov$y_pos, da_prov$spp, mean)
da$y_pos <- spp_y[da$numspp]

segments(
  x0 = da$fit_per25,
  x1 = da$fit_per75,
  y0 = da$y_pos,
  col = adjustcolor(my_colors[da$numspp], alpha.f = 1),
  lwd = 2
)

points(
  da$fit_mean,
  da$y_pos,
  pch = my_shapes[da$woody],
  col  = adjustcolor(my_colors[da$numspp], alpha.f = 1),
  # col = "black",
  cex = 1
)

# add vertical line at 0 
abline(v = 0, lty = 2)

# Add custom y-axis labels (reverse order if needed)
axis(
  side = 2,
  at = da$y_pos,
  labels = da$sppname,
  cex.axis = 0.5,
  las = 1
)
# spp mean
spp_y <- tapply(da_prov$y_pos, da_prov$spp, mean)

woody_legend_order <- c("Y", "N")
# woody legend
legend(
  x = max(da$fit_per95) - 5,
  y = max(da$y_pos) - 2,
  legend = woody_legend_order,
  pch = my_shapes[woody_legend_order],
  pt.cex = 1.2,
  title = "Woody (Y/N)",
  bty = "n"
)
dev.off()


# === === === === === === === === === === === === === === === === === === === 
##### bt_prov ##### 
# === === === === === === === === === === === === === === === === === === === 
# get bt_prov
btprovvec <- paste("bt_prov", "[", 1:length(unique(modeld$numprov)), "]", sep = "")
dbt_prov <- subset(dwithprov2, prmID %in% btprovvec)
dbt_prov$numprov <- as.character(sub(".*\\[(\\d+)\\]", "\\1", dbt_prov$prmID))
dbt_prov$numspp <- modeld$numspp[match(dbt_prov$numprov, modeld$numprov)]

# get b
btvec <- paste("bt", "[", 1:length(unique(modeld$numprov)), "]", sep = "")
dbt <- subset(dwithprov2, prmID %in% btvec)
dbt$numspp <- as.numeric(sub(".*\\[(\\d+)\\]", "\\1", dbt$prmID))

# add species name to df
dbt_prov$sppname <- modeld$genusspecies[match(dbt_prov$numspp, modeld$numspp)]
dbt$sppname <- modeld$genusspecies[match(dbt$numspp, modeld$numspp)]

# add woody
dbt$woody <- dmain$woody[match(dbt$sppname, dmain$latbi)]

jpeg(
  filename = "provenance/figures/muPlotProv_btprov.jpeg",
  width = 2400,      
  height = 2400,
  res = 300         
)
par(mar = c(4, 6, 4, 5))

# define a gap between species clusters
gap <- 3

# y positions
dbt_prov$y_pos <- NA
current_y <- 1

species_order <- as.character(1 : max(dbt_prov$numspp))

dbt_prov$spp  <- factor(dbt_prov$numspp, levels = species_order)

dbt_prov <- dbt_prov[order(dbt_prov$spp),]

dbt_prov$y_pos <- seq_len(nrow(dbt_prov))

for(sp in species_order){
  idx <- which(dbt_prov$spp == sp)
  n <- length(idx)
  # assign sequential positions for this species
  dbt_prov$y_pos[idx] <- current_y:(current_y + n - 1)
  # move cursor down with a gap before next species cluster
  current_y <- current_y + n + gap
}

dbt_prov$y_pos

# set up empty plot
plot(NA, NA,
     xlim = range(c(dbt$fit_per5-0.5, dbt$fit_per95+0.5)),
     ylim = c(0.5, max(dbt_prov$y_pos) + 0.5),
     xlab = "Days to germinate?",
     ylab = "",
     yaxt = "n",
     main = "bt and bt_prov"
)

# add error bars
segments(
  x0 = dbt_prov$fit_per25,
  x1 = dbt_prov$fit_per75,
  y0 = dbt_prov$y_pos,
  col = adjustcolor(my_colors[dbt_prov$spp], alpha.f = 0.7),
  lwd = 1
)

# Add the points
points(
  dbt_prov$fit_mean,
  dbt_prov$y_pos,
  cex = 0.8,
  pch = 19,
  col = adjustcolor(my_colors[dbt_prov$spp], alpha.f = 1)
)

# Add species intervals and mean
dbt$spp <- dbt$spp_name
spp_y <- tapply(dbt_prov$y_pos, dbt_prov$spp, mean)
dbt$y_pos <- spp_y[dbt$numspp]

segments(
  x0 = dbt$fit_per25,
  x1 = dbt$fit_per75,
  y0 = dbt$y_pos,
  col = adjustcolor(my_colors[dbt$numspp], alpha.f = 1),
  lwd = 2
)

points(
  dbt$fit_mean,
  dbt$y_pos,
  pch = 19,
  col  = adjustcolor(my_colors[dbt$numspp], alpha.f = 1),
  # col = "black",
  cex = 0.8
)

# add vertical line at 0 
abline(v = 0, lty = 2)

# Add custom y-axis labels (reverse order if needed)
axis(
  side = 2,
  at = dbt$y_pos,
  labels = dbt$sppname,
  cex.axis = 0.5,
  las = 1
)
# spp mean
spp_y <- tapply(dbt_prov$y_pos, dbt_prov$spp, mean)

## order species by mean y descending (top of plot first)
species_legend_order <- names(sort(spp_y, decreasing = TRUE))

dev.off()


# === === === === === === === === === === === === === === === === === === === 
##### bf_prov ##### 
# === === === === === === === === === === === === === === === === === === === 
# get bf_prov
bfprovvec <- paste("bf_prov", "[", 1:length(unique(modeld$numprov)), "]", sep = "")
dbf_prov <- subset(dwithprov2, prmID %in% bfprovvec)
dbf_prov$numprov <- as.character(sub(".*\\[(\\d+)\\]", "\\1", dbf_prov$prmID))
dbf_prov$numspp <- modeld$numspp[match(dbf_prov$numprov, modeld$numprov)]

# get b
bfvec <- paste("bf", "[", 1:length(unique(modeld$numprov)), "]", sep = "")
dbf <- subset(dwithprov2, prmID %in% bfvec)
dbf$numspp <- as.numeric(sub(".*\\[(\\d+)\\]", "\\1", dbf$prmID))

# add species name to df
dbf_prov$sppname <- modeld$genusspecies[match(dbf_prov$numspp, modeld$numspp)]
dbf$sppname <- modeld$genusspecies[match(dbf$numspp, modeld$numspp)]

# add woody
dbf$woody <- dmain$woody[match(dbf$sppname, dmain$latbi)]

jpeg(
  filename = "provenance/figures/muPlotProv_bfprov.jpeg",
  width = 2400,      
  height = 2400,
  res = 300         
)
par(mar = c(4, 6, 4, 5))

# define a gap between species clusters
gap <- 3

# y positions
dbf_prov$y_pos <- NA
current_y <- 1

species_order <- as.character(1 : max(dbf_prov$numspp))

dbf_prov$spp  <- factor(dbf_prov$numspp, levels = species_order)

dbf_prov <- dbf_prov[order(dbf_prov$spp),]

dbf_prov$y_pos <- seq_len(nrow(dbf_prov))

for(sp in species_order){
  idx <- which(dbf_prov$spp == sp)
  n <- length(idx)
  # assign sequential positions for this species
  dbf_prov$y_pos[idx] <- current_y:(current_y + n - 1)
  # move cursor down with a gap before next species cluster
  current_y <- current_y + n + gap
}

dbf_prov$y_pos

# set up empty plot
plot(NA, NA,
     xlim = range(c(dbf$fit_per5-0.5, dbf$fit_per95+0.5)),
     ylim = c(0.5, max(dbf_prov$y_pos) + 0.5),
     xlab = "Days to germinate?",
     ylab = "",
     yaxt = "n",
     main = "bf and bf_prov"
)

# add error bars
segments(
  x0 = dbf_prov$fit_per25,
  x1 = dbf_prov$fit_per75,
  y0 = dbf_prov$y_pos,
  col = adjustcolor(my_colors[dbf_prov$spp], alpha.f = 0.7),
  lwd = 1
)

# Add the points
points(
  dbf_prov$fit_mean,
  dbf_prov$y_pos,
  cex = 0.8,
  pch = 19,
  col = adjustcolor(my_colors[dbf_prov$spp], alpha.f = 1)
)

# Add species intervals and mean
dbf$spp <- dbf$spp_name
spp_y <- tapply(dbf_prov$y_pos, dbf_prov$spp, mean)
dbf$y_pos <- spp_y[dbf$numspp]

segments(
  x0 = dbf$fit_per25,
  x1 = dbf$fit_per75,
  y0 = dbf$y_pos,
  col = adjustcolor(my_colors[dbf$numspp], alpha.f = 1),
  lwd = 2
)

points(
  dbf$fit_mean,
  dbf$y_pos,
  pch = 19,
  col  = adjustcolor(my_colors[dbf$numspp], alpha.f = 1),
  # col = "black",
  cex = 0.8
)

# add vertical line at 0 
abline(v = 0, lty = 2)

# Add custom y-axis labels (reverse order if needed)
axis(
  side = 2,
  at = dbf$y_pos,
  labels = dbf$sppname,
  cex.axis = 0.5,
  las = 1
)
# spp mean
spp_y <- tapply(dbf_prov$y_pos, dbf_prov$spp, mean)

## order species by mean y descending (top of plot first)
species_legend_order <- names(sort(spp_y, decreasing = TRUE))

dev.off()


# === === === === === === === === === === === === === === === === === === === 
##### bcs_prov ##### 
# === === === === === === === === === === === === === === === === === === === 
# get bcs_prov
bcsprovvec <- paste("bcs_prov", "[", 1:length(unique(modeld$numprov)), "]", sep = "")
dbcs_prov <- subset(dwithprov2, prmID %in% bcsprovvec)
dbcs_prov$numprov <- as.character(sub(".*\\[(\\d+)\\]", "\\1", dbcs_prov$prmID))
dbcs_prov$numspp <- modeld$numspp[match(dbcs_prov$numprov, modeld$numprov)]

# get b
bcsvec <- paste("bcs", "[", 1:length(unique(modeld$numprov)), "]", sep = "")
dbcs <- subset(dwithprov2, prmID %in% bcsvec)
dbcs$numspp <- as.numeric(sub(".*\\[(\\d+)\\]", "\\1", dbcs$prmID))

# add species name to df
dbcs_prov$sppname <- modeld$genusspecies[match(dbcs_prov$numspp, modeld$numspp)]
dbcs$sppname <- modeld$genusspecies[match(dbcs$numspp, modeld$numspp)]

# add woody
dbcs$woody <- dmain$woody[match(dbcs$sppname, dmain$latbi)]

jpeg(
  filename = "provenance/figures/muPlotProv_bcsprov.jpeg",
  width = 2400,      
  height = 2400,
  res = 300         
)
par(mar = c(4, 6, 4, 5))

# define a gap between species clusters
gap <- 3

# y positions
dbcs_prov$y_pos <- NA
current_y <- 1

species_order <- as.character(1 : max(dbcs_prov$numspp))

dbcs_prov$spp  <- factor(dbcs_prov$numspp, levels = species_order)

dbcs_prov <- dbcs_prov[order(dbcs_prov$spp),]

dbcs_prov$y_pos <- seq_len(nrow(dbcs_prov))

for(sp in species_order){
  idx <- which(dbcs_prov$spp == sp)
  n <- length(idx)
  # assign sequential positions for this species
  dbcs_prov$y_pos[idx] <- current_y:(current_y + n - 1)
  # move cursor down with a gap before next species cluster
  current_y <- current_y + n + gap
}

dbcs_prov$y_pos

# set up empty plot
plot(NA, NA,
     xlim = range(c(dbcs$fit_per5-0.5, dbcs$fit_per95+0.5)),
     ylim = c(0.5, max(dbcs_prov$y_pos) + 0.5),
     xlab = "Days to germinate?",
     ylab = "",
     yaxt = "n",
     main = "bcs and bcs_prov"
)

# add error bars
segments(
  x0 = dbcs_prov$fit_per25,
  x1 = dbcs_prov$fit_per75,
  y0 = dbcs_prov$y_pos,
  col = adjustcolor(my_colors[dbcs_prov$spp], alpha.f = 0.7),
  lwd = 1
)

# Add the points
points(
  dbcs_prov$fit_mean,
  dbcs_prov$y_pos,
  cex = 0.8,
  pch = 19,
  col = adjustcolor(my_colors[dbcs_prov$spp], alpha.f = 1)
)

# Add species intervals and mean
dbcs$spp <- dbcs$spp_name
spp_y <- tapply(dbcs_prov$y_pos, dbcs_prov$spp, mean)
dbcs$y_pos <- spp_y[dbcs$numspp]

segments(
  x0 = dbcs$fit_per25,
  x1 = dbcs$fit_per75,
  y0 = dbcs$y_pos,
  col = adjustcolor(my_colors[dbcs$numspp], alpha.f = 1),
  lwd = 2
)

points(
  dbcs$fit_mean,
  dbcs$y_pos,
  pch = 19,
  col  = adjustcolor(my_colors[dbcs$numspp], alpha.f = 1),
  # col = "black",
  cex = 0.8
)

# add vertical line at 0 
abline(v = 0, lty = 2)

# Add custom y-axis labels (reverse order if needed)
axis(
  side = 2,
  at = dbcs$y_pos,
  labels = dbcs$sppname,
  cex.axis = 0.5,
  las = 1
)
# spp mean
spp_y <- tapply(dbcs_prov$y_pos, dbcs_prov$spp, mean)

## order species by mean y descending (top of plot first)
species_legend_order <- names(sort(spp_y, decreasing = TRUE))

dev.off()

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# Plot Picea glauca vs betula ####
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
pibe <- subset(modeld, genusspecies %in% c("Picea_glauca", "Betula_utilis"))

ggplot(pibe) + 
  geom_point(aes(x = germDuration, y = responseValueNum, color = datasetID)) +
  theme_minimal()

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# Compare models with and without forcing ####
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
fit_nophy_noforcing <- readRDS("/Users/christophe_rouleau-desrochers/Desktop/UBC/egretLOCAL/fit_nophy_noforcing.rds")
# read the object 'fit_nophy' as an RDS file
fit_withprov <- readRDS("/Users/christophe_rouleau-desrochers/Desktop/UBC/egretLOCAL/fit_nophy.rds")

# Try a more efficient way to plot the different parameters
df_withprov <- as.data.frame(fit_withprov)

colswithprov <- colnames(df_withprov)
# colswithprov <- colswithprov[grepl("prov", colswithprov)]
# colswithprov <- colswithprov[!grepl("tilde", colswithprov)]

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

# dwithprov2$num <- sub(".*\\[(\\d+)\\]", "\\1", dwithprov2$prmID)

# dwithprov2$numspp <- modeld$numspp[modeld$num]

# get a subset for just the slope and intercepts 
vec <- c(paste("a", "[", 1:27, "]", sep = ""),
         paste("a_prov", "[", 1:27, "]", sep = ""),
         paste("bt", "[", 1:27, "]", sep = ""),
         paste("bcs", "[", 1:27, "]", sep = ""),
         paste("bt_prov", "[", 1:27, "]", sep = ""),
         paste("bcs_prov", "[", 1:27, "]", sep = "")
)
# dnoforcing2$prmID[grepl("sigma", dnoforcing2$prmID)])
prmvec <- c(rep("a", each = 27),
            rep("a_prov", each = 27),
            rep("bt", each = 27), 
            rep("bcs", each =  27),
            rep("bt_prov", each = 27), 
            rep("bcs_prov", each =  27))
dwithprov3 <- subset(dwithprov2, prmID %in% vec)

# No forcing
df_noforcing <- as.data.frame(fit_nophy_noforcing)

colsnoforcing <- colnames(df_noforcing)
colsnoforcing <- colsnoforcing[!grepl("tilde", colsnoforcing)]
colsnoforcing <- colsnoforcing[!grepl("forcing", colsnoforcing)]

dnoforcing <- df_noforcing[, colnames(df_noforcing) %in% colsnoforcing]

dnoforcing2 <- data.frame(
  prmID = character(ncol(dnoforcing)),
  fit_mean  = numeric(ncol(dnoforcing)),  
  fit_per5  = NA, 
  fit_per25 = NA,
  fit_per75 = NA,
  fit_per95 = NA
)

for (i in 1:ncol(dnoforcing)) { # i = 1
  dnoforcing2$prmID[i] <- colnames(dnoforcing)[i]         
  dnoforcing2$fit_mean[i] <- round(mean(dnoforcing[[i]]),3)  
  dnoforcing2$fit_per5[i] <- round(quantile(dnoforcing[[i]], probs = 0.05), 3)
  dnoforcing2$fit_per25[i] <- round(quantile(dnoforcing[[i]], probs = 0.25), 3)
  dnoforcing2$fit_per75[i] <- round(quantile(dnoforcing[[i]], probs = 0.75), 3)
  dnoforcing2$fit_per95[i] <- round(quantile(dnoforcing[[i]], probs = 0.95), 3)
}
dnoforcing2
unique(dnoforcing2$prmID)
# get a subset for just the slope and intercepts 
vec <- c(paste("a", "[", 1:27, "]", sep = ""),
         paste("a_prov", "[", 1:27, "]", sep = ""),
         paste("bt", "[", 1:27, "]", sep = ""),
         paste("bcs", "[", 1:27, "]", sep = ""),
         paste("bt_prov", "[", 1:27, "]", sep = ""),
         paste("bcs_prov", "[", 1:27, "]", sep = "")
)
# dnoforcing2$prmID[grepl("sigma", dnoforcing2$prmID)])
prmvec <- c(rep("a", each = 27),
            rep("a_prov", each = 27),
            rep("bt", each = 27), 
            rep("bcs", each =  27),
            rep("bt_prov", each = 27), 
            rep("bcs_prov", each =  27))

dnoforcing3 <- subset(dnoforcing2, prmID %in% vec)

# Merge!
colnames(dnoforcing3)[2:ncol(dnoforcing3)] <- paste(
  colnames(dnoforcing3)[2:ncol(dnoforcing3)], "noforcing", sep = "_")

dnoforcing3$prm <- prmvec

dforplot <- merge(dwithprov3, dnoforcing3, by = "prmID")

# Get the number of forcingenances per species
# count the number of unique forcingenance per species

dforplot$numspp <- sub(".*\\[(\\d+)\\]", "\\1", dforplot$prmID)
dforplot$spp <- modeld$genusspecies[match(dforplot$numspp, modeld$numspp)]

# dforplot$forcingperspp <- forcingcounts$forcingLatLonAlt[match(dforplot$spp, forcingcounts$genusspecies)]

# Plot!
ggplot(dforplot, aes(x = fit_mean, y = fit_mean_noforcing)) +
  geom_errorbar(aes(xmin = fit_per25, xmax = fit_per75), 
                width = 0, linewidth = 0.5, color = "darkgray", alpha=0.7) +
  geom_errorbar(aes(ymin = fit_per25_noforcing, ymax = fit_per75_noforcing), 
                width = 0, linewidth = 0.5, color = "darkgray", alpha = 0.7) +
  geom_point(aes(color = spp), size = 1.5) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "#B40F20", linewidth = 0.8) +
  facet_wrap(~prm, scales = "free") +
  labs(x = "with forcing", y = "no forcing", title = "") +
  theme_minimal()
ggsave("provenance/figures/forcingComparison.jpeg", width = 12, height = 6, 
       units = "in", dpi = 300)
