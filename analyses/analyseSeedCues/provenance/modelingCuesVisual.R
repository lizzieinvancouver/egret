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

# load modeling cues (without running the models for now)
source('analyseSeedCues/provenance/modelingCues.R')

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
ggsave("analyseSeedCues/provenance/figures/sigmaVals.jpeg", width = 5, height = 5, 
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

# get a small df of which provenance correspond to which species
xindex <- unique(modeld[,c("datasetID", "genusspecies", "numspp", "numprov")])

# remove the parameters were not interested in for now
cols <- colnames(df_withprov)
cols <- cols[!grepl("z", cols) &
               !grepl("calc", cols) &
               !grepl("sigma", cols) &
               !grepl("kappa", cols) &
               !grepl("logistic", cols) &
               !grepl("tilde", cols)]

# === === === === === === === === === === === === === === === === === === === 
##### a_prov ##### 
# === === === === === === === === === === === === === === === === === === === 
# start with the intercept
amtrx <- data.frame(matrix(ncol = nrow(xindex), nrow = nrow(df_withprov)))

colnames(amtrx) <- xindex$numprov
colsa <- cols[grepl("a", cols) &
                !grepl("prov", cols)]
colsaprov <- cols[grepl("a_prov", cols) ]

da <- subset(df_withprov, select = colsa)
colnames(da) <- sub(".*\\[(\\d+)\\]", "\\1", colnames(da))

daprov <- subset(df_withprov, select = colsaprov)
colnames(daprov) <- sub(".*\\[(\\d+)\\]", "\\1", colnames(daprov))

for (i in seq_len(ncol(amtrx))) { # i = 30
  prov_id <- as.integer(colnames(amtrx)[i])
  spp_id <- xindex$numspp[match(prov_id, xindex$numprov)]
  amtrx[, i] <- da[, spp_id]
}
amtrx

# sum spp values in matrix to provenance value
aprovspp <- amtrx + daprov[, colnames(amtrx)]

aprovspp2 <- data.frame(
  prmID = character(ncol(aprovspp)),
  fit_mean  = numeric(ncol(aprovspp)),  
  fit_per5  = NA, 
  fit_per25 = NA,
  fit_per75 = NA,
  fit_per95 = NA
)

for (i in 1:ncol(aprovspp)) { # i = 1
  aprovspp2$prmID[i] <- colnames(aprovspp)[i]         
  aprovspp2$fit_mean[i] <- round(mean(aprovspp[[i]]),3)  
  aprovspp2$fit_per5[i] <- round(quantile(aprovspp[[i]], probs = 0.05), 3)
  aprovspp2$fit_per25[i] <- round(quantile(aprovspp[[i]], probs = 0.25), 3)
  aprovspp2$fit_per75[i] <- round(quantile(aprovspp[[i]], probs = 0.75), 3)
  aprovspp2$fit_per95[i] <- round(quantile(aprovspp[[i]], probs = 0.95), 3)
}

# get just a
avec <- paste("a", "[", 1:length(unique(modeld$numspp)), "]", sep = "")
da2 <- subset(dwithprov2, prmID %in% avec)
da2$numspp <- as.numeric(sub(".*\\[(\\d+)\\]", "\\1", da2$prmID))
da2$sppname <- modeld$genusspecies[match(da2$numspp, modeld$numspp)]

# add species name to df
aprovspp2$numspp <- modeld$numspp[match(aprovspp2$prmID, modeld$numprov)]
aprovspp2$sppname <- modeld$genusspecies[match(aprovspp2$prmID, modeld$numprov)]

# add woody
da2$woody <- dmain$woody[match(da2$sppname, dmain$latbi)]
aprovspp2$woody <- dmain$woody[match(aprovspp2$sppname, dmain$latbi)]

jpeg(
  filename = "analyseSeedCues/provenance/figures/muPlotProv_aprov.jpeg",
  width = 2400,      
  height = 2400,
  res = 300         
)
par(mar = c(4, 6, 4, 5))

# define a gap between species clusters
gap <- 3

# y positions
aprovspp2$y_pos <- NA
current_y <- 1

species_order <- as.character(1 : max(aprovspp2$numspp))

aprovspp2$spp  <- factor(aprovspp2$numspp, levels = species_order)

aprovspp2 <- aprovspp2[order(aprovspp2$spp),]

aprovspp2$y_pos <- seq_len(nrow(aprovspp2))

for(sp in species_order){
  idx <- which(aprovspp2$spp == sp)
  n <- length(idx)
  # assign sequential positions for this species
  aprovspp2$y_pos[idx] <- current_y:(current_y + n - 1)
  # move cursor down with a gap before next species cluster
  current_y <- current_y + n + gap
}

aprovspp2$y_pos

# set up empty plot
plot(NA, NA,
     xlim = range(c(da2$fit_per5-0.5, da2$fit_per95+0.5)),
     ylim = c(0.5, max(aprovspp2$y_pos) + 0.5),
     xlab = "Days to germinate?",
     ylab = "",
     yaxt = "n",
     main = "a and a_prov"
)

# add error bars
segments(
  x0 = aprovspp2$fit_per25,
  x1 = aprovspp2$fit_per75,
  y0 = aprovspp2$y_pos,
  col = adjustcolor(my_colors[aprovspp2$spp], alpha.f = 0.7),
  lwd = 1
)

# Add the points
points(
  aprovspp2$fit_mean,
  aprovspp2$y_pos,
  cex = 0.5,
  pch = 21,
  col = adjustcolor(my_colors[aprovspp2$spp], alpha.f = 1)
)

# Add species intervals and mean
da2$spp <- da2$spp_name
spp_y <- tapply(aprovspp2$y_pos, aprovspp2$spp, mean)
da2$y_pos <- spp_y[da2$numspp]

segments(
  x0 = da2$fit_per25,
  x1 = da2$fit_per75,
  y0 = da2$y_pos,
  col = adjustcolor(my_colors[da2$numspp], alpha.f = 1),
  lwd = 2
)

points(
  da2$fit_mean,
  da2$y_pos,
  pch = my_shapes[da2$woody],
  col  = adjustcolor(my_colors[da2$numspp], alpha.f = 1),
  # col = "black",
  cex = 1
)

# add vertical line at 0 
abline(v = 0, lty = 2)

# Add custom y-axis labels (reverse order if needed)
axis(
  side = 2,
  at = da2$y_pos,
  labels = da2$sppname,
  cex.axis = 0.5,
  las = 1
)

# spp mean
spp_y <- tapply(aprovspp2$y_pos, aprovspp2$spp, mean)

woody_legend_order <- c("Y", "N")
# woody legend
legend(
  x = max(da2$fit_per95) - 5,
  y = max(da2$y_pos) - 2,
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
# start with the intercept
btmtrx <- data.frame(matrix(ncol = nrow(xindex), nrow = nrow(df_withprov)))

colnames(btmtrx) <- xindex$numprov
colsbt <- cols[grepl("bt", cols) &
                !grepl("prov", cols)]
colsbtprov <- cols[grepl("bt_prov", cols) ]

dbt <- subset(df_withprov, select = colsbt)
colnames(dbt) <- sub(".*\\[(\\d+)\\]", "\\1", colnames(dbt))

dbtprov <- subset(df_withprov, select = colsbtprov)
colnames(dbtprov) <- sub(".*\\[(\\d+)\\]", "\\1", colnames(dbtprov))

for (i in seq_len(ncol(btmtrx))) { # i = 30
  prov_id <- as.integer(colnames(btmtrx)[i])
  spp_id <- xindex$numspp[match(prov_id, xindex$numprov)]
  btmtrx[, i] <- dbt[, spp_id]
}
btmtrx

# sum spp values in matrix to provenance value
dbtprovspp <- btmtrx + dbtprov[, colnames(btmtrx)]

dbtprovspp2 <- data.frame(
  prmID = character(ncol(dbtprovspp)),
  fit_mean  = numeric(ncol(dbtprovspp)),  
  fit_per5  = NA, 
  fit_per25 = NA,
  fit_per75 = NA,
  fit_per95 = NA
)

for (i in 1:ncol(dbtprovspp)) { # i = 1
  dbtprovspp2$prmID[i] <- colnames(dbtprovspp)[i]         
  dbtprovspp2$fit_mean[i] <- round(mean(dbtprovspp[[i]]),3)  
  dbtprovspp2$fit_per5[i] <- round(quantile(dbtprovspp[[i]], probs = 0.05), 3)
  dbtprovspp2$fit_per25[i] <- round(quantile(dbtprovspp[[i]], probs = 0.25), 3)
  dbtprovspp2$fit_per75[i] <- round(quantile(dbtprovspp[[i]], probs = 0.75), 3)
  dbtprovspp2$fit_per95[i] <- round(quantile(dbtprovspp[[i]], probs = 0.95), 3)
}

# get just bt
btvec <- paste("bt", "[", 1:length(unique(modeld$numspp)), "]", sep = "")
dbt2 <- subset(dwithprov2, prmID %in% btvec)
dbt2$numspp <- as.numeric(sub(".*\\[(\\d+)\\]", "\\1", dbt2$prmID))
dbt2$sppname <- modeld$genusspecies[match(dbt2$numspp, modeld$numspp)]

# add species name to df
dbtprovspp2$numspp <- modeld$numspp[match(dbtprovspp2$prmID, modeld$numprov)]
dbtprovspp2$sppname <- modeld$genusspecies[match(dbtprovspp2$prmID, modeld$numprov)]

# add woody
dbt2$woody <- dmain$woody[match(dbt2$sppname, dmain$latbi)]
dbtprovspp2$woody <- dmain$woody[match(dbtprovspp2$sppname, dmain$latbi)]

jpeg(
  filename = "analyseSeedCues/provenance/figures/muPlotProv_btprov.jpeg",
  width = 2400,      
  height = 2400,
  res = 300         
)
par(mar = c(4, 6, 4, 5))

# define a gap between species clusters
gap <- 3

# y positions
dbtprovspp2$y_pos <- NA
current_y <- 1

species_order <- as.character(1 : max(dbtprovspp2$numspp))

dbtprovspp2$spp  <- factor(dbtprovspp2$numspp, levels = species_order)

dbtprovspp2 <- dbtprovspp2[order(dbtprovspp2$spp),]

dbtprovspp2$y_pos <- seq_len(nrow(dbtprovspp2))

for(sp in species_order){
  idx <- which(dbtprovspp2$spp == sp)
  n <- length(idx)
  # assign sequential positions for this species
  dbtprovspp2$y_pos[idx] <- current_y:(current_y + n - 1)
  # move cursor down with a gap before next species cluster
  current_y <- current_y + n + gap
}

dbtprovspp2$y_pos

# set up empty plot
plot(NA, NA,
     xlim = range(c(dbt2$fit_per5-0.5, dbt2$fit_per95+0.5)),
     ylim = c(0.5, max(dbtprovspp2$y_pos) + 0.5),
     xlab = "Days to germinate?",
     ylab = "",
     yaxt = "n",
     main = "bt and bt_prov"
)

# add error bars
segments(
  x0 = dbtprovspp2$fit_per25,
  x1 = dbtprovspp2$fit_per75,
  y0 = dbtprovspp2$y_pos,
  col = adjustcolor(my_colors[dbtprovspp2$spp], alpha.f = 0.7),
  lwd = 1
)

# Add the points
points(
  dbtprovspp2$fit_mean,
  dbtprovspp2$y_pos,
  cex = 0.5,
  pch = 21,
  col = adjustcolor(my_colors[dbtprovspp2$spp], alpha.f = 1)
)

# Add species intervals and mean
dbt2$spp <- dbt2$spp_name
spp_y <- tapply(dbtprovspp2$y_pos, dbtprovspp2$spp, mean)
dbt2$y_pos <- spp_y[dbt2$numspp]

segments(
  x0 = dbt2$fit_per25,
  x1 = dbt2$fit_per75,
  y0 = dbt2$y_pos,
  col = adjustcolor(my_colors[dbt2$numspp], alpha.f = 1),
  lwd = 2
)

points(
  dbt2$fit_mean,
  dbt2$y_pos,
  pch = my_shapes[dbt2$woody],
  col  = adjustcolor(my_colors[dbt2$numspp], alpha.f = 1),
  # col = "black",
  cex = 1
)

# add vertical line at 0 
abline(v = 0, lty = 2)

# Add custom y-axis labels (reverse order if needed)
axis(
  side = 2,
  at = dbt2$y_pos,
  labels = dbt2$sppname,
  cex.axis = 0.5,
  las = 1
)

# spp mean
spp_y <- tapply(dbtprovspp2$y_pos, dbtprovspp2$spp, mean)

woody_legend_order <- c("Y", "N")
# woody legend
legend(
  x = max(dbt2$fit_per95) - 5,
  y = max(dbt2$y_pos) - 2,
  legend = woody_legend_order,
  pch = my_shapes[woody_legend_order],
  pt.cex = 1.2,
  title = "Woody (Y/N)",
  bty = "n"
)
dev.off()

# === === === === === === === === === === === === === === === === === === === 
##### bf_prov ##### 
# === === === === === === === === === === === === === === === === === === === 
# start with the intercept
bfmtrx <- data.frame(matrix(ncol = nrow(xindex), nrow = nrow(df_withprov)))

colnames(bfmtrx) <- xindex$numprov
colsbf <- cols[grepl("bf", cols) &
                 !grepl("prov", cols)]
colsbfprov <- cols[grepl("bf_prov", cols) ]

dbf <- subset(df_withprov, select = colsbf)
colnames(dbf) <- sub(".*\\[(\\d+)\\]", "\\1", colnames(dbf))

dbfprov <- subset(df_withprov, select = colsbfprov)
colnames(dbfprov) <- sub(".*\\[(\\d+)\\]", "\\1", colnames(dbfprov))

for (i in seq_len(ncol(bfmtrx))) { # i = 30
  prov_id <- as.integer(colnames(bfmtrx)[i])
  spp_id <- xindex$numspp[match(prov_id, xindex$numprov)]
  bfmtrx[, i] <- dbf[, spp_id]
}
bfmtrx

# sum spp values in matrix to provenance value
dbfprovspp <- bfmtrx + dbfprov[, colnames(bfmtrx)]

dbfprovspp2 <- data.frame(
  prmID = character(ncol(dbfprovspp)),
  fit_mean  = numeric(ncol(dbfprovspp)),  
  fit_per5  = NA, 
  fit_per25 = NA,
  fit_per75 = NA,
  fit_per95 = NA
)

for (i in 1:ncol(dbfprovspp)) { # i = 1
  dbfprovspp2$prmID[i] <- colnames(dbfprovspp)[i]         
  dbfprovspp2$fit_mean[i] <- round(mean(dbfprovspp[[i]]),3)  
  dbfprovspp2$fit_per5[i] <- round(quantile(dbfprovspp[[i]], probs = 0.05), 3)
  dbfprovspp2$fit_per25[i] <- round(quantile(dbfprovspp[[i]], probs = 0.25), 3)
  dbfprovspp2$fit_per75[i] <- round(quantile(dbfprovspp[[i]], probs = 0.75), 3)
  dbfprovspp2$fit_per95[i] <- round(quantile(dbfprovspp[[i]], probs = 0.95), 3)
}

# get just bf
bfvec <- paste("bf", "[", 1:length(unique(modeld$numspp)), "]", sep = "")
dbf2 <- subset(dwithprov2, prmID %in% bfvec)
dbf2$numspp <- as.numeric(sub(".*\\[(\\d+)\\]", "\\1", dbf2$prmID))
dbf2$sppname <- modeld$genusspecies[match(dbf2$numspp, modeld$numspp)]

# add species name to df
dbfprovspp2$numspp <- modeld$numspp[match(dbfprovspp2$prmID, modeld$numprov)]
dbfprovspp2$sppname <- modeld$genusspecies[match(dbfprovspp2$prmID, modeld$numprov)]

# add woody
dbf2$woody <- dmain$woody[match(dbf2$sppname, dmain$latbi)]
dbfprovspp2$woody <- dmain$woody[match(dbfprovspp2$sppname, dmain$latbi)]

jpeg(
  filename = "analyseSeedCues/provenance/figures/muPlotProv_bfprov.jpeg",
  width = 2400,      
  height = 2400,
  res = 300         
)
par(mar = c(4, 6, 4, 5))

# define a gap between species clusters
gap <- 3

# y positions
dbfprovspp2$y_pos <- NA
current_y <- 1

species_order <- as.character(1 : max(dbfprovspp2$numspp))

dbfprovspp2$spp  <- factor(dbfprovspp2$numspp, levels = species_order)

dbfprovspp2 <- dbfprovspp2[order(dbfprovspp2$spp),]

dbfprovspp2$y_pos <- seq_len(nrow(dbfprovspp2))

for(sp in species_order){
  idx <- which(dbfprovspp2$spp == sp)
  n <- length(idx)
  # assign sequential positions for this species
  dbfprovspp2$y_pos[idx] <- current_y:(current_y + n - 1)
  # move cursor down with a gap before next species cluster
  current_y <- current_y + n + gap
}

dbfprovspp2$y_pos

# set up empty plot
plot(NA, NA,
     xlim = range(c(dbf2$fit_per5-0.5, dbf2$fit_per95+0.5)),
     ylim = c(0.5, max(dbfprovspp2$y_pos) + 0.5),
     xlab = "Days to germinate?",
     ylab = "",
     yaxt = "n",
     main = "bf and bf_prov"
)

# add error bars
segments(
  x0 = dbfprovspp2$fit_per25,
  x1 = dbfprovspp2$fit_per75,
  y0 = dbfprovspp2$y_pos,
  col = adjustcolor(my_colors[dbfprovspp2$spp], alpha.f = 0.7),
  lwd = 1
)

# Add the points
points(
  dbfprovspp2$fit_mean,
  dbfprovspp2$y_pos,
  cex = 0.5,
  pch = 21,
  col = adjustcolor(my_colors[dbfprovspp2$spp], alpha.f = 1)
)

# Add species intervals and mean
dbf2$spp <- dbf2$spp_name
spp_y <- tapply(dbfprovspp2$y_pos, dbfprovspp2$spp, mean)
dbf2$y_pos <- spp_y[dbf2$numspp]

segments(
  x0 = dbf2$fit_per25,
  x1 = dbf2$fit_per75,
  y0 = dbf2$y_pos,
  col = adjustcolor(my_colors[dbf2$numspp], alpha.f = 1),
  lwd = 2
)

points(
  dbf2$fit_mean,
  dbf2$y_pos,
  pch = my_shapes[dbf2$woody],
  col  = adjustcolor(my_colors[dbf2$numspp], alpha.f = 1),
  # col = "black",
  cex = 1
)

# add vertical line at 0 
abline(v = 0, lty = 2)

# Add custom y-axis labels (reverse order if needed)
axis(
  side = 2,
  at = dbf2$y_pos,
  labels = dbf2$sppname,
  cex.axis = 0.5,
  las = 1
)

# spp mean
spp_y <- tapply(dbfprovspp2$y_pos, dbfprovspp2$spp, mean)

woody_legend_order <- c("Y", "N")
# woody legend
legend(
  x = max(dbf2$fit_per95) - 5,
  y = max(dbf2$y_pos) - 2,
  legend = woody_legend_order,
  pch = my_shapes[woody_legend_order],
  pt.cex = 1.2,
  title = "Woody (Y/N)",
  bty = "n"
)
dev.off()


# === === === === === === === === === === === === === === === === === === === 
##### bcs_prov ##### 
# === === === === === === === === === === === === === === === === === === === 
# start with the intercept
bcsmtrx <- data.frame(matrix(ncol = nrow(xindex), nrow = nrow(df_withprov)))

colnames(bcsmtrx) <- xindex$numprov
colsbcs <- cols[grepl("bcs", cols) &
                  !grepl("prov", cols)]
colsbcsprov <- cols[grepl("bcs_prov", cols) ]

dbcs <- subset(df_withprov, select = colsbcs)
colnames(dbcs) <- sub(".*\\[(\\d+)\\]", "\\1", colnames(dbcs))

dbcsprov <- subset(df_withprov, select = colsbcsprov)
colnames(dbcsprov) <- sub(".*\\[(\\d+)\\]", "\\1", colnames(dbcsprov))

for (i in seq_len(ncol(bcsmtrx))) { # i = 30
  prov_id <- as.integer(colnames(bcsmtrx)[i])
  spp_id <- xindex$numspp[match(prov_id, xindex$numprov)]
  bcsmtrx[, i] <- dbcs[, spp_id]
}
bcsmtrx

# sum spp values in matrix to provenance value
dbcsprovspp <- bcsmtrx + dbcsprov[, colnames(bcsmtrx)]

dbcsprovspp2 <- data.frame(
  prmID = character(ncol(dbcsprovspp)),
  fit_mean  = numeric(ncol(dbcsprovspp)),  
  fit_per5  = NA, 
  fit_per25 = NA,
  fit_per75 = NA,
  fit_per95 = NA
)

for (i in 1:ncol(dbcsprovspp)) { # i = 1
  dbcsprovspp2$prmID[i] <- colnames(dbcsprovspp)[i]         
  dbcsprovspp2$fit_mean[i] <- round(mean(dbcsprovspp[[i]]),3)  
  dbcsprovspp2$fit_per5[i] <- round(quantile(dbcsprovspp[[i]], probs = 0.05), 3)
  dbcsprovspp2$fit_per25[i] <- round(quantile(dbcsprovspp[[i]], probs = 0.25), 3)
  dbcsprovspp2$fit_per75[i] <- round(quantile(dbcsprovspp[[i]], probs = 0.75), 3)
  dbcsprovspp2$fit_per95[i] <- round(quantile(dbcsprovspp[[i]], probs = 0.95), 3)
}

# get just bcs
bcsvec <- paste("bcs", "[", 1:length(unique(modeld$numspp)), "]", sep = "")
dbcs2 <- subset(dwithprov2, prmID %in% bcsvec)
dbcs2$numspp <- as.numeric(sub(".*\\[(\\d+)\\]", "\\1", dbcs2$prmID))
dbcs2$sppname <- modeld$genusspecies[match(dbcs2$numspp, modeld$numspp)]

# add species name to df
dbcsprovspp2$numspp <- modeld$numspp[match(dbcsprovspp2$prmID, modeld$numprov)]
dbcsprovspp2$sppname <- modeld$genusspecies[match(dbcsprovspp2$prmID, modeld$numprov)]

# add woody
dbcs2$woody <- dmain$woody[match(dbcs2$sppname, dmain$latbi)]
dbcsprovspp2$woody <- dmain$woody[match(dbcsprovspp2$sppname, dmain$latbi)]

jpeg(
  filename = "analyseSeedCues/provenance/figures/muPlotProv_bcsprov.jpeg",
  width = 2400,      
  height = 2400,
  res = 300         
)
par(mar = c(4, 6, 4, 5))

# define a gap between species clusters
gap <- 3

# y positions
dbcsprovspp2$y_pos <- NA
current_y <- 1

species_order <- as.character(1 : max(dbcsprovspp2$numspp))

dbcsprovspp2$spp  <- factor(dbcsprovspp2$numspp, levels = species_order)

dbcsprovspp2 <- dbcsprovspp2[order(dbcsprovspp2$spp),]

dbcsprovspp2$y_pos <- seq_len(nrow(dbcsprovspp2))

for(sp in species_order){
  idx <- which(dbcsprovspp2$spp == sp)
  n <- length(idx)
  # assign sequential positions for this species
  dbcsprovspp2$y_pos[idx] <- current_y:(current_y + n - 1)
  # move cursor down with a gap before next species cluster
  current_y <- current_y + n + gap
}

dbcsprovspp2$y_pos

# set up empty plot
plot(NA, NA,
     xlim = range(c(dbcs2$fit_per5-0.5, dbcs2$fit_per95+0.5)),
     ylim = c(0.5, max(dbcsprovspp2$y_pos) + 0.5),
     xlab = "Days to germinate?",
     ylab = "",
     yaxt = "n",
     main = "bcs and bcs_prov"
)

# add error bars
segments(
  x0 = dbcsprovspp2$fit_per25,
  x1 = dbcsprovspp2$fit_per75,
  y0 = dbcsprovspp2$y_pos,
  col = adjustcolor(my_colors[dbcsprovspp2$spp], alpha.f = 0.7),
  lwd = 1
)

# Add the points
points(
  dbcsprovspp2$fit_mean,
  dbcsprovspp2$y_pos,
  cex = 0.5,
  pch = 21,
  col = adjustcolor(my_colors[dbcsprovspp2$spp], alpha.f = 1)
)

# Add species intervals and mean
dbcs2$spp <- dbcs2$spp_name
spp_y <- tapply(dbcsprovspp2$y_pos, dbcsprovspp2$spp, mean)
dbcs2$y_pos <- spp_y[dbcs2$numspp]

segments(
  x0 = dbcs2$fit_per25,
  x1 = dbcs2$fit_per75,
  y0 = dbcs2$y_pos,
  col = adjustcolor(my_colors[dbcs2$numspp], alpha.f = 1),
  lwd = 2
)

points(
  dbcs2$fit_mean,
  dbcs2$y_pos,
  pch = my_shapes[dbcs2$woody],
  col  = adjustcolor(my_colors[dbcs2$numspp], alpha.f = 1),
  # col = "black",
  cex = 1
)

# add vertical line at 0 
abline(v = 0, lty = 2)

# Add custom y-axis labels (reverse order if needed)
axis(
  side = 2,
  at = dbcs2$y_pos,
  labels = dbcs2$sppname,
  cex.axis = 0.5,
  las = 1
)

# spp mean
spp_y <- tapply(dbcsprovspp2$y_pos, dbcsprovspp2$spp, mean)

woody_legend_order <- c("Y", "N")
# woody legend
legend(
  x = max(dbcs2$fit_per95) - 5,
  y = max(dbcs2$y_pos) - 2,
  legend = woody_legend_order,
  pch = my_shapes[woody_legend_order],
  pt.cex = 1.2,
  title = "Woody (Y/N)",
  bty = "n"
)
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
ggsave("analyseSeedCues/provenance/figures/forcingComparison.jpeg", width = 12, height = 6, 
       units = "in", dpi = 300)
