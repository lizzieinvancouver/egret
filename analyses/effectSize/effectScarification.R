## Started 12 Aug 2026 ##
## Started by Mao ##
## Understand the effect of scarification ##

library(ggplot2)
library(ape)
library(rstan)

if(length(grep("deirdreloughnan", getwd()) > 0)) {
  setwd("~/Documents/github/egret/analyses")
} else if(length(grep("lizzie", getwd()) > 0)) {
  setwd("/Users/lizzie/Documents/git/projects/egret/analyses")
} else if(length(grep("sapph", getwd()) > 0)) {
  setwd("/Users/sapph/Documents/ubc things/work/egret/analyses")
} else if(length(grep("dbuona", getwd()) > 0)) {
  setwd("/Users/dbuona/Documents/git/egret/analyses/")
} else if(length(grep("Xiaomao", getwd()) > 0)) {
  setwd("C:/PhD/Project/egret/analyses/")
}
util <- new.env()
source('mcmc_analysis_tools_rstan.R', local=util)
source('mcmc_visualization_tools.R', local=util)

d <- read.csv("output/egretclean.csv", header = TRUE)

d$scarification <- as.factor(d$scarification)
d$scarifTypeGen <- as.factor(d$scarifTypeGen)
d$scarifTypeSpe <- as.factor(d$scarifTypeSpe)
summary(d$scarification)
summary(d$scarifTypeGen)

d_scar <- d[!is.na(d$scarification), ]
summary(d_scar$scarification)

treatment_cols <- c("source.population","chillTemp","chillDuration","germTemp","germDuration","germPhotoperiod","chemicalCor","storageType","storageTemp","storageDuration","photoperiodCor","provLatLon","provLatLonAlt","treatmentOverview")

group_cols <- c("datasetIDstudy","latbi", treatment_cols)

# convert NA to an comparable value
make_key <- function(x) ifelse(is.na(x), "___NA___", as.character(x))

# build a grouping key from datasetID + species + treatment we care
key_cols <- c(group_cols, treatment_cols)
group_key <- do.call(paste, c(lapply(d_scar[key_cols], make_key), sep = "\r"))

# key for scarification values
scar_key <- make_key(d_scar[["scarification"]])

# select for rows with multiple levels
more_scar <- ave(scar_key, group_key, FUN = function(x) length(unique(x)) > 1)

# Subset
unique_scar <- d_scar[as.logical(more_scar), ]

# keep only percent germination
unique_scar_perc <- unique_scar[unique_scar$responseVar == "percent.germ", ]

# make a boxplot to plot two scarification group for selected rows of data

plot.new()

ggplot(unique_scar_perc, aes(x = scarification, y = responseValueNum, fill = scarification)) +
  geom_boxplot() +
  facet_wrap(~ datasetID) +
  labs(
    x = "Scarification",
    y = "Percent germination") +
  theme_minimal() +
  theme(legend.position = "none")

ggplot(unique_scar_perc, aes(x = scarification, y = responseValueNum, fill = scarification)) +
  geom_boxplot() +
  labs(
    x = "Scarification",
    y = "Percent germination") +
  theme_minimal() +
  theme(legend.position = "none")

# subset each single dataset
bhatt00exp4 <- unique_scar_perc[unique_scar_perc$datasetIDstudy == "bhatt00exp4", ]
bhatt00exp5 <- unique_scar_perc[unique_scar_perc$datasetIDstudy == "bhatt00exp5", ]
bhatt00exp6 <- unique_scar_perc[unique_scar_perc$datasetIDstudy == "bhatt00exp6", ]
parmenter96exp1 <-  unique_scar_perc[unique_scar_perc$datasetIDstudy == "parmenter96exp1", ]
parmenter96exp2 <-  unique_scar_perc[unique_scar_perc$datasetIDstudy == "parmenter96exp2", ]
alptekin02exp1 <-  unique_scar_perc[unique_scar_perc$datasetIDstudy == "alptekin02exp1", ]
amini18exp1 <-  unique_scar_perc[unique_scar_perc$datasetIDstudy == "amini18exp1", ] # potential problem?
amini18exp2 <-  unique_scar_perc[unique_scar_perc$datasetIDstudy == "amini18exp2", ] # potential problem?
cho18bexp1 <-  unique_scar_perc[unique_scar_perc$datasetIDstudy == "cho18bexp1", ]
chuanren04exp1 <-  unique_scar_perc[unique_scar_perc$datasetIDstudy == "chuanren04exp1", ]
dalling99exp4 <-  unique_scar_perc[unique_scar_perc$datasetIDstudy == "dalling99exp4", ]
naseri18exp1 <-  unique_scar_perc[unique_scar_perc$datasetIDstudy == "naseri18exp1", ]
naseri18exp2 <-  unique_scar_perc[unique_scar_perc$datasetIDstudy == "naseri18exp2", ]
rafiq21exp1 <-  unique_scar_perc[unique_scar_perc$datasetIDstudy == "rafiq21exp1", ] # potential problem
teimouri13exp1 <-  unique_scar_perc[unique_scar_perc$datasetIDstudy == "teimouri13exp1", ]
thomsen02exp1 <-  unique_scar_perc[unique_scar_perc$datasetIDstudy == "thomsen02exp1", ]
arslan11exp1 <-  unique_scar_perc[unique_scar_perc$datasetIDstudy == "arslan11exp1", ]
sharma03exp1 <-  unique_scar_perc[unique_scar_perc$datasetIDstudy == "sharma03exp1", ]
zhou03exp3 <-  unique_scar_perc[unique_scar_perc$datasetIDstudy == "zhou03exp3", ]
li11exp2 <-  unique_scar_perc[unique_scar_perc$datasetIDstudy == "li11exp2", ]
fulbright86exp1 <-  unique_scar_perc[unique_scar_perc$datasetIDstudy == "fulbright86exp1", ]

### Run a model on scarification checking for effect size
unique_scar_perc$responseValueNum <- ifelse(
  unique_scar_perc$responseValueNum > 1,
  unique_scar_perc$responseValueNum / 100,
  unique_scar_perc$responseValueNum
)

unique_scar_perc$responseValueNum <- pmin(unique_scar_perc$responseValueNum, 1)
unique_scar_perc$scarification <- ifelse(unique_scar_perc$scarification == "Y", 1, 0)
phylo <- ape::read.tree("output/usdaEgretFull.tre")
scar_tree <- keep.tip(phylo, intersect(unique(unique_scar_perc$latbi), phylo$tip.label))

cphy <- ape::vcv.phylo(scar_tree,corr=TRUE)


unique_scar_perc$numspp = as.integer(factor(unique_scar_perc$latbi, levels = colnames(cphy)))

scarData <- list(N_degen = sum(unique_scar_perc$responseValueNum %in% c(0,1)),
                      N_prop = sum(unique_scar_perc$responseValueNum>0 & unique_scar_perc$responseValueNum<1),
                      
                      Nsp =  length(unique(unique_scar_perc$latbi)),
                      sp_degen = array(unique_scar_perc$numspp[unique_scar_perc$responseValueNum %in% c(0,1)],
                                       dim = sum(unique_scar_perc$responseValueNum%in% c(0,1))),
                      sp_prop = array(unique_scar_perc$numspp[unique_scar_perc$responseValueNum>0 & unique_scar_perc$responseValueNum<1],
                                      dim = sum(unique_scar_perc$responseValueNum>0 & unique_scar_perc$responseValueNum<1)),
                      
                      y_degen = array(unique_scar_perc$responseValueNum[unique_scar_perc$responseValueNum %in% c(0,1)],
                                      dim = sum(unique_scar_perc$responseValueNum%in% c(0,1))),
                      y_prop = array(unique_scar_perc$responseValueNum[unique_scar_perc$responseValueNum>0 & unique_scar_perc$responseValueNum<1],
                                     dim = sum(unique_scar_perc$responseValueNum>0 & unique_scar_perc$responseValueNum<1)),
                      scar_degen = array(unique_scar_perc$scarification[unique_scar_perc$responseValueNum %in% c(0,1)],
                                      dim = sum(unique_scar_perc$responseValueNum%in% c(0,1))),
                      scar_prop = array(unique_scar_perc$scarification[unique_scar_perc$responseValueNum>0 & unique_scar_perc$responseValueNum<1],
                                     dim = sum(unique_scar_perc$responseValueNum>0 & unique_scar_perc$responseValueNum<1)),
                      
                      Vphy = cphy)

scarificationModel <-stan_model("stan/scarificationModel.stan")
fit <- sampling(scarificationModel, scarData, 
                iter = 4000, warmup = 3000,
                chains = 4)

diagnostics <- util$extract_hmc_diagnostics(fit)

print(util$check_all_hmc_diagnostics(diagnostics))

samples <- util$extract_expectand_vals(fit)
names <- c(grep('a_z', names(samples), value = TRUE),
           grep('lambda_a', names(samples), value = TRUE),
           grep('sigma_a', names(samples), value = TRUE),
           grep('b_scar', names(samples), value = TRUE),
           grep('kappa', names(samples), value = TRUE))

base_samples <- util$filter_expectands(samples,names)
print(util$check_all_expectand_diagnostics(base_samples))
print(fit, pars = names)

parameter_scar <- c(names(fit)[grep("b_scar", names(fit))])
stats <- summary(fit, pars = parameter_scar, probs = c(0.25, 0.75))$summary
scar <- as.data.frame(stats)
scar$parameter <- rownames(scar)
colnames(scar)[grep("25%", colnames(scar))] <- "low"
colnames(scar)[grep("75%", colnames(scar))] <- "high"

pdf("C:/PhD/Project/egret/analyses/effectSize/figure/scarificationEffect.pdf", width = 5, height = 5)

ggplot(scar, aes(x = mean, y = parameter)) +
  geom_point(size = 2, alpha = 1) + 
  geom_errorbar(aes(xmin = low, 
                    xmax = high), 
                width = 0, alpha = 1, linewidth = 1) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  labs(y = "", x = "Scarification") +
  theme(
    axis.text.y = element_blank(), axis.ticks.y = element_blank(),
    legend.title = element_text(size = 12, face = "bold"),  
    legend.text = element_text(size = 10),                  
    legend.key.size = unit(1.5, "lines"), legend.position = "right"             
  ) +
  theme_minimal() +
  scale_y_discrete(limits = rev)  

dev.off()
