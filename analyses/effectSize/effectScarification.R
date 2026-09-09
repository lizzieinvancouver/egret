## Started 12 Aug 2026 ##
## Started by Mao ##
## Understand the effect of scarification ##

library(ggplot2)

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

d <- read.csv("output/egretclean.csv", header = TRUE)

d$scarification <- as.factor(d$scarification)
d$scarifTypeGen <- as.factor(d$scarifTypeGen)
d$scarifTypeSpe <- as.factor(d$scarifTypeSpe)
summary(d$scarification)
summary(d$scarifTypeGen)

d_scar <- d[!is.na(d$scarification), ]
summary(d_scar$scarification)

treatment_cols <- c("chillTemp","chillDuration","germTemp","germDuration","germPhotoperiod","chemicalCor","storageType","storageTemp","storageDuration","photoperiodCor","provLatLon","provLatLonAlt","treatmentOverview")

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

# subset each single dataset
bhatt00exp4 <- unique_scar_perc[unique_scar_perc$datasetIDstudy == "bhatt00exp4", ]
bhatt00exp5 <- unique_scar_perc[unique_scar_perc$datasetIDstudy == "bhatt00exp5", ]
bhatt00exp6 <- unique_scar_perc[unique_scar_perc$datasetIDstudy == "bhatt00exp6", ]
