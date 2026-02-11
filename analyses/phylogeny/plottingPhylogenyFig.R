# Started on 25 June 2025 by CRD
# Taken from the main phylogeny script to create the tree, but this one will be specifically for plotting the tree

if(length(grep("deirdreloughnan", getwd()) > 0)) {
  setwd("~/Documents/github/egret")
} else if(length(grep("Lizzie", getwd()) > 0)) {
  setwd("~/Documents/git/projects/others/deirdre/egret")
} else if (length(grep("christophe_rouleau-desrochers", getwd())) > 0) {
  setwd("/Users/christophe_rouleau-desrochers/github/egret") # Replace with the correct path for Christophe
} else if (length(grep("victor", getwd())) > 0) {
  setwd("/home/victor/projects/egret") # Replace with the correct path for Christophe
} else {
  setwd("/home/deirdre/egret") # for midge
}

# libraries
library(phytools)
library(taxize)
library(ape)

# read tree
egretTree <- read.tree("analyses/output/egretPhylogeny.tre")

# below is the code to create plot the egret tree. Thus I need to load the tree first
#### Phylogeny figure whole tree ####
egret$count <- 1
egretSub <- unique(egret[,c("sppMatch", "datasetID", "count")])
studyNo <- aggregate(egretSub["count"], egretSub[c("sppMatch")], FUN = sum)
temp <- subset(studyNo, count >1) 
nrow(temp)# 23 sp with more than one study

namesphy <- egretTree$tip.label
egretTree$root.edge <- 0

is.rooted(egretTree)
egretTree$node.label<-NULL

# create data for all species 
dataPhy <-  comparative.data(egretTree, studyNo, names.col = "sppMatch", na.omit = T,
                             vcv = T, warn.dropped = T)

phyloplot <-  dataPhy$phy
x <- dataPhy$data$count
names(x) <- dataPhy$phy$tip.label

pdf("analyses/figures/egret_contmap.pdf", width = 20, height = 50)
# plot(tree, cex = 0.3)
study <- contMap(egretTree, x, plot = T)
dev.off()



