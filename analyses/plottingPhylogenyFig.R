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

study <- contMap(egretTree, x, plot = T)


pdf("analyses/figures/usda_phyloIntColor.pdf", height = 45, width = 10)

### === === === === === === === === === === ###
#### Try with a subset of species to make the tree smaller ####
### === === === === === === === === === === ###
# select some random genus AND 3 that I will splice stuff in
vec <- c(sample(egret$genus, 3), c("Eucalyptus", "Betula", "Penstemon"))

t <- subset(egret, genus %in% vec)
spp_smalltree <- unique(t$sppMatch)
studyNosmall <- subset(studyNo, sppMatch %in% spp_smalltree)

smallTree <- keep.tip(phy.plants, which(phy.plants$tip.label %in% unique(t$sppMatch)))


smallnamesphy <- smallTree$tip.label

smallTree$root.edge <- 0

is.rooted(smallTree)
smallTree$node.label<-NULL


smallDataPhy <-  comparative.data(smallTree, studyNosmall, names.col = "sppMatch", na.omit = T,
                                  vcv = T, warn.dropped = T)

smallphytoplot <-  smallDataPhy$phy
smallx <- smallDataPhy$data$count
names(smallx) <- smallDataPhy$phy$tip.label

smallmap <- contMap(smallTree, smallx, plot = T)
