# Started Nov 27, 2023 by Deirdre L.
# Aim of this code is to get a phylogeny for all of the spp we have germination data scraped

if(length(grep("deirdreloughnan", getwd()) > 0)) {
  setwd("~/Documents/github/egret")
} else if(length(grep("Lizzie", getwd()) > 0)) {
  setwd("~/Documents/git/projects/others/deirdre/egret")
} else{
  setwd("/home/deirdre/egret") # for midge
}

library(tidyverse)
library(stringr)
library(ape)
library(phytools)
library(geiger)
library(pez)
library(caper)
library(phangorn)

rm(list = ls()) # Clear whatever is already in R's memory
options(stringsAsFactors=FALSE)# Make sure words are read in as characters rather than factors

egret <- read.csv("input/egretData.csv")
egret$sp.name <- paste(egret$genus, egret$species, sep = "_")

sps.list <- sort(unique(egret$sp.name))
genus.list=sort(unique(egret$genus))

## load phylo (from Smith and Brown 2019)
phy.plants<-read.tree("..//pheno_bc/data/ALLMB.tre")

## getting a list of genera in S&B's phylo
phy.genera<-unlist(
  lapply(strsplit(phy.plants$tip.label, "_"),function(x){return(x[1])})
)
phy.genera.uniq<-sort(unique(phy.genera))

## how many phenobc species are in the phylogeny?
phenosp.genus.inphylo<-genus.list[which(!genus.list%in%phy.genera.uniq)] #182 out of our 185

## first prune the phylogeny to include$ only these genera
# phy.genera.egret<-drop.tip(phy.plants,
#                              which(!phy.genera %in% phenosp.genus.inphylo)) #34940 tips
# length(phy.genera.egret$tip.label)
tree <- drop.tip(phy.plants, which(!phy.plants$tip.label %in% sps.list))

length(tree$tip.label)
sort(tree$tip.label)

write.tree(tree,"analyses/output/egretPhylogeny.tre")

# only 273 species are in the phylogeny, lost 61

unique(egret$datasetID)

egret$count <- 1
egretSub <- unique(egret[,c("sp.name", "datasetID", "count")])
studyNo <- aggregate(egretSub["count"], egretSub[c("sp.name")], FUN = sum)

temp <- subset(studyNo, count >1) # 26 sp with more than one study

# [1] "Asparagus_acutifolius"   "Betula_utilis"           "Clematis_vitalba"        "Colutea_armena"         
# [5] "Degenia_velebitica"      "Dorema_ammoniacum"       "Echinacea_angustifolia"  "Echinacea_purpurea"     
# [9] "Eucalyptus_delegatensis" "Fagus_sylvatica"         "Ferula_assa-foetida"     "Ferula_gummosa"         
# [13] "Gentiana_lutea"          "Hippophae_rhamnoides"    "Jatropha_curcas"         "Juniperus_communis"     
# [17] "Kelussia_odoratissima"   "Phlox_pilosa"            "Picea_glauca"            "Pinus_koraiensis"       
# [21] "Pinus_strobus"           "Primula_veris"           "Prunus_africana"         "Trema_cannabina"        
# [25] "Tsuga_heterophylla"      "Zizania_palustris"

namesphy <- tree$tip.label
tree$root.edge <- 0

is.rooted(tree)
tree$node.label<-NULL

dataPhy = comparative.data(tree, studyNo, names.col = "sp.name", na.omit = T,
                           vcv = T, warn.dropped = T)

phyloplot = dataPhy$phy
x = dataPhy$data$count
names(x)=dataPhy$phy$tip.label

study <- contMap(tree, x, plot = T)

slopeCol <- setMap(study, colors=c("blue","yellow","red"))
h<-max(nodeHeights(slopeCol$tree))

pdf("figures/phyloIntColor.pdf", height = 20, width = 7)
plot(slopeCol,legend = F, lwd=3, ylim=c(1-0.09*(Ntip(slopeCol$tree)),Ntip(slopeCol$tree)))

dev.off()
