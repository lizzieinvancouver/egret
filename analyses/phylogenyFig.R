# Started Nov 27, 2023 by Deirdre L.
# Aim of this code is to get a phylogeny for all of the spp we have germination data scraped

if(length(grep("deirdreloughnan", getwd()) > 0)) {
  setwd("~/Documents/github/egret")
} else if(length(grep("Lizzie", getwd()) > 0)) {
  setwd("~/Documents/git/projects/others/deirdre/egret")
} else if (length(grep("christophe_rouleau-desrochers", getwd())) > 0) {
  setwd("/Users/christophe_rouleau-desrochers/github/egret") # Replace with the correct path for Christophe
} else {
  setwd("/home/deirdre/egret") # for midge
}

#library(tidyverse)
library(stringr)
library(ape)
library(phytools)
library(geiger)
#library(pez)
library(caper)
library(phangorn)
library(taxize)

rm(list = ls()) # Clear whatever is already in R's memory
options(stringsAsFactors=FALSE)# Make sure words are read in as characters rather than factors

egret <- read.csv("analyses/output/egretclean.csv")
usda <- read.csv("analyses/output/usdaGerminationCleaned.csv")

## load phylo (from Smith and Brown 2019)
phy.plants<-read.tree("analyses/input/ALLMB.tre")

## getting a list of genera in S&B's phylo
phy.genera<-unlist(
  lapply(strsplit(phy.plants$tip.label, "_"),function(x){return(x[1])})
)
phy.genera.uniq<-sort(unique(phy.genera))
phy.sps.uniqu <- phy.plants$tip.label
# === === === === === === === === === ===  === === === === ===  === === === === 
### Start with Egret ###
# === === === === === === === === === ===  === === === === ===  === === === === 
egret.sps <- sort(unique(egret$latbi))
egret.genus=sort(unique(egret$genus))

## how many phenobc genus are NOT in the phylogeny?
egret.phenosp.genus.inphylo<-genus.list[which(!egret.genus%in%phy.genera.uniq)] #186 out of 187
## how many phenobc species are NOT in the phylogeny?
egret.phenosp.sps.inphylo<-egret.sps[which(!egret.sps%in%phy.sps.uniqu)] #48 out of 335

# check synonyms using 2 databases from the package taxize
### remove Eucalyptus_pauciflora ,Aster_laevis, Carex_scoparia for now 
# vec <- c("Aster_laevis", "Carex_scoparia", "Eucalyptus_pauciflora", "Eupatorium_maculatum", "Fagus_sylvatica", "Heliopsis_helianthoides", "NA_NA", "Penstemon_scariosus", "Penstemon_pachyphyllus", "Phlox_maculata", "Phlox_pilosa", "Potentilla_argentea", "Symphyotrichum_oolentangiense", "Verbesina_encelioides")
# egret.phenosp.sps.inphylo <- egret.phenosp.sps.inphylo[!egret.phenosp.sps.inphylo %in% vec]
# try with db itis first
# Load the taxize package



# Define your list of species

# View results

# Print the final decisions
print(decisions)
## fix genus 
# Chrysojasminum: according to wiki, it's a synonym of jasminum 
# GIT ISSUE: lizzie will find out if there is a more official ressource for Jasminum 
# Paper: Jasminus changed by world flora to Chrysojasminum, found online here (https://powo.science.kew.org/taxon/77144221-1#synonyms) that it's a synonym of Jasminum WHICH is the genus name in the tree package. It fits Jasminum_fruticans in the tree. 
phy.genera.uniq[grep("Jasminum", phy.genera.uniq)]
phy.sps.uniqu[grep("Jasminum_fruticans", phy.sps.uniqu)]
suby <- subset(egret, genus == "Chrysojasminum")
unique(suby$datasetID)

## fix species
# Abies_marocana: would be a subspecies of Abies_pinsapo, which is in the tree
phy.sps.uniqu[grep("Abies_pinsapo", phy.sps.uniqu)]
# Acer_coriaceifolium: Acer_cinnamomifolium which is in the tree
# https://www.worldfloraonline.org/taxon/wfo-0000514152
phy.sps.uniqu[grep("Acer_cinnamo", phy.sps.uniqu)]
# s
phy.sps.uniqu[grep("Acer_hyrcanum", phy.sps.uniqu)]
# Alstroemeria_ligtu
phy.sps.uniqu[grep("Alstroemeria_ligtu", phy.sps.uniqu)]
# Aster_laevis
phy.sps.uniqu[grep("Aster_laevis", phy.sps.uniqu)]
# Betonica_bulgarica
phy.sps.uniqu[grep("Betonica_bulgarica", phy.sps.uniqu)]
# Betula_pendula subsp. mandshurica
phy.sps.uniqu[grep("Betula_pendula subsp. mandshurica", phy.sps.uniqu)]
# Betula_utilis subsp. albosinensis
phy.sps.uniqu[grep("Betula_utilis subsp. albosinensis", phy.sps.uniqu)]
# Calligonum_alaschanicum
phy.sps.uniqu[grep("Calligonum_alaschanicum", phy.sps.uniqu)]
# Carex_scoparia
phy.sps.uniqu[grep("Carex_scoparia", phy.sps.uniqu)]
# Celtis_pallida
phy.sps.uniqu[grep("Celtis_pallida", phy.sps.uniqu)]
# Chrysojasminum_fruticans
phy.sps.uniqu[grep("Chrysojasminum_fruticans", phy.sps.uniqu)]
# Crambe_hispanica subsp. abyssinica
phy.sps.uniqu[grep("Crambe_hispanica subsp. abyssinica", phy.sps.uniqu)]
# Dianthus_arenarius
phy.sps.uniqu[grep("Dianthus_arenarius", phy.sps.uniqu)]
# Echinacea_spp.
phy.sps.uniqu[grep("Echinacea_spp.", phy.sps.uniqu)]
# Eucalyptus_delegatensis
phy.sps.uniqu[grep("Eucalyptus_delegatensis", phy.sps.uniqu)]
# Eucalyptus_ovata
phy.sps.uniqu[grep("Eucalyptus_ovata", phy.sps.uniqu)]
# Eucalyptus_pauciflora
phy.sps.uniqu[grep("Eucalyptus_pauciflora", phy.sps.uniqu)] # problem with taxise package where it finds multiple entries. Removed for now but will need to be fixed
# Eucalyptus_pauciflora subsp. niphophila
phy.sps.uniqu[grep("Eucalyptus_pauciflora subsp. niphophila", phy.sps.uniqu)]
# Eucomis_autumnalis
phy.sps.uniqu[grep("Eucomis_autumnalis", phy.sps.uniqu)]
# Eupatorium_maculatum
phy.sps.uniqu[grep("Eupatorium_maculatum", phy.sps.uniqu)]
# Fagus_sylvatica
phy.sps.uniqu[grep("Fagus_sylvatica", phy.sps.uniqu)]
# Gentiana_lutea
phy.sps.uniqu[grep("Gentiana_lutea", phy.sps.uniqu)]
# Guizotia_scabra
phy.sps.uniqu[grep("Guizotia_scabra", phy.sps.uniqu)]
# Heliopsis_helianthoides
phy.sps.uniqu[grep("Heliopsis_helianthoides", phy.sps.uniqu)]
# Leontice_incerta
phy.sps.uniqu[grep("Leontice_incerta", phy.sps.uniqu)]
# Leuzea_carthamoides
phy.sps.uniqu[grep("Leuzea_carthamoides", phy.sps.uniqu)]
# Loranthus_tanakae
phy.sps.uniqu[grep("Loranthus_tanakae", phy.sps.uniqu)]
# Maackia_taiwanensis
phy.sps.uniqu[grep("Maackia_taiwanensis", phy.sps.uniqu)]
# NA_NA
phy.sps.uniqu[grep("NA_NA", phy.sps.uniqu)]
# Penstemon_pachyphyllus
phy.sps.uniqu[grep("Penstemon_pachyphyllus", phy.sps.uniqu)]
# Penstemon_scariosus
phy.sps.uniqu[grep("Penstemon_scariosus", phy.sps.uniqu)]
# Phlox_maculata
phy.sps.uniqu[grep("Phlox_maculata", phy.sps.uniqu)]
# Phlox_pilosa
phy.sps.uniqu[grep("Phlox_pilosa", phy.sps.uniqu)]
# Potentilla_argentea
phy.sps.uniqu[grep("Potentilla_argentea", phy.sps.uniqu)]
# Potentilla_reptans
phy.sps.uniqu[grep("Potentilla_reptans", phy.sps.uniqu)]
# Primula_bulleyana subsp. beesiana
phy.sps.uniqu[grep("Primula_bulleyana subsp. beesiana", phy.sps.uniqu)]
# Prunus_lusitanica subsp. azorica
phy.sps.uniqu[grep("Prunus_lusitanica subsp. azorica", phy.sps.uniqu)]
# Pyrus_glabra
phy.sps.uniqu[grep("Pyrus_glabra", phy.sps.uniqu)]
# Rosa_damascena
phy.sps.uniqu[grep("Rosa_damascena", phy.sps.uniqu)]
# Solidago_albopilosa
phy.sps.uniqu[grep("Solidago_albopilosa", phy.sps.uniqu)]
# Solidago_niederederi
phy.sps.uniqu[grep("Solidago_niederederi", phy.sps.uniqu)]
# Symphyotrichum_oolentangiense
phy.sps.uniqu[grep("Symphyotrichum_oolentangiense", phy.sps.uniqu)]
# Taraxacum_platycarpum
phy.sps.uniqu[grep("Taraxacum_platycarpum", phy.sps.uniqu)]
# Thymophylla_tephroleuca
phy.sps.uniqu[grep("Thymophylla_tephroleuca", phy.sps.uniqu)]
# Tilia_platyphyllos subsp. corinthiaca
phy.sps.uniqu[grep("Tilia_platyphyllos subsp. corinthiaca", phy.sps.uniqu)]
# Verbesina_encelioides
phy.sps.uniqu[grep("Verbesina_encelioides", phy.sps.uniqu)]
# Veronicastrum_sibiricum
phy.sps.uniqu[grep("Veronicastrum_sibiricum", phy.sps.uniqu)]
## first prune the phylogeny to include$ only these genera
# phy.genera.egret<-drop.tip(phy.plants,
#                              which(!phy.genera %in% phenosp.genus.inphylo)) #34940 tips
# length(phy.genera.egret$tip.label)
egret.tree <- keep.tip(phy.plants, which(phy.plants$tip.label %in% egret.sps))

length(egret.tree$tip.label)
length(unique(egret$latbi))
length(unique(egret$latbi))-length(egret.tree$tip.label)
sort(egret.tree$tip.label)

write.tree(egret.tree,"analyses/output/egretPhylogeny.tre")

# only 288 species are in the phylogeny, ** are lost 
egret$count <- 1
egretSub <- unique(egret[,c("latbi", "datasetID", "count")])
studyNo <- aggregate(egretSub["count"], egretSub[c("latbi")], FUN = sum)
temp <- subset(studyNo, count >1) 
nrow(temp)# 23 sp with more than one study

namesphy <- tree$tip.label
tree$root.edge <- 0

is.rooted(tree)
tree$node.label<-NULL

# Egret species 
dataPhy = comparative.data(tree, studyNo, names.col = "latbi", na.omit = T,
                           vcv = T, warn.dropped = T)

phyloplot = dataPhy$phy
x = dataPhy$data$count
names(x)=dataPhy$phy$tip.label

study <- contMap(tree, x, plot = T)

slopeCol <- setMap(study, colors=c("blue","yellow","red"))
h<-max(nodeHeights(slopeCol$tree))

pdf("analyses/figures/egret_phyloIntColor.pdf", height = 45, width = 10)
plot(slopeCol,legend = F, lwd=3, ylim=c(1-0.09*(Ntip(slopeCol$tree)),Ntip(slopeCol$tree)))

dev.off()

# === === === === === === === === === ===  === === === === ===  === === === === 
### Start with USDA ###
# === === === === === === === === === ===  === === === === ===  === === === === 
usda.sps <- sort(unique(usda$latbi))
usda.genus=sort(unique(usda$genus))
length(usda.genus)
length(usda.sps)
## how many phenobc genus are in the phylogeny?
phenosp.genus.inphylo<-genus.list[which(!usda.genus%in%phy.genera.uniq)] #186 out of 187

## first prune the phylogeny to include$ only these genera
# phy.genera.usda<-drop.tip(phy.plants,
#                              which(!phy.genera %in% phenosp.genus.inphylo)) #34940 tips
# length(phy.genera.usda$tip.label)
usda.tree <- keep.tip(phy.plants, which(phy.plants$tip.label %in% usda.sps))

length(usda.tree$tip.label)
length(unique(usda$latbi))
length(unique(usda$latbi))-length(usda.tree$tip.label)
sort(usda.tree$tip.label)

write.tree(usda.tree,"analyses/output/usdaPhylogeny.tre")

# only 288 species are in the phylogeny, ** are lost 
usda$count <- 1
usdaSub <- unique(usda[,c("latbi", "datasetID", "count")])
studyNo <- aggregate(usdaSub["count"], usdaSub[c("latbi")], FUN = sum)
temp <- subset(studyNo, count >1) 
nrow(temp)# 23 sp with more than one study

namesphy <- tree$tip.label
tree$root.edge <- 0

is.rooted(tree)
tree$node.label<-NULL

# usda species 
dataPhy = comparative.data(tree, studyNo, names.col = "latbi", na.omit = T,
                           vcv = T, warn.dropped = T)

phyloplot = dataPhy$phy
x = dataPhy$data$count
names(x)=dataPhy$phy$tip.label

study <- contMap(tree, x, plot = T)

slopeCol <- setMap(study, colors=c("blue","yellow","red"))
h<-max(nodeHeights(slopeCol$tree))

pdf("analyses/figures/usda_phyloIntColor.pdf", height = 45, width = 10)
plot(slopeCol,legend = F, lwd=3, ylim=c(1-0.09*(Ntip(slopeCol$tree)),Ntip(slopeCol$tree)))

dev.off()


