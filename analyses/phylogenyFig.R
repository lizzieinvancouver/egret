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
library(WorldFlora)

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

# Get a list of synonyms for ALL species that aren't from the worldflora package
setwd("/Users/christophe_rouleau-desrochers/Desktop/UBC/egretLOCAL")
wfodf <- read.csv("classification.csv", header = TRUE, sep = "\t")

# pull all the fuzzy matches, which included those at infraspecifc level
e1 <- WFO.match(spec.data=egret.phenosp.sps.inphylo, WFO.data=wfodf, counter=1,
                Fuzzy.min=FALSE, Fuzzy.shortest=FALSE, verbose=TRUE)
head(e1$Old.name)
head(phy.sps.uniqu)
head(egret.phenosp.sps.inphylo)
# replace _ by blank space for species names
phy.sps.uniqu_ <- gsub("_", " ", phy.sps.uniqu)
# subset all entries that are categorized as synonyms by worldflora
syn <- subset(e1, Old.status == "Synonym")
wfovec <- syn$Old.name
# check occurence of synonyms in tree
matches <- wfovec[wfovec %in% phy.sps.uniqu_] # 16 matches out of 48.

# check one example for which there isn't a match
e1$Old.name[grep("Pyrus", e1$Old.name)]
phy.sps.uniqu[grep("Pyrus_syri", phy.sps.uniqu)] # ok so the package doesn't pull p. syriaca as a synonym, but it pulls it on the website. Lets look into this further:
pyrus <- subset(e1, spec.name == "Pyrus_glabra") # pulls 4 synonym which don't match the ones on the website.
pyrus$Old.name
# let's take another example: Acer_hyrcanum
phy.sps.uniqu_[grep("Acer h", phy.sps.uniqu_)] # in the tree, the only entry that matches this is Acer hyrcanum subsp. hyrcanum
acerh <- subset(e1, spec.name == "Acer_hyrcanum") # and in wold flora there is no subs hgyrcanum...
acerh$Old.name
# on the website, the synonyms are: 


# let's try when there is no _
egret.phenosp.sps.inphylo_ <- gsub("_", " ", egret.phenosp.sps.inphylo)
e1no_ <- WFO.match(spec.data=egret.phenosp.sps.inphylo, WFO.data=wfodf, counter=1,
                Fuzzy.min=FALSE, Fuzzy.shortest=FALSE, verbose=TRUE)
synno_ <- subset(e1no_, Old.status == "Synonym")
wfovecno_ <- synno_$Old.name
# check occurence of synonyms in tree
matchesno_ <- wfovecno_[wfovecno_ %in% phy.sps.uniqu_] # 16 matches out of 48 still, so changing to no _ doens't make a difference
# check with WFO.synonyms
syno <- WFO.synonyms(egret.phenosp.sps.inphylo, WFO.file = NULL, WFO.data = wfodf)
# check with WFO.synonyms with no _
WFO.synonyms(egret.phenosp.sps.inphylo_, WFO.file = NULL, WFO.data = wfodf) # error message
# go smaller
subby <- egret.phenosp.sps.inphylo[1:35]
synosubby <- WFO.synonyms(subby, WFO.file = NULL, WFO.data = wfodf) 
#check with WFO.browse
bro <- WFO.browse(egret.phenosp.sps.inphylo, WFO.file = NULL, WFO.data = wfodf,
           accepted.only = FALSE, acceptedNameUsageID.match = TRUE)

syn <- WFO.synonyms(spec.test, WFO.data = wfodf) 
syn <- WFO.synonyms(spec.test, WFO.file = wfodf, WFO.data = NULL) 

# try the vignette with the old package and lizzies loaded bryophyte data
setwd("/Users/christophe_rouleau-desrochers/Downloads")
bryophytes <- read.csv("bryophytes.csv")

# check species name
w1 <- WFO.match(bryophytes[1:20, ], WFO.file=WFO.file.RK, spec.name="Full.name", counter=1)

# rename wfodf to fit the name of the vignette
WFO.file.RK <- wfodf

# check species name
w1 <- WFO.match(bryophytes[1:20, ], WFO.file=WFO.file.RK, spec.name="Full.name", counter=1)

-## first prune the phylogeny to include$ only these genera
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


