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
# remove _
phy.sps.uniqu <- gsub("_", " ", phy.sps.uniqu)
# === === === === === === === === === ===  === === === === ===  === === === ===
### Start with Egret ###
# === === === === === === === === === ===  === === === === ===  === === === ===
egret$latbi <-gsub("_", " ", egret$latbi)
egret.sps <- sort(unique(egret$latbi))
egret.genus=sort(unique(egret$genus))

## how many phenobc genus are NOT in the phylogeny?
egret.phenosp.genus.inphylo<-genus.list[which(!egret.genus%in%phy.genera.uniq)] #186 out of 187
## how many phenobc species are NOT in the phylogeny?
egret.phenosp.sps.inphylo<-egret.sps[which(!egret.sps%in%phy.sps.uniqu)] #48 out of 335

# Get a list of synonyms for ALL species that aren't from the worldflora package
setwd("/Users/christophe_rouleau-desrochers/Desktop/UBC/egretLOCAL")
# wfodf <- read.csv("classification.csv", header = TRUE, sep = "\t")
kew <- read.csv("wcvp/wcvp_names.csv", header = TRUE, sep = "|", stringsAsFactors = FALSE)
head(kew)
### look up kew to start
unique(kew$taxon_status)


kewvec <- unique(kew$taxon_name)
# now look up only for the species that we don't have a match with the tree:
matchestree <- egret.phenosp.sps.inphylo[egret.phenosp.sps.inphylo %in% kewvec] 
length(matchestree) # 32 species in egret are not in kew taxon_name column. Below I will extract all taxon names that return a common accepted parent name ID

# create df of species names from egret
sppegretkew <- subset(egret, latbi %in% egret.phenosp.sps.inphylo)
# remove duplicated rows
sppegretkew <- sppegretkew[!duplicated(sppegretkew$latbi), ]
# select only species column
sppegretkew2 <- sppegretkew[, c("datasetID", "latbi", "genus","species")]

# add accepted plant name ID from kewsub accepted_plant_name_id

# subset in kew's for all 48 species that we don't have a match in the tree
kewsub <- subset(kew, taxon_name %in% egret.phenosp.sps.inphylo)

# grab all parent ID for these species
accparentIDs <- kewsub$accepted_plant_name_id
# pull a vector of species names that correspond to these accepted IDs.
sub <- subset(kew, accepted_plant_name_id %in% accparentIDs)
# remove NAs
suby <- sub[sub$accepted_plant_name_id != "", ]
# pull a vector of all of these species 
kewnames <- suby$taxon_name
# look how many of these species are in the phylogeny tree:
withaccepted<-kewnames[which(kewnames%in%phy.sps.uniqu)] 
# subset kew for these species
matchednames <- subset(kew, taxon_name %in% withaccepted)
# remove unecessary columns
matchednamessub <- matchednames[, c("accepted_plant_name_id", "taxon_name")]

# add parent name ID in the df containing ALL the species we don't have match for in the tree
latbiwithId <- merge(sppegretkew2, kewsub[, c("taxon_name", "accepted_plant_name_id")], 
             by.x = "latbi", by.y = "taxon_name", 
             all.x = TRUE)

# merge matchednamessub and sppegretkew by accepted_plant_name_id
matchednamesegret <- merge(latbiwithId, matchednamessub, by = "accepted_plant_name_id", all.x = TRUE) 
# change colnames
colnames(matchednamesegret) <- c("accepted_plant_name_id", "egretname", "datasetID", "genus", "species", "matchedName")

nrow(matchednamesegret[!duplicated(matchednamesegret$egretname), ])

nomatch <- matchednamesegret[which(is.na(matchednamesegret$matchedName)),]
# remove anoying row of NA NA
nomatch <- subset(nomatch, egretname != "NA NA")
# now grab the varieties for the latbi names that we don't have matches in the tree

# ok now we have new taxon names, for some species more than name. Below Ill investigate why the remaining don't have matches

# Betonica bulgarica: only one synonym = Stachys bulgarica and it's not in the tree
phy.sps.uniqu[grepl("Stachys bulgarica", phy.sps.uniqu)] 

# Leontice incerta: only one synonym =Leontice vesicaria and it's not in the tree
phy.sps.uniqu[grepl("Leontice vesicaria", phy.sps.uniqu)] 

# Maackia taiwanensis. Synonyms: none
phy.sps.uniqu[grepl("taiwanensis", phy.sps.uniqu)] 

# Penstemon pachyphyllus. Synonyms: 
kew$taxon_name[grepl("Penstemon pachyphyllus", kew$taxon_name)] 
phy.sps.uniqu[grepl("Penstemon pachyphyllus var. pachyphyllus", phy.sps.uniqu)] 
nomatch$matchedName[which(nomatch$egretname == "Penstemon pachyphyllus")] <- "Penstemon pachyphyllus var. pachyphyllus" # accepted infraspecifics

# Penstemon scariosus. Synonyms: none. Infraspecifics but none that match the tree
kew$taxon_name[grepl("Penstemon pachyphyllus", kew$taxon_name)] 
phy.sps.uniqu[grepl("Penstemon scariosus", phy.sps.uniqu)] 

# Phlox maculata. Synonyms: many, but none that match the one in the tree
kew$taxon_name[grepl("Phlox maculata", kew$taxon_name)] 
phy.sps.uniqu[grepl("Phlox alba", phy.sps.uniqu)] 
phy.sps.uniqu[grepl("Phlox bimaculata", phy.sps.uniqu)] 
phy.sps.uniqu[grepl("Phlox candida", phy.sps.uniqu)] 
phy.sps.uniqu[grepl("Phlox excelsa", phy.sps.uniqu)] 
phy.sps.uniqu[grepl("Phlox fruticosa", phy.sps.uniqu)] 
phy.sps.uniqu[grepl("Phlox maculata", phy.sps.uniqu)] 
phy.sps.uniqu[grepl("Phlox odorata", phy.sps.uniqu)] 
phy.sps.uniqu[grepl("Phlox penduliflora", phy.sps.uniqu)] 
phy.sps.uniqu[grepl("Phlox reflexa", phy.sps.uniqu)] 
phy.sps.uniqu[grepl("Phlox suaveolens", phy.sps.uniqu)] 

# Phlox pilosa. Synonyms: 
kew$taxon_name[grepl("Phlox pilosa", kew$taxon_name)] 
phy.sps.uniqu[grepl("Armeria pilosa", phy.sps.uniqu)] 
phy.sps.uniqu[grepl("Phlox pilosa", phy.sps.uniqu)]
phy.sps.uniqu[grepl("Phlox pilosa subsp. deamii", phy.sps.uniqu)] # first accepted infraspecific
nomatch$matchedName[which(nomatch$egretname == "Phlox pilosa")] <- "Phlox pilosa subsp. deamii"

# Acer hyrcanum. Synonyms: Acer hyrcanum subsp. hyrcanum
kew$taxon_name[grepl("Acer hyrcanum", kew$taxon_name)] 
phy.sps.uniqu[grepl("Acer hyrcanum", phy.sps.uniqu)] 
nomatch$matchedName[which(nomatch$egretname == "Acer hyrcanum")] <- "Acer hyrcanum subsp. hyrcanum"

# Dianthus arenarius. Synonyms: Dianthus arenarius subsp. bohemicus
kew$taxon_name[grepl("Dianthus arenarius", kew$taxon_name)] 
phy.sps.uniqu[grepl("Dianthus arenarius", phy.sps.uniqu)] 
nomatch$matchedName[which(nomatch$egretname == "Dianthus arenarius")] <-"Dianthus arenarius subsp. bohemicus"
# Gentiana lutea. Synonyms: Gentiana lutea subsp. lutea
kew$taxon_name[grepl("Gentiana lutea", kew$taxon_name)] 
phy.sps.uniqu[grepl("Gentiana lutea", phy.sps.uniqu)] 
nomatch$matchedName[which(nomatch$egretname == "Gentiana lutea")] <- "Gentiana lutea subsp. lutea"

# Potentilla reptans
kew$taxon_name[grepl("Potentilla reptans", kew$taxon_name)] 
phy.sps.uniqu[grepl("Potentilla reptans", phy.sps.uniqu)] 
phy.sps.uniqu[grepl("Dasiphora reptans", phy.sps.uniqu)] 
phy.sps.uniqu[grepl("Dynamidium reptans", phy.sps.uniqu)] 
phy.sps.uniqu[grepl("Fragaria reptans", phy.sps.uniqu)] 
phy.sps.uniqu[grepl("Tormentilla linnaeana", phy.sps.uniqu)] 
phy.sps.uniqu[grepl("Potentilla reptans var. sericophylla", phy.sps.uniqu)] # accepted infraspecific


# Alstroemeria ligtu. Synonyms:  many, but none that match the one in the tree 
kew$taxon_name[grepl("Alstroemeria ligtu", kew$taxon_name)] 
phy.sps.uniqu[grepl("Alstroemeria ligtu subsp. simsii", phy.sps.uniqu)] # accepted infraspecific

# Taraxacum platycarpum. Synonyms: Taraxacum platycarpum subsp. hondoense
kew$taxon_name[grepl("Taraxacum platycarpum", kew$taxon_name)] 
phy.sps.uniqu[grepl("Taraxacum platycarpum", phy.sps.uniqu)] 
nomatch$matchedName[which(nomatch$egretname == "Taraxacum platycarpum")] <- "Taraxacum platycarpum subsp. hondoense"

# Verbesina encelioide. Synonyms: many, but none that match the one in the tree
kew$taxon_name[grepl("Verbesina encelioides", kew$taxon_name)] 
phy.sps.uniqu[grepl("Ximenesia encelioides", phy.sps.uniqu)] 
phy.sps.uniqu[grepl("Encelia albescens", phy.sps.uniqu)] 
phy.sps.uniqu[grepl("Pallasia serratifolia", phy.sps.uniqu)] 
phy.sps.uniqu[grepl("Verbesina australis", phy.sps.uniqu)] 
phy.sps.uniqu[grepl("Verbesina encelioides", phy.sps.uniqu)] 
phy.sps.uniqu[grepl("Verbesina exauriculata", phy.sps.uniqu)] 
phy.sps.uniqu[grepl("Verbesina microptera ", phy.sps.uniqu)] 
phy.sps.uniqu[grepl("Verbesina scabra", phy.sps.uniqu)] 
phy.sps.uniqu[grepl("Ximenesia australis", phy.sps.uniqu)] 
phy.sps.uniqu[grepl("Ximenesia encelioides", phy.sps.uniqu)]
phy.sps.uniqu[grepl("Ximenesia exauriculata", phy.sps.uniqu)]
phy.sps.uniqu[grepl("Ximenesia microptera", phy.sps.uniqu)] 

# Heliopsis helianthoides. Synonyms: Heliopsis helianthoides var. scabra
kew$taxon_name[grepl("Heliopsis helianthoides", kew$taxon_name)] 
phy.sps.uniqu[grepl("Heliopsis helianthoides", phy.sps.uniqu)] 
nomatch$matchedName[which(nomatch$egretname == "Heliopsis helianthoides")] <- "Heliopsis helianthoides var. scabra"

# Guizotia scabra. Synonyms: many,  but none that match the one in the tree 
kew$taxon_name[grepl("Guizotia scabra", kew$taxon_name)] 
phy.sps.uniqu[grepl("Veslingia scabra", phy.sps.uniqu)] 
phy.sps.uniqu[grepl("Coreopsis galericulata", phy.sps.uniqu)] 
phy.sps.uniqu[grepl("Guizotia collina", phy.sps.uniqu)] 
phy.sps.uniqu[grepl("Guizotia eylesii", phy.sps.uniqu)] 
phy.sps.uniqu[grepl("Guizotia kassneri", phy.sps.uniqu)] 
phy.sps.uniqu[grepl("Guizotia nyikensis", phy.sps.uniqu)] 
phy.sps.uniqu[grepl("Guizotia oblonga", phy.sps.uniqu)] 
phy.sps.uniqu[grepl("Guizotia ringoetii", phy.sps.uniqu)] 
phy.sps.uniqu[grepl("Guizotia schultzii ", phy.sps.uniqu)] 
phy.sps.uniqu[grepl("Wedelia oblonga", phy.sps.uniqu)] 

# Eupatorium maculatum. Synonyms: many,  but none that match the one in the tree
kew$taxon_name[grepl("Eupatorium maculatum", kew$taxon_name)] 
phy.sps.uniqu[grepl("Eupatoriadelphus maculatus", phy.sps.uniqu)] 
phy.sps.uniqu[grepl("Eupatorium maculatum", phy.sps.uniqu)] 
phy.sps.uniqu[grepl("Eupatorium purpureum", phy.sps.uniqu)] 
phy.sps.uniqu[grepl("Eutrochium maculatum", phy.sps.uniqu)] 

# Abies marocana. Synonyms: Abies pinsapo
kew$taxon_name[grepl("Abies marocana", kew$taxon_name)] 
phy.sps.uniqu[grepl("Abies pinsapo", phy.sps.uniqu)] 
nomatch$matchedName[which(nomatch$egretname == "Abies marocana")] <- "Abies pinsapo"

# Betula utilis subsp. albosinensis. Synonyms: many,  but none that match the one in the tree
kew$taxon_name[grepl("Betula utilis subsp. albosinensis", kew$taxon_name)] 
phy.sps.uniqu[grepl("Betula albosinensis", phy.sps.uniqu)] 
phy.sps.uniqu[grepl("Betula utilis", phy.sps.uniqu)] 
phy.sps.uniqu[grepl("Betula bhojpattra", phy.sps.uniqu)] 

# Eucalyptus delegatensis. Synonyms: 
kew$taxon_name[grepl("Eucalyptus delegatensis", kew$taxon_name)] 
phy.sps.uniqu[grepl("Eucalyptus delegatensis", phy.sps.uniqu)] 
nomatch$matchedName[which(nomatch$egretname == "Eucalyptus delegatensis")] <- "Eucalyptus delegatensis subsp. delegatensis"

# Eucalyptus ovata. Synonyms: Eucalyptus ovata subsp. ovata, but not var. Is it the same thing?
kew$taxon_name[grepl("Eucalyptus ovata", kew$taxon_name)] 
phy.sps.uniqu[grepl("Eucalyptus ovata", phy.sps.uniqu)] 

# Eucalyptus pauciflora. Synonyms: 
kew$taxon_name[grepl("Eucalyptus pauciflora", kew$taxon_name)] 
phy.sps.uniqu[grepl("Eucalyptus pauciflora subsp. pauciflora", phy.sps.uniqu)] # accepted infraspecific
nomatch$matchedName[which(nomatch$egretname == "Eucalyptus pauciflora")] <-  "Eucalyptus pauciflora subsp. pauciflora"

# Betula pendula subsp. mandshurica
kew$taxon_name[grepl("Betula pendula subsp. mandshurica", kew$taxon_name)] 
phy.sps.uniqu[grepl("Betula pendula", phy.sps.uniqu)] # accepted intraspecific
nomatch$matchedName[which(nomatch$egretname == "Betula pendula subsp. mandshurica")] <-  "Betula pendula"

# Celtis pallida. Synonyms: many,  but none that match the one in the tree 
kew$taxon_name[grepl("Celtis pallida", kew$taxon_name)] 
phy.sps.uniqu[grepl("Celtis pallida var. discolor", phy.sps.uniqu)] 
phy.sps.uniqu[grepl("Celtis spinosa var. pallida", phy.sps.uniqu)] 
phy.sps.uniqu[grepl("Celtis tala var. pallida", phy.sps.uniqu)] 

# Echinacea spp.. Synonyms: 
kew$taxon_name[grepl("Echinacea spp.", kew$taxon_name)] 
phy.sps.uniqu[grepl("Echinacea spp.", phy.sps.uniqu)] 

# Eucalyptus pauciflora subsp. niphophila. Synonyms: Eucalyptus niphophila
kew$taxon_name[grepl("Eucalyptus pauciflora subsp. niphophila", kew$taxon_name)]
phy.sps.uniqu[grepl("Eucalyptus pauciflora", phy.sps.uniqu)] # TO ASK: do we accept Eucalyptus pauciflora subsp. pauciflora if POWO doesn't mention it directly?

# Eucomis autumnalis. Synonyms:  many, but none that match the one in the tree
kew$taxon_name[grepl("Eucomis autumnalis", kew$taxon_name)] 
phy.sps.uniqu[grepl("Fritillaria autumnalis", phy.sps.uniqu)] 

# Thymophylla tephroleuca. Synonyms: many,but none that match the one in the tree
kew$taxon_name[grepl("Thymophylla tephroleuca", kew$taxon_name)] 
phy.sps.uniqu[grepl("Thymophylla", phy.sps.uniqu)] 

# Tilia platyphyllos subsp. corinthiaca. Synonyms: 
kew$taxon_name[grepl("Tilia platyphyllos subsp. corinthiaca", kew$taxon_name)] 
phy.sps.uniqu[grepl("Tilia platyphyllos", phy.sps.uniqu)] # heterotypic accepted
phy.sps.uniqu[grepl("Tilia corinthiaca", phy.sps.uniqu)] 
nomatch$matchedName[which(nomatch$egretname == "Tilia platyphyllos subsp. corinthiaca")] <-  "Tilia platyphyllos"

# Primula bulleyana subsp. beesiana. Synonyms: 
phy.sps.uniqu[grepl("Primula bulleyana subsp. beesiana", phy.sps.uniqu)] 
phy.sps.uniqu[grepl("Aleuritia beesiana", phy.sps.uniqu)] 
phy.sps.uniqu[grepl("Primula beesiana", phy.sps.uniqu)] # homotypic accepted
nomatch$matchedName[which(nomatch$egretname == "Primula bulleyana subsp. beesiana")] <-  "Primula beesiana"

# Pyrus glabra. Synonyms: 
phy.sps.uniqu[grepl("Pyrus syriaca", phy.sps.uniqu)] # homotypic accepted
nomatch$matchedName[which(nomatch$egretname == "Pyrus glabra")] <-  "Pyrus syriaca"

# Veronicastrum sibiricum. Synonyms: 
phy.sps.uniqu[grepl("Callistachya sibirica", phy.sps.uniqu)] 
phy.sps.uniqu[grepl("Eustachya cerulea", phy.sps.uniqu)] 
phy.sps.uniqu[grepl("Eustachya sibirica", phy.sps.uniqu)] 
phy.sps.uniqu[grepl("Leptandra sibirica", phy.sps.uniqu)] 
phy.sps.uniqu[grepl("Veronica sibirica", phy.sps.uniqu)] 
phy.sps.uniqu[grepl("Veronica virginica var. sibirica", phy.sps.uniqu)] 
phy.sps.uniqu[grepl("Veronicastrum virginicum", phy.sps.uniqu)] # TO ASK: what happens when the only name in the tree is an accepted infraspecific?

# Solidago albopilosa. Synonyms: no synonyms
phy.sps.uniqu[grepl("Solidago albo", phy.sps.uniqu)] 

# Symphyotrichum oolentangiense. Synonyms: 
phy.sps.uniqu[grepl("Aster azureus", phy.sps.uniqu)] 
phy.sps.uniqu[grepl("Aster capillaceus", phy.sps.uniqu)] 
phy.sps.uniqu[grepl("Aster capillaris", phy.sps.uniqu)] 
phy.sps.uniqu[grepl("Aster oolentangiensis", phy.sps.uniqu)] 
phy.sps.uniqu[grepl("Aster poaceus", phy.sps.uniqu)] 
phy.sps.uniqu[grepl("Aster shortii ", phy.sps.uniqu)] 
phy.sps.uniqu[grepl("Aster vernalis", phy.sps.uniqu)] 
phy.sps.uniqu[grepl("Symphyotrichum oolentangiense", phy.sps.uniqu)] 

# Rosa damascena. Synonyms: artificial hybrid, so many synnonyms 
phy.sps.uniqu[grepl("damascena", phy.sps.uniqu)] 

# Solidago niederederi. Synonyms: hybrid and no synonyms 
phy.sps.uniqu[grepl("Solidago niederederi", phy.sps.uniqu)] 


# look how many remaining
nrow(nomatch)
# count how many rows have NA in the column matchedName
nomatchAfterKewcheck <- subset(nomatch, is.na(matchedName))
nrow(na_rows)

### === === === === === === === === === === ###
# Find species that are alone in their genus #
### === === === === === === === === === === ###
# first look at how many species there is in each genus using Kew's database.
genus <- nomatchAfterKewcheck$genus

counts <- table(kew$genus)

# Extract counts for your genera
result <- data.frame(
  Genus = genus,
  SpeciesCount = as.integer(counts[genus]),
  row.names = NULL
)

nomatchAfterKewcheck
# rename columns 

# now that I have 24 species names that have matches in the tree, there are 24 remaining. Below I select these last 24.First I will 

# try with column parent_plant_name_id

# grab all parent ID for these species
parentIDs <- kewsub$parent_plant_name_id
# pull a vector of species names that correspond to these accepted IDs.
sub <- subset(kew, parent_plant_name_id %in% parentIDs)
# remove NAs
suby <- sub[sub$parent_plant_name_id != "", ]
suby2 <- suby[!is.na(sub$parent_plant_name_id), ]
# pull all of these species 
kewnames <- suby2$taxon_name
# look how many of these species are in the phylogeny tree:
withparent<-kewnames[which(kewnames%in%phy.sps.uniqu)] 








# vector of parent plant name of these 32 species
vecparentId <- kewsubsyn$accepted_plant_name_id
# vector of accepted plant name id of these 5 species
parentName <- subset(kew, plant_name_id %in% vecparentId)
# get a vector of plant name 
vecaparentname <- parentName$taxon_name
# Now look not only for synonyms, but all the different taxon status
vectaxonacceptedID <- kewsub$accepted_plant_name_id
allparentName <- subset(kew, plant_name_id %in% vectaxonacceptedID)
vecall <- allparentName$taxon_name
matchesKewTree<-vecall[which(!vecall%in%phy.sps.uniqu)]
# now look up matches in the phylo tree
bhel <- subset(kew, taxon_name == "Buphthalmum helianthoides L.") 
# grep Buphthalmum helianthoides L. in kew taxon list
grepl("helianthoides", kew$taxon_name)
# select taxon names that have helianthoided with the grepl function
kew$taxon_name[grepl("helianthoides", kew$taxon_name)]




# === === === === === === === === === === === === === === === === === === === === === ===
# === === === === === === === === === === === === === === === === === === === === === ===
# === === === === === === === === === === === === === === === === === === === === === ===
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


