# Started Nov 27, 2023 by Deirdre L., took over by CRD then
# Aim of this code is to get a phylogeny for all of the spp we have germination data scraped

if(length(grep("deirdreloughnan", getwd()) > 0)) {
  setwd("~/Documents/github/egret")
} else if(length(grep("Lizzie", getwd()) > 0)) {
  setwd("~/Documents/git/projects/others/deirdre/egret")
} else if (length(grep("christophe_rouleau-desrochers", getwd())) > 0) {
  setwd("/Users/christophe_rouleau-desrochers/github/egret") # Replace with the correct path for Christophe
} else if (length(grep("victor", getwd())) > 0) {
  setwd("/home/victor/projects/egret") # Replace with the correct path for Christophe
} else if (length(grep("Xiaomao", getwd())) > 0) {
  setwd("C:/PhD/Project/egret") # Replace with the correct path for Mao 
} else{
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

## load phylo (from Smith and Brown 2018)
phy.plants<-read.tree("analyses/input/ALLMB.tre")

## made S&B2018 tree ultrametric!
# added by vvdm on 17June2025
# largely inspired by Isidora code
# you need: devtools::install_github("josephwb/chronos", dependencies=TRUE)
run <- FALSE
if(run){
  
  library(chronos)
  library(doFuture)
  
  # The idea is to find the lambda values with the lowest CV
  b <- Sys.time()
  plan(multisession, workers = 13)
  lambdas <- 10^seq(-4, 2, 0.5)
  res <- foreach(lambda = lambdas, .combine=rbind, .options.future = list(seed = TRUE)) %dofuture% {
    resl <- CV(phy.plants, lambda)
    resl
  }
  plan(sequential);gc()
  e <- Sys.time()
  # Error: cannot allocate vector of size 1027.3 Gb (not due to parallelization)
  
  # We can narrow the search
  res <- CV(treeFam, 10^seq(-3, -2, 0.1), model = "correlated")
  plot(res, type = "o", log = "xy")
  
  #Once you choose lambda, you can make the tree ultrametric with chronos
  
  # Make it ultrametric
  treeFam_ultraR <- chronos(treeFam, lambda =  0.0031662, model = "correlated")
  #log-Lik = -18.33692 
  #PHIIC = 1210.74 
  
  #Resolve multichotomies
  treeFam_ultraR<-multi2di(treeFam_ultraR)
  
  # Rescale with the fossil information of how old is the phylogeny (My Botryo family was 69.9 Mya)
  library(geiger)
  treeFam_ultraRes <- rescale(treeFam_ultraR, model = "depth", 69.9)
  
}

### === === === === === === === === === === === === === === === === === === ###
# Get a list of synonyms for ALL species that aren't from the kew's list
#setwd("/Users/christophe_rouleau-desrochers/Desktop/UBC/egretLOCAL")
# wfodf <- read.csv("classification.csv", header = TRUE, sep = "\t")
kew <- read.csv("wcvp_Mao/wcvp_names.csv", header = TRUE, sep = "|", stringsAsFactors = FALSE )
kew$taxon_name <- gsub(" ", "_", kew$taxon_name)
# do the same for egret because there is a space when subsp.
egret$latbi <- gsub(" ", "_", egret$latbi)
# do the same for usda because there is a spcae when subsp.
usda$latbi <- gsub(" ", "_", usda$latbi)
### === === === === === === === === === === === === === === === === === === ###

## getting a list of genera in S&B's phylo
phy.genera<-unlist(
  lapply(strsplit(phy.plants$tip.label, "_"),function(x){return(x[1])})
)
phy.genera.uniq<-sort(unique(phy.genera))
phy.sps.uniqu <- phy.plants$tip.label
# just a check to confirm there are all the same format 
phy.sps.uniqu <- gsub(" ", "_", phy.sps.uniqu)

# === === === === === === === === === ===  === === === === ===  === === === ===
### Start with Egret ###
# === === === === === === === === === ===  === === === === ===  === === === ===
# egret$latbi <-gsub("_", " ", egret$latbi)
egret.sps <- sort(unique(egret$latbi))

## how many phenobc species are NOT in the phylogeny?
egret.phenosp.sps.inphylo <- egret.sps[which(!egret.sps%in%phy.sps.uniqu)] #48 out of 335

# grab a vec for kew spp
kewvec <- unique(kew$taxon_name)
# now look up only for the species that we don't have a match with the tree:
matchestree <- egret.phenosp.sps.inphylo[egret.phenosp.sps.inphylo %in% kewvec] 
length(matchestree) # 26 out of 48 species in egret are not in kew taxon_name column. Below I will extract all taxon names that return a common accepted parent name ID

# create df of species names from egret
sppegretkew <- subset(egret, latbi %in% egret.phenosp.sps.inphylo)
# remove duplicated rows
sppegretkew <- sppegretkew[!duplicated(sppegretkew$latbi), ]
# select only interesting columns for now
sppegretkew2 <- sppegretkew[, c("datasetID", "latbi", "genus","species")]

# subset in kew's for all 30 species that we don't have a match in the tree
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
colnames(matchednamesegret) <- c("accepted_plant_name_id", "egretname", "datasetID", "genus", "species", "sppMatch")
# this is the names that can be matched by code to kew's data base. 
head(matchednamesegret)
# === === === === === === === === === ===  === === === === ===  === === === ===
##### Look up other synonyms using search on kew's website #####
### below I am using the grepl function to search if the synonyms I found on kew's website match some species in the tree. Then using which function, I am adding these new matches to the df matchednamesegret
# === === === === === === === === === ===  === === === === ===  === === === ===
# list all egret species that still don't have matches
nomatch <- matchednamesegret[which(is.na(matchednamesegret$sppMatch)),]
nrow(nomatch)
# row of NA NA
matchednamesegret <- subset(matchednamesegret, egretname != "NA_NA")

# Betonica_bulgarica: only one synonym = Stachys_bulgarica and it's not in the tree

# Leontice_incerta: only one synonym =Leontice_vesicaria and it's not in the tree

# Maackia_taiwanensis. Synonyms: none

# Penstemon_pachyphyllus. Synonyms: 
matchednamesegret$sppMatch[which(matchednamesegret$egretname == "Penstemon_pachyphyllus")] <- "Penstemon_pachyphyllus_var._pachyphyllus" # accepted infraspecifics

# Penstemon_scariosus. Synonyms: none. Infraspecifics but none that match the tree
# Phlox_maculata. Synonyms: many, but none that match the one in the tree
matchednamesegret$sppMatch[which(matchednamesegret$egretname == "Phlox_maculata")] <- "Phlox_maculata_subsp._maculata" # accepted 

# Phlox_pilosa. Synonyms: 
matchednamesegret$sppMatch[which(matchednamesegret$egretname == "Phlox_pilosa")] <- "Phlox_pilosa_subsp._deamii"

# Acer_hyrcanum. Synonyms: Acer_hyrcanum_subsp._hyrcanum
matchednamesegret$sppMatch[which(matchednamesegret$egretname == "Acer_hyrcanum")] <- "Acer_hyrcanum_subsp._hyrcanum"

# Dianthus_arenarius. Synonyms: Dianthus_arenarius_subsp._bohemicus
matchednamesegret$sppMatch[which(matchednamesegret$egretname == "Dianthus_arenarius")] <-"Dianthus_arenarius_subsp._bohemicus"

# Gentiana_lutea. Synonyms: Gentiana_lutea_subsp._lutea
matchednamesegret$sppMatch[which(matchednamesegret$egretname == "Gentiana_lutea")] <- "Gentiana_lutea_subsp._lutea"

# Potentilla_reptans
matchednamesegret$sppMatch[which(matchednamesegret$egretname == "Potentilla_reptans")] <- "Potentilla_reptans_var._sericophylla"


# Alstroemeria_ligtu. Synonyms:  many, but none that match the one in the tree 
matchednamesegret$sppMatch[which(matchednamesegret$egretname == "Alstroemeria_ligtu")] <- "Alstroemeria_ligtu_subsp._simsii" 

# Taraxacum_platycarpum. Synonyms: Taraxacum_platycarpum_subsp._hondoense
matchednamesegret$sppMatch[which(matchednamesegret$egretname == "Taraxacum_platycarpum")] <- "Taraxacum_platycarpum_var._longeappendiculatum"

# Verbesina_encelioides. Synonyms: many, but none that match the one in the tree

# Heliopsis_helianthoides. Synonyms: Heliopsis_helianthoides_var._scabra
matchednamesegret$sppMatch[which(matchednamesegret$egretname == "Heliopsis_helianthoides")] <- "Heliopsis_helianthoides_var._scabra"
# Updates with new wcvp: Heliopsis_helianthoides_var._occidentalis and Heliopsis_helianthoides_var._scabra are both synonyms and can be found in the tree

# Guizotia_scabra. Synonyms: many,  but none that match the one in the tree 
matchednamesegret$sppMatch[which(matchednamesegret$egretname == "Guizotia_scabra")] <- "Guizotia_scabra_subsp._schimperi"

# Eupatorium maculatum. Synonyms: many,  but none that match the one in the tree

# Abies marocana. Synonyms: Abies pinsapo
matchednamesegret$sppMatch[which(matchednamesegret$egretname == "Abies_marocana")] <- "Abies_pinsapo"

# Eucalyptus delegatensis. Synonyms: 
matchednamesegret$sppMatch[which(matchednamesegret$egretname == "Eucalyptus_delegatensis")] <- "Eucalyptus_delegatensis_subsp._delegatensis"

# Eucalyptus ovata. Synonyms: Eucalyptus ovata subsp. ovata, but not var. Is it the same thing?
matchednamesegret$sppMatch[which(matchednamesegret$egretname == "Eucalyptus_ovata")] <- "Eucalyptus_ovata_var._ovata"


# Eucalyptus pauciflora. Synonyms: 
matchednamesegret$sppMatch[which(matchednamesegret$egretname == "Eucalyptus_pauciflora")] <-  "Eucalyptus_pauciflora_subsp._pauciflora"

# Celtis pallida. Synonyms: many,  but none that match the one in the tree 

# Echinacea spp.. Synonyms: 

# Eucalyptus pauciflora subsp. niphophila. Synonyms: Eucalyptus niphophila, doesn't match the tree

# Eucomis autumnalis. Synonyms:  many, but none that match the one in the tree

# Thymophylla tephroleuca. Synonyms: many,but none that match the one in the tree

# Pyrus glabra. Synonyms: 
matchednamesegret$sppMatch[which(matchednamesegret$egretname == "Pyrus_glabra")] <-  "Pyrus_syriaca"

# Veronicastrum sibiricum. Synonyms: 
matchednamesegret$sppMatch[which(matchednamesegret$egretname == "Veronicastrum_sibiricum")] <-  "Veronicastrum_sibiricum_var._zuccarinii"


# Solidago albopilosa. Synonyms: no synonyms

# Symphyotrichum oolentangiense. Synonyms:

# Rosa damascena. Synonyms: artificial hybrid, so many synnonyms 

# Solidago niederederi. Synonyms: hybrid and no synonyms 

# Fagus sylvatica. Synonyms: a lot
matchednamesegret$sppMatch[which(matchednamesegret$egretname == "Fagus_sylvatica")] <-  "Fagus_sylvatica_subsp._orientalis"

# Carex scoparia. Synonyms: a lot, but non of them is in the tree

# Chrysojasminum fruticans
matchednamesegret$sppMatch[which(matchednamesegret$egretname == "Chrysojasminum_fruticans")] <-  "Jasminum_fruticans"

# Acer coriaceifolium
matchednamesegret$sppMatch[which(matchednamesegret$egretname == "Acer_coriaceifolium")] <-  "Acer_cinnamomifolium"

# Aster laevis
matchednamesegret$sppMatch[which(matchednamesegret$egretname == "Aster_laevis")] <-  "Symphyotrichum_laeve"

# Calligonum alaschanicum
matchednamesegret$sppMatch[which(matchednamesegret$egretname == "Calligonum_alaschanicum")] <-  "Calligonum_mongolicum"

# Loranthus_tanakae
matchednamesegret$sppMatch[which(matchednamesegret$egretname == "Loranthus_tanakae")] <-  "Hyphear_tanakae"

# Potentilla_argentea
matchednamesegret$sppMatch[which(matchednamesegret$egretname == "Potentilla_argentea")] <-  "Potentilla_argentea_var._pseudocalabra"

# --- --- --- ---- --- --- --- --- --- ---- --- --- --- --- --- ---- --- ---
# look how many species with no match even after this search
# count how many rows have NA in the column sppMatch
nomatchAfterKewcheck <- subset(matchednamesegret , is.na(sppMatch))
nrow(nomatchAfterKewcheck)

### === === === === === === === === === === ###
#### Add matched names to egret df ####
### === === === === === === === === === === ###
# create a df of matched names without NAs
matchnona <- matchednamesegret[!is.na(matchednamesegret$sppMatch),]
egret$sppMatch <- NA
# first add species that match the tree already
easymatch <- egret.sps[which(egret.sps%in%phy.sps.uniqu)] 
egret$sppMatch <- ifelse(egret$latbi %in% easymatch, egret$latbi, NA)

missing_idx <- is.na(egret$sppMatch)
matched_values <- match(egret$latbi[missing_idx], matchnona$egretname)

# match spp names back in egret df
egret$sppMatch[missing_idx] <- matchnona$sppMatch[matched_values]
# check
egret[!duplicated(egret$latbi), c("latbi", "sppMatch")]


### === === === === === === === === === === === === === ###
###### Splice in species with no match from Kew Database ######
### === === === === === === === === === === === === === ###
# splice in a species that is not currently in the tree
# group the species I want to splice in the tree
### === === === === === === === === === === ###
#### Try with a subset of species to make the tree smaller ####
### === === === === === === === === === === ###
# select some random genus AND 3 that I will splice stuff in
egret_sub <- egret[c("latbi","genus","sppMatch")]

set.seed(123)
vec <- c("Carex", "Eucomis","Leontice", "Maackia", "Penstemon", "Celtis", "Solidago", "Symphyotrichum", "Thymophylla", "Verbesina", "Betonica", "Eupatorium", "Echinacea", "Rosa")
t <- subset(egret_sub, genus %in% vec)
t <- t$latbi
spp_smalltree <- unique(t)
studyNosmall <- subset(egret_sub, latbi %in% spp_smalltree)

#phy.sps.uniqu[grepl("Eucomis", phy.sps.uniqu)]#Eucomis_zambesiaca
#phy.sps.uniqu[grepl("Leontice", phy.sps.uniqu)]#Leontice_leontopetalum
#phy.sps.uniqu[grepl("Maackia", phy.sps.uniqu)]#Maackia_amurensis
#phy.sps.uniqu[grepl("Celtis", phy.sps.uniqu)]#Celtis_schippii
#phy.sps.uniqu[grepl("Thymophylla", phy.sps.uniqu)]#Thymophylla_setifolia
#phy.sps.uniqu[grepl("Verbesina", phy.sps.uniqu)]#Verbesina_occidentalis
#phy.sps.uniqu[grepl("Betonica", phy.sps.uniqu)]#Betonica_officinalis
#phy.sps.uniqu[grepl("Eupatorium", phy.sps.uniqu)]#Eupatorium_fernaldii


t <- c(t, "Eucomis_zambesiaca","Leontice_leontopetalum","Maackia_amurensis","Celtis_schippii","Thymophylla_setifolia","Verbesina_occidentalis","Betonica_officinalis","Eupatorium_fernaldii")


smallTree <- keep.tip(phy.plants, which(phy.plants$tip.label %in% unique(t)))

smallnamesphy <- smallTree$tip.label
#I run the following three lines of code before I fixed the missing genus in the tree. There are some genus in nomatchAfterKewcheck only has one species in egret, so if we don't mannually add a sister species in the list, it won't be added to the tree, that's why I wrote line 513-523
#smallgenus <- unique(sub("_.*", "", smallnamesphy))
#smallgenus
#vec[which(!vec %in% smallgenus)]


smallTree$root.edge <- 0

is.rooted(smallTree)
smallTree$node.label<-NULL

studyNosmall$latbi[which(studyNosmall$latbi %in% smallTree$tip.label)]


### === === === === === === === === === === ###
# Force the tree to be ultrametric FOR NOW.
### === === === === === === === === === === ###
smallTreeUltra <- force.ultrametric(
  smallTree,
  method = "extend"  # Extends terminal branches
)


unique(nomatchAfterKewcheck$egretname)
spptoplice <- c()

for (genus in vec) {
  matches <- nomatchAfterKewcheck$egretname[grepl(genus, nomatchAfterKewcheck$egretname)]
  spptoplice <- c(spptoplice, matches)
}


smallTreeUltraSpliced <- smallTreeUltra

# Loop and update the tree with one species a time
for (i in spptoplice) {
  smallTreeUltraSpliced <- add.species.to.genus(tree = smallTreeUltraSpliced, species = i, where = "root")
}

smallnamesphy <- smallTreeUltraSpliced$tip.label
studyNosmallSpliced <- subset(egret, latbi %in% smallnamesphy)

smallTreeUltraSpliced$root.edge <- 0

is.rooted(smallTreeUltraSpliced)
smallTreeUltraSpliced$node.label<-NULL

smallTreeUltraSpliced
studyNosmallSpliced

# remove duplicated rows
studyNosmallSpliced <- studyNosmallSpliced[!duplicated(studyNosmallSpliced$latbi), ]

rownames(studyNosmallSpliced) <- studyNosmallSpliced$latbi

# species in both tree and data
common_species <- intersect(smallTreeUltraSpliced$tip.label, studyNosmallSpliced$latbi)

# drop unmatched species from the tree and data
pruned_tree <- drop.tip(smallTreeUltraSpliced, setdiff(smallTreeUltraSpliced$tip.label, common_species))

# list of species got spliced
target <- nomatchAfterKewcheck$egretname

# get the index of the tip
tip_index <- match(target, pruned_tree$tip.label)

# categorize them into splicing and non-splicing for color purpose
status <- ifelse(common_species %in% target, "splicing", "non-splicing")
status <- factor(status, levels = c("non-splicing", "splicing"))
mycol <- c("black", "red")[status]

# drop the tips we added to assist with single genus in egret
pruned_tree <- drop.tip(pruned_tree, c("Eucomis_zambesiaca","Leontice_leontopetalum","Maackia_amurensis","Celtis_schippii","Thymophylla_setifolia","Verbesina_occidentalis","Betonica_officinalis","Eupatorium_fernaldii"))

# plot the tree
pdf("analyses/figures/egret_spliced.pdf", width = 20, height = 40)
plot(pruned_tree, cex = 1.5, tip.color = mycol)
dev.off()

# make the full egret tree
egretlist <- egret$sppMatch
egretlist <- c(egretlist, "Eucomis_zambesiaca","Leontice_leontopetalum","Maackia_amurensis","Celtis_schippii","Thymophylla_setifolia","Verbesina_occidentalis","Betonica_officinalis","Eupatorium_fernaldii")

egretTree1 <- keep.tip(phy.plants, phy.plants$tip.label[phy.plants$tip.label %in% egretlist])
matchednamesegret1 <- matchednamesegret[!is.na(matchednamesegret$sppMatch), ]
name_map <- setNames(matchednamesegret1$egretname, matchednamesegret1$sppMatch)

# replace the tip name with the name in egret
egretTree1$tip.label <- ifelse(egretTree1$tip.label %in% names(name_map),
                              name_map[egretTree1$tip.label],
                              egretTree1$tip.label)


egretUltra <- force.ultrametric(
  egretTree1,
  method = "extend"  # Extends terminal branches
)

# splicing
egretSpliced <- egretUltra
for (i in spptoplice) {
  egretSpliced <- add.species.to.genus(tree = egretSpliced, species = i, where = "root")
}
# drop the tips we added to assist with single genus in egret
egretSpliced <- drop.tip(egretSpliced, c("Eucomis_zambesiaca","Leontice_leontopetalum","Maackia_amurensis","Celtis_schippii","Thymophylla_setifolia","Verbesina_occidentalis","Betonica_officinalis","Eupatorium_fernaldii"))

# plot the tree
pdf("analyses/figures/egret_phy.pdf", width = 20, height = 50)
plot(egretSpliced, cex = 0.4, show.tip.label = TRUE)
dev.off()

# write out the tree
write.tree(egretSpliced,"analyses/output/egretPhylogenyFull.tre")


# === === === === === === === === === ===  === === === === ===  === === === ===
### Start with USDA ###
# === === === === === === === === === ===  === === === === ===  === === === ===

usda.sps <- sort(unique(usda$latbi))

## how many phenobc species are NOT in the phylogeny?
usda.phenosp.sps.inphylo <- usda.sps[which(!usda.sps%in%phy.sps.uniqu)] #32 out of 416

# grab a vec for kew spp
kewvec <- unique(kew$taxon_name)
# now look up only for the species that we don't have a match with the tree:
matchestree_usda <- usda.phenosp.sps.inphylo[usda.phenosp.sps.inphylo %in% kewvec] 
length(matchestree_usda) 

# create df of species names from usda
sppusdakew <- subset(usda, latbi %in% usda.phenosp.sps.inphylo)
# remove duplicated rows
sppusdakew <- sppusdakew[!duplicated(sppusdakew$latbi), ]
# select only interesting columns for now
sppusdakew2 <- sppusdakew[, c("rowID","latbi", "genus","species")]

# subset in kew's for all species that we don't have a match in the tree
kewsub_usda <- subset(kew, taxon_name %in% usda.phenosp.sps.inphylo)

# grab all parent ID for these species
accparentIDs_usda <- kewsub_usda$accepted_plant_name_id
# pull a vector of species names that correspond to these accepted IDs.
sub <- subset(kew, accepted_plant_name_id %in% accparentIDs_usda)
# remove NAs
suby <- sub[sub$accepted_plant_name_id != "", ]
# pull a vector of all of these species 
kewnames <- suby$taxon_name
# look how many of these species are in the phylogeny tree:
withaccepted<-kewnames[which(kewnames%in%phy.sps.uniqu)] 
# subset kew for these species
matchednames <- subset(kew, taxon_name %in% withaccepted)
matchednames <- subset(matchednames, taxon_status != "Unplaced")
# remove unecessary columns
matchednamessub <- matchednames[, c("accepted_plant_name_id", "taxon_name")]
# add parent name ID in the df containing ALL the species we don't have match for in the tree
latbiwithId_usda <- merge(sppusdakew2, kewsub_usda[, c("taxon_name", "accepted_plant_name_id")], 
                     by.x = "latbi", by.y = "taxon_name", 
                     all.x = TRUE)
# merge matchednamessub and sppusdakew by accepted_plant_name_id
matchednamesusda <- merge(latbiwithId_usda, matchednamessub, by = "accepted_plant_name_id", all.x = TRUE) 
# change colnames
colnames(matchednamesusda) <- c("accepted_plant_name_id", "usdaname", "datasetID", "genus", "species", "sppMatch")
# this is the names that can be matched by code to kew's data base. 
head(matchednamesusda)
# === === === === === === === === === ===  === === === === ===  === === === ===
##### Look up other synonyms using search on kew's website #####
### below I am using the grepl function to search if the synonyms I found on kew's website match some species in the tree. Then using which function, I am adding these new matches to the df matchednamesegret
# === === === === === === === === === ===  === === === === ===  === === === ===
# list all usda species that still don't have matches
nomatchusda <- matchednamesusda[which(is.na(matchednamesusda$sppMatch)),]
nrow(nomatchusda)

#following function is what we used to serch for the possible match in the tree, by ignoring the sub. or var. we try to find if there's anything closer to the species, or we just ignore the sub. and var.
#phy.sps.uniqu[grepl("Alnus_communis", phy.sps.uniqu)] 


#Fagus_sylvatica
matchednamesusda$sppMatch[which(matchednamesusda$usdaname == "Fagus_sylvatica")] <-  "Fagus_sylvatica_subsp._orientalis"
#Magnolia_acuminata
matchednamesusda$sppMatch[which(matchednamesusda$usdaname == "Magnolia_acuminata")] <- "Magnolia_acuminata_var._subcordata"
#Magnolia_fraseri
matchednamesusda$sppMatch[which(matchednamesusda$usdaname == "Magnolia_fraseri")] <- "Magnolia_fraseri_var._fraseri"
#Quercus_petraea
matchednamesusda$sppMatch[which(matchednamesusda$usdaname == "Quercus_petraea")] <- "Quercus_petraea_subsp._petraea"
#Quercus_robur
matchednamesusda$sppMatch[which(matchednamesusda$usdaname == "Quercus_robur")] <- "Quercus_robur_subsp._robur"
#Amorpha_californica
matchednamesusda$sppMatch[which(matchednamesusda$usdaname == "Amorpha_californica")] <- "Amorpha_californica_var._californica"
#Fraxinus_velutina
matchednamesusda$sppMatch[which(matchednamesusda$usdaname == "Fraxinus_velutina")] <- "Fraxinus_velutina_var._toumeyi"

  # look how many species with no match even after this search
  # count how many rows have NA in the column sppMatch
nomatchAfterKewcheck <- subset(matchednamesusda , is.na(sppMatch))
nrow(nomatchAfterKewcheck)

### === === === === === === === === === === ###
#### Add matched names to usda df ####
### === === === === === === === === === === ###
# create a df of matched names without NAs
matchnona <- matchednamesusda[!is.na(matchednamesusda$sppMatch),]
usda$sppMatch <- NA
# first add species that match the tree already
easymatch <- usda.sps[which(usda.sps %in% phy.sps.uniqu)] 
usda$sppMatch <- ifelse(usda$latbi %in% easymatch, usda$latbi, NA)

missing_idx <- is.na(usda$sppMatch)
matched_values <- match(usda$latbi[missing_idx], matchnona$usdaname)

# match spp names back in usda df
usda$sppMatch[missing_idx] <- matchnona$sppMatch[matched_values]
# check
usda[!duplicated(usda$latbi), c("latbi", "sppMatch")]

usda_sub <- usda[c("latbi","genus","sppMatch")]

set.seed(123)
vec <- c("Celtis", "Aronia", "Baccharis", "Ceanothus", "Cercis", "Magnolia", "Prunus", "Rosa", "Pinus")
t <- subset(usda_sub, genus %in% vec)
t <- t$latbi
spp_smalltree <- unique(t)
studyNosmall <- subset(usda_sub, latbi %in% spp_smalltree)


smallTree <- keep.tip(phy.plants, which(phy.plants$tip.label %in% unique(t)))

smallnamesphy <- smallTree$tip.label


#smallgenus <- unique(sub("_.*", "", smallnamesphy))
#smallgenus
#vec[which(!vec %in% smallgenus)]

smallTree$root.edge <- 0

is.rooted(smallTree)
smallTree$node.label<-NULL

studyNosmall$latbi[which(studyNosmall$latbi %in% smallTree$tip.label)]

### === === === === === === === === === === ###
# Force the tree to be ultrametric FOR NOW.
### === === === === === === === === === === ###
smallTreeUltra <- force.ultrametric(
  smallTree,
  method = "extend"  # Extends terminal branches
)

unique(nomatchusda$usdaname)
spptoplice <- c()

for (genus in vec) {
  matches <- nomatchAfterKewcheck$usdaname[grepl(genus, nomatchAfterKewcheck$usdaname)]
  spptoplice <- c(spptoplice, matches)
}

smallTreeUltraSpliced <- smallTreeUltra

# Loop and update the tree with one species a time
for (i in spptoplice) {
  smallTreeUltraSpliced <- add.species.to.genus(tree = smallTreeUltraSpliced, species = i, where = "root")
}

smallnamesphy <- smallTreeUltraSpliced$tip.label
studyNosmallSpliced <- subset(usda, latbi %in% smallnamesphy)

smallTreeUltraSpliced$root.edge <- 0

is.rooted(smallTreeUltraSpliced)
smallTreeUltraSpliced$node.label<-NULL

smallTreeUltraSpliced
studyNosmallSpliced

# remove duplicated rows
studyNosmallSpliced <- studyNosmallSpliced[!duplicated(studyNosmallSpliced$latbi), ]

rownames(studyNosmallSpliced) <- studyNosmallSpliced$latbi

# species in both tree and data
common_species <- intersect(smallTreeUltraSpliced$tip.label, studyNosmallSpliced$latbi)

# drop unmatched species from the tree and data
pruned_tree <- drop.tip(smallTreeUltraSpliced, setdiff(smallTreeUltraSpliced$tip.label, common_species))

# list of species got spliced
target <- nomatchusda$usdaname

# get the index of the tip
tip_index <- match(target, pruned_tree$tip.label)


# categorize them into splicing and non-splicing for color purpose
status <- ifelse(common_species %in% target, "splicing", "non-splicing")
status <- factor(status, levels = c("non-splicing", "splicing"))
mycol <- c("black", "red")[status]

# plot the tree
pdf("analyses/figures/usdaSpliced.pdf", width = 40, height = 60)
plot(pruned_tree, cex = 1.5, tip.color = mycol)
dev.off()

# make the full usda tree
usdalist <- unique(usda$sppMatch)

usdaTree <- keep.tip(phy.plants, phy.plants$tip.label[phy.plants$tip.label %in% usdalist])
matchednamesusda1 <- matchednamesusda[!is.na(matchednamesusda$sppMatch), ]
name_map <- setNames(matchednamesusda1$usdaname, matchednamesusda1$sppMatch)

# replace the tip name with the name in usda
usdaTree$tip.label <- ifelse(usdaTree$tip.label %in% names(name_map),
                               name_map[usdaTree$tip.label],
                               usdaTree$tip.label)

usdaUltra <- force.ultrametric(
  usdaTree,
  method = "extend"  # Extends terminal branches
)

# splicing
usdaSpliced <- usdaUltra
for (i in spptoplice) {
  usdaSpliced <- add.species.to.genus(tree = usdaSpliced, species = i, where = "root")
}

usdaSpliced$tip.label[duplicated(usdaSpliced$tip.label)]
unique(usdaSpliced$tip.label)
unique(usda$latbi)
# plot the tree
pdf("analyses/figures/usda_phy.pdf", width = 50, height =100 )
plot(usdaSpliced, cex = 1.5, show.tip.label = TRUE)
dev.off()

# write out the tree
write.tree(usdaSpliced,"analyses/output/usdaPhylogenyFull.tre")
