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
  setwd("C:/PhD/Project/egret") # Replace with the correct path for Mao {
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
kew <- read.csv("wcvp/wcvp_names.csv", header = TRUE, sep = "|", stringsAsFactors = FALSE )
kew$taxon_name <- gsub(" ", "_", kew$taxon_name)
kew <- read.csv("wcvp_Mao/wcvp_names.csv", header = TRUE, sep = "|", stringsAsFactors = FALSE )
# do the same for egret because there is a space when subsp.
egret$latbi <- gsub(" ", "_", egret$latbi)
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
egret.genus <- sort(unique(egret$genus))

## how many phenobc genus are NOT in the phylogeny?
egret.phenosp.genus.inphylo <- egret.genus[which(!egret.genus%in%phy.genera.uniq)] #186 out of 187
## how many phenobc species are NOT in the phylogeny?
egret.phenosp.sps.inphylo<-egret.sps[which(!egret.sps%in%phy.sps.uniqu)] #48 out of 335

# grab a vec for kew spp
kewvec <- unique(kew$taxon_name)
# now look up only for the species that we don't have a match with the tree:
matchestree <- egret.phenosp.sps.inphylo[egret.phenosp.sps.inphylo %in% kewvec] 
length(matchestree) # 32 out of 48 species in egret are not in kew taxon_name column. Below I will extract all taxon names that return a common accepted parent name ID

# create df of species names from egret
sppegretkew <- subset(egret, latbi %in% egret.phenosp.sps.inphylo)
# remove duplicated rows
sppegretkew <- sppegretkew[!duplicated(sppegretkew$latbi), ]
# select only interesting columns for now
sppegretkew2 <- sppegretkew[, c("datasetID", "latbi", "genus","species")]

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
phy.sps.uniqu[grepl("Stachys_bulgarica", phy.sps.uniqu)] 

# Leontice_incerta: only one synonym =Leontice_vesicaria and it's not in the tree
phy.sps.uniqu[grepl("Leontice_vesicaria", phy.sps.uniqu)] 

# Maackia_taiwanensis. Synonyms: none
phy.sps.uniqu[grepl("Maackia", phy.sps.uniqu)] 

# Penstemon_pachyphyllus. Synonyms: 
kew$taxon_name[grepl("Penstemon_pachyphyllus", kew$taxon_name)] 
phy.sps.uniqu[grepl("Penstemon_pachyphyllus_var._pachyphyllus", phy.sps.uniqu)] 
matchednamesegret$sppMatch[which(matchednamesegret$egretname == "Penstemon_pachyphyllus")] <- "Penstemon_pachyphyllus_var._pachyphyllus" # accepted infraspecifics

# Penstemon_scariosus. Synonyms: none. Infraspecifics but none that match the tree
kew$taxon_name[grepl("Penstemon_pachyphyllus", kew$taxon_name)] 
phy.sps.uniqu[grepl("Penstemon_scariosus", phy.sps.uniqu)] # TO ASK: if 2 varieties of one species, should we keep one?

# Phlox_maculata. Synonyms: many, but none that match the one in the tree
kew$taxon_name[grepl("Phlox_maculata", kew$taxon_name)]
phy.sps.uniqu[grepl("Phlox_maculata", phy.sps.uniqu)] 
matchednamesegret$sppMatch[which(matchednamesegret$egretname == "Phlox_maculata")] <- "Phlox_maculata_subsp._maculata" # accepted 
#phy.sps.uniqu[grepl("Phlox_alba", phy.sps.uniqu)] 
#phy.sps.uniqu[grepl("Phlox_bimaculata", phy.sps.uniqu)] 
#phy.sps.uniqu[grepl("Phlox_candida", phy.sps.uniqu)] 
#phy.sps.uniqu[grepl("Phlox_excelsa", phy.sps.uniqu)] 
#phy.sps.uniqu[grepl("Phlox_fruticosa", phy.sps.uniqu)] 
#phy.sps.uniqu[grepl("Phlox_maculata", phy.sps.uniqu)] 
#phy.sps.uniqu[grepl("Phlox_odorata", phy.sps.uniqu)] 
#phy.sps.uniqu[grepl("Phlox_penduliflora", phy.sps.uniqu)] 
#phy.sps.uniqu[grepl("Phlox_reflexa", phy.sps.uniqu)] 
#phy.sps.uniqu[grepl("Phlox_suaveolens", phy.sps.uniqu)] 

# Phlox_pilosa. Synonyms: 
kew$taxon_name[grepl("Phlox_pilosa", kew$taxon_name)] 
phy.sps.uniqu[grepl("Armeria_pilosa", phy.sps.uniqu)] 
phy.sps.uniqu[grepl("Phlox_pilosa", phy.sps.uniqu)]
phy.sps.uniqu[grepl("Phlox_pilosa_subsp._deamii", phy.sps.uniqu)] # first accepted infraspecific
matchednamesegret$sppMatch[which(matchednamesegret$egretname == "Phlox_pilosa")] <- "Phlox_pilosa_subsp._deamii"

# Acer_hyrcanum. Synonyms: Acer_hyrcanum_subsp._hyrcanum
kew$taxon_name[grepl("Acer_hyrcanum", kew$taxon_name)] 
phy.sps.uniqu[grepl("Acer_hyrcanum", phy.sps.uniqu)] 
matchednamesegret$sppMatch[which(matchednamesegret$egretname == "Acer_hyrcanum")] <- "Acer_hyrcanum_subsp._hyrcanum"

# Dianthus_arenarius. Synonyms: Dianthus_arenarius_subsp._bohemicus
kew$taxon_name[grepl("Dianthus_arenarius", kew$taxon_name)] 
phy.sps.uniqu[grepl("Dianthus_arenarius", phy.sps.uniqu)] 
matchednamesegret$sppMatch[which(matchednamesegret$egretname == "Dianthus_arenarius")] <-"Dianthus_arenarius_subsp._bohemicus"

# Gentiana_lutea. Synonyms: Gentiana_lutea_subsp._lutea
kew$taxon_name[grepl("Gentiana_lutea", kew$taxon_name)] 
phy.sps.uniqu[grepl("Gentiana_lutea", phy.sps.uniqu)] 
matchednamesegret$sppMatch[which(matchednamesegret$egretname == "Gentiana_lutea")] <- "Gentiana_lutea_subsp._lutea"

# Potentilla_reptans
kew$taxon_name[grepl("Potentilla_reptans", kew$taxon_name)] 
phy.sps.uniqu[grepl("Potentilla_reptans", phy.sps.uniqu)] 
#phy.sps.uniqu[grepl("Dasiphora_reptans", phy.sps.uniqu)] 
#phy.sps.uniqu[grepl("Dynamidium_reptans", phy.sps.uniqu)] 
#phy.sps.uniqu[grepl("Fragaria_reptans", phy.sps.uniqu)] 
#phy.sps.uniqu[grepl("Tormentilla_linnaeana", phy.sps.uniqu)] 
#phy.sps.uniqu[grepl("Potentilla_reptans_var._sericophylla", phy.sps.uniqu)] # accepted infraspecific
matchednamesegret$sppMatch[which(matchednamesegret$egretname == "Potentilla_reptans")] <- "Potentilla_reptans_var._sericophylla"


# Alstroemeria_ligtu. Synonyms:  many, but none that match the one in the tree 
kew$taxon_name[grepl("Alstroemeria_ligtu", kew$taxon_name)] 
phy.sps.uniqu[grepl("Alstroemeria_ligtu", phy.sps.uniqu)] # TO ASK: accepted infraspecific
matchednamesegret$sppMatch[which(matchednamesegret$egretname == "Alstroemeria_ligtu")] <- "Alstroemeria_ligtu_subsp._simsii" 

# Taraxacum_platycarpum. Synonyms: Taraxacum_platycarpum_subsp._hondoense
kew$taxon_name[grepl("Taraxacum_platycarpum", kew$taxon_name)] 
phy.sps.uniqu[grepl("Taraxacum_platycarpum", phy.sps.uniqu)] 
#matchednamesegret$sppMatch[which(matchednamesegret$egretname == "Taraxacum_platycarpum")] <- "Taraxacum_platycarpum_subsp._hondoense"
matchednamesegret$sppMatch[which(matchednamesegret$egretname == "Taraxacum_platycarpum")] <- "Taraxacum_platycarpum_var._longeappendiculatum"

# Verbesina_encelioides. Synonyms: many, but none that match the one in the tree
kew$taxon_name[grepl("Verbesina_encelioides", kew$taxon_name)] 
phy.sps.uniqu[grepl("Ximenesia encelioides", phy.sps.uniqu)] 
phy.sps.uniqu[grepl("Encelia_albescens", phy.sps.uniqu)] 
phy.sps.uniqu[grepl("Pallasia_serratifolia", phy.sps.uniqu)] 
phy.sps.uniqu[grepl("Verbesina_australis", phy.sps.uniqu)] 
phy.sps.uniqu[grepl("Verbesina_encelioides", phy.sps.uniqu)] 
phy.sps.uniqu[grepl("Verbesina_exauriculata", phy.sps.uniqu)] 
phy.sps.uniqu[grepl("Verbesina_microptera", phy.sps.uniqu)] 
phy.sps.uniqu[grepl("Verbesina_scabra", phy.sps.uniqu)] 
phy.sps.uniqu[grepl("Ximenesia_australis", phy.sps.uniqu)] 
phy.sps.uniqu[grepl("Ximenesia_encelioides", phy.sps.uniqu)]
phy.sps.uniqu[grepl("Ximenesia_exauriculata", phy.sps.uniqu)]
phy.sps.uniqu[grepl("Ximenesia_microptera", phy.sps.uniqu)] 

# Heliopsis_helianthoides. Synonyms: Heliopsis_helianthoides_var._scabra
kew$taxon_name[grepl("Heliopsis_helianthoides", kew$taxon_name)] 
phy.sps.uniqu[grepl("Heliopsis_helianthoides", phy.sps.uniqu)] 
matchednamesegret$sppMatch[which(matchednamesegret$egretname == "Heliopsis_helianthoides")] <- "Heliopsis_helianthoides_var._scabra"
# Updates with new wcvp: Heliopsis_helianthoides_var._occidentalis and Heliopsis_helianthoides_var._scabra are both synonyms and can be found in the tree

# Guizotia_scabra. Synonyms: many,  but none that match the one in the tree 
kew$taxon_name[grepl("Guizotia_scabra", kew$taxon_name)] 
phy.sps.uniqu[grepl("Guizotia_scabra", phy.sps.uniqu)] 
#phy.sps.uniqu[grepl("Coreopsis_galericulata", phy.sps.uniqu)] 
#phy.sps.uniqu[grepl("Guizotia_collina", phy.sps.uniqu)] 
#phy.sps.uniqu[grepl("Guizotia_eylesii", phy.sps.uniqu)] 
#phy.sps.uniqu[grepl("Guizotia_kassneri", phy.sps.uniqu)] 
#phy.sps.uniqu[grepl("Guizotia_nyikensis", phy.sps.uniqu)] 
#phy.sps.uniqu[grepl("Guizotia_oblonga", phy.sps.uniqu)] 
#phy.sps.uniqu[grepl("Guizotia_ringoetii", phy.sps.uniqu)] 
#phy.sps.uniqu[grepl("Guizotia_schultzii_", phy.sps.uniqu)] 
#phy.sps.uniqu[grepl("Wedelia_oblonga", phy.sps.uniqu)] 
matchednamesegret$sppMatch[which(matchednamesegret$egretname == "Guizotia_scabra")] <- "Guizotia_scabra_subsp._schimperi"

# Eupatorium maculatum. Synonyms: many,  but none that match the one in the tree
kew$taxon_name[grepl("Eupatorium_maculatum", kew$taxon_name)]
phy.sps.uniqu[grepl("Eupatoriadelphus_maculatus", phy.sps.uniqu)] 
phy.sps.uniqu[grepl("Eupatorium_maculatum", phy.sps.uniqu)] 
phy.sps.uniqu[grepl("Eupatorium_purpureum", phy.sps.uniqu)] 
phy.sps.uniqu[grepl("Eutrochium_maculatum", phy.sps.uniqu)] 

# Abies marocana. Synonyms: Abies pinsapo
kew$taxon_name[grepl("Abies_marocana", kew$taxon_name)] 
phy.sps.uniqu[grepl("Abies_pinsapo", phy.sps.uniqu)] 
matchednamesegret$sppMatch[which(matchednamesegret$egretname == "Abies_marocana")] <- "Abies_pinsapo"

# Betula utilis subsp. albosinensis. Synonyms: many,  but none that match the one in the tree
kew$taxon_name[grepl("Betula_utilis_subsp._albosinensis", kew$taxon_name)] 
phy.sps.uniqu[grepl("Betula_albosinensis", phy.sps.uniqu)] 
phy.sps.uniqu[grepl("Betula_utilis", phy.sps.uniqu)] 
phy.sps.uniqu[grepl("Betula_bhojpattra", phy.sps.uniqu)] 
matchednamesegret$sppMatch[which(matchednamesegret$egretname == "Betula_utilis_subsp._albosinensis")] <- "Betula_utilis" # TO CHECK: not accepted as a synonym in kew, but utilis fits the tree

# Eucalyptus delegatensis. Synonyms: 
kew$taxon_name[grepl("Eucalyptus_delegatensis", kew$taxon_name)] 
phy.sps.uniqu[grepl("Eucalyptus_delegatensis", phy.sps.uniqu)] 
matchednamesegret$sppMatch[which(matchednamesegret$egretname == "Eucalyptus_delegatensis")] <- "Eucalyptus_delegatensis_subsp._delegatensis"

# Eucalyptus ovata. Synonyms: Eucalyptus ovata subsp. ovata, but not var. Is it the same thing?
kew$taxon_name[grepl("Eucalyptus_ovata", kew$taxon_name)] 
phy.sps.uniqu[grepl("Eucalyptus_ovata", phy.sps.uniqu)] # TO ask: subsp ovata, splice in to ovata wouldn't it be better than splicing to genera?

# Eucalyptus pauciflora. Synonyms: 
kew$taxon_name[grepl("Eucalyptus_pauciflora", kew$taxon_name)] 
phy.sps.uniqu[grepl("Eucalyptus_pauciflora_subsp._pauciflora", phy.sps.uniqu)] # accepted infraspecific
matchednamesegret$sppMatch[which(matchednamesegret$egretname == "Eucalyptus_pauciflora")] <-  "Eucalyptus_pauciflora_subsp._pauciflora"

# Betula pendula subsp. mandshurica
kew$taxon_name[grepl("Betula_pendula_subsp._mandshurica", kew$taxon_name)] 
phy.sps.uniqu[grepl("Betula_pendula", phy.sps.uniqu)] # accepted intraspecific
matchednamesegret$sppMatch[which(matchednamesegret$egretname == "Betula_pendula_subsp._mandshurica")] <-  "Betula_pendula"

# Celtis pallida. Synonyms: many,  but none that match the one in the tree 
kew$taxon_name[grepl("Celtis_pallida", kew$taxon_name)]
phy.sps.uniqu[grepl("Celtis_pallida", phy.sps.uniqu)] 
phy.sps.uniqu[grepl("Celtis_spinosa_var._pallida", phy.sps.uniqu)] 
phy.sps.uniqu[grepl("Celtis_tala_var._pallida", phy.sps.uniqu)] 

# Echinacea spp.. Synonyms: 
kew$taxon_name[grepl("Echinacea_spp.", kew$taxon_name)] 
phy.sps.uniqu[grepl("Echinacea_spp.", phy.sps.uniqu)] 

# Eucalyptus pauciflora subsp. niphophila. Synonyms: Eucalyptus niphophila, doesn't match the tree
kew$taxon_name[grepl("Eucalyptus_pauciflora_subsp._niphophila", kew$taxon_name)]
phy.sps.uniqu[grepl("Eucalyptus niphophila", phy.sps.uniqu)] 


# Eucomis autumnalis. Synonyms:  many, but none that match the one in the tree
kew$taxon_name[grepl("Eucomis_autumnalis", kew$taxon_name)] 
phy.sps.uniqu[grepl("Fritillaria_autumnalis", phy.sps.uniqu)] 

# Thymophylla tephroleuca. Synonyms: many,but none that match the one in the tree
kew$taxon_name[grepl("Thymophylla_tephroleuca", kew$taxon_name)] 
phy.sps.uniqu[grepl("Dyssodia_tephroleuca", phy.sps.uniqu)] 

# Tilia platyphyllos subsp. corinthiaca. Synonyms: 
kew$taxon_name[grepl("Tilia_platyphyllos_subsp._corinthiaca", kew$taxon_name)] 
phy.sps.uniqu[grepl("Tilia_platyphyllos", phy.sps.uniqu)] # heterotypic accepted
phy.sps.uniqu[grepl("Tilia_corinthiaca", phy.sps.uniqu)] 
matchednamesegret$sppMatch[which(matchednamesegret$egretname == "Tilia_platyphyllos_subsp._corinthiaca")] <-  "Tilia_platyphyllos"

# Primula bulleyana subsp. beesiana. Synonyms: 
kew$taxon_name[grepl("Primula_bulleyana_subsp._beesiana", kew$taxon_name)]
phy.sps.uniqu[grepl("Primula_bulleyana", phy.sps.uniqu)] 
phy.sps.uniqu[grepl("Aleuritia_beesiana", phy.sps.uniqu)] 
phy.sps.uniqu[grepl("Primula_beesiana", phy.sps.uniqu)] # homotypic accepted
matchednamesegret$sppMatch[which(matchednamesegret$egretname == "Primula_bulleyana_subsp._beesiana")] <-  "Primula_beesiana"

# Pyrus glabra. Synonyms: 
kew$taxon_name[grepl("Pyrus_glabra", kew$taxon_name)]
phy.sps.uniqu[grepl("Pyrus_syriaca", phy.sps.uniqu)] # homotypic accepted
matchednamesegret$sppMatch[which(matchednamesegret$egretname == "Pyrus_glabra")] <-  "Pyrus_syriaca"

# Veronicastrum sibiricum. Synonyms: 
kew$taxon_name[grepl("Veronicastrum_sibiricum", kew$taxon_name)]
phy.sps.uniqu[grepl("Veronicastrum_sibiricum", phy.sps.uniqu)]
#phy.sps.uniqu[grepl("Callistachya_sibirica", phy.sps.uniqu)] 
#phy.sps.uniqu[grepl("Eustachya_cerulea", phy.sps.uniqu)] 
#phy.sps.uniqu[grepl("Eustachya_sibirica", phy.sps.uniqu)] 
#phy.sps.uniqu[grepl("Leptandra_sibirica", phy.sps.uniqu)] 
#phy.sps.uniqu[grepl("Veronica_sibirica", phy.sps.uniqu)] 
#phy.sps.uniqu[grepl("Veronica_virginica_var._sibirica", phy.sps.uniqu)] 
#phy.sps.uniqu[grepl("Veronicastrum_virginicum", phy.sps.uniqu)] # TO ASK: what happens when the only name in the tree is an accepted infraspecific?
matchednamesegret$sppMatch[which(matchednamesegret$egretname == "Veronicastrum_sibiricum")] <-  "Veronicastrum_sibiricum_var._zuccarinii"


# Solidago albopilosa. Synonyms: no synonyms
kew$taxon_name[grepl("Solidago_albopilosa", kew$taxon_name)]
phy.sps.uniqu[grepl("Solidago_albo", phy.sps.uniqu)] 

# Symphyotrichum oolentangiense. Synonyms:
kew$taxon_name[grepl("Symphyotrichum_oolentangiense", kew$taxon_name)]

phy.sps.uniqu[grepl("Aster_azureus", phy.sps.uniqu)] 
phy.sps.uniqu[grepl("Aster_capillaceus", phy.sps.uniqu)] 
phy.sps.uniqu[grepl("Aster_capillaris", phy.sps.uniqu)] 
phy.sps.uniqu[grepl("Aster_oolentangiensis", phy.sps.uniqu)] 
phy.sps.uniqu[grepl("Aster_poaceus", phy.sps.uniqu)] 
phy.sps.uniqu[grepl("Aster_shortii ", phy.sps.uniqu)] 
phy.sps.uniqu[grepl("Aster_vernalis", phy.sps.uniqu)] 
phy.sps.uniqu[grepl("Symphyotrichum_oolentangiense", phy.sps.uniqu)] 

# Rosa damascena. Synonyms: artificial hybrid, so many synnonyms 
kew$taxon_name[grepl("Rosa_damascena", kew$taxon_name)]
phy.sps.uniqu[grepl("damascena", phy.sps.uniqu)] 

# Solidago niederederi. Synonyms: hybrid and no synonyms 
phy.sps.uniqu[grepl("Solidago", phy.sps.uniqu)] 

# Fagus sylvatica. Synonyms: a lot
kew$taxon_name[grepl("Fagus_sylvatica", kew$taxon_name)]
phy.sps.uniqu[grepl("Fagus_sylvatica", phy.sps.uniqu)] 
matchednamesegret$sppMatch[which(matchednamesegret$egretname == "Fagus_sylvatica")] <-  "Fagus_sylvatica_subsp._orientalis"

# Carex scoparia. Synonyms: a lot, but non of them is in the tree
kew$taxon_name[grepl("Carex_scoparia", kew$taxon_name)]
phy.sps.uniqu[grepl("Loncoperis scoparia", phy.sps.uniqu)]
phy.sps.uniqu[grepl("Vignea scoparia", phy.sps.uniqu)]
phy.sps.uniqu[grepl("Carex lagopodioides", phy.sps.uniqu)]
phy.sps.uniqu[grepl("Carex leporina", phy.sps.uniqu)]
phy.sps.uniqu[grepl("Carex ovalis", phy.sps.uniqu)]
phy.sps.uniqu[grepl("Carex scoparia", phy.sps.uniqu)]
phy.sps.uniqu[grepl("Carex tribuloides", phy.sps.uniqu)]

# Chrysojasminum fruticans
kew$taxon_name[grepl("Chrysojasminum_fruticans", kew$taxon_name)]
phy.sps.uniqu[grepl("Chrysojasminum_fruticans", phy.sps.uniqu)]
phy.sps.uniqu[grepl("Jasminum_collinum", phy.sps.uniqu)]
phy.sps.uniqu[grepl("Jasminum frutescens", phy.sps.uniqu)]
phy.sps.uniqu[grepl("Jasminum frutescens", phy.sps.uniqu)]
phy.sps.uniqu[grepl("Jasminum_fruticans", phy.sps.uniqu)]
matchednamesegret$sppMatch[which(matchednamesegret$egretname == "Chrysojasminum_fruticans")] <-  "Jasminum_fruticans"

# Acer coriaceifolium
kew$taxon_name[grepl("Acer_coriaceifolium", kew$taxon_name)]
phy.sps.uniqu[grepl("Acer_cinnamomifolium", phy.sps.uniqu)]
phy.sps.uniqu[grepl("Acer_coriaceifolium", phy.sps.uniqu)]
phy.sps.uniqu[grepl("Acer_oblongum", phy.sps.uniqu)]
matchednamesegret$sppMatch[which(matchednamesegret$egretname == "Acer_coriaceifolium")] <-  "Acer_cinnamomifolium"

# Aster laevis
kew$taxon_name[grepl("Aster_laevis", kew$taxon_name)]
phy.sps.uniqu[grepl("Symphyotrichum_laeve", phy.sps.uniqu)]
matchednamesegret$sppMatch[which(matchednamesegret$egretname == "Aster_laevis")] <-  "Symphyotrichum_laeve"

# Calligonum alaschanicum
kew$taxon_name[grepl("Calligonum_alaschanicum", kew$taxon_name)]
phy.sps.uniqu[grepl("Calligonum_mongolicum", phy.sps.uniqu)]
matchednamesegret$sppMatch[which(matchednamesegret$egretname == "Calligonum_alaschanicum")] <-  "Calligonum_mongolicum"

# Loranthus_tanakae
kew$taxon_name[grepl("Loranthus_tanakae", kew$taxon_name)]
phy.sps.uniqu[grepl("Hyphear_tanakae", phy.sps.uniqu)]
matchednamesegret$sppMatch[which(matchednamesegret$egretname == "Loranthus_tanakae")] <-  "Hyphear_tanakae"

# Potentilla_argentea
kew$taxon_name[grepl("Potentilla_argentea", kew$taxon_name)]
phy.sps.uniqu[grepl("Potentilla_argentea", phy.sps.uniqu)]
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

### === === === === === === === === === === ###
#### Create EGRET tree ####
### === === === === === === === === === === ###
## first prune the phylogeny to include$ only these genera
phy.genera.egret<-drop.tip(phy.plants,
                           which(!phy.genera %in% egret.genus)) #36738 tips

egretTree <- keep.tip(phy.plants, which(phy.plants$tip.label %in% unique(egret$sppMatch)))

# Replace the tip names with the species names we have in egret
name_map <- setNames(matchednamesegret$egretname, matchednamesegret$sppMatch)
egretTree$tip.label <- ifelse(egretTree$tip.label %in% names(name_map),
                              name_map[egretTree$tip.label],
                              egretTree$tip.label)
phy.plants$tip.label <- ifelse(phy.plants$tip.label %in% names(name_map),
                               name_map[phy.plants$tip.label],
                               phy.plants$tip.label)


write.tree(egretTree,"analyses/output/egretPhylogeny.tre")

length(egretTree$tip.label)
length(unique(egret$sppMatch))
length(unique(egret$sppMatch))-length(egretTree$tip.label)
sort(egretTree$tip.label)



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
vec <- c("Carex", "Eucomis","Leontice", "Maackia", "Penstemon", "Celtis", "Solidago", "Symphyotrichum", "Thymophylla", "Verbesina", "Betonica", "Eupatorium", "Echinacea", "Eucalyptus", "Rosa")
t <- subset(egret_sub, genus %in% vec)
t <- t$latbi
spp_smalltree <- unique(t)
studyNosmall <- subset(egret_sub, latbi %in% spp_smalltree)

phy.sps.uniqu[grepl("Eucomis", phy.sps.uniqu)]#Eucomis_zambesiaca
phy.sps.uniqu[grepl("Leontice", phy.sps.uniqu)]#Leontice_leontopetalum
phy.sps.uniqu[grepl("Maackia", phy.sps.uniqu)]#Maackia_amurensis
phy.sps.uniqu[grepl("Celtis", phy.sps.uniqu)]#Celtis_schippii
phy.sps.uniqu[grepl("Thymophylla", phy.sps.uniqu)]#Thymophylla_setifolia
phy.sps.uniqu[grepl("Verbesina", phy.sps.uniqu)]#Verbesina_occidentalis
phy.sps.uniqu[grepl("Betonica", phy.sps.uniqu)]#Betonica_officinalis
phy.sps.uniqu[grepl("Eupatorium", phy.sps.uniqu)]#Eupatorium_fernaldii


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

#studyNosmall <- studyNosmall[!duplicated(studyNosmall$latbi), ]
#smallDataPhy <-  comparative.data(smallTree, studyNosmall, names.col = "latbi", na.omit = TRUE, vcv = TRUE, warn.dropped = TRUE)

#smallphytoplot <-  smallDataPhy$phy
#smallx <- smallDataPhy$data$count
#names(smallx) <- smallDataPhy$phy$tip.label

#smallmap <- contMap(smallTree, smallx, plot = T)

### === === === === === === === === === === ###
# Force the tree to be ultrametric FOR NOW.
### === === === === === === === === === === ###
smallTreeUltra <- force.ultrametric(
  smallTree,
  method = "extend"  # Extends terminal branches
)


unique(nomatchAfterKewcheck$egretname)
spptoplice <- c(nomatchAfterKewcheck$egretname[grepl("Carex", nomatchAfterKewcheck$egretname)], 
                nomatchAfterKewcheck$egretname[grepl("Eucomis", nomatchAfterKewcheck$egretname)], 
                nomatchAfterKewcheck$egretname[grepl("Leontice", nomatchAfterKewcheck$egretname)], 
                nomatchAfterKewcheck$egretname[grepl("Maackia", nomatchAfterKewcheck$egretname)], 
                nomatchAfterKewcheck$egretname[grepl("Penstemon", nomatchAfterKewcheck$egretname)], 
                nomatchAfterKewcheck$egretname[grepl("Celtis", nomatchAfterKewcheck$egretname)], 
                nomatchAfterKewcheck$egretname[grepl("Solidago", nomatchAfterKewcheck$egretname)], 
                nomatchAfterKewcheck$egretname[grepl("Symphyotrichum", nomatchAfterKewcheck$egretname)], 
                nomatchAfterKewcheck$egretname[grepl("Thymophylla", nomatchAfterKewcheck$egretname)], 
                nomatchAfterKewcheck$egretname[grepl("Verbesina", nomatchAfterKewcheck$egretname)], 
                nomatchAfterKewcheck$egretname[grepl("Betonica", nomatchAfterKewcheck$egretname)], 
                nomatchAfterKewcheck$egretname[grepl("Eupatorium", nomatchAfterKewcheck$egretname)], 
                nomatchAfterKewcheck$egretname[grepl("Echinacea", nomatchAfterKewcheck$egretname)], 
                nomatchAfterKewcheck$egretname[grepl("Eucalyptus", nomatchAfterKewcheck$egretname)], 
                nomatchAfterKewcheck$egretname[grepl("Rosa", nomatchAfterKewcheck$egretname)])


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


# create data with this small subset
#dataPhysmall <- comparative.data(egretTree.spliced, studyNosmall, names.col = "latbi", na.omit = T,vcv = F, warn.dropped = T)

#phyloplotsmall = dataPhysmall$phy
#xsmall = dataPhysmall$data$count
#names(xsmall) <- phyloplotsmall$tip.label

# plot something
#studysmall <- contMap(phyloplotsmall, xsmall, plot = T)

# jpeg("figures/splicedtree.jpeg", height = 1600, width = 2400, res = 300)
#plot(phyloplotsmall, cex = 0.8, show.tip.label = TRUE)

# instead of using comparative.data in caper, trying to use something simpler in ape for the same purpose

smallTreeUltraSpliced
studyNosmallSpliced

# remove duplicated rows
studyNosmallSpliced <- studyNosmallSpliced[!duplicated(studyNosmallSpliced$latbi), ]

rownames(studyNosmallSpliced) <- studyNosmallSpliced$latbi

# species in both tree and data
common_species <- intersect(smallTreeUltraSpliced$tip.label, studyNosmallSpliced$latbi)

# drop unmatched species from the tree and data
pruned_tree <- drop.tip(smallTreeUltraSpliced, setdiff(smallTreeUltraSpliced$tip.label, common_species))

# plot the tree
plot(pruned_tree, cex = 0.4, show.tip.label = TRUE)

# list of species got spliced
target <- nomatchAfterKewcheck$egretname

# get the index of the tip
tip_index <- match(target, pruned_tree$tip.label)

# Add a red dot to the tip
#edgelabels(pch = 21, col = "white", bg = "red", cex = 1, tip = tip_index, adj = c(3,0.5))
#slopeCol <- setMap(studysmall, colors=c("black"))
#h<-max(nodeHeights(slopeCol$tree))

# categorize them into splicing and non-splicing for color purpose
status <- ifelse(common_species %in% target, "splicing", "non-splicing")
status <- factor(status, levels = c("non-splicing", "splicing"))
mycol <- c("black", "red")[status]

# plot the tree
pdf("analyses/figures/egret_spliced.pdf", width = 20, height = 40)
plot(pruned_tree, cex = 1.5, tip.color = mycol)
dev.off()

# subset down to the species we have in egret 
egretTree.spliced <- keep.tip(studyNosmallSpliced, which(studyNosmallSpliced$tip.label %in% unique(d$latbi)))
# remove node label
egretTree.spliced$node.label <- NULL
plot(slopeCol,legend = F, lwd=3, ylim=c(1-0.09*(Ntip(slopeCol$tree)),Ntip(slopeCol$tree)))
### === === === === === === === === === === ###
# Find species that are alone in their genus #
### === === === === === === === === === === ###
# first look at how many species there is in each genus using Kew's database.
genustomatch <- nomatchAfterKewcheck$genus

# unique species in egret
egretunique <- egret[!duplicated(egret$latbi),]
#subset for the genus that we dont have match
egretnomatch <- subset(egretunique, genus %in% genustomatch)
# table to check how many species of these no match genus
counts <- table(egretnomatch$genus)

# Extract counts for your genera
result <- data.frame(
  Genus = names(counts),
  SpeciesCount = as.integer(counts),
  row.names = NULL
)

# Find species that are alone in their genus and grab a sister species instead
uniquespp <- subset(result, SpeciesCount == "1")
# start with one genus
test <- uniquespp$Genus[3]
spp <- subset(egretnomatch, genus %in% test)
eucalyptus <- subset(egretnomatch, genus == "Eucalyptus")
eucvecAll <- eucalyptus$latbi
eucNomatch <- unique(subset(nomatchAfterKewcheck, genus == "Eucalyptus"))
eucvecNomatch <- eucNomatch$egretname
eucvecNomatch <- gsub(" ", "_", eucvecNomatch)
# prune the tree to the only genus we have in egret
phy.genera.egret<-drop.tip(phy.plants,
                           which(!phy.genera %in% egret.genus)) 
# add tip for that species
result_nonultrametric <- add.species.to.genus(tree = phy.genera.egret,
                                              species = "Maackia_taiwanensis",
                                              where = "root")



# === === === === === === === === === ===  === === === === ===  === === === === 
#### USDA ####
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

# write.tree(egret.tree,"analyses/output/egretPhylogeny.tre")
