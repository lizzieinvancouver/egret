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
setwd("/Users/christophe_rouleau-desrochers/Desktop/UBC/egretLOCAL")
# wfodf <- read.csv("classification.csv", header = TRUE, sep = "\t")
kew <- read.csv("wcvp/wcvp_names.csv", header = TRUE, sep = "|", stringsAsFactors = FALSE )
kew$taxon_name <- gsub(" ", "_", kew$taxon_name)
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
phy.sps.uniqu[grepl("Phlox_alba", phy.sps.uniqu)] 
phy.sps.uniqu[grepl("Phlox_bimaculata", phy.sps.uniqu)] 
phy.sps.uniqu[grepl("Phlox_candida", phy.sps.uniqu)] 
phy.sps.uniqu[grepl("Phlox_excelsa", phy.sps.uniqu)] 
phy.sps.uniqu[grepl("Phlox_fruticosa", phy.sps.uniqu)] 
phy.sps.uniqu[grepl("Phlox_maculata", phy.sps.uniqu)] 
phy.sps.uniqu[grepl("Phlox_odorata", phy.sps.uniqu)] 
phy.sps.uniqu[grepl("Phlox_penduliflora", phy.sps.uniqu)] 
phy.sps.uniqu[grepl("Phlox_reflexa", phy.sps.uniqu)] 
phy.sps.uniqu[grepl("Phlox_suaveolens", phy.sps.uniqu)] 

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
phy.sps.uniqu[grepl("Dasiphora_reptans", phy.sps.uniqu)] 
phy.sps.uniqu[grepl("Dynamidium_reptans", phy.sps.uniqu)] 
phy.sps.uniqu[grepl("Fragaria_reptans", phy.sps.uniqu)] 
phy.sps.uniqu[grepl("Tormentilla_linnaeana", phy.sps.uniqu)] 
phy.sps.uniqu[grepl("Potentilla_reptans_var._sericophylla", phy.sps.uniqu)] # accepted infraspecific

# Alstroemeria_ligtu. Synonyms:  many, but none that match the one in the tree 
kew$taxon_name[grepl("Alstroemeria_ligtu", kew$taxon_name)] 
phy.sps.uniqu[grepl("Alstroemeria_ligtu_subsp._simsii", phy.sps.uniqu)] # TO ASK: accepted infraspecific
 
# Taraxacum_platycarpum. Synonyms: Taraxacum_platycarpum_subsp._hondoense
kew$taxon_name[grepl("Taraxacum_platycarpum", kew$taxon_name)] 
phy.sps.uniqu[grepl("Taraxacum_platycarpum", phy.sps.uniqu)] 
matchednamesegret$sppMatch[which(matchednamesegret$egretname == "Taraxacum_platycarpum")] <- "Taraxacum_platycarpum_subsp._hondoense"

# Verbesina_encelioide. Synonyms: many, but none that match the one in the tree
kew$taxon_name[grepl("Verbesina_encelioides", kew$taxon_name)] 
phy.sps.uniqu[grepl("Ximenesia_encelioides", phy.sps.uniqu)] 
phy.sps.uniqu[grepl("Encelia_albescens", phy.sps.uniqu)] 
phy.sps.uniqu[grepl("Pallasia_serratifolia", phy.sps.uniqu)] 
phy.sps.uniqu[grepl("Verbesina_australis", phy.sps.uniqu)] 
phy.sps.uniqu[grepl("Verbesina_encelioides", phy.sps.uniqu)] 
phy.sps.uniqu[grepl("Verbesina_exauriculata", phy.sps.uniqu)] 
phy.sps.uniqu[grepl("Verbesina_microptera_", phy.sps.uniqu)] 
phy.sps.uniqu[grepl("Verbesina_scabra", phy.sps.uniqu)] 
phy.sps.uniqu[grepl("Ximenesia_australis", phy.sps.uniqu)] 
phy.sps.uniqu[grepl("Ximenesia_encelioides", phy.sps.uniqu)]
phy.sps.uniqu[grepl("Ximenesia_exauriculata", phy.sps.uniqu)]
phy.sps.uniqu[grepl("Ximenesia_microptera", phy.sps.uniqu)] 

# Heliopsis_helianthoides. Synonyms: Heliopsis_helianthoides_var._scabra
kew$taxon_name[grepl("Heliopsis_helianthoides", kew$taxon_name)] 
phy.sps.uniqu[grepl("Heliopsis_helianthoides", phy.sps.uniqu)] 
matchednamesegret$sppMatch[which(matchednamesegret$egretname == "Heliopsis_helianthoides")] <- "Heliopsis_helianthoides_var._scabra"

# Guizotia_scabra. Synonyms: many,  but none that match the one in the tree 
kew$taxon_name[grepl("Guizotia_scabra", kew$taxon_name)] 
phy.sps.uniqu[grepl("Veslingia_scabra", phy.sps.uniqu)] 
phy.sps.uniqu[grepl("Coreopsis_galericulata", phy.sps.uniqu)] 
phy.sps.uniqu[grepl("Guizotia_collina", phy.sps.uniqu)] 
phy.sps.uniqu[grepl("Guizotia_eylesii", phy.sps.uniqu)] 
phy.sps.uniqu[grepl("Guizotia_kassneri", phy.sps.uniqu)] 
phy.sps.uniqu[grepl("Guizotia_nyikensis", phy.sps.uniqu)] 
phy.sps.uniqu[grepl("Guizotia_oblonga", phy.sps.uniqu)] 
phy.sps.uniqu[grepl("Guizotia_ringoetii", phy.sps.uniqu)] 
phy.sps.uniqu[grepl("Guizotia_schultzii_", phy.sps.uniqu)] 
phy.sps.uniqu[grepl("Wedelia_oblonga", phy.sps.uniqu)] 

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
phy.sps.uniqu[grepl("Celtis_pallida_var._discolor", phy.sps.uniqu)] 
phy.sps.uniqu[grepl("Celtis_spinosa_var._pallida", phy.sps.uniqu)] 
phy.sps.uniqu[grepl("Celtis_tala_var._pallida", phy.sps.uniqu)] 

# Echinacea spp.. Synonyms: 
kew$taxon_name[grepl("Echinacea_spp.", kew$taxon_name)] 
phy.sps.uniqu[grepl("Echinacea_spp.", phy.sps.uniqu)] 

# Eucalyptus pauciflora subsp. niphophila. Synonyms: Eucalyptus niphophila
kew$taxon_name[grepl("Eucalyptus_pauciflora_subsp._niphophila", kew$taxon_name)]
phy.sps.uniqu[grepl("Eucalyptus_pauciflora", phy.sps.uniqu)] 

# Eucomis autumnalis. Synonyms:  many, but none that match the one in the tree
kew$taxon_name[grepl("Eucomis_autumnalis", kew$taxon_name)] 
phy.sps.uniqu[grepl("Fritillaria_autumnalis", phy.sps.uniqu)] 

# Thymophylla tephroleuca. Synonyms: many,but none that match the one in the tree
kew$taxon_name[grepl("Thymophylla_tephroleuca", kew$taxon_name)] 
phy.sps.uniqu[grepl("Thymophylla", phy.sps.uniqu)] 

# Tilia platyphyllos subsp. corinthiaca. Synonyms: 
kew$taxon_name[grepl("Tilia_platyphyllos_subsp._corinthiaca", kew$taxon_name)] 
phy.sps.uniqu[grepl("Tilia_platyphyllos", phy.sps.uniqu)] # heterotypic accepted
phy.sps.uniqu[grepl("Tilia_corinthiaca", phy.sps.uniqu)] 
matchednamesegret$sppMatch[which(matchednamesegret$egretname == "Tilia_platyphyllos_subsp._corinthiaca")] <-  "Tilia_platyphyllos"

# Primula bulleyana subsp. beesiana. Synonyms: 
phy.sps.uniqu[grepl("Primula_bulleyana_subsp._beesiana", phy.sps.uniqu)] 
phy.sps.uniqu[grepl("Aleuritia_beesiana", phy.sps.uniqu)] 
phy.sps.uniqu[grepl("Primula_beesiana", phy.sps.uniqu)] # homotypic accepted
matchednamesegret$sppMatch[which(matchednamesegret$egretname == "Primula_bulleyana_subsp._beesiana")] <-  "Primula_beesiana"

# Pyrus glabra. Synonyms: 
phy.sps.uniqu[grepl("Pyrus_syriaca", phy.sps.uniqu)] # homotypic accepted
matchednamesegret$sppMatch[which(matchednamesegret$egretname == "Pyrus_glabra")] <-  "Pyrus_syriaca"

# Veronicastrum sibiricum. Synonyms: 
phy.sps.uniqu[grepl("Callistachya_sibirica", phy.sps.uniqu)] 
phy.sps.uniqu[grepl("Eustachya_cerulea", phy.sps.uniqu)] 
phy.sps.uniqu[grepl("Eustachya_sibirica", phy.sps.uniqu)] 
phy.sps.uniqu[grepl("Leptandra_sibirica", phy.sps.uniqu)] 
phy.sps.uniqu[grepl("Veronica_sibirica", phy.sps.uniqu)] 
phy.sps.uniqu[grepl("Veronica_virginica_var._sibirica", phy.sps.uniqu)] 
phy.sps.uniqu[grepl("Veronicastrum_virginicum", phy.sps.uniqu)] # TO ASK: what happens when the only name in the tree is an accepted infraspecific?

# Solidago albopilosa. Synonyms: no synonyms
phy.sps.uniqu[grepl("Solidago_albo", phy.sps.uniqu)] 

# Symphyotrichum oolentangiense. Synonyms: 
phy.sps.uniqu[grepl("Aster_azureus", phy.sps.uniqu)] 
phy.sps.uniqu[grepl("Aster_capillaceus", phy.sps.uniqu)] 
phy.sps.uniqu[grepl("Aster_capillaris", phy.sps.uniqu)] 
phy.sps.uniqu[grepl("Aster_oolentangiensis", phy.sps.uniqu)] 
phy.sps.uniqu[grepl("Aster_poaceus", phy.sps.uniqu)] 
phy.sps.uniqu[grepl("Aster_shortii ", phy.sps.uniqu)] 
phy.sps.uniqu[grepl("Aster_vernalis", phy.sps.uniqu)] 
phy.sps.uniqu[grepl("Symphyotrichum_oolentangiense", phy.sps.uniqu)] 

# Rosa damascena. Synonyms: artificial hybrid, so many synnonyms 
phy.sps.uniqu[grepl("damascena", phy.sps.uniqu)] 

# Solidago niederederi. Synonyms: hybrid and no synonyms 
phy.sps.uniqu[grepl("Solidago_niederederi", phy.sps.uniqu)] 

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
set.seed(123)
vec <- c("Clematis", "Koelreuteria","Rosa", "Eucalyptus", "Potentilla", "Penstemon")
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

### === === === === === === === === === === ###
# Force the tree to be ultrametric FOR NOW.
### === === === === === === === === === === ###
smallTreeUltra <- force.ultrametric(
  smallTree,
  method = "extend"  # Extends terminal branches
)

unique(nomatchAfterKewcheck$egretname)
spptoplice <- c(nomatchAfterKewcheck$egretname[grepl("Eucalyptus", nomatchAfterKewcheck$egretname)], 
                nomatchAfterKewcheck$egretname[grepl("Potentilla", nomatchAfterKewcheck$egretname)],
                nomatchAfterKewcheck$egretname[grepl("Rosa", nomatchAfterKewcheck$egretname)])


# Loop through and add species one-by-one to the tree
for (i in spptoplice ) {
  smallTreeUltraSpliced <- add.species.to.genus(tree = smallTreeUltra, species = i, where = "root")
}

smallnamesphy <- smallTreeUltraSpliced$tip.label
studyNosmallSpliced <- subset(studyNo, sppMatch %in% smallnamesphy)

smallTreeUltraSpliced$root.edge <- 0

is.rooted(smallTreeUltraSpliced)
smallTreeUltraSpliced$node.label<-NULL



# create data with this small subset
dataPhysmall <- comparative.data(egretTree.spliced, studyNosmall, names.col = "latbi", na.omit = T,
                                 vcv = F, warn.dropped = T)

phyloplotsmall = dataPhysmall$phy
xsmall = dataPhysmall$data$count
names(xsmall) <- phyloplotsmall$tip.label

# plot something
studysmall <- contMap(phyloplotsmall, xsmall, plot = T)

# jpeg("figures/splicedtree.jpeg", height = 1600, width = 2400, res = 300)
plot(phyloplotsmall, cex = 0.8, show.tip.label = TRUE)
target <- "Eucalyptus_ovata"

# Get the index of the tip
tip_index <- which(phyloplotsmall$tip.label == target)

# Add a red dot to the tip
tiplabels(pch = 21, col = "white", bg = "red", cex = 2, tip = tip_index, adj = c(3,0.5))

slopeCol <- setMap(studysmall, colors=c("black"))
h<-max(nodeHeights(slopeCol$tree))




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
