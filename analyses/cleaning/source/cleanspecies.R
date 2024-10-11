## Started 1 Dec 2023 ##
## Started by Deirdre ##
## Revised 20 Feb 2024 by Mao# #
## Revised 11 Oct 2024 by Mao using WorldFlora instead of taxize ##
## taken from cleaningDL.R ##

# Needs to be sourced in cleanall.R

#library("taxize")
#library("stringr")
#library("WorldFlora")

#backbone <- read.csv("C:/PhD/Project/classification.csv",head = TRUE, sep="\t")
d$species <- tolower(d$species)
# Remove trailing spaces:
d$genus <- str_trim(d$genus)
d$species <- str_trim(d$species)
#d_species <- unique(paste(d$genus, d$species))
#checks<-WFO.match(spec.data=d_species, WFO.data=backbone, counter=1, verbose=TRUE)
#d_species_fix <- unique(checks$scientificName)
#names_changed <- setdiff(d_species, d_species_fix)
#names_changed
# General chekcs:
#1. fix typos and minor issues:
### Clean Species ##############################
#library("taxize")
#d$species <- tolower(d$species)
#d_species <- unique(paste(d$genus, d$species))

# Use taxize package to inspect whether names are correct
#ref <- gnr_datasources() # Full list of dabases available
#fix_names <- gnr_resolve(sci = d_species, with_canonical_ranks = T)
#d_species_fix_1 <- unique(fix_names$matched_name2)
#names_changed_1 <- setdiff(d_species, d_species_fix_1)
#names_changed_1

#sort(d_species)

# Fix#########################################
d$genus[which(d$genus == "Borreria" & d$species == "articularis")] <- "Spermacoce"
d$species[which(d$genus == "Dichotomanthes" & d$species == "tristaniaecarpa")] <- "tristaniicarpa"
d$species[which(d$genus == "Magnolia" & d$species == "ingrata")] <- "fulva"
d$variety[which(d$genus == "Magnolia" & d$species == "fulva")] <- "fulva"
d$species[which(d$genus == "Colutea" & d$species == "bohsei")] <- "buhsei"
d$species[which(d$genus == "Pinus" & d$species == "leucodermis")] <- "heldreichii"
d$species[which(d$genus == "Capparis" & d$species == "ovata")] <- "spinosa"
d$variety[which(d$genus == "Capparis" & d$species == "spinosa")] <- "ovata"
d$species[which(d$genus == "Prunus" & d$species == "azorica")] <- "lusitanica subsp. azorica"
d$species[which(d$genus == "Lathyrus" & d$species == "sativa")] <- "sativus"
d$species[which(d$genus == "Vicia" & d$species == "bythinica")] <- "bithynica"
d$species[which(d$genus == "Betula" & d$species == "platyphylla")] <- "pendula subsp. mandshurica"
d$species[which(d$genus == "Thalictrum" & d$species == "rochebrunianum")] <- "rochebruneanum"
d$species[which(d$genus == "Carex" & d$species == "crytolepis")] <- "cryptolepis"
d$genus[which(d$genus == "Aanigozanthos" & d$species == "flavidus")] <- "Anigozanthos"
d$species[which(d$genus == "Abies" & d$species == "amabils")] <- "amabilis"
d$genus[which(d$genus == "Selinum" & d$species == "wallichianum")] <- "Ligusticopsis"
d$species[which(d$genus == "Ligusticopsis" & d$species == "wallichianum")] <- "wallichiana"
d$species[which(d$genus == "Penstemon" & d$species == "commarhenus")] <- "comarrhenus"
d$species[which(d$genus == "Penstemon" & d$species == "moffattii")] <- "moffatii"
d$genus[which(d$genus == "Porteresia" & d$species == "coarctata")] <- "Oryza"
d$species[which(d$genus == "Tilia" & d$species == "rubra")] <- "platyphyllos subsp. corinthiaca"
d$species[which(d$genus == "Acer" & d$species == "morrisonense")] <- "caudatifolium"
d$genus[which(d$genus == "Pterocaryafra" & d$species == "fraxinifolia")] <- "Pterocarya"
d$species[which(d$genus == "Asparagus" & d$species == "acutifolius l.")] <- "acutifolius"
d$genus[which(d$genus == "Deginia" & d$species == "velebitica")] <- "Degenia"
d$species[which(d$genus == "Pinus" & d$species == "sylvestris l.")] <- "sylvestris"
d$genus[which(d$genus == "Alstromeria" & d$species == "ligtu")] <- "Alstroemeria"
d$species[which(d$genus == "Eucalyptus" & d$species == "niphophila")] <- "pauciflora subsp. niphophila"
d$species[which(d$genus == "Eucalyptus" & d$species == "andreana")] <- "elata"
d$genus[which(d$genus == "Jasminus" & d$species == "fruiticans")] <- "Chrysojasminum"
d$species[which(d$genus == "Chrysojasminum" & d$species == "fruiticans")] <- "fruticans"
d$genus[which(d$genus == "Sorbus" & d$species == "domestica")] <- "Cormus"
d$species[which(d$genus == "Echinacea" & d$species == "angustifolia, purpurea, pallida")] <- "spp."
d$species[which(d$genus == "Astragalus" & d$species == "cyclophyllu")] <- "cyclophyllon"
d$genus[which(d$genus == "Mimulus" & d$species == "guttatus")] <- "Erythranthe"
d$species[which(d$genus == "Erythranthe" & d$species == "guttatus")] <- "guttata"
d$genus[which(d$genus == "Emblica" & d$species == "officinalis")] <- "Phyllanthus"
d$species[which(d$genus == "Phyllanthus" & d$species == "officinalis")] <- "emblica"
d$species[which(d$genus == "Acer" & d$species == "cinnamomifolium")] <-  "coriaceifolium"
d$species[which(d$genus == "Rhamnus" & d$species == "catharticus")] <- "cathartica"
d$genus[which(d$genus == "Cleome" & d$species == "serrulata")] <- "Cleomella"
d$genus[which(d$genus == "Pasania" & d$species == "glabra")] <-"Lithocarpus"
d$species[which(d$genus == "Lithocarpus" & d$species == "glabra")] <- "glaber"
d$species[which(d$genus == "Ferula" & d$species == "assa foetida")] <- "assa-foetida" #For some reason WFO.match doesn't like "-", but the species name actually has a "-" in it, so remove the "-" for confirming later.
d$species[which(d$genus == "Vitis" & d$species == "vinifera x amurensis")] <- "amurensis"
d$species[which(d$genus == "Crambe" & d$species == "abyssinica")] <-  "hispanica subsp. abyssinica"
d$species[which(d$genus == "Primula" & d$species == "beesiana")] <- "bulleyana subsp. beesiana"
d$species[which(d$genus == "Polygonum" & d$species == "aviculare l.")] <- "aviculare"
d$genus[which(d$genus == "Eucalytpus" & d$species == "delegatensis")] <- "Eucalyptus"
d$genus[which(d$genus == "Rhaponticum" & d$species == "carthamoides")] <- "Leuzea"
d$genus[which(d$genus == "Cupressus" & d$species == "arizonica")] <- "Hesperocyparis"
d$species[which(d$genus == "Calligonum" & d$species == "potaninii")] <- "mongolicum"
d$species[which(d$genus == "Betula" & d$species == "albo-sinensis")] <- "utilis subsp. albosinensis"
d$species[which(d$genus == "Pedicularis" & d$species == "longiflora var.\ntubiformis")] <- "longiflora"
d$variety[which(d$genus == "Pedicularis" & d$species == "longiflora")] <- "tubiformis"
d$genus[which(d$genus == "Polygonum" & d$species == "persicaria")] <- "Persicaria"
d$species[which(d$genus == "Persicaria" & d$species == "persicaria")] <- "maculosa"
d$genus[which(d$genus == "Dodecatheon" & d$species == "meadia")] <- "Primula"
d$genus[which(d$genus == "Aster" & d$species == "azureus")] <- "Symphyotrichum"
d$species[which(d$genus == "Symphyotrichum" & d$species == "azureus")] <- "oolentangiense"
d$genus[which(d$genus == "Aster" & d$species == "novae-angliae")] <- "Symphyotrichum"
d$species[which(d$genus == "Tradescantia" & d$species == "ohioensis")] <- "ohiensis"
d$genus[which(d$genus == "Lingularia" & d$species == "sibirica")] <- "Ligularia"
d$genus[which(d$genus == "Dorema" & d$species == "ammoniacum d.")] <- "Ferula"
d$species[which(d$genus == "Ferula" & d$species == "ammoniacum d.")] <- "ammoniacum"




# Confirm ####################################
#d_species <- unique(paste(d$genus, d$species))
#checks<-WFO.match(spec.data=d_species, WFO.data=backbone, counter=1, verbose=TRUE)
#d_species_fix <- unique(checks$scientificName)
#names_changed <- setdiff(d_species, d_species_fix)
#names_changed

### TO CHECK: look though the species names or there any that seem strange? If so---flag for discussion. For example:
#Rows of NAs found in the dataset.
