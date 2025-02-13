## Started 8 July 2024 by Mao##
## Updated 10 Feb. 2025 by Mao ##

# General chekcs:
#1. fix typos and minor issues:
### Clean Species ##############################
baskin <- read.csv("input/Baskin_Dormancy_Database.csv", skip=2) 
baskin$X1 <- NULL
# Substitute the underscore with space
baskin$Genus_species <- sub("_", " ", baskin$Genus_species)
# Use worldFlora package to inspect whether names are correct
runworldflora <- TRUE
if(runworldflora){	
  library("stringr")
  library("WorldFlora")
  # Read in the backbone dataset
  backbone <- read.csv("C:/PhD/Project/classification.csv",head = TRUE, sep="\t")
   checks<-WFO.match(spec.data=baskin$Genus_species, WFO.data=backbone, counter=1, verbose=TRUE)
  d_species_fix <- unique(checks$scientificName)
  names_changed <- setdiff(baskin$Genus_species, d_species_fix)
  names_changed
}
write.csv(checks, "cleaningOther/baskin/checksFull.csv")
# Ok, there are way too many species names need to be fixed, including species we might not end up using.
# Only correct species names appears in egret dataset
source("cleaning/source/mergeData.R") 
source("cleaning/source/cleandatasetID.R")
source("cleaning/source/cleanSpecies.R")
egret_sp <- paste(d$genus, d$species)
shared_sp <- intersect(egret_sp, d_species_fix)
shared_sp_list <- checks[checks$scientificName %in% shared_sp, ]
shared_sp_list <- shared_sp_list[, c("spec.name","spec.name.ORIG", "scientificName")]
baskin_sp_overlap <- intersect(shared_sp_list$spec.name.ORIG, baskin$Genus_species)
baskin_sp_overlap

# I don't want to change them manually, let's see if we can use the code to help
baskin_egret <- baskin[baskin$Genus_species %in% baskin_sp_overlap, ]
shared_sp_list$Genus_species <- shared_sp_list$spec.name.ORIG
shared_sp_list <- unique(shared_sp_list)
sp_name_changed <- merge(baskin_egret,shared_sp_list, by = "Genus_species", all.x = TRUE)
# This could work, but I realized that there are some species with multiple accepted names, which needs decisions, so I will do it manually anyway...
names_changed <- setdiff(sp_name_changed$Genus_species, sp_name_changed$scientificName)
names_changed
baskin_egret$Genus_species[which(baskin_egret$Genus_species == "Acer kawakamii")] <- "Acer caudatifolium"
baskin_egret$Genus_species[which(baskin_egret$Genus_species == "Acer morrisonense")] <- "Acer caudatifolium"
baskin_egret$Genus_species[which(baskin_egret$Genus_species == "Anigozanthos manglesii")] <- "Anigozanthos flavidus"
baskin_egret$Genus_species[which(baskin_egret$Genus_species == "Aster azureus")] <- "Symphyotrichum oolentangiense"
baskin_egret$Genus_species[which(baskin_egret$Genus_species == "Aster novae-angliae")] <- "Symphyotrichum novae-angliae"
baskin_egret$Genus_species[which(baskin_egret$Genus_species == "Aster oolentangiensis")] <- "Symphyotrichum oolentangiense"
baskin_egret$Genus_species[which(baskin_egret$Genus_species == "Betula albo-sinensis")] <- "Betula utilis subsp. albosinensis"
baskin_egret$Genus_species[which(baskin_egret$Genus_species == "Betula platyphylla")] <- "Betula pendula subsp. mandshurica"
baskin_egret$Genus_species[which(baskin_egret$Genus_species == "Bidens tripartita")] <- "Bidens pilosa"
baskin_egret$Genus_species[which(baskin_egret$Genus_species == "Borreria articularis")] <- "Spermacoce articularis"
baskin_egret$Genus_species[which(baskin_egret$Genus_species == "Brassica arvensis")] <- "Sinapis arvensis"
baskin_egret$Genus_species[which(baskin_egret$Genus_species == "Brassica kaber")] <- "Sinapis arvensis"
baskin_egret$Genus_species[which(baskin_egret$Genus_species == "Calligonum alashanicum")] <- "Calligonum alaschanicum"
baskin_egret$Genus_species[which(baskin_egret$Genus_species == "Calligonum leucocladium")] <- "Calligonum leucocladum"
baskin_egret$Genus_species[which(baskin_egret$Genus_species == "Calligonum potaninii")] <- "Calligonum mongolicum"
baskin_egret$Genus_species[which(baskin_egret$Genus_species == "Carex leporina")] <- "Carex scoparia"
baskin_egret$Genus_species[which(baskin_egret$Genus_species == "Carex ovalis")] <- "Carex scoparia"
baskin_egret$Genus_species[which(baskin_egret$Genus_species == "Carex sempervirens")] <- "Carex stricta"
baskin_egret$Genus_species[which(baskin_egret$Genus_species == "Carex spicata")] <- "Carex divisa"
baskin_egret$Genus_species[which(baskin_egret$Genus_species == "Cleome serrulata")] <- "Cleomella serrulata"
baskin_egret$Genus_species[which(baskin_egret$Genus_species == "Cupressus arizonica")] <- "Hesperocyparis arizonica"
baskin_egret$Genus_species[which(baskin_egret$Genus_species == "Cyclobalanopsis glauca")] <- "Quercus glauca"
baskin_egret$Genus_species[which(baskin_egret$Genus_species == "Dodecatheon meadia")] <- "Primula meadia"
baskin_egret$Genus_species[which(baskin_egret$Genus_species == "Emblica officinalis")] <- "Phyllanthus emblica"
baskin_egret$Genus_species[which(baskin_egret$Genus_species == "Eucalpytus ovata")] <- "Eucalyptus ovata"
baskin_egret$Genus_species[which(baskin_egret$Genus_species == "Eucalpytus pauciflora")] <- "Eucalyptus pauciflora"
baskin_egret$Genus_species[which(baskin_egret$Genus_species == "Eucalpytus regnans")] <- "Eucalyptus regnans"
baskin_egret$Genus_species[which(baskin_egret$Genus_species == "Eucalyptus longifolia")] <- "Eucalyptus elata"
baskin_egret$Genus_species[which(baskin_egret$Genus_species == "Geranium lanuginosum")] <- "Geranium carolinianum"
baskin_egret$Genus_species[which(baskin_egret$Genus_species == "Jasminum fruticans")] <- "Chrysojasminum fruticans"
baskin_egret$Genus_species[which(baskin_egret$Genus_species == "Picea abiesc")] <- "Picea abies"
baskin_egret$Genus_species[which(baskin_egret$Genus_species == "Pinus excelsa")] <- "Pinus wallichiana"
baskin_egret$Genus_species[which(baskin_egret$Genus_species == "Pinus leucodermis")] <- "Pinus heldreichii"
baskin_egret$Genus_species[which(baskin_egret$Genus_species == "Pinus longifolia")] <- "Pinus roxburghii"
baskin_egret$Genus_species[which(baskin_egret$Genus_species == "Pinus resinosa")] <- "Pinus sylvestris"
baskin_egret$Genus_species[which(baskin_egret$Genus_species == "Polygonum persicaria")] <- "Persicaria maculosa"
baskin_egret$Genus_species[which(baskin_egret$Genus_species == "Prunus dulcis")] <- "Prunus avium"
baskin_egret$Genus_species[which(baskin_egret$Genus_species == "Quercus bambusaefolia")] <- "Quercus glauca"
baskin_egret$Genus_species[which(baskin_egret$Genus_species == "Rhamnus carthartica")] <- "Rhamnus cathartica"
baskin_egret$Genus_species[which(baskin_egret$Genus_species == "Rosa multiflora")] <- "Rosa damascena"
baskin_egret$Genus_species[which(baskin_egret$Genus_species == "Scaevola frutescens")] <- "Scaevola taccada"
baskin_egret$Genus_species[which(baskin_egret$Genus_species == "Selinum wallichianum")] <- "Ligusticopsis wallichiana"
baskin_egret$Genus_species[which(baskin_egret$Genus_species == "Sorbus domestica")] <- "Cormus domestica"
baskin_egret$Genus_species[which(baskin_egret$Genus_species == "Tsuga heterophyllad")] <- "Tsuga heterophylla"
baskin_egret$Genus_species[which(baskin_egret$Genus_species == "Zannichellia palustrisi")] <- "Zannichellia palustris"
baskin_egret$Genus_species[which(baskin_egret$Genus_species == "Zannichellia pedunculata")] <- "Ruppia maritima"
baskin_egret <- unique(baskin_egret)

write.csv(baskin_egret,"output/baskinegretclean.csv")

