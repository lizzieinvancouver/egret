## Started on 10 Feb. by Mao ##

d <- read.csv("input/ospreeforegret.csv")

d
runworldflora <- FALSE
if(runworldflora){	
  library("stringr")
  library("WorldFlora")
  # Read in the backbone dataset
  backbone <- read.csv("C:/PhD/Project/classification.csv",head = TRUE, sep="\t")
  d_species <- unique(paste(d$genus, d$species))
  checks<-WFO.match(spec.data=d_species, WFO.data=backbone, counter=1, verbose=TRUE)
  d_species_fix <- unique(checks$scientificName)
  names_changed <- setdiff(d_species, d_species_fix)
  names_changed
}

d$species[which(d$genus == "Populus" & d$species == "koreana")] <- "suaveolens"
d$species[which(d$genus == "Rosa" & d$species == "hugonis")] <- "xanthina"
d$genus[which(d$genus == "Photinia" & d$species == "villosa")] <- "Pourthiaea"
d$species[which(d$genus == "Eleutherococcus" & d$species == "setchuenensis")] <- "leucorrhizus var. setchuenensis"
d$species[which(d$genus == "Berberis" & d$species == "dielsiana")] <- "dasystachya"
d$species[which(d$genus == "Acer" & d$species == "ginnala")] <- "tataricum subsp. ginnala"
d$species[which(d$genus == "Cladrastis" & d$species == "lutea")] <- "kentukea"
d$species[which(d$genus == "Elaeagnus" & d$species == "ebbingei")] <- "× submacrophylla"
d$species[which(d$genus == "Juglans" & d$species == "ailantifolia")] <- "mandshurica var. sachalinensis"
d$species[which(d$genus == "Parrotiopsis" & d$species == "jaquemontiana")] <- "jacquemontiana"
d$genus[which(d$genus == "Rhamnus" & d$species == "frangula")] <- "Frangula"
d$species[which(d$genus == "Frangula" & d$species == "frangula")] <- "alnus"
d$species[which(d$genus == "Salix" & d$species == "smithiana")] <- "gmelinii"
d$species[which(d$genus == "Sambucus" & d$species == "pubens")] <- "racemosa subsp. pubens"
d$genus[which(d$genus == "Spirea" & d$species == "alba")] <- "Spiraea"
d$species[which(d$genus == "Stachyurus" & d$species == "sinensis")] <- "chinensis"



