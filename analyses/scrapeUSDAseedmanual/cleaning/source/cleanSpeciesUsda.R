## Started 10 July 2024 ##
## By Dan, continued by Justin ##

## Updated 26 Jan 2025 by Mao ##

# Read in the backbone dataset
runworldflora <- FALSE
if(runworldflora){
  backbone <- read.csv("C:/PhD/Project/classification.csv",head = TRUE, sep="\t")
  d$species_name <- tolower(d$species_name)
  d$genus_name <- str_trim(d$genus_name)
  d$species_name <- str_trim(d$species_name)
  d_species <- unique(paste(d$genus_name, d$species_name))
  checks<-WFO.match(spec.data=d_species, WFO.data=backbone, counter=1, verbose=TRUE)
  d_species_fix <- unique(checks$scientificName)
  names_changed <- setdiff(d_species, d_species_fix)
  names_changed
}

d$seed_source[which(d$species_name == "macrophyllum source I")] <- "Source 1"
d$seed_source[which(d$species_name == "acrophyllum source 2")] <- "Source 2"
d$seed_source[which(d$species_name == "acrophyllum source 3")] <- "Source 3"
d$seed_source[which(d$species_name == "rubrump low elevation (U)")] <- "low elevation (u)"
d$seed_source[which(d$species_name == "rubrump low elevation (S)")] <- "low elevation (s)"
d$seed_source[which(d$species_name == "rubrump High elevation (U)")] <- "high elevation (U)"
d$seed_source[which(d$species_name == "rubrump High elevation (S)")] <- "high elevation (S)"
d$seed_source[which(d$species_name == "glutinosa (Pennsylvania)")] <- "Pennsylvania"
d$seed_source[which(d$species_name == "glutinosa (Finland)")] <- "Finland"
d$seed_source[which(d$species_name == "incana (Europe)")] <- "Europe"
d$seed_source[which(d$species_name == "incana (Finland)")] <- "Finland"
d$seed_type[which(is.na(d$seed_type) & d$species_name == "incana ssp. tenuifolia fresh seeds")] <- "fresh seed"

# Fixing species names
d$species_name[which(d$genus_name == "Acer" & d$species_name == "ginnala")] <-"tataricum subsp. ginnala"
d$species_name[which(d$species_name == "macrophyllum Source I")] <-"macrophyllum"
d$species_name[which(d$species_name == "macrophyllum Source 2")] <-"macrophyllum"
d$species_name[which(d$species_name == "macrophyllum Source 3")] <-"macrophyllum"
d$species_name[which(d$genus_name == "Acer" & d$species_name == "pensylvanicumt")] <-"pensylvanicum"
d$species_name[which(d$genus_name == "Acer" & d$species_name == "")] <-"sp."
d$species_name[which(d$genus_name == "Acer" & d$species_name == "rubrump")] <-"rubrum"
d$species_name[which(d$species_name == "rubrump Low elevation (U)")] <-"rubrum"
d$species_name[which(d$species_name == "rubrump Low elevation (S)")] <-"rubrum"
d$species_name[which(d$species_name == "rubrump High elevation (U)")] <-"rubrum"
d$species_name[which(d$species_name == "rubrump High elevation (S)")] <-"rubrum"
d$species_name[which(d$genus_name == "Alnus" & d$species_name == "glutinosa (Pennsylvania)")] <-"glutinosa"
d$species_name[which(d$genus_name == "Alnus" & d$species_name == "glutinosa (Finland)")] <-"glutinosa"
d$species_name[which(d$genus_name == "Alnus" & d$species_name == "incana (Europe)")] <-"incana"
d$species_name[which(d$genus_name == "Alnus" & d$species_name == "incana (Finland)")] <-"incana"
d$species_name[which(d$genus_name == "Alnus" & d$species_name == "i. ssp. tenuifolia fresh seeds")] <-"incana subsp. tenuifolia"
d$species_name[which(d$genus_name == "Alnus" & d$species_name == "i. ssp. tenuifolia fresh seeds")] <-"incana subsp. tenuifolia"
d$species_name[which(d$genus_name == "Alnus" & d$species_name == "viridis")] <-"alnobetula subsp. fruticosa"
d$species_name[which(d$genus_name == "Alnus" & d$species_name == "viridis ssp. crispa")] <-"alnobetula subsp. crispa"
d$species_name[which(d$genus_name == "Alnus" & d$species_name == "viridis ssp. sinuata")] <-"alnobetula subsp. sinuata"
d$species_name[which(d$genus_name == "Aesculus" & d$species_name == "arguta")] <-"glabra var. arguta"
d$species_name[which(d$genus_name == "Cotoneaster" & d$species_name == "lucidus")] <-"acutifolius"
d$species_name[which(d$genus_name == "Corylus" & d$species_name == "cornuta var. californica")] <-"cornuta subsp. californica"
d$species_name[which(d$genus_name == "Crataegus" & d$species_name == "anomala")] <-"holmesiana"
d$species_name[which(d$genus_name == "Ceanothus" & d$species_name == "greggii")] <-"pauciflorus"
d$species_name[which(d$genus_name == "Ceanothus" & d$species_name == "sorediatus")] <-"arboreus"
d$species_name[which(d$genus_name == "Carya" & d$species_name == "illinoensis")] <-"illinoinensis"
d$species_name[which(d$genus_name == "Cercis" & d$species_name == "canadensis var. texensis")] <-"canadensis subsp. texensis"
d$species_name[which(d$genus_name == "Celtis" & d$species_name == "laevigata var. reticulata")] <-"reticulata"
d$species_name[which(d$genus_name == "Euonymus" & d$species_name == "americana")] <-"americanum"
d$species_name[which(d$genus_name == "Euonymus" & d$species_name == "atropurpurea")] <-"atropurpureus"
d$species_name[which(d$genus_name == "Euonymus" & d$species_name == "bungeana")] <-"maackii"
d$species_name[which(d$genus_name == "Euonymus" & d$species_name == "europaea")] <-"europaeus"
d$species_name[which(d$genus_name == "Euonymus" & d$species_name == "hamiltoniana ssp. maackii")] <-"maackii"
d$species_name[which(d$genus_name == "Euonymus" & d$species_name == "verrucosa")] <-"verrucosus"
d$species_name[which(d$genus_name == "Fraxinus" & d$species_name == "veluting")] <-"velutina"
d$genus_name[which(d$genus_name == "Acacia" & d$species_name == "farnesiana")] <-"Vachellia"
d$genus_name[which(d$genus_name == "Acacia" & d$species_name == "nilotica")] <-"Vachellia"
d$genus_name[which(d$genus_name == "Mahonia" & d$species_name == "aquifolium")] <-"Berberis"
d$genus_name[which(d$genus_name == "Mahonia" & d$species_name == "fremontii")] <-"Berberis"
d$genus_name[which(d$genus_name == "Mahonia" & d$species_name == "nevinii")] <-"Berberis"
d$genus_name[which(d$genus_name == "Mahonia" & d$species_name == "repens")] <-"Berberis"
d$species_name[which(d$genus_name == "Malus" & d$species_name == "bacatta")] <-"baccata"
d$species_name[which(d$genus_name == "Malus" & d$species_name == "ioensist")] <-"ioensis"
d$species_name[which(d$genus_name == "Nyssa" & d$species_name == "sylvatica var. sylvatica")] <-"sylvatica"
d$species_name[which(d$genus_name == "Sambucus" & d$species_name == "nigra ssp. canadensis")] <-"canadensis"
d$species_name[which(d$genus_name == "Sambucus" & d$species_name == "nigra spp. cerulea")] <-"cerulea"
d$species_name[which(d$genus_name == "Sambucus" & d$species_name == "racemosa var. racemosa")] <-"racemosa subsp. racemosa"
d$species_name[which(d$genus_name == "Syringa" & d$species_name == "reticulata var. amurensis")] <-"reticulata subsp. amurensis"
d$species_name[which(d$genus_name == "Taxodium" & d$species_name == "ascendens")] <-"distichum var. imbricarium"
d$species_name[which(d$genus_name == "Ribes" & d$species_name == "oxyacanthoides ssp. irriguum")] <-"oxyacanthoides var. irriguum"
d$species_name[which(d$genus_name == "Ribes" & d$species_name == "rotundifolia")] <-"rotundifolium"
d$species_name[which(d$genus_name == "Rubus" & d$species_name == "idaeust Glen Cova")] <-"idaeus"
d$species_name[which(d$genus_name == "Rubus" & d$species_name == "chamemorus")] <-"chamaemorus"
d$species_name[which(d$genus_name == "Quercus" & d$species_name == "durmosa")] <-"dumosa"
d$species_name[which(d$genus_name == "Quercus" & d$species_name == "prinus")] <-"michauxii"
d$species_name[which(d$genus_name == "Quercus" & d$species_name == "vaccinifolia")] <-"vacciniifolia"
d$species_name[which(d$genus_name == "Quercus" & d$species_name == "wislizenii")] <-"wislizeni"
d$species_name[which(d$genus_name == "Prunus" & d$species_name == "alleghaniensis")] <-"umbellata"
d$species_name[which(d$genus_name == "Prunus" & d$species_name == "caroliana")] <-"caroliniana"
d$species_name[which(d$genus_name == "Pinus" & d$species_name == "latifolia")] <-"engelmannii"
d$species_name[which(d$genus_name == "Pinus" & d$species_name == "murrayana")] <-"contorta var. murrayana"
d$species_name[which(d$genus_name == "Pinus" & d$species_name == "densa")] <-"elliottii var. densa"
d$species_name[which(d$genus_name == "Pinus" & d$species_name == "elliotti")] <-"elliottii"
d$species_name[which(d$genus_name == "Pinus" & d$species_name == "")] <-"sp."
d$species_name[which(d$genus_name == "Pinus" & d$species_name == "thunbergiana")] <-"thunbergii"
d$species_name[which(d$genus_name == "Pinus" & d$species_name == "scopulorum")] <-"scopulorum"
d$species_name[which(d$genus_name == "Pseudotsuga" & d$species_name == "")] <-"sp."
d$species_name[which(d$genus_name == "Pseudotsuga" & d$species_name == "glauca")] <-"menziesii var. glauca"
d$genus_name[which(d$genus_name == "Prosopis" & d$species_name == "juliflora")] <-"Neltuma"

# make a new column "spec" to combine genus and species
d$latbi <- paste(d$genus_name, d$species_name, sep = "_")