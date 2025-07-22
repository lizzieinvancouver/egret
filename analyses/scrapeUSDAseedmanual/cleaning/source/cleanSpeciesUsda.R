## Started 10 July 2024 ##
## By Dan, continued by Justin ##

## Updated 26 Jan 2025 by Mao ##

## Fix empty sp names
library("stringr")
d$species_name <- tolower(d$species_name)
d$species_name <- str_trim(d$species_name)
d$genus_name <- str_trim(d$genus_name)

d$species_name <- replace(d$species_name, is.na(d$species_name), "")

d$species_name[which(d$genus_name == "Acer" & d$species_name == "")] <-"pensylvanicum"
d$species_name[which(d$genus_name == "Pinus" & d$species_name == "" & d$samples == "135")] <-"roxburghii"
d$species_name[which(d$genus_name == "Pinus" & d$species_name == "" & d$day_temp_celsius == "22-26")] <-"sabiniana"
d$species_name[which(d$genus_name == "Pinus" & d$species_name == "" & d$samples == "4" & d$test_duration_in_days == ">60")] <-"sibirica"
d$species_name[which(d$genus_name == "Pinus" & d$species_name == "" & d$samples == "31")] <-"strobiformis"
d$species_name[which(d$genus_name == "Pinus" & d$species_name == "" & d$samples == "20")] <-"strobus"
d$species_name[which(d$genus_name == "Pinus" & d$species_name == "" & d$samples == "28")] <-"strobus"
d$species_name[which(d$genus_name == "Pinus" & d$species_name == "" & d$samples == "99")] <-"sylvestris"
d$species_name[which(d$genus_name == "Pinus" & d$species_name == "" & d$samples == "36")] <-"sylvestris"
d$species_name[which(d$genus_name == "Pinus" & d$species_name == "" & d$samples == "18")] <-"sylvestris"
d$species_name[which(d$genus_name == "Pinus" & d$species_name == "" & d$samples == "481")] <-"taeda"
d$species_name[which(d$genus_name == "Pinus" & d$species_name == "" & d$samples == "19" & d$day_temp_celsius == "24" )] <-"thunbergiana"
d$species_name[which(d$genus_name == "Pinus" & d$species_name == "" & d$samples == "100" & d$day_temp_celsius == "25" )] <-"thunbergiana"
d$species_name[which(d$genus_name == "Pinus" & d$species_name == "" &d$samples == "29" )] <-"virginiana"
d$species_name[which(d$genus_name == "Pinus" & d$species_name == "" & d$medium == "pe, S" & d$samples == "5" )] <-"virginiana"
d$species_name[which(d$genus_name == "Pinus" & d$species_name == "" & d$samples == "12" )] <-"wallichiana"
d$species_name[which(d$genus_name == "Pinus" & d$species_name == "" & d$samples == "14" )] <-"heldreichii"
d$species_name[which(d$genus_name == "Pinus" & d$species_name == "" & d$avg_germinative_energy_percent == "69" & d$test_duration_in_days == "40" )] <-"heldreichii"
d$species_name[which(d$genus_name == "Pinus" & d$species_name == "" & d$avg_germinative_energy_percent == "99")] <-"jeffreyi"
d$species_name[which(d$genus_name == "Pinus" & d$species_name == "" & d$medium == "V" & d$avg_germinative_energy_percent == "79")] <-"jeffreyi"
d$species_name[which(d$genus_name == "Pinus" & d$species_name == "" & d$avg_germinative_energy_percent == "85-95")] <-"koraiensis"
d$species_name[which(d$genus_name == "Pinus" & d$species_name == "" & d$medium == "V" & d$avg_germinative_energy_percent == "59")] <-"lambertiana"
d$species_name[which(d$genus_name == "Pinus" & d$species_name == "" & d$medium == "P, S" & d$avg_germinative_energy_percent == "55")] <-"lambertiana"
d$species_name[which(d$genus_name == "Pinus" & d$species_name == "" & d$medium == "A" & d$avg_germinative_energy_percent == "67" & d$day_temp_celsius == "20")] <-"merkusii"
d$species_name[which(d$genus_name == "Pinus" & d$species_name == "" & d$medium == "pl  S" & d$avg_germinative_energy_percent == "44")] <-"monticola"
d$species_name[which(d$genus_name == "Pinus" & d$species_name == "" & d$medium == "A" & d$avg_germinative_energy_percent == "45")] <-"mugo"
d$species_name[which(d$genus_name == "Pinus" & d$species_name == "" & d$medium == "A" & d$avg_germinative_energy_percent == "80" & d$day_temp_celsius == "30")] <-"mugo"
d$species_name[which(d$genus_name == "Pinus" & d$species_name == "" & d$medium == "V" & d$avg_germinative_energy_percent == "38")] <-"muricata"
d$species_name[which(d$genus_name == "Pinus" & d$species_name == "" & d$medium == "P" & d$avg_germinative_energy_percent == "85")] <-"muricata"
d$species_name[which(d$genus_name == "Pinus" & d$species_name == "" & d$medium == "A" & d$avg_germinative_energy_percent == "86")] <-"nigra"
d$species_name[which(d$genus_name == "Pinus" & d$species_name == "" & d$medium == "S V" & d$avg_germinative_energy_percent == "95")] <-"palustris"
d$species_name[which(d$genus_name == "Pinus" & d$species_name == "" & d$medium == "S" & d$germination_time_days == "35")] <-"parviflora"
d$species_name[which(d$genus_name == "Pinus" & d$species_name == "" & d$medium == "A" & d$avg_germinative_energy_percent == "80" & d$day_temp_celsius == "25")] <-"parviflora"
d$species_name[which(d$genus_name == "Pinus" & d$species_name == "" & d$medium == "A" & d$avg_germinative_energy_percent == "69" & d$day_temp_celsius == "20")] <-"peuce"
d$species_name[which(d$genus_name == "Pinus" & d$species_name == "" & d$medium == "A, P" & d$avg_germinative_energy_percent == "79")] <-"pinaster"
d$species_name[which(d$genus_name == "Pinus" & d$species_name == "" & d$medium == "A" & d$avg_germinative_energy_percent == "83")] <-"pinaster"
d$species_name[which(d$genus_name == "Pinus" & d$species_name == "" & d$medium == "A" & d$avg_germinative_energy_percent == "81")] <-"pinea"
d$species_name[which(d$genus_name == "Pinus" & d$species_name == "" & d$medium == "A" & d$avg_germinative_energy_percent == "98")] <-"pinea"
d$species_name[which(d$genus_name == "Pinus" & d$species_name == "" & d$medium == "A" & d$avg_germinative_energy_percent == "67" & d$samples == "100")] <-"ponderosa var. ponderosa"
d$species_name[which(d$genus_name == "Pinus" & d$species_name == "" & d$medium == "pe, S" & d$avg_germinative_energy_percent == "59" & d$samples == "186")] <-"ponderosa var. ponderosa"
d$species_name[which(d$genus_name == "Pinus" & d$species_name == "scopulorum")] <-"ponderosa var. scopulorum"
d$species_name[which(d$genus_name == "Pinus" & d$species_name == "" & d$medium == "S" & d$avg_germinative_energy_percent == "64" & d$samples == "40")] <-"ponderosa var. scopulorum"
d$species_name[which(d$genus_name == "Pinus" & d$species_name == "" & d$medium == "pe, S" & d$avg_germinative_energy_percent == "65" & d$samples == "9")] <-"pungens"
d$species_name[which(d$genus_name == "Pinus" & d$species_name == "" & d$medium == "P" & d$avg_germinative_energy_percent == "81" & d$samples == "9")] <-"radiata"
d$species_name[which(d$genus_name == "Pinus" & d$species_name == "" & d$medium == "V" & d$avg_germinative_energy_percent == "67" & d$samples == "15")] <-"radiata"
d$species_name[which(d$genus_name == "Pinus" & d$species_name == "" & d$medium == "S" & d$avg_germinative_energy_percent == "75" & d$samples == "551")] <-"resinosa"
d$species_name[which(d$genus_name == "Pinus" & d$species_name == "" & d$medium == "A" & d$avg_germinative_energy_percent == "47" & d$samples == "6")] <-"rigida"
d$species_name[which(d$genus_name == "Pinus" & d$species_name == "" & d$medium == "A" & d$avg_germinative_energy_percent == "70" & d$samples == "19")] <-"rigida"
d$species_name[which(d$genus_name == "Pseudotsuga" & d$species_name == "" & d$test_duration_in_days == "100")] <-"macrocarpa"
d$species_name[which(d$genus_name == "Pseudotsuga" & d$species_name == "" & d$medium == "Sand")] <-"macrocarpa"
d$species_name[which(d$genus_name == "Pseudotsuga" & d$species_name == "menziesii" & d$test_duration_in_days == "14-21")] <-"menziesii var. glauca"
d$species_name[which(d$genus_name == "Pseudotsuga" & d$species_name == "glauca")] <-"menziesii var. glauca"
d$species_name[which(d$genus_name == "Pseudotsuga" & d$species_name == "" & d$medium == "Paper" & d$test_duration_in_days == "17")] <-"menziesii var. glauca"
d$species_name[which(d$genus_name == "Pseudotsuga" & d$species_name == "" & d$medium == "Paper" & d$test_duration_in_days == "29")] <-"menziesii var. glauca"
d$species_name[which(d$genus_name == "Pseudotsuga" & d$species_name == "" & d$medium == "Vermiculite" & d$test_duration_in_days == "30" & d$samples == "6")] <-"menziesii var. glauca"
d$species_name[which(d$genus_name == "Pseudotsuga" & d$species_name == "" & d$medium == "Vermiculite" & d$test_duration_in_days == "50" & d$samples == "6")] <-"menziesii var. glauca"
d$species_name[which(d$genus_name == "Pseudotsuga" & d$species_name == "" & d$medium == "Vermiculite" & d$test_duration_in_days == "30" & d$samples == "6")] <-"menziesii var. glauca"
d$species_name[which(d$genus_name == "Pseudotsuga" & d$species_name == "menziesii" )] <-"menziesii var. menziesii"
d$species_name[which(d$genus_name == "Pseudotsuga" & d$species_name == "" & d$medium == "Vermiculite" & d$samples =="20")] <-"menziesii var. menziesii"
d$species_name[which(d$genus_name == "Pseudotsuga" & d$species_name == "" & d$samples =="129")] <-"menziesii var. menziesii"
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

d$seed_source[which(d$species_name == "macrophyllum source i")] <- "Source 1"
d$seed_source[which(d$species_name == "macrophyllum source 2")] <- "Source 2"
d$seed_source[which(d$species_name == "macrophyllum source 3")] <- "Source 3"
d$seed_source[which(d$species_name == "rubrump low elevation (u)")] <- "low elevation (u)"
d$seed_source[which(d$species_name == "rubrump low elevation (s)")] <- "low elevation (s)"
d$seed_source[which(d$species_name == "rubrump High elevation (u)")] <- "high elevation (U)"
d$seed_source[which(d$species_name == "rubrump High elevation (s)")] <- "high elevation (S)"
d$seed_source[which(d$species_name == "glutinosa (pennsylvania)")] <- "Pennsylvania"
d$seed_source[which(d$species_name == "glutinosa (finland)")] <- "Finland"
d$seed_source[which(d$species_name == "incana (europe)")] <- "Europe"
d$seed_source[which(d$species_name == "incana (finland)")] <- "Finland"

# Fixing species names
d$species_name[which(d$genus_name == "Acer" & d$species_name == "ginnala")] <-"tataricum subsp. ginnala"
d$species_name[which(d$species_name == "macrophyllum source i")] <-"macrophyllum"
d$species_name[which(d$species_name == "macrophyllum source 2")] <-"macrophyllum"
d$species_name[which(d$species_name == "macrophyllum source 3")] <-"macrophyllum"
d$species_name[which(d$genus_name == "Acer" & d$species_name == "pensylvanicumt")] <-"pensylvanicum"
d$species_name[which(d$genus_name == "Acer" & d$species_name == "")] <-"pensylvanicum"
d$species_name[which(d$genus_name == "Acer" & d$species_name == "rubrump")] <-"rubrum"
d$species_name[which(d$species_name == "rubrump low elevation (u)")] <-"rubrum"
d$species_name[which(d$species_name == "rubrump low elevation (s)")] <-"rubrum"
d$species_name[which(d$species_name == "rubrump high elevation (u)")] <-"rubrum"
d$species_name[which(d$species_name == "rubrump high elevation (s)")] <-"rubrum"
d$species_name[which(d$genus_name == "Alnus" & d$species_name == "glutinosa (pennsylvania)")] <-"glutinosa"
d$species_name[which(d$genus_name == "Alnus" & d$species_name == "glutinosa (finland)")] <-"glutinosa"
d$species_name[which(d$genus_name == "Alnus" & d$species_name == "incana (europe)")] <-"incana"
d$species_name[which(d$genus_name == "Alnus" & d$species_name == "incana (finland)")] <-"incana"
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
d$species_name[which(d$genus_name == "Rubus" & d$species_name == "idaeust glen cova")] <-"idaeus"
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
d$species_name[which(d$genus_name == "Pinus" & d$species_name ==  "thunbergiana")] <-"thunbergii"
d$species_name[which(d$genus_name == "Pinus" & d$species_name == "scopulorum")] <-"scopulorum"
d$genus_name[which(d$genus_name == "Prosopis" & d$species_name == "juliflora")] <-"Neltuma"

# make a new column "spec" to combine genus and species
d$latbi <- paste(d$genus_name, d$species_name, sep = "_")
unique(d$latbi)
# There are 326 unique species in USDA
