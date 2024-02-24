## Started 1 Dec 2023 ##
## Revised 20 Feb 2024 by Mao##
## taken from cleaningDL.R ##

# Needs to be sourced in cleanall.R

library("taxize")
library("stringr")

# General chekcs:
#1. fix typos and minor issues:
### Clean Species ##############################
d$species <- tolower(d$species)
d_species <- unique(paste(d$genus, d$species))

# Use taxize package to inspect whether names are correct
ref <- gnr_datasources() # Full list of dabases available
fix_names <- gnr_resolve(sci = d_species, with_canonical_ranks = T)
d_species_fix <- unique(fix_names$matched_name2)
names_changed <- setdiff(d_species, d_species_fix)
names_changed

sort(d_species)

# Remove trailing spaces:
d$genus <- str_trim(d$genus)
d$species <- str_trim(d$species)
# Go back and run line 10-17 again

# Fix#########################################

d$species[which(d$genus == "Colutea" & d$species == "bohsei")] <- "buhsei"
d$species[which(d$genus == "Abies" & d$species == "amabils")] <- "amabilis"
d$species[which(d$genus == "Lathyrus" & d$species == "sativa")] <- "sativus"
d$species[which(d$genus == "Carex" & d$species == "crytolepis")] <- "cryptolepis"
d$species[which(d$genus == "Vicia" & d$species == "bythinica")] <- "bithynica"
d$species[which(d$genus == "Penstemon" & d$species == "commarhenus")] <- "comarrhenus"
d$species[which(d$genus == "Asparagus" & d$species == "acutifolius l.")] <- "acutifolius"
d$species[which(d$genus == "Pinus" & d$species == "sylvestris l.")] <- "sylvestris"
d$species[which(d$genus == "Polygonum" & d$species == "aviculare l.")] <- "aviculare"
d$species[which(d$genus == "Dorema" & d$species == "ammoniacum d.")] <- "ammoniacum"
d$species[which(d$genus == "Tradescantia" & d$species == "ohioensis")] <- "ohiensis"
d$species[which(d$genus == "Betula" & d$species == "albo-sinensis")] <- "albosinensis"

d$genus[which(d$genus == "Pterocaryafra" & d$species == "fraxinifolia")] <- "Pterocarya"
#d$genus[which(d$genus == "Leontice\r\n" & d$species == "incerta")] <- "Leontice"
d$genus[which(d$genus == "Aanigozanthos" & d$species == "flavidus")] <- "Anigozanthos"
d$genus[which(d$genus == "Deginia" & d$species == "velebitica")] <- "Degenia"
d$genus[which(d$genus == "Lingularia" & d$species == "sibirica")] <- "Ligularia"
d$genus[which(d$genus == "Eucalytpus" & d$species == "delegatensis")] <- "Eucalyptus"

d$genus[which(d$genus == "Jasminus" & d$species == "fruiticans")] <- "Jasminum"
d$species[which(d$genus == "Jasminum" & d$species == "fruiticans")] <- "fruticans"

d$variety[which(d$genus == "Pedicularis" & d$species == "longiflora var.\ntubiformis")] <- "tubiformis"
d$species[which(d$genus == "Pedicularis" & d$species == "longiflora var.\ntubiformis")] <- "longiflora"
d$species[which(d$genus == "Ferula" & d$species == "assa foetida")] <- "assa-foetida"
#d$species[which(d$genus == "Astrgalus" & d$species == "cyclophyllon")] <- "cyclophyllus"
#d$species[which(d$genus == "Astrgalus" & d$species == "cyclophyllu")] <- "cyclophyllus"
#d$species[which(d$species == "Amurensis")] <- "amurensis"
#d$species[which(d$species == "aviculare L.")] <- "aviculare"
#d$species[which(d$species == "longiflora var.\r\ntubiformis")] <- "longiflora"
#d$species[which(d$species == "Pagoda")] <- "pagoda"
#d$species[which(d$species == "Sylvestris L.")] <- "sylvestris"
d$genus[which(d$genus == "Alstromeria" & d$species == "ligtu")] <- "Alstroemeria"
d$species[which(d$genus == "Vitis" & d$species == "vinifera x amurensis")] <- "amurensis"
d$species[which(d$genus == "Astragalus" & d$species == "cyclophyllu")] <- "cyclophyllos"
# Confirm ####################################
d_species <- unique(paste(d$genus, d$species))
fix_names <- gnr_resolve(sci = d_species, with_canonical_ranks = T)
d_species_fix <- unique(fix_names$matched_name2)
names_changed <- setdiff(d_species, d_species_fix) # Confirm this is of length 0
names_changed

### TO CHECK: look though the species names or there any that seem strange? If so---flag for discussion. For example:
#Echinacea angustifolia, purpurea, pallida -- 3 species in 1 study
#For this particular experiment, they mixed seeds of 3 species together.
#Rows of NAs found in the dataset.
