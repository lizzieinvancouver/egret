## Started 1 Dec 2023 ##
## taken from cleaningDL.R ##

# Needs to be sourced in cleanall.R

# General chekcs:
#1. fix typos and minor issues:
### Clean Species ##############################
dat$species <- tolower(dat$species)
dat_species <- unique(paste(dat$genus, dat$species))

# Use taxize package to inspect whether names are correct
ref <- gnr_datasources() # Full list of databases available
fix_names <- gnr_resolve(sci = dat_species, with_canonical_ranks = T)
dat_species_fix <- unique(fix_names$matched_name2)
names_changed <- setdiff(dat_species, dat_species_fix)
names_changed

sort(dat_species)

# Remove trailing spaces:
dat$genus <- str_trim(dat$genus)
dat$species <- str_trim(dat$species)

# Fix#########################################
dat$species[which(dat$genus == "Colutea" & dat$species == "bohsei")] <- "buhsei"
dat$species[which(dat$genus == "Abies" & dat$species == "amabils")] <- "amabilis"
dat$species[which(dat$genus == "Lathyrus" & dat$species == "sativa")] <- "sativus"
dat$species[which(dat$genus == "Carex" & dat$species == "crytolepis")] <- "cryptolepis"
dat$species[which(dat$genus == "Vicia" & dat$species == "bythinica")] <- "bithynica"
dat$species[which(dat$genus == "Penstemon" & dat$species == "commarhenus")] <- "comarrhenus"
dat$species[which(dat$genus == "Asparagus" & dat$species == "acutifolius l.")] <- "acutifolius"
dat$species[which(dat$genus == "Pinus" & dat$species == "sylvestris l.")] <- "sylvestris"
dat$species[which(dat$genus == "Polygonum" & dat$species == "aviculare l.")] <- "aviculare"
dat$species[which(dat$genus == "Dorema" & dat$species == "ammoniacum d.")] <- "ammoniacum"
dat$species[which(dat$genus == "Tradescantia" & dat$species == "ohioensis")] <- "ohiensis"
dat$species[which(dat$genus == "Betula" & dat$species == "albo-sinensis")] <- "albosinensis"

dat$genus[which(dat$genus == "Pterocaryafra" & dat$species == "fraxinifolia")] <- "Pterocarya"
dat$genus[which(dat$genus == "Leontice\r\n" & dat$species == "incerta")] <- "Leontice"
dat$genus[which(dat$genus == "Aanigozanthos" & dat$species == "flavidus")] <- "Anigozanthos"
dat$genus[which(dat$genus == "Deginia" & dat$species == "velebitica")] <- "Degenia"
dat$genus[which(dat$genus == "Lingularia" & dat$species == "sibirica")] <- "Ligularia"
dat$genus[which(dat$genus == "Eucalytpus" & dat$species == "delegatensis")] <- "Eucalyptus"

dat$genus[which(dat$genus == "Jasminus" & dat$species == "fruiticans")] <- "Jasminum"
dat$species[which(dat$genus == "Jasminum" & dat$species == "fruiticans")] <- "fruticans"

dat$variety[which(dat$genus == "Pedicularis" & dat$species == "longiflora var.\r\ntubiformis")] <- "tubiformis"
dat$species[which(dat$genus == "Pedicularis" & dat$species == "longiflora var.\r\ntubiformis")] <- "longiflora"
dat$species[which(dat$genus == "Ferula" & dat$species == "assa foetida")] <- "assa-foetida"
dat$species[which(dat$genus == "Astrgalus" & dat$species == "cyclophyllon")] <- "cyclophyllus"
dat$species[which(dat$genus == "Astrgalus" & dat$species == "cyclophyllu")] <- "cyclophyllus"
dat$species[which(dat$species == "Amurensis")] <- "amurensis"
dat$species[which(dat$species == "aviculare L.")] <- "aviculare"
dat$species[which(dat$species == "longiflora var.\r\ntubiformis")] <- "longiflora"
dat$species[which(dat$species == "Pagoda")] <- "pagoda"
dat$species[which(dat$species == "Sylvestris L.")] <- "sylvestris"

# Confirm ####################################
dat_species <- unique(paste(dat$genus, dat$species))
fix_names <- gnr_resolve(sci = dat_species, with_canonical_ranks = T)
dat_species_fix <- unique(fix_names$matched_name2)
names_changed <- setdiff(dat_species, dat_species_fix) # Confirm this is of length 0
names_changed

### TO CHECK: look though the species names or there any that seem strange? If so---flag for discussion. For example:
#Echinacea angustifolia, purpurea, pallida -- 3 species in 1 study

