## Started 1 Dec 2023 ##
## taken from cleaningDL.R ##

# Needs to be sourced in cleanall.R

# General chekcs:
#1. fix typos and minor issues:
### Clean Species ##############################
egret$species <- tolower(egret$species)
egret_species <- unique(paste(egret$genus, egret$species))
# Use taxize package to inspect
# ref <- gnr_datasources() # Full list of databases available
# fix_names <- gnr_resolve(sci = egret_species, with_canonical_ranks = T)
# egret_species_fix <- unique(fix_names$matched_name2)
# names_changed <- setdiff(egret_species, egret_species_fix)
# names_changed
sort(egret_species)

# Remove training spaces:
egret$genus <- str_trim(egret$genus)
egret$species <- str_trim(egret$species)

# Fix#########################################
egret$species[which(egret$genus == "Colutea" & egret$species == "bohsei")] <- "buhsei"
egret$species[which(egret$genus == "Abies" & egret$species == "amabils")] <- "amabilis"
egret$species[which(egret$genus == "Lathyrus" & egret$species == "sativa")] <- "sativus"
egret$species[which(egret$genus == "Carex" & egret$species == "crytolepis")] <- "cryptolepis"
egret$species[which(egret$genus == "Vicia" & egret$species == "bythinica")] <- "bithynica"
egret$species[which(egret$genus == "Penstemon" & egret$species == "commarhenus")] <- "comarrhenus"
egret$species[which(egret$genus == "Asparagus" & egret$species == "acutifolius l.")] <- "acutifolius"
egret$species[which(egret$genus == "Pinus" & egret$species == "sylvestris l.")] <- "sylvestris"
egret$species[which(egret$genus == "Polygonum" & egret$species == "aviculare l.")] <- "aviculare"
egret$species[which(egret$genus == "Dorema" & egret$species == "ammoniacum d.")] <- "ammoniacum"
egret$species[which(egret$genus == "Tradescantia" & egret$species == "ohioensis")] <- "ohiensis"
egret$species[which(egret$genus == "Betula" & egret$species == "albo-sinensis")] <- "albosinensis"

egret$genus[which(egret$genus == "Pterocaryafra" & egret$species == "fraxinifolia")] <- "Pterocarya"
egret$genus[which(egret$genus == "Leontice\r\n" & egret$species == "incerta")] <- "Leontice"
egret$genus[which(egret$genus == "Aanigozanthos" & egret$species == "flavidus")] <- "Anigozanthos"
egret$genus[which(egret$genus == "Deginia" & egret$species == "velebitica")] <- "Degenia"
egret$genus[which(egret$genus == "Lingularia" & egret$species == "sibirica")] <- "Ligularia"
egret$genus[which(egret$genus == "Eucalytpus" & egret$species == "delegatensis")] <- "Eucalyptus"

egret$genus[which(egret$genus == "Jasminus" & egret$species == "fruiticans")] <- "Jasminum"
egret$species[which(egret$genus == "Jasminum" & egret$species == "fruiticans")] <- "fruticans"

egret$variety[which(egret$genus == "Pedicularis" & egret$species == "longiflora var.\r\ntubiformis")] <- "tubiformis"
egret$species[which(egret$genus == "Pedicularis" & egret$species == "longiflora var.\r\ntubiformis")] <- "longiflora"
egret$species[which(egret$genus == "Ferula" & egret$species == "assa foetida")] <- "assa-foetida"
egret$species[which(egret$genus == "Astrgalus" & egret$species == "cyclophyllon")] <- "cyclophyllus"
egret$species[which(egret$genus == "Astrgalus" & egret$species == "cyclophyllu")] <- "cyclophyllus"
egret$species[which(egret$species == "Amurensis")] <- "amurensis"
egret$species[which(egret$species == "aviculare L.")] <- "aviculare"
egret$species[which(egret$species == "longiflora var.\r\ntubiformis")] <- "longiflora"
egret$species[which(egret$species == "Pagoda")] <- "pagoda"
egret$species[which(egret$species == "Sylvestris L.")] <- "sylvestris"


# Confirm ####################################
egret_species <- unique(paste(egret$genus, egret$species))
fix_names <- gnr_resolve(sci = egret_species, with_canonical_ranks = T)
egret_species_fix <- unique(fix_names$matched_name2)
names_changed <- setdiff(egret_species, egret_species_fix) # Confirm this is of length 0
names_changed

### TO CHECK: Echinacea angustifolia, purpurea, pallida -- 3 species in 1 study
egret$sp.name <- paste(egret$genus, egret$species, sep = "_")
#View(sort(unique(egret$sp.name)))
unique(egret$study)
egret$study <- gsub(" ","", egret$study)

unique(egret$variety) # 49