# Housekeeping
rm(list = ls())
setwd("/Users/dindin/Documents/work/oegres_scraping/oegres")

# 
install.packages("devtools")
install.packages("readxl")
library("readxl")
library("devtools")

# trying out taxonomic matching packages
install_github("LimaRAF/plantR")
library("plantR")


# Reading in data
oegres_dm = read_excel("data/oegres_DM/oegres_DM.xlsx", sheet = "data_detailed")
oegres_ta = read_excel("data/oegres_TA/oegres_TA.xlsx", sheet = "data_detailed")
oegres_mn = read_excel("data/oegres_MN/oegres_MN.xlsx", sheet = "data_detailed")
oegres_sc = read_excel("data/oegres_SC/oegres_SC.xlsx", sheet = "data_detailed")
oegres_gg = read_excel("data/oegres_GG/oegres_GG.xlsx", sheet = "data_detailed")
oegres_az = read_excel("data/oegres_AZ/oegres_AZ.xlsx", sheet = "data_detailed")
oegres_hhn = read_excel("data/oegres_HHN/oegres_HHN.xlsx", sheet = "data_detailed")
oegres_dl = read_excel("data/oegres_DL.xlsx", sheet = "data_detailed")

# changing name of columns to merge
colnames(oegres_ta)[45] <- "Notes"
colnames(oegres_sc)[45] <- "Notes"
colnames(oegres_sc)[43] <- "germ.tim.zero"
colnames(oegres_dl)[43] <- "germ.tim.zero"

# reorder columns
colnames_order <- colnames(oegres_dm)
oegres_ta <- oegres_ta[, colnames_order]
oegres_sc <- oegres_sc[, colnames_order]
oegres_dl <- oegres_dl[, colnames_order]

# merge all dataframes  
oegres_full <- do.call(rbind, mget(ls(pattern="oegres_")))

# match species names to a uniform taxonomic representation
all_names <- data.frame(oegres_full[,c("genus", "species", "variety")])

# probably won't use the following code
# all_names$scientificName <- unlist(lapply(1:nrow(all_names), function(x) {
#   if (all_names[x,"variety"] != "NA" & !is.na(all_names[x,"variety"])) {
#     return(paste0(all_names[x,"genus"], " ", all_names[x,"species"], " ", all_names[x,"variety"]))
#   } else {
#     return(paste0(all_names[x,"genus"], " ", all_names[x,"species"]))
#   }
# }))

all_names$scientificName = paste0(all_names$genus, " ", all_names$species)
# plantR
all_names_fix <- fixSpecies(all_names[, "scientificName"])

oegres_full$genus <- gsub(" .*$", "", all_names_fix$scientificName.new)
oegres_full$species <- gsub("^\\S+ ", "", all_names_fix$scientificName.new)

# this one is not used as well
# new_names <- strsplit(all_names_fix$scientificName.new," ")
# oegres_full[, c("genus", "species", "variety")] <- lapply(1:length(new_names), function(i) {
#   if (length(new_names[[i]]) >= 3) {
#     var <- paste0(new_names[[i]][3:length(new_names[[i]])], collapse = " ")
#     return(c(new_names[[i]][1], new_names[[i]][2], var))
#   } else {
#     return(c(new_names[[i]][1], new_names[[i]][2]))
#   }
# })

# what varieties exist
varieties_non_null <- oegres_full[oegres_full$variety != "NA" & !is.na(oegres_full$variety),]

# # of species
species_len <- nrow(unique(oegres_full[, c("genus", "species")]))
studies_len <- nrow(unique(oegres_full[, "datasetID"]))


