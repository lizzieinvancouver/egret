# Housekeeping
rm(list = ls())
setwd("/Users/dindin/Documents/work/oegres_scraping/oegres")

# 
install.packages("ggplot2")
install.packages("devtools")
install.packages("readxl")
install.packages("ggforce")
library("ggplot2")
library("readxl")
library("devtools")
library("ggforce")

# trying out taxonomic matching packages
install_github("LimaRAF/plantR")
library("plantR")

# remove scientific notation
options(scipen = 999)

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

# response to numeric values
oegres_full$response <- as.numeric(oegres_full$response)

# code that identifies the studies with data beyond the 95th percentile 
# and data with the lowest 5th percentile for each response variable

# what unique response variables are there?
respvar_groups <- unique(oegres_full$respvar)

beyond_95 <- lapply(X = respvar_groups, FUN = function(x) {
  df <- oegres_full[oegres_full$respvar == x, ]
  qnt <- quantile(df$response, c(.95), na.rm = TRUE)
  df[df$response > qnt["95%"],]
})
beyond_95 <- do.call(rbind, beyond_95)
# what's the mean response value per respvar in the study?
study_beyond95 <- aggregate(beyond_95$response, by = list(beyond_95$respvar, beyond_95$datasetID), FUN = mean, na.action="pass")
colnames(study_beyond95)[3] <- "mean_response"

below_5 <- lapply(X = respvar_groups, FUN = function(x) {
  df <- oegres_full[oegres_full$respvar == x, ]
  qnt <- quantile(df$response, c(.05), na.rm = TRUE)
  df[df$response < qnt["5%"],]
})

below_5 <- do.call(rbind, below_5)
study_below5 <- aggregate(below_5$response, by = list(below_5$respvar, below_5$datasetID), FUN = mean, na.action="pass")
colnames(study_below5)[3] <- "mean_response"



# ------------------------------------------------ #
# PLOTS
# Figures of the responses variables - with the respvar on the y-axis and the raw data across all studies 
# Response vs Chemicals (content in ppm)
oegres_chem <- oegres_full[oegres_full$chemical != "NA" & !is.na(oegres_full$chemical), ]
oegres_chem$chemcial.concent <- as.numeric(oegres_chem$chemcial.concent)
oegres_chem <- oegres_chem[!is.na(oegres_chem$response) & !is.na(oegres_chem$chemcial.concent),]

chem_resp <- ggplot(oegres_chem, mapping = aes(x = chemcial.concent, y = response)) +
  geom_point() +
  facet_wrap(~respvar, scales ="free") +
  labs(x = "Chemical content in ppm", y = "Response")

pdf("cleaning/preliminary_DM/ChemicalsVsResponse.pdf", height = 15, width = 20)

for (i in 1:2) {
  print(p_save <- chem_resp +
          facet_wrap_paginate(~respvar, nrow = 4, ncol = 5, page = i, scales ="free"))
}
dev.off()

# Response vs germination temperature
# will introduce NA for those values that can't be converted
oegres_germ <- oegres_full[oegres_full$germ.temp != "NA" & !is.na(oegres_full$germ.temp), ]
oegres_germ$germ.temp <- as.numeric(oegres_germ$germ.temp)
# remove records with temperature above 4000 to make plots readable since they are wrong anyway
oegres_germ <- oegres_germ[oegres_germ$germ.temp < 4000, ] 
oegres_germ <- oegres_germ[!is.na(oegres_germ$germ.temp) & !is.na(oegres_germ$response),]

temp_resp <- ggplot(oegres_germ, mapping = aes(x = germ.temp, y = response)) +
  geom_point() +
  facet_wrap(~respvar, scales ="free") +
  labs(x = "Temperature in °C", y = "Response")


pdf("cleaning/preliminary_DM/TemperatureVsResponse.pdf", height = 15, width = 20)
for (i in 1:3) {
  print(p_save <- temp_resp +
          facet_wrap_paginate(~respvar, nrow = 4, ncol = 4, page = i, scales ="free"))
}
dev.off()

# Response vs Chilling Temperature
oegres_chill <- oegres_full[oegres_full$chill.temp != "NA"
                            & !is.na(oegres_full$chill.temp), ]
oegres_chill$chill.temp <- as.numeric(oegres_chill$chill.temp)
oegres_chill <- oegres_chill[!is.na(oegres_chill$chill.temp) & !is.na(oegres_chill$response),]

chill_temp_resp <- ggplot(oegres_chill, mapping = aes(x = chill.temp, y = response)) +
  geom_point() +
  facet_wrap(~respvar, scales ="free") +
  labs(x = "Chilling Temperature in °C", y = "Response")


pdf("cleaning/preliminary_DM/ChillingTemperatureVsResponse.pdf", height = 15, width = 20)
for (i in 1:3) {
  print(p_save <- chill_temp_resp +
          facet_wrap_paginate(~respvar, nrow = 4, ncol = 4, page = i, scales ="free"))
}
dev.off()

# Response vs Storage Temperature
oegres_store <- oegres_full[oegres_full$storage.temp != "NA" & !is.na(oegres_full$storage.temp), ]
oegres_store$storage.temp <- as.numeric(oegres_store$storage.temp)
oegres_store <- oegres_store[!is.na(oegres_store$storage.temp) & !is.na(oegres_store$response),]

storage_temp_resp <- ggplot(oegres_store, mapping = aes(x = storage.temp, y = response)) +
  geom_point() +
  facet_wrap(~respvar, scales ="free") +
  labs(x = "Storage Temperature in °C", y = "Response")


pdf("cleaning/preliminary_DM/StorageTemperatureVsResponse.pdf", height = 15, width = 20)
for (i in 1:2) {
  print(p_save <- storage_temp_resp +
          facet_wrap_paginate(~respvar, nrow = 4, ncol = 4, page = i, scales ="free"))
}
dev.off()
