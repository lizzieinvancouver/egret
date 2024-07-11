## Started 8 July 2024 by Mao##


library("taxize")
library("stringr")


setwd("C:/PhD/Project/egret/analyses")
# General chekcs:
#1. fix typos and minor issues:
### Clean Species ##############################
baskin <- read.csv("input/Baskin_Dormancy_Database.csv", skip=2) 
baskin$X1 <- NULL
# Substitute the underscore with space
baskin$Genus_species <- sub("_", " ", baskin$Genus_species)
# Use taxize package to inspect whether names are correct
ref <- gnr_datasources() # Full list of databases available

# Couldn't get it working if use the whole column. 1. There might be some problematic rows, 2. The dataframe is too big.
# Find problematic rows
# baskinnew <- baskin$Genus_species[1:10]
# fix_names <- gnr_resolve(sci = baskinnew, with_canonical_ranks = T)
# Repeat this until you run all the rows.
# Get rid of problematic rows
baskinnew <- baskin[baskin$Genus_species != c("Crataegus x sinaica","Manilkara zapota","Ulmus minor"), ]

# Break down the big dataframe into smaller sections: 1:3000, 3001:6000, 6001:9000, 9001:12000, 12001:14250
baskin3000<-baskinnew$Genus_species[12001:14250]
fix_names <- gnr_resolve(sci = baskin3000, with_canonical_ranks = T)
baskin_species_fix <- unique(fix_names$matched_name2)
baskinnewsub <- baskinnew[c(12001:14250), ]
unique_fix_names <- fix_names[!duplicated(fix_names$submitted_name), ]
unique_baskin <- baskinnewsub[!duplicated(baskinnewsub$Genus_species), ]

# Merge the fix_names with original dataframe
baskin_3000 <- merge(x = unique_baskin, y = unique_fix_names[ , c("submitted_name", "matched_name2")], by.x = "Genus_species", by.y = "submitted_name", all.x=TRUE)
baskin_6000 <- merge(x = unique_baskin, y = unique_fix_names[ , c("submitted_name", "matched_name2")], by.x = "Genus_species", by.y = "submitted_name", all.x=TRUE)
baskin_9000 <- merge(x = unique_baskin, y = unique_fix_names[ , c("submitted_name", "matched_name2")], by.x = "Genus_species", by.y = "submitted_name", all.x=TRUE)
baskin_12000 <- merge(x = unique_baskin, y = unique_fix_names[ , c("submitted_name", "matched_name2")], by.x = "Genus_species", by.y = "submitted_name", all.x=TRUE)
baskin_14250 <- merge(x = unique_baskin, y = unique_fix_names[ , c("submitted_name", "matched_name2")], by.x = "Genus_species", by.y = "submitted_name", all.x=TRUE)
# Bind them together
baskin_fix <- rbind(baskin_3000, baskin_6000, baskin_9000, baskin_12000, baskin_14250)

# Merge with original baskin dataframe
baskin_fix_final <- merge(x = baskin, y = baskin_fix[ , c("Genus_species", "matched_name2")], by= "Genus_species", all.x=TRUE)

# Remove all duplicated rows based on sp and dormclass
baskin_fix_final <- baskin_fix_final[!duplicated(baskin_fix_final[c("Genus_species","Dormancy.Class")]),]
library(dplyr)
baskin_clean <- baskin_fix_final %>% 
  mutate(matched_name2 = coalesce(matched_name2, Genus_species))

baskin <- select(baskin_clean, c('matched_name2','Original.Genus','Corrected.Genus','Family','Dormancy.Class'))
colnames(baskin)<- c("latbi","Original.Genus","Corrected.Genus","Family","Dormancy.Class")
baskin$latbi <- sub(" ", "_", baskin$latbi)

baskin <- unique(baskin)
# Save the final dataframe with matched names
write.csv(baskin,"output/baskinclean.csv")

