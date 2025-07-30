# Started July 10, 2024 by mao

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)
library(dplyr)
library(ggplot2)
# Get the datasets

baskegret <- read.csv("output/baskinegretclean.csv")
baskegret <- baskegret[, c("Genus_species","Dormancy.Class")]

baskusda <- read.csv("output/baskinusdaclean.csv")
baskusda <- baskusda[, c("Genus_species","Dormancy.Class")]

egret<-read.csv("output/egretclean.csv")
egret$Genus_species <- paste(egret$genus, egret$species, sep = " ")

ospree <- read.csv("output/ospreeEgretCleaned.csv")
ospree$Genus_species <- paste(ospree$genus, ospree$species, sep = " ")

USDA <- read.csv("output/usdaGerminationCleaned.csv")
USDA$Genus_species <- paste(USDA$genus, USDA$species, sep = " ")

egret <- merge(egret,baskegret,by = "Genus_species", all.x = TRUE)
egret <- egret[, c("Genus_species","Dormancy.Class")]
egret <- filter(egret,!is.na(`Dormancy.Class`))
egret <- unique(egret)

freq <- table(egret$Dormancy.Class)

text(x = barplot(freq, 
                 main = "Dormancy Class for Egret", 
                 xlab = "Dormancy class", 
                 ylab = "Frequency",
                 col = "Black",
                 ylim = c(0, max(freq) + 50)),
     y = freq + 0.5,  
     labels = freq,
     pos = 3)

species_dormancy_table <- table(egret$Genus_species, egret$Dormancy.Class)

# Find species with more than one dormancy class
species_with_multiple_classes <- rownames(species_dormancy_table)[rowSums(species_dormancy_table > 0) > 1]

egret_subset <- egret[egret$Genus_species %in% species_with_multiple_classes, ]

egret_subset <- aggregate(Dormancy.Class~ Genus_species, data = egret_subset, FUN = function(x) paste(unique(x), collapse = ", "))
egret_subset$Dormancy.Class[which(egret_subset$Dormancy.Class == "PD, ND")] <- "ND, PD"

freq <- table(egret_subset$Dormancy.Class)

text(x = barplot(freq, 
                 main = "Species with Multiple Dormancy Class for Egret", 
                 xlab = "Dormancy class", 
                 ylab = "Frequency",
                 col = "Black",  # Bar color
                 ylim = c(0, max(freq) + 10)),
     y = freq + 0.5,  
     labels = freq,
     pos = 3)


USDA <- merge(USDA, baskusda, by = "Genus_species", all.x = TRUE)
USDA <- USDA[, c("Genus_species","Dormancy.Class")]
USDA <- filter(USDA,!is.na(`Dormancy.Class`))
USDA <- unique(USDA)


freq <- table(USDA$Dormancy.Class)

text(x = barplot(freq, 
                 main = "Dormancy Class for USDA", 
                 xlab = "Dormancy class", 
                 ylab = "Frequency",
                 col = "Black",  # Bar color
                 ylim = c(0, max(freq) + 50)),
     y = freq + 0.5,  
     labels = freq,
     pos = 3)

species_dormancy_table <- table(USDA$Genus_species, USDA$Dormancy.Class)

# Find species with more than one dormancy class
species_with_multiple_classes <- rownames(species_dormancy_table)[rowSums(species_dormancy_table > 0) > 1]

USDA_subset <- USDA[USDA$Genus_species %in% species_with_multiple_classes, ]

USDA_subset <- aggregate(Dormancy.Class~ Genus_species, data = USDA_subset, FUN = function(x) paste(unique(x), collapse = ", "))
USDA_subset$Dormancy.Class[which(USDA_subset$Dormancy.Class == "PD, ND")] <- "ND, PD"

freq <- table(USDA_subset$Dormancy.Class)

text(x = barplot(freq, 
                 main = "Species with Multiple Dormancy Class for USDA", 
                 xlab = "Dormancy class", 
                 ylab = "Frequency",
                 col = "Black",  # Bar color
                 ylim = c(0, max(freq) + 5)),
     y = freq + 0.5,  
     labels = freq,
     pos = 3)


# Combine egret and USDA
egretusda <- rbind(egret,USDA)
egretusda <- unique(egretusda)

freq <- table(egretusda$Dormancy.Class)

text(x = barplot(freq, 
                 main = "Dormancy Class for Egret + USDA", 
                 xlab = "Dormancy class", 
                 ylab = "Frequency",
                 col = "Black",  # Bar color
                 ylim = c(0, max(freq) + 50)),
     y = freq + 0.5,  
     labels = freq,
     pos = 3)

species_dormancy_table <- table(egretusda$Genus_species, egretusda$Dormancy.Class)

# Find species with more than one dormancy class
species_with_multiple_classes <- rownames(species_dormancy_table)[rowSums(species_dormancy_table > 0) > 1]

egretusda_subset <- egretusda[egretusda$Genus_species %in% species_with_multiple_classes, ]

egretusda_subset <- aggregate(Dormancy.Class~ Genus_species, data = egretusda_subset, FUN = function(x) paste(unique(x), collapse = ", "))
egretusda_subset$Dormancy.Class[which(egretusda_subset$Dormancy.Class == "PD, ND")] <- "ND, PD"
egretusda_subset$Dormancy.Class[which(egretusda_subset$Dormancy.Class == "PY, PYPD")] <- "PYPD, PY"

freq <- table(egretusda_subset$Dormancy.Class)

text(x = barplot(freq, 
                 main = "Species with Multiple Dormancy Class for Egret + USDA", 
                 xlab = "Dormancy class", 
                 ylab = "Frequency",
                 col = "Black",  # Bar color
                 ylim = c(0, max(freq) + 10)),
     y = freq + 0.5,  
     labels = freq,
     pos = 3)

# Overlapping between OSPREE (EGRET + USDA)
intersect_o_eu <- intersect(ospree$Genus_species, egretusda$Genus_species)
oeu <- data.frame(intersect_o_eu)
oeu$Genus_species <- oeu$intersect_o_eu

baskin <- unique(rbind(baskegret,baskusda))
oeu <- merge(oeu,baskin,by = "Genus_species", all.x = TRUE)

freq <- table(oeu$Dormancy.Class)

text(x = barplot(freq, 
                 main = "Dormancy Class for Egret + USDA x Ospree", 
                 xlab = "Dormancy class", 
                 ylab = "Frequency",
                 col = "Black",  # Bar color
                 ylim = c(0, max(freq) + 10)),
     y = freq + 0.5,  
     labels = freq,
     pos = 3)

species_dormancy_table <- table(oeu$Genus_species, oeu$Dormancy.Class)

# Find species with more than one dormancy class
species_with_multiple_classes <- rownames(species_dormancy_table)[rowSums(species_dormancy_table > 0) > 1]

oeu_subset <- oeu[oeu$Genus_species %in% species_with_multiple_classes, ]

oeu_subset <- aggregate(Dormancy.Class~ Genus_species, data = oeu_subset, FUN = function(x) paste(unique(x), collapse = ", "))
oeu_subset$Dormancy.Class[which(oeu_subset$Dormancy.Class == "PD, ND")] <- "ND, PD"
oeu_subset$Dormancy.Class[which(oeu_subset$Dormancy.Class == "PY, PYPD")] <- "PYPD, PY"

freq <- table(oeu_subset$Dormancy.Class)

text(x = barplot(freq, 
                 main = "Species with Multiple Dormancy Class for Egret + USDA x Ospree", 
                 xlab = "Dormancy class", 
                 ylab = "Frequency",
                 col = "Black",  # Bar color
                 ylim = c(0, max(freq) + 10)),
     y = freq + 0.5,  
     labels = freq,
     pos = 3)
dev.off()
