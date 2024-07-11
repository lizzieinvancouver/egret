# Started July 10, 2024 by mao

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

library(ggplot2)
# Get the datasets
egret <- read.csv("output/egretclean.csv")

ospree <- read.csv("input/ospree_clean_withchill.csv")
ospree$latbi <- paste(ospree$genus, ospree$species, sep = "_")

USDA <- read.csv("scrapeUSDAseedmanual/output/usdaGerminationData.csv")
USDA$latbi <- paste(USDA$genus, USDA$species, sep = "_")

bb <- read.csv("output/baskinclean.csv", header =T) 
bb$latbi <- bb$matched_name2
bb$latbi <- sub(" ", "_", bb$latbi)
bb <- select(bb, c('latbi','Dormancy.Class'))
bb <- unique(bb)
# Combine egret and USDA
egretusda <- data.frame(latbi = c(egret$latbi, USDA$latbi))
egretusda <- unique(egretusda)
# Adding dormancy class to egret + USDA
eu_dorm <- merge(egretusda, bb, by.x = "latbi", by.y = "latbi", all.x = T)

freq <- table(eu_dorm$Dormancy.Class)

text(x = barplot(freq, 
                 main = "Dormancy Class for Egret + USDA", 
                 xlab = "Dormancy class", 
                 ylab = "Frequency",
                 col = "Black",  # Bar color
                 ylim = c(0, max(freq) + 50)),
     y = freq + 0.5,  
     labels = freq,
     pos = 3)

# Overlapping between OSPREE (EGRET + USDA)
intersect_o_eu <- intersect(ospree$latbi, egretusda$latbi)
oeu <- data.frame(intersect_o_eu)

# Adding dormancy class
oeu_dorm <- merge(oeu, bb, by.x = "intersect_o_eu", by.y = "latbi", all.x = T)
freq <- table(oeu_dorm$Dormancy.Class)

text(x = barplot(freq, 
                 main = "Dormancy Class for OSPREE x Egret + USDA", 
                 xlab = "Dormancy class", 
                 ylab = "Frequency",
                 col = "Black",  # Bar color
                 ylim = c(0, max(freq) + 10)),
     y = freq + 0.5,  
     labels = freq,
     pos = 3)

# Adding dormancy class to egret
egretSp <- data.frame(unique(egret$latbi))
e_dorm <- merge(egretSp, bb, by.x = "unique.egret.latbi.", by.y = "latbi", all.x = T)

freq <- table(e_dorm$Dormancy.Class)

text(x = barplot(freq, 
                 main = "Dormancy Class for Egret", 
                 xlab = "Dormancy class", 
                 ylab = "Frequency",
                 col = "Black",  # Bar color
                 ylim = c(0, max(freq) + 50)),
     y = freq + 0.5,  
     labels = freq,
     pos = 3)

# Adding dormancy class to USDA
USDASp <- data.frame(unique(USDA$latbi))
u_dorm <- merge(USDASp, bb, by.x = "unique.USDA.latbi.", by.y = "latbi", all.x = T)
freq <- table(u_dorm$Dormancy.Class)

text(x = barplot(freq, 
                 main = "Dormancy Class for USDA", 
                 xlab = "Dormancy class", 
                 ylab = "Frequency",
                 col = "Black",  # Bar color
                 ylim = c(0, max(freq) + 50)),
     y = freq + 0.5,  
     labels = freq,
     pos = 3)
