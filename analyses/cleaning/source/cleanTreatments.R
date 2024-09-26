## 26 September 2024 ##
# By Christophe

# Code to clean the treatment categories 

# Create new column of treatment category of cleaned treatments
d$treatment <- d$treatmentCat ### could potentially change that name

# Vector of all unique treatment names
d.treatunique <- d[!duplicated(d$treatment), ]


d$source.population.edit[which(d$datasetID == "Bhatt00" & d$other.treatment == "Kalika - population")] <- "Kalika, Kumaun, Himalaya"