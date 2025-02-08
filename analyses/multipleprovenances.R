# Started on 30 August 2024
# By Christophe

# goal of this scrip is to check how many studies have more than one provenance
# Package
library(ggplot2)


# how many provenances per study
provenance_count <- aggregate(provLatLon  ~ datasetID, data = d, function(x) length(unique(x)))
head(provenance_count)
# how many have more than one provenances
suby <- subset(provenance_count, provLatLon > 1)
head(suby)
length(rownames(suby))
# vector of datasetID with multiple provenances
vec <- suby$datasetID
# subset datasetID with multiple provenances
dformap <- subset(d, datasetID == vec)

# plotting the number of studies with more than 1 provenance
count <- ggplot(suby, aes(x = provLatLon)) +
  geom_histogram(binwidth = 1, color = "black", fill = "blue") +
  theme_minimal()
count
