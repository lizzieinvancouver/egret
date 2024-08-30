# Started on 30 August 2024
# By Christophe

# goal of this scrip is to check how many studies have more than one provenance

d <- read.csv("output/egretclean.csv")

# visualize data 
str(d)
dcut <- d[, c("datasetID","provLatLon")]
head(dcut)
dcut2 <- dcut[1:300,]
dput(dcut2)

col2check <- c("datasetID","provLatLon", "provLatLonAlt")

unique.studies <- unique(d$datasetID)

unique.provenances <- unique(d$provLatLon)
# fill the right ncol and nrow
multipleprovenances <- data.frame(matrix(ncol = length(col2check), nrow=length(unique.studies)))
# set colnames
names(multipleprovenances) <- col2check
# add datasetIDstudy names in the first columns
multipleprovenances$datasetID <- unique.studies

# loop to count unique treatments in each column
# currently treats NAs as a treatment (NAs are not deleted out)

for (i in c(1:length(unique.provenances))) { # i = 1
  subby <- d[which(d$provLatLon == unique.provenances[i]),]
  for(j in c(1:length(col2check))) { # j = 2
    provenances <- length(unique(subby[, c(col2check[j])]))
    multipleprovenances[i, j +1 ] <- unique.provenances
  }
} 


# how many provenances per study
provenance_count <- aggregate(provLatLon ~ datasetID, data = d, function(x) length(unique(x)))

# how many have more than one provenances
suby <- subset(provenance_count, provLatLon > 1)

hist(suby$provLatLon)
count <- ggplot(suby, aes(x = provLatLon)) +
  geom_histogram(binwidth = 1, color = "black", fill = "blue") +
  theme_minimal()
list.files()
# Print the new dataframe


