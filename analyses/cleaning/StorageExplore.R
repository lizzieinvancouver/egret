# started Jul 11, 2024 by Fredi

# aim is explore storage as a potential driver of seed germination
# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

if(length(grep("deirdreloughnan", getwd()) > 0)) {
  setwd("~/Documents/github/egret/analyses")
} else if(length(grep("christophe_rouleau-desrochers", getwd())) > 0){
  setwd("/Users/christophe_rouleau-desrochers/Documents/github/egret/analyses")
} else if(length(grep("frederik", getwd())) > 0){
  setwd("/Users/frederik/github/egret/analyses")
}

egret <- read.csv("./output/egretclean.csv", sep=",", header=TRUE)
usda <- read.csv("./scrapeUSDAseedmanual/output/usdaGerminationData.csv", sep=",", header=TRUE)




# In egret
str(egret)
length(egret[!is.na(egret$storageType),"datasetID"])
egret$storageType
#### explore storage types and how many spp per storage temp and duration
sppwith_storagetime <- egret$latbi[which(!is.na(egret$storage.time))]
unique(sppwith_storagetime) #these spp have a storage time

sppwith_storagetemp <- egret$latbi[which(!is.na(egret$storage.temp))]
unique(sppwith_storagetemp) #these spp have a storage temp

sppwith_both <- sppwith_storagetime[which(sppwith_storagetime %in% sppwith_storagetemp)]
unique(sppwith_both) #these spp have both storage time and temp -> great 149 species have that criteria

tmp_spp <- aggregate(latbi ~ storageType, data = egret, FUN = function(x) length(unique(x, na.rm=TRUE)))
spp_na <- unique(egret[is.na(egret$storageType),"latbi"])#spp with no storage type (NA)


#make a new datafram out of tmp_spp
tmp_spp <- as.data.frame(tmp_spp)
tmp_spp <- rbind(tmp_spp, c("NA", as.numeric(length(spp_na))))
#sort the table by decreasing latbi
tmp_spp <- tmp_spp[order(as.numeric(tmp_spp$latbi), decreasing = TRUE),]

#fix the order of levels inside "storageType" in "tmp_spp"
tmp_spp$storageType <- factor(tmp_spp$storageType, levels = tmp_spp$storageType)


library(graphics)
#make a barplot with levels of "storageType" on the x-axis and number of unique spec on the y-axis based on the table tmp_spp. dont use ggplot
tmp_spp <- as.data.frame(tmp_spp)

library(ggplot2)
#need to fix this, plot is strange
ggplot(tmp_spp, aes(x = storageType, y = latbi, fill = storageType)) +
    geom_bar(stat = "identity")


#how many "latid" do we have in each level of "storage_Type"
tapply(egret$latbi, egret$storageType, function(x) length(unique(x)))
tapply(egret$datasetIDstudy, egret$storageType, function(x) length(unique(x)))

tmp_spp <- aggregate(latbi ~ storageType, data = egret, FUN = function(x) length(unique(x)))
tmp_stud <- aggregate(datasetIDstudy ~ storageType, data = egret, FUN = function(x) length(unique(x)))

#aggregate(latbi ~ scarifTypeGen, data = egret, FUN = function(x) length(unique(x)))
#aggregate(datasetIDstudy ~ scarifTypeGen, data = egret, FUN = function(x) length(unique(x)))


ggplot(tmp_spp, aes(x = storageType, y = latbi, fill = storageType)) +
    geom_bar(stat = "identity", position = "dodge")




#in usda
#
#make a new column "spec" to combine genus and species
usda$latbi <- paste(usda$genus, usda$species, sep = "_")
summary(is.na(usda$spec))

#these are the interesting exploratory variables
names(usda)
head(usda)
usda$scarifTypeGen

#explore scarification types
tmp_spp <- aggregate(latbi ~ scarifTypeGen, data = usda, FUN = function(x) length(unique(x)))
spp_na <- unique(usda[is.na(usda$scarifTypeGen),"latbi"])#spp with no storage type (NA)
tmp_spp <- rbind(tmp_spp, c("NA", as.numeric(length(spp_na))))

#make a barplot with levels of "scarifTypeGen" on the x-axis and number of unique spec on the y-axis
ggplot(tmp_spp, aes(x = scarifTypeGen, y = latbi, fill = scarifTypeGen)) +
    geom_bar(stat = "identity", position = "dodge")



tapply(usda$latbi, usda$chill.dur.Max, function(x) length(unique(x)))
tapply(usda$latbi, usda$chill.dur.Max, function(x) (unique(x)))

aggregate(latbi ~ chill.dur.Max, data = usda, FUN = function(x) length(unique(x)))

length(unique(usda[!is.na(usda$chill.dur.Max), "latbi"])) #57 spp have a value in chill.dur.Max