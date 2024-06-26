# Started July 26, 2022 by Deirdre

# Aim of this code is to seperate out the monocots from the dicots

rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)

library(Taxonstand)
library(taxize)
library(stringr)

if(length(grep("deirdreloughnan", getwd())>0)) {
  setwd("~/Documents/github/oegres")
} else if(length(grep("lizzie", getwd())>0)) {
  setwd("~/Documents/git/projects/oegres/")
} else if(length(grep("buonaiuto", getwd())>0)) {
  setwd("~/git/oegres/")
} else{
  setwd("~/deirdre/oegres") # for midge
}

pap <- read.csv("data/oegres_fullsearch.csv") 

head(strsplit(pap$Authors, c(";")))

#pap$studyID <- as.numeric(unlist(lapply(strsplit(pap$Authors, split = c(";", ",", " "), "[", 2))))

pap$first <- unlist(lapply( strsplit(pap$Authors, split= c(";")), "[", 1))
strsplit(pap$first, c(","))

pap$firstTemp <- unlist(lapply( strsplit(pap$first, split= c(",")), "[", 1))
pap$surname <- unlist(lapply( strsplit(pap$firstTemp, split= c(" ")), "[", 1))

pap$surname[pap$surname == "de"] <- "de Casas"
pap$surname[pap$surname == "De"] <- "De Wilde"

pap$surname <- tolower(pap$surname)

yr <- substr(pap$Publication.Year, 3, 4)

pap$studyID <- paste(pap$surname, yr, sep = "")

unique(pap$accept_reject)
papA <- subset(pap, accept_reject != "R")

#remove all non-english papers
unique(pap$language)

lang <- c("French", "Portuguese", "Persian","Italian","Korean", "Spanish", "Not Eng")
papA <- papA[!papA$language %in% lang, ]

#remove crops
unique(pap$crops)
papA <- subset(papA, crops != "C")

head(pap)

sort(unique(papA$species))

papA$species[papA$species == "vinifera×V. amurensis"] <- "vinifera"
papA$species[papA$species == "alterniflora Loisel"] <- "alterniflora"
papA$species[papA$species == "dichotomiflorum Michx."] <- "dichotomiflorum"
papA$species[papA$species == "dioicus Fernald"] <- "dioicus"
papA$species[papA$species == "discolor Hemsley"] <- "discolor"

papA$genus[papA$genus == "malus"] <- "Malus"

papA$genus[papA$genus == "Multiple"] <- "multiple"
papA$genus[papA$genus == "Multple"] <- "multiple"
papA$genus[papA$genus == "Mutiple"] <- "multiple"

papA$species.name <- paste(papA$genus, papA$species, sep = " ")

papA$genus[papA$genus == "mutiple multiple"] <- "multiple"
papA$genus[papA$genus == "mutiple spp."] <- "multiple"

papA$species.name <- trimws(papA$species.name)

sort(unique(papA$genus))
sort(unique(papA$species.name))

papNames <- papA[, c("genus","species.name")] 
sort(unique(papNames$genus))

#multiple <- c("multiple", "Multiple", "Multple","Mutiple", "")
#papNames <- papNames[!papNames$genus %in% multiple, ]

#sp.class.clean.ncbi <- tax_name(sort(unique(papA$genus)), get = c("genus","family","order","class","phylum","kingdom", "division"), db = "ncbi")
#sp.class.clean.itis <- tax_name("Iris", get = c("genus","family","order","class","phylum","kingdom", "division", "clade"), db = "ncbi")

# write.csv(sp.class.clean, "data/taxonomicInfoNCBI.csv", row.names = F)
# write.csv(sp.class.clean.itis, "data/taxonomicInfoITIS.csv", row.names = F)

sp.class.clean <- read.csv("data/taxonomicInfoNCBI.csv")

papTaxa <- merge(papA, sp.class.clean, by = "genus")

#Not found:
# Idesia polycarpa 
# Machilus thunbergii
# Olea
# Paris

monocots <- c("Poaceae", "Orchidaceae", "Liliaceae", "Arecaceae", "Iridaceae","Acoraceae", "Burmanniaceae","Cyclanthaceae","Dioscoreaceae","Nartheciaceae","Pandanaceae","Petrosaviaceae","Stemonaceae","Taccaceae","Velloziaceae") 

cot.order<-c("Acora les","Alismatales", "Asparagales",
             "Dioscoreales", "Liliales","Pandanales","Petrosaviales", "Arecales",
             "Commelinales","Poales","Zingiberales")


eudicot <- papTaxa[!papTaxa$family %in% monocots, ]
eudicot2 <- papTaxa[!papTaxa$order %in% cot.order, ]
monocots <- papTaxa[papTaxa$family %in% monocots, ]
monocots2 <- papTaxa[papTaxa$order %in% cot.order, ]

length(unique(eudicot2$species.name))
length(unique(eudicot2$studyID))

eudicot2Sub <- eudicot2[, c( "studyID", "Publication.Type", "Authors", "Article.Title",   
                             "Source.Title", "Volume", "Issue", "Start.Page",  
                             "Publication.Year", "species", "accept_reject", "reason_reject",
                             "language", "available", "paper_pulled", "crops",           
                             "notes", "checkedby" )]
eudicot2Sub$ subset <- "eudicot"

oegres <- read.csv("data/oegres.csv")
oegres$subset <- "old"


names(eudicot2Sub)
names(oegres)

newSub <- merge(oegres, eudicot2Sub, 
              by = c("studyID", "Publication.Type", "Authors", "Article.Title",   
                      "Source.Title", "Volume", "Issue", "Start.Page",  
                      "Publication.Year", "species", "accept_reject", "reason_reject",
                      "language", "available", "paper_pulled", "crops", "notes", "checkedby"), all.y = T)

write.csv(newSub, "data/oegresEudicot.csv", row.names = F)

