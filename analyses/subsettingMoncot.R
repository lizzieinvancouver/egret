# Started July 26, 2022 by Deirdre

# Aim of this code is to seperate out the monocots from the dicots

rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)

library(Taxonstand)
library(taxize)

if(length(grep("deirdreloughnan", getwd())>0)) {
  setwd("~/Documents/github/oegres")
} else if(length(grep("lizzie", getwd())>0)) {
  setwd("~/Documents/git/projects/oegres/")
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

papA$species.name <- paste(papA$genus, papA$species, sep = " ")

sort(unique(papA$genus))

papNames <- papA[, c("genus","species.name")] 
sort(unique(papNames$genus))

multiple <- c("multiple", "Multiple", "Multple","Mutiple", "")
papNames <- papNames[!papNames$genus %in% multiple, ]

#sp.class.clean.ncbi <- tax_name(sort(unique(papA$genus)), get = c("genus","family","order","class","phylum","kingdom", "division"), db = "ncbi")
#sp.class.clean.itis <- tax_name("Iris", get = c("genus","family","order","class","phylum","kingdom", "division", "clade"), db = "ncbi")

2# write.csv(sp.class.clean, "data/taxonomicInfoNCBI.csv", row.names = F)
# write.csv(sp.class.clean.itis, "data/taxonomicInfoITIS.csv", row.names = F)

sp.class.clean <- read.csv("data/taxonomicInfoNCBI.csv")

papTaxa <- merge(papA, sp.class.clean, by = "genus")

#Not found:
# Idesia polycarpa 
# Machilus thunbergii
# Olea
# Paris

monocots <- c("Poaceae", "Orchidaceae", "Liliaceae", "Arecaceae", "Iridaceae","Acoraceae", "Burmanniaceae","Cyclanthaceae","Dioscoreaceae","Nartheciaceae","Pandanaceae","Petrosaviaceae","Stemonaceae","Taccaceae","Velloziaceae") 

eudicot <- papTaxa[!papTaxa$family %in% monocots, ]
monocots <- papTaxa[papTaxa$family %in% monocots, ]

names <- sort(unique(papNames$genus))

res = lapply(names, function(w) {
  Sys.sleep(8) # sleep for a second, possibly less to avoid rate limit
  get_uid(sci_com = w, division_filter = "eudicots")
})

class(res)
str(res)

resAttr <- lapply(res, attributes)
eudicotFiltered <- as.data.frame(do.call(rbind, resAttr))
eudicotFiltered$genus <- names
t <- data.frame(eudicotFiltered)
write.csv(t, "data/eudicotFiltered.csv", row.names = F)

resm = lapply(names[1:20], function(w) {
  Sys.sleep(8) # sleep for a second, possibly less to avoid rate limit
  get_uid(sci_com = w, division_filter = "monocot")
})


capture.output(summary(eudicotFiltered), file = "data/eudicotFiltered.csv")
apply(eudicotFiltered, function(x) write.table( data.frame(x), 'data/eudicotFiltered.csv'  , append= T, sep=',' ))

sink("data/eudicotFiltered.csv")
print(eudicotFiltered)
sink()
