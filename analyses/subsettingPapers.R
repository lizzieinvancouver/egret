# Started June 13, 2022 by Deirdre

# Aim of this code is to subset the papers that we accepted for the seed germination meta-analysis
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)

if(length(grep("deirdreloughnan", getwd())>0)) {
  setwd("~/Documents/github/oegres")
} else if(length(grep("lizzie", getwd())>0)) {
  setwd("~/Documents/git/projects/oegres/")
} else{
  setwd("~/deirdre/Synchrony") # for midge
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

head(pap$studyID)

# Remove rejected papers
unique(pap$accept_reject)
papA <- subset(pap, accept_reject != "R")

#remove all non-english papers
unique(pap$language)

lang <- c("French", "Portuguese", "Persian","Italian","Korean", "Spanish")
papA <- papA[!papA$language %in% lang, ]

#remove crops
unique(pap$crops)
papA <- subset(papA, crops != "C")

# sampling without replacement
toReview <- sample(x = papA$studyID, size = 100, replace = FALSE) #

papers <- papA[papA$studyID %in% toReview, ]

#sometimes there are duplicates - but I only one from a given author
papers$dup <- duplicated(papers[,c("studyID")])
papersDup <- subset(papers, dup != "TRUE") 

write.csv(papersDup, "papersToReview2.csv", row.names = T)
# # split up an entry based on a character/symbol/numer, example with hypen:
# strsplit(df$Sample.Description, "-")
# # then make a new column...
# d2$flaskID <- as.numeric(unlist(lapply( strsplit(d2$Flask_code, split="-"), "[", 2)))
# # how to grab a count of digits ...
# substr(dates, 1, 4)
# # and make new columns ... here break by a period
# breakbyperiod <- strsplit(as.character(df$key), ".", fixed=TRUE) # fixed=TRUE means you don't have to escape the period
# df$var <- unlist(lapply(breakbyperiod, function(x) x[1]))
# df$sp <- unlist(lapply(breakbyperiod, function(x) x[2]))
