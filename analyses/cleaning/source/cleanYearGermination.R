## Updated 30 November 2023 by Lizzie and Deirdre ##
## Updated 29 July 2024 by Deirdre

## This contains cleaning of year.germination ##
## Original code taken from file called coordinate_cleaning_JS.R ##

# summary of data scraped:
# data of year germination missing for 163 of the 256 studies; after Julie's cleaning 106
# appears that Julie started to go back and check the manuscripts, but many of the pdf's were missing

# sort(unique(d$year.germination))
temp <- subset(d, is.na(year.germination))

# fix NAs to the correct germ years that I found - JS
nonGiven <- c(
  "ahmad07","amooaghaie09", "ahola99", "alhelal96", "cicek08", "deb17", "dalling99",
  "naumovski05", "nawrot-chorabik21", "nin17","sacande05", "barros12","jacquemart21", 
  "jiro10", "keshtkar08", "ma03", "meyer94", "meyer95", "na11", "onen20", "ordonez-salanueva15", "lee21"
)
temp <- temp[!temp$datasetID %in% nonGiven, ]
length(unique(temp$datasetID))

d$year.germination[which(d$datasetID == "acosta12" & is.na(d$year.germination))] <- "2009"
d$year.germination[which(d$datasetID == "al-absi10" & is.na(d$year.germination))] <- "2004"
d$year.germination[which(d$datasetID == "bhatt00" & is.na(d$year.germination))] <- "1997"
d$year.germination[which(d$datasetID == "batlla03" & is.na(d$year.germination))] <- "2000"
d$year.germination[which(d$datasetID == "chien10" & is.na(d$year.germination))] <- "2008"

d$year.germination[which(d$datasetID == "beikmohammadi12" & is.na(d$year.germination))] <- "2011"
#could not find paper for aldridge1992,93,94,95,etc (not on source list in spreadsheet, don't know paper title) - JS
#amini18 should be on the google drive but I can't currently access it (obtained by ILL I think) - JS
d$year.germination[which(d$datasetID == "chien11" & is.na(d$year.germination))] <- "2011"

for(i in unique(d$year.collected)) {
  d$year.germination[which(d$year.collected == i & is.na(d$year.germination) & d$datasetID == "chien11")] <- i
}

d$year.germination[which(d$datasetID == "cho18" & is.na(d$year.germination))] <- "2012"
d$year.germination[which(d$datasetID == "chuaren04" & is.na(d$year.germination))] <- "2002-2004"
d$year.germination[which(d$datasetID == "conversa09" & is.na(d$year.germination))] <- "2006"
#for crank92, could not find paper on ubc library - JS
d$year.germination[which(d$datasetID == "cuena-lombrana18" & is.na(d$year.germination))] <- "2014"
#couldn't find dehgan84 on ubclibrary - JS
d$year.germination[which(d$datasetID == "cousins10" & is.na(d$year.germination))] <- "2007"
d$year.germination[which(d$datasetID == "necajeva13" & is.na(d$year.germination))] <- "2008"
#could not access naseri18 - JS
d$year.germination[which(d$datasetID == "schutz02" & d$chill.duration == 0)] <- "1998"
d$year.germination[which(d$datasetID == "schutz02" & d$chill.duration != 0)] <- "2000"
# could not find saeed16 paper
d$year.germination[which(d$datasetID == "jabarzare11" & is.na(d$year.germination))] <- "2007"
d$year.germination[which(d$datasetID == "jensen97" & is.na(d$year.germination))] <- "1993/94"
d$year.germination[which(d$datasetID == "jusung16" & is.na(d$year.germination))] <- "2011"
d$year.germination[which(d$datasetID == "kamareh12" & is.na(d$year.germination))] <- "2009"
d$year.germination[which(d$datasetID == "kato11" & is.na(d$year.germination))] <- "2008"
d$year.germination[which(d$datasetID == "kazaz10" & is.na(d$year.germination))] <- "2009"
d$year.germination[which(d$datasetID == "kettenring07" & is.na(d$year.germination))] <- "2004"
d$year.germination[which(d$datasetID == "lo19" & d$year.collected == 2017)] <- "2017"
d$year.germination[which(d$datasetID == "lo19" & d$year.collected == 2018)] <- "2018"
d$year.germination[which(d$datasetID == "ma18")] <- "2013"
#could not find martinik14
#na11 did not specify - JS
#could not find panayotova15 - JS
#could not find parvin16 - JS
#could not find phondani10 - JS
d$year.germination[which(d$datasetID == "picciau17" & is.na(d$year.germination))] <- "2012"
#chen15 germ year was unclear (was between 2012 and 2015, but depended on treatment. Which treatment was when is unclear) - JS
#could not find chen06 - JS
d$year.germination[which(d$datasetID == "chichizola18" & is.na(d$year.germination))] <- "2016"
#kim16 germ year was either 2013 or 2014 depending on length of cold strat treatment - JS
d$year.germination[which(d$datasetID == "king12" & is.na(d$year.germination))] <- "2007"
d$year.germination[which(d$datasetID == "kolodziejek18" & is.na(d$year.germination))] <- "2014/15"
#kolodziejek19 germ year was probably 2015 as storage time after collection was at most 16 weeks - JS
d$year.germination[which(d$datasetID == "kulkarni06" & is.na(d$year.germination))] <- "2003" 
#could not access lai03 - JS
#langlois17 had an experiment conducted in 2007 and one in 2014 - JS
d$year.germination[which(d$datasetID == "aiello17" & is.na(d$year.germination))] <- "2015"
#could not find budisavljevic21 
#could not find chen08
#could not find harrington09
d$year.germination[which(d$datasetID == "hatzilazarou21" & is.na(d$year.germination))] <- "2021"
d$year.germination[which(d$datasetID == "herron01" & is.na(d$year.germination))] <- "1997"
#could not find irvani12

