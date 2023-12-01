## Updated 30 November 2023 ##
## By Lizzie and Deirdre ##

## This contains cleaning of year.germination ##
## Original code taken from file called coordinate_cleaning_JS.R ##

# fix NAs to the correct germ years that I found - JS
egret$year.germination[which(egret$datasetID == "acosta13" & is.na(egret$year.germination))] <- "2009"
egret$year.germination[which(egret$datasetID == "bhatt00" & is.na(egret$year.germination))] <- "1997"
egret$year.germination[which(egret$datasetID == "beikmohammadi12" & is.na(egret$year.germination))] <- "2011"
#could not find paper for aldridge1992,93,94,95,etc (not on source list in spreadsheet, don't know paper title) - JS
#amini18 should be on the google drive but I can't currently access it (obtained by ILL I think) - JS
egret$year.germination[which(egret$datasetID == "chien11" & is.na(egret$year.germination))] <- "2011"
#chien10 gives year collected and that the seeds were stored "temporarily" before germination, but I don't know exactly which year that would have been - JS
#chien11 states that experiment was done ~ a week after collection (2 days drying, 7 days storing) so year germination and year collected should be the same - JS
for(i in unique(egret$year.collected)) {
  egret$year.germination[which(egret$year.collected == i & is.na(egret$year.germination) & egret$datasetID == "chien11")] <- i
}
egret$year.germination[which(egret$datasetID == "cho18" & is.na(egret$year.germination))] <- "2012"
#for chuaren04, germination year is sometime between 2002 and 2004, but they ran multiple experiments and they don't specify each's time period - JS
#for cicek08, gem year was not specified - JS
egret$year.germination[which(egret$datasetID == "conversa09" & is.na(egret$year.germination))] <- "2006"
#for crank92, could not find paper on ubc library - JS
egret$year.germination[which(egret$datasetID == "cuena-lombrana18" & is.na(egret$year.germination))] <- "2014"
#deb17 doesn't give year collected or year germination - JS
#couldn't find dehgan84 on ubclibrary - JS
egret$year.germination[which(egret$datasetID == "cousins10" & is.na(egret$year.germination))] <- "2007"
#for dalling99, year germ is not specified - JS
#could not access naseri18 - JS
#naumovski05 does not specify year germ - JS
#nawrot-chorabik21 no year given - JS
#necajeva13 did not specify, but probably 2008 or 09 - JS
#nin17 did not specify - JS
#sacande05 did not specify - JS
egret$year.germination[which(egret$datasetID == "schutz02" & egret$chill.duration == 0)] <- "1998"
egret$year.germination[which(egret$datasetID == "schutz02" & egret$chill.duration != 0)] <- "2000"
# could not find saeed16 paper
#barros12 did not specify - JS
egret$year.germination[which(egret$datasetID == "jabarzare11" & is.na(egret$year.germination))] <- "2007"
#jacquemart21 did not specify - JS
egret$year.germination[which(egret$datasetID == "jensen97" & is.na(egret$year.germination))] <- "1993/94"
#jiro10 did not specify - JS
egret$year.germination[which(egret$datasetID == "jusung16" & is.na(egret$year.germination))] <- "2011"
#kamareh12 did not specify, but probably 2009 given context (no storage time given) - JS
egret$year.germination[which(egret$datasetID == "kato11" & is.na(egret$year.germination))] <- "2008"
egret$year.germination[which(egret$datasetID == "kazaz10" & is.na(egret$year.germination))] <- "2009"
#keshtkar08 did not specify - JS
egret$year.germination[which(egret$datasetID == "kettenring07" & is.na(egret$year.germination))] <- "2004"
egret$year.germination[which(egret$datasetID == "lo19" & egret$year.collected == 2017)] <- "2017"
egret$year.germination[which(egret$datasetID == "lo19" & egret$year.collected == 2018)] <- "2018"
egret$year.germination[which(egret$datasetID == "ma18")] <- "2013"
#ma03 did not specify - JS
#meyer94 and meyer95 did not specify - JS
#could not find martinik14
#na11 did not specify - JS
#onen20 did not specify
#ordonez-salanueva15 did not specify - JS
#could not find panayotova15 - JS
#could not find parvin16 - JS
#could not find phondani10 - JS
egret$year.germination[which(egret$datasetID == "picciau17" & is.na(egret$year.germination))] <- "2012"
#chen15 germ year was unclear (was between 2012 and 2015, but depended on treatment. Which treatment was when is unclear) - JS
#could not find chen06 - JS
egret$year.germination[which(egret$datasetID == "chichizola18" & is.na(egret$year.germination))] < - "2016"
#kim16 germ year was either 2013 or 2014 depending on length of cold strat treatment - JS
egret$year.germination[which(egret$datasetID == "king12" & is.na(egret$year.germination))] < - "2007"
egret$year.germination[which(egret$datasetID == "kolodziejek18" & is.na(egret$year.germination))] < - "2014/15"
#kolodziejek19 germ year was probably 2015 as storage time after collection was at most 16 weeks - JS
egret$year.germination[which(egret$datasetID == "kulkarni06" & is.na(egret$year.germination))] < - "2003" #collected 2003 only storage mentioned was for a week - JS
#lee21 did not specify - JS
#could not access lai03 - JS
#langlois17 had an experiment conducted in 2007 and one in 2014 - JS
#redondo-gomex11 exp 3 was conducted in 2010. Dates for other experiments not given but were presumably conducted earlier  - JS
# amooghaie09 dd not specify - JS
#could not find ahmad07
#could not find ahola99 
#aiello17 if "subsequently" is inferred to mean directly after collection, germ year is 2015
egret$year.germination[which(egret$datasetID == "aiello17" & is.na(egret$year.germination))] < - "2015"
#could not find budisavljevic21 
#could not find chen08
#could not find harrington09
egret$year.germination[which(egret$datasetID == "hatzilazarou21" & is.na(egret$year.germination))] < - "2021"
egret$year.germination[which(egret$datasetID == "herron01" & is.na(egret$year.germination))] < - "1997"
#could not find irvani12
