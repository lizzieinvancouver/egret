## Started 27 July 2025 ##
## By Lizzie ##
## Some cheap code to check if updated data introduces new errors ##
## This is not well thought out but better than nothing ##

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

if(length(grep("deirdre", getwd()) > 0)) {
  setwd("~/Documents/github/egret/analyses")
} else if(length(grep("lizzie", getwd()) > 0)) {
  setwd("/Users/lizzie/Documents/git/projects/egret/analyses")
} else if(length(grep("sapph", getwd()) > 0)) {
  setwd("/Users/sapph/Documents/ubc things/work/egret/analyses")
} else if(length(grep("danielbuonaiuto", getwd()) > 0)) {
  setwd("/Users/danielbuonaiuto/Documents/git/egret/analyses")
} else if(length(grep("Xiaomao", getwd()) > 0)) {
  setwd("C:/PhD/Project/egret/analyses")
} else if(length(grep("britanywuuu", getwd()) > 0)) {
  setwd("/Documents/ubc/year5/TemporalEcologyLab/egret/analyses")
} else if(length(grep("Ken", getwd())) > 0){
  setwd("/Users/Ken Michiko Samson/Documents/Temporal Ecology Lab/egret/analyses")
} else if(length(grep("christophe_rouleau-desrochers", getwd())) > 0){
  setwd("/Users/christophe_rouleau-desrochers/github/egret/analyses")
} else if(length(grep("victor", getwd())) > 0){
  setwd('~/projects/egret/analyses')
} 

# Some cheap but handy f(x)s
printchilling <- function(studylist){
  for (whichstudy in studylist){
    message(paste("HI!!! for the study:", whichstudy))
    hereisme <- d[which(d$datasetID == whichstudy),]
    for(tabfighere in unique(hereisme$figure)){
          hereismeagain <- hereisme[which(hereisme$figure == tabfighere),]
        message(paste("and the table or fig:", tabfighere))
        print("chilling is entered as (temp and duration) ...")
        print(unique(paste(hereismeagain$chill.temp, hereismeagain$chill.duration)))
        print("which gets corrected to (temp, duration, cycle--which includes 'then') ...")
        print(unique(paste(hereismeagain$chillTemp, 
          hereismeagain$chillDuration, 
          hereismeagain$chillTempCycle)))
    }
  }   
}

printphotoperiod <- function(studylist){
  for (whichstudy in studylist){
    message(paste("HI!!! for the study:", whichstudy))
    hereisme <- d[which(d$datasetID == whichstudy),]
    for(tabfighere in unique(hereisme$figure)){
          hereismeagain <- hereisme[which(hereisme$figure == tabfighere),]
        message(paste("and the table or fig:", tabfighere))
        print("photoperiod is entered as..")
        print(unique(paste(hereismeagain$photoperiod)))
        print("which gets corrected to (full photo, then day, then night, then light/dark...")
        print(unique(paste(hereismeagain$germPhotoperiod, 
          hereismeagain$germPhotoperiodDay, 
          hereismeagain$germPhotoperiodNight, 
          hereismeagain$photoperiodCor)))
    }
  }   
}

printotherstuff <- function(studylist){
  for (whichstudy in studylist){
    message(paste("HI!!! for the study:", whichstudy))
    hereisme <- d[which(d$datasetID == whichstudy),]
    for(tabfighere in unique(hereisme$figure)){
          hereismeagain <- hereisme[which(hereisme$figure == tabfighere),]
        message(paste("and the table or fig:", tabfighere))
        print("response value is entered as..")
        print(unique(paste(hereismeagain$response)))
        print("which gets corrected to...")
        print(unique(paste(hereismeagain$responseValue, 
          hereismeagain$responseValueNum)))
        print("scarification is entered as..")
        print(unique(paste(hereismeagain$scarification, hereismeagain$scarif.type)))
        print("which gets corrected to...")
        print(unique(paste(hereismeagain$scarifType, 
          hereismeagain$scarifTypeGen)))
        print("chemical is entered as..")
        print(unique(paste(hereismeagain$chemical)))
        print("which gets corrected to...")
        print(unique(paste(hereismeagain$chemicalCor)))
        print("storage is entered as (three things: time, humidity, temp)..")
        print(unique(paste(hereismeagain$storage.time,
          hereismeagain$storage.humidity,
          hereismeagain$storage.temp)))
        print("which gets corrected to...")
        print(unique(paste(hereismeagain$storageType, 
          hereismeagain$storageDetails,
          hereismeagain$storageTemp,
          hereismeagain$storageDuration)))
        print("and here's a histogram of responseValueNum:")
        quartz()
        hist(hereismeagain$responseValueNum)
    }
  }   
}


d <- read.csv("output/egretclean.csv")

# 1. redondo-gomez11: changes to photoperiod and chilling for Table 1
printchilling(c("redondo-gomez11")) # this is all NA
printphotoperiod(c("redondo-gomez11")) # this look fine to me. 

# 2. meyer94 and meyer95, from the commit I think we edited chilling
printchilling(c("meyer94", "meyer95"))


# 3. chen08: chen08: CRD scraped Fig 4a, 4b, 4c, 4d.
printchilling(c("chen08"))
printphotoperiod(c("chen08")) 
printotherstuff(c("chen08"))
# looks good to me

# 4. downie91: CRD scraped Fig 1
printchilling(c("downie91"))
printphotoperiod(c("downie91")) 
printotherstuff(c("downie91"))
# looks good to me

# 5. al-absi10: DL added data on hot water and H2SO4 scarification and GA3 addition on mgt, fig 2a-c
printotherstuff(c("al-absi10")) # double-check scarification 

# 6. farhadi13: added data from elevational gradient, cold-moist stratification, Table 2
printchilling("farhadi13") 

# 7. ghimeray14: added data on different types of hot scarification (HCl or sand), Table 5
printotherstuff(c("ghimeray14")) # double-check scarification 

# 8. hawkins19, added data on GA concentrations from 0-1000 mg/L, FIg 4
printotherstuff(c("hawkins19")) # looks good

# extra! tang10b needs 12:12 to become 12/12 
printphotoperiod(c("tang10b"))