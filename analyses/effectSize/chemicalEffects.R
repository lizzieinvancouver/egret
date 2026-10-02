## Started 12 August 2026 ##
## by Lizzie ##

# How big is the effect size of chemical treatments?
# In a hacky way ... 

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

if(length(grep("lizzie", getwd()) > 0)) {
  setwd("/Users/lizzie/Documents/git/projects/egret/analyses")
} else if(length(grep("Xiaomao", getwd()) > 0)) {
  setwd("C:/PhD/Project/egret/analyses")
}

d <- read.csv("output/egretclean.csv")

sort(table(d$chemicalCor))
sort(table(d$chemicalConcent))

# let's make a list of all the colnames that I need to control for!
# doesn't that sound fun? Yes, yes, it does.... 

colztocontrol <- c("latbi", "provLatLon", "provLatLonAlt",
  "chillTemp", "chillDuration", "chillTempUnc",        
  "chillTempCycle", "chillLightCycle", "germTempGen",        
  "germTemp", "germDuration", "germTempClass",    
  "germTempDay", "germTempNight", "germPhotoperiod",     
  "germPhotoperiodDay", "germPhotoperiodNight", 
  "scarifTypeGen", "scarifTypeSpe",         
  "storageType", "storageDetails", "storageTemp", "storageDuration", 
  "photoperiodCor",
  "chemicalCor", "chemicalConcent", "chemicalConcentUnit")

thestudies <- unique(d$datasetIDstudy)
studydesign <- data.frame(matrix(ncol = length(colztocontrol), nrow=length(thestudies)))
names(studydesign) <- colztocontrol

for(astudy in seq_along(thestudies)){
  subby <- d[which(d$datasetIDstudy==thestudies[astudy]),]
  for(colhere in seq_along(colztocontrol)){
    studydesign[astudy, colhere] <- length(unique(subby[,colztocontrol[colhere]]))
  }
}
studydesign$datasetIDstudy <- thestudies

pdf("figures/effectsize/diffTreatsPerStudy.pdf", width=10, height=12)
par(mfrow=c(4,4))
  for(colhere in seq_along(colztocontrol)){
    hist(studydesign[,colztocontrol[colhere]], main="", xlab=colztocontrol[colhere])
  }
dev.off()

# How many are just one?
howmanylevels <- data.frame(whatcol=character(), 
  howmany1=numeric(), 
  howmany2=numeric(), 
  howmanymodan2=numeric())
for(colhere in seq_along(colztocontrol)){
  howmanylevels[colhere,"whatcol"] <- colztocontrol[colhere]
  howmanylevels[colhere,"howmany1"] <- table(studydesign[,colztocontrol[colhere]])[1]
  howmanylevels[colhere,"howmany2"] <- table(studydesign[,colztocontrol[colhere]])[2]
} 
howmanylevels$howmanymodan2 <- nrow(studydesign)-howmanylevels$howmany1-howmanylevels$howmany2
howmanylevels[with(howmanylevels, order(-howmany1)), ]

# Okay, that was a fun and not super important detour ...
# Next, I will subset to the studies that vary 
#   "chemicalCor"         
#   "chemicalConcent" 
# which are ...
studydesign$datasetIDstudy[which(studydesign$chemicalCor>1)]
studydesign$datasetIDstudy[which(studydesign$chemicalConcent>1)]

chemstudiesall <- c(studydesign$datasetIDstudy[which(studydesign$chemicalCor>1)], 
  studydesign$datasetIDstudy[which(studydesign$chemicalConcent>1)])  

chemstudies <- unique(chemstudiesall)
dchem <- d[which(d$datasetIDstudy %in% chemstudies),]
# ... and then get the min and max response for each unique set of ALL possible columns (from above)

colztocontrolplus <- c("responseVar", "datasetIDstudy", colztocontrol)
dathere <- d[which(d$datasetIDstudy %in% chemstudies),]

colztocontrolminchem <- colztocontrolplus[which(!colztocontrolplus %in% 
  c("chemicalConcent"))]

# Ugly way to built a dataframe!
minmax <- dchem[1,]
minmax$counter <- NA
minmax <- minmax[-1,] 

# I am a VERY slow loop (like a minute or two I think....)
for(studyhere in seq_along(chemstudies)){
  subby <- dchem[which(dchem$datasetIDstudy==chemstudies[studyhere]),]
  # For subby, get the unique treatments ... 
  uniquestuff <- unique(subby[colztocontrolminchem])
  for(i in c(1:length(uniquestuff))){
    # merge in the full data and find examples where there are >2 rows 
    uniquestuffind <- merge(dchem, uniquestuff[i,])
    uniquestuffind$counter <- rep(i, nrow(uniquestuffind))
    if (nrow(uniquestuffind)>1 & length(unique(uniquestuffind$chemicalConcent))>1) {
      minmax <- rbind(minmax, uniquestuffind)
     }
   }
}

minmax$datasetIDstudycount <- paste(minmax$datasetIDstudy, minmax$counter)
unique(minmax$datasetIDstudycount)

table(minmax$datasetIDstudycount)
length(table(minmax$datasetIDstudycount)) # boff, 640 unique things happened -- oy!

# So now we need to know what varies in each study I think.... 
# li21exp4, for example, is easy -- it looks like they just varied the chemical 

onestudy <- subset(minmax, datasetIDstudy=="li21exp4")

library(ggplot2)
ggplot(onestudy, aes(x=as.numeric(chemicalConcent), y=as.numeric(responseValueNum))) + 
  geom_point() + 
  facet_wrap(counter~.)

ggplot(subset(minmax, datasetIDstudy=="li21exp4"), aes(x=as.numeric(chemicalConcent), y=as.numeric(responseValueNum), 
    color=chemical)) + 
  geom_point() + 
  geom_line()

howmanyhere <- length(unique(minmax$datasetIDstudycount))
# I could figure this out by stepping through each column and then grabbing what is varying ...
whathappenedwcounter <- data.frame(datasetIDstudycount=unique(minmax$datasetIDstudycount),
  datasetIDstudy=rep(NA, howmanyhere), counter=rep(NA, howmanyhere),
  responseVar=rep(NA, howmanyhere),
  chemical=rep(NA, howmanyhere), chemicalConcent=rep(NA, howmanyhere))

for(i in c(1:howmanyhere)){
  subby <- minmax[which(minmax$datasetIDstudycount==unique(minmax$datasetIDstudycount)[i]),]
  whathappenedwcounter$datasetIDstudy[i] <- subby$datasetIDstudy[1]
  whathappenedwcounter$counter[i] <- subby$counter[1]
  whathappenedwcounter$responseVar[i] <- subby$responseVar[1]
  whathappenedwcounter$chemical[i] <- length(unique(subby$chemical))
  whathappenedwcounter$chemicalConcent[i] <- length(unique(subby$chemicalConcent))
}

whathappenedwcounter <- whathappenedwcounter[with(whathappenedwcounter, order(-chemicalConcent, -chemical)),]

# Wait, the above does each counter... I need each study... 
howmanyhere <- length(unique(minmax$datasetIDstudy))
# I could figure this out by stepping through each column and then grabbing what is varying ...
whathappened <- data.frame(datasetIDstudy=unique(minmax$datasetIDstudy),
  responseVar=rep(NA, howmanyhere),
  chemical=rep(NA, howmanyhere), chemicalConcent=rep(NA, howmanyhere))
addmeplease <- data.frame(matrix(ncol = length(colztocontrol), nrow=howmanyhere))
names(addmeplease) <- colztocontrol
whathappened <- cbind(whathappened, addmeplease)


for(i in c(1:howmanyhere)){
  subby <- minmax[which(minmax$datasetIDstudy==unique(minmax$datasetIDstudy)[i]),]
  whathappened$responseVar[i] <- subby$responseVar[1]
  whathappened$chemical[i] <- length(unique(subby$chemical))
  whathappened$chemicalConcent[i] <- length(unique(subby$chemicalConcent))
  for(j in c(1:length(colztocontrol))){ # 7 onward ... 
    whathappened[i,colztocontrol[j]] <- length(unique(subby[,colztocontrol[j]]))-1 # so I can get 0 back
  }
}

countingstuff <- data.frame(whatcol=colztocontrol, howmanystudies=rep(NA, length(colztocontrol)),
  howmanylevels=rep(NA, length(colztocontrol)))
for(j in c(1:length(colztocontrol))){ 
  countingstuff$whatcol[j] <- colztocontrol[j]
  countingstuff$howmanystudies[j] <- sum(whathappened[,colztocontrol[j]] !=0) 
  countingstuff$howmanylevels[j] <- sum(whathappened[,colztocontrol[j]])
}

countingstuff[with(countingstuff, order(-howmanystudies)),]
whathappened[with(whathappened, order(-chemical)),]

# Okay! Of 89 studies 27 vary chillDuration (top hit!)
table(minmax$chemicalCor)
# citric acid is C6H8O7
# salicylic acid is C7H6O73
# thiourea is some sort of organosulfur CH4N2S
table(whathappened$chemical)

# So I could do ...
# (a) GA stuff (and maybe some salts?) with chemical concentration
# (b) I could probably do ANY chemical min and max response....

# Okay, so here's a start to (a)
ganames <- c("GA", "GA3", "GA4")
gastuff <- minmax[which(minmax$chemicalCor %in% ganames),]

table(gastuff$responseVar)

# Most of the observed other variation explained by chillDuration and germTempGen
# not much to see for scarifTypeGen
ggplot(gastuff[which(gastuff$responseVar=="percent.germ"),], 
  aes(x=as.numeric(chemicalConcent), y=as.numeric(responseValueNum), group=latbi, color=chillDuration)) + 
  geom_point() + 
  facet_wrap(latbi~.)

ggplot(gastuff[which(gastuff$responseVar=="percent.germ"),], 
  aes(x=as.numeric(chemicalConcent), y=as.numeric(responseValueNum), group=latbi, color=germTempGen)) + 
  geom_point() + 
  facet_wrap(latbi~.)

ggplot(gastuff[which(gastuff$responseVar=="percent.germ"),], 
  aes(x=as.numeric(chemicalConcent), y=as.numeric(responseValueNum), color=latbi)) + 
  geom_point() 

write.csv(gastuff, "effectSize/output/biggestEffectDataGA.csv", row.names=FALSE)

# Now, I need to do (b) 
# I probably need just ONE of my counters from each study ... 
# I could find the highest average germination per study of the counters and then look at min/max for ANY chemical 
# I think I will do that... and can adjust later



pergerm <- minmax[which(minmax$responseVar=="percent.germ"),]
pergermstudies <- unique(pergerm$datasetIDstudy)
pergermstudiescount <- unique(pergerm$datasetIDstudycount)
howmanypergerm <- length(unique(pergerm$datasetIDstudycount))

meanbycounter <- data.frame(datasetIDstudycount=unique(pergerm$datasetIDstudycount),
  datasetIDstudy=rep(NA, howmanypergerm), counter=rep(NA, howmanypergerm),
  meanhere=rep(NA, howmanypergerm))

for(studyhere in seq_along(pergermstudiescount)){
  subby <- pergerm[which(pergerm$datasetIDstudycount==pergermstudiescount[studyhere]),]
  meanbycounter$datasetIDstudy[studyhere] <- subby$datasetIDstudy[1]
  meanbycounter$counter[studyhere] <- subby$counter[1]
  meanbycounter$meanhere[studyhere] <- mean(subby$responseValueNum, na.rm=TRUE)
  }

# Okay, now I need to get the max mean and go back to the minmax data...
# I did a very quick visual check and I think this works... 
maxones <- c()
gothruthese <- unique(meanbycounter$datasetIDstudy)

for(datasetIDstudyhere in seq_along(gothruthese)){
    subby <- meanbycounter[which(meanbycounter$datasetIDstudy==gothruthese[datasetIDstudyhere]),]
    maxones[datasetIDstudyhere] <- subby$datasetIDstudycount[which(subby$meanhere==max(subby$meanhere))][1] # take the first for cases with ties (yes, this is cheap)
}

# Finally ... get the min and max response from these ... 
highestmeangerm <- pergerm[which(pergerm$datasetIDstudycount %in% maxones),]
# Ugly way to built a dataframe!
minmaxoneperstudy <- minmax[1,]
minmaxoneperstudy <- minmaxoneperstudy[-1,] 

diffs <- data.frame(datasetIDstudy=character(), respDiff=numeric(),
  chemicalConcentDiff=numeric(), chemical=character()
  )

for(studyhere in seq_along(unique(highestmeangerm$datasetIDstudy))){
  subby <- highestmeangerm[which(highestmeangerm$datasetIDstudy==unique(highestmeangerm$datasetIDstudy)[studyhere]),]
  minmaxoneperstudy <- rbind(minmaxoneperstudy,
    subby[which(subby$responseValueNum==max(subby$responseValueNum)),],
    subby[which(subby$responseValueNum==min(subby$responseValueNum)),]) # now you get both rows when there is a duplicate ...
  }
