# started Jul 8, 2024 by Fredi

# aim is read in the csv files from everyone's data that will include both sourced cleaning code and write out a raw file

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

if(length(grep("deirdreloughnan", getwd()) > 0)) {
  setwd("~/Documents/github/egret/analyses")
} else if(length(grep("frederik", getwd())) > 0){
  setwd("/Users/frederik/github/egret/analyses")
}

#################################################################################
usda <- read.csv("./scrapeUSDAseedmanual/output/usdaGerminationData.csv", sep=",", header=TRUE)

#
###add stratification and chilling
usda$chilling<-ifelse(!is.na(usda$cold.strat.dur.Avg),"Y",usda$chilling)
usda$chilling<-ifelse(!is.na(usda$cold.strat.dur.Max),"Y",usda$chilling)
usda$chilling<-ifelse(!is.na(usda$cold.strat.dur.Min),"Y",usda$chilling)


###add stratification to chilling values
usda$chill.dur.Min<-ifelse(!is.na(usda$cold.strat.dur.Min),usda$cold.strat.dur.Min,usda$chill.dur.Min)
usda$chill.dur.Max<-ifelse(!is.na(usda$cold.strat.dur.Max),usda$cold.strat.dur.Max,usda$chill.dur.Max)
usda$chill.dur.Avg<-ifelse(!is.na(usda$cold.strat.dur.Avg),usda$cold.strat.dur.Avg,usda$chill.dur.Avg)
usda$chill.duraton<-ifelse(!is.na(usda$cold.stratification.duration),usda$cold.stratification.duration,usda$chill.duraton)

#
usda$responseVarClean <- usda$responseVar
#these columns show probably the same type of percentage, so rename them to "perc.standard"
usda$responseVarClean[usda$responseVarClean == "percent.germ"] <- "perc.standard"
usda$responseVarClean[usda$responseVarClean == "percent.germ.total"] <- "perc.standard"
usda$responseVarClean[usda$responseVarClean == "germ.capacity"] <- "perc.standard"
usda$responseVarClean[usda$responseVarClean == "mean.germ.capacity"] <- "perc.standard"
usda$responseVarClean[usda$responseVarClean == "percent.germ.15degC.incubated"] <- "perc.standard"
#"mean.percent.germ.energy" and "percent.germ.energy" are probably the same and can be named "percent.germ.energy"
usda$responseVarClean[usda$responseVarClean == "mean.percent.germ.energy"] <- "percent.germ.energy"

#make a new column "spec" to combine genus and species
usda$latbi <- paste(usda$genus, usda$species, sep = "_")


##################################################################
#### make additional rows for every min/max pair, eg. chilling that has a minimum and maximum value lead to a min and maximum percentage outcaome 
# identify the columns with min max
#identify any colums with "Min" or "Max" in the name
mincols <- grep("Min", names(usda), value = TRUE)
maxcols <- grep("Max", names(usda), value = TRUE)

############ Some rows can have min and max values both for chilling and a response variable. On Jul 9 we decided to match the min and max pair to make the most out of the data.

#how many species do we have with values in 
tapply(usda$latbi, usda$chill.dur.Max, function(x) length(unique(x)))
tapply(usda$latbi, usda$chill.dur.Max, function(x) (unique(x)))

aggregate(latbi ~ chill.dur.Max, data = usda, FUN = function(x) length(unique(x)))
length(unique(usda[!is.na(usda$chill.dur.Max), "latbi"])) #57 spp have a value in chill.dur.Max

# Step 1: filter rows with non-NA values in all specified columns
filtered_rows <- usda[!is.na(usda$chill.dur.Min) & !is.na(usda$chill.dur.Max) & 
                      !is.na(usda$responseValueMin) & !is.na(usda$responseValueMax), ]

filtered_spp <- usda[!is.na(usda$chill.dur.Min) & !is.na(usda$chill.dur.Max) & 
                      !is.na(usda$responseValueMin) & !is.na(usda$responseValueMax), "spec"]

tmp_X <- filtered_rows$X #this is used as a unique identifyier of rows
spec_X <- filtered_rows$spec

# remove all filtered rows from the original data frame
usda_new <- usda[!usda$X %in% tmp_X, ]
#check
length(usda_new$X) + length(tmp_X)
length(usda$X)

#create new dataframe for chill.dur.Min and responseValueMin
df_min <- filtered_rows
#assign "NA" to all rows of column "chill.dur.Max" and "responseValueMax"
df_min$chill.dur.Max <- NA
df_min$responseValueMax <- NA
#moving the values from "chill.dur.Min" to "chill.dur"
df_min$chill.duraton <- df_min$responseValueMin
#moving the values from "responseValueMin" to "responseValue"
df_min$responseValue <- df_min$responseValueMin


#create new dataframe for chill.dur.Max and responseValueMax
df_max <- filtered_rows
#assign "NA" to all rows of column "chill.dur.Max" and "responseValueMax"
df_max$chill.dur.Min <- NA
df_max$responseValueMin <- NA
#moving the values from "chill.dur.Max" to "chill.dur"
df_max$chill.duraton <- df_max$responseValueMax
#moving the values from "responseValueMax" to "responseValue"
df_max$responseValue <- df_max$responseValueMax

#add the new dataframes "df_min" and "df_max" to the "usda_new" dataframe
usda_new <- rbind(usda_new, df_min)
usda_new <- rbind(usda_new, df_max)

#check
length(usda_new$X) - (length(tmp_X))
length(usda$X)

#some more checks
#extract species that have min/max values for chill duration and a corresponding min/max value in the response variable
chilldurminnonNA <- usda$spec[which(!is.na(usda$chill.dur.Min))]
chilldurmaxonNA <- usda$spec[which(!is.na(usda$chill.dur.Max))]
respvarminnoNA <- usda$spec[which(!is.na(usda$responseValueMin))]
respvarmaxnoNA <- usda$spec[which(!is.na(usda$responseValueMax))]

sppwithminmaxchill <- chilldurminnonNA[which(chilldurminnonNA %in% chilldurmaxonNA)]

sppwithminmaxresp <- respvarminnoNA[which(respvarminnoNA %in% respvarmaxnoNA)]

sppwithcillresp <- sppwithminmaxchill[which(sppwithminmaxchill %in% sppwithminmaxresp)]

spp_split<-unique(sppwithcillresp) #20 species have this combination and we can split them up

