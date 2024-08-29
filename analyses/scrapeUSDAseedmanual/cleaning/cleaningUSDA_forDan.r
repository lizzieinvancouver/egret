# started Jul 8, 2024 by Fredi
if(FALSE){
# aim: cleaning usda dataset. 1)
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

#################################################################################
#################################################################################
#usda.orig <- read.csv("./scrapeUSDAseedmanual/output/usdaGerminationData.csv", sep=",", header=TRUE)
}
usda<-d

#setdiff(colnames(usda.orig),colnames(usda))
#
###add stratification and chilling
usda$chilling<-ifelse(!is.na(usda$cold.strat.dur.Avg),"Y",usda$chilling)
usda$chilling<-ifelse(!is.na(usda$cold.strat.dur.Max),"Y",usda$chilling)
usda$chilling<-ifelse(!is.na(usda$cold.strat.dur.Min),"Y",usda$chilling)


###add stratification to chilling values
usda$chill.dur.Min<-ifelse(!is.na(usda$cold.strat.dur.Min),usda$cold.strat.dur.Min,usda$chill.dur.Min)
usda$chill.dur.Max<-ifelse(!is.na(usda$cold.strat.dur.Max),usda$cold.strat.dur.Max,usda$chill.dur.Max)
usda$chill.dur.Avg<-ifelse(!is.na(usda$cold.strat.dur.Avg),usda$cold.strat.dur.Avg,usda$chill.dur.Avg)
usda$chill.duration<-ifelse(!is.na(usda$cold.stratification.duration),usda$cold.stratification.duration,usda$chill.duration) # error there is no column chill.duration in that comes from Justin's new work flow

#  ^ from Justin: I fixed "chill.duraton" to "chill.duration"

#create a new column 
usda$responseVarClean <- usda$responseVar

#how many uniques entries are in usda$responseVar
tapply(usda$speciesID, usda$responseVarClean, function(x) length(!is.na(x)))
aggregate(speciesID ~ responseVar, data = usda, FUN = function(x) length(unique(x)))
usda[usda$responseVarClean == "percent.germ","responseVarClean"]

#these columns most likely show the same type of percentage, so rename them to "perc.standard"
usda$responseVarClean[usda$responseVarClean == "percent.germ"] <- "perc.standard"
usda$responseVarClean[usda$responseVarClean == "percent.germ.total"] <- "perc.standard"
usda$responseVarClean[usda$responseVarClean == "germ.capacity"] <- "perc.standard"
usda$responseVarClean[usda$responseVarClean == "mean.germ.capacity"] <- "perc.standard"
usda$responseVarClean[usda$responseVarClean == "percent.germ.15degC.incubated"] <- "perc.standard"
#another two that are probably the same
usda$responseVarClean[usda$responseVarClean == "mean.percent.germ.energy"] <- "percent.germ.energy"

#make a new column "spec" to combine genus and species
usda$latbi <- paste(usda$genus, usda$species, sep = "_")


##################################################################
#### make additional rows for every min/max pair, eg. chilling that has a minimum and maximum value lead to a min and maximum percentage outcaome 
# identify the columns with min max
#identify any colums with "Min" or "Max" in the name
mincols <- grep("Min", names(usda), value = TRUE)
maxcols <- grep("Max", names(usda), value = TRUE)


############

aggregate(latbi ~ chill.dur.Max, data = usda, FUN = function(x) length(unique(x)))

###need to make a rowmnames column
usda$X<-rownames(usda)


# Step 1: Filter rows with non-NA values in all specified columns
filtered_rows <- usda[!is.na(usda$chill.dur.Min) & !is.na(usda$chill.dur.Max) & 
                      !is.na(usda$responseValueMin) & !is.na(usda$responseValueMax), ]

filtered_spp <- usda[!is.na(usda$chill.dur.Min) & !is.na(usda$chill.dur.Max) & 
                      !is.na(usda$responseValueMin) & !is.na(usda$responseValueMax), "latbi"]


tmp_X <- filtered_rows$X
spec_X <- filtered_rows$latbi

# Remove all filtered rows from the original data frame
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
##dmb need to change to resposeValue
class(usda_new$responseValue)
class(df_min$responseValue)

df_min$responseValue<-as.character(df_min$responseValue)
usda_new <- rbind(usda_new, df_min)

df_max$responseValue<-as.character(df_max$responseValue)
usda_new <- rbind(usda_new, df_max)

#check
length(usda_new$X) - (length(tmp_X))
length(usda$X)



#some checks
chilldurminnonNA <- usda$latbi[which(!is.na(usda$chill.dur.Min))]
chilldurmaxonNA <- usda$latbi[which(!is.na(usda$chill.dur.Max))]
respvarminnoNA <- usda$latbi[which(!is.na(usda$responseValueMin))]
respvarmaxnoNA <- usda$latbi[which(!is.na(usda$responseValueMax))]

sppwithminmaxchill <- chilldurminnonNA[which(chilldurminnonNA %in% chilldurmaxonNA)]

sppwithminmaxresp <- respvarminnoNA[which(respvarminnoNA %in% respvarmaxnoNA)]

sppwithcillresp <- sppwithminmaxchill[which(sppwithminmaxchill %in% sppwithminmaxresp)]

spp_split<-unique(sppwithcillresp) #20 species have this combination and we can split them up

