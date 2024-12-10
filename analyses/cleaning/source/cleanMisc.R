## Started 30 November 2023 ##
## By Lizzie ##

## Updated Aug 1 2024 by DL

## This contains miscellaneous cleaning of specific entries ##
## or ... it should, once we have stuff like that ... ##

# Lee 21 table2 and Fig 1c redundant data, subset to table only
d <- d[-(which(d$datasetID == "lee21" & d$figure %in% c("Figure 1c", "Figure 1b", "Figure 2a", "Figure 3b"))),]

#na11: the description of the treatments vary between the methods and the results, so we are removing this study
d <- d[-(which(d$datasetID == "na11")),]

# datasets that were entered my multiple people to check for data consistency, but keeping only one
d <- d[-(which(d$datasetID == "batlla03" & d$entered.by == "DM")),]
d <- d[-(which(d$datasetID == "chen15" & d$entered.by == "AZ")),]
d <- d[-which(d$datasetID == "al-absi10" & d$entered.by == "TA"),]  #96 rows to remove
d <- d[-which(d$datasetID == "chen06" & d$entered.by == "AZ"),] #96
d <- d[-which(d$datasetID == "chichizola18" & d$entered.by == "AZ"),] #45
d <- d[-which(d$datasetID == "han10" & d$entered.by == "CRD"),] #40
d <- d[-which(d$datasetID == "lee21" & d$entered.by == "AZ"),] #30
d <- d[-which(d$datasetID == "moeini21" & d$entered.by == "MN"),] #45
d <- d[-which(d$datasetID == "tilki07" & d$entered.by == "MN"),] #104
d <- d[-which(d$datasetID == "wytsalucy21" & d$entered.by == "DK"),]#92
d <- d[-which(d$datasetID == "yusefi-tanha19" & d$entered.by == "JS"),]#40

# 10 papers were re-scraped by SS and corrected by DL:
corrected <- c("batlla03","chen15","jang22","langlois17", "lo19", "nurse08","olmez07","olmez09", "redondo-gomez11", "zhou08")     

d <- d[-which(d$datasetID == "chen15" & d$entered.by == "TA"),] #96
d <- d[-which(d$datasetID == "langlois17" & d$entered.by == "AZ"),] #40
d <- d[-which(d$datasetID == "lo19" & d$entered.by == "SC"),] #30
d <- d[-which(d$datasetID == "nurse08" & d$entered.by == "MN"),] #45
d <- d[-which(d$datasetID == "olmez07" & d$entered.by == "MN"),] #104
d <- d[-which(d$datasetID == "olmez09" & d$entered.by == "MN"),]#92
d <- d[-which(d$datasetID == "redondo-gomez11" & d$entered.by == "DM"),]#40
d <- d[-which(d$datasetID == "zhou08" & d$entered.by == "DK"),]  #96 rows to remove
d <- d[-which(d$datasetID == "pritchard93" & d$entered.by == "DM"),]  #96 rows to remove


# d <- d[-(which(d$datasetID == "olmez07" & d$entered.by == "MN" & d$figure == "Table 2")),]
# d <- d[-(which(d$datasetID == "olmez09" & d$entered.by == "MN" & d$figure == "Table 3")),]

if(FALSE){
##	
## From clean_other.R, Original file called coordinate_cleaning_JS.R
unique(d$woody) # N, Y, NA, "rhizomatus shrub/bamboo", "succulent" and more

unique(d$seed.mass.given)
d$seed.mass.given[which(d$seed.mass.given == "yes")] <- "Y"
d$seed.mass.given[which(d$seed.mass.given == "no")] <- "N"
d$seed.mass.given[which(d$seed.mass.given == "Yes")] <- "Y"
d$seed.mass.given[which(d$seed.mass.given == "No")] <- "N"
d$seed.mass.given[which(d$seed.mass.given == "1200")] <- "Y"
d$seed.mass.given[which(d$seed.mass.given == "FALSE")] <- "N"


}
