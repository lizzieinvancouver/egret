## Started 30 November 2023 ##
## By Lizzie ##

## Updated Aug 1 2024 by DL

## This contains miscellaneous cleaning of specific entries ##
## or ... it should, once we have stuff like that ... ##

# Lee 21 table2 and Fig 1c redundant data, subset to table only
d <- d[-(which(d$datasetID == "lee21" & d$figure %in% c("Figure 1c", "Figure 1b", "Figure 2a", "Figure 3b"))),] # 112 rows

#na11: the description of the treatments vary between the methods and the results, so we are removing this study
d <- d[-(which(d$datasetID == "na11")),] #12 rows

# zhou08: one entry is mislabbeled figure 1.A but is for Table two 
d$figure[which(d$datasetID == "zhou08" & d$response =="8.4")] <- "table 2"

# cho18b: species name is incorrect
d$genus[which(d$datasetID == "cho18b")] <- "Cornus"
d$species[which(d$datasetID == "cho18b")] <- "kousa"
d$latbi[which(d$datasetID == "cho18b")] <- "Cornus_kousa"


# datasets that were entered my multiple people to check for data consistency, but keeping only one
d <- d[-(which(d$datasetID == "batlla03" & d$entered.by == "DM")),] #34
d <- d[-(which(d$datasetID == "chen15" & d$entered.by == "AZ")),] #257
d <- d[-which(d$datasetID == "al-absi10" & d$entered.by == "TA"),]  #96 
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

d <- d[-which(d$datasetID == "chen15" & d$entered.by == "TA"),] #11
d <- d[-which(d$datasetID == "jang22" & d$entered.by == "SC"),] #11
d <- d[-which(d$datasetID == "langlois17" & d$entered.by == "AZ"),] #322
d <- d[-which(d$datasetID == "lo19" & d$entered.by == "SC"),] #95
d <- d[-which(d$datasetID == "nurse08" & d$entered.by == "MN"),] #156
d <- d[-which(d$datasetID == "olmez07" & d$entered.by == "MN"),] #92
d <- d[-which(d$datasetID == "olmez09" & d$entered.by == "MN"),] #56
d <- d[-which(d$datasetID == "redondo-gomez11" & d$entered.by == "DM"),]#142
d <- d[-which(d$datasetID == "zhou08" & d$entered.by == "DK" & d$figure == "figure 1.A"),]  #43
d <- d[-which(d$datasetID == "zhou08" & d$entered.by == "DK" & d$figure == "figure 1.B"),]  
d <- d[-which(d$datasetID == "zhou08" & d$entered.by == "DK" & d$figure == "figure 1.C"),]  
d <- d[-which(d$datasetID == "zhou08" & d$entered.by == "DK" & d$figure == "figure 1.D"),]  
d <- d[-which(d$datasetID == "song20" & d$entered.by == "DM"),]  
d <- d[-which(d$datasetID == "pritchard93" & d$entered.by == "MN" & d$figure == "Table 3"),]  
d <- d[-which(d$datasetID == "pritchard93" & d$figure == "Figure 2"),]  
d <- d[-which(d$datasetID == "rubin18" & d$entered.by == "MN" & d$figure == "Figure 1"),]  
d <- d[-which(d$datasetID == "picciau17" & d$entered.by == "SC" & d$figure == "Table 1"),]  


# momonoki79: issue #65 these two tables switch the seeds to lettuce---a crop
d <- d[-(which(d$datasetID == "momonoki79" & d$figure == "table20")),] # 4
d <- d[-(which(d$datasetID == "momonoki79" & d$figure == "table21")),] # 4

# Washitani85 values greater than 100 bc the x-axis values were entered as
d <- d[-(which(d$datasetID == "washitani85" & d$entered.by == "HHN" & d$figure == "Figure 5")),] 

# yang18b: changing figure labels. 
d$figure[which(d$datasetID == "yang18b" & d$figure == "Figure 3")] <- "Figure 4"
d$figure[which(d$datasetID == "yang18b" & d$figure == "Figure 2")] <- "Figure 3"
d$figure[which(d$datasetID == "yang18b" & d$figure == "Figure 1")] <- "Figure 2"

# chen08 vs chien09? This is the same paper, but one is the original journal article (chen08) and the other is from the
# Taiwan Forestry Research Institute Data Catalog, but under a different first author so it got a unique dataset ID (chien09)

d <- d[-which(d$datasetID == "chien09"),]  #6 

# issue with alhelal96, other.treatment = N (1 row)
# looks weird, all other rows (=27) have other.treatment = NA (note: I did not check the original paper) - V.V.
d[which(d$datasetID == "alhelal96" & d$other.treatment == "N"), 'other.treatment']  <- NA

# missing other.treatment
d[which(d$datasetID == "downie98" & is.na(d$other.treatment)), "other.treatment"] <- d[which(d$datasetID == "downie98" & is.na(d$other.treatment)), "treatment"]

# non-existent data in budisavljevic21
d <- d[-which(d$datasetID == "budisavljevic21" &
                d$treatment %in% c("warm stratification", "Warm stratification") &
                d$figure != "Table2e"),]
d <- d[-which(d$datasetID == "budisavljevic21" & duplicated(d)), ]

# two different papers were scraped as cho18
#exp1 table 1 and fig 5, and exp 2 table 3 is from a
d$datasetID[which(d$datasetID == "cho18" & d$study == "exp1" & d$figure == "Table 1")] <-
  "cho18a"
d$datasetID[which(d$datasetID == "cho18" & d$study == "exp1" & d$figure == "Figure 5")] <-
  "cho18a"
d$datasetID[which(d$datasetID == "cho18" & d$study == "exp2" & d$figure == "Table 3")] <-
  "cho18a"

#exp1 fig 3 and exp2 fig 5 is from b, wrong species
d$datasetID[which(d$datasetID == "cho18" & d$study == "exp1" & d$figure == "Figure 3")] <-
  "cho18b"
d$datasetID[which(d$datasetID == "cho18" & d$study == "exp2" & d$figure == "Figure 5")] <-
  "cho18b"

# fix grouping, fix study and figure some other time
d$study[which(d$datasetID == "kulkarni06" & d$study == "exp3" & d$figure == "fiigure 3")] <- "exp2"
d$figure[which(d$datasetID == "pipinis20" & d$treatment == "Warm statification (0 months) + cold stratification (0 months)")] <-
  "Table 2"
d$study[which(d$datasetID == "li11" & d$study == "exp 1 ")] <- "exp 1"

# rescraped jusung16, Table 1
d <- d[-which(d$datasetID == "jusung16" & d$entered.by == "SC" & d$figure == "Table 1"),]  

# rescraped tang10_2
d <- d[-which(d$datasetID == "tang10b" & d$entered.by == "JN"),] #4/7 Dan found this deletes all data

# rescraped bungard97 Fig1B
d <- d[-which(d$datasetID == "bungard97" & d$entered.by == "BW" & d$figure == "Figure 1b"),]

# fetouh14: fixing table number
d$figure[which(d$datasetID == "fetouh14")] <- "table 1"

# santos19: fixing table numbers
d$figure[which(d$datasetID == "santos19" & d$respvar == "IVG")] <-
  "Table 2"
d$figure[which(d$datasetID == "santos19" & d$respvar == "IVG")] <-
  "Table 3"

# skordilis95: 
d$figure[which(d$datasetID == "skordilis95" & d$figure == "Figure 2" & d$species == "brutia")] <-
  "Figure 2a"
d$figure[which(d$datasetID == "skordilis95" & d$figure == "Figure 2" & d$species == "halepensis")] <-
  "Figure 2b"
# thomsen02
d$figure[which(d$datasetID == "thomsen02" & d$figure == "Figure 2" & d$respvar == "per.germ")] <-
  "Figure 2a"
d$figure[which(d$datasetID == "thomsen02" & d$figure == "Figure 2" & d$respvar == "mgt")] <-
  "Figure 2b"
d$figure[which(d$datasetID == "thomsen02" & d$figure == "Figure 3")] <-
  "Figure 4"

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

d$figure[which(d$datasetID == "airi09" & d$respvar == "mgt")] <- "Table 3"


