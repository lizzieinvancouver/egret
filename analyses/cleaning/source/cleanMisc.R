## Started 30 November 2023 ##
## By Lizzie ##

## Updated Aug 1 2024 by DL

## This contains miscellaneous cleaning of specific entries ##
## or ... it should, once we have stuff like that ... ##

# Lee 21 table2 and Fig 1c redundant data, subset to table only
d <- d[-(which(d$datasetID == "lee21" & d$figure %in% c("Figure 1c", "Figure 1b", "Figure 2a", "Figure 3b"))),]


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