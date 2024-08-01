## Started 30 November 2023 ##
## By Lizzie ##

## This contains miscellaneous cleaning of specific entries ##
## or ... it should, once we have stuff like that ... ##

# Lee 21 table2 and Fig 1c redundant data, subset to table only

if(FALSE){
##	
## From clean_other.R, Original file called coordinate_cleaning_JS.R
unique(egret$woody) # N, Y, NA, "rhizomatus shrub/bamboo", "succulent" and more


unique(egret$seed.mass.given)
egret$seed.mass.given[which(egret$seed.mass.given == "yes")] <- "Y"
egret$seed.mass.given[which(egret$seed.mass.given == "no")] <- "N"
egret$seed.mass.given[which(egret$seed.mass.given == "Yes")] <- "Y"
egret$seed.mass.given[which(egret$seed.mass.given == "No")] <- "N"
egret$seed.mass.given[which(egret$seed.mass.given == "1200")] <- "Y"
egret$seed.mass.given[which(egret$seed.mass.given == "FALSE")] <- "N"

unique(egret$error.type)
egret$error.type[which(egret$error.type == "mean+/-SE")] <- "SE"
egret$error.type[which(egret$error.type == "mean+/-SD")] <- "SD"
egret$error.type[which(egret$error.type == "not specified")] <- "not.specified"

# TO CHECK what is xz
xz <- egret[which(egret$error.type == "xz"),]
#I think it should be NA because the error bars on the figure are too small to use - JS


unique(egret$resp.error)
unique(egret$reps)
unique(egret$n.per.rep)
unique(egret$germ.duration)
# TO CHECK
# why are there so many with decimal places? should it not be in days?
unique(egret$germ.tim.zero)
# TO CHECK - "TRUE" 
germtru <- egret[which(egret$germ.tim.zero == "TRUE"),]
#hatzilazou21 started counting germination "at the beginning of the test" (presumably when the treatments started) - JS


##
## from cleaningDL.R (Lizzie adds: I tried to delete the TONS of duplicate code that was in cleaning_other.R)
unique(egret$woody) # N, Y, NA, "rhizomatus shrub/bamboo", "succulent" 
temp <- subset(egret, woody == "O") # Magnolia_ingrata aka Magnolia_fulva, yes woody
egret$woody[which(egret$woody == "O")] <- "Y"

unique(egret$no.indiv.collected) # only 13 reported values
# some ranges -- some NA possibly...
unique(egret$year.collected)
egret$year.collected[which(egret$year.collected == "N/A")] <- "NA"
#character bc 4 values entered as ranges, also from 1951-2019

unique(egret$error.type)
egret$error.type[which(egret$error.type == "mean+/-SE")] <- "SE"
egret$error.type[which(egret$error.type == "mean+/-SD")] <- "SD"
egret$error.type[which(egret$error.type == "not specified")] <- "not.specified"

# TO CHECK what is xz

unique(egret$resp.error)

unique(egret$reps)

unique(egret$n.per.rep)
unique(egret$germ.duration)
# TO CHECK
# why are there so many with decimal places? should it not be in days?

unique(egret$germ.tim.zero)
# TO CHECK - "TRUE"

}