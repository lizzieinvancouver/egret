## Started 1 Dec 2023 ##
## taken from cleaningDL.R ##

# Needs to be sourced in cleanall.R

# TO DO! We need to fix some of the multiple name-year IDs to nameyra and nameyrb

# Create new column
dat$datasetIDCor <- tolower(dat$datasetID)

# Fix ones with incorrect years
dat$datasetIDCor[which(dat$datasetIDCor == "acosta12")] <- "acosta13"
dat$datasetIDCor[which(dat$datasetIDCor == "brandel2005")] <- "brandel05"
dat$datasetIDCor[which(dat$datasetIDCor == "airi2009")] <- "airi09"
dat$datasetIDCor[which(dat$datasetIDCor == "alptekin2002")] <- "alptekin02"
dat$datasetIDCor[which(dat$datasetIDCor == "amini2018")] <- "amini18"
dat$datasetIDCor[which(dat$datasetIDCor == "pipinus12")] <- "pipinis12"
dat$datasetIDCor[which(dat$datasetIDCor == "picciau18")] <- "picciau19"

# Find datasetID's that are duplicated---ie a author published 2 papers in the same year: change to authorYeara and authorYearb

