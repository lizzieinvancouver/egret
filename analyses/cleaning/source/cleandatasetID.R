## Started 1 Dec 2023 ##
## taken from cleaningDL.R ##

# Needs to be sourced in cleanall.R

# TO DO! We need to fix some of the multiple name-year IDs to nameyra and nameyrb

# Create new column
d$dasetIDCor <- tolower(d$dasetID)

# Fix ones with incorrect years
d$dasetIDCor[which(d$dasetIDCor == "acosta12")] <- "acosta13"
d$dasetIDCor[which(d$dasetIDCor == "brandel2005")] <- "brandel05"
d$dasetIDCor[which(d$dasetIDCor == "airi2009")] <- "airi09"
d$dasetIDCor[which(d$dasetIDCor == "alptekin2002")] <- "alptekin02"
d$dasetIDCor[which(d$dasetIDCor == "amini2018")] <- "amini18"
d$dasetIDCor[which(d$dasetIDCor == "pipinus12")] <- "pipinis12"
d$dasetIDCor[which(d$dasetIDCor == "picciau18")] <- "picciau19"

# Find dasetID's that are duplicated---ie a author published 2 papers in the same year: change to authorYeara and authorYearb

