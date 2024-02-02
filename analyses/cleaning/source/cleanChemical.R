## Upded 30 January 2024 ##
## By Deirdre ##

## This contains code to clean chill duration and temperature ##
## Original code taken from file called cleaningDL.R ##

#1. Chemical---type
unique(d$chemical)

d$chemicalCor <- d$chemical
d$chemicalCor[which(d$checmicalCor == "Water")] <- "H2O"
d$chemicalCor[which(d$checmicalCor == "H20")] <- "H2O"

#Check unusual values:
# log2(red/far.red)
# E
# control---is it water?
