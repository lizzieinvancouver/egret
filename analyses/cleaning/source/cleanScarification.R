## Upded 30 January 2024 ##
## By Deirdre ##

## This contains code to clean chill duration and temperature ##
## Original code taken from file called cleaningDL.R ##

#1. Scarification
unique(d$scarification)
unique(d$scarif.type)

# group specifically 
d$scarif.typeCora <- d$scarif.type

d$scarif.typeCora[which(d$scarif.typeCora == "sandpaper")] <- "mechanical - sandpaper"
d$scarif.typeCora[which(d$scarif.typeCora == "sand paper (Np. 150)")] <- "mechanical - sandpaper"

d$scarif.typeCora[which(d$scarif.typeCora == "mechanical with razor")] <- "mechanical - razor"
d$scarif.typeCora[which(d$scarif.typeCora == "H2SO4")] <- "chemical - H2SO4"

# group more generally --- chemical vs mechanical
d$scarif.typeCorb <- d$scarif.type

d$scarif.typeCorb[which(d$scarif.typeCorb == "sandpaper")] <- "mechanical"
d$scarif.typeCorb[which(d$scarif.typeCorb == "sand paper (Np. 150)")] <- "mechanical"

d$scarif.typeCorb[which(d$scarif.typeCorb == "mechanical with razor")] <- "mechanical"
d$scarif.typeCorb[which(d$scarif.typeCorb == "H2SO4")] <- "chemical"


### Check unusal entries--- Y, cold
# soaking---wrong column? is this da in the soaking column?
# is hot water scarificiation vs soaking---to disucss as a group