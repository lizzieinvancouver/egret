## Updated 30 January 2024 ##
## By Deirdre ##

## This contains code to clean chill duration and temperature ##
## Original code taken from file called cleaningDL.R ##

#1. Scarification
unique(dat$scarification)
unique(dat$scarif.type)

# group specifically 
dat$scarif.typeCora <- dat$scarif.type

dat$scarif.typeCora[which(dat$scarif.typeCora == "sandpaper")] <- "mechanical - sandpaper"
dat$scarif.typeCora[which(dat$scarif.typeCora == "sand paper (Np. 150)")] <- "mechanical - sandpaper"

dat$scarif.typeCora[which(dat$scarif.typeCora == "mechanical with razor")] <- "mechanical - razor"
dat$scarif.typeCora[which(dat$scarif.typeCora == "H2SO4")] <- "chemical - H2SO4"

# group more generally --- chemical vs mechanical
dat$scarif.typeCorb <- dat$scarif.type

dat$scarif.typeCorb[which(dat$scarif.typeCorb == "sandpaper")] <- "mechanical"
dat$scarif.typeCorb[which(dat$scarif.typeCorb == "sand paper (Np. 150)")] <- "mechanical"

dat$scarif.typeCorb[which(dat$scarif.typeCorb == "mechanical with razor")] <- "mechanical"
dat$scarif.typeCorb[which(dat$scarif.typeCorb == "H2SO4")] <- "chemical"


### Check unusal entries--- Y, cold
# soaking---wrong column? is this data in the soaking column?
# is hot water scarificiation vs soaking---to disucss as a group