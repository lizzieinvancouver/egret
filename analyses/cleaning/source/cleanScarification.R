## Upded 30 January 2024 ##
## By Deirdre ##

## This contains code to clean chill duration and temperature ##
## Original code taken from file called cleaningDL.R ##

#1. Scarification
unique(d$scarification)
unique(d$scarif.type)

# group specifically 
d$scarifTypeA <- d$scarif.type

d$scarifTypeA[which(d$scarifTypeA == "sandpaper")] <- "mechanical - sandpaper"
d$scarifTypeA[which(d$scarifTypeA == "sand paper (Np. 150)")] <- "mechanical - sandpaper"

d$scarifTypeA[which(d$scarifTypeA == "mechanical with razor")] <- "mechanical - razor"
d$scarifTypeA[which(d$scarifTypeA == "H2SO4")] <- "chemical - H2SO4"


# group more generally --- chemical vs mechanical
d$scarifTypeB <- d$scarif.type
# mechanical 
d$scarifTypeB[which(d$scarifTypeB == "Mechanical")] <- "mechanical"
d$scarifTypeB[which(d$scarifTypeB == "sandpaper")] <- "mechanical"
d$scarifTypeB[which(d$scarifTypeB == "sand paper (Np. 150)")] <- "mechanical"
d$scarifTypeB[which(d$scarifTypeB == "mechanical with razor")] <- "mechanical"
d$scarifTypeB[which(d$scarifTypeB == "coat removal")] <- "mechanical"
d$scarifTypeB[which(d$scarifTypeB == "pericarp removal")] <- "mechanical"
d$scarifTypeB[which(d$scarifTypeB == "seed polishing")] <- "mechanical"
d$scarifTypeB[which(d$scarifTypeB == "trimmed pappus")] <- "mechanical"
d$scarifTypeB[which(d$scarifTypeB == "pelleted")] <- "mechanical"
d$scarifTypeB[which(d$scarifTypeB == "seed coat removed")] <- "mechanical"
d$scarifTypeB[which(d$scarifTypeB == "Seed coat removed")] <- "mechanical"
d$scarifTypeB[which(d$scarifTypeB == "burned")] <- "mechanical"
d$scarifTypeB[which(d$scarifTypeB == "scapel")] <- "mechanical"
d$scarifTypeB[which(d$scarifTypeB == "scalpel on lenticular side")] <- "mechanical"
d$scarifTypeB[which(d$scarifTypeB == "manual with needle at the cotyledon end.")] <- "mechanical"

# chemical
d$scarifTypeB[which(d$scarifTypeB == "Chemical")] <- "chemical"
d$scarifTypeB[which(d$scarifTypeB == "H2SO4")] <- "chemical"
d$scarifTypeB[which(d$scarifTypeB == "H2SO4.98per.0min")] <- "chemical"
d$scarifTypeB[which(d$scarifTypeB == "H2SO4.98per.10min")] <- "chemical"
d$scarifTypeB[which(d$scarifTypeB == "H2SO4.98per.20min")] <- "chemical"
d$scarifTypeB[which(d$scarifTypeB == "H2SO4.98per.30min")] <- "chemical"
d$scarifTypeB[which(d$scarifTypeB == "acid scarification")] <- "chemical"
d$scarifTypeB[which(d$scarifTypeB == "acid")] <- "chemical"
d$scarifTypeB[which(d$scarifTypeB == "NaClO")] <- "chemical"


### Check unusual entries--- 
### Y, cold, seed coat, partly scarified, use temerpature to permeate seedcoat,
### partial scarification of seed coat, soaking in water
# soaking---wrong column? is this da in the soaking column?
# is hot water scarification vs soaking---to dicuss as a group