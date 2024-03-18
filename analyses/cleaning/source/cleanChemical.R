## Upded 18 March 2024 ##
## By Mao ##
## Upded 30 January 2024 ##
## By Deirdre ##

## This contains code to clean chill duration and temperature ##
## Original code taken from file called cleaningDL.R ##

#1. Chemical---type
unique(d$chemical)

d$chemicalCor <- d$chemical
d$chemicalCor[which(d$checmicalCor == "Water")] <- "H2O"
d$chemicalCor[which(d$checmicalCor == "H20")] <- "H2O"
d$chemicalCor[which(d$checmicalCor == "hydrogen peroxide")] <- "H2O2"
d$chemicalCor[which(d$checmicalCor == "control.water")] <- "H2O"
d$chemicalCor[which(d$checmicalCor == "hydrogen.peroxide")] <- "H2O2"
d$chemicalCor[which(d$checmicalCor == "citric.acid/GA3")] <- "citric.acid+GA3"
d$chemicalCor[which(d$checmicalCor == "water")] <- "H2O"
d$chemicalCor[which(d$checmicalCor == "poly ethylene glycol")] <- "PEG"
d$chemicalCor[which(d$checmicalCor == "NA ClO - 5min")] <- "NaClO - 5min"
d$chemicalCor[which(d$checmicalCor == "NA ClO - 10min")] <- "NaClO - 10min"
d$chemicalCor[which(d$checmicalCor == "NA ClO - 30min")] <- "NaClO - 30min"
d$chemicalCor[which(d$checmicalCor == "NA ClO - 60min")] <- "NaClO - 60min"
d$chemicalCor[which(d$checmicalCor == "NA ClO - 90min")] <- "NaClO - 90min"
d$chemicalCor[which(d$checmicalCor == "NA ClO - 120min")] <- "NaClO - 120min"
d$chemicalCor[which(d$checmicalCor == "NA ClO - 180min")] <- "NaClO - 180min"
d$chemicalCor[which(d$checmicalCor == "Phosfert ")] <- "Phosfert"
d$chemicalCor[which(d$checmicalCor == "phosphertt+bioplin ")] <- "Phosfert+Bioplin"
d$chemicalCor[which(d$checmicalCor == "Thiourea")] <- "thiourea"
d$chemicalCor[which(d$checmicalCor == "Kinetin")] <- "kinetin"
d$chemicalCor[which(d$checmicalCor == "Y")] <- "H2SO4"
d$chemicalCor[which(d$checmicalCor == "Salicylic acid")] <- "salicylic.acid"
d$chemicalCor[which(d$checmicalCor == "smoke water")] <- "smoke-water"
d$chemicalCor[which(d$checmicalCor == "KNO3 + KCl")] <- "KNO3+KCl"
d$chemicalCor[which(d$checmicalCor == "NH4Cl + KCl")] <- "NH4Cl+KCl"
d$chemicalCor[which(d$checmicalCor == "GA3, KNO3")] <- "GA3+KNO3"
d$chemicalCor[which(d$checmicalCor == "GA3, Kin")] <- "GA3+Kin"
d$chemicalCor[which(d$checmicalCor == "GA3, BAP")] <- "GA3+BAP"
d$chemicalCor[which(d$checmicalCor == "Kin, BAP")] <- "Kin+BAP"
d$chemicalCor[which(d$checmicalCor == "Sucrose, KNO3")] <- "Sucrose+KNO3"
d$chemicalCor[which(d$checmicalCor == "Half-strength M+S basalt salt, vitamins, sucrose ")] <- "Half-strength.M+S+vitamins+sucrose"
d$chemicalCor[which(d$checmicalCor == "NaClO, GA3")] <- "NaClO+GA3"
d$chemicalCor[which(d$checmicalCor == "NaClO, GA3, KNO3")] <- "NaClO+GA3+KNO3"
d$chemicalCor[which(d$checmicalCor == "NaClO, KNO3")] <- "NaClO+KNO3"
d$chemicalCor[which(d$checmicalCor == "GA + IBA")] <- "GA+IBA"
d$chemicalCor[which(d$checmicalCor == "GA + BA")] <- "GA+BA"
d$chemicalCor[which(d$checmicalCor == "BA + IBA")] <- "BA+IBA"
d$chemicalCor[which(d$checmicalCor == "BA + NAA")] <- "BA+NAA"
d$chemicalCor[which(d$checmicalCor == "NAA + IBA")] <- "NAA+IBA"
d$chemicalCor[which(d$checmicalCor == "GA + BA + NAA")] <- "GA+BA+NAA"
d$chemicalCor[which(d$checmicalCor == "GA + IBA + BA")] <- "GA+IBA+BA"
d$chemicalCor[which(d$checmicalCor == "GA + NAA + IBA")] <- "GA+NAA+IBA"
d$chemicalCor[which(d$checmicalCor == "BA + NAA + IBA")] <- "BA+NAA+IBA"
d$chemicalCor[which(d$checmicalCor == "GA + IBA + NAA + BA")] <- "GA+IBA+NAA+BA"
d$chemicalCor[which(d$checmicalCor == "Fluridon ")] <- "Fluridon"
d$chemicalCor[which(d$checmicalCor == "NaOCl")] <- "NaClO"
d$chemicalCor[which(d$checmicalCor == "Agar media culture + Sucrose")] <- "agar.media.culture+sucrose"
d$chemicalCor[which(d$checmicalCor == "Agar media culture + Sucrose + GA3")] <- "agar.media.culture+sucrose+GA3"
d$chemicalCor[which(d$checmicalCor == "GA3, PRO")] <- "GA3+PRO"
d$chemicalCor[which(d$checmicalCor == "NOVA, MAX")] <- "NOVA+MAX"
d$chemicalCor[which(d$checmicalCor == "NOVA, MAX, PRO")] <- "NOVA+MAX+PRO"
d$chemicalCor[which(d$checmicalCor == "GA3, 6-BA")] <- "GA3+6-BA"
d$chemicalCor[which(d$checmicalCor == "E")] <- "ethephon"
d$chemicalCor[which(d$checmicalCor == "K")] <- "kinetin"
d$chemicalCor[which(d$checmicalCor == "GA + K")] <- "GA+kinetin"
d$chemicalCor[which(d$checmicalCor == "GA + E")] <- "GA+ethephon"
d$chemicalCor[which(d$checmicalCor == "K + E")] <- "kinetin+ethephon"
d$chemicalCor[which(d$checmicalCor == "GA + E + K")] <- "GA+ethephon+kinetin"
d$chemicalCor[which(d$checmicalCor == "N")] <- "No"
d$chemicalCor[which(d$checmicalCor == "captan + GA3")] <- "captan+GA3"
d$chemicalCor[which(d$checmicalCor == "Captan fungicide")] <- "captan"
d$chemicalCor[which(d$checmicalCor == "KT")] <- "kinetin"
d$chemicalCor[which(d$checmicalCor == "sodium hypochlorite")] <- "NaClO"
d$chemicalCor[which(d$checmicalCor == "Milstop fungicide")] <- "MilStop"
d$chemicalCor[which(d$checmicalCor == "Ethephon")] <- "ethephon"
d$chemicalCor[which(d$checmicalCor == "H20")] <- "H2O"
d$chemicalCor[which(d$checmicalCor == "H21")] <- "H2O"
d$chemicalCor[which(d$checmicalCor == "H22")] <- "H2O"
d$chemicalCor[which(d$checmicalCor == "NA ")] <- "No"
d$chemicalCor[which(d$checmicalCor == "polyethylene glycol")] <- "PEG"
d$chemicalCor[which(d$checmicalCor == "plant preservation solution")] <- "plant.preservation.solution"
d$chemicalCor[which(d$checmicalCor == "sulphuric acid (H2SO4)")] <- "H2SO4"
d$chemicalCor[which(d$checmicalCor == "Potassium Nitrate (KNO3)")] <- "KNO3"
d$chemicalCor[which(d$checmicalCor == "ddH2O")] <- "H2O"
d$chemicalCor[which(d$checmicalCor == "NACL")] <- "NaCl"

#What should we use for no chemical?: NA? No?

#Check unusual values:
# log2(red/far.red)--this is not chemical
# control---is it water?
