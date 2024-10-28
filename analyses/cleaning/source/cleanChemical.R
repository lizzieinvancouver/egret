## Upded 18 March 2024 ##
## By Mao ##
## Upded 30 January 2024 ##
## By Deirdre ##

## This contains code to clean the chemical column ##
## Because it is ONE word, we name the cleaned column chemicalCor to show it's corrected #
## (Otherwise cleaned columns are in camelCase.)
## Original code taken from file called cleaningDL.R ##

#1. Chemical---type
unique(d$chemical)

d$chemicalCor <- d$chemical
#Added type of chemicals for those with commercial names
d$chemicalCor[which(d$chemicalCor == "Water")] <- "H2O"
d$chemicalCor[which(d$chemicalCor == "H20")] <- "H2O"
d$chemicalCor[which(d$chemicalCor == "hydrogen peroxide")] <- "H2O2"
d$chemicalCor[which(d$chemicalCor == "control.water")] <- "H2O"
d$chemicalCor[which(d$chemicalCor == "hydrogen.peroxide")] <- "H2O2"
d$chemicalCor[which(d$chemicalCor == "citric.acid/GA3")] <- "citric.acid+GA3"
d$chemicalCor[which(d$chemicalCor == "water")] <- "H2O"
d$chemicalCor[which(d$chemicalCor == "poly ethylene glycol")] <- "PEG"
#For these treatments, I move the treatment time to the trt.duration column
d$trt.duration[which(d$chemicalCor == "NA ClO - 5min")] <- "0.0035"
d$chemicalCor[which(d$chemicalCor == "NA ClO - 5min")] <- "NaClO"
d$trt.duration[which(d$chemicalCor == "NA ClO - 10min")] <- "0.0069"
d$chemicalCor[which(d$chemicalCor == "NA ClO - 10min")] <- "NaClO"
d$trt.duration[which(d$chemicalCor == "NA ClO - 30min")] <- "0.0208"
d$chemicalCor[which(d$chemicalCor == "NA ClO - 30min")] <- "NaClO"
d$trt.duration[which(d$chemicalCor == "NA ClO - 60min")] <- "0.0417"
d$chemicalCor[which(d$chemicalCor == "NA ClO - 60min")] <- "NaClO"
d$trt.duration[which(d$chemicalCor == "NA ClO - 90min")] <- "0.0625"
d$chemicalCor[which(d$chemicalCor == "NA ClO - 90min")] <- "NaClO"
d$trt.duration[which(d$chemicalCor == "NA ClO - 120min")] <- "0.0833"
d$chemicalCor[which(d$chemicalCor == "NA ClO - 120min")] <- "NaClO"
d$trt.duration[which(d$chemicalCor == "NA ClO - 180min")] <- "0.125"
d$chemicalCor[which(d$chemicalCor == "NA ClO - 180min")] <- "NaClO"
d$chemicalCor[which(d$chemicalCor == "Phosfert ")] <- "Phosfert(P_fertilizer)"
d$chemicalCor[which(d$chemicalCor == "phosphertt+bioplin ")] <- "Phosfert+Bioplin(N_fertilizer)"
d$chemicalCor[which(d$chemicalCor == "Thiourea")] <- "thiourea"
d$chemicalCor[which(d$chemicalCor == "Kinetin")] <- "kinetin(hormone)"
d$chemicalCor[which(d$chemicalCor == "Y")] <- "H2SO4"
d$chemicalCor[which(d$chemicalCor == "Salicylic acid")] <- "salicylic.acid"
d$chemicalCor[which(d$chemicalCor == "smoke water")] <- "smoke-water"
d$chemicalCor[which(d$chemicalCor == "KNO3 + KCl")] <- "KNO3+KCl"
d$chemicalCor[which(d$chemicalCor == "NH4Cl + KCl")] <- "NH4Cl+KCl"
d$chemicalCor[which(d$chemicalCor == "GA3, KNO3")] <- "GA3+KNO3"
d$chemicalCor[which(d$chemicalCor == "GA3, Kin")] <- "GA3+kinetin"
d$chemicalCor[which(d$chemicalCor == "GA3, BAP")] <- "GA3+BAP(hormone)"
d$chemicalCor[which(d$chemicalCor == "Kin, BAP")] <- "kinetin+BAP(hormone)"
d$chemicalCor[which(d$chemicalCor == "Sucrose, KNO3")] <- "Sucrose+KNO3"
d$chemicalCor[which(d$chemicalCor == "Half-strength M+S basalt salt, vitamins, sucrose ")] <- "Half-strength.M+S+vitamins+sucrose"
d$chemicalCor[which(d$chemicalCor == "NaClO, GA3")] <- "NaClO+GA3"
d$chemicalCor[which(d$chemicalCor == "NaClO, GA3, KNO3")] <- "NaClO+GA3+KNO3"
d$chemicalCor[which(d$chemicalCor == "NaClO, KNO3")] <- "NaClO+KNO3"
d$chemicalCor[which(d$chemicalCor == "GA + IBA")] <- "GA+IBA(hormone)"
d$chemicalCor[which(d$chemicalCor == "GA + BA")] <- "GA+BAP(hormone)"
d$chemicalCor[which(d$chemicalCor == "BA + IBA")] <- "BAP(hormone)+IBA(hormone)"
d$chemicalCor[which(d$chemicalCor == "BA + NAA")] <- "BAP(hormone)+NAA(hormone)"
d$chemicalCor[which(d$chemicalCor == "NAA + IBA")] <- "NAA(hormone)+IBA(hormone)"
d$chemicalCor[which(d$chemicalCor == "GA + BA + NAA")] <- "GA+BAP(hormone)+NAA(hormone)"
d$chemicalCor[which(d$chemicalCor == "GA + IBA + BA")] <- "GA+IBA(hormone)+BAP(hormone)"
d$chemicalCor[which(d$chemicalCor == "GA + NAA + IBA")] <- "GA+NAA(hormone)+IBA(hormone)"
d$chemicalCor[which(d$chemicalCor == "BA + NAA + IBA")] <- "BAP(hormone)+NAA(hormone)+IBA(hormone)"
d$chemicalCor[which(d$chemicalCor == "GA + IBA + NAA + BA")] <- "GA+IBA(hormone)+NAA(hormone)+BAP(hormone)"
d$chemicalCor[which(d$chemicalCor == "Fluridon ")] <- "Fluridon(herbicide)"
d$chemicalCor[which(d$chemicalCor == "NaOCl")] <- "NaClO"
d$chemicalCor[which(d$chemicalCor == "Agar media culture + Sucrose")] <- "agar.media.culture+sucrose"
d$chemicalCor[which(d$chemicalCor == "Agar media culture + Sucrose + GA3")] <- "agar.media.culture+sucrose+GA3"
d$chemicalCor[which(d$chemicalCor == "GA3, PRO")] <- "GA3+PRO(hormone)"
d$chemicalCor[which(d$chemicalCor == "NOVA, MAX")] <- "NOVA(growth_regulator)+MAX(growth_regulator)"
d$chemicalCor[which(d$chemicalCor == "NOVA, MAX, PRO")] <- "NOVA(growth_regulator)+MAX(growth_regulator)+PRO(growth_regulator)"
d$chemicalCor[which(d$chemicalCor == "GA3, 6-BA")] <- "GA3+BAP(hormone)"
d$chemicalCor[which(d$chemicalCor == "E")] <- "ethephon(growth_regulator)"
d$chemicalCor[which(d$chemicalCor == "K")] <- "kinetin"
d$chemicalCor[which(d$chemicalCor == "GA + K")] <- "GA+kinetin"
d$chemicalCor[which(d$chemicalCor == "GA + E")] <- "GA+ethephon(growth_regulator)"
d$chemicalCor[which(d$chemicalCor == "K + E")] <- "kinetin+ethephon(growth_regulator)"
d$chemicalCor[which(d$chemicalCor == "GA + E + K")] <- "GA+ethephon(growth_regulator)+kinetin"
d$chemicalCor[which(d$chemicalCor == "N")] <- NA
d$chemicalCor[which(d$chemicalCor == "captan + GA3")] <- "captan(fungicide)+GA3"
d$chemicalCor[which(d$chemicalCor == "Captan fungicide")] <- "captan(fungicide)"
d$chemicalCor[which(d$chemicalCor == "KT")] <- "kinetin"
d$chemicalCor[which(d$chemicalCor == "sodium hypochlorite")] <- "NaClO"
d$chemicalCor[which(d$chemicalCor == "Milstop fungicide")] <- "MilStop(fungicide)"
d$chemicalCor[which(d$chemicalCor == "Ethephon")] <- "ethephon(growth_regulator)"
d$chemicalCor[which(d$chemicalCor == "H20")] <- "H2O"
d$chemicalCor[which(d$chemicalCor == "H21")] <- "H2O"
d$chemicalCor[which(d$chemicalCor == "H22")] <- "H2O"
d$chemicalCor[which(d$chemicalCor == "polyethylene glycol")] <- "PEG"
d$chemicalCor[which(d$chemicalCor == "plant preservation solution")] <- "plant.preservation.solution"
d$chemicalCor[which(d$chemicalCor == "sulphuric acid (H2SO4)")] <- "H2SO4"
d$chemicalCor[which(d$chemicalCor == "Potassium Nitrate (KNO3)")] <- "KNO3"
d$chemicalCor[which(d$chemicalCor == "ddH2O")] <- "H2O"
d$chemicalCor[which(d$chemicalCor == "NACL")] <- "NaCl"
d$chemicalCor[which(d$chemicalCor == "log2(red/far.red)")] <- NA

unique(d$chemicalCor)
