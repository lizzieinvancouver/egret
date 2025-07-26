### Started by Dan April 7 2025
### Then taken over (with a lot of enthusiasm) by Victor, 25 July 2025

sort(unique(d$chemical.concent))
length(unique(d$chemical.concent))

### Make a cleaned column
d$chemicalConcent <- d$chemical.concent
suppressWarnings(d$chemicalConcent <- as.numeric(d$chemicalConcent)) ### this should make all non numeric values NA
table(is.na(d$chemicalConcent))
cols_tocheck <- c("chemical", 
    "chemical.concent",
    "soaking",
    "soaked.in",
    "soaking.duration",
    "chemicalCor",
    "chemicalConcent") # chosen by Dan, changed to names by Lizzie

### First clean the NA's
temp <- d[is.na(d$chemicalConcent),cols_tocheck]
nrow(temp)

### Make anything with water as concentration = 0 (so that way it won't get dropped)
d$chemicalConcent[which(d$chemicalCor == "H2O")] <- 0
temp <- d[is.na(d$chemicalConcent),]
nrow(temp)

### Now clean zero values
equivalents_0 <- c("0 (control)", "0%", "0 mM", "0, 0")
d[which(d$chemical.concent %in% equivalents_0), 'chemicalConcent'] <- 0
temp <- d[is.na(d$chemicalConcent),]
nrow(temp)

### This is a biggie, if they are true NA (ie no chemical provided make them 0 so they dont get dropped)
d[which(is.na(d$chemical.concent) & is.na(d$chemicalCor)), 'chemicalConcent'] <- 0
temp <- d[is.na(d$chemicalConcent),cols_tocheck]
nrow(temp)

### Make a unit column
d$chemicalConcentUnit <- NA

### check
unique(d$chemicalConcent)

### millimoles
## it is NOT a concentration unit, so we assume mmol/L
unique(subset(temp, grepl("mM", temp$chemical.concent))$chemical.concent)
d[which(d$chemical.concent == "10 mM"), c('chemicalConcent', 'chemicalConcentUnit')] <- rep(c(10, 'mmol.L^-1'), each = length((which(d$chemical.concent == "10 mM"))))
d[which(d$chemical.concent == "0.029 mM"), c('chemicalConcent', 'chemicalConcentUnit')] <- rep(c(0.029, 'mmol.L^-1'), each = length((which(d$chemical.concent == "0.029 mM"))))
d[which(d$chemical.concent == "0.089 mM"), c('chemicalConcent', 'chemicalConcentUnit')] <- rep(c(0.089, 'mmol.L^-1'), each = length((which(d$chemical.concent == "0.089 mM"))))
d[which(d$chemical.concent == "2.877 mM"), c('chemicalConcent', 'chemicalConcentUnit')] <- rep(c(2.877, 'mmol.L^-1'), each = length((which(d$chemical.concent == "2.877 mM"))))
d[which(d$chemical.concent == "0.289 mM"), c('chemicalConcent', 'chemicalConcentUnit')] <- rep(c(0.289, 'mmol.L^-1'), each = length((which(d$chemical.concent == "0.289 mM"))))

### parts per million, with only one chemical
unique(subset(temp, grepl("ppm", temp$chemical.concent))$chemical.concent)
d[which(d$chemical.concent == "1000ppm"), c('chemicalConcent', 'chemicalConcentUnit')] <- rep(c(1000, 'ppm'), each = length((which(d$chemical.concent == "1000ppm"))))
d[which(d$chemical.concent == "4000ppm"), c('chemicalConcent', 'chemicalConcentUnit')] <- rep(c(4000, 'ppm'), each = length((which(d$chemical.concent == "4000ppm"))))
d[which(d$chemical.concent == "0.31 ppm (sucrose)"), c('chemicalConcent', 'chemicalConcentUnit')] <- rep(c(0.31, 'ppm'), each = length((which(d$chemical.concent == "0.31 ppm (sucrose)"))))
d[which(d$chemical.concent == "0.46 ppm (sucrose)"), c('chemicalConcent', 'chemicalConcentUnit')] <- rep(c(0.46, 'ppm'), each = length((which(d$chemical.concent == "0.46 ppm (sucrose)"))))

### molecular weight 
## it is NOT a concentration unit! In battaglia93, PEG was used to modify the water potential (see Figure 9)
## water potential values were entered in soaked.in columns, so I'm deleting this
unique(subset(temp, grepl("MW", temp$chemical.concent))$chemical.concent)
d[which(d$chemical.concent == "6000 MW"), c('chemicalConcent', 'chemicalConcentUnit')] <- rep(c(0, NA), each = length((which(d$chemical.concent == "6000 MW"))))
d[which(d$chemical.concent == "6000 MW"), c('chemicalCor')] <- NA

### volumetric percentage
## should be %v/v
unique(subset(temp, grepl("v/v", temp$chemical.concent))$chemical.concent)
pattern <- "diluted at concentration 1:500 v/v"
d[which(d$chemical.concent == pattern), c('chemicalConcent', 'chemicalConcentUnit')] <- rep(c(0.002, '%v/v'), each = length((which(d$chemical.concent == pattern))))

### just standardize the way we write molecular concentration
## and since there is two chemicals in this study, the concentration should be NA+1.4
unique(subset(temp, grepl("mmol", temp$chemical.concent))$chemical.concent)
pattern <- "1.4 mmol liter^-1"
d[which(d$chemical.concent == pattern), c('chemicalConcent', 'chemicalConcentUnit')] <- rep(c('NA+1.4', 'mmol.L^-1'), each = length((which(d$chemical.concent == pattern))))

### percentage, without further specification (i.e. volumetric or mass/mass or mass/volume)
unique(subset(temp, grepl("%", temp$chemical.concent))[, c('chemical.concent', 'chemicalCor')])
## for H2So4, we can assume it's v/v
d[which(d$chemical.concent == '50%' & d$chemicalCor == 'H2SO4'), c('chemicalConcent', 'chemicalConcentUnit')] <- 
  rep(c(50, '%v/v'), each = length((which(d$chemical.concent == '50%' & d$chemicalCor == 'H2SO4'))))
d[which(d$chemical.concent == '98%' & d$chemicalCor == 'H2SO4'), c('chemicalConcent', 'chemicalConcentUnit')] <- 
  rep(c(98, '%v/v'), each = length((which(d$chemical.concent == '98%' & d$chemicalCor == 'H2SO4'))))
## for NaCl, we can assume it's m/v
d[which(d$chemical.concent == '5%' & d$chemicalCor == 'NaClO'), c('chemicalConcent', 'chemicalConcentUnit')] <- 
  rep(c(5, '%m/v'), each = length((which(d$chemical.concent == '5%' & d$chemicalCor == 'NaClO'))))
d[which(d$chemical.concent == '0.50%' & d$chemicalCor == 'NaCl'), c('chemicalConcent', 'chemicalConcentUnit')] <- 
  rep(c(0.50, '%m/v'), each = length((which(d$chemical.concent == '0.50%' & d$chemicalCor == 'NaCl'))))
d[which(d$chemical.concent == '1%' & d$chemicalCor == 'NaCl'), c('chemicalConcent', 'chemicalConcentUnit')] <- 
  rep(c(1, '%m/v'), each = length((which(d$chemical.concent == '1%' & d$chemicalCor == 'NaCl'))))
d[which(d$chemical.concent == '2%' & d$chemicalCor == 'NaCl'), c('chemicalConcent', 'chemicalConcentUnit')] <- 
  rep(c(2, '%m/v'), each = length((which(d$chemical.concent == '2%' & d$chemicalCor == 'NaCl'))))
d[which(d$chemical.concent == '3%' & d$chemicalCor == 'NaCl'), c('chemicalConcent', 'chemicalConcentUnit')] <- 
  rep(c(3, '%m/v'), each = length((which(d$chemical.concent == '3%' & d$chemicalCor == 'NaCl'))))

### microns
## again, it is NOT a concentration unit!
## in the paper, it's written microMoles, which is again not a concentration...
## we assume it is micromol.L^-1 
unique(subset(temp, grepl("microns", temp$chemical.concent))[, c('chemical.concent', 'chemicalCor')])
d[which(d$chemical.concent == "25 microns"), c('chemicalConcent', 'chemicalConcentUnit')] <- rep(c(25, 'micromol.L^-1 '), 
                                                                                                 each = length((which(d$chemical.concent == "25 microns"))))
d[which(d$chemical.concent == "250 microns"), c('chemicalConcent', 'chemicalConcentUnit')] <- rep(c(250, 'micromol.L^-1 '), 
                                                                                                 each = length((which(d$chemical.concent == "250 microns"))))
d[which(d$chemical.concent == "2500 microns"), c('chemicalConcent', 'chemicalConcentUnit')] <- rep(c(2500, 'micromol.L^-1 '), 
                                                                                                  each = length((which(d$chemical.concent == "2500 microns"))))

### parts per million, with two chemicals
unique(subset(temp, grepl("ppm", temp$chemical.concent))[, c('chemical.concent', 'chemicalCor')])
## there is one obvious mistake, where no GA3 was not used at all in one of the treatment
d[which(d$chemical.concent == "0.31 ppm (sucrose) + 0.47 ppm (GA3)" & d$chemicalCor == 'agar.media.culture+sucrose'), 
  c('chemicalConcent', 'chemicalConcentUnit')] <- rep(c(0.31, 'ppm'), each = 3)
## and then easy fix
d[which(d$chemical.concent == "0.46 ppm (sucrose) + 0.47 ppm (GA3)" & d$chemicalCor == 'agar.media.culture+sucrose+GA3'), 
  c('chemicalConcent', 'chemicalConcentUnit')] <- rep(c('0.46+0.47', 'ppm+ppm'), each = 3)

### grams
## again, NOOOT a concentration unit! In acosta13, PEG was used to modify the water potential
## water potential values were entered in treatmentDetails columns, so I'm deleting this
unique(subset(temp, grepl("g", temp$chemical.concent))[, c('chemical.concent', 'chemicalCor')])
d[which(d$chemical.concent %in% c("154g", "191g", "230g", "297g", "350g")), c('chemicalConcent', 'chemicalConcentUnit')] <- rep(c(0, NA), each = 5)
d[which(d$chemical.concent %in% c("154g", "191g", "230g", "297g", "350g")), c('chemicalCor')] <- NA

### check
temp <- d[is.na(d$chemicalConcent),cols_tocheck]
nrow(temp)

### not let's deal with cases where we have several concentrations (and no units)
rows_of_interest <- which(is.na(d$chemicalConcent) & !is.na(stringr::str_split_i(d$chemical.concent, pattern = '[,+/]', i = 2)))
for(r in rows_of_interest){
  chemical.concent.r <- d[r, 'chemical.concent']
  chemical.concent.vec <- stringr::str_trim(unlist(stringr::str_split(chemical.concent.r, pattern = '[,+/]')))
  chemical.concent.vec <- stringr::str_split_i(chemical.concent.vec, pattern = '[ (]', i = 1) # in case the name is in parenthesis
  d[r, 'chemicalConcent'] <- paste0(chemical.concent.vec, collapse = '+')
}

### some cases where the name of the chemical is in parenthesis in the concentration column
rows_of_interest <- which(is.na(d$chemicalConcent) & !is.na(stringr::str_split_i(d$chemical.concent, pattern = '[(]', i = 2)))
d[rows_of_interest, 'chemicalConcent'] <- stringr::str_split_i(d[rows_of_interest, 'chemical.concent'], pattern = '[ (]', i = 1)

### a case where concentration is "concentrated"
## thanks buddy, very helpful, I will keep the NA for now
# d[d$chemical.concent %in% "\"concentrated\"", 'chemicalConcent']

### and finally a case where concentration = 'H2SO4'
## yeeek, chemical is = 'Y'... I don't know what happen, but chemicalCor is good
## and in the paper, it is written "98% sulfuric acid"---I assume again it's volumetric percentage
d[d$chemical.concent %in% "H2SO4" & d$datasetID == 'olmez08', 'chemicalConcent'] <- 98
d[d$chemical.concent %in% "H2SO4" & d$datasetID == 'olmez08', 'chemicalConcentUnit'] <- '%v/v'

### So now, let's look at the values, to see if they make sense!

### We start by looking only at the rows where we have only one chemical
temp <- d[which(!grepl("[+]", d$chemicalConcent)),]
temp <- temp[temp$chemicalConcent != 0, ]
temp$chemicalConcent <- as.numeric(temp$chemicalConcent)

### We are confident that the values were entered correctly
### and we will not use the concentration values per se
### but only the different concentration treatments 
### in our decision rules

### So we decidedwe're happy with this (partial) cleaning

