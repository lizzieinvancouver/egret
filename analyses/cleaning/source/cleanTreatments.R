## 26 September 2024 ##
# By Christophe

# Code to clean the treatment categories 

# Create new column of treatment category of cleaned treatments
d$treatmentCor <- d$treatment
d$treatmentDetails <- NA

#unique(d$treatment)
#unique(d$treatmentCor)
#unique(d$treatmentDetails)

prio <- read.csv("studyDesign/ids_for_Ken/ids_after_allworkflow.csv")
prio <- prio[order(prio$datasetID, prio$study, prio$genusspecies),]

# Find data points
idx <- which(d$datasetID == sort(unique(d$datasetID))[1])
check <- d[idx,]
check_short <- subset(check, select = c("datasetID", "study", "species", "treatment", "other.treatment",
                                        "treatmentCor", "treatmentDetails",
                                        "storageType", "storageDetails", "storageTemp", "storageDuration",
                                        "chillTemp", "chillDuration", "chillTempCycle", "chillLightCycle",
                                        "germTemp", "germDuration", "photoperiodCor",
                                        "scarification", "scarifType", "scarifTypeGen", "scarifTypeSpe",
                                        "soaking", "soaked.in", "soaking.duration",
                                        "chemicalCor", "trt.duration",
                                        "respvar", "response", "figure"))

#acosta13
d$treatmentCor[which(d$datasetID == "acosta13" & d$treatment == "temperature")] <- "germination temperature"
temp <- c(0, -0.3, -0.4, -0.6, -0.9, -1.3)
d$treatmentDetails[which(d$datasetID == "acosta13" & d$treatment == "osmotic potential")] <- paste0(temp, " MPa")
d$treatmentCor[which(d$datasetID == "acosta13" & d$treatment == "dry storage")] <- "storage x photoperiod"

#ahola99
d$treatmentCor[which(d$datasetID == "ahola99")] <- "r/fr light x moist chilling x germination temperature"

#ahmad07, exp2 treatment is chilling but germination data is during chilling

#aiello07
#possible missing data on gibberelic acid treatments in Table 3 and Table 6
d$treatmentCor[which(d$datasetID == "aiello07")] <- "population x cold stratification"

#airi09
temp <- c(rep(" x control", 5), rep(" x cold stratification", 15), rep(" x chemical", 15),
          rep(" x hormone", 15), rep(" x chemical", 15))
d$treatmentCor[which(d$datasetID == "airi09" & d$figure == "Table 2")] <- paste0("population", temp)

temp <- rep(c("control", "cold stratification", "hormone", "chemica", "chemical"), each = 5)
d$treatmentCor[which(d$datasetID == "airi09" & d$figure == "Table 3")] <- paste0("population", temp)

#al-absi10
temp <- c(rep("hot water", 16), rep("acid", 16), rep("hormone", 16))
d$treatmentCor[which(d$datasetID == "al-absi10")] <- paste0("cold stratification x ", temp)
#soaking duration needs to be cleaned, "120" instead of 0 - 30 minutes used in paper

#albrecht20
d$treatmentCor[which(d$datasetID == "albrecht20")] <- "cold stratification x germination temperature x photoperiod"
#weird shared data between experiments

#aldridge92
d$treatmentCor[which(d$datasetID == "aldridge92")] <- "moisture content x stratification x hormone"

#alhelal96
d$treatmentCor[which(d$datasetID == "alhelal96" & d$study == "exp1")] <- "soaking temperature x hormone x photoperiod"
d$treatmentCor[which(d$datasetID == "alhelal96" & d$study == "exp2")] <- "germination temperature"

#alptekin02
d$treatmentCor[which(d$datasetID == "alptekin02")] <- "population x stratification x scarification"

#amini18, paper pending

#amooaghaie09
d$treatmentCor[which(d$datasetID == "amooaghaie09")] <- "moist chilling x hormone"

#arslan11
d$treatmentCor[which(d$datasetID == "arslan11")] <- "moist chilling x hormone x photoperiod x germination temperature"
d$treatmentCor[which(d$datasetID == "arslan11" & d$figure == "table 1")]
# i think the rest of table 1, except the one with scarification, should be removed

#barnhill82
d$treatmentCor[which(d$datasetID == "barnhill82")] <- "stratification x germination temperature x photoperiod"
# add strat temp 3degC

#barros12
d$treatmentCor[which(d$datasetID == "barros12")] <- "stratification x hormone x germination temperature"

#basaran11, paper pending

#basbag09
d$treatmentCor[which(d$datasetID == "basbag09")] <- "germination temperature x germination duration"

#batlla03
d$treatmentCor[which(d$datasetID == "batlla03" & d$study != "exp3")] <- "germination temperature x germination duration"
d$treatmentCor[which(d$datasetID == "batlla03" & d$study == "exp3")] <- "storage"

#battaglia93
d$treatmentCor[which(d$datasetID == "battaglia93" & !(d$figure %in% c("Figure 8", "Figure 9")))] <-
  "seedlot x stratification x germination temperature"
d$treatmentCor[which(d$datasetID == "battaglia93" & d$figure == "Figure 8")] <- "stratification x high temperature"

#battaglia97
d$treatmentCor[which(d$datasetID == "battaglia97")] <- "seedlot x stratification x germination temperature"

#beikmohammadi12, various
temp <- c("control", "soaking", "soaking (hot water)", rep("acid", 3),
          rep("soaking (hormone)", 3), rep("cold stratification", 2),
          rep("acid x soaking (hormone)", 4))
d$treatmentCor[which(d$datasetID == "beikmohammadi12")] <- temp

#bhatt00, various
temp <- c("control", "soaking", "soaking (hot water)", "acid", "scarification",
          "hormone", "prechilling")
d$treatmentCor[which(d$datasetID == "bhatt00" & d$figure == "Figure 1")] <- temp
d$treatmentCor[which(d$datasetID == "bhatt00" & d$figure == "Table3")] <-
  "soaking x population"
d$treatmentCor[which(d$datasetID == "bhatt00" & d$figure == "Table4")] <-
  "soaking (hot water) x population"
d$treatmentCor[which(d$datasetID == "bhatt00" & d$figure == "Table5")] <-
  "acid x population"
d$treatmentCor[which(d$datasetID == "bhatt00" & d$figure == "Table6")] <-
  "scarification x population"
d$treatmentCor[which(d$datasetID == "bhatt00" & d$figure == "Table7")] <-
  "hormone x scarification x population"
d$treatmentCor[which(d$datasetID == "bhatt00" & d$figure == "Table8")] <-
  "prechilling x population"

#bibby53, supposed to be removed

#borghetti86
d$treatmentCor[which(d$datasetID == "borghetti86")] <- "seedlot x cold-moist stratification x germination temperature"

#boscagli01, various
temp <- c("control", rep("ultrasound", 2))
d$treatmentCor[which(d$datasetID == "boscagli01" & d$figure == "Fig 1")] <- temp
temp <- c("control", rep("prechilling x piercing", 3))
d$treatmentCor[which(d$datasetID == "boscagli01" & d$figure == "Fig 2")] <- temp
temp <- c("control", rep("soaking x piercing", 4))
d$treatmentCor[which(d$datasetID == "boscagli01" & d$figure == "Fig 3")] <- temp

temp <- c("control", "scarification", rep("acid", 4), "piercing", rep("dry heat", 2))
d$treatmentCor[which(d$datasetID == "boscagli01" & d$figure == "Table 1")] <- temp
temp <- rep(c("hormone", "piercing x hormone", "soaking x hormone"), each = 3)

d$treatmentCor[which(d$datasetID == "boscagli01" & d$figure == "Table 2")] <- temp
d$treatmentCor[which(d$datasetID == "boscagli01" & d$figure == "Table 3")] <-
  "soaking (oxidant)"

#brandel05
d$treatmentCor[which(d$datasetID == "brandel05")] <- "dormancy x stratification x germination chemical"

#brenchley98
d$treatmentCor[which(d$datasetID == "brenchley98" & !(d$study %in% c("exp1", "exp5")))] <- "salinity x oxygen"
d$treatmentCor[which(d$datasetID == "brenchley98" & d$study == "exp1")] <- "oxygen x pressure"
d$treatmentCor[which(d$datasetID == "brenchley98" & d$study == "exp5")] <- "salinity x oxygen x storage"

#eelgrass

#budisavljevic21
d$treatmentCor[which(d$datasetID == "budisavljevic21")] <-
  "stratification x germination temperature x photoperiod"
#chill temp for table 2e should be 23
#non-existent warm stratification treatment data for figures panels a-d

#bungard97
d$treatmentCor[which(d$datasetID == "bungard97" & d$study == "exp1")] <- "chilling x nitrogen"
d$treatmentCor[which(d$datasetID == "bungard97" & d$study == "exp2")] <- "chilling x nitrogen x photoperiod"
d$treatmentCor[which(d$datasetID == "bungard97" & d$study == "exp3")] <- "chilling x inorganic salt"
d$treatmentCor[which(d$datasetID == "bungard97" & d$study == "exp4")] <- "chemical"
# the range of values for figure 1b, ammonium, seem to have a wrong y axis, it might have used the range used for figure 1a

#bytnerowicz14
d$treatmentCor[which(d$datasetID == "bytnerowicz14")] <- "germination temperature x photoperiod"

#carpenter92
d$treatmentCor[which(d$datasetID == "carpenter92" & d$study %in% c("exp1", "exp2"))] <- "germination temperature"
d$treatmentCor[which(d$datasetID == "carpenter92" & d$study == "exp3")] <- "hormone"
d$treatmentCor[which(d$datasetID == "carpenter92" & d$study == "exp4")] <- "storage"

#castro95
d$treatmentCor[which(d$datasetID == "castro95")] <- "cold stratification x substrate moisture x species"

#chakraborty92, various

#chen06
d$treatmentCor[which(d$datasetID == "chen06")] <- "cold moist stratification"

#chen08
#figures 3 and 4 not scraped

#chen15
d$treatmentCor[which(d$datasetID == "chen15" & d$figure == "Fig 1a")] <- "germination temperature"
d$treatmentCor[which(d$datasetID == "chen15" & d$figure == "Fig 1b")] <- "cold stratification"
d$treatmentCor[which(d$datasetID == "chen15" & d$figure == "Table 1")] <- "germination temperature x cold stratification"
d$treatmentCor[which(d$datasetID == "chen15" & d$figure == "Table 2")] <- "hormone"

#chichizola18
d$treatmentCor[which(d$datasetID == "chichizola18" & d$treatment == "cold stratification")] <-
  "cold moist stratification"

#chien10
d$treatmentCor[which(d$datasetID == "chien10" & d$study == "exp1")] <- "soaking (hormone)"
d$treatmentCor[which(d$datasetID == "chien10" & d$study == "exp2")] <- "germination temperature x soaking (hormone)"
d$treatmentCor[which(d$datasetID == "chien10" & d$study == "exp4")] <- "moisture content x storage"

#chien11
d$treatmentCor[which(d$datasetID == "chien11")] <- "germination temperature"

#cho18, a 
#exp1 table 1 and fig 5, and exp 2 table 3 is from a
temp <- c(rep(c("control", rep("soaking (hormone, before stratification)", 5)), 3),
          rep("soaking (hormone, after stratification)", 18))
d$treatmentCor[which(d$datasetID == "cho18" & d$study == "exp1" & d$figure == "Table 1")] <- temp
d$treatmentCor[which(d$datasetID == "cho18" & d$study == "exp1" & d$figure == "Figure 5")] <- "germination temperature x photoperiod"
d$treatmentCor[which(d$datasetID == "cho18" & d$study == "exp2" & d$figure == "Table 3")] <- "soaking (chemical, before stratification)"

#exp1 fig 3 and exp2 fig 5 is from b, wrong species
d$treatmentCor[which(d$datasetID == "cho18" & d$study == "exp1" & d$figure == "Figure 3")] <-
  "mechanical scarification x cold moist stratification x photoperiod"
d$treatmentCor[which(d$datasetID == "cho18" & d$study == "exp2" & d$figure == "Figure 5")] <- "germination temperature x photoperiod"

#chuanren04
d$treatmentCor[which(d$datasetID == "chuanren04" & d$study == "exp1")] <- "mechanical scarification"
d$treatmentCor[which(d$datasetID == "chuanren04" & d$study == "exp2")] <- "chilling x light"
d$treatmentCor[which(d$datasetID == "chuanren04" & d$study == "exp3")] <- "soaking (hormone)"

#cuena-lombrana18
d$treatmentCor[which(d$datasetID == "cuena-lombrana18")] <- "site x stratification x germination temperature"
temp <- c(rep(c(rep("cold", 6), rep("warm and cold", 6)), 4))
d$treatmentDetails[which(d$datasetID == "cuena-lombrana18")] <- paste0(temp, " stratification")

#dalling99
d$treatmentCor[which(d$datasetID == "dalling99")] <- "scarification x germination temperature x photoperiod"

#deb17
d$treatmentCor[which(d$datasetID == "deb17" & d$study == "exp1")] <- "light"
d$treatmentCor[which(d$datasetID == "deb17" & d$figure == "Figure 5")] <- "stratification x soil"

#dehgan84
d$treatmentCor[which(d$datasetID == "dehgan84")] <- "stratification x hormone"
#missing stratification data as chilling

#downie91
#one data point from figure 3 seems to be left in
#what is invigoration...

#downie98
#paper pending

#edwards73_1
d$treatmentCor[which(d$datasetID == "edwards73_1" & d$figure == "table 2")] <- "seedlot"
d$treatmentCor[which(d$datasetID == "edwards73_1" & d$figure != "table 2")] <- "stratification"

d$treatmentDetails[which(d$datasetID == "edwards73_1" &
                           d$figure != "table 2" &
                           d$treatment == "dark/moist/cold stratification on filter paper")] <-
  "stratification on filter paper"
d$treatmentDetails[which(d$datasetID == "edwards73_1" &
                           d$figure != "table 2" &
                           d$treatment == "naked stratification in plastic bag")] <-
  "naked stratification in plastic bag"

#edwards73_2
d$treatmentCor[which(d$datasetID == "edwards73_2")] <- "seedlot x stratification x photoperiod"

#edwards96, paper pending

#erken21
d$treatmentCor[which(d$datasetID == "erken21" & d$study == "exp1")] <- "storage"
d$treatmentCor[which(d$datasetID == "erken21" & d$study == "exp2")] <- "germination temperature"
d$treatmentCor[which(d$datasetID == "erken21" & d$study == "exp3")] <- "photoperiod"
temp <- c("control", "cold-wet stratification", "cold-wet stratification + soaking (hot water)",
          "cold-wet stratification + soaking (hormone)", "cold-wet stratification (perlite) + germination temperature",
          "cold-wet stratification (perlite) + germination temperature")
d$treatmentCor[which(d$datasetID == "erken21" & d$study == "exp4")] <- temp

#esmaeili09
d$treatmentCor[which(d$datasetID == "esmaeili09" & d$study == "exp1")] <- "stratification"
d$treatmentCor[which(d$datasetID == "esmaeili09" & d$study == "exp2")] <- "germination temperature"
d$treatmentCor[which(d$datasetID == "esmaeili09" & d$study == "exp3")] <- "salinity"

#etemadi10
d$treatmentCor[which(d$datasetID == "etemadi10")] <- "cold stratification"

#farhadi13
d$treatmentCor[which(d$datasetID == "farhadi13")] <- "seedlot x cold-moist stratification"
#table 3 with mean germination time data may be unscraped

#feng18
d$treatmentCor[which(d$datasetID == "feng18" & d$study == "exp1")] <- "embryo location"
d$treatmentCor[which(d$datasetID == "feng18" & d$study == "exp2")] <- "days after flowering"
d$treatmentCor[which(d$datasetID == "feng18" & d$study == "exp3")] <- "germination temperature"

#fetouh14
d$treatmentCor[which(d$datasetID == "fetouh14")] <- "cold stratification"
#wrong table number

#forbes09
d$treatmentCor[which(d$datasetID == "forbes09")] <- "storage"

#fulbright86, various

#ghimeray14
d$treatmentCor[which(d$datasetID == "ghimeray14")] <- "storage x germination temperature"
#some scarification data in table 5

#gianni19
d$treatmentCor[which(d$datasetID == "gianni19")] <- "cold stratification"

#gimenez-benavides13
d$treatmentCor[which(d$datasetID == "gimenez-benavides13")] <- "stratification x germination temperature"
d$treatmentDetails[which(d$datasetID == "gimenez-benavides13")] <- "cold-wet stratification"

#goggans74, various
d$treatmentCor[which(d$datasetID == "goggans74")] <- "cold stratification x germination temperature"

#grose57, various
#maybe we should exclude grose57 table 2 as it seems to only be an example in determining
#the germination capacity of a seedlot
d$treatmentCor[which(d$datasetID == "grose57" & d$study == "exp1")] <- "seedlot subsamples"
d$treatmentCor[which(d$datasetID == "grose57" & d$study == "exp2")] <- "stratification"
d$treatmentCor[which(d$datasetID == "grose57" & d$study == "exp3")] <- "stratification x germination temperature"
d$treatmentCor[which(d$datasetID == "grose57" & d$study == "exp4")] <- "stratification x germination temperature"
d$treatmentCor[which(d$datasetID == "grose57" & d$study == "exp5")] <- "stratification"
#chilling of table 3 column 3 should be "64F then 40F" with duration of "15 then varied"
d$treatmentCor[which(d$datasetID == "grose57" & d$study == "exp6")] <- "seedlot x stratification x germination temperature"

#guo20
d$treatmentCor[which(d$datasetID == "guo20")] <- "storage x germination temperature x photoperiod"

#han10
d$treatmentCor[which(d$datasetID == "han10" & d$study == "exp1")] <- "hormone x germination temperature"
d$treatmentCor[which(d$datasetID == "han10" & d$study == "exp2")] <- "stratification x moisture content"

#harrington09
d$treatmentCor[which(d$datasetID == "harrington09" & d$study == "exp1")] <- "germination temperature"
d$treatmentCor[which(d$datasetID == "harrington09" & d$study == "exp2")] <- "cold stratification"

#harrington14
d$treatmentCor[which(d$datasetID == "harrington09" & d$figure == "Figure 1C")] <- "cold stratification"

#hatzilazarou21
d$treatmentCor[which(d$datasetID == "hatzilazarou21")] <- "cold stratification x germination temperature"

#hawkins19
d$treatmentCor[which(d$datasetID == "hawkins19")] <- "cold stratification x germination temperature"
#possible data on GA in figure 4

#he09, various

#herron01
d$treatmentCor[which(d$datasetID == "herron01")] <- "chemical scarification"

#huang14
d$treatmentCor[which(d$datasetID == "huang14" & d$study == "exp2")] <- "stratification x light intensity"
d$treatmentCor[which(d$datasetID == "huang14" & d$study == "exp4")] <- "light intensity"

#irvani12, various
temp <- c("control", rep("acid", 2), rep("cold stratification", 6), rep("washing x chilling", 4), rep("hormone", 6))
d$treatmentCor[which(d$datasetID == "irvani12")] <- temp
d$treatmentDetails[which(d$datasetID == "irvani12" & d$treatment == "cold stratification")] <-
  "moist filter paper"
d$treatmentDetails[which(d$datasetID == "irvani12" & d$treatment == "cold moist stratification")] <-
  "moist sand"

#jabarzare11
d$treatmentCor[which(d$datasetID == "jabarzare11")] <- "photoperiod"

#jang22, various

#javanmard14
d$treatmentCor[which(d$datasetID == "javanmard14")] <- "washing x hormone x cold stratification"

#jensen97
d$treatmentCor[which(d$datasetID == "jensen97")] <- "moisture content x prechilling"
#germ duration should have values in chill duration

#jiro10
d$treatmentCor[which(d$datasetID == "jiro10")] <- "stratification x germination temperature"
#species seems to be different than in paper

#joshi03, various

#jusung16
d$treatmentCor[which(d$datasetID == "jusung16" & d$figure == "Figure 5")] <- "wet-chilling"
d$treatmentCor[which(d$datasetID == "jusung16" & d$figure == "Table 1")] <- "hormone"

#kalimuthu95, various


#kamareh12
d$treatmentCor[which(d$datasetID == "kamareh12")] <- "cold stratification x soaking (hormone)"


#karlsson08, various


#kato11
d$treatmentCor[which(d$datasetID == "kato11")] <- "cold stratification x germination temperature x photoperiod x air"

#kazaz10
d$treatmentCor[which(d$datasetID == "kazaz10")] <- "fertilizer"

#keshtkar08, various

#kettenring07
d$treatmentCor[which(d$datasetID == "kettenring07")] <- "cold stratification x germination temperature"

#kim16
d$treatmentCor[which(d$datasetID == "kim16" & d$figure == "table")] <- "hormone"

#king12, various

#kolodziejek15, various

#kolodziejek18, various



#kolodziejek19, various

#kulkarni06
d$treatmentCor[which(d$datasetID == "kulkarni06" & d$study != "exp3")] <-
  "germination temperature x photoperiod"
d$treatmentCor[which(d$datasetID == "kulkarni06" & d$study == "exp3")] <-
  "cold stratification"

#jang22
d$treatmentCor[which(d$datasetID == "jang22" & d$figure == "Figure 4")] <-
  "germination temperature x photoperiod"
d$treatmentCor[which(d$datasetID == "jang22" & d$figure == "Figure 5")] <-
  "cold stratification"
d$treatmentCor[which(d$datasetID == "jang22" & d$figure == "Figure 6")] <-
  "hormone"
d$treatmentCor[which(d$datasetID == "jang22" & d$figure == "Figure 7")] <-
  "photoperiod"
d$treatmentCor[which(d$datasetID == "jang22" & d$figure == "Fig 5")] <-
  "cold stratification"
d$treatmentCor[which(d$datasetID == "jang22" & d$figure == "Fig 6")] <-
  "hormone"
d$treatmentCor[which(d$datasetID == "jang22" & d$figure == "Fig 7")] <-
  "photoperiod"



#langlois17
d$treatmentCor[which(d$datasetID == "langlois17" & d$study == "exp1")] <-
  "storage media x cold stratification x scarification"
d$treatmentCor[which(d$datasetID == "langlois17" & d$study == "exp2")] <-
  "light intensity"

#lo19
d$treatmentCor[which(d$datasetID == "lo19" & d$figure == "Fig 2")] <- "stratification x germination"
temp <- rep(c("control", "soaking", "priming", "hormone", rep("plasma", 9)), 3)
d$treatmentCor[which(d$datasetID == "lo19" & d$figure == "Table 2")] <- temp

#nurse08
d$treatmentCor[which(d$datasetID == "nurse08")] <- "seed type x soil water content x cold stratification"

#olmez07
temp <- rep(c(rep("provenance", 3), "control", "soaking", rep("cold stratification", 3), "soaking (hot water)", "acid"), 2)
d$treatmentCor[which(d$datasetID == "olmez07")] <-
  temp

#olmez09
temp <- c(rep(c("control", "soaking", rep("cold stratification", 3), "soaking (hot water)", "acid"), 2), rep("provenance", 6))
d$treatmentCor[which(d$datasetID == "olmez09")] <-
  temp

#redondo-gomez11
d$treatmentCor[which(d$datasetID == "redondo-gomez11")] <-
  "salinity x cold stratification x burial depth"

#veiga-barbosa14
d$treatmentCor[which(d$datasetID == "veiga-barbosa14" & d$study == "exp1")] <-
  "scarification x germination temperature"
d$treatmentCor[which(d$datasetID == "veiga-barbosa14" & d$study == "exp2")] <-
  "germination temperature x photoperiod"
temp <- rep(c("control", "soaking", "soaking (hormone)"), 2)
d$treatmentCor[which(d$datasetID == "veiga-barbosa14" & d$study == "exp3")] <-
  temp
d$treatmentCor[which(d$datasetID == "veiga-barbosa14" & d$figure == "Tab. 5")] <-
  "priming"
d$treatmentCor[which(d$datasetID == "veiga-barbosa14" & d$figure == "Tab. 6")] <-
  "inorganic salt"

#tylkowski91
d$treatmentCor[which(d$datasetID == "tylkowski91")] <- "stratification"
d$treatmentDetails[which(d$datasetID == "tylkowski91")] <- "warm and cold stratication"

#d$treatmentCor[which(d$datasetID == "")] <- ""

