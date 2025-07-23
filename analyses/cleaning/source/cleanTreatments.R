## 26 September 2024 ##
# By Christophe

# Code to clean the treatment categories 

# Create new column of treatment category of cleaned treatments
d$treatmentCor <- d$treatment
d$treatmentDetails <- NA

#unique(d$treatment)
#unique(d$treatmentCor)
#unique(d$treatmentDetails)

prio <- read.csv("studyDesign/ids_for_ken/ids_after_allworkflow.csv")
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

#aiello07: #possible missing data on gibberelic acid treatments in Table 3 and Table 6
d$treatmentCor[which(d$datasetID == "aiello17")] <- "population x cold stratification"

#airi09
temp <- c(rep(" x control", 5), rep(" x cold stratification", 15), rep(" x chemical", 15),
          rep(" x hormone", 15), rep(" x chemical", 15))
d$treatmentCor[which(d$datasetID == "airi09" & d$figure == "table 2")] <- paste0("population", temp)

temp <- rep(c("control", "cold stratification", "hormone", "chemical", "chemical"), each = 5)
d$treatmentCor[which(d$datasetID == "airi09" & d$figure == "Table 3")] <- paste0("population x ", temp) # TO check: table 3 was never entered...

#al-absi10
temp <- c(rep("hot water", 16), rep("acid", 16), rep("hormone", 16))
d$treatmentCor[which(d$datasetID == "al-absi10")] <- paste0("cold stratification x ", temp)
# soaking duration needs to be cleaned, "120" instead of 0 - 30 minutes used in paper: TO CHECK

#albrecht20
d$treatmentCor[which(d$datasetID == "albrecht20")] <- "cold stratification x germination temperature x photoperiod"

#aldridge92
d$treatmentCor[which(d$datasetID == "aldridge92")] <- "moisture content x stratification x hormone"

#alhelal96
d$treatmentCor[which(d$datasetID == "alhelal96" & d$study == "exp1")] <- "soaking temperature x hormone x photoperiod"
d$treatmentCor[which(d$datasetID == "alhelal96" & d$study == "exp2")] <- "germination temperature"

#alptekin02
d$treatmentCor[which(d$datasetID == "alptekin02")] <- "population x stratification x mechanical scarification"

#amini18: paper pending

#amooaghaie09
d$treatmentCor[which(d$datasetID == "amooaghaie09")] <- "moist chilling x hormone"

#arslan11
d$treatmentCor[which(d$datasetID == "arslan11")] <- "moist chilling x hormone x photoperiod x germination temperature"
d$treatmentCor[which(d$datasetID == "arslan11" & d$figure == "table 1")]

#barnhill82
d$treatmentCor[which(d$datasetID == "barnhill82")] <- "stratification x germination temperature x photoperiod"

#barros12
d$treatmentCor[which(d$datasetID == "barros12")] <- "stratification x hormone x germination temperature"

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

#beikmohammadi12: various treatments
temp <- c("control", "soaking", "soaking (hot water)", rep("acid", 3),
          rep("soaking (hormone)", 3), rep("cold stratification", 2),
          rep("acid x soaking (hormone)", 4))
d$treatmentCor[which(d$datasetID == "beikmohammadi12")] <- temp

#bhatt00: various treatments
temp <- c("control", "soaking", "soaking (hot water)", "acid", "scarification",
          "hormone", "prechilling")
d$treatmentCor[which(d$datasetID == "bhatt00" & d$figure == "Fig 1")] <- temp
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

#borghetti86
d$treatmentCor[which(d$datasetID == "borghetti86")] <- "seedlot x cold-moist stratification x germination temperature"

#boscagli01: various treatments
temp <- c("control", rep("ultrasound", 2))
d$treatmentCor[which(d$datasetID == "boscagli01" & d$figure == "Fig 1")] <- temp
temp <- c("control", rep("prechilling x piercing", 3))
d$treatmentCor[which(d$datasetID == "boscagli01" & d$figure == "Fig 2")] <-
  "piercing x prechilling"
d$treatmentDetails[which(d$datasetID == "boscagli01" &
                           d$figure == "Fig 2" &
                           d$treatment == "control - intact")] <-
  "control (no piercing, no prechilling)"
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

#budisavljevic21
d$treatmentCor[which(d$datasetID == "budisavljevic21")] <-
  "stratification x germination temperature x photoperiod"

#bungard97
d$treatmentCor[which(d$datasetID == "bungard97" & d$study == "exp1")] <- "chilling x chemical"
d$treatmentCor[which(d$datasetID == "bungard97" & d$study == "exp2")] <- "chilling x chemical x photoperiod"
d$treatmentCor[which(d$datasetID == "bungard97" & d$study == "exp3")] <- "chilling x salt"
d$treatmentCor[which(d$datasetID == "bungard97" & d$study == "exp4")] <- "chemical"
# the range of values for figure 1b, ammonium, seem to have a wrong y axis, it might have used the range used for figure 1a. TO CHECK: rescrape 1b if we want good data for seeds treated with amonimum chloride

#bytnerowicz14
d$treatmentCor[which(d$datasetID == "bytnerowicz14")] <- "germination temperature x photoperiod"

#carpenter92
d$treatmentCor[which(d$datasetID == "carpenter92" & d$study %in% c("exp1", "exp2"))] <- "germination temperature"
d$treatmentCor[which(d$datasetID == "carpenter92" & d$study == "exp3")] <- "hormone"
d$treatmentCor[which(d$datasetID == "carpenter92" & d$study == "exp4")] <- "storage"

#castro95
d$treatmentCor[which(d$datasetID == "castro95")] <- "cold stratification x substrate moisture x species"

#chakraborty92: various treatments

#chen06
d$treatmentCor[which(d$datasetID == "chen06")] <- "cold moist stratification"

#chen08: TO CHECK: need to scrape fig 3 and 4. Paper is IDed as chien09

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

#cho18a: exp1 table 1 and fig 5, and exp 2 table 3 is from a
temp <- c(rep(c("control", rep("soaking (hormone, before stratification)", 5)), 3),
          rep("soaking (hormone, after stratification)", 18))
d$treatmentCor[which(d$datasetID == "cho18a" & d$study == "exp1" & d$figure == "Table 1")] <- temp
d$treatmentCor[which(d$datasetID == "cho18a" & d$study == "exp1" & d$figure == "Figure 5")] <- "germination temperature x photoperiod"
d$treatmentCor[which(d$datasetID == "cho18a" & d$study == "exp2" & d$figure == "Table 3")] <- "soaking (chemical, before stratification)"

#cho18b: exp1 fig 3 and exp2 fig 5 is from b. TO CHECK: CHANGE circuta virosa to : Cornus kousa
d$treatmentCor[which(d$datasetID == "cho18b" & d$study == "exp1" & d$figure == "Figure 3")] <-
  "mechanical scarification x cold moist stratification x photoperiod"
d$treatmentCor[which(d$datasetID == "cho18b" & d$study == "exp2" & d$figure == "Figure 5")] <- "germination temperature x photoperiod"

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

#downie91: TO CHECK: need to scrape fig 3

#downie98: TO CHECK: interlibrary loan lost. Missing whole pdf so cannot verify

#edwards73a
d$treatmentCor[which(d$datasetID == "edwards73a" & d$figure == "table 2")] <- "seedlot"
d$treatmentCor[which(d$datasetID == "edwards73a" & d$figure != "table 2")] <- "stratification"

d$treatmentDetails[which(d$datasetID == "edwards73a" &
                           d$figure != "table 2" &
                           d$treatment == "dark/moist/cold stratification on filter paper")] <-
  "stratification on filter paper"
d$treatmentDetails[which(d$datasetID == "edwards73a" &
                           d$figure != "table 2" &
                           d$treatment == "naked stratification in plastic bag")] <-
  "naked stratification in plastic bag"

#edwards73b
d$treatmentCor[which(d$datasetID == "edwards73b")] <- "seedlot x stratification x photoperiod"

#edwards96: TO CHECK: data scraped is the mean of mixed seeds germination... 

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

#farhadi13: TO CHECK: do we want mean germination time data across 6 elevations to different stratification periods? if so, we need to scrape table 3
d$treatmentCor[which(d$datasetID == "farhadi13")] <- "seedlot x cold-moist stratification"

#feng18
d$treatmentCor[which(d$datasetID == "feng18" & d$study == "exp1")] <- "embryo location"
d$treatmentCor[which(d$datasetID == "feng18" & d$study == "exp2")] <- "days after flowering"
d$treatmentCor[which(d$datasetID == "feng18" & d$study == "exp3")] <- "germination temperature"

#fetouh14
d$treatmentCor[which(d$datasetID == "fetouh14")] <- "cold stratification"

#forbes09
d$treatmentCor[which(d$datasetID == "forbes09")] <- "storage"

#fulbright86: various treatments

#ghimeray14: TO CHECK: some scarification data in table 5, which was never scraped
d$treatmentCor[which(d$datasetID == "ghimeray14")] <- "storage x germination temperature"

#gianni19
d$treatmentCor[which(d$datasetID == "gianni19")] <- "cold stratification"

#gimenez-benavides13
d$treatmentCor[which(d$datasetID == "gimenez-benavides13")] <- "stratification x germination temperature"
d$treatmentDetails[which(d$datasetID == "gimenez-benavides13")] <- "cold-wet stratification"

#goggans74: various
d$treatmentCor[which(d$datasetID == "goggans74")] <- "cold stratification x germination temperature"

#grose57: various treatments treatments
d$treatmentCor[which(d$datasetID == "grose57" & d$study == "exp1")] <- "seedlot subsamples"
d$treatmentCor[which(d$datasetID == "grose57" & d$study == "exp2")] <- "stratification"
d$treatmentCor[which(d$datasetID == "grose57" & d$study == "exp3")] <- "stratification x germination temperature"
d$treatmentCor[which(d$datasetID == "grose57" & d$study == "exp4")] <- "stratification x germination temperature"
d$treatmentCor[which(d$datasetID == "grose57" & d$study == "exp5")] <- "stratification"
d$treatmentCor[which(d$datasetID == "grose57" & d$study == "exp6")] <- "seedlot x stratification x germination temperature"

#guo20
d$treatmentCor[which(d$datasetID == "guo20")] <- "storage x germination temperature x photoperiod"

#han10
d$treatmentCor[which(d$datasetID == "han10" & d$study == "exp1")] <- "hormone x germination temperature"
d$treatmentCor[which(d$datasetID == "han10" & d$study == "exp2")] <- "stratification x moisture content"

#harrington09
d$treatmentCor[which(d$datasetID == "harrington09" & d$study == "exp1")] <- "germination temperature"
d$treatmentCor[which(d$datasetID == "harrington09" & d$study == "exp2")] <- "cold stratification"

#harrison14
d$treatmentCor[which(d$datasetID == "harrison14")] <- "provenance x altitude x cold stratification"

#hatzilazarou21
d$treatmentCor[which(d$datasetID == "hatzilazarou21")] <- "cold stratification x germination temperature"
d$treatmentDetails[which(d$datasetID == "hatzilazarou21" & d$treatment == "control")] <-
  "control (no cold stratification)"

#hawkins19: TO CHECK: missing fig4 with GA3. Need to scrape it if we want it. no strat
d$treatmentCor[which(d$datasetID == "hawkins19")] <- "cold stratification x germination temperature"


#he09: various treatments
temp <- c("control", rep("chilling", 4), rep("hormone", 4), rep("chilling x hormone", 4),
          "mechanical scarification")
d$treatmentCor[which(d$datasetID == "he09")] <- "cold stratification x germination temperature"

#herron01
d$treatmentCor[which(d$datasetID == "herron01")] <- "chemical scarification"

#huang14
d$treatmentCor[which(d$datasetID == "huang14" & d$study == "exp2")] <- "stratification x light intensity"
d$treatmentCor[which(d$datasetID == "huang14" & d$study == "exp4")] <- "light intensity"

#irvani12: various treatments
temp <- c("control", rep("acid", 2), rep("cold stratification", 6), rep("washing + chilling", 4), rep("hormone", 6))
d$treatmentCor[which(d$datasetID == "irvani12")] <- temp
d$treatmentDetails[which(d$datasetID == "irvani12" & d$treatment == "cold stratification")] <-
  "moist filter paper"
d$treatmentDetails[which(d$datasetID == "irvani12" & d$treatment == "cold moist stratification")] <-
  "moist sand"

#jabarzare11
d$treatmentCor[which(d$datasetID == "jabarzare11")] <- "photoperiod"

#jang22: various treatments
d$treatmentCor[which(d$datasetID == "jang22" & d$figure == "Fig 5")] <-
  "cold stratification"
d$treatmentCor[which(d$datasetID == "jang22" & d$figure == "Fig 6")] <-
  "hormone"
d$treatmentCor[which(d$datasetID == "jang22" & d$figure == "Fig 7")] <-
  "photoperiod"

#javanmard14
d$treatmentCor[which(d$datasetID == "javanmard14")] <- "washing x hormone x cold stratification"

#jensen97
d$treatmentCor[which(d$datasetID == "jensen97")] <- "moisture content x prechilling"

#jiro10
d$treatmentCor[which(d$datasetID == "jiro10")] <- "stratification x germination temperature"

#joshi03: various treatments

#jusung16
d$treatmentCor[which(d$datasetID == "jusung16" & d$figure == "Figure 5 ")] <- "wet-chilling x germination temperature x photoperiod"
d$treatmentCor[which(d$datasetID == "jusung16" & d$figure == "Table 1")] <- "hormone"

#kalimuthu95: various treatments

#kamareh12
temp <- c("", " + cool water", " + hormone", " + hormone")
d$treatmentCor[which(d$datasetID == "kamareh12" & d$figure == "Figure 1")] <-
  paste0("cold stratification", temp)
d$treatmentCor[which(d$datasetID == "kamareh12" & d$figure == "Figure 3")] <-
  paste0("cold stratification", rep(temp, each = 18))

#karlsson08: various treatments
d$treatmentCor[which(d$datasetID == "karlsson08" & d$treatment == "cold")] <- "cold stratification"
d$treatmentCor[which(d$datasetID == "karlsson08")] <-
  paste0(d$treatmentCor[which(d$datasetID == "karlsson08")], " x germination temperature x photoperiod")

#kato11
d$treatmentCor[which(d$datasetID == "kato11")] <- "cold stratification x germination temperature x photoperiod x air"

#kazaz10
d$treatmentCor[which(d$datasetID == "kazaz10")] <- "fertilizer"

#keshtkar08: various treatments

#kettenring07
d$treatmentCor[which(d$datasetID == "kettenring07")] <- "cold stratification x germination temperature"

#kim16
d$treatmentCor[which(d$datasetID == "kim16" & d$figure == "table 1")] <- "hormone"

#king12: various treatments

#kolodziejek15: various treatments
d$treatmentCor[which(d$datasetID == "kolodziejek15" & d$study == "exp2 and exp3")] <-
  "cold stratification x germination temperature x photoperiod"
d$treatmentCor[which(d$datasetID == "kolodziejek15" & d$study == "exp4")] <- "hormone"
d$treatmentCor[which(d$datasetID == "kolodziejek15" & d$study == "exp5")] <- "salinity"

#kolodziejek18: various treatments
d$treatmentCor[which(d$datasetID == "kolodziejek18" & d$study == "exp0")] <-
  "untreated x germination temperature x photoperiod"
d$treatmentCor[which(d$datasetID == "kolodziejek18" & d$study == "exp1")] <-
  "dry storage x germination temperature x photoperiod"
d$treatmentCor[which(d$datasetID == "kolodziejek18" & d$study == "exp2")] <-
  "cold stratification x germination temperature x photoperiod"

#kolodziejek19: various treatments
d$treatmentCor[which(d$datasetID == "kolodziejek19")] <-
  "cold stratification x germination temperature x photoperiod"

#kulkarni06
d$treatmentCor[which(d$datasetID == "kulkarni06" & d$study != "exp3")] <-
  "germination temperature x photoperiod"
d$treatmentCor[which(d$datasetID == "kulkarni06" & d$study == "exp3")] <-
  "cold stratification"

#langlois17
d$treatmentCor[which(d$datasetID == "langlois17" & d$study == "exp1")] <-
  "storage media x cold stratification x scarification"
d$treatmentCor[which(d$datasetID == "langlois17" & d$study == "exp2")] <-
  "light intensity"

#lee06
d$treatmentCor[which(d$datasetID == "lee06" & d$study == "exp1")] <-
  "storage x germination duration"
d$treatmentCor[which(d$datasetID == "lee06" & d$study == "exp2")] <-
  "stratification"

#lee21
d$treatmentCor[which(d$datasetID == "lee21")] <- "cold stratification x germination duration"

#li11
d$treatmentCor[which(d$datasetID == "li11" & d$study == "exp 1")] <-
  "cold stratification x salinity"
d$treatmentCor[which(d$datasetID == "li11" & d$study == "exp 2")] <-
  "mechanical scarification x salinity"
d$treatmentCor[which(d$datasetID == "li11" & d$study == "exp 3")] <-
  "hormone x salinity"

#liu13
d$treatmentCor[which(d$datasetID == "liu13" & d$treatment != "provenance.data")] <-
  "moist chilling x thermopriming"
d$treatmentDetails[which(d$datasetID == "liu13" & d$treatment == "control")] <-
  "control (no moist chilling, no thermopriming)"

#lo19
d$treatmentCor[which(d$datasetID == "lo19" & d$figure == "Fig 2")] <- "stratification x germination"
temp <- rep(c("control", "soaking", "priming", "hormone", rep("plasma", 9)), 3)
d$treatmentCor[which(d$datasetID == "lo19" & d$figure == "Table 2")] <- temp

#ma03: TO CHECK: different treatment than other fig and tables and don't know what they did here
d$treatmentCor[which(d$datasetID == "ma03")] <- "moist chilling x solid matrix priming"

#ma18
d$treatmentCor[which(d$datasetID == "ma18")] <- "chemical x germination temperature"

#markovic20
temp <- rep(rep(c(" (sand)", " (plastic bag)", " (plastic bag)"), each = 2), 6)
d$treatmentCor[which(d$datasetID == "markovic20")] <- paste0("cold stratification", temp)

#martinik14

#mattana09
d$treatmentCor[which(d$datasetID == "mattana09" & d$study == "exp1")] <-
  "germination temperature x germination duration x photoperiod"
d$treatmentCor[which(d$datasetID == "mattana09" & d$study == "exp2")] <-
  "pre-chilling"

#meyer94
d$treatmentCor[which(d$datasetID == "meyer94" & d$treatment == "control")] <-
  "moist chilling"
d$treatmentDetails[which(d$datasetID == "meyer94" & d$treatment == "control")] <-
  "control (no moist chilling)"

#meyer95
d$treatmentCor[which(d$datasetID == "meyer95" & d$treatment == "control")] <-
  "moist chilling"
d$treatmentDetails[which(d$datasetID == "meyer95" & d$treatment == "control")] <-
  "control (no moist chilling)"

#millaku12
d$treatmentCor[which(d$datasetID == "millaku12")] <- "hormone x chemical x cold stratification"
d$treatmentDetails[which(d$datasetID == "millaku12" & d$treatment != "GA3, KNO3")] <-
  "control (no hormone, no chemical)"

d$treatmentDetails[which(d$datasetID == "millaku12" & d$treatment == "control")] <-
  "no stratification"
d$treatmentDetails[which(d$datasetID == "millaku12" & d$treatment == "cold-stratification w/t filter paper")] <-
  "cold stratification on filter paper"
d$treatmentDetails[which(d$datasetID == "millaku12" & d$treatment == "cold-stratification w/t sand-soil")] <-
  "cold stratification on sand-soil"

#moradi12
temp <- c("chilling", "acid", "control", rep("hormone", 10))
d$treatmentCor[which(d$datasetID == "moradi12" & d$figure == "Table 1")] <-
  temp
d$treatmentCor[which(d$datasetID == "moradi12" & d$figure == "Table 2")] <-
  "hormone"
d$treatmentCor[which(d$datasetID == "moradi12" & d$figure == "Table 3")] <-
  "hormone"

#naseri18
d$treatmentCor[which(d$datasetID == "naseri18" & d$study == "exp1")] <-
  "cold stratification x mechanical scarification"
d$treatmentCor[which(d$datasetID == "naseri18" & d$study == "exp2")] <-
  "mechanical scarification x chemical"

#nasri14
d$treatmentCor[which(d$datasetID == "nasri14")] <-
  "culture type x chilling x chemical"
d$treatmentDetails[which(d$datasetID == "nasri14" & d$treatment == "control")] <-
  "control (no chemical)"

#naumovski15
d$treatmentCor[which(d$datasetID == "naumovski05" & d$study == "exp1")] <-
  "photoperiod"
d$treatmentCor[which(d$datasetID == "naumovski05" & d$study == "exp2")] <-
  "provenance"
d$treatmentCor[which(d$datasetID == "naumovski05" & d$study == "exp3")] <-
  "provenance x cold stratification x germination temperature"
d$treatmentCor[which(d$datasetID == "naumovski05" & d$study == "exp4")] <-
  "cold stratification x germination temperature"

#nawrot-chorabik21: TO CHECK: what to do with treatment seed pelleting
d$treatmentCor[which(d$datasetID == "nawrot-chorabik21" & d$treatment == "control, no dormancy breaking treatment")] <-
  "control"
d$treatmentCor[which(d$datasetID == "nawrot-chorabik21" & d$treatment == "stratification/scarification")] <-
  "stratification + mechanical scarification"
d$treatmentCor[which(d$datasetID == "nawrot-chorabik21" & d$treatment == "growth regulators")] <-
  "hormone"
d$treatmentCor[which(d$datasetID == "nawrot-chorabik21" & d$treatment == "growth regulators; stratification/scarification")] <-
  "(stratification + mechanical scarification) x hormone"

#necajeva13
d$treatmentCor[which(d$datasetID == "necajeva13" & d$treatment == "chemical")] <-
  "hormone"
d$treatmentCor[which(d$datasetID == "necajeva13")] <-
  paste0("warm stratification x ", d$treatmentCor[which(d$datasetID == "necajeva13")])

#nin17
d$treatmentCor[which(d$datasetID == "nin17")] <-
  "stratification x germination temperature x photoperiod"
d$treatmentDetails[which(d$datasetID == "nin17" & d$treatment == "non stratified")] <-
  "control (no stratification)"

#nurse08
d$treatmentCor[which(d$datasetID == "nurse08")] <- "seed type x soil water content x cold stratification"

#okay11
d$treatmentCor[which(d$datasetID == "okay11")] <- "cold stratification x pH"
d$treatmentDetails[which(d$datasetID == "okay11" & d$treatment == "Control")] <-
  "control (no stratification)"
d$treatmentDetails[which(d$datasetID == "okay11" & d$chillTemp == "0" & d$chillDuration == "NA")] <-
  "average among stratification treatments"
d$treatmentDetails[which(d$datasetID == "okay11" & d$chillTemp == "0" & d$chillDuration == "NA")] <-
  "average among stratification treatments"
d$treatmentDetails[which(d$datasetID == "okay11" & d$other.treatment == "pH = 6.5, 7.5, 8.5")] <-
  "average among pH treatments"

#olmez07
temp <- rep(c(rep("provenance", 3), "control", "soaking", rep("cold stratification", 3), "soaking (hot water)", "acid"), 2)
d$treatmentCor[which(d$datasetID == "olmez07")] <-
  temp

#olmez09
temp <- c(rep(c("control", "soaking", rep("cold stratification", 3), "soaking (hot water)", "acid"), 2), rep("provenance", 6))
d$treatmentCor[which(d$datasetID == "olmez09")] <- temp

#panayotova15
temp <- c(rep("temperature", 3), "chilling", "soaking + temperature", "native soil")
d$treatmentCor[which(d$datasetID == "panayotova15")] <- temp

#paolini01
d$treatmentCor[which(d$datasetID == "paolini01")] <-
  "seed mass x seed color x prechilling x photoperiod"

#phondani10
d$treatmentCor[which(d$datasetID == "phondani10")] <-
  "provenance x prechilling x germination temperature x photoperiod"
d$treatmentDetails[which(d$datasetID == "phondani10" & d$treatment == "control")] <-
  "control (no prechilling)"

#picciau17
d$treatmentCor[which(d$datasetID == "picciau17" & d$treatment == "dry after-ripened")] <-
  "drying after ripening"
d$treatmentCor[which(d$datasetID == "picciau17" & d$treatment == "light control")] <-
  "control"
d$treatmentCor[which(d$datasetID == "picciau17" & d$treatment == "dark control")] <-
  "control"
d$treatmentCor[which(d$datasetID == "picciau17" & d$treatment == "chemical")] <-
  "hormone"
d$treatmentDetails[which(d$datasetID == "picciau17" & d$treatment == "light control")] <-
  "12 l / 12 d photoperiod"
d$treatmentDetails[which(d$datasetID == "picciau17" & d$treatment == "dark control")] <-
  "24 d photoperiod"
d$treatmentCor[which(d$datasetID == "picciau17")] <-
  paste0(d$treatmentCor[which(d$datasetID == "picciau17")], " x germination temperature")

#pipinis09
d$treatmentCor[which(d$datasetID == "pipinis09")] <-
  "warm stratification x cold stratification"
d$treatmentDetails[which(d$datasetID == "pipinis09" & d$treatment == "control")] <-
  "control (no warm stratification, no cold stratification)"

#pipinis20
d$treatmentCor[which(d$datasetID == "pipinis20" & d$figure == "Table 1")] <-
  "moisture content x cold stratification"
d$treatmentCor[which(d$datasetID == "pipinis20" & d$figure == "Table 2")] <-
  "warm stratification x cold stratification"

#rafiq21
d$treatmentCor[which(d$datasetID == "rafiq21")] <- "culture type x chilling"
d$treatmentDetails[which(d$datasetID == "rafiq21" & d$treatment == "control")] <-
  "control (no chilling)"

#rahnama-ghahfarokhi07
temp <- c(rep("cold stratification", 3), rep("hormone", 3),
          rep("cold stratification + hormone", 3), "control")
d$treatmentCor[which(d$datasetID == "rahnama-ghahfarokhi07" & d$figure == "Table 1")] <-
  temp
temp <- c(rep("hot water", 2), rep("acid", 2), rep("chemical", 2), "control")
d$treatmentCor[which(d$datasetID == "rahnama-ghahfarokhi07" & d$figure == "Table 2")] <-
  temp

#redondo-gomez11
d$treatmentCor[which(d$datasetID == "redondo-gomez11")] <-
  "salinity x cold stratification x burial depth"

#ren04
d$treatmentCor[which(d$datasetID == "ren04" & d$treatment == "cold")] <- "cold stratification"

#ren15
d$treatmentCor[which(d$datasetID == "ren15" & d$study == "exp1")] <-
  "light x germination temperature"
d$treatmentCor[which(d$datasetID == "ren15" & d$study == "exp2")] <-
  "soil moisture x chilling"

#rezvani14
d$treatmentCor[which(d$datasetID == "rezvani14" & d$study == "exp1")] <-
  "hormone x photoperiod"
d$treatmentCor[which(d$datasetID == "rezvani14" & d$study == "exp2")] <-
  "prechilling"
d$treatmentCor[which(d$datasetID == "rezvani14" & d$study == "exp3")] <-
  "germination temperature x photoperiod"

d$treatmentDetails[which(d$datasetID == "rezvani14" & d$study == "exp2")] <-
  d$treatment[which(d$datasetID == "rezvani14" & d$study == "exp2")]

#romero05
d$treatmentCor[which(d$datasetID == "romero05" & d$study == "exp2")] <-
  "photoperiod"
d$treatmentCor[which(d$datasetID == "romero05" & d$study == "exp3")] <-
  "cold-moist stratification x light"
d$treatmentDetails[which(d$datasetID == "romero05" & d$study == "exp3" & d$treatment == "control")] <-
  "control (no cold-moist stratification)"

#rouhi12
d$treatmentCor[which(d$datasetID == "rouhi12")] <- "stratification x hormone"
d$treatmentDetails[which(d$datasetID == "rouhi12")] <-
  "control (no stratification, no hormone)"

#rouhi13
temp <- c("control", rep("acid", 3), rep("hot water", 4), rep("chemical", 3),
          rep("stratification", 3))
d$treatmentCor[which(d$datasetID == "rouhi13")] <- temp

#rubin18
d$treatmentCor[which(d$datasetID == "rubin18")] <- "ecotype x stratification"

#saeed16
d$treatmentCor[which(d$datasetID == "saeed16" & d$figure == "Figure 1")] <-
  "provenance x germination temperature x photoperiod"
d$treatmentCor[which(d$datasetID == "saeed16" & d$figure == "Figure 2")] <-
  "storage x photoperiod"
d$treatmentCor[which(d$datasetID == "saeed16" & d$figure == "Figure 5")] <-
  "cold moist stratification x photoperiod"

#santos19
d$treatmentCor[which(d$datasetID == "santos19")] <-
  "stratification x germination temperature"

#schutz02
d$treatmentCor[which(d$datasetID == "schutz02" & d$figure == "Figure 1")] <-
  "cold stratification x germination temperature x photoperiod"
d$treatmentCor[which(d$datasetID == "schutz02" & d$figure == "Figure 2")] <-
  "germination temperature"
d$treatmentCor[which(d$datasetID == "schutz02" & d$figure == "Table 3")] <-
  "cold stratification x germination temperature"

#scocco98
temp <- c("control", "soaking + chilling", "soaking (hormone)")
d$treatmentCor[which(d$datasetID == "scocco98")] <- temp

#seng20
d$treatmentCor[which(d$datasetID == "seng20" &
                       d$treatment == "soaking" &
                       d$soaked.in == "hot water 70C")] <- "soaking (hot water)"
d$treatmentCor[which(d$datasetID == "seng20" &
                       d$treatment == "soaking" &
                       d$soaked.in == "cold water")] <- "soaking (cold water)"
d$treatmentCor[which(d$datasetID == "seng20" &
                       d$chemicalCor == "NaClO")] <- "soaking (chemical)"
d$treatmentCor[which(d$datasetID == "seng20" &
                       d$chemicalCor == "GA3")] <- "soaking (hormone)"
d$treatmentCor[which(d$datasetID == "seng20" &
                       d$soaked.in == "hot water 70C" &
                       d$chemicalCor == "GA3")] <- "soaking (hot water) + soaking (hormone)"
d$treatmentCor[which(d$datasetID == "seng20" & d$treatment == "cold strat")] <-
  "cold stratification"
d$treatmentCor[which(d$datasetID == "seng20" & d$treatment == "scarification")] <-
  "mechanical scarification"
d$treatmentCor[which(d$datasetID == "seng20" &
                       d$treatment == "scarification + soaking" &
                       d$soaked.in == "hot water 50C")] <-
  "mechanical scarification + soaking (hot water)"
d$treatmentCor[which(d$datasetID == "seng20" &
                       d$treatment == "scarification + soaking" &
                       d$soaked.in == "water")] <-
  "mechanical scarification + soaking (cold water)"

#skordilis95
d$treatmentCor[which(d$datasetID == "skordilis95" & d$study == "exp1")] <-
  "provenance x germination temperature"
d$treatmentCor[which(d$datasetID == "skordilis95" & d$study == "exp2")] <-
  "provenance x stratification x germination temperature"
d$treatmentCor[which(d$datasetID == "skordilis95" & d$study == "exp3")] <-
  "provenance x germination temperature"
d$treatmentCor[which(d$datasetID == "skordilis95" & d$study == "exp4")] <-
  "stratification"
d$treatmentCor[which(d$datasetID == "skordilis95" & d$study == "exp5")] <-
  "stratification x light"

#song20
d$treatmentCor[which(d$datasetID == "song20" & d$study == "exp1")] <-
  "cold stratification"
d$treatmentCor[which(d$datasetID == "song20" & d$study == "exp2")] <-
  "cold stratification"

#sundaramoorthy93
temp <- c("control", "storage", "dry heat", "chilling", "washing",
          "dry heat + chilling", "chilling + dry heat",
          rep("dry heat + chilling + washing", 2),
          rep("chilling + dry heat + washing", 2))
d$treatmentCor[which(d$datasetID == "sundaramoorthy93" & d$study == "exp1")] <- temp
d$treatmentCor[which(d$datasetID == "sundaramoorthy93" & d$study == "exp2")] <-
  "collection month x storage x washing"

#tabatabaeian18
d$treatmentCor[which(d$datasetID == "tabatabaeian18")] <- "stratification x hormone"
d$treatmentDetails[which(d$datasetID == "tabatabaeian18" & d$treatment == "control")] <-
  "control (no hormone, average across stratification)"

#tang10a
d$treatmentCor[which(d$datasetID == "tang10a" & d$study == "exp1")] <-
  "germination temperature"
d$treatmentCor[which(d$datasetID == "tang10a" & d$treatment == "Control")] <-
  "control"
d$treatmentCor[which(d$datasetID == "tang10a" & d$treatment == "stratification")] <-
  "cold stratification"
d$treatmentCor[which(d$datasetID == "tang10a" & d$treatment == "after-ripening")] <-
  "drying after ripening"
d$treatmentCor[which(d$datasetID == "tang10a" & d$treatment == "GA3")] <-
  "hormone"

d$treatmentCor[which(d$datasetID == "tang10a" & d$study == "exp5")] <-
  "(control, cold stratification, dry storage) x germination temperature"

###################################
# I AM HERE
###################################









#tang10b paper in gdrive is for tang10a
#what does the 0, 40, and 60 days mean for control treatments???

#tang21
d$treatmentCor[which(d$datasetID == "tang21" &
                       d$treatment != "stratification" &
                       d$figure == "Figure 2")] <-
  "dry storage"
d$treatmentCor[which(d$datasetID == "tang21" & d$chemicalCor == "GA3")] <- "hormone"

#teimouri13
temp <- c("control", rep("mechanical scarification", 3),
          rep(c("hot water + soaking", "chilling", "hormone", "acid"), each = 2),
          rep("chemical", 3), rep("magnetization", 4), "chilling + hot water",
          "chilling + heat", "chilling + hot water + magnetization",
          rep(c("agar", "agar + hormone"), each = 2))
d$treatmentCor[which(d$datasetID == "teimouri13" & d$figure == "Table 1")] <- temp
d$treatmentCor[which(d$datasetID == "teimouri13" & d$figure == "Table 2")] <-
  "germination temperature"

#thomsen02
d$treatmentCor[which(d$datasetID == "thomsen02" & d$figure == "Figure 2")] <-
  "mechanical scarification"
#fix figure number, Figure 3 -> Figure 4

#tilki06
d$treatmentCor[which(d$datasetID == "tilki06" & d$study == "exp3")] <- "sowing"

#tilki07
temp <- c("control", rep("cold stratification", 3),
          rep("warm stratification + cold stratification", 3), "soaking (acid)",
          "soaking (acid) + warm stratification + cold stratification",
          rep("soaking (chemical)", 2),
          "soaking (chemical) + cold stratification",
          "soaking (chemical) + warm stratification",
          rep("soaking (chemical)", 2),
          "soaking (chemical) + warm stratification",
          "soaking (chemical) + soaking (hormone)",
          "mechanical scarification + soaking (chemical)",
          "mechanical scarification + soaking (chemical) + cold stratification",
          "mechanical scarification + cold stratification")
d$treatmentCor[which(d$datasetID == "tilki07")] <- temp

#tylkowski91
d$treatmentCor[which(d$datasetID == "tylkowski91" & d$study == "exp1")] <-
  "warm stratification x cold stratification x germination temperature"
d$treatmentCor[which(d$datasetID == "tylkowski91" & d$study == "exp2")] <-
  "warm stratification x cold stratification x moisture content"
d$treatmentCor[which(d$datasetID == "tylkowski91" & d$study == "exp3")] <-
  "warm stratification x moisture content"
d$treatmentCor[which(d$datasetID == "tylkowski91" & d$study == "exp 1, 2, 3")] <-
  "germination temperature"

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

#walck12
d$treatmentCor[which(d$datasetID == "walck12" & d$study == "exp1")] <-
  "cold stratification x germination temperature x photoperiod x germination duration"
d$treatmentCor[which(d$datasetID == "walck12" & d$study == "exp2")] <-
  "cold stratification x germination temperature"

#wang09
d$treatmentCor[which(d$datasetID == "wang09")] <- "chilling"

#washitani85
d$treatmentCor[which(d$datasetID == "washitani85" & d$study == "exp1")] <-
  "germination temperature"
d$treatmentCor[which(d$datasetID == "washitani85" &
                       d$study == "exp2" &
                       d$treatment == "dry-chilling pretreatment")] <-
  "dry-chilling"
d$treatmentCor[which(d$datasetID == "washitani85" &
                       d$study == "exp2" &
                       d$treatment == "moist-chilling pretreatment")] <-
  "moist-chilling"

#watanabe02
temp <- rep(c("control", "cold stratification", "cold stratification",
              "cold stratification", "warm stratification + cold stratification"),
            each = 4)
d$treatmentCor[which(d$datasetID == "watanabe02" & d$study == "exp1")] <- temp
d$treatmentCor[which(d$datasetID == "watanabe02" & d$study == "exp2")] <-
  "hormone"

#wytsalucy21
temp <- c("chilling", "chilling + soaking (acid)",
          "chilling + soaking (acid) + soaking (hormone)")
d$treatmentCor[which(d$datasetID == "wytsalucy21" & d$study == "exp1")] <- temp
d$treatmentCor[which(d$datasetID == "wytsalucy21" & d$study == "exp2")] <- temp

temp <- c(rep("soaking (acid) + hormone", 3), rep("hormone", 6),
          rep("soaking (acid) + hormone", 3), "soaking (acid)", "control", rep("chilling", 3))
d$treatmentCor[which(d$datasetID == "wytsalucy21" & d$figure == "Figure 3")] <- temp

temp <- c(rep(c("soaking (acid) + hormone", "hormone"), 4), "soaking (acid)",
          "control", rep("chilling", 3))
d$treatmentCor[which(d$datasetID == "wytsalucy21" & d$figure == "Figure 5")] <- temp

#yang10
d$treatmentCor[which(d$datasetID == "yang10" & d$figure == "Figure 2")] <-
  "moist storage"
d$treatmentCor[which(d$datasetID == "yang10" & d$figure == "Figure 3")] <-
  "dry storage x moisture content"
d$treatmentCor[which(d$datasetID == "yang10" & d$figure == "Figure 4")] <-
  "dry storage x moisture content"

#yang16a
d$treatmentCor[which(d$datasetID == "yang16a")] <-
  "storage x stratification"

#yang16b
temp <- c("control", rep("cold stratification + chemical", 2),
          "cold stratification",  "cold stratification + chemical",
          "cold stratification + chemical + bottom heat")
d$treatmentCor[which(d$datasetID == "yang16b")] <- temp

#yang18a
d$treatmentCor[which(d$datasetID == "yang18a" & d$figure %in% c("Figure 3", "Figure 4", "Figure 5"))] <-
  "moist stratification"
d$treatmentCor[which(d$datasetID == "yang18a" & d$figure %in% c("Figure 6", "Figure 7"))] <-
  "dry storage x moisture content"

#yang18b
d$treatmentCor[which(d$datasetID == "yang18b" & d$figure == "Figure 2")] <-
  "control"
d$treatmentCor[which(d$datasetID == "yang18b" & d$figure %in% c("Figure 3", "Figure 4", "Figure 5"))] <-
  "moist stratification"
d$treatmentCor[which(d$datasetID == "yang18b" & d$figure %in% c("Figure 6", "Figure 7"))] <-
  "naked storage"

#yang18c
d$treatmentCor[which(d$datasetID == "yang18c" & d$figure == "Figure 2")] <-
  "control"
d$treatmentCor[which(d$datasetID == "yang18c" & d$figure == "Figure 3")] <-
  "moist storage"
d$treatmentCor[which(d$datasetID == "yang18c" & d$figure %in% c("Figure 4", "Figure 5"))] <-
  "dry storage x moisture content"

#yeom21
temp <- c("control", rep(c("drying x mechanical scarification", "soaking x mechanical scarification",
                          "soaking x mechanical scarification", "cold stratification x mechanical scarification"),
                         each = 3))
d$treatmentCor[which(d$datasetID == "yeom21" & d$figure == "Figure 2")] <- temp

temp <- c("cold stratification", "cold stratification + warm stratification", "warm stratification")
d$treatmentCor[which(d$datasetID == "yeom21" & d$figure == "Figure 4")] <- temp
d$treatmentCor[which(d$datasetID == "yeom21" & d$figure %in% c("Figure 5", "Figure 6", "Figure 7"))] <-
  "scarification x germination temperature"

#yin09
d$treatmentCor[which(d$datasetID == "yin09")] <- "cold stratification x seed density x light"

#yuan21
d$treatmentCor[which(d$datasetID == "yuan21")] <- "harvest date x cold stratification"
#fix response variables

#yusefi-tanha19
d$treatmentCor[which(d$datasetID == "yusefi-tanha19")] <- "priming x cold stratification"

#zadeh15
d$treatmentCor[which(d$datasetID == "zadeh15" & d$study == "exp1")] <-
  "cold stratification x hormone"
d$treatmentCor[which(d$datasetID == "zadeh15" & d$study == "exp2")] <-
  "salinity x hormone"

#zardari19
d$treatmentCor[which(d$datasetID == "zardari19" & d$study == "exp1")] <-
  "cold stratification x germination temperature"
d$treatmentCor[which(d$datasetID == "zardari19" & d$study == "exp2")] <-
  "hormone x germination temperature"
d$treatmentCor[which(d$datasetID == "zardari19" & d$study == "exp3")] <-
  "cold stratification x hormone"

d$treatmentCor[which(d$datasetID == "zardari19" &
                       d$study == "exp1" &
                       d$treatment == "control")] <-
  "control (no cold stratification)"
d$treatmentCor[which(d$datasetID == "zardari19" &
                       d$study == "exp3" &
                       d$treatment == "contol")] <-
  "control (no cold stratification)"

#zare11
temp <- c("control", "washing",
          rep("chilling", 4),
          rep("hormone", 4),
          rep("washing + chilling", 2),
          rep("chilling + hormone", 8))
d$treatmentCor[which(d$datasetID == "zare11")] <- temp

#zhou03
d$treatmentCor[which(d$datasetID == "zhou03" & d$study == "exp1")] <-
  "stratification"
d$treatmentCor[which(d$datasetID == "zhou03" & d$study == "exp2")] <-
  "hormone"
d$treatmentCor[which(d$datasetID == "zhou03" & d$study == "exp3")] <-
  "mechanical scarification"

#zlesak07
d$treatmentCor[which(d$datasetID == "zlesak07" & d$study == "exp 1")] <-
  "priming x cold stratification x light"
d$treatmentCor[which(d$datasetID == "zlesak07" & d$study == "exp 2")] <-
  "priming x cold stratification x light"
d$treatmentCor[which(d$datasetID == "zlesak07" & d$study == "exp 3")] <-
  "priming x cold stratification x light"
d$treatmentCor[which(d$datasetID == "zlesak07" & d$study == "exp 4")] <-
  "priming x cold stratification x light"
d$treatmentCor[which(d$datasetID == "zlesak07" & d$study == "exp 5")] <-
  "priming x cold stratification"
d$treatmentCor[which(d$datasetID == "zlesak07" & d$study == "exp 6")] <-
  "soaking (chemical) x cold stratification"
d$treatmentCor[which(d$datasetID == "zlesak07" & d$study == "exp 7")] <-
  "soaking (chemical) x light"

#d$treatmentCor[which(d$datasetID == "")] <- ""

sub_check <- FALSE
if(sub_check){
  sub <- list()
  for(i in 1:nrow(prio)){
    sub[[i]] <- subset(d, datasetID == prio$datasetID[i] &
                         study == prio$study[i] &
                         paste0(genus, "_", species) == prio$genusspecies[i])
  }
  sub <- do.call(rbind, sub)
  sub_short <- subset(sub, select = c("datasetID", "study", "species", "treatment", "other.treatment",
                                          "treatmentCor", "treatmentDetails",
                                          "storageType", "storageDetails", "storageTemp", "storageDuration",
                                          "chillTemp", "chillDuration", "chillTempCycle", "chillLightCycle",
                                          "germTemp", "germDuration", "photoperiodCor",
                                          "scarification", "scarifType", "scarifTypeGen", "scarifTypeSpe",
                                          "soaking", "soaked.in", "soaking.duration",
                                          "chemicalCor", "trt.duration",
                                          "respvar", "response", "figure"))
  
  trt_uniq <- data.frame(sort(unique(sub$treatmentCor)))
}
