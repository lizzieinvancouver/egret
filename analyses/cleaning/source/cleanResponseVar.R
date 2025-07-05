## Upded 11 June 2024 ##
## By Deirdre ##

## This contains code to clean the response variable names and values ##

length(unique(d$respvar)) # 167 raw unique resp vars #dmb Dec 4 2024 find 186

# germination rate
d$responseVar <- d$respvar
d$responseVar[which(d$responseVar == "germ.cap")] <- "germ.capacity"


d$responseVar[which(d$responseVar == "germ.speed")] <- "germ.rate"
d$responseVar[which(d$responseVar == "rates.germ")] <- "germ.rate"
d$responseVar[which(d$responseVar == "germ.rate (days)")] <- "germ.rate"
d$responseVar[which(d$responseVar == "germ.rate.total")] <- "germ.rate"
d$responseVar[which(d$responseVar == "germ.rt")] <- "germ.rate"
d$responseVar[which(d$responseVar == "germination rate")] <- "germ.rate"
d$responseVar[which(d$responseVar == "rate of seed germ")] <- "germ.rate"
d$responseVar[which(d$responseVar == "speed.emerge")] <- "germ.rate"
d$responseVar[which(d$responseVar == "germ.rate")] <- "germ.rate"
d$responseVar[which(d$responseVar == "germ rate")] <- "germ.rate"
d$responseVar[which(d$responseVar == "average germination speed")] <- "germ.rate"
d$responseVar[which(d$responseVar == "mean.germ.rate")] <- "germ.rate"

# Proporation germination
d$responseVar[which(d$responseVar == "germ.proportion")] <- "prop.germ"
d$responseVar[which(d$responseVar == "proportion of viable seeds germinated")] <- "prop.germ"
d$responseVar[which(d$responseVar == "prop.germ")] <- "prop.germ"

#mean germination time
d$responseVar[which(d$responseVar == "mtg")] <- "mgt"
d$responseVar[which(d$responseVar == "mean.germ.time")] <- "mgt"
d$responseVar[which(d$responseVar == "MGT")] <- "mgt"
d$responseVar[which(d$responseVar == "MTG")] <- "mgt"
d$responseVar[which(d$responseVar == "M.T.G")] <- "mgt"
d$responseVar[which(d$responseVar == "mean time (day)")] <- "mgt"
d$responseVar[which(d$responseVar == "meandaystogerm")] <- "mgt"
d$responseVar[which(d$responseVar == "mean germ.time")] <- "mgt"

d$responseVar[which(d$responseVar == "med.time.germ")] <- "median.germ.time"

d$responseVar[which(d$responseVar == "days of first observation of seedling emergence ")] <- "day.first.emergence"
d$responseVar[which(d$responseVar == "days taken until first seed germination")] <- "day.first.emergence"
d$responseVar[which(d$responseVar == "days to 1st germination")] <- "day.first.emergence"
d$responseVar[which(d$responseVar == "days to 1st seed germinated")] <- "day.first.emergence"
d$responseVar[which(d$datasetID == "liu13" & d$responseVar == "lag")] <- "day.first.emergence"
d$responseVar[which(d$responseVar == "lag")] <- "day.first.emergence"
d$responseVar[which(d$responseVar == "LAG")] <- "day.first.emergence"

d$responseVar[which(d$responseVar == "days to 50% emergence")] <- "50%.germ"
d$responseVar[which(d$responseVar == "time taken to 50% germ")] <- "50%.germ"
d$responseVar[which(d$responseVar == "D50")] <- "50%.germ"
d$responseVar[which(d$responseVar == "days50germ")] <- "50%.germ"
d$responseVar[which(d$responseVar == "50% germ")] <- "50%.germ"
d$responseVar[which(d$responseVar == "D50")] <- "50%.germ"
d$responseVar[which(d$responseVar == "t50")] <- "50%.germ"
d$responseVar[which(d$responseVar == "T51")] <- "50%.germ"
d$responseVar[which(d$responseVar == "T52")] <- "50%.germ"
d$responseVar[which(d$responseVar == "T53")] <- "50%.germ"
d$responseVar[which(d$responseVar == "T54")] <- "50%.germ"
d$responseVar[which(d$responseVar == "T55")] <- "50%.germ"
d$responseVar[which(d$responseVar == "T56")] <- "50%.germ"
d$responseVar[which(d$responseVar == "T57")] <- "50%.germ"
d$responseVar[which(d$responseVar == "T58")] <- "50%.germ"
d$responseVar[which(d$responseVar == "T59")] <- "50%.germ"
d$responseVar[which(d$responseVar == "T60")] <- "50%.germ"
d$responseVar[which(d$responseVar == "T61")] <- "50%.germ"
d$responseVar[which(d$responseVar == "T62")] <- "50%.germ"

d$responseVar[which(d$responseVar == "TMGR.max")] <- "time.of.max.germ.rate"


d$responseVar[which(d$responseVar == "time taken to 50%.germ")] <- "50%.germ"
d$responseVar[which(d$responseVar == "germ.time (50%)")] <- "50%.germ"
d$responseVar[which(d$responseVar == "germ.rate ((l/t50)")] <- "50%.germ"
d$responseVar[which(d$responseVar == "half time (days)")] <- "50%.germ"

d$responseVar[which(d$responseVar == "embryo/seed ratio")] <- "embryo:seed.ratio"

d$responseVar[which(d$responseVar == "germ time (days)")] <- "germ.time"
d$responseVar[which(d$responseVar == "germ.time (mean)")] <- "germ.time"
d$responseVar[which(d$responseVar == "mean germ.time")] <- "germ.time"
d$responseVar[which(d$responseVar == "days to final germination")] <- "germ.time" #to discuss

d$responseVar[which(d$responseVar == "germ value (D)")] <- "germ.value"
d$responseVar[which(d$responseVar == "germ value")] <- "germ.value"
d$responseVar[which(d$responseVar == "germ.val")] <- "germ.value"

d$responseVar[which(d$responseVar == "germ rate (days)")] <- "germ.time"
d$responseVar[which(d$responseVar == "germ.days")] <- "germ.time"
d$responseVar[which(d$responseVar == "days to germ.")] <- "germ.time"

d$responseVar[which(d$responseVar == "mean daily germ")] <- "mean.daily.germ"

# percent germination
d$responseVar[which(d$responseVar == "final per.germ")] <- "percent.germ"
d$responseVar[which(d$responseVar == "mean per germ")] <- "percent.germ"
d$responseVar[which(d$responseVar == "mean per.germ")] <- "percent.germ"
d$responseVar[which(d$responseVar == "mean germination percent")] <- "percent.germ"
d$responseVar[which(d$responseVar == "per.germ.petri")] <- "percent.germ"
d$responseVar[which(d$responseVar == "per.germ.runningwater")] <- "percent.germ"
d$responseVar[which(d$responseVar == "per.emerge")] <- "percent.germ"
d$responseVar[which(d$responseVar == "per.germ")] <- "percent.germ"
d$responseVar[which(d$responseVar == "per.germ")] <- "percent.germ"
d$responseVar[which(d$responseVar == "per.germ.cumulative")] <- "percent.germ"
d$responseVar[which(d$responseVar == "per.germ.cumulative.cumulative")] <- "percent.germ"
d$responseVar[which(d$responseVar == "cumulative germination percent")] <- "percent.germ"

d$responseVar[which(d$responseVar == "meandailygerm")] <- "mean.daily.germ"

d$responseVar[which(d$responseVar == "germination energy")] <- "germ.energy"
d$responseVar[which(d$responseVar == "germination.energy")] <- "germ.energy"

d$responseVar[which(d$responseVar == "germ.rate.index")] <- "germ.index"
d$responseVar[which(d$responseVar == "germ index")] <- "germ.index"
d$responseVar[which(d$responseVar == "mean germ index")] <- "germ.index"
d$responseVar[which(d$responseVar == "mean germination index")] <- "germ.index"

# Checking odd entries---specific papers
d$responseVar[which(d$datasetID == "liu13" & d$responseVar == "a")] <- "a:variation MS max germ percent" #source variation mean square maximum cumulative germination percentage equivalent to germination capacity
d$responseVar[which(d$datasetID == "liu13" & d$responseVar == "b")] <- "b:variation MS germ percent" # maximum cumulative germination percentage equivalent to germination capacity
d$responseVar[which(d$datasetID == "liu13" & d$responseVar == "c")] <- "c:variation MS T50" # c the time required to achieve 50% germination = R50
d$responseVar[which(d$datasetID == "liu13" & d$responseVar == "TMGR")] <- "time.of.max.germ.rate"
d$responseVar[which(d$datasetID == "liu13" & d$responseVar == "D_lag-50")] <- "duration.between.lag.and.c"
d$responseVar[which(d$datasetID == "liu13" & d$responseVar == "DI")] <- "dormancy.index"# area between the germination 

d$responseVar[which(d$datasetID == "schutz02" & d$figure == "Table 3")] <- "days.to.1%"# area between the germination 

d$responseVar[which(d$datasetID == "pritchard93" & d$figure == "Figure 2")] <- "percent.germ.probability"# area between the germination 

# liu13 data of % germ from table 1 missing

#	zhang21 one is not in english

# Boscagli01 coefficient rate of germination: (sum(days since sowing*no. of seeds germinated on day since sowing)/sum(no. of seeds germinated on day since sowing))*100

# Does germ.time = germ.period = germ
#ahola99 --- germ.prob germ.rate.index, germ.speed(%/day); germination.energy= germ.energy; 

#Edwards96: 
#germination   (GC) = % seeds germinated normally by the end of the test; 
#germination rate = R50; 
#peak value = daily accumulated germinates/corresponding day number---equivalent to Werner13 germ speed index?
# germination value = value combining germ speed and completeness

#Farhadi13
#germination capacity = (sum(number seeds germ each day)/total number seeds sown)*100 (% units)

#Werner13;
# germination speed index
d$responseVar[which(d$datasetID == "werner13" & d$responseVar == "GSI")] <- "germination.speed.index"# sum of seeds germinated in a day over number of days since start experiment

#Fetouh14; to discuss
#imbi.period = Inbibitions period = number of days from sowing to germination beginning and total germination period (TGP)
# TGP = total germination period
# Speed of emergence
# germination index = sum(No germinated seed/day of experiment)
# germination value = combines germination speed and total germination (eqn given)

#santos19; 
# IVG = index of germination speed
d$responseVar[which(d$datasetID == "santos19" & d$responseVar == "IVG")] <- "germination.speed.index"# average time and averge speed
#Naseri18 & Nin17
#lag = interval between start exper and first germ

#tilki97;
#peak value = % germination/number of elapsed days

#tilki06;
# survival: appears to be for seedling data (table 3), I think we just want data from table 2 on germination

#proportion of viable seeds = prop.germ harrison14; 

#NA  = tan10_2; castro95; huang14; downie91;strazisar13

d$responseVar[which(d$datasetID == "zlesak07" & d$responseVar == "AUGPC")] <- "area.under.germination.progress.curve" 
d$responseVar[which(d$datasetID == "conversa09" & d$responseVar == "cumulative.germ")] <- "percent.germ" 
d$responseVar[which(d$datasetID == "ranil15" & d$responseVar == "early per.germ")] <- "early.per.germ.day.4" 
d$responseVar[which(d$datasetID == "grose57" & d$responseVar == "G.E.I")] <- "germ.energy" 

sort(unique(d$responseVar))

temp <- subset(d, responseVar == "survival"); unique(temp$datasetID)

# response var tangential to germination:
other <- c("a:variation MS max germ percent","a.max","a.min","ABA_embryo (μg/g)","ABA_endosperm (μg/g)","adventitious.root.diameter","adventitious.root.length", "amylase.concentration","amylase.specific.activity", "amylase.unit.activity", "area.under.germination.progress.curve", "b:variation MS germ percent", "b.max", "b.min","base temperature","c:variation MS T50","c.max","c.min","CO2.evolution", "coeff.rate.germ", "cotyledon.area","cotyledon.length", "cotyledon.width","embryo:seed.ratio", "ethanol.formation", "GA/ABA_embryo", "GA/ABA_endosperm","GA3_embryo (μg/g)","GA3_endosperm (μg/g)", "growth.collar.diameter.cm", "growth.height.cm","growth.rate (cm days^-1)","growth.rate %", "hypocotyl.ave.diameter","hypocotyl.length", "moisture", "moisture content","normal seedling percentage","num.shoot","number per.seedlings","O2.uptake","oxygen.absorbance", "per.increase.seedmass","per.survive","per.ungerminated.fresh", "per.weight.gain", "primary.root.diameter","primary.root.length", "respiratory.rate","RNA.content","root.dryweight(g)","seed.moisture.content.fresh.weight", "shoot.diam(mm)","shoot.dryweight(g)","shoot.height(cm)","shootroot.ratio", "survival", "thermal time","vigour index (days)", "water.absorption","weight.gram") # 61 
# cleaning values :  "L.sativa.per.germ",
d$responseValue <- (d$response)
d$responseValueNum <- as.numeric(d$responseValue)
print("This `NAs introduced by coercion' happens when we make ResponseValue numeric (responseValueNum column) ")
#View(sort(unique(d$responseValueNum)))

# rows of data, number study (experiments per paper), papers, species
temp <- subset(d, responseVar == "percent.germ")
(nrow(temp)/nrow(d))*100 #77% data #dmb on Dec 4 2024 75.6%

# yan18 -ve value Fig 6; tabatabaeian18, Acosta12, Washitani85, battaglia93 > 100
#"1/5" 
temp <- subset(d, responseVar == "prop.germ") # values look reasonable
(nrow(temp)/nrow(d))*100 #0.3% data

temp <- subset(d, responseVar == "germ.rate") 
(nrow(temp)/nrow(d))*100 #3% data
#raisi13 - entered with backslashes, table 6

temp <- subset(d, responseVar == "mgt") 
(nrow(temp)/nrow(d))*100 #5% data
# brenchley98 >100
temp <- subset(d, responseVar == "50%.germ") 
(nrow(temp)/nrow(d))*100 #1.8% data
#Necajeva13 pritchard93 bytnerowicz14 pritchard93 50% germ 332?
temp <- subset(d, responseVar == "germ.time") # values look reasonable
(nrow(temp)/nrow(d))*100 #2% data
#wytsalucy21 table 2 no resp data winstead 71 table 1

# what do the -ve values mean?
#battaglia93
#ren 15

# I assume NG means no germination, should this not be 0
d$response[which(d$response == "NG")] <- "0"
d$response[which(d$response == "NA*")] <- "NA"

#TO CHECK what is "NA*"
d$errorType <- d$error.type
sort(unique(d$errorType))

# what is SEM, max standard
d$errorType[which(d$errorType == "mean+/-SE")] <- "SE"
d$errorType[which(d$errorType == "standard error")] <- "SE"
d$errorType[which(d$errorType == "mean standard error")] <- "SE"
d$errorType[which(d$errorType == " +/-SE")] <- "SE"
d$errorType[which(d$errorType == "se")] <- "SE"


d$errorType[which(d$errorType == "mean+/-SD")] <- "SD"
d$errorType[which(d$errorType == "standard deviation")] <- "SD"

d$errorType[which(d$errorType == "95 % confidence interval")] <- "95% CI"
d$errorType[which(d$errorType == "95CI")] <- "95% CI"

d$errorType[which(d$errorType == "not.specified")] <- "not specified"
d$errorType[which(d$errorType == "unknown")] <- "not specified"

d$resp.error[which(d$resp.error == "indistinguisable")] <- NA
d$resp.error[which(d$resp.error == "numbers in bracket?")] <- NA
d$resp.error[which(d$resp.error == "NG")] <- NA
d$resp.error[which(d$resp.error == "#REF!")] <- NA

# temp <- subset(d, errorType == "SEM")  #"bungard97"
# temp <- subset(d, errorType == "max standard") #"Washitani89"

dError <- subset(d, !is.na(resp.error))
nrow(dError)/nrow(d)  # 11782 rows has an entry for resp.error and 19155 rows have none; 38% of the data has some sort of entry for error Dec 4 24, dan for 3%

# How many studies have sample size---n.per.rep and reps
temp <- subset(d, !is.na(n.per.rep))
nrow(temp)/nrow(d) # 88% 

sort(unique(temp$n.per.rep))
#"100 + " "1196" "168" "598" "1"

temp <- subset(d, !is.na(reps))
nrow(temp)/nrow(d) # 86% 
sort(unique(temp$reps))

#"0, 0.5, 50, 60, 70, 100

temp <- d[,c("datasetID", "responseVar"#, "responseValue", "errorType"
             )]
temp <- unique(temp)


##########################################################
# Check response variable units: especially germination rate and capacity

germRate <- subset(d, responseVar == "germ.rate"); sort(unique(germRate$datasetID))
germSpeed <- subset(d, responseVar == "germ.speed(%/day)"); sort(unique(germSpeed$datasetID))
germSpeedIndex <- subset(d, responseVar =="germination.speed.index"); sort(unique(germSpeedIndex$datasetID))
germTime <- subset(d, responseVar =="germ.time"); sort(unique(germTime$datasetID))

germCap <- subset(d, responseVar == "germ.capacity")
# Farhadi13: unit = %
sort(unique(germCap$datasetID))


# "amini2018"   - % seeds /week
# "beikmohammadi12" — # seeds/day
# "bungard97”—   days to T50
# "cicek08”—               
# "edwards96" — days to T50            
# "feng18”— # seeds/day                
# "fetouh14”— no units, maybe %, (no seeds after 70 days/ no seeds after 120 days)*100             
# "fulbright86" — # seeds per day         
# "jabarzare11" — % seeds /days          
# "javanmard14”— # seeds/day           
# "kamareh12”— % seeds/ days            
# "keshtkar08”— # seeds/day           
# "ma03" — % seeds/day                 
# "markovic20"—  %          
# "martinik14"— % seeds/day           
# "moeini21"—  # seeds/day           
# "muller03"—   germ rate should be mgt           
# "nasri14"—  # seeds/day           
# "olmez07"—   # seeds/day             
# "olmez08" —  # seeds/day            
# "olmez09"—   # seeds/day             
# "olmez17"—   # seeds/day               
# "parvin15"—    # seeds/day             
# "rahnama-ghahfarokhi07"— # seeds/day  
# "raisi13" —   # seeds/day              
# "ranil15" —   % seeds/day         
# "santos19"— no units reported             
# "statton17"— # seeds/day        
# "surya17"—  # seeds/day            
# "tabatabaeian18" —  # seeds/day    
# "teimouri13"—   # seeds/day            
# "tilki06"—   # seeds/day            
# "vleeshouwers98" — (1/T50)/ days  inverse median of the distribution of germination times       
# "yan16”—% seeds after 60 days                 
# "yang20" — 1/T50 per days               
# "yaqoob17”— cites: Ashtari R, Heidari M, Omidi M, Zare AR (2013) Seed germination and dormancy breaking techniques for Ducrosia anethifolia (DC.). Trakia J Sci 11:82–87   
# "yusefi-tanha19" — # seeds/day 
# "zare11”— # seeds/day   

## Lizzie (5 July 2025) renaming 
names(d)[names(d)=="errorType"] <- "responseErrorType"
