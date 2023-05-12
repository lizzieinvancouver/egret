#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Checking paper scraping progress
# Started by Hoai Huong -- April 3
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

### Housekeeping
setwd("~/GitHub/oegres")
rm(list = ls()) # Clear whatever is already in R's memory
options(stringsAsFactors=FALSE)# Make sure words are read in as characters rather than factors

### Libraries
library("readxl") # To read Excel files
library(taxize) # To clean species names

### Import data##############################
folder <- list.dirs(path = "data", full.names = FALSE, recursive = TRUE)
folder <- folder[3:9] # Obtain the folders needed

### Merge Data##############################
# Reference
oegres <- read_xlsx("data/oegres.xlsx", sheet = "source")
oegres_DL.xlsx <- read_xlsx("data/oegres_DL.xlsx", sheet = "source")
for(i in 1:length(folder)) {
  file <- list.files(paste("data/", folder[i], sep = ""), pattern = NULL, all.files = TRUE, full.names = FALSE)
  file <- file[3]
  if(folder[i] == "oegres_HHN")
    dataframe <- read_xlsx(paste("data/", folder[i], "/", file, sep = ""), sheet = "source", skip = 4)
  else
    dataframe <- read_xlsx(paste("data/", folder[i], "/", file, sep = ""), sheet = "source")
  #oegres <- rbind(oegres, dataframe)
  assign(file, dataframe)
}
  # Check columns
colnames(oegres)
colnames(oegres_AZ.xlsx)
colnames(oegres_DL.xlsx)
colnames(oegres_DM.xlsx)
colnames(oegres_GG.xlsx)
colnames(oegres_HHN.xlsx)
colnames(oegres_MN.xlsx)
colnames(oegres_SC.xlsx)
colnames(oegres_TA.xlsx)

### Check paper assigned####################
unique(oegres$accept_reject) # "A", "***", "A*", "R*", "*", NA -- Assuming all accepted
  check <- unique(oegres[which(oegres$accept_reject == "***"),]) # All papers assigned
  check <- unique(oegres[which(oegres$accept_reject == "A*"),]) # 3/5 papers assigned
  check <- unique(oegres[which(oegres$accept_reject == "R*"),]) # Paper assigned
  check <- unique(oegres[which(oegres$accept_reject == "*"),]) # Paper not assigned -- ILL needed
  check <- unique(oegres[which(is.na(oegres$accept_reject)),]) # Paper assigned
total <- unique(oegres[,c("studyID","scraped.by")])
assigned <- total[which(!is.na(total$scraped.by)),]
assigned$scraped.by[which(assigned$scraped.by == "AZ" && assigned$scraped.by %in% c("chen15","chen06","chichizola18"))] <- "TA"
assigned$scraped.by[which(assigned$scraped.by == "DL" && assigned$scraped.by %in% c("amooaghaie09"))] <- "GG"
nrow(total) # Total papers: 466
nrow(assigned) # #Papers assigned: 425

### Check paper scraped#####################
scraped <- read.csv("input/raw_oegres.csv")
scraped <- unique(scraped[,c("datasetID","entered.by")])
colnames(scraped) <- c("studyID", "scraped.by")
nrow(scraped) #Papers scraped: 174
not_scraped <- setdiff(assigned,scraped)
# Make sure the datasetID are correct
not_scraped_check <- setdiff(scraped, assigned)

### Check in Excel
not_scraped_DL <- not_scraped[which(not_scraped$scraped.by == "DL"),]
other_lang_DL <- c("tian05","boyaci21") #2
reject_DL <- c("amooaghaie09","abbasi20","ziaf14","yang16","batlla04","braendel04","sherstenikina84") #7
ill_DL <- c() #0
not_scraped_DL <- data.frame(assigned = "DL", paper = c("leinonen97","tamaei01")) #2

not_scraped_AZ <- not_scraped[which(not_scraped$scraped.by == "AZ"),]
other_lang_AZ <- c("lee13","kwon20","koyama08") #3
reject_AZ <- c("khan81", "kim83", "kiseleva20") #3
ill_AZ <- c("kodatskii85","kojima94","kose98","kose00","kovaleva97","kumar08","kuo84","kwon95","lee66","lee93") #10
not_scraped_AZ <- data.frame(assigned = "AZ", paper = c("lee06","klinger20","keul04")) #3

not_scraped_DM <- not_scraped[which(not_scraped$scraped.by == "DM"),] # Check "bibby53" # DM's source tab does not match
other_lang_DM <- c("ghanbari18","bezdeckova12") #2
reject_DM <- c("hamala17","greenwood05","gokturk07","gere15","belletti05") #5
ill_DM <- c()
not_scraped_DM <- data.frame(assigned = "DM", paper = c("grose57","guan89","guo06","reinert18","riasat05","rizwan18","shi09","skordilis95","soltani03","song19","song20","statton17","strazisar13","su16","suh98","sumi94","veiga-barbosa16","vleeshouwers98","vogiatzis95","wagner07","wahid07","walck12","wang18","wang15","wang21")) #25

not_scraped_GG <- not_scraped[which(not_scraped$scraped.by == "GG"),] # Check 4 papers
other_lang_GG <- c("han09","hsieh04","hu12","hwang96","Ï°∞Ï†ïÍ±¥19") #5
reject_GG <- c("huang14","hudson20") #2
ill_GG <- c("han08","han10","harrison14","he09","honsova10") #5
not_scraped_GG <- data.frame(assigned = "GG", paper = c("ahn00","aiello04","amooaghaie06","carron08","castro95","chakraborty92","chang06","ikeda01","Ïù¥ÏàòÍ¥ë14","lempiainen89","leon06","leskova69","li18","li11","li07","li17","li14","li21","li22","li11","liao16","liao10","lim15","limbird13","lin96","lin93","liu16","liu12","liu04")) #29

not_scraped_HHN <- not_scraped[which(not_scraped$scraped.by == "HHN"),]
other_lang_HHN <- c("sharifi16","sharif16","seo12","fateh06","fan10", "dolgacheva77") #6
reject_HHN <- c("angelini21","atabaki21","derakhshan13","flores17","flores11","mangandi09","markovic17") #7
ill_HHN <- c("[anonymous]61","downie98","edwards96","fetouh14","mandossian66","scocco98","sedaghathoor12") #7
not_scraped_HHN <- data.frame(assigned = "HHN", paper = c("elisafenko15","feurtado05","serry12","shen20","wang09","washitani89","washitani85","watanabe02","watanabe22","wawrzyniak20","werner13")) #11

not_scraped_MN <- not_scraped[which(not_scraped$scraped.by == "MN"),]
other_lang_MN <- c("mohammadi21","mondani18","moradi18","moreno13","niu12","oliveira20","qian11","qiu17","qu11","radsarian17","raeisi21","rajabian07","rostamipoor15","ryu17","sun98","taghavi18","taghinezad16","takiya06","tuncer17","wu17") #20
reject_MN <- c("michael21","nomura96","rayburn13","thangjam17","torabi10","tzatzani20") #6
ill_MN <- c("momonoki79","povoa11","racek07","sundaramoorthy93","szabo70","tang06","tanuja20","tashev08","winstead71","xiang18","xiao12","xieNA","xuNA","xu16","yahyaoglu06") #15
not_scraped_MN <- data.frame(assigned = "MN", paper = c("ochuodho08","rodriguez17","rouhi15","tang10","tilki06","yang17","yang18","yang10","yang11","yang20","yang14","yang08","yang06","yang04")) #14

not_scraped_SC <- not_scraped[which(not_scraped$scraped.by == "SC"),]
other_lang_SC <- c("perez01","peng14","pei02","paal03","pan13","naseri16","motallebi11","lv12","luxiaoqian14","jeon10","jo88","mehrabi19","meza04") #13
reject_SC <- c("jolliff94","lopez-corrales14","pangua99") #3
ill_SC <- c("barnhill82","jensen09","joshi03","liu99","liu09","liu88","markovic20","masoomeh09","mattana09","morozowska02","morris16","mughal07","mughal10","mulaudzi09","mutele15","omar21","onursal07","pan09","paolini01") #19
not_scraped_SC <- data.frame(assigned = "SC", paper = c("kalimuthu95","kazinczi98","liu04","liu13","olmez08","patino11")) #6

not_scraped_TA <- not_scraped[which(not_scraped$scraped.by == "TA"),] # Check aldridge
other_lang_TA <- c("chien02","cho05","choi12","nasiri08","nasiri06","nishio09") #6
reject_TA <- c("cho09","denny04") #2
ill_TA <- c("nejad10","al-absi10","chung91") #3
not_scraped_TA <- data.frame(assigned = "TA", paper = c("chien09","nasri14","sacande04","saeed16","sajna19","salehi-eskandari21","salehi15","santos19","sato03","sayedena18","schafer89","schutz02")) #12

other_lang <- c(other_lang_AZ, other_lang_DL, other_lang_DM, other_lang_GG, other_lang_HHN, other_lang_MN, other_lang_SC, other_lang_TA) #57
reject <- c(reject_AZ, reject_DL, reject_DM, reject_GG, reject_HHN, reject_MN, reject_SC, reject_TA) #35
ill <- data.frame(ILL = c(ill_AZ, ill_DL, ill_DM, ill_GG, ill_HHN, ill_MN, ill_SC, ill_TA)) #59
not_scraped <- rbind(not_scraped_AZ, not_scraped_DL, not_scraped_DM, not_scraped_GG, not_scraped_HHN, not_scraped_MN, not_scraped_SC, not_scraped_TA) #102

write.csv(ill, "cleaning/preliminary_HH/ILL_needed.csv")
write.csv(not_scraped, "cleaning/preliminary_HH/not_scraped.csv")