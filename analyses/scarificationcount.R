# looking at scarification, warm strat and storage


# housekeeping
rm(list=ls())  
options(stringsAsFactors=FALSE)


if(length(grep("christophe_rouleau-desrochers", getwd()) > 0)) {
  setwd("~/Documents/github/egret/analyses")
} else if(length(grep("lizzie", getwd()) > 0)) {
  setwd("/Users/christophe_rouleau-desrochers/Documents/github/egret/analyses")
}
# load librairies
library(ggplot2)

# read csv
d <- read.csv2("output/egretclean.csv", sep=",")



#Create warm strat column
d$warmstrat <- NA
# vector for all treatments that have warm stratification
warmstrat.names <- unique(d$treatment[grep("warm", d$treatment)])
# remove entry that shouldn't be there
warmstrat.names[!warmstrat.names %in% c("cold strat + soak in warm water")]
# add a 1 in warmstrat column whenever "warm" appepeared in the treatment column
d$warmstrat[which(d$treatment %in% warmstrat.names)] <- 1
       

#### Scarification ####
# select only columns needed
scar <- d[, c("datasetIDstudy", "latbi", "scarifType", "scarifTypeGen", "scarifTypeSpe")]

# check how many different types of scarification by each study
# by the column scarifType
type <- unique(scar)

scarifType_agrstudy <- aggregate(type[c("datasetIDstudy")], type["scarifType"] , FUN= length )
scarifType_agrstudy <- scarifType_agrstudy[with(scarifType_agrstudy, order(-datasetIDstudy)), ]

# by the column scarifTypeGen
# double check that, deirdre arrives with a different 
scarifTypeGen_agrstudy <- aggregate(type[c("datasetIDstudy")], type["scarifTypeGen"] , FUN= length )
scarifTypeGen_agrstudy <- scarifTypeGen_agrstudy[with(scarifTypeGen_agrstudy, order(-datasetIDstudy)), ]

# by the column scarifTypeSpe
scarifTypeSpe_agrstudy <- aggregate(type[c("datasetIDstudy")], type["scarifTypeSpe"] , FUN= length )
scarifTypeSpe_agrstudy <- scarifTypeSpe_agrstudy[with(scarifTypeSpe_agrstudy, order(-datasetIDstudy)), ]
head(scarifTypeSpe_agrstudy)

# check how many different types of scarification by each species
# get a smaller df to check by species
scars <- scar[, c("latbi", "scarifType", "scarifTypeGen", "scarifTypeSpe")] 
scar_unique <- unique(scars)

# by the column scarifType
scarifType_agrspp <- aggregate(scar_unique[c("latbi")], scar_unique["scarifType"] , FUN= length )
scarifType_agrspp <- scarifType_agrspp[with(scarifType_agrspp, order(-latbi)), ]
head(scarifType_agrspp)
head(scarifType_agrstudy)
# by the column scarifTypeGen
scarifTypeGen_agrspp <- aggregate(scar_unique[c("latbi")], scar_unique[c("scarifTypeGen")] , FUN = length )
scarifTypeGen_agrspp <- scarifTypeGen_agrspp[with(scarifTypeGen_agrspp, order(-latbi)), ]
head(scarifTypeGen_agrspp)
# by the column scarifTypeSpe
scarifTypeSpe_agrspp <- aggregate(scar_unique[c("latbi")], scar_unique["scarifTypeSpe"] , FUN= length )
scarifTypeSpe_agrspp <- scarifTypeSpe_agrspp[with(scarifTypeSpe_agrspp, order(-latbi)), ]
head(scarifTypeSpe_agrspp)


# figures
# by studies
study<- scarifTypeGen_agrstudy[1:4,]
study.g <- ggplot(data=study, aes(x=scarifTypeGen, y=datasetIDstudy, fill=scarifTypeGen)) +
  geom_bar(stat = "identity")+ 
  labs(x="Scarification type", y="Number of studies")+
  ggtitle("By studies")+
  theme_minimal() +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.title = element_text(hjust = 0.5),
        legend.position = "right")
study.g


# by species
species <- scarifTypeGen_agrspp[1:4,]
species.g <- ggplot(data=species, aes(x=scarifTypeGen, y=latbi, fill=scarifTypeGen)) +
  geom_bar(stat = "identity")+ 
  labs(x="Scarification type", y="Number of species")+
  ggtitle("By species")+
  theme_minimal() +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.title = element_text(hjust = 0.5),
        legend.position = "right")
species.g

