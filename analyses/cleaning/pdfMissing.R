#started May 22 2024 D Loughnan
# aim of this code is to get a sense of what papers are missing from our new EGRET Google drive folder:

# This is based on the initial check by Selena and a list of what papers are missing:

rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)

if(length(grep("deirdreloughnan", getwd())>0)) {
  setwd("~/Documents/github/egret")
} else if(length(grep("lizzie", getwd())>0)) {
  setwd("~/Documents/git/projects/egret/")
} else if(length(grep("buonaiuto", getwd())>0)) {
  setwd("~/git/egret/")
} else{
  setwd("~/deirdre/egret") # for midge
}

pap <- read.csv("data/egretPdfCheck.csv") 
unique(pap$pdf.in.folder)

redo <- subset(pap, pdf.in.folder == "N")

done <- subset(pap, pdf.in.folder == "Y")

write.csv(redo, "data/missingPdf.csv")
