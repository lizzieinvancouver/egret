# Deleted from coordinate_cleaning_TA.R 

egret$datasetID[which(is.na(egret$provenance.lat) & egret$datasetID == "Aldridge1993")] <- "Aldridge1992"
egret$datasetID[which(is.na(egret$provenance.lat) & egret$datasetID == "Aldridge1994")] <- "Aldridge1992"
egret$datasetID[which(is.na(egret$provenance.lat) & egret$datasetID == "Aldridge1995")] <- "Aldridge1992"
egret$datasetID[which(is.na(egret$provenance.lat) & egret$datasetID == "Aldridge1996")] <- "Aldridge1992"
egret$datasetID[which(is.na(egret$provenance.lat) & egret$datasetID == "Aldridge1997")] <- "Aldridge1992"
egret$datasetID[which(is.na(egret$provenance.lat) & egret$datasetID == "Aldridge1998")] <- "Aldridge1992"

egret$study[which(is.na(egret$provenance.lat) & egret$datasetID == "Aldridge1992" & egret$study == "exp3")] <- "exp2"
egret$study[which(is.na(egret$provenance.lat) & egret$datasetID == "Aldridge1992" & egret$study == "exp4")] <- "exp2"
egret$study[which(is.na(egret$provenance.lat) & egret$datasetID == "Aldridge1992" & egret$study == "exp5")] <- "exp2"
egret$study[which(is.na(egret$provenance.lat) & egret$datasetID == "Aldridge1992" & egret$study == "exp6")] <- "exp2"
egret$study[which(is.na(egret$provenance.lat) & egret$datasetID == "Aldridge1992" & egret$study == "exp7")] <- "exp2"
egret$study[which(is.na(egret$provenance.lat) & egret$datasetID == "Aldridge1992" & egret$study == "exp8")] <- "exp2"