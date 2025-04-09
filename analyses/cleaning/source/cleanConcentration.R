### started by Dan Mapril 7

sort(unique(d$chemical.concent))
length(unique(d$chemical.concent))

### start witrh some easy fixes
##make a cleaned column

d$chemicalConcent<-d$chemical.concent
d$chemicalConcent<-as.numeric(d$chemicalConcent) ### this shoudl make all non numeric values NA
table(is.na(d$chemicalConcent))

####first clean the NA's


temp<-d[is.na(d$chemicalConcent),]
temp<-temp[,c(27,28,32,33,34,63,77)]
nrow(temp)
### make anything with water as chemical 0 (so that way it wont get dropped)
d$chemicalConcent[which(d$chemicalCor == "H2O")] <- 0
temp<-d[is.na(d$chemicalConcent),]
temp<-temp[,c(27,28,32,33,34,63,77)]
nrow(temp)
###
d$chemicalConcent[which(d$chemical.concent == "0 (control)")] <- 0
temp<-d[is.na(d$chemicalConcent),]
temp<-temp[,c(27,28,32,33,34,63,77)]
nrow(temp)

d$chemicalConcent[which(d$chemical.concent == "0%")] <- 0
temp<-d[is.na(d$chemicalConcent),]
temp<-temp[,c(27,28,32,33,34,63,77)]
nrow(temp)

d$chemicalConcent[which(d$chemical.concent == "0 mM")] <- 0
temp<-d[is.na(d$chemicalConcent),]
temp<-temp[,c(27,28,32,33,34,63,77)]
nrow(temp)

###this is a biggie, if they are true NA (ie no chemical provided make them 0 so they dont get dropped)
d$chemicalConcent[which(is.na(d$chemical.concent)) & is.na(d$chemicalCor)] <- 0
temp<-d[is.na(d$chemicalConcent),]
temp<-temp[,c(27,28,32,33,34,63,77)]
nrow(temp)

temp2<-subset(temp, grepl("mM", temp$chemical.concent))
unique(temp2$chemical.concent)
d$chemicalConcent[which(d$chemical.concent == "10 mM")] <- 10
d$chemicalConcent[which(d$chemical.concent == "0.029 mM")] <- 0.029
d$chemicalConcent[which(d$chemical.concent == "0.089 mM")] <- 0.089
d$chemicalConcent[which(d$chemical.concent == "2.877 mM")] <- 2.877

temp<-d[is.na(d$chemicalConcent),]
temp<-temp[,c(27,28,32,33,34,63,77)]
nrow(temp)

temp2<-subset(temp, grepl("ppm", temp$chemical.concent))
unique(temp2$chemical.concent)
d$chemicalConcent[which(d$chemical.concent == "1000ppm")] <- 1000
d$chemicalConcent[which(d$chemical.concent == "4000ppm")] <- 4000
d$chemicalConcent[which(d$chemical.concent == "0.31 ppm (sucrose)")] <- 0.31
d$chemicalConcent[which(d$chemical.concent ==  "0.46 ppm (sucrose)")] <- 0.46

temp<-d[is.na(d$chemicalConcent),]
temp<-temp[,c(27,28,32,33,34,63,77)]
nrow(temp)

temp2<-subset(temp, grepl("MW", temp$chemical.concent))
unique(temp2$chemical.concent)  
d$chemicalConcent[which(d$chemical.concent == "6000 MW")] <- 6000

temp<-d[is.na(d$chemicalConcent),]
temp<-temp[,c(27,28,32,33,34,63,77)]
nrow(temp)

###then assess the reasonableness if numerical values, I think we many need to make a units column here but wanted to check the decision rules