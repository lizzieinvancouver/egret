## Started 10 July 2024 ##
## By Dan, continued by Justin ##

## Updated 26 Jan 2025 by Mao ##

###add stratification and chilling
d$pretreatmentChill<-ifelse(!is.na(d$coldStratDurAvg),"Y",d$pretreatmentChill)
d$pretreatmentChill<-ifelse(!is.na(d$coldStratDurMax),"Y",d$pretreatmentChill)
d$pretreatmentChill<-ifelse(!is.na(d$coldStratDurMin),"Y",d$pretreatmentChill)

###add stratification to chilling values
d$pretrtChillDurMin<-ifelse(!is.na(d$coldStratDurMin),d$coldStratDurMin,d$pretrtChillDurMin)
d$pretrtChillDurMax<-ifelse(!is.na(d$coldStratDurMax),d$coldStratDurMax,d$pretrtChillDurMax)
d$pretrtChillDurAvg<-ifelse(!is.na(d$coldStratDurAvg),d$coldStratDurAvg,d$pretrtChillDurAvg)
d$pretreatmentChillDuration<-ifelse(!is.na(d$cold_stratification_days),d$cold_stratification_days,d$pretreatmentChillDuration)

############

aggregate(latbi ~ pretrtChillDurMax, data = d, FUN = function(x) length(unique(x)))

###need to make a rowmnames column
d$X<-rownames(d)


# Step 1: Filter rows with non-NA values in all specified columns
filtered_rows <- d[!is.na(d$pretrtChillDurMin) & !is.na(d$pretrtChillDurMax) & 
                     !is.na(d$responseValueMin) & !is.na(d$responseValueMax), ]

filtered_spp <- d[!is.na(d$pretrtChillDurMin) & !is.na(d$pretrtChillDurMax) & 
                    !is.na(d$responseValueMin) & !is.na(d$responseValueMax), "latbi"]


tmp_X <- filtered_rows$X
spec_X <- filtered_rows$latbi

# Remove all filtered rows from the original data frame
d <- d[!d$X %in% tmp_X, ]
