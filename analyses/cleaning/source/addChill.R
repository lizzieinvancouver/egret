library(chillR)

length(which(!is.na(d$chill.tempCor) & !is.na(d$chill.durationCor))) ##16410 to calculate chill
with_chill <- which(!is.na(d$chill.tempCor) & !is.na(d$chill.durationCor) &
                      d$chill.tempCor != "ave" & d$chill.durationCor != "ave")

d$chillHours <- NA
d$chillPortions <- NA
d$chillUnits <- NA

#loop over data points
for(i in with_chill){
  print(i)
  data_point <- d[i,]
  
  if(data_point$chill.durationCor == 0){
    d$chillHours[i] <- d$chillPortions[i] <- d$chillUnits[i] <- 0
    next
  }
  
  temp_all <- str_split_1(toString(data_point$chill.tempCor), " then ")
  dur_all <- as.numeric(str_split_1(toString(data_point$chill.durationCor), " then "))
  cyc_all <- str_split_1(toString(data_point$chill.tempCycle), " then ")
  
  chill <- matrix(NA, nrow = length(temp_all), ncol = 3)
  
  #loop over stratification combinations
  for(j in 1:length(temp_all)){
    temp <- as.numeric(str_split_1(temp_all[j], " and "))
    if(NA %in% temp | is.na(dur_all[j])) {next}
    
    if(length(temp) > 1){
      if(cyc_all[j] == "NA") {next}
      cyc <- as.numeric(str_split_1(cyc_all[j], " and "))
      
      hourly_temp <- rep(c(rep(temp[1], cyc[1]), rep(temp[2], cyc[2])), dur_all[j] * 24 / sum(cyc))
    } else {
      hourly_temp <- rep(temp, dur_all[j] * 24)
    }
    
    chill[j,] <- c(Chilling_Hours(hourly_temp)[length(hourly_temp)],
                   Dynamic_Model(hourly_temp)[length(hourly_temp)],
                   Utah_Model(hourly_temp)[length(hourly_temp)])
  }
  
  d$chillHours[i] <- sum(chill[,1], na.rm = TRUE)
  d$chillPortions[i] <- sum(chill[,2], na.rm = TRUE)
  d$chillUnits[i] <- sum(chill[,3], na.rm = TRUE)
}