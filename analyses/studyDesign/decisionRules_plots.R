
newdcopy <- newd[!is.na(newd$germDuration) & !is.na(newd$germTempGen),]
newdcopy$nstrat <- paste0(newdcopy$dormancyTemp, newdcopy$dormancyDuration)
newdcopy$ngerm <- paste0(newdcopy$germTempGen, newdcopy$germDuration)
ntreats <- merge(
  aggregate(nstrat ~ datasetID + study + genusspecies, data = newdcopy, function(x) length(unique(x))),
  aggregate(ngerm ~ datasetID + study + genusspecies, data = newdcopy, function(x) length(unique(x)))
)

newids <- unique(ntreats[c('datasetID', 'study', 'genusspecies')])

newids_chill <- unique(ntreats[ntreats$nstrat > ntreats$ngerm, c('datasetID', 'study', 'genusspecies')])
newids_forc <- unique(ntreats[ntreats$ngerm >= ntreats$nstrat, c('datasetID', 'study', 'genusspecies')])

pdf(file=paste0("figures/studyDesign/chillhours.pdf"), height = 15, width = 18)
par(mfrow = c(11,13), mar=c(1.4,0,0,0)+0.7, mgp=c(0,0.5,0))
for(i in 1:nrow(newids_chill)){

  di <- newd[paste0(newd$datasetID,newd$study,newd$genusspecies) ==
               paste0(newids_chill[i,c('datasetID', 'study', 'genusspecies')], collapse = ''),
             c('dormancyDuration', 'dormancyTemp',
               'germTempGen', 'germDuration', 'germPhotoperiod',
               'responseValueNum')]

  di$chillhours <- as.numeric(di$dormancyDuration) * 24 * as.numeric(as.numeric(di$dormancyTemp) < 10 & as.numeric(di$dormancyTemp) > -20)
  di$forc <- as.numeric(di$germDuration) * as.numeric(di$germTempGen)

  plot.new()
  limits <- c(min(di$chillhours, na.rm = T), max(di$chillhours, na.rm = T))
  plot.window(xlim = limits, ylim = c(0,100))
  grid()

  points(di$responseValueNum ~ di$chillhours, pch = 19, col = '#498ba7', cex = 0.5)
  # for(f in unique(paste0(di$germTempGen, di$germDuration))){
  #   subi <- di[paste0(di$germTempGen, di$germDuration) == f, ]
  #   lines(subi$responseValueNum ~ subi$chillhours, col = '#498ba7')
  # }

  title(ylab = "Germ. perc.", cex.lab = 0.5)
  title(xlab = "Chill hours", cex.lab = 0.5, line = -0.1)
  title(paste("ID", i, paste(newids_chill[i,c('datasetID', 'study', 'genusspecies')], collapse = '|')), adj=0, cex.main = 0.5)

}
dev.off()

pdf(file=paste0("figures/studyDesign/forcingtime.pdf"), height = 15, width = 18)
par(mfrow = c(11,13), mar=c(1.4,0,0,0)+0.7, mgp=c(0,0.5,0))
for(i in 1:nrow(newids_forc)){

  di <- newd[paste0(newd$datasetID,newd$study,newd$genusspecies) ==
               paste0(newids_forc[i,c('datasetID', 'study', 'genusspecies')], collapse = ''),
             c('dormancyDuration', 'dormancyTemp',
               'germTempGen', 'germDuration', 'germPhotoperiod',
               'responseValueNum')]

  di$chillhours <- as.numeric(di$dormancyDuration) * 24 * as.numeric(as.numeric(di$dormancyTemp) < 10 & as.numeric(di$dormancyTemp) > -20)
  di$forc <- as.numeric(di$germTempGen)
  di$t <- as.numeric(di$germDuration)

  plot.new()
  limits <- c(min(di$t, na.rm = T), max(di$t, na.rm = T))
  plot.window(xlim = limits, ylim = c(0,100))
  grid()

  points(di$responseValueNum ~ di$t, pch = 19, col = '#498ba7', cex = 0.5)
  # for(f in unique(paste0(di$germTempGen, di$germDuration))){
  #   subi <- di[paste0(di$germTempGen, di$germDuration) == f, ]
  #   lines(subi$responseValueNum ~ subi$chillhours, col = '#498ba7')
  # }

  title(ylab = "Germ. perc.", cex.lab = 0.5)
  title(xlab = "Time", cex.lab = 0.5, line = -0.1)
  title(paste("ID", i, paste(newids_forc[i,c('datasetID', 'study', 'genusspecies')], collapse = '|')), adj=0, cex.main = 0.5)

}
dev.off()

rm(list=ls()[which(!(ls() %in% c('d', 'newd')))])
