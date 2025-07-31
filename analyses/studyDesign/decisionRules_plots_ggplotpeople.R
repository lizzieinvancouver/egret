library(ggplot2)

# Identify studies that have more strat. or more germ. treatments
newdcopy <- newd[!is.na(newd$germDuration) & !is.na(newd$germTempGen),]
newdcopy$nstrat <- paste0(newdcopy$stratTemp_condensed, newdcopy$stratDur_condensed)
newdcopy$ngerm <- paste0(newdcopy$germTempGen, newdcopy$germDuration)
ntreats <- merge(
  aggregate(nstrat ~ datasetID + study + genusspecies, data = newdcopy, function(x) length(unique(x))),
  aggregate(ngerm ~ datasetID + study + genusspecies, data = newdcopy, function(x) length(unique(x)))
)
newids_chill <- unique(ntreats[ntreats$nstrat > ntreats$ngerm, c('datasetID', 'study', 'genusspecies')])
newids_forc <- unique(ntreats[ntreats$ngerm >= ntreats$nstrat, c('datasetID', 'study', 'genusspecies')])

forc_studies <- newd[paste0(newd$datasetID,newd$study,newd$genusspecies) %in%
                       sapply(1:nrow(newids_forc), function(i) paste0(newids_forc[i,c('datasetID', 'study', 'genusspecies')], collapse = '')),
                     c('datasetID', 'study', 'genusspecies', 
                       'provLatLonAlt',
                       'stratTemp_condensed', 'stratDur_condensed',
                       'germTempGen', 'germTempClass', 'germDuration', 'germPhotoperiod',
                       'responseValueNum')]

forc_studies$uniqueid <- paste0(forc_studies$datasetID,forc_studies$study,forc_studies$genusspecies)
forc_studies$t <- as.numeric(forc_studies$germDuration)

plot <- ggplot(data = unique(forc_studies)) +
  facet_wrap(~ uniqueid, scales = 'free', ncol = 10) +
  geom_line(aes(y = responseValueNum, x = t, group = paste0(germTempGen, germTempClass, stratTemp_condensed, stratDur_condensed, provLatLonAlt))) +
  theme_classic()

ggsave(plot, filename = paste0("figures/studyDesign/forcingtime_ggplot.pdf"), height = 35, width = 20)
