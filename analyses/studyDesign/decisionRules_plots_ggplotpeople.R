library(ggplot2)


forc_studies <- newd[paste0(newd$datasetID,newd$study,newd$genusspecies) %in%
                       sapply(1:nrow(newids_forc), function(i) paste0(newids_forc[i,c('datasetID', 'study', 'genusspecies')], collapse = '')),
                     c('datasetID', 'study', 'genusspecies', 
                       'provLatLonAlt',
                       'dormancyDuration', 'dormancyTemp',
                       'germTempGen', 'germDuration', 'germPhotoperiod',
                       'responseValue')]

forc_studies$uniqueid <- paste0(forc_studies$datasetID,forc_studies$study,forc_studies$genusspecies)

forc_studies$dormancyTemp <- ifelse(forc_studies$dormancyDuration == 0, 0, forc_studies$dormancyTemp)
forc_studies$chillhours <- as.numeric(forc_studies$dormancyDuration) * 24 * as.numeric(as.numeric(forc_studies$dormancyTemp) < 10 & as.numeric(forc_studies$dormancyTemp) > -20)
forc_studies$forc <- as.numeric(forc_studies$germTempGen)
forc_studies$t <- as.numeric(forc_studies$germDuration)

plot <- ggplot(data = unique(forc_studies)) +
  facet_wrap(~ uniqueid, scales = 'free', ncol = 10) +
  geom_line(aes(y = responseValue, x = t, group = paste0(forc, chillhours, provLatLonAlt))) +
  theme_classic()

ggsave(plot, filename = paste0("figures/studyDesign/forcingtime_ggplot.pdf"), height = 35, width = 20)
