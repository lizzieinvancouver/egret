

timeline_plot <- function(dataplot, add.names = FALSE, cutstorage = 200, cutstratperiod = 45, cutstrat = 201){
  
  timeline <- ggplot() +
    
    # Background
    geom_rect(
      data = data.frame(xmin = c(-cutstorage-60, 5,25+1), xmax= c(5-1,25,cutstrat+60+30), ymin = c(-10,-10,-10), ymax = c(+Inf,+Inf,+Inf), alpha = c("show", "no","show")),
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, alpha = alpha), fill = "grey") +
    scale_alpha_manual(values = c(0.1,0), breaks = c("show", "no"), guide = 'none') +
    
    ## storage duration is unknown
    geom_point(
      data = dataplot %>% dplyr::filter(is.na(storageDuration)),
      aes(y = uniqueID, x = -1, color = storageTempSimplified),
      size = 0.1) +
    geom_text(
      data = dataplot %>% dplyr::filter(is.na(storageDuration)),
      aes(y = uniqueID, x = -1, color = storageTempSimplified, label = "?"),
      size = 0.6, color = "white") + 
    ## storage duration is 0
    geom_text(
      data = dataplot %>% dplyr::filter(storageDuration == 0),
      aes(y = uniqueID, x = -1, color = storageTempSimplified, label = "0"),
      size = 0.6) +
    ## storage duration is known...
    geom_segment(
      data = dataplot %>% dplyr::filter(!is.na(storageDuration)) %>% dplyr::mutate(storageDuration = if_else(storageDuration > cutstorage, cutstorage, storageDuration)),
      aes(y = uniqueID, yend = uniqueID, x = -storageDuration, xend = 0, 
          color = storageTempSimplified), 
      linewidth = 0.55) +
    ## ... and longer than 300 days
    geom_segment(
      data = dataplot %>% dplyr::filter(storageDuration > cutstorage),
      aes(y = uniqueID, yend = uniqueID, x = -cutstorage-60, xend = -cutstorage-2,  color = storageTempSimplified), 
      linewidth = 0.2, linetype = "dotted") +
    scale_color_manual(
      name = "storage",
      values = c("#3B6790", "#67903b", "#EFB036"), breaks = c("cold", "ambient", "warm"), na.value = "grey80") + 
    
    
    ggnewscale::new_scale_color() +
    geom_segment(
      data = dataplot %>% dplyr::filter(!is.na(scarifTypeGenSimplified)),
      aes(y = uniqueID, yend = uniqueID, x = 6, xend = 24, 
          color = scarifTypeGenSimplified), 
      linewidth = 0.55) +
    geom_text(
      data = dataplot %>% dplyr::filter(is.na(scarifTypeGenSimplified)),
      aes(y = uniqueID, x = 15, label = "-"),
      size = 0.6) +
    scale_color_manual(
      name = "scarification",
      values = c("#90643b", "#d38783", "#903b67"), breaks = c("mechanical", "hot water", "chemical"), na.value = "grey80") + 
    
    ggnewscale::new_scale_color() +
    # no stratification?
    geom_text(
      data = dataplot %>% dplyr::filter((is.na(chillTemp) & is.na(stratDuration))),
      aes(y = uniqueID, x = 32, label = "-"),
      size = 0.6) +
    geom_text(
      data = dataplot %>% dplyr::filter(stratDuration == 0),
      aes(y = uniqueID, x = 32, label = "0"),
      size = 0.6) +
    
    # # stratification duration is unknown
    # geom_point(
    #   data = dataplot %>% dplyr::filter(is.na(stratDuration) & !is.na(chillTemp)),
    #   aes(y = uniqueID, x = 32),
    #   size = 0.1, color = "red") +
    # geom_text(
    #   data = dataplot %>% dplyr::filter(is.na(stratDuration) & !is.na(chillTemp)),
    #   aes(y = uniqueID, x = 32, label = "?"),
    #   size = 0.6, color = "white") + 
    # # stratification duration is 0
    # geom_text(
    #   data = dataplot %>% dplyr::filter(stratDuration == 0),
    #   aes(y = uniqueID, x = 32, label = "0"), color = "red",
    #   size = 0.6) +
    # # stratification duration is known...
    # geom_segment(
    #   data = dataplot %>% dplyr::filter(!is.na(stratDuration)) %>% dplyr::mutate(stratDuration = if_else(stratDuration > cutstrat, cutstrat, stratDuration)),
    #   aes(y = uniqueID, yend = uniqueID, x = 30, xend = 30 + stratDuration), 
    #   linewidth = 0.55, color = "red") +
    # # ... and longer than 300 days
    # geom_segment(
    #   data = dataplot %>% dplyr::filter(stratDuration > cutstrat),
    #   aes(y = uniqueID, yend = uniqueID, x = 30+cutstrat+2, xend = 30+cutstrat+60), 
    #   linewidth = 0.2, linetype = "dotted", color = "red") +
    
    # decomposed stratification
    ## first period
    ### duration is unknown
    geom_point(
      data = dataplot %>% dplyr::filter(is.na(chillDurPer1) & !is.na(chillTempPer1)),
      aes(y = uniqueID, x = 40, color = chillTempPer1),
      size = 0.1) +
    geom_text(
      data = dataplot %>% dplyr::filter(is.na(chillDurPer1) & !is.na(chillTempPer1)),
      aes(y = uniqueID, x = 40, label = "?"),
      size = 0.6, color = "white") + 
    ### duration is known, only one period
    geom_segment(
      data = dataplot %>% dplyr::filter(!is.na(chillDurPer1) & is.na(chillTempPer2) & is.na(chillTempPer3) & is.na(chillTempPer4)) %>% dplyr::mutate(chillDurPer1 = if_else(chillDurPer1 > cutstrat, cutstrat, chillDurPer1)),
      aes(y = uniqueID, yend = uniqueID, x = 30, xend = 30 + chillDurPer1, color = chillTempPer1), 
      linewidth = 0.55) +
    ### duration is known (>cutstrat), only one period
    geom_segment(
      data = dataplot %>% dplyr::filter(!is.na(chillDurPer1) & is.na(chillTempPer2) & is.na(chillTempPer3) & is.na(chillTempPer4) & chillDurPer1 > cutstrat),
      aes(y = uniqueID, yend = uniqueID, x = 30 + cutstrat, xend = 30 + cutstrat + 30, color = chillTempPer1), 
      linewidth = 0.2, linetype = "dotted") +
    ### duration is known, more than one period
    geom_segment(
      data = dataplot %>% dplyr::filter(!is.na(chillDurPer1) & !(is.na(chillTempPer2) & is.na(chillTempPer3) & is.na(chillTempPer4))) %>% dplyr::mutate(chillDurPer1 = if_else(chillDurPer1 > cutstratperiod*1 & stratDuration > cutstrat, cutstratperiod*1, chillDurPer1)),
      aes(y = uniqueID, yend = uniqueID, x = 30, xend = 30 + chillDurPer1, color = chillTempPer1), 
      linewidth = 0.55) +
    ### duration is known (> cutstratperiod), more than one period
    geom_segment(
      data = dataplot %>% dplyr::filter(!is.na(chillDurPer1) & !(is.na(chillTempPer2) & is.na(chillTempPer3) & is.na(chillTempPer4)) & chillDurPer1 > cutstratperiod & stratDuration > cutstrat),
      aes(y = uniqueID, yend = uniqueID, x = 30 + cutstratperiod*1, xend = 30 + cutstratperiod*1 + (57.5-cutstratperiod), color = chillTempPer1), 
      linewidth = 0.2, linetype = "dotted") +
    
    ## second period
    ### duration is unknown
    geom_point(
      data = dataplot %>% dplyr::filter(is.na(chillDurPer2) & !is.na(chillTempPer2)),
      aes(y = uniqueID, x = 40+57.5, color = chillTempPer2),
      size = 0.1) +
    geom_text(
      data = dataplot %>% dplyr::filter(is.na(chillDurPer2) & !is.na(chillTempPer2)),
      aes(y = uniqueID, x = 40+57.5, label = "?"),
      size = 0.6, color = "white") +
    ### duration is known, more than one period
    geom_segment(
      data = dataplot %>% dplyr::filter(!is.na(chillDurPer1) & !is.na(chillTempPer2)) %>% dplyr::mutate(chillDurPer2 = if_else((chillDurPer2 > cutstratperiod & stratDuration > cutstrat) , cutstratperiod, chillDurPer2),
                                                                                                        chillDurPer1 = if_else((chillDurPer1 > cutstratperiod & stratDuration > cutstrat) | is.na(chillDurPer1), 57.5*1, chillDurPer1)),
      aes(y = uniqueID, yend = uniqueID, x = 30 + chillDurPer1, xend = 30 + chillDurPer1 + chillDurPer2, color = chillTempPer2),
      linewidth = 0.55) +
    ### duration is known (> cutstratperiod), more than one period
    geom_segment(
      data = dataplot %>% dplyr::filter(!is.na(chillDurPer1) & !is.na(chillTempPer2) & chillDurPer2 > cutstratperiod & stratDuration > cutstrat) %>% dplyr::mutate(chillDurPer1 = if_else((chillDurPer1 > cutstratperiod & stratDuration > cutstrat) | is.na(chillDurPer1), 57.5*1, chillDurPer1)),
      aes(y = uniqueID, yend = uniqueID, x = 30 + chillDurPer1 + cutstratperiod, xend = 30 + chillDurPer1 + cutstratperiod + (57.5-cutstratperiod), color = chillTempPer2),
      linewidth = 0.2, linetype = "dotted") +
    ### nothing is known, but there is a third period (a priori)
    geom_segment(
      data = dataplot %>% dplyr::filter(!is.na(chillDurPer1) & is.na(chillTempPer2) & is.na(chillDurPer2) & !is.na(chillTempPer3)) %>% dplyr::mutate(chillDurPer1 = if_else((chillDurPer1 > cutstratperiod & stratDuration > cutstrat) | is.na(chillDurPer1), 57.5*1, chillDurPer1),
                                                                                                                                                     chillDurPer2 = 20),
      aes(y = uniqueID, yend = uniqueID, x = 30 + chillDurPer1 + 2, xend = 30 + chillDurPer1 + chillDurPer2),
      linewidth = 0.05, color = "grey40", linetype = "dashed") +
    
    ## third period
    ### duration is unknown
    geom_point(
      data = dataplot %>% dplyr::filter(is.na(chillDurPer3) & !is.na(chillTempPer3)),
      aes(y = uniqueID, x = 40+57.5+57.5, color = chillTempPer3),
      size = 0.1) +
    geom_text(
      data = dataplot %>% dplyr::filter(is.na(chillDurPer3) & !is.na(chillTempPer3)),
      aes(y = uniqueID, x = 40+57.5+57.5, label = "?"),
      size = 0.6, color = "white") + 
    ### duration is known, more than one period
    geom_segment(
      data = dataplot %>% dplyr::filter(!is.na(chillDurPer1) & !is.na(chillTempPer3)) %>% dplyr::mutate(chillDurPer3 = if_else((chillDurPer3 > cutstratperiod & stratDuration > cutstrat) , cutstratperiod, chillDurPer3),
                                                                                                        chillDurPer2 = if_else((chillDurPer2 > cutstratperiod & stratDuration > cutstrat), 57.5*1, chillDurPer2),
                                                                                                        chillDurPer2 = if_else(is.na(chillDurPer2), 20, chillDurPer2),
                                                                                                        chillDurPer1 = if_else((chillDurPer1 > cutstratperiod & stratDuration > cutstrat)| is.na(chillDurPer1), 57.5*1, chillDurPer1)),
      aes(y = uniqueID, yend = uniqueID, x = 30 + chillDurPer1 + chillDurPer2, xend = 30 + chillDurPer1 + chillDurPer2 + chillDurPer3, color = chillTempPer3), 
      linewidth = 0.55) +
    ### duration is known (> cutstratperiod), more than one period
    geom_segment(
      data = dataplot %>% dplyr::filter(!is.na(chillDurPer1) & !is.na(chillTempPer3) & chillDurPer3 > cutstratperiod & stratDuration > cutstrat) %>% dplyr::mutate(chillDurPer1 = if_else((chillDurPer1 > cutstratperiod & stratDuration > cutstrat) | is.na(chillDurPer1), 57.5*1, chillDurPer1),
                                                                                                                                        chillDurPer2 = if_else((chillDurPer2 > cutstratperiod & stratDuration > cutstrat), 57.5*1, chillDurPer2),
                                                                                                                                        chillDurPer2 = if_else(is.na(chillDurPer2), 20, chillDurPer2)),
      aes(y = uniqueID, yend = uniqueID, x = 30 + chillDurPer1 + chillDurPer2 + cutstratperiod, xend = 30 + chillDurPer1 + chillDurPer2 + cutstratperiod + (57.5-cutstratperiod), color = chillTempPer3),
      linewidth = 0.2, linetype = "dotted") +
    
    ## fourth period
    ### duration is unknown
    geom_point(
      data = dataplot %>% dplyr::filter(is.na(chillDurPer4) & !is.na(chillTempPer4)),
      aes(y = uniqueID, x = 40+57.5+57.5+57.5, color = chillTempPer4),
      size = 0.1) +
    geom_text(
      data = dataplot %>% dplyr::filter(is.na(chillDurPer3) & !is.na(chillTempPer3)),
      aes(y = uniqueID, x = 40+57.5+57.5+57.5, label = "?"),
      size = 0.6) + 
    ### duration is known, more than one period
    geom_segment(
      data = dataplot %>% dplyr::filter(!is.na(chillDurPer4)) %>% dplyr::mutate(chillDurPer4 = if_else((chillDurPer4 > cutstratperiod & stratDuration > cutstrat), cutstratperiod, chillDurPer4),
                                                                                chillDurPer3 = if_else((chillDurPer3 > cutstratperiod & stratDuration > cutstrat) | is.na(chillDurPer3), 57.5*1, chillDurPer3),
                                                                                chillDurPer2 = if_else((chillDurPer2 > cutstratperiod & stratDuration > cutstrat), 57.5*1, chillDurPer2),
                                                                                chillDurPer2 = if_else(is.na(chillDurPer2), 20, chillDurPer2),
                                                                                chillDurPer1 = if_else((chillDurPer1> cutstratperiod & stratDuration > cutstrat) | is.na(chillDurPer1), 57.5*1, chillDurPer1)),
      aes(y = uniqueID, yend = uniqueID, x = 30 + chillDurPer1 + chillDurPer2 +  chillDurPer3, xend = 30 + chillDurPer1 + chillDurPer2 + chillDurPer3 + + chillDurPer4,  color = chillTempPer4), 
      linewidth = 0.55) +
    ### duration is known (> cutstratperiod), more than one period
    geom_segment(
      data = dataplot %>% dplyr::filter(!is.na(chillTempPer4) & chillDurPer4 > cutstratperiod & stratDuration > cutstrat) %>% dplyr::mutate(chillDurPer1 = if_else((chillDurPer1 > cutstratperiod & stratDuration > cutstrat) | is.na(chillDurPer1), 57.5*1, chillDurPer1),
                                                                                                                                            chillDurPer2 = if_else((chillDurPer2 > cutstratperiod & stratDuration > cutstrat), 57.5*1, chillDurPer2),
                                                                                                                                            chillDurPer2 = if_else(is.na(chillDurPer2), 20, chillDurPer2),
                                                                                                                                        chillDurPer3 = if_else((chillDurPer1 > cutstratperiod & stratDuration > cutstrat) | is.na(chillDurPer3), 57.5*1, chillDurPer3)),
      aes(y = uniqueID, yend = uniqueID, x = 30 + chillDurPer1 + chillDurPer2 + chillDurPer3 + cutstratperiod, xend = 30 + chillDurPer1 + chillDurPer2 + chillDurPer3 + cutstratperiod + (57.5-cutstratperiod), color = chillTempPer4),
      linewidth = 0.2, linetype = "dotted") +
    
    scale_color_gradient2(low = "#174f8b", mid = "#ADBE7CFF", high = "#8B174DFF", midpoint = 0, na.value = "grey80",
                          limits = c(-20, 20), oob = scales::squish, name = "stratification (°C)") +
    
    
    coord_cartesian(xlim = c(-cutstorage-30,cutstrat+30+30)) +
    scale_x_continuous(breaks = c(seq(0, -270, -90), seq(0,270,90)+30), expand = c(0,2.5),
                       labels = c("0", "3", "6", "9", "0", "3", "6", "9"), name = "Duration (months)") +
    scale_y_discrete(expand = c(0.001,-00), position = "right") +
    theme_minimal() + 
    theme(axis.text.y = element_blank(), 
          axis.title.y = element_blank(),
          panel.grid = element_blank(), axis.ticks.y = element_blank(),
          axis.ticks.x = element_line(linewidth= 0.1), axis.line.x = element_line(linewidth= 0.2),
          legend.position = 'top', legend.title = element_text(size = 3, hjust = 0.5, margin = margin(t=2,b=-5), face="bold"), legend.title.position = "top",
          legend.text = element_text(size = 3, margin = margin()), 
          legend.spacing.x = unit(1, 'cm'), legend.key.spacing.x = unit(0.1, 'cm'),
          axis.title.x = element_text(size = 3), axis.text.x = element_text(size = 3),
          legend.key.width =  unit(0.2, 'cm'), legend.margin = margin(b = -15, t = 0),
          plot.margin = margin(t=0, r = 2, b = 1, l = 1)) +
    guides(
      color = guide_colorbar(frame.colour = "grey30", ticks.colour = NA,
                             frame.linewidth = 0.2,
                             theme = theme(legend.key.height  = unit(2, "pt"),
                                           legend.key.width  = unit(40, "pt"),
                                           legend.title = element_text(size = 3, hjust = 0.5, margin = margin(t=2,b=2), face="bold"),
                                           legend.text = element_text(size = 3, 
                                                                      margin = margin(t = 1), color = "grey20"))))
  
  if(add.names){
    
    timeline <- timeline + geom_text(
      data = dataplot,
      aes(y = uniqueID, x = 60, label = uniqueID),
      size = 0.2)
    
  }
  
  return(timeline)
  
}




  