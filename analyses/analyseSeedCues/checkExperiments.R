
apply_tol <- function(seeds, tolerance = 1) {
  for (i in 2:length(seeds)) {
    diff <- seeds[i] - seeds[i-1]
    if (diff < 0 & diff >= -tolerance) {
      seeds[i] <- seeds[i] - diff 
    }
  }
  return(seeds)
}

pdf('/home/victor/projects/egret/analyses/figures/problems.pdf', width = 12, height = 14)
par(mfrow = c(7,6))
toremove <- c()
for(i in 1:length(uniq_exp_ids)){
  
  e <- uniq_exp_ids[i]
  d_exp <- modeld[modeld$uniqueID == e,]
  d_exp <- unique(d_exp[,names(d_exp)[names(d_exp) != 'treatment']])
  if(nrow(d_exp) == 1){
    toremove <- c(toremove, e)
    next}
  
  d_exp$nseeds <- round(d_exp$nseeds,0)
  
  
  ord <- order(d_exp$germDuration)
  resp <- d_exp$nseeds[ord]
  
  resp <- apply_tol(resp, tolerance = ceiling(max(resp)*0.01)) # tolerance of +/- 1% of max.
  
  delta <- diff(resp)
  mindelta <- min(delta)
  
  res <- data.frame(t = round(d_exp$germDuration[ord],2), resp = resp)
  res <- unique(res)
  
  plot(x = NULL, y = NULL, 
       xlim = range(res$t),
       ylim = range(res$resp),
       xlab = 'Time', ylab = 'Germ. perc.',
       main = paste0('Experience ', i),
       bty="n")
  
  if(any(duplicated(res$t))){
    rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "#fad1d0", border = FALSE)
    toremove <- c(toremove, e)
  }
  
  if(is.unsorted(res$resp)){
    rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "#fad1d0", border = FALSE)
    text(x = par("usr")[2], y = par("usr")[3]+ (par("usr")[4]-par("usr")[3])*0.15, labels = paste0('delta=',mindelta), adj = 1)
    toremove <- c(toremove, e)
  }
  
  if(any(res$resp < 0)){
    rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "#fad1d0", border = FALSE)
    toremove <- c(toremove, e)
  }
  
  
  lines(res$resp ~ res$t)
  par(bg = "white") 
  
  
}
dev.off()

