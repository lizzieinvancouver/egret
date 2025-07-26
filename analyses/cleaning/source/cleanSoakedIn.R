### Started by Victor 26 July
## Only for few studies identified in the decision rules

## upper case, lower case 'water' to H2O
d[d$datasetID %in% 'harrington09' & d$study %in% 'exp1','soaked.in'] <- 'H2O'
d[d$datasetID %in% 'harrington09' & d$study %in% 'exp2','soaked.in'] <- 'H2O'
