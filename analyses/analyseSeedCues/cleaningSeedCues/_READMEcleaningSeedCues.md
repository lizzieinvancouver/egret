# EGRET
## UBC Temporal Ecology Lab

### Started 22 August 2024
### Started by Dan B.

This is a file for secondary cleaning of the EGRET dataset in order to calculate some of the additional predictors we need for analyses.

1. The cleanAllSeedCues.R is the main file that sources all the individual cleaning files form the source folder.

2. addChill.R calculates chilling from chilling (e.g., chill portions, utah units) treatment temperatures and duration in the original dataset.

3. dropExtraTreats.R is currently not implemented (as of Aug 22, 2024) but its purpose is to deal with treatments that vary but won't be included in our model.