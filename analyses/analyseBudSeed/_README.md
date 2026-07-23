## README for egret/analyseBudSeed
Started 22 July 2026

### Files
* betaDisMdl.R -- main model code, modelling both egret and usda data using the ordered beta likelihood model developed initially for egret
* budseedleadin.R -- code to run the ospree model and extract the posteriors for the species that overlap with egret
* egretOspreePlotting.R -- code comparing the chillind and forcing/germination point estimates from adult trees and seeds 
* prepEgretUsda.R -- combining and formatting egret and usda data for analysis. Outputs a simplified dataframe with just study, species, and the two parameters---chill duration and germination temperature
* usdaRawDataPlotting.R -- preliminary code to visualize the egret and usda data
* visualization.R -- visualizing model output and retrodictive checks