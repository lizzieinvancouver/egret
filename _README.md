# EGRET
## UBC Temporal Ecology Lab

### Started 5 July 2021
### Started by Lizzie

Aim of database: We want to understand the cues (factors/treatments etc.) that control germination of woody species, probably especially non-tropical species, but let's start by seeing how much info there is on woody species before we limit further. 

EGRET -- name changed May 10 2023 -- Environmental Germination Responses to Experimental Treatments -- could also be Eudicot instead of environmental

OEGRES -- the original name Dan suggested, I think it's Observed E--- Germination Responses to Experimental S---- I am shortening to OGRES, and awaiting a lab-agreed-upon name.


### UPDATED 1 June 2022 (and 2 July 2025 for Teams folder)

After Dan (B) and Sophia Collins worked on this all spring, we agreed we should try new search terms and start anew. Dan worked through several searches and we together selected the best ones. See OEGRES_searchnotes.txt

EGRET on Temporal Ecology Lab Microsoft Teams folder -- all the papers as PDF and an egretOtherFiles folder (previous google link for papers deleted 2 July 2025, so if you want that link, look for a previous version of this file)

Some notes from meeting with Dan, Lizzie, Deirdre (late May 2022):

More Notes, we decided that we are ... 
- Not cross checking with Sophia and Alina's list.
- Doing all species, not just woody.

Baskin: 
- We discussed how hard it may be to cross-check our list with the Baskin ref list (Willis list does not include reference and the book is in text form only so we would have to scrape it).
- We decided: we can say we did not scrape it as long as we either on woody species (they divide stuff by habitat not growth form), or we focus on time to germination (they focus on dormancy classification which is often done by showing a marked increase in percentage after a treatment)

### UPDATED 23 May 2024

Starting in January of 2024, we aimed to scrape the USDA Woody Plant Seed Manual (published 2008): https://www.fs.usda.gov/rm/pubs_series/wo/wo_ah727.pdf. Our goal was to to be able to scrape three main types of data tables from the book:

1) Phenology of flowering & fruiting
2) Germination test conditions & results
3) Height, seed-bearing age, and seedcrop frequency

This proved challenging due to the varying data table formats used by the different scientific authors throughout the book. We were eventually able to scrape the entire book using the Amazon Textract tool: https://aws.amazon.com/textract/.

We plan on continuing the cleaning of the data in R. 

All of the files associated with this project (input file, output data, cleaning scripts, etc.) can be found under analyses\scrapeUSDAseedmanual in this repository.