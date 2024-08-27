# Started 15 August 2024 by Deridre

This folder contains code and output files generated when we were checking data and tracking pdfs.

## Files:
R files:
- dataNotScraped.R: code created that merges everyone's updated source tab and checks whether all data that should have been scraped was. This accounts for papers that were later decided to be outside our criteria or not in english.
- pdfMissing.R: This was code used to generate a list of pdf's that we would need to download after it was discovered the original Google drive was missing.
- checkNewData.R: code created to check that the data and treatments scraped by SS are correct and now give us the curves of the percent germination we need 

data files:
- alternatingTempPaperList_withNotes.xlsx: this file was created to review all papers that had alternating germination temperature regimes and check whether they were "day/night" or "night/day" regimes.
- alternatingTempPaperList.csv: this file was created to review all papers that had alternating germination temperature and is the csv version
  * The code that writes these two files can be found in analyses/cleaning/source/cleanGerminationTempDuration.R


