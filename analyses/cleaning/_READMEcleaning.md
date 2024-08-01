## Updated August 1, 2024

## Folders and key files

1. Checks:
      - alternatingTempPaperList_withNotes.xlsx: list of papers with alternating germination temperatures, including notes
      - alternatingTempPaperList.csv: csv list of papers with alternating germination temperatures
      - dataNotScraped.R: code to check that all papers were scraped
      - pdfMissing.R: code to check which pdf's are still missing from the Google drive folder
        
2. Source
      - cleaningChemical.R: cleans the columns pertaining to chemical treatments
      - cleanChillTempDuration.R: cleans columns pertaining to the chilling treatments, including calculating chill units
      - cleandatasetID.R: cleans datasetID names, including duplicates
      - cleanGerminationTempDuration.R: clean columns pertaining to germination temperatures, including identifying studies in which germination temps alternate between day time and night
      - cleanPhotoperiod.R: cleans photoperiod treatment columns and extracts light data form other columns like "other.treatment"
      - cleanResponseVar.R: cleans the response variable names and values, along with error columns
      - cleanScarification.R: code cleaning the columns pertaining to scarification treatments
      - cleanspecies.R: code cleaning species names
      - cleanStorage.R: cleaning columns pertaining to the storage treatments
      - cleanYearGermination.R: code cleaning germination year column
      - mergedata.R: code that merges all the scraped data .csv files
      - 
3. cleanall.R: code that runs all the sourced cleaning column and outputs an egret data .csv

