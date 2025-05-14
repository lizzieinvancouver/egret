# Egret data
By Deirdre and Lizzie (as of August 2024)

This folder contains data scraped as part of the EGTRET meta-analysis. Files are named with the initials of the individual that scraped the data, with the exception of data that wad deemed missing.

## File types in the scrapedEgret folder
1. Excel files: All data was initially scraped into an excel file that included a tab for metadata, the source file information, data_detailed, and scratch
2. Csv files: For each data file, the data_detailed tabs were saved as .csv files

## Other files:
1. egret.xslx: the original egret file that no one was supposed to change
2. TaxonomicInfo files: a file each for the NCBI and ITIS databases
3. Saved abstracts initially reviewed: savedrecs_part1 and savedrecs_part2, as both .xls and .txt (FYI: these txt files also existed in the HU Google Drive -- just the general area -- as Lizzie was cleaning it in June 2024; the txt files were the EXACT same size as the ones on GitHub here so Lizzie did not keep them. They were created 1 Jun 2022 and last modified 27 May 2022 -- yes, you read that write, WTF Google Drive, WTF). 
4. Lists of papers flagged for further review: papersToReview1.csv and papersToReview2.csv
5. egretEudicot.csv: list of studies with eudicots made when deciding on whether to filter data further
6. oegres_fullsearch is a file Lizzie pulled off Google Drive when trying to close out the HU Google Drive in June 2024 (it seemed to be just in my general Drive area, no specific folder). Google drive info shows it was created 1 Jun 2022 and last modified 30 Jul 2022 by dbuonaiuto. Info in the README tab is about as much info as I have (yes, two years later -- I have almost no memory of this) and it has these columns: accept_reject; crops; notes and checkedby, suggesting this is where we subsetted down.
7. egret_fullsearch: appears to be a simplified version of oegres_fullsearch, but without the README, except without the crops column and additional columns: "reason_reject", "language", "available", and "paper_pulled." This file does have the same number of papers listed in oegres_fullsearch. Deirdre does not recall exactly what this file was for, but believes it was an early version of the egret.xlsx that included crops and papers that were later rejected.

## Folders

1. Baskin_Database: data and notes from the Baskin database:
   - Baskin dormancy database
   - Baskin database read me
2. Early: early files of the source papers and notes on search terms
3. scrapedEgred: raw excel files scraped by individuals, see scrapedExcel/README.md for more detail
4. Notes: notes on specific scraped papers or transfer of scraping tasks between people
5. ospree_searchers: Results of efforts to search for ospree species in the Baskin and Baskin seed book to see if it was worth doing more widely

## Where is the USDA seed manual data?
Good question! See egret/analyses/scrapeUSDAseedmanual/scraping