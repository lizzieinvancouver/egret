Data Scraping of the USDA Woody Plant Seed Manual, 2008 (scraping tables from PDF of manual)
UBC Temporal Ecology Lab ...
... with much work by Sandy Zhang and Selena Shew (who worked together on getting Amazon Textract to work), and Justin Ngo and Britany Wu (who helped with cleaning the resulting mess)
README started 24 May 2024

Update by Lizzie on 2 August 2024 (with much text added by Selena also in August 2024): This folder contains:
* cleaning/ which is cleaning after the scraping and includes all major cleaning of USDA data
* input/ the first 5 folders, which are all numbered, contain the scraped data tables directly from the Amazon Textract tool. There are also 2 other folders that contain copies of either all of the seed data or all of the phenology data; these folders contain the inputs for the data preparation script found in cleaning ("phenology_data_preparation_script.R"). Two other folders contain a random assortment of copies of data tables to be used as tests while building the scripts. A copy of the original PDF of the USDA Woody Plant Seed Manual (2008), which is the original source of all of the data, also lives in this folder.
* output/ a folder of cleaned data after running through cleaning scripts
* scraping/ one txt file explaining the basic plan, by Sandy Zhang (back when we were evaluating options in Feb 2024)

As of August 2024, all of the germination data has been properly cleaned and the various germination scripts used have been merged together (see egret issue #15). Work continues on cleaning the phenology & seed data.

General Overview:
Starting in January of 2024, we aimed to scrape the USDA Woody Plant Seed Manual (published 2008): https://www.fs.usda.gov/rm/pubs_series/wo/wo_ah727.pdf. 
Our goal was to to be able to scrape three main types of data tables from the book:

1) Phenology of flowering & fruiting
2) Germination test conditions & results
3) Height, seed-bearing age, and seedcrop frequency

This proved challenging due to the varying data table formats used by the different scientific authors throughout the book. 
We were eventually able to scrape the entire book using the Amazon Textract tool: https://aws.amazon.com/textract/.

We plan on continuing the cleaning of the data in R.

All of the files associated with this project (input file, output data, cleaning scripts, etc.) can be found under analyses\scrapeUSDAseedmanual in this repository.


Detailed Description:
All of the project files can be found within egret/analyses/scrapeUSDAseedmanual. 

There are four main folders:
1) input: where the original seed manual pdf and scraped data tables are located.

Within the input folder exists five other folders: 100-600, 700-800, 800-900, etc. 
The folder names represent the original page numbers in the plant manual that the data tables were scraped from. 
Inside each of these folders exist more folders that further dive in into the exact page range the tables were scraped from.
For example, 800-900/wo_ah727-19-1203-801-900-71-80.zip specifies that the data tables within this folder are from pages 871-880.

2) output: where the cleaned data is located.
UPDATE on 14 Feb 2025: the main USDA file we probably want is now in: analyses/output/usdaGerminationCleaned.csv

Within this folder is a master spreadsheet containing the relevant data. Within this spreadsheet exists a data dictionary explaining the acronyms,
a page for phenology data, a page for germination data, and a page for seedcrop data. This spreadsheet currently needs heavy revisions.

3) cleaning: the scripts used to clean the data.
4) scraping: the scripts/documentation of how the data was scraped.


UPDATES 13 June 2024
_______________________

- 2 scripts have been completed: 'renameRelevantDataTablesScript.R' goes through every single CSV file in all of the input folders and checks if they are a phenology, germination, or seed data table (and renames them if they are). The second script, 'countDataTableTypesScript.R' was run after to count the number of data tables per each type (phenology, germination, or seed).

- The counts are as follows:
1) Phenology tables: 50
2) Germination tables: 60
3) Seed tables: 17

A new spreadsheet will be created to contain all of the finalized germination data, as this dataset is needed sooner. This will take a lot of manual work as I will need to cross-reference the missing data with the original data from the PDF, ex. the missing genus names that were not parsed.

UPDATES 19 May 2025
_______________________

After checking the scraped data against the tables in the original book, we found that:
 1) some tables were scraped but not renamed, and
 2) some tables containing data we needed were not scraped at all.
Thus, we manually reviewed the entire book and re-scraped all relevant tables to capture any missing data. This process added 34 more tables.