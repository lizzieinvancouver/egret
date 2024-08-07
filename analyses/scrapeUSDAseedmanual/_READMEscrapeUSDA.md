Data Scraping of the USDA Woody Plant Seed Manual, 2008
UBC Temporal Ecology Lab
README started 24 May 2024

Update by Lizzie on 2 August 2024: This folder contains:
* cleaning/ which is cleaning after the scraping and includes all major cleaning of USDA data
* input/ a folder of ??? (I am guessing these are the OUTPUT of scraping that are INPUT for cleaning? May be worth a new name here like inputForCleaning)
* output/ a folder of cleaned data after running through cleaning scripts
* scraping/ 

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

Within this folder is a master spreadsheet containing the relevant data. Within this spreadsheet exists a data dictionary explaining the acronyms,
a page for phenology data, a page for germination data, and a page for seedcrop data. This spreadsheet currently needs heavy revisions.

3) cleaning: the scripts used to clean the data.
4) scraping: the scripts/documentation of how the data was scraped.


UPDATES June 13, 2024
_______________________

- 2 scripts have been completed: 'rename_relevant_data_tables_script.R' goes through every single CSV file in all of the input folders and checks if they are a phenology, germination, or seed data table (and renames them if they are). The second script, 'count_data_table_types_script.R' was run after to count the number of data tables per each type (phenology, germination, or seed).

- The counts are as follows:
1) Phenology tables: 50
2) Germination tables: 60
3) Seed tables: 17

A new spreadsheet will be created to contain all of the finalized germination data, as this dataset is needed sooner. This will take a lot of manual work as I will need to cross-reference the missing data with the original data from the PDF, ex. the missing genus names that were not parsed.


