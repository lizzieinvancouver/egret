Data Scraping of the USDA Woody Plant Seed Manual, 2008
UBC Temporal Ecology Lab
May 24, 2024


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

