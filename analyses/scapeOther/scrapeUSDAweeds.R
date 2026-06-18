## Started 3 June 2026 ##
## By Lizzie, coded with aid of chatGPT (hence the tidyverse) ##
## I spot checked the output ##

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

if(length(grep("deirdreloughnan", getwd()) > 0)) {
  setwd("~/Documents/github/egret/analyses")
} else if(length(grep("lizzie", getwd()) > 0)) {
  setwd("/Users/lizzie/Documents/git/projects/egret/analyses")
} else if(length(grep("Xiaomao", getwd()) > 0)) {
  setwd("C:/PhD/Project/egret/analyses")
} else{
  setwd("boomdittyboom") # for midge
}


library(rvest)
library(dplyr)
library(purrr)
library(stringr)

get_weeds_page <- function(page_num) {

  url <- if (page_num == 0) {
    "https://agrosus.eu/weeds-database/"
  } else {
    paste0(
      "https://agrosus.eu/weeds-database/?frm-page-2870=&frm-page-2288=",
      page_num
    )
  }

  message("Scraping page ", page_num)

  page <- read_html(url)

  page %>%
    html_element("table") %>%
    html_table(convert = FALSE) %>%
    select(
      scientific_name = 2,
      type = 4,
      life_cycle = 5
    ) %>%
    mutate(
      across(everything(), ~str_squish(.x))
    )
}

all_weeds <- map_dfr(1:12, get_weeds_page) # testing for now, should be 1:12

nrow(all_weeds)
head(all_weeds)

all_weeds <- all_weeds %>%
  distinct()


## Now pulling from Arkansas Weeds ID Database 

url <- "https://www.uaex.uada.edu/yard-garden/resource-library/weed-id/"

weedsuada <- read_html(url) %>%
  html_element("table") %>%
  html_table() %>%
  select(
    species = `Scientific Name`,
    life_cycle = `Life Cycle`
  ) %>%
  mutate(
    across(everything(), str_squish)
  )

weedsuada

## Now, merging (written just by Lizzie)

weedseuro <- as.data.frame(all_weeds)
weedsark <- as.data.frame(weedsuada)

# Fix up to rbind ...
weedsark$type <- sapply(strsplit(weedsark$life_cycle, "\\s+"), `[`, 2)
weedsark$lifecycle <- sapply(strsplit(weedsark$life_cycle, "\\s+"), `[`, 1)
weedsark$life_cycle <- NULL
names(weedsark)[names(weedsark)=="species"] <- "scientific_name"
names(weedseuro)[names(weedseuro)=="life_cycle"] <- "lifecycle"

# Add a where column
weedseuro$source <- "Agrosus Weeds"
weedsark$source <- "Arkansas Weed ID"

# Bind and pull out genus, species
weedsall <- rbind(weedseuro, weedsark)
weedsall$genus <- sapply(strsplit(weedsall$scientific_name, "\\s+"), `[`, 1)
weedsall$species <- sapply(strsplit(weedsall$scientific_name, "\\s+"), `[`, 2)

write.csv(
  weedsall,
  "output/lifecycleWeedsScraped.csv",
  row.names = FALSE
)