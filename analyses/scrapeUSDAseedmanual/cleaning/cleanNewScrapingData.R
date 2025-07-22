## Merge scraped data ##
## Started by Mao on July 1 ##

library(xlsx)

d <- read.csv("scrapeUSDAseedmanual/cleaning/germPreCleanedMasterSheet.csv", header = T)
d[d == ""] <- NA


folder <- "scrapeUSDAseedmanual/scraping/"
fileList <- list.files(path = folder, pattern = "\\.xlsx$", full.names = TRUE)
df <- list()

for (file in fileList) {
  d1 <- read.xlsx(file, sheetIndex = 2)
  df[[file]] <- d1
}

d1 <- do.call(rbind, df)

rownames(d1) <- seq_len(nrow(d1))

head(d)
setdiff(colnames(d), colnames(d1))
setdiff(colnames(d1), colnames(d))


unwanted <- c("file_path", "scraped_table_number", "seed_type", "stratification_temp_C", "mean_light", "mean_dark", "light_range", "dark_range")
d <- d[ , !(names(d) %in% unwanted)]
new <- c("warm_stratification_temp_C", "cold_stratification_temp_C", "germ_rate_days", "X50._germ", "Notes")

for (col in new) {
  d[[col]] <- NA
}

d <- rbind(d, d1)
