#Purpose: Loans and cleans DESA migrant stock data
library(here)
library(ggplot2)
library(tidyverse)

here()


#load DESA dataset stripped of UN header material#
origin_dest_clean <- as.data.frame(read.csv(here('UN_MigrantStockByOriginAndDestination_2019_clean.csv'), sep = ',',stringsAsFactors = FALSE))

#convert to numeric and remove NAs
i <- c(3:234)
origin_dest_clean[,i] <- apply(origin_dest_clean[ , i], 2,          
                               function(x) as.numeric(as.character(x)))

#clean basic country names and isolate by year, then convert to long format
desa_cleaning <- function(dataset, yr){
  dataset$Dest <- gsub('\\.', ' ', dataset$Dest)
  dataset$Dest <- gsub("Korea (Republic of)", "Republic of Korea", dataset$Dest)
  dataset$Dest <- gsub("Dem. People's Republic of Korea", "Korea (Democratic People's Rep. of)", dataset$Dest)
  dataset$Dest <- gsub("Hong Kong, China (SAR)", "Hong Kong, China SAR", dataset$Dest)
  
  origin_dest_yr_output <- dataset %>% subset(Year == yr)
  origin_dest_yr_output <- origin_dest_yr_output[-c(1)]
  origin_dest_yr_output_long <- gather(origin_dest_yr_output, "orig", "migrants", 2: 233)
  return(origin_dest_yr_output_long)
}

#run the function
origin_dest_1995_long <- desa_cleaning(origin_dest_clean, 1995)
origin_dest_2000_long <- desa_cleaning(origin_dest_clean, 2000)
origin_dest_2005_long <- desa_cleaning(origin_dest_clean, 2005)
origin_dest_2010_long <- desa_cleaning(origin_dest_clean, 2010)
origin_dest_2015_long <- desa_cleaning(origin_dest_clean, 2015)
origin_dest_2019_long <- desa_cleaning(origin_dest_clean, 2019)
