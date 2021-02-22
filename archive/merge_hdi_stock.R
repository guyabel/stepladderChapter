#Merge and DESA/UNHCR data and HDI data
library(dplyr)

#load hdi data from "hdi_desc.r"
hdi_country <- as.data.frame(hdi_five_prepped)

#subset by year ##NOTE: HDI is from 2018
hdi_1995 <-  hdi_country %>% subset(Year == 1995)
hdi_2000 <-  hdi_country %>% subset(Year == 2000)
hdi_2005 <-  hdi_country %>% subset(Year == 2005)
hdi_2010 <-  hdi_country %>% subset(Year == 2010)
hdi_2015 <-  hdi_country %>% subset(Year == 2015)
hdi_2018 <-  hdi_country %>% subset(Year == 2018)

#Merge DESA/UNHCR data with HDI data, rename columns and drop redundant columns
desa_unhcr_hdi_merge <- function(desa_unhcr, hdi) {
  all_merged <- merge(desa_unhcr, hdi, by.x = 'Dest', by.y = 'Country', all.x = TRUE)
  names(all_merged)[11] <- 'Dest_HDI'
  all_merged <- merge(all_merged, hdi, by.x = 'orig', by.y = 'Country', all.x = TRUE)
  names(all_merged)[13] <- 'Orig_HDI'
  all_merged <- all_merged[-c(4,10)]
  return(all_merged)
} 

hdi_stocks_1995 <- desa_unhcr_hdi_merge(stocks_1995, hdi_1995)
hdi_stocks_2000 <- desa_unhcr_hdi_merge(stocks_2000, hdi_2000)
hdi_stocks_2005 <- desa_unhcr_hdi_merge(stocks_2005, hdi_2005)
hdi_stocks_2010 <- desa_unhcr_hdi_merge(stocks_2010, hdi_2010)
hdi_stocks_2015 <- desa_unhcr_hdi_merge(stocks_2015, hdi_2015)
hdi_stocks_2019 <- desa_unhcr_hdi_merge(stocks_2019, hdi_2018)

#Isolate 1995 HDI classifications
hdi_stocks_1995$Dest_HDI_1995 <- hdi_stocks_1995$Dest_HDI
hdi_stocks_1995$Orig_HDI_1995 <- hdi_stocks_1995$Orig_HDI
just_1995 <- hdi_stocks_1995[-c(3:11)]

#Drop 1995 back to 11 columns to be consistent with other year datasets
hdi_stocks_1995_short <- hdi_stocks_1995[-c(12:13)]


add_1995_control <- function(merged_data){
  output_95 <- merge(merged_data, just_1995, by.x = c('orig', 'Dest'), by.y = c('orig', 'Dest'))
  return(output_95)
}

#create datasets with 1995 HDI classifications
hdi_stocks_2000_95 <- add_1995_control(hdi_stocks_2000)
hdi_stocks_2005_95 <- add_1995_control(hdi_stocks_2005)
hdi_stocks_2010_95 <- add_1995_control(hdi_stocks_2010)
hdi_stocks_2015_95 <- add_1995_control(hdi_stocks_2015)
hdi_stocks_2019_95 <- add_1995_control(hdi_stocks_2019)

#Due to discrepancy between year between HDI and DESA/UNHCR, convert HDI 2018 to 2019 for purposes of merge
hdi_stocks_2019$Year <- 2019
hdi_stocks_2019_95$Year <- 2019


#merge all year datasets into single dataframe
hdi_stocks_all <- rbind(hdi_stocks_1995_short, hdi_stocks_2000, hdi_stocks_2005, hdi_stocks_2010,
                       hdi_stocks_2015, hdi_stocks_2019)

