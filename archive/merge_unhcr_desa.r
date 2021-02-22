#Merges and cleans DESA and UNHCR data
#Calculates non-forced migrant stock
library(tidyverse)

####
#merge desa data with unhcr. Then clean and calculate: migrant stock - forced migrants
###1995
#merge
stocks_1995 <- merge(origin_dest_1995_long, unhcr_1995, by.x = c("Dest", 'orig'), 
                    by.y = c("Country.of.asylum", "Country.of.origin"), all = TRUE)
#replace NAs with 0
stocks_1995$total_concern <- stocks_1995$total_concern %>% replace_na(0)
#Calculate the difference between overall migrant stock and the total forced migrants
stocks_1995$stock <- stocks_1995$migrants - stocks_1995$total_concern

#clean country names
stocks_1995$orig <- gsub('\\.', ' ', stocks_1995$orig)
stocks_1995$orig <- gsub('Bolivia  Plurinational State of', 'Bolivia (Plurinational State of)', stocks_1995$orig)
stocks_1995$orig <- gsub("Bolivia..Plurinational.State.of.", "Bolivia (Plurinational State of)", stocks_1995$orig)
stocks_1995$orig <- gsub('Bonaire  Sint Eustatius and Saba', 'Bonaire, Sint Eustatius and Saba', stocks_1995$orig)
stocks_1995$orig <- gsub('Dem  People s Republic of Korea', "Korea (Democratic People's Rep of)", stocks_1995$orig)
stocks_1995$orig <- gsub('People s', "People's", stocks_1995$orig)
stocks_1995$orig <- gsub('Iran  Islamic Republic of', "Iran (Islamic Republic of)", stocks_1995$orig)
stocks_1995$Dest <- gsub('Iran (Islamic Rep. of)', "Iran (Islamic Republic of)", stocks_1995$Dest)
stocks_1995$orig <- gsub('Micronesia  Fed  States of', "Micronesia (Fed. States of)", stocks_1995$orig)
stocks_1995$Dest <- gsub('Micronesia', "Micronesia (Fed. States of)", stocks_1995$Dest)
stocks_1995$orig <- gsub('Timor Leste', "Timor-Leste", stocks_1995$orig)
stocks_1995$orig <- gsub('Venezuela Bolivarian Republic of', "Venezuela (Bolivarian Republic of)", stocks_1995$orig)
stocks_1995$orig <- gsub('China  Hong Kong SAR', 'China, Hong Kong SAR', stocks_1995$orig)
stocks_1995$orig <- gsub('China  Macao', 'China, Macao', stocks_1995$orig)

#replace NAs with 0
stocks_1995$total_concern <- stocks_1995$total_concern %>% replace_na
stocks_1995$migrants <- stocks_1995$migrants %>% replace_na(0)

#Calculate the difference between overall migrant stock and the total forced migrants
stocks_1995$stock <- stocks_1995$migrants - stocks_1995$total_concern
#If migrant stock after the subtraction is negative, change it to zero
stocks_1995$migrants_less_forced <- sapply(stocks_1995[,c('stock')], function(x) {ifelse(x < 0, 0, x)})


####REPEAT FOR ALL OTHER YEARS###
###2000###

stocks_2000 <- merge(origin_dest_2000_long, unhcr_2000, by.x = c("Dest", 'orig'), 
                    by.y = c("Country.of.asylum", "Country.of.origin"), all = TRUE)

stocks_2000$total_concern <- stocks_2000$total_concern %>% replace_na(0)
stocks_2000$stock <- stocks_2000$migrants - stocks_2000$total_concern
stocks_2000$orig <- gsub('\\.', ' ', stocks_2000$orig)
stocks_2000$orig <- gsub('Bolivia  Plurinational State of', 'Bolivia (Plurinational State of)', stocks_2000$orig)
stocks_2000$orig <- gsub("Bolivia..Plurinational.State.of.", "Bolivia (Plurinational State of)", stocks_2000$orig)
stocks_2000$orig <- gsub('Bonaire  Sint Eustatius and Saba', 'Bonaire, Sint Eustatius and Saba', stocks_2000$orig)
stocks_2000$orig <- gsub('Dem  People s Republic of Korea', "Korea (Democratic People's Rep of)", stocks_2000$orig)
stocks_2000$orig <- gsub('People s', "People's", stocks_2000$orig)
stocks_2000$orig <- gsub('Iran  Islamic Republic of', "Iran (Islamic Republic of)", stocks_2000$orig)
stocks_2000$Dest <- gsub('Iran (Islamic Rep. of)', "Iran (Islamic Republic of)", stocks_2000$Dest)
stocks_2000$orig <- gsub('Micronesia  Fed  States of', "Micronesia (Fed. States of)", stocks_2000$orig)
stocks_2000$Dest <- gsub('Micronesia', "Micronesia (Fed. States of)", stocks_2000$Dest)
stocks_2000$orig <- gsub('Timor Leste', "Timor-Leste", stocks_2000$orig)
stocks_2000$orig <- gsub('Venezuela Bolivarian Republic of', "Venezuela (Bolivarian Republic of)", stocks_2000$orig)
stocks_2000$orig <- gsub('China  Hong Kong SAR', 'China, Hong Kong SAR', stocks_2000$orig)
stocks_2000$orig <- gsub('China  Macao', 'China, Macao', stocks_2000$orig)

stocks_2000$total_concern <- stocks_2000$total_concern %>% replace_na(0)
stocks_2000$migrants <- stocks_2000$migrants %>% replace_na(0)
stocks_2000$stock <- stocks_2000$migrants - stocks_2000$total_concern
stocks_2000$migrants_less_forced <- sapply(stocks_2000[,c('stock')], function(x) {ifelse(x < 0, 0, x)})

####
###2005###

stocks_2005 <- merge(origin_dest_2005_long, unhcr_2005, by.x = c("Dest", 'orig'), 
                    by.y = c("Country.of.asylum", "Country.of.origin"), all = TRUE)

stocks_2005$total_concern <- stocks_2005$total_concern %>% replace_na(0)
stocks_2005$stock <- stocks_2005$migrants - stocks_2005$total_concern
stocks_2005$orig <- gsub('\\.', ' ', stocks_2005$orig)
stocks_2005$orig <- gsub('Bolivia  Plurinational State of', 'Bolivia (Plurinational State of)', stocks_2005$orig)
stocks_2005$orig <- gsub("Bolivia..Plurinational.State.of.", "Bolivia (Plurinational State of)", stocks_2005$orig)
stocks_2005$orig <- gsub('Bonaire  Sint Eustatius and Saba', 'Bonaire, Sint Eustatius and Saba', stocks_2005$orig)
stocks_2005$orig <- gsub('Dem  People s Republic of Korea', "Korea (Democratic People's Rep of)", stocks_2005$orig)
stocks_2005$orig <- gsub('People s', "People's", stocks_2005$orig)
stocks_2005$orig <- gsub('Iran  Islamic Republic of', "Iran (Islamic Republic of)", stocks_2005$orig)
stocks_2005$Dest <- gsub('Iran (Islamic Rep. of)', "Iran (Islamic Republic of)", stocks_2005$Dest)
stocks_2005$orig <- gsub('Micronesia  Fed  States of', "Micronesia (Fed. States of)", stocks_2005$orig)
stocks_2005$Dest <- gsub('Micronesia', "Micronesia (Fed. States of)", stocks_2005$Dest)
stocks_2005$orig <- gsub('Timor Leste', "Timor-Leste", stocks_2005$orig)
stocks_2005$orig <- gsub('Venezuela Bolivarian Republic of', "Venezuela (Bolivarian Republic of)", stocks_2005$orig)
stocks_2005$orig <- gsub('China  Hong Kong SAR', 'China, Hong Kong SAR', stocks_2005$orig)
stocks_2005$orig <- gsub('China  Macao', 'China, Macao', stocks_2005$orig)

stocks_2005$total_concern <- stocks_2005$total_concern %>% replace_na(0)
stocks_2005$migrants <- stocks_2005$migrants %>% replace_na(0)
stocks_2005$stock <- stocks_2005$migrants - stocks_2005$total_concern
stocks_2005$migrants_less_forced <- sapply(stocks_2005[,c('stock')], function(x) {ifelse(x < 0, 0, x)})

####
###2010###

stocks_2010 <- merge(origin_dest_2010_long, unhcr_2010, by.x = c("Dest", 'orig'), 
                    by.y = c("Country.of.asylum", "Country.of.origin"), all = TRUE)

stocks_2010$total_concern <- stocks_2010$total_concern %>% replace_na(0)
stocks_2010$stock <- stocks_2010$migrants - stocks_2010$total_concern
stocks_2010$orig <- gsub('\\.', ' ', stocks_2010$orig)
stocks_2010$orig <- gsub('Bolivia  Plurinational State of', 'Bolivia (Plurinational State of)', stocks_2010$orig)
stocks_2010$orig <- gsub("Bolivia..Plurinational.State.of.", "Bolivia (Plurinational State of)", stocks_2010$orig)
stocks_2010$orig <- gsub('Bonaire  Sint Eustatius and Saba', 'Bonaire, Sint Eustatius and Saba', stocks_2010$orig)
stocks_2010$orig <- gsub('Dem  People s Republic of Korea', "Korea (Democratic People's Rep of)", stocks_2010$orig)
stocks_2010$orig <- gsub('People s', "People's", stocks_2010$orig)
stocks_2010$orig <- gsub('Iran  Islamic Republic of', "Iran (Islamic Republic of)", stocks_2010$orig)
stocks_2010$Dest <- gsub('Iran (Islamic Rep. of)', "Iran (Islamic Republic of)", stocks_2010$Dest)
stocks_2010$orig <- gsub('Micronesia  Fed  States of', "Micronesia (Fed. States of)", stocks_2010$orig)
stocks_2010$Dest <- gsub('Micronesia', "Micronesia (Fed. States of)", stocks_2010$Dest)
stocks_2010$orig <- gsub('Timor Leste', "Timor-Leste", stocks_2010$orig)
stocks_2010$orig <- gsub('Venezuela Bolivarian Republic of', "Venezuela (Bolivarian Republic of)", stocks_2010$orig)
stocks_2010$orig <- gsub('China  Hong Kong SAR', 'China, Hong Kong SAR', stocks_2010$orig)
stocks_2010$orig <- gsub('China  Macao', 'China, Macao', stocks_2010$orig)


stocks_2010$total_concern <- stocks_2010$total_concern %>% replace_na(0)
stocks_2010$migrants <- stocks_2010$migrants %>% replace_na(0)
stocks_2010$stock <- stocks_2010$migrants - stocks_2010$total_concern
stocks_2010$migrants_less_forced <- sapply(stocks_2010[,c('stock')], function(x) {ifelse(x < 0, 0, x)})

####
###2015###

stocks_2015 <- merge(origin_dest_2015_long, unhcr_2015, by.x = c("Dest", 'orig'), 
                    by.y = c("Country.of.asylum", "Country.of.origin"), all = TRUE)

stocks_2015$total_concern <- stocks_2015$total_concern %>% replace_na(0)
stocks_2015$stock <- stocks_2015$migrants - stocks_2015$total_concern
stocks_2015$orig <- gsub('\\.', ' ', stocks_2015$orig)
stocks_2015$orig <- gsub('Bolivia  Plurinational State of', 'Bolivia (Plurinational State of)', stocks_2015$orig)
stocks_2015$orig <- gsub("Bolivia..Plurinational.State.of.", "Bolivia (Plurinational State of)", stocks_2015$orig)
stocks_2015$orig <- gsub('Bonaire  Sint Eustatius and Saba', 'Bonaire, Sint Eustatius and Saba', stocks_2015$orig)
stocks_2015$orig <- gsub('Dem  People s Republic of Korea', "Korea (Democratic People's Rep of)", stocks_2015$orig)
stocks_2015$orig <- gsub('People s', "People's", stocks_2015$orig)
stocks_2015$orig <- gsub('Iran  Islamic Republic of', "Iran (Islamic Republic of)", stocks_2015$orig)
stocks_2015$Dest <- gsub('Iran (Islamic Rep. of)', "Iran (Islamic Republic of)", stocks_2015$Dest)
stocks_2015$orig <- gsub('Micronesia  Fed  States of', "Micronesia (Fed. States of)", stocks_2015$orig)
stocks_2015$Dest <- gsub('Micronesia', "Micronesia (Fed. States of)", stocks_2015$Dest)
stocks_2015$orig <- gsub('Timor Leste', "Timor-Leste", stocks_2015$orig)
stocks_2015$orig <- gsub('Venezuela Bolivarian Republic of', "Venezuela (Bolivarian Republic of)", stocks_2015$orig)
stocks_2015$orig <- gsub('China  Hong Kong SAR', 'China, Hong Kong SAR', stocks_2015$orig)
stocks_2015$orig <- gsub('China  Macao', 'China, Macao', stocks_2015$orig)


stocks_2015$total_concern <- stocks_2015$total_concern %>% replace_na(0)
stocks_2015$migrants <- stocks_2015$migrants %>% replace_na(0)
stocks_2015$stock <- stocks_2015$migrants - stocks_2015$total_concern
stocks_2015$migrants_less_forced <- sapply(stocks_2015[,c('stock')], function(x) {ifelse(x < 0, 0, x)})

####
###2019###

stocks_2019 <- merge(origin_dest_2019_long, unhcr_2019, by.x = c("Dest", 'orig'), 
                    by.y = c("Country.of.asylum", "Country.of.origin"), all = TRUE)

stocks_2019$total_concern <- stocks_2019$total_concern %>% replace_na(0)
stocks_2019$stock <- stocks_2019$migrants - stocks_2019$total_concern
stocks_2019$orig <- gsub('\\.', ' ', stocks_2019$orig)
stocks_2019$orig <- gsub('Bolivia  Plurinational State of', 'Bolivia (Plurinational State of)', stocks_2019$orig)
stocks_2019$orig <- gsub("Bolivia..Plurinational.State.of.", "Bolivia (Plurinational State of)", stocks_2019$orig)
stocks_2019$orig <- gsub('Bonaire  Sint Eustatius and Saba', 'Bonaire, Sint Eustatius and Saba', stocks_2019$orig)
stocks_2019$orig <- gsub('Dem  People s Republic of Korea', "Korea (Democratic People's Rep of)", stocks_2019$orig)
stocks_2019$orig <- gsub('People s', "People's", stocks_2019$orig)
stocks_2019$orig <- gsub('Iran  Islamic Republic of', "Iran (Islamic Republic of)", stocks_2019$orig)
stocks_2019$Dest <- gsub('Iran (Islamic Rep. of)', "Iran (Islamic Republic of)", stocks_2019$Dest)
stocks_2019$orig <- gsub('Micronesia  Fed  States of', "Micronesia (Fed. States of)", stocks_2019$orig)
stocks_2019$Dest <- gsub('Micronesia', "Micronesia (Fed. States of)", stocks_2019$Dest)
stocks_2019$orig <- gsub('Timor Leste', "Timor-Leste", stocks_2019$orig)
stocks_2019$orig <- gsub('Venezuela Bolivarian Republic of', "Venezuela (Bolivarian Republic of)", stocks_2019$orig)
stocks_2019$orig <- gsub('China  Hong Kong SAR', 'China, Hong Kong SAR', stocks_2019$orig)
stocks_2019$orig <- gsub('China  Macao', 'China, Macao', stocks_2019$orig)


stocks_2019$total_concern <- stocks_2019$total_concern %>% replace_na(0)
stocks_2019$migrants <- stocks_2019$migrants %>% replace_na(0)
stocks_2019$stock <- stocks_2019$migrants - stocks_2019$total_concern
stocks_2019$migrants_less_forced <- sapply(stocks_2019[,c('stock')], function(x) {ifelse(x < 0, 0, x)})
