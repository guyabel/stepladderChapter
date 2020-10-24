library(tidyverse)

####

flows_1995 <- merge(origin_dest_1995_long, unhcr_1995, by.x = c("Dest", 'orig'), 
                    by.y = c("Country.of.asylum", "Country.of.origin"), all = TRUE)

flows_1995$total_concern <- flows_1995$total_concern %>% replace_na(0)
flows_1995$flow <- flows_1995$migrants - flows_1995$total_concern
flows_1995$orig <- gsub('\\.', ' ', flows_1995$orig)
flows_1995$orig <- gsub('Bolivia  Plurinational State of', 'Bolivia (Plurinational State of)', flows_1995$orig)
flows_1995$orig <- gsub("Bolivia..Plurinational.State.of.", "Bolivia (Plurinational State of)", flows_1995$orig)
flows_1995$orig <- gsub('Bonaire  Sint Eustatius and Saba', 'Bonaire, Sint Eustatius and Saba', flows_1995$orig)
flows_1995$orig <- gsub('Dem  People s Republic of Korea', "Korea (Democratic People's Rep of)", flows_1995$orig)
flows_1995$orig <- gsub('People s', "People's", flows_1995$orig)
flows_1995$orig <- gsub('Iran  Islamic Republic of', "Iran (Islamic Republic of)", flows_1995$orig)
flows_1995$Dest <- gsub('Iran (Islamic Rep. of)', "Iran (Islamic Republic of)", flows_1995$Dest)
flows_1995$orig <- gsub('Micronesia  Fed  States of', "Micronesia (Fed. States of)", flows_1995$orig)
flows_1995$Dest <- gsub('Micronesia', "Micronesia (Fed. States of)", flows_1995$Dest)
flows_1995$orig <- gsub('Timor Leste', "Timor-Leste", flows_1995$orig)
flows_1995$orig <- gsub('Venezuela Bolivarian Republic of', "Venezuela (Bolivarian Republic of)", flows_1995$orig)
flows_1995$orig <- gsub('China  Hong Kong SAR', 'China, Hong Kong SAR', flows_1995$orig)
flows_1995$orig <- gsub('China  Macao', 'China, Macao', flows_1995$orig)


flows_1995$total_concern <- flows_1995$total_concern %>% replace_na
flows_1995$migrants <- flows_1995$migrants %>% replace_na(0)
flows_1995$flow <- flows_1995$migrants - flows_1995$total_concern
flows_1995$migrants_less_forced <- sapply(flows_1995[,c('flow')], function(x) {ifelse(x < 0, 0, x)})

plot(flows_1995$migrants_less_forced)

####

flows_2000 <- merge(origin_dest_2000_long, unhcr_2000, by.x = c("Dest", 'orig'), 
                    by.y = c("Country.of.asylum", "Country.of.origin"), all = TRUE)

flows_2000$total_concern <- flows_2000$total_concern %>% replace_na(0)
flows_2000$flow <- flows_2000$migrants - flows_2000$total_concern
flows_2000$orig <- gsub('\\.', ' ', flows_2000$orig)
flows_2000$orig <- gsub('Bolivia  Plurinational State of', 'Bolivia (Plurinational State of)', flows_2000$orig)
flows_2000$orig <- gsub("Bolivia..Plurinational.State.of.", "Bolivia (Plurinational State of)", flows_2000$orig)
flows_2000$orig <- gsub('Bonaire  Sint Eustatius and Saba', 'Bonaire, Sint Eustatius and Saba', flows_2000$orig)
flows_2000$orig <- gsub('Dem  People s Republic of Korea', "Korea (Democratic People's Rep of)", flows_2000$orig)
flows_2000$orig <- gsub('People s', "People's", flows_2000$orig)
flows_2000$orig <- gsub('Iran  Islamic Republic of', "Iran (Islamic Republic of)", flows_2000$orig)
flows_2000$Dest <- gsub('Iran (Islamic Rep. of)', "Iran (Islamic Republic of)", flows_2000$Dest)
flows_2000$orig <- gsub('Micronesia  Fed  States of', "Micronesia (Fed. States of)", flows_2000$orig)
flows_2000$Dest <- gsub('Micronesia', "Micronesia (Fed. States of)", flows_2000$Dest)
flows_2000$orig <- gsub('Timor Leste', "Timor-Leste", flows_2000$orig)
flows_2000$orig <- gsub('Venezuela Bolivarian Republic of', "Venezuela (Bolivarian Republic of)", flows_2000$orig)
flows_2000$orig <- gsub('China  Hong Kong SAR', 'China, Hong Kong SAR', flows_2000$orig)
flows_2000$orig <- gsub('China  Macao', 'China, Macao', flows_2000$orig)

flows_2000$total_concern <- flows_2000$total_concern %>% replace_na(0)
flows_2000$migrants <- flows_2000$migrants %>% replace_na(0)
flows_2000$flow <- flows_2000$migrants - flows_2000$total_concern
flows_2000$migrants_less_forced <- sapply(flows_2000[,c('flow')], function(x) {ifelse(x < 0, 0, x)})


plot(flows_2000$migrants_less_forced)

####

flows_2005 <- merge(origin_dest_2005_long, unhcr_2005, by.x = c("Dest", 'orig'), 
                    by.y = c("Country.of.asylum", "Country.of.origin"), all = TRUE)

flows_2005$total_concern <- flows_2005$total_concern %>% replace_na(0)
flows_2005$flow <- flows_2005$migrants - flows_2005$total_concern
flows_2005$orig <- gsub('\\.', ' ', flows_2005$orig)
flows_2005$orig <- gsub('Bolivia  Plurinational State of', 'Bolivia (Plurinational State of)', flows_2005$orig)
flows_2005$orig <- gsub("Bolivia..Plurinational.State.of.", "Bolivia (Plurinational State of)", flows_2005$orig)
flows_2005$orig <- gsub('Bonaire  Sint Eustatius and Saba', 'Bonaire, Sint Eustatius and Saba', flows_2005$orig)
flows_2005$orig <- gsub('Dem  People s Republic of Korea', "Korea (Democratic People's Rep of)", flows_2005$orig)
flows_2005$orig <- gsub('People s', "People's", flows_2005$orig)
flows_2005$orig <- gsub('Iran  Islamic Republic of', "Iran (Islamic Republic of)", flows_2005$orig)
flows_2005$Dest <- gsub('Iran (Islamic Rep. of)', "Iran (Islamic Republic of)", flows_2005$Dest)
flows_2005$orig <- gsub('Micronesia  Fed  States of', "Micronesia (Fed. States of)", flows_2005$orig)
flows_2005$Dest <- gsub('Micronesia', "Micronesia (Fed. States of)", flows_2005$Dest)
flows_2005$orig <- gsub('Timor Leste', "Timor-Leste", flows_2005$orig)
flows_2005$orig <- gsub('Venezuela Bolivarian Republic of', "Venezuela (Bolivarian Republic of)", flows_2005$orig)
flows_2005$orig <- gsub('China  Hong Kong SAR', 'China, Hong Kong SAR', flows_2005$orig)
flows_2005$orig <- gsub('China  Macao', 'China, Macao', flows_2005$orig)

flows_2005$total_concern <- flows_2005$total_concern %>% replace_na(0)
flows_2005$migrants <- flows_2005$migrants %>% replace_na(0)
flows_2005$flow <- flows_2005$migrants - flows_2005$total_concern
flows_2005$migrants_less_forced <- sapply(flows_2005[,c('flow')], function(x) {ifelse(x < 0, 0, x)})


plot(flows_2005$migrants_less_forced)

####

flows_2010 <- merge(origin_dest_2010_long, unhcr_2010, by.x = c("Dest", 'orig'), 
                    by.y = c("Country.of.asylum", "Country.of.origin"), all = TRUE)

flows_2010$orig <- gsub('\\.', ' ', flows_2010$orig)
flows_2010$orig <- gsub('Bolivia  Plurinational State of', 'Bolivia (Plurinational State of)', flows_2010$orig)
flows_2010$orig <- gsub("Bolivia..Plurinational.State.of.", "Bolivia (Plurinational State of)", flows_2010$orig)
flows_2010$orig <- gsub('Bonaire  Sint Eustatius and Saba', 'Bonaire, Sint Eustatius and Saba', flows_2010$orig)
flows_2010$orig <- gsub('Dem  People s Republic of Korea', "Korea (Democratic People's Rep of)", flows_2010$orig)
flows_2010$orig <- gsub('People s', "People's", flows_2010$orig)
flows_2010$orig <- gsub('Iran  Islamic Republic of', "Iran (Islamic Republic of)", flows_2010$orig)
flows_2010$Dest <- gsub('Iran (Islamic Rep. of)', "Iran (Islamic Republic of)", flows_2010$Dest)
flows_2010$orig <- gsub('Micronesia  Fed  States of', "Micronesia (Fed. States of)", flows_2010$orig)
flows_2010$Dest <- gsub('Micronesia', "Micronesia (Fed. States of)", flows_2010$Dest)
flows_2010$orig <- gsub('Timor Leste', "Timor-Leste", flows_2010$orig)
flows_2010$orig <- gsub('Venezuela Bolivarian Republic of', "Venezuela (Bolivarian Republic of)", flows_2010$orig)
flows_2010$orig <- gsub('China  Hong Kong SAR', 'China, Hong Kong SAR', flows_2010$orig)
flows_2010$orig <- gsub('China  Macao', 'China, Macao', flows_2010$orig)


flows_2010$total_concern <- flows_2010$total_concern %>% replace_na(0)
flows_2010$migrants <- flows_2010$migrants %>% replace_na(0)
flows_2010$flow <- flows_2010$migrants - flows_2010$total_concern
flows_2010$migrants_less_forced <- sapply(flows_2010[,c('flow')], function(x) {ifelse(x < 0, 0, x)})

plot(flows_2010$migrants_less_forced)

####

flows_2015 <- merge(origin_dest_2015_long, unhcr_2015, by.x = c("Dest", 'orig'), 
                    by.y = c("Country.of.asylum", "Country.of.origin"), all = TRUE)

flows_2015$orig <- gsub('\\.', ' ', flows_2015$orig)
flows_2015$orig <- gsub('Bolivia  Plurinational State of', 'Bolivia (Plurinational State of)', flows_2015$orig)
flows_2015$orig <- gsub("Bolivia..Plurinational.State.of.", "Bolivia (Plurinational State of)", flows_2015$orig)
flows_2015$orig <- gsub('Bonaire  Sint Eustatius and Saba', 'Bonaire, Sint Eustatius and Saba', flows_2015$orig)
flows_2015$orig <- gsub('Dem  People s Republic of Korea', "Korea (Democratic People's Rep of)", flows_2015$orig)
flows_2015$orig <- gsub('People s', "People's", flows_2015$orig)
flows_2015$orig <- gsub('Iran  Islamic Republic of', "Iran (Islamic Republic of)", flows_2015$orig)
flows_2015$Dest <- gsub('Iran (Islamic Rep. of)', "Iran (Islamic Republic of)", flows_2015$Dest)
flows_2015$orig <- gsub('Micronesia  Fed  States of', "Micronesia (Fed. States of)", flows_2015$orig)
flows_2015$Dest <- gsub('Micronesia', "Micronesia (Fed. States of)", flows_2015$Dest)
flows_2015$orig <- gsub('Timor Leste', "Timor-Leste", flows_2015$orig)
flows_2015$orig <- gsub('Venezuela Bolivarian Republic of', "Venezuela (Bolivarian Republic of)", flows_2015$orig)
flows_2015$orig <- gsub('China  Hong Kong SAR', 'China, Hong Kong SAR', flows_2015$orig)
flows_2015$orig <- gsub('China  Macao', 'China, Macao', flows_2015$orig)


flows_2015$total_concern <- flows_2015$total_concern %>% replace_na(0)
flows_2015$migrants <- flows_2015$migrants %>% replace_na(0)
flows_2015$flow <- flows_2015$migrants - flows_2015$total_concern
flows_2015$migrants_less_forced <- sapply(flows_2015[,c('flow')], function(x) {ifelse(x < 0, 0, x)})


plot(flows_2015$migrants_less_forced)

####

flows_2019 <- merge(origin_dest_2019_long, unhcr_2019, by.x = c("Dest", 'orig'), 
                    by.y = c("Country.of.asylum", "Country.of.origin"), all = TRUE)

flows_2019$orig <- gsub('\\.', ' ', flows_2019$orig)
flows_2019$orig <- gsub('Bolivia  Plurinational State of', 'Bolivia (Plurinational State of)', flows_2019$orig)
flows_2019$orig <- gsub("Bolivia..Plurinational.State.of.", "Bolivia (Plurinational State of)", flows_2019$orig)
flows_2019$orig <- gsub('Bonaire  Sint Eustatius and Saba', 'Bonaire, Sint Eustatius and Saba', flows_2019$orig)
flows_2019$orig <- gsub('Dem  People s Republic of Korea', "Korea (Democratic People's Rep of)", flows_2019$orig)
flows_2019$orig <- gsub('People s', "People's", flows_2019$orig)
flows_2019$orig <- gsub('Iran  Islamic Republic of', "Iran (Islamic Republic of)", flows_2019$orig)
flows_2019$Dest <- gsub('Iran (Islamic Rep. of)', "Iran (Islamic Republic of)", flows_2019$Dest)
flows_2019$orig <- gsub('Micronesia  Fed  States of', "Micronesia (Fed. States of)", flows_2019$orig)
flows_2019$Dest <- gsub('Micronesia', "Micronesia (Fed. States of)", flows_2019$Dest)
flows_2019$orig <- gsub('Timor Leste', "Timor-Leste", flows_2019$orig)
flows_2019$orig <- gsub('Venezuela Bolivarian Republic of', "Venezuela (Bolivarian Republic of)", flows_2019$orig)
flows_2019$orig <- gsub('China  Hong Kong SAR', 'China, Hong Kong SAR', flows_2019$orig)
flows_2019$orig <- gsub('China  Macao', 'China, Macao', flows_2019$orig)


flows_2019$total_concern <- flows_2019$total_concern %>% replace_na(0)
flows_2019$migrants <- flows_2019$migrants %>% replace_na(0)
flows_2019$flow <- flows_2019$migrants - flows_2019$total_concern
flows_2019$migrants_less_forced <- sapply(flows_2019[,c('flow')], function(x) {ifelse(x < 0, 0, x)})

plot(flows_2019$migrants_less_forced)
