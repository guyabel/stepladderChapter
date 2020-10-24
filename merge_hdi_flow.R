library(dplyr)

hdi_country <- as.data.frame(hdi_five_prepped)

hdi_1995 <-  hdi_country %>% subset(Year == 1995)
hdi_2000 <-  hdi_country %>% subset(Year == 2000)
hdi_2005 <-  hdi_country %>% subset(Year == 2005)
hdi_2010 <-  hdi_country %>% subset(Year == 2010)
hdi_2015 <-  hdi_country %>% subset(Year == 2015)
hdi_2018 <-  hdi_country %>% subset(Year == 2018)

hdi_flows_1995 <- merge(flows_1995, hdi_1995, by.x = 'Dest', by.y = 'Country', all.x = TRUE)
names(hdi_flows_1995)[11] <- 'Dest_HDI'
hdi_flows_1995 <- merge(hdi_flows_1995, hdi_1995, by.x = 'orig', by.y = 'Country', all.x = TRUE)
names(hdi_flows_1995)[13] <- 'Orig_HDI'
hdi_flows_1995 <- hdi_flows_1995[-c(4,10)]
hdi_flows_1995$Dest_HDI_1995 <- hdi_flows_1995$Dest_HDI
hdi_flows_1995$Orig_HDI_1995 <- hdi_flows_1995$Orig_HDI

just_1995 <- hdi_flows_1995[-c(3:11)]
hdi_flows_1995 <- hdi_flows_1995[-c(12:13)]

hdi_flows_2000 <- merge(flows_2000, hdi_2000, by.x = 'Dest', by.y = 'Country', all.x = TRUE)
names(hdi_flows_2000)[11] <- 'Dest_HDI'
hdi_flows_2000 <- merge(hdi_flows_2000, hdi_2000, by.x = 'orig', by.y = 'Country', all.x = TRUE)
names(hdi_flows_2000)[13] <- 'Orig_HDI'
hdi_flows_2000 <- hdi_flows_2000[-c(4,10)]
hdi_flows_2000_95 <- merge(hdi_flows_2000, just_1995, by.x = c('orig', 'Dest'), by.y = c('orig', 'Dest'))


hdi_flows_2005 <- merge(flows_2005, hdi_2005, by.x = 'Dest', by.y = 'Country', all.x = TRUE)
names(hdi_flows_2005)[11] <- 'Dest_HDI'
hdi_flows_2005 <- merge(hdi_flows_2005, hdi_2005, by.x = 'orig', by.y = 'Country', all.x = TRUE)
names(hdi_flows_2005)[13] <- 'Orig_HDI'
hdi_flows_2005 <- hdi_flows_2005[-c(4,10)]
hdi_flows_2005_95 <- merge(hdi_flows_2005, just_1995, by.x = c('orig', 'Dest'), by.y = c('orig', 'Dest'))


hdi_flows_2010 <- merge(flows_2010, hdi_2010, by.x = 'Dest', by.y = 'Country', all.x = TRUE)
names(hdi_flows_2010)[11] <- 'Dest_HDI'
hdi_flows_2010 <- merge(hdi_flows_2010, hdi_2010, by.x = 'orig', by.y = 'Country', all.x = TRUE)
names(hdi_flows_2010)[13] <- 'Orig_HDI'
hdi_flows_2010 <- hdi_flows_2010[-c(4,10)]
hdi_flows_2010_95 <- merge(hdi_flows_2010, just_1995, by.x = c('orig', 'Dest'), by.y = c('orig', 'Dest'))


hdi_flows_2015 <- merge(flows_2015, hdi_2015, by.x = 'Dest', by.y = 'Country', all.x = TRUE)
names(hdi_flows_2015)[11] <- 'Dest_HDI'
hdi_flows_2015 <- merge(hdi_flows_2015, hdi_2015, by.x = 'orig', by.y = 'Country', all.x = TRUE)
names(hdi_flows_2015)[13] <- 'Orig_HDI'
hdi_flows_2015 <- hdi_flows_2015[-c(4,10)]
hdi_flows_2015_95 <- merge(hdi_flows_2015, just_1995, by.x = c('orig', 'Dest'), by.y = c('orig', 'Dest'))


hdi_flows_2019 <- merge(flows_2019, hdi_2018, by.x = 'Dest', by.y = 'Country', all.x = TRUE)
names(hdi_flows_2019)[11] <- 'Dest_HDI'
hdi_flows_2019 <- merge(hdi_flows_2019, hdi_2018, by.x = 'orig', by.y = 'Country', all.x = TRUE)
names(hdi_flows_2019)[13] <- 'Orig_HDI'
hdi_flows_2019 <- hdi_flows_2019[-c(4,10)]
hdi_flows_2019_95 <- merge(hdi_flows_2019, just_1995, by.x = c('orig', 'Dest'), by.y = c('orig', 'Dest'))
hdi_flows_2019$Year <- 2019
hdi_flows_2019_95$Year <- 2019
hdi_flows_2019

