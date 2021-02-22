##Loads DESA population data and merges it with Migrant/UNHCR/HDI dataset
library(here)
library(ggplot2)
library(tidyverse)

here()

#load DESA dataset stripped of UN header material#
pop_global <- read.csv(here('desa_pop_totals.csv'), stringsAsFactors = FALSE)

#DROP 1990#
pop_global_no90 <- pop_global[-c(2)]

#convert to long for origin populations
pop_long_orig <- pop_global_no90 %>%
  gather(key=Year, value = "Population", c(2:7),na.rm=FALSE)

#Clean column names
names(pop_long_orig)[1] <- "Country"
pop_long_orig$Year <- gsub("X", "", pop_long_orig$Year)
pop_long_orig$Year <- as.numeric(pop_long_orig$Year)

#convert to long and create separate dataset for Destination populations
pop_long_dest <- pop_long_orig
names(pop_long_dest)[3] <- "Pop_Dest"

#merge origin and dest datasets with hdi dataset (from "merge_hdi_stock.r")
hdi_migrants_pop <- merge(hdi_stocks_all, pop_long_orig, by.x = c("orig", "Year"), by.y = c("Country", "Year"), all.x = TRUE)
hdi_migrants_pop <- merge(hdi_migrants_pop, pop_long_dest, by.x = c("Dest", "Year"), by.y = c("Country", "Year"), all.x = TRUE)

#disaggregate populations by year and orig/dest
hdi_pop_1995 <-  hdi_migrants_pop %>% subset(Year == 1995)
pop_orig_1995 <- pop_long_orig %>% subset(Year == 1995)
pop_dest_1995 <- pop_long_dest %>% subset(Year == 1995)

hdi_pop_2000 <-  hdi_migrants_pop %>% subset(Year == 2000)
pop_orig_2000 <- pop_long_orig %>% subset(Year == 2000)
pop_dest_2000 <- pop_long_dest %>% subset(Year == 2000)

hdi_pop_2005 <-  hdi_migrants_pop %>% subset(Year == 2005)
pop_orig_2005 <- pop_long_orig %>% subset(Year == 2005)
pop_dest_2005 <- pop_long_dest %>% subset(Year == 2005)

hdi_pop_2010 <-  hdi_migrants_pop %>% subset(Year == 2010)
pop_orig_2010 <- pop_long_orig %>% subset(Year == 2010)
pop_dest_2010 <- pop_long_dest %>% subset(Year == 2010)

hdi_pop_2015 <-  hdi_migrants_pop %>% subset(Year == 2015)
pop_orig_2015 <- pop_long_orig %>% subset(Year == 2015)
pop_dest_2015 <- pop_long_dest %>% subset(Year == 2015)

hdi_pop_2019 <-  hdi_migrants_pop %>% subset(Year == 2019)
pop_orig_2019 <- pop_long_orig %>% subset(Year == 2019)
pop_dest_2019 <- pop_long_dest %>% subset(Year == 2019)


##Sum Immigrant Info and merge with population
migrant_pop_summing <- function(dataset, dest){
  summed <- dataset %>%
    gather(key="Country", value = "Orig_HDI", c(11),na.rm=TRUE)%>%
    group_by(Dest, Orig_HDI, Dest_HDI, Year) %>%
    summarise(migrants = sum(stock, na.rm = TRUE))
  merge(summed, dest, by.x = c("Dest", "Year"), by.y = c("Country", "Year"), all.x = TRUE)
}

migrant_pop_orig_summing <- function(dataset, orig){
  summed <- dataset %>%
    gather(key="Country", value = "Dest_HDI", c(10),na.rm=TRUE)%>%
    group_by(orig, Orig_HDI, Dest_HDI, Year) %>%
    summarise(migrants = sum(stock, na.rm = TRUE))
  merge(summed, orig, by.x = c("orig", "Year"), by.y = c("Country", "Year"), all.x = TRUE)
}
#FIND THE MEDIAN IMMIGRANT Percents
median_immigrant_proportion <- function(dataset){
  dataset %>%
    mutate(dest_perc_immigrant = migrants/Pop_Dest) %>%
    group_by(Dest_HDI) %>%
    summarise(median_immigrants = median(dest_perc_immigrant, na.rm = TRUE))
}
median_emigrant_proportion <- function(dataset){
  dataset %>%
    mutate(orig_perc_emigrant = migrants/Population) %>%
    group_by(Orig_HDI) %>%
    summarise(median_emigrants = (-1*(median(orig_perc_emigrant, na.rm = TRUE))))
}

##Find the TOTAL Immigrant/Emigrant Percents
total_immigrant_proportion <- function(dataset){
  dataset %>%
    group_by(Dest_HDI) %>%
    summarise(total_immigrants = ((sum(migrants)/sum(Pop_Dest))))
}
total_emigrant_proportion <- function(dataset){
  dataset %>%
    group_by(Orig_HDI) %>%
    summarise(total_emigrants = ((-1*(sum(migrants)/sum(Population)))))
}

##Find the MEAN Immigrant/Emigrant Percents
mean_immigrant_proportion <- function(dataset){
  dataset %>%
    mutate(dest_perc_immigrant = migrants/Pop_Dest) %>%
    group_by(Dest_HDI) #%>%
    #summarise(mean_immigrants = mean(dest_perc_immigrant, na.rm = TRUE))
}
mean_emigrant_proportion <- function(dataset){
  dataset %>%
    mutate(orig_perc_emigrant = migrants/(migrants + Population)) %>%
    group_by(Orig_HDI) #%>%
    #summarise(mean_emigrants = (-1*(mean(orig_perc_emigrant, na.rm = TRUE))))
}

#DISAGGREGATE BY YEAR

##1995
pop_sum_1995 <- migrant_pop_summing(hdi_pop_1995, pop_dest_1995)
pop_sum_orig_1995 <- migrant_pop_orig_summing(hdi_pop_1995, pop_orig_1995)
pop_median_imm_1995 <- median_immigrant_proportion(pop_sum_1995)
pop_median_em_1995 <- median_emigrant_proportion(pop_sum_orig_1995)
pop_total_imm_1995 <- total_immigrant_proportion(pop_sum_1995)
pop_total_em_1995 <- total_emigrant_proportion(pop_sum_orig_1995)
pop_mean_imm_1995 <- mean_immigrant_proportion(pop_sum_1995)
pop_mean_em_1995 <- mean_emigrant_proportion(pop_sum_orig_1995)

##2000
pop_sum_2000 <- migrant_pop_summing(hdi_pop_2000, pop_dest_2000)
pop_sum_orig_2000 <- migrant_pop_orig_summing(hdi_pop_2000, pop_orig_2000)
pop_median_imm_2000 <- median_immigrant_proportion(pop_sum_2000)
pop_median_em_2000 <- median_emigrant_proportion(pop_sum_orig_2000)
pop_total_imm_2000 <- total_immigrant_proportion(pop_sum_2000)
pop_total_em_2000 <- total_emigrant_proportion(pop_sum_orig_2000)
pop_mean_imm_2000 <- mean_immigrant_proportion(pop_sum_2000)
pop_mean_em_2000 <- mean_emigrant_proportion(pop_sum_orig_2000)

##2005
pop_sum_2005 <- migrant_pop_summing(hdi_pop_2005, pop_dest_2005)
pop_sum_orig_2005 <- migrant_pop_orig_summing(hdi_pop_2005, pop_orig_2005)
pop_median_imm_2005 <- median_immigrant_proportion(pop_sum_2005)
pop_median_em_2005 <- median_emigrant_proportion(pop_sum_orig_2005)
pop_total_imm_2005 <- total_immigrant_proportion(pop_sum_2005)
pop_total_em_2005 <- total_emigrant_proportion(pop_sum_orig_2005)
pop_mean_imm_2005 <- mean_immigrant_proportion(pop_sum_2005)
pop_mean_em_2005 <- mean_emigrant_proportion(pop_sum_orig_2005)

##2010
pop_sum_2010 <- migrant_pop_summing(hdi_pop_2010, pop_dest_2010)
pop_sum_orig_2010 <- migrant_pop_orig_summing(hdi_pop_2010, pop_orig_2010)
pop_median_imm_2010 <- median_immigrant_proportion(pop_sum_2010)
pop_median_em_2010 <- median_emigrant_proportion(pop_sum_orig_2010)
pop_total_imm_2010 <- total_immigrant_proportion(pop_sum_2010)
pop_total_em_2010 <- total_emigrant_proportion(pop_sum_orig_2010)
pop_mean_imm_2010 <- mean_immigrant_proportion(pop_sum_2010)
pop_mean_em_2010 <- mean_emigrant_proportion(pop_sum_orig_2010)

##2015
pop_sum_2015 <- migrant_pop_summing(hdi_pop_2015, pop_dest_2015)
pop_sum_orig_2015 <- migrant_pop_orig_summing(hdi_pop_2015, pop_orig_2015)
pop_median_imm_2015 <- median_immigrant_proportion(pop_sum_2015)
pop_median_em_2015 <- median_emigrant_proportion(pop_sum_orig_2015)
pop_total_imm_2015 <- total_immigrant_proportion(pop_sum_2015)
pop_total_em_2015 <- total_emigrant_proportion(pop_sum_orig_2015)
pop_mean_imm_2015 <- mean_immigrant_proportion(pop_sum_2015)
pop_mean_em_2015 <- mean_emigrant_proportion(pop_sum_orig_2015)

##2019
pop_sum_2019 <- migrant_pop_summing(hdi_pop_2019, pop_dest_2019)
pop_sum_orig_2019 <- migrant_pop_orig_summing(hdi_pop_2019, pop_orig_2019)
pop_median_imm_2019 <- median_immigrant_proportion(pop_sum_2019)
pop_median_em_2019 <- median_emigrant_proportion(pop_sum_orig_2019)
pop_total_imm_2019 <- total_immigrant_proportion(pop_sum_2019)
pop_total_em_2019 <- total_emigrant_proportion(pop_sum_orig_2019)
pop_mean_imm_2019 <- mean_immigrant_proportion(pop_sum_2019)
pop_mean_em_2019 <- mean_emigrant_proportion(pop_sum_orig_2019)
