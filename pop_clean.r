library(here)
library(ggplot2)
library(tidyverse)

here()

pop_global <- read.csv(here('desa_pop_totals.csv'), stringsAsFactors = FALSE)

#DROP 1990#
pop_global_no90 <- pop_global[-c(2)]

pop_long <- pop_global_no90 %>%
  gather(key=Year, value = "Population", c(2:7),na.rm=FALSE)


names(pop_long)[1] <- "Country"
pop_long$Year <- gsub("X", "", pop_long$Year)
pop_long$Year <- as.numeric(pop_long$Year)
head(pop_long)

pop_long_dest <- pop_long
names(pop_long_dest)[3] <- "Pop_Dest"
head(pop_long_dest)
head(hdi_flows_all)

hdi_migrants_pop <- merge(hdi_flows_all, pop_long, by.x = c("orig", "Year"), by.y = c("Country", "Year"), all.x = TRUE)
hdi_migrants_pop <- merge(hdi_migrants_pop, pop_long_dest, by.x = c("Dest", "Year"), by.y = c("Country", "Year"), all.x = TRUE)
head(hdi_migrants_pop, 5)

hdi_pop_1995 <-  hdi_migrants_pop %>% subset(Year == 1995)
pop_orig_1995 <- pop_long %>% subset(Year == 1995)
pop_dest_1995 <- pop_long_dest %>% subset(Year == 1995)

hdi_pop_2000 <-  hdi_migrants_pop %>% subset(Year == 2000)
pop_orig_2000 <- pop_long %>% subset(Year == 2000)
pop_dest_2000 <- pop_long_dest %>% subset(Year == 2000)

hdi_pop_2005 <-  hdi_migrants_pop %>% subset(Year == 2005)
pop_orig_2005 <- pop_long %>% subset(Year == 2005)
pop_dest_2005 <- pop_long_dest %>% subset(Year == 2005)

hdi_pop_2010 <-  hdi_migrants_pop %>% subset(Year == 2010)
pop_orig_2010 <- pop_long %>% subset(Year == 2010)
pop_dest_2010 <- pop_long_dest %>% subset(Year == 2010)

hdi_pop_2015 <-  hdi_migrants_pop %>% subset(Year == 2015)
pop_orig_2015 <- pop_long %>% subset(Year == 2015)
pop_dest_2015 <- pop_long_dest %>% subset(Year == 2015)

hdi_pop_2019 <-  hdi_migrants_pop %>% subset(Year == 2019)
pop_orig_2019 <- pop_long %>% subset(Year == 2019)
pop_dest_2019 <- pop_long_dest %>% subset(Year == 2019)

head(hdi_pop_2019)
syria_orig_2019 <- flows_2019 %>%
  filter(orig=="Syrian Arab Republic") %>%
  summarise(sum = sum(flow))
syria_orig_2019

kazakhstan_orig_2019 <- flows_2019 %>%
  filter(orig=="Kazakhstan") %>%
  summarise(sum = sum(flow))
kazakhstan_orig_2019


##Sum Immigrant Info and merge with population
migrant_pop_summing <- function(dataset, dest){
  summed <- dataset %>%
    gather(key="Country", value = "Orig_HDI", c(11),na.rm=TRUE)%>%
    group_by(Dest, Orig_HDI, Dest_HDI, Year) %>%
    summarise(migrants = sum(flow, na.rm = TRUE))
  merge(summed, dest, by.x = c("Dest", "Year"), by.y = c("Country", "Year"), all.x = TRUE)
}

migrant_pop_orig_summing <- function(dataset, orig){
  summed <- dataset %>%
    gather(key="Country", value = "Dest_HDI", c(10),na.rm=TRUE)%>%
    group_by(orig, Orig_HDI, Dest_HDI, Year) %>%
    summarise(migrants = sum(flow, na.rm = TRUE))
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
    mutate(orig_perc_emigrant = migrants/Population) %>%
    group_by(Orig_HDI) #%>%
    #summarise(mean_emigrants = (-1*(mean(orig_perc_emigrant, na.rm = TRUE))))
}


##1995
pop_sum_1995 <- migrant_pop_summing(hdi_pop_1995, pop_dest_1995)
pop_sum_orig_1995 <- migrant_pop_orig_summing(hdi_pop_1995, pop_orig_1995)
pop_median_imm_1995 <- median_immigrant_proportion(pop_sum_1995)
pop_median_em_1995 <- median_emigrant_proportion(pop_sum_orig_1995)
pop_total_imm_1995 <- total_immigrant_proportion(pop_sum_1995)
pop_total_em_1995 <- total_emigrant_proportion(pop_sum_orig_1995)
head(pop_total_imm_1995)
head(pop_total_em_1995)
##2000
pop_sum_2000 <- migrant_pop_summing(hdi_pop_2000, pop_dest_2000)
pop_sum_orig_2000 <- migrant_pop_orig_summing(hdi_pop_2000, pop_orig_2000)
pop_median_imm_2000 <- median_immigrant_proportion(pop_sum_2000)
pop_median_em_2000 <- median_emigrant_proportion(pop_sum_orig_2000)
pop_total_imm_2000 <- total_immigrant_proportion(pop_sum_2000)
pop_total_em_2000 <- total_emigrant_proportion(pop_sum_orig_2000)

##2005
pop_sum_2005 <- migrant_pop_summing(hdi_pop_2005, pop_dest_2005)
pop_sum_orig_2005 <- migrant_pop_orig_summing(hdi_pop_2005, pop_orig_2005)
pop_median_imm_2005 <- median_immigrant_proportion(pop_sum_2005)
pop_median_em_2005 <- median_emigrant_proportion(pop_sum_orig_2005)
pop_total_imm_2005 <- total_immigrant_proportion(pop_sum_2005)
pop_total_em_2005 <- total_emigrant_proportion(pop_sum_orig_2005)

##2010
pop_sum_2010 <- migrant_pop_summing(hdi_pop_2010, pop_dest_2010)
pop_sum_orig_2010 <- migrant_pop_orig_summing(hdi_pop_2010, pop_orig_2010)
pop_median_imm_2010 <- median_immigrant_proportion(pop_sum_2010)
pop_median_em_2010 <- median_emigrant_proportion(pop_sum_orig_2010)
pop_total_imm_2010 <- total_immigrant_proportion(pop_sum_2010)
pop_total_em_2010 <- total_emigrant_proportion(pop_sum_orig_2010)


##2015
pop_sum_2015 <- migrant_pop_summing(hdi_pop_2015, pop_dest_2015)
pop_sum_orig_2015 <- migrant_pop_orig_summing(hdi_pop_2015, pop_orig_2015)
pop_median_imm_2015 <- median_immigrant_proportion(pop_sum_2015)
pop_median_em_2015 <- median_emigrant_proportion(pop_sum_orig_2015)
pop_total_imm_2015 <- total_immigrant_proportion(pop_sum_2015)
pop_total_em_2015 <- total_emigrant_proportion(pop_sum_orig_2015)

##2019
pop_sum_2019 <- migrant_pop_summing(hdi_pop_2019, pop_dest_2019)
pop_sum_orig_2019 <- migrant_pop_orig_summing(hdi_pop_2019, pop_orig_2019)
pop_median_imm_2019 <- median_immigrant_proportion(pop_sum_2019)
pop_median_em_2019 <- median_emigrant_proportion(pop_sum_orig_2019)
pop_total_imm_2019 <- total_immigrant_proportion(pop_sum_2019)
pop_total_em_2019 <- total_emigrant_proportion(pop_sum_orig_2019)
pop_mean_imm_2019 <- mean_immigrant_proportion(pop_sum_2019)
pop_mean_em_2019 <- mean_emigrant_proportion(pop_sum_orig_2019)

###Aggregation###
head(pop_sum_2019)

dest_hdi_summary <- function(dataset){
  dataset %>%
    drop_na()%>%
    group_by(Dest)%>%
    summarise(sum_migrants_region = sum(migrants))%>%
    #mutate(avg_immigrant = sum_migrants_region/sum_population_region)%>%
    view()
}
dest_hdi_summary(hdi_pop_2019)
