##Used with ArcGIS Pro to generate table for Map.

library(here)
library(ggplot2)
library(tidyverse)

here()

#Load csvs based upon calculations from ArcGIS pro
perc_region_imm_2019 <- read.csv('country_pct_immigrants_2019_region.csv')
perc_region_em_2019 <- read.csv('country_pct_emigrants_2019_region.csv')
perc_region_imm_1995 <- read.csv('country_pct_immigrants_1995_region.csv')
perc_region_em_1995 <- read.csv('country_pct_emigrants_1995_region.csv')

#Stock and percent of population by region. #Only IMMIGRANT
region_summary <- function(dataset){
  dataset %>%
    drop_na()%>%
    group_by(Region)%>%
    summarise(sum_population_region = sum(Population), sum_migrants_region = sum(sum_migrants))%>%
    mutate(avg_immigrant = sum_migrants_region/sum_population_region)%>% #CHANGE TO migrants/emigrants + sum_population_region for emigrants
    view()
}
imm_2019_regions <- region_summary(perc_region_imm_2019)
em_2019_regions <- region_summary(perc_region_em_2019)
imm_1995_regions <- region_summary(perc_region_imm_1995)
em_1995_regions <- region_summary(perc_region_em_1995)


#merge by year
merge_regions_2019 <- merge(imm_2019_regions, em_2019_regions, by.x = "Region", by.y = "Region")
names(merge_regions_2019)[2:7] <- c("Total_pop_em_2019", "Total_emigrants_2019", "Avg.emigrant_2019", 
                                        "Total_pop_2019", "Total_immigrants_2019", "Avg.immigrant_2019")
merge_regions_1995 <- merge(imm_1995_regions, em_1995_regions, by.x = "Region", by.y = "Region")
names(merge_regions_1995)[2:7] <- c("Total_pop_em_1995", "Total_emigrants_1995", "Avg.emigrant_1995", 
                                        "Total_pop_1995", "Total_immigrants_1995", "Avg.immigrant_1995")
#merge into one dataset
merge_all_regions_years <- merge(merge_regions_1995, merge_regions_2019, by.x = "Region", by.y = "Region")

#write to csv
merge_all_regions_years %>%
  write.csv("merge_all_regions_years.csv")
