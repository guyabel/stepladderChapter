##Creates underlying dataset for top 20 table  **CURRENTLY IN CHAPTER**

library(here)
library(ggplot2)
library(tidyverse)

here()

#Calculate absolute migrant stocks and percents of populations based on HDI classification. Files from "pop_clean.r"

#DESTINATION 1995
country_pct_imm_1995_dest <- pop_sum_1995 %>%
  group_by(Dest, Pop_Dest) %>%
  summarise(sum_immigrants = sum(migrants)) %>%
  mutate(pct_of_pop = sum_immigrants/Pop_Dest) %>%
  write.csv("country_pct_immigrants_1995.csv")

#DESTINATION 2019
country_pct_imm_2019_dest <- pop_sum_2019 %>%
  group_by(Dest, Pop_Dest) %>%
  summarise(sum_immigrants = sum(migrants)) %>%
  mutate(pct_of_pop = sum_immigrants/Pop_Dest) %>%
  write.csv("country_pct_immigrants_2019.csv")

#ORIGIN 1995
country_pct_em_1995_orig <- pop_sum_orig_1995 %>%
  group_by(orig, Population) %>%
  summarise(sum_emigrants = sum(migrants)) %>%
  mutate(pct_of_pop = sum_emigrants/(sum_emigrants + Population)) %>%
  write.csv("country_pct_emigrants_1995.csv")

#ORIGIN 2019
country_pct_em_2019_orig <- pop_sum_orig_2019 %>%
  group_by(orig, Population) %>%
  summarise(sum_emigrants = sum(migrants)) %>%
  mutate(pct_of_pop = sum_emigrants/(sum_emigrants + Population)) %>%
  write.csv("country_pct_emigrants_2019.csv")

