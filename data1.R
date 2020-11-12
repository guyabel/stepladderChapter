##
## data1 un bilateral stocks
## data2 un country and region summary
## data3 hdi
## data4 migrant totals by hdi group
##

library(tidyverse)
library(readxl)
library(countrycode)

d <- read_excel(path = "./data-raw/undesa_pd_2019_migrant_stock_origin_destination_dataset.xlsx", 
                sheet = 2, skip = 15, na = c("..", "-"))

d0 <- d %>%
  select(-2, -4, -6) %>%
  rename(year = 1, 
         por_name = 2, 
         por_code = 3) %>%
  filter(!is.na(por_code)) %>%
  pivot_longer(cols = -(1:3), names_to = "pob_name", values_to = "stock") %>%
  mutate(por = countryname(sourcevar = por_name, destination = "iso3c"), 
         pob = countryname(sourcevar = pob_name, destination = "iso3c"))

# check code guess worked - channel islands
d0 %>%
  select(pob_name, pob) %>%
  distinct() %>%
  filter(is.na(pob))

d0 %>%
  select(por_name, por) %>%
  distinct() %>%
  filter(is.na(por)) %>%
  print(n = 60)

# add a code for channel islands
d0 <- d0 %>%
  mutate(pob = ifelse(pob_name == "Channel Islands", 
                      yes = "CISL", no = pob),
         por = ifelse(por_name == "Channel Islands", 
                      yes = "CISL", no = por))

# save
write_csv(x = d0, path = "./data/stock_bilat.csv")
