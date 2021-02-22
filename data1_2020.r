##
## data1 un bilateral stocks
## data2 un country and region summary
## data3 hdi and unhcr
## data4 migrant totals by hdi group
## data5 ecowas
## data6 schengen


library(tidyverse)
library(readxl)
library(countrycode)

d <- read_excel(path = "./data_raw/undesa_pd_2020_ims_stock_by_sex_destination_and_origin.xlsx", 
                sheet = 2, skip = 9, na = c("..", "-"))

d0 <- d %>%
  select(-1, -3, -5, -15:-28) %>%
  rename_all(funs(gsub("[[:punct::]+[0-9]+", "", .))) %>%
  rename(por_name = 1, 
         por_code = 2,
         pob_name = 3,
         pob_code = 4) %>%
  filter(!is.na(por_code) | !is.na(pob_code)) %>%
  pivot_longer(cols = -(1:4), names_to = "year", values_to = "stock") %>%
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
write_csv(x = d0, path = "./data/stock_bilat_2020.csv")
