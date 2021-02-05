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


read_un_totals <- function(s = NULL, col = NULL, col_name = NULL){
  read_excel(path = "./data_raw/undesa_pd_2020_ims_stock_by_sex_and_destination.xlsx", 
             sheet = s, skip = 10, na = "..") %>%
    select({col}) %>%
    rename(name = 1, 
           country_code = 2) %>%
    pivot_longer(cols = -(1:2), names_to = "year", values_to = {col_name}) %>%
    mutate(year = str_sub(string = year, end = 4),
           year = as.integer(year),
           alpha3 = countryname(sourcevar = name, destination = "iso3c"),
           alpha3 = ifelse(name == "Channel Islands", yes = "CISL", no = alpha3))
}

# not sure if we need all these.. but grabbing them just in case
d0 <- read_un_totals(s = 2, col = c(2, 4, 6:12), col_name = "fb")
d1 <- read_un_totals(s = 3, col = c(2, 4, 6:12), col_name = "pop")
d2 <- read_un_totals(s = 4, col = c(2, 4, 6:12), col_name = "fb_share")
d3 <- read_un_totals(s = 7, col = c(2, 4, 6:12), col_name = "refugee")

# need bilat for dispora totals in table
d <- read_csv(file = "./data/stock_bilat_2020.csv", guess_max = 1e5)
d4 <- d %>%
  filter(por_name == "WORLD") %>%
  mutate(pob_name = ifelse(pob_name == "Total", yes = "WORLD", no = pob_name)) %>%
  select(contains("pob"), year, stock) %>%
  rename(name = pob_name, 
         alpha3 = pob,
         disp = stock)

# imm from a bilat data as a check/alternative to fb in d0 (should be the same)
d5 <- d %>%
  filter(pob_name == "Total") %>%
  select(contains("por"), year, stock) %>%
  rename(name = por_name, 
         country_code = por_code, 
         alpha3 = por,
         imm = stock)

d6 <- d0 %>%
  left_join(d1) %>%
  left_join(d2) %>%
  left_join(d3) %>%
  left_join(d4) %>%
  left_join(d5) %>%
  relocate(alpha3) %>%
  filter(!is.na(country_code)) %>%
  mutate(pop = pop * 1e3, 
         disp_share = disp/pop)
write_csv(x = d6, path = "./data/stock_totals_2020.csv")

# one non-zero difference... not sure why?... going to write to pop division
# d6 %>%
#   mutate(x = fb - imm) %>%
#   filter(x != 0) %>%
#   relocate(x, fb, imm, year) %>%
#   select(1:6)

