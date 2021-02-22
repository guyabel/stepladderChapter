##
## DESA update 2020 code
## data1_2020 un bilateral stocks
## data2_2020 un country and region summary
## data3_2020 hdi and unhcr
## data4_2020 migrant totals by hdi group
## data5_2020 ecowas
## data6_2020 schengen

library(tidyverse)
library(countrycode)

# Load HDI data, in GitHub file but also can be found at http://hdr.undp.org/en/data
d <- read_csv(file = "./data_raw/Human development index (HDI)_2019.csv", 
              na = c(".."), skip = 5)

# countries...have to rename year to 2020 to make consistent with DESA update
d0 <- d %>%
  rename("2020"="2019")%>%
  slice(1:189) %>%
  # tail()
  pivot_longer(cols = -(1:2), names_to = "year", values_to = "hdi") %>%
  rename(rank = 1, 
         name = 2)

# regions
d1 <- d %>%
  slice(196:n()) %>%
  select(-1) %>%
  pivot_longer(cols = -1, names_to = "year", values_to = "hdi") %>%
  rename(region = 1)

# levels  
d2 <- d %>%
  slice(191:195) %>%
  select(-1) %>%
  mutate(level_abb = c("very_high", "high", "medium", "low", "developing")) %>%
  relocate(level_abb) %>%
  pivot_longer(cols = -(1:2), names_to = "year", values_to = "hdi") %>%
  rename(level = 2) %>%
  arrange(year)

# country codes to add to d0
d3a <- d0 %>%
  select(name) %>%
  distinct() %>%
  mutate(alpha3 = countryname(sourcevar = name, destination = "iso3c"))

d3a %>%
  filter(is.na(alpha3))

# add code and levels to d0
d3 <- d0 %>%
  left_join(d3a) %>%
  mutate(hdi_level = case_when(
    is.na(hdi) ~ "Low",
    hdi > 0.8 ~ "Very High",
    hdi > 0.7 ~ "High",
    hdi > 0.55 ~ "Medium",
    TRUE ~ "Low",
  )) %>%
  relocate(alpha3, year, hdi, hdi_level)

# - impute hdi level to avoid dropping big sending countries
#   from analysis using fill() function in tidyr
# - add hdi level 1995 following what is in some of the scripts
# - add 2020 and impute for too.
d4 <- d3 %>%
  group_by(alpha3) %>%
  mutate(hdi_level = na_if(x = hdi_level, y = ""),
         hdi_level_imp = hdi_level,
         year = as.integer(year)) %>%
  complete(year = 1995:2020, name) %>%
  fill(hdi_level_imp, .direction = "downup") %>%
  mutate(#hdi_level95 = hdi_level[year == 1995],
    #hdi_level95_imp = hdi_level_imp[year == 1995],
    hdi_level20 = hdi_level[year == 2020],
    hdi_level20_imp = hdi_level_imp[year == 2020])
d4
# imputation for past values
d4 %>%
  select(-rank, -name) %>%
  filter(alpha3 == "AGO")

# imputation for 2020
d4 %>%
  select(-rank, -name) %>%
  tail()

# save
write_csv(x = d4, path = "./data/hdi_2020.csv")


# Load unhcr data, can also be found at https://www.unhcr.org/refugee-statistics/download/?url=E1ZxP4
d <- read_csv(file = "./data_raw/population.csv", 
              na = c(".."), skip = 14)[,1:7]

# rename
d0 <- d %>%
  select(-2, -4) %>%
  rename(year = 1,
         orig = 2,
         dest = 3,
         refugees = 4,
         asylum_seekers = 5) %>%
  mutate(year = ifelse(year=="2019", "2020", year))
  

# save
write_csv(x = d0, path = "./data/unhcr_2020.csv")
