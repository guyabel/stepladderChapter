##
## data1 un bilateral stocks
## data2 un country and region summary
## data3 hdi
## data4 migrant totals by hdi group
##

library(tidyverse)
library(countrycode)

d <- read_csv(file = "./data-raw/Human development index (HDI).csv", 
              na = c(".."), skip = 1)

# countries
d0 <- d %>%
  slice(1:189) %>%
  # tail()
  pivot_longer(cols = -(1:2), names_to = "year", values_to = "hdi") %>%
  rename(rank = 1, 
         name = 2)

# regions
d1 <- d %>%
  slice(203:n()) %>%
  select(-1) %>%
  pivot_longer(cols = -1, names_to = "year", values_to = "hdi") %>%
  rename(region = 1)

# levels  
d2 <- d %>%
  slice(197:201) %>%
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
    is.na(hdi) ~ "",
    hdi > 0.8 ~ "Very High",
    hdi > 0.7 ~ "High",
    hdi > 0.55 ~ "Medium",
    TRUE ~ "Low",
  )) %>%
  relocate(alpha3, year, hdi, hdi_level)

# - impute hdi level to avoid dropping big sending countries
#   from analysis using fill() function in tidyr
# - add hdi level 1995 following what is in some of the scripts
# - add 2019 and impute for too.
d4 <- d3 %>%
  group_by(alpha3) %>%
  mutate(hdi_level = na_if(x = hdi_level, y = ""),
         hdi_level_imp = hdi_level,
         year = as.integer(year)) %>%
  complete(year = 1990:2019, name) %>%
  fill(hdi_level_imp, .direction = "downup") %>%
  mutate(hdi_level95 = hdi_level[year == 1995],
         hdi_level95_imp = hdi_level_imp[year == 1995])

# imputation for past values
d4 %>%
  select(-rank, -name) %>%
  filter(alpha3 == "AGO")

# imputation for 2019
d4 %>%
  select(-rank, -name) %>%
  tail()

# save
write_csv(x = d4, path = "./data/hdi.csv")