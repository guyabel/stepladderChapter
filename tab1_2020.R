##
## plot1_2020 bar charts and divergent bar charts
## plot2_2020 area charts
## plot3_2020 area charts for ECOWAS/Schengen
## tab1_2020 table of regional (not HDI) stepladder migrants
## tab2_2020 top20s stepladder (1995, 2020)

library(tidyverse)
d <- read_csv(file = "./data/country_region.csv")
d0 <- read_csv(file = "./data/stock_bilat_2020.csv", guess_max = 1e6)
d1 <- read_csv(file = "./data/stock_totals_2020.csv")
d2 <- read_csv(file = "./data/hdi_2020.csv")
d3 <- read_csv(file = "./data/unhcr_2020.csv")
pop <- read_csv(file = "./data/pop_totals_2020.csv")


regions <- c(903, 904, 905, 908, 909, 935)

d4a <- d2 %>%
  select(alpha3, year, contains("imp")) %>%
  group_by(alpha3) %>%
  rename(orig = alpha3, 
         orig_hdi = hdi_level_imp,
         orig_hdi20 = hdi_level20_imp)

d4b <- d4a %>%
  set_names(nm = str_replace_all(
    string = names(.), pattern = "orig", replacement = "dest"))

# bilateral stocks with hdi classifications
d4 <- d0 %>%
  filter(por_code < 900 , pob_code < 900, !is.na(pob)) %>%
  replace_na(list(stock = 0)) %>%
  rename(orig = pob, 
         dest = por) %>%
  left_join(d4a) %>%
  left_join(d4b) %>%
  left_join(d3) %>%
  replace_na(list(refugees = 0, asylum_seekers = 0))
d4 


# countries with no hdi, but with stock data
d4 %>%
  filter(is.na(orig_hdi)) %>%
  select(orig, pob_name) %>%
  distinct() %>%
  left_join(d1 %>% filter(year == 2020) %>% select(alpha3, pop),
            by = c("orig" = "alpha3")) %>%
  arrange(desc(pop)) %>%
  print(n = 50)


##Subtract refugees and asylum-seekers from stock
d5 <- d4 %>%
  mutate(stock_less_forced = stock-(refugees+asylum_seekers)) %>%
  replace_na(list(stock_less_forced = 0)) %>%
  mutate(stepladder = sapply(stock_less_forced, function(x) {ifelse(x < 0, 0, x)})) %>%
  replace_na(list(stepladder = 0)) #%>%
d5

d6a <- d5 %>%
  left_join(d, by = c("dest" = "alpha3")) %>%
  select(-17) %>%
  rename("Dest_region" = "Region") %>%
  group_by(Dest_region, year) %>%
  summarise(imm = sum(stepladder))


d6b <- d5 %>%
  left_join(d, by = c("orig" = "alpha3")) %>%
  select(-17) %>%
  rename("Orig_region" = "Region") %>%
  group_by(Orig_region, year) %>%
  summarise(emi = sum(stepladder))

d6a$Dest_region <- str_to_title(d6a$Dest_region)
d6b$Orig_region <- str_to_title(d6b$Orig_region)
pop$name <- str_to_title(pop$name)

d6c <- pop %>%
  filter(country_code %in% regions) %>%
  mutate(pop = pop*1000) %>%
  rename("Dest_region" = "name")

d6 <- d6a %>%
  left_join(d6c) %>%
  mutate(imm_share = imm/pop)

write_csv(x = d6, path = "./data/imm_pop_region_2020.csv")

