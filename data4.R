##
## data1 un bilateral stocks
## data2 un country and region summary
## data3 hdi
## data4 migrant totals by hdi group
##

library(tidyverse)

d0 <- read_csv(file = "./data/stock_bilat.csv", guess_max = 1e6)
d1 <- read_csv(file = "./data/stock_totals.csv")
d2 <- read_csv(file = "./data/hdi.csv")

d3a <- d2 %>%
  select(alpha3, year, contains("imp")) %>%
  group_by(alpha3) %>%
  rename(orig = alpha3, 
         orig_hdi = hdi_level_imp,
         orig_hdi95 = hdi_level95_imp)

d3b <- d3a %>%
  set_names(nm = str_replace_all(
    string = names(.), pattern = "orig", replacement = "dest"))

# bilateral stocks with hdi classifications
d3 <- d0 %>%
  filter(por_code < 900, 
         !is.na(pob)) %>%
  replace_na(list(stock = 0)) %>%
  rename(orig = pob, 
         dest = por) %>%
  left_join(d3a) %>%
  left_join(d3b)
d3 

# countries with no hdi, but with stock data
d3 %>%
  filter(is.na(orig_hdi)) %>%
  select(orig, pob_name) %>%
  distinct() %>%
  left_join(d1 %>% filter(year == 2019) %>% select(alpha3, pop),
            by = c("orig" = "alpha3")) %>%
  arrange(desc(pop)) %>%
  print(n = 50)

##
## hdi emigrant, immigrant and population totals - and shares
##
d4a <- d3 %>%
  drop_na() %>%
  group_by(orig_hdi, year) %>%
  summarise(emi = sum(stock)) %>%
  rename(hdi = orig_hdi)

d4b <- d3 %>%
  drop_na() %>%
  group_by(dest_hdi, year) %>%
  summarise(imm = sum(stock)) %>%
  rename(hdi = dest_hdi)
  
d4c <- d1 %>% 
  filter(!is.na(alpha3)) %>%
  select(alpha3, year, fb, pop, disp) %>%
  left_join(d3a, by = c("alpha3" = "orig", "year" = "year")) %>%
  rename(hdi = orig_hdi) %>%
  drop_na() %>%
  group_by(hdi, year) %>%
  summarise(fb = sum(fb), 
            pop = sum(pop),
            disp = sum(disp))

# combine into total data base
# note the fb and imm are measuring the same thing.. but imm total will 
# be less than foreign born total as based on bilateral data where have 
# to drop some origins as do not have HDI data
#
# the same for disp and emi, where disp is the dispora total from the 
# complete bilateral data 
#
# ... saving both with the option to use either.
d4 <- d4a %>%
  left_join(d4b) %>%
  left_join(d4c) %>%
  mutate(imm_share = imm/pop,
         emi_share = emi/pop,
         fb_share = fb/pop, 
         disp_share = disp/pop)

# long form for ggplot
d5 <- d4 %>%
  pivot_longer(cols = -(1:2), names_to = "total", values_to = "stock") %>%
  mutate(share = str_detect(string = total, pattern = "share"), 
         stock = ifelse(share, yes = stock, no = stock/1e6))

# save
write_csv(x = d5, path = "./data/hdi_totals.csv")

##
## hdi bilat totals
##
d6 <- d3 %>%
  drop_na() %>%
  group_by(orig_hdi, dest_hdi, year) %>%
  summarise(stock = sum(stock)/1e6) %>%
  rename(orig = orig_hdi, 
         dest = dest_hdi)

d7 <- d3 %>%
  drop_na() %>%
  group_by(orig_hdi95, dest_hdi95, year) %>%
  summarise(stock95 = sum(stock)/1e6) %>%
  rename(orig = orig_hdi95, 
         dest = dest_hdi95)

d8 <- left_join(d6, d7)
# save
write_csv(x = d8, path = "./data/hdi_bilat.csv")