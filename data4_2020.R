##
## data1 un bilateral stocks
## data2 un country and region summary
## data3 hdi and unhcr
## data4 migrant totals by hdi group
## data5 ecowas
## data6 schengen


library(tidyverse)

d0 <- read_csv(file = "./data/stock_bilat_2020.csv", guess_max = 1e6)
d1 <- read_csv(file = "./data/stock_totals_2020.csv")
d2 <- read_csv(file = "./data/hdi_2020.csv")
d3 <- read_csv(file = "./data/unhcr_2020.csv")



d4a <- d2 %>%
  select(alpha3, year, contains("imp")) %>%
  group_by(alpha3) %>%
  rename(orig = alpha3, 
         orig_hdi = hdi_level_imp,
         orig_hdi19 = hdi_level19_imp)

d4b <- d4a %>%
  set_names(nm = str_replace_all(
    string = names(.), pattern = "orig", replacement = "dest"))

# bilateral stocks with hdi classifications
d4 <- d0 %>%
  filter(por_code < 900, pob_code < 900,
         !is.na(pob)) %>%
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
  replace_na(list(stepladder = 0))
d5


# save
write_csv(x = d5, path = "./data/hdi_country_bilat_2020.csv")

##
## hdi emigrant, immigrant and population totals - and shares
##
d6a <- d5 %>%
  drop_na() %>%
  group_by(orig_hdi, year) %>%
  summarise(emi = sum(stepladder)) %>%
  rename(hdi = orig_hdi)

d6b <- d5 %>%
  drop_na() %>%
  group_by(dest_hdi, year) %>%
  summarise(imm = sum(stepladder)) %>%
  rename(hdi = dest_hdi)

d6c <- d1 %>% 
  filter(!is.na(alpha3)) %>%
  select(alpha3, year, fb, pop, disp) %>%
  left_join(d4a, by = c("alpha3" = "orig", "year" = "year")) %>%
  rename(hdi = orig_hdi) %>%
  #drop_na() %>%
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
d6 <- d6a %>%
  left_join(d6b) %>%
  left_join(d6c) %>%
  mutate(imm_share = imm/pop,
         emi_share = emi/pop,
         fb_share = fb/pop, 
         disp_share = disp/pop)

# long form for ggplot
d7 <- d6 %>%
  pivot_longer(cols = -(1:2), names_to = "total", values_to = "stepladder") %>%
  mutate(share = str_detect(string = total, pattern = "share"), 
         stepladder = ifelse(share, yes = stepladder, no = stepladder/1e6))

# save
write_csv(x = d7, path = "./data/hdi_totals_2020.csv")

##
## hdi bilat totals
##
d8 <- d5 %>%
  drop_na() %>%
  group_by(orig_hdi, dest_hdi, year) %>%
  summarise(stock = sum(stepladder)/1e6) %>%
  rename(orig = orig_hdi, 
         dest = dest_hdi)

d9 <- d5 %>%
  drop_na() %>%
  group_by(orig_hdi19, dest_hdi19, year) %>%
  summarise(stock19 = sum(stepladder)/1e6) %>%
  rename(orig = orig_hdi19, 
         dest = dest_hdi19)

d10 <- left_join(d8, d9) %>%
  filter(year != 1990)
# save
write_csv(x = d10, path = "./data/hdi_bilat_2020.csv")

