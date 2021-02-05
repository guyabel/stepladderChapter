##
## data1 un bilateral stocks
## data2 un country and region summary
## data3 hdi and unhcr
## data4 migrant totals by hdi group
## data5 ecowas
## data6 ecowas

library(tidyverse)

d0 <- read_csv(file = "./data/hdi_country_bilat_2020.csv")
d1 <- read_csv(file = "./data/ecowas.csv")
d2 <- read_csv(file = "./data/stock_totals_2020.csv")

d1 <- d1 %>%
  select(-1) %>%
  pivot_longer(!alpha3, names_to = "year", values_to = "ecowas") %>%
  mutate(year = str_sub(string = year, end = 4),
         year = as.integer(year)) %>%
  filter(year != 1990)
unique(d1$alpha3)

# join ecowas status to main database
d3a <- d0 %>%
  left_join(d1, by = c("year" = "year", "dest"  ="alpha3")) %>%
  mutate(ecowas_dest = ifelse(dest %in% d1$alpha3, 1, 0)) %>%
  replace_na(list(ecowas_dest = 0))


d3b <- d0 %>%
  left_join(d1, by = c("year" = "year", "orig"="alpha3")) %>%
  mutate(ecowas_orig = ifelse(orig %in% d1$alpha3, 1, 0)) %>%
  replace_na(list(ecowas_orig = 0))

# Filter only bilats according to whether they include ecowas country
d3 <- d3a %>%
  full_join(d3b) %>%
  mutate(ecowas = case_when(ecowas_orig == 1 & ecowas_dest == 1 ~ "3.ecowas corridor", 
                            ecowas_orig == 1 | ecowas_dest == 1 ~ "2.ecowas origin or destination",
                            ecowas_orig != 1 | ecowas_dest != 1 ~ "1.not ecowas")) %>%
  filter(year != 1990)

d3_us <- d3 %>% filter(por_name == "United States of America*")

##
## hdi emigrant, immigrant and population totals - and shares
##
d4a <- d3 %>%
  group_by(orig_hdi, year, ecowas) %>%
  summarise(emi = sum(stepladder)) %>%
  rename(hdi = orig_hdi)

d4b <- d3 %>%
  group_by(dest_hdi, year, ecowas) %>%
  summarise(imm = sum(stepladder)) %>%
  rename(hdi = dest_hdi)

d4c <- d2 %>% 
  filter(!is.na(alpha3)) %>%
  select(alpha3, year, fb, pop, disp) %>%
  left_join(d3, by = c("alpha3" = "orig", "year" = "year")) %>%
  rename(hdi = orig_hdi) %>%
  drop_na() %>%
  group_by(hdi, year, ecowas) %>%
  summarise(fb = sum(fb), 
            pop = sum(pop),
            disp = sum(disp))

d4 <- d4a %>%
  left_join(d4b) %>%
  left_join(d4c) %>%
  mutate(imm_share = imm/pop,
         emi_share = emi/pop,
         fb_share = fb/pop, 
         disp_share = disp/pop)


# long form for ggplot
d5 <- d4 %>%
  pivot_longer(cols = -(1:3), names_to = "total", values_to = "stepladder") %>%
  mutate(share = str_detect(string = total, pattern = "share"), 
         stepladder = ifelse(share, yes = stepladder, no = stepladder/1e6)) %>%
  replace_na(list(stepladder = 0))

# save
write_csv(x = d5, path = "./data/hdi_ecowas_totals_2020.csv")

##
## hdi bilat
##
d6 <- d3 %>%
  #drop_na() %>%
  group_by(orig_hdi, dest_hdi, year, ecowas) %>%
  summarise(stock = sum(stepladder)/1e6) %>%
  rename(orig = orig_hdi, 
         dest = dest_hdi)

d7 <- d3 %>%
  #drop_na() %>%
  group_by(orig_hdi19, dest_hdi19, year, ecowas) %>%
  summarise(stock19 = sum(stepladder)/1e6) %>%
  rename(orig = orig_hdi19, 
         dest = dest_hdi19)

d8 <- left_join(d6, d7)

d9<- d8 %>%
  replace_na(list(stock19 = 0))

# save
write_csv(x = d8, path = "./data/ecowas_bilats_2020.csv")
