##
## plot1_2020 bar charts and divergent bar charts
## plot2_2020 area charts
## plot3_2020 area charts for ECOWAS/Schengen
## tab1_2020 table of regional (not HDI) stepladder migrants
## tab2_2020 top20s stepladder (1995, 2020)

library(tidyverse)

d0 <- read_csv(file = "./data/hdi_country_bilat_2020.csv")
d1 <- read_csv(file = "./data/stock_totals_2020.csv")

d2a <- d0 %>%
  drop_na() %>%
  group_by(orig, year, orig_hdi) %>%
  summarise(emm = sum(stepladder)) %>%
  rename(alpha3 = 1)

d2b <- d0 %>%
  drop_na() %>%
  group_by(dest, year, dest_hdi) %>%
  summarise(imm = sum(stepladder)) %>%
  rename(alpha3 = 1)

d2 <- d2a %>%
  left_join(d2b)#%>%


d3 <- d1 %>% 
  filter(!is.na(alpha3)) %>%
  select(alpha3, year, fb, pop, disp)

top20 <- function(x = d3, y = d2, year, m){
  x %>%
    filter(year == {{year}}, 
           !is.na(alpha3)) %>%
    left_join(y) %>%
    mutate(imm_share = imm/pop, emm_share = emm/(emm+pop)) %>%
    arrange(desc(!!ensym(m))) %>%
    slice(1:22) #%>%
}

d3a <- top20(year = 1995, m = "emm")
d3b <- top20(year = 2020, m = "emm")
d3c <- top20(year = 1995, m = "imm")
d3d <- top20(year = 2020, m = "imm")

d4 <- bind_cols(d3a, d3b, d3c, d3d)
write_csv(x = d4, path = "./data/tab2_2020.csv")
