library(here)
library(ggplot2)
library(tidyverse)

here()

unhcr <- read.csv(here('unhcr_refugees1.csv'), stringsAsFactors = FALSE)

head(unhcr, 5)

unhcr$total_concern <- unhcr$Refugees.under.UNHCRâ..s.mandate + unhcr$Asylum.seekers

head(unhcr, 5)

unhcr_1995 <- unhcr %>% subset(Year == 1995)
unhcr_2000 <- unhcr %>% subset(Year == 2000)
unhcr_2005 <- unhcr %>% subset(Year == 2005)
unhcr_2010 <- unhcr %>% subset(Year == 2010)
unhcr_2015 <- unhcr %>% subset(Year == 2015)
unhcr_2019 <- unhcr %>% subset(Year == 2019)

head(unhcr_1995, 50)