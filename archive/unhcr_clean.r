#Purpose: Loans and cleans UNHCR forced migrant data
library(here)
library(ggplot2)
library(tidyverse)

here()

#load dataset stripped of UN header material
unhcr <- read.csv(here('unhcr_refugees1.csv'), stringsAsFactors = FALSE)

#conduct calculation relevant for this study: total_concern = Mandate Refugees + Asylum Seekers
unhcr$total_concern <- unhcr$Refugees.under.UNHCRâ..s.mandate + unhcr$Asylum.seekers

#subset by year
unhcr_1995 <- unhcr %>% subset(Year == 1995)
unhcr_2000 <- unhcr %>% subset(Year == 2000)
unhcr_2005 <- unhcr %>% subset(Year == 2005)
unhcr_2010 <- unhcr %>% subset(Year == 2010)
unhcr_2015 <- unhcr %>% subset(Year == 2015)
unhcr_2019 <- unhcr %>% subset(Year == 2019)
