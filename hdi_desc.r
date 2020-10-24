library(here)
library(ggplot2)
library(tidyverse)

here()

hdi_five <- read.csv(here('HDI_five_year.csv'), stringsAsFactors = FALSE)
hdi_five_short <- hdi_five[-c(1)] 
hdi_five_short <- hdi_five_short[seq(1,ncol(hdi_five_short),2)]
view(hdi_five_short)


hdi_five_prepped <- hdi_five_short  %>%
  gather(key=Year, value = "category", c(2:8),na.rm=FALSE)
  
hdi_five_prepped$Year <- as.numeric(gsub("[cat_]", "", hdi_five_prepped$Year))
hdi_five_prepped$Country <- gsub("Korea (Democratic People's Rep. of)", "Korea (Democratic People's Rep of)", hdi_five_prepped$Country)
hdi_five_prepped$Country <- gsub('People s', "People's", hdi_five_prepped$Country)
hdi_five_prepped$Country <- gsub('Iran  Islamic Republic of', "Iran (Islamic Republic of)", hdi_five_prepped$Country)
hdi_five_prepped$Country <- gsub('Iran (Islamic Rep. of)', "Iran (Islamic Republic of)", hdi_five_prepped$Country)
hdi_five_prepped$Country <- gsub('Micronesia (Federated  States of)', "Micronesia (Fed. States of)", hdi_five_prepped$Country)
