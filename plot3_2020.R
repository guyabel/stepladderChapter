##
## plot1_2020 bar charts and divergent bar charts
## plot2_2020 area charts
## plot3_2020 area charts for ECOWAS/Schengen
## tab1_2020 table of regional (not HDI) stepladder migrants
## tab2_2020 top20s stepladder (1995, 2020)


library(tidyverse)
library(plotly)

###
## Schengen
###
d1 <- read_csv("./data/schengen_bilats_2020.csv")

very_high <- d1 %>% filter(orig == "Very High" & dest == "Very High")


g5 <- very_high %>%
  ggplot(aes(x = year, y = stock, fill = schengen)) +
  geom_area(stat="summary", fun=sum, alpha=0.4) +
  scale_fill_manual(values=c("#253494", "#890017", "#214900"))+
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(x = "Year", y = "Migrant stock (in millions)", 
       fill = "schengen?", title="Migration involving Very High HDI countries")
g5
ggsave(filename = "./plot/schengen.tiff", width = 9, height = 6)
file.show("./plot/schengen.tiff")

###
## ECOWAS
###
d1 <- read_csv("./data/ecowas_bilats_2020.csv")

Ecowas_only <- d1 %>% filter(ecowas != "1.not ecowas") 

g5 <- Ecowas_only %>%
  ggplot(aes(x = year, y = stock, fill = ecowas)) +
  geom_area(stat="summary", fun=sum, alpha=0.4) +
  scale_fill_manual(values=c("#253494", "#890017", "#214900"))+
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(x = "Year", y = "Migrant stock (in millions)", 
       fill = "Ecowas?", title="Migration involving Low HDI countries")
g5
ggsave(filename = "./plot/ecowas_HDI_2020.tiff", width = 9, height = 6)
file.show("./plot/ecowas_HDI_2020.tiff")

## Use the following for Low HDI...not included in manuscript as of 22 Feb since it
## emphasizes the reclassification of India more than migration between ECOWAS countries

#Low_hdi <- d1 %>% drop_na() %>% filter(orig == "Low" & dest == "Low")

#g6 <- Low_hdi %>%
#   ggplot(aes(x = year, y = stock, fill = ecowas)) +
#   geom_area(stat="summary", fun=sum, alpha=0.4) +
#   scale_fill_manual(values=c("#253494", "#890017", "#214900"))+
#   theme_bw() +
#   theme(legend.position = "bottom") +
#   labs(x = "Year", y = "Migrant stock (in millions)", 
#        fill = "Ecowas?", title="Migration involving Low HDI countries")
# g6
# ggsave(filename = "./plot/Low_HDI_2020.tiff", width = 9, height = 6)
# file.show("./plot/Low_HDI_2020.tiff")
