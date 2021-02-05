##
## plot1 bar charts and divergent bar charts
## plot2 area charts
## plot3 bar charts/divergent charts for ECOWAS/Schengen
## plot4 area charts for ECOWAS/Schengen

library(tidyverse)
library(plotly)

##Movement within ecowas
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


##Movement within ecowas
d1 <- read_csv("./data/ecowas_bilats_2020.csv")

Low_hdi <- d1 %>% drop_na() %>% filter(orig == "Low" & dest == "Low")
Ecowas_only <- d1 %>% filter(ecowas != "1.not ecowas") 

g5 <- Low_hdi %>%
  ggplot(aes(x = year, y = stock, fill = ecowas)) +
  geom_area(stat="summary", fun=sum, alpha=0.4) +
  scale_fill_manual(values=c("#253494", "#890017", "#214900"))+
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(x = "Year", y = "Migrant stock (in millions)", 
       fill = "Ecowas?", title="Migration involving ECOWAS countries")
g5
ggsave(filename = "./plot/ecowas_only_2020.tiff", width = 9, height = 6)
file.show("./plot/ecowas_only_2020.tiff")

