##Creates visual based on de Haas (2010) 
##found at http://heindehaas.blogspot.com/2020/02/why-development-will-not-stop-migration.html

library(ggplot2)
library(tidyr)
library(dplyr)
library(ggpubr)
library(data.table)
library(gganimate)

##Based on de Haas (2010) found at http://heindehaas.blogspot.com/2020/02/why-development-will-not-stop-migration.html

#Load in DESA data stripped of UN header
pop_global <- read.csv(here('desa_pop_totals.csv'), stringsAsFactors = FALSE)

#Drop unnecessary columns and rename columns
pop_global_clean <- pop_global[-c(2:7)]
names(pop_global_clean)[1] <- "Country"

#Merge population data with HDI and rename column
hdi_pop_agg <- merge(hdi_2018, pop_global_clean, by.x = "Country", by.y = "Country")
names(hdi_pop_agg)[4] <- "population"

#Clean the aggregated hdi/population data by dropping NAs and grouping population into categories
hdi_pop_agg <- hdi_pop_agg %>%
    drop_na()%>%
    group_by(category)%>%
    summarise(sum_population_hdi = sum(population))

#Bar chart using HDI_stocks data (From "merge_hdi_stock.r")
dodge_bar_imm_em_perc_2019 <- hdi_stocks_2019 %>%
  #Convert to long based on HDI Classification and Origin/Destination
  gather(key="emm_imm", value = "migrants_total", c(9,11),na.rm=TRUE)%>%
  group_by(emm_imm, migrants_total) %>%
  #Sum countries by Origin/Destination
  summarise(sum = sum(stock, na.rm = TRUE)) %>%
  #Separate into separate HDI classifications and origin/destination
  mutate(migrants_total = factor(migrants_total, levels= c('Low','Medium', 'High',"Very High")))%>%
  mutate(emm_imm=factor(emm_imm, levels = c("Orig_HDI", "Dest_HDI"))) %>%
  merge(hdi_pop_agg, by.x = "migrants_total", by.y = "category")%>%
  #Calculate percent based on whether immigration (pure proportion) or emigration (sum / population in country plus emigrant population)
  mutate(imm_percent = ifelse(emm_imm == "Dest_HDI", sum/sum_population_hdi, (sum/(sum+sum_population_hdi)))) %>%
  #Plot with color based on HDI classification and Origin/Destination
  ggplot(aes(x = migrants_total, y=imm_percent, fill=emm_imm)) +
  geom_bar(position="dodge", stat='identity') +
  #Scale, color, label, themes
  scale_y_continuous(limits = c(0, 0.16), breaks = seq(0, 0.16, 0.02), labels = c("0", "2", "4", "6", "8", "10", "12", "14", "16")) +
  scale_fill_manual(name="HDI Classification", values = colors2, labels= c("Emigrant", "Immigrant")) +
  labs(title="", y="Migrant stock as percent of population", x="HDI Classification")+
  theme(axis.text = element_text(face="bold", size=14), 
        panel.background = element_blank(),
        plot.title = element_text(face='bold', size=16, hjust=0.5),
        legend.title = element_blank(),
        legend.text = element_text(face = 'bold',  size = 14),
        legend.position = 'bottom',
        axis.title.x = element_text(face='bold',size=14, margin = margin(t = 20, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(face='bold',size=14, margin=margin(0,20,0,0)))
dodge_bar_imm_em_perc_2019  
