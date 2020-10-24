###VISUALIZE POP###
library(ggplot2)
library(tidyr)
library(dplyr)
library(ggpubr)
library(data.table)
library(gganimate)

pop_global <- read.csv(here('desa_pop_totals.csv'), stringsAsFactors = FALSE)

pop_global_clean <- pop_global[-c(2:7)]
names(pop_global_clean)[1] <- "Country"
hdi_pop_agg <- merge(hdi_2018, pop_global_clean, by.x = "Country", by.y = "Country")
names(hdi_pop_agg)[4] <- "population"

hdi_pop_agg <- hdi_pop_agg %>%
    drop_na()%>%
    group_by(category)%>%
    summarise(sum_population_hdi = sum(population))%>%
    view()

dodge_bar_imm_em_perc_2019 <- hdi_flows_2019 %>%
  gather(key="emm_imm", value = "migrants_total", c(9,11),na.rm=TRUE)%>%
  group_by(emm_imm, migrants_total) %>%
  summarise(sum = sum(flow, na.rm = TRUE)) %>%
  mutate(migrants_total = factor(migrants_total, levels= c('Low','Medium', 'High',"Very High")))%>%
  mutate(emm_imm=factor(emm_imm, levels = c("Orig_HDI", "Dest_HDI"))) %>%
  merge(hdi_pop_agg, by.x = "migrants_total", by.y = "category")%>%
  mutate(imm_percent = ifelse(emm_imm == "Dest_HDI", sum/sum_population_hdi, (sum/(sum+sum_population_hdi)))) %>%
  ggplot(aes(x = migrants_total, y=imm_percent, fill=emm_imm)) +
  geom_bar(position="dodge", stat='identity') +
  #geom_text(aes(label=abs(round(total/1000000, digits = 0))), hjust=-0.1, color="black", size=5)+
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

hdi_flows_2019 %>%
  gather(key="emm_imm", value = "migrants_total", c(9,11),na.rm=TRUE)%>%
  group_by(emm_imm, migrants_total) %>%
  summarise(sum = sum(migrants_less_forced, na.rm = TRUE)) %>%
  mutate(migrants_total = factor(migrants_total, levels= c('Low','Medium', 'High',"Very High")))%>%
  mutate(emm_imm=factor(emm_imm, levels = c("Orig_HDI", "Dest_HDI"))) %>%
  view()
  #merge(hdi_pop_agg, by.x = "migrants_total", by.y = "category")%>%
  #write.csv("aggregated_perc_of_pop.csv")
