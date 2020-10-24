##Diverge altogether##
library(ggplot2)
library(tidyr)
library(dplyr)
library(ggpubr)
library(data.table)
library(gganimate)

plot_diverge_all <- function(dataset, yearlabel){
  absolutes <- dataset %>%
    gather(key="emm_imm", value = "migrants_total", c(9,11),na.rm=TRUE)%>%
    group_by(emm_imm, migrants_total) %>%
    summarise(sum = sum(flow, na.rm = TRUE)) %>%
    mutate(total = ifelse(emm_imm == "Dest_HDI",
                          sum, -1*sum), migrants_total = factor(migrants_total, levels= c('Low','Medium', 'High',"Very High")))%>%
    mutate(emm_imm=factor(emm_imm, levels = c("Orig_HDI", "Dest_HDI"))) %>%
    ggplot(aes(x = migrants_total, y=total, fill=emm_imm)) +
    geom_bar(stat='identity') +
    geom_text(aes(label=abs(round(total/1000000, digits = 0))), hjust=-0.1, color="black", size=5)+
    scale_y_continuous(limits = c(-100000000, 200000000), breaks = seq(-100000000, 200000000, 25000000), labels = c("100", "75", "50", "25", "0", "25", "50", "75", "100", "125", "150", "175", "200")) +
    scale_fill_manual(name="HDI Classification", values = colors2, labels= c("Emigrant", "Immigrant")) +
    labs(title="", y="Migrant stock\n(in millions)", x="HDI Classification")+
    theme(axis.text = element_text(face="bold", size=14), 
          panel.background = element_blank(),
          plot.title = element_text(face='bold', size=16, hjust=0.5),
          legend.title = element_blank(),
          legend.text = element_text(face = 'bold',  size = 14),
          legend.position = 'bottom',
          axis.title.x = element_text(face='bold',size=14, margin = margin(t = 20, r = 0, b = 0, l = 0)),
          axis.title.y = element_text(face='bold',size=14, margin=margin(0,20,0,0)))+
    coord_flip()
  
  
  percents <- dataset %>%
    gather(key="emm_imm", value = "migrants_total", c(9,11),na.rm=TRUE)%>%
    group_by(emm_imm, migrants_total) %>%
    summarise(sum = sum(flow, na.rm = TRUE)) %>%
    mutate(migrants_total = factor(migrants_total, levels= c('Low','Medium', 'High',"Very High")))%>%
    mutate(emm_imm=factor(emm_imm, levels = c("Orig_HDI", "Dest_HDI"))) %>%
    merge(hdi_pop_agg, by.x = "migrants_total", by.y = "category")%>%
    mutate(imm_percent = ifelse(emm_imm == "Dest_HDI", sum/sum_population_hdi, -1*(sum/(sum+sum_population_hdi)))) %>%
    ggplot(aes(x = migrants_total, y=imm_percent, fill=emm_imm)) +
    geom_bar(stat='identity') +
    geom_text(aes(label=abs(round(imm_percent*100, digits = 1))), hjust=-0.1, color="black", size=5)+
    scale_y_continuous(limits = c(-0.06, 0.14), breaks = seq(-0.06, 0.14, 0.02), labels = c("6", "4", "2", "0", "2", "4", "6", "8", "10", "12", "14")) +
    scale_fill_manual(name="", values = alpha(colors2, 0.75), labels= c("Emigrant", "Immigrant")) +
    labs(title="", y="Migrant stock as percent of population\n(%)", x="")+
    theme(axis.text = element_text(face="bold", size=14), 
          panel.background = element_blank(),
          plot.title = element_text(face='bold', size=16, hjust=0.5),
          legend.title = element_blank(),
          legend.text = element_text(face = 'bold',  size = 14),
          legend.position = 'bottom',
          axis.title.x = element_text(face='bold',size=14, margin = margin(t = 20, r = 0, b = 0, l = 0)),
          axis.title.y = element_text(face='bold',size=14, margin=margin(0,20,0,0)))+
    coord_flip()
  
  figure <- ggpubr::ggarrange(absolutes, percents, labels = yearlabel,
                              ncol = 2, nrow = 1, align = "hv")
  figure
}
plot_diverge_all(hdi_flows_1995, "1995")
plot_diverge_all(hdi_flows_2000, "2000")
plot_diverge_all(hdi_flows_2005, "2005")
plot_diverge_all(hdi_flows_2010, "2010")
plot_diverge_all(hdi_flows_2015, "2015")
plot_diverge_all(hdi_flows_2019, "2019")

hdi_flows_2019 %>%
  gather(key="emm_imm", value = "migrants_total", c(9,11),na.rm=TRUE)%>%
  group_by(emm_imm, migrants_total) %>%
  summarise(sum = sum(flow, na.rm = TRUE)) %>% 
  mutate(total = ifelse(emm_imm == "Dest_HDI",
                        sum, -1*sum), migrants_total = factor(migrants_total, levels= c('Low','Medium', 'High',"Very High"))) %>%
  View()

'
'