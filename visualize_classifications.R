library(ggplot2)
library(tidyr)
library(dplyr)
library(ggpubr)
library(data.table)
library(gganimate)

names(hdi_flows_1990)


cols <- c("Very High" = "#253494", "High" = "#2c7fb8", "Medium" = "#41b6c4", "Low" = "#a1dab4")
# re-order by variable Win
# the variables are re-orderd in the order of the win

plot_orig <- function(data){
  data %>%
  group_by(Orig_HDI) %>%
  summarise(sum = sum(flow, na.rm = TRUE)) %>%
  ggplot(aes(x = reorder(Orig_HDI, sum), y=sum, fill=Orig_HDI)) +
  geom_bar(stat='identity', position = 'dodge') +
  scale_fill_manual(name="HDI Classification", values = rev(colors4)) +
  labs(title="Average migrant stock among countries by HDI classification over time", y="Average migrant stock\n(in millions)")+
  theme(axis.text = element_text(face="bold", size=14), 
        panel.background = element_blank(),
        plot.title = element_text(face='bold', size=16, hjust=0.5),
        legend.title = element_blank(),
        legend.text = element_text(face = 'bold',  size = 14),
        legend.position = 'bottom',
        axis.title.x = element_text(face='bold',size=14, margin = margin(t = 20, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(face='bold',size=14, margin=margin(0,20,0,0)))  
}

plot_orig(hdi_flows_1995)
plot_orig(hdi_flows_2000)
plot_orig(hdi_flows_2005)
plot_orig(hdi_flows_2010)
plot_orig(hdi_flows_2015)
plot_orig(hdi_flows_2019)

plot_dest <- function(data){
  data %>%
    group_by(Dest_HDI) %>%
    summarise(sum = sum(flow, na.rm = TRUE)) %>%
    ggplot(aes(x = Dest_HDI, y=sum, fill=Dest_HDI)) +
    geom_bar(stat='identity', position = 'dodge') +
    #(limits = c(0, 3500000), breaks = seq(0,3500000, 1000000), labels = c("0", "1", "2", "3")) +
    scale_fill_manual(name="HDI Classification", values = rev(colors4)) +
    labs(title="Average migrant stock among countries by HDI classification over time", y="Average migrant stock\n(in millions)")+
    theme(axis.text = element_text(face="bold", size=14), 
          panel.background = element_blank(),
          plot.title = element_text(face='bold', size=16, hjust=0.5),
          legend.title = element_blank(),
          legend.text = element_text(face = 'bold',  size = 14),
          legend.position = 'bottom',
          axis.title.x = element_text(face='bold',size=14, margin = margin(t = 20, r = 0, b = 0, l = 0)),
          axis.title.y = element_text(face='bold',size=14, margin=margin(0,20,0,0)))  
}
plot_dest(hdi_flows_1995)
plot_dest(hdi_flows_2000)
plot_dest(hdi_flows_2005)
plot_dest(hdi_flows_2010)
plot_dest(hdi_flows_2015)
plot_dest(hdi_flows_2019)


plot_diverge <- function(data){
  data %>%
    gather(key="emm_imm", value = "migrants_total", c(9,11),na.rm=TRUE)%>%
    group_by(emm_imm, migrants_total) %>%
    summarise(sum = sum(flow, na.rm = TRUE)) %>%
    mutate(total = ifelse(emm_imm == "Dest_HDI",
                          sum, -1*sum), migrants_total = factor(migrants_total, levels= c('Low','Medium', 'High',"Very High")))%>%
    ggplot(aes(x = migrants_total, y=total, fill=emm_imm)) +
    geom_bar(stat='identity') +
    geom_text(aes(label=abs(round(total/1000000, digits = 0))), hjust=-0.1, color="black", size=5)+
    scale_y_continuous(limits = c(-125000000, 225000000), breaks = seq(-125000000, 225000000, 25000000), labels = c("125", "100", "75", "50", "25", "0", "25", "50", "75", "100", "125", "150", "175", "200", "225")) +
    scale_fill_manual(name="HDI Classification", values = rev(colors4), labels= c("Immigrant", "Emigrant")) +
    labs(title="Immigrant and Emigrant stocks by HDI classification, 2010", y="Migrant stock\n(in millions)", x="Classification")+
    theme(axis.text = element_text(face="bold", size=14), 
          panel.background = element_blank(),
          plot.title = element_text(face='bold', size=16, hjust=0.5),
          legend.title = element_blank(),
          legend.text = element_text(face = 'bold',  size = 14),
          legend.position = 'right',
          axis.title.x = element_text(face='bold',size=14, margin = margin(t = 20, r = 0, b = 0, l = 0)),
          axis.title.y = element_text(face='bold',size=14, margin=margin(0,20,0,0)))+
    coord_flip() 
    #transition_states(Year, wrap = FALSE) +
    #shadow_mark() +
    #enter_grow() +
    #enter_fade()
}
hdi_sums <- hdi_flows_all %>%
 gather(key="emm_imm", value = "migrants_total", c(7:8),na.rm=TRUE)%>%
  group_by(emm_imm, migrants_total, Year) %>%
  summarise(sum = sum(flow, na.rm = TRUE)) %>%
  mutate(total = ifelse(emm_imm == "Dest_HDI",
                        sum, -1*sum))
hdi_sums %>%
  group_by(Year) %>%
  sum(hdi_sums$sum)

 #levels = c("Very High", "High", "Medium", "Low")
plot_diverge(hdi_flows_1995)
plot_diverge(hdi_flows_2000)
plot_diverge(hdi_flows_2005)
plot_diverge(hdi_flows_2010)
plot_diverge(hdi_flows_2015)
plot_diverge(hdi_flows_2019)


plot_diverge_animated <- function(data){
  data %>%
    gather(key="emm_imm", value = "migrants_total", c(7:8),na.rm=TRUE)%>%
    group_by(emm_imm, migrants_total, Year) %>%
    summarise(sum = sum(flow, na.rm = TRUE)) %>%
    mutate(total = ifelse(emm_imm == "Dest_HDI",
                          sum, -1*sum), migrants_total = factor(migrants_total, levels= c('Low','Medium', 'High',"Very High")))%>%
    ggplot(aes(x = migrants_total, y=total, fill=emm_imm)) +
    geom_bar(stat='identity') +
    #geom_text(aes(label=abs(round(total/1000000, digits = 0))), hjust=-0.1, color="black", size=5)+
    scale_y_continuous(limits = c(-100000000, 220000000), breaks = seq(-100000000, 200000000, 50000000), labels = c("100", "50", "0", "50", "100", "150", "200")) +
    scale_fill_manual(name="HDI Classification", values = rev(colors4), labels= c("Immigrant", "Emigrant")) +
    labs(title="Immigrant and Emigrant stocks by HDI classification, {closest_state}", y="Migrant stock\n(in millions)", x="Classification")+
    theme(axis.text = element_text(face="bold", size=14), 
          panel.background = element_blank(),
          plot.title = element_text(face='bold', size=16, hjust=0.5),
          legend.title = element_blank(),
          legend.text = element_text(face = 'bold',  size = 14),
          legend.position = 'right',
          axis.title.x = element_text(face='bold',size=14, margin = margin(t = 20, r = 0, b = 0, l = 0)),
          axis.title.y = element_text(face='bold',size=14, margin=margin(0,20,0,0)))+
    coord_flip() +
    transition_states(Year, wrap = FALSE)
    
    #shadow_mark() +
    #enter_grow() +
    #enter_fade()
}


animation <- plot_diverge_animated(hdi_flows_all)
anim_save("hdi_diverge_13082020.gif", animation = last_animation())
hdi_flows_1995$Year <- 1995
hdi_flows_2000$Year <- 2000
hdi_flows_2005$Year <- 2005
hdi_flows_2010$Year <- 2010
hdi_flows_2015$Year <- 2015
hdi_flows_2019$Year <- 2019

hdi_flows_all <- rbind(hdi_flows_1995, hdi_flows_2000, hdi_flows_2005, hdi_flows_2010,
                       hdi_flows_2015, hdi_flows_2019)
head(hdi_flows_all)

hdi_flow_toLow <- hdi_flows_all %>% subset(Dest_HDI == "Low" & Orig_HDI != "Low")
hdi_flow_toMed <- hdi_flows_all %>% subset(Dest_HDI == "Medium" & Orig_HDI != "Medium")
hdi_flow_toHigh <- hdi_flows_all %>% subset(Dest_HDI == "High" & Orig_HDI != "High")
hdi_flow_toVHigh <- hdi_flows_all %>% subset(Dest_HDI == "Very High"& Orig_HDI != "Very High")

hdi_flow_withinLow <- hdi_flows_all %>% subset(Dest_HDI == "Low" & Orig_HDI == "Low")
hdi_flow_withinMed <- hdi_flows_all %>% subset(Dest_HDI == "Medium" & Orig_HDI == "Medium")
hdi_flow_withinHigh <- hdi_flows_all %>% subset(Dest_HDI == "High" & Orig_HDI == "High")
hdi_flow_withinVHigh <- hdi_flows_all %>% subset(Dest_HDI == "Very High"& Orig_HDI == "Very High")

hdi_flow_fromLow <- hdi_flows_all %>% subset(Dest_HDI != "Low" & Orig_HDI == "Low")
hdi_flow_fromMed <- hdi_flows_all %>% subset(Dest_HDI != "Medium" & Orig_HDI == "Medium")
hdi_flow_fromHigh <- hdi_flows_all %>% subset(Dest_HDI != "High" & Orig_HDI == "High")
hdi_flow_fromVHigh <- hdi_flows_all %>% subset(Dest_HDI != "Very High"& Orig_HDI == "Very High")

plot_area_from <- function(data){
  data %>%
    group_by(Year, Dest_HDI) %>%
    summarise_at(vars(flow), list(emigrants=sum), na.rm = TRUE) %>%
    ggplot(aes(x = Year, y=emigrants, fill = Dest_HDI)) +
    geom_area() +
    #(limits = c(0, 3500000), breaks = seq(0,3500000, 1000000), labels = c("0", "1", "2", "3")) +
    scale_fill_manual(name="HDI Classification", values = rev(colors4), na.value = "#a1dab4") +
    labs(title="Average migrant stock among countries by HDI classification over time", y="Average migrant stock\n(in millions)")+
    theme(axis.text = element_text(face="bold", size=14), 
          panel.background = element_blank(),
          plot.title = element_text(face='bold', size=16, hjust=0.5),
          legend.title = element_blank(),
          legend.text = element_text(face = 'bold',  size = 14),
          legend.position = 'bottom',
          axis.title.x = element_text(face='bold',size=14, margin = margin(t = 20, r = 0, b = 0, l = 0)),
          axis.title.y = element_text(face='bold',size=14, margin=margin(0,20,0,0)))
}
plot_area_from(hdi_flow_fromLow)
plot_area_from(hdi_flow_fromMed)
plot_area_from(hdi_flow_fromHigh)
plot_area_from(hdi_flow_fromVHigh)


plot_all <- function(dataTO, dataWITHIN, dataFROM, max_mig, seq_mig) {
  #to
  to_plot <- dataTO %>%
    group_by(Year, Orig_HDI) %>%
    summarise_at(vars(flow), list(emigrants=sum), na.rm = TRUE) %>%
    ggplot(aes(x = Year, y=emigrants, fill = Orig_HDI)) +
    geom_area() +
    scale_y_continuous(limits = c(0, max_mig), breaks = seq(0, max_mig, seq_mig), labels = c("0", "50", "100", "150")) +
    scale_fill_manual(name="HDI Classification", values = cols, na.value = "#a1dab4") +
    labs(title="", y="Migrant Stock \n(in millions)")+
    theme(axis.text = element_text(face="bold", size=14), 
          panel.background = element_blank(),
          panel.grid.major.y = element_line(colour = "grey80"),
          plot.title = element_text(face='bold', size=16, hjust=0.5),
          plot.margin = margin(0, 15, 0, 0),
          legend.title = element_blank(),
          legend.text = element_text(face = 'bold',  size = 14),
          legend.position = 'bottom',
          axis.title.x = element_blank(),
          axis.title.y = element_text(face='bold',size=14, vjust = 0.5))
  
  to_table <- dataTO %>%
    group_by(Year, Orig_HDI) %>%
    summarise_at(vars(flow), list(emigrants=sum), na.rm = TRUE) %>%
    spread(Year, emigrants)
    
  #within
  within_plot <- dataWITHIN %>%
    group_by(Year, Orig_HDI) %>%
    summarise_at(vars(flow), list(emigrants=sum), na.rm = TRUE) %>%
    ggplot(aes(x = Year, y=emigrants, fill = Orig_HDI)) +
    geom_area() +
    scale_y_continuous(limits = c(0, max_mig), breaks = seq(0, max_mig, seq_mig)) +
    scale_fill_manual(name="HDI Classification", values = cols, na.value = "#a1dab4") +
    theme(axis.text = element_text(face="bold", size=14), 
          panel.background = element_blank(),
          panel.grid.major.y = element_line(colour = "grey80"),
          plot.title = element_text(face='bold', size=16, hjust=0.5),
          plot.margin = margin(0, 15, 0, 0),
          legend.title = element_blank(),
          legend.text = element_text(face = 'bold',  size = 14),
          legend.position = 'bottom',
          axis.text.y = element_blank(),
          axis.title.x = element_text(face='bold',size=14, margin = margin(t = 20, r = 0, b = 0, l = 0)),
          axis.title.y = element_blank())
  
  within_table <- dataWITHIN %>%
    group_by(Year, Orig_HDI) %>%
    summarise_at(vars(flow), list(emigrants=sum), na.rm = TRUE) %>%
    spread(Year, emigrants)
  
  #from
  from_plot <- dataFROM %>%
    group_by(Year, Dest_HDI) %>%
    summarise_at(vars(flow), list(emigrants=sum), na.rm = TRUE) %>%
    ggplot(aes(x = Year, y=emigrants, fill = Dest_HDI)) +
    geom_area() +
    scale_y_continuous(limits = c(0, max_mig), breaks = seq(0, max_mig, seq_mig)) +
    scale_fill_manual(name="HDI Classification", values = cols, na.value = "#a1dab4") +
    theme(axis.text = element_text(face="bold", size=14), 
          panel.background = element_blank(),
          panel.grid.major.y = element_line(colour = "grey80"),
          plot.title = element_text(face='bold', size=16, hjust=0.5),
          plot.margin = margin(0, 15, 0, 0),
          legend.title = element_blank(),
          legend.text = element_text(face = 'bold',  size = 14),
          legend.position = 'bottom',
          axis.text.y = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank())
  
  from_table <- dataFROM %>%
    group_by(Year, Dest_HDI) %>%
    summarise_at(vars(flow), list(emigrants=sum), na.rm = TRUE) %>%
    spread(Year, emigrants)
  
  #wrap
  figure <- ggpubr::ggarrange(to_plot, within_plot, from_plot,
                      labels = c("To low HDI Countries", "Within low HDI Countries", "From low HDI Countries"),
                      ncol = 3, nrow = 1, align = "hv")
  
  write.csv(to_table, "to_table_vhigh.csv")
  write.csv(within_table, "within_table_vhigh.csv")
  write.csv(from_table, "from_table_vhigh.csv")
  #write.csv(dataTO, 'To_Low.csv')
  #write.csv(dataWITHIN, "Within_Low.csv")
  #write.csv(dataFROM, "From_Low.csv")
  figure
}

plot_all(hdi_flow_toLow, hdi_flow_withinLow, hdi_flow_fromLow, 150000000, 50000000)
plot_all(hdi_flow_toMed, hdi_flow_withinMed, hdi_flow_fromMed, 150000000, 50000000)
plot_all(hdi_flow_toHigh,hdi_flow_withinHigh, hdi_flow_fromHigh, 150000000, 50000000)
plot_all(hdi_flow_toVHigh, hdi_flow_withinVHigh, hdi_flow_fromVHigh, 150000000, 50000000)
