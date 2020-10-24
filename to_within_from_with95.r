library(ggplot2)
library(tidyr)
library(dplyr)
library(ggpubr)
library(data.table)
library(gganimate)
library(patchwork)

hdi_order <- c("Very High", "High", "Medium", "Low")

convert_to_95 <- function(dataset){
  dataset %>%
    group_by(Year, Dest_HDI_1995) %>%
    summarise_at(vars(flow), list(immigrants=sum), na.rm = TRUE)
}

convert_within_95 <- function(dataset){
  dataset %>%
    group_by(Year, Orig_HDI_1995) %>%
    summarise_at(vars(flow), list(emigrants=sum), na.rm = TRUE)
}

convert_from_95 <- function(dataset){
  dataset %>%
    group_by(Year, Orig_HDI_1995) %>%
    summarise_at(vars(flow), list(emigrants=sum), na.rm = TRUE)
}

hdi_flow_toLow_95_mod <- convert_to_95(hdi_flow_toLow_95)
hdi_flow_withinLow_95_mod <- convert_within_95(hdi_flow_withinLow_95)
hdi_flow_fromLow_95_mod <- convert_from_95(hdi_flow_fromLow_95)

hdi_flow_toMed_95_mod <- convert_to_95(hdi_flow_toMed_95)
hdi_flow_withinMed_95_mod <- convert_within_95(hdi_flow_withinMed_95)
hdi_flow_fromMed_95_mod <- convert_from_95(hdi_flow_fromMed_95)

hdi_flow_toHigh_95_mod <- convert_to_95(hdi_flow_toHigh_95)
hdi_flow_withinHigh_95_mod <- convert_within_95(hdi_flow_withinHigh_95)
hdi_flow_fromHigh_95_mod <- convert_from_95(hdi_flow_fromHigh_95)

hdi_flow_toVHigh_95_mod <- convert_to_95(hdi_flow_toVHigh_95)
hdi_flow_withinVHigh_95_mod <- convert_within_95(hdi_flow_withinVHigh_95)
hdi_flow_fromVHigh_95_mod <- convert_from_95(hdi_flow_fromVHigh_95)

hdi_flow_toVHigh_95_mod

plot_all <- function(dataTO, mod_to, dataWITHIN, mod_within, dataFROM, mod_from, max_mig, seq_mig) {
  to_plot <- dataTO %>%
    group_by(Year, Orig_HDI) %>%
    summarise_at(vars(flow), list(immigrants=sum), na.rm = TRUE) %>%
    mutate(series = 1) %>%
    merge(mod_to, by.x= "Year", by.y= "Year") %>%
    ggplot(aes(x = Year, y=immigrants.x, fill = factor(Orig_HDI, levels = hdi_order))) +
    geom_area() +
    geom_line(aes(x = Year, y=immigrants.y), linetype=4, size=2) +
    scale_y_continuous(limits = c(0, max_mig), breaks = seq(0, max_mig, seq_mig), labels = c("0", "50", "100", "150")) +
    scale_fill_manual(name="HDI Classification", values = rev(cols), na.value = "#a1dab4") +
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
    merge(mod_within, by.x= "Year", by.y= "Year") %>%
    ggplot(aes(x = Year, y=emigrants.x, fill = factor(Orig_HDI, levels = hdi_order))) +
    geom_area() +
    geom_line(aes(x = Year, y=emigrants.y), linetype=4, size=2) +
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
    merge(mod_from, by.x= "Year", by.y= "Year") %>%
    ggplot(aes(x = Year, y=emigrants.x, fill = factor(Dest_HDI, levels = hdi_order))) +
    geom_area() +
    geom_line(aes(x = Year, y=emigrants.y), linetype=4, size=2) +
    scale_y_continuous(limits = c(0, max_mig), breaks = seq(0, max_mig, seq_mig)) +
    scale_fill_manual(name="HDI Classification", values = cols, na.value = "#a1dab4") +
    scale_linetype(name="1995 classification held constant") +
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
                              labels = c("To Very High HDI Countries", "Within Very High HDI Countries", "From Very High HDI Countries"),
                              ncol = 3, nrow = 1, align = "hv")
  
  #write.csv(to_table, "to_table_low.csv")
  #write.csv(within_table, "within_table_low.csv")
  #write.csv(from_table, "from_table_low.csv")
  #write.csv(dataTO, 'To_Low.csv')
  #write.csv(dataWITHIN, "Within_Low.csv")
  #write.csv(dataFROM, "From_Low.csv")
  figure
}

plot_all(hdi_flow_toLow, hdi_flow_toLow_95_mod, hdi_flow_withinLow, hdi_flow_withinLow_95_mod, hdi_flow_fromLow, hdi_flow_fromLow_95_mod, 150000000, 50000000)
plot_all(hdi_flow_toMed, hdi_flow_toMed_95_mod, hdi_flow_withinMed, hdi_flow_withinMed_95_mod, hdi_flow_fromMed, hdi_flow_fromMed_95_mod, 150000000, 50000000)
plot_all(hdi_flow_toHigh,hdi_flow_toHigh_95_mod, hdi_flow_withinHigh, hdi_flow_withinHigh_95_mod, hdi_flow_fromHigh, hdi_flow_fromHigh_95_mod,150000000, 50000000)
plot_all(hdi_flow_toVHigh, hdi_flow_toVHigh_95_mod, hdi_flow_withinVHigh, hdi_flow_withinVHigh_95_mod, hdi_flow_fromVHigh, hdi_flow_fromVHigh_95_mod,150000000, 50000000)
