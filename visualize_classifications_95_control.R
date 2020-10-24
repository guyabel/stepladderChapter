##Visualizes To-Within_From as if HDI Classifications in 1995 were held constant.
##Helps calculate HDI line in the current to_within_from charts in chapter.
library(ggplot2)
library(tidyr)
library(dplyr)
library(ggpubr)
library(data.table)
library(gganimate)


hdi_stocks_all_1995 <- rbind(hdi_stocks_1995, hdi_stocks_2000_95, hdi_stocks_2005_95, hdi_stocks_2010_95,
                       hdi_stocks_2015_95, hdi_stocks_2019_95)

##Subset by Directionality: "TO"
hdi_stock_toLow_95 <- hdi_stocks_all_1995 %>% subset(Dest_HDI_1995 == "Low" & Orig_HDI_1995 != "Low")
hdi_stock_toMed_95 <- hdi_stocks_all_1995 %>% subset(Dest_HDI_1995 == "Medium" & Orig_HDI_1995 != "Medium")
hdi_stock_toHigh_95 <- hdi_stocks_all_1995 %>% subset(Dest_HDI_1995 == "High" & Orig_HDI_1995 != "High")
hdi_stock_toVHigh_95 <- hdi_stocks_all_1995 %>% subset(Dest_HDI_1995 == "Very High"& Orig_HDI_1995 != "Very High")

##Subset by directionality: "Within"
hdi_stock_withinLow_95 <- hdi_stocks_all_1995 %>% subset(Dest_HDI_1995 == "Low" & Orig_HDI_1995 == "Low")
hdi_stock_withinMed_95 <- hdi_stocks_all_1995 %>% subset(Dest_HDI_1995 == "Medium" & Orig_HDI_1995 == "Medium")
hdi_stock_withinHigh_95 <- hdi_stocks_all_1995 %>% subset(Dest_HDI_1995 == "High" & Orig_HDI_1995 == "High")
hdi_stock_withinVHigh_95 <- hdi_stocks_all_1995 %>% subset(Dest_HDI_1995 == "Very High"& Orig_HDI_1995 == "Very High")

##Subset by directionality: "From"
hdi_stock_fromLow_95 <- hdi_stocks_all_1995 %>% subset(Dest_HDI_1995 != "Low" & Orig_HDI_1995 == "Low")
hdi_stock_fromMed_95 <- hdi_stocks_all_1995 %>% subset(Dest_HDI_1995 != "Medium" & Orig_HDI_1995 == "Medium")
hdi_stock_fromHigh_95 <- hdi_stocks_all_1995 %>% subset(Dest_HDI_1995 != "High" & Orig_HDI_1995 == "High")
hdi_stock_fromVHigh_95 <- hdi_stocks_all_1995 %>% subset(Dest_HDI_1995 != "Very High"& Orig_HDI_1995 == "Very High")

#Practice "From" Figure. NOTE: Change label for each separate HDI classification
plot_area_from <- function(data){
  data %>%
    group_by(Year, Dest_HDI_1995) %>%
    summarise_at(vars(stock), list(emigrants=sum), na.rm = TRUE) %>%
    ggplot(aes(x = Year, y=emigrants, fill = Dest_HDI_1995)) +
    geom_area() +
    #(limits = c(0, 3500000), breaks = seq(0,3500000, 1000000), labels = c("0", "1", "2", "3")) +
    #scale_fill_manual(name="HDI Classification", values = rev(cols), na.value = "#a1dab4") +
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
plot_area_from(hdi_stock_fromLow_95)
plot_area_from(hdi_stock_fromMed_95)
plot_area_from(hdi_stock_fromHigh_95)
plot_area_from(hdi_stock_fromVHigh_95)

#To-Within-From Figure. NOTE: Change label for each separate HDI classification
plot_all <- function(dataTO, dataWITHIN, dataFROM, max_mig, seq_mig) {
  #to
  to_plot <- dataTO %>%
    group_by(Year, Orig_HDI_1995) %>%
    summarise_at(vars(stock), list(immigrants=sum), na.rm = TRUE) %>%
    ggplot(aes(x = Year, y=immigrants, fill = Orig_HDI_1995)) +
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
    group_by(Year, Orig_HDI_1995) %>%
    summarise_at(vars(stock), list(emigrants=sum), na.rm = TRUE) %>%
    spread(Year, emigrants)
  
  #within
  within_plot <- dataWITHIN %>%
    group_by(Year, Orig_HDI_1995) %>%
    summarise_at(vars(stock), list(emigrants=sum), na.rm = TRUE) %>%
    ggplot(aes(x = Year, y=emigrants, fill = Orig_HDI_1995)) +
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
    group_by(Year, Orig_HDI_1995) %>%
    summarise_at(vars(stock), list(emigrants=sum), na.rm = TRUE) %>%
    spread(Year, emigrants)
  
  #from
  from_plot <- dataFROM %>%
    group_by(Year, Dest_HDI_1995) %>%
    summarise_at(vars(stock), list(emigrants=sum), na.rm = TRUE) %>%
    ggplot(aes(x = Year, y=emigrants, fill = Dest_HDI_1995)) +
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
    group_by(Year, Dest_HDI_1995) %>%
    summarise_at(vars(stock), list(emigrants=sum), na.rm = TRUE) %>%
    spread(Year, emigrants)
  #wrap
  figure <- ggpubr::ggarrange(to_plot, within_plot, from_plot,
                              labels = c("To low HDI Countries (1995)", "Within low HDI Countries (1995)", "From low HDI Countries (1995)"), #CHANGE LABEL HERE
                              ncol = 3, nrow = 1, align = "hv")
  
  #OPTION TO PRINT OUT TABLE#
  
  #write.csv(to_table, "to_table_low_1995.csv")
  #write.csv(within_table, "within_table_low_1995.csv")
  #write.csv(from_table, "from_table_low_1995.csv")
  #write.csv(dataTO, 'To_low_1995.csv')
  #write.csv(dataWITHIN, "Within_low_1995.csv")
  #write.csv(dataFROM, "From_low_1995.csv")
  figure
}


plot_all(hdi_stock_toLow_95, hdi_stock_withinLow_95, hdi_stock_fromLow_95, 150000000, 50000000)
plot_all(hdi_stock_toMed_95, hdi_stock_withinMed_95, hdi_stock_fromMed_95, 150000000, 50000000)
plot_all(hdi_stock_toHigh_95,hdi_stock_withinHigh_95, hdi_stock_fromHigh_95, 150000000, 50000000)
plot_all(hdi_stock_toVHigh_95, hdi_stock_withinVHigh_95, hdi_stock_fromVHigh_95, 150000000, 50000000)
