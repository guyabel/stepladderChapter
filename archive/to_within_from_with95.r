##Visualizes To-WITHIN-FROM figures with 1995 line held constant. **CURRENTLY IN CHAPTER**
library(ggplot2)
library(tidyr)
library(dplyr)
library(ggpubr)
library(data.table)
library(gganimate)
library(patchwork)


##TO-WITHIN-FROM CHARTS

##Subset by Directionality: "TO"
hdi_stock_toLow <- hdi_stocks_all %>% subset(Dest_HDI == "Low" & Orig_HDI != "Low")
hdi_stock_toMed <- hdi_stocks_all %>% subset(Dest_HDI == "Medium" & Orig_HDI != "Medium")
hdi_stock_toHigh <- hdi_stocks_all %>% subset(Dest_HDI == "High" & Orig_HDI != "High")
hdi_stock_toVHigh <- hdi_stocks_all %>% subset(Dest_HDI == "Very High"& Orig_HDI != "Very High")

##Subset by directionality: "Within"
hdi_stock_withinLow <- hdi_stocks_all %>% subset(Dest_HDI == "Low" & Orig_HDI == "Low")
hdi_stock_withinMed <- hdi_stocks_all %>% subset(Dest_HDI == "Medium" & Orig_HDI == "Medium")
hdi_stock_withinHigh <- hdi_stocks_all %>% subset(Dest_HDI == "High" & Orig_HDI == "High")
hdi_stock_withinVHigh <- hdi_stocks_all %>% subset(Dest_HDI == "Very High"& Orig_HDI == "Very High")

##Subset by directionality: "From"
hdi_stock_fromLow <- hdi_stocks_all %>% subset(Dest_HDI != "Low" & Orig_HDI == "Low")
hdi_stock_fromMed <- hdi_stocks_all %>% subset(Dest_HDI != "Medium" & Orig_HDI == "Medium")
hdi_stock_fromHigh <- hdi_stocks_all %>% subset(Dest_HDI != "High" & Orig_HDI == "High")
hdi_stock_fromVHigh <- hdi_stocks_all %>% subset(Dest_HDI != "Very High"& Orig_HDI == "Very High")

#Reorder Classifications
hdi_order <- c("Very High", "High", "Medium", "Low")

#Convert the sums of the 1995 stocks to lines
convert_to_95 <- function(dataset){
  dataset %>%
    group_by(Year, Dest_HDI_1995) %>%
    summarise_at(vars(stock), list(immigrants=sum), na.rm = TRUE)
}

convert_within_95 <- function(dataset){
  dataset %>%
    group_by(Year, Orig_HDI_1995) %>%
    summarise_at(vars(stock), list(emigrants=sum), na.rm = TRUE)
}

convert_from_95 <- function(dataset){
  dataset %>%
    group_by(Year, Orig_HDI_1995) %>%
    summarise_at(vars(stock), list(emigrants=sum), na.rm = TRUE)
}

#Create modified lines representing 1995 totals
hdi_stock_toLow_95_mod <- convert_to_95(hdi_stock_toLow_95)
hdi_stock_withinLow_95_mod <- convert_within_95(hdi_stock_withinLow_95)
hdi_stock_fromLow_95_mod <- convert_from_95(hdi_stock_fromLow_95)

hdi_stock_toMed_95_mod <- convert_to_95(hdi_stock_toMed_95)
hdi_stock_withinMed_95_mod <- convert_within_95(hdi_stock_withinMed_95)
hdi_stock_fromMed_95_mod <- convert_from_95(hdi_stock_fromMed_95)

hdi_stock_toHigh_95_mod <- convert_to_95(hdi_stock_toHigh_95)
hdi_stock_withinHigh_95_mod <- convert_within_95(hdi_stock_withinHigh_95)
hdi_stock_fromHigh_95_mod <- convert_from_95(hdi_stock_fromHigh_95)

hdi_stock_toVHigh_95_mod <- convert_to_95(hdi_stock_toVHigh_95)
hdi_stock_withinVHigh_95_mod <- convert_within_95(hdi_stock_withinVHigh_95)
hdi_stock_fromVHigh_95_mod <- convert_from_95(hdi_stock_fromVHigh_95)

##To-Within-From Figure with 1995 line. NOTE: Change label for each separate HDI classification
plot_all <- function(dataTO, mod_to, dataWITHIN, mod_within, dataFROM, mod_from, max_mig, seq_mig) {
  to_plot <- dataTO %>%
    #Set up calculation for deriving migration to countries with the argument's given HDI classification
    group_by(Year, Orig_HDI) %>%
    summarise_at(vars(stock), list(immigrants=sum), na.rm = TRUE) %>%
    #Merge in line representing 1995 classifications held constant
    merge(mod_to, by.x= "Year", by.y= "Year") %>%
    #Plot based on year, with number of immigrants, and fill based on order
    ggplot(aes(x = Year, y=immigrants.x, fill = factor(Orig_HDI, levels = hdi_order))) +
    geom_area() +
    geom_line(aes(x = Year, y=immigrants.y), linetype=4, size=2) +
    #Designate scale and color
    scale_y_continuous(limits = c(0, max_mig), breaks = seq(0, max_mig, seq_mig), labels = c("0", "50", "100", "150")) +
    scale_fill_manual(name="HDI Classification", values = rev(cols), na.value = "#a1dab4") +
    #Label and theme. Note: "TO" plot has y axis label
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
  
  #"To" table created for verification 
  to_table <- dataTO %>%
    group_by(Year, Orig_HDI) %>%
    summarise_at(vars(stock), list(emigrants=sum), na.rm = TRUE) %>%
    spread(Year, emigrants)
  
  #within
  within_plot <- dataWITHIN %>%
    #Set up calculation for deriving migration within countries with the argument's given HDI classification
    group_by(Year, Orig_HDI) %>%
    summarise_at(vars(stock), list(emigrants=sum), na.rm = TRUE) %>%
    #Merge in line representing 1995 classifications held constant
    merge(mod_within, by.x= "Year", by.y= "Year") %>%
    #Plot based on year, with number of emigrants, and fill based on order
    ggplot(aes(x = Year, y=emigrants.x, fill = factor(Orig_HDI, levels = hdi_order))) +
    geom_area() +
    geom_line(aes(x = Year, y=emigrants.y), linetype=4, size=2) +
    #Designate scale and color
    scale_y_continuous(limits = c(0, max_mig), breaks = seq(0, max_mig, seq_mig)) +
    scale_fill_manual(name="HDI Classification", values = cols, na.value = "#a1dab4") +
    #theme
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
  
  #"Within" table created for verification 
  within_table <- dataWITHIN %>%
    group_by(Year, Orig_HDI) %>%
    summarise_at(vars(stock), list(emigrants=sum), na.rm = TRUE) %>%
    spread(Year, emigrants)
  
  from_plot <- dataFROM %>%
    #Set up calculation for deriving migration from countries with the argument's given HDI classification
    group_by(Year, Dest_HDI) %>%
    summarise_at(vars(stock), list(emigrants=sum), na.rm = TRUE) %>%
    #Merge in line representing 1995 classifications held constant
    merge(mod_from, by.x= "Year", by.y= "Year") %>%
    #Plot based on year, with number of emigrants, and fill based on order
    ggplot(aes(x = Year, y=emigrants.x, fill = factor(Dest_HDI, levels = hdi_order))) +
    geom_area() +
    geom_line(aes(x = Year, y=emigrants.y), linetype=4, size=2) +
    #Designate scale and color
    scale_y_continuous(limits = c(0, max_mig), breaks = seq(0, max_mig, seq_mig)) +
    scale_fill_manual(name="HDI Classification", values = cols, na.value = "#a1dab4") +
    #Theme
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
  
  #"From" table created for verification 
  from_table <- dataFROM %>%
    group_by(Year, Dest_HDI) %>%
    summarise_at(vars(stock), list(emigrants=sum), na.rm = TRUE) %>%
    spread(Year, emigrants)
  
  #wrap into one figure. NOTE: Change labels to reflect HDI Classification
  figure <- ggpubr::ggarrange(to_plot, within_plot, from_plot,
                              labels = c("To Very High HDI Countries", "Within Very High HDI Countries", "From Very High HDI Countries"),
                              ncol = 3, nrow = 1, align = "hv")
  
  ##Option to write tables to csv
  
  #write.csv(to_table, "to_table_low.csv")
  #write.csv(within_table, "within_table_low.csv")
  #write.csv(from_table, "from_table_low.csv")
  #write.csv(dataTO, 'To_Low.csv')
  #write.csv(dataWITHIN, "Within_Low.csv")
  #write.csv(dataFROM, "From_Low.csv")
  figure
}

plot_all(hdi_stock_toLow, hdi_stock_toLow_95_mod, hdi_stock_withinLow, hdi_stock_withinLow_95_mod, hdi_stock_fromLow, hdi_stock_fromLow_95_mod, 150000000, 50000000)
plot_all(hdi_stock_toMed, hdi_stock_toMed_95_mod, hdi_stock_withinMed, hdi_stock_withinMed_95_mod, hdi_stock_fromMed, hdi_stock_fromMed_95_mod, 150000000, 50000000)
plot_all(hdi_stock_toHigh,hdi_stock_toHigh_95_mod, hdi_stock_withinHigh, hdi_stock_withinHigh_95_mod, hdi_stock_fromHigh, hdi_stock_fromHigh_95_mod,150000000, 50000000)
plot_all(hdi_stock_toVHigh, hdi_stock_toVHigh_95_mod, hdi_stock_withinVHigh, hdi_stock_withinVHigh_95_mod, hdi_stock_fromVHigh, hdi_stock_fromVHigh_95_mod,150000000, 50000000)
