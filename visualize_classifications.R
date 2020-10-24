##Provides basic script, imports colors and practice doc. ##Note: NOTHING HERE IS CURRENTLY IN THE CHAPTER
library(ggplot2)
library(tidyr)
library(dplyr)
library(ggpubr)
library(data.table)
library(gganimate)

###SCRATCH FILE###

#set colors
cols <- c("Very High" = "#253494", "High" = "#2c7fb8", "Medium" = "#41b6c4", "Low" = "#a1dab4")
colors2 <- c("#253494", "#2c7fb8")

#Plots emigration from origin by HDI classification
plot_orig <- function(data){
  data %>%
  group_by(Orig_HDI) %>%
  summarise(sum = sum(stock, na.rm = TRUE)) %>%
  ggplot(aes(x = reorder(Orig_HDI, sum), y=sum, fill=Orig_HDI)) +
  geom_bar(stat='identity', position = 'dodge') +
  scale_fill_manual(name="HDI Classification", values = rev(cols)) +
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

plot_orig(hdi_stocks_1995)
plot_orig(hdi_stocks_2000)
plot_orig(hdi_stocks_2005)
plot_orig(hdi_stocks_2010)
plot_orig(hdi_stocks_2015)
plot_orig(hdi_stocks_2019)

#plots immigration to destination by HDI classification
plot_dest <- function(data){
  data %>%
    group_by(Dest_HDI) %>%
    summarise(sum = sum(stock, na.rm = TRUE)) %>%
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
plot_dest(hdi_stocks_1995)
plot_dest(hdi_stocks_2000)
plot_dest(hdi_stocks_2005)
plot_dest(hdi_stocks_2010)
plot_dest(hdi_stocks_2015)
plot_dest(hdi_stocks_2019)

#Plots immigration and emigration on one diverging plot.
plot_diverge <- function(data){
  data %>%
    gather(key="emm_imm", value = "migrants_total", c(9,11),na.rm=TRUE)%>%
    group_by(emm_imm, migrants_total) %>%
    summarise(sum = sum(stock, na.rm = TRUE)) %>%
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
}


plot_diverge(hdi_stocks_1995_short)
plot_diverge(hdi_stocks_2000)
plot_diverge(hdi_stocks_2005)
plot_diverge(hdi_stocks_2010)
plot_diverge(hdi_stocks_2015)
plot_diverge(hdi_stocks_2019)

#Animated diverging bar plot
plot_diverge_animated <- function(data){
  data %>%
    gather(key="emm_imm", value = "migrants_total", c(7:8),na.rm=TRUE)%>%
    group_by(emm_imm, migrants_total, Year) %>%
    summarise(sum = sum(stock, na.rm = TRUE)) %>%
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
animation <- plot_diverge_animated(hdi_stocks_all)
anim_save("hdi_diverge_13082020.gif", animation = last_animation()) ###Set date of animation

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


#PRACTICE "FROM" CHART
plot_area_from <- function(data){
  data %>%
    group_by(Year, Dest_HDI) %>%
    summarise_at(vars(stock), list(emigrants=sum), na.rm = TRUE) %>%
    ggplot(aes(x = Year, y=emigrants, fill = Dest_HDI)) +
    geom_area() +
    #(limits = c(0, 3500000), breaks = seq(0,3500000, 1000000), labels = c("0", "1", "2", "3")) +
    scale_fill_manual(name="HDI Classification", values = rev(cols), na.value = "#a1dab4") +
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
plot_area_from(hdi_stock_fromLow)
plot_area_from(hdi_stock_fromMed)
plot_area_from(hdi_stock_fromHigh)
plot_area_from(hdi_stock_fromVHigh)

#Practice All Chart. NOTE: Change label for each separate HDI classification
plot_all <- function(dataTO, dataWITHIN, dataFROM, max_mig, seq_mig) {
  #to
  to_plot <- dataTO %>%
    group_by(Year, Orig_HDI) %>%
    summarise_at(vars(stock), list(emigrants=sum), na.rm = TRUE) %>%
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
    summarise_at(vars(stock), list(emigrants=sum), na.rm = TRUE) %>%
    spread(Year, emigrants)
    
  #within
  within_plot <- dataWITHIN %>%
    group_by(Year, Orig_HDI) %>%
    summarise_at(vars(stock), list(emigrants=sum), na.rm = TRUE) %>%
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
    summarise_at(vars(stock), list(emigrants=sum), na.rm = TRUE) %>%
    spread(Year, emigrants)
  
  #from
  from_plot <- dataFROM %>%
    group_by(Year, Dest_HDI) %>%
    summarise_at(vars(stock), list(emigrants=sum), na.rm = TRUE) %>%
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
    summarise_at(vars(stock), list(emigrants=sum), na.rm = TRUE) %>%
    spread(Year, emigrants)
  
  #wrap
  figure <- ggpubr::ggarrange(to_plot, within_plot, from_plot,
                      labels = c("To low HDI Countries", "Within low HDI Countries", "From low HDI Countries"), #THESE MUST BE CHANGED BY CLASSIFICATION
                      ncol = 3, nrow = 1, align = "hv")
  
  write.csv(to_table, "to_table_vhigh.csv")
  write.csv(within_table, "within_table_vhigh.csv")
  write.csv(from_table, "from_table_vhigh.csv")
  #write.csv(dataTO, 'To_Low.csv')
  #write.csv(dataWITHIN, "Within_Low.csv")
  #write.csv(dataFROM, "From_Low.csv")
  figure
}

plot_all(hdi_stock_toLow, hdi_stock_withinLow, hdi_stock_fromLow, 150000000, 50000000)
plot_all(hdi_stock_toMed, hdi_stock_withinMed, hdi_stock_fromMed, 150000000, 50000000)
plot_all(hdi_stock_toHigh,hdi_stock_withinHigh, hdi_stock_fromHigh, 150000000, 50000000)
plot_all(hdi_stock_toVHigh, hdi_stock_withinVHigh, hdi_stock_fromVHigh, 150000000, 50000000)
