##
## plot1_2020 bar charts and divergent bar charts
## plot2_2020 area charts
## plot3_2020 area charts for ECOWAS/Schengen
## tab1_2020 table of regional (not HDI) stepladder migrants
## tab2_2020 top20s stepladder (1995, 2020)

library(tidyverse)
library(lemon)
library(scales)
library(patchwork)

cc4 <- c("Very High" = "#253494", "High" = "#2c7fb8", "Medium" = "#41b6c4", "Low" = "#a1dab4")
cc2 <- c("Immigrants" = "#253494", "Emigrants" = "#2c7fb8")

d <- read_csv("./data/hdi_totals_2020.csv")
d$hdi <- factor(x = d$hdi, levels = names(cc4))

##
## origin and destination total plots 
##  
# change year and total (to imm or fb or emi or disp) in filter 
d %>%
  filter(year == 2020,
         total == "emi_share") %>%
  ggplot(mapping = aes(x = hdi, y = stepladder, fill = hdi)) +
  geom_col() +
  scale_fill_manual(values = cc4) +
  guides(fill = FALSE) +
  theme_bw()

##
## de haas plot
##

# using the bilateral data filtered for only HDI countries
# d %>%
#   filter(year == 2019,
#          total %in% c("imm_share", "emi_share")) %>%
#   mutate(y_lab = ifelse(total == "imm_share", yes = names(cc2[1]), no = names(cc2[2]))) %>%

# using bilateral data but without filtering for if in HDI
d %>%
  filter(year == 2020,
         total %in% c("imm_share", "emi_share")) %>%
  mutate(y_lab = ifelse(total == "imm_share", yes = names(cc2[1]), no = names(cc2[2]))) %>%
  ggplot(mapping = aes(x = fct_rev(hdi), y = stepladder, fill = y_lab)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = cc2, name = "") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(x = "Human Devemopment Index", y = "Percentage of population")
ggsave(filename = "./plot/fig6_dehaas_2020.tiff", width = 6, height = 4)
file.show("./plot/fig6_dehaas_2020.tiff")

# run above with imm and emi
# ggsave(filename = "./plot/fig6_dehaas_hdi_only.tiff", width = 6, height = 4)


##
## divergent plots
##
# d %>%
#   filter(year == 2019,
#          total %in% c("imm", "emi")) %>%
#   mutate(stepladder = ifelse(total == "imm", yes = stepladder, no = -stepladder),
#          y_lab = ifelse(total == "imm", yes = names(cc2[1]), no = names(cc2[2]))) %>%
d %>%
  filter(year == 2020,
         total %in% c("imm", "emi")) %>%
  mutate(stepladder = ifelse(total == "imm", yes = stepladder, no = -stepladder),
         y_lab = ifelse(total == "imm", yes = names(cc2[1]), no = names(cc2[2]))) %>%
  ggplot(mapping = aes(x = stepladder, y = fct_rev(hdi), fill = y_lab)) +
  geom_col() +
  scale_fill_manual(values = cc2, name = "") +
  scale_x_symmetric(labels = abs) +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(x = "Migrant stepladder (in millions)", 
       y = "Human Devemopment Index")
g1 <- last_plot()

##
## divergent plots (percentages)
##
# d %>%
#   filter(year == 2019,
#          total %in% c("imm_share", "emi_share")) %>%
#   mutate(stepladder = ifelse(total == "imm_share", yes = stepladder, no = -stepladder),
#          y_lab = ifelse(total == "imm_share", yes = names(cc2[1]), no = names(cc2[2]))) %>%
d %>%
  filter(year == 2020,
         total %in% c("imm_share", "emi_share")) %>%
  mutate(stepladder = ifelse(total == "imm_share", yes = stepladder, no = -stepladder),
         y_lab = ifelse(total == "imm_share", yes = names(cc2[1]), no = names(cc2[2]))) %>%
  ggplot(mapping = aes(x = stepladder, y = fct_rev(hdi), fill = y_lab)) +
  geom_col() +
  scale_fill_manual(values = cc2, name = "") +
  scale_x_symmetric(labels = function(x) percent(abs(x))) +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(x = "Migrant stepladder as percentage of population", 
       y = "Human Devemopment Index")

g2 <- last_plot()

g1 + (g2 + labs(y = "")) + 
  plot_layout(guides = "collect") & 
  theme(legend.position = 'bottom')

ggsave(filename = "./plot/fig5_div_bars_2020.tiff", width = 8, height = 5)
file.show("./plot/fig5_div_bars_2020.tiff")

# run above with imm and emi
# ggsave(filename = "./plot/fig5_div_bars_hdi_only.tiff", width = 8, height = 5)

