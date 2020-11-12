##
## plot1 bar charts and divergent bar charts
## plot2 area charts
##

library(tidyverse)
library(lemon)
library(scales)
library(patchwork)

cc4 <- c("Very High" = "#253494", "High" = "#2c7fb8", "Medium" = "#41b6c4", "Low" = "#a1dab4")
cc2 <- c("Immigrants" = "#253494", "Emigrants" = "#2c7fb8")

d <- read_csv("./data/hdi_totals.csv")
d$hdi <- factor(x = d$hdi, levels = names(cc4))

##
## origin and destination total plots 
##  
# change year and total (to imm or fb or emi or disp) in filter 
d %>%
  filter(year == 1995,
         total == "imm") %>%
  ggplot(mapping = aes(x = hdi, y = stock, fill = hdi)) +
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
  filter(year == 2019,
         total %in% c("fb_share", "disp_share")) %>%
  mutate(y_lab = ifelse(total == "fb_share", yes = names(cc2[1]), no = names(cc2[2]))) %>%
  ggplot(mapping = aes(x = fct_rev(hdi), y = stock, fill = y_lab)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = cc2, name = "") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(x = "Human Devemopment Index", y = "Percentage of population")
ggsave(filename = "./plot/fig6_dehaas.pdf", width = 6, height = 4)
file.show("./plot/fig6_dehaas.pdf")

# run above with imm and emi
# ggsave(filename = "./plot/fig6_dehaas_hdi_only.pdf", width = 6, height = 4)


##
## divergent plots
##
# d %>%
#   filter(year == 2019,
#          total %in% c("imm", "emi")) %>%
#   mutate(stock = ifelse(total == "imm", yes = stock, no = -stock),
#          y_lab = ifelse(total == "imm", yes = names(cc2[1]), no = names(cc2[2]))) %>%
d %>%
  filter(year == 2019,
         total %in% c("fb", "disp")) %>%
  mutate(stock = ifelse(total == "fb", yes = stock, no = -stock),
         y_lab = ifelse(total == "fb", yes = names(cc2[1]), no = names(cc2[2]))) %>%
  ggplot(mapping = aes(x = stock, y = fct_rev(hdi), fill = y_lab)) +
  geom_col() +
  scale_fill_manual(values = cc2, name = "") +
  scale_x_symmetric(labels = abs) +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(x = "Migrant stock (in millions)", 
       y = "Human Devemopment Index")
g1 <- last_plot()
  
##
## divergent plots (percentages)
##
# d %>%
#   filter(year == 2019,
#          total %in% c("imm_share", "emi_share")) %>%
#   mutate(stock = ifelse(total == "imm_share", yes = stock, no = -stock),
#          y_lab = ifelse(total == "imm_share", yes = names(cc2[1]), no = names(cc2[2]))) %>%
d %>%
  filter(year == 2019,
         total %in% c("fb_share", "disp_share")) %>%
  mutate(stock = ifelse(total == "fb_share", yes = stock, no = -stock),
         y_lab = ifelse(total == "fb_share", yes = names(cc2[1]), no = names(cc2[2]))) %>%
  ggplot(mapping = aes(x = stock, y = fct_rev(hdi), fill = y_lab)) +
  geom_col() +
  scale_fill_manual(values = cc2, name = "") +
  scale_x_symmetric(labels = function(x) percent(abs(x))) +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(x = "Migrant stock as percentage of population", 
       y = "Human Devemopment Index")

g2 <- last_plot()

g1 + (g2 + labs(y = "")) + 
  plot_layout(guides = "collect") & 
  theme(legend.position = 'bottom')

ggsave(filename = "./plot/fig5_div_bars.pdf", width = 8, height = 5)
file.show("./plot/fig5_div_bars.pdf")

# run above with imm and emi
# ggsave(filename = "./plot/fig5_div_bars_hdi_only.pdf", width = 8, height = 5)