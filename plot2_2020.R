##
## plot1_2020 bar charts and divergent bar charts
## plot2_2020 area charts
## plot3_2020 area charts for ECOWAS/Schengen
## tab1_2020 table of regional (not HDI) stepladder migrants
## tab2_2020 top20s stepladder (1995, 2020)

library(tidyverse)
cc4 <- c("Very High" = "#253494", "High" = "#2c7fb8", "Medium" = "#41b6c4", "Low" = "#a1dab4")
d <- read_csv("./data/hdi_bilat_2020.csv")
#d$hdi <- factor(x = d$hdi, levels = names(cc4))

bilat_tot <- function(x = NULL, hdi = NULL){
  x %>%
    filter(orig == {hdi} | dest == {hdi}) %>%
    mutate(type = case_when(orig == dest ~ "Migration between",
                            orig == {hdi} ~ "Migration from",
                            dest == {hdi} ~ "Migration to"),
           type = paste(type, hdi, "HDI Countries"),
           type = factor(type),
           type = fct_shift(type, n = -1),
           region = case_when(orig == dest ~ orig, 
                              orig == {hdi} ~ dest, 
                              dest == {hdi} ~ orig)) %>%
    relocate(region)
}


##
## facet version
## 
d0 <- tibble(hdi = rev(names(cc4))) %>%
  mutate(dd = map(.x = hdi, .f = ~bilat_tot(x = d, hdi = .x)))

d0 %>%
  unnest(col = dd) %>%
  mutate(region = factor(region, levels = names(cc4))) %>%
  ggplot(mapping = aes(x = year, y = stock, fill = region)) +
  facet_wrap(facets = "type", nrow = 4, ncol = 3,
             labeller = label_wrap_gen(width = 30)) + 
  geom_area() +
  # add line based on HDI in 2019
  geom_area(mapping = aes(y = stock20), stat = 'summary', fun = sum,
            colour = "black", fill = "transparent", linetype = 2) +
  scale_fill_manual(values = cc4, 
                    guide = guide_legend(title.position = "top")) +
  theme_bw() +
  theme(legend.position = "bottom", panel.spacing.x = unit(1, "lines")) +
  labs(x = "Year", y = "Migrant stock (in millions)", 
       fill = "Human Development Index")

ggsave(filename = "./plot/fig7_area1_2020.tiff", width = 8, height = 12)
file.show("./plot/fig7_area1_2020.tiff")


##
## patchwork version
## 

area_plot <- function(x, hdi){
  hdi <- x %>%
    filter(orig == dest) %>%
    pull(region) %>%
    unique()
  
  ggplot(data = x, 
         mapping = aes(x = year, y = stock, fill = factor(region, levels = c("Very High", "High", "Medium", "Low")))) +
    facet_wrap(facets = "type", nrow = 4, ncol = 3,
               labeller = label_wrap_gen(width = 30)) + 
    geom_area() +
    geom_area(mapping = aes(y = stock20), stat = 'summary', fun = sum,
              colour = "black", fill = "transparent", linetype = 2, guide=guide_legend("top")) +
    scale_fill_manual(values = cc4, 
                      guide = guide_legend(title.position = "top")) +
    theme_bw() +
    theme(legend.position = "bottom", panel.spacing.x = unit(1, "lines")) +
    labs(x = "", y = "Migrant stock (in millions)", 
         fill = "Human Development Index", title = hdi)
}

g1 <- d %>%
  bilat_tot(hdi = "Low") %>%
  area_plot()

g2 <- d %>%
  bilat_tot(hdi = "Medium") %>%
  area_plot()

g3 <- d %>%
  bilat_tot(hdi = "High") %>%
  area_plot()

g4 <- d %>%
  bilat_tot(hdi = "Very High") %>%
  area_plot() +
  labs(x = "Year")

g1 / g2 / g3 / g4 +
  plot_layout(guides = "collect") & 
  theme(legend.position = 'bottom')

ggsave(filename = "./plot/fig7_area2_2020.tiff", width = 8, height = 12)
file.show("./plot/fig7_area2_2020.tiff")

# all the same y limits
g1 <- g1 + ylim(c(0, 140))
g2 <- g2 + ylim(c(0, 140))
g3 <- g3 + ylim(c(0, 140))
g4 <- g4 + ylim(c(0, 140))

g1 / g2 / g3 / g4 +
  plot_layout(guides = "collect") & 
  theme(legend.position = 'bottom')

ggsave(filename = "./plot/fig7_area3_2020.tiff", width = 8, height = 12)
file.show("./plot/fig7_area3_2020.tiff")

