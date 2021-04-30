library(tidyverse)

d0 <- read_csv(file = "./data/hdi.csv") %>%
  select(-name)
d1 <- read_csv(file = "./data/stock_totals.csv")

top20 <- function(x = d1, y = d0, year, m){
  x %>%
    filter(year == {{year}}, 
           !is.na(alpha3)) %>%
    arrange(desc(!!ensym(m))) %>%
    slice(1:20) %>%
    left_join(y) %>%
    select(name, m, paste0(m, "_share"), hdi_level_imp) %>%
    rename(" " = 2, 
           "share" = 3, 
           hdi = 4) %>%
    set_names(nm = paste(m, year, names(.), sep = "_")) %>%
    set_names(nm = str_remove(string = names(.), pattern = "_ $"))
}

d2a <- top20(year = 1990, m = "fb")
d2b <- top20(year = 2019, m = "fb")
d2c <- top20(year = 1990, m = "disp")
d2d <- top20(year = 2019, m = "disp")

d2 <- bind_cols(d2a, d2b, d2c, d2d)
write_csv(x = d2, path = "./table/tab2.csv")