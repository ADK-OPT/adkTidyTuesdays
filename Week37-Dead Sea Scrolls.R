dead_sea_scrolls <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-09-15/dead_sea_scrolls.csv')

library(tidyverse)

# Which biblical books were copied most frequently, and 
# what does that tell us about their relative importance to the Qumran community?

## Pslams is the most common with 34 and 30 for deuteronomy
dead_sea_scrolls  %>% 
  count(biblical_book) %>% 
  arrange(desc(n)) %>% 
  filter(!is.na(biblical_book)) %>% 
  ggplot(.,aes(x=reorder(biblical_book,-n),y=n))+
  geom_col()

# Were deuterocanonical books (Tobit, Sirach, Letter of Jeremiah) treated differently from 
# protocanonical books in terms of storage location, writing material, or scribal period?

