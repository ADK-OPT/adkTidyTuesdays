### Health Metrics in Urban Centers Worldwide
# Loading Data ####
library(tidyverse)
health <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-09-29/health.csv')

# Which urban centre or country has the most number of hospitals 
# and/or pharmacies per capita in 2025?

Hos_pharm_per_capita <-health %>% 
  select(2,3,4,12,13)

### hospital per capita 2025

Hos_pharm_per_capita %>% 
  arrange(desc(HL_FPC_HOS_2025)) %>% 
  head(5)

### Pharmacy per Capita 2025
Hos_pharm_per_capita %>% 
  arrange(desc(HL_FPC_PHA_2025)) %>% 
  head(5)

# Do urban centres that belong to a higher income group have a higher density 
# of hospitals compared to those in a lower income group?

ggplot(health, aes(x=HL_POP_HOS_2025,y=GC_DEV_WIG_2025))+
  geom_boxplot()
### Medain hospital per capita by income group
health %>% 
  group_by(GC_DEV_WIG_2025) %>% 
  summarise(median = median(HL_FPC_HOS_2025,na.rm = TRUE)) %>% # medain because the scatter of points in boxplot
  arrange(desc(median))
#  GC_DEV_WIG_2025    median
#   <chr>               <dbl>
# 1 High income     0.0000475
# 2 Upper Middle    0.0000371
# 3 NA              0.0000314
# 4 Lower Middle    0.0000297
# 5 Low income      0.0000242

health %>% 
  group_by(GC_DEV_WIG_2025) %>% 
  summarise(median = median(HL_FPC_PHA_2025, na.rm = TRUE)) %>% 
  arrange(desc(median))

#   GC_DEV_WIG_2025     median
#   <chr>                <dbl>
# 1 High income     0.0000402 
# 2 NA              0.0000379 
# 3 Upper Middle    0.0000309 
# 4 Lower Middle    0.0000185 
# 5 Low income      0.00000688

# Finding: Higher income have more hospitals/Pharmacy per cpaita than lower incomes.



#Do cities with more hospitals also have more pharmacies, or are the two resources unrelated?

