world_castles <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-09-01/world_castles.csv')



library(tidyverse)

# Which countries have the most castles?

world_castles %>% 
  filter(category == "castle") %>% 
  count(country) %>% 
  arrange(desc(n)) %>% 
  rename("Number_of_Castles" = n)

library(gt)
library(gtExtras)
world_castles %>% 
  filter(category == "castle") %>% 
  count(country) %>% 
  arrange(desc(n)) %>% 
  rename("Number_of_Castles" = n) %>% 
  gt() %>% 
  cols_label(
    country = "Country",
    Number_of_Castles = "# of Castles"
  ) %>% 
  tab_header(
    title = "Number of Castles In Each Country",
    subtitle = "Tidytuesday Week 35!"
  ) %>% 
  gt_color_rows(columns = Number_of_Castles,direction = 1,palette = c("#CC79A7", "#009E73"))





# A tibble: 87 x 2
# country        Number_of_Castles
# <chr>                      <int>
#   1 Japan                        169
# 2 Spain                        166
# 3 Germany                      164
# 4 Italy                        147
# 5 France                       145
# 6 Poland                       124
# 7 Switzerland                   99
# 8 Czech Republic                93
# 9 Iran                          91
# 10 Portugal                      90


# Are palaces newer than fortresses?


world_castles %>% 
  mutate(age = 2026-year) %>% 
  group_by(category) %>% 
  mutate(group_avg = mean(year,na.rm = TRUE)) %>% 
  mutate(group_med = median(year, na.rm = TRUE))

world_castles %>% 
  mutate(age = 2026- year) %>% 
  group_by(category) %>% 
  summarise(group_avg = mean(year,na.rm = TRUE),
            group_med = median(year, na.rm = TRUE))

# category group_avg group_med
# <chr>        <dbl>     <dbl>
# 1 castle       1357.     1350 
# 2 fortress     1587.     1667 
# 3 palace       1650.     1716 
# 4 ruin         1268.     1294.

#Palaces have an older average and median age than fortresses.
#Charts below show the context. within the dataset fortresses and palaces have the same number of occurances.
#Fortresses have a larger spread show in the ridgeplot and boxplots below.

library(forcats)
my_colors = c("fortress" = "coral","palace" = "blue2","castle"="azure4","ruin"="darkgoldenrod3")

count_bar_chart <- world_castles %>% 
  mutate(age = 2026-year) %>% 
  ggplot(., aes(x=fct_infreq(category),fill = category))+
  geom_bar(show.legend = FALSE)+
  scale_fill_manual(values = my_colors)+
  geom_text(stat= 'count',aes(label = after_stat(count)),vjust=1)+
  xlab("")+
  theme_bw()

basic_boxplot <- world_castles %>% 
  mutate(age = 2026-year) %>% 
  mutate(category2= fct_reorder(category,age,.fun = 'median')) %>% 
  ggplot(.,aes(x=reorder(category2,age),y=age,fill=category2))+
  geom_boxplot(show.legend = FALSE)+
  scale_fill_manual(values = my_colors)+
  scale_x_discrete(guide = guide_axis(angle = 45))+
  xlab("")+
  theme_bw()

boxplot_jitter <- world_castles %>% 
  mutate(age = 2026-year) %>% 
  mutate(category2= fct_reorder(category,age,.fun = 'median')) %>% 
  ggplot(.,aes(x=reorder(category2,age),y=age,fill=category2))+
  geom_boxplot(show.legend = FALSE)+
  scale_x_discrete(guide = guide_axis(angle = 45))+
  xlab("")+
  ylab("")+
  scale_fill_manual(values = my_colors)+
  geom_jitter(color = "black",size=0.2,alpha =0.9,show.legend = FALSE)+
  theme_bw()

violin_boxplot <- world_castles %>% 
  mutate(age = 2026-year) %>% 
  mutate(category2= fct_reorder(category,age,.fun = 'median')) %>% 
  ggplot(.,aes(x=reorder(category2,age),y=age,fill = category2))+
  scale_x_discrete(guide = guide_axis(angle = 45))+
  xlab("")+
  ylab("")+
  scale_fill_manual(values = my_colors)+
  geom_violin(width = 1.4, show.legend = FALSE)+
  geom_boxplot(width=0.1, color="darkolivegreen", alpha=0.2,show.legend = FALSE)+
  theme_bw()



library(ggridges)
ridges_cat <- world_castles %>% 
  mutate(age = 2026-year) %>% 
  ggplot(.,aes(y=category,x=age,fill = category))+
  geom_density_ridges(stat = "binline",show.legend = FALSE)+
  xlab("")+
  ylab("")+
  scale_fill_manual(values = my_colors)+
  theme_ridges()

world_castles %>% 
  mutate(age = 2026-year) %>% 
  filter(category %in% c("palace", "fortress")) %>% 
  ggplot(.,aes(y=category,x=age,fill = category))+
  geom_density_ridges2()+
  theme_ridges()

library(patchwork)

count_bar_chart / ridges_cat / (basic_boxplot | boxplot_jitter | violin_boxplot )+ plot_layout(heights = c(1,1, 2))
  


# Which landmarks have articles in many languages but few readers?

### sitelinks = languges
### Pageviews = readers


world_castles %>% 
  summary()

ggplot(world_castles, aes(x=pageviews))+
  geom_boxplot()
ggplot(world_castles, aes(x=sitelinks))+
  geom_boxplot()

avg_pageviews <- median(world_castles$pageviews, na.rm = TRUE)  #in summary, mean is way higher due to outliars
avg_sitelinks <- median(world_castles$sitelinks, na.rm = TRUE)

ratio <- avg_sitelinks/avg_pageviews

# if sitelinks is  > (greater than) avg, lots of sites
# if pageviews are less than average, then few readers

world_castles %>% 
  mutate(med_pageviews = median(world_castles$pageviews, na.rm = TRUE),
         med_sitelinks =  median(world_castles$sitelinks, na.rm = TRUE)) %>%
  mutate(site_check = case_when(sitelinks > avg_sitelinks ~ "high", .default = "lower")) %>% 
  mutate(page_check = case_when(pageviews < avg_pageviews ~ "lower", .default = "high")) %>% 
  filter(site_check == "high" & page_check == "lower") %>% 
  mutate(ratio = sitelinks/pageviews,
         ratio = if_else(is.infinite(ratio),0, ratio)) %>% 
  filter(ratio < quantile(ratio, probs = 0.25)  & ratio > 0 ) %>% #question said FEW readers, not zero.
  leaflet(data=.,) %>% 
  addTiles() %>% 
  addCircleMarkers(~lon, ~lat , popup = ~as.character(name))



#####
# Side Quests

world_castles %>% 
  slice(1:10) %>% 
  select(name, country, image, category) %>% 
  gt() %>% 
  gt_img_rows(columns = image, height = 40)


library(leaflet)
world_castles %>% 
  leaflet(data=.,) %>% 
  addTiles() %>% 
  addCircleMarkers(~lon, ~lat , popup = ~as.character(name))
  


world_castles %>% 
  group_by(country)
 