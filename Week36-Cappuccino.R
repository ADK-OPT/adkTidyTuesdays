library(tidyverse)

cafe <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-09-08/cafe.csv')
cappuccino_index <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-09-08/cappuccino_index.csv')

## Question 1 #####
# Some sample sizes are small. How uncertain is the ranking?



## Question 2 #####
# Which countries have the biggest variability in the price of a small cappuccino?
# Answer: USA,Denmark,China, Singapore, Swizterland, France, UK, Russia, Germany, Czechia
ggplot(cafe,aes(x=price_gbp,y=country))+
  geom_boxplot()

## Find the max for each group and Min for each group (group = Country)
cafe %>% 
  group_by(country) %>% 
  mutate(max_price = max(price_gbp,na.rm = TRUE)) %>% 
  mutate(min_price = min(price_gbp,na.rm = TRUE)) %>% 
  mutate(differnce = abs(max_price-min_price)) %>% 
  arrange(desc(differnce)) %>% 
  select(1,13) %>% 
  distinct()



# Question 3 ####
# Are there any outliers?


ggplot(data = cappuccino_index,
    mapping = aes(x=index)
)+
  geom_histogram()


library(rstatix)
identify_outliers(data = cappuccino_index,
variable = "index")

# Vietnam and India have an outlier index value, but have higher "n" Values.
# other outliers include Djibouti, Qatar, and Pakistan

lower_bound_index <- quantile(cappuccino_index$index, 0.025)
upper_bound_index <- quantile(cappuccino_index$index, 0.975)
outlier_ind <- which(cappuccino_index$index < lower_bound_index | cappuccino_index$index > upper_bound_index)


cappuccino_index %>% 
  mutate(z_index = scale(index)) %>% 
  ggplot(.,aes(x=z_index))+
  geom_histogram()