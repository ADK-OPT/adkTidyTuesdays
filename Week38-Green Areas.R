library(tidyverse)

# Loading Data
urban <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-09-22/urban.csv')


# Which city has the most green area per capita in each year?
urban %>% distinct(year)  # Determine the different years in the dataset
    # 1  1990
    # 2  2000
    # 3  2010
    # 4  2020
    # 5  2025

urban  %>% 
  filter(!is.na(cityCode)) %>%  #Remove empty city names (Don't want Country)
  filter(!is.na(greenAreaPerCapitaM2)) %>%  #Remove NA values in column for greenspace
  group_by(year) %>% 
  summarise(max = max(greenAreaPerCapitaM2),across()) %>% #across keeps other columns
  ungroup() %>% 
  filter(year & max == greenAreaPerCapitaM2) %>%  
  pull(cityName,year)

# 1990: "Bunia"
# 2000: "Faqus (Faqous)"
# 2010: "Faqus (Faqous)"
# 2020: "Faqus (Faqous)"
# 2025: "Kozhikode (Calicut)"

## Checking
urban %>% 
  filter(year == 1990) %>% 
  filter(!is.na(cityCode)) %>% 
  filter(!is.na(greenAreaPerCapitaM2)) %>% 
  arrange(desc(greenAreaPerCapitaM2)) %>% 
  slice(1) %>% 
  pull(cityName)
    # "Bunia"

urban %>% 
  filter(year == 2000) %>% 
  filter(!is.na(cityCode)) %>% 
  filter(!is.na(greenAreaPerCapitaM2)) %>% 
  arrange(desc(greenAreaPerCapitaM2)) %>% 
  slice(1) %>% 
  pull(cityName)
    # "Faqus (Faqous)"

urban %>% 
  filter(year == 2010) %>% 
  filter(!is.na(cityCode)) %>% 
  filter(!is.na(greenAreaPerCapitaM2)) %>% 
  arrange(desc(greenAreaPerCapitaM2)) %>% 
  slice(1) %>% 
  pull(cityName)
    # "Faqus (Faqous)"

urban %>% 
  filter(year == 2020) %>% 
  filter(!is.na(cityCode)) %>% 
  filter(!is.na(greenAreaPerCapitaM2)) %>% 
  arrange(desc(greenAreaPerCapitaM2)) %>% 
  slice(1) %>% 
  pull(cityName)
    # "Faqus (Faqous)"

urban %>% 
  filter(year == 2025) %>% 
  filter(!is.na(cityCode)) %>% 
  filter(!is.na(greenAreaPerCapitaM2)) %>% 
  arrange(desc(greenAreaPerCapitaM2)) %>% 
  slice(1) %>% 
  pull(cityName)
    # "Kozhikode (Calicut)"
  # Names matched the check by year!

## Plotting
plot_most_green_cities <-urban  %>% 
  filter(!is.na(cityCode)) %>% 
  filter(!is.na(greenAreaPerCapitaM2)) %>% 
  group_by(year) %>% 
  summarise(max = max(greenAreaPerCapitaM2),across()) %>% 
  ungroup() %>% 
  filter(year & max == greenAreaPerCapitaM2) %>% 
  ggplot(., aes(x=factor(year),y=greenAreaPerCapitaM2,fill=cityName))+
  labs(x="Year",y="Green Area Per Capita",subtitle = "Squared Meters",fill="City Name",title = "Cities With Most Green Spaces Per Capita")+
  geom_col()+
  theme_bw()+
  theme(axis.ticks = element_blank(),panel.grid = element_blank())
ggsave("week38_GreenCities.png",plot = plot_most_green_cities,path = "plots/")

# Which city has lost the largest percentage of its green area since 1990?

urban %>% 
  filter(!is.na(greenAreaPerCapitaM2)) %>% 
  filter(!is.na(cityName)) %>% 
  group_by(cityName) %>% 
  mutate(max_year=max(year),
          min_year = min(year)) %>% 
  filter(year == max_year | year == min_year)  %>% 
  summarise(area_change = (sum(greenAreaPerCapitaM2[year==max_year]) - sum(greenAreaPerCapitaM2[year==min_year]))/max(greenAreaPerCapitaM2))


Lost_Green_Area_Cities <- urban %>% 
  filter(!is.na(averageShareOfGreenAreaInCityUrbanAreaPct)) %>% 
  filter(!is.na(cityName)) %>% 
  select(2, 3, 8, 9) %>% 
  group_by(cityName) %>% 
  filter(year == max(year) | year == min(year)) %>% 
  summarise(
    area_change = sum(averageShareOfGreenAreaInCityUrbanAreaPct[year == max(year)], na.rm = TRUE) -
                  sum(averageShareOfGreenAreaInCityUrbanAreaPct[year == min(year)], na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  arrange(area_change)  %>% 
  head(10)

library(gt)
library(gtExtras)

Lost_table <- Lost_Green_Area_Cities %>% 
  gt() %>% 
  fmt_number(columns = area_change, decimals = 2) %>% 
  tab_header(title = "Cities Percent Green Spaces Lost") %>% 
  cols_label(cityName = "City Name",
            area_change = "% Lost") %>% 
  tab_source_note(source_note = "Percent calculated by taking percent share of green space per capita of the most recent year from the oldest year") %>% 
  gt_highlight_rows(rows = 1)

gtsave(Lost_table,"week38_PercentGreenSpacesLost.png",path = "plots/")




  
# Which city has gained the largest percentage of green area since 1990?

Gain_Green_Area_Cities <- urban %>% 
  filter(!is.na(averageShareOfGreenAreaInCityUrbanAreaPct)) %>% 
  filter(!is.na(cityName)) %>% 
  select(2, 3, 8, 9) %>% 
  group_by(cityName) %>% 
  filter(year == max(year) | year == min(year)) %>% 
  summarise(
    area_change = sum(averageShareOfGreenAreaInCityUrbanAreaPct[year == max(year)], na.rm = TRUE) -
                  sum(averageShareOfGreenAreaInCityUrbanAreaPct[year == min(year)], na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  arrange(desc(area_change))  %>% 
  head(10)

Gain_table <- Gain_Green_Area_Cities %>% 
  gt() %>% 
  fmt_number(columns = area_change, decimals = 2) %>% 
  tab_header(title = "Cities Percent Green Spaces Gained") %>% 
  cols_label(cityName = "City Name",
            area_change = "% Gain") %>% 
  tab_source_note(source_note = "Percent calculated by taking percent share of green space per capita of the most recent year from the oldest year") %>% 
  gt_highlight_rows(rows = 1)

gtsave(Lost_table,"week38_PercentGreenSpaceGained.png",path = "plots/")