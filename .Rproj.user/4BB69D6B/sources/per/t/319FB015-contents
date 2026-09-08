
# Week 30
# Ecotourism

library(tidyverse)

occurrences <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-07-28/occurrences.csv')
tourism <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-07-28/tourism.csv')
weather <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-07-28/weather.csv')


# Under which weather conditions are you most likely to observe a Gouldian finch?

occ_wea <-left_join(occurrences,weather)

# views by month to see a season they are more likely seen
ggplot(occ_wea,aes(x=month,y=organism_name))+
  geom_boxplot()

ggplot(occ_wea, aes(x=prcp,y=temp, color=organism_name))+
  geom_point()

ggplot(occ_wea, aes(x=rh,y=wind_speed,color=organism_name))+
  geom_point()

library(tidymodels)
rand_forest_data <- occ_wea %>% 
  select(12,18:26) %>% 
  mutate(organism_name = as.factor(organism_name)) %>% 
  drop_na()

data_split <- initial_split(rand_forest_data, prop = 0.80, strata = organism_name)
train_data <- training(data_split)
test_data  <- testing(data_split)

rf_spec <- rand_forest(trees = 500, min_n = 5) %>% 
  set_engine("ranger", importance = "permutation") %>% 
  set_mode("classification")

rf_workflow <- workflow() %>%
  add_formula(organism_name ~ temp + rh + prcp + wind_speed) %>%
  add_model(rf_spec)

library(modelenv)

rf_fit <- fit(rf_workflow, data = train_data)

# 1. Extract the raw importance scores from the trained model
raw_importance <- rf_fit %>%
  extract_fit_engine() %>%
  purrr::pluck("variable.importance") # Pulls out the named numeric vector

# 2. Convert the scores into a clean data frame for plotting
importance_df <- tibble(
  Variable = names(raw_importance),
  Importance = raw_importance
) %>%
  arrange(desc(Importance)) # Sort highest to lowest

# 3. Plot the data using standard ggplot2
ggplot(importance_df, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_col(fill = "midnightblue", width = 0.7) +
  coord_flip() + # Makes it a horizontal bar chart for easy reading
  theme_minimal() +
  labs(
    title = "Which Weather Conditions Matter Most for Finding the Gouldian Finch?",
    x = "Weather Variables",
    y = "Importance Score (Permutation)"
  )

# Temp and realative humidty are the most likly predictors for when you can see a finch. 
# From the plots earlier, warmer weather gives you that chance.

predictions <- augment(rf_fit, new_data = test_data)





# How does weather affect tourism numbers in each region?
# How do observations of the different animals relate to numbers of tourists?


