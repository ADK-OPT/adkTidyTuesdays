# 2026 Week 29 Data
# Near-Death Experiences (NDERF)
# https://github.com/rfordatascience/tidytuesday/blob/main/data/2026/2026-07-21/readme.md

# Data
nde_experiences <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-07-21/nde_experiences.csv')

library(tidyverse)


# Question 1
# What features most commonly co-occur in NDEs? 
#     Most commong is clinical, OBE, and ESP.
# Are out-of-body experiences correlated with ESP or unity?
#     No

nde_experiences %>% 
  filter(category == "NDE") %>% 
  pivot_longer(.,cols=11:18,
               names_to = "feature",
               values_to = "feat value") %>% 
  group_by(feature,`feat value`) %>% 
  filter(`feat value` == "TRUE") %>% 
  count() %>% 
  arrange(desc(n))
  

cor(x=nde_experiences$ai_obe,y=nde_experiences$ai_esp)
cor(x=nde_experiences$ai_obe,y=nde_experiences$ai_unity)

lm(data = nde_experiences,formula = ai_obe ~ ai_esp+ai_unity)



# Question 2
# Are distressing NDEs more common in certain demographics or time periods?
#   More common in F than M, most common in the U.S and in the 1998.

nde_experiences %>% 
  select(1:10,13) %>% 
  count(gender)

nde_experiences %>% 
  select(1:10,13) %>% 
  count(country) %>% 
  arrange(desc(n))


nde_experiences %>% 
  select(1:10,13) %>%
  mutate(year = str_sub(exp_date,1,4)) %>% 
  mutate(month = str_sub(exp_date,6,7)) %>%
  filter(ai_hellish == "TRUE" & year != "NA") %>% 
  ggplot(.,aes(x=year,y=ai_hellish))+
  geom_col()


# Question 3
# How has the rate of NDERF submissions changed over time (1999–2025)?
#  More submissions as time has gone on
nde_experiences %>% 
  mutate(year = str_sub(post_date,1,4)) %>% 
  mutate(month = str_sub(post_date,6,7)) %>% 
  group_by(year) %>% 
  count() %>% 
  view()

# Question 4
# Do deeper NDEs (higher Greyson scores) tend to have longer narratives?
#    Does not appear to a high correlation with higher scores having higher time
ggplot(nde_experiences,
       aes(x=narrative_length,y=greyson_score))+
  geom_point()+
  geom_hline(yintercept = 7,linetype = "dashed")
