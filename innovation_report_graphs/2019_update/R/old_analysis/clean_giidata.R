gii17 <- read.csv("~/dropboxsydneyuni/innovation_report_graphs/complete_graphs/scores/scores_2017.csv")
gii18 <- read.csv("~/dropboxsydneyuni/innovation_report_graphs/complete_graphs/scores/scores_2018.csv")

library(tidyverse)
gii181 <-  gii17 %>% 
  select(-c(difference, usa, australia))

gii17 <- gii17 %>% 
  mutate(year = "2017")

gii181 <- gii181 %>% 
  mutate(year = "2018")

gii_18 <- read.csv("~/dropboxsydneyuni/innovation_report_graphs/2018/scores/data/gii.csv")

gii_18 <- gii_18 %>% 
  mutate(year = "2018") %>% 
  mutate(australia = gsub("-500", NA, australia),
         usa = gsub("-500", NA, usa),
         difference = as.numeric(australia) - as.numeric(usa)) %>% 
  filter(!is.na(australia)) %>% 
  filter(!is.na(usa)) %>% 
  left_join(gii17, by = c("indicators", "indicators")) %>% 
  select(-australia.y, -usa.y, -difference.y, -year.y) %>% 
  rename(australia = australia.x, usa = usa.x, difference = difference.x, year = year.x) %>% 
  mutate(category = zoo::na.locf(category)) %>% 
  mutate(australia = as.numeric(australia),
         usa = as.numeric(usa), 
         difference = as.numeric(difference))

gii <- gii_18 %>% right_join(gii17, by = c("indicators", "indicators"))
write.csv(gii, "gii.csv")
