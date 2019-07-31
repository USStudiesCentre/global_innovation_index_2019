##################################################
## Project: Plot GII data
## Date: Mon Jul  1 10:25:40 2019
## Author: Zoe Meers
##################################################

library(tidyverse)

library(here)

# Read 2017 and 2018 GII data

gii_us <- read_csv("gii.csv") %>% 
    drop_na(category) %>% 
    mutate(usa = str_replace(usa, "-500", NA_character_),
           usa = as.numeric(as.character(usa)),
           year = as.integer(as.character(year))) %>% 
    drop_na(usa) %>% 
    filter(indicators != category)  %>% 
    select(year, indicators, category, usa)

gii_au <- read_csv("gii.csv") %>% 
    drop_na(category) %>% 
    mutate(australia = str_replace(australia, "-500", NA_character_),
           australia = as.numeric(as.character(australia)),
           year = as.integer(as.character(year))) %>% 
    drop_na(australia) %>% 
    filter(indicators != category) %>% 
    select(year, indicators, category, australia)

# Simulate data for 2019

## Grab distinct indicators and categories
distinct_indicators_au <- gii_au %>% 
    filter(year %in% c(2017, 2018)) %>% 
    distinct(indicators, category) 

distinct_indicators_us <- gii_us %>% 
    filter(year %in% c(2017, 2018)) %>% 
    distinct(indicators, category) 

## Australian simulation
gii_au_2019 <- distinct_indicators_au %>% 
    mutate(year = 2019,
           year = as.integer(as.character(year))) %>% 
    mutate(australia = sample(0:100, n(), replace = TRUE)) %>% 
    select(year, indicators, category, australia)

## US simulation
gii_us_2019 <- distinct_indicators_us %>% 
    mutate(year = 2019,
           year = as.integer(as.character(year))) %>% 
    mutate(usa = sample(0:100, n(), replace = TRUE)) %>% 
    select(year, indicators, category, usa)

# Merge back into main data frame

gii_us <- gii_us %>% 
    bind_rows(gii_us_2019) %>% 
    filter(!str_detect(indicators, " \\*\\*") &
               !str_detect(indicators, "Printing and publishing output"))

gii_au <- gii_au %>% 
    bind_rows(gii_au_2019) %>% 
    filter(!str_detect(indicators, " \\*\\*") & 
               !str_detect(indicators, "Printing and publishing output"))


# Plot data 

ggplot() + 
    geom_point(data =gii_us %>% filter(category == "Knowledge absorption"),
               aes(x = year,
                   y = usa,
                   group = indicators),
               color = ussc::ussc_colours("light blue"),
               alpha = 0.5,
               show.legend = T) +
    geom_line(data = gii_us %>% filter(category == "Knowledge absorption"),
              aes(x = year,
                  y = usa,
                  group = indicators),
              color = ussc::ussc_colours("light blue"),
              alpha = 0.5) +
    geom_point(data = gii_au %>% filter(category == "Knowledge absorption"),
               aes(x = year,
                   y = australia,
                   group = indicators),
               color = ussc::ussc_colours("red"),
               alpha = 0.5,
               show.legend = T) +
    geom_line(data = gii_au %>% filter(category == "Knowledge absorption"),
              aes(x = year,
                  y = australia,
                  group = indicators),
              color = ussc::ussc_colours("red"),
              alpha = 0.5) +
    scale_x_continuous(breaks = c(2017, 2018, 2019),
                       labels = c("'17", "'18", "'19"),
                       limits = c(2017, 2019)) +
    scale_y_continuous(breaks = c(0, 50, 100),
                       labels = c("0","50","100"),
                       limits = c(0,100)) +
    ussc::theme_ussc() + 
    labs(x = NULL,
         y = NULL,
         color = NULL,
         title = "Knowledge absorption") + 
    facet_wrap(~indicators, 
               scales = "free",
               labeller = labeller(indicators = label_wrap_gen(12)),
               ncol = 8
               )  + 
    coord_cartesian(clip = 'off') +
    theme(panel.grid.minor = element_blank(),
          legend.position = 'top')
    

ggsave(last_plot(), 
       file = "gii_plot_knowledge.png",                                 
       height = 7, 
       width =  10)


