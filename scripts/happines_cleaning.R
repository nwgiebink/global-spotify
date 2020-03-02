happy <- read.csv("data/happines2019.csv")

library(tidyverse)
glimpse(happy)

happy <- filter(happy, Year == 2018) %>% select(Country.name, Life.Ladder)

