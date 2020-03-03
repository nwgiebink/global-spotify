# Add Sociopolitical variables
# 2020-3-2
# Noah Giebink and Sebastian Deimen


# Packages
library(tidyverse)

# Data
happy <- read_csv("data/happines2019.csv")
age <- read_csv('data/ages_kaggle.csv') 
urban <- read_csv('data/urban_population_percent_of_total.csv')
internet <- read_csv('data/internet_users.csv')
density <- read_csv('data/population_density_per_square_km.csv')
freedom <- read_csv('data/freedix_fh.csv')
gdp <- read_csv('data/gdp_total_yearly_growth.csv')

# select country and year for each sociopolitical variable
happy <- filter(happy, Year == 2018) %>% select(country = 'Country name', 
                                                happiness = 'Life Ladder')
age <- select(age, country = Year, median_age = '2015') %>%
  mutate(country = str_replace(age$country, 'United States of America', 'United States')) %>%
  mutate(country = str_replace(age$country, 'Russian Federation', 'Russia')) 
urban <- select(urban, country = country, percent_urban = '2017')
internet <- select(internet, country = country, percent_internet_users = '2016')
density <- select(density, country = country, density_sqkm = '2020')
freedom <- select(freedom, country = country, freedom = '2018') # IndexL: 1(most free) - 7(least free)
gdp <- select(gdp, country = country, gdp = '2013')

# inner_join each sociopolitical variable with spot_clean by country
# make list of new variable df's to iterate inner_join over
vars <- list(happy, age, urban, internet, density, freedom, gdp)
join_spot <- function(df){inner_join(spot_clean, df, by = 'country')}

# join all the variables
for (v in vars) {
  spot_clean <- join_spot(v)
  }

# write csv for spot_clean with sociopolitical variables
write_csv(spot_clean, 'data/spot_clean.csv')


