# The goal of this script is to clean up the Gridmet data from 
# Google Earth Engine (see script 01 for link)



# 1. Libraries ------------------------------------------------------------

library(tidyverse)
library(here)
library(janitor)


# 2. Import data ----------------------------------------------------------

# clim_raw <- read_csv(here('data/mlsp_gridmet_1995_2025.csv'))
clim_raw <- read_csv(here('data/mlsp_climate_data_1979_2025.csv'))


# 3. Clean data -----------------------------------------------------------

clim_clean <- clim_raw %>%
  #remove characters from column names
  clean_names() %>% 
  #separate system_index into the index number and date
  # separate_wider_delim(
  #   system_index,
  #   delim = '_',
  #   names = c('index', 'date')
  # ) %>% 
  #remove index (we'll use 'lake') and geo (not needed)
  #rename temp & precip to include units
  select(
    -c(system_index, geo),
    #precip is the daily sum in mm
    pr_mm = pr, 
    #Temp vars are in Kelvin
    #I convert to Celsius below.
    tmin_c = tmmn,
    tmax_c = tmmx
  ) %>% 
  #Make date a date class and lake as a factor
  #Convert temp vars from K to C
  mutate(
    date = ymd(date),
    lake = as.factor(lake),
    tmin_c = tmin_c - 273.15,
    tmax_c = tmax_c - 273.15,
    tmean_c = (tmin_c+tmax_c)/2,
    #Calculating snow as in Smits et al., 2021 (doi:10.1029/2021JG006277)
    snow_mm = if_else(
      tmean_c < 0, pr_mm, 
      if_else(
        tmean_c >= 0 & tmean_c <= 6, pr_mm - 0.1678*tmean_c, 0 
      )
    )
  )


# 4. Write clean data to CSV ----------------------------------------------

# write_csv(
#   clim_clean,
#   here('data/gridmet_clean_1979_2025.csv')
# )


# 5. Seasonal and cumulative data -----------------------------------------


# **5a. Add seasons and annual cumulative sums ----------------------------

clim_season <- clim_clean %>% 
  mutate(
    water_year = as.factor(
      if_else(
        month(date) >=10, year(date)+1, year(date)
      )
    ),
    season = as.factor(
      case_when(
        month(date) %in% c(12,1,2) ~ 'winter',
        month(date) %in% c(3,4,5) ~ 'spring',
        month(date) %in% c(6,7,8) ~ 'summer',
        month(date) %in% c(9,10,11) ~ 'autumn',
      )
    )
  ) %>% 
  group_by(lake, water_year) %>% 
  mutate(
    #Add the annual cumulative sum of precip and snow
    pr_cumsum_mm = sum(pr_mm),
    snow_cumsum_mm = sum(snow_mm)
  ) %>% 
  ungroup() %>% 
  select(
    lake, date, water_year, season, everything()
  )


# **5b. Get seasonal means ------------------------------------------------

clim_means <- clim_season %>% 
  group_by(lake, water_year, season) %>% 
  summarise(
    across(
      where(is.numeric),
      mean
    )
  )

# write_csv(
#   clim_means,
#   here('data/gridmet_means_1979_2025.csv')
# )




















































































