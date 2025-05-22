# The goal of this script is to clean up the Gridmet data from 
# Google Earth Engine (see script 01 for link)



# 1. Libraries ------------------------------------------------------------

library(tidyverse)
library(here)
library(janitor)


# 2. Import data ----------------------------------------------------------

clim_raw <- read_csv(here('data/mlsp_gridmet_1995_2025.csv'))


# 3. Clean data -----------------------------------------------------------

clim_clean <- clim_raw %>%
  #remove characters from column names
  clean_names() %>% 
  #separate system_index into the index number and date
  separate_wider_delim(
    system_index,
    delim = '_',
    names = c('index', 'date')
  ) %>% 
  #remove index (we'll use 'lake') and geo (not needed)
  #rename temp & precip to include units
  select(
    -c(index, geo),
    #precip is the daily sum in mm
    #I convert to cm below
    pr_cm = pr, 
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
    pr_cm = pr_cm / 10,
    tmin_c = tmin_c - 273.15,
    tmax_c = tmax_c - 273.15,
    tmean_c = (tmin_c+tmax_c)/2
  )


# 4. Write clean data to CSV ----------------------------------------------

write_csv(
  clim_clean,
  here('data/gridmet_clean.csv')
)




















































































