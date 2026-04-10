## ---------------------------
## Script name: pull_gridmet_snotel.R
## Purpose of script: Pull GridMET and SNOTEL data based on given coordinates (clean, standalone version that can be used for any site)
## Authors: MJ Farruggia 
## Modified by: Bella Oleksy
## Date Created: Nov 2024
## Last Updated: Jan 2026
##
## ---------------------------

# install.packages(c("snotelr", "tidyverse","geosphere"))
# 
# library(tidyverse)
# library(snotelr)
# library(geosphere)



# SNOTEL -----------------------------------------------------------------------------
#  load in snotel data and match lakes to nearest snotel  
# uses the package snotelr. See https://bluegreen-labs.github.io/snotelr/index.html 

#to use the shiny GUI for data exploration:
  # install.packages(c("DT","shinydashboard", "plotly", "leaflet"))
   # snotel_explorer()


data_path <- "~/Library/CloudStorage/Box-Box/Mountain Lakes Sensor Data Synthesis/Data/"
site_metadata <- read.csv(paste0(data_path,"Site_metadata.csv"))

#define sites
# site_locations <- data.frame(
#   site_name = c("wolf creek summit"), # lake names, in quotes like c("Lake_1", "Lake_2", "Lake_3")
#   lat_DD = c(37.48),   # latitude, decimal degrees
#   lon_DD = c(-106.80)  # longitude, decimal degrees
# )
# Match the format above
head(site_metadata)
site_locations <- site_metadata %>%
  select(Latitude, Longitude, Name) %>%
  rename(site_name=Name,
         lat_DD=Latitude,
         lon_DD=Longitude)
  # slice(1:2) # grab one for now for testing


#limit geographic scope from which we're pulling from (makes it run a little faster, otherwise it searches all of US)
#right now it icludes CO and WY; can change states as needed
#can also just use this df to view/scroll through all the snotel stations in the states you define here
snotel_sites <- snotel_info()[snotel_info()$state %in% c("CO","WY","UT","CA","WA"), ]


#Match lakes to nearest snotel site and pull out snotel site ID

calculate_distances <- function(lake_lat, lake_lon, sites_df) {
  distances <- distGeo(c(lake_lon, lake_lat), cbind(sites_df$lon, sites_df$lat))
  return(distances)
}

#can check the nearest_snotel df to make sure it's the station you want/expect
nearest_snotel <- site_locations %>%
  rowwise() %>%
  mutate({
    distances <- calculate_distances(lat_DD, lon_DD, snotel_sites)
    nearest_site_index <- which.min(distances)
    nearest_site = snotel_sites$site_name[nearest_site_index]
    distance_to_nearest_site = distances[nearest_site_index] / 1000
    tibble(nearest_site, distance_to_nearest_site)
  })

# Add site_id from snotel_sites 
nearest_snotel <- nearest_snotel %>%
  left_join(snotel_sites %>% select(site_name, site_id, state), by = c("nearest_site" = "site_name")) 

unique_snotel_site_ids <- unique(nearest_snotel$site_id)

#get snotel data for each of the unique snotel siteIDs from above
snotel_data_combined <- data.frame()

# Loop through each snotel site_id, download using snotel_download, combine into one df
for (site in unique_snotel_site_ids) {
  snotel_data <- snotel_download(site_id = site, network = "sntl", path = tempdir(), internal = TRUE)
  
  snotel_data <- snotel_data %>%
    mutate(date = as.Date(date)) %>%
    mutate(snotel_site_id = site) %>%  
    select(snotel_site_id, site_name,latitude, longitude, elev, date, 
           snow_water_equivalent, snow_depth, precipitation_cumulative, 
           temperature_max, temperature_min, temperature_mean, 
           precipitation)  
  
  snotel_data_combined <- bind_rows(snotel_data_combined, snotel_data)
}


#if having download issues, test outside of loop here to check things. could be 1 troublesome site or something that's breaking the loop
#snotel_data <- snotel_download(site_id = 1251, network = "sntl", path = tempdir(), internal = TRUE)


#calculate snotel phenology for each site in unique_snotel_site_ids
      #note: the function snotel_phenology is from the snotelr package. calculates snow phenology from SWE data

snow_phenology_df <- data.frame()

for (site in unique_snotel_site_ids) {
  snotel_data <- snotel_download(site_id = site, network = "sntl", path = tempdir(), internal = TRUE)
  phenology_results <- snotel_phenology(snotel_data, threshold = 0, offset = 210)
  phenology_results <- phenology_results %>%
    mutate(snotel_site_id = site)
  snow_phenology_df <- bind_rows(snow_phenology_df, phenology_results)
}

# Add the lake name (site_name) to this table
snow_phenology <- snow_phenology_df %>%
  left_join(
    nearest_snotel %>%
      select(site_name, site_id),
    by = c("snotel_site_id" = "site_id"),
    relationship = "many-to-many" # you might have multiple lakes with the same SNOTEL site 
  ) %>%
  mutate(waterYear=calcWaterYear(max_swe_date))

#final SNOTEL dfs: 
# - snotel_data_combined (does not have calculated values, just raw) 
# - snow_phenology_df (has calculated values)
# - nearest_snotel (metadata for which snotel sites belong to which lake)

write.csv(nearest_snotel, paste0(data_path,"snotel_nearest_site.csv"))
write.csv(snow_phenology, paste0(data_path,"snotel_phenology_calculated_metrics.csv"))
write.csv(snotel_data_combined, paste0(data_path,"snotel_data_raw.csv"))

