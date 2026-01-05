# The goal of this script is to 
#1. Import cleaned Gridmet data
#2. Calculate averages and MK trends and Sen's slope
#   for each lake
#3. Basic visuals for temp min, max, mean
#4. Basic visuals of temp min, max, mean trends
#5. Basic visuals of precip mean and trends



# 1. Libraries ------------------------------------------------------------

library(tidyverse)
library(here)
library(janitor)
library(wql)
library(sf)
library(sp)
library(rnaturalearth)
library(rnaturalearthdata)

# 2. Import data ----------------------------------------------------------

clim <- read_csv(here('data/gridmet_clean.csv')) %>% 
  #convert lake column to factor
  mutate(
    lake = as.factor(lake),
  ) %>% 
  arrange(
    lake
  )


# 3. Include seasonal variables -------------------------------------------

clim_season <- clim %>% 
  #Add water year and seasonal variables
  mutate(
    w_year = if_else(
      month(date) >= 10, year(date)+1 , year(date)
    ),
    #Add seasons
    season = as.factor(
      if_else(
        month(date) == 12 | month(date) == 1 | month(date) == 2, 
        'winter', 
        if_else(
          month(date) == 3 | month(date) == 4 | month(date) == 5, 
          'spring',
          if_else(
            month(date) == 6 | month(date) == 7 | month(date) == 8, 
            'summer',
            'fall'
          )
        )
      )
    )
  ) 


# 4. Annual summary stats and SD ------------------------------------------

clim_sum_annual <- clim_season %>% 
  #Get summaries of the lakes by water year and season 
  #for each variable
  group_by(lake, w_year) %>% 
  summarise(
    tmin_ann = min(tmin_c, na.rm = T),
    tmax_ann = max(tmax_c, na.rm = T),
    tmean_ann = mean(tmean_c, na.rm = T),
    annual_pr_mm = sum(pr_mm, na.rm = T),
    annual_snow_mm = sum(snow_mm, na.rm = T)
  ) %>% 
  group_by(lake) %>% 
  mutate(
    tmin_sd = sd(tmin_ann, na.rm = T),
    tmax_sd = sd(tmax_ann, na.rm = T),
    tmean_sd = sd(tmean_ann, na.rm = T),
    pr_mean_mm = mean(annual_pr_mm, na.rm = T),
    pr_sd = sd(annual_pr_mm, na.rm = T),
    snow_mean_mm = mean(annual_snow_mm, na.rm = T),
    snow_sd = sd(annual_snow_mm, na.rm = T)
  )
  

# 5. Seasonal summary stats and SD ----------------------------------------


clim_sum_seasonal <- clim_season %>% 
  #Get summaries of the lakes by water year and season 
  #for each variable
  group_by(lake, w_year, season) %>% 
  summarise(
    tmin_mean = mean(tmin_c, na.rm = T),
    tmax_mean = mean(tmax_c, na.rm = T),
    tmean_mean = mean(tmean_c, na.rm = T),
    cumsum_pr = sum(pr_mm, na.rm = T),
    pr_mean_mm = mean(cumsum_pr, na.rm = T),
    cumsum_snow = sum(snow_mm, na.rm = T),
    snow_mean_mm = mean(cumsum_snow, na.rm = T),
  ) %>% 
  group_by(lake, w_year) %>% 
  mutate(
    annual_pr_mm = sum(cumsum_pr),
    precip_frac = cumsum_pr/annual_pr_mm,
    annual_snow_mm = sum(cumsum_snow),
    snow_frac = cumsum_snow/annual_snow_mm
  ) %>% 
  group_by(lake, w_year, season) %>% 
  mutate(
    snow_frac_mean = mean(snow_frac, na.rm = T)
  )

# 4. Calculate means and trends -------------------------------------------

clim_trend <- clim_season %>% 
  #Means and trends calculated per lake
  group_by(lake, season) %>% 
  summarise(
    #Calculate means per variable
    tmin_mean_s = mean(tmin_mean, na.rm = T),
    tmax_mean_s = mean(tmax_mean, na.rm = T),
    tmean_mean_s = mean(tmean_mean, na.rm = T),
    pr_mean_mm_s = mean(cumsum_precip, na.rm = T),
    snow_mean_mm_s = mean(cumsum_snow, na.rm = T),
    #Calculate trends (p.value and slope)
    tmin_pval = mannKen(tmin_mean)$p.value,
    tmin_trend = mannKen(tmin_mean)$sen.slope,
    tmax_pval = mannKen(tmax_mean)$p.value,
    tmax_trend = mannKen(tmax_mean)$sen.slope,
    tmean_pval = mannKen(tmean_mean)$p.value,
    tmean_trend = mannKen(tmean_mean)$sen.slope,
    # pr_pval = mannKen(pr_mean_mm)$p.value,
    # pr_trend = mannKen(pr_mean_mm)$sen.slope,
    #Cumulative sum may be more useful than the average
    pr_sum_pval = mannKen(cumsum_precip)$p.value,
    pr_sum_trend = mannKen(cumsum_precip)$sen.slope,
    snow_sum_pval = mannKen(cumsum_snow)$p.value,
    snow_sum_trend = mannKen(cumsum_snow)$sen.slope,
  )


# 5. Blend seasonal data with site metadata -------------------------------

mlsp <- read_csv(here('data/mlsp_site_metadata.csv')) %>% 
  mutate(
    lake = as.factor(lake)
  )

mlsp_clim_annual <- mlsp %>% 
  full_join(
    clim_sum_annual,
    relationship = 'many-to-many'
  ) %>% 
  mutate(
    elev_band = if_else(
      elevation <= 1500, as.factor('low'),
      if_else(
        elevation > 1500 & elevation <=2500, as.factor('medium'), as.factor('high')
      )
    )
  )

mlsp_clim_seasonal <- mlsp %>% 
  full_join(
    clim_sum_seasonal,
    relationship = 'many-to-many'
  ) %>% 
  mutate(
    elev_band = if_else(
      elevation <= 1500, as.factor('low'),
      if_else(
        elevation > 1500 & elevation <=2500, as.factor('medium'), as.factor('high')
      )
    )
  )

#**Write cleaned and aggregated data to CSV----
# write_csv(mlsp_clim_annual, here('data/mlsp_clim_annual.csv'))
# write_csv(mlsp_clim_seasonal, here('data/mlsp_clim_seasonal.csv'))

# 6. Simple viz -----------------------------------------------------------

#Load map background

na <- rnaturalearth::ne_countries(
  scale = "medium", returnclass = "sf") %>%
  select(name, continent, geometry) %>%
  filter(continent == 'North America')

#defin midpoint
mid <- 0

#I think most important plots for now are mean temp. and precip.

#Mean temperature plot

ggplot() +
  ggplot2::geom_sf(data = na) +
  coord_sf(xlim = c(-126, -103), ylim = c(36, 50), expand = FALSE)+
  geom_point(
    data = mlsp_clim %>% filter(season == 'fall', pr_sum_pval >= 0.5), 
    aes(x = long, y = lat, fill = pr_sum_trend), 
    size = 4, 
    pch =21, 
    stroke = 0.5,
    alpha = 0.6
  )+
  geom_point(
    data = mlsp_clim %>% filter(season == 'fall', pr_sum_pval <= 0.5), 
    aes(x = long, y = lat, fill = pr_sum_trend), 
    size = 4, 
    pch =24, 
    stroke = 0.5,
    alpha = 0.6
  )+
  scale_fill_gradient2(midpoint = mid, low = 'red', mid = 'white', high = 'blue', space = 'Lab')+
  #scale_colour_gradient(low = 'blue', high = 'red', space = 'Lab')+
  xlab("Longitude")+
  ylab("Latitude")+
  #labs(fill = 'Ice On Trend \nSlope Magnitude \nDays/yr')
  labs(fill = expression(paste("Precip Trend \n(mm/yr)")))+
  theme_classic()+
  theme(
    legend.position = c(0.8, 0.7),
    legend.background = element_blank(),
    text = element_text(
      size = 15
    )
  )+
  guides(fill=guide_colorbar(ticks.colour = NA))

# ggsave(
#   here('output/pr_trend/fall_pr_trend.png'),
#   dpi = 300,
#   width = 6,
#   height = 6,
#   units = 'in'
# )


# Test function -----------------------------------------------------------


#Load map background


map_func <- function(
    df, 
    c_long, 
    c_lat, 
    pval, 
    trend, 
    sea_filt, 
    legend_title, 
    leg_pos
  ) {
  
  #Define map
  # na <- rnaturalearth::ne_countries(
  #   scale = "medium", returnclass = "sf") %>%
  #   select(name, continent, geometry) %>%
  #   filter(continent == 'North America')
  
  na <- rnaturalearth::ne_states(
    country = c('United States of America', 'Canada'), returnclass = "sf")
  
  #defin midpoint
  mid <- 0
  
  #Generic map for trend with chosen variable
  
  ggplot() +
    ggplot2::geom_sf(data = na) +
    coord_sf(xlim = c_long, ylim = c_lat, expand = FALSE)+
    geom_point(
      data = df %>% filter(season == {{sea_filt}}, {{pval}} >= 0.5), 
      aes(x = long, y = lat, fill = {{trend}}, shape =  {{elev}}), 
      size = 4, 
      pch =21, 
      stroke = 0.5,
      alpha = 0.6
    )+
    geom_point(
      data = df %>% filter(season == {{sea_filt}}, {{pval}} <= 0.5), 
      aes(x = long, y = lat, fill = {{trend}}, shape =  {{elev}}), 
      size = 4, 
      pch =24, 
      stroke = 0.5,
      alpha = 0.6
    )+
    scale_fill_gradient2(midpoint = mid, low = 'red', mid = 'white', high = 'blue', space = 'Lab')+
    #scale_colour_gradient(low = 'blue', high = 'red', space = 'Lab')+
    xlab("")+
    ylab("")+
    labs(fill = legend_title)+
    theme_classic()+
    theme(
      legend.position = leg_pos,
      legend.background = element_blank(),
      text = element_text(
        size = 15
      )
    )+
    guides(fill=guide_colorbar(ticks.colour = NA))
}

map_func(
  df = mlsp_clim, 
  #full map
  # c_long = c(-126, -103),
  # c_lat = c(36, 50),
  #PNW map
  # c_long = c(-126, -116.5),
  # c_lat = c(45.5, 49.5),
  #Sierra map
  # c_long = c(-125, -114),
  # c_lat = c(36, 42),
  # #Rockies
  c_long = c(-112, -104),
  c_lat = c(36, 42),
  pval = snow_sum_pval, 
  trend = snow_sum_trend, 
  sea_filt = 'winter', 
  legend_title = expression(paste("Snow \nmm yr"^"-1")),
  leg_pos = c(1.05, 0.5)
)
