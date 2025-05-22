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
    lake = as.factor(lake)
  )


# 3. Include seasonal variables -------------------------------------------

clim_season <- clim %>% 
  #Add water year and seasonal variables
  mutate(
    w_year = if_else(
      year(date) >= 10, year(date) +1, year(date)
    ),
    #Add seasons
    season = as.factor(
      if_else(
        month(date) == 12 | month(date) == 1 | month(date) == 2, 'winter', 
        if_else(
          month(date) == 3 | month(date) == 4 | month(date) == 5, 'spring',
          if_else(
            month(date) == 6 | month(date) == 7 | month(date) == 8, 'summer',
            'fall'
          )
        )
      )
    )
  ) %>%
  #Get summaries of the lakes by water year and season 
  #for each variable
  group_by(lake, w_year, season) %>% 
  summarise(
    tmin_mean = mean(tmin_c, na.rm = T),
    tmax_mean = mean(tmax_c, na.rm = T),
    tmean_mean = mean(tmean_c, na.rm = T),
    pr_mean_mm = mean(pr_cm, na.rm = T)*10
  )

# 4. Calculate means and trends -------------------------------------------

clim_sum <- clim_season %>% 
  #Means and trends calculated per lake
  group_by(lake, season) %>% 
  summarise(
    #Calculate means per variable
    tmin_mean_s = mean(tmin_mean, na.rm = T),
    tmax_mean_s = mean(tmax_mean, na.rm = T),
    tmean_mean_s = mean(tmean_mean, na.rm = T),
    pr_mean_mm_s = mean(pr_mean_mm, na.rm = T),
    #Calculate trends (p.value and slope)
    tmin_pval = mannKen(tmin_mean)$p.value,
    tmin_trend = mannKen(tmin_mean)$sen.slope,
    tmax_pval = mannKen(tmax_mean)$p.value,
    tmax_trend = mannKen(tmax_mean)$sen.slope,
    tmean_pval = mannKen(tmean_mean)$p.value,
    tmean_trend = mannKen(tmean_mean)$sen.slope,
    pr_pval = mannKen(pr_mean_mm)$p.value,
    pr_trend = mannKen(pr_mean_mm)$sen.slope,
  )


# 5. Blend seasonal data with site metadata -------------------------------

mlsp <- read_csv(here('data/mlsp_site_metadata.csv')) %>% 
  mutate(
    lake = as.factor(lake)
  )

mlsp_clim <- mlsp %>% 
  full_join(
    clim_sum,
    relationship = 'many-to-many'
  )

#**Write cleaned and aggregated data to CSV----
#write_csv(mlsp_clim, here('data/mlsp_clim.csv'))

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
    data = mlsp_clim %>% filter(season == 'fall', pr_pval >= 0.5), 
    aes(x = long, y = lat, fill = pr_trend), 
    size = 4, 
    pch =21, 
    stroke = 0.5,
    alpha = 0.6
  )+
  geom_point(
    data = mlsp_clim %>% filter(season == 'fall', pr_pval <= 0.5), 
    aes(x = long, y = lat, fill = pr_trend), 
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

ggsave(
  here('output/pr_trend/fall_pr_trend.png'),
  dpi = 300,
  width = 6,
  height = 6,
  units = 'in'
)
