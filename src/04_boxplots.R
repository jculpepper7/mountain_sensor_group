
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

annual <- read_csv(here('data/mlsp_clim_annual.csv')) %>% 
  mutate(
    lake = as.factor(lake),
    w_year = as.factor(w_year),
    elev_band = as.factor(elev_band),
    elev_band = fct_relevel(elev_band,
                            c('low','medium','high'))
  )

seasonal <- read_csv(here('data/mlsp_clim_seasonal.csv')) %>% 
  mutate(
    lake = as.factor(lake),
    w_year = as.factor(w_year),
    elev_band = as.factor(elev_band),
    elev_band = fct_relevel(elev_band,
                            c('low','medium','high'))
  )


# 3. annual boxplot -------------------------------------------------------


#Precip----
ggplot()+
  geom_boxplot(
    annual,
    mapping = aes(x = elev_band, y = pr_sd),
    outlier.shape = NA
  )+
  geom_jitter(
    annual %>% 
      group_by(lake) %>% 
      slice(1),
    mapping = aes(
      x = elev_band, 
      y = pr_sd, 
      # color = surf_area
    ),
    size = 4,
    alpha = 0.6,
    position = position_jitter(width = 0.2)
  )+
  scale_color_viridis_c()+
  theme_classic()+
  #labs(color = 'Surface Area')
  xlab('Elevation Band')+
  ylab('Precip. Mean (mm)')
  

ggsave(
  here('output/boxplots/pr_sd_bxplt.png'),
  dpi = 300,
  width = 6,
  height = 5,
  units = 'in'
)  

#Snow----

ggplot()+
  geom_boxplot(
    annual %>% mutate(elev_band = fct_relevel(elev_band,
                                              c('low','medium','high'))),
    mapping = aes(x = elev_band, y = snow_mean_mm),
    outlier.shape = NA
  )+
  geom_jitter(
    annual %>% 
      mutate(
        elev_band = fct_relevel(elev_band, c('low','medium','high'))
      ) %>% 
      group_by(lake) %>% 
      slice(1),
    mapping = aes(
      x = elev_band, 
      y = snow_mean_mm, 
      # color = surf_area
    ),
    size = 4,
    alpha = 0.6,
    position = position_jitter(width = 0.2)
  )+
  scale_color_viridis_c()+
  theme_classic()+
  #labs(color = 'Surface Area')
  xlab('Elevation Band')+
  ylab('Snow Mean (mm)')


ggsave(
  here('output/boxplots/snow_mean_bxplt.png'),
  dpi = 300,
  width = 6,
  height = 5,
  units = 'in'
)  

# Temp----

ggplot()+
  geom_boxplot(
    annual %>% mutate(elev_band = fct_relevel(elev_band,
                                              c('low','medium','high'))),
    mapping = aes(x = elev_band, y = tmean_ann),
    outlier.shape = NA
  )+
  geom_jitter(
    annual %>% 
      mutate(
        elev_band = fct_relevel(elev_band, c('low','medium','high'))
      ) %>% 
      group_by(lake) %>% 
      slice(1),
    mapping = aes(
      x = elev_band, 
      y = tmean_ann, 
      # color = surf_area
    ),
    size = 4,
    alpha = 0.6,
    position = position_jitter(width = 0.2)
  )+
  scale_color_viridis_c()+
  theme_classic()+
  #labs(color = 'Surface Area')
  xlab('Elevation Band')+
  ylab('Temperature Mean (\u00B0C)')


ggsave(
  here('output/boxplots/temp_mean_bxplt.png'),
  dpi = 300,
  width = 6,
  height = 5,
  units = 'in'
) 

# Snow Fraction----

ggplot()+
  geom_boxplot(
    seasonal %>% filter(season == 'winter')%>% 
      group_by(lake) %>% 
      slice(1),
    mapping = aes(x = elev_band, y = snow_frac_mean),
    outlier.shape = NA
  )+
  geom_jitter(
    seasonal %>% 
      filter(season == 'winter') %>% 
      group_by(lake) %>% 
      slice(1),
    mapping = aes(
      x = elev_band, 
      y = snow_frac_mean, 
      # color = surf_area
    ),
    size = 4,
    alpha = 0.6,
    position = position_jitter(width = 0.2)
  )+
  scale_color_viridis_c()+
  theme_classic()+
  #labs(color = 'Surface Area')
  xlab('Elevation Band')+
  ylab('Spring - Snow Fraction (%)')+
  scale_y_continuous(labels = scales::percent)


ggsave(
  here('output/boxplots/snow_frac_winter_bxplt.png'),
  dpi = 300,
  width = 6,
  height = 5,
  units = 'in'
) 
#Function----

# bxplt_func <- function(
#     df,
#     xvar,
#     yvar,
#     clr,
#     xlab,
#     ylab,
#     legend.lab
# ){
#   ggplot()+
#     geom_boxplot(
#       df,
#       mapping = aes(x = {{xvar}}, y = {{yvar}}),
#       outlier.shape = NA
#     )+
#     geom_jitter(
#       df %>% 
#         group_by(lake) %>% 
#         slice(1),
#       mapping = aes(
#         x = {{xvar}}, 
#         y = {{yvar}}, 
#         color = clr
#       ),
#       size = 4,
#       alpha = 0.6,
#       position = position_jitter(width = 0.2)
#     )+
#     scale_color_viridis_c()+
#     theme_classic()+
#     labs(color = {{legend.lab}})
#     xlab({{xlab}})+
#     ylab({{ylab}})
# }
# 
# 
# bxplt_func(
#     df = annual,
#     xvar = elev_band,
#     yvar = pr_mean_mm,
#     clr = surf_area,
#     xlab = Elevation Band,
#     ylab = Precipitation,
#     legend.lab = Surface Area
# )


test <- annual %>% 
  filter(
    elev_band == 'high'
  ) %>% 
  group_by(lake) %>% 
  slice(1)





