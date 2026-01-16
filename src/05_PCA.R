
# Libraries ---------------------------------------------------------------

library(tidyverse)
library(here)
library(janitor)
library(vegan)
library(ggrepel)
library(cowplot)
library(grid)
library(gridExtra) 

# 1. Import data ----------------------------------------------------------

#Mountain lake climate data
mtn_clim <- read_csv(
  here('data/gridmet_means_1979_2025.csv')
) %>% 
  mutate(
    lake = as.factor(lake),
    water_year = as.factor(water_year),
    season = as.factor(season)
  )

#Site metadata
mlsp <- read_csv(here('data/mlsp_site_metadata.csv')) %>% 
  mutate(
    lake = as.factor(lake)
  )

#Combine climate and site metadata
mtn_df <- mlsp %>% 
  full_join(
    mtn_clim,
    relationship = 'many-to-many'
  )

# 2. Combine dataframes ---------------------------------------------------


#**5a. PCA Prep new----
pca_prep <- mtn_df %>% 
  #Adjust season of interest for PCA
  # filter(
  #   season == 'autumn'
  # ) %>% 
  #Need to remove non-numeric columns
  #Combining site name and date, then moving them to row names
  unite(
    site_date, c(lake, water_year, season), sep = '_', remove = F
  ) %>% 
  distinct(
    site_date, .keep_all = T
  ) 

#**5b. Combine PCA dataframe----
pca_df <- pca_prep %>% 
  select(
    -c(lake, water_year, season) 
  ) %>% 
  column_to_rownames(var = 'site_date') %>% 
  na.omit()# %>% 
  # select(
  #   # 1:5, #mrph vars
  #   13,14,17, #temp vars
  #   # 7,18, #Average seasonal daily rain and snow
  #   19,20 #cumlative vars
  # )

#**5c. Normalize the variables----
df_standard <- decostand(pca_df,"standardize")

# **5d. Calculate the PCA----
ipc_pca <- rda(df_standard)
summary(ipc_pca)  
# plot(ipc_pca)
# 
# #**5e. Test Plot----
# 
# # Plots using biplot.rda
# # Not for presentation but can be used to quickly evaluate 
# dev.new(width = 12,
#         height = 6,
#         title = "PCA biplots - Physical Variables - biplot.rda", 
#         noRStudioGD = FALSE
# )
# par(mfrow = c(1, 2))
# biplot(ipc_pca, scaling = 1, main = "PCA - scaling 1")
# biplot(ipc_pca, main = "PCA - scaling 2")


#**5f. Extract site scores----
# Extract site scores on 1st two PC axes along with metadata
# Remember to specify scaling (1 for variables, 2 for sites)
site_scores <- tibble(as.data.frame(
  scores(ipc_pca,
         choices=c(1,2),
         display="sites", 
         scaling = 2))[,1:2]) |>
  mutate(site_date = row.names(pca_df)) |>
  left_join(select(
    pca_prep, site_date, lake, water_year, season)) |>
  #all_data, sample_id, collection_site, collect_date, layer)) |>
  unique() 

par_scores <- tibble(as.data.frame(
  scores(ipc_pca,
         choices=c(1,2),
         display="species", 
         scaling = 2))[,1:2]) |>
  mutate(variable = colnames(pca_df))

#**5g. PCA plot final----
ggplot(par_scores, aes(x = PC1, y = PC2)) +
  # Draw axes along the zero intercept
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  # Draw arrows representing the variables
  geom_segment(aes(
    x = 0,
    y = 0,
    xend = PC1,
    yend = PC2),
    arrow = arrow(length = unit(0.1, "inches"))) +
  # Label the arrows
  # geom_text(aes(label = variable),
  #           nudge_x = 0.08,
  #           nudge_y = 0.1) +
  geom_text_repel(
    aes(x = PC1, y = PC2, label = variable),
    box.padding = unit(0.5, 'lines'),
    point.padding = unit(0.5, 'lines'),
    segment.color = NA
  )+
  # Draw points representing the sites
  geom_point(data = site_scores,
             aes(x = PC1,
                 y = PC2,
                 colour = lake,
                 # shape = season
                 ),
             size = 3,
             alpha = 0.3) +
  # scale_color_wa_d(
  #   palette = 'rainier',
  #   which = c('lake', 'lodge', 'ground'),
  #   labels = c('Paint Lake (Deep)','Paint Lake (Shallow)','Kempenfelt Bay')
  # )+
  scale_color_viridis_d()+
  theme_classic() +
  theme(
    axis.line = element_blank(),
    legend.title = element_blank(),
    legend.position = 'none'
  ) +
  # ggtitle("Principal Component Analysis, scaling 2")+
  xlab(paste0("PCA1 (40%)"))+ 
  ylab(paste0("PCA2 (27%)")) 

ggsave(
  here('output/pca/pca_all.png'),
  dpi = 300,
  width = 6,
  height = 6,
  units = 'in'
)

ggplot(data = mtn_df)+
  geom_point(
    aes(x = snow_cumsum_mm, y = elevation, color = lake)
  )+
  scale_color_viridis_d()

ggsave(
  here('output/pca/pca_leg.png'),
  dpi = 300,
  width = 8,
  height = 8,
  units = 'in'
)
