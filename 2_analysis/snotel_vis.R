# source("snotel_pull.R")
#Or just read in the files loaded into Box - the full script can take a few minutes
source("0_helper scripts/libraries.R")
data_path <- "~/Library/CloudStorage/Box-Box/Mountain Lakes Sensor Data Synthesis/Data/"
figure_path <- "~/Library/CloudStorage/Box-Box/Mountain Lakes Sensor Data Synthesis/Figures/"
site_metadata <- read.csv(paste0(data_path,"Site_metadata.csv"))
snotel_data_combined <- read.csv(paste0(data_path,"snotel_data_raw.csv"))
nearest_snotel <- read.csv(paste0(data_path,"snotel_nearest_site.csv"))
snow_phenology <- read.csv(paste0(data_path,"snotel_phenology_calculated_metrics.csv"))


#How far is the closest SNOTEL?
nearest_snotel %>%
  ggplot(aes(x=distance_to_nearest_site)) +
  geom_histogram() +
  theme_minimal() +
  labs(y="count",
       x="Distance to nearest SNOTEL (km)")
ggsave(paste0(figure_path,"SNOTEL/closest_snotel_km.png"),
       width = 7, height = 4, units = 'in', dpi=300)


#How many lakes reference the same SNOTEL site?
nearest_site_counts <- nearest_snotel %>%
  group_by(nearest_site, state) %>%
  summarize(n_lakes = length(unique(site_name))) %>%
  arrange(desc(n_lakes))

ggplot(nearest_site_counts,
       aes(x = reorder(paste0(nearest_site, " (", state, ")"), -n_lakes),
           y = n_lakes)) +
  geom_col() +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  labs(
    x = "Nearest SNOTEL site (state)",
    y = "Number of lakes"
  )+
  scale_y_continuous(
    breaks = scales::pretty_breaks(n = 10),
    labels = scales::number_format(accuracy = 1)
  ) 
ggsave(paste0(figure_path,"SNOTEL/snotel_site_by_lake_number.png"),
       width = 7, height = 4, units = 'in', dpi=300)

# How many years of data do some of these sites have?
snow_phenology %>%
  group_by(snotel_site_id) %>%
  summarize(n_years=length(unique(year))) %>%
  ggplot(aes(x=n_years))+
  geom_histogram()+
  theme_minimal()+
  labs(x="Number of years of data per station",
       caption="might be ok since we only have recent years of buoy data")
ggsave(paste0(figure_path,"SNOTEL/snotel_site_years_of_data.png"),
       width = 7, height = 4, units = 'in', dpi=300)


# Example of kinds of summary stats
# But first
# Site 998? wtf is going on?
snotel_data_combined %>%
  filter(snotel_site_id=="998") %>%
  mutate(waterYear=calcWaterYear(date)) %>%
  filter(waterYear > 2012 & waterYear < 2016) %>%
  ggplot(aes(x=date,y=snow_water_equivalent, color=factor(waterYear)))+
  geom_point()
#Exclude this some analysis... there's hardly any data so the stats get messed up.
#May need to do some more spot checking before a formal analysis


head(snow_phenology)
snow_phenology %>%
  ggplot(aes(x=year,y=max_swe,color=factor(snotel_site_id)))+
  geom_point()+
  labs(y="Maximum SWE (cm)",
       x="Water year")
ggsave(paste0(figure_path,"SNOTEL/snotel_max_SWE_all_years.png"),
       width = 7, height = 4, units = 'in', dpi=300)

snow_phenology %>%
  filter(year>=2010)%>%
  filter(snotel_site_id != "998" | year != 2014) %>%
  ggplot(aes(x=waterYear,y=max_swe,color=factor(snotel_site_id)))+
  geom_point()+
  geom_line()+
  labs(y="Maximum SWE (cm)",
       x="Water year")
ggsave(paste0(figure_path,"SNOTEL/snotel_max_SWE_since_2010.png"),
       width = 7, height = 4, units = 'in', dpi=300)

snow_phenology %>%
  filter(year>=2010)%>%
  filter(!(snotel_site_id=="998" & !waterYear=="2014")) %>%
  left_join(., site_metadata %>% select(Name, Range), by=c("site_name"="Name")) %>%
  ggplot(aes(x=waterYear,y=first_snow_melt_doy,color=factor(snotel_site_id)))+
  geom_point()+
  geom_line()+
  labs(y="First snowmelt DOY",
       x="Water year")+
  facet_wrap(~Range, scales="free_y")
ggsave(paste0(figure_path,"SNOTEL/DOY_first_snowmelt_by_Range_since_2010.png"),
       width = 7, height = 4, units = 'in', dpi=300)

#Date of max SWE by Range
snow_phenology %>%
  mutate(waterYear=calcWaterYear(max_swe_date)) %>%
  filter(waterYear>=2010)%>%
  filter(!(snotel_site_id=="998" & !waterYear=="2014")) %>%
  # filter(!(snotel_site_id=="941")) %>%
  left_join(., site_metadata %>% select(Name, Range), by=c("site_name"="Name")) %>%
  ggplot(aes(x=waterYear,y=max_swe_doy,color=factor(snotel_site_id)))+
  geom_point()+
  geom_line()+
  labs(y="Max SWE DOY",
       x="Water year")+
  facet_wrap(~Range)
ggsave(paste0(figure_path,"SNOTEL/DOY_max_SWE_by_Range_since_2010.png"),
       width = 7, height = 4, units = 'in', dpi=300)


#How variable is timing of first snowmelt by range?
snow_phenology %>%
  filter(year>=2010)%>%
  filter(!(snotel_site_id=="998" & !year=="2014")) %>%
  left_join(., site_metadata %>% select(Name, Range), by=c("site_name"="Name")) %>%
  group_by(Range) %>%
  summarize(snowmelt_sd = sd(first_snow_melt_doy, na.rm=FALSE),
            snowmelt_mean = mean(first_snow_melt_doy, na.rm=FALSE),
            snowmelt_cv = snowmelt_sd/snowmelt_mean) %>%
  ggplot(aes(x=reorder(Range, -snowmelt_cv), y=snowmelt_cv,fill=Range))+
  geom_col()+
  labs(y="Coefficient of variation in snowmelt DOY (2010-2025)")+
  theme_minimal()+
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) 
ggsave(paste0(figure_path,"SNOTEL/CV_snowmelt_timing_by_Range_2010-2025.png"),
       width = 7, height = 5, units = 'in', dpi=300)


snow_phenology %>%
  filter(year>=2010)%>%
  filter(!(snotel_site_id=="998" & !year=="2014")) %>%
  left_join(., site_metadata %>% select(Name, Range), by=c("site_name"="Name")) %>%
  group_by(Range) %>%
  summarize(maxswe_sd = sd(max_swe, na.rm=FALSE),
            maxswe_mean = mean(max_swe, na.rm=FALSE),
            maxswe_cv = maxswe_sd/maxswe_mean) %>%
  ggplot(aes(x=reorder(Range, -maxswe_cv), y=maxswe_cv,fill=Range))+
  geom_col()+
  labs(y="Coefficient of variation in max SWE (2010-2025)")+
  theme_minimal()+
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) 
ggsave(paste0(figure_path,"SNOTEL/CV_max_SWE_by_Range_2010-2025.png"),
       width = 7, height = 5, units = 'in', dpi=300)



