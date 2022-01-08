# Load and incorporate new input data for load and VRE potentials
# Data taken from the Open Power Systems Data platform
# https://doi.org/10.25832/time_series/2020-10-06 

library(tidyverse)
library(lubridate)

# Directory and variables
entsoe.file <- "time_series_60min_singleindex_filtered.csv"

# Load data of Germany for 2019, precisely from 2019-01-01 to 2020-01-01
data <- read.csv(entsoe.file) %>% 
  as_tibble() %>% 
  filter(lubridate::year(utc_timestamp)==2019) %>% 
  # Calculate profile for technologies if value is not available due to missing capacity data (only past few days of the year)
  # Assuming that capacity in these few days remains constant
  mutate(DE_solar_profile = case_when(is.na(DE_solar_profile) ~ DE_solar_generation_actual / last(na.omit(DE_solar_capacity)),
                                      TRUE ~ DE_solar_profile),
         DE_wind_onshore_profile = case_when(is.na(DE_wind_onshore_profile) ~ DE_wind_onshore_generation_actual / last(na.omit(DE_wind_onshore_capacity)),
                                             TRUE ~ DE_wind_onshore_profile),
         DE_wind_offshore_profile = case_when(is.na(DE_wind_offshore_profile) ~ DE_wind_offshore_generation_actual / last(na.omit(DE_wind_offshore_capacity)),
                                              TRUE ~ DE_wind_offshore_profile)
         ) %>% 
  # Divide wind offshore profile by maximum value, which strangely is >1 for several hundred years
  # This normalises the profile, but remains a WORKAROUND for now
  mutate(DE_wind_offshore_profile = DE_wind_offshore_profile/max(DE_wind_offshore_profile))

# Load previous data for electricity load and save new
load_DEU <- read.csv('Load_DEU.csv', header=FALSE) %>% 
  mutate(V1 = 2019, 
         V4 = data$DE_load_actual_entsoe_transparency) %>% 
  write.table(sep=',', 'Load_DEU_2019.csv', row.names=FALSE, col.names=FALSE, quote=FALSE)

# Load previous data for VRE potentials and save new
VREpot_DEU <- read.csv('VRE_potential_DEU.csv') %>% 
  mutate(dummy = 2019,
         Solar = data$DE_solar_profile,
         Wind_off = data$DE_wind_offshore_profile,
         Wind_on = data$DE_wind_onshore_profile) %>% 
  write.table(sep=',', 'VRE_potential_DEU_2019.csv', col.names=c('dummy', 'dummy', 'dummy', 'Solar', 'Wind_off', 'Wind_on'), row.names=FALSE, quote=FALSE)
  

# Plot load duration curves of CFs

wind_onshore <- data %>% 
  as_tibble() %>% 
  select(DE_wind_onshore_profile) %>% 
  arrange(desc(DE_wind_onshore_profile))

wind_offshore <- data %>% 
  as_tibble() %>% 
  select(DE_wind_offshore_profile) %>% 
  arrange(desc(DE_wind_offshore_profile))
  
solar <- data %>% 
  as_tibble() %>% 
  select(DE_solar_profile) %>% 
  arrange(desc(DE_solar_profile))

p <- ggplot() + 
  geom_line(data=wind_onshore, aes(x=seq(1,8760), y=DE_wind_onshore_profile, colour="onshore")) + 
  geom_line(data=wind_offshore, aes(x=seq(1,8760), y=DE_wind_offshore_profile, colour="offshore")) +
  geom_line(data=solar, aes(x=seq(1,8760), y=DE_solar_profile, colour="solar"))

p
