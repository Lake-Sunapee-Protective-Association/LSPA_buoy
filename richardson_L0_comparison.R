# compare L1/2 ccc/dr compilation with raw data from sunapee buoy

library(tidyverse)
library(readxl)


met_buoy <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/Richardson 07-13 L1-2/L1_buoy_comp_24Oct16.csv',
                     col_types = cols(.default = 'c'))

L0_buoy <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L0/Richardson 07-13 L0/archive/L0_buoy_comp_21Oct16.csv',
                    col_types = cols(.default = 'c'))                     

met_buoy_do <- met_buoy %>% 
  mutate(DateTime = as.POSIXct(DateTime, format = '%m/%d/%Y %H:%M')) %>% 
  select(DateTime, DO_mgL_1m) %>% 
  rename(met_do_mgl = DO_mgL_1m) %>% 
  mutate(met_do_mgl = as.numeric(met_do_mgl))

L0_buoy_do <- L0_buoy %>% 
  mutate(DateTime = as.POSIXct(DateTime, format = '%Y-%m-%d %H:%M')) %>% 
  select(DateTime, DO_mgL_1m) %>% 
  rename(l0_do_mgl = DO_mgL_1m) %>% 
  mutate(l0_do_mgl = as.numeric(l0_do_mgl))

buoy_do_comp <- full_join(met_buoy_do, L0_buoy_do) 

ggplot(buoy_do_comp, aes(x = l0_do_mgl, y = met_do_mgl)) + 
  geom_point()


met_buoy_temp <- met_buoy %>% 
  mutate(DateTime = as.POSIXct(DateTime, format = '%m/%d/%Y %H:%M')) %>% 
  select(DateTime, Temp_0m_degC:Temp_14m_degC) %>% 
  gather(variable, met_value, -DateTime) %>% 
  mutate(met_value = as.numeric(met_value))

L0_buoy_temp <- L0_buoy %>% 
  mutate(DateTime = as.POSIXct(DateTime, format = '%Y-%m-%d %H:%M')) %>% 
  select(DateTime, Temp_0m_degC:Temp_13m_degC) %>% 
  gather(variable, l0_value, -DateTime) %>% 
  mutate(l0_value = as.numeric(l0_value))

buoy_temp_comp <- full_join(met_buoy_temp, L0_buoy_temp) 

ggplot(buoy_temp_comp, aes(x = met_value, y = l0_value)) + 
  geom_point() +
  facet_grid(variable ~ .) +
  coord_cartesian(xlim = c(0, 30), ylim = c(0,30))
