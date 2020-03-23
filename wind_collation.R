#### collate wind data ####
wind_07 <-  read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2007_wind_L1.csv',
                     col_types = 'Tcnnc')
colnames(wind_07)

wind_07 <- wind_07 %>% 
  mutate(WindDir_deg = case_when(wind_dir_flag == 'e' ~ NA_real_,
                                 TRUE ~ WindDir_deg))

wind_08 <-  read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2008_wind_L1.csv',
                     col_types = 'Tcnnc')
colnames(wind_08)

wind_08 <- wind_08 %>% 
  mutate(WindDir_deg = case_when(wind_dir_flag == 'e' ~ NA_real_,
                                 TRUE ~ WindDir_deg))


wind_09 <-  read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2009_wind_L1.csv',
                     col_types = 'Tcnnnnc')
colnames(wind_09)

wind_09 <- wind_09 %>% 
  mutate(WindDir_deg = case_when(wind_dir_flag == 'e' ~ NA_real_,
                                 TRUE ~ WindDir_deg))


wind_10 <-  read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2010_wind_L1.csv',
                     col_types = 'Tcnnnn')
colnames(wind_10)

wind_11 <-  read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2011_wind_L1.csv',
                     col_types = 'Tcnn')
colnames(wind_11)

wind_12 <-  read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2012_wind_L1.csv',
                     col_types = 'Tcnn')
colnames(wind_12)

wind_13 <-  read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2013_wind_L1.csv',
                     col_types = 'Tcnnnnnn')
colnames(wind_13)

wind_14 <-  read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2014_wind_L1.csv',
                     col_types = 'Tcnnnnnn')
colnames(wind_14)
wind_14 <- wind_14 %>% 
  rename(WindSp_ms = InstWindSp,
         WindDir_deg = InstWindDir,
         AveWindSp_ms = AveWindSp,
         AveWindDir_deg  = AveWindDir,
         MaxWindSp_ms = MaxWindSp,
         MaxWindDir_deg = MaxWindDir)
colnames(wind_14)

wind_15 <-  read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2015_wind_L1.csv',
                     col_types = 'Tnnnnnnc')
colnames(wind_15)
wind_15 <- wind_15 %>% 
  rename(WindSp_ms = InstWindSp,
         WindDir_deg = InstWindDir,
         AveWindSp_ms = AveWindSp,
         AveWindDir_deg  = AveWindDir,
         MaxWindSp_ms = MaxWindSp,
         MaxWindDir_deg = MaxWindDir)
colnames(wind_15)

wind_16 <-  read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2016_wind_L1.csv',
                     col_types = 'Tnnnnc')
colnames(wind_16)
wind_16 <- wind_16 %>% 
  rename(AveWindSp_ms = AveWindSp,
         AveWindDir_deg  = AveWindDir,
         MaxWindSp_ms = MaxWindSp,
         MaxWindDir_deg = MaxWindDir)
colnames(wind_16)

wind_17 <-  read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2017_wind_L1.csv',
                     col_types = 'Tnnnnc')
colnames(wind_17)
wind_17 <- wind_17 %>% 
  rename(AveWindSp_ms = AveWindSp,
         AveWindDir_deg  = AveWindDir,
         MaxWindSp_ms = MaxWindSp,
         MaxWindDir_deg = MaxWindDir)
colnames(wind_17)


#### collate all files into one ####
wind_0717 <- full_join(wind_07, wind_08) %>% 
  full_join(., wind_09) %>% 
  full_join(., wind_10) %>% 
  full_join(., wind_11) %>% 
  full_join(., wind_12) %>% 
  full_join(., wind_13) %>% 
  full_join(., wind_14) %>% 
  full_join(., wind_15) %>% 
  full_join(., wind_16) %>% 
  full_join(., wind_17) %>% 
  mutate_at(vars(WindDir_deg, WindSp_ms, AveWindDir_deg, AveWindSp_ms, MaxWindDir_deg, MaxWindSp_ms),
            funs(case_when(location == 'offline' ~ NA_real_,
                           location == 'in transit' ~ NA_real_, 
                           TRUE ~ .)))  %>%  # recode any offline or in transit data to NA for export
  select(-wind_dir_flag) #drop wind dir flag, as not needed any longer

wind_0717_h <- wind_0717 %>% 
  gather(variable, value, -datetime, -location)
str(wind_0717_h)

ggplot(wind_0717_h, aes(x=datetime, y = value, color = location)) +
  geom_point() +
  facet_grid(variable ~ .) +
  final_theme +
  scale_color_colorblind()

#### export file ####
wind_0717 %>% 
  mutate(datetime = as.character(datetime)) %>% 
  write_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2007-2017_wind_L1.csv')

colnames(wind_0717)
