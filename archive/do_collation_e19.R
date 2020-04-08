#R version: 3.5.2
#R Studio version: 1.1.463

# do collation
do_0717 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2007-2017_do_L1.csv',
                    col_types = 'Tcnnncnnnc')
str(do_0717)

#make vertical
do_0717_v <- do_0717 %>% 
  gather(variable, value, -datetime, -location, - upper_do_flag, -lower_do_flag) %>% 
  mutate(sensor = case_when(variable == 'DOSat' | variable == 'DOppm' | variable == 'DOTempC' ~ 'upper do',
                            variable == 'DOLowSat' | variable == 'DOLowPPM' | variable == 'DOLowTempC' ~ 'lower do',
                            TRUE ~ NA_character_),
         flag = case_when(sensor == 'upper do' & !is.na(upper_do_flag) ~ upper_do_flag,
                          sensor == 'lower do' & !is.na(lower_do_flag) ~ lower_do_flag,
                          TRUE ~ NA_character_)) %>% 
  select(-upper_do_flag, -lower_do_flag) %>% 
  mutate(variable = case_when(grepl('ppm', variable, ignore.case = T) ~ 'conc ppm',
                              grepl('sat', variable, ignore.case = T) ~ 'sat pct',
                              grepl('temp', variable, ignore.case = T) ~ 'temp degC',
                              TRUE ~ NA_character_))
str(do_0717_v)  
unique(do_0717_v$variable)
unique(do_0717_v$sensor)
unique(do_0717_v$flag)

#all recorded with the ponsel units. label depth as 1.5 and 10.5
do_0717_v <- do_0717_v %>% 
  mutate(source = 'ponsel',
         depth_m = case_when(sensor == 'upper do' ~ 1.5,
                           sensor == 'lower do' ~ 10.5,
                           TRUE ~ NA_real_))
str(do_0717_v)
unique(do_0717_v$depth_m)

#add 1718 u26
do_1718 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2017-2018_hobotempstringdo_L1.csv',
                    col_types = 'Tnnnnnnnnnnn') %>% 
  select(datetime, do_ppm, TempC_1p5m) %>% 
  gather(variable, value, -datetime) %>% 
  mutate(variable = case_when(variable == 'do_ppm' ~ 'conc ppm',
                              variable == 'TempC_1p5m' ~ 'temp degC',
                              TRUE ~ NA_character_),
         sensor = 'upper do',
         source = 'hobo U26',
         location = 'loon',
         depth_m = 1.5)
str(do_1718)
unique(do_1718$depth_m)

#add ponsel
do2018 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2018_do_L1.csv',
                   col_types = 'Tnnncc') %>% 
  filter(datetime >= as.POSIXct('2018-01-01', tz='UTC') & datetime < as.POSIXct('2019-01-01', tz='UTC')) %>% 
  rename(flag = lowDO_flag) %>% 
  gather(variable, value, -datetime, -location, -flag) %>% 
  mutate(variable = case_when(variable == 'DOLowTempC' ~ 'temp degC',
                              variable == 'DOLowSat' ~ 'sat pct',
                              variable == 'DOLowPPM' ~ 'conc ppm',
                              TRUE ~ NA_character_),
         sensor = 'lower do',
         source = 'ponsel',
         depth_m = 10.5)
str(do2018)
unique(do2018$depth_m)

#add summer 18 u26
do_18hobo <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2018_hobodo_L1.csv') %>% 
  gather(variable, value, -datetime) %>% 
  mutate(variable = case_when(variable == 'do_mgl' ~ 'conc ppm',
                              variable == 'temp_degC' ~ 'temp degC',
                              TRUE ~ NA_character_),
         sensor = 'upper do',
         source = 'hobo U26',
         location = 'loon',
         depth_m = 0.25)
str(do_18hobo)
unique(do_18hobo$depth_m)

#add 1819 u26
do_1819 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2018-2019_hobotempstringdo_L1.csv',
                    col_types = 'Tnnnnnnnnnnn') %>% 
  select(datetime, do_ppm, TempC_1p5m) %>% 
  gather(variable, value, -datetime) %>% 
  mutate(variable = case_when(variable == 'do_ppm' ~ 'conc ppm',
                              variable == 'TempC_1p5m' ~ 'temp degC',
                              TRUE ~ NA_character_),
         sensor = 'upper do',
         source = 'hobo U26',
         location = 'loon',
         depth_m = 1.5)
str(do_1819)
unique(do_1819$depth_m)

#add insitu
doe2019 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2019_do_L1.csv',
                   col_types = 'Tnnnnnnc') %>% 
  filter(datetime >= as.POSIXct('2019-01-01', tz='UTC') & datetime < as.POSIXct('2020-01-01', tz='UTC')) %>% 
  gather(variable, value, -datetime, -location) %>% 
  mutate(sensor = case_when(variable == 'DOSat' | variable == 'DOppm' | variable == 'DOTempC' ~ 'upper do',
                            variable == 'DOLowSat' | variable == 'DOLowPPM' | variable == 'DOLowTempC' ~ 'lower do',
                            TRUE ~ NA_character_),
         variable = case_when(grepl('ppm', variable, ignore.case = T) ~ 'conc ppm',
                              grepl('sat', variable, ignore.case = T) ~ 'sat pct',
                              grepl('temp', variable, ignore.case = T) ~ 'temp degC',
                              TRUE ~ NA_character_), 
         source = 'in-situ',
         depth_m = case_when(sensor == 'upper do' ~ 0.25,
                             sensor == 'lower do' ~ 10.5,
                             TRUE ~ NA_real_))

unique(doe2019$depth_m)


str(doe2019)


####collate buoy temp data together ####
buoy_record_do <- full_join(do_0717_v, do_1718) %>% 
  full_join(., do2018) %>%
  full_join(., do_18hobo) %>% 
  full_join(., do_1819) %>% 
  full_join(., doe2019) %>% 
  arrange(datetime) %>% 
  select(datetime, location, variable, value, depth_m, sensor, source, flag)

str(buoy_record_do)

ggplot(buoy_record_do, aes(x = datetime, y = value, color = sensor, shape = location)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 year')
  
ggplot(subset(buoy_record_do,
              subset = datetime >= as.POSIXct('2015-01-01', tz='UTC')),
       aes(x = datetime, y = value, color = sensor, shape = location)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 year')


buoy_record_do %>% 
  mutate(datetime = as.character(datetime)) %>% 
  write_csv(., 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2007-e2019_do_L1.csv')
