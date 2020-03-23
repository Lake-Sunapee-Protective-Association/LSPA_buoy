#*****************************************************************
#*      Cary Institute of Ecosystem Studies (Millbrook, NY)      *
#*                                                               *
#* TITLE:   Sunapee_hobo_2017-2018.r                             *
#* AUTHOR:  Bethel Steele                                        *
#* SYSTEM:  Lenovo ThinkCentre, Win 10, R 3.4.2, RStudio 1.1.383 *
#* PROJECT: Lake Sunapee Buoy Data Cleaning                      *
#* PURPOSE: create L0 and L1 data for buoy data 2016 using       *
#*          similar methods to CCC and DR                        *
#* PREVIOUS VERSION:                                             *
#*****************************************************************

#bring in winter 17-18 hobo raw data
hobo_w17_2m <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L0/winter hobo data/2017-18 Winter Pendant/U22-2m.csv',
                      col_types = 'icn', 
                      col_names = c('obs', 'datetime', 'TempC_2m'),
                      skip =2) %>% 
  select(-obs)
hobo_w17_3m <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L0/winter hobo data/2017-18 Winter Pendant/U22-3m.csv',
                        col_types = 'icn', 
                        col_names = c('obs', 'datetime', 'TempC_3m'),
                        skip =2)%>% 
  select(-obs) 
hobo_w17_4m <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L0/winter hobo data/2017-18 Winter Pendant/U22-4m.csv',
                        col_types = 'icn', 
                        col_names = c('obs', 'datetime', 'TempC_4m'),
                        skip =2) %>% 
  select(-obs)
hobo_w17_5m <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L0/winter hobo data/2017-18 Winter Pendant/U22-5m.csv',
                        col_types = 'icn', 
                        col_names = c('obs', 'datetime', 'TempC_5m'),
                        skip =2) %>% 
  select(-obs)
hobo_w17_6m <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L0/winter hobo data/2017-18 Winter Pendant/U22-6m.csv',
                        col_types = 'icn', 
                        col_names = c('obs', 'datetime', 'TempC_6m'),
                        skip =2) %>% 
  select(-obs)
hobo_w17_8m <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L0/winter hobo data/2017-18 Winter Pendant/U22-8m.csv',
                        col_types = 'icn', 
                        col_names = c('obs', 'datetime', 'TempC_8m'),
                        skip =2) %>% 
  select(-obs)
hobo_w17_9m <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L0/winter hobo data/2017-18 Winter Pendant/U22-9m.csv',
                        col_types = 'icn', 
                        col_names = c('obs', 'datetime', 'TempC_9m'),
                        skip =2) %>% 
  select(-obs)
hobo_w17_10m <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L0/winter hobo data/2017-18 Winter Pendant/U22-10m.csv',
                        col_types = 'icn', 
                        col_names = c('obs', 'datetime', 'TempC_10m'),
                        skip =2) %>% 
  select(-obs)
hobo_w17_do <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L0/winter hobo data/2017-18 Winter Pendant/2017-2018_U26DO.csv',
                        col_types = 'icnn',
                        col_names = c('obs', 'datetime', 'do_ppm', 'TempC_1m'),
                        skip=2) %>% 
  select(-obs)

hobo_w17 <- full_join(hobo_w17_do, hobo_w17_2m) %>% 
  full_join(., hobo_w17_3m) %>% 
  full_join(., hobo_w17_4m) %>% 
  full_join(., hobo_w17_5m) %>% 
  full_join(., hobo_w17_6m) %>% 
  full_join(., hobo_w17_8m) %>% 
  full_join(., hobo_w17_9m) %>% 
  full_join(., hobo_w17_10m)

rm(hobo_w17_do, hobo_w17_2m, hobo_w17_3m, hobo_w17_4m, hobo_w17_5m, hobo_w17_6m, hobo_w17_8m, hobo_w17_9m, hobo_w17_10m)

#format date of hobo sensors
hobo_w17 <- hobo_w17 %>% 
  mutate(datetime = as.POSIXct(datetime, format='%m/%d/%y %I:%M:%S %p', tz='UTC')) %>% 
  arrange(datetime)

#### hobo line ####
hobo_temp_vert <- hobo_w17 %>%
  select(-do_ppm) %>% 
  gather(variable, value, -datetime)
range(hobo_temp_vert$value, na.rm = T)

hobo_w17_L1 <- hobo_w17 %>% 
  mutate_at(vars("TempC_1m", "TempC_2m", "TempC_3m", "TempC_4m", "TempC_5m", "TempC_6m", "TempC_8m", "TempC_9m", "TempC_10m"),
            funs(case_when(. < -50 ~ NA_real_,
                 TRUE ~ .)))
hobo_temp_vert_L1 <- hobo_w17_L1 %>%
  select(-do_ppm) %>% 
  gather(variable, value, -datetime)

ggplot(hobo_temp_vert_L1, aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='hobo temp sensor winter 2017-2018', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  scale_x_datetime(date_breaks = '1 month') +
  final_theme

ggplot(subset(hobo_temp_vert_L1, subset=(datetime>='2017-10-01' & datetime < '2017-11-01')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='hobo temp Oct 2017', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme

#oct 19 deployment
ggplot(subset(hobo_temp_vert_L1, subset=(datetime>='2017-10-19' & datetime < '2017-10-20')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='hobo temp Oct 2017', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme

#settles by 16:00
hobo_w17_L1 <- hobo_w17_L1 %>% 
  mutate_at(vars("TempC_2m", "TempC_3m", "TempC_4m", "TempC_5m", "TempC_6m", "TempC_8m", "TempC_9m", "TempC_10m"),
            funs(case_when(datetime < as.POSIXct('2017-10-19 16:00', tz='UTC') ~ NA_real_,
                           TRUE ~ .)))
hobo_temp_vert_L1 <- hobo_w17_L1 %>%
  select(-do_ppm) %>% 
  gather(variable, value, -datetime)

ggplot(subset(hobo_temp_vert_L1, subset=(datetime>='2017-10-01' & datetime < '2017-11-01')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='hobo temp Oct 2017', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme


ggplot(subset(hobo_temp_vert_L1, subset=(datetime>='2017-11-01' & datetime < '2017-12-01')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='hobo temp Nov 2017', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme

#nov 2 do 1m deployment
ggplot(subset(hobo_temp_vert_L1, subset=(datetime>='2017-11-02' & datetime < '2017-11-03')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='hobo temp Nov 2017', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme

#full string impacted 14:00-14:30; tempc1 from 13:20-15:00
hobo_w17_L1 <- hobo_w17_L1 %>% 
  mutate_at(vars("TempC_2m", "TempC_3m", "TempC_4m", "TempC_5m", "TempC_6m", "TempC_8m", "TempC_9m", "TempC_10m"),
            funs(case_when(datetime >= as.POSIXct('2017-11-02 14:00', tz='UTC') &
                             datetime < as.POSIXct('2017-11-02 14:40', tz='UTC') ~ NA_real_,
                           TRUE ~ .))) %>%   
  mutate(TempC_1m = case_when(datetime >= as.POSIXct('2017-11-02 13:20', tz='UTC') &
                             datetime < as.POSIXct('2017-11-02 15:00', tz='UTC')~ NA_real_,
                           TRUE ~ TempC_1m))

hobo_temp_vert_L1 <- hobo_w17_L1 %>%
  select(-do_ppm) %>% 
  gather(variable, value, -datetime)

ggplot(subset(hobo_temp_vert_L1, subset=(datetime>='2017-11-01' & datetime < '2017-12-01')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='hobo temp Nov 2017', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme


ggplot(subset(hobo_temp_vert_L1, subset=(datetime>='2017-12-01' & datetime < '2018-01-01')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='hobo temp Dec 2017', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme

ggplot(subset(hobo_temp_vert_L1, subset=(datetime>='2018-01-01' & datetime < '2018-02-01')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='hobo temp Jan 2018', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme

ggplot(subset(hobo_temp_vert_L1, subset=(datetime>='2018-02-01' & datetime < '2018-03-01')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='hobo temp Feb 2018', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme

ggplot(subset(hobo_temp_vert_L1, subset=(datetime>='2018-03-01' & datetime < '2018-04-01')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='hobo temp Mar 2018', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme

ggplot(subset(hobo_temp_vert_L1, subset=(datetime>='2018-04-01' & datetime < '2018-05-01')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='hobo temp Apr 2018', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme

ggplot(subset(hobo_temp_vert_L1, subset=(datetime>='2018-05-01' & datetime < '2018-06-01')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='hobo temp May 2018', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme

#may 19 do removal
ggplot(subset(hobo_temp_vert_L1, subset=(datetime>='2018-05-19' & datetime < '2018-05-20')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='hobo temp May 2018', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme

#may 21 removal
ggplot(subset(hobo_temp_vert_L1, subset=(datetime>='2018-05-21' & datetime < '2018-05-22')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='hobo temp May 2018', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme

#settles by 16:00
hobo_w17_L1 <- hobo_w17_L1 %>% 
  mutate_at(vars("TempC_1m", "TempC_2m", "TempC_3m", "TempC_4m", "TempC_5m", "TempC_6m", "TempC_8m", "TempC_9m", "TempC_10m"),
            funs(case_when(datetime >= as.POSIXct('2018-05-21 9:00', tz='UTC') ~ NA_real_,
                           TRUE ~ .)))
hobo_temp_vert_L1 <- hobo_w17_L1 %>%
  select(-do_ppm) %>% 
  gather(variable, value, -datetime)

ggplot(subset(hobo_temp_vert_L1, subset=(datetime>='2018-05-01' & datetime < '2018-06-01')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='hobo temp May 2018', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme

ggplot(hobo_temp_vert_L1, aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='hobo temp 2018', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  scale_x_datetime(date_minor_breaks = '1 month') +
  final_theme


#### do unit ####
hobo_do_w17 <- hobo_w17_L1 %>% 
  select(datetime, TempC_1m, do_ppm)

hobo_do_w17_vert <- hobo_do_w17 %>% 
  gather(variable, value, - datetime)

ggplot(hobo_do_w17_vert, aes(x=datetime, y=value)) +
  facet_grid(variable ~. , scales = 'free_y') +
  geom_point() +
  labs(title='hobo do/temp 2018', x='date', y='temp (deg C)') +
  scale_x_datetime(date_minor_breaks = '1 month') +
  final_theme

hobo_w17_L1 <- hobo_w17_L1 %>% 
  mutate(do_ppm = case_when(do_ppm < -50 ~ NA_real_,
                            TRUE ~ do_ppm))
  
hobo_do_w17 <- hobo_w17_L1 %>% 
  select(datetime, TempC_1m, do_ppm)

hobo_do_w17_vert <- hobo_do_w17 %>% 
  gather(variable, value, - datetime)

  
ggplot(subset(hobo_do_w17_vert, subset = (datetime >= as.POSIXct('2017-11-01', tz='UTC') &
                                            datetime < as.POSIXct('2017-12-01', tz='UTC'))),
              aes(x=datetime, y=value)) +
  facet_grid(variable ~. , scales = 'free_y') +
  geom_point() +
  labs(title='hobo do/temp 2018', x='date', y='temp (deg C)') +
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme

ggplot(subset(hobo_do_w17_vert, subset = (datetime >= as.POSIXct('2017-11-02', tz='UTC') &
                                            datetime < as.POSIXct('2017-11-03', tz='UTC'))),
       aes(x=datetime, y=value)) +
  facet_grid(variable ~. , scales = 'free_y') +
  geom_point() +
  labs(title='hobo do/temp 2018', x='date', y='temp (deg C)') +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme

hobo_w17_L1 <- hobo_w17_L1 %>% 
  mutate(do_ppm = case_when(datetime >= as.POSIXct('2017-11-02 13:20', tz='UTC') &
                                datetime < as.POSIXct('2017-11-02 15:00', tz='UTC')~ NA_real_,
                              TRUE ~ do_ppm))

hobo_do_w17 <- hobo_w17_L1 %>% 
  select(datetime, TempC_1m, do_ppm)

hobo_do_w17_vert <- hobo_do_w17 %>% 
  gather(variable, value, - datetime)

ggplot(subset(hobo_do_w17_vert, subset = (datetime >= as.POSIXct('2017-11-01', tz='UTC') &
                                            datetime < as.POSIXct('2017-12-01', tz='UTC'))),
       aes(x=datetime, y=value)) +
  facet_grid(variable ~. , scales = 'free_y') +
  geom_point() +
  labs(title='hobo do/temp 2018', x='date', y='temp (deg C)') +
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme

ggplot(subset(hobo_do_w17_vert, subset = (datetime >= as.POSIXct('2017-12-01', tz='UTC') &
                                            datetime < as.POSIXct('2018-01-01', tz='UTC'))),
       aes(x=datetime, y=value)) +
  facet_grid(variable ~. , scales = 'free_y') +
  geom_point() +
  labs(title='hobo do/temp 2018', x='date', y='temp (deg C)') +
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme

ggplot(subset(hobo_do_w17_vert, subset = (datetime >= as.POSIXct('2018-01-01', tz='UTC') &
                                            datetime < as.POSIXct('2018-02-01', tz='UTC'))),
       aes(x=datetime, y=value)) +
  facet_grid(variable ~. , scales = 'free_y') +
  geom_point() +
  labs(title='hobo do/temp 2018', x='date', y='temp (deg C)') +
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme


ggplot(subset(hobo_do_w17_vert, subset = (datetime >= as.POSIXct('2018-02-01', tz='UTC') &
                                            datetime < as.POSIXct('2018-03-01', tz='UTC'))),
       aes(x=datetime, y=value)) +
  facet_grid(variable ~. , scales = 'free_y') +
  geom_point() +
  labs(title='hobo do/temp 2018', x='date', y='temp (deg C)') +
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme

#flag feb 20-21
ggplot(subset(hobo_do_w17_vert, subset = (datetime >= as.POSIXct('2018-02-20', tz='UTC') &
                                            datetime < as.POSIXct('2018-02-21', tz='UTC'))),
       aes(x=datetime, y=value)) +
  facet_grid(variable ~. , scales = 'free_y') +
  geom_point() +
  labs(title='hobo do/temp 2018', x='date', y='temp (deg C)') +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme

ggplot(subset(hobo_do_w17_vert, subset = (datetime >= as.POSIXct('2018-02-21', tz='UTC') &
                                            datetime < as.POSIXct('2018-02-22', tz='UTC'))),
       aes(x=datetime, y=value)) +
  facet_grid(variable ~. , scales = 'free_y') +
  geom_point() +
  labs(title='hobo do/temp 2018', x='date', y='temp (deg C)') +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme

hobo_w17_L1 <- hobo_w17_L1 %>% 
  mutate(do_flag = case_when(datetime >= as.POSIXct('2018-02-20 11:00', tz='UTC') &
                              datetime < as.POSIXct('2018-02-21 18:00', tz='UTC')~ 'e',
                            TRUE ~ NA_character_))

hobo_do_w17 <- hobo_w17_L1 %>% 
  select(datetime, TempC_1m, do_ppm, do_flag)

hobo_do_w17_vert <- hobo_do_w17 %>% 
  gather(variable, value, - datetime, -do_flag)

ggplot(subset(hobo_do_w17_vert, subset = (datetime >= as.POSIXct('2018-02-01', tz='UTC') &
                                            datetime < as.POSIXct('2018-03-01', tz='UTC'))),
       aes(x=datetime, y=value, color = do_flag)) +
  facet_grid(variable ~. , scales = 'free_y') +
  geom_point() +
  labs(title='hobo do/temp 2018', x='date', y='temp (deg C)') +
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme

ggplot(subset(hobo_do_w17_vert, subset = (datetime >= as.POSIXct('2018-03-01', tz='UTC') &
                                            datetime < as.POSIXct('2018-04-01', tz='UTC'))),
       aes(x=datetime, y=value)) +
  facet_grid(variable ~. , scales = 'free_y') +
  geom_point() +
  labs(title='hobo do/temp 2018', x='date', y='temp (deg C)') +
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme

#mar 12
ggplot(subset(hobo_do_w17_vert, subset = (datetime >= as.POSIXct('2018-03-12', tz='UTC') &
                                            datetime < as.POSIXct('2018-03-13', tz='UTC'))),
       aes(x=datetime, y=value)) +
  facet_grid(variable ~. , scales = 'free_y') +
  geom_point() +
  labs(title='hobo do/temp 2018', x='date', y='temp (deg C)') +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme

#mar 30
ggplot(subset(hobo_do_w17_vert, subset = (datetime >= as.POSIXct('2018-03-30', tz='UTC') &
                                            datetime < as.POSIXct('2018-03-31', tz='UTC'))),
       aes(x=datetime, y=value)) +
  facet_grid(variable ~. , scales = 'free_y') +
  geom_point() +
  labs(title='hobo do/temp 2018', x='date', y='temp (deg C)') +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme


hobo_w17_L1 <- hobo_w17_L1 %>% 
  mutate(do_ppm = case_when(datetime >= as.POSIXct('2018-03-12 7:00', tz='UTC') &
                               datetime < as.POSIXct('2018-03-12 8:20', tz='UTC')~ NA_real_,
                             TRUE ~ do_ppm)) %>% 
  mutate(do_flag = case_when(datetime >= as.POSIXct('2018-03-30 12:15', tz='UTC') &
                               datetime < as.POSIXct('2018-03-30 15:15', tz='UTC')~ 'e',
                             TRUE ~ NA_character_))


hobo_do_w17 <- hobo_w17_L1 %>% 
  select(datetime, TempC_1m, do_ppm, do_flag)

hobo_do_w17_vert <- hobo_do_w17 %>% 
  gather(variable, value, - datetime, -do_flag)

ggplot(subset(hobo_do_w17_vert, subset = (datetime >= as.POSIXct('2018-03-01', tz='UTC') &
                                            datetime < as.POSIXct('2018-04-01', tz='UTC'))),
       aes(x=datetime, y=value, color = do_flag)) +
  facet_grid(variable ~. , scales = 'free_y') +
  geom_point() +
  labs(title='hobo do/temp 2018', x='date', y='temp (deg C)') +
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme


ggplot(subset(hobo_do_w17_vert, subset = (datetime >= as.POSIXct('2018-04-01', tz='UTC') &
                                            datetime < as.POSIXct('2018-05-01', tz='UTC'))),
       aes(x=datetime, y=value)) +
  facet_grid(variable ~. , scales = 'free_y') +
  geom_point() +
  labs(title='hobo do/temp 2018', x='date', y='temp (deg C)') +
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme

ggplot(subset(hobo_do_w17_vert, subset = (datetime >= as.POSIXct('2018-05-01', tz='UTC') &
                                            datetime < as.POSIXct('2018-06-01', tz='UTC'))),
       aes(x=datetime, y=value)) +
  facet_grid(variable ~. , scales = 'free_y') +
  geom_point() +
  labs(title='hobo do/temp 2018', x='date', y='temp (deg C)') +
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme

ggplot(subset(hobo_do_w17_vert, subset = (datetime >= as.POSIXct('2018-05-19', tz='UTC') &
                                            datetime < as.POSIXct('2018-05-20', tz='UTC'))),
       aes(x=datetime, y=value)) +
  facet_grid(variable ~. , scales = 'free_y') +
  geom_point() +
  labs(title='hobo do/temp 2018', x='date', y='temp (deg C)') +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme


#plot all 
hobo_w17_L1 %>% 
  gather(variable, value, -datetime, -do_flag) %>% 
  ggplot(.,
       aes(x=datetime, y=value, color = do_flag)) +
  facet_grid(variable ~. , scales = 'free_y') +
  geom_point() +
  labs(title='hobo do/temp 2017-2018', x='date') +
  scale_x_datetime(date_minor_breaks = '1 month') +
  final_theme
  

  
#export L1 tempstring file
hobo_w17_L1 %>% 
  select(datetime, TempC_1m, TempC_2m, TempC_3m, TempC_4m, TempC_5m, TempC_6m, TempC_8m, TempC_9m, TempC_10m, do_ppm) %>% 
  rename(TempC_1p5m = 'TempC_1m',
         TempC_2p5m = 'TempC_2m',
         TempC_3p5m = 'TempC_3m',
         TempC_4p5m = 'TempC_4m',
         TempC_5p5m = 'TempC_5m',
         TempC_6p5m = 'TempC_6m',
         TempC_8p5m = 'TempC_8m',
         TempC_9p5m = 'TempC_9m',
         TempC_10p5m = 'TempC_10m') %>%
  mutate(datetime = as.character(datetime)) %>% 
  write_csv(., 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2017-2018_hobotempstringdo_L1.csv')

#clean up workspace
rm(hobo_vert)




