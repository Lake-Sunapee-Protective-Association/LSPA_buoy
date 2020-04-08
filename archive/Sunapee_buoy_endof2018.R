#*****************************************************************
#*      Cary Institute of Ecosystem Studies (Millbrook, NY)      *
#*                                                               *
#* TITLE:   Sunapee_buoy_end2018.r                               *
#* AUTHOR:  Bethel Steele                                        *
#* SYSTEM:  Lenovo ThinkCentre, Win 10, R 3.5.2, RStudio 1.1.383 *
#* DATE:    10Jun2018                                            *
#* PROJECT: Lake Sunapee Buoy Data Cleaning                      *
#* PURPOSE: create L0 and L1 data for buoy data 2018 using       *
#*          similar methods to CCC and DR                        *
#*****************************************************************


#bring in  buoy raw data
buoy2018 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L0/2018 Buoy Data.csv',
                     col_types = 'iiiinnnnnnnnnnnnnnnnnnnnnnnnnnnnnn')

# format data
buoy2018 <- buoy2018  %>%
  rename(Hr.Min = 'Hr/Min',
         DOLowTempC = 'DOLoTempC',
         AveWindSp = 'WindSpdAv',
         AveWindDir = 'WindVect',
         MaxWindSp = 'MaxWind') %>% 
  mutate(hour = Hr.Min%/%100,
         minutes = Hr.Min%%100,
         time = paste(hour, minutes, sep=':')) %>% #break out time from Hr.Min, create time column
  mutate(date = as.Date(paste(Day, Year, sep = '-'), format='%j-%Y'), #create date in ymd format
         datetime = as.POSIXct(paste(date, time, sep=' '), format='%Y-%m-%d %H:%M', tz='UTC')) %>%  #tibble forces to UTC, so coerce for our uses.
  select(-hour, -minutes, -Hr.Min, -Year, -Day, -time, -ArrayID) #remove unnecessary columns

#filter for data after October move
buoy2018 <- buoy2018 %>% 
  filter(datetime >= as.POSIXct('2018-10-19 11:30', tz='UTC'))

#join with buoy L1 from previous
buoy2018_L1a_temp <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2018_tempstring_L1.csv',
                              col_types = 'cnnnnnnnnnncc',
                              col_names = c('datetime', 'TempC_0m', 'TempC_1m', 'TempC_2m', 'TempC_3m', 'TempC_4m', 'TempC_5m', 
                                            'TempC_6m', 'TempC_7m', 'TempC_8m', 'TempC_9m', 'temp_flag', 'location'),
                              skip =1) %>% 
  mutate(datetime = as.POSIXct(datetime, tz='UTC'))
buoy2018_L1a_do <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2018_do_L1.csv',
                              col_types = 'cnnncc') %>% 
  mutate(datetime = as.POSIXct(datetime, tz='UTC'))
buoy2018_L1a_hobodo <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2018_hobodo_L1.csv',
                              col_types = 'cnnc') %>% 
  mutate(datetime = as.POSIXct(datetime, tz='UTC'))
buoy2018_L1a_air <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2018_airtemp_L1.csv',
                              col_types = 'cnc') %>% 
  mutate(datetime = as.POSIXct(datetime, tz='UTC'))
buoy2018_L1a_par <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2018_PAR_L1.csv',
                              col_types = 'cnc') %>% 
  mutate(datetime = as.POSIXct(datetime, tz='UTC'))
buoy2018_L1a_wind <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2018_wind_L1.csv',
                              col_types = 'cnnnnc') %>% 
  mutate(datetime = as.POSIXct(datetime, tz='UTC'))

buoy2018_L1a <- full_join(buoy2018_L1a_temp, buoy2018_L1a_do) %>% 
  full_join(., buoy2018_L1a_air) %>% 
  full_join(., buoy2018_L1a_par) %>% 
  full_join(., buoy2018_L1a_wind)

buoy2018_location <- buoy2018_L1a %>% 
  select(datetime, location)

buoy2018_L1 <- buoy2018_L1a %>% 
  filter(datetime < as.POSIXct('2018-10-19 11:30', tz='UTC')) %>% 
  select(-location) %>% 
  full_join(., buoy2018) %>% 
  full_join(., buoy2018_location) %>% 
  arrange(datetime)


####THERMISTERS####
buoy2018_therm_vert <- buoy2018_L1 %>% 
  select(datetime, alltemp2016) %>%
  gather(variable, value, -datetime)

ggplot(buoy2018_therm_vert, aes(x=datetime, y=value, color = variable)) +
  geom_point() +
  scale_x_datetime(date_minor_breaks = '1 month') +
  final_theme

buoy2018_L1 <- buoy2018_L1 %>% 
  mutate_at(vars(alltemp2016),
            funs(case_when(. == -6999 ~ NA_real_,
                           TRUE ~ .)))

buoy2018_therm_vert_L1 <- buoy2018_L1 %>% 
  select(datetime, alltemp2016) %>%
  gather(variable, value, -datetime)

ggplot(buoy2018_therm_vert_L1,
       aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  scale_x_datetime(date_minor_breaks = '1 month') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  final_theme 

buoy2018_therm_vert <- buoy2018_L1 %>% 
  select(datetime, alltemp2016) %>%
  gather(variable, value, -datetime)

ggplot(subset(buoy2018_therm_vert,
              subset=(datetime >= as.POSIXct('2018-10-01', tz='UTC') & datetime < as.POSIXct('2018-11-01', tz='UTC'))),
       aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  scale_x_datetime(date_minor_breaks = '1 day') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  final_theme

#buoy removal
ggplot(subset(buoy2018_therm_vert,
              subset=(datetime >= as.POSIXct('2018-10-19', tz='UTC') & datetime < as.POSIXct('2018-10-20', tz='UTC'))),
       aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  final_theme

buoy2018_L1 <- buoy2018_L1 %>% 
  mutate_at(vars(alltemp2016),
            funs(case_when(datetime >= as.POSIXct('2018-10-19 11:30', tz='UTC') ~ NA_real_,
                           TRUE ~ .))) 
buoy2018_therm_vert_L1 <- buoy2018_L1 %>% 
  select(datetime, alltemp2016) %>%
  gather(variable, value, -datetime)

ggplot(buoy2018_therm_vert_L1,
       aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  scale_x_datetime(date_minor_breaks = '1 month') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  final_theme 

rm(buoy2018_therm_vert, buoy2018_therm_vert_L1)



#### DO ####
buoy2018_do_vert <- buoy2018_L1 %>% 
  select(datetime, upDO, lowDO) %>%
  gather(variable, value, -datetime)

ggplot(buoy2018_do_vert, aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  scale_x_datetime(date_minor_breaks = '1 month') +
  final_theme

ggplot(subset(buoy2018_do_vert,
              subset=(datetime >= as.POSIXct('2018-10-19', tz='UTC') & datetime < as.POSIXct('2018-10-20', tz='UTC'))),
       aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  final_theme

buoy2018_L1 <- buoy2018_L1 %>% 
  mutate(DOppm = NA_real_,
         DOSat = NA_real_,
         DOTempC = NA_real_) %>% 
  mutate_at(vars(lowDO),
            funs(case_when(datetime >= as.POSIXct('2018-10-19 11:30', tz='UTC') ~ NA_real_,
                           TRUE ~ .)))

buoy2018_do_vert_L1 <- buoy2018_L1 %>% 
  select(datetime, upDO, lowDO) %>%
  gather(variable, value, -datetime)

ggplot(buoy2018_do_vert_L1, aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  scale_x_datetime(date_minor_breaks = '1 month') +
  final_theme



####wind####
buoy_wind_vert <- buoy2018_L1 %>%
  select(datetime, AveWindDir, AveWindSp, MaxWindDir, MaxWindSp) %>%
  gather(variable, value, -datetime)

ggplot(buoy_wind_vert,
       aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title = 'wind 2018, raw') +
  scale_x_datetime(date_minor_breaks = '1 month') +
  final_theme +
  scale_color_colorblind()

ggplot(subset(buoy_wind_vert,
              subset=(datetime >= as.POSIXct('2018-10-01', tz='UTC') & datetime < as.POSIXct('2018-11-01', tz='UTC'))),
       aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title = 'oct wind 2018') +
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme +
  scale_color_colorblind()

ggplot(subset(buoy_wind_vert,
              subset=(datetime >= as.POSIXct('2018-10-19', tz='UTC') & datetime < as.POSIXct('2018-10-20', tz='UTC'))),
       aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title = 'buoy move') +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme +
  scale_color_colorblind()

buoy2018_L1 <- buoy2018_L1 %>% 
  mutate(location = case_when(datetime >= as.POSIXct('2018-10-19 10:30', tz='UTC') & datetime < as.POSIXct('2018-10-19 11:40', tz='UTC') ~ 'in transit',
                              datetime >= as.POSIXct('2018-10-19 11:40', tz='UTC') ~ 'harbor',
                              TRUE ~ location)) %>% 
  mutate_at(vars(AveWindDir, AveWindSp, MaxWindDir, MaxWindSp),
            funs(case_when(location == 'in transit' ~ NA_real_,
                           TRUE ~ .)))

buoy_wind_vert_L1 <- buoy2018_L1 %>% 
  select(datetime, AveWindDir, AveWindSp, MaxWindDir, MaxWindSp, location) %>% 
  gather(variable, value, -datetime, -location)

ggplot(subset(buoy_wind_vert_L1,
              subset=(datetime >= as.POSIXct('2018-10-19', tz='UTC') & datetime < as.POSIXct('2018-10-20', tz='UTC'))),
       aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title = 'buoy move') +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme +
  scale_color_colorblind()


ggplot(subset(buoy_wind_vert,
              subset=(datetime >= as.POSIXct('2018-11-01', tz='UTC') & datetime < as.POSIXct('2018-12-01', tz='UTC'))),
       aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title = 'nov wind 2018') +
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme +
  scale_color_colorblind()

#sensor frozen nov 20- nov22
ggplot(subset(buoy_wind_vert_L1,
              subset=(datetime >= as.POSIXct('2018-11-20', tz='UTC') & datetime < as.POSIXct('2018-11-21', tz='UTC'))),
       aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme +
  scale_color_colorblind()
ggplot(subset(buoy_wind_vert_L1,
              subset=(datetime >= as.POSIXct('2018-11-22', tz='UTC') & datetime < as.POSIXct('2018-11-23', tz='UTC'))),
       aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme +
  scale_color_colorblind()

#frozen nov 27-28
ggplot(subset(buoy_wind_vert_L1,
              subset=(datetime >= as.POSIXct('2018-11-27', tz='UTC') & datetime < as.POSIXct('2018-11-28', tz='UTC'))),
       aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme +
  scale_color_colorblind()
ggplot(subset(buoy_wind_vert_L1,
              subset=(datetime >= as.POSIXct('2018-11-28', tz='UTC') & datetime < as.POSIXct('2018-11-29', tz='UTC'))),
       aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme +
  scale_color_colorblind()

buoy2018_L1 <- buoy2018_L1 %>% 
  mutate_at(vars(AveWindDir, AveWindSp, MaxWindDir, MaxWindSp),
            funs(case_when(datetime >= as.POSIXct('2018-11-20 7:00', tz='UTC') & datetime < as.POSIXct('2018-11-22 9:50', tz='UTC') ~ NA_real_,
                           datetime >= as.POSIXct('2018-11-26', tz='UTC') & datetime < as.POSIXct('2018-11-29', tz='UTC') & MaxWindSp == 0 ~ NA_real_,
                           TRUE ~ .)))

buoy_wind_vert_L1 <- buoy2018_L1 %>% 
  select(datetime, AveWindDir, AveWindSp, MaxWindDir, MaxWindSp) %>% 
  gather(variable, value, -datetime)

ggplot(subset(buoy_wind_vert_L1,
              subset=(datetime >= as.POSIXct('2018-11-01', tz='UTC') & datetime < as.POSIXct('2018-12-01', tz='UTC'))),
       aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title = 'nov wind 2018') +
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme +
  scale_color_colorblind()


ggplot(subset(buoy_wind_vert,
              subset=(datetime >= as.POSIXct('2018-12-01', tz='UTC') & datetime < as.POSIXct('2019-01-01', tz='UTC'))),
       aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title = 'dec wind 2018') +
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme +
  scale_color_colorblind()

#frozen28-29
ggplot(subset(buoy_wind_vert,
              subset=(datetime >= as.POSIXct('2018-12-28', tz='UTC') & datetime < as.POSIXct('2018-12-29', tz='UTC'))),
       aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title = 'dec wind 2018') +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme +
  scale_color_colorblind()
ggplot(subset(buoy_wind_vert,
              subset=(datetime >= as.POSIXct('2018-12-29', tz='UTC') & datetime < as.POSIXct('2018-12-30', tz='UTC'))),
       aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title = 'dec wind 2018') +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme +
  scale_color_colorblind()

buoy2018_L1 <- buoy2018_L1 %>% 
  mutate_at(vars(AveWindDir, AveWindSp, MaxWindDir, MaxWindSp),
            funs(case_when(datetime >= as.POSIXct('2018-12-28', tz='UTC') & datetime < as.POSIXct('2018-12-30', tz='UTC') & MaxWindSp == 0 ~ NA_real_,
                           TRUE ~ .)))

buoy_wind_vert_L1 <- buoy2018_L1 %>% 
  select(datetime, AveWindDir, AveWindSp, MaxWindDir, MaxWindSp, location) %>% 
  gather(variable, value, -datetime, -location)

ggplot(subset(buoy_wind_vert_L1,
              subset=(datetime >= as.POSIXct('2018-12-01', tz='UTC') & datetime < as.POSIXct('2019-01-01', tz='UTC'))),
       aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title = 'dec wind 2018') +
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme +
  scale_color_colorblind()


ggplot(buoy_wind_vert_L1,
       aes(x=datetime, y=value, color=location)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title = 'wind 2018, clean') +
  scale_x_datetime(date_minor_breaks = '1 month') +
  final_theme +
  scale_color_colorblind()

rm(buoy_wind_vert, buoy_wind_vert_L1)

####PAR####
ggplot(buoy2018_L1,
       aes(x=datetime, y=PAR, color=location)) +
  geom_point() +
  labs(title = 'PAR 2018, raw') +
  scale_x_datetime(date_minor_breaks = '1 month') +
  final_theme +
  scale_color_colorblind()

#recode when in transit or offline
buoy2018_L1 <- buoy2018_L1 %>% 
  mutate(PAR = case_when(location == 'in transit' ~ NA_real_,
                         PAR < 0 ~ 0,
                         TRUE ~ PAR)) 
ggplot(buoy2018_L1,
       aes(x=datetime, y=PAR, color=location)) +
  geom_point() +
  labs(title = 'PAR 2018, NA recoded') +
  scale_x_datetime(date_minor_breaks = '1 month') +
  final_theme +
  scale_color_colorblind()

ggplot(subset(buoy2018_L1,
              subset=(datetime >= as.POSIXct('2018-10-01', tz='UTC') & datetime < as.POSIXct('2018-11-01', tz='UTC'))),
       aes(x=datetime, y=PAR, color=location)) +
  geom_point() +
  labs(title = 'PAR 2018, NA recoded') +
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme +
  scale_color_colorblind()

ggplot(subset(buoy2018_L1,
              subset=(datetime >= as.POSIXct('2018-11-01', tz='UTC') & datetime < as.POSIXct('2018-12-01', tz='UTC'))),
       aes(x=datetime, y=PAR, color=location)) +
  geom_point() +
  labs(title = 'PAR 2018, NA recoded') +
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme +
  scale_color_colorblind()

ggplot(subset(buoy2018_L1,
              subset=(datetime >= as.POSIXct('2018-12-01', tz='UTC') & datetime < as.POSIXct('2019-01-01', tz='UTC'))),
       aes(x=datetime, y=PAR, color=location)) +
  geom_point() +
  labs(title = 'PAR 2018, NA recoded') +
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme +
  scale_color_colorblind()

#from jul 23 noon until end of year, PAR sometimes not correct at night - registering as PAR >0, which is not possible. Flagging as 'n'
buoy2018_L1 <- buoy2018_L1 %>% 
  mutate(PAR_flag = case_when(datetime >= as.POSIXct('2018-07-23 12:00') ~ 'n',
                         TRUE ~ NA_character_)) 



#### Air temp ####
# ggplot(buoy2018_L1,
#        aes(x=datetime, y=AirTempC, color=location)) +
#   geom_point() +
#   labs(title = 'air temp 2018, NA recoded') +
#   scale_x_datetime(date_minor_breaks = '1 month') +
#   final_theme +
#   scale_color_colorblind()

#recode when in transit or offline
buoy2018_L1 <- buoy2018_L1 %>% 
  mutate(AirTempC = case_when(location == 'in transit' ~ NA_real_,
                         TRUE ~ AirTempC))

ggplot(buoy2018_L1,
       aes(x=datetime, y=AirTempC, color=location)) +
  geom_point() +
  labs(title = 'air temp 2018, NA recoded') +
  scale_x_datetime(date_minor_breaks = '1 month') +
  final_theme +
  scale_color_colorblind()




#### EXPORT L1 DATA STREAMS ####
#export L1 tempstring file
buoy2018_L1 %>%
  select(datetime, TempC_0m, TempC_1m, TempC_2m, TempC_3m, TempC_4m, TempC_5m, TempC_6m, TempC_7m, TempC_8m, TempC_9m, temp_flag, location) %>%
  rename(TempC_9p5m = 'TempC_9m',
         TempC_8p5m = 'TempC_8m',
         TempC_7p5m = 'TempC_7m',
         TempC_6p5m = 'TempC_6m',
         TempC_5p5m = 'TempC_5m',
         TempC_4p5m = 'TempC_4m',
         TempC_3p5m = 'TempC_3m',
         TempC_2p5m = 'TempC_2m',
         TempC_1p5m = 'TempC_1m',
         TempC_0p5m = 'TempC_0m') %>%
  mutate(datetime = as.character(datetime)) %>%
  write_csv(., 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2018_tempstring_full_L1.csv')

#export l1 do file
buoy2018_L1 %>%
  select(datetime, lowDO, lowDO_flag, location) %>%
  mutate(datetime = as.character(datetime)) %>%
  write_csv(., 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2018_do_full_L1.csv')

#export l1 hobo do file
hobo_updo_L1 %>%
  mutate(datetime = as.character(datetime)) %>%
  write_csv(., 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2018_hobodo_L1.csv')

#export l1 par file
buoy2018_L1 %>%
  select(datetime, PAR, PAR_flag, location) %>%
  mutate(datetime = as.character(datetime)) %>%
  write_csv(., 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2018_PAR_L1.csv')

#export l1 wind
buoy2018_L1 %>%
  select(datetime, AveWindSp, AveWindDir, MaxWindSp, MaxWindDir, location) %>%
  mutate(datetime = as.character(datetime)) %>%
  write_csv(., 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2018_wind_L1.csv')

#export l1 air temp file
buoy2018_L1 %>%
  select(datetime, AirTempC, location) %>%
  mutate(datetime = as.character(datetime)) %>%
  write_csv(., 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2018_airtemp_L1.csv')


rm(alltimes_2018)


