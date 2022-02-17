#*****************************************************************
#*      Cary Institute of Ecosystem Studies (Millbrook, NY)      *
#*                                                               *
#* TITLE:   Sunapee_buoy_2013.r                                  *
#* AUTHOR:  Bethel Steele                                        *
#* PROJECT: Lake Sunapee Buoy Data Cleaning                      *
#*****************************************************************

source('library_func_lists.R')

#point to data directories
raw_dir = 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L0/'
dump_dir = 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/'

#set tz
buoy_tz = 'Etc/GMT+5'

#bring in 2013 buoy raw data
buoy2013_L0 <- read.csv(file.path(raw_dir, 'Sunapee2013_rawData.csv'),
                        col.names = c('station', 'year', 'day', 'hr_min', 'DOTempC', 'DOSat',
                                      'DOppm', 'TempC_0m', 'TempC_1m', 'TempC_2m', 'TempC_3m', 'TempC_4m',
                                      'TempC_5m', 'TempC_6m', 'TempC_7m', 'TempC_8m', 'TempC_9m',
                                      'AirTempC', 'RH', 'PAR', 'WindSp', 
                                      'CorrWind', 'AveWindSp', 'AveWindDir', 'MaxWindSp', 'MaxWindDir', 
                                      'LoggerBat', 'RadioBat', 'IntTemp', 'Heading', 'UnkDir', 
                                      'DOLowTempC', 'DOLowSat','DOLowPPM', 'Date', 'Time', 
                                      'datetime', 'diff', 'gap'),
                        skip=1) %>%
  select(-station, -year, -day, -hr_min, -IntTemp, Heading, -Date, -Time, -diff, -gap) %>%  #drop redundant columns
  mutate(datetime = as.POSIXct(datetime, format='%Y-%m-%d %H:%M', tz=buoy_tz))

#look at radio battery
ggplot(buoy2013_L0, aes(x = datetime, y = RadioBat)) +
  geom_point()
#early record battery is low; will see where it shows up.

#double check to make sure there are no DST issues
datelength2013 <- buoy2013_L0 %>% 
  mutate(date = format(datetime, '%Y-%m-%d')) %>% 
  group_by(date) %>% 
  summarize(length(datetime))

#check dst dates
datelength2013[datelength2013$date == '2013-03-10',]
#dst observed here
datelength2013[datelength2013$date == '2013-11-03',]
#offline

#check neighboring dates
datelength2013[datelength2013$date == '2013-11-02',]
datelength2013[datelength2013$date == '2013-11-04',]

#force into NYtz with dst; convert to utc-5
buoy2013_L1 <- buoy2013_L0 %>% 
  mutate(datetime_instrument = force_tz(datetime, tz = 'America/New_York'),
         datetime = with_tz(datetime_instrument, tz = buoy_tz))

#check again
datelength2013 <- buoy2013_L1 %>% 
  mutate(date = format(datetime, '%Y-%m-%d')) %>% 
  group_by(date) %>%
  summarize(length(datetime))
datelength2013[datelength2013$date == '2013-03-10',]

#time change is in the wrong location.

#grab time before wrong time change and shift data
buoy2013_L1a <- buoy2013_L0 %>% 
  filter(datetime < as.POSIXct('2013-03-10 02:00', tz = buoy_tz))%>% 
  mutate(datetime_instrument = force_tz(datetime, tz = 'America/New_York'),
         datetime = with_tz(datetime_instrument, tz = buoy_tz))
#grab time that doesn't need a change
buoy2013_L1b <- buoy2013_L0 %>% 
  filter(datetime >= as.POSIXct('2013-03-10 02:00', tz = buoy_tz) & 
           datetime <=as.POSIXct('2013-03-10 23:00', tz = buoy_tz))%>% 
  mutate(datetime_instrument = datetime)
#grab time after wrong time change and shift data
buoy2013_L1c <- buoy2013_L0 %>% 
  filter(datetime>= as.Date('2013-03-11'))%>% 
  mutate(datetime_instrument = force_tz(datetime, tz = 'America/New_York'),
         datetime = with_tz(datetime_instrument, tz = buoy_tz))

#join two together
buoy2013_L1 <- full_join(buoy2013_L1a, buoy2013_L1b) %>% 
  full_join(., buoy2013_L1c)

#join up and check work
datelength2013 <- buoy2013_L1 %>% 
  mutate(date = format(datetime, '%Y-%m-%d')) %>% 
  group_by(date) %>%
  summarize(length(datetime))
datelength2013[datelength2013$date == '2013-03-10',]


#create dummy timestamp so there are no blanks
alltimes_2013 <- as.data.frame(seq.POSIXt(as.POSIXct('2013-01-01 00:00', tz=buoy_tz), as.POSIXct('2013-12-31 23:50', tz=buoy_tz), '10 min')) %>% 
  rename("datetime" = !!names(.[1]))

buoy2013_L1 <- buoy2013_L1 %>% 
  right_join(., alltimes_2013) %>% 
  arrange(datetime)

#clean up workspace
rm(datelength2013, alltimes_2013, buoy2013_L1a, buoy2013_L1b, buoy2013_L1c)


#### thermistors ####
buoy2013_temp_vert <- buoy2013_L1 %>%
  select(datetime, all_of(alltemp2011)) %>%
  pivot_longer(names_to = 'variable', values_to = 'value', -datetime) %>% 
  mutate(variable = factor(variable, levels=alltemp2011))

# ggplot(subset(buoy2013_temp_vert,
#               subset=(datetime >=as.POSIXct('2013-01-01', tz=buoy_tz) &
#                         datetime < as.POSIXct('2014-01-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   final_theme +
# labs(title = '2013 buoy temp data, raw',
#      x=NULL,
#      y='temp ((deg C))') +
#   scale_x_datetime(date_minor_breaks = '1 month') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust

buoy2013_L1 <- buoy2013_L1 %>%
  mutate_at(vars(all_of(alltemp2011)),
            ~(case_when(. == -6999 ~ NA_real_,
                           . == 555.4 ~ NA_real_,
                           TRUE ~ .))) %>%
  mutate(location = 'loon')

buoy2013_temp_vert <- buoy2013_L1 %>%
  select(datetime, all_of(alltemp2011)) %>%
  pivot_longer(names_to = 'variable', values_to = 'value', -datetime) %>% 
  mutate(variable = factor(variable, levels=alltemp2011))

ggplot(subset(buoy2013_temp_vert,
              subset=(datetime >=as.POSIXct('2013-01-01', tz=buoy_tz) &
                        datetime < as.POSIXct('2014-01-01', tz=buoy_tz))),
       aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  final_theme +
  labs(title = '2013 buoy temp data, NAs recoded',
       x=NULL,
       y='temp (deg C)') +
  scale_x_datetime(date_minor_breaks = '1 month') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
                              "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust

# ggplot(subset(buoy2013_temp_vert,
#               subset=(datetime >=as.POSIXct('2013-05-01', tz=buoy_tz) &
#                         datetime < as.POSIXct('2013-06-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   final_theme +
#   labs(title='May 2013, NAs recoded',
#        x=NULL,
#        y='temp (deg C)') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# #May 15 buoy deployed
# ggplot(subset(buoy2013_temp_vert,
#               subset=(datetime >=as.POSIXct('2013-05-15', tz=buoy_tz) &
#                         datetime < as.POSIXct('2013-05-16', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   final_theme +
#   labs(title='May 2013, NAs recoded',
#        x=NULL,
#        y='temp (deg C)') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust

buoy2013_L1 <- buoy2013_L1 %>%
  mutate_at(vars(alltemp2011),
            ~(case_when(datetime < as.POSIXct('2013-05-15 11:00', tz=buoy_tz) ~ NA_real_,
                           TRUE ~ .))) 

buoy2013_temp_vert_b <- buoy2013_L1 %>%
  select(datetime, all_of(alltemp2011)) %>%
  pivot_longer(names_to = 'variable', values_to = 'value', -datetime) %>% 
  mutate(variable = factor(variable, levels=alltemp2011))

# ggplot(subset(buoy2013_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2013-05-01', tz=buoy_tz) &
#                         datetime < as.POSIXct('2013-06-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   final_theme +
#   labs(title='May 2013, clean',
#        x=NULL,
#        y='temp (deg C)') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# ggplot(subset(buoy2013_temp_vert,
#               subset=(datetime >=as.POSIXct('2013-06-01', tz=buoy_tz) &
#                         datetime < as.POSIXct('2013-07-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   final_theme +
#   labs(title='June 2013, NAs recoded',
#        x=NULL,
#        y='temp (deg C)') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# ggplot(subset(buoy2013_temp_vert,
#               subset=(datetime >=as.POSIXct('2013-07-01', tz=buoy_tz) &
#                         datetime < as.POSIXct('2013-08-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   final_theme +
#   labs(title='July 2013, NAs recoded',
#        x=NULL,
#        y='temp (deg C)') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# ggplot(subset(buoy2013_temp_vert,
#               subset=(datetime >=as.POSIXct('2013-08-01', tz=buoy_tz) &
#                         datetime < as.POSIXct('2013-09-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   final_theme +
#   labs(title='Aug 2013, NAs recoded',
#        x=NULL,
#        y='temp (deg C)') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# #Aug 12 anomolous, errors starting 15 through sept 10
# ggplot(subset(buoy2013_temp_vert,
#               subset=(datetime >=as.POSIXct('2013-08-12', tz=buoy_tz) &
#                         datetime < as.POSIXct('2013-08-13', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   final_theme +
#   labs(title='Aug 2013, NAs recoded',
#        x=NULL,
#        y='temp (deg C)') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# ggplot(subset(buoy2013_temp_vert,
#               subset=(datetime >=as.POSIXct('2013-08-15', tz=buoy_tz) &
#                         datetime < as.POSIXct('2013-08-16', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   final_theme +
#   labs(title='Aug 2013, NAs recoded',
#        x=NULL,
#        y='temp (deg C)') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# ggplot(subset(buoy2013_temp_vert,
#               subset=(datetime >=as.POSIXct('2013-09-10', tz=buoy_tz) &
#                         datetime < as.POSIXct('2013-09-11', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   final_theme +
#   labs(title='Sept
#        2013, NAs recoded',
#        x=NULL,
#        y='temp (deg C)') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# ggplot(subset(buoy2013_temp_vert,
#               subset=(datetime >=as.POSIXct('2013-09-01', tz=buoy_tz) &
#                         datetime < as.POSIXct('2013-10-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   final_theme +
#   labs(title='Sept 2013, NAs recoded',
#        x=NULL,
#        y='temp (deg C)') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust


buoy2013_L1 <- buoy2013_L1 %>%
  mutate_at(vars(TempC_6m:TempC_9m),
            ~(case_when(datetime >= as.POSIXct('2013-08-12 15:40', tz=buoy_tz) & datetime < as.POSIXct('2013-08-12 16:00', tz=buoy_tz) ~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate_at(vars(all_of(alltemp2011)),
            ~(case_when(datetime >= as.POSIXct('2013-08-15 8:00', tz=buoy_tz) & datetime < as.POSIXct('2013-09-10 10:50', tz=buoy_tz) ~ NA_real_,
                           TRUE ~ . )))

buoy2013_temp_vert_b <- buoy2013_L1 %>%
  select(datetime, all_of(alltemp2011)) %>%
  pivot_longer(names_to = 'variable', values_to = 'value', -datetime) %>% 
  mutate(variable = factor(variable, levels=alltemp2011))

# ggplot(subset(buoy2013_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2013-08-01', tz=buoy_tz) &
#                         datetime < as.POSIXct('2013-09-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   final_theme +
#   labs(title='Aug 2013, clean',
#        x=NULL,
#        y='temp (deg C)') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# ggplot(subset(buoy2013_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2013-09-01', tz=buoy_tz) &
#                         datetime < as.POSIXct('2013-10-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   final_theme +
#   labs(title='Sept 2013, NAs recoded',
#        x=NULL,
#        y='temp (deg C)') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# ggplot(subset(buoy2013_temp_vert,
#               subset=(datetime >=as.POSIXct('2013-10-01', tz=buoy_tz) &
#                         datetime < as.POSIXct('2013-11-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   final_theme +
#   labs(title='Oct 2013, NAs recoded',
#        x=NULL,
#        y='temp (deg C)') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
#  
ggplot(subset(buoy2013_temp_vert_b),
       aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  final_theme +
  labs(title='2013, clean',
       x=NULL,
       y='temp (deg C)') +
  scale_x_datetime(date_minor_breaks = '1 month') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
                              "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust

#correct thermistor depth for offset; add CV
buoy2013_L1 <- buoy2013_L1 %>% 
  rename(waterTemperature_degC_9p5m = TempC_9m,
         waterTemperature_degC_8p5m = TempC_8m,
         waterTemperature_degC_7p5m = TempC_7m,
         waterTemperature_degC_6p5m = TempC_6m,
         waterTemperature_degC_5p5m = TempC_5m,
         waterTemperature_degC_4p5m = TempC_4m,
         waterTemperature_degC_3p5m = TempC_3m,
         waterTemperature_degC_2p5m = TempC_2m,
         waterTemperature_degC_1p5m = TempC_1m,
         waterTemperature_degC_0p5m = TempC_0m)

#clean up workspace
rm(buoy2013_temp_vert, buoy2013_temp_vert_b)


#### DO sensors ####
range(buoy2013_L1$DOSat, na.rm=T)
range(buoy2013_L1$DOppm, na.rm=T)
range(buoy2013_L1$DOTempC, na.rm=T)
range(buoy2013_L1$DOLowSat, na.rm=T)
range(buoy2013_L1$DOLowPPM, na.rm=T)
range(buoy2013_L1$DOLowTempC, na.rm=T)

#recode na strings
buoy2013_L1 <- buoy2013_L1 %>% 
  mutate_at(vars(DOTempC, DOLowSat, DOLowPPM, DOLowTempC),
            ~(case_when(. == -6999 ~ NA_real_,
                        TRUE ~ .))) %>% 
  mutate_at(vars(DOLowSat, DOLowPPM, DOLowTempC),
            ~(case_when(. == 0 ~ NA_real_,
                        TRUE ~ .))) %>% 
  mutate(flag_do1p5m = '',
         flag_do10p5m = '')

do_vert <- buoy2013_L1 %>% 
  select(datetime, DOSat, DOppm, DOTempC, DOLowSat, DOLowPPM, DOLowTempC) %>% 
  pivot_longer(names_to = 'variable', values_to = 'value', -datetime)

ggplot(do_vert, aes(x = datetime, y = value)) +
  geom_point() +
  facet_grid(variable~., scales = 'free_y') +
  final_theme +
  labs(title = '2013 DO data raw',
       x = NULL,
       y = NULL) +
  scale_x_datetime(date_minor_breaks = '1 month')

# ggplot(subset(do_vert,
#               subset = (datetime >= as.POSIXct('2013-01-01', tz=buoy_tz) &
#                           datetime < as.POSIXct('2013-02-01', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'jan 2013 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(do_vert,
#               subset = (datetime >= as.POSIXct('2013-02-01', tz=buoy_tz) &
#                           datetime < as.POSIXct('2013-03-01', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'feb 2013 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(do_vert,
#               subset = (datetime >= as.POSIXct('2013-03-01', tz=buoy_tz) &
#                           datetime < as.POSIXct('2013-04-01', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'mar 2013 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(do_vert,
#               subset = (datetime >= as.POSIXct('2013-04-01', tz=buoy_tz) &
#                           datetime < as.POSIXct('2013-05-01', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'apr 2013 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(do_vert,
#               subset = (datetime >= as.POSIXct('2013-05-01', tz=buoy_tz) &
#                           datetime < as.POSIXct('2013-06-01', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'may 2013 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# #buoy move to loon may 15
# ggplot(subset(do_vert,
#               subset = (datetime >= as.POSIXct('2013-05-15', tz=buoy_tz) &
#                           datetime < as.POSIXct('2013-05-16', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'may 2013 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 hour')
# 
buoy2013_L1 <- buoy2013_L1 %>% 
  mutate_at(vars(DOSat, DOppm, DOTempC),
            ~(case_when(datetime >= as.POSIXct('2013-05-15 8:40', tz=buoy_tz) &
                             datetime < as.POSIXct('2013-05-15 10:50', tz=buoy_tz) ~ NA_real_,
                           TRUE ~ .))) %>%
  mutate(location = case_when(datetime < as.POSIXct('2013-05-15 8:40', tz=buoy_tz) ~'harbor',
                              datetime >= as.POSIXct('2013-05-15 8:40', tz=buoy_tz) &
                                datetime < as.POSIXct('2013-05-15 10:50', tz=buoy_tz) ~ 'in transit',
                              TRUE ~ location),
         flag_do1p5m = case_when(datetime == as.POSIXct('2013-05-15 10:40', tz=buoy_tz) ~ 'wp',
                                 TRUE ~ flag_do1p5m))

do_vert_b <- buoy2013_L1 %>% 
  select(datetime, location, DOSat, DOppm, DOTempC, DOLowSat, DOLowPPM, DOLowTempC) %>% 
  pivot_longer(names_to = 'variable', values_to = 'value', -c(datetime, location))

# ggplot(subset(do_vert_b,
#               subset = (datetime >= as.POSIXct('2013-05-01', tz=buoy_tz) &
#                           datetime < as.POSIXct('2013-06-01', tz=buoy_tz))),
#        aes(x = datetime, y = value, color=location)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'may 2013 DO data clean',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(do_vert,
#               subset = (datetime >= as.POSIXct('2013-06-01', tz=buoy_tz) &
#                           datetime < as.POSIXct('2013-07-01', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'jun 2013 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(do_vert,
#               subset = (datetime >= as.POSIXct('2013-07-01', tz=buoy_tz) &
#                           datetime < as.POSIXct('2013-08-01', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'jun 2013 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(do_vert,
#               subset = (datetime >= as.POSIXct('2013-08-01', tz=buoy_tz) &
#                           datetime < as.POSIXct('2013-09-01', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'aug 2013 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(do_vert,
#               subset = (datetime >= as.POSIXct('2013-09-01', tz=buoy_tz) &
#                           datetime < as.POSIXct('2013-10-01', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'sept 2013 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# #sept 10 low do online
# ggplot(subset(do_vert,
#               subset = (datetime >= as.POSIXct('2013-09-10', tz=buoy_tz) &
#                           datetime < as.POSIXct('2013-09-11', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'sept 2013 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 hour')
# 
# #sept 25 change to logger program for offset
# ggplot(subset(do_vert,
#               subset = (datetime >= as.POSIXct('2013-09-25', tz=buoy_tz) &
#                           datetime < as.POSIXct('2013-09-26', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'sept 2013 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 hour')

# add offset amount from logger
deepsat = -73.7
deepppm = -6.7
shalsat = -16.5
shalppm = -1.72

buoy2013_L1 <- buoy2013_L1 %>% 
  #save offset in column
  mutate(offval_do10p5_mgl = case_when(datetime >= as.POSIXct('2013-09-25 14:00', tz=buoy_tz) ~ deepppm,
                                       TRUE ~ 0),
         offval_do10p5_sat = case_when(datetime >= as.POSIXct('2013-09-25 14:00', tz=buoy_tz) ~ deepsat,
                                       TRUE ~ 0),
         offval_do1p5_mgl = case_when(datetime >= as.POSIXct('2013-09-25 14:00', tz=buoy_tz) ~ shalppm,
                                       TRUE ~ 0),
         offval_do1p5_sat = case_when(datetime >= as.POSIXct('2013-09-25 14:00', tz=buoy_tz) ~ shalsat,
                                       TRUE ~ 0)) %>% 
  #copy data from program to new column during offset period
  mutate(oxygenDissolved_mgl_1p5m_withoffval = case_when(datetime >=  as.POSIXct('2013-09-25 14:00', tz=buoy_tz)~ DOppm,
                                                         TRUE ~ NA_real_),
         oxygenDissolvedPercentOfSaturation_pct_1p5m_withoffval = case_when(datetime >= as.POSIXct('2013-09-25 14:00', tz=buoy_tz) ~ DOSat,
                                                                            TRUE ~ NA_real_),
         oxygenDissolved_mgl_10p5m_withoffval = case_when(datetime >= as.POSIXct('2013-09-25 14:00', tz=buoy_tz) ~ DOLowPPM,
                                                          TRUE ~ NA_real_),
         oxygenDissolvedPercentOfSaturation_pct_10p5m_withoffval = case_when(datetime >= as.POSIXct('2013-09-25 14:00', tz=buoy_tz) ~ DOLowSat,
                                                                             TRUE ~ NA_real_)) %>% 
  #add flags for calculated from offset
  mutate(flag_do1p5m = case_when(datetime >=  as.POSIXct('2013-09-25 14:00', tz=buoy_tz) ~ 'o',
                                 TRUE ~ flag_do1p5m)) %>%
  mutate(flag_do10p5m = case_when(datetime >=  as.POSIXct('2013-09-25 14:00', tz=buoy_tz) ~ 'o',
                                   TRUE ~ flag_do10p5m)) %>%  
  #calculate data without offset
  mutate(DOppm = case_when(flag_do1p5m == 'o' ~ oxygenDissolved_mgl_1p5m_withoffval - offval_do1p5_mgl,
                           TRUE~DOppm),
         DOSat = case_when(flag_do1p5m == 'o' ~oxygenDissolvedPercentOfSaturation_pct_1p5m_withoffval - offval_do1p5_sat,
                           TRUE~DOSat),
         DOLowPPM = case_when(flag_do10p5m == 'o' ~ oxygenDissolved_mgl_10p5m_withoffval - offval_do10p5_mgl,
                              TRUE~DOLowPPM),
         DOLowSat = case_when(flag_do10p5m == 'o'~oxygenDissolvedPercentOfSaturation_pct_10p5m_withoffval - offval_do10p5_sat,
                              TRUE~DOLowSat))

do_vert_b <- buoy2013_L1 %>% 
  select(datetime, location, DOSat, DOppm, DOTempC, DOLowSat, DOLowPPM, DOLowTempC) %>% 
  pivot_longer(names_to = 'variable', values_to = 'value', -c(datetime, location))

# ggplot(subset(do_vert_b,
#               subset = (datetime >= as.POSIXct('2013-09-01', tz=buoy_tz) &
#                           datetime < as.POSIXct('2013-10-01', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'sept 2013 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(do_vert,
#               subset = (datetime >= as.POSIXct('2013-10-01', tz=buoy_tz) &
#                           datetime < as.POSIXct('2013-11-01', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'oct 2013 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# #buoy moved to harbor oct 16
# ggplot(subset(do_vert,
#               subset = (datetime >= as.POSIXct('2013-10-16', tz=buoy_tz) &
#                           datetime < as.POSIXct('2013-10-17', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'oct 2013 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 hour')
# 
buoy2013_L1 <- buoy2013_L1 %>% 
  mutate_at(vars(lowDO, upDO),
            ~(case_when(datetime >= as.POSIXct('2013-10-16 8:10', tz=buoy_tz) &
                             datetime < as.POSIXct('2013-10-16 9:10', tz=buoy_tz) ~ NA_real_,
                           TRUE ~ .))) %>%
  mutate(location = case_when(datetime >= as.POSIXct('2013-10-16 8:10', tz=buoy_tz) &
                                datetime < as.POSIXct('2013-10-16 9:10', tz=buoy_tz) ~ 'in transit',
                              datetime >= as.POSIXct('2013-10-16 9:10', tz=buoy_tz) ~'harbor',
                             TRUE ~ location))

#add presumed cleaning flags
buoy2013_L1 <- buoy2013_L1 %>% 
  mutate(flag_do1p5m = case_when(datetime == as.POSIXct('2013-10-16 09:00') ~ 'o; wp',
                                   TRUE ~ flag_do1p5m)) 

do_vert_b <- buoy2013_L1 %>% 
  select(datetime, location, DOSat, DOppm, DOTempC, DOLowSat, DOLowPPM, DOLowTempC) %>% 
  gather(variable, value, -datetime, -location)

# ggplot(subset(do_vert_b,
#               subset = (datetime >= as.POSIXct('2013-10-01', tz=buoy_tz) &
#                           datetime < as.POSIXct('2013-11-01', tz=buoy_tz))),
#        aes(x = datetime, y = value, color = location)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'oct 2013 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 

buoy2013_L1 <- buoy2013_L1 %>% 
  mutate(location = case_when(datetime >= as.POSIXct('2013-10-23 11:00', tz=buoy_tz)  ~ 'offline',
                              TRUE ~ location))

# ggplot(do_vert_b, aes(x = datetime, y = value, color = location)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = '2013 DO data clean',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 month')

# add x flag for no calibration on record
buoy2013_L1 <- buoy2013_L1 %>% 
  mutate_at(vars(flag_do1p5m, flag_do10p5m),
            ~case_when(. == '' ~ 'x',
                       TRUE ~ paste('x', ., sep = '; ')))

do_vert_up <- buoy2013_L1 %>% 
  select(datetime, location,DOSat, DOppm, DOTempC, flag_do1p5m) %>% 
  pivot_longer(names_to = 'variable', values_to = 'value', -c(datetime, location, flag_do1p5m))

ggplot(do_vert_up, aes(x = datetime, y = value, color = flag_do1p5m, shape = location)) +
  geom_point() +
  facet_grid(variable~., scales = 'free_y') +
  final_theme +
  labs(title = '2013 DO data clean',
       x = NULL,
       y = NULL) +
  scale_x_datetime(date_minor_breaks = '1 month')

do_vert_lo <- buoy2013_L1 %>% 
  select(datetime, location, DOLowSat, DOLowPPM, DOLowTempC, flag_do10p5m) %>% 
  pivot_longer(names_to = 'variable', values_to = 'value', -c(datetime, location, flag_do10p5m))

ggplot(do_vert_lo, aes(x = datetime, y = value, color = flag_do10p5m, shape = location)) +
  geom_point() +
  facet_grid(variable~., scales = 'free_y') +
  final_theme +
  labs(title = '2013 DO data clean',
       x = NULL,
       y = NULL) +
  scale_x_datetime(date_minor_breaks = '1 month')

#clean up workspace
rm(do_vert, do_vert_b, do_vert_lo, do_vert_up)
 
#rename with CV
buoy2013_L1 <- buoy2013_L1 %>% 
  rename(oxygenDissolved_mgl_1p5m = DOppm,
         oxygenDissolvedPercentOfSaturation_pct_1p5m = DOSat,
         waterTemperature_DO_degC_1p5m = DOTempC,
         oxygenDissolved_mgl_10p5m = DOLowPPM,
         oxygenDissolvedPercentOfSaturation_pct_10p5m = DOLowSat,
         waterTemperature_DO_degC_10p5m = DOLowTempC)

# recode offset values where raw values have been recoded
buoy2013_L1 <- buoy2013_L1 %>% 
  mutate(oxygenDissolved_mgl_1p5m_withoffval = case_when(is.na(oxygenDissolved_mgl_1p5m) ~ NA_real_,
                                                         TRUE ~ oxygenDissolved_mgl_1p5m_withoffval),
         oxygenDissolved_mgl_10p5m_withoffval = case_when(is.na(oxygenDissolved_mgl_10p5m) ~ NA_real_,
                                                         TRUE ~ oxygenDissolved_mgl_10p5m_withoffval),
         oxygenDissolvedPercentOfSaturation_pct_1p5m_withoffval = case_when(is.na(oxygenDissolvedPercentOfSaturation_pct_1p5m) ~ NA_real_,
                                                                            TRUE ~ oxygenDissolvedPercentOfSaturation_pct_1p5m_withoffval),
         oxygenDissolvedPercentOfSaturation_pct_10p5m_withoffval = case_when(is.na(oxygenDissolvedPercentOfSaturation_pct_10p5m) ~ NA_real_,
                                                                            TRUE ~ oxygenDissolvedPercentOfSaturation_pct_10p5m_withoffval))


#### column issues ####
#with addition of sensors, columns off. need to split file and rejoin with proper column names
colnames(buoy2013_L1)
buoy2013_L1_a <- buoy2013_L1 %>% 
  select(datetime, location, 
         waterTemperature_DO_degC_1p5m:waterTemperature_degC_9p5m, 
         waterTemperature_DO_degC_10p5m:oxygenDissolved_mgl_10p5m, 
         flag_do1p5m:oxygenDissolvedPercentOfSaturation_pct_10p5m_withoffval)

buoy2013_L1_b <- buoy2013_L1 %>% 
  select(datetime, AirTempC, RH, PAR, WindSp, CorrWind, AveWindSp, AveWindDir, MaxWindSp, MaxWindDir, LoggerBat) %>% 
  filter(datetime <= as.POSIXct('2013-08-27 8:00', tz=buoy_tz))

buoy2013_L1_c <- buoy2013_L1 %>% 
  select(datetime, AirTempC, RH, PAR, WindSp, CorrWind, AveWindSp, AveWindDir, MaxWindSp, MaxWindDir, LoggerBat) %>% 
  filter(datetime > as.POSIXct('2013-08-27 8:00', tz=buoy_tz)) %>% 
  rename(MaxWindDir2 = 'LoggerBat',
         MaxWindSp2 = 'MaxWindDir',
         AveWindDir2 = 'MaxWindSp', 
         AveWindSp2 = 'AveWindDir',
         CorrWind2 = 'AveWindSp',
         WindSp2 = 'CorrWind',
         PAR2 = 'WindSp',
         RH2 = 'PAR',
         AirTempC2 = 'RH') %>% 
  select(-AirTempC) %>% 
  rename(MaxWindDir= 'MaxWindDir2',
         MaxWindSp= 'MaxWindSp2',
         AveWindDir= 'AveWindDir2', 
         AveWindSp= 'AveWindSp2',
         CorrWind= 'CorrWind2',
         WindSp= 'WindSp2',
         PAR= 'PAR2',
         RH= 'RH2',
         AirTempC= 'AirTempC2')
         
buoy2013_L1 <- full_join(buoy2013_L1_b, buoy2013_L1_c) %>% 
  full_join(buoy2013_L1_a, .)

rm(buoy2013_L1_a, buoy2013_L1_b, buoy2013_L1_c)

#### wind sensors ####
range(buoy2013_L1$WindSp, na.rm = T)
range(buoy2013_L1$CorrWind, na.rm = T)
range(buoy2013_L1$AveWindSp, na.rm = T)
range(buoy2013_L1$AveWindDir, na.rm = T)
range(buoy2013_L1$MaxWindSp, na.rm = T)
range(buoy2013_L1$MaxWindDir, na.rm = T)

buoy2013_L1 <- buoy2013_L1 %>% 
  mutate(AveWindSp = case_when(AveWindSp == 6999 ~ NA_real_,
                               TRUE ~ AveWindSp))

#recode data when buoy in transit
buoy2013_L1 <- buoy2013_L1 %>% 
  mutate_at(vars(WindSp, CorrWind, AveWindSp, AveWindDir, MaxWindSp, MaxWindDir),
            ~(case_when(location == 'in transit' ~ NA_real_,
                        TRUE ~ .)))

wind_vert <- buoy2013_L1 %>% 
  select(datetime, location, WindSp, CorrWind, AveWindSp, AveWindDir, MaxWindSp, MaxWindDir) %>% 
  pivot_longer(names_to = 'variable', values_to = 'value', -c(datetime, location))

ggplot(wind_vert, aes(x = datetime, y = value, color = location)) +
  geom_point() +
  facet_grid(variable~., scales = 'free_y') +
  final_theme +
  labs(title = '2013 wind data raw',
       x = NULL,
       y = NULL) +
  scale_x_datetime(date_minor_breaks = '1 month')

# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2013-01-01', tz=buoy_tz) &
#                         datetime<as.POSIXct('2013-02-01', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'jan 2013 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# #errant data jan 16-17
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2013-01-16 9:00', tz=buoy_tz) &
#                         datetime<as.POSIXct('2013-01-17 15:00', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'jan 2013 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 hour')

buoy2013_L1 <- buoy2013_L1 %>% 
  mutate(flag_allwind = case_when(datetime >= as.POSIXct('2013-01-16 12:20', tz=buoy_tz) &
                                 datetime < as.POSIXct('2013-01-17 10:00', tz=buoy_tz) ~ 's',
                               TRUE ~ ''))

wind_vert_b <- buoy2013_L1 %>% 
  select(datetime, location, flag_allwind, WindSp, CorrWind, AveWindSp, AveWindDir, MaxWindSp, MaxWindDir) %>% 
  pivot_longer(names_to = 'variable', values_to = 'value', -c(datetime, location, flag_allwind))

# ggplot(subset(wind_vert_b,
#               subset=(datetime>=as.POSIXct('2013-01-01', tz=buoy_tz) &
#                         datetime<as.POSIXct('2013-02-01', tz=buoy_tz))),
#        aes(x = datetime, y = value, color = flag_allwind)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'jan 2013 wind data clean',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2013-02-01', tz=buoy_tz) &
#                         datetime<as.POSIXct('2013-03-01', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'feb 2013 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2013-03-01', tz=buoy_tz) &
#                         datetime<as.POSIXct('2013-04-01', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'mar 2013 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2013-04-01', tz=buoy_tz) &
#                         datetime<as.POSIXct('2013-05-01', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'apr 2013 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2013-05-01', tz=buoy_tz) &
#                         datetime<as.POSIXct('2013-06-01', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'may 2013 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
#
# ggplot(subset(wind_vert_b,
#               subset=(datetime>=as.POSIXct('2013-05-01', tz=buoy_tz) &
#                         datetime<as.POSIXct('2013-06-01', tz=buoy_tz))),
#        aes(x = datetime, y = value, color=location)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'may 2013 wind data clean',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2013-06-01', tz=buoy_tz) &
#                         datetime<as.POSIXct('2013-07-01', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'jun 2013 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2013-07-01', tz=buoy_tz) &
#                         datetime<as.POSIXct('2013-08-01', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'jul 2013 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# #look at jul 9-10
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2013-07-09 12:00', tz=buoy_tz) &
#                         datetime<as.POSIXct('2013-07-10 12:00', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'jul 2013 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 hour')
# 
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2013-08-01', tz=buoy_tz) &
#                         datetime<as.POSIXct('2013-09-01', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'aug 2013 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2013-09-01', tz=buoy_tz) &
#                         datetime<as.POSIXct('2013-10-01', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'sept 2013 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2013-10-01', tz=buoy_tz) &
#                         datetime<as.POSIXct('2013-11-01', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'oct 2013 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
#
# ggplot(subset(wind_vert_b,
#               subset=(datetime>=as.POSIXct('2013-10-01', tz=buoy_tz) &
#                         datetime<as.POSIXct('2013-11-01', tz=buoy_tz))),
#        aes(x = datetime, y = value, color = location)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'oct 2013 wind data clean',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')

#buoy offline for remainder of year

wind_vert_b <- buoy2013_L1 %>% 
  select(datetime, location, flag_allwind, WindSp, CorrWind, AveWindSp, AveWindDir, MaxWindSp, MaxWindDir) %>% 
  pivot_longer(names_to = 'variable', values_to = 'value', -c(datetime, location, flag_allwind))

ggplot(wind_vert_b, aes(x = datetime, y = value, shape = location, color = flag_allwind)) +
  geom_point() +
  facet_grid(variable~., scales = 'free_y') +
  final_theme +
  labs(title = '2013 wind data clean',
       x = NULL,
       y = NULL) +
  scale_x_datetime(date_minor_breaks = '1 month')

#clean up workspace
rm(wind_vert, wind_vert_b)

#rename with CV
buoy2013_L1 <- buoy2013_L1 %>% 
  rename(windDirectionInstantaneous_deg =CorrWind, 
         windSpeedInstantaneous_mps = WindSp,
         windDirectionAverage_deg = AveWindDir,
         windSpeedAverage_mps = AveWindSp,
         windGustDirection_deg = MaxWindDir,
         windGustSpeed_mps = MaxWindSp)

#### PAR sensors ####
range(buoy2013_L1$PAR, na.rm = T)

buoy2013_L1 <-  buoy2013_L1 %>%
  mutate(flag_par = case_when(PAR <0 ~ 'z',
                         TRUE ~ '')) %>% 
  mutate(PAR = case_when(PAR <0 ~ 0,
                         TRUE ~ PAR))

#recode when in transit
buoy2013_L1 <-  buoy2013_L1 %>%
  mutate(PAR = case_when(location == 'in transit' ~ NA_real_,
                         TRUE ~ PAR))

ggplot(buoy2013_L1, aes(x = datetime, y = PAR, color = location, shape = flag_par)) +
  geom_point() +
  final_theme +
  labs(title = '2013 PAR data below 0 recoded',
       x = NULL,
       y = 'PAR (umol/m2/s)') +
  scale_x_datetime(date_minor_breaks = '1 month')

# ggplot(subset(buoy2013_L1,
#               subset = (datetime>=as.POSIXct('2013-01-01', tz=buoy_tz) &
#                           datetime<as.POSIXct('2013-02-01', tz=buoy_tz))),
#        aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'jan 2013 PAR data raw',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')

#add flag for possibly obscured beg jan
buoy2013_L1 <- buoy2013_L1 %>% 
  mutate(flag_par = case_when(flag_par == '' & datetime < as.Date('2013-01-05') ~ 'o',
                              flag_par != '' & datetime < as.Date('2013-01-05') ~ paste('o', flag_par, sep = '; '),
                              TRUE ~ flag_par))
# ggplot(subset(buoy2013_L1,
#               subset = (datetime>=as.POSIXct('2013-01-01', tz=buoy_tz) &
#                           datetime<as.POSIXct('2013-02-01', tz=buoy_tz))),
#        aes(x = datetime, y = PAR, color = flag_par)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'jan 2013 PAR data raw',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2013_L1,
#               subset = (datetime>=as.POSIXct('2013-02-01', tz=buoy_tz) &
#                           datetime<as.POSIXct('2013-03-01', tz=buoy_tz))),
#        aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'feb 2013 PAR data raw',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')

#add flag for possibly obscured mid feb
buoy2013_L1 <- buoy2013_L1 %>% 
  mutate(flag_par = case_when(flag_par == '' & datetime >= as.Date('2013-02-09') & datetime < as.Date('2013-02-12') ~ 'o',
                              flag_par != '' & datetime >= as.Date('2013-02-09') & datetime < as.Date('2013-02-12') ~ paste('o', flag_par, sep = '; '),
                              TRUE ~ flag_par))

# ggplot(subset(buoy2013_L1,
#               subset = (datetime>=as.POSIXct('2013-02-01', tz=buoy_tz) &
#                           datetime<as.POSIXct('2013-03-01', tz=buoy_tz))),
#        aes(x = datetime, y = PAR, color = flag_par)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'feb 2013 PAR data raw',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2013_L1,
#               subset = (datetime>=as.POSIXct('2013-03-01', tz=buoy_tz) &
#                           datetime<as.POSIXct('2013-04-01', tz=buoy_tz))),
#        aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'mar 2013 PAR data raw',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2013_L1,
#               subset = (datetime>=as.POSIXct('2013-04-01', tz=buoy_tz) &
#                           datetime<as.POSIXct('2013-05-01', tz=buoy_tz))),
#        aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'apr 2013 PAR data raw',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
#
# ggplot(subset(buoy2013_L1,
#               subset = (datetime>=as.POSIXct('2013-05-01', tz=buoy_tz) &
#                           datetime<as.POSIXct('2013-06-01', tz=buoy_tz))),
#        aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'may 2013 PAR data raw',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2013_L1,
#               subset = (datetime>=as.POSIXct('2013-06-01', tz=buoy_tz) &
#                           datetime<as.POSIXct('2013-07-01', tz=buoy_tz))),
#        aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'jun 2013 PAR data raw',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2013_L1,
#               subset = (datetime>=as.POSIXct('2013-07-01', tz=buoy_tz) &
#                           datetime<as.POSIXct('2013-08-01', tz=buoy_tz))),
#        aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'jul 2013 PAR data raw',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2013_L1,
#               subset = (datetime>=as.POSIXct('2013-08-01', tz=buoy_tz) &
#                           datetime<as.POSIXct('2013-09-01', tz=buoy_tz))),
#        aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'aug 2013 PAR data raw',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2013_L1,
#               subset = (datetime>=as.POSIXct('2013-09-01', tz=buoy_tz) &
#                           datetime<as.POSIXct('2013-10-01', tz=buoy_tz))),
#        aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'sept 2013 PAR data raw',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2013_L1,
#               subset = (datetime>=as.POSIXct('2013-10-01', tz=buoy_tz) &
#                           datetime<as.POSIXct('2013-11-01', tz=buoy_tz))),
#        aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'oct 2013 PAR data raw',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')

#buoy offline

ggplot(buoy2013_L1, aes(x = datetime, y = PAR, color = flag_par, shape = location)) +
  geom_point() +
  final_theme +
  labs(title = '2013 PAR data below 0 recoded',
       x = NULL,
       y = 'PAR (umol/m2/s)') +
  scale_x_datetime(date_minor_breaks = '1 month')

#rename with CV
buoy2013_L1 <- buoy2013_L1 %>% 
  rename(radiationIncomingPAR_umolm2s = PAR)


#### Air Temp ####
range(buoy2013_L1$AirTempC, na.rm = T)
range(buoy2013_L1$RH, na.rm =T)

#recode in transit
buoy2013_L1 <- buoy2013_L1 %>% 
  mutate_at(vars(AirTempC, RH),
            ~ case_when(location == 'in transit' ~ NA_real_, 
                        TRUE ~.))

air_vert <- buoy2013_L1 %>% 
  select(datetime, AirTempC, RH, location) %>% 
  pivot_longer(names_to = 'variable', values_to = 'value', -c(datetime, location))

ggplot(air_vert, aes(x=datetime, y = value)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  final_theme +
  labs(title = '2013 air temp raw',
       x= NULL,
       y= 'air temp (deg C)') +
  scale_x_datetime(date_minor_breaks = '1 month')

# ggplot(subset(air_vert,
#               subset=(datetime>=as.POSIXct('2013-01-01', tz=buoy_tz) &
#                         datetime < as.POSIXct('2013-02-01', tz=buoy_tz))),
#        aes(x=datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   final_theme +
#   labs(title = 'jan2013 air temp raw',
#        x= NULL,
#        y= 'air temp (deg C)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(air_vert,
#               subset=(datetime>=as.POSIXct('2013-02-01', tz=buoy_tz) &
#                         datetime < as.POSIXct('2013-03-01', tz=buoy_tz))),
#        aes(x=datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   final_theme +
#   labs(title = 'feb 2013 air temp raw',
#        x= NULL,
#        y= 'air temp (deg C)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(air_vert,
#               subset=(datetime>=as.POSIXct('2013-03-01', tz=buoy_tz) &
#                         datetime < as.POSIXct('2013-04-01', tz=buoy_tz))),
#        aes(x=datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   final_theme +
#   labs(title = 'mar 2013 air temp raw',
#        x= NULL,
#        y= 'air temp (deg C)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(air_vert,
#               subset=(datetime>=as.POSIXct('2013-04-01', tz=buoy_tz) &
#                         datetime < as.POSIXct('2013-05-01', tz=buoy_tz))),
#        aes(x=datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   final_theme +
#   labs(title = 'apr 2013 air temp raw',
#        x= NULL,
#        y= 'air temp (deg C)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(air_vert,
#               subset=(datetime>=as.POSIXct('2013-05-01', tz=buoy_tz) &
#                         datetime < as.POSIXct('2013-06-01', tz=buoy_tz))),
#        aes(x=datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   final_theme +
#   labs(title = 'may 2013 air temp raw',
#        x= NULL,
#        y= 'air temp (deg C)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
#
# ggplot(subset(air_vert,
#               subset=(datetime>=as.POSIXct('2013-06-01', tz=buoy_tz) &
#                         datetime < as.POSIXct('2013-07-01', tz=buoy_tz))),
#        aes(x=datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   final_theme +
#   labs(title = 'jun 2013 air temp raw',
#        x= NULL,
#        y= 'air temp (deg C)') +
#   scale_x_datetime(date_minor_breaks = '1 day')

#errant point on Jun 30

buoy2013_L1 <- buoy2013_L1 %>% 
  mutate(AirTempC = case_when(datetime >= as.POSIXct('2013-06-30', tz=buoy_tz) &
                                datetime < as.POSIXct('2013-07-01', tz=buoy_tz) & 
                                AirTempC >50 ~ NA_real_,
                              TRUE ~ AirTempC),
         RH = case_when(datetime >= as.POSIXct('2013-06-30', tz=buoy_tz) &
                          datetime < as.POSIXct('2013-07-01', tz=buoy_tz) & 
                          RH >150 ~ NA_real_, 
                        TRUE ~ RH))
air_vert <- buoy2013_L1 %>% 
  select(datetime, AirTempC, RH, location) %>% 
  pivot_longer(names_to = 'variable', values_to = 'value', -c(datetime, location))

# ggplot(subset(air_vert,
#               subset=(datetime>=as.POSIXct('2013-06-01', tz=buoy_tz) &
#                         datetime < as.POSIXct('2013-07-01', tz=buoy_tz))),
#        aes(x=datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   final_theme +
#   labs(title = 'jun 2013 air temp clean',
#        x= NULL,
#        y= 'air temp (deg C)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(air_vert,
#               subset=(datetime>=as.POSIXct('2013-07-01', tz=buoy_tz) &
#                         datetime < as.POSIXct('2013-08-01', tz=buoy_tz))),
#        aes(x=datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   final_theme +
#   labs(title = 'jul 2013 air temp raw',
#        x= NULL,
#        y= 'air temp (deg C)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(air_vert,
#               subset=(datetime>=as.POSIXct('2013-08-01', tz=buoy_tz) &
#                         datetime < as.POSIXct('2013-09-01', tz=buoy_tz))),
#        aes(x=datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   final_theme +
#   labs(title = 'aug 2013 air temp raw',
#        x= NULL,
#        y= 'air temp (deg C)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(air_vert,
#               subset=(datetime>=as.POSIXct('2013-09-01', tz=buoy_tz) &
#                         datetime < as.POSIXct('2013-10-01', tz=buoy_tz))),
#        aes(x=datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   final_theme +
#   labs(title = 'sept 2013 air temp raw',
#        x= NULL,
#        y= 'air temp (deg C)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(air_vert,
#               subset=(datetime>=as.POSIXct('2013-10-01', tz=buoy_tz) &
#                         datetime < as.POSIXct('2013-11-01', tz=buoy_tz))),
#        aes(x=datetime, y = value)) +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   geom_point() +
#   final_theme +
#   labs(title = 'oct 2013 air temp raw',
#        x= NULL,
#        y= 'air temp (deg C)') +
#   scale_x_datetime(date_minor_breaks = '1 day')

#buoy offline


ggplot(air_vert,
       aes(x=datetime, y = value, color = location)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  final_theme +
  labs(title = '2013 air temp clean',
       x= NULL,
       y= 'air temp (deg C)') +
  scale_x_datetime(date_minor_breaks = '1 month')


#add RH flags for supersaturated 
buoy2013_L1 <- buoy2013_L1 %>% 
  mutate(flag_rh = case_when(RH >100 ~ 's',
                             TRUE ~ ''))

#rename with CV
buoy2013_L1 <- buoy2013_L1 %>% 
  rename(relativeHumidity_perc = RH,
         airTemperature_degC = AirTempC)

rm(air_vert)

#### EXPORT L1 FILES ####
colnames(buoy2013_L1)

#export L1 tempstring file
buoy2013_L1 %>%
  select(datetime, location, waterTemperature_degC_0p5m:waterTemperature_degC_9p5m) %>%
  mutate(datetime = as.character(datetime)) %>%
  write_csv(., file.path(dump_dir,'tempstring/2013_tempstring_L1_v2022.csv'))

# export L1 do file
buoy2013_L1 %>%
  select(datetime, location, 
         oxygenDissolved_mgl_1p5m, oxygenDissolvedPercentOfSaturation_pct_1p5m, waterTemperature_DO_degC_1p5m, 
         flag_do1p5m,
         oxygenDissolved_mgl_10p5m, oxygenDissolvedPercentOfSaturation_pct_10p5m, waterTemperature_DO_degC_10p5m, 
         flag_do10p5m,
         offval_do1p5_mgl, offval_do1p5_sat, offval_do10p5_mgl, offval_do10p5_sat,
         oxygenDissolved_mgl_1p5m_withoffval, oxygenDissolvedPercentOfSaturation_pct_1p5m_withoffval,
         oxygenDissolved_mgl_10p5m_withoffval, oxygenDissolvedPercentOfSaturation_pct_10p5m_withoffval) %>%
  mutate(datetime = as.character(datetime)) %>%
  write_csv(., file.path(dump_dir, 'do/2013_do_L1_v2022.csv'))

#export L1 met file
buoy2013_L1 %>%
  select(datetime, location, 
         windSpeedInstantaneous_mps:windGustDirection_deg, flag_allwind,
         radiationIncomingPAR_umolm2s, flag_par,
         airTemperature_degC, relativeHumidity_perc, flag_rh) %>%
  mutate(datetime = as.character(datetime)) %>%
  write_csv(., file.path(dump_dir, 'met/2013_met_L1_v2022.csv'))