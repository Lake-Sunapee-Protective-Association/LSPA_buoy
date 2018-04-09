#*****************************************************************
#*      Cary Institute of Ecosystem Studies (Millbrook, NY)      *
#*                                                               *
#* TITLE:   Sunapee_buoy_2013.r                                  *
#* AUTHOR:  Bethel Steele                                        *
#* SYSTEM:  Lenovo ThinkCentre, Win 10, R 3.4.2, RStudio 1.1.383 *
#* PROJECT: Lake Sunapee Buoy Data Cleaning                      *
#* PURPOSE: subset data for met/compare with L1                  *
#*****************************************************************

#bring in 2013 buoy raw data
buoy2013_L0 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L0/Sunapee2013_rawData.csv',
                        col_names = c('station', 'year', 'day', 'hr_min', 'DOTempC', 'DOSat',
                                      'DOppm', 'TempC_0m', 'TempC_1m', 'TempC_2m', 'TempC_3m', 'TempC_4m',
                                      'TempC_5m', 'TempC_6m', 'TempC_7m', 'TempC_8m', 'TempC_9m',
                                      'AirTempC', 'RH', 'PAR', 'WndSp', 
                                      'CorrWind', 'AveWindSp', 'AveWindDir', 'MaxWindSp', 'MaxWindDir', 
                                      'LoggerBat', 'RadioBat', 'IntTemp', 'Heading', 'X', 
                                      'DOLoTempC', 'DOLowSat',' DOLowPPM', 'Date', 'Time', 
                                      'datetime', 'diff', 'gap'),
                        col_types = 'nnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnncccnn',
                        skip=1) %>%
  select(-station, -year, -day, -hr_min, -LoggerBat, -RadioBat, -IntTemp, -X, -Date, -Time, -diff, -gap) %>%  #drop redundant columns
  mutate(datetime = as.POSIXct(datetime, format='%Y-%m-%d %H:%M', tz='UTC'))


#bring in 2013 LMP data for comparison
LMP2013 <- read_xlsx('C:/Users/steeleb/Dropbox/Lake Sunapee/long term Sunapee data/raw data/LMP files/LMP 2017/DO.xlsx',
                     sheet='DO',
                     col_types = c('text', 'text', 'numeric', 'guess', 'numeric',
                                   'numeric', 'numeric', 'numeric', 'numeric' ,'numeric',
                                   'text', 'text', 'numeric', 'text')) %>%
  mutate(DATE = as.Date(DATE, format='%Y-%m-%d')) %>%  #format date
  filter(DATE >= '2013-01-01' & DATE < '2014-01-01', #filter for 2013 only
         STATION == 210)

LMP2013_temp <- LMP2013 %>%
  select(DATE, DEPTH, TEMP) %>%
  filter(DEPTH<=12) %>%
  rename(variable = 'DEPTH',
         value = 'TEMP',
         datetime = 'DATE') %>%
  mutate(source = 'LMP',
         datetime = as.POSIXct(paste(datetime, '12:00', sep=' '), format = '%Y-%m-%d %H:%M', tz='UTC'),
         variable = as.factor(variable),
         variable = case_when(variable == '0.5' ~ 'TempC_0p5m',
                              variable == '1' ~ 'TempC_1m',
                              variable == '2' ~ 'TempC_2m',
                              variable == '3' ~ 'TempC_3m',
                              variable == '4' ~ 'TempC_4m',
                              variable == '5' ~ 'TempC_5m',
                              variable == '6' ~ 'TempC_6m',
                              variable == '7' ~ 'TempC_7m',
                              variable == '8' ~ 'TempC_8m',
                              variable == '9' ~ 'TempC_9m',
                              variable == '10' ~ 'TempC_10m',
                              variable == '11' ~ 'TempC_11m',
                              variable == '12' ~ 'TempC_12m'))


#### thermisters ####
buoy2013_temp_vert <- buoy2013_L0 %>%
  select(datetime, alltemp2013) %>%
  gather(variable, value, -datetime) %>% 
  mutate(variable = factor(variable, levels=alltemp2013))

# ggplot(subset(buoy2013_temp_vert,
#               subset=(datetime >=as.POSIXct('2013-01-01', tz='UTC') &
#                         datetime < as.POSIXct('2014-01-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   final_theme +
# labs(title = '2013 buoy temp data, raw',
#      x=NULL,
#      y='temp ((deg C))') +
#   scale_x_datetime(date_minor_breaks = '1 month') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust

buoy2013_L1 <- buoy2013_L0 %>%
  mutate_at(vars(alltemp2013),
            funs(case_when(. == -6999 ~ NA_real_,
                           . == 555.4 ~ NA_real_,
                           TRUE ~ .))) %>%
  mutate(location = 'loon')

buoy2013_temp_vert <- buoy2013_L1 %>%
  select(datetime, location, alltemp2013) %>%
  gather(variable, value, -location, -datetime) %>%
  mutate(variable = factor(variable, levels=alltemp2013))

# ggplot(subset(buoy2013_temp_vert,
#               subset=(datetime >=as.POSIXct('2013-01-01', tz='UTC') &
#                         datetime < as.POSIXct('2014-01-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   final_theme +
#   labs(title = '2013 buoy temp data, NAs recoded',
#        x=NULL,
#        y='temp (deg C)') +
#   scale_x_datetime(date_minor_breaks = '1 month') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# ggplot(subset(buoy2013_temp_vert,
#               subset=(datetime >=as.POSIXct('2013-05-01', tz='UTC') &
#                         datetime < as.POSIXct('2013-06-01', tz='UTC'))),
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
#               subset=(datetime >=as.POSIXct('2013-05-15', tz='UTC') &
#                         datetime < as.POSIXct('2013-05-16', tz='UTC'))),
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
  mutate_at(vars(alltemp2013),
            funs(case_when(datetime < as.POSIXct('2013-05-15 12:00', tz='UTC') ~ NA_real_,
                           TRUE ~ .))) 

buoy2013_temp_vert_b <- buoy2013_L1 %>%
  select(datetime, location, alltemp2013) %>%
  gather(variable, value, -location, -datetime) %>%
  mutate(variable = factor(variable, levels=alltemp2013))

# ggplot(subset(buoy2013_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2013-05-01', tz='UTC') &
#                         datetime < as.POSIXct('2013-06-01', tz='UTC'))),
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
#               subset=(datetime >=as.POSIXct('2013-06-01', tz='UTC') &
#                         datetime < as.POSIXct('2013-07-01', tz='UTC'))),
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
#               subset=(datetime >=as.POSIXct('2013-07-01', tz='UTC') &
#                         datetime < as.POSIXct('2013-08-01', tz='UTC'))),
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
#               subset=(datetime >=as.POSIXct('2013-08-01', tz='UTC') &
#                         datetime < as.POSIXct('2013-09-01', tz='UTC'))),
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
#               subset=(datetime >=as.POSIXct('2013-08-12', tz='UTC') &
#                         datetime < as.POSIXct('2013-08-13', tz='UTC'))),
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
#               subset=(datetime >=as.POSIXct('2013-08-15', tz='UTC') &
#                         datetime < as.POSIXct('2013-08-16', tz='UTC'))),
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
#               subset=(datetime >=as.POSIXct('2013-09-10', tz='UTC') &
#                         datetime < as.POSIXct('2013-09-11', tz='UTC'))),
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
#               subset=(datetime >=as.POSIXct('2013-09-01', tz='UTC') &
#                         datetime < as.POSIXct('2013-10-01', tz='UTC'))),
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
  mutate_at(vars(TempC_7m:TempC_10m),
            funs(case_when(datetime >= as.POSIXct('2013-08-12 16:40', tz='UTC') & datetime < as.POSIXct('2013-08-12 17:00', tz='UTC') ~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate_at(vars(alltemp2013),
            funs(case_when(datetime >= as.POSIXct('2013-08-15 9:00', tz='UTC') & datetime < as.POSIXct('2013-09-10 11:50', tz='UTC') ~ NA_real_,
                           TRUE ~ . )))

buoy2013_temp_vert_b <- buoy2013_L1 %>%
  select(datetime, location, alltemp2013) %>%
  gather(variable, value, -location, -datetime) %>%
  mutate(variable = factor(variable, levels=alltemp2013))

# ggplot(subset(buoy2013_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2013-08-01', tz='UTC') &
#                         datetime < as.POSIXct('2013-09-01', tz='UTC'))),
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
#               subset=(datetime >=as.POSIXct('2013-09-01', tz='UTC') &
#                         datetime < as.POSIXct('2013-10-01', tz='UTC'))),
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
#               subset=(datetime >=as.POSIXct('2013-10-01', tz='UTC') &
#                         datetime < as.POSIXct('2013-11-01', tz='UTC'))),
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
# ggplot(subset(buoy2013_temp_vert_b),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   final_theme +
#   labs(title='2013, clean',
#        x=NULL,
#        y='temp (deg C)') +
#   scale_x_datetime(date_minor_breaks = '1 month') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust

# #reality check with LMP
# buoy_LMP_2013 <- buoy2013_temp_vert_b %>%
#   mutate(source='buoy') %>%
#   full_join(., LMP2013_temp) %>%
#   select(-location) %>% 
#   mutate(variable = factor(variable, levels = alltemp2007LMP))
# 
# #June 17, July 15, Aug 19, Sept 17
# ggplot(subset(buoy_LMP_2013,
#               subset=(datetime>=as.POSIXct('2013-06-17', tz='UTC') &
#                         datetime<as.POSIXct('2013-06-18', tz='UTC'))),
#               aes(x=datetime, y=value, color=variable, shape = source)) +
#   geom_point() +
#   final_theme +
#   labs(title='temp reality check\n2013 buoy and LMP site 210 data\nJune 17, 2013',
#        x=NULL,
#        y='temp (deg C)') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# ggplot(subset(buoy_LMP_2013,
#               subset=(datetime>=as.POSIXct('2013-07-15', tz='UTC') &
#                         datetime<as.POSIXct('2013-07-16', tz='UTC'))),
#        aes(x=datetime, y=value, color=variable, shape = source)) +
#   geom_point() +
#   final_theme +
#   labs(title='temp reality check\n2013 buoy and LMP site 210 data\nJuly 15, 2013',
#        x=NULL,
#        y='temp (deg C)') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# ggplot(subset(buoy_LMP_2013,
#               subset=(datetime>=as.POSIXct('2013-08-19', tz='UTC') &
#                         datetime<as.POSIXct('2013-08-20', tz='UTC'))),
#        aes(x=datetime, y=value, color=variable, shape = source)) +
#   geom_point() +
#   final_theme +
#   labs(title='temp reality check\n2013 buoy and LMP site 210 data\nAugust 19, 2013',
#        x=NULL,
#        y='temp (deg C)') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# ggplot(subset(buoy_LMP_2013,
#               subset=(datetime>=as.POSIXct('2013-09-17', tz='UTC') &
#                         datetime<as.POSIXct('2013-09-18', tz='UTC'))),
#        aes(x=datetime, y=value, color=variable, shape = source)) +
#   geom_point() +
#   final_theme +
#   labs(title='temp reality check\n2013 buoy and LMP site 210 data\nSeptember 17, 2013',
#        x=NULL,
#        y='temp (deg C)') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust

#export L1 tempstring file
buoy2013_L1 %>% 
  select(datetime, TempC_1m, TempC_2m, TempC_3m, TempC_4m, TempC_5m, TempC_6m, TempC_7m, TempC_8m, TempC_9m, TempC_10m) %>% 
  mutate(datetime = as.character(datetime)) %>% 
  write_csv(., 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2013_tempstring_L1.csv')

#clean up workspace
rm(buoy_LMP_2013, buoy2013_temp_vert, buoy2013_temp_vert_b, LMP2013_temp)



#### DO sensors ####
