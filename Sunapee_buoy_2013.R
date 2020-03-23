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
                                      'AirTempC', 'RH', 'PAR', 'WindSp', 
                                      'CorrWind', 'AveWindSp', 'AveWindDir', 'MaxWindSp', 'MaxWindDir', 
                                      'LoggerBat', 'RadioBat', 'IntTemp', 'Heading', 'UnkDir', 
                                      'DOLowTempC', 'DOLowSat','DOLowPPM', 'Date', 'Time', 
                                      'datetime', 'diff', 'gap'),
                        col_types = 'nnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnncccnn',
                        skip=1) %>%
  select(-station, -year, -day, -hr_min, -RadioBat, -IntTemp, Heading, -Date, -Time, -diff, -gap) %>%  #drop redundant columns
  mutate(datetime = as.POSIXct(datetime, format='%Y-%m-%d %H:%M', tz='UTC'))


# #bring in 2013 LMP data for comparison
# LMP2013 <- read_xlsx('C:/Users/steeleb/Dropbox/Lake Sunapee/long term Sunapee data/raw data/LMP files/LMP 2017/DO.xlsx',
#                      sheet='DO',
#                      col_types = c('text', 'text', 'numeric', 'guess', 'numeric',
#                                    'numeric', 'numeric', 'numeric', 'numeric' ,'numeric',
#                                    'text', 'text', 'numeric', 'text')) %>%
#   mutate(DATE = as.Date(DATE, format='%Y-%m-%d')) %>%  #format date
#   filter(DATE >= '2013-01-01' & DATE < '2014-01-01', #filter for 2013 only
#          STATION == 210)
# 
# LMP2013_temp <- LMP2013 %>%
#   select(DATE, DEPTH, TEMP) %>%
#   filter(DEPTH<=12) %>%
#   rename(variable = 'DEPTH',
#          value = 'TEMP',
#          datetime = 'DATE') %>%
#   mutate(source = 'LMP',
#          datetime = as.POSIXct(paste(datetime, '12:00', sep=' '), format = '%Y-%m-%d %H:%M', tz='UTC'),
#          variable = as.factor(variable),
#          variable = case_when(variable == '0.5' ~ 'TempC_0p5m',
#                               variable == '1' ~ 'TempC_1m',
#                               variable == '2' ~ 'TempC_2m',
#                               variable == '3' ~ 'TempC_3m',
#                               variable == '4' ~ 'TempC_4m',
#                               variable == '5' ~ 'TempC_5m',
#                               variable == '6' ~ 'TempC_6m',
#                               variable == '7' ~ 'TempC_7m',
#                               variable == '8' ~ 'TempC_8m',
#                               variable == '9' ~ 'TempC_9m',
#                               variable == '10' ~ 'TempC_10m',
#                               variable == '11' ~ 'TempC_11m',
#                               variable == '12' ~ 'TempC_12m'))

#create dummy timestamp so there are no blanks
alltimes_2013 <- as.data.frame(seq.POSIXt(as.POSIXct('2013-01-01 00:00', tz='UTC'), as.POSIXct('2013-12-31 23:50', tz='UTC'), '10 min')) %>% 
  rename("datetime" = !!names(.[1]))

buoy2013_L1 <- buoy2013_L0 %>% 
  right_join(., alltimes_2013) %>% 
  arrange(datetime)

#### thermisters ####
buoy2013_temp_vert <- buoy2013_L1 %>%
  select(datetime, alltemp2011) %>%
  gather(variable, value, -datetime) %>% 
  mutate(variable = factor(variable, levels=alltemp2011))

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

buoy2013_L1 <- buoy2013_L1 %>%
  mutate_at(vars(alltemp2011),
            funs(case_when(. == -6999 ~ NA_real_,
                           . == 555.4 ~ NA_real_,
                           TRUE ~ .))) %>%
  mutate(location = 'loon')

buoy2013_temp_vert <- buoy2013_L1 %>%
  select(datetime, location, alltemp2011) %>%
  gather(variable, value, -location, -datetime) %>%
  mutate(variable = factor(variable, levels=alltemp2011))

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
  mutate_at(vars(alltemp2011),
            funs(case_when(datetime < as.POSIXct('2013-05-15 12:00', tz='UTC') ~ NA_real_,
                           TRUE ~ .))) 

buoy2013_temp_vert_b <- buoy2013_L1 %>%
  select(datetime, location, alltemp2011) %>%
  gather(variable, value, -location, -datetime) %>%
  mutate(variable = factor(variable, levels=alltemp2011))

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
  mutate_at(vars(TempC_6m:TempC_9m),
            funs(case_when(datetime >= as.POSIXct('2013-08-12 16:40', tz='UTC') & datetime < as.POSIXct('2013-08-12 17:00', tz='UTC') ~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate_at(vars(alltemp2011),
            funs(case_when(datetime >= as.POSIXct('2013-08-15 9:00', tz='UTC') & datetime < as.POSIXct('2013-09-10 11:50', tz='UTC') ~ NA_real_,
                           TRUE ~ . )))

buoy2013_temp_vert_b <- buoy2013_L1 %>%
  select(datetime, location, alltemp2011) %>%
  gather(variable, value, -location, -datetime) %>%
  mutate(variable = factor(variable, levels=alltemp2011))

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
# 
#clean up workspace
rm(buoy_LMP_2013, buoy2013_temp_vert, buoy2013_temp_vert_b, LMP2013_temp)


#### DO sensors ####
range(buoy2013_L1$DOSat, na.rm=T)
range(buoy2013_L1$DOppm, na.rm=T)
range(buoy2013_L1$DOTempC, na.rm=T)
range(buoy2013_L1$DOLowSat, na.rm=T)
range(buoy2013_L1$DOLowPPM, na.rm=T)
range(buoy2013_L1$DOLowTempC, na.rm=T)

do_vert <- buoy2013_L1 %>% 
  select(datetime, DOSat, DOppm, DOTempC, DOLowSat, DOLowPPM, DOLowTempC) %>% 
  gather(variable, value, -datetime)

# ggplot(do_vert, aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = '2013 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 month')

buoy2013_L1 <- buoy2013_L1 %>% 
  mutate_at(vars(DOTempC, DOLowSat, DOLowPPM, DOLowTempC),
            funs(case_when(. == -6999 ~ NA_real_,
                             TRUE ~ .))) %>% 
  mutate_at(vars(DOLowSat, DOLowPPM, DOLowTempC),
            funs(case_when(. == 0 ~ NA_real_,
                           TRUE ~ .)))

do_vert <- buoy2013_L1 %>% 
  select(datetime, DOSat, DOppm, DOTempC, DOLowSat, DOLowPPM, DOLowTempC) %>% 
  gather(variable, value, -datetime)

ggplot(do_vert, aes(x = datetime, y = value)) +
  geom_point() +
  facet_grid(variable~., scales = 'free_y') +
  final_theme +
  labs(title = '2013 DO data NAs recoded',
       x = NULL,
       y = NULL) +
  scale_x_datetime(date_minor_breaks = '1 month')

# ggplot(subset(do_vert,
#               subset = (datetime >= as.POSIXct('2013-01-01', tz='UTC') &
#                           datetime < as.POSIXct('2013-02-01', tz='UTC'))),
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
#               subset = (datetime >= as.POSIXct('2013-02-01', tz='UTC') &
#                           datetime < as.POSIXct('2013-03-01', tz='UTC'))),
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
#               subset = (datetime >= as.POSIXct('2013-03-01', tz='UTC') &
#                           datetime < as.POSIXct('2013-04-01', tz='UTC'))),
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
#               subset = (datetime >= as.POSIXct('2013-04-01', tz='UTC') &
#                           datetime < as.POSIXct('2013-05-01', tz='UTC'))),
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
#               subset = (datetime >= as.POSIXct('2013-05-01', tz='UTC') &
#                           datetime < as.POSIXct('2013-06-01', tz='UTC'))),
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
#               subset = (datetime >= as.POSIXct('2013-05-15', tz='UTC') &
#                           datetime < as.POSIXct('2013-05-16', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'may 2013 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 hour')

buoy2013_L1 <- buoy2013_L1 %>% 
  mutate_at(vars(DOSat, DOppm, DOTempC),
            funs(case_when(datetime >= as.POSIXct('2013-05-15 9:40', tz='UTC') &
                             datetime < as.POSIXct('2013-05-15 11:50', tz='UTC') ~ NA_real_,
                           TRUE ~ .))) %>%
  mutate(location = case_when(datetime < as.POSIXct('2013-05-15 9:40', tz='UTC') ~'harbor',
                              datetime >= as.POSIXct('2013-05-15 9:40', tz='UTC') &
                                datetime < as.POSIXct('2013-05-15 11:50', tz='UTC') ~ 'in transit',
                              datetime >= as.POSIXct('2013-05-15 11:50', tz='UTC') ~'loon',
                              TRUE ~ ''))

do_vert_b <- buoy2013_L1 %>% 
  select(datetime, location, DOSat, DOppm, DOTempC) %>% 
  gather(variable, value, -datetime, -location)

# ggplot(subset(do_vert_b,
#               subset = (datetime >= as.POSIXct('2013-05-01', tz='UTC') &
#                           datetime < as.POSIXct('2013-06-01', tz='UTC'))),
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
#               subset = (datetime >= as.POSIXct('2013-06-01', tz='UTC') &
#                           datetime < as.POSIXct('2013-07-01', tz='UTC'))),
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
#               subset = (datetime >= as.POSIXct('2013-07-01', tz='UTC') &
#                           datetime < as.POSIXct('2013-08-01', tz='UTC'))),
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
#               subset = (datetime >= as.POSIXct('2013-08-01', tz='UTC') &
#                           datetime < as.POSIXct('2013-09-01', tz='UTC'))),
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
#               subset = (datetime >= as.POSIXct('2013-09-01', tz='UTC') &
#                           datetime < as.POSIXct('2013-10-01', tz='UTC'))),
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
#               subset = (datetime >= as.POSIXct('2013-09-10', tz='UTC') &
#                           datetime < as.POSIXct('2013-09-11', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'sept 2013 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 hour')
# 
# #sept 25 calibration
# ggplot(subset(do_vert,
#               subset = (datetime >= as.POSIXct('2013-09-25', tz='UTC') &
#                           datetime < as.POSIXct('2013-09-26', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'sept 2013 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 hour')

buoy2013_L1 <- buoy2013_L1 %>% 
  mutate_at(vars(DOLowSat, DOLowPPM),
            funs(case_when(datetime < as.POSIXct('2013-09-25 15:00', tz='UTC') ~ NA_real_,
                           TRUE ~ .))) %>%
  mutate(upper_do_flag = case_when(datetime == as.POSIXct('2013-09-25 15:00', tz='UTC') ~'cp',
                              TRUE ~ '')) %>%
  mutate(lower_do_flag = case_when(datetime == as.POSIXct('2013-09-25 15:00', tz='UTC') ~'cp',
                                   TRUE ~ ''))

do_vert_b <- buoy2013_L1 %>% 
  select(datetime, location, DOSat, DOppm, DOTempC, DOLowSat, DOLowPPM, DOLowTempC) %>% 
  gather(variable, value, -datetime, -location)


# ggplot(subset(do_vert_b,
#               subset = (datetime >= as.POSIXct('2013-09-01', tz='UTC') &
#                           datetime < as.POSIXct('2013-10-01', tz='UTC'))),
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
#               subset = (datetime >= as.POSIXct('2013-10-01', tz='UTC') &
#                           datetime < as.POSIXct('2013-11-01', tz='UTC'))),
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
#               subset = (datetime >= as.POSIXct('2013-10-16', tz='UTC') &
#                           datetime < as.POSIXct('2013-10-17', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'oct 2013 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 hour')

buoy2013_L1 <- buoy2013_L1 %>% 
  mutate_at(vars(lowDO, upDO),
            funs(case_when(datetime >= as.POSIXct('2013-10-16 9:10', tz='UTC') &
                             datetime < as.POSIXct('2013-10-16 10:10', tz='UTC') ~ NA_real_,
                           TRUE ~ .))) %>%
  mutate(location = case_when(datetime >= as.POSIXct('2013-10-16 9:10', tz='UTC') &
                                datetime < as.POSIXct('2013-10-16 10:10', tz='UTC') ~ 'in transit',
                              datetime >= as.POSIXct('2013-10-16 10:10', tz='UTC') ~'harbor',
                             TRUE ~ location))

do_vert_b <- buoy2013_L1 %>% 
  select(datetime, location, DOSat, DOppm, DOTempC, DOLowSat, DOLowPPM, DOLowTempC) %>% 
  gather(variable, value, -datetime, -location)

# ggplot(subset(do_vert_b,
#               subset = (datetime >= as.POSIXct('2013-10-01', tz='UTC') &
#                           datetime < as.POSIXct('2013-11-01', tz='UTC'))),
#        aes(x = datetime, y = value, color = location)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'oct 2013 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(do_vert,
#               subset = (datetime >= as.POSIXct('2013-11-01', tz='UTC') &
#                           datetime < as.POSIXct('2013-12-01', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'nov 2013 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(do_vert,
#               subset = (datetime >= as.POSIXct('2013-12-01', tz='UTC') &
#                           datetime < as.POSIXct('2014-01-01', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'dec 2013 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')

buoy2013_L1 <- buoy2013_L1 %>% 
  mutate(location = case_when(datetime >= as.POSIXct('2013-10-23 11:00', tz='UTC')  ~ 'offline',
                              TRUE ~ location))

# ggplot(do_vert_b, aes(x = datetime, y = value, color = location)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = '2013 DO data clean',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 month')

#add presumed cleaning flags
buoy2013_L1 <- buoy2013_L1 %>% 
  mutate(upper_do_flag = case_when(datetime == as.POSIXct('2013-05-15 11:50') ~ 'wp',
                             datetime == as.POSIXct('2013-10-16 10:10') ~ 'wp',
                             TRUE ~ upper_do_flag)) %>% 
  mutate(upper_do_flag = case_when(datetime < as.POSIXct('2013-09-25 15:00', tz='UTC') ~ paste('x', upper_do_flag, sep = ''),
                                   TRUE ~ upper_do_flag)) 

do_vert_up <- buoy2013_L1 %>% 
  select(datetime, location,DOSat, DOppm, DOTempC, upper_do_flag) %>% 
  gather(variable, value, -datetime, -location, -upper_do_flag)

ggplot(do_vert_up, aes(x = datetime, y = value, color = location, shape = upper_do_flag)) +
  geom_point() +
  facet_grid(variable~., scales = 'free_y') +
  final_theme +
  labs(title = '2013 DO data clean',
       x = NULL,
       y = NULL) +
  scale_x_datetime(date_minor_breaks = '1 month')

do_vert_lo <- buoy2013_L1 %>% 
  select(datetime, location, DOLowSat, DOLowPPM, DOLowTempC, lower_do_flag) %>% 
  gather(variable, value, -datetime, -location, -lower_do_flag)

ggplot(do_vert_lo, aes(x = datetime, y = value, color = location, shape = lower_do_flag)) +
  geom_point() +
  facet_grid(variable~., scales = 'free_y') +
  final_theme +
  labs(title = '2013 DO data clean',
       x = NULL,
       y = NULL) +
  scale_x_datetime(date_minor_breaks = '1 month')

#### column issues ####
#with addition of sensors, columns off. need to split file and rejoin with proper column names
buoy2013_L1_a <- buoy2013_L1 %>% 
  select(datetime, DOTempC, DOSat, DOppm, TempC_0m, TempC_1m, TempC_2m, TempC_3m, TempC_4m, TempC_5m, TempC_6m, TempC_7m, TempC_8m, TempC_9m, DOLowTempC, DOLowSat, DOLowPPM, location, upper_do_flag, lower_do_flag)

buoy2013_L1_b <- buoy2013_L1 %>% 
  select(datetime, AirTempC, RH, PAR, WindSp, CorrWind, AveWindSp, AveWindDir, MaxWindSp, MaxWindDir, LoggerBat) %>% 
  filter(datetime <= as.POSIXct('2013-08-27 9:00', tz='UTC'))

buoy2013_L1_c <- buoy2013_L1 %>% 
  select(datetime, AirTempC, RH, PAR, WindSp, CorrWind, AveWindSp, AveWindDir, MaxWindSp, MaxWindDir, LoggerBat) %>% 
  filter(datetime > as.POSIXct('2013-08-27 9:00', tz='UTC')) %>% 
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
  full_join(., buoy2013_L1_a)

rm(buoy2013_L1_a, buoy2013_L1_b, buoy2013_L1_c)

#### wind sensors ####
range(buoy2013_L1$WindSp, na.rm = T)
range(buoy2013_L1$CorrWind, na.rm = T)
range(buoy2013_L1$AveWindSp, na.rm = T)
range(buoy2013_L1$AveWindDir, na.rm = T)
range(buoy2013_L1$MaxWindSp, na.rm = T)
range(buoy2013_L1$MaxWindDir, na.rm = T)

wind_vert <- buoy2013_L1 %>% 
  select(datetime, WindSp, CorrWind, AveWindSp, AveWindDir, MaxWindSp, MaxWindDir) %>% 
  gather(variable, value, -datetime)

# ggplot(wind_vert, aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = '2013 wind data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 month')

buoy2013_L1 <- buoy2013_L1 %>% 
  mutate(AveWindSp = case_when(AveWindSp == 6999 ~ NA_real_,
                                TRUE ~ AveWindSp))

wind_vert <- buoy2013_L1 %>% 
  select(datetime, WindSp, CorrWind, AveWindSp, AveWindDir, MaxWindSp, MaxWindDir) %>% 
  gather(variable, value, -datetime)

ggplot(wind_vert, aes(x = datetime, y = value)) +
  geom_point() +
  facet_grid(variable~., scales = 'free_y') +
  final_theme +
  labs(title = '2013 wind data NAs recoded',
       x = NULL,
       y = NULL) +
  scale_x_datetime(date_minor_breaks = '1 month')

# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2013-01-01', tz='UTC') &
#                         datetime<as.POSIXct('2013-02-01', tz='UTC'))),
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
#               subset=(datetime>=as.POSIXct('2013-01-16 9:00', tz='UTC') &
#                         datetime<as.POSIXct('2013-01-17 15:00', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'jan 2013 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 hour')

buoy2013_L1 <- buoy2013_L1 %>% 
  mutate_at(vars(WindSp, CorrWind, AveWindSp, AveWindDir, MaxWindSp, MaxWindDir),
            funs(case_when(datetime >= as.POSIXct('2013-01-16 12:20', tz='UTC') &
                             datetime < as.POSIXct('2013-01-17 10:00', tz='UTC') ~ NA_real_,
                           TRUE ~ .)))

wind_vert_b <- buoy2013_L1 %>% 
  select(datetime, WindSp, CorrWind, AveWindSp, AveWindDir, MaxWindSp, MaxWindDir) %>% 
  gather(variable, value, -datetime)

# ggplot(subset(wind_vert_b,
#               subset=(datetime>=as.POSIXct('2013-01-01', tz='UTC') &
#                         datetime<as.POSIXct('2013-02-01', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'jan 2013 wind data clean',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2013-02-01', tz='UTC') &
#                         datetime<as.POSIXct('2013-03-01', tz='UTC'))),
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
#               subset=(datetime>=as.POSIXct('2013-03-01', tz='UTC') &
#                         datetime<as.POSIXct('2013-04-01', tz='UTC'))),
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
#               subset=(datetime>=as.POSIXct('2013-04-01', tz='UTC') &
#                         datetime<as.POSIXct('2013-05-01', tz='UTC'))),
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
#               subset=(datetime>=as.POSIXct('2013-05-01', tz='UTC') &
#                         datetime<as.POSIXct('2013-06-01', tz='UTC'))),
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
#               subset=(datetime>=as.POSIXct('2013-05-01', tz='UTC') &
#                         datetime<as.POSIXct('2013-06-01', tz='UTC'))),
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
#               subset=(datetime>=as.POSIXct('2013-06-01', tz='UTC') &
#                         datetime<as.POSIXct('2013-07-01', tz='UTC'))),
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
#               subset=(datetime>=as.POSIXct('2013-07-01', tz='UTC') &
#                         datetime<as.POSIXct('2013-08-01', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'jul 2013 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2013-08-01', tz='UTC') &
#                         datetime<as.POSIXct('2013-09-01', tz='UTC'))),
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
#               subset=(datetime>=as.POSIXct('2013-09-01', tz='UTC') &
#                         datetime<as.POSIXct('2013-10-01', tz='UTC'))),
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
#               subset=(datetime>=as.POSIXct('2013-10-01', tz='UTC') &
#                         datetime<as.POSIXct('2013-11-01', tz='UTC'))),
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
#               subset=(datetime>=as.POSIXct('2013-10-01', tz='UTC') &
#                         datetime<as.POSIXct('2013-11-01', tz='UTC'))),
#        aes(x = datetime, y = value, color = location)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'oct 2013 wind data clean',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')


#buoy offline for remainder of year


#recode data when buoy in transit
buoy2013_L1 <- buoy2013_L1 %>% 
  mutate_at(vars(WindSp, CorrWind, AveWindSp, AveWindDir, MaxWindSp, MaxWindDir),
            funs(case_when(location == 'in transit' ~ NA_real_,
                           TRUE ~ .)))

wind_vert_b <- buoy2013_L1 %>% 
  select(datetime, location, WindSp, CorrWind, AveWindSp, AveWindDir, MaxWindSp, MaxWindDir) %>% 
  gather(variable, value, -datetime, -location)

ggplot(wind_vert_b, aes(x = datetime, y = value, color = location)) +
  geom_point() +
  facet_grid(variable~., scales = 'free_y') +
  final_theme +
  labs(title = '2013 wind data clean',
       x = NULL,
       y = NULL) +
  scale_x_datetime(date_minor_breaks = '1 month')


#### PAR sensors ####
range(buoy2013_L1$PAR, na.rm = T)

# ggplot(buoy2013_L1, aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme

buoy2013_L1 <-  buoy2013_L1 %>%
  mutate(PAR = case_when(PAR <0 ~ 0,
                         TRUE ~ PAR))

ggplot(buoy2013_L1, aes(x = datetime, y = PAR)) +
  geom_point() +
  final_theme +
  labs(title = '2013 PAR data below 0 recoded',
       x = NULL,
       y = 'PAR (umol/m2/s)') +
  scale_x_datetime(date_minor_breaks = '1 month')

# ggplot(subset(buoy2013_L1,
#               subset = (datetime>=as.POSIXct('2013-01-01', tz='UTC') &
#                           datetime<as.POSIXct('2013-02-01', tz='UTC'))),
#        aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'jan 2013 PAR data raw',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2013_L1,
#               subset = (datetime>=as.POSIXct('2013-02-01', tz='UTC') &
#                           datetime<as.POSIXct('2013-03-01', tz='UTC'))),
#        aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'feb 2013 PAR data raw',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2013_L1,
#               subset = (datetime>=as.POSIXct('2013-03-01', tz='UTC') &
#                           datetime<as.POSIXct('2013-04-01', tz='UTC'))),
#        aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'mar 2013 PAR data raw',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2013_L1,
#               subset = (datetime>=as.POSIXct('2013-04-01', tz='UTC') &
#                           datetime<as.POSIXct('2013-05-01', tz='UTC'))),
#        aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'apr 2013 PAR data raw',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
#
# ggplot(subset(buoy2013_L1,
#               subset = (datetime>=as.POSIXct('2013-05-01', tz='UTC') &
#                           datetime<as.POSIXct('2013-06-01', tz='UTC'))),
#        aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'may 2013 PAR data raw',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2013_L1,
#               subset = (datetime>=as.POSIXct('2013-06-01', tz='UTC') &
#                           datetime<as.POSIXct('2013-07-01', tz='UTC'))),
#        aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'jun 2013 PAR data raw',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2013_L1,
#               subset = (datetime>=as.POSIXct('2013-07-01', tz='UTC') &
#                           datetime<as.POSIXct('2013-08-01', tz='UTC'))),
#        aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'jul 2013 PAR data raw',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2013_L1,
#               subset = (datetime>=as.POSIXct('2013-08-01', tz='UTC') &
#                           datetime<as.POSIXct('2013-09-01', tz='UTC'))),
#        aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'aug 2013 PAR data raw',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2013_L1,
#               subset = (datetime>=as.POSIXct('2013-09-01', tz='UTC') &
#                           datetime<as.POSIXct('2013-10-01', tz='UTC'))),
#        aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'sept 2013 PAR data raw',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2013_L1,
#               subset = (datetime>=as.POSIXct('2013-10-01', tz='UTC') &
#                           datetime<as.POSIXct('2013-11-01', tz='UTC'))),
#        aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'oct 2013 PAR data raw',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')

#buoy offline

#recode PAr when buoy in transit
buoy2013_L1 <- buoy2013_L1 %>% 
  mutate(PAR = case_when(location == 'in transit' ~ NA_real_,
                         TRUE ~ PAR))


ggplot(buoy2013_L1, aes(x = datetime, y = PAR, color = location)) +
  geom_point() +
  final_theme +
  labs(title = '2013 PAR data below 0 recoded',
       x = NULL,
       y = 'PAR (umol/m2/s)') +
  scale_x_datetime(date_minor_breaks = '1 month')


#### Air Temp ####
range(buoy2013_L1$AirTempC)

ggplot(buoy2013_L1, aes(x=datetime, y = AirTempC)) +
  geom_point() +
  final_theme +
  labs(title = '2013 air temp raw',
       x= NULL,
       y= 'air temp (deg C)') +
  scale_x_datetime(date_minor_breaks = '1 month')

# ggplot(subset(buoy2013_L1,
#               subset=(datetime>=as.POSIXct('2013-01-01', tz='UTC') &
#                         datetime < as.POSIXct('2013-02-01', tz='UTC'))),
#        aes(x=datetime, y = AirTempC)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'jan2013 air temp raw',
#        x= NULL,
#        y= 'air temp (deg C)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2013_L1,
#               subset=(datetime>=as.POSIXct('2013-02-01', tz='UTC') &
#                         datetime < as.POSIXct('2013-03-01', tz='UTC'))),
#        aes(x=datetime, y = AirTempC)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'feb 2013 air temp raw',
#        x= NULL,
#        y= 'air temp (deg C)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2013_L1,
#               subset=(datetime>=as.POSIXct('2013-03-01', tz='UTC') &
#                         datetime < as.POSIXct('2013-04-01', tz='UTC'))),
#        aes(x=datetime, y = AirTempC)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'mar 2013 air temp raw',
#        x= NULL,
#        y= 'air temp (deg C)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2013_L1,
#               subset=(datetime>=as.POSIXct('2013-04-01', tz='UTC') &
#                         datetime < as.POSIXct('2013-05-01', tz='UTC'))),
#        aes(x=datetime, y = AirTempC)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'apr 2013 air temp raw',
#        x= NULL,
#        y= 'air temp (deg C)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2013_L1,
#               subset=(datetime>=as.POSIXct('2013-05-01', tz='UTC') &
#                         datetime < as.POSIXct('2013-06-01', tz='UTC'))),
#        aes(x=datetime, y = AirTempC)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'may 2013 air temp raw',
#        x= NULL,
#        y= 'air temp (deg C)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
#
# ggplot(subset(buoy2013_L1,
#               subset=(datetime>=as.POSIXct('2013-06-01', tz='UTC') &
#                         datetime < as.POSIXct('2013-07-01', tz='UTC'))),
#        aes(x=datetime, y = AirTempC)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'jun 2013 air temp raw',
#        x= NULL,
#        y= 'air temp (deg C)') +
#   scale_x_datetime(date_minor_breaks = '1 day')

#errant point on Jun 30

buoy2013_L1 <- buoy2013_L1 %>% 
  mutate(AirTempC = case_when(datetime >= as.POSIXct('2013-06-30', tz='UTC') &
                                datetime < as.POSIXct('2013-07-01', tz='UTC') & 
                                AirTempC >50 ~ NA_real_,
                              TRUE ~ AirTempC))
           
# ggplot(subset(buoy2013_L1,
#               subset=(datetime>=as.POSIXct('2013-06-01', tz='UTC') &
#                         datetime < as.POSIXct('2013-07-01', tz='UTC'))),
#        aes(x=datetime, y = AirTempC)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'jun 2013 air temp clean',
#        x= NULL,
#        y= 'air temp (deg C)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2013_L1,
#               subset=(datetime>=as.POSIXct('2013-07-01', tz='UTC') &
#                         datetime < as.POSIXct('2013-08-01', tz='UTC'))),
#        aes(x=datetime, y = AirTempC)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'jul 2013 air temp raw',
#        x= NULL,
#        y= 'air temp (deg C)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2013_L1,
#               subset=(datetime>=as.POSIXct('2013-08-01', tz='UTC') &
#                         datetime < as.POSIXct('2013-09-01', tz='UTC'))),
#        aes(x=datetime, y = AirTempC)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'aug 2013 air temp raw',
#        x= NULL,
#        y= 'air temp (deg C)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2013_L1,
#               subset=(datetime>=as.POSIXct('2013-09-01', tz='UTC') &
#                         datetime < as.POSIXct('2013-10-01', tz='UTC'))),
#        aes(x=datetime, y = AirTempC)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'sept 2013 air temp raw',
#        x= NULL,
#        y= 'air temp (deg C)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2013_L1,
#               subset=(datetime>=as.POSIXct('2013-10-01', tz='UTC') &
#                         datetime < as.POSIXct('2013-11-01', tz='UTC'))),
#        aes(x=datetime, y = AirTempC)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'oct 2013 air temp raw',
#        x= NULL,
#        y= 'air temp (deg C)') +
#   scale_x_datetime(date_minor_breaks = '1 day')

#buoy moved to harbor oct 16
buoy2013_L1 <- buoy2013_L1 %>% 
  mutate(AirTempC = case_when(location == 'in transit' ~ NA_real_,
                         TRUE ~ AirTempC))

#buoy offline

ggplot(buoy2013_L1,
       aes(x=datetime, y = AirTempC, color = location)) +
  geom_point() +
  final_theme +
  labs(title = '2013 air temp clean',
       x= NULL,
       y= 'air temp (deg C)') +
  scale_x_datetime(date_minor_breaks = '1 month')




#### EXPORT L1 FILES ####

#recode flags when buoy offline
buoy2013_L1 <-  buoy2013_L1 %>% 
  mutate(upper_do_flag = case_when(location == 'offline' ~ '',
                             TRUE ~ upper_do_flag))  %>% 
  mutate(lower_do_flag = case_when(location == 'offline' ~ '',
                                   TRUE ~ lower_do_flag))

# #export L1 tempstring file
# buoy2013_L1 %>%
#   select(datetime, location, alltemp2011) %>%
#   rename(TempC_9p5m = 'TempC_9m',
#          TempC_8p5m = 'TempC_8m',
#          TempC_7p5m = 'TempC_7m',
#          TempC_6p5m = 'TempC_6m',
#          TempC_5p5m = 'TempC_5m',
#          TempC_4p5m = 'TempC_4m',
#          TempC_3p5m = 'TempC_3m',
#          TempC_2p5m = 'TempC_2m',
#          TempC_1p5m = 'TempC_1m',
#          TempC_0p5m = 'TempC_0m') %>%
#   mutate(datetime = as.character(datetime)) %>%
#   write_csv(., 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2013_tempstring_L1.csv')

#crete vertical dataset
buoy_2013_L1_vert <- buoy2013_L1 %>%
  select(datetime, location, TempC_0m, TempC_1m, TempC_2m, TempC_3m, TempC_4m, TempC_5m, TempC_6m, TempC_7m, TempC_8m, TempC_9m) %>%
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
  gather(depth_m, temp_degC, -datetime, -location) %>% 
  mutate(depth_m = case_when(grepl(pattern = '_0p5m', x = depth_m) ~ '0.5',
                             grepl(pattern = '_1p5m', x = depth_m) ~ '1.5',
                             grepl(pattern = '_2p5m', x = depth_m) ~ '2.5',
                             grepl(pattern = '_3p5m', x = depth_m) ~ '3.5',
                             grepl(pattern = '_4p5m', x = depth_m) ~ '4.5',
                             grepl(pattern = '_5p5m', x = depth_m) ~ '5.5',
                             grepl(pattern = '_6p5m', x = depth_m) ~ '6.5',
                             grepl(pattern = '_7p5m', x = depth_m) ~ '7.5',
                             grepl(pattern = '_8p5m', x = depth_m) ~ '8.5',
                             grepl(pattern = '_9p5m', x = depth_m) ~ '9.5',
                             TRUE ~ NA_character_)) %>% 
  mutate(depth_m = as.numeric(depth_m))


#no flags to parse                   

#plot to check
ggplot(buoy_2013_L1_vert, aes(x = datetime, y = temp_degC, color = as.factor(depth_m))) +
  geom_point() +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
                              "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust


#order by date, depth
buoy_2013_L1_vert <- buoy_2013_L1_vert %>% 
  arrange(datetime, depth_m)

# buoy_2013_L1_vert %>%
#   mutate(datetime = as.character(datetime)) %>%
#   write_csv(., 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2013_tempstring_vert_L1.csv')
# 
# 
# # export L1 DO file
# buoy2013_L1 %>%
#   select(datetime, location, DOSat, DOppm, DOTempC, DOLowSat, DOLowPPM, DOLowTempC, upper_do_flag, lower_do_flag) %>%
#   mutate(datetime = as.character(datetime)) %>%
#   write_csv(., 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2013_do_L1.csv')
# 
# #export wind file
# buoy2013_L1 %>%
#   select(datetime, location, WindSp, CorrWind, AveWindSp, AveWindDir, MaxWindSp, MaxWindDir) %>%
#   mutate(datetime = as.character(datetime)) %>%
#   rename(WindSp_ms = 'WindSp',
#          WindDir_deg = 'CorrWind',
#          AveWindSp_ms = 'AveWindSp',
#          AveWindDir_deg = 'AveWindDir',
#          MaxWindSp_ms = 'MaxWindSp',
#          MaxWindDir_deg = 'MaxWindDir') %>%
#   write_csv(., 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2013_wind_L1.csv')
# 
# #export PAR file
# buoy2013_L1 %>%
#   select(datetime, location, PAR) %>%
#   mutate(datetime = as.character(datetime)) %>%
#   rename(PAR_umolm2s = 'PAR') %>%
#   write_csv(., 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2013_PAR_L1.csv')
# 
# #export air temp file
# buoy2013_L1 %>%
#   select(datetime, location, AirTempC) %>%
#   mutate(datetime = as.character(datetime)) %>%
#   rename(AirTemp_degC = 'AirTempC') %>%
#   write_csv(., 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2013_AirTemp_L1.csv')
# 

