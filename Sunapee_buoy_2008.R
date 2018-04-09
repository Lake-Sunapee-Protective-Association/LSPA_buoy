#*****************************************************************
#*      Cary Institute of Ecosystem Studies (Millbrook, NY)      *
#*                                                               *
#* TITLE:   Sunapee_buoy_2008.r                                  *
#* AUTHOR:  Bethel Steele                                        *
#* SYSTEM:  Lenovo ThinkCentre, Win 10, R 3.4.2, RStudio 1.1.383 *
#* PROJECT: Lake Sunapee Buoy Data Cleaning                      *
#* PURPOSE: subset data for met/compare with L1                  *
#*****************************************************************

#bring in 2008 buoy raw data
buoy2008_L0 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L0/Sunapee2008_rawData.csv',
                        col_names = c('datetime', 'AirTempC', 'BattVolt', 'DOSat', 'DOppm', 
                                      'DOSat2', 'PAR', 'BattVolt2', 'RH', 'DOTempC', 
                                      'TempC_0m', 'TempC_0p5m', 'TempC_1m', 'TempC_1p5m', 'TempC_2m', 
                                      'TempC_2p5m', 'TempC_3m', 'TempC_4m', 'TempC_5m', 'TempC_6m',
                                      'TempC_7m', 'TempC_8m', 'TempC_9m', 'TempC_10m', 'TempC_11m',
                                      'TempC_13m', 'InstWindDir', 'AveWindDir', 'AveWindSp', 'InstWindSp'), 
                     col_types = 'cnnnnnnnnnnnnnnnnnnnnnnnnnnnnn',
                     skip=1) %>% 
  select(-BattVolt, -DOSat2, -BattVolt2, -AveWindDir, -AveWindSp) %>%  #drop blank columns
  mutate(datetime = as.POSIXct(datetime, format='%Y-%m-%d %H:%M:%S', tz='UTC'))


#bring in 2008 LMP data for comparison
LMP2008 <- read_xlsx('C:/Users/steeleb/Dropbox/Lake Sunapee/long term Sunapee data/raw data/LMP files/LMP 2017/DO.xlsx', 
                     sheet='DO',
                     col_types = c('text', 'text', 'numeric', 'guess', 'numeric', 
                                   'numeric', 'numeric', 'numeric', 'numeric' ,'numeric',
                                   'text', 'text', 'numeric', 'text')) %>% 
  mutate(DATE = as.Date(DATE, format='%Y-%m-%d')) %>%  #format date
  filter(DATE >= '2008-01-01' & DATE < '2009-01-01', #filter for 2016 only
         STATION == 210)

LMP2008_temp <- LMP2008 %>% 
  select(DATE, DEPTH, TEMP) %>% 
  filter(DEPTH<=14) %>% 
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
                              variable == '12' ~ 'TempC_12m',
                              variable == '13' ~ 'TempC_13m',
                              variable == '14' ~ 'TempC_14m'))


#### thermisters ####
buoy2008_temp_vert <- buoy2008_L0 %>%
  select(datetime, alltemp2007) %>%
  gather(variable, value, -datetime) %>% 
  mutate(variable = factor(variable, levels=alltemp2007))

# ggplot(subset(buoy2008_temp_vert,
#               subset=(datetime >=as.POSIXct('2008-01-01', tz='UTC') &
#                         datetime < as.POSIXct('2009-01-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   labs(title = '2008 buoy temp data - raw',
#        x=NULL,
#        y='temp (deg C)') +
#   final_theme +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust

buoy2008_L1 <- buoy2008_L0 %>% 
  mutate_at(vars(alltemp2007),
            funs(case_when(. == -6999 ~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate(location = 'loon')

buoy2008_temp_vert <- buoy2008_L1 %>%
  select(datetime, location, alltemp2007) %>%
  gather(variable, value, -location, -datetime) %>%
  mutate(variable = factor(variable, levels=alltemp2007))

# ggplot(subset(buoy2008_temp_vert,
#               subset=(datetime >=as.POSIXct('2008-01-01', tz='UTC') &
#                         datetime < as.POSIXct('2009-01-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 month') +
#   labs(title = '2008 buoy temp data - NAs recoded',
#        x=NULL,
#        y='temp (deg C)') +
#   final_theme +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust

#10-13 hung up all year - temps never separated in summer
buoy2008_L1 <- buoy2008_L1 %>% 
  mutate(TempC_10m = as.numeric(NA), 
         TempC_11m = as.numeric(NA), 
         TempC_13m = as.numeric(NA))

buoy2008_temp_vert_b <- buoy2008_L1 %>%
  select(datetime, location, alltemp2007) %>%
  gather(variable, value, -location, -datetime) %>%
  mutate(variable = factor(variable, levels=alltemp2007))

# ggplot(subset(buoy2008_temp_vert,
#               subset=(datetime >=as.POSIXct('2008-01-01', tz='UTC') &
#                         datetime < as.POSIXct('2008-02-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   labs(title='Jan 2008, NAs recoded',
#        x=NULL,
#        y='temp (deg C)') +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# ggplot(subset(buoy2008_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2008-01-01', tz='UTC') &
#                         datetime < as.POSIXct('2008-02-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   labs(title='Jan 2008, clean',
#        x=NULL,
#        y='temp (deg C)') +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# ggplot(subset(buoy2008_temp_vert,
#               subset=(datetime >=as.POSIXct('2008-02-01', tz='UTC') &
#                         datetime < as.POSIXct('2008-03-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   labs(title='Feb 2008, NAs recoded',
#        x=NULL,
#        y='temp (deg C)') +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# #inspect mixing Feb 18 against air temp
# buoytemp_20080218 <- ggplot(subset(buoy2008_temp_vert,
#               subset=(datetime >=as.POSIXct('2008-02-18', tz='UTC') &
#                         datetime < as.POSIXct('2008-02-19', tz='UTC'))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   labs(title='Feb 2008\nodd temp event on 18th',
#        x=NULL,
#        y='temp (deg C)') +
#   geom_line(size=1.5) +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme +
#   theme(legend.position = 'bottom')+
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# airtemp_20080218 <- ggplot(subset(buoy2008_L1,
#               subset=(datetime >=as.POSIXct('2008-02-18', tz='UTC') &
#                         datetime < as.POSIXct('2008-02-19', tz='UTC'))),
#        aes(x=datetime, y=AirTempC)) +
#   labs(title = 'Air Temp\nFeb 18, 2008',
#        x=NULL,
#        y='air temp (deg C)') +
#   geom_line() +
#   final_theme
# multiplot(airtemp_20080218, buoytemp_20080218)
# 
# ggplot(subset(buoy2008_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2008-02-01', tz='UTC') &
#                         datetime < as.POSIXct('2008-03-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   labs(title='Feb 2008, clean',
#        x=NULL,
#        y='temp (deg C)') +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# ggplot(subset(buoy2008_temp_vert,
#               subset=(datetime >=as.POSIXct('2008-03-01', tz='UTC') &
#                         datetime < as.POSIXct('2008-04-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=variable)) +
#   labs(title='Mar 2008, NAs recoded',
#        x=NULL,
#        y='temp (deg C)') +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# #inspect odd balls on Mar 8&25
# buoytemp_20080308 <- ggplot(subset(buoy2008_temp_vert,
#                                    subset=(datetime >=as.POSIXct('2008-03-08', tz='UTC') &
#                                              datetime < as.POSIXct('2008-03-09', tz='UTC'))),
#                             aes(x=datetime, y=value, color=as.factor(variable))) +
#   labs(title='Mar 2008, NAs recoded',
#        x=NULL,
#        y='temp (deg C)') +
#   geom_line() +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme +
#   theme(legend.position = 'bottom') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# airtemp_20080308 <- ggplot(subset(buoy2008_L1,
#                                   subset=(datetime >=as.POSIXct('2008-03-08', tz='UTC') &
#                                             datetime < as.POSIXct('2008-03-09', tz='UTC'))),
#                            aes(x=datetime, y=AirTempC)) +
#   labs(title = 'Air Temp\nMar 08, 2008',
#        x=NULL,
#        y='air temp (deg C)') +
#   geom_line() +
#   final_theme
# windspeed_20080308 <- ggplot(subset(buoy2008_L1,
#                                   subset=(datetime >=as.POSIXct('2008-03-08', tz='UTC') &
#                                             datetime < as.POSIXct('2008-03-09', tz='UTC'))),
#                            aes(x=datetime, y=InstWindSp)) +
#   labs(title = 'Wind Speed\nMar 08, 2008',
#        x=NULL,
#        y='Wind Speed') +
#   geom_line() +
#   final_theme
# multiplot(airtemp_20080308, buoytemp_20080308)
# 
# buoytemp_20080325 <- ggplot(subset(buoy2008_temp_vert,
#               subset=(datetime >=as.POSIXct('2008-03-25', tz='UTC') &
#                         datetime < as.POSIXct('2008-03-26', tz='UTC'))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   labs(title='Mar 2008, NAs recoded',
#        x=NULL,
#        y='temp (deg C)') +
#   geom_line() +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme +
#   theme(legend.position = 'bottom') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# airtemp_20080325 <- ggplot(subset(buoy2008_L1,
#                                   subset=(datetime >=as.POSIXct('2008-03-25', tz='UTC') &
#                                             datetime < as.POSIXct('2008-03-26', tz='UTC'))),
#                            aes(x=datetime, y=AirTempC)) +
#   labs(title = 'Air Temp\nMar 25, 2008',
#        x=NULL,
#        y='air temp (deg C)') +
#   geom_line() +
#   final_theme
# windspeed_20080325 <- ggplot(subset(buoy2008_L1,
#                                   subset=(datetime >=as.POSIXct('2008-03-25', tz='UTC') &
#                                             datetime < as.POSIXct('2008-03-26', tz='UTC'))),
#                            aes(x=datetime, y=InstWindSp)) +
#   labs(title = 'Wind Speed\nMar 25, 2008',
#        x=NULL,
#        y='WindSpeed') +
#   geom_line() +
#   final_theme
# multiplot(airtemp_20080325, buoytemp_20080325)

buoy2008_L1 <- buoy2008_L1 %>%
  mutate_at(vars(alltemp2007),
            funs(case_when(datetime == as.POSIXct('2008-03-08 18:40', tz='UTC') ~ NA_real_,
                           datetime == as.POSIXct('2008-03-25 14:00', tz='UTC') ~ NA_real_,
                           TRUE ~ .)))
buoy2008_temp_vert_b <- buoy2008_L1 %>%
  select(datetime, location, alltemp2007) %>%
  gather(variable, value, -location, -datetime) %>%
  mutate(variable = factor(variable, levels=alltemp2007))

# ggplot(subset(buoy2008_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2008-03-01', tz='UTC') &
#                         datetime < as.POSIXct('2008-04-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   labs(title='Mar 2008, clean',
#        x=NULL,
#        y='temp (deg C)') +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# ggplot(subset(buoy2008_temp_vert,
#               subset=(datetime >=as.POSIXct('2008-04-01', tz='UTC') &
#                         datetime < as.POSIXct('2008-05-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=variable)) +
#   labs(title='Apr 2008, NAs recoded',
#        x=NULL,
#        y='temp (deg C)') +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# #inspect Apr 1, 15
# buoytemp_20080401 <- ggplot(subset(buoy2008_temp_vert,
#                                    subset=(datetime >=as.POSIXct('2008-04-01', tz='UTC') &
#                                              datetime < as.POSIXct('2008-04-02', tz='UTC'))),
#                             aes(x=datetime, y=value, color=as.factor(variable))) +
#   labs(title='Apr 2008, NAs recoded',
#        x=NULL,
#        y='temp (deg C)') +
#   geom_line(size=1.5) +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme +
#   theme(legend.position = 'bottom') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# airtemp_20080401 <- ggplot(subset(buoy2008_L1,
#                                   subset=(datetime >=as.POSIXct('2008-04-01', tz='UTC') &
#                                             datetime < as.POSIXct('2008-04-02', tz='UTC'))),
#                            aes(x=datetime, y=AirTempC)) +
#   labs(title = 'Air Temp\nApr 01, 2008',
#        x=NULL,
#        y='air temp (deg C)') +
#   geom_line() +
#   final_theme
# multiplot(airtemp_20080401, buoytemp_20080401)
# 
# buoytemp_20080415 <- ggplot(subset(buoy2008_temp_vert,
#                                    subset=(datetime >=as.POSIXct('2008-04-15', tz='UTC') &
#                                              datetime < as.POSIXct('2008-04-16', tz='UTC'))),
#                             aes(x=datetime, y=value, color=as.factor(variable))) +
#   labs(title='Apr 2008, NAs recoded',
#        x=NULL,
#        y='temp (deg C)') +
#   geom_line(size=1.5) +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme +
#   theme(legend.position = 'bottom') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# airtemp_20080415 <- ggplot(subset(buoy2008_L1,
#                                   subset=(datetime >=as.POSIXct('2008-04-15', tz='UTC') &
#                                             datetime < as.POSIXct('2008-04-16', tz='UTC'))),
#                            aes(x=datetime, y=AirTempC)) +
#   labs(title = 'Air Temp\nApr 15, 2008',
#        x=NULL,
#        y='air temp (deg C)') +
#   geom_line() +
#   final_theme
# multiplot(airtemp_20080415, buoytemp_20080415)
# 
# ggplot(subset(buoy2008_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2008-04-01', tz='UTC') &
#                         datetime < as.POSIXct('2008-05-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=(variable))) +
#   labs(title='Apr 2008, clean',
#        x=NULL,
#        y='temp (deg C)') +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# ggplot(subset(buoy2008_temp_vert,
#               subset=(datetime >=as.POSIXct('2008-05-01', tz='UTC') &
#                         datetime < as.POSIXct('2008-06-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=(variable))) +
#   labs(title='May 2008, NAs recoded',
#        x=NULL,
#        y='temp (deg C)') +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# #odball may 22/23
# ggplot(subset(buoy2008_temp_vert,
#               subset=(datetime >=as.POSIXct('2008-05-21 12:00', tz='UTC') &
#                         datetime < as.POSIXct('2008-05-22 12:00', tz='UTC'))),
#        aes(x=datetime, y=value, color=variable)) +
#   labs(title='May 2008, NAs recoded',
#        x='date',
#        y='temp deg C') +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust

buoy2008_L1<- buoy2008_L1 %>%
  mutate_at(vars(TempC_8m),
            funs(case_when(datetime == as.POSIXct('2008-05-22 0:50', tz='UTC') ~ NA_real_,
                           TRUE ~ .)))

buoy2008_temp_vert_b <- buoy2008_L1 %>%
  select(datetime, location, alltemp2007) %>%
  gather(variable, value, -location, -datetime) %>%
  mutate(variable = factor(variable, levels=alltemp2007))

# ggplot(subset(buoy2008_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2008-05-01', tz='UTC') &
#                         datetime < as.POSIXct('2008-06-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=(variable))) +
#   labs(title='May 2008, clean',
#        x=NULL,
#        y='temp (deg C)') +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# ggplot(subset(buoy2008_temp_vert,
#               subset=(datetime >=as.POSIXct('2008-06-01', tz='UTC') &
#                         datetime < as.POSIXct('2008-07-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   labs(title='Jun 2008, NAs recoded',
#        x='date',
#        y='temp deg C') +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# ggplot(subset(buoy2008_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2008-06-01', tz='UTC') &
#                         datetime < as.POSIXct('2008-07-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   labs(title='Jun 2008, clean',
#        x='date',
#        y='temp deg C') +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# ggplot(subset(buoy2008_temp_vert,
#               subset=(datetime >=as.POSIXct('2008-07-01', tz='UTC') &
#                         datetime < as.POSIXct('2008-08-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=(variable))) +
#   labs(title='Jul 2008, NAs recoded',
#        x=NULL,
#        y='temp (deg C)') +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# ggplot(subset(buoy2008_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2008-07-01', tz='UTC') &
#                         datetime < as.POSIXct('2008-08-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=(variable))) +
#   labs(title='Jul 2008, clean',
#        x=NULL,
#        y='temp (deg C)') +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# ggplot(subset(buoy2008_temp_vert,
#               subset=(datetime >=as.POSIXct('2008-08-01', tz='UTC') &
#                         datetime < as.POSIXct('2008-09-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=(variable))) +
#   labs(title='Aug 2008, NAs recoded',
#        x=NULL,
#        y='temp (deg C)') +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# ggplot(subset(buoy2008_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2008-08-01', tz='UTC') &
#                         datetime < as.POSIXct('2008-09-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=(variable))) +
#   labs(title='Aug 2008, clean',
#        x=NULL,
#        y='temp (deg C)') +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# ggplot(subset(buoy2008_temp_vert,
#               subset=(datetime >=as.POSIXct('2008-09-01', tz='UTC') &
#                         datetime < as.POSIXct('2008-10-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   labs(title='Sept 2008, NAs recoded',
#        x=NULL,
#        y='temp (deg C)') +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# #oddball on Sept 27
# ggplot(subset(buoy2008_temp_vert,
#               subset=(datetime >=as.POSIXct('2008-09-27', tz='UTC') &
#                         datetime < as.POSIXct('2008-09-28', tz='UTC'))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   labs(title='Sept 2008, NAs recoded',
#        x='date',
#        y='temp deg C') +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
buoy2008_L1 <- buoy2008_L1 %>%
  mutate_at(vars(TempC_8m),
            funs(case_when(datetime == as.POSIXct('2008-09-27 3:00', tz='UTC') ~ NA_real_,
                           TRUE ~ .)))

buoy2008_temp_vert_b <- buoy2008_L1 %>%
  select(datetime, location, alltemp2007) %>%
  gather(variable, value, -location, -datetime) %>%
  mutate(variable = factor(variable, levels=alltemp2007p))

# ggplot(subset(buoy2008_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2008-09-01', tz='UTC') &
#                         datetime < as.POSIXct('2008-10-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   labs(title='Sept 2008, clean',
#        x='date',
#        y='temp deg C') +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89",
#                               "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# ggplot(subset(buoy2008_temp_vert,
#               subset=(datetime >=as.POSIXct('2008-10-01', tz='UTC') &
#                         datetime < as.POSIXct('2008-11-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   labs(title='Oct 2008, NAs recoded',
#        x=NULL,
#        y='temp (deg C)') +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjus
# 
# #odd balls Oct 13 and 15
# ggplot(subset(buoy2008_temp_vert,
#               subset=(datetime >=as.POSIXct('2008-10-13', tz='UTC') &
#                         datetime < as.POSIXct('2008-10-14', tz='UTC'))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   labs(title='Oct 2008, NAs recoded',
#        x='date',
#        y='temp deg C') +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjus
# ggplot(subset(buoy2008_temp_vert,
#               subset=(datetime >=as.POSIXct('2008-10-15', tz='UTC') &
#                         datetime < as.POSIXct('2008-10-16', tz='UTC'))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   labs(title='Oct 2008, NAs recoded',
#        x='date',
#        y='temp deg C') +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjus

buoy2008_L1 <- buoy2008_L1 %>%
  mutate_at(vars(TempC_8m),
            funs(case_when(datetime == as.POSIXct('2008-10-13 10:20', tz='UTC') ~ NA_real_,
                           datetime == as.POSIXct('2008-10-15 13:00', tz='UTC') ~ NA_real_,
                           TRUE ~ .)))

buoy2008_temp_vert_b <- buoy2008_L1 %>%
  select(datetime, location, alltemp2007) %>%
  gather(variable, value, -location, -datetime) %>%
  mutate(variable = factor(variable, levels=alltemp2007p))

# ggplot(subset(buoy2008_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2008-10-01', tz='UTC') &
#                         datetime < as.POSIXct('2008-11-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   labs(title='Oct 2008, clean',
#        x='date',
#        y='temp deg C') +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjus
# 
# ggplot(subset(buoy2008_temp_vert,
#               subset=(datetime >=as.POSIXct('2008-11-01', tz='UTC') &
#                         datetime < as.POSIXct('2008-12-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=(variable))) +
#   labs(title='Nov 2008, NAs recoded',
#        x=NULL,
#        y='temp (deg C)') +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# ggplot(subset(buoy2008_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2008-11-01', tz='UTC') &
#                         datetime < as.POSIXct('2008-12-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=(variable))) +
#   labs(title='Nov 2008, clean',
#        x=NULL,
#        y='temp (deg C)') +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# ggplot(subset(buoy2008_temp_vert,
#               subset=(datetime >=as.POSIXct('2008-12-01', tz='UTC') &
#                         datetime < as.POSIXct('2009-01-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=(variable))) +
#   labs(title='Dec 2008, NAs recoded',
#        x=NULL,
#        y='temp (deg C)') +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# ggplot(subset(buoy2008_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2008-12-01', tz='UTC') &
#                         datetime < as.POSIXct('2009-01-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=(variable))) +
#   labs(title='Dec 2008, clean',
#        x=NULL,
#        y='temp (deg C)') +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# ggplot(subset(buoy2008_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2008-01-01', tz='UTC') &
#                         datetime < as.POSIXct('2009-01-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   labs(title='2008 buoy temp data, clean',
#        x=NULL,
#        y='temp (deg C)') +
#   scale_x_datetime(date_minor_breaks = '1 month') +
#   final_theme +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust


# #reality check with LMP
# buoy_LMP_2008 <- buoy2008_temp_vert_b %>%
#   mutate(source='buoy') %>%
#   full_join(., LMP2008_temp) %>%
#   select(-location) %>% 
#   mutate(variable = factor(variable, levels=alltemp2007LMP))
# unique(LMP2008_temp$datetime)
# 
# #June 3, July 1, July 29, Sept 2
# ggplot(subset(buoy_LMP_2008,
#               subset=(datetime>=as.POSIXct('2008-06-03', tz='UTC') &
#                         datetime<as.POSIXct('2008-06-04', tz='UTC'))),
#        aes(x=datetime, y=value, color=(variable), shape = source)) +
#   geom_point() +
#   final_theme +
#   labs(title='temp reality check\n2008 buoy and LMP site 210 data\nJune 3, 2008',
#        x=NULL,
#        y='temp (deg C)') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# ggplot(subset(buoy_LMP_2008,
#               subset=(datetime>=as.POSIXct('2008-07-01', tz='UTC') &
#                         datetime<as.POSIXct('2008-07-02', tz='UTC'))),
#        aes(x=datetime, y=value, color=as.factor(variable), shape = source)) +
#   geom_point() +
#   final_theme +
#   labs(title='temp reality check\n2008 buoy and LMP site 210 data\nJuly 1, 2008',
#        x=NULL,
#        y='temp (deg C)') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# ggplot(subset(buoy_LMP_2008,
#               subset=(datetime>=as.POSIXct('2008-07-29', tz='UTC') &
#                         datetime<as.POSIXct('2008-07-30', tz='UTC'))),
#        aes(x=datetime, y=value, color=as.factor(variable), shape = source)) +
#   geom_point() +
#   final_theme +
#   labs(title='temp reality check\n2008 buoy and LMP site 210 data\nJuly 29, 2008',
#        x=NULL,
#        y='temp (deg C)') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# ggplot(subset(buoy_LMP_2008,
#               subset=(datetime>=as.POSIXct('2008-09-02', tz='UTC') &
#                         datetime<as.POSIXct('2008-09-03', tz='UTC'))),
#        aes(x=datetime, y=value, color=as.factor(variable), shape = source)) +
#   geom_point() +
#   final_theme +
#   labs(title='temp reality check\n2008 buoy and LMP site 210 data\nSept 2, 2008',
#        x=NULL,
#        y='temp (deg C)') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust

#export L1 tempstring file
buoy2008_L1 %>%
  select(datetime, TempC_0m, TempC_0p5m, TempC_1m, TempC_1p5m, TempC_2m, TempC_2p5m, TempC_3m, TempC_4m, TempC_5m, TempC_6m, TempC_7m, TempC_8m, TempC_9m, TempC_10m, TempC_11m, TempC_13m) %>%
  mutate(datetime = as.character(datetime)) %>%
  write_csv(., 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2008_tempstring_L1.csv')

#clean up workspace
rm(buoy_LMP_2008, buoy2008_temp_vert, buoy2008_temp_vert_b, LMP2008_temp, buoytemp_20080218, 
   buoytemp_20080308, buoytemp_20080325, buoytemp_20080401, buoytemp_20080415, windspeed_20080308,
   windspeed_20080325, airtemp_20080218, airtemp_20080308, airtemp_20080325, airtemp_20080401, 
   airtemp_20080415)


#### DO sensors ####





