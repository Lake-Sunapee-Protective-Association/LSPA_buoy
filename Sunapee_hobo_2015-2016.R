#*****************************************************************
#*      Cary Institute of Ecosystem Studies (Millbrook, NY)      *
#*                                                               *
#* TITLE:   Sunapee_buoy_2015.r                                  *
#* AUTHOR:  Bethel Steele                                        *
#* SYSTEM:  Lenovo ThinkCentre, Win 10, R 3.4.2, RStudio 1.1.383 *
#* PROJECT: Lake Sunapee Buoy Data Cleaning                      *
#* PURPOSE: create L0 and L1 data for buoy data 2015 using       *
#*          similar methods to CCC and DR                        *
#* PREVIOUS VERSION: 'Sunapee_buoy_2015_17Oct2017.R'             *
#*                   'Sunapee_buoy_2015_11Oct2017.R'             *
#*                   'Sunapee_buoy_2014-2016_07Aug2017.R'        *
#*****************************************************************

#bring in summer 2015 hobo raw data
hobo_w15 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L0/winter hobo data/2015-16 Winter Pendant.csv',
                      col_types = 'icnnnnnnnnnnnnnnnnnn')

#format date of hobo sensors
hobo_w15 <- hobo_w15 %>% 
  rename(Date.Time = 'Date/Time') %>% 
  mutate(datetime = as.POSIXct(Date.Time, format='%m/%d/%Y %H:%M', tz='UTC')) %>% 
  select(-Date.Time)

#### hobo line ####
hobo_vert <- hobo_w15 %>%
  select(datetime, TempC_1m, TempC_2m, TempC_3m, TempC_4m, TempC_5m, TempC_6m, TempC_7m, TempC_8m, TempC_9m) %>%
  gather(variable, value, -datetime)

ggplot(hobo_vert, aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='hobo temp sensor winter 2015-2016', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  scale_x_datetime(date_breaks = '1 month') +
  final_theme

# ggplot(subset(hobo_vert, subset=(datetime>='2015-10-01' & datetime < '2015-11-01')), aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='hobo temp Oct 2015', x='date', y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme
# 
# ggplot(subset(hobo_vert, subset=(datetime>='2015-11-01' & datetime < '2015-12-01')), aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='hobo temp Nov 2015', x='date', y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme
# 
# ggplot(subset(hobo_vert, subset=(datetime>='2015-12-01' & datetime < '2016-01-01')), aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='hobo temp Dec 2015', x='date', y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme
# 
# ggplot(subset(hobo_vert, subset=(datetime>='2016-01-01' & datetime < '2016-02-01')), aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='hobo temp Jan 2016', x='date', y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme
# 
# ggplot(subset(hobo_vert, subset=(datetime>='2016-02-01' & datetime < '2016-03-01')), aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='hobo temp Feb 2016', x='date', y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme
# 
# ggplot(subset(hobo_vert, subset=(datetime>='2016-03-01' & datetime < '2016-04-01')), aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='hobo temp Mar 2016', x='date', y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme
# 
# ggplot(subset(hobo_vert, subset=(datetime>='2016-04-01' & datetime < '2016-05-04')), aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='hobo temp Apr 2016', x='date', y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme



#export L1 tempstring file
hobo_w15 %>%
  select(datetime, TempC_1m, TempC_2m, TempC_3m, TempC_4m, TempC_5m, TempC_6m, TempC_7m, TempC_8m, TempC_9m) %>%
  rename(TempC_1p5m = 'TempC_1m',
         TempC_2p5m = 'TempC_2m',
         TempC_3p5m = 'TempC_3m',
         TempC_4p5m = 'TempC_4m',
         TempC_5p5m = 'TempC_5m',
         TempC_6p5m = 'TempC_6m',
         TempC_7p5m = 'TempC_7m',
         TempC_8p5m = 'TempC_8m',
         TempC_9p5m = 'TempC_9m') %>%
  mutate(datetime = as.character(datetime)) %>%
  write_csv(., 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2015-2016_hobotempstring_L1.csv')

#clean up workspace
rm(hobo_vert)




