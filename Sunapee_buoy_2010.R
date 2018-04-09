#*****************************************************************
#*      Cary Institute of Ecosystem Studies (Millbrook, NY)      *
#*                                                               *
#* TITLE:   Sunapee_buoy_2010.r                                  *
#* AUTHOR:  Bethel Steele                                        *
#* SYSTEM:  Lenovo ThinkCentre, Win 10, R 3.4.2, RStudio 1.1.383 *
#* PROJECT: Lake Sunapee Buoy Data Cleaning                      *
#* PURPOSE: subset data for met/compare with L1                  *
#*****************************************************************

#bring in 2010 buoy raw data
buoy2010_L0 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L0/Sunapee2010_rawData.csv',
                        col_names = c('datetime', 'AirTempC', 'DOppm', 'DOSat', 'DOSat2', 
                                      'PAR', 'DOTempC', 'TempC_0m', 'TempC_0p5m', 'TempC_1m', 
                                      'TempC_1p5m', 'TempC_2m', 'TempC_2p5m', 'TempC_3m', 'TempC_4m', 
                                      'TempC_5m', 'TempC_6m', 'TempC_7m', 'TempC_8m', 'TempC_9m', 
                                      'TempC_10m', 'TempC_11m', 'TempC_13m', 'AveWindDir', 'InstWindDir', 
                                      'InstWindSp', 'AveWindSp'), 
                        col_types = 'cnnnnnnnnnnnnnnnnnnnnnnnnnn',
                        skip=1) %>% 
  select(-DOSat2) %>%  #drop redundant columns - this has been previously plotted -it is on the 1:1
  mutate(datetime = as.POSIXct(datetime, format='%Y-%m-%d %H:%M:%S', tz='UTC'))

# ggplot(buoy2010_L0, aes(x=DOSat, y=DOSat2)) +
#   geom_point() +
#   geom_abline(intercept = 0, slope=1)

#bring in 2010 LMP data for comparison
LMP2010 <- read_xlsx('C:/Users/steeleb/Dropbox/Lake Sunapee/long term Sunapee data/raw data/LMP files/LMP 2017/DO.xlsx', 
                     sheet='DO',
                     col_types = c('text', 'text', 'numeric', 'guess', 'numeric', 
                                   'numeric', 'numeric', 'numeric', 'numeric' ,'numeric',
                                   'text', 'text', 'numeric', 'text')) %>% 
  mutate(DATE = as.Date(DATE, format='%Y-%m-%d')) %>%  #format date
  filter(DATE >= '2010-01-01' & DATE < '2011-01-01', #filter for 2010 only
         STATION == 210)

LMP2010_temp <- LMP2010 %>% 
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

#create dummy timestamp so there are no blanks
alltimes_2010 <- as.data.frame(seq.POSIXt(as.POSIXct('2010-01-01 00:00', tz='UTC'), as.POSIXct('2010-12-31 23:50', tz='UTC'), '10 min')) %>% 
  rename("datetime" = !!names(.[1]))

buoy2010_L1 <- buoy2010_L0 %>% 
  full_join(., alltimes_2010) %>% 
  arrange(datetime)


#### thermisters ####
buoy2010_temp_vert <- buoy2010_L1 %>%
  select(datetime, alltemp2007) %>%
  gather(variable, value, -datetime) %>% 
  mutate(variable = factor(variable, levels=alltemp2007))

# ggplot(subset(buoy2010_temp_vert,
#               subset=(datetime >=as.POSIXct('2010-01-01', tz='UTC') &
#                         datetime < as.POSIXct('2011-01-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='2010 buoy temp data raw',
#        x=NULL,
#        y='temp (deg C)') +
#   scale_x_datetime(date_minor_breaks = '1 month') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust

buoy2010_L1 <- buoy2010_L1 %>% 
  mutate_at(vars(alltemp2007),
            funs(case_when(. == -6999 ~ NA_real_,
                           . == -99.9 ~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate(location = 'loon')

buoy2010_temp_vert <- buoy2010_L1 %>%
  select(datetime, location, alltemp2007) %>%
  gather(variable, value, -location, -datetime) %>%
  mutate(variable = factor(variable, levels=alltemp2007))

ggplot(subset(buoy2010_temp_vert,
              subset=(datetime >=as.POSIXct('2010-01-01', tz='UTC') &
                        datetime < as.POSIXct('2011-01-01', tz='UTC'))),
       aes(x=datetime, y=value, color=as.factor(variable))) +
  geom_point() +
  final_theme +
  labs(title='2010 buoy temp data, NAs recoded',
       x=NULL,
       y='temp (deg C)') +
  scale_x_datetime(date_minor_breaks = '1 month') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
                              "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust

# ggplot(subset(buoy2010_temp_vert,
#               subset=(datetime >=as.POSIXct('2010-01-01', tz='UTC') &
#                         datetime < as.POSIXct('2010-02-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Jan 2010, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# ggplot(subset(buoy2010_temp_vert,
#               subset=(datetime >=as.POSIXct('2010-01-01', tz='UTC') &
#                         datetime < as.POSIXct('2010-02-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Jan 2010, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   coord_cartesian(ylim=c(-5, 10)) +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_y_continuous(minor_breaks = c(-5:10)) +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust

buoy2010_L1 <- buoy2010_L1 %>% 
  mutate_at(vars(alltemp2007),
            funs(case_when(datetime >= as.POSIXct('2010-01-01', tz='UTC') & datetime < as.POSIXct('2010-02-01', tz='UTC') & (. > 2 | . < -1) ~ NA_real_,
                           TRUE ~ .))) 

buoy2010_temp_vert_b <- buoy2010_L1 %>%
  select(datetime, location, alltemp2007) %>%
  gather(variable, value, -location, -datetime) %>%
  mutate(variable = factor(variable, levels=alltemp2007))

# ggplot(subset(buoy2010_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2010-01-01', tz='UTC') &
#                         datetime < as.POSIXct('2010-02-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Jan 2010, NAs recoded, v2',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust

#errant data Jan 3, Jan9-10, Jan 11, Jan 13-14, Jan 16, Jan 22-23, Jan 29, 30, 31
ggplot(subset(buoy2010_temp_vert_b,
              subset=(datetime >=as.POSIXct('2010-01-03', tz='UTC') &
                        datetime < as.POSIXct('2010-01-04', tz='UTC'))),
       aes(x=datetime, y=value, color=as.factor(variable))) +
  geom_point() +
  final_theme +
  labs(title='Jan 2010, NAs recoded',
       x=NULL,
       y='temp deg C') +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
                              "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust

ggplot(subset(buoy2010_temp_vert_b,
              subset=(datetime >=as.POSIXct('2010-01-08', tz='UTC') &
                        datetime < as.POSIXct('2010-01-09', tz='UTC'))),
       aes(x=datetime, y=value, color=as.factor(variable))) +
  geom_point() +
  final_theme +
  labs(title='Jan 2010, NAs recoded',
       x=NULL,
       y='temp deg C') +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
                              "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust

ggplot(subset(buoy2010_temp_vert_b,
              subset=(datetime >=as.POSIXct('2010-01-09', tz='UTC') &
                        datetime < as.POSIXct('2010-01-10', tz='UTC'))),
       aes(x=datetime, y=value, color=as.factor(variable))) +
  geom_point() +
  final_theme +
  labs(title='Jan 2010, NAs recoded',
       x=NULL,
       y='temp deg C') +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
                              "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust

ggplot(subset(buoy2010_temp_vert_b,
              subset=(datetime >=as.POSIXct('2010-01-11', tz='UTC') &
                        datetime < as.POSIXct('2010-01-12', tz='UTC'))),
       aes(x=datetime, y=value, color=as.factor(variable))) +
  geom_point() +
  final_theme +
  labs(title='Jan 2010, NAs recoded',
       x=NULL,
       y='temp deg C') +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
                              "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
ggplot(subset(buoy2010_temp_vert_b,
              subset=(datetime >=as.POSIXct('2010-01-13', tz='UTC') &
                        datetime < as.POSIXct('2010-01-14 12:00', tz='UTC'))),
       aes(x=datetime, y=value, color=as.factor(variable))) +
  geom_point() +
  final_theme +
  labs(title='Jan 2010, NAs recoded',
       x=NULL,
       y='temp deg C') +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
                              "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
ggplot(subset(buoy2010_temp_vert_b,
              subset=(datetime >=as.POSIXct('2010-01-16', tz='UTC') &
                        datetime < as.POSIXct('2010-01-17 12:00', tz='UTC'))),
       aes(x=datetime, y=value, color=as.factor(variable))) +
  geom_point() +
  final_theme +
  labs(title='Jan 2010, NAs recoded',
       x=NULL,
       y='temp deg C') +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
                              "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust

ggplot(subset(buoy2010_temp_vert_b,
              subset=(datetime >=as.POSIXct('2010-01-22 12:00', tz='UTC') &
                        datetime < as.POSIXct('2010-01-23 16:00', tz='UTC'))),
       aes(x=datetime, y=value, color=as.factor(variable))) +
  geom_point() +
  final_theme +
  labs(title='Jan 2010, NAs recoded',
       x=NULL,
       y='temp deg C') +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
                              "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
ggplot(subset(buoy2010_temp_vert_b,
              subset=(datetime >=as.POSIXct('2010-01-30', tz='UTC') &
                        datetime < as.POSIXct('2010-01-31', tz='UTC'))),
       aes(x=datetime, y=value, color=as.factor(variable))) +
  geom_point() +
  final_theme +
  labs(title='Jan 2010, NAs recoded',
       x=NULL,
       y='temp deg C') +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
                              "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
ggplot(subset(buoy2010_temp_vert_b,
              subset=(datetime >=as.POSIXct('2010-01-31', tz='UTC') &
                        datetime < as.POSIXct('2010-02-01', tz='UTC'))),
       aes(x=datetime, y=value, color=as.factor(variable))) +
  geom_point() +
  final_theme +
  labs(title='Jan 2010, NAs recoded',
       x=NULL,
       y='temp deg C') +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
                              "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust

buoy2010_L1 <- buoy2010_L1 %>% 
  mutate_at(vars(alltemp2007),
            funs(case_when(datetime >= as.POSIXct('2010-01-03 2:20', tz='UTC') & datetime < as.POSIXct('2010-01-03 12:20', tz='UTC') ~ NA_real_,
                           datetime == as.POSIXct('2010-01-08 8:10', tz='UTC') ~ NA_real_,
                           datetime >= as.POSIXct('2010-01-08 20:30', tz='UTC') & datetime < as.POSIXct('2010-01-09 8:10', tz='UTC') ~ NA_real_,
                           datetime == as.POSIXct('2010-01-11 11:00', tz='UTC') ~ NA_real_,
                           datetime >= as.POSIXct('2010-01-13 14:10', tz='UTC') & datetime < as.POSIXct('2010-01-14 10:30', tz='UTC') ~ NA_real_,
                           datetime >= as.POSIXct('2010-01-16 13:50', tz='UTC') & datetime < as.POSIXct('2010-01-17 4:40', tz='UTC') ~ NA_real_,
                           datetime >= as.POSIXct('2010-01-22 23:00', tz='UTC') & datetime < as.POSIXct('2010-01-23 0:50', tz='UTC') ~ NA_real_,
                           datetime == as.POSIXct('2010-01-23 10:40', tz='UTC') ~ NA_real_,
                           datetime >= as.POSIXct('2010-01-30 16:50', tz='UTC') & datetime < as.POSIXct('2010-01-30 19:00', tz='UTC') ~ NA_real_,
                           datetime >= as.POSIXct('2010-01-30 16:50', tz='UTC') & datetime < as.POSIXct('2010-01-30 19:00', tz='UTC') ~ NA_real_,
                           datetime == as.POSIXct('2010-01-31 11:50', tz='UTC') ~ NA_real_,
                           TRUE ~ .))) 

buoy2010_temp_vert_b <- buoy2010_L1 %>%
  select(datetime, location, alltemp2007) %>%
  gather(variable, value, -location, -datetime) %>%
  mutate(variable = factor(variable, levels=alltemp2007))

# ggplot(subset(buoy2010_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2010-01-01', tz='UTC') &
#                         datetime < as.POSIXct('2010-02-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Jan 2010, semi clean',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# ggplot(subset(buoy2010_temp_vert,
#               subset=(datetime >=as.POSIXct('2010-02-01', tz='UTC') &
#                         datetime < as.POSIXct('2010-03-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Feb 2010, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# ggplot(subset(buoy2010_temp_vert,
#               subset=(datetime >=as.POSIXct('2010-02-01', tz='UTC') &
#                         datetime < as.POSIXct('2010-03-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Feb 2010, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   coord_cartesian(ylim=c(-5, 10)) +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_y_continuous(minor_breaks = c(-5:10)) +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust

buoy2010_L1 <- buoy2010_L1 %>%
  mutate_at(vars(alltemp2007),
            funs(case_when(datetime >= as.POSIXct('2010-02-01', tz='UTC') & datetime < as.POSIXct('2010-03-01', tz='UTC') & (. > 2 | . < -1) ~ NA_real_,
                           TRUE ~ .)))

buoy2010_temp_vert_b <- buoy2010_L1 %>%
  select(datetime, location, alltemp2007) %>%
  gather(variable, value, -location, -datetime) %>%
  mutate(variable = factor(variable, levels=alltemp2007))

# ggplot(subset(buoy2010_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2010-02-01', tz='UTC') &
#                         datetime < as.POSIXct('2010-03-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Feb 2010, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# #Feb 05, 06, 07, 08, 08-09, 09, 13, 19, 22, 24
# ggplot(subset(buoy2010_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2010-02-05', tz='UTC') &
#                         datetime < as.POSIXct('2010-02-06', tz='UTC'))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Feb 2010, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# ggplot(subset(buoy2010_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2010-02-06', tz='UTC') &
#                         datetime < as.POSIXct('2010-02-07', tz='UTC'))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Feb 2010, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# ggplot(subset(buoy2010_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2010-02-07', tz='UTC') &
#                         datetime < as.POSIXct('2010-02-08', tz='UTC'))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Feb 2010, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# ggplot(subset(buoy2010_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2010-02-08', tz='UTC') &
#                         datetime < as.POSIXct('2010-02-09', tz='UTC'))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Feb 2010, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# ggplot(subset(buoy2010_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2010-02-08 12:00', tz='UTC') &
#                         datetime < as.POSIXct('2010-02-09 12:00', tz='UTC'))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Feb 2010, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# ggplot(subset(buoy2010_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2010-02-09', tz='UTC') &
#                         datetime < as.POSIXct('2010-02-10', tz='UTC'))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Feb 2010, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# ggplot(subset(buoy2010_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2010-02-12', tz='UTC') &
#                         datetime < as.POSIXct('2010-02-13', tz='UTC'))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Feb 2010, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# ggplot(subset(buoy2010_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2010-02-13', tz='UTC') &
#                         datetime < as.POSIXct('2010-02-14', tz='UTC'))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Feb 2010, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# ggplot(subset(buoy2010_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2010-02-19', tz='UTC') &
#                         datetime < as.POSIXct('2010-02-20', tz='UTC'))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Feb 2010, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# ggplot(subset(buoy2010_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2010-02-22', tz='UTC') &
#                         datetime < as.POSIXct('2010-02-23', tz='UTC'))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Feb 2010, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# ggplot(subset(buoy2010_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2010-02-24', tz='UTC') &
#                         datetime < as.POSIXct('2010-02-25', tz='UTC'))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Feb 2010, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
buoy2010_L1 <- buoy2010_L1 %>% 
  mutate_at(vars(alltemp2007),
            funs(case_when(datetime >= as.POSIXct('2010-02-05 01:30', tz='UTC') & datetime < as.POSIXct('2010-02-05 2:40', tz='UTC') ~ NA_real_,
                           datetime >= as.POSIXct('2010-02-06 01:00', tz='UTC') & datetime < as.POSIXct('2010-02-06 2:30', tz='UTC') ~ NA_real_,
                           datetime == as.POSIXct('2010-02-06 13:00', tz='UTC') ~ NA_real_,
                           datetime >= as.POSIXct('2010-02-06 17:50', tz='UTC') & datetime < as.POSIXct('2010-02-06 19:50', tz='UTC') ~ NA_real_,
                           datetime >= as.POSIXct('2010-02-07 12:30', tz='UTC') & datetime < as.POSIXct('2010-02-07 12:50', tz='UTC') ~ NA_real_,
                           datetime >= as.POSIXct('2010-02-08 03:10', tz='UTC') & datetime < as.POSIXct('2010-02-08 11:50', tz='UTC') ~ NA_real_,
                           datetime >= as.POSIXct('2010-02-08 21:40', tz='UTC') & datetime < as.POSIXct('2010-02-09 8:30', tz='UTC') ~ NA_real_,
                           datetime >= as.POSIXct('2010-02-09 12:30', tz='UTC') & datetime < as.POSIXct('2010-02-09 14:00', tz='UTC') ~ NA_real_,
                           datetime >= as.POSIXct('2010-02-12 6:30', tz='UTC') & datetime < as.POSIXct('2010-02-12 8:30', tz='UTC') ~ NA_real_,
                           datetime >= as.POSIXct('2010-02-13 3:10', tz='UTC') & datetime < as.POSIXct('2010-02-13 9:50', tz='UTC') ~ NA_real_,
                           datetime == as.POSIXct('2010-02-19 8:20', tz='UTC') ~ NA_real_,
                           datetime == as.POSIXct('2010-02-19 20:00', tz='UTC') ~ NA_real_,
                           datetime == as.POSIXct('2010-02-22 11:30', tz='UTC') ~ NA_real_,
                           datetime == as.POSIXct('2010-02-24 10:10', tz='UTC') ~ NA_real_,
                           TRUE ~ .))) 

buoy2010_temp_vert_b <- buoy2010_L1 %>%
  select(datetime, location, alltemp2007) %>%
  gather(variable, value, -location, -datetime) %>%
  mutate(variable = factor(variable, levels=alltemp2007))

# ggplot(subset(buoy2010_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2010-02-01', tz='UTC') &
#                         datetime < as.POSIXct('2010-03-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Feb 2010, semi clean',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# ggplot(subset(buoy2010_temp_vert,
#               subset=(datetime >=as.POSIXct('2010-03-01', tz='UTC') &
#                         datetime < as.POSIXct('2010-04-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Mar 2010, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# ggplot(subset(buoy2010_temp_vert,
#               subset=(datetime >=as.POSIXct('2010-03-01', tz='UTC') &
#                         datetime < as.POSIXct('2010-04-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Mar 2010, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   coord_cartesian(ylim=c(-5, 10)) +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_y_continuous(minor_breaks = c(-5:10)) +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust

buoy2010_L1 <- buoy2010_L1 %>% 
  mutate_at(vars(alltemp2007),
            funs(case_when(datetime >= as.POSIXct('2010-03-01', tz='UTC') & datetime < as.POSIXct('2010-04-01', tz='UTC') & (. > 5 | . < 0) ~ NA_real_,
                           TRUE ~ .))) 

buoy2010_temp_vert_b <- buoy2010_L1 %>%
  select(datetime, location, alltemp2007) %>%
  gather(variable, value, -location, -datetime) %>%
  mutate(variable = factor(variable, levels=alltemp2007))

# ggplot(subset(buoy2010_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2010-03-01', tz='UTC') &
#                         datetime < as.POSIXct('2010-04-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Mar 2010, NAs recoded, v2',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust

# #Mar 1, 9,  15, 19, 24, 27, 29
# ggplot(subset(buoy2010_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2010-03-01', tz='UTC') &
#                         datetime < as.POSIXct('2010-03-02', tz='UTC'))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Mar 2010, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# ggplot(subset(buoy2010_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2010-03-09', tz='UTC') &
#                         datetime < as.POSIXct('2010-03-10', tz='UTC'))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Mar 2010, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# ggplot(subset(buoy2010_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2010-03-15', tz='UTC') &
#                         datetime < as.POSIXct('2010-03-16', tz='UTC'))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Mar 2010, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# ggplot(subset(buoy2010_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2010-03-19', tz='UTC') &
#                         datetime < as.POSIXct('2010-03-20', tz='UTC'))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Mar 2010, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# ggplot(subset(buoy2010_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2010-03-24', tz='UTC') &
#                         datetime < as.POSIXct('2010-03-25', tz='UTC'))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Mar 2010, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# ggplot(subset(buoy2010_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2010-03-27', tz='UTC') &
#                         datetime < as.POSIXct('2010-03-28', tz='UTC'))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Mar 2010, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# ggplot(subset(buoy2010_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2010-03-29', tz='UTC') &
#                         datetime < as.POSIXct('2010-03-30', tz='UTC'))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Mar 2010, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
buoy2010_L1 <- buoy2010_L1 %>% 
  mutate_at(vars(alltemp2007),
            funs(case_when(datetime == as.POSIXct('2010-03-01 4:50', tz='UTC') ~ NA_real_,
                           datetime >= as.POSIXct('2010-03-09 4:00', tz='UTC') & datetime < as.POSIXct('2010-03-09 19:50', tz='UTC') ~ NA_real_,
                           datetime == as.POSIXct('2010-03-15 3:10', tz='UTC') ~ NA_real_,
                           datetime >= as.POSIXct('2010-03-19 14:10', tz='UTC') & datetime < as.POSIXct('2010-03-19 15:40', tz='UTC') ~ NA_real_,
                           datetime == as.POSIXct('2010-03-24 8:30', tz='UTC') ~ NA_real_,
                           datetime == as.POSIXct('2010-03-27 20:10', tz='UTC') ~ NA_real_,
                           datetime == as.POSIXct('2010-03-29 19:00', tz='UTC') ~ NA_real_,
                           datetime == as.POSIXct('2010-03-29 19:50', tz='UTC') ~ NA_real_,
                           TRUE ~ .))) 

buoy2010_temp_vert_b <- buoy2010_L1 %>%
  select(datetime, location, alltemp2007) %>%
  gather(variable, value, -location, -datetime) %>%
  mutate(variable = factor(variable, levels=alltemp2007))

# ggplot(subset(buoy2010_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2010-03-01', tz='UTC') &
#                         datetime < as.POSIXct('2010-04-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Mar 2010, semi clean',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# ggplot(subset(buoy2010_temp_vert,
#               subset=(datetime >=as.POSIXct('2010-04-01', tz='UTC') &
#                         datetime < as.POSIXct('2010-05-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Apr 2010, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# ggplot(subset(buoy2010_temp_vert,
#               subset=(datetime >=as.POSIXct('2010-04-01', tz='UTC') &
#                         datetime < as.POSIXct('2010-05-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Apr 2010, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   coord_cartesian(ylim=c(-5, 10)) +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_y_continuous(minor_breaks = c(-5:10)) +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# #buoy not functioning beginning apr 3 through 20 May
# ggplot(subset(buoy2010_temp_vert,
#               subset=(datetime >=as.POSIXct('2010-04-03', tz='UTC') &
#                         datetime < as.POSIXct('2010-04-04', tz='UTC'))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Apr 2010, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   coord_cartesian(ylim=c(-5, 10)) +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# ggplot(subset(buoy2010_temp_vert,
#               subset=(datetime >=as.POSIXct('2010-05-20', tz='UTC') &
#                         datetime < as.POSIXct('2010-05-21', tz='UTC'))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='May 2010, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust

buoy2010_L1 <- buoy2010_L1 %>% 
  mutate_at(vars(alltemp2007),
            funs(case_when(datetime >= as.POSIXct('2010-04-01', tz='UTC') & datetime < as.POSIXct('2010-05-01', tz='UTC') & (. > 5 | . < 2) ~ NA_real_,
                           datetime >= as.POSIXct('2010-04-03 12:20', tz='UTC') & datetime < as.POSIXct('2010-05-20 11:20', tz='UTC') ~ NA_real_,
                           TRUE ~ .))) 

buoy2010_temp_vert_b <- buoy2010_L1 %>%
  select(datetime, location, alltemp2007) %>%
  gather(variable, value, -location, -datetime) %>%
  mutate(variable = factor(variable, levels=alltemp2007))

#adding flags to data until thermisters replaced in Apr 2010
buoy2010_L1 <- buoy2010_L1 %>% 
  mutate(temp_flag = case_when(datetime<as.POSIXct('2010-04-03', tz='UTC') ~ 'q',
                               TRUE ~ ''))

# ggplot(subset(buoy2010_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2010-04-01', tz='UTC') &
#                         datetime < as.POSIXct('2010-05-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Apr 2010, NAs recoded v2',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust

# ggplot(subset(buoy2010_temp_vert,
#               subset=(datetime >=as.POSIXct('2010-04-02', tz='UTC') &
#                         datetime < as.POSIXct('2010-04-03', tz='UTC'))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Apr 2010, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust

buoy2010_L1 <- buoy2010_L1 %>% 
  mutate_at(vars(alltemp2007),
            funs(case_when(datetime == as.POSIXct('2010-04-02 11:20', tz='UTC') ~ NA_real_,
                           TRUE ~ .))) 

buoy2010_temp_vert_b <- buoy2010_L1 %>%
  select(datetime, location, alltemp2007) %>%
  gather(variable, value, -location, -datetime) %>%
  mutate(variable = factor(variable, levels=alltemp2007))

# ggplot(subset(buoy2010_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2010-04-01', tz='UTC') &
#                         datetime < as.POSIXct('2010-05-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Apr 2010, semi clean',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust


# ggplot(subset(buoy2010_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2010-05-01', tz='UTC') &
#                         datetime < as.POSIXct('2010-06-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='May 2010, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust

#addflag to 0p5 after thermistors come back online
buoy2010_L1 <- buoy2010_L1 %>% 
  mutate(temp_flag = case_when(datetime>=as.POSIXct('2010-05-20 11:20', tz='UTC') ~ 'q0.5m',
                               TRUE ~ temp_flag))

# ggplot(subset(buoy2010_temp_vert,
#               subset=(datetime >=as.POSIXct('2010-06-01', tz='UTC') &
#                         datetime < as.POSIXct('2010-07-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Jun 2010, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# ggplot(subset(buoy2010_temp_vert,
#               subset=(datetime >=as.POSIXct('2010-07-01', tz='UTC') &
#                         datetime < as.POSIXct('2010-08-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Jul 2010, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# #Jul 9
# ggplot(subset(buoy2010_temp_vert,
#               subset=(datetime >=as.POSIXct('2010-07-09', tz='UTC') &
#                         datetime < as.POSIXct('2010-07-10', tz='UTC'))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Jul 2010, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust

buoy2010_L1 <- buoy2010_L1 %>% 
  mutate_at(vars(alltemp2007),
            funs(case_when(datetime == as.POSIXct('2010-07-09 8:30', tz='UTC') ~ NA_real_,
                           TRUE ~ .))) 

buoy2010_temp_vert_b <- buoy2010_L1 %>%
  select(datetime, location, alltemp2007) %>%
  gather(variable, value, -location, -datetime) %>%
  mutate(variable = factor(variable, levels=alltemp2007))

# ggplot(subset(buoy2010_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2010-07-01', tz='UTC') &
#                         datetime < as.POSIXct('2010-08-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Jul 2010, clean',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# ggplot(subset(buoy2010_temp_vert,
#               subset=(datetime >=as.POSIXct('2010-08-01', tz='UTC') &
#                         datetime < as.POSIXct('2010-09-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Aug 2010, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# #buoy visit/ anamolous points
# ggplot(subset(buoy2010_temp_vert,
#               subset=(datetime >=as.POSIXct('2010-08-21', tz='UTC') &
#                         datetime < as.POSIXct('2010-08-22', tz='UTC'))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Aug 2010, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust

buoy2010_L1 <- buoy2010_L1 %>% 
  mutate_at(vars(TempC_10m:TempC_13m),
            funs(case_when(datetime == as.POSIXct('2010-08-21 2:10', tz='UTC') ~ NA_real_,
               TRUE ~ .))) %>% 
  mutate_at(vars(alltemp2007),
            funs(case_when(datetime == as.POSIXct('2010-08-21 13:40', tz='UTC') ~ NA_real_,
                           datetime == as.POSIXct('2010-08-21 19:50', tz='UTC') ~ NA_real_,
                           TRUE ~ .))) 

buoy2010_temp_vert_b <- buoy2010_L1 %>%
  select(datetime, location, alltemp2007) %>%
  gather(variable, value, -location, -datetime) %>%
  mutate(variable = factor(variable, levels=alltemp2007))

# ggplot(subset(buoy2010_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2010-08-01', tz='UTC') &
#                         datetime < as.POSIXct('2010-09-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Aug 2010, clean',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# ggplot(subset(buoy2010_temp_vert,
#               subset=(datetime >=as.POSIXct('2010-09-01', tz='UTC') &
#                         datetime < as.POSIXct('2010-10-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Sept 2010, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# ggplot(subset(buoy2010_temp_vert,
#               subset=(datetime >=as.POSIXct('2010-10-01', tz='UTC') &
#                         datetime < as.POSIXct('2010-11-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Oct 2010, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# #anamolous points on Oct2 and buoy to harbor Oct 25
# ggplot(subset(buoy2010_temp_vert,
#               subset=(datetime >=as.POSIXct('2010-10-02', tz='UTC') &
#                         datetime < as.POSIXct('2010-10-03', tz='UTC'))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Oct 2010, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# ggplot(subset(buoy2010_temp_vert,
#               subset=(datetime >=as.POSIXct('2010-10-25', tz='UTC') &
#                         datetime < as.POSIXct('2010-10-26', tz='UTC'))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Oct 2010, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust

buoy2010_L1 <- buoy2010_L1 %>% 
  mutate_at(vars(alltemp2007),
            funs(case_when(datetime == as.POSIXct('2010-10-02 16:10', tz='UTC') ~ NA_real_,
                           datetime >= as.POSIXct('2010-10-25 12:40', tz='UTC') & datetime < as.POSIXct('2011-01-01') ~ NA_real_, #buoy back in water in harbor for a bit, but temp would not be applicable
                           TRUE ~ .)))

buoy2010_temp_vert_b <- buoy2010_L1 %>%
  select(datetime, location, alltemp2007) %>%
  gather(variable, value, -location, -datetime) %>%
  mutate(variable = factor(variable, levels=alltemp2007))

# ggplot(subset(buoy2010_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2010-10-01', tz='UTC') &
#                         datetime < as.POSIXct('2010-11-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Oct 2010, clean',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# #data gap thru Nov
# 
# ggplot(subset(buoy2010_temp_vert,
#               subset=(datetime >=as.POSIXct('2010-12-01', tz='UTC') &
#                         datetime < as.POSIXct('2011-01-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Dec 2010, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# ggplot(subset(buoy2010_temp_vert,
#               subset=(datetime >=as.POSIXct('2010-12-14', tz='UTC') &
#                         datetime < as.POSIXct('2010-12-15', tz='UTC'))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Dec 2010, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# ggplot(subset(buoy2010_temp_vert,
#               subset=(datetime >=as.POSIXct('2010-12-15', tz='UTC') &
#                         datetime < as.POSIXct('2010-12-16', tz='UTC'))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Dec 2010, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# ggplot(subset(buoy2010_temp_vert,
#               subset=(datetime >=as.POSIXct('2010-12-22', tz='UTC') &
#                         datetime < as.POSIXct('2010-12-23', tz='UTC'))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Dec 2010, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust

buoy2010_L1 <- buoy2010_L1 %>% 
  mutate_at(vars(alltemp2007),
            funs(case_when(datetime >= as.POSIXct('2010-12-01', tz='UTC') ~ NA_real_,
                           TRUE ~ .))) 

buoy2010_temp_vert_b <- buoy2010_L1 %>%
  select(datetime, location, alltemp2007) %>%
  gather(variable, value, -location, -datetime) %>%
  mutate(variable = factor(variable, levels=alltemp2007))

# ggplot(subset(buoy2010_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2010-12-01', tz='UTC') &
#                         datetime < as.POSIXct('2011-01-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Dec 2010, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# ggplot(subset(buoy2010_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2010-01-01', tz='UTC') &
#                         datetime < as.POSIXct('2011-01-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=as.factor(variable), shape=location)) +
#   geom_point() +
#   final_theme +
#   labs(title='2010, temp clean',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 month') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# #reality check with LMP
# buoy_LMP_2010 <- buoy2010_temp_vert_b %>%
#   mutate(source='buoy') %>%
#   full_join(., LMP2010_temp) %>%
#   select(-location) %>% 
#   mutate(variable = factor(variable, levels=alltemp2007LMP))
# unique(LMP2010_temp$datetime)
# 
# #June 2, July 12, Aug 17, Sept 13
# ggplot(subset(buoy_LMP_2010,
#               subset=(datetime>=as.POSIXct('2010-06-02', tz='UTC') &
#                         datetime<as.POSIXct('2010-06-03', tz='UTC'))),
#        aes(x=datetime, y=value, color=as.factor(variable), shape = source)) +
#   geom_point() +
#   final_theme +
# labs(title='temp reality check\n2010 buoy and LMP site 210 data\nJune 2, 2010',
#      x=NULL,
#      y='temp (deg C)') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# ggplot(subset(buoy_LMP_2010,
#               subset=(datetime>=as.POSIXct('2010-07-12', tz='UTC') &
#                         datetime<as.POSIXct('2010-07-13', tz='UTC'))),
#        aes(x=datetime, y=value, color=as.factor(variable), shape = source)) +
#   geom_point() +
#   final_theme +
#   labs(title='temp reality check\n2010 buoy and LMP site 210 data\nJuly 12, 2010',
#        x=NULL,
#        y='temp (deg C)') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# ggplot(subset(buoy_LMP_2010,
#               subset=(datetime>=as.POSIXct('2010-08-17', tz='UTC') &
#                         datetime<as.POSIXct('2010-08-18', tz='UTC'))),
#        aes(x=datetime, y=value, color=as.factor(variable), shape = source)) +
#   geom_point() +
#   final_theme +
#   labs(title='temp reality check\n2010 buoy and LMP site 210 data\nAugust 17, 2010',
#        x=NULL,
#        y='temp (deg C)') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# ggplot(subset(buoy_LMP_2010,
#               subset=(datetime>=as.POSIXct('2010-09-13', tz='UTC') &
#                         datetime<as.POSIXct('2010-09-14', tz='UTC'))),
#        aes(x=datetime, y=value, color=as.factor(variable), shape = source)) +
#   geom_point() +
#   final_theme +
#   labs(title='temp reality check\n2010 buoy and LMP site 210 data\nSept 13, 2010',
#        x=NULL,
#        y='temp (deg C)') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust

#export L1 tempstring file
buoy2010_L1 %>%
  select(datetime, TempC_0m, TempC_0p5m, TempC_1m, TempC_1p5m, TempC_2m, TempC_2p5m, TempC_3m, TempC_4m, TempC_5m, TempC_6m, TempC_7m, TempC_8m, TempC_9m, TempC_10m, TempC_11m, TempC_13m, temp_flag) %>%
  mutate(datetime = as.character(datetime)) %>%
  write_csv(., 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2010_tempstring_L1.csv')

#clean up workspace
rm(buoy_LMP_2010, buoy2010_temp_vert, buoy2010_temp_vert_b, LMP2010_temp)
