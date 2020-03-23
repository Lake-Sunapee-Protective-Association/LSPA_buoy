#*****************************************************************
#*      Cary Institute of Ecosystem Studies (Millbrook, NY)      *
#*                                                               *
#* TITLE:   Sunapee_buoy_2009.r                                  *
#* AUTHOR:  Bethel Steele                                        *
#* SYSTEM:  Lenovo ThinkCentre, Win 10, R 3.5.2, RStudio 1.1.383 *
#* PROJECT: Lake Sunapee Buoy Data Cleaning                      *
#* PURPOSE: subset data for met/compare with L1                  *
#* LAST MODIFIED: 05Sept2019 to create vertical dataset for      *
#*          master collation                                     *
#*****************************************************************

#bring in 2009 buoy raw data
buoy2009_L0 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L0/Sunapee2009_rawData.csv',
                        col_names = c('datetime', 'AirTempC', 'DOppm', 'DOSat', 'DOSat2', 
                                      'PAR', 'DOTempC', 'TempC_0m', 'TempC_0p5m', 'TempC_1m', 
                                      'TempC_1p5m', 'TempC_2m', 'TempC_2p5m', 'TempC_3m', 'TempC_4m', 
                                      'TempC_5m', 'TempC_6m', 'TempC_7m', 'TempC_8m', 'TempC_9m', 
                                      'TempC_10m', 'TempC_11m', 'TempC_13m', 'AveWindDir', 'InstWindDir', 
                                      'InstWindSp', 'AveWindSp'), 
                        col_types = 'cnnnnnnnnnnnnnnnnnnnnnnnnnn',
                        skip=1) %>% 
  select(-DOSat2) %>%  #drop blank columns
  mutate(datetime = as.POSIXct(datetime, format='%Y-%m-%d %H:%M:%S', tz='UTC'))

# #bring in 2009 LMP data for comparison
# LMP2009 <- read_xlsx('C:/Users/steeleb/Dropbox/Lake Sunapee/long term Sunapee data/raw data/LMP files/LMP 2017/DO.xlsx', 
#                      sheet='DO',
#                      col_types = c('text', 'text', 'numeric', 'guess', 'numeric', 
#                                    'numeric', 'numeric', 'numeric', 'numeric' ,'numeric',
#                                    'text', 'text', 'numeric', 'text')) %>% 
#   mutate(DATE = as.Date(DATE, format='%Y-%m-%d')) %>%  #format date
#   filter(DATE >= '2009-01-01' & DATE < '2010-01-01', #filter for 2009 only
#          STATION == 210)
# 
# LMP2009_temp <- LMP2009 %>% 
#   select(DATE, DEPTH, TEMP) %>% 
#   filter(DEPTH<=14) %>% 
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
#                               variable == '12' ~ 'TempC_12m',
#                               variable == '13' ~ 'TempC_13m',
#                               variable == '14' ~ 'TempC_14m'))

#create dummy timestamp so there are no blanks
alltimes_2009 <- as.data.frame(seq.POSIXt(as.POSIXct('2009-01-01 00:00', tz='UTC'), as.POSIXct('2009-12-31 23:50', tz='UTC'), '10 min')) %>% 
  rename("datetime" = !!names(.[1]))

buoy2009_L1 <- buoy2009_L0 %>% 
  right_join(., alltimes_2009) %>% 
  arrange(datetime)

#### thermisters ####
buoy2009_temp_vert <- buoy2009_L1 %>%
  select(datetime, alltemp2007) %>%
  gather(variable, value, -datetime) %>% 
  mutate(variable = factor(variable, levels=alltemp2007))

# ggplot(subset(buoy2009_temp_vert,
#               subset=(datetime >=as.POSIXct('2009-01-01', tz='UTC') &
#                         datetime < as.POSIXct('2010-01-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='2009 buoy temp raw',
#        x=NULL,
#        y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", 
#                               "#005180", "#0081cc", "#66c7ff")) #so you can adjust

buoy2009_L1 <- buoy2009_L1 %>% 
  mutate_at(vars(alltemp2007),
            funs(case_when(. == -6999 ~ NA_real_,
                           . == -99.9 ~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate(location = 'loon')

buoy2009_temp_vert <- buoy2009_L1 %>%
  select(datetime, location, alltemp2007) %>%
  gather(variable, value, -location, -datetime) %>%
  mutate(variable = factor(variable, levels=alltemp2007))

ggplot(subset(buoy2009_temp_vert,
              subset=(datetime >=as.POSIXct('2009-01-01', tz='UTC') &
                        datetime < as.POSIXct('2010-01-01', tz='UTC'))),
       aes(x=datetime, y=value, color=(variable))) +
  geom_point() +
  final_theme +
  labs(title='2009 buoy temp NAs recoded',
       x=NULL,
       y='temp (deg C)') +
  scale_x_datetime(date_minor_breaks = '1 month') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
                              "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust

# ggplot(subset(buoy2009_temp_vert,
#               subset=(datetime >=as.POSIXct('2009-01-01', tz='UTC') &
#                         datetime < as.POSIXct('2009-02-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Jan 2009, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust

buoy2009_L1 <- buoy2009_L1 %>% 
  mutate(temp_flag = case_when(datetime >= as.POSIXct('2009-01-01', tz='UTC') & datetime < as.POSIXct('2009-02-28', tz='UTC') ~ '9.5d, 10.5d, 11.5d, 13.5d',
                           TRUE ~ ''))
buoy2009_temp_vert_b <- buoy2009_L1 %>%
  select(datetime, location, alltemp2007) %>%
  gather(variable, value, -location, -datetime) %>%
  mutate(variable = factor(variable, levels=alltemp2007))

# ggplot(subset(buoy2009_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2009-01-01', tz='UTC') &
#                         datetime < as.POSIXct('2009-02-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Jan 2009, clean',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# ggplot(subset(buoy2009_temp_vert,
#               subset=(datetime >=as.POSIXct('2009-02-01', tz='UTC') &
#                         datetime < as.POSIXct('2009-03-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Feb 2009, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# #data gap until jul
# ggplot(subset(buoy2009_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2009-07-28', tz='UTC') &
#                         datetime < as.POSIXct('2009-09-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Jul/Aug 2009, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# #anomalous points Aug 13 and 24
# ggplot(subset(buoy2009_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2009-08-11', tz='UTC') &
#                         datetime < as.POSIXct('2009-08-12', tz='UTC'))),
#        aes(x=datetime, y=value, color=(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Jul/Aug 2009, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# ggplot(subset(buoy2009_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2009-08-13', tz='UTC') &
#                         datetime < as.POSIXct('2009-08-14', tz='UTC'))),
#        aes(x=datetime, y=value, color=(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Jul/Aug 2009, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust

# ggplot(subset(buoy2009_temp_vert,
#               subset=(datetime >=as.POSIXct('2009-08-24', tz='UTC') &
#                         datetime < as.POSIXct('2009-08-25', tz='UTC'))),
#        aes(x=datetime, y=value, color=(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Jul/Aug 2009, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# ggplot(subset(buoy2009_temp_vert,
#               subset=(datetime >=as.POSIXct('2009-08-31', tz='UTC') &
#                         datetime < as.POSIXct('2009-09-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Jul/Aug 2009, clean',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
 
buoy2009_L1 <- buoy2009_L1 %>% 
  mutate_at(vars(alltemp2007),
            funs(case_when(datetime >= as.POSIXct('2009-02-01', tz='UTC') & datetime < as.POSIXct('2009-07-28', tz='UTC') ~ NA_real_,
                           datetime == as.POSIXct('2009-08-11 23:40', tz='UTC') ~ NA_real_,
                           datetime == as.POSIXct('2009-08-13 23:20', tz='UTC') ~ NA_real_,
                           datetime == as.POSIXct('2009-08-24 8:30', tz='UTC') ~ NA_real_,
                           datetime == as.POSIXct('2009-08-31 11:40', tz='UTC') ~ NA_real_,
                           TRUE ~ .))) 

buoy2009_temp_vert_b <- buoy2009_L1 %>%
  select(datetime, location, alltemp2007) %>%
  gather(variable, value, -location, -datetime) %>%
  mutate(variable = factor(variable, levels=alltemp2007))

# ggplot(subset(buoy2009_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2009-07-28', tz='UTC') &
#                         datetime < as.POSIXct('2009-09-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Jul/Aug 2009, clean',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# ggplot(subset(buoy2009_temp_vert,
#               subset=(datetime >=as.POSIXct('2009-10-01', tz='UTC') &
#                         datetime < as.POSIXct('2009-11-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Oct 2009, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# ggplot(subset(buoy2009_temp_vert,
#               subset=(datetime >=as.POSIXct('2009-11-01', tz='UTC') &
#                         datetime < as.POSIXct('2009-12-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Nov 2009, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# #Nov 16
# ggplot(subset(buoy2009_temp_vert,
#               subset=(datetime >=as.POSIXct('2009-11-16', tz='UTC') &
#                         datetime < as.POSIXct('2009-11-17', tz='UTC'))),
#        aes(x=datetime, y=value, color=(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Nov 2009, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust

buoy2009_L1 <- buoy2009_L1 %>% 
  mutate_at(vars(alltemp2007),
            funs(case_when(datetime == as.POSIXct('2009-11-16 6:50', tz='UTC') ~ NA_real_,
                           TRUE ~ .))) 

buoy2009_temp_vert_b <- buoy2009_L1 %>%
  select(datetime, location, alltemp2007) %>%
  gather(variable, value, -location, -datetime) %>%
  mutate(variable = factor(variable, levels=alltemp2007))

# ggplot(subset(buoy2009_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2009-11-01', tz='UTC') &
#                         datetime < as.POSIXct('2009-12-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Nov 2009, clean',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust

# ggplot(subset(buoy2009_temp_vert,
#               subset=(datetime >=as.POSIXct('2009-12-01', tz='UTC') &
#                         datetime < as.POSIXct('2010-01-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Dec 2009, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# #Dec 8, 12, 17, 23/24
# ggplot(subset(buoy2009_temp_vert,
#               subset=(datetime >=as.POSIXct('2009-12-08', tz='UTC') &
#                         datetime < as.POSIXct('2009-12-09', tz='UTC'))),
#        aes(x=datetime, y=value, color=(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Dec 2009, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# ggplot(subset(buoy2009_temp_vert,
#               subset=(datetime >=as.POSIXct('2009-12-12', tz='UTC') &
#                         datetime < as.POSIXct('2009-12-13', tz='UTC'))),
#        aes(x=datetime, y=value, color=(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Dec 2009, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# ggplot(subset(buoy2009_temp_vert,
#               subset=(datetime >=as.POSIXct('2009-12-17', tz='UTC') &
#                         datetime < as.POSIXct('2009-12-18', tz='UTC'))),
#        aes(x=datetime, y=value, color=(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Dec 2009, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# ggplot(subset(buoy2009_temp_vert,
#               subset=(datetime >=as.POSIXct('2009-12-23 12:00', tz='UTC') &
#                         datetime < as.POSIXct('2009-12-24 12:00', tz='UTC'))),
#        aes(x=datetime, y=value, color=(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Dec 2009, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust

buoy2009_L1 <- buoy2009_L1 %>% 
  mutate(TempC_6m = case_when(datetime == as.POSIXct('2009-12-08 23:20', tz='UTC') ~ NA_real_,
                              TRUE ~ TempC_6m)) %>% 
  mutate(TempC_4m = case_when(datetime == as.POSIXct('2009-12-23 23:30', tz='UTC') ~ NA_real_,
                              TRUE ~ TempC_4m)) %>% 
  mutate_at(vars(alltemp2007),
            funs(case_when(datetime >= as.POSIXct('2009-12-12 4:20', tz='UTC') & datetime < as.POSIXct('2009-12-12 9:00', tz='UTC') & .<2.5 ~ NA_real_,
                           datetime >= as.POSIXct('2009-12-17 4:00', tz='UTC') & datetime < as.POSIXct('2009-12-17 7:10', tz='UTC') ~ NA_real_,
                           TRUE ~ .))) 

buoy2009_temp_vert_b <- buoy2009_L1 %>%
  select(datetime, location, alltemp2007) %>%
  gather(variable, value, -location, -datetime) %>%
  mutate(variable = factor(variable, levels=alltemp2007))

# ggplot(subset(buoy2009_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2009-12-01', tz='UTC') &
#                         datetime < as.POSIXct('2010-01-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Dec 2009, clean',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
#
ggplot(subset(buoy2009_temp_vert_b,
              subset=(datetime >=as.POSIXct('2009-01-01', tz='UTC') &
                        datetime < as.POSIXct('2010-01-01', tz='UTC'))),
       aes(x=datetime, y=value, color=(variable))) +
  geom_point() +
  final_theme +
  labs(title='2009, clean',
       x=NULL,
       y='temp deg C') +
  scale_x_datetime(date_minor_breaks = '1 month') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
                              "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust

buoy2009_L1 <- buoy2009_L1 %>% 
  mutate(temp_flag = case_when(temp_flag == '9.5d, 10.5d, 11.5d, 13.5d' & datetime<as.POSIXct('2009-01-15', tz='UTC') ~ paste('i', temp_flag, sep = ', '),
                               temp_flag == '' & datetime<as.POSIXct('2009-01-15', tz='UTC') ~ 'i',
                               datetime>=as.POSIXct('2009-12-17', tz='UTC') ~ 'q',
                               TRUE ~ temp_flag))
buoy2009_temp_vert_b <- buoy2009_L1 %>%
  select(datetime, location, alltemp2007, temp_flag) %>%
  gather(variable, value, -location, -datetime, -temp_flag) %>%
  mutate(variable = factor(variable, levels=alltemp2007))

ggplot(subset(buoy2009_temp_vert_b,
              subset=(datetime >=as.POSIXct('2009-01-01', tz='UTC') &
                        datetime < as.POSIXct('2010-01-01', tz='UTC'))),
       aes(x=datetime, y=value, color=(variable), shape = temp_flag)) +
  geom_point() +
  final_theme +
  labs(title='2009, clean',
       x=NULL,
       y='temp deg C') +
  scale_x_datetime(date_minor_breaks = '1 month') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
                              "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust

# #reality check with LMP
# buoy_LMP_2009 <- buoy2009_temp_vert_b %>%
#   mutate(source='buoy') %>%
#   full_join(., LMP2009_temp) %>%
#   select(-location) %>% 
#   mutate(variable = factor(variable, levels=alltemp2007LMP))
# unique(LMP2009_temp$datetime)
# 
# #May 26, June 22, July 13, Aug 12, Sept 1, Oct 05
# ggplot(subset(buoy_LMP_2009,
#               subset=(datetime>=as.POSIXct('2009-05-26', tz='UTC') &
#                         datetime<as.POSIXct('2009-05-27', tz='UTC'))),
#        aes(x=datetime, y=value, color=(variable), shape = source)) +
#   geom_point() +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# ggplot(subset(buoy_LMP_2009,
#               subset=(datetime>=as.POSIXct('2009-06-22', tz='UTC') &
#                         datetime<as.POSIXct('2009-06-23', tz='UTC'))),
#        aes(x=datetime, y=value, color=(variable), shape = source)) +
#   geom_point() +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# ggplot(subset(buoy_LMP_2009,
#               subset=(datetime>=as.POSIXct('2009-07-13', tz='UTC') &
#                         datetime<as.POSIXct('2009-07-14', tz='UTC'))),
#        aes(x=datetime, y=value, color=(variable), shape = source)) +
#   geom_point() +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# ggplot(subset(buoy_LMP_2009,
#               subset=(datetime>=as.POSIXct('2009-08-12', tz='UTC') &
#                         datetime<as.POSIXct('2009-08-13', tz='UTC'))),
#        aes(x=datetime, y=value, color=(variable), shape = source)) +
#   geom_point() +
#   final_theme +
#   labs(title='temp reality check\n2009 buoy and LMP site 210 data\nAug 12, 2009',
#        x=NULL,
#        y='temp (deg C)') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# ggplot(subset(buoy_LMP_2009,
#               subset=(datetime>=as.POSIXct('2009-09-01', tz='UTC') &
#                         datetime<as.POSIXct('2009-09-02', tz='UTC'))),
#        aes(x=datetime, y=value, color=(variable), shape = source)) +
#   geom_point() +
#   final_theme +
#   labs(title='temp reality check\n2009 buoy and LMP site 210 data\nSept 1, 2009',
#        x=NULL,
#        y='temp (deg C)') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# ggplot(subset(buoy_LMP_2009,
#               subset=(datetime>=as.POSIXct('2009-10-05', tz='UTC') &
#                         datetime<as.POSIXct('2009-10-06', tz='UTC'))),
#        aes(x=datetime, y=value, color=(variable), shape = source)) +
#   geom_point() +
#   final_theme +
#   labs(title='temp reality check\n2009 buoy and LMP site 210 data\nOct 5, 2009',
#        x=NULL,
#        y='temp (deg C)') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# 
#clean up workspace
rm(buoy_LMP_2009, buoy2009_temp_vert, buoy2009_temp_vert_b, LMP2009_temp)


#### DO sensors ####
range(buoy2009_L1$DOSat, na.rm=T)
range(buoy2009_L1$DOppm, na.rm=T)
range(buoy2009_L1$DOTempC, na.rm=T)

do_vert <- buoy2009_L1 %>% 
  select(datetime, DOSat, DOppm, DOTempC) %>% 
  gather(variable, value, -datetime)

# ggplot(do_vert, aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = '2009 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 month')

buoy2009_L1 <- buoy2009_L1 %>% 
  mutate(DOTempC = case_when(DOTempC == -6999 ~ NA_real_,
                             TRUE ~ DOTempC))

do_vert <- buoy2009_L1 %>% 
  select(datetime, DOSat, DOppm, DOTempC) %>% 
  gather(variable, value, -datetime)

ggplot(do_vert, aes(x = datetime, y = value)) +
  geom_point() +
  facet_grid(variable~., scales = 'free_y') +
  final_theme +
  labs(title = '2009 DO data NA values recoded',
       x = NULL,
       y = NULL) +
  scale_x_datetime(date_minor_breaks = '1 month')

# ggplot(subset(do_vert, 
#               subset = (datetime >= as.POSIXct('2009-01-01', tz='UTC') & 
#                           datetime < as.POSIXct('2009-02-01', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'jan 2009 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')

#start seeing intermittent readings dec 29 - adding flag of intermittent do data from then throught the end of the month
buoy2009_L1 <- buoy2009_L1 %>% 
  mutate(upper_do_flag = case_when(datetime < as.POSIXct('2009-01-15', tz='UTC') ~ 'i',
                             TRUE ~ ''))

# ggplot(subset(do_vert, 
#               subset = (datetime >= as.POSIXct('2009-02-01', tz='UTC') & 
#                           datetime < as.POSIXct('2009-03-01', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'feb 2009 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')

#data before feb 23 recoded to NA
buoy2009_L1 <- buoy2009_L1 %>% 
  mutate_at(vars(DOSat, DOppm, DOTempC),
            funs(case_when(datetime >= as.POSIXct('2009-02-01', tz='UTC') &
                             datetime < as.POSIXct('2009-02-23', tz='UTC') ~ NA_real_,
                             TRUE ~ .)))
do_vert_b <- buoy2009_L1 %>% 
  select(datetime, DOSat, DOppm, DOTempC) %>% 
  gather(variable, value, -datetime)

# ggplot(subset(do_vert_b, 
#               subset = (datetime >= as.POSIXct('2009-02-01', tz='UTC') & 
#                           datetime < as.POSIXct('2009-03-01', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'feb 2009 DO data clean',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(do_vert, 
#               subset = (datetime >= as.POSIXct('2009-03-01', tz='UTC') & 
#                           datetime < as.POSIXct('2009-04-01', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'mar 2009 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(do_vert, 
#               subset = (datetime >= as.POSIXct('2009-04-01', tz='UTC') & 
#                           datetime < as.POSIXct('2009-05-01', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'apr 2009 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# #errant readings beginning apr 6
# ggplot(subset(do_vert, 
#               subset = (datetime >= as.POSIXct('2009-04-05', tz='UTC') & 
#                           datetime < as.POSIXct('2009-04-06', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'apr 2009 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 hour')

buoy2009_L1 <- buoy2009_L1 %>% 
  mutate_at(vars(DOSat, DOppm, DOTempC),
            funs(case_when(datetime >= as.POSIXct('2009-04-05 1:40', tz='UTC') &
                             datetime < as.POSIXct('2009-04-16', tz='UTC') ~ NA_real_,
                           TRUE ~ .)))
do_vert_b <- buoy2009_L1 %>% 
  select(datetime, DOSat, DOppm, DOTempC) %>% 
  gather(variable, value, -datetime)

# ggplot(subset(do_vert_b, 
#               subset = (datetime >= as.POSIXct('2009-04-01', tz='UTC') & 
#                           datetime < as.POSIXct('2009-05-01', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'apr 2009 DO data clean',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(do_vert, 
#               subset = (datetime >= as.POSIXct('2009-05-01', tz='UTC') & 
#                           datetime < as.POSIXct('2009-06-01', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'may 2009 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(do_vert, 
#               subset = (datetime >= as.POSIXct('2009-06-01', tz='UTC') & 
#                           datetime < as.POSIXct('2009-07-01', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'jun 2009 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(do_vert, 
#               subset = (datetime >= as.POSIXct('2009-07-01', tz='UTC') & 
#                           datetime < as.POSIXct('2009-08-01', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'jul 2009 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(do_vert, 
#               subset = (datetime >= as.POSIXct('2009-08-01', tz='UTC') & 
#                           datetime < as.POSIXct('2009-09-01', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'aug 2009 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(do_vert, 
#               subset = (datetime >= as.POSIXct('2009-09-01', tz='UTC') & 
#                           datetime < as.POSIXct('2009-10-01', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'sept 2009 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(do_vert, 
#               subset = (datetime >= as.POSIXct('2009-10-01', tz='UTC') & 
#                           datetime < as.POSIXct('2009-11-01', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'oct 2009 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(do_vert, 
#               subset = (datetime >= as.POSIXct('2009-11-01', tz='UTC') & 
#                           datetime < as.POSIXct('2009-12-01', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'nov 2009 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(do_vert, 
#               subset = (datetime >= as.POSIXct('2009-12-01', tz='UTC') & 
#                           datetime < as.POSIXct('2010-01-01', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'dec 2009 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')

# add in buoy location as offline
buoy2009_L1 <- buoy2009_L1 %>% 
  mutate(location = case_when(datetime < as.POSIXct('2009-01-01 7:50', tz='UTC') ~ 'offline',
                              datetime >= as.POSIXct('2009-01-14 18:00', tz='UTC') & 
                                datetime < as.POSIXct('2009-02-18 0:10', tz='UTC') ~ 'offline',
                              datetime >= as.POSIXct('2009-02-19 12:50', tz='UTC') & 
                                datetime < as.POSIXct('2009-02-23 13:00', tz='UTC') ~ 'offline',
                              datetime >= as.POSIXct('2009-04-15 3:00', tz='UTC') & 
                                datetime < as.POSIXct('2009-07-28 13:30', tz='UTC') ~ 'offline',
                              datetime >= as.POSIXct('2009-12-31 0:10', tz='UTC') ~ 'offline',
                              TRUE ~ location))

do_vert_b <- buoy2009_L1 %>% 
  select(datetime, location, DOSat, DOppm, DOTempC) %>% 
  gather(variable, value, -datetime, -location)

ggplot(do_vert_b,
       aes(x = datetime, y = value, color = location)) +
  geom_point() +
  facet_grid(variable~., scales = 'free_y') +
  final_theme +
  labs(title = '2009 DO data clean',
       x = NULL,
       y = NULL) +
  scale_x_datetime(date_minor_breaks = '1 month')

# add presumed cleaning flags
buoy2009_L1 <-  buoy2009_L1 %>%
  mutate(upper_do_flag = case_when(datetime == as.POSIXct('2009-07-28 13:30', tz='UTC') ~ 'wp',
                             TRUE ~ upper_do_flag)) %>% 
  mutate(upper_do_flag = paste('x', upper_do_flag, sep = ''))
  
do_vert_b <- buoy2009_L1 %>% 
  select(datetime, location, DOSat, DOppm, DOTempC, upper_do_flag) %>% 
  gather(variable, value, -datetime, -location, - upper_do_flag)

ggplot(do_vert_b,
       aes(x = datetime, y = value, color = location, shape = upper_do_flag)) +
  geom_point() +
  facet_grid(variable~., scales = 'free_y') +
  final_theme +
  labs(title = '2009 DO data clean',
       x = NULL,
       y = NULL) +
  scale_x_datetime(date_minor_breaks = '1 month')

rm(do_vert, do_vert_b)


#### wind sensors ####
range(buoy2009_L1$InstWindDir, na.rm = T)
range(buoy2009_L1$InstWindSp, na.rm = T)
range(buoy2009_L1$AveWindDir, na.rm = T)
range(buoy2009_L1$AveWindSp, na.rm = T)

wind_vert <- buoy2009_L1 %>% 
  select(datetime, InstWindDir, AveWindDir,InstWindSp, AveWindSp) %>% 
  gather(variable, value, -datetime)

# ggplot(wind_vert, aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = '2009 wind data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 month')

buoy2009_L1 <- buoy2009_L1 %>% 
  mutate(InstWindDir = case_when(InstWindDir==-6999 ~ NA_real_,
                                 TRUE ~ InstWindDir))

wind_vert <- buoy2009_L1 %>% 
  select(datetime, InstWindDir, AveWindDir,InstWindSp, AveWindSp) %>% 
  gather(variable, value, -datetime)

ggplot(wind_vert, aes(x = datetime, y = value)) +
  geom_point() +
  facet_grid(variable~., scales = 'free_y') +
  final_theme +
  labs(title = '2009 wind data NAs recoded',
       x = NULL,
       y = NULL) +
  scale_x_datetime(date_minor_breaks = '1 month')

# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2009-01-01', tz='UTC') &
#                         datetime<as.POSIXct('2009-02-01', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'jan 2009 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2009-02-01', tz='UTC') &
#                         datetime<as.POSIXct('2009-03-01', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'feb 2009 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')

buoy2009_L1 <- buoy2009_L1 %>% 
  mutate_at(vars(InstWindSp, InstWindDir),
            funs(case_when(datetime<as.POSIXct('2009-02-18', tz='UTC') ~ NA_real_,
                           TRUE ~ .)))

wind_vert_b <- buoy2009_L1 %>% 
  select(datetime, InstWindDir, AveWindDir,InstWindSp, AveWindSp) %>% 
  gather(variable, value, -datetime)

# ggplot(subset(wind_vert_b,
#               subset=(datetime>=as.POSIXct('2009-01-01', tz='UTC') &
#                         datetime<as.POSIXct('2009-03-01', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'feb 2009 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2009-03-01', tz='UTC') &
#                         datetime<as.POSIXct('2009-04-01', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'mar 2009 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# 
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2009-04-01', tz='UTC') &
#                         datetime<as.POSIXct('2009-05-01', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'apr 2009 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2009-05-01', tz='UTC') &
#                         datetime<as.POSIXct('2009-06-01', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'may 2009 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2009-06-01', tz='UTC') &
#                         datetime<as.POSIXct('2009-07-01', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'jun 2009 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2009-07-01', tz='UTC') &
#                         datetime<as.POSIXct('2009-08-01', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'july 2009 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2009-08-01', tz='UTC') &
#                         datetime<as.POSIXct('2009-09-01', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'aug 2009 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2009-09-01', tz='UTC') &
#                         datetime<as.POSIXct('2009-10-01', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'sept 2009 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2009-10-01', tz='UTC') &
#                         datetime<as.POSIXct('2009-11-01', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'oct 2009 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2009-11-01', tz='UTC') &
#                         datetime<as.POSIXct('2009-12-01', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'nov 2009 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2009-12-01', tz='UTC') &
#                         datetime<as.POSIXct('2010-01-01', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'sept 2009 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# #dec 26 sensor frozen
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2009-12-26', tz='UTC') &
#                         datetime<as.POSIXct('2009-12-27', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'sept 2009 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 hour')
# 
buoy2009_L1 <- buoy2009_L1 %>%
  mutate_at(vars(InstWindSp, InstWindDir, AveWindSp, AveWindDir),
            funs(case_when(datetime>=as.POSIXct('2009-12-26 19:40', tz='UTC') &
                             datetime<as.POSIXct('2009-12-26 21:50', tz='UTC')~ NA_real_,
                           TRUE ~ .)))

wind_vert_b <- buoy2009_L1 %>% 
  select(datetime, InstWindDir, AveWindDir,InstWindSp, AveWindSp, location) %>% 
  gather(variable, value, -datetime, -location)

# ggplot(subset(wind_vert_b,
#               subset=(datetime>=as.POSIXct('2009-12-01', tz='UTC') &
#                         datetime<as.POSIXct('2010-01-01', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'sept 2009 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')

# add flag to wind direction prior to jul 28 when buoy online
buoy2009_L1 <- buoy2009_L1 %>% 
  mutate(wind_dir_flag = case_when(datetime < as.POSIXct('2009-07-28', tz='UTC') ~ 'e',
                                   TRUE ~ ''))

ggplot(wind_vert_b,
       aes(x = datetime, y = value, color = location)) +
  geom_point() +
  facet_grid(variable~., scales = 'free_y') +
  final_theme +
  labs(title = '2009 wind data clean',
       x = NULL,
       y = NULL) +
  scale_x_datetime(date_minor_breaks = '1 month')

rm(wind_vert, wind_vert_b)

#### PAR ####
range(buoy2009_L1$PAR, na.rm = T)

# ggplot(buoy2009_L1, aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme

buoy2009_L1 <-  buoy2009_L1 %>%
  mutate(PAR = case_when(PAR <0 ~ 0,
                         TRUE ~ PAR))

ggplot(buoy2009_L1, aes(x = datetime, y = PAR)) +
  geom_point() +
  final_theme +
  labs(title = '2009 PAR data below 0 recoded',
       x = NULL,
       y = 'PAR (umol/m2/s)') +
  scale_x_datetime(date_minor_breaks = '1 month')

# ggplot(subset(buoy2009_L1,
#               subset = (datetime>=as.POSIXct('2009-01-01', tz='UTC') &
#                           datetime<as.POSIXct('2009-02-01', tz='UTC'))),
#        aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'jan 2009 PAR data raw',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')

#jan data incomplete. flagging as intermittent
buoy2009_L1 <-  buoy2009_L1 %>%
  mutate(PAR_flag = case_when(datetime<as.POSIXct('2009-01-15', tz='UTC') ~ 'i',
                         TRUE ~ ''))

# ggplot(subset(buoy2009_L1,
#               subset = (datetime>=as.POSIXct('2009-02-01', tz='UTC') &
#                           datetime<as.POSIXct('2009-03-01', tz='UTC'))),
#        aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'feb 2009 PAR data raw',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')

#incomplete data through feb 23 midday
buoy2009_L1 <-  buoy2009_L1 %>%
  mutate(PAR = case_when(datetime>=as.POSIXct('2009-02-01', tz='UTC') & datetime<as.POSIXct('2009-02-23', tz='UTC') ~ NA_real_,
                         TRUE ~ PAR))

# ggplot(subset(buoy2009_L1,
#               subset = (datetime>=as.POSIXct('2009-03-01', tz='UTC') &
#                           datetime<as.POSIXct('2009-04-01', tz='UTC'))),
#        aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'mar 2009 PAR data raw',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2009_L1,
#               subset = (datetime>=as.POSIXct('2009-04-01', tz='UTC') &
#                           datetime<as.POSIXct('2009-05-01', tz='UTC'))),
#        aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'apr 2009 PAR data raw',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2009_L1,
#               subset = (datetime>=as.POSIXct('2009-05-01', tz='UTC') &
#                           datetime<as.POSIXct('2009-06-01', tz='UTC'))),
#        aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'may 2009 PAR data raw',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2009_L1,
#               subset = (datetime>=as.POSIXct('2009-06-01', tz='UTC') &
#                           datetime<as.POSIXct('2009-07-01', tz='UTC'))),
#        aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'jun 2009 PAR data raw',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2009_L1,
#               subset = (datetime>=as.POSIXct('2009-07-01', tz='UTC') &
#                           datetime<as.POSIXct('2009-08-01', tz='UTC'))),
#        aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'jul 2009 PAR data raw',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2009_L1,
#               subset = (datetime>=as.POSIXct('2009-08-01', tz='UTC') &
#                           datetime<as.POSIXct('2009-09-01', tz='UTC'))),
#        aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'aug 2009 PAR data raw',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2009_L1,
#               subset = (datetime>=as.POSIXct('2009-09-01', tz='UTC') &
#                           datetime<as.POSIXct('2009-10-01', tz='UTC'))),
#        aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'sept 2009 PAR data raw',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2009_L1,
#               subset = (datetime>=as.POSIXct('2009-10-01', tz='UTC') &
#                           datetime<as.POSIXct('2009-11-01', tz='UTC'))),
#        aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'oct 2009 PAR data raw',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2009_L1,
#               subset = (datetime>=as.POSIXct('2009-11-01', tz='UTC') &
#                           datetime<as.POSIXct('2009-12-01', tz='UTC'))),
#        aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'nov 2009 PAR data raw',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2009_L1,
#               subset = (datetime>=as.POSIXct('2009-12-01', tz='UTC') &
#                           datetime<as.POSIXct('2010-01-01', tz='UTC'))),
#        aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'dec 2009 PAR data raw',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')

ggplot(buoy2009_L1, aes(x = datetime, y = PAR)) +
  geom_point() +
  final_theme +
  labs(title = '2009 PAR data clean',
       x = NULL,
       y = 'PAR (umol/m2/s)') +
  scale_x_datetime(date_minor_breaks = '1 month')



#### Air Temp ####
range(buoy2009_L1$AirTempC, na.rm = T)

ggplot(buoy2009_L1, aes(x=datetime, y = AirTempC)) +
  geom_point() +
  final_theme +
  labs(title = '2009 air temp raw',
       x= NULL,
       y= 'air temp (deg C)') +
  scale_x_datetime(date_minor_breaks = '1 month')

# data intermittent - recode Feb intermittent data

# ggplot(subset(buoy2009_L1,
#               subset=(datetime>=as.POSIXct('2009-01-01', tz='UTC') &
#                         datetime < as.POSIXct('2009-02-01', tz='UTC'))),
#        aes(x=datetime, y = AirTempC)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'feb 2009 air temp raw',
#        x= NULL,
#        y= 'air temp (deg C)') +
#   scale_x_datetime(date_minor_breaks = '1 day')

#flag through jan 15 as intermittent
buoy2009_L1 <- buoy2009_L1 %>% 
  mutate(airtemp_flag = case_when(datetime < as.POSIXct('2009-01-15', tz='UTC') ~ 'i',
                                  TRUE ~ ''))

# ggplot(subset(buoy2009_L1,
#               subset=(datetime>=as.POSIXct('2009-02-01', tz='UTC') &
#                         datetime < as.POSIXct('2009-03-01', tz='UTC'))),
#        aes(x=datetime, y = AirTempC)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'feb 2009 air temp raw',
#        x= NULL,
#        y= 'air temp (deg C)') +
#   scale_x_datetime(date_minor_breaks = '1 day')

buoy2009_L1 <- buoy2009_L1 %>% 
  mutate(AirTempC = case_when(datetime >= as.POSIXct('2009-02-06', tz='UTC') &
                                datetime < as.POSIXct('2009-02-23', tz='UTC') ~ NA_real_,
                              TRUE ~ AirTempC))

ggplot(buoy2009_L1, aes(x=datetime, y = AirTempC, color = airtemp_flag)) +
  geom_point() +
  final_theme +
  labs(title = '2009 air temp clean',
       x= NULL,
       y= 'air temp (deg C)') +
  scale_x_datetime(date_minor_breaks = '1 month')



#### EXPORT L1 FILES ####

#recode flags to '' when the buoy is offline
buoy2009_L1 <-buoy2009_L1 %>% 
  mutate_at(vars(temp_flag, upper_do_flag, wind_dir_flag, PAR_flag, airtemp_flag),
            funs(case_when(location == 'offline' ~ '',
                           TRUE ~ .)))

# #export L1 tempstring file
# buoy2009_L1 %>%
#   select(datetime, location, TempC_0m, TempC_0p5m, TempC_1m, TempC_1p5m, TempC_2m, TempC_2p5m, TempC_3m, TempC_4m, TempC_5m, TempC_6m, TempC_7m, TempC_8m, TempC_9m, TempC_10m, TempC_11m, TempC_13m, temp_flag) %>%
#   rename(TempC_13p5m = 'TempC_13m',
#          TempC_11p5m = 'TempC_11m',
#          TempC_10p5m = 'TempC_10m',
#          TempC_9p5m = 'TempC_9m',
#          TempC_8p5m = 'TempC_8m',
#          TempC_7p5m = 'TempC_7m',
#          TempC_6p5m = 'TempC_6m',
#          TempC_5p5m = 'TempC_5m',
#          TempC_4p5m = 'TempC_4m',
#          TempC_3p5m = 'TempC_3m',
#          TempC_3m = 'TempC_2p5m',
#          TempC_2p5m = 'TempC_2m',
#          TempC_2m = 'TempC_1p5m',
#          TempC_1p5m = 'TempC_1m',
#          TempC_1m = 'TempC_0p5m',
#          TempC_0p5m = 'TempC_0m') %>%
#   mutate(datetime = as.character(datetime)) %>%
#   write_csv(., 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2009_tempstring_L1.csv')
# 

#crete vertical dataset
buoy_2009_L1_vert <- buoy2009_L1 %>%
  select(datetime, location, TempC_0m, TempC_0p5m, TempC_1m, TempC_1p5m, TempC_2m, TempC_2p5m, TempC_3m, TempC_4m, TempC_5m, TempC_6m, TempC_7m, TempC_8m, TempC_9m, TempC_10m, TempC_11m, TempC_13m, temp_flag) %>%
  rename(TempC_13p5m = 'TempC_13m',
         TempC_11p5m = 'TempC_11m',
         TempC_10p5m = 'TempC_10m',
         TempC_9p5m = 'TempC_9m',
         TempC_8p5m = 'TempC_8m',
         TempC_7p5m = 'TempC_7m',
         TempC_6p5m = 'TempC_6m',
         TempC_5p5m = 'TempC_5m',
         TempC_4p5m = 'TempC_4m',
         TempC_3p5m = 'TempC_3m',
         TempC_3m = 'TempC_2p5m',
         TempC_2p5m = 'TempC_2m',
         TempC_2m = 'TempC_1p5m',
         TempC_1p5m = 'TempC_1m',
         TempC_1m = 'TempC_0p5m',
         TempC_0p5m = 'TempC_0m') %>%
  gather(depth_m, temp_degC, -datetime, -location, -temp_flag) %>% 
  mutate(depth_m = case_when(grepl(pattern = '_0p5m', x = depth_m) ~ '0.5',
                             grepl(pattern ='_1m', x = depth_m) ~ '1',
                             grepl(pattern = '_1p5m', x = depth_m) ~ '1.5',
                             grepl(pattern = '_2m', x = depth_m) ~ '2',
                             grepl(pattern = '_2p5m', x = depth_m) ~ '2.5',
                             grepl(pattern = '_3m', x = depth_m) ~ '3',
                             grepl(pattern = '_3p5m', x = depth_m) ~ '3.5',
                             grepl(pattern = '_4p5m', x = depth_m) ~ '4.5',
                             grepl(pattern = '_5p5m', x = depth_m) ~ '5.5',
                             grepl(pattern = '_6p5m', x = depth_m) ~ '6.5',
                             grepl(pattern = '_7p5m', x = depth_m) ~ '7.5',
                             grepl(pattern = '_8p5m', x = depth_m) ~ '8.5',
                             grepl(pattern = '_9p5m', x = depth_m) ~ '9.5',
                             grepl(pattern = '_10p5m', x = depth_m) ~ '10.5',
                             grepl(pattern = '_11p5m', x = depth_m) ~ '11.5',
                             grepl(pattern = '_13p5m', x = depth_m) ~ '13.5',
                             TRUE ~ NA_character_)) %>% 
  mutate(depth_m = as.numeric(depth_m))

#parse out flags
unique(buoy_2009_L1_vert$temp_flag)

buoy_2009_L1_vert <- buoy_2009_L1_vert %>% 
  mutate(temp_flag = case_when(temp_flag == 'i, 9.5d, 10.5d, 11.5d, 13.5d' & (depth_m == 9.5 | depth_m == 10.5 | depth_m == 11.5 | depth_m == 13.5) & !is.na(temp_degC) ~ 'i, d',
                               temp_flag == 'i, 9.5d, 10.5d, 11.5d, 13.5d' ~ 'i',
                               temp_flag == '9.5d, 10.5d, 11.5d, 13.5d' & (depth_m == 9.5 | depth_m == 10.5 | depth_m == 11.5 | depth_m == 13.5) & !is.na(temp_degC) ~ 'd',
                               temp_flag == 'q' ~ 's',
                               TRUE ~ NA_character_))
unique(buoy_2009_L1_vert$temp_flag)

#add flags to 11.5-13.5 as possibly in sediment
buoy_2009_L1_vert <-   buoy_2009_L1_vert %>% 
  mutate(temp_flag = case_when(!is.na(temp_degC) & depth_m >= 11.5 & is.na(temp_flag) ~ 'b',
                               !is.na(temp_degC) & depth_m >= 11.5 & !is.na(temp_flag) ~ paste(temp_flag, 'b', sep = ', '),
                               TRUE ~ temp_flag))

unique(buoy_2009_L1_vert$temp_flag)                               

#plot to check
ggplot(buoy_2009_L1_vert, aes(x = datetime, y = temp_degC, color = as.factor(depth_m))) +
  geom_point() +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
                              "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust

ggplot(buoy_2009_L1_vert, aes(x = datetime, y = temp_degC, color = as.factor(depth_m))) +
  geom_point(aes(shape = temp_flag)) +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
                              "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust

#order by date, depth
buoy_2009_L1_vert <- buoy_2009_L1_vert %>% 
  arrange(datetime, depth_m)

# buoy_2009_L1_vert %>%
#   mutate(datetime = as.character(datetime)) %>%
#   write_csv(., 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2009_tempstring_vert_L1.csv')
# 
# # export L1 do file
# buoy2009_L1 %>%
#   select(datetime, location, DOSat, DOppm, DOTempC, upper_do_flag) %>%
#   mutate(datetime = as.character(datetime)) %>%
#   write_csv(., 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2009_do_L1.csv')
# 
# # export L1 wind data
# buoy2009_L1 %>%
#   select(datetime, location, InstWindDir, InstWindSp, AveWindDir, AveWindSp, wind_dir_flag) %>%
#   mutate(datetime = as.character(datetime)) %>%
#   rename(WindSp_ms = 'InstWindSp',
#          WindDir_deg = 'InstWindDir',
#          AveWindSp_ms = 'AveWindSp',
#          AveWindDir_deg = 'AveWindDir') %>%
#   write_csv(., 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2009_wind_L1.csv')
# 
# # export PAR data
# buoy2009_L1 %>%
#   select(datetime, location, PAR, PAR_flag) %>%
#   mutate(datetime = as.character(datetime)) %>%
#   rename(PAR_umolm2s = 'PAR') %>%
#   write_csv(., 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2009_PAR_L1.csv')
# 
# # export L1 air temp data
# buoy2009_L1 %>%
#   select(datetime, location, AirTempC, airtemp_flag) %>%
#   mutate(datetime = as.character(datetime)) %>%
#   rename(AirTemp_degC = 'AirTempC') %>%
#   write_csv(., 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2009_AirTemp_L1.csv')

