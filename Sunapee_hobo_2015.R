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
hobo2015 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L0/2015 Summer Hobo.csv',
                      col_types = 'icnnnnnnnnnnnnnnnnnn')

# #bring in 2017 LMP data for comparison
# LMP2015 <- read_xlsx('C:/Users/steeleb/Dropbox/Lake Sunapee/long term Sunapee data/raw data/LMP files/LMP 2017/DO.xlsx', 
#                      sheet='DO',
#                      col_types = c('text', 'text', 'numeric', 'guess', 'numeric', 
#                                    'numeric', 'numeric', 'numeric', 'numeric' ,'numeric',
#                                    'text', 'text', 'numeric', 'text')) %>% 
#   mutate(DATE = as.Date(DATE, format='%Y-%m-%d')) %>%  #format date
#   filter(DATE >= '2015-01-01' & DATE < '2016-01-01', #filter for 2015 only
#          STATION == 210) %>%  #include only location closest to buoy
#   mutate(TIME = case_when(TIME == 99 ~ 909, #fix times in LMP database
#                           TIME == 98 ~ 908,
#                           is.na(TIME) ~ 1200, #force NA to noon for graphing
#                           TRUE ~ TIME))
# 
# #### format data ####
# # format time in LMP data
# LMP2015 <- LMP2015 %>% 
#   mutate(hour = TIME%/%100,
#          minutes = TIME%%100,
#          hour = replace(hour, hour==1, 13), #force data in 24h time
#          time = paste(hour, minutes, sep=':')) %>% #break out time from TIME, create time column
#   mutate(datetime = as.POSIXct(paste(DATE, time, sep=' '), format='%Y-%m-%d %H:%M', tz='UTC')) %>%  #tibble forces to UTC, so coerce for our uses.
#   select(-hour, -minutes, -TIME, -time) #remove unnecessary columns
#   
# str(LMP2015)
# 
# 
# #subset LMP for truthing data
# LMP2015_temp <- LMP2015 %>% 
#   select(datetime, DEPTH, TEMP) %>% 
#   filter(DEPTH<=12) %>% 
#   rename(value = 'TEMP') %>% 
#   mutate(variable = case_when(DEPTH == 0.5 ~ 'TempC_0p5m',
#                               DEPTH == 1 ~ 'TempC_1m',
#                               DEPTH == 2 ~ 'TempC_2m',
#                               DEPTH == 3 ~ 'TempC_3m',
#                               DEPTH == 4 ~ 'TempC_4m',
#                               DEPTH == 5 ~ 'TempC_5m',
#                               DEPTH == 6 ~ 'TempC_6m',
#                               DEPTH == 7 ~ 'TempC_7m',
#                               DEPTH == 8 ~ 'TempC_8m',
#                               DEPTH == 9 ~ 'TempC_9m',
#                               DEPTH == 10 ~ 'TempC_10m',
#                               DEPTH == 11 ~ 'TempC_11m',
#                               DEPTH == 12 ~ 'TempC_12m',
#                               TRUE ~ ''),
#          source = 'LMP')

#format date of hobo sensors
hobo2015 <- hobo2015 %>% 
  rename(Date.Time = 'Date/Time') %>% 
  mutate(datetime = as.POSIXct(Date.Time, format='%m/%d/%Y %H:%M', tz='UTC')) %>% 
  select(-Date.Time)


#### hobo line ####
hobo_vert <- hobo2015 %>%
  select(datetime, TempC_1m, TempC_2m, TempC_3m, TempC_4m, TempC_5m, TempC_6m, TempC_7m, TempC_8m, TempC_9m) %>%
  gather(variable, value, -datetime)

ggplot(subset(hobo_vert, subset=(datetime>='2015-01-01' & datetime < '2016-01-01')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='hobo temp sensor summer 2015', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  final_theme

# ggplot(subset(hobo_vert, subset=(datetime>='2015-08-01' & datetime < '2015-09-01')), aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='hobo temp aug 2015', x='date', y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme
# 
# ggplot(subset(hobo_vert, subset=(datetime>='2015-09-01' & datetime < '2015-10-01')), aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='hobo temp sept 2015', x='date', y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme



# #reality check with LMP
# hobo_LMP_2015 <- hobo_vert %>%
#   mutate(source='hobo') %>%
#   full_join(., LMP2015_temp)
# unique(LMP2015_temp$datetime)
# 
# #June 22, July 20, Aug 10, Sept 21
# ggplot(subset(hobo_LMP_2015,
#               subset=(datetime>=as.POSIXct('2015-06-22', tz='UTC') &
#                         datetime<as.POSIXct('2015-06-23', tz='UTC'))),
#        aes(x=datetime, y=value, color=as.factor(variable), shape = source)) +
#   geom_point() +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# ggplot(subset(hobo_LMP_2015,
#               subset=(datetime>=as.POSIXct('2015-07-20', tz='UTC') &
#                         datetime<as.POSIXct('2015-07-21', tz='UTC'))),
#        aes(x=datetime, y=value, color=as.factor(variable), shape = source)) +
#   geom_point() +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# ggplot(subset(hobo_LMP_2015,
#               subset=(datetime>=as.POSIXct('2015-08-10', tz='UTC') &
#                         datetime<as.POSIXct('2015-08-11', tz='UTC'))),
#        aes(x=datetime, y=value, color=as.factor(variable), shape = source)) +
#   geom_point() +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# ggplot(subset(hobo_LMP_2015,
#               subset=(datetime>=as.POSIXct('2015-09-21', tz='UTC') &
#                         datetime<as.POSIXct('2015-09-22', tz='UTC'))),
#        aes(x=datetime, y=value, color=as.factor(variable), shape = source)) +
#   geom_point() +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust


# #export L1 tempstring file
# hobo2015 %>% 
#   select(datetime, TempC_1m, TempC_2m, TempC_3m, TempC_4m, TempC_5m, TempC_6m, TempC_7m, TempC_8m, TempC_9m) %>% 
#   rename(TempC_0p5m = 'TempC_1m',
#          TempC_1p5m = 'TempC_2m',
#          TempC_2p5m = 'TempC_3m',
#          TempC_3p5m = 'TempC_4m',
#          TempC_4p5m = 'TempC_5m',
#          TempC_5p5m = 'TempC_6m',
#          TempC_6p5m = 'TempC_7m',
#          TempC_7p5m = 'TempC_8m',
#          TempC_8p5m = 'TempC_9m') %>% 
#   mutate(datetime = as.character(datetime)) %>% 
#   write_csv(., 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2015_hobotempstring_L1.csv')

#clean up workspace
rm(hobo_vert, hobo_LMP_2015)




