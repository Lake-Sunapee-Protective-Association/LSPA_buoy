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

#bring in 2015 buoy raw data
buoy2015 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L0/2015 Buoy Data.csv',
                     col_types = 'iiiinnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnn')
hobo2015 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L0/2015 Summer Hobo.csv',
                      col_types = 'icnnnnnnnnnnnnnnnnnn')

#bring in 2017 LMP data for comparison
LMP2015 <- read_xlsx('C:/Users/steeleb/Dropbox/Lake Sunapee/long term Sunapee data/raw data/LMP files/LMP 2017/DO.xlsx', 
                     sheet='DO',
                     col_types = c('text', 'text', 'numeric', 'guess', 'numeric', 
                                   'numeric', 'numeric', 'numeric', 'numeric' ,'numeric',
                                   'text', 'text', 'numeric', 'text')) %>% 
  mutate(DATE = as.Date(DATE, format='%Y-%m-%d')) %>%  #format date
  filter(DATE >= '2015-01-01' & DATE < '2016-01-01', #filter for 2015 only
         STATION == 210) %>%  #include only location closest to buoy
  mutate(TIME = case_when(TIME == 99 ~ 909, #fix times in LMP database
                          TIME == 98 ~ 908,
                          is.na(TIME) ~ 1200, #force NA to noon for graphing
                          TRUE ~ TIME))

LMP2015_bio <- read_xlsx('C:/Users/steeleb/Dropbox/Lake Sunapee/long term Sunapee data/raw data/LMP files/LMP 2017/BIOLOGY.xlsx', 
                     sheet='BIOLOGY',
                     col_types = c('text', 'text', 'numeric', 'guess', 'numeric', 
                                   'numeric', 'text', 'numeric', 'text' ,'numeric',
                                   'text', 'numeric', 'numeric', 'numeric', 'text',
                                   'text', 'text', 'text')) %>% 
  mutate(DATE = as.Date(DATE, format='%Y-%m-%d')) %>%  #format date
  filter(DATE >= '2015-01-01' & DATE < '2016-01-01', #filter for 2015 only
         STATION == 210) 


#### format data ####
buoy2015 <- buoy2015 %>%
  rename(Hr.Min = 'Hr/Min') %>% 
  mutate(hour = Hr.Min%/%100,
         minutes = Hr.Min%%100,
         time = paste(hour, minutes, sep=':')) %>% #break out time from Hr.Min, create time column
  mutate(date = as.Date(paste(Day, Year, sep = '-'), format='%j-%Y'), #create date in ymd format
         datetime = as.POSIXct(paste(date, time, sep=' '), format='%Y-%m-%d %H:%M', tz='UTC')) %>%  #tibble forces to UTC, so coerce for our uses.
  select(-hour, -minutes, -Hr.Min, -time) #remove unnecessary columns
  

# format time in LMP data
LMP2015 <- LMP2015 %>% 
  mutate(hour = TIME%/%100,
         minutes = TIME%%100,
         hour = replace(hour, hour==1, 13), #force data in 24h time
         time = paste(hour, minutes, sep=':')) %>% #break out time from TIME, create time column
  mutate(datetime = as.POSIXct(paste(DATE, time, sep=' '), format='%Y-%m-%d %H:%M', tz='UTC')) %>%  #tibble forces to UTC, so coerce for our uses.
  select(-hour, -minutes, -TIME, -time) #remove unnecessary columns
  
str(buoy2015)
str(LMP2015)


#subset LMP for truthing data
LMP2015_temp <- LMP2015 %>% 
  select(datetime, DEPTH, TEMP) %>% 
  filter(DEPTH<=12) %>% 
  rename(value = 'TEMP') %>% 
  mutate(variable = case_when(DEPTH == 0.5 ~ 'TempC_0p5m',
                              DEPTH == 1 ~ 'TempC_1m',
                              DEPTH == 2 ~ 'TempC_2m',
                              DEPTH == 3 ~ 'TempC_3m',
                              DEPTH == 4 ~ 'TempC_4m',
                              DEPTH == 5 ~ 'TempC_5m',
                              DEPTH == 6 ~ 'TempC_6m',
                              DEPTH == 7 ~ 'TempC_7m',
                              DEPTH == 8 ~ 'TempC_8m',
                              DEPTH == 9 ~ 'TempC_9m',
                              DEPTH == 10 ~ 'TempC_10m',
                              DEPTH == 11 ~ 'TempC_11m',
                              DEPTH == 12 ~ 'TempC_12m',
                              TRUE ~ ''),
         source = 'LMP')

LMP2015_upDO <- LMP2015 %>% 
  select(datetime, DEPTH, DO, PCNTSAT) %>% 
  filter(DEPTH <= 2) %>% 
  rename(depth = 'DEPTH', 
         DOppm = 'DO', 
         DOSat = 'PCNTSAT') %>% 
  gather(variable, value, -datetime, -depth) %>% 
  mutate(source = 'LMP')

LMP2015_lowDO <- LMP2015 %>% 
  select(datetime, DEPTH, DO, PCNTSAT) %>% 
  filter(DEPTH >= 2) %>% 
  rename(depth = 'DEPTH', 
         DOLowPPM = 'DO', 
         DOLowSat = 'PCNTSAT') %>% 
  gather(variable, value, -datetime, -depth) %>% 
  mutate(source = 'LMP')

#format date of hobo sensors
hobo2015 <- hobo2015 %>% 
  rename(Date.Time = 'Date/Time') %>% 
  mutate(datetime = as.POSIXct(Date.Time, format='%m/%d/%Y %H:%M', tz='UTC')) %>% 
  select(-Date.Time)


buoy2015_L1 <- buoy2015

#####2015 thermisters - remove/replace NA values ####
buoy2015_L1 <- buoy2015_L1 %>% 
  mutate_at(vars(TempC_0m:TempC_10m),
            funs(case_when(. == -6999 ~ NA_real_,
                           . == 1215 ~ NA_real_,
                           . == 555.4 ~ NA_real_,
                           . == 0 ~ NA_real_,
                           TRUE ~ .)))

# ##transform data to vertical for display
# buoy2015_vert_temp <- buoy2015_L1 %>% 
#   select(datetime, TempC_0m, TempC_1m, TempC_2m, TempC_3m, TempC_4m, TempC_5m, TempC_6m, TempC_7m, TempC_8m, TempC_9m, TempC_10m) %>% 
#   gather(variable, value, -datetime)
# 
# ggplot(subset(buoy2015_vert_temp, subset=(datetime>='2015-01-01' & datetime < '2016-01-01')), aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='2015, raw', x='date', y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme
# 
# ggplot(subset(buoy2015_vert_temp, subset=(datetime>='2015-06-01' & datetime < '2015-07-01')), aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='June 2015', x='date', y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme
# 
# ggplot(subset(buoy2015_vert_temp, subset=(datetime>='2015-07-01' & datetime < '2015-08-01')), aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='July 2015', x='date', y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme
# 
# #temp line removed Jul 22 13:00


####2015 thermisters compare with LMP####
# buoy2015_vert_temp_L1 <- buoy2015_L1 %>% 
#   select(datetime, TempC_0m, TempC_1m, TempC_2m, TempC_3m, TempC_4m, TempC_5m, TempC_6m, TempC_7m, TempC_8m, TempC_9m, TempC_10m) %>% 
#   gather(variable, value, -datetime) %>% 
#   mutate(depth = case_when(variable == 'TempC_0m'~ 0,
#                            variable == 'TempC_1m'~ 1,
#                            variable == 'TempC_2m'~ 2,
#                            variable == 'TempC_3m'~ 3,
#                            variable == 'TempC_4m'~ 4,
#                            variable == 'TempC_5m'~ 5,
#                            variable == 'TempC_6m'~ 6,
#                            variable == 'TempC_7m'~ 7,
#                            variable == 'TempC_8m'~ 8,
#                            variable == 'TempC_9m'~ 9,
#                            variable == 'TempC_10m'~ 10,
#                            TRUE ~ NA_real_),
#          source = 'buoy')
# 
# buoy2015_temp_LMP_L1 <- full_join(buoy2015_vert_temp_L1, LMP2015_temp) %>% 
#   mutate(depth = factor(depth, levels=c('0', '0.5', '1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12')))
# 
# #plot buoy temp against LMP sonde values
# ggplot(subset(buoy2015_temp_LMP_L1, subset=(datetime>=as.POSIXct('2015-06-22', tz='UTC') & datetime < as.POSIXct('2015-06-23', tz='UTC'))), aes(x=datetime, y=value, col=depth, shape=source)) +
#   geom_point(size=2) +
#   coord_cartesian(ylim=c(5,25)) +
#   labs(title='June 22 buoy and LMP', x='date', y='') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +
#   final_theme
# 
# ggplot(subset(buoy2015_temp_LMP_L1, subset=(datetime>=as.POSIXct('2015-07-19', tz='UTC') & datetime < as.POSIXct('2015-07-21', tz='UTC'))), aes(x=datetime, y=value, col=depth, shape=source)) +
#   geom_point(size=2) +
#   coord_cartesian(ylim=c(5,25)) +
#   labs(title='July 20 buoy and LMP', x='date', y='') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +
#   final_theme
# 
# ggplot(subset(buoy2015_temp_LMP_L1, subset=(datetime>=as.POSIXct('2015-08-09', tz='UTC') & datetime < as.POSIXct('2015-08-11', tz='UTC'))), aes(x=datetime, y=value, col=depth, shape=source)) +
#   geom_point(size=2) +
#   coord_cartesian(ylim=c(5,25)) +
#   labs(title='August 10 buoy and LMP', x='date', y='') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +
#   final_theme
# 
# ggplot(subset(buoy2015_temp_LMP_L1, subset=(datetime>=as.POSIXct('2015-09-20', tz='UTC') & datetime < as.POSIXct('2015-09-22', tz='UTC'))), aes(x=datetime, y=value, col=depth, shape=source)) +
#   geom_point(size=2) +
#   coord_cartesian(ylim=c(5,25)) +
#   labs(title='August 10 buoy and LMP', x='date', y='') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +
#   final_theme

####2015 thermisters - data cleaning####
# remove 0m completely
buoy2015_L1 <- buoy2015_L1 %>% 
  mutate(TempC_0m = NA_real_)

# buoy2015_vert_temp_L1 <- buoy2015_L1 %>% 
#   select(datetime, TempC_0m, TempC_1m, TempC_2m, TempC_3m, TempC_4m, TempC_5m, TempC_6m, TempC_7m, TempC_8m, TempC_9m, TempC_10m) %>% 
#   gather(variable, value, -datetime) %>% 
#   mutate(depth = case_when(variable == 'TempC_0m'~ 0,
#                            variable == 'TempC_1m'~ 1,
#                            variable == 'TempC_2m'~ 2,
#                            variable == 'TempC_3m'~ 3,
#                            variable == 'TempC_4m'~ 4,
#                            variable == 'TempC_5m'~ 5,
#                            variable == 'TempC_6m'~ 6,
#                            variable == 'TempC_7m'~ 7,
#                            variable == 'TempC_8m'~ 8,
#                            variable == 'TempC_9m'~ 9,
#                            variable == 'TempC_10m'~ 10,
#                            TRUE ~ NA_real_),
#          variable = factor(variable, levels=c('TempC_0m', 'TempC_1m', 'TempC_2m', 'TempC_3m', 'TempC_4m', 'TempC_5m', 'TempC_6m', 'TempC_7m', 'TempC_8m', 'TempC_9m', 'TempC_10m')))
#          
# #buoy moved to June 11 11:00
# ggplot(subset(buoy2015_vert_temp_L1, subset=(datetime>=as.POSIXct('2015-06-01', tz='UTC') & datetime < as.POSIXct('2015-06-16', tz='UTC'))), aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='early June 2015, v2', x='date', y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme
# 
# ggplot(subset(buoy2015_vert_temp, subset=(datetime>=as.POSIXct('2015-06-11', tz='UTC') & datetime < as.POSIXct('2015-06-11 12:00', tz='UTC'))), aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='June 11 2015, buoy to summer location', x='date', y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme

buoy2015_L1 <- buoy2015_L1 %>% 
  mutate_at(vars(alltemp),
            funs(case_when(datetime>=as.POSIXct('2015-01-01', tz='UTC') & datetime<=as.POSIXct('2015-06-11 9:50', tz='UTC') ~ NA_real_,
                           TRUE ~ .))) 

# buoy2015_vert_temp_L1 <- buoy2015_L1 %>% 
#   select(datetime, TempC_0m, TempC_1m, TempC_2m, TempC_3m, TempC_4m, TempC_5m, TempC_6m, TempC_7m, TempC_8m, TempC_9m, TempC_10m) %>% 
#   gather(variable, value, -datetime) %>% 
#   mutate(depth = case_when(variable == 'TempC_0m'~ 0,
#                            variable == 'TempC_1m'~ 1,
#                            variable == 'TempC_2m'~ 2,
#                            variable == 'TempC_3m'~ 3,
#                            variable == 'TempC_4m'~ 4,
#                            variable == 'TempC_5m'~ 5,
#                            variable == 'TempC_6m'~ 6,
#                            variable == 'TempC_7m'~ 7,
#                            variable == 'TempC_8m'~ 8,
#                            variable == 'TempC_9m'~ 9,
#                            variable == 'TempC_10m'~ 10,
#                            TRUE ~ NA_real_),
#          variable = factor(variable, levels=c('TempC_0m', 'TempC_1m', 'TempC_2m', 'TempC_3m', 'TempC_4m', 'TempC_5m', 'TempC_6m', 'TempC_7m', 'TempC_8m', 'TempC_9m', 'TempC_10m')))
# 
# ggplot(subset(buoy2015_vert_temp_L1, subset=(datetime>=as.POSIXct('2015-06-11', tz='UTC') & datetime < as.POSIXct('2015-06-11 12:00', tz='UTC'))), aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='June 11 2015, buoy to summer location, L1', x='date', y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme
# 
# ggplot(subset(buoy2015_vert_temp_L1, subset=(datetime>=as.POSIXct('2015-06-01', tz='UTC') & datetime < as.POSIXct('2015-06-16', tz='UTC'))), aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='early June 2015, v3', x='date', y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme
# 
# #remove data after initial break in data
# ggplot(subset(buoy2015_vert_temp_L1, subset=(datetime>=as.POSIXct('2015-06-24 18:00', tz='UTC') & datetime < as.POSIXct('2015-06-25 6:00', tz='UTC'))), aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='late June 2015, v2', x='date', y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme

#remove data after 3am until end of year
buoy2015_L1 <- buoy2015_L1 %>% 
  mutate_at(vars(alltemp),
            funs(case_when(datetime>=as.POSIXct('2015-06-25 3:00', tz='UTC') & datetime<as.POSIXct('2016-01-01', tz='UTC') ~ NA_real_,
                           TRUE ~ .))) 

buoy2015_vert_temp_L1 <- buoy2015_L1 %>%
  select(datetime, TempC_0m, TempC_1m, TempC_2m, TempC_3m, TempC_4m, TempC_5m, TempC_6m, TempC_7m, TempC_8m, TempC_9m, TempC_10m) %>%
  gather(variable, value, -datetime) %>%
  mutate(depth = case_when(variable == 'TempC_0m'~ 0,
                           variable == 'TempC_1m'~ 1,
                           variable == 'TempC_2m'~ 2,
                           variable == 'TempC_3m'~ 3,
                           variable == 'TempC_4m'~ 4,
                           variable == 'TempC_5m'~ 5,
                           variable == 'TempC_6m'~ 6,
                           variable == 'TempC_7m'~ 7,
                           variable == 'TempC_8m'~ 8,
                           variable == 'TempC_9m'~ 9,
                           variable == 'TempC_10m'~ 10,
                           TRUE ~ NA_real_),
         variable = factor(variable, levels=c('TempC_0m', 'TempC_1m', 'TempC_2m', 'TempC_3m', 'TempC_4m', 'TempC_5m', 'TempC_6m', 'TempC_7m', 'TempC_8m', 'TempC_9m', 'TempC_10m')))
# 
# 
# ggplot(subset(buoy2015_vert_temp_L1, subset=(datetime>=as.POSIXct('2015-06-16', tz='UTC') & datetime < as.POSIXct('2015-07-01', tz='UTC'))), aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='late June 2015, v3', x='date', y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme
# 
ggplot(subset(buoy2015_vert_temp_L1, subset=(datetime>=as.POSIXct('2015-01-01', tz='UTC') & datetime < as.POSIXct('2016-01-01', tz='UTC'))), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='2015, clean', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  final_theme

buoy2015_L1 <- buoy2015_L1 %>% 
  mutate(source = 'buoy')

#reality check with LMP
buoy_LMP_2015 <- buoy2015_vert_temp_L1 %>%
  mutate(source='buoy') %>%
  full_join(., LMP2015_temp)
unique(LMP2015_temp$datetime)

#June 22, July 20, Aug 10, Sept 21
ggplot(subset(buoy_LMP_2015,
              subset=(datetime>=as.POSIXct('2015-06-22', tz='UTC') &
                        datetime<as.POSIXct('2015-06-23', tz='UTC'))),
       aes(x=datetime, y=value, color=as.factor(variable), shape = source)) +
  geom_point() +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
                              "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust

ggplot(subset(buoy_LMP_2015,
              subset=(datetime>=as.POSIXct('2015-07-20', tz='UTC') &
                        datetime<as.POSIXct('2015-07-21', tz='UTC'))),
       aes(x=datetime, y=value, color=as.factor(variable), shape = source)) +
  geom_point() +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
                              "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust

ggplot(subset(buoy_LMP_2015,
              subset=(datetime>=as.POSIXct('2015-08-10', tz='UTC') &
                        datetime<as.POSIXct('2015-08-11', tz='UTC'))),
       aes(x=datetime, y=value, color=as.factor(variable), shape = source)) +
  geom_point() +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
                              "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust

ggplot(subset(buoy_LMP_2015,
              subset=(datetime>=as.POSIXct('2015-09-21', tz='UTC') &
                        datetime<as.POSIXct('2015-09-22', tz='UTC'))),
       aes(x=datetime, y=value, color=as.factor(variable), shape = source)) +
  geom_point() +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
                              "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust

#export L1 tempstring file
buoy2015_L1 %>% 
  select(datetime, TempC_0m, TempC_1m, TempC_2m, TempC_3m, TempC_4m, TempC_5m, TempC_6m, TempC_7m, TempC_8m, TempC_9m, TempC_10m) %>% 
  mutate(datetime = as.character(datetime)) %>% 
  write_csv(., 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2015_tempstring_L1.csv')

#### hobo line ####
hobo_vert <- hobo2015 %>%
  select(datetime, TempC_1m, TempC_2m, TempC_3m, TempC_4m, TempC_5m, TempC_6m, TempC_7m, TempC_8m, TempC_9m) %>%
  gather(variable, value, -datetime)
# 
# ggplot(subset(hobo_vert, subset=(datetime>='2015-01-01' & datetime < '2016-01-01')), aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='hobo temp sensor summer 2015', x='date', y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +   
#   final_theme
# 
# ggplot(subset(hobo_vert, subset=(datetime>='2015-08-01' & datetime < '2015-08-16')), aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='hobo temp early aug 2015', x='date', y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +   
#   final_theme
# 
# ggplot(subset(hobo_vert, subset=(datetime>='2015-08-16' & datetime < '2015-09-01')), aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='hobo temp late aug 2015', x='date', y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +   
#   final_theme
# 
# ggplot(subset(hobo_vert, subset=(datetime>='2015-09-01' & datetime < '2015-09-16')), aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='hobo temp early sept 2015', x='date', y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +   
#   final_theme
# 
# ggplot(subset(hobo_vert, subset=(datetime>='2015-09-16' & datetime < '2015-10-01')), aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='hobo temp late sept 2015', x='date', y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +   
#   final_theme
# 
# 
# hobo_temp <- hobo2015 %>%
#   select(datetime, TempC_1m, TempC_2m, TempC_3m, TempC_4m, TempC_5m, TempC_6m, TempC_7m, TempC_8m, TempC_9m) %>% 
#   mutate(source = 'hobo')

buoy_hobo_2015_L1 <- full_join(buoy2015_L1, hobo_temp)



         variable = factor(variable, levels=c('TempC_0m', 'TempC_1m', 'TempC_2m', 'TempC_3m', 'TempC_4m', 'TempC_5m', 'TempC_6m', 'TempC_7m', 'TempC_8m', 'TempC_9m', 'TempC_10m')))

#reality check with LMP
hobo_LMP_2015 <- hobo_vert %>%
  mutate(source='hobo') %>%
  full_join(., LMP2015_temp)
unique(LMP2015_temp$datetime)

#June 22, July 20, Aug 10, Sept 21
ggplot(subset(hobo_LMP_2015,
              subset=(datetime>=as.POSIXct('2015-06-22', tz='UTC') &
                        datetime<as.POSIXct('2015-06-23', tz='UTC'))),
       aes(x=datetime, y=value, color=as.factor(variable), shape = source)) +
  geom_point() +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
                              "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust

ggplot(subset(hobo_LMP_2015,
              subset=(datetime>=as.POSIXct('2015-07-20', tz='UTC') &
                        datetime<as.POSIXct('2015-07-21', tz='UTC'))),
       aes(x=datetime, y=value, color=as.factor(variable), shape = source)) +
  geom_point() +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
                              "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust

ggplot(subset(hobo_LMP_2015,
              subset=(datetime>=as.POSIXct('2015-08-10', tz='UTC') &
                        datetime<as.POSIXct('2015-08-11', tz='UTC'))),
       aes(x=datetime, y=value, color=as.factor(variable), shape = source)) +
  geom_point() +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
                              "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust

ggplot(subset(hobo_LMP_2015,
              subset=(datetime>=as.POSIXct('2015-09-21', tz='UTC') &
                        datetime<as.POSIXct('2015-09-22', tz='UTC'))),
       aes(x=datetime, y=value, color=as.factor(variable), shape = source)) +
  geom_point() +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
                              "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
#export L1 tempstring file
hobo2015 %>% 
  select(datetime, TempC_1m, TempC_2m, TempC_3m, TempC_4m, TempC_5m, TempC_6m, TempC_7m, TempC_8m, TempC_9m) %>% 
  mutate(datetime = as.character(datetime)) %>% 
  write_csv(., 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2015_hobotempstring_L1.csv')

#clean up workspace
rm(buoy_LMP_2015, buoy2015_vert_temp_L1, LMP2015_temp, hobo_vert, hobo_LMP_2015)

# ####2015 hobo + thermistor line####
# ggplot(subset(hobotherm_temp_vert, subset=(datetime>=as.POSIXct('2015-01-01', tz='UTC') & datetime < as.POSIXct('2016-01-01', tz='UTC'))), aes(x=datetime, y=value, col=variable, shape=source)) +
#  geom_point() +
#  labs(title='thermistor + hoboline 2015', x='date', y='temp (deg C)') +
#  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                              "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +
#   final_theme

# ####2015 hobo and LMP####
# hobo_vert <- hobo_vert %>% 
#   mutate(depth = case_when(variable == 'TempC_0m' ~ 0,
#                               variable == 'TempC_1m' ~ 1,
#                               variable == 'TempC_2m' ~ 2,
#                               variable == 'TempC_3m' ~ 3,
#                               variable == 'TempC_4m' ~ 4,
#                               variable == 'TempC_5m' ~ 5,
#                               variable == 'TempC_6m' ~ 6,
#                               variable == 'TempC_7m' ~ 7,
#                               variable == 'TempC_8m' ~ 8,
#                               variable == 'TempC_9m' ~ 9),
#          source = 'hobo')
# 
# buoy2015_hobo_LMP_L1 <- full_join(hobo_vert, LMP2015_temp) 
# 
# #plot buoy temp against LMP sonde values
# ggplot(subset(buoy2015_hobo_LMP_L1, subset=(datetime>='2015-08-09' & datetime < '2015-08-11')), aes(x=datetime, y=value, col=as.factor(depth), shape=source)) +
#   geom_point(size=2) +
#   coord_cartesian(ylim=c(5,25)) +
#   labs(title='August 10 buoy and LMP', x='date', y='') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +
#   final_theme
# 
# ggplot(subset(buoy2015_hobo_LMP_L1, subset=(datetime>='2015-09-20' & datetime < '2015-09-22')), aes(x=datetime, y=value, col=as.factor(depth), shape=source)) +
#   geom_point(size=2) +
#   coord_cartesian(ylim=c(5,25)) +
#   labs(title='September 21 buoy and LMP', x='date', y='') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +
#   final_theme
# 
# ####clean hobo line####
# ggplot(subset(hobotherm_temp_vert, subset=(datetime>=as.POSIXct('2015-08-01', tz='UTC') & datetime < as.POSIXct('2015-09-01', tz='UTC'))), aes(x=datetime, y=value, col=variable, shape=source)) +
#   geom_point() +
#   labs(title='hobo August', x='date', y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +
#   final_theme
# 
# ggplot(subset(hobotherm_temp_vert, subset=(datetime>=as.POSIXct('2015-09-01', tz='UTC') & datetime < as.POSIXct('2015-10-01', tz='UTC'))), aes(x=datetime, y=value, col=variable, shape=source)) +
#   geom_point() +
#   labs(title='hobo September', x='date', y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +
#   final_theme

hobo2015 <-  hobo2015 %>% 
  mutate(tempsource = 'hobo',
         location = 'deep spot') %>% 
  rename(TempC_1m_hobo = 'TempC_1m',
         TempC_2m_hobo = 'TempC_2m',
         TempC_3m_hobo = 'TempC_3m',
         TempC_4m_hobo = 'TempC_4m',
         TempC_5m_hobo = 'TempC_5m',
         TempC_6m_hobo = 'TempC_6m',
         TempC_7m_hobo = 'TempC_7m',
         TempC_8m_hobo = 'TempC_8m',
         TempC_9m_hobo = 'TempC_9m')

####plot Upper DO ####
# remove NA values
buoy2015_L1 <- buoy2015_L1 %>% 
  mutate_at(vars(upDO),
            funs(case_when(. == -6999 ~ NA_real_,
                           . == 0 ~ NA_real_,
                           TRUE ~ .)))

# ##transform data to vertical for display
# buoy2015_vert_updo <- buoy2015_L1 %>% 
#   select(datetime, upDO) %>% 
#   gather(variable, value, -datetime)
# 
# ggplot(subset(buoy2015_vert_updo, subset=(datetime>=as.POSIXct('2015-01-01', tz='UTC') & datetime < as.POSIXct('2016-01-01', tz='UTC'))), aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='2015, raw', x='date', y='') +
#   final_theme
# 
# ggplot(subset(buoy2015_vert_updo, subset=(datetime>=as.POSIXct('2015-01-01', tz='UTC') & datetime < as.POSIXct('2015-02-01', tz='UTC'))), aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='January 2015', x='date', y='') +
#   final_theme
# 
# ggplot(subset(buoy2015_vert_updo, subset=(datetime>=as.POSIXct('2015-02-01', tz='UTC') & datetime < as.POSIXct('2015-03-01', tz='UTC'))), aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='February 2015', x='date', y='') +
#   final_theme
# 
# ggplot(subset(buoy2015_vert_updo, subset=(datetime>=as.POSIXct('2015-03-01', tz='UTC') & datetime < as.POSIXct('2015-04-01', tz='UTC'))), aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='March 2015', x='date', y='') +
#   final_theme
# 
# ggplot(subset(buoy2015_vert_updo, subset=(datetime>=as.POSIXct('2015-04-01', tz='UTC') & datetime < as.POSIXct('2015-05-01', tz='UTC'))), aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='April 2015', x='date', y='') +
#   final_theme

#remove artifacts of buoy move/going offline April
buoy2015_L1 <- buoy2015_L1 %>% 
  mutate_at(vars(upDO),
            funs(case_when(datetime>=as.POSIXct('2015-04-22 11:00', tz='UTC') & datetime<=as.POSIXct('2015-04-22 11:10', tz='UTC') ~ NA_real_,
                           TRUE ~ .)))
# buoy2015_vert_updo_L1 <- buoy2015_L1 %>% 
#   select(datetime, upDO) %>% 
#   gather(variable, value, -datetime)
# 
# ggplot(subset(buoy2015_vert_updo_L1, subset=(datetime>=as.POSIXct('2015-04-01', tz='UTC') & datetime < as.POSIXct('2015-05-01', tz='UTC'))), aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='April 2015, v2', x='date', y='') +
#   final_theme
# 
# #data gap april to june
# 
# ggplot(subset(buoy2015_vert_updo, subset=(datetime>=as.POSIXct('2015-06-01', tz='UTC') & datetime < as.POSIXct('2015-07-01', tz='UTC'))), aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='early June 2015', x='date', y='') +
#   final_theme

#remove data from 1st to 3rd, before sensors correctly reporting, then short data gap until the 4th
buoy2015_L1 <- buoy2015_L1 %>% 
  mutate_at(vars(upDO),
            funs(case_when(datetime>=as.POSIXct('2015-06-01', tz='UTC') & datetime<=as.POSIXct('2015-06-04 10:00', tz='UTC') ~ NA_real_,
                           TRUE ~ .)))

# buoy2015_vert_updo_L1 <- buoy2015_L1 %>% 
#   select(datetime, upDO) %>% 
#   gather(variable, value, -datetime)
# 
# ggplot(subset(buoy2015_vert_updo_L1, subset=(datetime>=as.POSIXct('2015-06-01', tz='UTC') & datetime < as.POSIXct('2015-07-01', tz='UTC'))), aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='June 2015, v2', x='date', y='') +
#   final_theme

#remove artifacts of buoy move to loon, assign buoy locatoin to l1 data
buoy2015_L1 <- buoy2015_L1 %>% 
  mutate_at(vars(upDO),
            funs(case_when(datetime>=as.POSIXct('2015-06-11 9:50', tz='UTC') & datetime<=as.POSIXct('2015-06-11 10:00', tz='UTC') ~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate(location = case_when(datetime>=as.POSIXct('2015-01-01', tz='UTC') & datetime<as.POSIXct('2015-06-11 9:50', tz='UTC') ~ 'harbor',
                              TRUE ~ 'deep spot'))

# buoy2015_vert_updo_L1 <- buoy2015_L1 %>% 
#   select(datetime, location, upDO) %>% 
#   gather(variable, value, -datetime, -location)
# 
# ggplot(subset(buoy2015_vert_updo_L1, subset=(datetime>=as.POSIXct('2015-06-01', tz='UTC') & datetime < as.POSIXct('2015-07-01', tz='UTC'))), aes(x=datetime, y=value, shape=location)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='June 2015, v3', x='date', y='') +
#   final_theme
# 
# ggplot(subset(buoy2015_vert_updo, subset=(datetime>=as.POSIXct('2015-07-01', tz='UTC') & datetime < as.POSIXct('2015-08-01', tz='UTC'))), aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='July 2015', x='date', y='') +
#   final_theme
# 
# ggplot(subset(buoy2015_vert_updo, subset=(datetime>=as.POSIXct('2015-08-01', tz='UTC') & datetime < as.POSIXct('2015-09-01', tz='UTC'))), aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='August 2015', x='date', y='') +
#   final_theme
# 
# ggplot(subset(buoy2015_vert_updo, subset=(datetime>=as.POSIXct('2015-09-01', tz='UTC') & datetime < as.POSIXct('2015-10-01', tz='UTC'))), aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='September 2015', x='date', y='') +
#   final_theme

#errant point around the 19th
buoy2015_L1 <- buoy2015_L1 %>% 
  mutate_at(vars(upDO),
            funs(case_when(datetime==as.POSIXct('2015-09-19 5:50', tz='UTC') ~ NA_real_,
                           TRUE ~ .)))
# buoy2015_vert_updo_L1 <- buoy2015_L1 %>% 
#   select(datetime, location, upDO) %>% 
#   gather(variable, value, -datetime, -location)
# 
# ggplot(subset(buoy2015_vert_updo_L1, subset=(datetime>=as.POSIXct('2015-09-01', tz='UTC') & datetime < as.POSIXct('2015-10-01', tz='UTC'))), aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='September 2015, v2', x='date', y='') +
#   final_theme
# 
# 
# ggplot(subset(buoy2015_vert_updo, subset=(datetime>=as.POSIXct('2015-10-01', tz='UTC') & datetime < as.POSIXct('2015-11-01', tz='UTC'))), aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='October 2015', x='date', y='') +
#   final_theme
# 
# 
# ggplot(subset(buoy2015_vert_updo, subset=(datetime>=as.POSIXct('2015-10-08 9:00', tz='UTC') & datetime < as.POSIXct('2015-10-08 15:00', tz='UTC'))), aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='October 2015', x='date', y='') +
#   final_theme


#remove artifacts of buoy movement and data thereafter, update location data
buoy2015_L1 <- buoy2015_L1 %>% 
  mutate_at(vars(upDO),
            funs(case_when(datetime>=as.POSIXct('2015-10-08 10:40', tz='UTC') & datetime<as.POSIXct('2016-01-01', tz='UTC') ~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate(location = case_when(datetime>=as.POSIXct('2015-10-08 11:20', tz='UTC') & buoy2015_L1$datetime<as.POSIXct('2016-01-01', tz='UTC') ~ 'harbor',
                              TRUE ~ location))

# buoy2015_vert_updo_L1 <- buoy2015_L1 %>% 
#   select(datetime, location, upDO) %>% 
#   gather(variable, value, -datetime, -location)
# 
# ggplot(subset(buoy2015_vert_updo_L1, subset=(datetime>=as.POSIXct('2015-10-01', tz='UTC') & datetime < as.POSIXct('2015-11-01', tz='UTC'))), aes(x=datetime, y=value, shape = location)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='October 2015, v2', x='date', y='') +
#   final_theme
# 
# 
# ggplot(subset(buoy2015_vert_updo_L1, subset=(datetime>=as.POSIXct('2015-01-01', tz='UTC') & datetime < as.POSIXct('2016-01-01', tz='UTC'))), aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='2015, clean', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme



#### plot Lower DO ####
#recode NA values to na
buoy2015_L1 <- buoy2015_L1 %>% 
  mutate_at(vars(lowDO),
            funs(case_when(. == -6999 ~ NA_real_,
                           . == 0 ~ NA_real_,
                           TRUE ~ .)))

# buoy2015_vert_lowdo <-  buoy2015_L1 %>% 
#   select(datetime, location, lowDO) %>% 
#   gather(variable, value, -datetime, -location)
# 
# ggplot(subset(buoy2015_vert_lowdo, subset=(datetime>=as.POSIXct('2015-01-01', tz='UTC') & datetime < as.POSIXct('2016-01-01', tz='UTC'))), aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='2015', x='date', y='') +
#   final_theme

# remove data prior to buoy move -- note, that recode time is later because of equilibration of do readings; when in harbor, this would not have been at 10m,
# so all data removed until buoy moved to loon 
buoy2015_L1 <- buoy2015_L1 %>% 
  mutate_at(vars(lowDO),
            funs(case_when(datetime>=as.POSIXct('2015-01-01', tz='UTC') & datetime<as.POSIXct('2015-06-11 11:10', tz='UTC') ~ NA_real_,
                           TRUE ~ .)))
# buoy2015_vert_lowdo_L1 <-  buoy2015_L1 %>% 
#   select(datetime, location, lowDO) %>% 
#   gather(variable, value, -datetime, -location)
# 
# ggplot(subset(buoy2015_vert_lowdo_L1, subset=(datetime>=as.POSIXct('2015-06-01', tz='UTC') & datetime < as.POSIXct('2015-07-01', tz='UTC'))), aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='June 2015', x='date', y='') +
#   final_theme
# 
# ggplot(subset(buoy2015_vert_lowdo, subset=(datetime>=as.POSIXct('2015-07-01', tz='UTC') & datetime < as.POSIXct('2015-08-01', tz='UTC'))), aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='July 2015', x='date', y='') +
#   final_theme
# 
# ggplot(subset(buoy2015_vert_lowdo, subset=(datetime>=as.POSIXct('2015-08-01', tz='UTC') & datetime < as.POSIXct('2015-09-01', tz='UTC'))), aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='August 2015', x='date', y='') +
#   final_theme


#August 13th visit
buoy2015_L1 <-  buoy2015_L1 %>% 
  mutate_at(vars(lowDO),
            funs(case_when(datetime==as.POSIXct('2015-08-13 10:00', tz='UTC') ~ NA_real_,
                           TRUE ~ .)))
# buoy2015_vert_lowdo_L1 <-  buoy2015_L1 %>% 
#   select(datetime, location, lowDO) %>% 
#   gather(variable, value, -datetime, -location)
# 
# ggplot(subset(buoy2015_vert_lowdo_L1, subset=(datetime>=as.POSIXct('2015-08-01', tz='UTC') & datetime < as.POSIXct('2015-09-01', tz='UTC'))), aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='August 2015', x='date', y='') +
#   final_theme
# 
# ggplot(subset(buoy2015_vert_lowdo, subset=(datetime>=as.POSIXct('2015-09-01', tz='UTC') & datetime < as.POSIXct('2015-10-01', tz='UTC'))), aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='September 2015', x='date', y='') +
#   final_theme
# 
# ggplot(subset(buoy2015_vert_lowdo, subset=(datetime>=as.POSIXct('2015-10-01', tz='UTC') & datetime < as.POSIXct('2015-11-01', tz='UTC'))), aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='October 2015', x='date', y='') +
#   final_theme

#remove artifacts of buoy movement and data after move - all errant
buoy2015_L1 <-  buoy2015_L1 %>% 
  mutate_at(vars(lowDO),
            funs(case_when(datetime>=as.POSIXct('2015-10-08 10:40', tz='UTC') & datetime<as.POSIXct('2016-01-01', tz='UTC') ~ NA_real_,
                           TRUE ~ .)))
# buoy2015_vert_lowdo_L1 <-  buoy2015_L1 %>% 
#   select(datetime, location, lowDO) %>% 
#   gather(variable, value, -datetime, -location)
# 
# ggplot(subset(buoy2015_vert_lowdo_L1, subset=(datetime>=as.POSIXct('2015-10-01', tz='UTC') & datetime < as.POSIXct('2015-11-01', tz='UTC'))), aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='October 2015, v2', x='date', y='') +
#   final_theme
#  
# ggplot(subset(buoy2015_vert_lowdo_L1, subset=(datetime>=as.POSIXct('2015-01-01', tz='UTC') & datetime < as.POSIXct('2016-01-01', tz='UTC'))), aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='2015, clean', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme


####PAR ####
#replace negative values with 0
buoy2015_L1 <-  buoy2015_L1 %>% 
  mutate(PAR = case_when(PAR < 0 ~ 0,
                         TRUE ~ PAR))

# ggplot(subset(buoy2015_L1, subset=(datetime>=as.POSIXct('2015-01-01', tz='UTC') & datetime<as.POSIXct('2016-01-01', tz='UTC'))), aes(x=datetime, y=PAR)) +
#   geom_point() + 
#   labs(title='2015, raw', x='date', y='PAR (uE*m-2*s-1)') +
#   final_theme
# 
# #par comes back online June 2015
# 
# ggplot(subset(buoy2015_L1, subset=(datetime>=as.POSIXct('2015-06-01', tz='UTC') & datetime<as.POSIXct('2015-07-01', tz='UTC'))), aes(x=datetime, y=PAR)) +
#   geom_point() + 
#   labs(title='June 2015', x='date', y='PAR (uE*m-2*s-1)') +
#   final_theme
# 
# #need to remove data before PAR functioning correctly
# ggplot(subset(buoy2015_L1, subset=(datetime>=as.POSIXct('2015-06-03', tz='UTC') & datetime<as.POSIXct('2015-06-05', tz='UTC'))), aes(x=datetime, y=PAR)) +
#   geom_point() + 
#   labs(title='early June 2015', x='date', y='PAR (uE*m-2*s-1)') +
#   final_theme

buoy2015_L1 <-  buoy2015_L1 %>% 
  mutate(PAR = case_when(datetime>=as.POSIXct('2015-01-01', tz='UTC') & datetime<=as.POSIXct('2015-06-04', tz='UTC') ~ NA_real_,
                         TRUE ~ PAR))

# ggplot(subset(buoy2015_L1, subset=(datetime>=as.POSIXct('2015-06-01', tz='UTC') & datetime<as.POSIXct('2015-07-01', tz='UTC'))), aes(x=datetime, y=PAR)) +
#   geom_point() + 
#   labs(title='June 2015, v2', x='date', y='PAR (uE*m-2*s-1)') +
#   final_theme

#remove data possibly errant due to buoy move
buoy2015_L1 <-  buoy2015_L1 %>% 
  mutate(PAR = case_when(datetime>=as.POSIXct('2015-06-11 9:50', tz='UTC') & datetime<=as.POSIXct('2015-06-11 10:00', tz='UTC') ~ NA_real_,
                         TRUE ~ PAR))

# ggplot(subset(buoy2015_L1, subset=(datetime>=as.POSIXct('2015-06-01', tz='UTC') & datetime<as.POSIXct('2015-07-01', tz='UTC'))), aes(x=datetime, y=PAR)) +
#   geom_point() + 
#   labs(title='June 2015, v3', x='date', y='PAR (uE*m-2*s-1)') +
#   final_theme
# 
# ggplot(subset(buoy2015_L1, subset=(datetime>=as.POSIXct('2015-07-01', tz='UTC') & datetime<as.POSIXct('2015-08-01', tz='UTC'))), aes(x=datetime, y=PAR)) +
#   geom_point() + 
#   labs(title='July 2015', x='date', y='PAR (uE*m-2*s-1)') +
#   final_theme
# 
# ggplot(subset(buoy2015_L1, subset=(datetime>=as.POSIXct('2015-08-01', tz='UTC') & datetime<as.POSIXct('2015-09-01', tz='UTC'))), aes(x=datetime, y=PAR)) +
#   geom_point() + 
#   labs(title='August 2015', x='date', y='PAR (uE*m-2*s-1)') +
#   final_theme
# 
# ggplot(subset(buoy2015_L1, subset=(datetime>=as.POSIXct('2015-09-01', tz='UTC') & datetime<as.POSIXct('2015-10-01', tz='UTC'))), aes(x=datetime, y=PAR)) +
#   geom_point() + 
#   labs(title='Sept 2015', x='date', y='PAR (uE*m-2*s-1)') +
#   final_theme
# 
# ggplot(subset(buoy2015_L1, subset=(datetime>=as.POSIXct('2015-10-01', tz='UTC') & datetime<as.POSIXct('2015-11-01', tz='UTC'))), aes(x=datetime, y=PAR)) +
#   geom_point() + 
#   labs(title='Oct 2015', x='date', y='PAR (uE*m-2*s-1)') +
#   final_theme

#remove possibly errant data during buoy move
buoy2015_L1 <-  buoy2015_L1 %>% 
  mutate(PAR = case_when(datetime>=as.POSIXct('2015-10-08 10:40', tz='UTC') & datetime<=as.POSIXct('2015-10-08 11:00', tz='UTC') ~ NA_real_,
                         TRUE ~ PAR))

# ggplot(subset(buoy2015_L1, subset=(datetime>=as.POSIXct('2015-10-01', tz='UTC') & datetime<as.POSIXct('2015-11-01', tz='UTC'))), aes(x=datetime, y=PAR)) +
#   geom_point() + 
#   labs(title='Oct 2015, v2', x='date', y='PAR (uE*m-2*s-1)') +
#   final_theme
# 
# ggplot(subset(buoy2015_L1, subset=(datetime>=as.POSIXct('2015-11-01', tz='UTC') & datetime<as.POSIXct('2015-12-01', tz='UTC'))), aes(x=datetime, y=PAR)) +
#   geom_point() + 
#   labs(title='November 2015', x='date', y='PAR (uE*m-2*s-1)') +
#   final_theme
# 
# ggplot(subset(buoy2015_L1, subset=(datetime>=as.POSIXct('2015-12-01', tz='UTC') & datetime<as.POSIXct('2016-01-01', tz='UTC'))), aes(x=datetime, y=PAR)) +
#   geom_point() + 
#   labs(title='December 2015', x='date', y='PAR (uE*m-2*s-1)') +
#   final_theme
# 
# ggplot(subset(buoy2015_L1, subset=(datetime>=as.POSIXct('2015-01-01', tz='UTC') & datetime<as.POSIXct('2016-01-01', tz='UTC'))), aes(x=datetime, y=PAR, color=location)) +
#   geom_point() + 
#   labs(title='2015, clean', x='date', y='PAR (uE*m-2*s-1)') +
#   final_theme


####Wind data ####
# buoy2015_vert_wind <- buoy2015_L1 %>% 
#   select(datetime, wind) %>% 
#   gather(variable, value, -datetime)
# 
# ggplot(subset(buoy2015_vert_wind, subset=(datetime>=as.POSIXct('2015-01-01', tz='UTC') & datetime<as.POSIXct('2016-01-01', tz='UTC'))), aes(x=datetime, y=value, color=variable)) +
#   geom_point() + 
#   facet_grid(variable~., scales='free_y') +
#   labs(title='2015, raw', x='date', y=' ') +
#   final_theme
# 
# ggplot(subset(buoy2015_vert_wind, subset=(datetime>=as.POSIXct('2015-01-01', tz='UTC') & datetime<as.POSIXct('2015-02-01', tz='UTC'))), aes(x=datetime, y=value, color=variable)) +
#   geom_point() +   
#   facet_grid(variable~., scales='free_y') +
#   labs(title='January 2015', x='date', y=' ') +
#   final_theme
# 
# ggplot(subset(buoy2015_vert_wind, subset=(datetime>=as.POSIXct('2015-02-01', tz='UTC') & datetime<as.POSIXct('2015-03-01', tz='UTC'))), aes(x=datetime, y=value, color=variable)) +
#   geom_point() + 
#   facet_grid(variable~., scales='free_y') +
#   labs(title='February 2015', x='date', y=' ') +
#   final_theme
# 
# ## flat lines on feb 15 and 19
# 
# ggplot(subset(buoy2015_vert_wind, subset=(datetime>=as.POSIXct('2015-02-15', tz='UTC') & datetime<as.POSIXct('2015-02-16', tz='UTC'))), aes(x=datetime, y=value, color=variable)) +
#   geom_point() + 
#   facet_grid(variable~., scales='free_y') +
#   labs(title='late February 2015', x='date', y=' ') +
#   final_theme
#   
# ggplot(subset(buoy2015_vert_wind, subset=(datetime>=as.POSIXct('2015-02-18 18:00', tz='UTC') & datetime<as.POSIXct('2015-02-19 12:00', tz='UTC'))), aes(x=datetime, y=value, color=variable)) +
#   geom_point() + 
#   facet_grid(variable~., scales='free_y') +
#   labs(title='late February 2015', x='date', y=' ') +
#   final_theme

buoy2015_L1 <- buoy2015_L1 %>% 
  mutate_at(vars(wind),
            funs(case_when(datetime>=as.POSIXct('2015-02-15 00:40', tz='UTC') & datetime<=as.POSIXct('2015-02-15 14:00', tz='UTC') ~ NA_real_,
                           datetime>=as.POSIXct('2015-02-18 22:40', tz='UTC') & datetime<=as.POSIXct('2015-02-19 10:50', tz='UTC') ~ NA_real_,
                           TRUE ~ .)))

# buoy2015_vert_wind_L1 <- buoy2015_L1 %>% 
#   select(datetime, wind) %>% 
#   gather(variable, value, -datetime)
# 
# ggplot(subset(buoy2015_vert_wind_L1, subset=(datetime>=as.POSIXct('2015-02-01', tz='UTC') & datetime<as.POSIXct('2015-03-01', tz='UTC'))), aes(x=datetime, y=value, color=variable)) +
#   geom_point() + 
#   facet_grid(variable~., scales='free_y') +
#   labs(title='February 2015, v2', x='date', y=' ') +
#   final_theme
# 
# ggplot(subset(buoy2015_vert_wind, subset=(datetime>=as.POSIXct('2015-03-01', tz='UTC') & datetime<as.POSIXct('2015-04-01', tz='UTC'))), aes(x=datetime, y=value, color=variable)) +
#   geom_point() + 
#   facet_grid(variable~., scales='free_y') +
#   labs(title='March 2015', x='date', y=' ') +
#   final_theme
# 
# ggplot(subset(buoy2015_vert_wind, subset=(datetime>=as.POSIXct('2015-04-01', tz='UTC') & datetime<as.POSIXct('2015-05-01', tz='UTC'))), aes(x=datetime, y=value, color=variable)) +
#   geom_point() + 
#   facet_grid(variable~., scales='free_y') +
#   labs(title='April 2015', x='date', y=' ') +
#   final_theme
# 
# #data gap until June
# 
# ggplot(subset(buoy2015_vert_wind, subset=(datetime>=as.POSIXct('2015-06-01', tz='UTC') & datetime<as.POSIXct('2015-07-01', tz='UTC'))), aes(x=datetime, y=value, color=variable)) +
#   geom_point() + 
#   facet_grid(variable~., scales='free_y') +
#   labs(title='June 2015', x='date', y=' ') +
#   final_theme
# 
# #errant data beginning of June and possibly errant data during buoy move
# ggplot(subset(buoy2015_vert_wind, subset=(datetime>=as.POSIXct('2015-06-01', tz='UTC') & datetime<as.POSIXct('2015-06-05', tz='UTC'))), aes(x=datetime, y=value, color=variable)) +
#   geom_point() + 
#   facet_grid(variable~., scales='free_y') +
#   labs(title='early June 2015', x='date', y=' ') +
#   final_theme

buoy2015_L1 <- buoy2015_L1 %>% 
  mutate_at(vars(wind),
            funs(case_when(datetime>=as.POSIXct('2015-06-01 00:00', tz='UTC') & datetime<=as.POSIXct('2015-06-04 10:00', tz='UTC') ~ NA_real_,
                           datetime>=as.POSIXct('2015-06-11 9:50', tz='UTC') & datetime<=as.POSIXct('2015-06-11 10:00', tz='UTC') ~ NA_real_,
                           TRUE ~ .)))

# buoy2015_vert_wind_L1 <- buoy2015_L1 %>% 
#   select(datetime, wind) %>% 
#   gather(variable, value, -datetime)
# 
# ggplot(subset(buoy2015_vert_wind_L1, subset=(datetime>=as.POSIXct('2015-06-01', tz='UTC') & datetime<as.POSIXct('2015-06-16', tz='UTC'))), aes(x=datetime, y=value, color=variable)) +
#   geom_point() + 
#   facet_grid(variable~., scales='free_y') +
#   labs(title='early June 2015, v2', x='date', y=' ') +
#   final_theme
# 
# ggplot(subset(buoy2015_vert_wind, subset=(datetime>=as.POSIXct('2015-07-01', tz='UTC') & datetime<as.POSIXct('2015-08-01', tz='UTC'))), aes(x=datetime, y=value, color=variable)) +
#   geom_point() + 
#   facet_grid(variable~., scales='free_y') +
#   labs(title='July 2015', x='date', y=' ') +
#   final_theme
# 
# ggplot(subset(buoy2015_vert_wind, subset=(datetime>=as.POSIXct('2015-08-01', tz='UTC') & datetime<as.POSIXct('2015-09-01', tz='UTC'))), aes(x=datetime, y=value, color=variable)) +
#   geom_point() + 
#   facet_grid(variable~., scales='free_y') +
#   labs(title='August 2015', x='date', y=' ') +
#   final_theme
# 
# ggplot(subset(buoy2015_vert_wind, subset=(datetime>=as.POSIXct('2015-09-01', tz='UTC') & datetime<as.POSIXct('2015-10-01', tz='UTC'))), aes(x=datetime, y=value, color=variable)) +
#   geom_point() + 
#   facet_grid(variable~., scales='free_y') +
#   labs(title='September 2015', x='date', y=' ') +
#   final_theme
# 
# ggplot(subset(buoy2015_vert_wind, subset=(datetime>=as.POSIXct('2015-10-01', tz='UTC') & datetime<as.POSIXct('2015-11-01', tz='UTC'))), aes(x=datetime, y=value, color=variable)) +
#   geom_point() + 
#   facet_grid(variable~., scales='free_y') +
#   labs(title='October 2015', x='date', y=' ') +
#   final_theme

#remove possibly errant data during buoy move
buoy2015_L1 <-  buoy2015_L1 %>%
  mutate_at(vars(wind),
            funs(case_when(datetime>=as.POSIXct('2015-10-08 10:40', tz='UTC') & datetime<=as.POSIXct('2015-10-08 11:00', tz='UTC') ~ NA_real_,
                           TRUE ~ .)))

# buoy2015_vert_wind_L1 <- buoy2015_L1 %>% 
#   select(datetime, wind) %>% 
#   gather(variable, value, -datetime)
# 
# ggplot(subset(buoy2015_vert_wind, subset=(datetime>=as.POSIXct('2015-11-01', tz='UTC') & datetime<as.POSIXct('2015-12-01', tz='UTC'))), aes(x=datetime, y=value, color=variable)) +
#   geom_point() + 
#   facet_grid(variable~., scales='free_y') +
#   labs(title='November 2015', x='date', y=' ') +
#   final_theme
# 
# ggplot(subset(buoy2015_vert_wind, subset=(datetime>=as.POSIXct('2015-12-01', tz='UTC') & datetime<as.POSIXct('2016-01-01', tz='UTC'))), aes(x=datetime, y=value, color=variable)) +
#   geom_point() + 
#   facet_grid(variable~., scales='free_y') +
#   labs(title='December 2015', x='date', y=' ') +
#   final_theme
# 
# #errant wind data 30-31
# ggplot(subset(buoy2015_vert_wind, subset=(datetime>=as.POSIXct('2015-12-29 12:00', tz='UTC') & datetime<as.POSIXct('2016-01-01', tz='UTC'))), aes(x=datetime, y=value, color=variable)) +
#   geom_point() + 
#   facet_grid(variable~., scales='free_y') +
#   labs(title='late December 2015', x='date', y=' ') +
#   final_theme
# ggplot(subset(buoy2015_vert_wind, subset=(datetime>=as.POSIXct('2015-12-29 12:00', tz='UTC') & datetime<as.POSIXct('2015-12-30', tz='UTC'))), aes(x=datetime, y=value, color=variable)) +
#   geom_point() + 
#   facet_grid(variable~., scales='free_y') +
#   labs(title='late December 2015', x='date', y=' ') +
#   final_theme
# ggplot(subset(buoy2015_vert_wind, subset=(datetime>=as.POSIXct('2015-12-31', tz='UTC') & datetime<as.POSIXct('2016-01-01', tz='UTC'))), aes(x=datetime, y=value, color=variable)) +
#   geom_point() + 
#   facet_grid(variable~., scales='free_y') +
#   labs(title='late December 2015', x='date', y=' ') +
#   final_theme

buoy2015_L1 <- buoy2015_L1 %>% 
  mutate_at(vars(wind),
            funs(case_when(datetime>=as.POSIXct('2015-12-29 17:30', tz='UTC') & datetime<=as.POSIXct('2015-12-31 7:10', tz='UTC') ~ NA_real_,
                           TRUE ~ .)))

# buoy2015_vert_wind_L1 <- buoy2015_L1 %>% 
#   select(datetime, location, wind) %>% 
#   gather(variable, value, -datetime, -location)
# 
# ggplot(subset(buoy2015_vert_wind_L1, subset=(datetime>=as.POSIXct('2015-12-01', tz='UTC') & datetime<as.POSIXct('2016-01-01', tz='UTC'))), aes(x=datetime, y=value, color=variable)) +
#   geom_point() + 
#   facet_grid(variable~., scales='free_y') +
#   labs(title='December 2015, v2', x='date', y=' ') +
#   final_theme
# 
# ggplot(subset(buoy2015_vert_wind_L1, subset=(datetime>=as.POSIXct('2015-01-01', tz='UTC') & datetime<as.POSIXct('2016-01-01', tz='UTC'))), aes(x=datetime, y=value, color=location)) +
#   geom_point() + 
#   facet_grid(variable~., scales='free_y') +
#   labs(title='2015, clean', x='date', y=' ') +
#   final_theme


####chla data ####
# buoy2015_vert_chla <- buoy2015_L1 %>% 
#   select(datetime, chla) %>% 
#   gather(variable, value, -datetime)
# 
# ggplot(subset(buoy2015_vert_chla, subset=(datetime>=as.POSIXct('2015-01-01', tz='UTC') & datetime<as.POSIXct('2016-01-01', tz='UTC'))), aes(x=datetime, y=value, color=variable)) +
#   geom_point() + 
#   facet_grid(variable~., scales='free_y') +
#   labs(title='2015, raw', x='date', y=' ') +
#   final_theme

#recode NA strings to NA
buoy2015_L1 <- buoy2015_L1 %>% 
  mutate_at(vars(chla),
            funs(case_when(. == -6999 ~ NA_real_,
                           . == -3.3 ~ NA_real_,
                           . == 183 ~ NA_real_,
                           . == 6999 ~ NA_real_,
                           TRUE ~ .)))

# buoy2015_vert_chla <- buoy2015_L1 %>% 
#   select(datetime, chla) %>% 
#   gather(variable, value, -datetime)
# 
# ggplot(subset(buoy2015_vert_chla, subset=(datetime>=as.POSIXct('2015-01-01', tz='UTC') & datetime<as.POSIXct('2016-01-01', tz='UTC'))), aes(x=datetime, y=value, color=variable)) +
#   geom_point() + 
#   facet_grid(variable~., scales='free_y') +
#   labs(title='2015, raw', x='date', y=' ') +
#   final_theme

#SpecCond is out of range (should be more like 100, not 150) may be miscalibrated or measuring something different, remove for entire record for now
buoy2015_L1 <- buoy2015_L1 %>% 
  mutate(flag = 'SpecCond out of range measured by LMP')

# #RFU is relatively meaning less (except to plot agains chla)
# ggplot(buoy2015_L1, aes(x=Chlor_RFU, y=Chlor_UGL)) +
#   geom_point()
# #apply any recoding of ugl to rfu
# 
# #look at chl-ugl against measured in lmp
# LMP2015_chla <- LMP2015_bio %>% 
#   mutate(datetime = as.POSIXct(paste(DATE, '12:00', sep=' '), tz='UTC'),
#          source = 'LMP') %>% 
#   select(datetime, source, CHL) %>% 
#   rename(chla_ugl = 'CHL')
# 
# buoy_chla_LMP_vert <- buoy2015_L1 %>% 
#   select(datetime, Chlor_UGL) %>% 
#   mutate(source = 'buoy') %>% 
#   rename(chla_ugl = 'Chlor_UGL') %>% 
#   full_join(., LMP2015_chla) %>% 
#   gather(variable, value, -datetime, -source)
# 
# ggplot(subset(buoy_chla_LMP_vert, subset=(datetime>=as.POSIXct('2015-01-01', tz='UTC') & datetime<as.POSIXct('2016-01-01', tz='UTC'))), aes(x=datetime, y=value, color=source)) +
#   geom_point() +
#   labs(title='2015, raw', x='date', y=' ') +
#   final_theme
# 
# #June 22
# ggplot(subset(buoy_chla_LMP_vert, subset=(datetime>=as.POSIXct('2015-06-21', tz='UTC') & datetime<as.POSIXct('2015-06-24', tz='UTC'))), aes(x=datetime, y=value, color=source)) +
#   geom_point() +
#   labs(title='2015, raw', x='date', y=' ') +
#   final_theme
# 
# #July 20
# ggplot(subset(buoy_chla_LMP_vert, subset=(datetime>=as.POSIXct('2015-07-19', tz='UTC') & datetime<as.POSIXct('2015-07-22', tz='UTC'))), aes(x=datetime, y=value, color=source)) +
#   geom_point() +
#   labs(title='2015, raw', x='date', y=' ') +
#   final_theme
# 
# #Aug 10
# ggplot(subset(buoy_chla_LMP_vert, subset=(datetime>=as.POSIXct('2015-08-09', tz='UTC') & datetime<as.POSIXct('2015-08-12', tz='UTC'))), aes(x=datetime, y=value, color=source)) +
#   geom_point() +
#   labs(title='2015, raw', x='date', y=' ') +
#   final_theme
# 
# #Sept 21
# ggplot(subset(buoy_chla_LMP_vert, subset=(datetime>=as.POSIXct('2015-09-20', tz='UTC') & datetime<as.POSIXct('2015-09-22', tz='UTC'))), aes(x=datetime, y=value, color=source)) +
#   geom_point() +
#   labs(title='2015, raw', x='date', y=' ') +
#   final_theme
# 
# #clean chla data
# ggplot(subset(buoy2015_L1, subset=(datetime>=as.POSIXct('2015-01-01', tz='UTC') & datetime<as.POSIXct('2016-01-01', tz='UTC'))), aes(x=datetime, y=Chlor_UGL)) +
#   geom_point() +
#   labs(title='2015, raw', x='date', y=' ') +
#   final_theme
# 
# ggplot(subset(buoy2015_L1, subset=(datetime>=as.POSIXct('2015-06-01', tz='UTC') & datetime<as.POSIXct('2015-07-01', tz='UTC'))), aes(x=datetime, y=Chlor_UGL)) +
#   geom_point() +
#   labs(title='June 2015', x='date', y=' ') +
#   final_theme

#remove data before launch at loon
buoy2015_L1 <- buoy2015_L1 %>% 
  mutate_at(vars(chla),
            funs(case_when(datetime>=as.POSIXct('2015-06-01 00:00', tz='UTC') & datetime<=as.POSIXct('2015-06-04 10:00', tz='UTC') ~ NA_real_,
                           TRUE ~ .)))

# ggplot(subset(buoy2015_L1, subset=(datetime>=as.POSIXct('2015-06-01', tz='UTC') & datetime<as.POSIXct('2015-07-01', tz='UTC'))), aes(x=datetime, y=Chlor_UGL)) +
#   geom_point() +
#   labs(title='June 2015, v2', x='date', y=' ') +
#   final_theme
# 
# ggplot(subset(buoy2015_L1, subset=(datetime>=as.POSIXct('2015-07-01', tz='UTC') & datetime<as.POSIXct('2015-08-01', tz='UTC'))), aes(x=datetime, y=Chlor_UGL)) +
#   geom_point() +
#   labs(title='July 2015', x='date', y=' ') +
#   final_theme
# 
# #July 9 temp string disturbance
# ggplot(subset(buoy2015_L1, subset=(datetime>=as.POSIXct('2015-07-08', tz='UTC') & datetime<as.POSIXct('2015-07-11', tz='UTC'))), aes(x=datetime, y=Chlor_UGL)) +
#   geom_point() +
#   labs(title='July 2015', x='date', y=' ') +
#   final_theme
# 
# #July 22 temp string disturbance
# ggplot(subset(buoy2015_L1, subset=(datetime>=as.POSIXct('2015-07-21', tz='UTC') & datetime<as.POSIXct('2015-07-24', tz='UTC'))), aes(x=datetime, y=Chlor_UGL)) +
#   geom_point() +
#   labs(title='July 2015', x='date', y=' ') +
#   final_theme
# 
# ggplot(subset(buoy2015_L1, subset=(datetime>=as.POSIXct('2015-07-22 12:00', tz='UTC') & datetime<as.POSIXct('2015-07-22 14:00', tz='UTC'))), aes(x=datetime, y=Chlor_UGL)) +
#   geom_point() +
#   labs(title='July 2015', x='date', y=' ') +
#   final_theme
# 
# ggplot(subset(buoy2015_L1, subset=(datetime>=as.POSIXct('2015-08-01', tz='UTC') & datetime<as.POSIXct('2015-09-01', tz='UTC'))), aes(x=datetime, y=Chlor_UGL)) +
#   geom_point() +
#   labs(title='August 2015', x='date', y=' ') +
#   final_theme
# 
# #August 7
# ggplot(subset(buoy2015_L1, subset=(datetime>=as.POSIXct('2015-08-07', tz='UTC') & datetime<as.POSIXct('2015-08-08', tz='UTC'))), aes(x=datetime, y=Chlor_UGL)) +
#   geom_point() +
#   labs(title='August 2015', x='date', y=' ') +
#   final_theme
# 
# 
# #sensor fails after Aug buoy visit 
# ggplot(subset(buoy2015_L1, subset=(datetime>=as.POSIXct('2015-08-13 6:00', tz='UTC') & datetime<as.POSIXct('2015-08-13 12:00', tz='UTC'))), aes(x=datetime, y=Chlor_UGL)) +
#   geom_point() +
#   labs(title='August 2015', x='date', y=' ') +
#   final_theme

#remove possibly errant data during buoy move
buoy2015_L1 <-  buoy2015_L1 %>%
  mutate_at(vars(chla),
            funs(case_when(datetime>=as.POSIXct('2015-08-13 10:00', tz='UTC') & datetime<=as.POSIXct('2016-01-01', tz='UTC') ~ NA_real_,
                           TRUE ~ .)))

# ggplot(subset(buoy2015_L1, subset=(datetime>=as.POSIXct('2015-06-01', tz='UTC') & datetime<as.POSIXct('2015-06-15', tz='UTC'))), aes(x=datetime, y=Chlor_UGL)) +
#   geom_point() +
#   geom_point(aes(x=datetime, y=PAR/500), color='blue') +
#   labs(title=' 2015', x='date', y=' ') +
#   final_theme
# 
# ggplot(subset(buoy2015_L1, subset=(datetime>=as.POSIXct('2015-06-15', tz='UTC') & datetime<as.POSIXct('2015-07-01', tz='UTC'))), aes(x=datetime, y=Chlor_UGL)) +
#   geom_point() +
#   geom_point(aes(x=datetime, y=PAR/500), color='blue') +
#   labs(title=' 2015', x='date', y=' ') +
#   final_theme
#   
# ggplot(subset(buoy2015_L1, subset=(datetime>=as.POSIXct('2015-07-01', tz='UTC') & datetime<as.POSIXct('2015-07-15', tz='UTC'))), aes(x=datetime, y=Chlor_UGL)) +
#   geom_point() +
#   geom_point(aes(x=datetime, y=PAR/500), color='blue') +
#   labs(title=' 2015', x='date', y=' ') +
#   final_theme
# 
# ggplot(subset(buoy2015_L1, subset=(datetime>=as.POSIXct('2015-07-15', tz='UTC') & datetime<as.POSIXct('2015-08-01', tz='UTC'))), aes(x=datetime, y=Chlor_UGL)) +
#   geom_line() +
#   geom_line(aes(x=datetime, y=PAR/100), color='blue') +
#   labs(title=' 2015', x='date', y=' ') +
#   final_theme
#  
# #does not consistently cycle with PAR - does look like many times spikes come in early morn hours before sunrise and increase in PAR  

#### air temp and humidity ####
air_vert <- buoy2015_L1 %>% 
  select(datetime, air) %>% 
  gather(variable, value, -datetime)

ggplot(subset(air_vert, subset=(datetime>=as.POSIXct('2015-01-01', tz='UTC') & datetime<as.POSIXct('2016-01-01', tz='UTC'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title = '2015', x='date', y=' ') +
  final_theme

#no NA values to recode

#values above 98 +/- 5% is errant - sensor is only able to accurately measure RH up to 98%
buoy2015_L1 <- buoy2015_L1 %>% 
  mutate(RelHum = case_when(RelHum > 103 ~ NA_real_,
                           TRUE ~ RelHum))
air_vert <- buoy2015_L1 %>% 
  select(datetime, air) %>% 
  gather(variable, value, -datetime)
ggplot(subset(air_vert, subset=(datetime>=as.POSIXct('2015-01-01', tz='UTC') & datetime<as.POSIXct('2016-01-01', tz='UTC'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title = '2015', x='date', y=' ') +
  final_theme

#clean data#
ggplot(subset(air_vert, subset=(datetime>=as.POSIXct('2015-01-01', tz='UTC') & datetime<as.POSIXct('2015-02-01', tz='UTC'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title = 'Jan 2015', x='date', y=' ') +
  final_theme

ggplot(subset(air_vert, subset=(datetime>=as.POSIXct('2015-02-01', tz='UTC') & datetime<as.POSIXct('2015-03-01', tz='UTC'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title = 'Feb 2015', x='date', y=' ') +
  final_theme

ggplot(subset(air_vert, subset=(datetime>=as.POSIXct('2015-03-01', tz='UTC') & datetime<as.POSIXct('2015-04-01', tz='UTC'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title = 'Mar 2015', x='date', y=' ') +
  final_theme

ggplot(subset(air_vert, subset=(datetime>=as.POSIXct('2015-04-01', tz='UTC') & datetime<as.POSIXct('2015-05-01', tz='UTC'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title = 'Apr 2015', x='date', y=' ') +
  final_theme

#data gap

ggplot(subset(air_vert, subset=(datetime>=as.POSIXct('2015-06-01', tz='UTC') & datetime<as.POSIXct('2015-07-01', tz='UTC'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title = 'June 2015', x='date', y=' ') +
  final_theme

#remove data prior to consistent reporting
buoy2015_L1 <- buoy2015_L1 %>% 
  mutate_at(vars(air),
            funs(case_when(datetime>=as.POSIXct('2015-06-01 00:00', tz='UTC') & datetime<=as.POSIXct('2015-06-04 10:00', tz='UTC') ~ NA_real_,
                           TRUE ~ .)))

air_vert_L1 <- buoy2015_L1 %>% 
  select(datetime, air) %>% 
  gather(variable, value, -datetime)

ggplot(subset(air_vert_L1, subset=(datetime>=as.POSIXct('2015-06-01', tz='UTC') & datetime<as.POSIXct('2015-07-01', tz='UTC'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title = 'June 2015, v2', x='date', y=' ') +
  final_theme


ggplot(subset(air_vert, subset=(datetime>=as.POSIXct('2015-07-01', tz='UTC') & datetime<as.POSIXct('2015-08-01', tz='UTC'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title = 'Jul 2015', x='date', y=' ') +
  final_theme

ggplot(subset(air_vert, subset=(datetime>=as.POSIXct('2015-08-01', tz='UTC') & datetime<as.POSIXct('2015-09-01', tz='UTC'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title = 'Aug 2015', x='date', y=' ') +
  final_theme

ggplot(subset(air_vert, subset=(datetime>=as.POSIXct('2015-09-01', tz='UTC') & datetime<as.POSIXct('2015-10-01', tz='UTC'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title = 'Sept 2015', x='date', y=' ') +
  final_theme

ggplot(subset(air_vert, subset=(datetime>=as.POSIXct('2015-10-01', tz='UTC') & datetime<as.POSIXct('2015-11-01', tz='UTC'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title = 'Oct 2015', x='date', y=' ') +
  final_theme

ggplot(subset(air_vert, subset=(datetime>=as.POSIXct('2015-11-01', tz='UTC') & datetime<as.POSIXct('2015-12-01', tz='UTC'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title = 'Nov 2015', x='date', y=' ') +
  final_theme

ggplot(subset(air_vert, subset=(datetime>=as.POSIXct('2015-12-01', tz='UTC') & datetime<as.POSIXct('2016-01-01', tz='UTC'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title = 'Dec 2015', x='date', y=' ') +
  final_theme



####harmonize and select desired variables ####

buoy2015_L1 <- buoy2015_L1 %>% 
  mutate(flag = paste(flag, ', Chlor_UGL is questionable', sep='')) %>% 
  select(-source)

buoy_hobo2015_L1 <- hobo2015 %>% 
  select(datetime, location, TempC_1m_hobo, TempC_2m_hobo, TempC_3m_hobo, TempC_4m_hobo, TempC_5m_hobo, TempC_6m_hobo, TempC_7m_hobo, TempC_8m_hobo, TempC_9m_hobo) %>% 
  full_join(buoy2015_L1, .) %>% 
  select(-c(ArrayID, Year, Day, date, LoggerBatV, RadioBatV, IntLgBxTempC)) %>% 
  select(datetime, location, alltemp, hobotemp, upDO, lowDO, chla, allmet, flag)

write_csv(buoy_hobo2015_L1, 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/buoy2015_L1_10Jan2018.csv')
