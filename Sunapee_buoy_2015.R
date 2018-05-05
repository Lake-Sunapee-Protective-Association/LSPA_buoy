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
buoy2015_L0 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L0/2015 Buoy Data.csv',
                        col_types = 'iiiinnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnn')

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
# LMP2015_bio <- read_xlsx('C:/Users/steeleb/Dropbox/Lake Sunapee/long term Sunapee data/raw data/LMP files/LMP 2017/BIOLOGY.xlsx', 
#                      sheet='BIOLOGY',
#                      col_types = c('text', 'text', 'numeric', 'guess', 'numeric', 
#                                    'numeric', 'text', 'numeric', 'text' ,'numeric',
#                                    'text', 'numeric', 'numeric', 'numeric', 'text',
#                                    'text', 'text', 'text')) %>% 
#   mutate(DATE = as.Date(DATE, format='%Y-%m-%d')) %>%  #format date
#   filter(DATE >= '2015-01-01' & DATE < '2016-01-01', #filter for 2015 only
#          STATION == 210) 


#### format data ####
buoy2015_L0 <- buoy2015_L0 %>%
  rename(Hr.Min = 'Hr/Min',
         DOLowTempC = 'DOLoTempC',
         InstWindSp = 'WindSpd',
         InstWindDir = 'CorrWind',
         AveWindSp = 'WindSpdAv',
         AveWindDir = 'WindVect',
         MaxWindSp = 'MaxWind') %>% 
  mutate(hour = Hr.Min%/%100,
         minutes = Hr.Min%%100,
         time = paste(hour, minutes, sep=':')) %>% #break out time from Hr.Min, create time column
  mutate(date = as.Date(paste(Day, Year, sep = '-'), format='%j-%Y'), #create date in ymd format
         datetime = as.POSIXct(paste(date, time, sep=' '), format='%Y-%m-%d %H:%M', tz='UTC')) %>%  #tibble forces to UTC, so coerce for our uses.
  select(-hour, -minutes, -Hr.Min, -time, TempC_10m) #remove unnecessary columns -- 10m not in use


# # format time in LMP data
# LMP2015 <- LMP2015 %>% 
#   mutate(hour = TIME%/%100,
#          minutes = TIME%%100,
#          hour = replace(hour, hour==1, 13), #force data in 24h time
#          time = paste(hour, minutes, sep=':')) %>% #break out time from TIME, create time column
#   mutate(datetime = as.POSIXct(paste(DATE, time, sep=' '), format='%Y-%m-%d %H:%M', tz='UTC')) %>%  #tibble forces to UTC, so coerce for our uses.
#   select(-hour, -minutes, -TIME, -time) #remove unnecessary columns


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
# 
# LMP2015_upDO <- LMP2015 %>% 
#   select(datetime, DEPTH, DO, PCNTSAT) %>% 
#   filter(DEPTH <= 2) %>% 
#   rename(depth = 'DEPTH', 
#          DOppm = 'DO', 
#          DOSat = 'PCNTSAT') %>% 
#   gather(variable, value, -datetime, -depth) %>% 
#   mutate(source = 'LMP')
# 
# LMP2015_lowDO <- LMP2015 %>% 
#   select(datetime, DEPTH, DO, PCNTSAT) %>% 
#   filter(DEPTH >= 2) %>% 
#   rename(depth = 'DEPTH', 
#          DOLowPPM = 'DO', 
#          DOLowSat = 'PCNTSAT') %>% 
#   gather(variable, value, -datetime, -depth) %>% 
#   mutate(source = 'LMP')


#add all dates/times to record
alltimes_2015 <- as.data.frame(seq.POSIXt(as.POSIXct('2015-01-01 00:00', tz='UTC'), as.POSIXct('2015-12-31 23:50', tz='UTC'), '10 min')) %>% 
  rename("datetime" = !!names(.[1]))

buoy2015_L1 <- buoy2015_L0 %>% 
  right_join(., alltimes_2015) %>% 
  arrange(datetime)

#####2015 thermisters - remove/replace NA values ####
buoy2015_vert_temp <- buoy2015_L1 %>%
  select(datetime, TempC_0m, TempC_1m, TempC_2m, TempC_3m, TempC_4m, TempC_5m, TempC_6m, TempC_7m, TempC_8m, TempC_9m) %>%
  gather(variable, value, -datetime)

# ggplot(buoy2015_vert_temp, 
#        aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='2015, raw', 
#        x=NULL,
#        y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme

buoy2015_L1 <- buoy2015_L1 %>% 
  mutate_at(vars(TempC_0m:TempC_9m),
            funs(case_when(. == -6999 ~ NA_real_,
                           . == 1215 ~ NA_real_,
                           . == 555.4 ~ NA_real_,
                           . == 0 ~ NA_real_,
                           TRUE ~ .)))

buoy2015_vert_temp <- buoy2015_L1 %>%
  select(datetime, TempC_0m, TempC_1m, TempC_2m, TempC_3m, TempC_4m, TempC_5m, TempC_6m, TempC_7m, TempC_8m, TempC_9m) %>%
  gather(variable, value, -datetime)

ggplot(buoy2015_vert_temp,
       aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='2015, NAs recoded',
       x=NULL,
       y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 month')

# remove 0m completely
buoy2015_L1 <- buoy2015_L1 %>% 
  mutate(TempC_0m = NA_real_)

buoy2015_vert_temp <- buoy2015_L1 %>%
  select(datetime, TempC_0m, TempC_1m, TempC_2m, TempC_3m, TempC_4m, TempC_5m, TempC_6m, TempC_7m, TempC_8m, TempC_9m) %>%
  gather(variable, value, -datetime)

# ggplot(subset(buoy2015_vert_temp, 
#               subset=(datetime >= as.POSIXct('2015-06-01', tz='UTC') & datetime < as.POSIXct('2015-07-01', tz='UTC'))),
#        aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='jun 2015, NAs recoded', 
#        x=NULL,
#        y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme+
#     scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2015_vert_temp, 
#               subset=(datetime >= as.POSIXct('2015-06-11', tz='UTC') & datetime < as.POSIXct('2015-06-12', tz='UTC'))),
#        aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='jun 2015, NAs recoded', 
#        x=NULL,
#        y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme+
#   scale_x_datetime(date_minor_breaks = '1 hour')

buoy2015_L1 <- buoy2015_L1 %>% 
  mutate_at(vars(alltemp2011),
            funs(case_when(datetime>=as.POSIXct('2015-06-01', tz='UTC') & datetime<=as.POSIXct('2015-06-11 10:00', tz='UTC') ~ NA_real_,
                           TRUE ~ .))) 

buoy2015_vert_temp_L1 <- buoy2015_L1 %>%
  select(datetime, alltemp2011) %>%
  gather(variable, value, -datetime)

# ggplot(subset(buoy2015_vert_temp_L1, 
#               subset=(datetime >= as.POSIXct('2015-06-01', tz='UTC') & datetime < as.POSIXct('2015-07-01', tz='UTC'))),
#        aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='jun 2015, clean', 
#        x=NULL,
#        y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme+
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2015_vert_temp, 
#               subset=(datetime >= as.POSIXct('2015-07-01', tz='UTC') & datetime < as.POSIXct('2015-08-01', tz='UTC'))),
#        aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='jul 2015, NAs recoded', 
#        x=NULL,
#        y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme+
#   scale_x_datetime(date_minor_breaks = '1 day')

#add intermittent flag for jun 25 through jul 10
buoy2015_L1 <- buoy2015_L1 %>% 
  mutate(temp_flag = case_when(datetime>=as.POSIXct('2015-06-25', tz='UTC') & datetime<=as.POSIXct('2015-07-10', tz='UTC') ~ 'i',
                               TRUE ~ '')) 
buoy2015_vert_temp_L1 <- buoy2015_L1 %>%
  select(datetime, temp_flag, alltemp2011) %>%
  gather(variable, value, -datetime, -temp_flag)


ggplot(buoy2015_vert_temp_L1,
       aes(x=datetime, y=value, col=variable, shape = temp_flag)) +
  geom_point() +
  labs(title='2015, clean, with flags',
       x=NULL,
       y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 month')


# #reality check with LMP
# buoy_LMP_2015 <- buoy2015_vert_temp_L1 %>%
#   mutate(source='buoy') %>%
#   full_join(., LMP2015_temp)
# unique(LMP2015_temp$datetime)
# 
# #June 22, July 20, Aug 10, Sept 21
# ggplot(subset(buoy_LMP_2015,
#               subset=(datetime>=as.POSIXct('2015-06-22', tz='UTC') &
#                         datetime<as.POSIXct('2015-06-23', tz='UTC'))),
#        aes(x=datetime, y=value, color=as.factor(variable), shape = source)) +
#   geom_point() +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# ggplot(subset(buoy_LMP_2015,
#               subset=(datetime>=as.POSIXct('2015-07-20', tz='UTC') &
#                         datetime<as.POSIXct('2015-07-21', tz='UTC'))),
#        aes(x=datetime, y=value, color=as.factor(variable), shape = source)) +
#   geom_point() +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# ggplot(subset(buoy_LMP_2015,
#               subset=(datetime>=as.POSIXct('2015-08-10', tz='UTC') &
#                         datetime<as.POSIXct('2015-08-11', tz='UTC'))),
#        aes(x=datetime, y=value, color=as.factor(variable), shape = source)) +
#   geom_point() +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# ggplot(subset(buoy_LMP_2015,
#               subset=(datetime>=as.POSIXct('2015-09-21', tz='UTC') &
#                         datetime<as.POSIXct('2015-09-22', tz='UTC'))),
#        aes(x=datetime, y=value, color=as.factor(variable), shape = source)) +
#   geom_point() +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust


#### DO ####

buoy2015_vert_do <- buoy2015_L1 %>%
  select(datetime, upDO, lowDO) %>%
  gather(variable, value, -datetime)

# ggplot(buoy2015_vert_do, 
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='2015, raw', x='date', y='') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 month')

# remove NA values
buoy2015_L1 <- buoy2015_L1 %>% 
  mutate_at(vars(upDO, lowDO),
            funs(case_when(. == -6999 ~ NA_real_,
                           . == 0 ~ NA_real_,
                           TRUE ~ .)))

buoy2015_vert_do <- buoy2015_L1 %>%
  select(datetime, upDO, lowDO) %>%
  gather(variable, value, -datetime)

ggplot(buoy2015_vert_do, 
       aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='2015, NAs recoded', x='date', y='') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 month')

buoy2015_L1 <- buoy2015_L1 %>%
  mutate_at(vars(lowDO),
            funs(case_when(datetime>=as.POSIXct('2015-01-01', tz='UTC') & datetime<as.POSIXct('2015-06-01', tz='UTC') ~ NA_real_,
                           TRUE ~ .)))
buoy2015_vert_do <- buoy2015_L1 %>%
  select(datetime, upDO, lowDO) %>%
  gather(variable, value, -datetime)

# ggplot(subset(buoy2015_vert_do, 
#               subset=(datetime >= as.POSIXct('2015-01-01', tz='UTC') & datetime < as.POSIXct('2015-02-01', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='jan 2015, NAs recoded', 
#        x=NULL, 
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2015_vert_do, 
#               subset=(datetime >= as.POSIXct('2015-02-01', tz='UTC') & datetime < as.POSIXct('2015-03-01', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='feb 2015, NAs recoded', 
#        x=NULL, 
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2015_vert_do, 
#               subset=(datetime >= as.POSIXct('2015-03-01', tz='UTC') & datetime < as.POSIXct('2015-04-01', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='mar 2015, NAs recoded', 
#        x=NULL, 
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2015_vert_do, 
#               subset=(datetime >= as.POSIXct('2015-04-01', tz='UTC') & datetime < as.POSIXct('2015-05-01', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='apr 2015, NAs recoded', 
#        x=NULL, 
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2015_vert_do, 
#               subset=(datetime >= as.POSIXct('2015-04-22', tz='UTC') & datetime < as.POSIXct('2015-04-23', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='apr 2015, NAs recoded', 
#        x=NULL, 
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour')

buoy2015_L1 <- buoy2015_L1 %>% 
  mutate_at(vars(upDO),
            funs(case_when(datetime>=as.POSIXct('2015-04-22 11:00', tz='UTC') & datetime<=as.POSIXct('2015-04-22 11:10', tz='UTC') ~ NA_real_,
                           TRUE ~ .)))

buoy2015_vert_do_L1 <- buoy2015_L1 %>%
  select(datetime, upDO, lowDO) %>%
  gather(variable, value, -datetime)

# ggplot(subset(buoy2015_vert_do_L1,
#               subset=(datetime >= as.POSIXct('2015-04-01', tz='UTC') & datetime < as.POSIXct('2015-05-01', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='apr 2015, clean',
#        x=NULL,
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2015_vert_do, 
#               subset=(datetime >= as.POSIXct('2015-05-01', tz='UTC') & datetime < as.POSIXct('2015-06-01', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='may 2015, NAs recoded', 
#        x=NULL, 
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2015_vert_do, 
#               subset=(datetime >= as.POSIXct('2015-06-01', tz='UTC') & datetime < as.POSIXct('2015-07-01', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='jun 2015, NAs recoded', 
#        x=NULL, 
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2015_vert_do, 
#               subset=(datetime >= as.POSIXct('2015-06-01', tz='UTC') & datetime < as.POSIXct('2015-06-02', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='jun 2015, NAs recoded', 
#        x=NULL, 
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour')
# 
# ggplot(subset(buoy2015_vert_do, 
#               subset=(datetime >= as.POSIXct('2015-06-11', tz='UTC') & datetime < as.POSIXct('2015-06-12', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='jun 2015, NAs recoded', 
#        x=NULL, 
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour')

buoy2015_L1 <- buoy2015_L1 %>% 
  mutate_at(vars(upDO, DOLowTempC),
            funs(case_when(datetime>=as.POSIXct('2015-06-01', tz='UTC') & datetime<as.POSIXct('2015-06-11 10:10', tz='UTC') ~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate_at(vars(DOLowSat, DOLowPPM),
            funs(case_when(datetime>=as.POSIXct('2015-06-01', tz='UTC') & datetime<as.POSIXct('2015-06-11 11:10', tz='UTC') ~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate(location = case_when(datetime>=as.POSIXct('2015-01-01', tz='UTC') & datetime<as.POSIXct('2015-06-11 9:10', tz='UTC') ~ 'harbor',
                              datetime>=as.POSIXct('2015-06-11 9:10', tz='UTC') & datetime<as.POSIXct('2015-06-11 10:10', tz='UTC') ~ 'in transit',
                              TRUE ~ 'loon'))%>% 
  mutate(upper_do_flag = case_when(datetime==as.POSIXct('2015-06-11 10:10', tz='UTC') ~ 'wp',
                                   TRUE ~ '')) %>% 
  mutate(lower_do_flag = case_when(datetime==as.POSIXct('2015-06-11 11:10', tz='UTC') ~ 'wp',
                                   TRUE ~ ''))

buoy2015_vert_do_L1 <- buoy2015_L1 %>%
  select(datetime, location, upDO, lowDO) %>%
  gather(variable, value, -datetime, -location)

# ggplot(subset(buoy2015_vert_do_L1, 
#               subset=(datetime >= as.POSIXct('2015-06-01', tz='UTC') & datetime < as.POSIXct('2015-07-01', tz='UTC'))),
#        aes(x=datetime, y=value, color = location)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='jun 2015, clean', 
#        x=NULL, 
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2015_vert_do, 
#               subset=(datetime >= as.POSIXct('2015-07-01', tz='UTC') & datetime < as.POSIXct('2015-08-01', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='jul 2015, NAs recoded', 
#        x=NULL, 
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2015_vert_do, 
#               subset=(datetime >= as.POSIXct('2015-08-01', tz='UTC') & datetime < as.POSIXct('2015-09-01', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='aug 2015, NAs recoded', 
#        x=NULL, 
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2015_vert_do, 
#               subset=(datetime >= as.POSIXct('2015-08-13', tz='UTC') & datetime < as.POSIXct('2015-08-14', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='aug 2015, NAs recoded', 
#        x=NULL, 
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour')

#August 13th visit
buoy2015_L1 <-  buoy2015_L1 %>% 
  mutate_at(vars(lowDO, upDO),
            funs(case_when(datetime==as.POSIXct('2015-08-13 10:00', tz='UTC') ~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate(upper_do_flag = case_when(datetime==as.POSIXct('2015-08-13 10:00', tz='UTC') ~ 'w',
                                   TRUE ~ upper_do_flag)) %>% 
  mutate(lower_do_flag = case_when(datetime==as.POSIXct('2015-08-13 10:00', tz='UTC') ~ 'w',
                                   TRUE ~ lower_do_flag))

buoy2015_vert_do_L1 <- buoy2015_L1 %>%
  select(datetime, location, upDO, lowDO) %>%
  gather(variable, value, -datetime, -location)

# ggplot(subset(buoy2015_vert_do_L1, 
#               subset=(datetime >= as.POSIXct('2015-08-01', tz='UTC') & datetime < as.POSIXct('2015-09-01', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='aug 2015, clean', 
#        x=NULL, 
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2015_vert_do, 
#               subset=(datetime >= as.POSIXct('2015-09-01', tz='UTC') & datetime < as.POSIXct('2015-10-01', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='sept 2015, NAs recoded', 
#        x=NULL, 
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2015_vert_do, 
#               subset=(datetime >= as.POSIXct('2015-09-19', tz='UTC') & datetime < as.POSIXct('2015-09-20', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='sept 2015, NAs recoded', 
#        x=NULL, 
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour')

#errant point around the 19th
buoy2015_L1 <- buoy2015_L1 %>% 
  mutate_at(vars(upDO),
            funs(case_when(datetime==as.POSIXct('2015-09-19 5:50', tz='UTC') ~ NA_real_,
                           TRUE ~ .)))

buoy2015_vert_do_L1 <- buoy2015_L1 %>%
  select(datetime, location, upDO, lowDO) %>%
  gather(variable, value, -datetime, -location)

# ggplot(subset(buoy2015_vert_do_L1, 
#               subset=(datetime >= as.POSIXct('2015-09-01', tz='UTC') & datetime < as.POSIXct('2015-10-01', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='sept 2015, clean', 
#        x=NULL, 
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2015_vert_do, 
#               subset=(datetime >= as.POSIXct('2015-10-01', tz='UTC') & datetime < as.POSIXct('2015-11-01', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='oct 2015, NAs recoded', 
#        x=NULL, 
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2015_vert_do, 
#               subset=(datetime >= as.POSIXct('2015-10-08', tz='UTC') & datetime < as.POSIXct('2015-10-09', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='oct 2015, NAs recoded', 
#        x=NULL, 
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour')

#remove artifacts of buoy movement and data thereafter, update location data
buoy2015_L1 <- buoy2015_L1 %>% 
  mutate_at(vars(upDO, lowDO),
            funs(case_when(datetime>=as.POSIXct('2015-10-08 10:40', tz='UTC') & datetime<as.POSIXct('2016-01-01', tz='UTC') ~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate(location = case_when(datetime>=as.POSIXct('2015-10-08 10:40', tz='UTC') & buoy2015_L1$datetime<as.POSIXct('2015-10-08 11:30', tz='UTC') ~ 'in transit',
                              datetime>=as.POSIXct('2015-10-08 11:30', tz='UTC') & buoy2015_L1$datetime<as.POSIXct('2016-01-01', tz='UTC') ~ 'harbor',
                              TRUE ~ location))

buoy2015_vert_do_L1 <- buoy2015_L1 %>%
  select(datetime, location, upDO, lowDO) %>%
  gather(variable, value, -datetime, -location)

# ggplot(subset(buoy2015_vert_do_L1,
#               subset=(datetime >= as.POSIXct('2015-10-01', tz='UTC') & datetime < as.POSIXct('2015-11-01', tz='UTC'))),
#        aes(x=datetime, y=value, color = location)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='oct 2015, clean',
#        x=NULL,
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2015_vert_do, 
#               subset=(datetime >= as.POSIXct('2015-11-01', tz='UTC') & datetime < as.POSIXct('2015-12-01', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='nov 2015, NAs recoded', 
#        x=NULL, 
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2015_vert_do, 
#               subset=(datetime >= as.POSIXct('2015-12-01', tz='UTC') & datetime < as.POSIXct('2016-01-01', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='dec 2015, NAs recoded', 
#        x=NULL, 
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')

ggplot(buoy2015_vert_do_L1, 
       aes(x=datetime, y=value, color = location)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='2015, clean', x='date', y='') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 month')


####PAR ####
ggplot(buoy2015_L1,
       aes(x=datetime, y=PAR, color = location)) +
  geom_point() +
  labs(title='2015, raw', 
       x=NULL, 
       y='PAR (uE*m-2*s-1)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 month')

#replace negative values with 0
buoy2015_L1 <-  buoy2015_L1 %>% 
  mutate(PAR = case_when(PAR < 0 ~ 0,
                         TRUE ~ PAR))

# ggplot(buoy2015_L1,
#        aes(x=datetime, y=PAR, color = location)) +
#   geom_point() +
#   labs(title='2015, raw', 
#        x=NULL, 
#        y='PAR (uE*m-2*s-1)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 month')
# 
# ggplot(subset(buoy2015_L1,
#               subset=(datetime>=as.POSIXct('2015-06-01', tz='UTC') & datetime<as.POSIXct('2015-07-01', tz='UTC'))),
#        aes(x=datetime, y=PAR, color = location)) +
#   geom_point() +
#   labs(title='jun2015, raw', 
#        x=NULL, 
#        y='PAR (uE*m-2*s-1)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')

buoy2015_L1 <-  buoy2015_L1 %>% 
  mutate(PAR = case_when(datetime>=as.POSIXct('2015-01-01', tz='UTC') & datetime<as.POSIXct('2015-06-05', tz='UTC') ~ NA_real_,
                         TRUE ~ PAR))

# ggplot(buoy2015_L1,
#        aes(x=datetime, y=PAR, color = location)) +
#   geom_point() +
#   labs(title='2015, partial clean', 
#        x=NULL, 
#        y='PAR (uE*m-2*s-1)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 month')

#remove data possibly errant due to buoy move
buoy2015_L1 <-  buoy2015_L1 %>% 
  mutate(PAR = case_when(location == 'in transit' ~ NA_real_,
                         TRUE ~ PAR))

# ggplot(buoy2015_L1,
#        aes(x=datetime, y=PAR, color = location)) +
#   geom_point() +
#   labs(title='2015, partial clean', 
#        x=NULL, 
#        y='PAR (uE*m-2*s-1)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 month')
# 
# ggplot(subset(buoy2015_L1,
#               subset=(datetime>=as.POSIXct('2015-07-01', tz='UTC') & datetime<as.POSIXct('2015-08-01', tz='UTC'))),
#        aes(x=datetime, y=PAR, color = location)) +
#   geom_point() +
#   labs(title='jul 2015, raw', 
#        x=NULL, 
#        y='PAR (uE*m-2*s-1)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2015_L1,
#               subset=(datetime>=as.POSIXct('2015-08-01', tz='UTC') & datetime<as.POSIXct('2015-09-01', tz='UTC'))),
#        aes(x=datetime, y=PAR, color = location)) +
#   geom_point() +
#   labs(title='aug 2015, raw', 
#        x=NULL, 
#        y='PAR (uE*m-2*s-1)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2015_L1,
#               subset=(datetime>=as.POSIXct('2015-09-01', tz='UTC') & datetime<as.POSIXct('2015-10-01', tz='UTC'))),
#        aes(x=datetime, y=PAR, color = location)) +
#   geom_point() +
#   labs(title='sept 2015, raw', 
#        x=NULL, 
#        y='PAR (uE*m-2*s-1)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2015_L1,
#               subset=(datetime>=as.POSIXct('2015-10-01', tz='UTC') & datetime<as.POSIXct('2015-11-01', tz='UTC'))),
#        aes(x=datetime, y=PAR, color = location)) +
#   geom_point() +
#   labs(title='oct 2015, raw', 
#        x=NULL, 
#        y='PAR (uE*m-2*s-1)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2015_L1,
#               subset=(datetime>=as.POSIXct('2015-11-01', tz='UTC') & datetime<as.POSIXct('2015-12-01', tz='UTC'))),
#        aes(x=datetime, y=PAR, color = location)) +
#   geom_point() +
#   labs(title='nov 2015, raw', 
#        x=NULL, 
#        y='PAR (uE*m-2*s-1)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2015_L1,
#               subset=(datetime>=as.POSIXct('2015-12-01', tz='UTC') & datetime<as.POSIXct('2016-01-01', tz='UTC'))),
#        aes(x=datetime, y=PAR, color = location)) +
#   geom_point() +
#   labs(title='dec 2015, raw', 
#        x=NULL, 
#        y='PAR (uE*m-2*s-1)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')

ggplot(buoy2015_L1,
       aes(x=datetime, y=PAR, color = location)) +
  geom_point() +
  labs(title='2015, clean', 
       x=NULL, 
       y='PAR (uE*m-2*s-1)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 month')


####Wind data ####
buoy2015_vert_wind <- buoy2015_L1 %>%
  select(datetime, location, InstWindSp, InstWindDir, AveWindSp, AveWindDir, MaxWindSp, MaxWindDir) %>%
  gather(variable, value, -datetime, -location)

ggplot(buoy2015_vert_wind, 
       aes(x=datetime, y=value, color=location)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='2015, raw', 
       x=NULL, 
       y=NULL) +
  final_theme

# ggplot(subset(buoy2015_vert_wind, 
#               subset=(datetime>=as.POSIXct('2015-01-01', tz='UTC') & datetime<as.POSIXct('2015-02-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='jan 2015, raw', 
#        x=NULL, 
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# #frozen sensor jan 4
# ggplot(subset(buoy2015_vert_wind, 
#               subset=(datetime>=as.POSIXct('2015-01-04', tz='UTC') & datetime<as.POSIXct('2015-01-05', tz='UTC'))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='jan 2015, raw', 
#        x=NULL, 
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour')

buoy2015_L1 <- buoy2015_L1 %>% 
  mutate_at(vars(InstWindSp, InstWindDir, AveWindSp, AveWindDir, MaxWindSp, MaxWindDir),
            funs(case_when(datetime>=as.POSIXct('2015-01-04 8:40', tz='UTC') & datetime<=as.POSIXct('2015-01-04 9:30', tz='UTC') ~ NA_real_,
                           TRUE ~ .)))

buoy2015_vert_wind_L1 <- buoy2015_L1 %>%
  select(datetime, location, InstWindSp, InstWindDir, AveWindSp, AveWindDir, MaxWindSp, MaxWindDir) %>%
  gather(variable, value, -datetime, -location)

# ggplot(subset(buoy2015_vert_wind_L1, 
#               subset=(datetime>=as.POSIXct('2015-01-01', tz='UTC') & datetime<as.POSIXct('2015-02-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='jan 2015, raw', 
#        x=NULL, 
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2015_vert_wind, 
#               subset=(datetime>=as.POSIXct('2015-02-01', tz='UTC') & datetime<as.POSIXct('2015-03-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='feb 2015, raw', 
#        x=NULL, 
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2015_vert_wind, 
#               subset=(datetime>=as.POSIXct('2015-02-14 18:00', tz='UTC') & datetime<as.POSIXct('2015-02-16', tz='UTC'))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='feb 2015, raw', 
#        x=NULL, 
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour')
# 
# ggplot(subset(buoy2015_vert_wind, 
#               subset=(datetime>=as.POSIXct('2015-02-18 18:00', tz='UTC') & datetime<as.POSIXct('2015-02-20', tz='UTC'))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='feb 2015, raw', 
#        x=NULL, 
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour')

buoy2015_L1 <- buoy2015_L1 %>% 
  mutate_at(vars(InstWindSp, InstWindDir, AveWindSp, AveWindDir, MaxWindSp, MaxWindDir),
            funs(case_when(datetime>=as.POSIXct('2015-02-15 00:50', tz='UTC') & datetime<as.POSIXct('2015-02-15 14:10', tz='UTC') ~ NA_real_,
                           datetime>=as.POSIXct('2015-02-18 22:40', tz='UTC') & datetime<as.POSIXct('2015-02-19 11:00', tz='UTC') ~ NA_real_,
                           TRUE ~ .)))

buoy2015_vert_wind_L1 <- buoy2015_L1 %>%
  select(datetime, location, InstWindSp, InstWindDir, AveWindSp, AveWindDir, MaxWindSp, MaxWindDir) %>%
  gather(variable, value, -datetime, -location)

# ggplot(subset(buoy2015_vert_wind_L1,
#               subset=(datetime>=as.POSIXct('2015-02-01', tz='UTC') & datetime<as.POSIXct('2015-03-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='feb 2015, clean',
#        x=NULL,
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2015_vert_wind,
#               subset=(datetime>=as.POSIXct('2015-03-01', tz='UTC') & datetime<as.POSIXct('2015-04-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='mar 2015, raw',
#        x=NULL,
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2015_vert_wind,
#               subset=(datetime>=as.POSIXct('2015-03-14', tz='UTC') & datetime<as.POSIXct('2015-03-16', tz='UTC'))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='mar 2015, raw',
#        x=NULL,
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour')
# 
# ggplot(subset(buoy2015_vert_wind,
#               subset=(datetime>=as.POSIXct('2015-04-01', tz='UTC') & datetime<as.POSIXct('2015-05-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='apr 2015, raw',
#        x=NULL,
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2015_vert_wind,
#               subset=(datetime>=as.POSIXct('2015-05-01', tz='UTC') & datetime<as.POSIXct('2015-06-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='may 2015, raw',
#        x=NULL,
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2015_vert_wind,
#               subset=(datetime>=as.POSIXct('2015-06-01', tz='UTC') & datetime<as.POSIXct('2015-07-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='jun 2015, raw',
#        x=NULL,
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')


buoy2015_L1 <- buoy2015_L1 %>% 
  mutate_at(vars(InstWindSp, InstWindDir, AveWindSp, AveWindDir, MaxWindSp, MaxWindDir),
            funs(case_when(datetime>=as.POSIXct('2015-06-01 00:00', tz='UTC') & datetime<=as.POSIXct('2015-06-04 10:00', tz='UTC') ~ NA_real_,
                           location == 'in transit' ~ NA_real_,
                           TRUE ~ .)))

buoy2015_vert_wind_L1 <- buoy2015_L1 %>%
  select(datetime, InstWindSp, InstWindDir, AveWindSp, AveWindDir, MaxWindSp, MaxWindDir, location) %>%
  gather(variable, value, -datetime, -location)

# ggplot(subset(buoy2015_vert_wind_L1,
#               subset=(datetime>=as.POSIXct('2015-06-01', tz='UTC') & datetime<as.POSIXct('2015-07-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='jun 2015, clean',
#        x=NULL,
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2015_vert_wind,
#               subset=(datetime>=as.POSIXct('2015-07-01', tz='UTC') & datetime<as.POSIXct('2015-08-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='jul 2015, raw',
#        x=NULL,
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2015_vert_wind,
#               subset=(datetime>=as.POSIXct('2015-08-01', tz='UTC') & datetime<as.POSIXct('2015-09-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='jul 2015, raw',
#        x=NULL,
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2015_vert_wind,
#               subset=(datetime>=as.POSIXct('2015-09-01', tz='UTC') & datetime<as.POSIXct('2015-10-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='sept 2015, raw',
#        x=NULL,
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2015_vert_wind,
#               subset=(datetime>=as.POSIXct('2015-10-01', tz='UTC') & datetime<as.POSIXct('2015-11-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='oct 2015, raw',
#        x=NULL,
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# #recode data during buoy move
# ggplot(subset(buoy2015_vert_wind_L1,
#               subset=(datetime>=as.POSIXct('2015-10-01', tz='UTC') & datetime<as.POSIXct('2015-11-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='oct 2015, clean',
#        x=NULL,
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2015_vert_wind,
#               subset=(datetime>=as.POSIXct('2015-11-01', tz='UTC') & datetime<as.POSIXct('2015-12-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='nov 2015, raw',
#        x=NULL,
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2015_vert_wind,
#               subset=(datetime>=as.POSIXct('2015-12-01', tz='UTC') & datetime<as.POSIXct('2016-01-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='dec 2015, raw',
#        x=NULL,
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2015_vert_wind,
#               subset=(datetime>=as.POSIXct('2015-12-30', tz='UTC') & datetime<as.POSIXct('2015-12-31', tz='UTC'))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='dec 2015, raw',
#        x=NULL,
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour')
# 
# ggplot(subset(buoy2015_vert_wind,
#               subset=(datetime>=as.POSIXct('2015-12-31', tz='UTC') & datetime<as.POSIXct('2016-01-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='dec 2015, raw',
#        x=NULL,
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour')


buoy2015_L1 <- buoy2015_L1 %>% 
  mutate_at(vars(InstWindSp, InstWindDir, AveWindSp, AveWindDir, MaxWindSp, MaxWindDir),
            funs(case_when(datetime>=as.POSIXct('2015-12-30 8:50', tz='UTC') & datetime<as.POSIXct('2015-12-30 20:00', tz='UTC') ~ NA_real_,
                           datetime>=as.POSIXct('2015-12-31 5:40', tz='UTC') & datetime<as.POSIXct('2015-12-31 7:20', tz='UTC') ~ NA_real_,
                           TRUE ~ .)))
buoy2015_vert_wind_L1 <- buoy2015_L1 %>%
  select(datetime, InstWindSp, InstWindDir, AveWindSp, AveWindDir, MaxWindSp, MaxWindDir, location) %>%
  gather(variable, value, -datetime, -location)


# ggplot(subset(buoy2015_vert_wind_L1,
#               subset=(datetime>=as.POSIXct('2015-12-01', tz='UTC') & datetime<as.POSIXct('2016-01-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='dec 2015, clean',
#        x=NULL,
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')

ggplot(buoy2015_vert_wind_L1,
       aes(x=datetime, y=value, color=location)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='2015, clean',
       x=NULL,
       y=NULL) +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 month')



####chla data ####
buoy2015_vert_chla <- buoy2015_L1 %>%
  select(datetime, chla) %>%
  gather(variable, value, -datetime)

#recode cond readings to NA for enitre year
buoy2015_L1 <- buoy2015_L1 %>% 
  mutate(SpecCond = NA_real_) %>% 
  mutate_at(vars(Chlor_RFU, Chlor_UGL),
            funs(case_when(. == -6999 ~ NA_real_,
                           . == 6999 ~ NA_real_,
                           TRUE ~ .)))

buoy2015_vert_chla <- buoy2015_L1 %>%
  select(datetime, chla) %>%
  gather(variable, value, -datetime)

# ggplot(buoy2015_vert_chla, aes(x=datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable ~ .) +
#   final_theme +
#   labs(title = 'chl-a and cond 2015, NA values recoded',
#        x=NULL,
#        y=NULL) +
#   scale_x_datetime(date_minor_breaks = '1 month')

ggplot(subset(buoy2015_vert_chla, 
              subset = (datetime>=as.POSIXct('2015-06-01', tz='UTC') & datetime< as.POSIXct('2015-07-01', tz='UTC'))),
              aes(x=datetime, y = value)) +
  geom_point() +
  facet_grid(variable ~ .) +
  final_theme +
  labs(title = 'chl-a and cond jun 2015, NA values recoded',
       x=NULL,
       y=NULL) +
  scale_x_datetime(date_minor_breaks = '1 day')

#recode data before move to loon
buoy2015_L1 <- buoy2015_L1 %>% 
  mutate_at(vars(Chlor_RFU, Chlor_UGL),
            funs(case_when(datetime< as.POSIXct('2015-06-11 10:10', tz='UTC') ~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate(chla_flag = case_when(datetime == as.POSIXct('2015-06-11 10:10', tz='UTC') ~ 'cp',
                               TRUE ~ ''))

#recode - data as 0
buoy2015_L1 <- buoy2015_L1 %>% 
  mutate(Chlor_UGL = case_when(Chlor_UGL < 0 ~ 0,
                               TRUE ~ Chlor_UGL))

buoy2015_vert_chla_L1 <- buoy2015_L1 %>%
  select(datetime, chla) %>%
  gather(variable, value, -datetime)

# ggplot(subset(buoy2015_vert_chla_L1, 
#               subset = (datetime>=as.POSIXct('2015-06-01', tz='UTC') & datetime< as.POSIXct('2015-07-01', tz='UTC'))),
#        aes(x=datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable ~ .) +
#   final_theme +
#   labs(title = 'chl-a and cond jun 2015, NA values recoded',
#        x=NULL,
#        y=NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2015_vert_chla, 
#               subset = (datetime>=as.POSIXct('2015-07-01', tz='UTC') & datetime< as.POSIXct('2015-08-01', tz='UTC'))),
#        aes(x=datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   final_theme +
#   labs(title = 'chl-a and cond jul 2015, NA values recoded',
#        x=NULL,
#        y=NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# #recode outlier on jul 16 
# ggplot(subset(buoy2015_vert_chla, 
#               subset = (datetime>=as.POSIXct('2015-07-16', tz='UTC') & datetime< as.POSIXct('2015-07-17', tz='UTC'))),
#        aes(x=datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable ~ .) +
#   final_theme +
#   labs(title = 'chl-a and cond jul 2015, NA values recoded',
#        x=NULL,
#        y=NULL) +
#   scale_x_datetime(date_minor_breaks = '1 hour')

buoy2015_L1 <- buoy2015_L1 %>% 
  mutate_at(vars(Chlor_RFU, Chlor_UGL),
            funs(case_when(datetime==as.POSIXct('2015-07-16 21:20', tz='UTC') ~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate(chla_flag = case_when(datetime >= as.POSIXct('2015-07-23', tz='UTC') ~ 's',
                               TRUE ~ chla_flag))
buoy2015_vert_chla_L1 <- buoy2015_L1 %>%
  select(datetime, chla) %>%
  gather(variable, value, -datetime)

# ggplot(subset(buoy2015_vert_chla_L1, 
#               subset = (datetime>=as.POSIXct('2015-07-01', tz='UTC') & datetime< as.POSIXct('2015-08-01', tz='UTC'))),
#        aes(x=datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   final_theme +
#   labs(title = 'chl-a and cond jul 2015, NA values recoded',
#        x=NULL,
#        y=NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2015_vert_chla, 
#               subset = (datetime>=as.POSIXct('2015-08-01', tz='UTC') & datetime< as.POSIXct('2015-09-01', tz='UTC'))),
#        aes(x=datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   final_theme +
#   labs(title = 'chl-a and cond aug 2015, NA values recoded',
#        x=NULL,
#        y=NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2015_vert_chla, 
#               subset = (datetime>=as.POSIXct('2015-08-13', tz='UTC') & datetime< as.POSIXct('2015-08-14', tz='UTC'))),
#        aes(x=datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable ~ .) +
#   final_theme +
#   labs(title = 'chl-a and cond jun 2015, NA values recoded',
#        x=NULL,
#        y=NULL) +
#   scale_x_datetime(date_minor_breaks = '1 hour')

buoy2015_L1 <- buoy2015_L1 %>% 
  mutate_at(vars(Chlor_RFU, Chlor_UGL),
            funs(case_when(datetime>=as.POSIXct('2015-08-13 10:10', tz='UTC') ~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate(chla_flag = case_when(datetime >= as.POSIXct('2015-08-13 10:10', tz='UTC') ~ '',
                               TRUE ~ chla_flag))

buoy2015_vert_chla_L1 <- buoy2015_L1 %>%
  select(datetime, chla, chla_flag) %>%
  gather(variable, value, -datetime, -chla_flag)

# ggplot(subset(buoy2015_vert_chla_L1, 
#               subset = (datetime>=as.POSIXct('2015-08-01', tz='UTC') & datetime< as.POSIXct('2015-09-01', tz='UTC'))),
#        aes(x=datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   final_theme +
#   labs(title = 'chl-a and cond aug 2015, NA values recoded',
#        x=NULL,
#        y=NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')

ggplot(buoy2015_vert_chla_L1, 
       aes(x=datetime, y = value, color = chla_flag)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  final_theme +
  labs(title = 'chl-a and cond 2015, clean with flags',
       x=NULL,
       y=NULL) +
  scale_x_datetime(date_minor_breaks = '1 month') +
  scale_color_colorblind()


#### air temp ####
ggplot(buoy2015_L1,
       aes(x=datetime, y=AirTempC, color = location)) +
  geom_point() +
  labs(title = '2015 air temp', x=NULL, y='Air temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 month')

#no NA values to recode

# ggplot(subset(buoy2015_L1,
#               subset=(datetime>=as.POSIXct('2015-01-01', tz='UTC') & datetime<as.POSIXct('2015-02-01', tz='UTC'))),
#        aes(x=datetime, y=AirTempC, color = location)) +
#   geom_point() +
#   labs(title = 'jan 2015 air temp', x=NULL, y='Air temp (deg C)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2015_L1,
#               subset=(datetime>=as.POSIXct('2015-02-01', tz='UTC') & datetime<as.POSIXct('2015-03-01', tz='UTC'))),
#        aes(x=datetime, y=AirTempC, color = location)) +
#   geom_point() +
#   labs(title = 'feb 2015 air temp', x=NULL, y='Air temp (deg C)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2015_L1,
#               subset=(datetime>=as.POSIXct('2015-03-01', tz='UTC') & datetime<as.POSIXct('2015-04-01', tz='UTC'))),
#        aes(x=datetime, y=AirTempC, color = location)) +
#   geom_point() +
#   labs(title = 'mar 2015 air temp', x=NULL, y='Air temp (deg C)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2015_L1,
#               subset=(datetime>=as.POSIXct('2015-04-01', tz='UTC') & datetime<as.POSIXct('2015-05-01', tz='UTC'))),
#        aes(x=datetime, y=AirTempC, color = location)) +
#   geom_point() +
#   labs(title = 'apr 2015 air temp', x=NULL, y='Air temp (deg C)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# #data gap
# 
# ggplot(subset(buoy2015_L1,
#               subset=(datetime>=as.POSIXct('2015-06-01', tz='UTC') & datetime<as.POSIXct('2015-07-01', tz='UTC'))),
#        aes(x=datetime, y=AirTempC, color = location)) +
#   geom_point() +
#   labs(title = 'jun 2015 air temp', x=NULL, y='Air temp (deg C)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2015_L1,
#               subset=(datetime>=as.POSIXct('2015-06-04', tz='UTC') & datetime<as.POSIXct('2015-06-05', tz='UTC'))),
#        aes(x=datetime, y=AirTempC, color = location)) +
#   geom_point() +
#   labs(title = 'jun 2015 air temp', x=NULL, y='Air temp (deg C)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour')

#recode data prior to data columns being fixed and data during buoy move
buoy2015_L1 <- buoy2015_L1 %>% 
  mutate(AirTempC = case_when(datetime>=as.POSIXct('2015-06-01 00:00', tz='UTC') & datetime<=as.POSIXct('2015-06-04 08:50', tz='UTC') ~ NA_real_,
                              location == 'in transit' ~ NA_real_,
                              TRUE ~ AirTempC))

# ggplot(subset(buoy2015_L1,
#               subset=(datetime>=as.POSIXct('2015-06-01', tz='UTC') & datetime<as.POSIXct('2015-07-01', tz='UTC'))),
#        aes(x=datetime, y=AirTempC, color = location)) +
#   geom_point() +
#   labs(title = 'jun 2015 air temp', x=NULL, y='Air temp (deg C)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2015_L1,
#               subset=(datetime>=as.POSIXct('2015-07-01', tz='UTC') & datetime<as.POSIXct('2015-08-01', tz='UTC'))),
#        aes(x=datetime, y=AirTempC, color = location)) +
#   geom_point() +
#   labs(title = 'jul 2015 air temp', x=NULL, y='Air temp (deg C)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2015_L1,
#               subset=(datetime>=as.POSIXct('2015-08-01', tz='UTC') & datetime<as.POSIXct('2015-09-01', tz='UTC'))),
#        aes(x=datetime, y=AirTempC, color = location)) +
#   geom_point() +
#   labs(title = 'aug 2015 air temp', x=NULL, y='Air temp (deg C)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2015_L1,
#               subset=(datetime>=as.POSIXct('2015-09-01', tz='UTC') & datetime<as.POSIXct('2015-10-01', tz='UTC'))),
#        aes(x=datetime, y=AirTempC, color = location)) +
#   geom_point() +
#   labs(title = 'sept 2015 air temp', x=NULL, y='Air temp (deg C)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2015_L1,
#               subset=(datetime>=as.POSIXct('2015-10-01', tz='UTC') & datetime<as.POSIXct('2015-11-01', tz='UTC'))),
#        aes(x=datetime, y=AirTempC, color = location)) +
#   geom_point() +
#   labs(title = 'oct 2015 air temp', x=NULL, y='Air temp (deg C)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2015_L1,
#               subset=(datetime>=as.POSIXct('2015-11-01', tz='UTC') & datetime<as.POSIXct('2015-12-01', tz='UTC'))),
#        aes(x=datetime, y=AirTempC, color = location)) +
#   geom_point() +
#   labs(title = 'nov 2015 air temp', x=NULL, y='Air temp (deg C)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2015_L1,
#               subset=(datetime>=as.POSIXct('2015-12-01', tz='UTC') & datetime<as.POSIXct('2016-01-01', tz='UTC'))),
#        aes(x=datetime, y=AirTempC, color = location)) +
#   geom_point() +
#   labs(title = 'dec 2015 air temp', x=NULL, y='Air temp (deg C)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')


#### EXPORT L1 DATA ####

# #export L1 tempstring file
# buoy2015_L1 %>% 
#   select(datetime, alltemp2011, temp_flag, location) %>% 
#   mutate(datetime = as.character(datetime)) %>% 
#   write_csv(., 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2015_tempstring_L1.csv')
# 
# #export l1 do file
# buoy2015_L1 %>% 
#   select(datetime, upDO, lowDO, upper_do_flag, lower_do_flag, location) %>% 
#   mutate(datetime = as.character(datetime)) %>% 
#   write_csv(., 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2015_do_L1.csv')
# 
# #export l1 par file
# buoy2015_L1 %>% 
#   select(datetime, PAR, location) %>% 
#   mutate(datetime = as.character(datetime)) %>% 
#   write_csv(., 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2015_PAR_L1.csv')
# 
# #export l1 wind
# buoy2015_L1 %>% 
#   select(datetime, InstWindSp, InstWindDir, AveWindSp, AveWindDir, MaxWindSp, MaxWindDir, location) %>% 
#   mutate(datetime = as.character(datetime)) %>% 
#   write_csv(., 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2015_wind_L1.csv')
# 
# #export l1 air temp file
# buoy2015_L1 %>% 
#   select(datetime, AirTempC, location) %>% 
#   mutate(datetime = as.character(datetime)) %>% 
#   write_csv(., 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2015_airtemp_L1.csv')
# 
# #export l1 chla cond file
# buoy2015_L1 %>% 
#   select(datetime, chla, location, chla_flag) %>% 
#   mutate(datetime = as.character(datetime)) %>% 
#   write_csv(., 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2015_chla_L1.csv')




