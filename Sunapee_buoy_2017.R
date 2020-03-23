#*****************************************************************
#*      Cary Institute of Ecosystem Studies (Millbrook, NY)      *
#*                                                               *
#* TITLE:   Sunapee_buoy_2017.r                                  *
#* AUTHOR:  Bethel Steele                                        *
#* SYSTEM:  Lenovo ThinkCentre, Win 10, R 3.4.2, RStudio 1.1.383 *
#* DATE:    24Jan2018                                            *
#* PROJECT: Lake Sunapee Buoy Data Cleaning                      *
#* PURPOSE: create L0 and L1 data for buoy data 2018 using       *
#*          similar methods to CCC and DR                        *
#*****************************************************************

#bring in 2017 buoy raw data
buoy2017 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L0/2017 Buoy Data.csv',
                     col_types = 'iiiinnnnnnnnnnnnnnnnnnnnnnnnnnnnnn')

# #bring in 2017 LMP data for comparison
# LMP2017 <- read_xlsx('C:/Users/steeleb/Dropbox/Lake Sunapee/long term Sunapee data/raw data/LMP files/LMP 2017/DO.xlsx', 
#                      sheet='DO',
#                      col_types = c('text', 'text', 'numeric', 'guess', 'numeric', 
#                                    'numeric', 'numeric', 'numeric', 'numeric' ,'numeric',
#                                    'text', 'text', 'numeric', 'text')) %>% 
#   mutate(DATE = as.Date(DATE, format='%Y-%m-%d')) %>%  #format date
#   filter(DATE >= '2017-01-01' & DATE < '2018-01-01', #filter for 2017 only
#          STATION == 210)
# 
# LMP2017_bio <- read_xlsx('C:/Users/steeleb/Dropbox/Lake Sunapee/long term Sunapee data/raw data/LMP files/LMP 2017/BIOLOGY.xlsx', 
#                          sheet='BIOLOGY',
#                          col_types = c('text', 'text', 'numeric', 'guess', 'numeric', 
#                                        'numeric', 'text', 'numeric', 'text' ,'numeric',
#                                        'text', 'numeric', 'numeric', 'numeric', 'text',
#                                        'text', 'text', 'text')) %>% 
#   mutate(DATE = as.Date(DATE, format='%Y-%m-%d')) %>%  #format date
#   filter(DATE >= '2017-01-01' & DATE < '2018-01-01', #filter for 2017 only
#          STATION == 210) 
# 
# LMP2017_chem <- read_xlsx('C:/Users/steeleb/Dropbox/Lake Sunapee/long term Sunapee data/raw data/LMP files/LMP 2017/CHEMISTRY.xlsx', 
#                           sheet='CHEMISTRY',
#                           col_types = c('text', 'text', 'numeric', 'guess', 'numeric', 
#                                         'text', 'numeric', 'numeric', 'numeric' ,'numeric',
#                                         'numeric', 'numeric', 'numeric', 'numeric', 'numeric',
#                                         'text', 'text', 'guess', 'text', 'text')) %>% 
#   mutate(DATE = as.Date(DATE, format='%Y-%m-%d')) %>%  #format date
#   filter(DATE >= '2017-01-01' & DATE < '2018-01-01', #filter for 2017 only
#          STATION == 210) 


#### format data ####
buoy2017 <- buoy2017  %>%
  rename(Hr.Min = 'Hr/Min',
         DOLowTempC = 'DOLoTempC',
         AveWindSp = 'WindSpdAv',
         AveWindDir = 'WindVect',
         MaxWindSp = 'MaxWind') %>% 
  mutate(hour = Hr.Min%/%100,
         minutes = Hr.Min%%100,
         time = paste(hour, minutes, sep=':')) %>% #break out time from Hr.Min, create time column
  mutate(date = as.Date(paste(Day, Year, sep = '-'), format='%j-%Y'), #create date in ymd format
         datetime = as.POSIXct(paste(date, time, sep=' '), format='%Y-%m-%d %H:%M', tz='UTC')) %>%  #tibble forces to UTC, so coerce for our uses.
  select(-hour, -minutes, -Hr.Min, -Year, -Day, -time, -ArrayID) #remove unnecessary columns


# # format time in LMP data
# LMP2017 <- LMP2017 %>% 
#   mutate(datetime = as.POSIXct(paste(DATE, '12:00', sep=' '), format='%Y-%m-%d %H:%M', tz='UTC')) %>%  #tibble forces to UTC, so coerce for our uses. assume noon, because no time available
#   select(-c(TIME)) #remove unnecessary columns
# 
# str(buoy2017)
# str(LMP2017)
# 
# 
# #subset LMP for truthing data
# LMP2017_temp <- LMP2017 %>% 
#   select(datetime, DEPTH, TEMP) %>% 
#   filter(DEPTH<=12) %>% 
#   rename(depth = 'DEPTH', 
#          value = 'TEMP') %>% 
#   mutate(source = 'LMP')
# 
# LMP2017_upDO <- LMP2017 %>% 
#   select(datetime, DEPTH, DO, PCNTSAT) %>% 
#   filter(DEPTH <= 2) %>% 
#   rename(depth = 'DEPTH', 
#          DOppm = 'DO', 
#          DOSat = 'PCNTSAT') %>% 
#   gather(variable, value, -datetime, -depth) %>% 
#   mutate(source = 'LMP')
# 
# LMP2017_lowDO <- LMP2017 %>% 
#   select(datetime, DEPTH, DO, PCNTSAT) %>% 
#   filter(DEPTH > 9 & DEPTH <12) %>% 
#   rename(depth = 'DEPTH', 
#          DOLowPPM = 'DO', 
#          DOLowSat = 'PCNTSAT') %>% 
#   gather(variable, value, -datetime, -depth) %>% 
#   mutate(source = 'LMP')
# 
# LMP2017_chla <- LMP2017_bio %>%  
#   select(DATE, CHL) %>% 
#   mutate(DATE = as.POSIXct(paste(DATE, '12:00', sep = ' '), tz='UTC')) %>% 
#   rename(Chlor_UGL = 'CHL',
#          datetime = 'DATE') %>% 
#   gather(variable, value, -datetime) %>% 
#   mutate(source = 'LMP')
# 
# LMP2017_cond <- LMP2017_chem %>%  
#   select(DATE, Depth, COND) %>% 
#   mutate(DATE = as.POSIXct(paste(DATE, '12:00', sep = ' '), tz='UTC')) %>% 
#   rename(SpecCond = 'COND',
#          datetime = 'DATE',
#          depth = 'Depth') %>% 
#   gather(variable, value, -datetime) %>% 
#   mutate(source = 'LMP')

# add in all date time options in L1 data set
alltimes_2017 <- as.data.frame(seq.POSIXt(as.POSIXct('2017-01-01 00:00', tz='UTC'), as.POSIXct('2017-12-31 23:50', tz='UTC'), '10 min')) %>% 
  rename("datetime" = !!names(.[1]))

buoy2017_L1 <- buoy2017 %>% 
  right_join(., alltimes_2017) %>% 
  arrange(datetime)

buoy2017_L1 <- buoy2017_L1[!duplicated(buoy2017_L1$datetime),]


####THERMISTERS####
buoy2017_therm_vert <- buoy2017_L1 %>% 
  select(datetime, alltemp2011) %>%
  gather(variable, value, -datetime)

# ggplot(buoy2017_therm_vert, aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 month') +
#   final_theme

buoy2017_L1 <- buoy2017_L1 %>% 
  mutate_at(vars(alltemp2011),
            funs(case_when(. == 1215 ~ NA_real_,
                           . == 555.4 ~ NA_real_,
                           . == -6999 ~ NA_real_,
                           TRUE ~ .)))

buoy2017_therm_vert <- buoy2017_L1 %>% 
  select(datetime, alltemp2011) %>%
  gather(variable, value, -datetime)

# ggplot(buoy2017_therm_vert,
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 month') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme 

buoy2017_L1 <- buoy2017_L1 %>% 
  mutate(TempC_0m = NA_real_)

buoy2017_therm_vert <- buoy2017_L1 %>% 
  select(datetime, alltemp2011) %>%
  gather(variable, value, -datetime)

# ggplot(subset(buoy2017_therm_vert,
#               subset=(datetime >= as.POSIXct('2017-05-01', tz='UTC') & datetime < as.POSIXct('2017-06-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme 
# 
# ggplot(subset(buoy2017_therm_vert,
#               subset=(datetime >= as.POSIXct('2017-05-17', tz='UTC') & datetime < as.POSIXct('2017-05-18', tz='UTC'))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme 

buoy2017_L1 <- buoy2017_L1 %>% 
  mutate_at(vars(alltemp2011),
            funs(case_when(datetime < as.POSIXct('2017-05-17 11:10', tz='UTC') ~ NA_real_,
                           TRUE ~ .)))
buoy2017_therm_vert_L1 <- buoy2017_L1 %>% 
  select(datetime, alltemp2011) %>%
  gather(variable, value, -datetime)

# ggplot(subset(buoy2017_therm_vert_L1,
#               subset=(datetime >= as.POSIXct('2017-05-01', tz='UTC') & datetime < as.POSIXct('2017-06-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme 
# 
# ggplot(subset(buoy2017_therm_vert_L1,
#               subset=(datetime >= as.POSIXct('2017-06-01', tz='UTC') & datetime < as.POSIXct('2017-07-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme 
# 
# ggplot(subset(buoy2017_therm_vert_L1,
#               subset=(datetime >= as.POSIXct('2017-07-01', tz='UTC') & datetime < as.POSIXct('2017-08-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme 

buoy2017_L1 <- buoy2017_L1 %>% 
  mutate_at(vars(TempC_1m:TempC_7m),
            funs(case_when(datetime >= as.POSIXct('2017-06-14', tz='UTC') & datetime < as.POSIXct('2017-07-07', tz='UTC') ~ NA_real_,
                           TRUE ~ .)))
buoy2017_therm_vert_L1 <- buoy2017_L1 %>% 
  select(datetime, alltemp2011) %>%
  gather(variable, value, -datetime)

# ggplot(subset(buoy2017_therm_vert_L1,
#               subset=(datetime >= as.POSIXct('2017-07-01', tz='UTC') & datetime < as.POSIXct('2017-08-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme 
# 
# ggplot(subset(buoy2017_therm_vert_L1,
#               subset=(datetime >= as.POSIXct('2017-07-28', tz='UTC') & datetime < as.POSIXct('2017-07-29', tz='UTC'))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme 
# 
# ggplot(subset(buoy2017_therm_vert_L1,
#               subset=(datetime >= as.POSIXct('2017-08-01', tz='UTC') & datetime < as.POSIXct('2017-09-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme 
# 
# #presumed visit aug 16
# ggplot(subset(buoy2017_therm_vert_L1,
#               subset=(datetime >= as.POSIXct('2017-08-16', tz='UTC') & datetime < as.POSIXct('2017-08-17', tz='UTC'))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme 

buoy2017_L1 <- buoy2017_L1 %>% 
  mutate_at(vars(alltemp2011),
            funs(case_when(datetime == as.POSIXct('2017-08-16 11:40', tz='UTC') ~ NA_real_,
                           TRUE ~ .)))
buoy2017_therm_vert_L1 <- buoy2017_L1 %>% 
  select(datetime, alltemp2011) %>%
  gather(variable, value, -datetime)

# ggplot(subset(buoy2017_therm_vert_L1,
#               subset=(datetime >= as.POSIXct('2017-08-01', tz='UTC') & datetime < as.POSIXct('2017-09-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme 
# 
# ggplot(subset(buoy2017_therm_vert_L1,
#               subset=(datetime >= as.POSIXct('2017-09-01', tz='UTC') & datetime < as.POSIXct('2017-10-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme 
# 
# ggplot(subset(buoy2017_therm_vert_L1,
#               subset=(datetime >= as.POSIXct('2017-10-01', tz='UTC') & datetime < as.POSIXct('2017-11-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme 
# 
# #buoy moved for winter
# ggplot(subset(buoy2017_therm_vert_L1,
#               subset=(datetime >= as.POSIXct('2017-10-19', tz='UTC') & datetime < as.POSIXct('2017-10-20', tz='UTC'))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme 

buoy2017_L1 <- buoy2017_L1 %>% 
  mutate_at(vars(alltemp2011),
            funs(case_when(datetime >= as.POSIXct('2017-10-19 10:00', tz='UTC') ~ NA_real_,
                           TRUE ~ .)))
buoy2017_therm_vert_L1 <- buoy2017_L1 %>% 
  select(datetime, alltemp2011) %>%
  gather(variable, value, -datetime)


# ggplot(buoy2017_therm_vert_L1,
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 month') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme 

rm(buoy2017_therm_vert, buoy2017_therm_vert_L1)


#### DO ####
buoy2017_do_vert <- buoy2017_L1 %>% 
  select(datetime, upDO, lowDO) %>%
  gather(variable, value, -datetime)

# ggplot(buoy2017_do_vert, aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 month') +
#   final_theme

buoy2017_L1 <- buoy2017_L1 %>% 
  mutate_at(vars(upDO, lowDO),
            funs(case_when(. == -6999 ~ NA_real_,
                           . == 0 ~ NA_real_,
                           TRUE ~ .)))

buoy2017_do_vert <- buoy2017_L1 %>% 
  select(datetime, upDO, lowDO) %>%
  gather(variable, value, -datetime)

ggplot(buoy2017_do_vert, aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  scale_x_datetime(date_minor_breaks = '1 month') +
  final_theme

# ggplot(subset(buoy2017_do_vert,
#               subset=(datetime >= as.POSIXct('2017-01-01', tz='UTC') & datetime < as.POSIXct('2017-02-01', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme
# 
# ggplot(subset(buoy2017_do_vert,
#               subset=(datetime >= as.POSIXct('2017-02-01', tz='UTC') & datetime < as.POSIXct('2017-03-01', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme
# 
# ggplot(subset(buoy2017_do_vert,
#               subset=(datetime >= as.POSIXct('2017-03-01', tz='UTC') & datetime < as.POSIXct('2017-04-01', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme
# 
# ggplot(subset(buoy2017_do_vert,
#               subset=(datetime >= as.POSIXct('2017-03-06', tz='UTC') & datetime < as.POSIXct('2017-03-07', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme
# 
# ggplot(subset(buoy2017_L1,
#               subset=(datetime >= as.POSIXct('2017-04-21', tz='UTC') & datetime < as.POSIXct('2017-04-22', tz='UTC'))),
#        aes(x=datetime, y=PAR)) +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme

#data gap - buoy out of water
buoy2017_L1 <- buoy2017_L1 %>% 
  mutate(location = case_when(datetime < as.POSIXct('2017-03-06 10:10') ~ 'harbor',
                              datetime >= as.POSIXct('2017-03-06 10:10') & datetime < as.POSIXct('2017-04-21 11:00') ~ 'offline',
                              TRUE ~ 'harbor'))
  

# ggplot(subset(buoy2017_do_vert,
#               subset=(datetime >= as.POSIXct('2017-04-01', tz='UTC') & datetime < as.POSIXct('2017-05-01', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme
# 
# ggplot(subset(buoy2017_do_vert,
#               subset=(datetime >= as.POSIXct('2017-04-28', tz='UTC') & datetime < as.POSIXct('2017-04-29', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme

#remove errant data - will deal with low do when buoy moves
buoy2017_L1 <- buoy2017_L1 %>% 
  mutate_at(vars(upDO),
            funs(case_when(datetime >= as.POSIXct('2017-04-20', tz='UTC') & datetime < as.POSIXct('2017-04-28 16:00', tz='UTC') ~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate(upper_do_flag = case_when(datetime == as.POSIXct('2017-04-28 16:00', tz='UTC') ~ 'wp',
                                   TRUE ~ ''))

buoy2017_do_vert_L1 <- buoy2017_L1 %>% 
  select(datetime, upDO, lowDO) %>%
  gather(variable, value, -datetime)

# ggplot(subset(buoy2017_do_vert_L1,
#               subset=(datetime >= as.POSIXct('2017-04-01', tz='UTC') & datetime < as.POSIXct('2017-05-01', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme
# 
# ggplot(subset(buoy2017_do_vert_L1,
#               subset=(datetime >= as.POSIXct('2017-05-01', tz='UTC') & datetime < as.POSIXct('2017-06-01', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme
# 
# ggplot(subset(buoy2017_do_vert_L1,
#               subset=(datetime >= as.POSIXct('2017-05-17', tz='UTC') & datetime < as.POSIXct('2017-05-18', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme

#buoy move 5-17
buoy2017_L1 <- buoy2017_L1 %>% 
  mutate_at(vars(upDO),
            funs(case_when(datetime >= as.POSIXct('2017-05-17 9:20', tz='UTC') & datetime < as.POSIXct('2017-05-17 10:50', tz='UTC') ~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate(location = case_when(datetime >= as.POSIXct('2017-05-17 9:20', tz='UTC') & datetime < as.POSIXct('2017-05-17 10:50', tz='UTC') ~ 'in transit',
                              datetime >= as.POSIXct('2017-05-17 10:50', tz='UTC') ~ 'loon',
                              TRUE ~ location))

buoy2017_do_vert_L1 <- buoy2017_L1 %>% 
  select(datetime, upDO, lowDO) %>%
  gather(variable, value, -datetime)

# ggplot(subset(buoy2017_do_vert_L1,
#               subset=(datetime >= as.POSIXct('2017-05-18', tz='UTC') & datetime < as.POSIXct('2017-05-19', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme
# 
# #buoy move 5-19
# ggplot(subset(buoy2017_do_vert_L1,
#               subset=(datetime >= as.POSIXct('2017-05-19', tz='UTC') & datetime < as.POSIXct('2017-05-20', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme
# 
# ggplot(subset(buoy2017_do_vert_L1,
#               subset=(datetime >= as.POSIXct('2017-05-23', tz='UTC') & datetime < as.POSIXct('2017-05-24', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme
# 
# ggplot(subset(buoy2017_do_vert_L1,
#               subset=(datetime >= as.POSIXct('2017-06-01', tz='UTC') & datetime < as.POSIXct('2017-07-01', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme
# 
# ggplot(subset(buoy2017_do_vert_L1,
#               subset=(datetime >= as.POSIXct('2017-06-14', tz='UTC') & datetime < as.POSIXct('2017-06-15', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme

buoy2017_L1 <- buoy2017_L1 %>% 
  mutate_at(vars(upDO),
            funs(case_when(datetime >= as.POSIXct('2017-05-19 9:00', tz='UTC') & datetime < as.POSIXct('2017-05-23 13:50', tz='UTC') ~ NA_real_,
                           datetime >= as.POSIXct('2017-06-14 12:00', tz='UTC') & datetime < as.POSIXct('2017-06-14 14:40', tz='UTC') ~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate_at(vars(lowDO),
            funs(case_when(datetime < as.POSIXct('2017-06-14 15:30', tz='UTC')~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate(location = case_when(datetime >= as.POSIXct('2017-05-19 9:00', tz='UTC') & datetime < as.POSIXct('2017-05-19 9:50', tz='UTC') ~ 'in transit',
                              datetime >= as.POSIXct('2017-05-19 9:50', tz='UTC') & datetime < as.POSIXct('2017-05-23 13:50', tz='UTC') ~ 'harbor, water sensors offline',
                              datetime >= as.POSIXct('2017-05-23 13:50', tz='UTC') & datetime < as.POSIXct('2017-06-14 12:00', tz='UTC')~ 'harbor', 
                              datetime >= as.POSIXct('2017-06-14 12:00', tz='UTC') & datetime < as.POSIXct('2017-06-14 13:10', tz='UTC') ~ 'in transit',
                              datetime >= as.POSIXct('2017-06-14 13:10', tz='UTC') ~ 'loon',
                              TRUE ~ location)) %>% 
  mutate(upper_do_flag = case_when(datetime == as.POSIXct('2017-05-23 13:50', tz='UTC') ~ 'wp',
                                   datetime == as.POSIXct('2017-06-14 13:10', tz='UTC') ~ 'wp',
                                   TRUE ~ upper_do_flag)) %>% 
  mutate(lower_do_flag = case_when(datetime == as.POSIXct('2017-06-14 13:10', tz='UTC') ~ 'wp',
                                   TRUE ~ ''))
buoy2017_do_vert_L1 <- buoy2017_L1 %>% 
  select(datetime, upDO, lowDO, location) %>%
  gather(variable, value, -datetime, -location)

# ggplot(subset(buoy2017_do_vert_L1,
#               subset=(datetime >= as.POSIXct('2017-05-01', tz='UTC') & datetime < as.POSIXct('2017-07-01', tz='UTC'))),
#        aes(x=datetime, y=value, color = location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme
# 
# ggplot(subset(buoy2017_do_vert_L1,
#               subset=(datetime >= as.POSIXct('2017-06-01', tz='UTC') & datetime < as.POSIXct('2017-07-01', tz='UTC'))),
#        aes(x=datetime, y=value, color = location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme
# 
# ggplot(subset(buoy2017_do_vert_L1,
#               subset=(datetime >= as.POSIXct('2017-07-01', tz='UTC') & datetime < as.POSIXct('2017-08-01', tz='UTC'))),
#        aes(x=datetime, y=value, color = location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme


#flag do data as intermittent beginning jun 26 through jul 2
buoy2017_L1 <- buoy2017_L1 %>% 
  mutate(upper_do_flag = case_when(datetime >= as.POSIXct('2017-06-26', tz='UTC') & datetime < as.POSIXct('2017-07-03', tz='UTC') ~ 'i',
                           TRUE ~ .)) %>% 
  mutate(lower_do_flag = case_when(datetime >= as.POSIXct('2017-06-26', tz='UTC') & datetime < as.POSIXct('2017-07-03', tz='UTC') ~ 'i',
                                   TRUE ~ lower_do_flag)) %>% 
buoy2017_do_vert_L1 <- buoy2017_L1 %>% 
  select(datetime, upDO, lowDO, location) %>%
  gather(variable, value, -datetime, -location)

# ggplot(subset(buoy2017_do_vert_L1,
#               subset=(datetime >= as.POSIXct('2017-08-01', tz='UTC') & datetime < as.POSIXct('2017-09-01', tz='UTC'))),
#        aes(x=datetime, y=value, color = location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme
# 
# ggplot(subset(buoy2017_do_vert_L1,
#               subset=(datetime >= as.POSIXct('2017-09-01', tz='UTC') & datetime < as.POSIXct('2017-10-01', tz='UTC'))),
#        aes(x=datetime, y=value, color = location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme
# 
# ggplot(subset(buoy2017_do_vert_L1,
#               subset=(datetime >= as.POSIXct('2017-10-01', tz='UTC') & datetime < as.POSIXct('2017-11-01', tz='UTC'))),
#        aes(x=datetime, y=value, color = location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme
# 
# ggplot(subset(buoy2017_do_vert_L1,
#               subset=(datetime >= as.POSIXct('2017-10-19', tz='UTC') & datetime < as.POSIXct('2017-10-20', tz='UTC'))),
#        aes(x=datetime, y=value, color = location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme

buoy2017_L1 <- buoy2017_L1 %>% 
  mutate_at(vars(lowDO, upDO),
            funs(case_when(datetime >= as.POSIXct('2017-10-19 9:20', tz='UTC') ~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate(location = case_when(datetime >= as.POSIXct('2017-10-19 9:20', tz='UTC') & datetime < as.POSIXct('2017-10-19 10:30', tz='UTC') ~ 'in transit',
                              datetime >= as.POSIXct('2017-10-19 10:30', tz='UTC') ~ 'harbor',
                              TRUE ~ location))
buoy2017_do_vert_L1 <- buoy2017_L1 %>%
  select(datetime, location, lowDO, upDO) %>%
  gather(variable, value, -datetime, -location)

# ggplot(buoy2017_do_vert_L1,
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 month') +
#   labs(title = 'do 2017, clean') +
#   final_theme +
#   scale_color_colorblind()

buoy2017_updo_vert_L1 <- buoy2017_L1 %>%
  select(datetime, location, upDO, upper_do_flag) %>%
  gather(variable, value, -datetime, -location, -upper_do_flag)

ggplot(buoy2017_updo_vert_L1,
       aes(x=datetime, y=value, color=location, shape = upper_do_flag)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  scale_x_datetime(date_minor_breaks = '1 month') +
  labs(title = 'do 2017, clean') +
  final_theme +
  scale_color_colorblind()

buoy2017_lowdo_vert_L1 <- buoy2017_L1 %>%
  select(datetime, location, upDO, lower_do_flag) %>%
  gather(variable, value, -datetime, -location, -lower_do_flag)

ggplot(buoy2017_lowdo_vert_L1,
       aes(x=datetime, y=value, color=location, shape = lower_do_flag)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  scale_x_datetime(date_minor_breaks = '1 month') +
  labs(title = 'do 2017, clean') +
  final_theme +
  scale_color_colorblind()

rm(buoy2017_do_vert, buoy2017_do_vert_L1, buoy2017_updo_vert_L1, buoy2017_lowdo_vert_L1)

####CHLA####
# buoy2017_chla_vert <- buoy2017_L1 %>% 
#   select(datetime, location, chla) %>%
#   gather(variable, value, -datetime, -location)
# 
# ggplot(buoy2017_chla_vert, aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 month') +
#   final_theme

#recode NA
buoy2017_L1 <- buoy2017_L1 %>% 
  mutate_at(vars(chla),
            funs(case_when(. == -6999 ~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate(Chlor_UGL = case_when(Chlor_UGL < 0 ~ 0,
                               Chlor_UGL == 587 ~ NA_real_,
                               TRUE ~ Chlor_UGL)) %>% 
  mutate(SpecCond = case_when(SpecCond == 6999 ~ NA_real_,
                              TRUE ~ SpecCond)) 
buoy2017_chla_vert <- buoy2017_L1 %>% 
  select(datetime, location, chla) %>%
  gather(variable, value, -datetime, -location)

ggplot(buoy2017_chla_vert, aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  scale_x_datetime(date_minor_breaks = '1 month') +
  final_theme

# ggplot(subset(buoy2017_chla_vert, subset=(datetime>=as.POSIXct('2017-04-01', tz='UTC') & datetime < as.POSIXct('2017-05-01', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'chla apr 2017, NAs recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme
# 
# #sensor online and in water apr 28
# ggplot(subset(buoy2017_chla_vert, subset=(datetime>=as.POSIXct('2017-04-28', tz='UTC') & datetime < as.POSIXct('2017-04-29', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'chla apr 2017, NAs recoded') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme

buoy2017_L1 <- buoy2017_L1 %>% 
  mutate_at(vars(chla),
            funs(case_when(datetime < as.POSIXct('2017-04-28 15:00', tz='UTC')~ NA_real_,
                           TRUE ~ .)))  %>% 
  mutate(chla_flag = case_when(datetime == as.POSIXct('2017-04-28 15:00', tz='UTC')~ 'wp',
                               TRUE ~ '')) %>% 
  mutate(cond_flag = case_when(datetime == as.POSIXct('2017-04-28 15:00', tz='UTC')~ 'wp',
                               TRUE ~ ''))

buoy2017_chla_vert_b <- buoy2017_L1 %>% 
  select(datetime, location, chla) %>%
  gather(variable, value, -datetime, -location)

# ggplot(subset(buoy2017_chla_vert_b, subset=(datetime>=as.POSIXct('2017-04-01', tz='UTC') & datetime < as.POSIXct('2017-05-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'chla apr 2017, clean') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy2017_chla_vert, subset=(datetime>=as.POSIXct('2017-05-01', tz='UTC') & datetime < as.POSIXct('2017-06-01', tz='UTC'))),
#        aes(x=datetime, y=value, color = location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'chla apr 2017, NAs recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme
# 
# #may 16 chla and cond probe removed for calibration
# ggplot(subset(buoy2017_chla_vert, subset=(datetime>=as.POSIXct('2017-05-16', tz='UTC') & datetime < as.POSIXct('2017-05-17', tz='UTC'))),
#        aes(x=datetime, y=value, color = location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'chla may 2017, NAs recoded') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme
# 
# #may 17 sonde reattached, buoy moved to loon
# ggplot(subset(buoy2017_chla_vert, subset=(datetime>=as.POSIXct('2017-05-17', tz='UTC') & datetime < as.POSIXct('2017-05-18', tz='UTC'))),
#        aes(x=datetime, y=value, color = location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'chla may 2017, clean') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme
# 
# #may 19 buoy back to harbor, sensors offline
# ggplot(subset(buoy2017_chla_vert, subset=(datetime>=as.POSIXct('2017-05-19', tz='UTC') & datetime < as.POSIXct('2017-05-20', tz='UTC'))),
#        aes(x=datetime, y=value, color = location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'chla may 2017, NAs recoded') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme
# 
# #may 23 sensors back online
# ggplot(subset(buoy2017_chla_vert, subset=(datetime>=as.POSIXct('2017-05-23', tz='UTC') & datetime < as.POSIXct('2017-05-24', tz='UTC'))),
#        aes(x=datetime, y=value, color = location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'chla may 2017, NAs recoded') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme
# 
# #starting may 31, all sensors measuring only integers
# ggplot(subset(buoy2017_chla_vert, subset=(datetime>=as.POSIXct('2017-05-31', tz='UTC') & datetime < as.POSIXct('2017-06-01', tz='UTC'))),
#        aes(x=datetime, y=value, color = location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'chla may 2017, NAs recoded') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme

buoy2017_L1 <- buoy2017_L1 %>% 
  mutate_at(vars(chla),
            funs(case_when(datetime >= as.POSIXct('2017-05-16 12:50', tz='UTC') & datetime < as.POSIXct('2017-05-16 13:10', tz='UTC')~ NA_real_,
                           location == 'in transit' ~ NA_real_,
                          location == 'harbor, water sensors offline' ~NA_real_,
                           TRUE ~ .))) %>% 
  mutate(cond_flag = case_when(datetime == as.POSIXct('2017-05-17 8:20') ~ 'ct',
                               cond_flag == '' & !is.na(SpecCond) & datetime>as.POSIXct('2017-05-17', tz='UTC') ~ 't',
                               TRUE ~ cond_flag)) %>% 
  mutate(chla_flag = case_when(!is.na(Chlor_UGL) & datetime>as.POSIXct('2017-05-31 11:00', tz='UTC') ~ 't',
                                        TRUE ~ chla_flag))

buoy2017_chla_vert_b <- buoy2017_L1 %>% 
  select(datetime, location, chla) %>%
  gather(variable, value, -datetime, -location)

# ggplot(subset(buoy2017_chla_vert_b, subset=(datetime>=as.POSIXct('2017-05-01', tz='UTC') & datetime < as.POSIXct('2017-06-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'chla may 2017, clean') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy2017_chla_vert_b, subset=(datetime>=as.POSIXct('2017-06-01', tz='UTC') & datetime < as.POSIXct('2017-07-01', tz='UTC'))),
#        aes(x=datetime, y=value, color = location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'chla jun 2017, NAs recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme
# 
# #jun 14 buoy back to loon
# ggplot(subset(buoy2017_chla_vert_b, subset=(datetime>=as.POSIXct('2017-06-14', tz='UTC') & datetime < as.POSIXct('2017-06-15', tz='UTC'))),
#        aes(x=datetime, y=value, color = location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'chla jun 2017, NAs recoded') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme

buoy2017_L1 <- buoy2017_L1 %>% 
  mutate_at(vars(chla),
            funs(case_when(datetime >= as.POSIXct('2017-06-14 13:00', tz='UTC') & datetime < as.POSIXct('2017-06-14 14:10', tz='UTC')~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate_at(vars(chla_flag, cond_flag),
            funs(case_when(datetime==as.POSIXct('2017-06-14 14:20', tz='UTC') ~ 'twp',
                           TRUE ~ .)))
buoy2017_chla_vert_b <- buoy2017_L1 %>% 
  select(datetime, location, chla) %>%
  gather(variable, value, -datetime, -location)

# ggplot(subset(buoy2017_chla_vert_b, subset=(datetime>=as.POSIXct('2017-06-01', tz='UTC') & datetime < as.POSIXct('2017-07-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'chla jun 2017, clean') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy2017_chla_vert, subset=(datetime>=as.POSIXct('2017-07-01', tz='UTC') & datetime < as.POSIXct('2017-08-01', tz='UTC'))),
#        aes(x=datetime, y=value, color = location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'chla jul 2017, NAs recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme
# 
# ggplot(subset(buoy2017_chla_vert, subset=(datetime>=as.POSIXct('2017-08-01', tz='UTC') & datetime < as.POSIXct('2017-09-01', tz='UTC'))),
#        aes(x=datetime, y=value, color = location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'chla aug 2017, NAs recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme
# 
# ggplot(subset(buoy2017_chla_vert, subset=(datetime>=as.POSIXct('2017-09-01', tz='UTC') & datetime < as.POSIXct('2017-10-01', tz='UTC'))),
#        aes(x=datetime, y=value, color =location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'chla sept 2017, NAs recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme
# 
# ggplot(subset(buoy2017_chla_vert, subset=(datetime>=as.POSIXct('2017-10-01', tz='UTC') & datetime < as.POSIXct('2017-11-01', tz='UTC'))),
#        aes(x=datetime, y=value, color = location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'chla oct 2017, NAs recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme
# 
# #oct 19 buoy moved to harbor
#   ggplot(subset(buoy2017_chla_vert_b, subset=(datetime>=as.POSIXct('2017-10-19', tz='UTC') & datetime < as.POSIXct('2017-10-20', tz='UTC'))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'chla oct 2017, NAs recoded') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme +
#   scale_color_colorblind()
# 
# #oct 23 sonde back online
# ggplot(subset(buoy2017_chla_vert, subset=(datetime>=as.POSIXct('2017-10-23', tz='UTC') & datetime < as.POSIXct('2017-10-24', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'chla oct 2017, NAs recoded') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme

buoy2017_L1 <- buoy2017_L1 %>% 
  mutate_at(vars(chla),
            funs(case_when(datetime >= as.POSIXct('2017-10-19 9:30', tz='UTC') & datetime < as.POSIXct('2017-10-23 14:00', tz='UTC')~ NA_real_,
                           TRUE ~ .))) 
buoy2017_chla_vert_b <- buoy2017_L1 %>% 
  select(datetime, location, chla) %>%
  gather(variable, value, -datetime, -location)

# ggplot(subset(buoy2017_chla_vert_b, subset=(datetime>=as.POSIXct('2017-10-01', tz='UTC') & datetime < as.POSIXct('2017-11-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'chla oct 2017, clean') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy2017_chla_vert, subset=(datetime>=as.POSIXct('2017-11-01', tz='UTC') & datetime < as.POSIXct('2017-12-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'chla nov 2017, NAs recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy2017_chla_vert, subset=(datetime>=as.POSIXct('2017-11-13', tz='UTC') & datetime < as.POSIXct('2017-11-14', tz='UTC'))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'chla nov 2017, NAs recoded') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme +
#   scale_color_colorblind()

#sonde removed nov20 - but going to end spec cond at same time as other data streams here
buoy2017_L1 <- buoy2017_L1 %>% 
  mutate_at(vars(chla),
            funs(case_when(datetime >= as.POSIXct('2017-11-13 12:00', tz='UTC') ~ NA_real_,
                           TRUE ~ .))) 
buoy2017_chla_vert_b <- buoy2017_L1 %>% 
  select(datetime, location, chla) %>%
  gather(variable, value, -datetime, -location)

# ggplot(subset(buoy2017_chla_vert_b, subset=(datetime>=as.POSIXct('2017-11-01', tz='UTC') & datetime < as.POSIXct('2017-12-01', tz='UTC'))), 
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'chla nov 2017, clean') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()

#add flag as suspect above 10 ugl
buoy2017_L1 <- buoy2017_L1 %>% 
  mutate(chla_flag = case_when(Chlor_UGL>10 ~ paste(chla_flag, 's', sep = ''),
                               TRUE ~ chla_flag))

#plot with flags
buoy2017_chla_b <- buoy2017_L1 %>% 
  select(datetime, location, Chlor_UGL, chla_flag) %>%
  gather(variable, value, -datetime, -location, -chla_flag)

ggplot(buoy2017_chla_b, 
       aes(x=datetime, y=value, color=location, shape = chla_flag)) +
  geom_point() +
  labs(title = 'chla 2017, clean') +
  scale_x_datetime(date_minor_breaks = '1 month') +
  final_theme +
  scale_color_colorblind()

buoy2017_cond_b <- buoy2017_L1 %>% 
  select(datetime, location, SpecCond, cond_flag) %>%
  gather(variable, value, -datetime, -location, -cond_flag)

ggplot(buoy2017_cond_b, 
       aes(x=datetime, y=value, color=location, shape = cond_flag)) +
  geom_point() +
  labs(title = 'chla 2017, clean') +
  scale_x_datetime(date_minor_breaks = '1 month') +
  final_theme +
  scale_color_colorblind()

rm(buoy2017_chla_b, buoy2017_chla_vert, buoy2017_chla_vert_b, buoy2017_cond_b)


####wind####
# buoy_wind_vert <- buoy2017_L1 %>% 
#   select(datetime, AveWindDir, AveWindSp, MaxWindDir, MaxWindSp, location) %>% 
#   gather(variable, value, -datetime, -location)
# 
# ggplot(buoy_wind_vert, 
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'wind 2017, raw') +
#   scale_x_datetime(date_minor_breaks = '1 month') +
#   final_theme +
#   scale_color_colorblind()

#recode offline/in transit
buoy2017_L1 <- buoy2017_L1 %>% 
  mutate_at(vars(AveWindDir, AveWindSp, MaxWindDir, MaxWindSp),
            funs(case_when(location == 'offline' ~ NA_real_,
                           location == 'in transit' ~ NA_real_,
                           TRUE ~ .)))

buoy_wind_vert <- buoy2017_L1 %>% 
  select(datetime, AveWindDir, AveWindSp, MaxWindDir, MaxWindSp, location) %>% 
  gather(variable, value, -datetime, -location)

ggplot(buoy_wind_vert, 
       aes(x=datetime, y=value, color=location)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title = 'wind 2017, NAs recoded') +
  scale_x_datetime(date_minor_breaks = '1 month') +
  final_theme +
  scale_color_colorblind()

# ggplot(subset(buoy_wind_vert, 
#               subset=(datetime >= as.POSIXct('2017-01-01', tz='UTC') & datetime < as.POSIXct('2017-02-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'jan wind 2017, NAs recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# #sensor frozen jan 4
# ggplot(subset(buoy_wind_vert, 
#               subset=(datetime >= as.POSIXct('2017-01-04', tz='UTC') & datetime < as.POSIXct('2017-01-05', tz='UTC'))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'jan wind 2017, NAs recoded') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme +
#   scale_color_colorblind()
# 
# #sensor frozen jan 17-19
# ggplot(subset(buoy_wind_vert, 
#               subset=(datetime >= as.POSIXct('2017-01-17', tz='UTC') & datetime < as.POSIXct('2017-01-18 3:00', tz='UTC'))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'jan wind 2017, NAs recoded') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy_wind_vert, 
#               subset=(datetime >= as.POSIXct('2017-01-19', tz='UTC') & datetime < as.POSIXct('2017-01-20', tz='UTC'))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'jan wind 2017, NAs recoded') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme +
#   scale_color_colorblind()

buoy2017_L1 <- buoy2017_L1 %>% 
  mutate_at(vars(AveWindDir, AveWindSp, MaxWindDir, MaxWindSp),
            funs(case_when(datetime >= as.POSIXct('2017-01-04 3:50', tz='UTC') & datetime < as.POSIXct('2017-01-04 8:20', tz='UTC') ~ NA_real_,
                           datetime >= as.POSIXct('2017-01-17 23:50', tz='UTC') & datetime < as.POSIXct('2017-01-19 10:10', tz='UTC') ~ NA_real_,
                           TRUE ~ .)))

buoy_wind_vert_L1 <- buoy2017_L1 %>% 
  select(datetime, AveWindDir, AveWindSp, MaxWindDir, MaxWindSp, location) %>% 
  gather(variable, value, -datetime, -location)

# ggplot(subset(buoy_wind_vert_L1, 
#               subset=(datetime >= as.POSIXct('2017-01-01', tz='UTC') & datetime < as.POSIXct('2017-02-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'jan wind 2017, clean') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy_wind_vert, 
#               subset=(datetime >= as.POSIXct('2017-02-01', tz='UTC') & datetime < as.POSIXct('2017-03-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'feb wind 2017, NAs recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# #sensor frozen feb 9
# ggplot(subset(buoy_wind_vert, 
#               subset=(datetime >= as.POSIXct('2017-02-09', tz='UTC') & datetime < as.POSIXct('2017-02-10', tz='UTC'))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'feb wind 2017, NAs recoded') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme +
#   scale_color_colorblind()
# 
# #sensor frozen feb 13
# ggplot(subset(buoy_wind_vert, 
#               subset=(datetime >= as.POSIXct('2017-02-13', tz='UTC') & datetime < as.POSIXct('2017-02-14', tz='UTC'))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'feb wind 2017, NAs recoded') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme +
#   scale_color_colorblind()
# 
# #sensor frozen feb 15-17
# ggplot(subset(buoy_wind_vert, 
#               subset=(datetime >= as.POSIXct('2017-02-15', tz='UTC') & datetime < as.POSIXct('2017-02-16', tz='UTC'))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'feb wind 2017, NAs recoded') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme +
#   scale_color_colorblind()
# ggplot(subset(buoy_wind_vert, 
#               subset=(datetime >= as.POSIXct('2017-02-17', tz='UTC') & datetime < as.POSIXct('2017-02-18', tz='UTC'))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'feb wind 2017, NAs recoded') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme +
#   scale_color_colorblind()

buoy2017_L1 <- buoy2017_L1 %>% 
  mutate_at(vars(AveWindDir, AveWindSp, MaxWindDir, MaxWindSp),
            funs(case_when(datetime >= as.POSIXct('2017-02-09 8:50', tz='UTC') & datetime < as.POSIXct('2017-02-09 13:50', tz='UTC') ~ NA_real_,
                           datetime >= as.POSIXct('2017-02-13 4:10', tz='UTC') & datetime < as.POSIXct('2017-02-13 9:10', tz='UTC') ~ NA_real_,
                           datetime >= as.POSIXct('2017-02-15 19:10', tz='UTC') & datetime < as.POSIXct('2017-02-17 9:20', tz='UTC') ~ NA_real_,
                           TRUE ~ .)))

buoy_wind_vert_L1 <- buoy2017_L1 %>% 
  select(datetime, AveWindDir, AveWindSp, MaxWindDir, MaxWindSp, location) %>% 
  gather(variable, value, -datetime, -location)

# ggplot(subset(buoy_wind_vert_L1, 
#               subset=(datetime >= as.POSIXct('2017-02-01', tz='UTC') & datetime < as.POSIXct('2017-03-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'feb wind 2017, clean') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy_wind_vert, 
#               subset=(datetime >= as.POSIXct('2017-03-01', tz='UTC') & datetime < as.POSIXct('2017-04-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'mar wind 2017, NAs recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy_wind_vert, 
#               subset=(datetime >= as.POSIXct('2017-04-01', tz='UTC') & datetime < as.POSIXct('2017-05-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'apr wind 2017, NAs recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy_wind_vert, 
#               subset=(datetime >= as.POSIXct('2017-05-01', tz='UTC') & datetime < as.POSIXct('2017-06-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'may wind 2017, NAs recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy_wind_vert, 
#               subset=(datetime >= as.POSIXct('2017-06-01', tz='UTC') & datetime < as.POSIXct('2017-07-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'jun wind 2017, NAs recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy_wind_vert, 
#               subset=(datetime >= as.POSIXct('2017-07-01', tz='UTC') & datetime < as.POSIXct('2017-08-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'jul wind 2017, NAs recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy_wind_vert, 
#               subset=(datetime >= as.POSIXct('2017-08-01', tz='UTC') & datetime < as.POSIXct('2017-09-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'aug wind 2017, NAs recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy_wind_vert, 
#               subset=(datetime >= as.POSIXct('2017-09-01', tz='UTC') & datetime < as.POSIXct('2017-10-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'sept wind 2017, NAs recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy_wind_vert, 
#               subset=(datetime >= as.POSIXct('2017-10-01', tz='UTC') & datetime < as.POSIXct('2017-11-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'oct wind 2017, NAs recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy_wind_vert, 
#               subset=(datetime >= as.POSIXct('2017-11-01', tz='UTC') & datetime < as.POSIXct('2017-12-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'nov wind 2017, NAs recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy_wind_vert, 
#               subset=(datetime >= as.POSIXct('2017-12-01', tz='UTC') & datetime < as.POSIXct('2018-01-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'dec wind 2017, NAs recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# #sensor frozen dec 9-10
# ggplot(subset(buoy_wind_vert, 
#               subset=(datetime >= as.POSIXct('2017-12-09 12:00', tz='UTC') & datetime < as.POSIXct('2017-12-11', tz='UTC'))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'dec wind 2017, NAs recoded') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme +
#   scale_color_colorblind()
# 
# #sensor frozen dec 23 - 26
# ggplot(subset(buoy_wind_vert, 
#               subset=(datetime >= as.POSIXct('2017-12-23', tz='UTC') & datetime < as.POSIXct('2017-12-24', tz='UTC'))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'dec wind 2017, NAs recoded') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme +
#   scale_color_colorblind()
# ggplot(subset(buoy_wind_vert, 
#               subset=(datetime >= as.POSIXct('2017-12-26', tz='UTC') & datetime < as.POSIXct('2017-12-27', tz='UTC'))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'dec wind 2017, NAs recoded') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme +
#   scale_color_colorblind()

buoy2017_L1 <- buoy2017_L1 %>% 
  mutate_at(vars(AveWindDir, AveWindSp, MaxWindDir, MaxWindSp),
            funs(case_when(datetime >= as.POSIXct('2017-12-09 22:40', tz='UTC') & datetime < as.POSIXct('2017-12-10 15:30', tz='UTC') ~ NA_real_,
                           datetime >= as.POSIXct('2017-12-23 10:50', tz='UTC') & datetime < as.POSIXct('2017-12-26 14:30', tz='UTC') ~ NA_real_,
                           TRUE ~ .)))

buoy_wind_vert_L1 <- buoy2017_L1 %>% 
  select(datetime, AveWindDir, AveWindSp, MaxWindDir, MaxWindSp, location) %>% 
  gather(variable, value, -datetime, -location)

# ggplot(subset(buoy_wind_vert_L1,
#               subset=(datetime >= as.POSIXct('2017-12-01', tz='UTC') & datetime < as.POSIXct('2018-01-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'dec wind 2017, clean') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()

ggplot(buoy_wind_vert_L1,
       aes(x=datetime, y=value, color=location)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title = 'wind 2017, clean') +
  scale_x_datetime(date_minor_breaks = '1 month') +
  final_theme +
  scale_color_colorblind()

rm(buoy_wind_vert, buoy_wind_vert_L1)

####PAR####
# ggplot(buoy2017_L1,
#        aes(x=datetime, y=PAR, color=location)) +
#   geom_point() +
#   labs(title = 'PAR 2017, raw') +
#   scale_x_datetime(date_minor_breaks = '1 month') +
#   final_theme +
#   scale_color_colorblind()

#recode when in transit or offline
buoy2017_L1 <- buoy2017_L1 %>% 
  mutate(PAR = case_when(location == 'offline' ~ NA_real_,
                         location == 'in transit' ~ NA_real_,
                         TRUE ~ PAR))
ggplot(buoy2017_L1,
       aes(x=datetime, y=PAR, color=location)) +
  geom_point() +
  labs(title = 'PAR 2017, NA recoded') +
  scale_x_datetime(date_minor_breaks = '1 month') +
  final_theme +
  scale_color_colorblind()

# ggplot(subset(buoy2017_L1,
#               subset=(datetime >= as.POSIXct('2017-01-01', tz='UTC') & datetime < as.POSIXct('2017-02-01', tz='UTC'))),
#        aes(x=datetime, y=PAR, color=location)) +
#   geom_point() +
#   labs(title = 'PAR 2017, NA recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy2017_L1,
#               subset=(datetime >= as.POSIXct('2017-02-01', tz='UTC') & datetime < as.POSIXct('2017-03-01', tz='UTC'))),
#        aes(x=datetime, y=PAR, color=location)) +
#   geom_point() +
#   labs(title = 'PAR 2017, NA recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()

buoy2017_L1 <- buoy2017_L1 %>% 
  mutate(PAR_flag = case_when(datetime >= as.POSIXct('2017-02-09', tz='UTC') & datetime < as.POSIXct('2017-02-18', tz='UTC') ~ 'o',
                              TRUE ~ ''))

# ggplot(subset(buoy2017_L1,
#               subset=(datetime >= as.POSIXct('2017-03-01', tz='UTC') & datetime < as.POSIXct('2017-04-01', tz='UTC'))),
#        aes(x=datetime, y=PAR, color=location)) +
#   geom_point() +
#   labs(title = 'PAR 2017, NA recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy2017_L1,
#               subset=(datetime >= as.POSIXct('2017-04-01', tz='UTC') & datetime < as.POSIXct('2017-05-01', tz='UTC'))),
#        aes(x=datetime, y=PAR, color=location)) +
#   geom_point() +
#   labs(title = 'PAR 2017, NA recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy2017_L1,
#               subset=(datetime >= as.POSIXct('2017-05-01', tz='UTC') & datetime < as.POSIXct('2017-06-01', tz='UTC'))),
#        aes(x=datetime, y=PAR, color=location)) +
#   geom_point() +
#   labs(title = 'PAR 2017, NA recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy2017_L1,
#               subset=(datetime >= as.POSIXct('2017-06-01', tz='UTC') & datetime < as.POSIXct('2017-07-01', tz='UTC'))),
#        aes(x=datetime, y=PAR, color=location)) +
#   geom_point() +
#   labs(title = 'PAR 2017, NA recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy2017_L1,
#               subset=(datetime >= as.POSIXct('2017-07-01', tz='UTC') & datetime < as.POSIXct('2017-08-01', tz='UTC'))),
#        aes(x=datetime, y=PAR, color=location)) +
#   geom_point() +
#   labs(title = 'PAR 2017, NA recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy2017_L1,
#               subset=(datetime >= as.POSIXct('2017-08-01', tz='UTC') & datetime < as.POSIXct('2017-09-01', tz='UTC'))),
#        aes(x=datetime, y=PAR, color=location)) +
#   geom_point() +
#   labs(title = 'PAR 2017, NA recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy2017_L1,
#               subset=(datetime >= as.POSIXct('2017-09-01', tz='UTC') & datetime < as.POSIXct('2017-10-01', tz='UTC'))),
#        aes(x=datetime, y=PAR, color=location)) +
#   geom_point() +
#   labs(title = 'PAR 2017, NA recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy2017_L1,
#               subset=(datetime >= as.POSIXct('2017-10-01', tz='UTC') & datetime < as.POSIXct('2017-11-01', tz='UTC'))),
#        aes(x=datetime, y=PAR, color=location)) +
#   geom_point() +
#   labs(title = 'PAR 2017, NA recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy2017_L1,
#               subset=(datetime >= as.POSIXct('2017-11-01', tz='UTC') & datetime < as.POSIXct('2017-12-01', tz='UTC'))),
#        aes(x=datetime, y=PAR, color=location)) +
#   geom_point() +
#   labs(title = 'PAR 2017, NA recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy2017_L1,
#               subset=(datetime >= as.POSIXct('2017-12-01', tz='UTC') & datetime < as.POSIXct('2018-01-01', tz='UTC'))),
#        aes(x=datetime, y=PAR, color=location)) +
#   geom_point() +
#   labs(title = 'PAR 2017, NA recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()


#### Air temp ####
# ggplot(buoy2017_L1,
#        aes(x=datetime, y=AirTempC, color=location)) +
#   geom_point() +
#   labs(title = 'air temp 2017, NA recoded') +
#   scale_x_datetime(date_minor_breaks = '1 month') +
#   final_theme +
#   scale_color_colorblind()

#recode when in transit or offline
buoy2017_L1 <- buoy2017_L1 %>% 
  mutate(AirTempC = case_when(location == 'offline' ~ NA_real_,
                         location == 'in transit' ~ NA_real_,
                         TRUE ~ AirTempC))

# ggplot(buoy2017_L1,
#        aes(x=datetime, y=AirTempC, color=location)) +
#   geom_point() +
#   labs(title = 'air temp 2017, NA recoded') +
#   scale_x_datetime(date_minor_breaks = '1 month') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy2017_L1,
#               subset=(datetime >= as.POSIXct('2017-01-01', tz='UTC') & datetime < as.POSIXct('2017-02-01', tz='UTC'))),
#        aes(x=datetime, y=AirTempC, color=location)) +
#   geom_point() +
#   labs(title = 'air temp 2017, NA recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy2017_L1,
#               subset=(datetime >= as.POSIXct('2017-02-01', tz='UTC') & datetime < as.POSIXct('2017-03-01', tz='UTC'))),
#        aes(x=datetime, y=AirTempC, color=location)) +
#   geom_point() +
#   labs(title = 'air temp 2017, NA recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy2017_L1,
#               subset=(datetime >= as.POSIXct('2017-03-01', tz='UTC') & datetime < as.POSIXct('2017-04-01', tz='UTC'))),
#        aes(x=datetime, y=AirTempC, color=location)) +
#   geom_point() +
#   labs(title = 'air temp 2017, NA recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy2017_L1,
#               subset=(datetime >= as.POSIXct('2017-04-01', tz='UTC') & datetime < as.POSIXct('2017-05-01', tz='UTC'))),
#        aes(x=datetime, y=AirTempC, color=location)) +
#   geom_point() +
#   labs(title = 'air temp 2017, NA recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy2017_L1,
#               subset=(datetime >= as.POSIXct('2017-05-01', tz='UTC') & datetime < as.POSIXct('2017-06-01', tz='UTC'))),
#        aes(x=datetime, y=AirTempC, color=location)) +
#   geom_point() +
#   labs(title = 'air temp 2017, NA recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy2017_L1,
#               subset=(datetime >= as.POSIXct('2017-06-01', tz='UTC') & datetime < as.POSIXct('2017-07-01', tz='UTC'))),
#        aes(x=datetime, y=AirTempC, color=location)) +
#   geom_point() +
#   labs(title = 'air temp 2017, NA recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy2017_L1,
#               subset=(datetime >= as.POSIXct('2017-07-01', tz='UTC') & datetime < as.POSIXct('2017-08-01', tz='UTC'))),
#        aes(x=datetime, y=AirTempC, color=location)) +
#   geom_point() +
#   labs(title = 'air temp 2017, NA recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy2017_L1,
#               subset=(datetime >= as.POSIXct('2017-08-01', tz='UTC') & datetime < as.POSIXct('2017-09-01', tz='UTC'))),
#        aes(x=datetime, y=AirTempC, color=location)) +
#   geom_point() +
#   labs(title = 'air temp 2017, NA recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy2017_L1,
#               subset=(datetime >= as.POSIXct('2017-09-01', tz='UTC') & datetime < as.POSIXct('2017-10-01', tz='UTC'))),
#        aes(x=datetime, y=AirTempC, color=location)) +
#   geom_point() +
#   labs(title = 'air temp 2017, NA recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy2017_L1,
#               subset=(datetime >= as.POSIXct('2017-10-01', tz='UTC') & datetime < as.POSIXct('2017-11-01', tz='UTC'))),
#        aes(x=datetime, y=AirTempC, color=location)) +
#   geom_point() +
#   labs(title = 'air temp 2017, NA recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy2017_L1,
#               subset=(datetime >= as.POSIXct('2017-11-01', tz='UTC') & datetime < as.POSIXct('2017-12-01', tz='UTC'))),
#        aes(x=datetime, y=AirTempC, color=location)) +
#   geom_point() +
#   labs(title = 'air temp 2017, NA recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy2017_L1,
#               subset=(datetime >= as.POSIXct('2017-12-01', tz='UTC') & datetime < as.POSIXct('2018-01-01', tz='UTC'))),
#        aes(x=datetime, y=AirTempC, color=location)) +
#   geom_point() +
#   labs(title = 'air temp 2017, NA recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()

# #for kicks, see what happens when you add the chlor_rfu to the temp string
# buoy2017_L1 %>% 
#   select(datetime, Chlor_RFU, TempC_0m, TempC_1m, TempC_2m, TempC_3m, TempC_4m, TempC_5m, TempC_6m, TempC_7m, TempC_8m, TempC_9m, location) %>% 
#   gather(variable, value, -datetime, -location) %>% 
#   ggplot(., aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   final_theme +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   scale_x_datetime(date_minor_breaks = '1 month')
# 
# buoy2017_L1 %>% 
#   select(datetime, Chlor_RFU, TempC_1m, TempC_2m, location) %>% 
#   gather(variable, value, -datetime, -location) %>% 
#   filter(datetime >= as.POSIXct('2017-08-01', tz='UTC') & datetime < as.POSIXct('2017-10-01')) %>% 
#   ggplot(., aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   final_theme +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   scale_x_datetime(date_minor_breaks = '1 month')


#### EXPORT L1 DATA STREAMS ####
buoy2017_L1 <-  buoy2017_L1 %>% 
  mutate_at(vars(upper_do_flag, lower_do_flag, chla_flag, cond_flag, PAR_flag),
            funs(case_when(location == 'offline' ~ '',
                           TRUE ~ .)))

#export L1 tempstring file
# buoy2017_L1 %>%
#   select(datetime, TempC_0m, TempC_1m, TempC_2m, TempC_3m, TempC_4m, TempC_5m, TempC_6m, TempC_7m, TempC_8m, TempC_9m, location) %>%
#   rename(TempC_9p85m = 'TempC_9m',
#          TempC_8p85m = 'TempC_8m',
#          TempC_7p85m = 'TempC_7m',
#          TempC_6p85m = 'TempC_6m',
#          TempC_5p85m = 'TempC_5m',
#          TempC_4p85m = 'TempC_4m',
#          TempC_3p85m = 'TempC_3m',
#          TempC_2p85m = 'TempC_2m',
#          TempC_1p85m = 'TempC_1m',
#          TempC_0p85m = 'TempC_0m') %>%
#   mutate(datetime = as.character(datetime)) %>%
#   write_csv(., 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2017_tempstring_L1_corrdepth.csv')

#crete vertical dataset
buoy_2017_L1_vert <- buoy2017_L1 %>%
  select(datetime, location, TempC_0m, TempC_1m, TempC_2m, TempC_3m, TempC_4m, TempC_5m, TempC_6m, TempC_7m, TempC_8m, TempC_9m) %>%
  rename(TempC_9p85m = 'TempC_9m',
          TempC_8p85m = 'TempC_8m',
          TempC_7p85m = 'TempC_7m',
          TempC_6p85m = 'TempC_6m',
          TempC_5p85m = 'TempC_5m',
          TempC_4p85m = 'TempC_4m',
          TempC_3p85m = 'TempC_3m',
          TempC_2p85m = 'TempC_2m',
          TempC_1p85m = 'TempC_1m',
          TempC_0p85m = 'TempC_0m') %>%
  gather(depth_m, temp_degC, -datetime, -location) %>% 
  mutate(depth_m = case_when(grepl(pattern = '_0p85m', x = depth_m) ~ '0.85',
                             grepl(pattern = '_1p85m', x = depth_m) ~ '1.85',
                             grepl(pattern = '_2p85m', x = depth_m) ~ '2.85',
                             grepl(pattern = '_3p85m', x = depth_m) ~ '3.85',
                             grepl(pattern = '_4p85m', x = depth_m) ~ '4.85',
                             grepl(pattern = '_5p85m', x = depth_m) ~ '5.85',
                             grepl(pattern = '_6p85m', x = depth_m) ~ '6.85',
                             grepl(pattern = '_7p85m', x = depth_m) ~ '7.85',
                             grepl(pattern = '_8p85m', x = depth_m) ~ '8.85',
                             grepl(pattern = '_9p85m', x = depth_m) ~ '9.85',
                             TRUE ~ NA_character_)) %>% 
  mutate(depth_m = as.numeric(depth_m))


# no flags to parse


#plot to check
ggplot(buoy_2017_L1_vert, aes(x = datetime, y = temp_degC, color = as.factor(depth_m))) +
  geom_point() +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
                              "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust




#order by date, depth
buoy_2017_L1_vert <- buoy_2017_L1_vert %>% 
  arrange(datetime, depth_m)

# buoy_2017_L1_vert %>%
#   mutate(datetime = as.character(datetime)) %>%
#   write_csv(., 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2017_tempstring_vert_L1.csv')

#export l1 do file
buoy2017_L1 %>%
  select(datetime, upDO, lowDO, upper_do_flag, lower_do_flag, location) %>%
  mutate(datetime = as.character(datetime)) %>%
  write_csv(., 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2017_do_L1.csv')

#export l1 par file
buoy2017_L1 %>%
  select(datetime, PAR, location, PAR_flag) %>%
  mutate(datetime = as.character(datetime)) %>%
  write_csv(., 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2017_PAR_L1.csv')

#export l1 wind
buoy2017_L1 %>%
  select(datetime, AveWindSp, AveWindDir, MaxWindSp, MaxWindDir, location) %>%
  mutate(datetime = as.character(datetime)) %>%
  write_csv(., 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2017_wind_L1.csv')

#export l1 air temp file
buoy2017_L1 %>%
  select(datetime, AirTempC, location) %>%
  mutate(datetime = as.character(datetime)) %>%
  write_csv(., 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2017_airtemp_L1.csv')

# #export l1 chla cond file
# buoy2017_L1 %>%
#   select(datetime, Chlor_UGL, SpecCond, location, cond_flag, chla_flag) %>% #leave out rfu - it is not rfu - more likely temp.
#   mutate(datetime = as.character(datetime)) %>%
#   write_csv(., 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2017_chla_cond_L1.csv')



rm(alltimes_2007, alltimes_2008, alltimes_2009, alltimes_2010, alltimes_2011, alltimes_2012, alltimes_2013, alltimes_2014, alltimes_2015, alltimes_2016, alltimes_2017)


