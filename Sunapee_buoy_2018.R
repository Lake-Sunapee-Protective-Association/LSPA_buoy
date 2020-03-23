#*****************************************************************
#*      Cary Institute of Ecosystem Studies (Millbrook, NY)      *
#*                                                               *
#* TITLE:   Sunapee_buoy_2018.r                                  *
#* AUTHOR:  Bethel Steele                                        *
#* SYSTEM:  Lenovo ThinkCentre, Win 10, R 3.4.2, RStudio 1.1.383 *
#* DATE:    30Oct2018                                            *
#* PROJECT: Lake Sunapee Buoy Data Cleaning                      *
#* PURPOSE: create L0 and L1 data for buoy data 2018 using       *
#*          similar methods to CCC and DR                        *
#*****************************************************************

#bring in  buoy raw data
buoy2018 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L0/2018 Buoy Data - through Oct.csv',
                     col_types = 'iiiinnnnnnnnnnnnnnnnnnnnnnnnnnnnnn')

#bring in 2018 LMP data for comparison - no 2018 data yet
# LMP2018 <- read_xlsx('C:/Users/steeleb/Dropbox/Lake Sunapee/long term Sunapee data/raw data/LMP files/LMP 2018/DO.xlsx',
#                      sheet='DO',
#                      col_types = c('text', 'text', 'numeric', 'guess', 'numeric',
#                                    'numeric', 'numeric', 'numeric', 'numeric' ,'numeric',
#                                    'text', 'text', 'numeric', 'text')) %>%
#   mutate(DATE = as.Date(DATE, format='%Y-%m-%d')) %>%  #format date
#   filter(DATE >= '2018-01-01' & DATE < '2018-01-01', #filter for 2018 only
#          STATION == 210)
# 
# LMP2018_bio <- read_xlsx('C:/Users/steeleb/Dropbox/Lake Sunapee/long term Sunapee data/raw data/LMP files/LMP 2018/BIOLOGY.xlsx',
#                          sheet='BIOLOGY',
#                          col_types = c('text', 'text', 'numeric', 'guess', 'numeric',
#                                        'numeric', 'text', 'numeric', 'text' ,'numeric',
#                                        'text', 'numeric', 'numeric', 'numeric', 'text',
#                                        'text', 'text', 'text')) %>%
#   mutate(DATE = as.Date(DATE, format='%Y-%m-%d')) %>%  #format date
#   filter(DATE >= '2018-01-01' & DATE < '2018-01-01', #filter for 2018 only
#          STATION == 210)
# 
# LMP2018_chem <- read_xlsx('C:/Users/steeleb/Dropbox/Lake Sunapee/long term Sunapee data/raw data/LMP files/LMP 2018/CHEMISTRY.xlsx',
#                           sheet='CHEMISTRY',
#                           col_types = c('text', 'text', 'numeric', 'guess', 'numeric',
#                                         'text', 'numeric', 'numeric', 'numeric' ,'numeric',
#                                         'numeric', 'numeric', 'numeric', 'numeric', 'numeric',
#                                         'text', 'text', 'guess', 'text', 'text')) %>%
#   mutate(DATE = as.Date(DATE, format='%Y-%m-%d')) %>%  #format date
#   filter(DATE >= '2018-01-01' & DATE < '2018-01-01', #filter for 2018 only
#          STATION == 210)


#### format data ####
buoy2018 <- buoy2018  %>%
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
# LMP2018 <- LMP2018 %>% 
#   mutate(datetime = as.POSIXct(paste(DATE, '12:00', sep=' '), format='%Y-%m-%d %H:%M', tz='UTC')) %>%  #tibble forces to UTC, so coerce for our uses. assume noon, because no time available
#   select(-c(TIME)) #remove unnecessary columns
# 
# str(buoy2018)
# str(LMP2018)
# 
# 
# #subset LMP for truthing data
# LMP2018_temp <- LMP2018 %>% 
#   select(datetime, DEPTH, TEMP) %>% 
#   filter(DEPTH<=12) %>% 
#   rename(depth = 'DEPTH', 
#          value = 'TEMP') %>% 
#   mutate(source = 'LMP')
# 
# LMP2018_upDO <- LMP2018 %>% 
#   select(datetime, DEPTH, DO, PCNTSAT) %>% 
#   filter(DEPTH <= 2) %>% 
#   rename(depth = 'DEPTH', 
#          DOppm = 'DO', 
#          DOSat = 'PCNTSAT') %>% 
#   gather(variable, value, -datetime, -depth) %>% 
#   mutate(source = 'LMP')
# 
# LMP2018_lowDO <- LMP2018 %>% 
#   select(datetime, DEPTH, DO, PCNTSAT) %>% 
#   filter(DEPTH > 9 & DEPTH <12) %>% 
#   rename(depth = 'DEPTH', 
#          DOLowPPM = 'DO', 
#          DOLowSat = 'PCNTSAT') %>% 
#   gather(variable, value, -datetime, -depth) %>% 
#   mutate(source = 'LMP')
# 
# LMP2018_chla <- LMP2018_bio %>%  
#   select(DATE, CHL) %>% 
#   mutate(DATE = as.POSIXct(paste(DATE, '12:00', sep = ' '), tz='UTC')) %>% 
#   rename(Chlor_UGL = 'CHL',
#          datetime = 'DATE') %>% 
#   gather(variable, value, -datetime) %>% 
#   mutate(source = 'LMP')
# 
# LMP2018_cond <- LMP2018_chem %>%  
#   select(DATE, Depth, COND) %>% 
#   mutate(DATE = as.POSIXct(paste(DATE, '12:00', sep = ' '), tz='UTC')) %>% 
#   rename(SpecCond = 'COND',
#          datetime = 'DATE',
#          depth = 'Depth') %>% 
#   gather(variable, value, -datetime) %>% 
#   mutate(source = 'LMP')

# add in all date time options in L1 data set
alltimes_2018 <- as.data.frame(seq.POSIXt(as.POSIXct('2018-01-01 00:00', tz='UTC'), as.POSIXct('2018-12-31 23:50', tz='UTC'), '10 min')) %>% 
  rename("datetime" = !!names(.[1]))

buoy2018_L1 <- buoy2018 %>% 
  right_join(., alltimes_2018) %>% 
  arrange(datetime)

buoy2018_L1 <- buoy2018_L1[!duplicated(buoy2018_L1$datetime),]


####THERMISTERS####
buoy2018_therm_vert <- buoy2018_L1 %>% 
  select(datetime, alltemp2011) %>%
  gather(variable, value, -datetime)

# ggplot(buoy2018_therm_vert, aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 month') +
#   final_theme

buoy2018_L1 <- buoy2018_L1 %>% 
  mutate_at(vars(alltemp2011),
            funs(case_when(. == -6999 ~ NA_real_,
                           TRUE ~ .)))

buoy2018_therm_vert <- buoy2018_L1 %>% 
  select(datetime, alltemp2011) %>%
  gather(variable, value, -datetime)

# ggplot(buoy2018_therm_vert_L1,
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 month') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme 

buoy2018_therm_vert <- buoy2018_L1 %>% 
  select(datetime, alltemp2011) %>%
  gather(variable, value, -datetime)

# ggplot(subset(buoy2018_therm_vert,
#               subset=(datetime >= as.POSIXct('2018-05-01', tz='UTC') & datetime < as.POSIXct('2018-06-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme
# 
# #buoy deployment
# ggplot(subset(buoy2018_therm_vert,
#               subset=(datetime >= as.POSIXct('2018-05-21', tz='UTC') & datetime < as.POSIXct('2018-05-22', tz='UTC'))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme

buoy2018_L1 <- buoy2018_L1 %>% 
  mutate_at(vars(alltemp2011),
            funs(case_when(datetime < as.POSIXct('2018-05-21 9:30', tz='UTC') ~ NA_real_,
                           TRUE ~ .))) 
buoy2018_therm_vert_L1 <- buoy2018_L1 %>% 
  select(datetime, alltemp2011) %>%
  gather(variable, value, -datetime)

#look at 2m temp alone - looks like it is only reporting integers
unique(buoy2018_L1$TempC_2m)

#remove all 2m data - only integers reported
buoy2018_L1 <- buoy2018_L1 %>% 
  mutate(TempC_2m = NA_real_)
buoy2018_therm_vert_L1 <- buoy2018_L1 %>% 
  select(datetime, alltemp2011) %>%
  gather(variable, value, -datetime)

# ggplot(subset(buoy2018_therm_vert_L1,
#               subset=(datetime >= as.POSIXct('2018-05-01', tz='UTC') & datetime < as.POSIXct('2018-06-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme
# 
# ggplot(subset(buoy2018_therm_vert_L1,
#               subset=(datetime >= as.POSIXct('2018-06-01', tz='UTC') & datetime < as.POSIXct('2018-07-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme
# 
# ggplot(subset(buoy2018_therm_vert_L1,
#               subset=(datetime >= as.POSIXct('2018-07-01', tz='UTC') & datetime < as.POSIXct('2018-08-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme
# 
# ggplot(subset(buoy2018_therm_vert_L1,
#               subset=(datetime >= as.POSIXct('2018-08-01', tz='UTC') & datetime < as.POSIXct('2018-09-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme
# 
# ggplot(subset(buoy2018_therm_vert_L1,
#               subset=(datetime >= as.POSIXct('2018-09-01', tz='UTC') & datetime < as.POSIXct('2018-10-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme
# 
# ggplot(subset(buoy2018_therm_vert_L1,
#               subset=(datetime >= as.POSIXct('2018-10-01', tz='UTC') & datetime < as.POSIXct('2018-11-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme
# 
# #buoy moved for winter
# ggplot(subset(buoy2018_therm_vert_L1,
#               subset=(datetime >= as.POSIXct('2018-10-19', tz='UTC') & datetime < as.POSIXct('2018-10-20', tz='UTC'))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme

buoy2018_L1 <- buoy2018_L1 %>% 
  mutate_at(vars(alltemp2011),
            funs(case_when(datetime >= as.POSIXct('2018-10-19 10:30', tz='UTC') ~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate(temp_flag = case_when(datetime >= as.POSIXct('2018-05-21 9:30', tz='UTC') & datetime < as.POSIXct('2018-10-19 10:30', tz='UTC') ~ '0.85a',
                          TRUE ~ NA_character_))

buoy2018_therm_vert_L1 <- buoy2018_L1 %>% 
  select(datetime, alltemp2011) %>%
  gather(variable, value, -datetime)


# ggplot(buoy2018_therm_vert_L1,
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 month') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme 

rm(buoy2018_therm_vert, buoy2018_therm_vert_L1)



#### DO ####
buoy2018_do_vert <- buoy2018_L1 %>% 
  select(datetime, upDO, lowDO) %>%
  gather(variable, value, -datetime)

# ggplot(buoy2018_do_vert, aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 month') +
#   final_theme

buoy2018_L1 <- buoy2018_L1 %>% 
  mutate_at(vars(upDO, lowDO),
            funs(case_when(. == -6999 ~ NA_real_,
                           . == 0 ~ NA_real_,
                           TRUE ~ .)))

buoy2018_L1 <- buoy2018_L1 %>% 
  mutate(DOTempC = NA_real_,
         DOSat = NA_real_,
         DOppm = NA_real_)

buoy2018_do_vert_L1 <- buoy2018_L1 %>% 
  select(datetime, upDO, lowDO) %>%
  gather(variable, value, -datetime)

# ggplot(buoy2018_do_vert, aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 month') +
#   final_theme


# ggplot(subset(buoy2018_do_vert_L1,
#               subset=(datetime >= as.POSIXct('2018-05-01', tz='UTC') & datetime < as.POSIXct('2018-06-01', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme
# 
# ggplot(subset(buoy2018_do_vert_L1,
#               subset=(datetime >= as.POSIXct('2018-05-21', tz='UTC') & datetime < as.POSIXct('2018-05-22', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme

#buoy move 5-21
buoy2018_L1 <- buoy2018_L1 %>% 
  mutate_at(vars(lowDO),
            funs(case_when(datetime < as.POSIXct('2018-05-21 10:00', tz='UTC') ~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate(location = case_when(datetime >= as.POSIXct('2018-05-21 9:30', tz='UTC') ~ 'loon',
                              TRUE ~ NA_character_)) %>% 
  mutate(lowDO_flag = case_when(datetime == as.POSIXct('2018-05-21 9:30', tz='UTC') ~ 'c',
                                TRUE ~ NA_character_))

buoy2018_do_vert_L1 <- buoy2018_L1 %>% 
  select(datetime, lowDO) %>%
  gather(variable, value, -datetime)


# ggplot(subset(buoy2018_do_vert_L1,
#               subset=(datetime >= as.POSIXct('2018-05-01', tz='UTC') & datetime < as.POSIXct('2018-06-01', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme
# 
# ggplot(subset(buoy2018_do_vert_L1,
#               subset=(datetime >= as.POSIXct('2018-06-01', tz='UTC') & datetime < as.POSIXct('2018-07-01', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme
# 
# #june 20-26 do errant
# ggplot(subset(buoy2018_do_vert_L1,
#               subset=(datetime >= as.POSIXct('2018-06-20', tz='UTC') & datetime < as.POSIXct('2018-06-21', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme
# 
# ggplot(subset(buoy2018_do_vert_L1,
#               subset=(datetime >= as.POSIXct('2018-06-26', tz='UTC') & datetime < as.POSIXct('2018-06-27', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme

buoy2018_L1 <- buoy2018_L1 %>% 
  mutate(DOLowTempC = case_when(datetime == as.POSIXct('2018-06-20 8:10', tz='UTC') ~ NA_real_,
                                datetime == as.POSIXct('2018-06-26 8:50', tz='UTC') ~ NA_real_,
                                TRUE ~ DOLowTempC)) %>% 
  mutate_at(vars(DOLowPPM, DOLowSat),
            funs(case_when(datetime >= as.POSIXct('2018-06-20 8:10', tz='UTC') & datetime < as.POSIXct('2018-06-26 9:30', tz='UTC') ~ NA_real_,
                           TRUE ~ .)))
buoy2018_do_vert_L1 <- buoy2018_L1 %>% 
  select(datetime, lowDO) %>%
  gather(variable, value, -datetime)

# ggplot(subset(buoy2018_do_vert_L1,
#               subset=(datetime >= as.POSIXct('2018-06-01', tz='UTC') & datetime < as.POSIXct('2018-07-01', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme
# 
# ggplot(subset(buoy2018_do_vert_L1,
#               subset=(datetime >= as.POSIXct('2018-07-01', tz='UTC') & datetime < as.POSIXct('2018-08-01', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme
# 
# ggplot(subset(buoy2018_do_vert_L1,
#               subset=(datetime >= as.POSIXct('2018-08-01', tz='UTC') & datetime < as.POSIXct('2018-09-01', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme
# 
# ggplot(subset(buoy2018_do_vert_L1,
#               subset=(datetime >= as.POSIXct('2018-09-01', tz='UTC') & datetime < as.POSIXct('2018-10-01', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme
# 
# #oddball 9/10
# ggplot(subset(buoy2018_do_vert_L1,
#               subset=(datetime >= as.POSIXct('2018-09-10', tz='UTC') & datetime < as.POSIXct('2018-09-11', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme

buoy2018_L1 <- buoy2018_L1 %>% 
  mutate_at(vars(lowDO),
            funs(case_when(datetime == as.POSIXct('2018-09-10 19:20', tz='UTC') ~ NA_real_,
                           TRUE ~ .)))
buoy2018_do_vert_L1 <- buoy2018_L1 %>% 
  select(datetime, lowDO) %>%
  gather(variable, value, -datetime)

# ggplot(subset(buoy2018_do_vert_L1,
#               subset=(datetime >= as.POSIXct('2018-09-01', tz='UTC') & datetime < as.POSIXct('2018-10-01', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme
# 
# 
# ggplot(subset(buoy2018_do_vert_L1,
#               subset=(datetime >= as.POSIXct('2018-10-01', tz='UTC') & datetime < as.POSIXct('2018-11-01', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme
# 
# #buoy to harbor 10-19
# ggplot(subset(buoy2018_do_vert_L1,
#               subset=(datetime >= as.POSIXct('2018-10-19', tz='UTC') & datetime < as.POSIXct('2018-10-20', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme

buoy2018_L1 <- buoy2018_L1 %>% 
  mutate_at(vars(lowDO),
            funs(case_when(datetime >= as.POSIXct('2018-10-19 10:30', tz='UTC') ~ NA_real_,
                           TRUE ~ .)))

buoy2018_do_vert_L1 <- buoy2018_L1 %>%
  select(datetime, lowDO) %>%
  gather(variable, value, -datetime)

# ggplot(buoy2018_do_vert_L1,
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 month') +
#   labs(title = 'do 2018, clean') +
#   final_theme +
#   scale_color_colorblind()
# 
# buoy_temp_check <- buoy2018_L1 %>% 
#   select(datetime, DOLowTempC, TempC_9m) %>% 
#   gather(variable, value, -datetime)
# 
# ggplot(buoy_temp_check,
#        aes(x=datetime, y=value, color = variable)) +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 month') +
#   final_theme +
#   scale_color_colorblind()

rm(buoy2018_do_vert, buoy2018_do_vert_L1, buoy_temp_check)


#### HOBO DO ####
hobo_updo <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L0/2018_Summer_U26_DO.csv',
         skip = 2,
         col_types = c('icnnccccc'),
         col_names = c('x', 'datetime', 'do_mgl', 'temp_degC', 'x2', 'x3', 'x4', 'x5', 'x6')) %>% 
  select(datetime, do_mgl, temp_degC) %>% 
  mutate(datetime = as.POSIXct(datetime, tz='UTC', format = '%m/%d/%y %I:%M:%S %p'))

hobo_updo_vert <- hobo_updo %>% 
  gather(variable, value, -datetime)

# ggplot(hobo_updo_vert,
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 month') +
#   labs(title = 'do 2018') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(hobo_updo_vert,
#               (subset = datetime >= as.POSIXct('2018-10-01', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   labs(title = 'do 2018') +
#   final_theme +
#   scale_color_colorblind()
# 
# #sensor removed oct 12
# ggplot(subset(hobo_updo_vert,
#               (subset = datetime >= as.POSIXct('2018-10-12', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   labs(title = 'do 2018') +
#   final_theme +
#   scale_color_colorblind()

hobo_updo_L1 <- hobo_updo %>% 
  mutate_at(vars(do_mgl, temp_degC),
            funs(case_when(datetime >= as.POSIXct('2018-10-12 10:30', tz='UTC') ~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate(upDO_flag = case_when(datetime == as.POSIXct('2018-05-23 12:00:00', tz='UTC') ~ 'c',
                                         TRUE ~ NA_character_))

hobo_updo_vert_L1 <- hobo_updo_L1 %>% 
  gather(variable, value, -datetime)

# ggplot(hobo_updo_vert_L1,
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 month') +
#   labs(title = 'do 2018') +
#   final_theme +
#   scale_color_colorblind()


####CHLA####
# buoy2018_chla_vert <- buoy2018_L1 %>%
#   select(datetime, chla) %>%
#   gather(variable, value, -datetime)
# 
# ggplot(buoy2018_chla_vert, aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 month') +
#   final_theme

#chla sensor not working
buoy2018_L1 <- buoy2018_L1 %>% 
  mutate(Chlor_RFU = is.na(Chlor_RFU),
         Chlor_UGL = is.na(Chlor_UGL),
         SpecCond = is.na(SpecCond))



####wind####
buoy_wind_vert <- buoy2018_L1 %>%
  select(datetime, AveWindDir, AveWindSp, MaxWindDir, MaxWindSp) %>%
  gather(variable, value, -datetime)

# ggplot(buoy_wind_vert,
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'wind 2018, raw') +
#   scale_x_datetime(date_minor_breaks = '1 month') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy_wind_vert,
#               subset=(datetime >= as.POSIXct('2018-01-01', tz='UTC') & datetime < as.POSIXct('2018-02-01', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'jan wind 2018') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# #sensor frozen jan 17-18
# ggplot(subset(buoy_wind_vert,
#               subset=(datetime >= as.POSIXct('2018-01-17 12:00', tz='UTC') & datetime < as.POSIXct('2018-01-18 16:00', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'jan wind 2018, NAs recoded') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme +
#   scale_color_colorblind()
# 
# #sensor frozen jan 23
# ggplot(subset(buoy_wind_vert,
#               subset=(datetime >= as.POSIXct('2018-01-23', tz='UTC') & datetime < as.POSIXct('2018-01-24', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'jan wind 2018, NAs recoded') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme +
#   scale_color_colorblind()


buoy2018_L1 <- buoy2018_L1 %>% 
  mutate_at(vars(AveWindDir, AveWindSp, MaxWindDir, MaxWindSp),
            funs(case_when(datetime >= as.POSIXct('2018-01-17 13:40', tz='UTC') & datetime < as.POSIXct('2018-01-18 14:30', tz='UTC') ~ NA_real_,
                           datetime >= as.POSIXct('2018-01-23 6:10', tz='UTC') & datetime < as.POSIXct('2018-01-23 17:50', tz='UTC') ~ NA_real_,
                           TRUE ~ .)))

buoy_wind_vert_L1 <- buoy2018_L1 %>% 
  select(datetime, AveWindDir, AveWindSp, MaxWindDir, MaxWindSp) %>% 
  gather(variable, value, -datetime)

# ggplot(subset(buoy_wind_vert_L1,
#               subset=(datetime >= as.POSIXct('2018-01-01', tz='UTC') & datetime < as.POSIXct('2018-02-01', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'jan wind 2018, clean') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# 
# ggplot(subset(buoy_wind_vert_L1,
#               subset=(datetime >= as.POSIXct('2018-02-01', tz='UTC') & datetime < as.POSIXct('2018-03-01', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'feb wind 2018') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# #sensor frozen feb 2
# ggplot(subset(buoy_wind_vert,
#               subset=(datetime >= as.POSIXct('2018-02-02', tz='UTC') & datetime < as.POSIXct('2018-02-03', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme +
#   scale_color_colorblind()
# 
# #sensor frozen feb 18
# ggplot(subset(buoy_wind_vert,
#               subset=(datetime >= as.POSIXct('2018-02-18', tz='UTC') & datetime < as.POSIXct('2018-02-19', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme +
#   scale_color_colorblind()

buoy2018_L1 <- buoy2018_L1 %>% 
  mutate_at(vars(AveWindDir, AveWindSp, MaxWindDir, MaxWindSp),
            funs(case_when(datetime >= as.POSIXct('2018-02-02 4:30', tz='UTC') & datetime < as.POSIXct('2018-02-02 12:50', tz='UTC') ~ NA_real_,
                           datetime >= as.POSIXct('2018-02-18 4:30', tz='UTC') & datetime < as.POSIXct('2018-02-18 10:30', tz='UTC') ~ NA_real_,
                           TRUE ~ .)))

buoy_wind_vert_L1 <- buoy2018_L1 %>% 
  select(datetime, AveWindDir, AveWindSp, MaxWindDir, MaxWindSp) %>% 
  gather(variable, value, -datetime)

# ggplot(subset(buoy_wind_vert_L1,
#               subset=(datetime >= as.POSIXct('2018-02-01', tz='UTC') & datetime < as.POSIXct('2018-03-01', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'feb wind 2018') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# 
# ggplot(subset(buoy_wind_vert_L1,
#               subset=(datetime >= as.POSIXct('2018-03-01', tz='UTC') & datetime < as.POSIXct('2018-04-01', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'mar wind 2018') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# #mar08 - mar10
# ggplot(subset(buoy_wind_vert_L1,
#               subset=(datetime >= as.POSIXct('2018-03-08', tz='UTC') & datetime < as.POSIXct('2018-03-09', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'mar wind 2018') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme +
#   scale_color_colorblind()
# ggplot(subset(buoy_wind_vert_L1,
#               subset=(datetime >= as.POSIXct('2018-03-10', tz='UTC') & datetime < as.POSIXct('2018-03-11', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'mar wind 2018') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme +
#   scale_color_colorblind()
# 
# #mar13 - mar14
# ggplot(subset(buoy_wind_vert_L1,
#               subset=(datetime >= as.POSIXct('2018-03-13 12:00', tz='UTC') & datetime < as.POSIXct('2018-03-14 16:00', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'mar wind 2018') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme +
#   scale_color_colorblind()

buoy2018_L1 <- buoy2018_L1 %>% 
  mutate_at(vars(AveWindDir, AveWindSp, MaxWindDir, MaxWindSp),
            funs(case_when(datetime >= as.POSIXct('2018-03-08 5:00', tz='UTC') & datetime < as.POSIXct('2018-03-10 14:30', tz='UTC') ~ NA_real_,
                           datetime >= as.POSIXct('2018-03-13 21:20', tz='UTC') & datetime < as.POSIXct('2018-03-14 14:20', tz='UTC') ~ NA_real_,
                           TRUE ~ .)))

buoy_wind_vert_L1 <- buoy2018_L1 %>% 
  select(datetime, AveWindDir, AveWindSp, MaxWindDir, MaxWindSp) %>% 
  gather(variable, value, -datetime)

# ggplot(subset(buoy_wind_vert_L1,
#               subset=(datetime >= as.POSIXct('2018-03-01', tz='UTC') & datetime < as.POSIXct('2018-04-01', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'mar wind 2018') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# 
# ggplot(subset(buoy_wind_vert_L1,
#               subset=(datetime >= as.POSIXct('2018-04-01', tz='UTC') & datetime < as.POSIXct('2018-05-01', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'apr wind 2018') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy_wind_vert_L1,
#               subset=(datetime >= as.POSIXct('2018-05-01', tz='UTC') & datetime < as.POSIXct('2018-06-01', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'may wind 2018') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# #may 21 buoy moved to loon
# ggplot(subset(buoy_wind_vert_L1,
#               subset=(datetime >= as.POSIXct('2018-05-21', tz='UTC') & datetime < as.POSIXct('2018-05-22', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'may wind 2018') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme +
#   scale_color_colorblind()

buoy2018_L1 <- buoy2018_L1 %>% 
  mutate(location = case_when(datetime < as.POSIXct('2018-05-21 8:40', tz='UTC') ~ 'harbor',
                              datetime >= as.POSIXct('2018-05-21 8:40', tz='UTC') & datetime < as.POSIXct('2018-05-21 9:30', tz='UTC') ~ 'in transit',
                              TRUE ~ location)) %>% 
  mutate_at(vars(AveWindDir, AveWindSp, MaxWindDir, MaxWindSp),
            funs(case_when(location == 'in transit' ~ NA_real_,
                           TRUE ~ .)))

# ggplot(subset(buoy_wind_vert_L1,
#               subset=(datetime >= as.POSIXct('2018-05-01', tz='UTC') & datetime < as.POSIXct('2018-06-01', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'may wind 2018') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# 
# ggplot(subset(buoy_wind_vert_L1,
#               subset=(datetime >= as.POSIXct('2018-06-01', tz='UTC') & datetime < as.POSIXct('2018-07-01', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'june wind 2018') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# 
# ggplot(subset(buoy_wind_vert_L1,
#               subset=(datetime >= as.POSIXct('2018-07-01', tz='UTC') & datetime < as.POSIXct('2018-08-01', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'july wind 2018') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy_wind_vert_L1,
#               subset=(datetime >= as.POSIXct('2018-08-01', tz='UTC') & datetime < as.POSIXct('2018-09-01', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'august wind 2018') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy_wind_vert_L1,
#               subset=(datetime >= as.POSIXct('2018-09-01', tz='UTC') & datetime < as.POSIXct('2018-10-01', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'sept wind 2018') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy_wind_vert_L1,
#               subset=(datetime >= as.POSIXct('2018-10-01', tz='UTC') & datetime < as.POSIXct('2018-11-01', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'oct wind 2018') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# #oct 19 buoy to harbor
# ggplot(subset(buoy_wind_vert_L1,
#               subset=(datetime >= as.POSIXct('2018-10-19', tz='UTC') & datetime < as.POSIXct('2018-10-20', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'oct wind 2018') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme +
#   scale_color_colorblind()

buoy2018_L1 <- buoy2018_L1 %>% 
  mutate(location = case_when(datetime >= as.POSIXct('2018-10-19 10:30', tz='UTC') & datetime < as.POSIXct('2018-10-19 11:30', tz='UTC') ~ 'in transit',
                              datetime >= as.POSIXct('2018-10-19 11:30', tz='UTC') ~ 'harbor',
                              TRUE ~ location)) %>% 
  mutate_at(vars(AveWindDir, AveWindSp, MaxWindDir, MaxWindSp),
            funs(case_when(location == 'in transit' ~ NA_real_,
                           TRUE ~ .)))

buoy_wind_vert_L1 <- buoy2018_L1 %>% 
  select(datetime, AveWindDir, AveWindSp, MaxWindDir, MaxWindSp, location) %>% 
  gather(variable, value, -datetime, -location)

# ggplot(buoy_wind_vert_L1,
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'wind 2018, clean') +
#   scale_x_datetime(date_minor_breaks = '1 month') +
#   final_theme +
#   scale_color_colorblind()

rm(buoy_wind_vert, buoy_wind_vert_L1)

####PAR####
# ggplot(buoy2018_L1,
#        aes(x=datetime, y=PAR, color=location)) +
#   geom_point() +
#   labs(title = 'PAR 2018, raw') +
#   scale_x_datetime(date_minor_breaks = '1 month') +
#   final_theme +
#   scale_color_colorblind()

#recode when in transit or offline
buoy2018_L1 <- buoy2018_L1 %>% 
  mutate(PAR = case_when(location == 'in transit' ~ NA_real_,
                         PAR < 0 ~ 0,
                         TRUE ~ PAR)) 
# ggplot(buoy2018_L1,
#        aes(x=datetime, y=PAR, color=location)) +
#   geom_point() +
#   labs(title = 'PAR 2018, NA recoded') +
#   scale_x_datetime(date_minor_breaks = '1 month') +
#   final_theme +
#   scale_color_colorblind()

# ggplot(subset(buoy2018_L1,
#               subset=(datetime >= as.POSIXct('2018-01-01', tz='UTC') & datetime < as.POSIXct('2018-02-01', tz='UTC'))),
#        aes(x=datetime, y=PAR, color=location)) +
#   geom_point() +
#   labs(title = 'PAR 2018, NA recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy2018_L1,
#               subset=(datetime >= as.POSIXct('2018-02-01', tz='UTC') & datetime < as.POSIXct('2018-03-01', tz='UTC'))),
#        aes(x=datetime, y=PAR, color=location)) +
#   geom_point() +
#   labs(title = 'PAR 2018, NA recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy2018_L1,
#               subset=(datetime >= as.POSIXct('2018-03-01', tz='UTC') & datetime < as.POSIXct('2018-04-01', tz='UTC'))),
#        aes(x=datetime, y=PAR, color=location)) +
#   geom_point() +
#   labs(title = 'PAR 2018, NA recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy2018_L1,
#               subset=(datetime >= as.POSIXct('2018-04-01', tz='UTC') & datetime < as.POSIXct('2018-05-01', tz='UTC'))),
#        aes(x=datetime, y=PAR, color=location)) +
#   geom_point() +
#   labs(title = 'PAR 2018, NA recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy2018_L1,
#               subset=(datetime >= as.POSIXct('2018-05-01', tz='UTC') & datetime < as.POSIXct('2018-06-01', tz='UTC'))),
#        aes(x=datetime, y=PAR, color=location)) +
#   geom_point() +
#   labs(title = 'PAR 2018, NA recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy2018_L1,
#               subset=(datetime >= as.POSIXct('2018-06-01', tz='UTC') & datetime < as.POSIXct('2018-07-01', tz='UTC'))),
#        aes(x=datetime, y=PAR, color=location)) +
#   geom_point() +
#   labs(title = 'PAR 2018, NA recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy2018_L1,
#               subset=(datetime >= as.POSIXct('2018-07-01', tz='UTC') & datetime < as.POSIXct('2018-08-01', tz='UTC'))),
#        aes(x=datetime, y=PAR, color=location)) +
#   geom_point() +
#   labs(title = 'PAR 2018, NA recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy2018_L1,
#               subset=(datetime >= as.POSIXct('2018-08-01', tz='UTC') & datetime < as.POSIXct('2018-09-01', tz='UTC'))),
#        aes(x=datetime, y=PAR, color=location)) +
#   geom_point() +
#   labs(title = 'PAR 2018, NA recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy2018_L1,
#               subset=(datetime >= as.POSIXct('2018-09-01', tz='UTC') & datetime < as.POSIXct('2018-10-01', tz='UTC'))),
#        aes(x=datetime, y=PAR, color=location)) +
#   geom_point() +
#   labs(title = 'PAR 2018, NA recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy2018_L1,
#               subset=(datetime >= as.POSIXct('2018-10-01', tz='UTC') & datetime < as.POSIXct('2018-11-01', tz='UTC'))),
#        aes(x=datetime, y=PAR, color=location)) +
#   geom_point() +
#   labs(title = 'PAR 2018, NA recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()


#### Air temp ####
# ggplot(buoy2018_L1,
#        aes(x=datetime, y=AirTempC, color=location)) +
#   geom_point() +
#   labs(title = 'air temp 2018, NA recoded') +
#   scale_x_datetime(date_minor_breaks = '1 month') +
#   final_theme +
#   scale_color_colorblind()

#recode when in transit or offline
buoy2018_L1 <- buoy2018_L1 %>% 
  mutate(AirTempC = case_when(location == 'in transit' ~ NA_real_,
                         TRUE ~ AirTempC))

# ggplot(buoy2018_L1,
#        aes(x=datetime, y=AirTempC, color=location)) +
#   geom_point() +
#   labs(title = 'air temp 2018, NA recoded') +
#   scale_x_datetime(date_minor_breaks = '1 month') +
#   final_theme +
#   scale_color_colorblind()

# ggplot(subset(buoy2018_L1,
#               subset=(datetime >= as.POSIXct('2018-01-01', tz='UTC') & datetime < as.POSIXct('2018-02-01', tz='UTC'))),
#        aes(x=datetime, y=AirTempC, color=location)) +
#   geom_point() +
#   labs(title = 'air temp 2018, NA recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy2018_L1,
#               subset=(datetime >= as.POSIXct('2018-02-01', tz='UTC') & datetime < as.POSIXct('2018-03-01', tz='UTC'))),
#        aes(x=datetime, y=AirTempC, color=location)) +
#   geom_point() +
#   labs(title = 'air temp 2018, NA recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy2018_L1,
#               subset=(datetime >= as.POSIXct('2018-03-01', tz='UTC') & datetime < as.POSIXct('2018-04-01', tz='UTC'))),
#        aes(x=datetime, y=AirTempC, color=location)) +
#   geom_point() +
#   labs(title = 'air temp 2018, NA recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy2018_L1,
#               subset=(datetime >= as.POSIXct('2018-04-01', tz='UTC') & datetime < as.POSIXct('2018-05-01', tz='UTC'))),
#        aes(x=datetime, y=AirTempC, color=location)) +
#   geom_point() +
#   labs(title = 'air temp 2018, NA recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy2018_L1,
#               subset=(datetime >= as.POSIXct('2018-05-01', tz='UTC') & datetime < as.POSIXct('2018-06-01', tz='UTC'))),
#        aes(x=datetime, y=AirTempC, color=location)) +
#   geom_point() +
#   labs(title = 'air temp 2018, NA recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy2018_L1,
#               subset=(datetime >= as.POSIXct('2018-06-01', tz='UTC') & datetime < as.POSIXct('2018-07-01', tz='UTC'))),
#        aes(x=datetime, y=AirTempC, color=location)) +
#   geom_point() +
#   labs(title = 'air temp 2018, NA recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy2018_L1,
#               subset=(datetime >= as.POSIXct('2018-07-01', tz='UTC') & datetime < as.POSIXct('2018-08-01', tz='UTC'))),
#        aes(x=datetime, y=AirTempC, color=location)) +
#   geom_point() +
#   labs(title = 'air temp 2018, NA recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy2018_L1,
#               subset=(datetime >= as.POSIXct('2018-08-01', tz='UTC') & datetime < as.POSIXct('2018-09-01', tz='UTC'))),
#        aes(x=datetime, y=AirTempC, color=location)) +
#   geom_point() +
#   labs(title = 'air temp 2018, NA recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy2018_L1,
#               subset=(datetime >= as.POSIXct('2018-09-01', tz='UTC') & datetime < as.POSIXct('2018-10-01', tz='UTC'))),
#        aes(x=datetime, y=AirTempC, color=location)) +
#   geom_point() +
#   labs(title = 'air temp 2018, NA recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy2018_L1,
#               subset=(datetime >= as.POSIXct('2018-10-01', tz='UTC') & datetime < as.POSIXct('2018-11-01', tz='UTC'))),
#        aes(x=datetime, y=AirTempC, color=location)) +
#   geom_point() +
#   labs(title = 'air temp 2018, NA recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()




#### look at two do sources ####
lowdo2018 <- buoy2018_L1 %>%
  select(datetime, lowDO)
  
do2018 <- full_join(lowdo2018, hobo_updo_L1) %>% 
  gather(variable, value, -datetime)

# ggplot(do2018,
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'do 2018') +
#   scale_x_datetime(date_minor_breaks = '1 month') +
#   final_theme +
#   scale_color_colorblind()


#### EXPORT L1 DATA STREAMS ####
#export L1 tempstring file
# buoy2018_L1 %>%
#   select(datetime, TempC_0m, TempC_1m, TempC_2m, TempC_3m, TempC_4m, TempC_5m, TempC_6m, TempC_7m, TempC_8m, TempC_9m, temp_flag, location) %>%
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
#   write_csv(., 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2018_tempstring_L1.csv')

#crete vertical dataset
buoy_2018_L1_vert <- buoy2018_L1 %>%
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


# add flag for 0.85 reporting in 20min intervals
buoy_2018_L1_vert <- buoy_2018_L1_vert %>% 
  mutate(temp_flag = case_when(datetime >= as.POSIXct('2018-05-21 9:30', tz='UTC') & datetime < as.POSIXct('2018-10-19 10:30', tz='UTC') & depth_m == 0.85 & !is.na(temp_degC) ~ 'a',
                               TRUE ~ NA_character_))


#plot to check
ggplot(buoy_2018_L1_vert, aes(x = datetime, y = temp_degC, color = as.factor(depth_m))) +
  geom_point() +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
                              "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust


#order by date, depth
buoy_2018_L1_vert <- buoy_2018_L1_vert %>% 
  arrange(datetime, depth_m)

# buoy_2018_L1_vert %>%
#   mutate(datetime = as.character(datetime)) %>%
#   write_csv(., 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2018_tempstring_vert_L1.csv')


#export l1 do file
buoy2018_L1 %>%
  select(datetime, lowDO, lowDO_flag, location) %>%
  mutate(datetime = as.character(datetime)) %>%
  write_csv(., 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2018_do_L1.csv')

#export l1 hobo do file
hobo_updo_L1 %>%
  mutate(datetime = as.character(datetime)) %>%
  write_csv(., 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2018_hobodo_L1.csv')

#export l1 par file
buoy2018_L1 %>%
  select(datetime, PAR, location) %>%
  mutate(datetime = as.character(datetime)) %>%
  write_csv(., 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2018_PAR_L1.csv')

#export l1 wind
buoy2018_L1 %>%
  select(datetime, AveWindSp, AveWindDir, MaxWindSp, MaxWindDir, location) %>%
  mutate(datetime = as.character(datetime)) %>%
  write_csv(., 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2018_wind_L1.csv')

#export l1 air temp file
buoy2018_L1 %>%
  select(datetime, AirTempC, location) %>%
  mutate(datetime = as.character(datetime)) %>%
  write_csv(., 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2018_airtemp_L1.csv')


rm(alltimes_2018)


