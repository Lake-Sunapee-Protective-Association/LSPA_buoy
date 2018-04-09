#*****************************************************************
#*      Cary Institute of Ecosystem Studies (Millbrook, NY)      *
#*                                                               *
#* TITLE:   Sunapee_buoy_2016.r                                  *
#* AUTHOR:  Bethel Steele                                        *
#* SYSTEM:  Lenovo ThinkCentre, Win 10, R 3.4.2, RStudio 1.1.383 *
#* PROJECT: Lake Sunapee Buoy Data Cleaning                      *
#* PURPOSE: create L0 and L1 data for buoy data 2016 using       *
#*          similar methods to CCC and DR                        *
#*****************************************************************

#bring in 2016 buoy raw data
buoy2016 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L0/2016 Buoy Data.csv',
                     col_types = 'iiiinnnnnnnnnnnnnnnnnnnnnnnnnnnnnn')

#bring in 2017 LMP data for comparison
LMP2016 <- read_xlsx('C:/Users/steeleb/Dropbox/Lake Sunapee/long term Sunapee data/raw data/LMP files/LMP 2017/DO.xlsx', 
                     sheet='DO',
                     col_types = c('text', 'text', 'numeric', 'guess', 'numeric', 
                                   'numeric', 'numeric', 'numeric', 'numeric' ,'numeric',
                                   'text', 'text', 'numeric', 'text')) %>% 
  mutate(DATE = as.Date(DATE, format='%Y-%m-%d')) %>%  #format date
  filter(DATE >= '2016-01-01' & DATE < '2017-01-01', #filter for 2016 only
         STATION == 210)

LMP2016_bio <- read_xlsx('C:/Users/steeleb/Dropbox/Lake Sunapee/long term Sunapee data/raw data/LMP files/LMP 2017/BIOLOGY.xlsx', 
                         sheet='BIOLOGY',
                         col_types = c('text', 'text', 'numeric', 'guess', 'numeric', 
                                       'numeric', 'text', 'numeric', 'text' ,'numeric',
                                       'text', 'numeric', 'numeric', 'numeric', 'text',
                                       'text', 'text', 'text')) %>% 
  mutate(DATE = as.Date(DATE, format='%Y-%m-%d')) %>%  #format date
  filter(DATE >= '2016-01-01' & DATE < '2017-01-01', #filter for 2016 only
         STATION == 210) 

LMP2016_chem <- read_xlsx('C:/Users/steeleb/Dropbox/Lake Sunapee/long term Sunapee data/raw data/LMP files/LMP 2017/CHEMISTRY.xlsx', 
                         sheet='CHEMISTRY',
                         col_types = c('text', 'text', 'numeric', 'guess', 'numeric', 
                                       'text', 'numeric', 'numeric', 'numeric' ,'numeric',
                                       'numeric', 'numeric', 'numeric', 'numeric', 'numeric',
                                       'text', 'text', 'guess', 'text', 'text')) %>% 
  mutate(DATE = as.Date(DATE, format='%Y-%m-%d')) %>%  #format date
  filter(DATE >= '2016-01-01' & DATE < '2017-01-01', #filter for 2016 only
         STATION == 210) 


#### format data ####
buoy2016 <- buoy2016 %>%
  rename(Hr.Min = 'Hr/Min') %>% 
  mutate(hour = Hr.Min%/%100,
         minutes = Hr.Min%%100,
         time = paste(hour, minutes, sep=':')) %>% #break out time from Hr.Min, create time column
  mutate(date = as.Date(paste(Day, Year, sep = '-'), format='%j-%Y'), #create date in ymd format
         datetime = as.POSIXct(paste(date, time, sep=' '), format='%Y-%m-%d %H:%M', tz='UTC')) %>%  #tibble forces to UTC, so coerce for our uses.
  select(-c(hour, minutes, Hr.Min, time)) #remove unnecessary columns


# format time in LMP data
LMP2016 <- LMP2016 %>% 
  mutate(datetime = as.POSIXct(paste(DATE, '12:00', sep=' '), format='%Y-%m-%d %H:%M', tz='UTC')) %>%  #tibble forces to UTC, so coerce for our uses. assume noon, because no time available
  select(-c(TIME)) #remove unnecessary columns

str(buoy2016)
str(LMP2016)


#subset LMP for truthing data
LMP2016_temp <- LMP2016 %>% 
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


LMP2016_upDO <- LMP2016 %>% 
  select(datetime, DEPTH, DO, PCNTSAT) %>% 
  filter(DEPTH <= 2) %>% 
  rename(depth = 'DEPTH', 
         DOppm = 'DO', 
         DOSat = 'PCNTSAT') %>% 
  gather(variable, value, -datetime, -depth) %>% 
  mutate(source = 'LMP')

LMP2016_lowDO <- LMP2016 %>% 
  select(datetime, DEPTH, DO, PCNTSAT) %>% 
  filter(DEPTH > 9 & DEPTH <12) %>% 
  rename(depth = 'DEPTH', 
         DOLowPPM = 'DO', 
         DOLowSat = 'PCNTSAT') %>% 
  gather(variable, value, -datetime, -depth) %>% 
  mutate(source = 'LMP')

LMP2016_chla <- LMP2016_bio %>%  
  select(DATE, CHL) %>% 
  mutate(DATE = as.POSIXct(paste(DATE, '12:00', sep = ' '), tz='UTC')) %>% 
  rename(Chlor_UGL = 'CHL',
         datetime = 'DATE') %>% 
  gather(variable, value, -datetime) %>% 
  mutate(source = 'LMP')

LMP2016_cond <- LMP2016_chem %>%  
  select(DATE, Depth, COND) %>% 
  mutate(DATE = as.POSIXct(paste(DATE, '12:00', sep = ' '), tz='UTC')) %>% 
  rename(SpecCond = 'COND',
         datetime = 'DATE',
         depth = 'Depth') %>% 
  gather(variable, value, -datetime) %>% 
  mutate(source = 'LMP')

buoy2016_L1 <- buoy2016

####thermisters####
buoy2016_vert_temp <- buoy2016_L1 %>% 
  select(datetime, alltemp2016) %>% 
  gather(variable, value, -datetime)

# ggplot(subset(buoy2016_vert_temp, subset=(datetime>=as.POSIXct('2016-01-01', tz='UTC') & datetime < as.POSIXct('2017-01-01', tz='UTC'))), aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='2016, raw', x='date', y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme

buoy2016_L1 <- buoy2016_L1 %>% 
  mutate_at(vars(TempC_0m:TempC_9m),
            funs(case_when(. == -6999 ~ NA_real_,
                           . >= 555.4 ~ NA_real_,
                           TRUE ~ .)))

# buoy2016_vert_temp <- buoy2016_L1 %>% 
#   select(datetime, alltemp2016) %>% 
#   gather(variable, value, -datetime)
# 
# ggplot(subset(buoy2016_vert_temp, subset=(datetime>=as.POSIXct('2016-01-01', tz='UTC') & datetime < as.POSIXct('2017-01-01', tz='UTC'))), aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='2016, NA values recoded', x='date', y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme

#remove all of 0m thermister
buoy2016_L1 <- buoy2016_L1 %>% 
  mutate(TempC_0m = NA_real_)

buoy2016_vert_temp <- buoy2016_L1 %>%
  select(datetime, alltemp2016) %>%
  gather(variable, value, -datetime)

# ggplot(subset(buoy2016_vert_temp, subset=(datetime>=as.POSIXct('2016-01-01', tz='UTC') & datetime < as.POSIXct('2017-01-01', tz='UTC'))), aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='2016, 0m removed', x='date', y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme
# 
# ggplot(subset(buoy2016_vert_temp, subset=(datetime>=as.POSIXct('2016-04-01', tz='UTC') & datetime < as.POSIXct('2016-05-01', tz='UTC'))), aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='Apr 2016', x='date', y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme
# 
# ggplot(subset(buoy2016_vert_temp, subset=(datetime>=as.POSIXct('2016-05-01', tz='UTC') & datetime < as.POSIXct('2016-06-01', tz='UTC'))), aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='May 2016', x='date', y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme
# 
# #recode therm data to NA prior to move to loon - the depths are not correct. 5-3-16 11am
# ggplot(subset(buoy2016_vert_temp, subset=(datetime>=as.POSIXct('2016-05-03 10:00', tz='UTC') & datetime < as.POSIXct('2016-05-03 14:00', tz='UTC'))), aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='May move 2016', x='date', y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme

buoy2016_L1 <- buoy2016_L1 %>% 
  mutate_at(vars(TempC_0m:TempC_9m),
            funs(case_when(datetime <= as.POSIXct('2016-05-03 12:00', tz='UTC') ~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate(location = case_when(datetime <= as.POSIXct('2016-05-03 12:00', tz='UTC') ~ 'harbor',
                              TRUE ~ 'deep spot'))

# buoy2016_vert_temp_L1 <- buoy2016_L1 %>% 
#   select(datetime, alltemp2016) %>% 
#   gather(variable, value, -datetime)
# 
# ggplot(subset(buoy2016_vert_temp_L1, subset=(datetime>=as.POSIXct('2016-05-01', tz='UTC') & datetime < as.POSIXct('2016-06-01', tz='UTC'))), aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='May 2016, v2', x='date', y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme
# 
# #errant data on may 18 and 19
# ggplot(subset(buoy2016_vert_temp_L1, subset=(datetime>=as.POSIXct('2016-05-18 10:00', tz='UTC') & datetime < as.POSIXct('2016-05-18 14:00', tz='UTC'))), aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='mid May 2016', x='date', y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme
# 
# ggplot(subset(buoy2016_vert_temp_L1, subset=(datetime>=as.POSIXct('2016-05-19 3:00', tz='UTC') & datetime < as.POSIXct('2016-05-19 7:00', tz='UTC'))), aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='mid May 2016', x='date', y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme

buoy2016_L1 <- buoy2016_L1 %>% 
  mutate_at(vars(TempC_0m:TempC_9m),
            funs(case_when(datetime == as.POSIXct('2016-05-18 11:00', tz='UTC') ~ NA_real_,
                           datetime == as.POSIXct('2016-05-19 5:10', tz='UTC') ~ NA_real_,
                           TRUE ~ .)))
# buoy2016_vert_temp_L1 <- buoy2016_L1 %>% 
#   select(datetime, alltemp2016) %>% 
#   gather(variable, value, -datetime)
# 
# 
# ggplot(subset(buoy2016_vert_temp_L1, subset=(datetime>=as.POSIXct('2016-05-01', tz='UTC') & datetime < as.POSIXct('2016-06-01', tz='UTC'))), aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='May 2016, v3', x='date', y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme
# 
# ggplot(subset(buoy2016_vert_temp, subset=(datetime>=as.POSIXct('2016-06-01', tz='UTC') & datetime < as.POSIXct('2016-07-01', tz='UTC'))), aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='June 2016', x='date', y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme
# 
# #buoy visit June 7 8:10
# ggplot(subset(buoy2016_vert_temp, subset=(datetime>=as.POSIXct('2016-06-07', tz='UTC') & datetime < as.POSIXct('2016-06-08', tz='UTC'))), aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='June 7 2016', x='date', y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme
# #looks okay - a little wonky at 6m, but good otherwise
# 
# #buoy visit June 14 8:15
# ggplot(subset(buoy2016_vert_temp, subset=(datetime>=as.POSIXct('2016-06-14', tz='UTC') & datetime < as.POSIXct('2016-06-15', tz='UTC'))), aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='June 14 2016', x='date', y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme
# #looks fine, no artifacts
# 
# #buoy visit June 15 10:50a
# ggplot(subset(buoy2016_vert_temp, subset=(datetime>=as.POSIXct('2016-06-15 09:00', tz='UTC') & datetime < as.POSIXct('2016-06-15 13:00', tz='UTC'))), aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='June 15 2016', x='date', y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme
#remove artifacts
buoy2016_L1 <- buoy2016_L1 %>% 
  mutate_at(vars(TempC_0m:TempC_9m),
            funs(case_when(datetime == as.POSIXct('2016-06-15 11:00', tz='UTC') ~ NA_real_,
                           TRUE ~ .)))
# buoy2016_vert_temp_L1 <- buoy2016_L1 %>% 
#   select(datetime, alltemp2016) %>% 
#   gather(variable, value, -datetime)
# ggplot(subset(buoy2016_vert_temp_L1, subset=(datetime>=as.POSIXct('2016-06-15 09:00', tz='UTC') & datetime < as.POSIXct('2016-06-15 13:00', tz='UTC'))), aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='June 15 2016, v2', x='date', y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme
# 
# #buoy visit June 17 8:30
# ggplot(subset(buoy2016_vert_temp, subset=(datetime>=as.POSIXct('2016-06-17', tz='UTC') & datetime < as.POSIXct('2016-06-18', tz='UTC'))), aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='June 17 2016', x='date', y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme
# #looks fine, no artifacts
# 
# #buoy visit June 20 8:15
# ggplot(subset(buoy2016_vert_temp, subset=(datetime>=as.POSIXct('2016-06-20', tz='UTC') & datetime < as.POSIXct('2016-06-21', tz='UTC'))), aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='June 20 2016', x='date', y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme
# #looks fine, no artifacts
# 
# 
# ggplot(subset(buoy2016_vert_temp, subset=(datetime>=as.POSIXct('2016-07-01', tz='UTC') & datetime < as.POSIXct('2016-08-01', tz='UTC'))), aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='July 2016', x='date', y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme
# 
# #possible errant warm-up Jul 23/24
# ggplot(subset(buoy2016_vert_temp, subset=(datetime>=as.POSIXct('2016-07-23', tz='UTC') & datetime < as.POSIXct('2016-07-25', tz='UTC'))), aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='July 2016', x='date', y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme
# #not human-caused, leaving as is - if it was really warm and windy, this seems plausible
# 
# ggplot(subset(buoy2016_vert_temp, subset=(datetime>=as.POSIXct('2016-08-01', tz='UTC') & datetime < as.POSIXct('2016-09-01', tz='UTC'))), aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='Aug 2016', x='date', y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme
# 
# ggplot(subset(buoy2016_vert_temp, subset=(datetime>=as.POSIXct('2016-09-01', tz='UTC') & datetime < as.POSIXct('2016-10-01', tz='UTC'))), aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='Sept 2016', x='date', y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme
# 
# ggplot(subset(buoy2016_vert_temp, subset=(datetime>=as.POSIXct('2016-10-01', tz='UTC') & datetime < as.POSIXct('2016-11-01', tz='UTC'))), aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='Oct 2016', x='date', y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme
# 
# #buoy moved to harbor Oct 12 9:30
# ggplot(subset(buoy2016_vert_temp, subset=(datetime>=as.POSIXct('2016-10-12 6:00', tz='UTC') & datetime < as.POSIXct('2016-10-12 12:00', tz='UTC'))), aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='Oct 12 2016', x='date', y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme
# #no artifacts of buoy move

#assign buoy location
buoy2016_L1 <- buoy2016_L1 %>% 
  mutate(location = case_when(datetime >= as.POSIXct('2016-10-12 9:40', tz='UTC') ~ 'harbor',
                              TRUE ~ location))

buoy2016_vert_temp_L1 <- buoy2016_L1 %>%
  select(datetime, alltemp2016) %>%
  gather(variable, value, -datetime)


ggplot(subset(buoy2016_vert_temp_L1, subset=(datetime>=as.POSIXct('2016-01-01', tz='UTC') & datetime < as.POSIXct('2017-01-01', tz='UTC'))), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='2016, raw', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  final_theme


#reality check with LMP
buoy_LMP_2016 <- buoy2016_vert_temp_L1 %>%
  mutate(source='buoy') %>%
  full_join(., LMP2016_temp)
unique(LMP2016_temp$datetime)

#May 31, June 21, July 18, Aug 23, Sept 28
ggplot(subset(buoy_LMP_2016,
              subset=(datetime>=as.POSIXct('2016-05-31', tz='UTC') &
                        datetime<as.POSIXct('2016-06-01', tz='UTC'))),
       aes(x=datetime, y=value, color=as.factor(variable), shape = source)) +
  geom_point() +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
                              "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust

ggplot(subset(buoy_LMP_2016,
              subset=(datetime>=as.POSIXct('2016-06-21', tz='UTC') &
                        datetime<as.POSIXct('2016-06-22', tz='UTC'))),
       aes(x=datetime, y=value, color=as.factor(variable), shape = source)) +
  geom_point() +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
                              "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust

ggplot(subset(buoy_LMP_2016,
              subset=(datetime>=as.POSIXct('2016-07-18', tz='UTC') &
                        datetime<as.POSIXct('2016-07-19', tz='UTC'))),
       aes(x=datetime, y=value, color=as.factor(variable), shape = source)) +
  geom_point() +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
                              "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust

ggplot(subset(buoy_LMP_2016,
              subset=(datetime>=as.POSIXct('2016-08-23', tz='UTC') &
                        datetime<as.POSIXct('2016-08-24', tz='UTC'))),
       aes(x=datetime, y=value, color=as.factor(variable), shape = source)) +
  geom_point() +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
                              "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust

ggplot(subset(buoy_LMP_2016,
              subset=(datetime>=as.POSIXct('2016-09-28', tz='UTC') &
                        datetime<as.POSIXct('2016-09-29', tz='UTC'))),
       aes(x=datetime, y=value, color=as.factor(variable), shape = source)) +
  geom_point() +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
                              "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust

#export L1 tempstring file
buoy2016_L1 %>% 
  select(datetime, TempC_0m, TempC_1m, TempC_2m, TempC_3m, TempC_4m, TempC_5m, TempC_6m, TempC_7m, TempC_8m, TempC_9m) %>% 
  mutate(datetime = as.character(datetime)) %>% 
  write_csv(., 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2016_tempstring_L1.csv')


####upper DO####
buoy2016_vert_updo <- buoy2016_L1 %>% 
  select(datetime, upDO, location) %>% 
  gather(variable, value, -datetime, -location)

# ggplot(subset(buoy2016_vert_updo, subset=(datetime>=as.POSIXct('2016-01-01', tz='UTC') & datetime < as.POSIXct('2017-01-01', tz='UTC'))), aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='2016, raw', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme

buoy2016_L1 <- buoy2016_L1 %>% 
  mutate_at(vars(upDO),
            funs(case_when(. == -6999 ~ NA_real_,
                           TRUE ~ .)))

# buoy2016_vert_updo <- buoy2016_L1 %>% 
#   select(datetime, upDO, location) %>% 
#   gather(variable, value, -datetime, -location)
# 
# ggplot(subset(buoy2016_vert_updo, subset=(datetime>=as.POSIXct('2016-01-01', tz='UTC') & datetime < as.POSIXct('2017-01-01', tz='UTC'))), aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='2016, NA values recoded', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme
# 
# #remove data prior to sensor deployment 4-19-16 10:00
# ggplot(subset(buoy2016_vert_updo, subset=(datetime>=as.POSIXct('2016-04-19 6:00', tz='UTC') & datetime < as.POSIXct('2016-04-19 14:00', tz='UTC'))), aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='Apr 19 2016', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme

buoy2016_L1 <- buoy2016_L1 %>% 
  mutate_at(vars(upDO),
            funs(case_when(datetime <= as.POSIXct('2016-04-19 11:30') ~ NA_real_, #equilibration required more time after deployment
                           TRUE ~ .)))

# buoy2016_vert_updo <- buoy2016_L1 %>% 
#   select(datetime, upDO, location) %>% 
#   gather(variable, value, -datetime, -location)
# 
# ggplot(subset(buoy2016_vert_updo, subset=(datetime>=as.POSIXct('2016-01-01', tz='UTC') & datetime < as.POSIXct('2017-01-01', tz='UTC'))), aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='2016, NA values recoded, v2', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme
# 
# ggplot(subset(buoy2016_vert_updo, subset=(datetime>=as.POSIXct('2016-04-01', tz='UTC') & datetime < as.POSIXct('2016-05-01', tz='UTC'))), aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='Apr 2016', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme
# 
# ggplot(subset(buoy2016_vert_updo, subset=(datetime>=as.POSIXct('2016-05-01', tz='UTC') & datetime < as.POSIXct('2016-06-01', tz='UTC'))), aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='May 2016', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme
# 
# #remove artifacts of buoy move - 05-03 11a
# ggplot(subset(buoy2016_vert_updo, subset=(datetime>=as.POSIXct('2016-05-03 09:00', tz='UTC') & datetime < as.POSIXct('2016-05-03 13:00', tz='UTC'))), aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='May 3 2016', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme

buoy2016_L1 <- buoy2016_L1 %>% 
  mutate_at(vars(upDO),
            funs(case_when(datetime >= as.POSIXct('2016-05-03 10:10', tz='UTC') & datetime <= as.POSIXct('2016-05-03 12:30', tz='UTC') ~ NA_real_, #equilibration required more time after deployment
                           TRUE ~ .)))

# buoy2016_vert_updo_L1 <- buoy2016_L1 %>%
#   select(datetime, upDO, location) %>%
#   gather(variable, value, -datetime, -location)
# 
# ggplot(subset(buoy2016_vert_updo_L1, subset=(datetime>=as.POSIXct('2016-05-03 09:00', tz='UTC') & datetime < as.POSIXct('2016-05-03 13:00', tz='UTC'))), aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='May 3 2016, v1', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme
# 
# ggplot(subset(buoy2016_vert_updo_L1, subset=(datetime>=as.POSIXct('2016-05-01', tz='UTC') & datetime < as.POSIXct('2016-06-01', tz='UTC'))), aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='May 2016, v2', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme
# 
# #issues with thermisters May 18 and 19 - look to see if any artifacts
# ggplot(subset(buoy2016_vert_updo_L1, subset=(datetime>=as.POSIXct('2016-05-18', tz='UTC') & datetime < as.POSIXct('2016-05-20', tz='UTC'))), aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='mid May 2016', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme
#nothing egregious. leaving as is.

#small data gap present around May 22-24

# ggplot(subset(buoy2016_vert_updo_L1, subset=(datetime>=as.POSIXct('2016-06-01', tz='UTC') & datetime < as.POSIXct('2016-07-01', tz='UTC'))), aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='June 2016', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme
# 
# #buoy visit Jun 7 8:10 - do cleaned - no obvious signs (data gap before)
# ggplot(subset(buoy2016_vert_updo_L1, subset=(datetime>=as.POSIXct('2016-06-07', tz='UTC') & datetime < as.POSIXct('2016-06-08', tz='UTC'))), aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='June 7 2016', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme
# 
# #buoy visit June 14 8:15
# ggplot(subset(buoy2016_vert_updo_L1, subset=(datetime>=as.POSIXct('2016-06-14', tz='UTC') & datetime < as.POSIXct('2016-06-15', tz='UTC'))), aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='June 14 2016', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme
# #looks okay 
# 
# #buoy visit June 15 10:50
# ggplot(subset(buoy2016_vert_updo_L1, subset=(datetime>=as.POSIXct('2016-06-15', tz='UTC') & datetime < as.POSIXct('2016-06-16', tz='UTC'))), aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='June 15 2016', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme
# #looks okay
# 
# #buoy visit June 17 8:30
# ggplot(subset(buoy2016_vert_updo_L1, subset=(datetime>=as.POSIXct('2016-06-17', tz='UTC') & datetime < as.POSIXct('2016-06-18', tz='UTC'))), aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='June 17 2016', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme
# #looks okay
# 
# #buoy visit June 20 8:15
# ggplot(subset(buoy2016_vert_updo_L1, subset=(datetime>=as.POSIXct('2016-06-20', tz='UTC') & datetime < as.POSIXct('2016-06-21', tz='UTC'))), aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='June 7 2016', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme
# #looks fine
# 
# ggplot(subset(buoy2016_vert_updo_L1, subset=(datetime>=as.POSIXct('2016-07-01', tz='UTC') & datetime < as.POSIXct('2016-08-01', tz='UTC'))), aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='July 2016', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme
# 
# ggplot(subset(buoy2016_vert_updo_L1, subset=(datetime>=as.POSIXct('2016-08-01', tz='UTC') & datetime < as.POSIXct('2016-09-01', tz='UTC'))), aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='August 2016', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme
# 
# ggplot(subset(buoy2016_vert_updo_L1, subset=(datetime>=as.POSIXct('2016-09-01', tz='UTC') & datetime < as.POSIXct('2016-10-01', tz='UTC'))), aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='Sept 2016', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme
# 
# ggplot(subset(buoy2016_vert_updo_L1, subset=(datetime>=as.POSIXct('2016-10-01', tz='UTC') & datetime < as.POSIXct('2016-11-01', tz='UTC'))), aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='Oct 2016', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme
# 
# #buoy moved 10-12 9:30
# ggplot(subset(buoy2016_vert_updo_L1, subset=(datetime>=as.POSIXct('2016-10-12', tz='UTC') & datetime < as.POSIXct('2016-10-13', tz='UTC'))), aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='Oct 12 2016', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme

#remove buoy move artifacts
buoy2016_L1 <- buoy2016_L1 %>% 
  mutate_at(vars(upDO),
            funs(case_when(datetime >= as.POSIXct('2016-10-12 9:30', tz='UTC') & datetime <= as.POSIXct('2016-10-12 11:30', tz='UTC') ~ NA_real_, #equilibration required more time after deployment
                           TRUE ~ .)))
# buoy2016_vert_updo_L1 <- buoy2016_L1 %>%
#   select(datetime, upDO, location) %>%
#   gather(variable, value, -datetime, -location)
# 
# ggplot(subset(buoy2016_vert_updo_L1, subset=(datetime>=as.POSIXct('2016-10-12', tz='UTC') & datetime < as.POSIXct('2016-10-13', tz='UTC'))), aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='Oct 12 2016, v2', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme
# 
# ggplot(subset(buoy2016_vert_updo_L1, subset=(datetime>=as.POSIXct('2016-10-01', tz='UTC') & datetime < as.POSIXct('2016-11-01', tz='UTC'))), aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='Oct 2016, v2', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme
# 
# ggplot(subset(buoy2016_vert_updo_L1, subset=(datetime>=as.POSIXct('2016-11-01', tz='UTC') & datetime < as.POSIXct('2016-12-01', tz='UTC'))), aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='Nov 2016', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme
# 
# ggplot(subset(buoy2016_vert_updo_L1, subset=(datetime>=as.POSIXct('2016-12-01', tz='UTC') & datetime < as.POSIXct('2017-01-01', tz='UTC'))), aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='Dec 2016', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme
# 
# ggplot(subset(buoy2016_vert_updo_L1, subset=(datetime>=as.POSIXct('2016-01-01', tz='UTC') & datetime < as.POSIXct('2017-01-01', tz='UTC'))), aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='2016, v2', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme

# #compare with LMP
# buoy_lmp <- full_join(buoy2016_vert_updo_L1, LMP2016_upDO) %>% 
#   mutate(source = case_when(is.na(source) ~ 'buoy',
#                               TRUE ~ source))
# 
# unique(LMP2016_upDO$datetime)
# 
# #may 31
# ggplot(subset(buoy_lmp, subset=(datetime>=as.POSIXct('2016-05-31', tz='UTC') & datetime < as.POSIXct('2016-06-01', tz='UTC'))), aes(x=datetime, y=value, col=source)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='May 31', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme
# ggplot(subset(buoy_lmp, subset=(datetime>=as.POSIXct('2016-05-15', tz='UTC') & datetime < as.POSIXct('2016-06-15', tz='UTC'))), aes(x=datetime, y=value, col=source)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='May 31', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme
# 
# #june 21
# ggplot(subset(buoy_lmp, subset=(datetime>=as.POSIXct('2016-06-21', tz='UTC') & datetime < as.POSIXct('2016-06-22', tz='UTC'))), aes(x=datetime, y=value, col=source)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='June 21', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme
# ggplot(subset(buoy_lmp, subset=(datetime>=as.POSIXct('2016-06-01', tz='UTC') & datetime < as.POSIXct('2016-07-01', tz='UTC'))), aes(x=datetime, y=value, col=source)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='June 21', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme
# 
# #jul 18
# ggplot(subset(buoy_lmp, subset=(datetime>=as.POSIXct('2016-07-18', tz='UTC') & datetime < as.POSIXct('2016-07-19', tz='UTC'))), aes(x=datetime, y=value, col=source)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='Jul 18', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme
# ggplot(subset(buoy_lmp, subset=(datetime>=as.POSIXct('2016-07-01', tz='UTC') & datetime < as.POSIXct('2016-08-01', tz='UTC'))), aes(x=datetime, y=value, col=source)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='Jul 18', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme
# 
# #aug 23
# ggplot(subset(buoy_lmp, subset=(datetime>=as.POSIXct('2016-08-23', tz='UTC') & datetime < as.POSIXct('2016-08-24', tz='UTC'))), aes(x=datetime, y=value, col=source)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='Aug 23', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme
# ggplot(subset(buoy_lmp, subset=(datetime>=as.POSIXct('2016-08-10', tz='UTC') & datetime < as.POSIXct('2016-09-10', tz='UTC'))), aes(x=datetime, y=value, col=source)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='Aug 23', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme
# 
# #sept 28
# ggplot(subset(buoy_lmp, subset=(datetime>=as.POSIXct('2016-09-28', tz='UTC') & datetime < as.POSIXct('2016-09-29', tz='UTC'))), aes(x=datetime, y=value, col=source)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='Sept 28', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme
# ggplot(subset(buoy_lmp, subset=(datetime>=as.POSIXct('2016-09-15', tz='UTC') & datetime < as.POSIXct('2016-10-15', tz='UTC'))), aes(x=datetime, y=value, col=source)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='Sept 28', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme
# 
# #ppm consistently higher than LMP by ~0.6ppm, sat higher until July, where it is within range of the day.

####low do####
# buoy2016_vert_lowdo <- buoy2016_L1 %>% 
#   select(datetime,location, lowDO) %>% 
#   gather(variable, value, -datetime, -location)
# 
# ggplot(subset(buoy2016_vert_lowdo, subset=(datetime>=as.POSIXct('2016-01-01', tz='UTC') & datetime < as.POSIXct('2017-01-01', tz='UTC'))), aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='2016, raw', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme

buoy2016_L1 <- buoy2016_L1 %>% 
  mutate_at(vars(lowDO),
            funs(case_when(. == -6999 ~ NA_real_,
                           TRUE ~ .)))

# buoy2016_vert_lowdo <- buoy2016_L1 %>% 
#   select(datetime,location, lowDO) %>% 
#   gather(variable, value, -datetime, -location)
# 
# ggplot(subset(buoy2016_vert_lowdo, subset=(datetime>=as.POSIXct('2016-01-01', tz='UTC') & datetime < as.POSIXct('2017-01-01', tz='UTC'))), aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='2016, NA strings recoded', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme
# 
# #recode data prior to moving to loon - sensor not at correct depth
# ggplot(subset(buoy2016_vert_lowdo, subset=(datetime>=as.POSIXct('2016-05-03 09:00', tz='UTC') & datetime < as.POSIXct('2016-05-03 15:00', tz='UTC'))), aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='May 3 2016', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme

buoy2016_L1 <- buoy2016_L1 %>% 
  mutate_at(vars(lowDO),
            funs(case_when(datetime <= as.POSIXct('2016-05-03 11:50', tz='UTC') ~ NA_real_, #equilibration required more time after deployment
                           TRUE ~ .)))

# buoy2016_vert_lowdo <- buoy2016_L1 %>% 
#   select(datetime,location, lowDO) %>% 
#   gather(variable, value, -datetime, -location)
# 
# ggplot(subset(buoy2016_vert_lowdo, subset=(datetime>=as.POSIXct('2016-05-01', tz='UTC') & datetime < as.POSIXct('2016-06-01', tz='UTC'))), aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='May 2016, v2', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme
# 
# #errant data on may 18 and 19
# ggplot(subset(buoy2016_vert_lowdo, subset=(datetime>=as.POSIXct('2016-05-18 10:00', tz='UTC') & datetime < as.POSIXct('2016-05-18 14:00', tz='UTC'))), aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='mid May 2016', x='date', y='') +
#   final_theme
# 
# ggplot(subset(buoy2016_vert_lowdo, subset=(datetime>=as.POSIXct('2016-05-19 3:00', tz='UTC') & datetime < as.POSIXct('2016-05-19 7:00', tz='UTC'))), aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='mid May 2016', x='date', y='') +
#   final_theme
# 
# ggplot(subset(buoy2016_vert_lowdo_L1, subset=(datetime>=as.POSIXct('2016-05-21 12:00', tz='UTC') & datetime < as.POSIXct('2016-05-21 16:00', tz='UTC'))), aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='mid May 2016', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme


buoy2016_L1 <- buoy2016_L1 %>% 
  mutate_at(vars(lowDO),
            funs(case_when(datetime == as.POSIXct('2016-05-18 11:00', tz='UTC') ~ NA_real_,
                           datetime == as.POSIXct('2016-05-19 5:10', tz='UTC') ~ NA_real_,
                           datetime == as.POSIXct('2016-05-21 14:10', tz='UTC') ~ NA_real_,
                           TRUE ~ .)))

# buoy2016_vert_lowdo_L1 <- buoy2016_L1 %>% 
#   select(datetime,location, lowDO) %>% 
#   gather(variable, value, -datetime, -location)
# 
# ggplot(subset(buoy2016_vert_lowdo_L1, subset=(datetime>=as.POSIXct('2016-05-01', tz='UTC') & datetime < as.POSIXct('2016-06-01', tz='UTC'))), aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='May 2016, v3', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme
# 
# #short data gap mid/late May
# 
# ggplot(subset(buoy2016_vert_lowdo, subset=(datetime>=as.POSIXct('2016-06-01', tz='UTC') & datetime < as.POSIXct('2016-07-01', tz='UTC'))), aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='June 2016', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme

#do low temp starting to show noise in signal

#patchy data early June

#select errant data by low sat field
buoy2016_L1 <- buoy2016_L1 %>% 
  mutate_at(vars(lowDO),
            funs(case_when(datetime >= as.POSIXct('2016-06-01', tz='UTC') & 
                             datetime < as.POSIXct('2016-07-01', tz='UTC') &
                             DOLowSat < 25 ~ NA_real_,
                           TRUE ~ .)))
# buoy2016_vert_lowdo_L1 <- buoy2016_L1 %>% 
#   select(datetime,location, lowDO) %>% 
#   gather(variable, value, -datetime, -location)
# 
# 
# ggplot(subset(buoy2016_vert_lowdo_L1, subset=(datetime>=as.POSIXct('2016-06-01', tz='UTC') & datetime < as.POSIXct('2016-07-01', tz='UTC'))), aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='June 2016, v2', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme

#select beginning of June, low ppm <10.5
buoy2016_L1 <- buoy2016_L1 %>% 
  mutate_at(vars(lowDO),
            funs(case_when(datetime >= as.POSIXct('2016-06-01', tz='UTC') & 
                             datetime < as.POSIXct('2016-06-13', tz='UTC') &
                             DOLowPPM < 10.5 ~ NA_real_,
                           TRUE ~ .)))
# buoy2016_vert_lowdo_L1 <- buoy2016_L1 %>% 
#   select(datetime,location, lowDO) %>% 
#   gather(variable, value, -datetime, -location)
# 
# ggplot(subset(buoy2016_vert_lowdo_L1, subset=(datetime>=as.POSIXct('2016-06-01', tz='UTC') & datetime < as.POSIXct('2016-07-01', tz='UTC'))), aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='June 2016, v3', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme
# 
# #June 7
# ggplot(subset(buoy2016_vert_lowdo_L1, subset=(datetime>=as.POSIXct('2016-06-06', tz='UTC') & datetime < as.POSIXct('2016-06-09', tz='UTC'))), aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='June 7 2016', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme
# 
# #June 14
# ggplot(subset(buoy2016_vert_lowdo_L1, subset=(datetime>=as.POSIXct('2016-06-13', tz='UTC') & datetime < as.POSIXct('2016-06-16', tz='UTC'))), aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='June 14 2016', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme
# 
# #June 15
# ggplot(subset(buoy2016_vert_lowdo_L1, subset=(datetime>=as.POSIXct('2016-06-15 6:00', tz='UTC') & datetime < as.POSIXct('2016-06-15 12:00', tz='UTC'))), aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='June 15 2016', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme

#recode artifacts from buoy visit
buoy2016_L1 <- buoy2016_L1 %>% 
  mutate_at(vars(lowDO),
            funs(case_when(datetime == as.POSIXct('2016-06-15 10:30', tz='UTC') ~ NA_real_,
                           TRUE ~ .)))
# buoy2016_vert_lowdo_L1 <- buoy2016_L1 %>% 
#   select(datetime,location, lowDO) %>% 
#   gather(variable, value, -datetime, -location)
# 
# ggplot(subset(buoy2016_vert_lowdo_L1, subset=(datetime>=as.POSIXct('2016-06-15', tz='UTC') & datetime < as.POSIXct('2016-06-16', tz='UTC'))), aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='June 15 2016, v2', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme
# 
# #Jun 20
# ggplot(subset(buoy2016_vert_lowdo_L1, subset=(datetime>=as.POSIXct('2016-06-20 6:00', tz='UTC') & datetime < as.POSIXct('2016-06-20 12:00', tz='UTC'))), aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='June 20 2016', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme

#recode artifacts from buoy visit
buoy2016_L1 <- buoy2016_L1 %>% 
  mutate_at(vars(lowDO),
            funs(case_when(datetime == as.POSIXct('2016-06-20 8:40', tz='UTC') ~ NA_real_,
                           TRUE ~ .)))
# buoy2016_vert_lowdo_L1 <- buoy2016_L1 %>% 
#   select(datetime,location, lowDO) %>% 
#   gather(variable, value, -datetime, -location)
# 
# ggplot(subset(buoy2016_vert_lowdo_L1, subset=(datetime>=as.POSIXct('2016-06-20 6:00', tz='UTC') & datetime < as.POSIXct('2016-06-20 12:00', tz='UTC'))), aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='June 20 2016, v2', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme
# 
# ggplot(subset(buoy2016_vert_lowdo_L1, subset=(datetime>=as.POSIXct('2016-06-01', tz='UTC') & datetime < as.POSIXct('2016-07-01', tz='UTC'))), aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='June 2016, 3', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme
# 
# ggplot(subset(buoy2016_vert_lowdo, subset=(datetime>=as.POSIXct('2016-07-01', tz='UTC') & datetime < as.POSIXct('2016-08-01', tz='UTC'))), aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='July 2016', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme
# 
# ggplot(subset(buoy2016_vert_lowdo, subset=(datetime>=as.POSIXct('2016-08-01', tz='UTC') & datetime < as.POSIXct('2016-09-01', tz='UTC'))), aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='August 2016', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme
# 
# ggplot(subset(buoy2016_vert_lowdo, subset=(datetime>=as.POSIXct('2016-09-01', tz='UTC') & datetime < as.POSIXct('2016-10-01', tz='UTC'))), aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='Sept 2016', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme
# 
# ggplot(subset(buoy2016_vert_lowdo, subset=(datetime>=as.POSIXct('2016-10-01', tz='UTC') & datetime < as.POSIXct('2016-11-01', tz='UTC'))), aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='Oct 2016', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme
# 
# ggplot(subset(buoy2016_vert_lowdo, subset=(datetime>=as.POSIXct('2016-10-12', tz='UTC') & datetime < as.POSIXct('2016-10-13', tz='UTC'))), aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='Oct 12 2016', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme

#recode artifact of buoy move
buoy2016_L1 <- buoy2016_L1 %>% 
  mutate_at(vars(lowDO),
            funs(case_when(datetime >= as.POSIXct('2016-10-12 9:40', tz='UTC') ~ NA_real_,
                           TRUE ~ .)))
# buoy2016_vert_lowdo_L1 <- buoy2016_L1 %>% 
#   select(datetime,location, lowDO) %>% 
#   gather(variable, value, -datetime, -location)
# 
# ggplot(subset(buoy2016_vert_lowdo_L1, subset=(datetime>=as.POSIXct('2016-10-12', tz='UTC') & datetime < as.POSIXct('2016-10-13', tz='UTC'))), aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='Oct 12 2016, v2', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme
# 
# ggplot(subset(buoy2016_vert_lowdo_L1, subset=(datetime>=as.POSIXct('2016-10-01', tz='UTC') & datetime < as.POSIXct('2016-11-01', tz='UTC'))), aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='Oct 2016, v2', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme
# 
# ggplot(subset(buoy2016_vert_lowdo_L1, subset=(datetime>=as.POSIXct('2016-01-01', tz='UTC') & datetime < as.POSIXct('2017-01-01', tz='UTC'))), aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='2016, v2', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme

# compare with lmp data#
# buoy_lmp_lowdo <- full_join(buoy2016_vert_lowdo_L1, LMP2016_lowDO) %>%
#   mutate(source = case_when(is.na(source) ~ 'buoy',
#                               TRUE ~ source))
# 
# unique(LMP2016_lowDO$datetime)
# 
# #may 31
# ggplot(subset(buoy_lmp_lowdo, subset=(datetime>=as.POSIXct('2016-05-31', tz='UTC') & datetime < as.POSIXct('2016-06-01', tz='UTC'))), aes(x=datetime, y=value, col=source)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='May 31', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme
# ggplot(subset(buoy_lmp_lowdo, subset=(datetime>=as.POSIXct('2016-05-15', tz='UTC') & datetime < as.POSIXct('2016-06-15', tz='UTC'))), aes(x=datetime, y=value, col=source)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='May 31', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme
# 
# #june 21
# ggplot(subset(buoy_lmp_lowdo, subset=(datetime>=as.POSIXct('2016-06-21', tz='UTC') & datetime < as.POSIXct('2016-06-22', tz='UTC'))), aes(x=datetime, y=value, col=source)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='June 21', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme
# ggplot(subset(buoy_lmp_lowdo, subset=(datetime>=as.POSIXct('2016-06-01', tz='UTC') & datetime < as.POSIXct('2016-07-01', tz='UTC'))), aes(x=datetime, y=value, col=source)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='June 21', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme
# 
# #jul 18
# ggplot(subset(buoy_lmp_lowdo, subset=(datetime>=as.POSIXct('2016-07-18', tz='UTC') & datetime < as.POSIXct('2016-07-19', tz='UTC'))), aes(x=datetime, y=value, col=source)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='Jul 18', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme
# ggplot(subset(buoy_lmp_lowdo, subset=(datetime>=as.POSIXct('2016-07-01', tz='UTC') & datetime < as.POSIXct('2016-08-01', tz='UTC'))), aes(x=datetime, y=value, col=source)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='Jul 18', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme
# 
# #aug 23
# ggplot(subset(buoy_lmp_lowdo, subset=(datetime>=as.POSIXct('2016-08-23', tz='UTC') & datetime < as.POSIXct('2016-08-24', tz='UTC'))), aes(x=datetime, y=value, col=source)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='Aug 23', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme
# ggplot(subset(buoy_lmp_lowdo, subset=(datetime>=as.POSIXct('2016-08-10', tz='UTC') & datetime < as.POSIXct('2016-09-10', tz='UTC'))), aes(x=datetime, y=value, col=source)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='Aug 23', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme
# 
# #sept 28
# ggplot(subset(buoy_lmp_lowdo, subset=(datetime>=as.POSIXct('2016-09-28', tz='UTC') & datetime < as.POSIXct('2016-09-29', tz='UTC'))), aes(x=datetime, y=value, col=source)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='Sept 28', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme
# ggplot(subset(buoy_lmp_lowdo, subset=(datetime>=as.POSIXct('2016-09-15', tz='UTC') & datetime < as.POSIXct('2016-10-15', tz='UTC'))), aes(x=datetime, y=value, col=source)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='Sept 28', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme

#looks good against lmp

#### wind data ####
# buoy2016_vert_wind <- buoy2016_L1 %>% 
#   select(datetime, location, wind2016) %>% 
#   gather(variable, value, -datetime, -location)
# 
# ggplot(subset(buoy2016_vert_wind, subset=(datetime>=as.POSIXct('2016-01-01', tz='UTC') & datetime < as.POSIXct('2017-01-01', tz='UTC'))), aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='2016, raw', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme
# 
# ggplot(subset(buoy2016_vert_wind, subset=(datetime>=as.POSIXct('2016-01-01', tz='UTC') & datetime < as.POSIXct('2016-02-01', tz='UTC'))), aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='Jan 2016', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme
# 
# #wind sensor frozen Jan 18
# ggplot(subset(buoy2016_vert_wind, subset=(datetime>=as.POSIXct('2016-01-18 2:00', tz='UTC') & datetime < as.POSIXct('2016-01-18 12:00', tz='UTC'))), aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='Jan 18 2016', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme

#select by jan 18 &max wind =0
buoy2016_L1 <- buoy2016_L1 %>% 
  mutate_at(vars(WindSpdAv:MaxWindDir),
            funs(case_when(datetime >= as.POSIXct('2016-01-18 3:10', tz='UTC') &
                             datetime < as.POSIXct('2016-01-18 9:40', tz='UTC')  ~ NA_real_,
                           TRUE ~ .)))
# buoy2016_vert_wind_L1 <- buoy2016_L1 %>% 
#   select(datetime, location, wind2016) %>% 
#   gather(variable, value, -datetime, -location)
# 
# ggplot(subset(buoy2016_vert_wind_L1, subset=(datetime>=as.POSIXct('2016-01-01', tz='UTC') & datetime < as.POSIXct('2016-02-01', tz='UTC'))), aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='Jan 2016, v2', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme
# 
# ggplot(subset(buoy2016_vert_wind, subset=(datetime>=as.POSIXct('2016-02-01', tz='UTC') & datetime < as.POSIXct('2016-03-01', tz='UTC'))), aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='Feb 2016', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme
# 
# #wind sensor frozen feb 9 and feb 24
# ggplot(subset(buoy2016_vert_wind, subset=(datetime>=as.POSIXct('2016-02-08 23:00', tz='UTC') & datetime < as.POSIXct('2016-02-09 13:00', tz='UTC'))), aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='Feb 9 2016', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme
# ggplot(subset(buoy2016_vert_wind, subset=(datetime>=as.POSIXct('2016-02-24 10:00', tz='UTC') & datetime < as.POSIXct('2016-02-24 21:00', tz='UTC'))), aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='Feb 2016', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme

#select by feb 9 and 24 & max wind =0
buoy2016_L1 <- buoy2016_L1 %>% 
  mutate_at(vars(WindSpdAv:MaxWindDir),
            funs(case_when(datetime >= as.POSIXct('2016-02-09 00:10', tz='UTC') &
                             datetime < as.POSIXct('2016-02-09 12:30', tz='UTC') ~ NA_real_,
                           datetime >= as.POSIXct('2016-02-24 11:40', tz='UTC') &
                             datetime < as.POSIXct('2016-02-24 17:40', tz='UTC') ~ NA_real_,
                           TRUE ~ .)))
# buoy2016_vert_wind_L1 <- buoy2016_L1 %>% 
#   select(datetime, location, wind2016) %>% 
#   gather(variable, value, -datetime, -location)
# 
# ggplot(subset(buoy2016_vert_wind_L1, subset=(datetime>=as.POSIXct('2016-02-01', tz='UTC') & datetime < as.POSIXct('2016-03-01', tz='UTC'))), aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='Feb 2016, v2', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme
# 
# 
# ggplot(subset(buoy2016_vert_wind, subset=(datetime>=as.POSIXct('2016-03-01', tz='UTC') & datetime < as.POSIXct('2016-04-01', tz='UTC'))), aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='Mar 2016', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme
# 
# #wind sensor frozen Mar 25
# ggplot(subset(buoy2016_vert_wind, subset=(datetime>=as.POSIXct('2016-03-25 6:00', tz='UTC') & datetime < as.POSIXct('2016-03-25 14:00', tz='UTC'))), aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='Mar 2016', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme

buoy2016_L1 <- buoy2016_L1 %>% 
  mutate_at(vars(wind2016),
            funs(case_when(datetime >= as.POSIXct('2016-03-25 9:10', tz='UTC') &
                             datetime < as.POSIXct('2016-03-25 12:10', tz='UTC') ~ NA_real_,
                           TRUE ~ .)))
# buoy2016_vert_wind_L1 <- buoy2016_L1 %>% 
#   select(datetime, location, wind2016) %>% 
#   gather(variable, value, -datetime, -location)
# 
# ggplot(subset(buoy2016_vert_wind_L1, subset=(datetime>=as.POSIXct('2016-03-01', tz='UTC') & datetime < as.POSIXct('2016-04-01', tz='UTC'))), aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='Mar 2016, v2', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme
# 
# ggplot(subset(buoy2016_vert_wind, subset=(datetime>=as.POSIXct('2016-04-01', tz='UTC') & datetime < as.POSIXct('2016-05-01', tz='UTC'))), aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='Apr 2016', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme
# 
# ggplot(subset(buoy2016_vert_wind, subset=(datetime>=as.POSIXct('2016-05-01', tz='UTC') & datetime < as.POSIXct('2016-06-01', tz='UTC'))), aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='May 2016', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme
# 
# ggplot(subset(buoy2016_vert_wind, subset=(datetime>=as.POSIXct('2016-06-01', tz='UTC') & datetime < as.POSIXct('2016-07-01', tz='UTC'))), aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='Jun 2016', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme
# 
# ggplot(subset(buoy2016_vert_wind, subset=(datetime>=as.POSIXct('2016-07-01', tz='UTC') & datetime < as.POSIXct('2016-08-01', tz='UTC'))), aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='Jul 2016', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme
# 
# ggplot(subset(buoy2016_vert_wind, subset=(datetime>=as.POSIXct('2016-08-01', tz='UTC') & datetime < as.POSIXct('2016-09-01', tz='UTC'))), aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='Aug 2016', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme
# 
# ggplot(subset(buoy2016_vert_wind, subset=(datetime>=as.POSIXct('2016-09-01', tz='UTC') & datetime < as.POSIXct('2016-10-01', tz='UTC'))), aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='Sept 2016', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme
# 
# ggplot(subset(buoy2016_vert_wind, subset=(datetime>=as.POSIXct('2016-10-01', tz='UTC') & datetime < as.POSIXct('2016-11-01', tz='UTC'))), aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='Oct 2016', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme
# 
# ggplot(subset(buoy2016_vert_wind, subset=(datetime>=as.POSIXct('2016-11-01', tz='UTC') & datetime < as.POSIXct('2016-12-01', tz='UTC'))), aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='Nov 2016', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme
# 
# ggplot(subset(buoy2016_vert_wind, subset=(datetime>=as.POSIXct('2016-12-01', tz='UTC') & datetime < as.POSIXct('2017-01-01', tz='UTC'))), aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='Dec 2016', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme
# 
# #sensor frozen Dec 18 and 28
# ggplot(subset(buoy2016_vert_wind, subset=(datetime>=as.POSIXct('2016-12-18 2:00', tz='UTC') & datetime < as.POSIXct('2016-12-18 12:00', tz='UTC'))), aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='Dec 18 2016', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme
# ggplot(subset(buoy2016_vert_wind, subset=(datetime>=as.POSIXct('2016-12-29 18:00', tz='UTC') & datetime < as.POSIXct('2016-12-30 12:00', tz='UTC'))), aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='Dec 29 2016', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme

buoy2016_L1 <- buoy2016_L1 %>% 
  mutate_at(vars(WindSpdAv:MaxWindDir),
            funs(case_when(datetime >= as.POSIXct('2016-12-18 3:40', tz='UTC') &
                             datetime < as.POSIXct('2016-12-18 7:40', tz='UTC') ~ NA_real_,
                           datetime >= as.POSIXct('2016-12-29 19:10', tz='UTC') &
                             datetime < as.POSIXct('2016-12-30 11:50', tz='UTC') ~ NA_real_,
                           TRUE ~ .)))
# buoy2016_vert_wind_L1 <- buoy2016_L1 %>% 
#   select(datetime, location, wind2016) %>% 
#   gather(variable, value, -datetime, -location)
# 
# ggplot(subset(buoy2016_vert_wind_L1, subset=(datetime>=as.POSIXct('2016-12-01', tz='UTC') & datetime < as.POSIXct('2017-01-01', tz='UTC'))), aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='Dec 2016, v2', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme

####chlorophyll sensor####
# buoy2016_vert_chla <- buoy2016_L1 %>% 
#   select(datetime, location, chla) %>% 
#   gather(variable, value, -datetime, -location)
# ggplot(subset(buoy2016_vert_chla, subset=(datetime>=as.POSIXct('2016-01-01', tz='UTC') & datetime < as.POSIXct('2017-01-01', tz='UTC'))), aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='2016', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme

buoy2016_L1 <- buoy2016_L1 %>% 
  mutate_at(vars(chla),
            funs(case_when(. == -6999 ~ NA_real_,
                           TRUE ~ .)))
# buoy2016_vert_chla <- buoy2016_L1 %>% 
#   select(datetime, location, chla) %>% 
#   gather(variable, value, -datetime, -location)
# 
# ggplot(subset(buoy2016_vert_chla, subset=(datetime>=as.POSIXct('2016-01-01', tz='UTC') & datetime < as.POSIXct('2017-01-01', tz='UTC'))), aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='2016, v2', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme
# 
# ggplot(subset(buoy2016_vert_chla, subset=(datetime>=as.POSIXct('2016-04-01', tz='UTC') & datetime < as.POSIXct('2016-05-01', tz='UTC'))), aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='Apr 2016', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme
# 
# #remove na strings prior to deployment of probe
# ggplot(subset(buoy2016_vert_chla, subset=(datetime>=as.POSIXct('2016-04-19 8:00', tz='UTC') & datetime < as.POSIXct('2016-04-19 13:00', tz='UTC'))), aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='Apr 19 2016', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme

buoy2016_L1 <- buoy2016_L1 %>% 
  mutate_at(vars(chla),
            funs(case_when(datetime <= as.POSIXct('2016-04-19 11:10', tz='UTC') ~ NA_real_,
                           TRUE ~ .)))
# buoy2016_vert_chla_L1 <- buoy2016_L1 %>% 
#   select(datetime, location, chla) %>% 
#   gather(variable, value, -datetime, -location)
# 
# ggplot(subset(buoy2016_vert_chla_L1, subset=(datetime>=as.POSIXct('2016-04-01', tz='UTC') & datetime < as.POSIXct('2016-05-01', tz='UTC'))), aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='Apr 2016', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme
# 
# ggplot(subset(buoy2016_vert_chla, subset=(datetime>=as.POSIXct('2016-05-01', tz='UTC') & datetime < as.POSIXct('2016-06-01', tz='UTC'))), aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='May 2016', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme
# 
# #remove artifacts of buoy move
# ggplot(subset(buoy2016_vert_chla, subset=(datetime>=as.POSIXct('2016-05-03 8:00', tz='UTC') & datetime < as.POSIXct('2016-05-03 14:00', tz='UTC'))), aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='May 2016', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme

buoy2016_L1 <- buoy2016_L1 %>% 
  mutate_at(vars(chla),
            funs(case_when(datetime >= as.POSIXct('2016-05-03 9:00', tz='UTC') &
                             datetime < as.POSIXct('2016-05-03 12:00', tz='uTC')~ NA_real_,
                           TRUE ~ .)))
# buoy2016_vert_chla_L1 <- buoy2016_L1 %>% 
#   select(datetime, location, chla) %>% 
#   gather(variable, value, -datetime, -location)
# 
# ggplot(subset(buoy2016_vert_chla_L1, subset=(datetime>=as.POSIXct('2016-05-01', tz='UTC') & datetime < as.POSIXct('2016-06-01', tz='UTC'))), aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='May 2016, v2', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme
# 
# ggplot(subset(buoy2016_vert_chla, subset=(datetime>=as.POSIXct('2016-06-01', tz='UTC') & datetime < as.POSIXct('2016-07-01', tz='UTC'))), aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='Jun 2016', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme

buoy2016_L1 <- buoy2016_L1 %>% 
  mutate(SpecCond = case_when(datetime >= as.POSIXct('2016-06-07', tz='UTC') &
                                datetime < as.POSIXct('2016-06-08', tz='UTC') &
                                SpecCond < 5 ~ NA_real_,
                           TRUE ~ SpecCond))
# buoy2016_vert_chla_L1 <- buoy2016_L1 %>% 
#   select(datetime, location, chla) %>% 
#   gather(variable, value, -datetime, -location)
# 
# ggplot(subset(buoy2016_vert_chla_L1, subset=(datetime>=as.POSIXct('2016-06-01', tz='UTC') & datetime < as.POSIXct('2016-07-01', tz='UTC'))), aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='Jun 2016, v2', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme
# 
# ggplot(subset(buoy2016_vert_chla, subset=(datetime>=as.POSIXct('2016-07-01', tz='UTC') & datetime < as.POSIXct('2016-08-01', tz='UTC'))), aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='Jul 2016', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme
# 
# ggplot(subset(buoy2016_vert_chla, subset=(datetime>=as.POSIXct('2016-08-01', tz='UTC') & datetime < as.POSIXct('2016-09-01', tz='UTC'))), aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='Aug 2016', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme
# 
# ggplot(subset(buoy2016_vert_chla, subset=(datetime>=as.POSIXct('2016-09-01', tz='UTC') & datetime < as.POSIXct('2016-10-01', tz='UTC'))), aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='Sept 2016', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme
# 
# ggplot(subset(buoy2016_vert_chla, subset=(datetime>=as.POSIXct('2016-10-01', tz='UTC') & datetime < as.POSIXct('2016-11-01', tz='UTC'))), aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='Oct 2016', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme
# 
# #remove buoy move artifacts
# ggplot(subset(buoy2016_vert_chla, subset=(datetime>=as.POSIXct('2016-10-12 6:00', tz='UTC') & datetime < as.POSIXct('2016-10-12 14:00', tz='UTC'))), aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='Oct 12 2016', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme

buoy2016_L1 <- buoy2016_L1 %>% 
  mutate_at(vars(chla),
            funs(case_when(datetime >= as.POSIXct('2016-10-12 9:40', tz='UTC') ~ NA_real_,
                           TRUE ~ .)))
# buoy2016_vert_chla_L1 <- buoy2016_L1 %>%
#   select(datetime, location, chla) %>%
#   gather(variable, value, -datetime, -location)
# 
# ggplot(subset(buoy2016_vert_chla_L1, subset=(datetime>=as.POSIXct('2016-10-01', tz='UTC') & datetime < as.POSIXct('2016-11-01', tz='UTC'))), aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='Oct 2016, v2', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme
# 
# ggplot(subset(buoy2016_vert_chla_L1, subset=(datetime>=as.POSIXct('2016-01-01', tz='UTC') & datetime < as.POSIXct('2017-01-01', tz='UTC'))), aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='2016, v2', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme

#plot chla with LMP data
# buoy_lmp_chla_vert <- full_join(buoy2016_vert_chla_L1, LMP2016_chla) %>%
#   mutate(source = case_when(is.na(source) ~ 'buoy',
#                               TRUE ~ source)) %>% 
#   filter(variable == 'Chlor_UGL')
# 
# unique(LMP2016_chla$datetime)
# 
# ggplot(subset(buoy_lmp_chla_vert, subset=(datetime>=as.POSIXct('2016-05-31', tz='UTC') & datetime<as.POSIXct('2016-06-01', tz='UTC'))), aes(x=datetime, y=value, color=source)) +
#   geom_point() +
#   labs(title='May 31, 2016', x='date', y=' ') +
#   final_theme
# 
# ggplot(subset(buoy_lmp_chla_vert, subset=(datetime>=as.POSIXct('2016-06-21', tz='UTC') & datetime<as.POSIXct('2016-06-22', tz='UTC'))), aes(x=datetime, y=value, color=source)) +
#   geom_point() +
#   labs(title='June 21, 2016', x='date', y=' ') +
#   final_theme
# 
# ggplot(subset(buoy_lmp_chla_vert, subset=(datetime>=as.POSIXct('2016-07-18', tz='UTC') & datetime<as.POSIXct('2016-07-19', tz='UTC'))), aes(x=datetime, y=value, color=source)) +
#   geom_point() +
#   labs(title='Jul 18, 2016', x='date', y=' ') +
#   final_theme
# 
# ggplot(subset(buoy_lmp_chla_vert, subset=(datetime>=as.POSIXct('2016-08-23', tz='UTC') & datetime<as.POSIXct('2016-08-24', tz='UTC'))), aes(x=datetime, y=value, color=source)) +
#   geom_point() +
#   labs(title='Aug 23, 2016', x='date', y=' ') +
#   final_theme
# 
# ggplot(subset(buoy_lmp_chla_vert, subset=(datetime>=as.POSIXct('2016-09-28', tz='UTC') & datetime<as.POSIXct('2016-09-29', tz='UTC'))), aes(x=datetime, y=value, color=source)) +
#   geom_point() +
#   labs(title='Septh 28, 2016', x='date', y=' ') +
#   final_theme

#all chla looks consistent with LMP


#plot chla with LMP data
# buoy_lmp_cond_vert <- full_join(buoy2016_vert_chla_L1, LMP2016_cond) %>%
#   mutate(source = case_when(is.na(source) ~ 'buoy',
#                             TRUE ~ source)) %>% 
#   filter(variable == 'SpecCond')
# 
# ggplot(subset(buoy_lmp_cond_vert, subset=(datetime>=as.POSIXct('2016-05-31', tz='UTC') & datetime<as.POSIXct('2016-06-01', tz='UTC'))), aes(x=datetime, y=value, color=source)) +
#   geom_point() +
#   labs(title='May 31, 2016', x='date', y=' ') +
#   final_theme
# 
# ggplot(subset(buoy_lmp_cond_vert, subset=(datetime>=as.POSIXct('2016-06-21', tz='UTC') & datetime<as.POSIXct('2016-06-22', tz='UTC'))), aes(x=datetime, y=value, color=source)) +
#   geom_point() +
#   labs(title='June 21, 2016', x='date', y=' ') +
#   final_theme
# 
# ggplot(subset(buoy_lmp_cond_vert, subset=(datetime>=as.POSIXct('2016-07-18', tz='UTC') & datetime<as.POSIXct('2016-07-19', tz='UTC'))), aes(x=datetime, y=value, color=source)) +
#   geom_point() +
#   labs(title='Jul 18, 2016', x='date', y=' ') +
#   final_theme
# 
# ggplot(subset(buoy_lmp_cond_vert, subset=(datetime>=as.POSIXct('2016-08-23', tz='UTC') & datetime<as.POSIXct('2016-08-24', tz='UTC'))), aes(x=datetime, y=value, color=source)) +
#   geom_point() +
#   labs(title='Aug 23, 2016', x='date', y=' ') +
#   final_theme
# 
# ggplot(subset(buoy_lmp_cond_vert, subset=(datetime>=as.POSIXct('2016-09-28', tz='UTC') & datetime<as.POSIXct('2016-09-29', tz='UTC'))), aes(x=datetime, y=value, color=source)) +
#   geom_point() +
#   labs(title='Septh 28, 2016', x='date', y=' ') +
#   final_theme

#conductivity generally lower at buoy site than 210, but seems okay generally

#plot Chlor_RFU vs Chlor_UGL - should be linear relationship
# ggplot(buoy2016_L1, aes(x=Chlor_RFU, y=Chlor_UGL)) +
#   geom_point() +
#   final_theme

#chlor RFU should be recoded to NA, as it is not actually RFU - RFU/ugl relationship should be linear
buoy2016_L1 <- buoy2016_L1 %>% 
  mutate(Chlor_RFU = NA)

#### air vars ####
# buoy2016_vert_air <- buoy2016_L1 %>% 
#   select(datetime, air, location) %>% 
#   gather(variable, value, -datetime, -location)
# 
# ggplot(subset(buoy2016_vert_air, subset=(datetime>=as.POSIXct('2016-01-01', tz='UTC') & datetime < as.POSIXct('2017-01-01', tz='UTC'))), aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='2016', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme
# 
# ggplot(subset(buoy2016_vert_air, subset=(datetime>=as.POSIXct('2016-01-01', tz='UTC') & datetime < as.POSIXct('2016-02-01', tz='UTC'))), aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='Jan 2016', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme
# 
# ggplot(subset(buoy2016_vert_air, subset=(datetime>=as.POSIXct('2016-02-01', tz='UTC') & datetime < as.POSIXct('2016-03-01', tz='UTC'))), aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='Feb 2016', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme
# 
# ggplot(subset(buoy2016_vert_air, subset=(datetime>=as.POSIXct('2016-03-01', tz='UTC') & datetime < as.POSIXct('2016-04-01', tz='UTC'))), aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='Mar 2016', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme
# 
# ggplot(subset(buoy2016_vert_air, subset=(datetime>=as.POSIXct('2016-04-01', tz='UTC') & datetime < as.POSIXct('2016-05-01', tz='UTC'))), aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='Apr 2016', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme
# 
# ggplot(subset(buoy2016_vert_air, subset=(datetime>=as.POSIXct('2016-05-01', tz='UTC') & datetime < as.POSIXct('2016-06-01', tz='UTC'))), aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='May 2016', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme
# 
# ggplot(subset(buoy2016_vert_air, subset=(datetime>=as.POSIXct('2016-06-01', tz='UTC') & datetime < as.POSIXct('2016-07-01', tz='UTC'))), aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='Jun 2016', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme
# 
# ggplot(subset(buoy2016_vert_air, subset=(datetime>=as.POSIXct('2016-07-01', tz='UTC') & datetime < as.POSIXct('2016-08-01', tz='UTC'))), aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='Jul 2016', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme
# 
# ggplot(subset(buoy2016_vert_air, subset=(datetime>=as.POSIXct('2016-08-01', tz='UTC') & datetime < as.POSIXct('2016-09-01', tz='UTC'))), aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='Aug 2016', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme
# 
# ggplot(subset(buoy2016_vert_air, subset=(datetime>=as.POSIXct('2016-09-01', tz='UTC') & datetime < as.POSIXct('2016-10-01', tz='UTC'))), aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='Sept 2016', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme
# 
# ggplot(subset(buoy2016_vert_air, subset=(datetime>=as.POSIXct('2016-10-01', tz='UTC') & datetime < as.POSIXct('2016-11-01', tz='UTC'))), aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='Oct 2016', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme
# 
# ggplot(subset(buoy2016_vert_air, subset=(datetime>=as.POSIXct('2016-11-01', tz='UTC') & datetime < as.POSIXct('2016-12-01', tz='UTC'))), aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='Nov 2016', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme
# 
# ggplot(subset(buoy2016_vert_air, subset=(datetime>=as.POSIXct('2016-12-01', tz='UTC') & datetime < as.POSIXct('2017-012-01', tz='UTC'))), aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='Dec 2016', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme

#temp looks good, but need to recode out-of-range RH readings. sensor is only accurate 98+/-5%
buoy2016_L1 <- buoy2016_L1 %>% 
  mutate(RelHum = case_when(RelHum > 103 ~ NA_real_,
                            TRUE ~ RelHum))

# buoy2016_vert_air_L1 <- buoy2016_L1 %>% 
#   select(datetime, air, location) %>% 
#   gather(variable, value, -datetime, -location)
# 
# ggplot(subset(buoy2016_vert_air_L1, subset=(datetime>=as.POSIXct('2016-01-01', tz='UTC') & datetime < as.POSIXct('2017-01-01', tz='UTC'))), aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='2016, v2', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme

#### PAR ####
# buoy_PAR <- buoy2016_L1 %>% 
#   select(datetime, location, PAR) 
# 
# ggplot(subset(buoy_PAR, subset=(datetime>=as.POSIXct('2016-01-01', tz='UTC') & datetime < as.POSIXct('2017-01-01', tz='UTC'))), aes(x=datetime, y=PAR, col=location)) +
#   geom_point() +
#   labs(title='2016', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme

#adjust negative par to 0
buoy2016_L1 <- buoy2016_L1 %>% 
  mutate(PAR = case_when(PAR<0 ~ 0,
                         TRUE ~ PAR))
# buoy_PAR <- buoy2016_L1 %>%
#   select(datetime, location, PAR)
# 
# ggplot(subset(buoy_PAR, subset=(datetime>=as.POSIXct('2016-01-01', tz='UTC') & datetime < as.POSIXct('2017-01-01', tz='UTC'))), aes(x=datetime, y=PAR, col=location)) +
#   geom_point() +
#   labs(title='2016', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme

# ggplot(subset(buoy_PAR, subset=(datetime>=as.POSIXct('2016-01-01', tz='UTC') & datetime < as.POSIXct('2016-02-01', tz='UTC'))), aes(x=datetime, y=PAR, col=location)) +
#   geom_point() +
#   labs(title='Jan 2016', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme
# 
# ggplot(subset(buoy_PAR, subset=(datetime>=as.POSIXct('2016-02-01', tz='UTC') & datetime < as.POSIXct('2016-03-01', tz='UTC'))), aes(x=datetime, y=PAR, col=location)) +
#   geom_point() +
#   labs(title='Feb 2016', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme
# 
# ggplot(subset(buoy_PAR, subset=(datetime>=as.POSIXct('2016-03-01', tz='UTC') & datetime < as.POSIXct('2016-04-01', tz='UTC'))), aes(x=datetime, y=PAR, col=location)) +
#   geom_point() +
#   labs(title='Mar 2016', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme
# 
# ggplot(subset(buoy_PAR, subset=(datetime>=as.POSIXct('2016-04-01', tz='UTC') & datetime < as.POSIXct('2016-05-01', tz='UTC'))), aes(x=datetime, y=PAR, col=location)) +
#   geom_point() +
#   labs(title='Apr 2016', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme
# 
# ggplot(subset(buoy_PAR, subset=(datetime>=as.POSIXct('2016-05-01', tz='UTC') & datetime < as.POSIXct('2016-06-01', tz='UTC'))), aes(x=datetime, y=PAR, col=location)) +
#   geom_point() +
#   labs(title='May 2016', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme
# 
# ggplot(subset(buoy_PAR, subset=(datetime>=as.POSIXct('2016-06-01', tz='UTC') & datetime < as.POSIXct('2016-07-01', tz='UTC'))), aes(x=datetime, y=PAR, col=location)) +
#   geom_point() +
#   labs(title='Jun 2016', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme
# 
# ggplot(subset(buoy_PAR, subset=(datetime>=as.POSIXct('2016-07-01', tz='UTC') & datetime < as.POSIXct('2016-08-01', tz='UTC'))), aes(x=datetime, y=PAR, col=location)) +
#   geom_point() +
#   labs(title='Jul 2016', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme
# 
# ggplot(subset(buoy_PAR, subset=(datetime>=as.POSIXct('2016-08-01', tz='UTC') & datetime < as.POSIXct('2016-09-01', tz='UTC'))), aes(x=datetime, y=PAR, col=location)) +
#   geom_point() +
#   labs(title='Aug 2016', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme
# 
# ggplot(subset(buoy_PAR, subset=(datetime>=as.POSIXct('2016-09-01', tz='UTC') & datetime < as.POSIXct('2016-10-01', tz='UTC'))), aes(x=datetime, y=PAR, col=location)) +
#   geom_point() +
#   labs(title='Sept 2016', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme
# 
# ggplot(subset(buoy_PAR, subset=(datetime>=as.POSIXct('2016-10-01', tz='UTC') & datetime < as.POSIXct('2016-11-01', tz='UTC'))), aes(x=datetime, y=PAR, col=location)) +
#   geom_point() +
#   labs(title='Oct 2016', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme
# 
# ggplot(subset(buoy_PAR, subset=(datetime>=as.POSIXct('2016-11-01', tz='UTC') & datetime < as.POSIXct('2016-12-01', tz='UTC'))), aes(x=datetime, y=PAR, col=location)) +
#   geom_point() +
#   labs(title='Nov 2016', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme
# 
# ggplot(subset(buoy_PAR, subset=(datetime>=as.POSIXct('2016-12-01', tz='UTC') & datetime < as.POSIXct('2017-01-01', tz='UTC'))), aes(x=datetime, y=PAR, col=location)) +
#   geom_point() +
#   labs(title='Dec 2016', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme

#PAR looks good

#select desired variables, export L1 data
buoy2016_tocsv <- buoy2016_L1 %>% 
  select(-c(ArrayID, Year, Day, date, LoggerBatV, RadioBatV, IntLgBxTempC)) %>% 
  select(datetime, location, alltemp2016, upDO, lowDO, chla, allmet2016)

write_csv(buoy2016_tocsv, 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/buoy2016_L1_15Jan2018.csv')

