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

#bring in 2017 LMP data for comparison
LMP2017 <- read_xlsx('C:/Users/steeleb/Dropbox/Lake Sunapee/long term Sunapee data/raw data/LMP files/LMP 2017/DO.xlsx', 
                     sheet='DO',
                     col_types = c('text', 'text', 'numeric', 'guess', 'numeric', 
                                   'numeric', 'numeric', 'numeric', 'numeric' ,'numeric',
                                   'text', 'text', 'numeric', 'text')) %>% 
  mutate(DATE = as.Date(DATE, format='%Y-%m-%d')) %>%  #format date
  filter(DATE >= '2017-01-01' & DATE < '2018-01-01', #filter for 2017 only
         STATION == 210)

LMP2017_bio <- read_xlsx('C:/Users/steeleb/Dropbox/Lake Sunapee/long term Sunapee data/raw data/LMP files/LMP 2017/BIOLOGY.xlsx', 
                         sheet='BIOLOGY',
                         col_types = c('text', 'text', 'numeric', 'guess', 'numeric', 
                                       'numeric', 'text', 'numeric', 'text' ,'numeric',
                                       'text', 'numeric', 'numeric', 'numeric', 'text',
                                       'text', 'text', 'text')) %>% 
  mutate(DATE = as.Date(DATE, format='%Y-%m-%d')) %>%  #format date
  filter(DATE >= '2017-01-01' & DATE < '2018-01-01', #filter for 2017 only
         STATION == 210) 

LMP2017_chem <- read_xlsx('C:/Users/steeleb/Dropbox/Lake Sunapee/long term Sunapee data/raw data/LMP files/LMP 2017/CHEMISTRY.xlsx', 
                          sheet='CHEMISTRY',
                          col_types = c('text', 'text', 'numeric', 'guess', 'numeric', 
                                        'text', 'numeric', 'numeric', 'numeric' ,'numeric',
                                        'numeric', 'numeric', 'numeric', 'numeric', 'numeric',
                                        'text', 'text', 'guess', 'text', 'text')) %>% 
  mutate(DATE = as.Date(DATE, format='%Y-%m-%d')) %>%  #format date
  filter(DATE >= '2017-01-01' & DATE < '2018-01-01', #filter for 2017 only
         STATION == 210) 


#### format data ####
buoy2017 <- buoy2017 %>%
  rename(Hr.Min = 'Hr/Min') %>% 
  mutate(hour = Hr.Min%/%100,
         minutes = Hr.Min%%100,
         time = paste(hour, minutes, sep=':')) %>% #break out time from Hr.Min, create time column
  mutate(date = as.Date(paste(Day, Year, sep = '-'), format='%j-%Y'), #create date in ymd format
         datetime = as.POSIXct(paste(date, time, sep=' '), format='%Y-%m-%d %H:%M', tz='UTC')) %>%  #tibble forces to UTC, so coerce for our uses.
  select(-c(hour, minutes, Hr.Min, time, ArrayID)) #remove unnecessary columns


# format time in LMP data
LMP2017 <- LMP2017 %>% 
  mutate(datetime = as.POSIXct(paste(DATE, '12:00', sep=' '), format='%Y-%m-%d %H:%M', tz='UTC')) %>%  #tibble forces to UTC, so coerce for our uses. assume noon, because no time available
  select(-c(TIME)) #remove unnecessary columns

str(buoy2017)
str(LMP2017)


#subset LMP for truthing data
LMP2017_temp <- LMP2017 %>% 
  select(datetime, DEPTH, TEMP) %>% 
  filter(DEPTH<=12) %>% 
  rename(depth = 'DEPTH', 
         value = 'TEMP') %>% 
  mutate(source = 'LMP')

LMP2017_upDO <- LMP2017 %>% 
  select(datetime, DEPTH, DO, PCNTSAT) %>% 
  filter(DEPTH <= 2) %>% 
  rename(depth = 'DEPTH', 
         DOppm = 'DO', 
         DOSat = 'PCNTSAT') %>% 
  gather(variable, value, -datetime, -depth) %>% 
  mutate(source = 'LMP')

LMP2017_lowDO <- LMP2017 %>% 
  select(datetime, DEPTH, DO, PCNTSAT) %>% 
  filter(DEPTH > 9 & DEPTH <12) %>% 
  rename(depth = 'DEPTH', 
         DOLowPPM = 'DO', 
         DOLowSat = 'PCNTSAT') %>% 
  gather(variable, value, -datetime, -depth) %>% 
  mutate(source = 'LMP')

LMP2017_chla <- LMP2017_bio %>%  
  select(DATE, CHL) %>% 
  mutate(DATE = as.POSIXct(paste(DATE, '12:00', sep = ' '), tz='UTC')) %>% 
  rename(Chlor_UGL = 'CHL',
         datetime = 'DATE') %>% 
  gather(variable, value, -datetime) %>% 
  mutate(source = 'LMP')

LMP2017_cond <- LMP2017_chem %>%  
  select(DATE, Depth, COND) %>% 
  mutate(DATE = as.POSIXct(paste(DATE, '12:00', sep = ' '), tz='UTC')) %>% 
  rename(SpecCond = 'COND',
         datetime = 'DATE',
         depth = 'Depth') %>% 
  gather(variable, value, -datetime) %>% 
  mutate(source = 'LMP')

buoy2017_L1 <- buoy2017

#### initial data cleaning - reoval of NA strings####
buoy2017_vert <- buoy2017 %>%
  select(-Year, -Day, -LoggerBatV, -RadioBatV, -IntLgBxTempC) %>% 
  gather(variable, value, -datetime, -date)

# ggplot(buoy2017_vert, aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 month') +
#   final_theme

#THERMISTERS
buoy2017_therm_vert <- buoy2017_L1 %>% 
  select(datetime, alltemp2016) %>%
  gather(variable, value, -datetime)

# ggplot(buoy2017_therm_vert, aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 month') +
#   final_theme

buoy2017_L1 <- buoy2017_L1 %>% 
  mutate_at(vars(alltemp2016),
            funs(case_when(. == 1215 ~ NA_real_,
                           . == 555.4 ~ NA_real_,
                           . == -6999 ~ NA_real_,
                           TRUE ~ .)))

buoy2017_therm_vert <- buoy2017_L1 %>% 
  select(datetime, alltemp2016) %>%
  gather(variable, value, -datetime)

# ggplot(buoy2017_therm_vert, 
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 month') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +  
#   final_theme
# 
# ggplot(buoy2017_therm_vert, aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 month') +
#   final_theme

#UPPER DO
buoy2017_updo_vert <- buoy2017_L1 %>% 
  select(datetime, upDO) %>%
  gather(variable, value, -datetime)

# ggplot(buoy2017_updo_vert, aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 month') +
#   final_theme

buoy2017_L1 <- buoy2017_L1 %>% 
  mutate_at(vars(upDO),
            funs(case_when(. == -6999 ~ NA_real_,
                           . == 0 ~ NA_real_,
                           TRUE ~ .)))

buoy2017_updo_vert <- buoy2017_L1 %>% 
  select(datetime, upDO) %>%
  gather(variable, value, -datetime)

# ggplot(buoy2017_updo_vert, aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 month') +
#   final_theme

#UPPER DO
buoy2017_lowdo_vert <- buoy2017_L1 %>% 
  select(datetime, lowDO) %>%
  gather(variable, value, -datetime)

# ggplot(buoy2017_lowdo_vert, aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 month') +
#   final_theme

buoy2017_L1 <- buoy2017_L1 %>% 
  mutate_at(vars(lowDO),
            funs(case_when(. == -6999 ~ NA_real_,
                           . == 0 ~ NA_real_,
                           TRUE ~ .)))

buoy2017_lowdo_vert <- buoy2017_L1 %>% 
  select(datetime, lowDO) %>%
  gather(variable, value, -datetime)

# ggplot(buoy2017_lowdo_vert, aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 month') +
#   final_theme

#CHLA
buoy2017_chla_vert <- buoy2017_L1 %>% 
  select(datetime, chla) %>%
  gather(variable, value, -datetime)

# ggplot(buoy2017_chla_vert, aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 month') +
#   final_theme

buoy2017_L1 <- buoy2017_L1 %>% 
  mutate_at(vars(chla),
            funs(case_when(. == -6999 ~ NA_real_,
                           . == 6999 ~ NA_real_,
                           . == 587 ~ NA_real_,
                           . < 0 ~ NA_real_,
                           TRUE ~ .)))

buoy2017_chla_vert <- buoy2017_L1 %>% 
  select(datetime, chla) %>%
  gather(variable, value, -datetime)

# ggplot(buoy2017_chla_vert, aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 month') +
#   final_theme

#MET
buoy2017_met_vert <- buoy2017_L1 %>% 
  select(datetime, allmet2016) %>%
  gather(variable, value, -datetime)

# ggplot(buoy2017_met_vert, aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 month') +
#   final_theme

#### L1 data cleaning ####

#THERMISTERS
#look at 0m probe first - really messy
ggplot(buoy2017_L1, aes(x=datetime, y=TempC_0m)) +
  geom_point() +
  scale_x_datetime(date_minor_breaks = '1 month') +
  final_theme

# #probe working intermittently from 7/27 until 10/20
# ggplot(subset(buoy2017_L1, subset=(datetime>=as.POSIXct('2017-07-27', tz='UTC') & datetime < as.POSIXct('2017-10-20', tz='UTC'))), 
#        aes(x=datetime, y=TempC_0m)) +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 month') +
#   final_theme
# 
# ggplot(subset(buoy2017_L1, subset=(datetime>=as.POSIXct('2017-07-27', tz='UTC') & datetime < as.POSIXct('2017-08-01', tz='UTC'))), 
#        aes(x=datetime, y=TempC_0m)) +
#   geom_point() +
#   coord_cartesian(ylim=c(0, 30)) +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme
# 
# ggplot(subset(buoy2017_L1, subset=(datetime>=as.POSIXct('2017-08-01', tz='UTC') & datetime < as.POSIXct('2017-09-01', tz='UTC'))), 
#        aes(x=datetime, y=TempC_0m)) +
#   geom_point() +
#   coord_cartesian(ylim=c(0, 30)) +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme
# 
# ggplot(subset(buoy2017_L1, subset=(datetime>=as.POSIXct('2017-09-01', tz='UTC') & datetime < as.POSIXct('2017-10-01', tz='UTC'))), 
#        aes(x=datetime, y=TempC_0m)) +
#   geom_point() +
#   coord_cartesian(ylim=c(0, 30)) +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme
# 
# ggplot(subset(buoy2017_L1, subset=(datetime>=as.POSIXct('2017-10-01', tz='UTC') & datetime < as.POSIXct('2017-11-01', tz='UTC'))), 
#        aes(x=datetime, y=TempC_0m)) +
#   geom_point() +
#   coord_cartesian(ylim=c(0, 30)) +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme

buoy2017_L1 <- buoy2017_L1 %>% 
  mutate(TempC_0m = case_when(datetime < as.POSIXct('2017-07-28', tz='UTC') ~ NA_real_,
                              datetime >= as.POSIXct('2017-07-28', tz='UTC') & datetime < as.POSIXct('2017-08-01', tz='UTC') & (TempC_0m <20 | TempC_0m >30) ~ NA_real_,
                              datetime >= as.POSIXct('2017-08-01', tz='UTC') & datetime < as.POSIXct('2017-09-01', tz='UTC') & (TempC_0m <20 | TempC_0m >30) ~ NA_real_,
                              datetime >= as.POSIXct('2017-09-01', tz='UTC') & datetime < as.POSIXct('2017-10-01', tz='UTC') & (TempC_0m <=15 | TempC_0m >30) ~ NA_real_,
                              datetime >= as.POSIXct('2017-10-01', tz='UTC') & datetime < as.POSIXct('2017-10-20', tz='UTC') & (TempC_0m <15 | TempC_0m >30) ~ NA_real_,
                              datetime >= as.POSIXct('2017-10-20', tz='UTC') ~ NA_real_,
                              TRUE ~ TempC_0m))

buoy2017_therm_vert <- buoy2017_L1 %>% 
  select(datetime, alltemp2016) %>%
  gather(variable, value, -datetime)

# ggplot(buoy2017_L1, aes(x=datetime, y=TempC_0m)) +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 month') +
#   final_theme

#now in context of others
# ggplot(subset(buoy2017_therm_vert, subset=(datetime>=as.POSIXct('2017-04-01', tz='UTC') & 
#                                              datetime<as.POSIXct('2017-05-01', tz='UTC'))), 
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   labs(title = 'thermisters Apr 2017') +
#   scale_x_datetime(date_minor_breaks = '1 month') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +  
#   final_theme
# 
# ggplot(subset(buoy2017_therm_vert, subset=(datetime>=as.POSIXct('2017-05-01', tz='UTC') & 
#                                              datetime<as.POSIXct('2017-06-01', tz='UTC'))), 
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   labs(title = 'thermisters May 2017') +
#   scale_x_datetime(date_minor_breaks = '1 month') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +  
#   final_theme
# 
# #remove therm data prior to May 17 move to harbor
# ggplot(subset(buoy2017_therm_vert, subset=(datetime>=as.POSIXct('2017-05-17 6:00', tz='UTC') & 
#                                              datetime<as.POSIXct('2017-05-17 14:00', tz='UTC'))), 
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   labs(title = 'thermisters May 2017') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +  
#   final_theme

#buoy to summer location 17 May - broke from mooring next day
#buoy moved back to harbor 19 May
buoy2017_L1 <- buoy2017_L1 %>% 
  mutate_at(vars(alltemp2016),
            funs(case_when(datetime <= as.POSIXct('2017-05-17 11:00', tz='UTC') ~ NA_real_,
                           TRUE ~ .)))
buoy2017_therm_vert_b <- buoy2017_L1 %>% 
  select(datetime, alltemp2016) %>%
  gather(variable, value, -datetime)

# ggplot(subset(buoy2017_therm_vert_b, subset=(datetime>=as.POSIXct('2017-05-01', tz='UTC') & 
#                                              datetime<as.POSIXct('2017-06-01', tz='UTC'))), 
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   labs(title = 'thermisters May 2017, v2') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +  
#   final_theme
# 
# ggplot(subset(buoy2017_therm_vert, subset=(datetime>=as.POSIXct('2017-06-01', tz='UTC') & 
#                                              datetime<as.POSIXct('2017-07-01', tz='UTC'))), 
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   labs(title = 'thermisters Jun 2017') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +  
#   final_theme
# 
# #buoy back out to summer loc 14 Jun
# #temp line inspected 22 Jun
# ggplot(subset(buoy2017_therm_vert, subset=(datetime>=as.POSIXct('2017-06-22', tz='UTC') & 
#                                              datetime<as.POSIXct('2017-06-23', tz='UTC'))), 
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   labs(title = 'thermisters Jun 2017') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +  
#   final_theme

buoy2017_L1 <- buoy2017_L1 %>% 
  mutate_at(vars(alltemp2016),
            funs(case_when(datetime >= as.POSIXct('2017-06-22 10:00', tz='UTC') & datetime < as.POSIXct('2017-06-22 15:40') ~ NA_real_,
                           TRUE ~ .)))
buoy2017_therm_vert_b <- buoy2017_L1 %>% 
  select(datetime, alltemp2016) %>%
  gather(variable, value, -datetime)

# ggplot(subset(buoy2017_therm_vert_b, subset=(datetime>=as.POSIXct('2017-06-01', tz='UTC') & 
#                                                datetime<as.POSIXct('2017-07-01', tz='UTC'))), 
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   labs(title = 'thermisters Jun 2017, v2') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +  
#   final_theme
# 
# ggplot(subset(buoy2017_therm_vert, subset=(datetime>=as.POSIXct('2017-07-01', tz='UTC') & 
#                                              datetime<as.POSIXct('2017-08-01', tz='UTC'))), 
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   labs(title = 'thermisters Jul 2017') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +  
#   final_theme
# 
# #templine removed jul 7
# ggplot(subset(buoy2017_therm_vert, subset=(datetime>=as.POSIXct('2017-07-07', tz='UTC') & 
#                                              datetime<as.POSIXct('2017-07-08', tz='UTC'))), 
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   labs(title = 'thermisters Jul 2017') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +  
#   final_theme
# 
# #temp line redeployed 28 Jul
# ggplot(subset(buoy2017_therm_vert, subset=(datetime>=as.POSIXct('2017-07-28', tz='UTC') & 
#                                              datetime<as.POSIXct('2017-07-29', tz='UTC'))), 
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   labs(title = 'thermisters Jul 2017') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +  
#   final_theme
# 
# ggplot(subset(buoy2017_therm_vert, subset=(datetime>=as.POSIXct('2017-08-01', tz='UTC') & 
#                                              datetime<as.POSIXct('2017-09-01', tz='UTC'))), 
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   labs(title = 'thermisters Aug 2017') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +  
#   final_theme
# 
# ggplot(subset(buoy2017_therm_vert, subset=(datetime>=as.POSIXct('2017-09-01', tz='UTC') &
#                                              datetime<as.POSIXct('2017-10-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   labs(title = 'thermisters Sept 2017') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme
# 
# ggplot(subset(buoy2017_therm_vert, subset=(datetime>=as.POSIXct('2017-10-01', tz='UTC') &
#                                              datetime<as.POSIXct('2017-11-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   labs(title = 'thermisters Oct 2017') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme
# 
# #buoy to harbor 19 Oct
# ggplot(subset(buoy2017_therm_vert, subset=(datetime>=as.POSIXct('2017-10-19', tz='UTC') & 
#                                              datetime<as.POSIXct('2017-10-20', tz='UTC'))), 
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   labs(title = 'thermisters OCt 2017') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +  
#   final_theme

buoy2017_L1 <- buoy2017_L1 %>% 
  mutate_at(vars(alltemp2016),
            funs(case_when(datetime >= as.POSIXct('2017-10-19 10:00', tz='UTC') ~ NA_real_,
                           TRUE ~ .)))
buoy2017_therm_vert_b <- buoy2017_L1 %>% 
  select(datetime, alltemp2016) %>%
  gather(variable, value, -datetime)

# ggplot(subset(buoy2017_therm_vert_b, subset=(datetime>=as.POSIXct('2017-10-01', tz='UTC') &
#                                              datetime<as.POSIXct('2017-11-01', tz='UTC'))), 
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   labs(title = 'thermisters Oct 2017, 2') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +  
#   final_theme
# 
# ggplot(buoy2017_therm_vert_b, 
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   labs(title = 'thermisters 2017, clean') +  
#   scale_x_datetime(date_minor_breaks = '1 month') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +  
#   final_theme


#UPPER DO
buoy2017_L1$location = 'harbor'

buoy2017_updo_vert <- buoy2017_L1 %>%
  select(datetime, location, upDO) %>%
  gather(variable, value, -datetime, -location)
# 
# ggplot(buoy2017_updo_vert, aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 month') +
#   labs(title = 'upper do 2017, NAs recoded') +
#   final_theme
# 
# ggplot(subset(buoy2017_updo_vert, subset=(datetime>= as.POSIXct('2017-01-01', tz='UTC') & datetime < as.POSIXct('2017-02-01', tz='EST'))),
#         aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   labs(title = 'upper do jan 2017, NAs recoded') +
#   final_theme
# 
# ggplot(subset(buoy2017_updo_vert, subset=(datetime>= as.POSIXct('2017-02-01', tz='UTC') & datetime < as.POSIXct('2017-03-01', tz='EST'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   labs(title = 'upper do feb 2017, NAs recoded') +
#   final_theme
# 
# ggplot(subset(buoy2017_updo_vert, subset=(datetime>= as.POSIXct('2017-03-01', tz='UTC') & datetime < as.POSIXct('2017-04-01', tz='EST'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   labs(title = 'upper do mar 2017, NAs recoded') +
#   final_theme
# 
# #buoy removed from water for cleaning and inspection mar 6 -- no data in need of recoding
# 
# #buoy back in water on Apr 21, sensors ready 28 apr
# ggplot(subset(buoy2017_updo_vert, subset=(datetime>= as.POSIXct('2017-04-01', tz='UTC') & datetime < as.POSIXct('2017-05-01', tz='EST'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   labs(title = 'upper do apr 2017, NAs recoded') +
#   final_theme
# 
# ggplot(subset(buoy2017_updo_vert, subset=(datetime>= as.POSIXct('2017-04-28', tz='UTC') & datetime < as.POSIXct('2017-04-29', tz='EST'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   labs(title = 'upper do jan 2017, NAs recoded') +
#   final_theme

buoy2017_L1 <- buoy2017_L1 %>% 
  mutate_at(vars(DOppm, DOSat),
            funs(case_when(datetime >= as.POSIXct('2017-04-20', tz='UTC') & datetime < as.POSIXct('2017-04-28 21:40', tz='UTC')~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate(DOTempC = case_when(datetime >= as.POSIXct('2017-04-20', tz='UTC') & datetime < as.POSIXct('2017-04-28 15:30', tz='UTC')~ NA_real_,
                            TRUE ~ DOTempC))
buoy2017_updo_vert_b <- buoy2017_L1 %>% 
  select(datetime, location, upDO) %>%
  gather(variable, value, -datetime, -location)

# ggplot(subset(buoy2017_updo_vert_b, subset=(datetime>= as.POSIXct('2017-04-01', tz='UTC') & datetime < as.POSIXct('2017-05-01', tz='EST'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   labs(title = 'upper do apr 2017, clean') +
#   final_theme
# 
# ggplot(subset(buoy2017_updo_vert, subset=(datetime>= as.POSIXct('2017-05-01', tz='UTC') & datetime < as.POSIXct('2017-06-01', tz='EST'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   labs(title = 'upper do may 2017, NAs recoded') +
#   final_theme
# 
# #may 17 buoy moved to loon
# ggplot(subset(buoy2017_updo_vert, subset=(datetime>= as.POSIXct('2017-05-17', tz='UTC') & datetime < as.POSIXct('2017-05-18', tz='EST'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   labs(title = 'upper do may 2017, NAs recoded') +
#   final_theme
# 
# #may 19 buoy back to harbor, sensors offline
# ggplot(subset(buoy2017_updo_vert, subset=(datetime>= as.POSIXct('2017-05-19', tz='UTC') & datetime < as.POSIXct('2017-05-20', tz='EST'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   labs(title = 'upper do may 2017, NAs recoded') +
#   final_theme
# 
# #may 23 sensors back online
# ggplot(subset(buoy2017_updo_vert, subset=(datetime>= as.POSIXct('2017-05-23', tz='UTC') & datetime < as.POSIXct('2017-05-24', tz='EST'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   labs(title = 'upper do may 2017, NAs recoded') +
#   final_theme


buoy2017_L1 <- buoy2017_L1 %>% 
  mutate_at(vars(DOppm, DOSat),
            funs(case_when(datetime >= as.POSIXct('2017-05-17 9:20', tz='UTC') & datetime < as.POSIXct('2017-05-17 12:20', tz='UTC')~ NA_real_,
                           datetime >= as.POSIXct('2017-05-19 9:00', tz='UTC') & datetime < as.POSIXct('2017-05-23 15:10', tz='UTC')~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate(DOTempC = case_when(datetime >= as.POSIXct('2017-05-17 9:20', tz='UTC') & datetime < as.POSIXct('2017-05-17 10:50', tz='UTC')~ NA_real_,
                             datetime >= as.POSIXct('2017-05-19 9:00', tz='UTC') & datetime < as.POSIXct('2017-05-23 13:50', tz='UTC')~ NA_real_,
                             TRUE ~ DOTempC),
         location = case_when(datetime >= as.POSIXct('2017-05-17 10:30', tz='UTC') & datetime < as.POSIXct('2017-05-19 9:30', tz='UTC') ~ 'loon',
                              datetime >= as.POSIXct('2017-05-19 9:30', tz='UTC') ~ 'harbor',
                             TRUE ~ location))
buoy2017_updo_vert_b <- buoy2017_L1 %>% 
  select(datetime, location, upDO) %>%
  gather(variable, value, -datetime, -location)

# ggplot(subset(buoy2017_updo_vert_b, subset=(datetime>= as.POSIXct('2017-05-01', tz='UTC') & datetime < as.POSIXct('2017-06-01', tz='EST'))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   labs(title = 'upper do may 2017, clean') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy2017_updo_vert, subset=(datetime>= as.POSIXct('2017-06-01', tz='UTC') & datetime < as.POSIXct('2017-07-01', tz='EST'))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   labs(title = 'upper do june 2017, NAs recoded') +
#   final_theme +
#   scale_color_colorblind()
# 
# 
# #jun 14 buoy back to loon
# ggplot(subset(buoy2017_updo_vert, subset=(datetime>= as.POSIXct('2017-06-14', tz='UTC') & datetime < as.POSIXct('2017-06-15', tz='EST'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   labs(title = 'upper do june 2017, NAs recoded') +
#   final_theme

buoy2017_L1 <- buoy2017_L1 %>% 
  mutate_at(vars(DOppm, DOSat),
            funs(case_when(datetime >= as.POSIXct('2017-06-14 12:30', tz='UTC') & datetime < as.POSIXct('2017-06-14 15:40', tz='UTC')~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate(DOTempC = case_when(datetime >= as.POSIXct('2017-06-14 12:30', tz='UTC') & datetime < as.POSIXct('2017-06-14 14:20', tz='UTC')~ NA_real_,
                             TRUE ~ DOTempC),
         location = case_when(datetime >= as.POSIXct('2017-06-14 13:00', tz='UTC') ~ 'loon',
                              TRUE ~ location))
buoy2017_updo_vert_b <- buoy2017_L1 %>% 
  select(datetime, location, upDO) %>%
  gather(variable, value, -datetime, -location)

# ggplot(subset(buoy2017_updo_vert_b, subset=(datetime>= as.POSIXct('2017-06-01', tz='UTC') & datetime < as.POSIXct('2017-07-01', tz='EST'))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   labs(title = 'upper do june 2017, clean') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy2017_updo_vert, subset=(datetime>= as.POSIXct('2017-07-01', tz='UTC') & datetime < as.POSIXct('2017-08-01', tz='EST'))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   labs(title = 'upper do july 2017, NAs recoded') +
#   final_theme +
#   scale_color_colorblind()
# 
# #jul 28 buoy visit
# ggplot(subset(buoy2017_updo_vert, subset=(datetime>= as.POSIXct('2017-07-28', tz='UTC') & datetime < as.POSIXct('2017-07-29', tz='EST'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   labs(title = 'upper do july 2017, NAs recoded') +
#   final_theme

buoy2017_L1 <- buoy2017_L1 %>% 
  mutate_at(vars(DOppm, DOSat),
            funs(case_when(datetime >= as.POSIXct('2017-07-28 10:40', tz='UTC') & datetime < as.POSIXct('2017-07-28 17:30', tz='UTC')~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate(DOTempC = case_when(datetime >= as.POSIXct('2017-07-28 10:40', tz='UTC') & datetime < as.POSIXct('2017-07-28 16:00', tz='UTC')~ NA_real_,
                             TRUE ~ DOTempC))
buoy2017_updo_vert_b <- buoy2017_L1 %>% 
  select(datetime, location, upDO) %>%
  gather(variable, value, -datetime, -location)

# ggplot(subset(buoy2017_updo_vert_b, subset=(datetime>= as.POSIXct('2017-07-01', tz='UTC') & datetime < as.POSIXct('2017-08-01', tz='EST'))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   labs(title = 'upper do july 2017, clean') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy2017_updo_vert, subset=(datetime>= as.POSIXct('2017-08-01', tz='UTC') & datetime < as.POSIXct('2017-09-01', tz='EST'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   labs(title = 'upper do aug 2017, NAs recoded') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy2017_updo_vert, subset=(datetime>= as.POSIXct('2017-09-01', tz='UTC') & datetime < as.POSIXct('2017-10-01', tz='EST'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   labs(title = 'upper do sept 2017, NAs recoded') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy2017_updo_vert, subset=(datetime>= as.POSIXct('2017-10-01', tz='UTC') & datetime < as.POSIXct('2017-11-01', tz='EST'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   labs(title = 'upper do oct 2017, NAs recoded') +
#   final_theme +
#   scale_color_colorblind()
# 
# #oct 19 buoy moved to harbor
# ggplot(subset(buoy2017_updo_vert, subset=(datetime>= as.POSIXct('2017-10-19', tz='UTC') & datetime < as.POSIXct('2017-10-20', tz='EST'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   labs(title = 'upper do oct 2017, NAs recoded') +
#   final_theme +
#   scale_color_colorblind()

buoy2017_L1 <- buoy2017_L1 %>% 
  mutate_at(vars(upDO),
            funs(case_when(datetime >= as.POSIXct('2017-10-19 9:10', tz='UTC') ~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate(location = case_when(datetime >= as.POSIXct('2017-10-19 9:30', tz='UTC') ~ 'harbor',
                              TRUE ~ location))
buoy2017_updo_vert_b <- buoy2017_L1 %>% 
  select(datetime, location, upDO) %>%
  gather(variable, value, -datetime, -location)

# ggplot(subset(buoy2017_updo_vert_b, subset=(datetime>= as.POSIXct('2017-10-01', tz='UTC') & datetime < as.POSIXct('2017-11-01', tz='EST'))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   labs(title = 'upper do oct 2017, clean') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(buoy2017_updo_vert_b, 
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 month') +
#   labs(title = 'upper do 2017, clean') +
#   final_theme +
#   scale_color_colorblind()

#LOWER DO#
buoy2017_lowdo_vert <- buoy2017_L1 %>%
  select(datetime, location, lowDO) %>%
  gather(variable, value, -datetime, -location)

# ggplot(buoy2017_lowdo_vert, aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 month') +
#   labs(title = 'lower do 2017, NAs recoded') +
#   final_theme
# 
# #low do NA until move to loon
# ggplot(subset(buoy2017_lowdo_vert, subset=(datetime>= as.POSIXct('2017-05-01', tz='UTC') & datetime < as.POSIXct('2017-06-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   labs(title = 'lower do may 2017, NAs recoded') +
#   final_theme +
#   scale_color_colorblind()
# 
# #may 17 buoy moved to loon
# ggplot(subset(buoy2017_lowdo_vert, subset=(datetime>= as.POSIXct('2017-05-17', tz='UTC') & datetime < as.POSIXct('2017-05-18', tz='UTC'))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   labs(title = 'lower do may 2017, NAs recoded') +
#   final_theme +
#   scale_color_colorblind()
# #may 18 buoy breaks from mooring, low do into sediment
# ggplot(subset(buoy2017_lowdo_vert, subset=(datetime>= as.POSIXct('2017-05-18', tz='UTC') & datetime < as.POSIXct('2017-05-19', tz='UTC'))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   labs(title = 'lower do may 2017, NAs recoded') +
#   final_theme +
#   scale_color_colorblind()
# 
# #may 19 buoy back to harbor, sensors offline
# ggplot(subset(buoy2017_lowdo_vert, subset=(datetime>= as.POSIXct('2017-05-19', tz='UTC') & datetime < as.POSIXct('2017-05-20', tz='UTC'))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   labs(title = 'lower do may 2017, NAs recoded') +
#   final_theme +
#   scale_color_colorblind()
# 
# #jun 14 lower do back in deep water
# ggplot(subset(buoy2017_lowdo_vert, subset=(datetime>= as.POSIXct('2017-06-14', tz='UTC') & datetime < as.POSIXct('2017-06-15', tz='UTC'))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   labs(title = 'lower do jun 2017, NAs recoded') +
#   final_theme +
#   scale_color_colorblind()

buoy2017_L1 <- buoy2017_L1 %>% 
  mutate_at(vars(DOLowPPM, DOLowSat),
            funs(case_when(datetime < as.POSIXct('2017-05-17 17:30', tz='UTC') ~ NA_real_,
                           datetime >= as.POSIXct('2017-05-18 21:50', tz='UTC') & datetime < as.POSIXct('2017-06-14 15:30', tz='UTC')~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate(DOLoTempC = case_when(datetime < as.POSIXct('2017-05-17 10:50', tz='UTC') ~ NA_real_,
                             datetime >= as.POSIXct('2017-05-18 21:50', tz='UTC') & datetime < as.POSIXct('2017-06-14 14:20', tz='UTC')~ NA_real_,
                             TRUE ~ DOLoTempC))
buoy2017_lowdo_vert_b <- buoy2017_L1 %>%
  select(datetime, location, lowDO) %>%
  gather(variable, value, -datetime, -location)

# ggplot(subset(buoy2017_lowdo_vert_b, subset=(datetime>= as.POSIXct('2017-05-01', tz='UTC') & datetime < as.POSIXct('2017-06-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   labs(title = 'lower do may 2017, clean') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy2017_lowdo_vert_b, subset=(datetime>= as.POSIXct('2017-06-01', tz='UTC') & datetime < as.POSIXct('2017-07-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   labs(title = 'lower do june 2017, NAs recoded') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy2017_lowdo_vert, subset=(datetime>= as.POSIXct('2017-07-01', tz='UTC') & datetime < as.POSIXct('2017-08-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   labs(title = 'lower do july 2017, NAs recoded') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy2017_lowdo_vert, subset=(datetime>= as.POSIXct('2017-08-01', tz='UTC') & datetime < as.POSIXct('2017-09-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   labs(title = 'lower do aug 2017, NAs recoded') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy2017_lowdo_vert, subset=(datetime>= as.POSIXct('2017-09-01', tz='UTC') & datetime < as.POSIXct('2017-10-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   labs(title = 'lower do sept 2017, NAs recoded') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy2017_lowdo_vert, subset=(datetime>= as.POSIXct('2017-10-01', tz='UTC') & datetime < as.POSIXct('2017-11-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   labs(title = 'lower do oct 2017, NAs recoded') +
#   final_theme +
#   scale_color_colorblind()
# 
# #oct 19 buoy moved to harbor
# ggplot(subset(buoy2017_lowdo_vert, subset=(datetime>= as.POSIXct('2017-10-19', tz='UTC') & datetime < as.POSIXct('2017-10-20', tz='UTC'))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   labs(title = 'lower do oct 2017, NAs recoded') +
#   final_theme +
#   scale_color_colorblind()

buoy2017_L1 <- buoy2017_L1 %>% 
  mutate_at(vars(lowDO),
            funs(case_when(datetime >= as.POSIXct('2017-10-19 9:30', tz='UTC') ~ NA_real_,
                           TRUE ~ .)))
buoy2017_lowdo_vert_b <- buoy2017_L1 %>%
  select(datetime, location, lowDO) %>%
  gather(variable, value, -datetime, -location)

# ggplot(subset(buoy2017_lowdo_vert_b, subset=(datetime>= as.POSIXct('2017-10-01', tz='UTC') & datetime < as.POSIXct('2017-11-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   labs(title = 'lower do oct 2017, clean') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(buoy2017_lowdo_vert_b, 
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 month') +
#   labs(title = 'lower do 2017, clean') +
#   final_theme +
#   scale_color_colorblind()


#CHLA#
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
                           TRUE ~ .))) 
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
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'chla apr 2017, NAs recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme
# 
# #may 16 chla probe removed for calibration
# ggplot(subset(buoy2017_chla_vert, subset=(datetime>=as.POSIXct('2017-05-16', tz='UTC') & datetime < as.POSIXct('2017-05-17', tz='UTC'))), 
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'chla may 2017, NAs recoded') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme 
# 
# #may 17 sonde reattached, buoy moved to loon
# ggplot(subset(buoy2017_chla_vert, subset=(datetime>=as.POSIXct('2017-05-17', tz='UTC') & datetime < as.POSIXct('2017-05-18', tz='UTC'))), 
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'chla may 2017, clean') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme 
# 
# #may 19 buoy back to harbor, sensors offline
# ggplot(subset(buoy2017_chla_vert, subset=(datetime>=as.POSIXct('2017-05-19', tz='UTC') & datetime < as.POSIXct('2017-05-20', tz='UTC'))), 
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'chla may 2017, NAs recoded') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme 
# 
# #may 23 sensors back online
# ggplot(subset(buoy2017_chla_vert, subset=(datetime>=as.POSIXct('2017-05-23', tz='UTC') & datetime < as.POSIXct('2017-05-24', tz='UTC'))), 
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'chla may 2017, NAs recoded') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme 

#starting may 31, all sensors measuring only integers

buoy2017_L1 <- buoy2017_L1 %>% 
  mutate_at(vars(chla),
            funs(case_when(datetime >= as.POSIXct('2017-05-16 10:50', tz='UTC') & datetime < as.POSIXct('2017-05-17 11:30', tz='UTC')~ NA_real_,
                           datetime >= as.POSIXct('2017-05-19 9:00', tz='UTC') & datetime < as.POSIXct('2017-05-23 13:50', tz='UTC')~ NA_real_,
                           TRUE ~ .))) 
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
# ggplot(subset(buoy2017_chla_vert, subset=(datetime>=as.POSIXct('2017-06-01', tz='UTC') & datetime < as.POSIXct('2017-07-01', tz='UTC'))), 
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'chla jun 2017, NAs recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme 
# 
# #jun 14 buoy back to loon
# ggplot(subset(buoy2017_chla_vert, subset=(datetime>=as.POSIXct('2017-06-14', tz='UTC') & datetime < as.POSIXct('2017-06-15', tz='UTC'))), 
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'chla jun 2017, NAs recoded') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme 

buoy2017_L1 <- buoy2017_L1 %>% 
  mutate_at(vars(chla),
            funs(case_when(datetime >= as.POSIXct('2017-06-14 13:00', tz='UTC') & datetime < as.POSIXct('2017-06-14 14:20', tz='UTC')~ NA_real_,
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
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'chla jul 2017, NAs recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme
# 
# ggplot(subset(buoy2017_chla_vert, subset=(datetime>=as.POSIXct('2017-08-01', tz='UTC') & datetime < as.POSIXct('2017-09-01', tz='UTC'))), 
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'chla aug 2017, NAs recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme 
# 
# ggplot(subset(buoy2017_chla_vert, subset=(datetime>=as.POSIXct('2017-09-01', tz='UTC') & datetime < as.POSIXct('2017-10-01', tz='UTC'))), 
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'chla sept 2017, NAs recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme 
# 
# ggplot(subset(buoy2017_chla_vert, subset=(datetime>=as.POSIXct('2017-10-01', tz='UTC') & datetime < as.POSIXct('2017-11-01', tz='UTC'))), 
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'chla oct 2017, NAs recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme 
# 
# #eary october spike in chl-a seems errant
# 
# #oct 19 buoy moved to harbor
# ggplot(subset(buoy2017_chla_vert, subset=(datetime>=as.POSIXct('2017-10-19', tz='UTC') & datetime < as.POSIXct('2017-10-20', tz='UTC'))), 
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

#sonde removed nov20
buoy2017_L1 <- buoy2017_L1 %>% 
  mutate_at(vars(chla),
            funs(case_when(datetime >= as.POSIXct('2017-11-20 12:00', tz='UTC') ~ NA_real_,
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

ggplot(buoy2017_chla_vert_b, 
       aes(x=datetime, y=value, color=location)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title = 'chla 2017, clean') +
  scale_x_datetime(date_minor_breaks = '1 month') +
  final_theme +
  scale_color_colorblind()



#buoy removed from water for cleaning and inspection mar 6

#buoy back in water on Apr 21, sensors ready 28 apr

#may 17 buoy moved to loon

#may 19 buoy back to harbor, sensors offline

#may 23 sensors back online

#jun 14 buoy back to loon

#jun22 buoy visit

#jul 7 buoy visit

#jul 28 buoy visit

#oct 19 buoy moved to harbor
