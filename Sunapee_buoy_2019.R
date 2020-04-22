#*****************************************************************
#*      Cary Institute of Ecosystem Studies (Millbrook, NY)      *
#*                                                               *
#* TITLE:   Sunapee_buoy_2019.r                                  *
#* AUTHOR:  Bethel Steele                                        *
#* SYSTEM:  Lenovo ThinkCentre, Win 10, R 3.4.2, RStudio 1.1.383 *
#* DATE:    16Jun2019                                            *
#* PROJECT: Lake Sunapee Buoy Data Cleaning                      *
#* PURPOSE: create L0 and L1 data for buoy data 2019 using       *
#*          similar methods to CCC and DR                        *
#*****************************************************************

source('library_func_lists.R')

#bring in  buoy raw data
buoy2019_L0 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L0/2019 Buoy Data_thru_jun.csv',
                     col_types = 'iiiinnnnnnnnnnnnnnnnnnnnnnnnnnnnnn')


#### format data ####
buoy2019_L0 <- buoy2019_L0  %>%
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
  select(-hour, -minutes, -Hr.Min, -Year, -Day, -time, -ArrayID) %>% #remove unnecessary columns
  rownames_to_column(var ='rowid')


# add in all date time options in L1 data set
range(buoy2019_L0$datetime)
# alltimes_2019 <- as.data.frame(seq.POSIXt(as.POSIXct('2019-01-01 00:00', tz='UTC'), as.POSIXct('2019-12-31 23:50', tz='UTC'), '10 min')) %>% 
#   rename("datetime" = !!names(.[1]))
# 
# buoy2019_L1 <- buoy2019_L0 %>% 
#   right_join(., alltimes_2019) %>% 
#   arrange(datetime)
# 
# #double check to make sure there are no DST issues
# datelength2019 <- buoy2019_L1 %>% 
#   mutate(date = format(datetime, '%Y-%m-%d')) %>% 
#   group_by(date) %>% 
#   summarize(length(datetime))
# max(datelength2019$`length(datetime)`)
# min(datelength2019$`length(datetime)`)
# #should only be 144 or less if partial days included
# 
# #clean up workspace
# rm(alltimes_2019, datelength2019)

# partial year only for weather and temp
partalltimes_2019 <- as.data.frame(seq.POSIXt(as.POSIXct('2019-01-01 00:00', tz='UTC'), as.POSIXct('2019-05-31 23:50', tz='UTC'), '10 min')) %>%
  rename("datetime" = !!names(.[1]))

buoy2019_L1 <- buoy2019_L0 %>%
  right_join(., partalltimes_2019) %>%
  arrange(datetime) %>% 
  filter(datetime < as.POSIXct('2019-06-01', tz='UTC'))

#double check to make sure there are no DST issues
partdatelength2019 <- buoy2019_L1 %>%
  mutate(date = format(datetime, '%Y-%m-%d')) %>%
  group_by(date) %>%
  summarize(length(datetime))
max(datelength2019$`length(datetime)`)
min(datelength2019$`length(datetime)`)
#should only be 144 or less if partial days included

#DST observed fix beginning of record
buoy2019_L1a <- buoy2019_L1 %>% 
  filter(datetime < as.POSIXct('2019-03-10 23:00:00', tz='UTC'))

buoy2019_L1b <- buoy2019_L1 %>% 
  filter(datetime >= as.POSIXct('2019-03-11 00:00', tz='UTC')) %>% 
  right_join(partalltimes_2019) %>% 
  filter(datetime >= as.POSIXct('2019-03-11 00:00', tz='UTC') & datetime < as.POSIXct('2019-05-31 23:50', tz='UTC')) %>% 
  arrange(datetime) %>% 
  rownames_to_column(var = 'rowid2')  %>% 
  select(-datetime)
#add all dates/times to record
partalltimes_2019b <- as.data.frame(seq.POSIXt(as.POSIXct('2019-03-10 23:00:00', tz='UTC'), as.POSIXct('2019-05-31 23:50', tz='UTC'), '10 min')) %>% 
  rename("datetime" = !!names(.[1])) %>% 
  rownames_to_column(var = 'rowid2')
buoy2019_L1b <- full_join(buoy2019_L1b, partalltimes_2019b)

buoy2019_L1 <- full_join(buoy2019_L1a, buoy2019_L1b) %>% 
  select(-rowid, -rowid2)

#double check to make sure there are no DST issues
datelength2019 <- buoy2019_L1 %>% 
  mutate(date = format(datetime, '%Y-%m-%d')) %>% 
  group_by(date) %>% 
  summarize(length(datetime))
max(datelength2019$`length(datetime)`)
min(datelength2019$`length(datetime)`)
#should only be 144 or less if partial days included

#clean up workspace
rm(partalltimes_2019, partdatelength2019, partalltimes_2019b, buoy2019_L1a, buoy2019_L1b)


####THERMISTORS####
buoy2019_therm_vert <- buoy2019_L1 %>% 
  select(datetime, alltemp2011) %>%
  gather(variable, value, -datetime)

# ggplot(buoy2019_therm_vert, aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 month') +
#   final_theme

buoy2019_L1 <- buoy2019_L1 %>% 
  mutate_at(vars(alltemp2011),
            funs(case_when(. == -6999 ~ NA_real_,
                           TRUE ~ .)))

buoy2019_therm_vert <- buoy2019_L1 %>% 
  select(datetime, alltemp2011) %>%
  gather(variable, value, -datetime)

ggplot(buoy2019_therm_vert,
       aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  scale_x_datetime(date_minor_breaks = '1 month') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  final_theme

# ggplot(subset(buoy2019_therm_vert,
#               subset=(datetime >= as.POSIXct('2019-05-01', tz='UTC') & datetime < as.POSIXct('2019-06-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme
# 
# #buoy deployment
# ggplot(subset(buoy2019_therm_vert,
#               subset=(datetime >= as.POSIXct('2019-05-23', tz='UTC') & datetime < as.POSIXct('2019-05-24', tz='UTC'))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme
# 
# ggplot(subset(buoy2019_therm_vert,
#               subset=(datetime >= as.POSIXct('2019-05-23', tz='UTC') & datetime < as.POSIXct('2019-05-24', tz='UTC'))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_y_continuous(limits = c(5,20)) +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme

buoy2019_L1 <- buoy2019_L1 %>% 
  mutate_at(vars(alltemp2011),
            funs(case_when(datetime < as.POSIXct('2019-05-23 09:30', tz='UTC') ~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate(location = case_when(datetime >= as.POSIXct('2019-05-23 09:30', tz='UTC') ~ 'loon',
                              TRUE ~ NA_character_))
buoy2019_therm_vert_L1 <- buoy2019_L1 %>% 
  select(datetime, alltemp2011, location) %>%
  gather(variable, value, -datetime, -location)

# ggplot(subset(buoy2019_therm_vert_L1,
#               subset=(datetime >= as.POSIXct('2019-05-01', tz='UTC') & datetime < as.POSIXct('2019-06-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   labs(title = 'May 2019 -thermistors') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme

buoy2019_L1 <- buoy2019_L1 %>% 
  mutate(TempC_0m = case_when(datetime >= as.POSIXct('2019-05-01', tz='UTC') &
                                datetime < as.POSIXct('2019-06-01', tz='UTC') &
                                (TempC_0m>=20 | TempC_0m < 9.5) ~ NA_real_,
                              TRUE ~ TempC_0m)) 
buoy2019_therm_vert_L1 <- buoy2019_L1 %>% 
  select(datetime, alltemp2011, location) %>%
  gather(variable, value, -datetime, -location)

# ggplot(subset(buoy2019_therm_vert_L1,
#               subset=(datetime >= as.POSIXct('2019-05-01', tz='UTC') & datetime < as.POSIXct('2019-06-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme
# 
# ggplot(subset(buoy2019_L1,
#               subset=(datetime >= as.POSIXct('2019-05-23', tz='UTC') & datetime < as.POSIXct('2019-06-01', tz='UTC'))),
#        aes(x=datetime, y=TempC_0m)) +
#   labs(title = '0m only thermistor error') +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme
# 
# 
# ggplot(subset(buoy2019_therm_vert_L1,
#               subset=(datetime >= as.POSIXct('2019-06-01', tz='UTC') & datetime < as.POSIXct('2019-07-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme

# buoy2019_L1 <- buoy2019_L1 %>% 
#   mutate(TempC_0m = case_when(datetime >= as.POSIXct('2019-06-01', tz='UTC') &
#                                 datetime < as.POSIXct('2019-07-01', tz='UTC') &
#                                 (TempC_0m>=20 | TempC_0m < 9.5) ~ NA_real_,
#                               TRUE ~ TempC_0m)) 
# buoy2019_therm_vert_L1 <- buoy2019_L1 %>% 
#   select(datetime, alltemp2011, location) %>%
#   gather(variable, value, -datetime, -location)
# 
# # ggplot(subset(buoy2019_therm_vert_L1,
# #               subset=(datetime >= as.POSIXct('2019-06-01', tz='UTC') & datetime < as.POSIXct('2019-07-01', tz='UTC'))),
# #        aes(x=datetime, y=value, color=variable)) +
# #   geom_point() +
# #   scale_x_datetime(date_minor_breaks = '1 day') +
# #   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
# #                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
# #   final_theme

ggplot(buoy2019_therm_vert_L1,
       aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  scale_x_datetime(date_minor_breaks = '1 month') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  final_theme

#correct column names for sensor offset
buoy2019_L1 <- buoy2019_L1 %>% 
  rename(TempC_9p75m = 'TempC_9m',
         TempC_8p75m = 'TempC_8m',
         TempC_7p75m = 'TempC_7m',
         TempC_6p75m = 'TempC_6m',
         TempC_5p75m = 'TempC_5m',
         TempC_4p75m = 'TempC_4m',
         TempC_3p75m = 'TempC_3m',
         TempC_2p75m = 'TempC_2m',
         TempC_1p75m = 'TempC_1m',
         TempC_0p75m = 'TempC_0m') 

#clean up workspace
rm(buoy2019_therm_vert, buoy2019_therm_vert_L1)

#### DO ####
buoy2019_do_vert <- buoy2019_L1 %>% 
  select(datetime, upDO, lowDO) %>%
  gather(variable, value, -datetime)

# ggplot(buoy2019_do_vert, aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 month') +
#   final_theme

buoy2019_L1 <- buoy2019_L1 %>% 
  mutate_at(vars(upDO, lowDO),
            funs(case_when(. == -6999 ~ NA_real_,
                           TRUE ~ .)))

buoy2019_do_vert_L1 <- buoy2019_L1 %>%
  select(datetime, upDO, lowDO) %>%
  gather(variable, value, -datetime)

ggplot(buoy2019_do_vert, aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  scale_x_datetime(date_minor_breaks = '1 month') +
  final_theme

#buoy move 5-23
buoy2019_L1 <- buoy2019_L1 %>% 
  mutate_at(vars(lowDO, upDO),
            funs(case_when(datetime < as.POSIXct('2019-05-23 9:30', tz='UTC') ~ NA_real_,
                           TRUE ~ .)))

buoy2019_do_vert_L1 <- buoy2019_L1 %>% 
  select(datetime, lowDO, upDO) %>%
  gather(variable, value, -datetime)

# ggplot(buoy2019_do_vert_L1, aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 month') +
#   final_theme
# 
# 
# ggplot(subset(buoy2019_do_vert_L1,
#               subset=(datetime >= as.POSIXct('2019-05-01', tz='UTC') & datetime < as.POSIXct('2019-06-01', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme
# 
# ggplot(subset(buoy2019_do_vert_L1,
#               subset=(datetime >= as.POSIXct('2019-05-23', tz='UTC') & datetime < as.POSIXct('2019-05-24', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme

#doesn't settle until 11
buoy2019_L1 <- buoy2019_L1 %>% 
  mutate_at(vars(lowDO, upDO),
            funs(case_when(datetime < as.POSIXct('2019-05-23 10:00', tz='UTC') ~ NA_real_,
                           TRUE ~ .)))
buoy2019_do_vert_L1 <- buoy2019_L1 %>% 
  select(datetime, lowDO, upDO) %>%
  gather(variable, value, -datetime)


# ggplot(subset(buoy2019_do_vert_L1,
#               subset=(datetime >= as.POSIXct('2019-06-01', tz='UTC') & datetime < as.POSIXct('2019-07-01', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme

#clean up workspace
rm(buoy2019_do_vert, buoy2019_do_vert_L1)

####CHLA####
buoy2019_chla_vert <- buoy2019_L1 %>%
  select(datetime, chla) %>%
  gather(variable, value, -datetime)

# ggplot(buoy2019_chla_vert, aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 month') +
#   final_theme

#chla sensor not working
buoy2019_L1 <- buoy2019_L1 %>% 
  mutate(Chlor_RFU = is.na(Chlor_RFU),
         Chlor_UGL = is.na(Chlor_UGL),
         SpecCond = is.na(SpecCond))



####wind####
buoy_wind_vert <- buoy2019_L1 %>%
  select(datetime, AveWindDir, AveWindSp, MaxWindDir, MaxWindSp) %>%
  gather(variable, value, -datetime)

ggplot(buoy_wind_vert,
       aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title = 'wind 2019, raw') +
  scale_x_datetime(date_minor_breaks = '1 month') +
  final_theme +
  scale_color_colorblind()

# ggplot(subset(buoy_wind_vert,
#               subset=(datetime >= as.POSIXct('2019-01-01', tz='UTC') & datetime < as.POSIXct('2019-02-01', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'jan wind 2019') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# 
# #sensor frozen jan 9-14
# ggplot(subset(buoy_wind_vert,
#               subset=(datetime >= as.POSIXct('2019-01-09', tz='UTC') & datetime < as.POSIXct('2019-01-10', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'jan wind 2019, NAs recoded') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme +
#   scale_color_colorblind()
# 
# #sensor frozen jan 13
# ggplot(subset(buoy_wind_vert,
#               subset=(datetime >= as.POSIXct('2019-01-13', tz='UTC') & datetime < as.POSIXct('2019-01-14', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'jan wind 2019, NAs recoded') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme +
#   scale_color_colorblind()


buoy2019_L1 <- buoy2019_L1 %>% 
  mutate_at(vars(AveWindDir, AveWindSp, MaxWindDir, MaxWindSp),
            funs(case_when(datetime >= as.POSIXct('2019-01-09 5:10', tz='UTC') & datetime < as.POSIXct('2019-01-13 13:30', tz='UTC') & MaxWindSp == 0 ~ NA_real_,
                           TRUE ~ .)))

buoy_wind_vert_L1 <- buoy2019_L1 %>% 
  select(datetime, AveWindDir, AveWindSp, MaxWindDir, MaxWindSp) %>% 
  gather(variable, value, -datetime)

# ggplot(subset(buoy_wind_vert_L1,
#               subset=(datetime >= as.POSIXct('2019-01-01', tz='UTC') & datetime < as.POSIXct('2019-02-01', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'jan wind 2019, clean') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# 
# ggplot(subset(buoy_wind_vert_L1,
#               subset=(datetime >= as.POSIXct('2019-02-01', tz='UTC') & datetime < as.POSIXct('2019-03-01', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'feb wind 2019') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# #sensor frozen feb 28
# ggplot(subset(buoy_wind_vert,
#               subset=(datetime >= as.POSIXct('2019-02-28', tz='UTC') & datetime < as.POSIXct('2019-03-01', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme +
#   scale_color_colorblind()

buoy2019_L1 <- buoy2019_L1 %>% 
  mutate_at(vars(AveWindDir, AveWindSp, MaxWindDir, MaxWindSp),
            funs(case_when(datetime >= as.POSIXct('2019-02-28 4:30', tz='UTC') & datetime < as.POSIXct('2019-02-28 10:20', tz='UTC') & MaxWindSp ==0 ~ NA_real_,
                           TRUE ~ .)))

buoy_wind_vert_L1 <- buoy2019_L1 %>% 
  select(datetime, AveWindDir, AveWindSp, MaxWindDir, MaxWindSp) %>% 
  gather(variable, value, -datetime)

# ggplot(subset(buoy_wind_vert_L1,
#               subset=(datetime >= as.POSIXct('2019-02-01', tz='UTC') & datetime < as.POSIXct('2019-03-01', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'feb wind 2019') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy_wind_vert_L1,
#               subset=(datetime >= as.POSIXct('2019-03-01', tz='UTC') & datetime < as.POSIXct('2019-04-01', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'mar wind 2019') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# #mar04
# ggplot(subset(buoy_wind_vert_L1,
#               subset=(datetime >= as.POSIXct('2019-03-04', tz='UTC') & datetime < as.POSIXct('2019-03-05', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'mar wind 2019') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme +
#   scale_color_colorblind()

buoy2019_L1 <- buoy2019_L1 %>% 
  mutate_at(vars(AveWindDir, AveWindSp, MaxWindDir, MaxWindSp),
            funs(case_when(datetime >= as.POSIXct('2019-03-04 5:20', tz='UTC') & datetime < as.POSIXct('2019-03-04 9:00', tz='UTC') & MaxWindSp ==0 ~ NA_real_,
                           TRUE ~ .)))

buoy_wind_vert_L1 <- buoy2019_L1 %>% 
  select(datetime, AveWindDir, AveWindSp, MaxWindDir, MaxWindSp) %>% 
  gather(variable, value, -datetime)

# ggplot(subset(buoy_wind_vert_L1,
#               subset=(datetime >= as.POSIXct('2019-03-01', tz='UTC') & datetime < as.POSIXct('2019-04-01', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'mar wind 2019') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# 
# ggplot(subset(buoy_wind_vert_L1,
#               subset=(datetime >= as.POSIXct('2019-04-01', tz='UTC') & datetime < as.POSIXct('2019-05-01', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'apr wind 2019') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy_wind_vert_L1,
#               subset=(datetime >= as.POSIXct('2019-05-01', tz='UTC') & datetime < as.POSIXct('2019-06-01', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'may wind 2019') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# #may 23 buoy moved to loon
# ggplot(subset(buoy_wind_vert_L1,
#               subset=(datetime >= as.POSIXct('2019-05-23', tz='UTC') & datetime < as.POSIXct('2019-05-24', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'may wind 2019') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme +
#   scale_color_colorblind()

buoy2019_L1 <- buoy2019_L1 %>% 
  mutate(location = case_when(datetime < as.POSIXct('2019-05-23 7:20', tz='UTC') ~ 'harbor',
                              datetime >= as.POSIXct('2019-05-23 7:20', tz='UTC') & datetime < as.POSIXct('2019-05-23 9:30', tz='UTC') ~ 'in transit',
                              TRUE ~ location)) %>% 
  mutate_at(vars(AveWindDir, AveWindSp, MaxWindDir, MaxWindSp),
            funs(case_when(location == 'in transit' ~ NA_real_,
                           TRUE ~ .)))
buoy_wind_vert_L1 <- buoy2019_L1 %>% 
  select(datetime, AveWindDir, AveWindSp, MaxWindDir, MaxWindSp, location) %>% 
  gather(variable, value, -datetime, -location)

# ggplot(subset(buoy_wind_vert_L1,
#               subset=(datetime >= as.POSIXct('2019-05-01', tz='UTC') & datetime < as.POSIXct('2019-06-01', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'may wind 2019') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# 
# ggplot(subset(buoy_wind_vert_L1,
#               subset=(datetime >= as.POSIXct('2019-06-01', tz='UTC') & datetime < as.POSIXct('2019-07-01', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'june wind 2019') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
ggplot(buoy_wind_vert_L1,
       aes(x=datetime, y=value, color=location)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title = 'wind 2019, clean') +
  scale_x_datetime(date_minor_breaks = '1 month') +
  final_theme +
  scale_color_colorblind()

rm(buoy_wind_vert, buoy_wind_vert_L1)

# ###PAR####
# ggplot(buoy2019_L1,
#        aes(x=datetime, y=PAR, color=location)) +
#   geom_point() +
#   labs(title = 'PAR 2019, raw') +
#   scale_x_datetime(date_minor_breaks = '1 month') +
#   final_theme +
#   scale_color_colorblind()

#recode when in transit or offline
buoy2019_L1 <- buoy2019_L1 %>% 
  mutate(PAR_flag = case_when(PAR < 0 ~ 'z',
                         TRUE ~ NA_character_)) %>% 
  mutate(PAR = case_when(location == 'in transit' ~ NA_real_,
                         PAR < 0 ~ 0,
                         TRUE ~ PAR)) 
# ggplot(buoy2019_L1,
#        aes(x=datetime, y=PAR, color=location)) +
#   geom_point() +
#   labs(title = 'PAR 2019, NA recoded') +
#   scale_x_datetime(date_minor_breaks = '1 month') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy2019_L1,
#               subset=(datetime >= as.POSIXct('2019-01-01', tz='UTC') & datetime < as.POSIXct('2019-02-01', tz='UTC'))),
#        aes(x=datetime, y=PAR, color=location)) +
#   geom_point() +
#   labs(title = 'PAR 2019, NA recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()

#par likely obscured jan 9-20, add flag
buoy2019_L1 <- buoy2019_L1 %>% 
  mutate(PAR_flag = case_when(is.na(PAR_flag) ~ 'n',
                              !is.na(PAR_flag) ~ paste('n', PAR_flag, sep = ', ')),
         PAR_flag = case_when(is.na(PAR_flag) & datetime >= as.POSIXct('2019-01-09', tz='UTC') &
                                datetime < as.POSIXct('2019-01-20', tz='UTC') ~ 'o',
                              !is.na(PAR_flag) & datetime >= as.POSIXct('2019-01-09', tz='UTC') &
                                datetime < as.POSIXct('2019-01-20', tz='UTC') ~ paste('o', PAR_flag, sep = ', '),
                              TRUE ~ PAR_flag))
unique(buoy2019_L1$PAR_flag)


# ggplot(subset(buoy2019_L1,
#               subset=(datetime >= as.POSIXct('2019-02-01', tz='UTC') & datetime < as.POSIXct('2019-03-01', tz='UTC'))),
#        aes(x=datetime, y=PAR, color=location)) +
#   geom_point() +
#   labs(title = 'PAR 2019, NA recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy2019_L1,
#               subset=(datetime >= as.POSIXct('2019-03-01', tz='UTC') & datetime < as.POSIXct('2019-04-01', tz='UTC'))),
#        aes(x=datetime, y=PAR, color=location)) +
#   geom_point() +
#   labs(title = 'PAR 2019, NA recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy2019_L1,
#               subset=(datetime >= as.POSIXct('2019-04-01', tz='UTC') & datetime < as.POSIXct('2019-05-01', tz='UTC'))),
#        aes(x=datetime, y=PAR, color=location)) +
#   geom_point() +
#   labs(title = 'PAR 2019, NA recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy2019_L1,
#               subset=(datetime >= as.POSIXct('2019-05-01', tz='UTC') & datetime < as.POSIXct('2019-06-01', tz='UTC'))),
#        aes(x=datetime, y=PAR, color=location)) +
#   geom_point() +
#   labs(title = 'PAR 2019, NA recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# 
ggplot(buoy2019_L1,
       aes(x=datetime, y=PAR, color=location)) +
  geom_point() +
  labs(title = 'PAR 2019, NA recoded') +
  scale_x_datetime(date_minor_breaks = '1 month') +
  final_theme +
  scale_color_colorblind()



#### Air temp ####
# ggplot(buoy2019_L1,
#        aes(x=datetime, y=AirTempC, color=location)) +
#   geom_point() +
#   labs(title = 'air temp 2019, NA recoded') +
#   scale_x_datetime(date_minor_breaks = '1 month') +
#   final_theme +
#   scale_color_colorblind()

#recode when in transit or offline
buoy2019_L1 <- buoy2019_L1 %>% 
  mutate(AirTempC = case_when(location == 'in transit' ~ NA_real_,
                         TRUE ~ AirTempC))

# ggplot(buoy2019_L1,
#        aes(x=datetime, y=AirTempC, color=location)) +
#   geom_point() +
#   labs(title = 'air temp 2019, NA recoded') +
#   scale_x_datetime(date_minor_breaks = '1 month') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy2019_L1,
#               subset=(datetime >= as.POSIXct('2019-01-01', tz='UTC') & datetime < as.POSIXct('2019-02-01', tz='UTC'))),
#        aes(x=datetime, y=AirTempC, color=location)) +
#   geom_point() +
#   labs(title = 'air temp 2019, NA recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy2019_L1,
#               subset=(datetime >= as.POSIXct('2019-02-01', tz='UTC') & datetime < as.POSIXct('2019-03-01', tz='UTC'))),
#        aes(x=datetime, y=AirTempC, color=location)) +
#   geom_point() +
#   labs(title = 'air temp 2019, NA recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy2019_L1,
#               subset=(datetime >= as.POSIXct('2019-03-01', tz='UTC') & datetime < as.POSIXct('2019-04-01', tz='UTC'))),
#        aes(x=datetime, y=AirTempC, color=location)) +
#   geom_point() +
#   labs(title = 'air temp 2019, NA recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy2019_L1,
#               subset=(datetime >= as.POSIXct('2019-04-01', tz='UTC') & datetime < as.POSIXct('2019-05-01', tz='UTC'))),
#        aes(x=datetime, y=AirTempC, color=location)) +
#   geom_point() +
#   labs(title = 'air temp 2019, NA recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy2019_L1,
#               subset=(datetime >= as.POSIXct('2019-05-01', tz='UTC') & datetime < as.POSIXct('2019-06-01', tz='UTC'))),
#        aes(x=datetime, y=AirTempC, color=location)) +
#   geom_point() +
#   labs(title = 'air temp 2019, NA recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy2019_L1,
#               subset=(datetime >= as.POSIXct('2019-06-01', tz='UTC') & datetime < as.POSIXct('2019-07-01', tz='UTC'))),
#        aes(x=datetime, y=AirTempC, color=location)) +
#   geom_point() +
#   labs(title = 'air temp 2019, NA recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()



#### EXPORT L1 DATA STREAMS ####
colnames(buoy2019_L1)
#export L1 tempstring file
buoy2019_L1 %>%
  select(datetime, TempC_0p75m:TempC_9p75m, location) %>%
  filter(datetime < as.POSIXct('2019-05-23', tz='UTC')) %>% 
  mutate(datetime = as.character(datetime)) %>%
  write_csv(., 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/tempstring/2019_tempstring_L1_corrdepths.csv')

#export l1 do file
buoy2019_L1 %>%
  select(datetime, upDO, lowDO, location) %>%
  filter(datetime < as.POSIXct('2019-05-23', tz='UTC')) %>% 
  mutate(datetime = as.character(datetime)) %>%
  write_csv(., 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/do/2019_do_L1.csv')

#export l1 par file
buoy2019_L1 %>%
  select(datetime, PAR, PAR_flag, location) %>%
  filter(datetime < as.POSIXct('2019-05-23', tz='UTC')) %>% 
  mutate(datetime = as.character(datetime)) %>%
  write_csv(., 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/met/2019_PAR_L1.csv')

#export l1 wind
buoy2019_L1 %>%
  select(datetime, AveWindSp, AveWindDir, MaxWindSp, MaxWindDir, location) %>%
  filter(datetime < as.POSIXct('2019-05-23', tz='UTC')) %>% 
  mutate(datetime = as.character(datetime)) %>%
  write_csv(., 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/met/2019_wind_L1.csv')

#export l1 air temp file
buoy2019_L1 %>%
  select(datetime, AirTempC, location) %>%
  filter(datetime < as.POSIXct('2019-05-23', tz='UTC')) %>% 
  mutate(datetime = as.character(datetime)) %>%
  write_csv(., 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/met/2019_airtemp_L1.csv')


