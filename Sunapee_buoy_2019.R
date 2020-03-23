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

#bring in  buoy raw data
buoy2019 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L0/2019 Buoy Data_thru_jun.csv',
                     col_types = 'iiiinnnnnnnnnnnnnnnnnnnnnnnnnnnnnn')


#### format data ####
buoy2019 <- buoy2019  %>%
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

buoy2019_L1 <- buoy2019

####THERMISTERS####
buoy2019_therm_vert <- buoy2019_L1 %>% 
  select(datetime, alltemp2016) %>%
  gather(variable, value, -datetime)

# ggplot(buoy2019_therm_vert, aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 month') +
#   final_theme

buoy2019_L1 <- buoy2019_L1 %>% 
  mutate_at(vars(alltemp2016),
            funs(case_when(. == -6999 ~ NA_real_,
                           TRUE ~ .)))

buoy2019_therm_vert <- buoy2019_L1 %>% 
  select(datetime, alltemp2016) %>%
  gather(variable, value, -datetime)

# ggplot(buoy2019_therm_vert,
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 month') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme 

buoy2019_therm_vert <- buoy2019_L1 %>% 
  select(datetime, alltemp2016) %>%
  gather(variable, value, -datetime)

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
  mutate_at(vars(alltemp2016),
            funs(case_when(datetime < as.POSIXct('2019-05-23 10:30', tz='UTC') ~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate(location = case_when(datetime >= as.POSIXct('2019-05-23 10:30', tz='UTC') ~ 'loon',
                              TRUE ~ NA_character_))
buoy2019_therm_vert_L1 <- buoy2019_L1 %>% 
  select(datetime, alltemp2016, location) %>%
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
  select(datetime, alltemp2016, location) %>%
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

buoy2019_L1 <- buoy2019_L1 %>% 
  mutate(TempC_0m = case_when(datetime >= as.POSIXct('2019-06-01', tz='UTC') &
                                datetime < as.POSIXct('2019-07-01', tz='UTC') &
                                (TempC_0m>=20 | TempC_0m < 9.5) ~ NA_real_,
                              TRUE ~ TempC_0m)) 
buoy2019_therm_vert_L1 <- buoy2019_L1 %>% 
  select(datetime, alltemp2016, location) %>%
  gather(variable, value, -datetime, -location)

# ggplot(subset(buoy2019_therm_vert_L1,
#               subset=(datetime >= as.POSIXct('2019-06-01', tz='UTC') & datetime < as.POSIXct('2019-07-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme


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

# buoy2019_do_vert_L1 <- buoy2019_L1 %>% 
#   select(datetime, upDO, lowDO) %>%
#   gather(variable, value, -datetime)
# 
# ggplot(buoy2019_do_vert, aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 month') +
#   final_theme

#buoy move 5-23
buoy2019_L1 <- buoy2019_L1 %>% 
  mutate_at(vars(lowDO, upDO),
            funs(case_when(datetime < as.POSIXct('2019-05-23 10:30', tz='UTC') ~ NA_real_,
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
            funs(case_when(datetime < as.POSIXct('2019-05-23 11:00', tz='UTC') ~ NA_real_,
                           TRUE ~ .)))
buoy2019_do_vert_L1 <- buoy2019_L1 %>% 
  select(datetime, lowDO, upDO) %>%
  gather(variable, value, -datetime)

# 
# ggplot(subset(buoy2019_do_vert_L1,
#               subset=(datetime >= as.POSIXct('2019-06-01', tz='UTC') & datetime < as.POSIXct('2019-07-01', tz='UTC'))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme

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
# buoy_wind_vert <- buoy2019_L1 %>%
#   select(datetime, AveWindDir, AveWindSp, MaxWindDir, MaxWindSp) %>%
#   gather(variable, value, -datetime)
# 
# ggplot(buoy_wind_vert,
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'wind 2019, raw') +
#   scale_x_datetime(date_minor_breaks = '1 month') +
#   final_theme +
#   scale_color_colorblind()
# 
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
            funs(case_when(datetime >= as.POSIXct('2019-01-09 5:10', tz='UTC') & datetime < as.POSIXct('2019-01-13 13:30', tz='UTC') ~ NA_real_,
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
            funs(case_when(datetime >= as.POSIXct('2019-02-28 4:30', tz='UTC') & datetime < as.POSIXct('2019-02-28 10:20', tz='UTC') ~ NA_real_,
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
            funs(case_when(datetime >= as.POSIXct('2019-03-04 5:20', tz='UTC') & datetime < as.POSIXct('2019-03-04 9:00', tz='UTC') ~ NA_real_,
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
  mutate(location = case_when(datetime < as.POSIXct('2019-05-23 8:20', tz='UTC') ~ 'harbor',
                              datetime >= as.POSIXct('2019-05-23 8:20', tz='UTC') & datetime < as.POSIXct('2019-05-23 10:30', tz='UTC') ~ 'in transit',
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
# ggplot(buoy_wind_vert_L1,
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'wind 2019, clean') +
#   scale_x_datetime(date_minor_breaks = '1 month') +
#   final_theme +
#   scale_color_colorblind()

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
  mutate(PAR_flag = case_when(datetime >= as.POSIXct('2019-01-09', tz='UTC') &
                                datetime < as.POSIXct('2019-01-20', tz='UTC') ~ 'n, o',
                              TRUE ~ 'n')) 


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
# ggplot(subset(buoy2019_L1,
#               subset=(datetime >= as.POSIXct('2019-06-01', tz='UTC') & datetime < as.POSIXct('2019-07-01', tz='UTC'))),
#        aes(x=datetime, y=PAR, color=location)) +
#   geom_point() +
#   labs(title = 'PAR 2019, NA recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()


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
#export L1 tempstring file
buoy2019_L1 %>%
  select(datetime, TempC_0m, TempC_1m, TempC_2m, TempC_3m, TempC_4m, TempC_5m, TempC_6m, TempC_7m, TempC_8m, TempC_9m, location) %>%
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
  mutate(datetime = as.character(datetime)) %>%
  write_csv(., 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2019_tempstring_L1_corrdepths.csv')

#export l1 do file
buoy2019_L1 %>%
  select(datetime, upDO, lowDO, location) %>%
  mutate(datetime = as.character(datetime)) %>%
  write_csv(., 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2019_do_L1.csv')

#export l1 par file
buoy2019_L1 %>%
  select(datetime, PAR, PAR_flag, location) %>%
  mutate(datetime = as.character(datetime)) %>%
  write_csv(., 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2019_PAR_L1.csv')

#export l1 wind
buoy2019_L1 %>%
  select(datetime, AveWindSp, AveWindDir, MaxWindSp, MaxWindDir, location) %>%
  mutate(datetime = as.character(datetime)) %>%
  write_csv(., 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2019_wind_L1.csv')

#export l1 air temp file
buoy2019_L1 %>%
  select(datetime, AirTempC, location) %>%
  mutate(datetime = as.character(datetime)) %>%
  write_csv(., 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2019_airtemp_L1.csv')


rm(alltimes_2019)


