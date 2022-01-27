#*****************************************************************
#*      Cary Institute of Ecosystem Studies (Millbrook, NY)      *
#*                                                               *
#* TITLE:   Sunapee_buoy_2011.r                                  *
#* AUTHOR:  Bethel Steele                                        *
#* SYSTEM:  Lenovo ThinkCentre, Win 10, R 3.4.2, RStudio 1.1.383 *
#* PROJECT: Lake Sunapee Buoy Data Cleaning                      *
#* PURPOSE: subset data for met/compare with L1                  *
#*****************************************************************


source('library_func_lists.R')

#point to data directories
raw_dir = 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L0/'
dump_dir = 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/'

#set tz
buoy_tz = 'Etc/GMT+5'

#bring in 2011 buoy raw data
buoy2011_L0 <- read.csv(file.path(raw_dir, 'Sunapee2011_rawData.csv'),
                        col.names = c('datetime', 'AirTempC', 'DOppm', 'DOSat', 'DOSat2',
                                      'PAR', 'DOTempC', 'TempC_0m', 'TempC_0p5m', 'TempC_1m',
                                      'TempC_1p5m', 'TempC_2m', 'TempC_2p5m', 'TempC_3m', 'TempC_4m',
                                      'TempC_5m', 'TempC_6m', 'TempC_7m', 'TempC_8m', 'TempC_9m',
                                      'TempC_10m', 'TempC_11m', 'TempC_13m', 'AveWindDir', 'InstWindDir',
                                      'InstWindSp', 'AveWindSp'),
                        skip=1) %>%
  select(-DOSat2, -AveWindDir, -AveWindSp, -TempC_0p5m, -TempC_1p5m, -TempC_2p5m, -TempC_10m, -TempC_11m, -TempC_13m) %>%  #drop redundant or blank columns
  mutate(datetime = as.POSIXct(datetime, format='%Y-%m-%d %H:%M:%S', tz=buoy_tz))

#double check to make sure there are no DST issues
datelength2011 <- buoy2011_L0 %>% 
  mutate(date = format(datetime, '%Y-%m-%d')) %>% 
  group_by(date) %>% 
  summarize(length(datetime))

#check dst dates
datelength2011[datelength2011$date == '2011-03-13',]
#dst observed here
datelength2011[datelength2011$date == '2011-11-06',]
#all present

#force into NYtz with dst; convert to utc-5
buoy2011_L1 <- buoy2011_L0 %>% 
  mutate(datetime_instrument = force_tz(datetime, tz = 'America/New_York'),
         datetime = with_tz(datetime_instrument, tz = buoy_tz))

#check again
datelength2011 <- buoy2011_L1 %>% 
  mutate(date = format(datetime, '%Y-%m-%d')) %>% 
  group_by(date) %>%
  summarize(length(datetime))
#look at dst
datelength2011[datelength2011$date == '2011-03-13',]
datelength2011[datelength2011$date == '2011-11-06',]

#create dummy timestamp so there are no blanks
alltimes_2011 <- as.data.frame(seq.POSIXt(as.POSIXct('2011-01-01 00:00', tz=buoy_tz), as.POSIXct('2011-12-31 23:50', tz=buoy_tz), '10 min')) %>% 
  rename("datetime" = !!names(.[1]))

buoy2011_L1 <- buoy2011_L1 %>% 
  right_join(., alltimes_2011) %>% 
  arrange(datetime)

#clean up workspace
rm(datelength2011, alltimes_2011)


#### thermistors ####
buoy2011_temp_vert <- buoy2011_L1 %>%
  select(datetime, all_of(alltemp2011)) %>%
  pivot_longer(names_to = 'variable', values_to = 'value', -datetime)

# ggplot(subset(buoy2011_temp_vert,
#               subset=(datetime >=as.POSIXct('2011-01-01', tz=buoy_tz) &
#                         datetime < as.POSIXct('2012-01-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='2011 buoy temp data, raw',
#        x=NULL,
#        y='temp (deg C)') +
#   scale_x_datetime(date_minor_breaks = '1 month') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust

buoy2011_L1 <- buoy2011_L1 %>%
  mutate_at(vars(all_of(alltemp2011)),
            ~(case_when(. == -6999 ~ NA_real_,
                           . == 555.4 ~ NA_real_,
                           TRUE ~ .))) 

buoy2011_temp_vert <- buoy2011_L1 %>%
  select(datetime, all_of(alltemp2011)) %>%
  pivot_longer(names_to = 'variable', values_to = 'value', -datetime) %>% 
  mutate(variable = factor(variable, levels=alltemp2011))

ggplot(subset(buoy2011_temp_vert,
              subset=(datetime >=as.POSIXct('2011-01-01', tz=buoy_tz) &
                        datetime < as.POSIXct('2012-01-01', tz=buoy_tz))),
       aes(x=datetime, y=value, color=as.factor(variable))) +
geom_point() +
final_theme +
  labs(title='2011 buoy temp data, NAs recoded',
       x=NULL,
       y='temp (deg C)') +
  scale_x_datetime(date_minor_breaks = '1 month') +
scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
                            "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust

# ggplot(subset(buoy2011_temp_vert,
#               subset=(datetime >=as.POSIXct('2011-05-01', tz=buoy_tz) &
#                         datetime < as.POSIXct('2011-06-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='May 2011, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# #buoy deployed May 12
# ggplot(subset(buoy2011_temp_vert,
#               subset=(datetime >=as.POSIXct('2011-05-12', tz=buoy_tz) &
#                         datetime < as.POSIXct('2011-05-13', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='May 2011, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust

buoy2011_L1 <- buoy2011_L1 %>% 
  mutate_at(vars(alltemp2011),
            ~(case_when(datetime < as.POSIXct('2011-05-12 09:40', tz=buoy_tz) ~NA_real_,
                           TRUE ~ .))) 

buoy2011_temp_vert_b <- buoy2011_L1 %>%
  select(datetime, all_of(alltemp2011)) %>%
  pivot_longer(names_to = 'variable', values_to = 'value', -datetime) %>% 
  mutate(variable = factor(variable, levels=alltemp2011))

# ggplot(subset(buoy2011_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2011-05-01', tz=buoy_tz) &
#                         datetime < as.POSIXct('2011-06-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='May 2011, clean',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# ggplot(subset(buoy2011_temp_vert,
#               subset=(datetime >=as.POSIXct('2011-06-01', tz=buoy_tz) &
#                         datetime < as.POSIXct('2011-07-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='June 2011, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# ggplot(subset(buoy2011_temp_vert,
#               subset=(datetime >=as.POSIXct('2011-07-01', tz=buoy_tz) &
#                         datetime < as.POSIXct('2011-08-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='July 2011, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# ggplot(subset(buoy2011_temp_vert,
#               subset=(datetime >=as.POSIXct('2011-08-01', tz=buoy_tz) &
#                         datetime < as.POSIXct('2011-09-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Aug 2011, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# ggplot(subset(buoy2011_temp_vert,
#               subset=(datetime >=as.POSIXct('2011-09-01', tz=buoy_tz) &
#                         datetime < as.POSIXct('2011-10-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Sept 2011, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# ggplot(subset(buoy2011_temp_vert,
#               subset=(datetime >=as.POSIXct('2011-10-01', tz=buoy_tz) &
#                         datetime < as.POSIXct('2011-11-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Oct 2011, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust

ggplot(subset(buoy2011_temp_vert_b,
              subset=(datetime >=as.POSIXct('2011-01-01', tz=buoy_tz) &
                        datetime < as.POSIXct('2012-01-01', tz=buoy_tz))),
       aes(x=datetime, y=value, color=as.factor(variable))) +
  geom_point() +
  final_theme +
  labs(title='2011, clean',
       x=NULL,
       y='temp deg C') +
  scale_x_datetime(date_minor_breaks = '1 month') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
                              "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
 

#correct thermistor depth for offset; add CV
buoy2011_L1 <- buoy2011_L1 %>% 
  rename(waterTemperature_degC_9p5m = TempC_9m,
         waterTemperature_degC_8p5m = TempC_8m,
         waterTemperature_degC_7p5m = TempC_7m,
         waterTemperature_degC_6p5m = TempC_6m,
         waterTemperature_degC_5p5m = TempC_5m,
         waterTemperature_degC_4p5m = TempC_4m,
         waterTemperature_degC_3p5m = TempC_3m,
         waterTemperature_degC_2p5m = TempC_2m,
         waterTemperature_degC_1p5m = TempC_1m,
         waterTemperature_degC_0p5m = TempC_0m)


#clean up workspace
rm(buoy2011_temp_vert, buoy2011_temp_vert_b)


#### DO sensors ####
range(buoy2011_L1$DOSat, na.rm=T)
range(buoy2011_L1$DOppm, na.rm=T)
range(buoy2011_L1$DOTempC, na.rm=T)

buoy2011_L1 <- buoy2011_L1 %>% 
  mutate(DOTempC = case_when(DOTempC == -6999 ~ NA_real_,
                             TRUE ~ DOTempC))

do_vert <- buoy2011_L1 %>% 
  select(datetime, DOSat, DOppm, DOTempC) %>% 
  pivot_longer(names_to = 'variable', values_to = 'value', -datetime)

ggplot(do_vert, aes(x = datetime, y = value)) +
  geom_point() +
  facet_grid(variable~., scales = 'free_y') +
  final_theme +
  labs(title = '2011 DO data NA values recoded',
       x = NULL,
       y = NULL) +
  scale_x_datetime(date_minor_breaks = '1 month')

# ggplot(subset(do_vert, 
#               subset = (datetime >= as.POSIXct('2011-01-01', tz=buoy_tz) & 
#                           datetime < as.POSIXct('2011-02-01', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'jan 2011 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# #do sensor back online jan 3
# ggplot(subset(do_vert,
#               subset = (datetime >= as.POSIXct('2011-01-03', tz=buoy_tz) &
#                           datetime < as.POSIXct('2011-01-04', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'jan 2011 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 hour')

buoy2011_L1 <- buoy2011_L1 %>% 
  mutate_at(vars(DOSat, DOppm, DOTempC),
            ~(case_when(datetime >= as.POSIXct('2011-01-03', tz=buoy_tz) &
                             datetime < as.POSIXct('2011-01-03 12:40', tz=buoy_tz) ~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate(location = 'harbor') %>% 
  mutate(flag_do1p5m = case_when(datetime == as.POSIXct('2011-01-03 12:40', tz=buoy_tz) ~ 'wp',
                                 TRUE ~ ''))

do_vert_b <- buoy2011_L1 %>% 
  select(datetime, DOSat, DOppm, DOTempC) %>% 
  pivot_longer(names_to = 'variable', values_to = 'value', -datetime)

# ggplot(subset(do_vert_b,
#               subset = (datetime >= as.POSIXct('2011-01-01', tz=buoy_tz) &
#                           datetime < as.POSIXct('2011-02-01', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'jan 2011 DO data clean',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(do_vert,
#               subset = (datetime >= as.POSIXct('2011-02-01', tz=buoy_tz) &
#                           datetime < as.POSIXct('2011-03-01', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'feb 2011 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(do_vert,
#               subset = (datetime >= as.POSIXct('2011-03-01', tz=buoy_tz) &
#                           datetime < as.POSIXct('2011-04-01', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'mar 2011 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(do_vert,
#               subset = (datetime >= as.POSIXct('2011-04-01', tz=buoy_tz) &
#                           datetime < as.POSIXct('2011-05-01', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'apr 2011 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(do_vert,
#               subset = (datetime >= as.POSIXct('2011-04-01', tz=buoy_tz) &
#                           datetime < as.POSIXct('2011-04-02', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'mar/apr 2011 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 hour')


buoy2011_L1 <- buoy2011_L1 %>% 
  mutate(location = case_when(datetime >= as.POSIXct('2011-04-01 8:50', tz=buoy_tz) &
                                datetime < as.POSIXct('2011-04-01 12:00', tz=buoy_tz) ~ 'in transit',
                              datetime >= as.POSIXct('2011-04-01 12:00', tz=buoy_tz) ~'loon',
                              TRUE ~ location)) %>% 
  mutate_at(vars(DOSat, DOppm, DOTempC),
            ~ case_when(location == 'in transit' ~ NA_real_, 
                        TRUE ~ .)) %>% 
  mutate(flag_do1p5m = case_when(datetime == as.POSIXct('2011-04-01 12:00', tz=buoy_tz) ~ 'wp',
                                 TRUE ~ flag_do1p5m))
do_vert_b <- buoy2011_L1 %>% 
  select(datetime, DOSat, DOppm, DOTempC, location) %>% 
  pivot_longer(names_to = 'variable', values_to = 'value', -c(datetime, location))

# ggplot(subset(do_vert_b,
#               subset = (datetime >= as.POSIXct('2011-03-15', tz=buoy_tz) &
#                           datetime < as.POSIXct('2011-04-15', tz=buoy_tz))),
#        aes(x = datetime, y = value, color=location)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'mar/apr 2011 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(do_vert,
#               subset = (datetime >= as.POSIXct('2011-05-01', tz=buoy_tz) &
#                           datetime < as.POSIXct('2011-06-01', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'may 2011 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# #thermister line installed may 12
# ggplot(subset(do_vert,
#               subset = (datetime >= as.POSIXct('2011-05-12', tz=buoy_tz) &
#                           datetime < as.POSIXct('2011-05-13', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'may 2011 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 hour')

buoy2011_L1 <- buoy2011_L1 %>% 
  mutate_at(vars(DOSat, DOppm, DOTempC),
            ~(case_when(datetime >= as.POSIXct('2011-05-12 8:10', tz=buoy_tz) &
                             datetime < as.POSIXct('2011-05-12 9:30', tz=buoy_tz) ~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate(flag_do1p5m = case_when(datetime == as.POSIXct('2011-05-12 9:20', tz=buoy_tz) ~ 'wp', 
                                 TRUE ~ flag_do1p5m))
do_vert_b <- buoy2011_L1 %>% 
  select(datetime, DOSat, DOppm, DOTempC, location) %>% 
  pivot_longer(names_to = 'variable', values_to = 'value', -c(datetime, location))

# ggplot(subset(do_vert_b,
#               subset = (datetime >= as.POSIXct('2011-05-01', tz=buoy_tz) &
#                           datetime < as.POSIXct('2011-06-01', tz=buoy_tz))),
#        aes(x = datetime, y = value, color=location)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'may 2011 DO data clean',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(do_vert,
#               subset = (datetime >= as.POSIXct('2011-06-01', tz=buoy_tz) &
#                           datetime < as.POSIXct('2011-07-01', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'jun 2011 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(do_vert,
#               subset = (datetime >= as.POSIXct('2011-07-01', tz=buoy_tz) &
#                           datetime < as.POSIXct('2011-08-01', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'jul 2011 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(do_vert,
#               subset = (datetime >= as.POSIXct('2011-08-01', tz=buoy_tz) &
#                           datetime < as.POSIXct('2011-09-01', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'aug 2011 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(do_vert,
#               subset = (datetime >= as.POSIXct('2011-09-01', tz=buoy_tz) &
#                           datetime < as.POSIXct('2011-10-01', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'sept 2011 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(do_vert,
#               subset = (datetime >= as.POSIXct('2011-10-01', tz=buoy_tz) &
#                           datetime < as.POSIXct('2011-11-01', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'oct 2011 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# #buoy moved to harbor oct 26
# ggplot(subset(do_vert,
#               subset = (datetime >= as.POSIXct('2011-10-26', tz=buoy_tz) &
#                           datetime < as.POSIXct('2011-10-27', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'oct 2011 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 hour')

buoy2011_L1 <- buoy2011_L1 %>% 
  mutate(location = case_when(datetime >= as.POSIXct('2011-10-26 5:40', tz=buoy_tz) &
                                datetime < as.POSIXct('2011-10-26 9:50', tz=buoy_tz) ~ 'in transit',
                              datetime >= as.POSIXct('2011-10-26 9:50', tz=buoy_tz) ~'harbor',
                              TRUE ~ location)) %>% 
  mutate_at(vars(DOSat, DOppm, DOTempC),
            ~(case_when(location == 'in transit' ~ NA_real_,
                        TRUE ~ .))) %>% 
  mutate(flag_do1p5m = case_when(datetime == as.POSIXct('2011-10-26 9:40', tz=buoy_tz) ~ 'wp', 
                                 TRUE ~ flag_do1p5m))
  
do_vert_b <- buoy2011_L1 %>% 
  select(datetime, DOSat, DOppm, DOTempC, location) %>% 
  pivot_longer(names_to = 'variable', values_to = 'value', -c(datetime, location))

# ggplot(subset(do_vert_b,
#               subset = (datetime >= as.POSIXct('2011-10-01', tz=buoy_tz) &
#                           datetime < as.POSIXct('2011-11-01', tz=buoy_tz))),
#        aes(x = datetime, y = value, color = location)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'oct 2011 DO data clean',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(do_vert,
#               subset = (datetime >= as.POSIXct('2011-11-01', tz=buoy_tz) &
#                           datetime < as.POSIXct('2011-12-01', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'nov 2011 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(do_vert,
#               subset = (datetime >= as.POSIXct('2011-12-01', tz=buoy_tz) &
#                           datetime < as.POSIXct('2012-01-01', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'dec 2011 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')

ggplot(do_vert_b, aes(x = datetime, y = value, color = location)) +
  geom_point() +
  facet_grid(variable~., scales = 'free_y') +
  final_theme +
  labs(title = '2011 DO data clean',
       x = NULL,
       y = NULL) +
  scale_x_datetime(date_minor_breaks = '1 month')

#add flag for no calibration on record
buoy2011_L1 <- buoy2011_L1 %>% 
  mutate(flag_do1p5m = case_when(flag_do1p5m == '' ~ 'x',
                                 TRUE ~ paste('x', flag_do1p5m, sep = '; ')))
         
#clean up workspace
rm(do_vert, do_vert_b)

#rename with CV
buoy2011_L1 <- buoy2011_L1 %>% 
  rename(oxygenDissolved_mgl_1p5m = DOppm,
         oxygenDissolvedPercentOfSaturation_pct_1p5m = DOSat,
         waterTemperature_DO_degC_1p5m = DOTempC)


#### wind sensors ####
range(buoy2011_L1$InstWindDir, na.rm = T)
range(buoy2011_L1$InstWindSp, na.rm = T)

# recode all data when in transit
buoy2011_L1 <- buoy2011_L1 %>% 
  mutate_at(vars(InstWindDir ,InstWindSp),
            ~ case_when(location == 'in transit' ~ NA_real_,
                        TRUE ~.))

wind_vert <- buoy2011_L1 %>% 
  select(datetime, InstWindDir ,InstWindSp, location) %>% 
  pivot_longer(names_to = 'variable', values_to = 'value', -c(datetime, location))

ggplot(wind_vert, aes(x = datetime, y = value, color = location)) +
  geom_point() +
  facet_grid(variable~., scales = 'free_y') +
  final_theme +
  labs(title = '2011 wind data raw',
       x = NULL,
       y = NULL) +
  scale_x_datetime(date_minor_breaks = '1 month')

# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2011-01-01', tz=buoy_tz) &
#                         datetime<as.POSIXct('2011-02-01', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'jan 2011 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# # frozen sesors jan 18-22 noon
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2011-01-18', tz=buoy_tz) &
#                         datetime<as.POSIXct('2011-01-23', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'jan 2011 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 hour')
# 

buoy2011_L1 <- buoy2011_L1 %>% 
  mutate_at(vars(InstWindDir, InstWindSp),
            ~(case_when(datetime>=as.POSIXct('2011-01-18 18:00', tz=buoy_tz) &
                             datetime<as.POSIXct('2011-01-22 12:00', tz=buoy_tz) &
                             InstWindSp == 0 ~ NA_real_,
                           TRUE ~ .)))

wind_vert_b <- buoy2011_L1 %>% 
  select(datetime, InstWindDir ,InstWindSp, location) %>% 
  pivot_longer(names_to = 'variable', values_to = 'value', -c(datetime, location))

# ggplot(subset(wind_vert_b,
#               subset=(datetime>=as.POSIXct('2011-01-01', tz=buoy_tz) &
#                         datetime<as.POSIXct('2011-02-01', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'jan 2011 wind data clean',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2011-02-01', tz=buoy_tz) &
#                         datetime<as.POSIXct('2011-03-01', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'feb 2011 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')

# #frozen on feb 05-06
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2011-02-05', tz=buoy_tz) &
#                         datetime<as.POSIXct('2011-02-07', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'feb 2011 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 hour')

buoy2011_L1 <- buoy2011_L1 %>% 
  mutate_at(vars(InstWindDir, InstWindSp),
            ~(case_when(datetime>=as.POSIXct('2011-02-05 11:00', tz=buoy_tz) &
                             datetime<as.POSIXct('2011-02-06 11:00', tz=buoy_tz) &
                             InstWindSp == 0~ NA_real_,
                           TRUE ~ .)))

wind_vert_b <- buoy2011_L1 %>% 
  select(datetime, InstWindDir ,InstWindSp, location) %>% 
  pivot_longer(names_to = 'variable', values_to = 'value', -c(datetime, location))

# ggplot(subset(wind_vert_b,
#               subset=(datetime>=as.POSIXct('2011-02-01', tz=buoy_tz) &
#                         datetime<as.POSIXct('2011-03-01', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'feb 2011 wind data clean',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2011-03-01', tz=buoy_tz) &
#                         datetime<as.POSIXct('2011-04-01', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'mar 2011 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')

# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2011-04-01', tz=buoy_tz) &
#                         datetime<as.POSIXct('2011-05-01', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'apr 2011 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2011-05-01', tz=buoy_tz) &
#                         datetime<as.POSIXct('2011-06-01', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'may 2011 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')

# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2011-06-01', tz=buoy_tz) &
#                         datetime<as.POSIXct('2011-07-01', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'june 2011 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2011-07-01', tz=buoy_tz) &
#                         datetime<as.POSIXct('2011-08-01', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'jul 2011 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')

# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2011-08-01', tz=buoy_tz) &
#                         datetime<as.POSIXct('2011-09-01', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'aug 2011 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2011-09-01', tz=buoy_tz) &
#                         datetime<as.POSIXct('2011-10-01', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'sept 2011 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2011-10-01', tz=buoy_tz) &
#                         datetime<as.POSIXct('2011-11-01', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'oct 2011 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# #sensor stuck oct 27-28
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2011-10-27 12:00', tz=buoy_tz) &
#                         datetime<as.POSIXct('2011-10-28 18:00', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'oct 2011 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 hour')
# 
# # sensor stuck oct 29-30
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2011-10-29 12:00', tz=buoy_tz) &
#                         datetime<as.POSIXct('2011-10-30 18:00', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'oct 2011 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 hour')

buoy2011_L1 <- buoy2011_L1 %>% 
  mutate_at(vars(InstWindDir, InstWindSp),
            ~(case_when(datetime>=as.POSIXct('2011-10-26 09:00', tz=buoy_tz) &
                             datetime<as.POSIXct('2011-10-26 10:00', tz=buoy_tz) &
                          InstWindSp == 0 ~ NA_real_,
                           datetime>=as.POSIXct('2011-10-27 17:10', tz=buoy_tz) &
                             datetime<as.POSIXct('2011-10-28 10:00', tz=buoy_tz)  &
                          InstWindSp == 0 ~ NA_real_,
                           datetime>=as.POSIXct('2011-10-29 20:30', tz=buoy_tz) &
                             datetime<as.POSIXct('2011-10-30 09:00', tz=buoy_tz)  &
                          InstWindSp == 0 ~ NA_real_,
                           TRUE ~ .)))

wind_vert_b <- buoy2011_L1 %>% 
  select(datetime, InstWindDir ,InstWindSp, location) %>% 
  pivot_longer(names_to = 'variable', values_to = 'value', -c(datetime, location))

# ggplot(subset(wind_vert_b,
#               subset=(datetime>=as.POSIXct('2011-10-01', tz=buoy_tz) &
#                         datetime<as.POSIXct('2011-11-01', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'oct 2011 wind data clean',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2011-11-01', tz=buoy_tz) &
#                         datetime<as.POSIXct('2011-12-01', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'nov 2011 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# # sensor frozen nov 23
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2011-11-23', tz=buoy_tz) &
#                         datetime<as.POSIXct('2011-11-24', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'nov 2011 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#     scale_x_datetime(date_minor_breaks = '1 hour')

buoy2011_L1 <- buoy2011_L1 %>% 
  mutate_at(vars(InstWindDir, InstWindSp),
            ~(case_when(datetime>=as.POSIXct('2011-11-23', tz=buoy_tz) &
                             datetime < as.POSIXct('2011-11-24', tZ=buoy_tz) &
                             InstWindSp == 0~ NA_real_,
                           TRUE ~ .)))

wind_vert_b <- buoy2011_L1 %>% 
  select(datetime, InstWindDir ,InstWindSp, location) %>% 
  pivot_longer(names_to = 'variable', values_to = 'value', -c(datetime, location))

# ggplot(subset(wind_vert_b,
#               subset=(datetime>=as.POSIXct('2011-11-01', tz=buoy_tz) &
#                         datetime<as.POSIXct('2011-12-01', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'nov 2011 wind data clean',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2011-12-01', tz=buoy_tz) &
#                         datetime<as.POSIXct('2012-01-01', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'dec 2011 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# #frozen sensor dec 8
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2011-12-08', tz=buoy_tz) &
#                         datetime<as.POSIXct('2011-12-09', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'dec 2011 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 hour')

buoy2011_L1 <- buoy2011_L1 %>% 
  mutate_at(vars(InstWindDir, InstWindSp),
            ~(case_when(datetime>=as.POSIXct('2011-12-08 3:00', tz=buoy_tz) &
                             datetime < as.POSIXct('2011-12-08 13:40', tZ=buoy_tz) &
                          InstWindSp == 0 ~ NA_real_,
                           TRUE ~ .)))

wind_vert_b <- buoy2011_L1 %>% 
  select(datetime, InstWindDir ,InstWindSp, location) %>% 
  pivot_longer(names_to = 'variable', values_to = 'value', -c(datetime, location))

# ggplot(subset(wind_vert_b,
#               subset=(datetime>=as.POSIXct('2011-12-01', tz=buoy_tz) &
#                         datetime<as.POSIXct('2012-01-01', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'dec 2011 wind data clean',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')

ggplot(wind_vert_b,
       aes(x = datetime, y = value, color = location)) +
  geom_point() +
  facet_grid(variable~., scales = 'free_y') +
  final_theme +
  labs(title = '2011 wind data clean',
       x = NULL,
       y = NULL) +
  scale_x_datetime(date_minor_breaks = '1 month')

#clean up workspace
rm(wind_vert, wind_vert_b)

#rename with CV
buoy2011_L1 <- buoy2011_L1 %>% 
  rename(windDirectionInstantaneous_deg = InstWindDir,
         windSpeedInstantaneous_mps = InstWindSp)


#### PAR sensors ####
range(buoy2011_L1$PAR, na.rm = T)

#flag and recode under zero
buoy2011_L1 <-  buoy2011_L1 %>%
  mutate(flag_par = case_when(PAR <0 ~ 'z',
                         TRUE ~ '')) %>% 
  mutate(PAR = case_when(PAR <0 ~ 0,
                         TRUE ~ PAR))

#recode when in transit
buoy2011_L1 <- buoy2011_L1 %>% 
  mutate(PAR = case_when(location == 'in transit' ~ NA_real_, 
                         TRUE ~ PAR))

ggplot(buoy2011_L1, aes(x = datetime, y = PAR)) +
  geom_point() +
  final_theme +
  labs(title = '2011 PAR data below 0 recoded',
       x = NULL,
       y = 'PAR (umol/m2/s)') +
  scale_x_datetime(date_minor_breaks = '1 month')

# #looks like par obscured from early-mid jan until early feb - recoding all of that to 'o'
#
# ggplot(subset(buoy2011_L1,
#               subset = (datetime>=as.POSIXct('2011-01-01', tz=buoy_tz) &
#                           datetime<as.POSIXct('2011-02-01', tz=buoy_tz))),
#        aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'jan 2011 PAR data raw',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2011_L1,
#               subset = (datetime>=as.POSIXct('2011-02-01', tz=buoy_tz) &
#                           datetime<as.POSIXct('2011-03-01', tz=buoy_tz))),
#        aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'feb 2011 PAR data raw',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')

buoy2011_L1 <-  buoy2011_L1 %>%
  mutate(flag_par = case_when(flag_par == '' & 
                                datetime >= as.POSIXct('2011-01-12', tz=buoy_tz) &
                                datetime < as.POSIXct('2011-02-14', tz=buoy_tz) ~ 'o',
                              flag_par != '' & 
                                datetime >= as.POSIXct('2011-01-12', tz=buoy_tz) &
                                datetime < as.POSIXct('2011-02-14', tz=buoy_tz) ~ paste('o', flag_par, sep = '; '),
                         TRUE ~ flag_par))

# ggplot(subset(buoy2011_L1,
#               subset = (datetime>=as.POSIXct('2011-01-01', tz=buoy_tz) &
#                           datetime<as.POSIXct('2011-02-01', tz=buoy_tz))),
#        aes(x = datetime, y = PAR, color = flag_par)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'jan 2011 PAR data clean',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')

# ggplot(subset(buoy2011_L1,
#               subset = (datetime>=as.POSIXct('2011-02-01', tz=buoy_tz) &
#                           datetime<as.POSIXct('2011-03-01', tz=buoy_tz))),
#        aes(x = datetime, y = PAR, color = flag_par)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'feb 2011 PAR data clean',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2011_L1,
#               subset = (datetime>=as.POSIXct('2011-03-01', tz=buoy_tz) &
#                           datetime<as.POSIXct('2011-04-01', tz=buoy_tz))),
#        aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'mar 2011 PAR data raw',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2011_L1,
#               subset = (datetime>=as.POSIXct('2011-04-01', tz=buoy_tz) &
#                           datetime<as.POSIXct('2011-05-01', tz=buoy_tz))),
#        aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'apr 2011 PAR data raw',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2011_L1,
#               subset = (datetime>=as.POSIXct('2011-05-01', tz=buoy_tz) &
#                           datetime<as.POSIXct('2011-06-01', tz=buoy_tz))),
#        aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'may 2011 PAR data raw',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2011_L1,
#               subset = (datetime>=as.POSIXct('2011-06-01', tz=buoy_tz) &
#                           datetime<as.POSIXct('2011-07-01', tz=buoy_tz))),
#        aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'jun 2011 PAR data raw',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2011_L1,
#               subset = (datetime>=as.POSIXct('2011-07-01', tz=buoy_tz) &
#                           datetime<as.POSIXct('2011-08-01', tz=buoy_tz))),
#        aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'jul 2011 PAR data raw',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2011_L1,
#               subset = (datetime>=as.POSIXct('2011-08-01', tz=buoy_tz) &
#                           datetime<as.POSIXct('2011-09-01', tz=buoy_tz))),
#        aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'aug 2011 PAR data raw',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2011_L1,
#               subset = (datetime>=as.POSIXct('2011-09-01', tz=buoy_tz) &
#                           datetime<as.POSIXct('2011-10-01', tz=buoy_tz))),
#        aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'sept 2011 PAR data raw',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2011_L1,
#               subset = (datetime>=as.POSIXct('2011-10-01', tz=buoy_tz) &
#                           datetime<as.POSIXct('2011-11-01', tz=buoy_tz))),
#        aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'oct 2011 PAR data raw',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# #buoy moved to harbor oct 26 10 a
# ggplot(subset(buoy2011_L1,
#               subset = (datetime>=as.POSIXct('2011-10-26', tz=buoy_tz) &
#                           datetime<as.POSIXct('2011-10-27', tz=buoy_tz))),
#        aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'oct 2011 PAR data raw',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 hour')

# ggplot(subset(buoy2011_L1,
#               subset = (datetime>=as.POSIXct('2011-11-01', tz=buoy_tz) &
#                           datetime<as.POSIXct('2011-12-01', tz=buoy_tz))),
#        aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'nov 2011 PAR data raw',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2011_L1,
#               subset = (datetime>=as.POSIXct('2011-12-01', tz=buoy_tz) &
#                           datetime<as.POSIXct('2012-01-01', tz=buoy_tz))),
#        aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'dec 2011 PAR data raw',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')

ggplot(buoy2011_L1,
       aes(x = datetime, y = PAR , shape = location, color = flag_par)) +
  geom_point() +
  final_theme +
  labs(title = '2011 PAR data clean',
       x = NULL,
       y = 'PAR (umol/m2/s)') +
  scale_x_datetime(date_minor_breaks = '1 month')

#rename with CV
buoy2011_L1 <- buoy2011_L1 %>% 
  rename(radiationIncomingPAR_umolm2s = PAR)

#### Air Temp ####
range(buoy2011_L1$AirTempC, na.rm = T)

#recode when in transit
buoy2011_L1 <- buoy2011_L1 %>% 
  mutate(AirTempC = case_when(location == 'in transit' ~ NA_real_, 
                              TRUE ~ AirTempC))

ggplot(buoy2011_L1, aes(x=datetime, y = AirTempC)) +
  geom_point() +
  final_theme +
  labs(title = '2011 air temp raw',
       x= NULL,
       y= 'air temp (deg C)') +
  scale_x_datetime(date_minor_breaks = '1 month')

# ggplot(subset(buoy2011_L1,
#               subset=(datetime>=as.POSIXct('2011-01-01', tz=buoy_tz) &
#                         datetime < as.POSIXct('2011-02-01', tz=buoy_tz))),
#        aes(x=datetime, y = AirTempC)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'jan 2011 air temp raw',
#        x= NULL,
#        y= 'air temp (deg C)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2011_L1,
#               subset=(datetime>=as.POSIXct('2011-01-03', tz=buoy_tz) &
#                         datetime < as.POSIXct('2011-01-04', tz=buoy_tz))),
#        aes(x=datetime, y = AirTempC)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'jan 2011 air temp raw',
#        x= NULL,
#        y= 'air temp (deg C)') +
#   scale_x_datetime(date_minor_breaks = '1 hour')

buoy2011_L1 <- buoy2011_L1 %>% 
  mutate(AirTempC = case_when(datetime < as.POSIXct('2011-01-03 12:10') ~ NA_real_,
                              TRUE ~ AirTempC))

# ggplot(subset(buoy2011_L1,
#               subset=(datetime>=as.POSIXct('2011-01-01', tz=buoy_tz) &
#                         datetime < as.POSIXct('2011-02-01', tz=buoy_tz))),
#        aes(x=datetime, y = AirTempC)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'jan 2011 air temp clean',
#        x= NULL,
#        y= 'air temp (deg C)') +
#   scale_x_datetime(date_minor_breaks = '1 day')

ggplot(buoy2011_L1, aes(x=datetime, y = AirTempC, color = location)) +
  geom_point() +
  final_theme +
  labs(title = '2011 air temp clean',
       x= NULL,
       y= 'air temp (deg C)') +
  scale_x_datetime(date_minor_breaks = '1 month')

#rename with CV
buoy2011_L1 <- buoy2011_L1 %>% 
  rename(airTemperature_degC = AirTempC)

#### add buoy location as offline for prior to jan 3 ####
buoy2011_L1 <- buoy2011_L1 %>% 
  mutate(location = case_when(datetime < as.POSIXct('2011-01-03 11:00', tz=buoy_tz) ~ 'offline',
                              TRUE ~ location))


#### EXPORT L1 DATA ####
colnames(buoy2011_L1)


#export L1 tempstring file
buoy2011_L1 %>%
  select(datetime, location, waterTemperature_degC_0p5m:waterTemperature_degC_9p5m) %>%
  mutate(datetime = as.character(datetime)) %>%
  write_csv(., file.path(dump_dir,'tempstring/2011_tempstring_L1_v2022.csv'))

# export L1 do file
buoy2011_L1 %>%
  select(datetime, location, 
         oxygenDissolved_mgl_1p5m, oxygenDissolvedPercentOfSaturation_pct_1p5m, waterTemperature_DO_degC_1p5m, 
         flag_do1p5m) %>%
  mutate(datetime = as.character(datetime)) %>%
  write_csv(., file.path(dump_dir, 'do/2011_do_L1_v2022.csv'))

#export L1 met file
buoy2011_L1 %>%
  select(datetime, location, 
         windDirectionInstantaneous_deg, windSpeedInstantaneous_mps,
         radiationIncomingPAR_umolm2s, flag_par,
         airTemperature_degC) %>%
  mutate(datetime = as.character(datetime)) %>%
  write_csv(., file.path(dump_dir, 'met/2011_met_L1_v2022.csv'))
