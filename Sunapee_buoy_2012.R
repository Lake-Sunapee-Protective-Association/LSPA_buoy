#*****************************************************************
#*      Cary Institute of Ecosystem Studies (Millbrook, NY)      *
#*                                                               *
#* TITLE:   Sunapee_buoy_2012.r                                  *
#* AUTHOR:  Bethel Steele                                        *
#*****************************************************************


source('library_func_lists.R')

#point to data directories
raw_dir = 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L0/'
dump_dir = 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/'

#set tz
buoy_tz = 'Etc/GMT+5'

#bring in 2012 buoy raw data
buoy2012_L0 <- read.csv(file.path(raw_dir, 'Sunapee2012_rawData.csv'),
                        col.names = c('datetime', 'AirTempC', 'DOSat', 'DOppm', 'DOSat2',
                                      'PAR', 'RH', 'DOTempC', 'TempC_0m', 'TempC_0p5m', 'TempC_1m',
                                      'TempC_1p5m', 'TempC_2m', 'TempC_2p5m', 'TempC_3m', 'TempC_4m',
                                      'TempC_5m', 'TempC_6m', 'TempC_7m', 'TempC_8m', 'TempC_9m',
                                      'TempC_10m', 'TempC_11m', 'TempC_13m', 'InstWindDir', 'AveWindDir',
                                      'AveWindSp', 'InstWindSp'),
                        skip=1) %>%
  select(-DOSat2, -AveWindDir, -AveWindSp, -TempC_0p5m, -TempC_1p5m, -TempC_2p5m, -TempC_10m, -TempC_11m, -TempC_13m) %>%  #drop redundant columns
  mutate(datetime = as.POSIXct(datetime, format='%m/%d/%Y %H:%M', tz=buoy_tz))

#double check to make sure there are no DST issues
datelength2012 <- buoy2012_L0 %>% 
  mutate(date = format(datetime, '%Y-%m-%d')) %>% 
  group_by(date) %>% 
  summarize(length(datetime))

#check dst dates
datelength2012[datelength2012$date == '2012-03-11',]
#dst observed here
datelength2012[datelength2012$date == '2012-11-04',]
#all present

#force into NYtz with dst; convert to utc-5
buoy2012_L1 <- buoy2012_L0 %>% 
  mutate(datetime_instrument = force_tz(datetime, tz = 'America/New_York'),
         datetime = with_tz(datetime_instrument, tz = buoy_tz))

#check again
datelength2012 <- buoy2012_L1 %>% 
  mutate(date = format(datetime, '%Y-%m-%d')) %>% 
  group_by(date) %>%
  summarize(length(datetime))
#look at dst
datelength2012[datelength2012$date == '2012-03-11',]
datelength2012[datelength2012$date == '2012-11-04',]

#create dummy timestamp so there are no blanks
alltimes_2012 <- as.data.frame(seq.POSIXt(as.POSIXct('2012-01-01 00:00', tz=buoy_tz), as.POSIXct('2012-12-31 23:50', tz=buoy_tz), '10 min')) %>% 
  rename("datetime" = !!names(.[1]))

buoy2012_L1 <- buoy2012_L1 %>% 
  right_join(., alltimes_2012) %>% 
  arrange(datetime)

#clean up workspace
rm(datelength2012, alltimes_2012)


#### thermistors ####
buoy2012_temp_vert <- buoy2012_L1 %>%
  select(datetime, all_of(alltemp2011)) %>%
  pivot_longer(names_to = 'variable', values_to = 'value', -datetime) %>% 
  mutate(variable = factor(variable, levels=alltemp2011))

# ggplot(subset(buoy2012_temp_vert,
#               subset=(datetime >=as.POSIXct('2012-01-01', tz=buoy_tz) &
#                         datetime < as.POSIXct('2013-01-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='2012 buoy temp, raw',
#        x=NULL,
#        y='temp (deg C)') +
#   scale_x_datetime(date_minor_breaks = '1 month') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust

buoy2012_L1 <- buoy2012_L1 %>%
  mutate_at(vars(all_of(alltemp2011)),
            ~(case_when(. == -6999 ~ NA_real_,
                           . == 555.4 ~ NA_real_,
                           TRUE ~ .))) %>%
  mutate(location = 'loon')

buoy2012_temp_vert <- buoy2012_L1 %>%
  select(datetime, all_of(alltemp2011)) %>%
  pivot_longer(names_to = 'variable', values_to = 'value', -datetime) %>% 
  mutate(variable = factor(variable, levels=alltemp2011))

ggplot(subset(buoy2012_temp_vert,
              subset=(datetime >=as.POSIXct('2012-01-01', tz=buoy_tz) &
                        datetime < as.POSIXct('2013-01-01', tz=buoy_tz))),
       aes(x=datetime, y=value, color=as.factor(variable))) +
  geom_point() +
  final_theme +
  labs(title='2012 buoy temp, NAs recoded',
       x=NULL,
       y='temp (deg C)') +
  scale_x_datetime(date_minor_breaks = '1 month') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
                              "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust

# ggplot(subset(buoy2012_temp_vert,
#               subset=(datetime >=as.POSIXct('2012-04-01', tz=buoy_tz) &
#                         datetime < as.POSIXct('2012-05-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Apr 2012, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# #April 1, buoy moved to loon, temp online apr 18
# ggplot(subset(buoy2012_temp_vert,
#               subset=(datetime >=as.POSIXct('2012-04-18', tz=buoy_tz) &
#                         datetime < as.POSIXct('2012-04-19', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Apr 2012, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   coord_cartesian(ylim=c(0,20)) +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust

buoy2012_L1 <- buoy2012_L1 %>%
  mutate_at(vars(alltemp2011),
            ~(case_when(datetime < as.POSIXct('2012-04-18 14:00', tz=buoy_tz) ~ NA_real_,
                           TRUE ~ .)))

buoy2012_temp_vert_b <- buoy2012_L1 %>%
  select(datetime, all_of(alltemp2011)) %>%
  pivot_longer(names_to = 'variable', values_to = 'value', -datetime) %>% 
  mutate(variable = factor(variable, levels=alltemp2011))

# ggplot(subset(buoy2012_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2012-04-01', tz=buoy_tz) &
#                         datetime < as.POSIXct('2012-05-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Apr 2012, clean',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
#
# ggplot(subset(buoy2012_temp_vert,
#               subset=(datetime >=as.POSIXct('2012-05-01', tz=buoy_tz) &
#                         datetime < as.POSIXct('2012-06-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='May 2012, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# ggplot(subset(buoy2012_temp_vert,
#               subset=(datetime >=as.POSIXct('2012-06-01', tz=buoy_tz) &
#                         datetime < as.POSIXct('2012-07-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='June 2012, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# ggplot(subset(buoy2012_temp_vert,
#               subset=(datetime >=as.POSIXct('2012-07-01', tz=buoy_tz) &
#                         datetime < as.POSIXct('2012-08-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='July 2012, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
#
# ggplot(subset(buoy2012_temp_vert,
#               subset=(datetime >=as.POSIXct('2012-08-01', tz=buoy_tz) &
#                         datetime < as.POSIXct('2012-09-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Aug 2012, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# ggplot(subset(buoy2012_temp_vert,
#               subset=(datetime >=as.POSIXct('2012-09-01', tz=buoy_tz) &
#                         datetime < as.POSIXct('2012-10-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Sept 2012, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust

# ggplot(subset(buoy2012_temp_vert,
#               subset=(datetime >=as.POSIXct('2012-10-01', tz=buoy_tz) &
#                         datetime < as.POSIXct('2012-11-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Oct 2012, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust

# #Oct 10 temp lines off
# ggplot(subset(buoy2012_temp_vert,
#               subset=(datetime >=as.POSIXct('2012-10-10', tz=buoy_tz) &
#                         datetime < as.POSIXct('2012-10-11', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Oct 2012, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust

buoy2012_L1 <- buoy2012_L1 %>%
  mutate_at(vars(alltemp2011),
            ~(case_when(datetime >= as.POSIXct('2012-10-10 12:00', tz=buoy_tz) ~ NA_real_,
                           TRUE ~ .)))

buoy2012_temp_vert_b <- buoy2012_L1 %>%
  select(datetime, all_of(alltemp2011)) %>%
  pivot_longer(names_to = 'variable', values_to = 'value', -datetime) %>% 
  mutate(variable = factor(variable, levels=alltemp2011))

# ggplot(subset(buoy2012_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2012-10-01', tz=buoy_tz) &
#                         datetime < as.POSIXct('2012-11-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Oct 2012, clean',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
ggplot(subset(buoy2012_temp_vert_b,
              subset=(datetime >=as.POSIXct('2012-01-01', tz=buoy_tz) &
                        datetime < as.POSIXct('2013-01-01', tz=buoy_tz))),
       aes(x=datetime, y=value, color=as.factor(variable))) +
  geom_point() +
  final_theme +
  labs(title='2012 buoy temp, clean',
       x=NULL,
       y='temp deg C') +
  scale_x_datetime(date_minor_breaks = '1 month') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
                              "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust

#correct thermistor depth for offset; add CV
buoy2012_L1 <- buoy2012_L1 %>% 
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
rm(buoy2012_temp_vert, buoy2012_temp_vert_b)


#### DO sensors ####
range(buoy2012_L1$DOSat, na.rm=T)
range(buoy2012_L1$DOppm, na.rm=T)
range(buoy2012_L1$DOTempC, na.rm=T)

do_vert <- buoy2012_L1 %>% 
  select(datetime, DOSat, DOppm, DOTempC, location) %>% 
  pivot_longer(names_to = 'variable', values_to = 'value', -c(datetime, location))

ggplot(do_vert, aes(x = datetime, y = value)) +
  geom_point() +
  facet_grid(variable~., scales = 'free_y') +
  final_theme +
  labs(title = '2012 DO data raw',
       x = NULL,
       y = NULL) +
  scale_x_datetime(date_minor_breaks = '1 month')

# ggplot(subset(do_vert,
#               subset = (datetime >= as.POSIXct('2012-01-01', tz=buoy_tz) &
#                           datetime < as.POSIXct('2012-02-01', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'jan 2012 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(do_vert,
#               subset = (datetime >= as.POSIXct('2012-02-01', tz=buoy_tz) &
#                           datetime < as.POSIXct('2012-03-01', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'feb 2012 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(do_vert,
#               subset = (datetime >= as.POSIXct('2012-03-01', tz=buoy_tz) &
#                           datetime < as.POSIXct('2012-04-01', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'mar 2012 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(do_vert,
#               subset = (datetime >= as.POSIXct('2012-04-01', tz=buoy_tz) &
#                           datetime < as.POSIXct('2012-05-01', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'apr 2012 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# #errant sat and ppm data apr 12
# ggplot(subset(do_vert,
#               subset = (datetime >= as.POSIXct('2012-04-12', tz=buoy_tz) &
#                           datetime < as.POSIXct('2012-04-13', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'apr 2012 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 hour')
# 
# #apr 18 buoy moved to loon
# ggplot(subset(do_vert,
#               subset = (datetime >= as.POSIXct('2012-04-18', tz=buoy_tz) &
#                           datetime < as.POSIXct('2012-04-19', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'apr 2012 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 hour')
# 
buoy2012_L1 <- buoy2012_L1 %>% 
  mutate_at(vars(DOSat, DOppm, DOTempC),
            ~(case_when(datetime >= as.POSIXct('2012-04-12 11:20', tz=buoy_tz) &
                             datetime < as.POSIXct('2012-04-12 13:30', tz=buoy_tz) ~ NA_real_,
                           datetime >= as.POSIXct('2012-04-18 6:50', tz=buoy_tz) &
                             datetime < as.POSIXct('2012-04-18 09:00', tz=buoy_tz) ~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate(location = case_when(datetime < as.POSIXct('2012-04-18 6:50', tz=buoy_tz) ~'harbor',
                              datetime >= as.POSIXct('2012-04-18 6:50', tz=buoy_tz) &
                                datetime < as.POSIXct('2012-04-18 9:00', tz=buoy_tz) ~ 'in transit',
                              TRUE ~ location)) %>% 
  mutate_at(vars(DOSat, DOppm),
         ~ case_when(datetime > as.Date('2012-04-12') & datetime < as.Date('2012-04-16') & DOppm < 12 ~ NA_real_,
                     TRUE ~ .)) %>% 
  mutate(flag_do1p5m = case_when(datetime == as.POSIXct('2012-04-18 8:50', tz=buoy_tz) ~ 'wp', 
                                 TRUE ~ ''))

do_vert_b <- buoy2012_L1 %>% 
  select(datetime, DOSat, DOppm, DOTempC, location) %>% 
  pivot_longer(names_to = 'variable', values_to = 'value', -c(datetime, location))

# ggplot(subset(do_vert_b,
#               subset = (datetime >= as.POSIXct('2012-04-01', tz=buoy_tz) &
#                           datetime < as.POSIXct('2012-05-01', tz=buoy_tz))),
#        aes(x = datetime, y = value, color = location)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'apr 2012 DO data clean',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(do_vert_b,
#               subset = (datetime >= as.POSIXct('2012-05-01', tz=buoy_tz) &
#                           datetime < as.POSIXct('2012-06-01', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'may 2012 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(do_vert,
#               subset = (datetime >= as.POSIXct('2012-06-01', tz=buoy_tz) &
#                           datetime < as.POSIXct('2012-07-01', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'jun 2012 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(do_vert,
#               subset = (datetime >= as.POSIXct('2012-07-01', tz=buoy_tz) &
#                           datetime < as.POSIXct('2012-08-01', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'jul 2012 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(do_vert,
#               subset = (datetime >= as.POSIXct('2012-08-01', tz=buoy_tz) &
#                           datetime < as.POSIXct('2012-09-01', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'aug 2012 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(do_vert,
#               subset = (datetime >= as.POSIXct('2012-09-01', tz=buoy_tz) &
#                           datetime < as.POSIXct('2012-10-01', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'sept 2012 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(do_vert,
#               subset = (datetime >= as.POSIXct('2012-10-01', tz=buoy_tz) &
#                           datetime < as.POSIXct('2012-11-01', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'oct 2012 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# #buoy move oct18
# ggplot(subset(do_vert,
#               subset = (datetime >= as.POSIXct('2012-10-18', tz=buoy_tz) &
#                           datetime < as.POSIXct('2012-10-19', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'oct 2012 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 hour')
# #DO back online on 22nd
# ggplot(subset(do_vert,
#               subset = (datetime >= as.POSIXct('2012-10-22', tz=buoy_tz) &
#                           datetime < as.POSIXct('2012-10-23', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'oct 2012 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 hour')
# 
buoy2012_L1 <- buoy2012_L1 %>% 
  mutate_at(vars(DOSat, DOppm, DOTempC),
            ~(case_when(datetime >= as.POSIXct('2012-10-18 06:10', tz=buoy_tz) &
                             datetime < as.POSIXct('2012-10-22 09:40', tz=buoy_tz) ~ NA_real_,
                           TRUE ~ .))) %>%
  mutate(location = case_when(datetime >= as.POSIXct('2012-10-18 9:10', tz=buoy_tz) & 
                                datetime < as.POSIXct('2012-10-18 10:00', tz=buoy_tz)~ 'in transit',
                              datetime >= as.POSIXct('2012-10-18 10:00', tz=buoy_tz) ~'harbor',
                              TRUE ~ location)) %>% 
  mutate(flag_do1p5m = case_when(datetime == as.POSIXct('2012-10-18 09:50', tz=buoy_tz) ~ 'wp', 
                                 TRUE ~ flag_do1p5m))

do_vert_b <- buoy2012_L1 %>% 
  select(datetime, DOSat, DOppm, DOTempC, location) %>% 
  pivot_longer(names_to = 'variable', values_to = 'value', -c(datetime, location))

# ggplot(subset(do_vert_b,
#               subset = (datetime >= as.POSIXct('2012-10-01', tz=buoy_tz) &
#                           datetime < as.POSIXct('2012-11-01', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'oct 2012 DO data clean',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')

# ggplot(subset(do_vert,
#               subset = (datetime >= as.POSIXct('2012-11-01', tz=buoy_tz) &
#                           datetime < as.POSIXct('2012-12-01', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'nov 2012 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(do_vert,
#               subset = (datetime >= as.POSIXct('2012-12-01', tz=buoy_tz) &
#                           datetime < as.POSIXct('2013-01-01', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'dec 2012 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(do_vert_b, aes(x = datetime, y = value, color = location)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = '2012 DO data clean',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 month')

#add flag for no calibration on record
buoy2012_L1 <- buoy2012_L1 %>% 
  mutate(flag_do1p5m = case_when(flag_do1p5m == '' ~ 'x',
                                   TRUE ~ paste('x', flag_do1p5m, sep = '; ')))

do_vert_b <- buoy2012_L1 %>% 
  select(datetime, DOSat, DOppm, DOTempC, location, flag_do1p5m) %>% 
  pivot_longer(names_to = 'variable', values_to = 'value', -c(datetime, location, flag_do1p5m))

ggplot(do_vert_b, aes(x = datetime, y = value, color = location, shape = flag_do1p5m)) +
  geom_point() +
  facet_grid(variable~., scales = 'free_y') +
  final_theme +
  labs(title = '2012 DO data clean',
       x = NULL,
       y = NULL) +
  scale_x_datetime(date_minor_breaks = '1 month')

#clean up workspace
rm(do_vert, do_vert_b)

#rename with CV
buoy2012_L1 <- buoy2012_L1 %>% 
  rename(oxygenDissolved_mgl_1p5m = DOppm,
         oxygenDissolvedPercentOfSaturation_pct_1p5m = DOSat,
         waterTemperature_DO_degC_1p5m = DOTempC)

#### wind sensors ####
range(buoy2012_L1$InstWindDir, na.rm = T)
range(buoy2012_L1$InstWindSp, na.rm = T)

#recode when buoy in transit
buoy2012_L1 <- buoy2012_L1 %>% 
  mutate_at(vars(InstWindDir ,InstWindSp),
            ~ case_when(location == 'in transit' ~ NA_real_,
                        TRUE ~ .))

wind_vert <- buoy2012_L1 %>% 
  select(datetime, location, InstWindDir ,InstWindSp) %>% 
  pivot_longer(names_to = 'variable', values_to = 'value', -c(datetime, location))

ggplot(wind_vert, aes(x = datetime, y = value, color = location)) +
  geom_point() +
  facet_grid(variable~., scales = 'free_y') +
  final_theme +
  labs(title = '2012 wind data raw',
       x = NULL,
       y = NULL) +
  scale_x_datetime(date_minor_breaks = '1 month')

# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2012-01-01', tz=buoy_tz) &
#                         datetime<as.POSIXct('2012-02-01', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'jan 2012 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# #sesnor frozen jan 13
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2012-01-13', tz=buoy_tz) &
#                         datetime<as.POSIXct('2012-01-14', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'jan 2012 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 hour')
# 
# #sensor frozen jan 27
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2012-01-27', tz=buoy_tz) &
#                         datetime<as.POSIXct('2012-01-28', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'jan 2012 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 hour')

buoy2012_L1 <- buoy2012_L1 %>% 
  mutate_at(vars(InstWindDir, InstWindSp),
            ~(case_when(datetime>=as.POSIXct('2012-01-13 9:00', tz=buoy_tz) &
                             datetime<as.POSIXct('2012-01-13 20:10', tz=buoy_tz) &
                             InstWindSp == 0 ~ NA_real_,
                           datetime>=as.POSIXct('2012-01-27 3:00', tz=buoy_tz) &
                             datetime<as.POSIXct('2012-01-27 19:00', tz=buoy_tz)&
                          InstWindSp == 0 ~ NA_real_,
                           TRUE ~ .)))

wind_vert_b <- buoy2012_L1 %>% 
  select(datetime, location, InstWindDir ,InstWindSp) %>% 
  pivot_longer(names_to = 'variable', values_to = 'value', -c(datetime, location))

# ggplot(subset(wind_vert_b,
#               subset=(datetime>=as.POSIXct('2012-01-01', tz=buoy_tz) &
#                         datetime<as.POSIXct('2012-02-01', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'jan 2012 wind data clean',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2012-02-01', tz=buoy_tz) &
#                         datetime<as.POSIXct('2012-03-01', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'feb 2012 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# #sensor frozen feb 24-25
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2012-02-24 12:00', tz=buoy_tz) &
#                         datetime<as.POSIXct('2012-02-25 12:00', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'feb 2012 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 hour')

buoy2012_L1 <- buoy2012_L1 %>% 
  mutate_at(vars(InstWindDir, InstWindSp),
            ~(case_when(datetime>=as.POSIXct('2012-02-24 20:00', tz=buoy_tz) &
                             datetime<as.POSIXct('2012-02-25 10:00', tz=buoy_tz) &
                          InstWindSp == 0 ~ NA_real_,
                           TRUE ~ .)))

wind_vert_b <- buoy2012_L1 %>% 
  select(datetime, location, InstWindDir ,InstWindSp) %>% 
  pivot_longer(names_to = 'variable', values_to = 'value', -c(datetime, location))

# ggplot(subset(wind_vert_b,
#               subset=(datetime>=as.POSIXct('2012-02-01', tz=buoy_tz) &
#                         datetime<as.POSIXct('2012-03-01', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'feb 2012 wind data clean',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2012-03-01', tz=buoy_tz) &
#                         datetime<as.POSIXct('2012-04-01', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'mar 2012 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# #sensor frozen mar 3
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2012-03-03', tz=buoy_tz) &
#                         datetime<as.POSIXct('2012-03-04', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'mar 2012 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 hour')
# 
# # sensor frozen mar 10
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2012-03-10', tz=buoy_tz) &
#                         datetime<as.POSIXct('2012-03-11', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'mar 2012 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 hour')

buoy2012_L1 <- buoy2012_L1 %>% 
  mutate_at(vars(InstWindDir, InstWindSp),
            ~(case_when(datetime>=as.POSIXct('2012-03-03 8:00', tz=buoy_tz) &
                             datetime<as.POSIXct('2012-03-03 12:00', tz=buoy_tz) &
                             InstWindSp == 0 ~ NA_real_,
                           datetime>=as.POSIXct('2012-03-10 6:00', tz=buoy_tz) &
                             datetime<as.POSIXct('2012-03-10 9:00', tz=buoy_tz)&
                          InstWindSp == 0  ~ NA_real_,
                           TRUE ~ .)))

wind_vert_b <- buoy2012_L1 %>% 
  select(datetime, location, InstWindDir ,InstWindSp) %>% 
  pivot_longer(names_to = 'variable', values_to = 'value', -c(datetime, location))

# ggplot(subset(wind_vert_b,
#               subset=(datetime>=as.POSIXct('2012-03-01', tz=buoy_tz) &
#                         datetime<as.POSIXct('2012-04-01', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'mar 2012 wind data clean',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2012-04-01', tz=buoy_tz) &
#                         datetime<as.POSIXct('2012-05-01', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'apr 2012 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# #april 18 buoy moved 8:30
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2012-04-18', tz=buoy_tz) &
#                         datetime<as.POSIXct('2012-04-19', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'apr 2012 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 hour')
# 
# ggplot(subset(wind_vert_b,
#               subset=(datetime>=as.POSIXct('2012-04-01', tz=buoy_tz) &
#                         datetime<as.POSIXct('2012-05-01', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'apr 2012 wind data clean',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2012-05-01', tz=buoy_tz) &
#                         datetime<as.POSIXct('2012-06-01', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'may 2012 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
#
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2012-06-01', tz=buoy_tz) &
#                         datetime<as.POSIXct('2012-07-01', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'jun 2012 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2012-07-01', tz=buoy_tz) &
#                         datetime<as.POSIXct('2012-08-01', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'jul 2012 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2012-08-01', tz=buoy_tz) &
#                         datetime<as.POSIXct('2012-09-01', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'aug 2012 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2012-09-01', tz=buoy_tz) &
#                         datetime<as.POSIXct('2012-10-01', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'sept 2012 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2012-10-01', tz=buoy_tz) &
#                         datetime<as.POSIXct('2012-11-01', tz=buoy_tz))),
#        aes(x = datetime, y = value, color = location)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'oct 2012 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# #oct 18 buoy moved to harbor 910a
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2012-10-18', tz=buoy_tz) &
#                         datetime<as.POSIXct('2012-10-19', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'oct 2012 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 hour')
# 
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2012-11-01', tz=buoy_tz) &
#                         datetime<as.POSIXct('2012-12-01', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'nov 2012 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2012-12-01', tz=buoy_tz) &
#                         datetime<as.POSIXct('2013-01-01', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'dec 2012 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# #errant data at end of dec - 17-18
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2012-12-17', tz=buoy_tz) &
#                         datetime<as.POSIXct('2012-12-18 12:00', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'dec 2012 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 hour')

buoy2012_L1 <- buoy2012_L1 %>% 
  mutate_at(vars(InstWindDir, InstWindSp),
            ~(case_when(datetime>=as.POSIXct('2012-12-17', tz=buoy_tz) &
                             datetime<as.POSIXct('2012-12-17 14:00', tz=buoy_tz) &
                             InstWindSp == 0~ NA_real_,
                           datetime>=as.POSIXct('2012-12-17 16:00', tz=buoy_tz) &
                             datetime<as.POSIXct('2012-12-18 3:00', tz=buoy_tz) ~ NA_real_,
                           TRUE ~ .)))

wind_vert_b <- buoy2012_L1 %>% 
  select(datetime, location, InstWindDir ,InstWindSp) %>% 
  pivot_longer(names_to = 'variable', values_to = 'value', -c(datetime, location))

# ggplot(subset(wind_vert_b,
#               subset=(datetime>=as.POSIXct('2012-12-01', tz=buoy_tz) &
#                         datetime<as.POSIXct('2013-01-01', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'dec 2012 wind data clean',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')

ggplot(wind_vert_b, aes(x = datetime, y = value, color = location)) +
  geom_point() +
  facet_grid(variable~., scales = 'free_y') +
  final_theme +
  labs(title = '2012 wind data clean',
       x = NULL,
       y = NULL) +
  scale_x_datetime(date_minor_breaks = '1 month')

#clean up workspace
rm(wind_vert, wind_vert_b)

#rename with CV
buoy2012_L1 <- buoy2012_L1 %>% 
  rename(windDirectionInstantaneous_deg = InstWindDir,
         windSpeedInstantaneous_mps = InstWindSp)


#### PAR sensors ####
range(buoy2012_L1$PAR, na.rm = T)

#recode negative to zero and flag
buoy2012_L1 <-  buoy2012_L1 %>%
  mutate(flag_par = case_when(PAR <0 ~ 'z',
                         TRUE ~ '')) %>%
  mutate(PAR = case_when(PAR <0 ~ 0,
                         TRUE ~ PAR))
#recode when in transit
buoy2012_L1 <- buoy2012_L1 %>% 
  mutate(PAR = case_when(location == 'in transit' ~ NA_real_, 
                         TRUE ~ PAR))

ggplot(buoy2012_L1, aes(x = datetime, y = PAR, color = location)) +
  geom_point() +
  final_theme +
  labs(title = '2012 PAR data below 0 recoded',
       x = NULL,
       y = 'PAR (umol/m2/s)') +
  scale_x_datetime(date_minor_breaks = '1 month')

# ggplot(subset(buoy2012_L1,
#               subset = (datetime>=as.POSIXct('2012-01-01', tz=buoy_tz) &
#                           datetime<as.POSIXct('2012-02-01', tz=buoy_tz))),
#        aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'jan 2012 PAR data raw',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2012_L1,
#               subset = (datetime>=as.POSIXct('2012-02-01', tz=buoy_tz) &
#                           datetime<as.POSIXct('2012-03-01', tz=buoy_tz))),
#        aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'feb 2012 PAR data raw',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')

# ggplot(subset(buoy2012_L1,
#               subset = (datetime>=as.POSIXct('2012-03-01', tz=buoy_tz) &
#                           datetime<as.POSIXct('2012-04-01', tz=buoy_tz))),
#        aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'mar 2012 PAR data raw',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2012_L1,
#               subset = (datetime>=as.POSIXct('2012-04-01', tz=buoy_tz) &
#                           datetime<as.POSIXct('2012-05-01', tz=buoy_tz))),
#        aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'apr 2012 PAR data raw',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# #buoy moved to loon apr 18
# 
# ggplot(subset(buoy2012_L1,
#               subset = (datetime>=as.POSIXct('2012-05-01', tz=buoy_tz) &
#                           datetime<as.POSIXct('2012-06-01', tz=buoy_tz))),
#        aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'may 2012 PAR data raw',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2012_L1,
#               subset = (datetime>=as.POSIXct('2012-06-01', tz=buoy_tz) &
#                           datetime<as.POSIXct('2012-07-01', tz=buoy_tz))),
#        aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'jun 2012 PAR data raw',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2012_L1,
#               subset = (datetime>=as.POSIXct('2012-07-01', tz=buoy_tz) &
#                           datetime<as.POSIXct('2012-08-01', tz=buoy_tz))),
#        aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'jul 2012 PAR data raw',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2012_L1,
#               subset = (datetime>=as.POSIXct('2012-08-01', tz=buoy_tz) &
#                           datetime<as.POSIXct('2012-09-01', tz=buoy_tz))),
#        aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'aug 2012 PAR data raw',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2012_L1,
#               subset = (datetime>=as.POSIXct('2012-09-01', tz=buoy_tz) &
#                           datetime<as.POSIXct('2012-10-01', tz=buoy_tz))),
#        aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'sept 2012 PAR data raw',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2012_L1,
#               subset = (datetime>=as.POSIXct('2012-10-01', tz=buoy_tz) &
#                           datetime<as.POSIXct('2012-11-01', tz=buoy_tz))),
#        aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'oct 2012 PAR data raw',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# #buoy moved to harbor oct 18
# 
# ggplot(subset(buoy2012_L1,
#               subset = (datetime>=as.POSIXct('2012-11-01', tz=buoy_tz) &
#                           datetime<as.POSIXct('2012-12-01', tz=buoy_tz))),
#        aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'nov 2012 PAR data raw',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2012_L1,
#               subset = (datetime>=as.POSIXct('2012-12-01', tz=buoy_tz) &
#                           datetime<as.POSIXct('2013-01-01', tz=buoy_tz))),
#        aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'dec 2012 PAR data raw',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')

ggplot(buoy2012_L1,
       aes(x = datetime, y = PAR, color = location, shape = flag_par)) +
  geom_point() +
  final_theme +
  labs(title = '2012 PAR data clean',
       x = NULL,
       y = 'PAR (umol/m2/s)') +
  scale_x_datetime(date_minor_breaks = '1 month')

#rename with CV
buoy2012_L1 <- buoy2012_L1 %>% 
  rename(radiationIncomingPAR_umolm2s = PAR)

#### Air Temp ####
range(buoy2012_L1$AirTempC, na.rm = T)

ggplot(buoy2012_L1, aes(x=datetime, y = AirTempC)) +
  geom_point() +
  final_theme +
  labs(title = '2012 air temp raw',
       x= NULL,
       y= 'air temp (deg C)') +
  scale_x_datetime(date_minor_breaks = '1 month')

#looks good

#recode times when buoy moved
buoy2012_L1 <- buoy2012_L1 %>%
  mutate(AirTempC = case_when(location == 'in transit' ~ NA_real_,
                              TRUE ~ AirTempC))

ggplot(buoy2012_L1, aes(x=datetime, y = AirTempC, color = location)) +
  geom_point() +
  final_theme +
  labs(title = '2012 air temp clean',
       x= NULL,
       y= 'air temp (deg C)') +
  scale_x_datetime(date_minor_breaks = '1 month')

#rename with CV
buoy2012_L1 <- buoy2012_L1 %>% 
  rename(airTemperature_degC = AirTempC)

#### relative humidity ####
range(buoy2012_L1$RH, na.rm = T)

#recode when in transit
buoy2012_L1 <- buoy2012_L1 %>% 
  mutate(RH= case_when(location == 'in transit' ~ NA_real_,
                       TRUE ~ RH))

ggplot(buoy2012_L1, aes(x=datetime, y = RH, color = location)) +
  geom_point() +
  final_theme +
  labs(title = '2012 relative humidity raw',
       x= NULL,
       y= 'Relative Humidity (percent)') +
  scale_x_datetime(date_minor_breaks = '1 month')

# ggplot(subset(buoy2012_L1,
#               datetime >= as.Date('2012-01-01') &
#                 datetime < as.Date('2012-02-01')), 
#        aes(x=datetime, y = RH, color = location)) +
#   geom_point() +
#   final_theme +
#   labs(title = '2012 relative humidity raw',
#        x= NULL,
#        y= 'Relative Humidity (percent)') +
#   scale_x_datetime(date_minor_breaks = '1 month')
# 
# ggplot(subset(buoy2012_L1,
#               datetime >= as.Date('2012-02-01') &
#                 datetime < as.Date('2012-03-01')), 
#        aes(x=datetime, y = RH, color = location)) +
#   geom_point() +
#   final_theme +
#   labs(title = '2012 relative humidity raw',
#        x= NULL,
#        y= 'Relative Humidity (percent)') +
#   scale_x_datetime(date_minor_breaks = '1 month')
# 
# ggplot(subset(buoy2012_L1,
#               datetime >= as.Date('2012-03-01') &
#                 datetime < as.Date('2012-04-01')), 
#        aes(x=datetime, y = RH, color = location)) +
#   geom_point() +
#   final_theme +
#   labs(title = '2012 relative humidity raw',
#        x= NULL,
#        y= 'Relative Humidity (percent)') +
#   scale_x_datetime(date_minor_breaks = '1 month')
# 
# ggplot(subset(buoy2012_L1,
#               datetime >= as.Date('2012-04-01') &
#                 datetime < as.Date('2012-05-01')), 
#        aes(x=datetime, y = RH, color = location)) +
#   geom_point() +
#   final_theme +
#   labs(title = '2012 relative humidity raw',
#        x= NULL,
#        y= 'Relative Humidity (percent)') +
#   scale_x_datetime(date_minor_breaks = '1 month')
# 
# ggplot(subset(buoy2012_L1,
#               datetime >= as.Date('2012-05-01') &
#                 datetime < as.Date('2012-06-01')), 
#        aes(x=datetime, y = RH, color = location)) +
#   geom_point() +
#   final_theme +
#   labs(title = '2012 relative humidity raw',
#        x= NULL,
#        y= 'Relative Humidity (percent)') +
#   scale_x_datetime(date_minor_breaks = '1 month')
# 
# ggplot(subset(buoy2012_L1,
#               datetime >= as.Date('2012-06-01') &
#                 datetime < as.Date('2012-07-01')), 
#        aes(x=datetime, y = RH, color = location)) +
#   geom_point() +
#   final_theme +
#   labs(title = '2012 relative humidity raw',
#        x= NULL,
#        y= 'Relative Humidity (percent)') +
#   scale_x_datetime(date_minor_breaks = '1 month')
# 
# ggplot(subset(buoy2012_L1,
#               datetime >= as.Date('2012-07-01') &
#                 datetime < as.Date('2012-08-01')), 
#        aes(x=datetime, y = RH, color = location)) +
#   geom_point() +
#   final_theme +
#   labs(title = '2012 relative humidity raw',
#        x= NULL,
#        y= 'Relative Humidity (percent)') +
#   scale_x_datetime(date_minor_breaks = '1 month')
# 
# ggplot(subset(buoy2012_L1,
#               datetime >= as.Date('2012-08-01') &
#                 datetime < as.Date('2012-09-01')), 
#        aes(x=datetime, y = RH, color = location)) +
#   geom_point() +
#   final_theme +
#   labs(title = '2012 relative humidity raw',
#        x= NULL,
#        y= 'Relative Humidity (percent)') +
#   scale_x_datetime(date_minor_breaks = '1 month')
# 
# ggplot(subset(buoy2012_L1,
#               datetime >= as.Date('2012-09-01') &
#                 datetime < as.Date('2012-10-01')), 
#        aes(x=datetime, y = RH, color = location)) +
#   geom_point() +
#   final_theme +
#   labs(title = '2012 relative humidity raw',
#        x= NULL,
#        y= 'Relative Humidity (percent)') +
#   scale_x_datetime(date_minor_breaks = '1 month')
# 
# ggplot(subset(buoy2012_L1,
#               datetime >= as.Date('2012-10-01') &
#                 datetime < as.Date('2012-11-01')), 
#        aes(x=datetime, y = RH, color = location)) +
#   geom_point() +
#   final_theme +
#   labs(title = '2012 relative humidity raw',
#        x= NULL,
#        y= 'Relative Humidity (percent)') +
#   scale_x_datetime(date_minor_breaks = '1 month')
# 
# ggplot(subset(buoy2012_L1,
#               datetime >= as.Date('2012-11-01') &
#                 datetime < as.Date('2012-12-01')), 
#        aes(x=datetime, y = RH, color = location)) +
#   geom_point() +
#   final_theme +
#   labs(title = '2012 relative humidity raw',
#        x= NULL,
#        y= 'Relative Humidity (percent)') +
#   scale_x_datetime(date_minor_breaks = '1 month')
# 
# ggplot(subset(buoy2012_L1,
#               datetime >= as.Date('2012-12-01') &
#                 datetime < as.Date('2013-01-01')), 
#        aes(x=datetime, y = RH, color = location)) +
#   geom_point() +
#   final_theme +
#   labs(title = '2012 relative humidity raw',
#        x= NULL,
#        y= 'Relative Humidity (percent)') +
#   scale_x_datetime(date_minor_breaks = '1 month')

#flag supersaturated as suspect
buoy2012_L1 <- buoy2012_L1 %>% 
  mutate(flag_rh = case_when(RH >100 ~ 's',
                             TRUE ~ ''))

#rename with CV
buoy2012_L1 <- buoy2012_L1 %>% 
  rename(relativeHumidity_perc = RH)

#### EXPORT L1 FILES ####
colnames(buoy2012_L1)


#export L1 tempstring file
buoy2012_L1 %>%
  select(datetime, location, waterTemperature_degC_0p5m:waterTemperature_degC_9p5m) %>%
  mutate(datetime = as.character(datetime)) %>%
  write_csv(., file.path(dump_dir,'tempstring/2012_tempstring_L1_v2022.csv'))

# export L1 do file
buoy2012_L1 %>%
  select(datetime, location, 
         oxygenDissolved_mgl_1p5m, oxygenDissolvedPercentOfSaturation_pct_1p5m, waterTemperature_DO_degC_1p5m, 
         flag_do1p5m) %>%
  mutate(datetime = as.character(datetime)) %>%
  write_csv(., file.path(dump_dir, 'do/2012_do_L1_v2022.csv'))

#export L1 met file
buoy2012_L1 %>%
  select(datetime, location, 
         windDirectionInstantaneous_deg, windSpeedInstantaneous_mps,
         radiationIncomingPAR_umolm2s, flag_par,
         airTemperature_degC,
         relativeHumidity_perc, flag_rh) %>%
  mutate(datetime = as.character(datetime)) %>%
  write_csv(., file.path(dump_dir, 'met/2012_met_L1_v2022.csv'))
