#*****************************************************************
#*      Cary Institute of Ecosystem Studies (Millbrook, NY)      *
#*                                                               *
#* TITLE:   Sunapee_buoy_2010.r                                  *
#* AUTHOR:  Bethel Steele                                        *
#*****************************************************************


source('library_func_lists.R')

#point to data directories
raw_dir = 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L0/'
dump_dir = 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/'

#set tz
buoy_tz = 'Etc/GMT+5'

#bring in 2010 buoy raw data
buoy2010_L0 <- read.csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L0/Sunapee2010_rawData.csv',
                        col.names = c('datetime', 'AirTempC', 'DOppm', 'DOSat', 'DOSat2', 
                                      'PAR', 'DOTempC', 'TempC_0m', 'TempC_0p5m', 'TempC_1m', 
                                      'TempC_1p5m', 'TempC_2m', 'TempC_2p5m', 'TempC_3m', 'TempC_4m', 
                                      'TempC_5m', 'TempC_6m', 'TempC_7m', 'TempC_8m', 'TempC_9m', 
                                      'TempC_10m', 'TempC_11m', 'TempC_13m', 'AveWindDir', 'InstWindDir', 
                                      'InstWindSp', 'AveWindSp'), 
                        skip=1) %>% 
  select(-DOSat2) %>%  #drop redundant columns - this has been previously plotted -it is on the 1:1
  mutate(datetime = as.POSIXct(datetime, format='%Y-%m-%d %H:%M:%S', tz=buoy_tz))

#double check to make sure there are no DST issues
datelength2010 <- buoy2010_L0 %>% 
  mutate(date = format(datetime, '%Y-%m-%d')) %>% 
  group_by(date) %>% 
  summarize(length(datetime))

#check dst dates
datelength2010[datelength2010$date == '2010-03-14',]
#dst observed here
datelength2010[datelength2010$date == '2010-11-07',]
#no data here 

#force into NYtz with dst; convert to utc-5
buoy2010_L1 <- buoy2010_L0 %>% 
  mutate(datetime_instrument = force_tz(datetime, tz = 'America/New_York'),
         datetime = with_tz(datetime_instrument, tz = buoy_tz))

#check again
datelength2010 <- buoy2010_L1 %>% 
  mutate(date = format(datetime, '%Y-%m-%d')) %>% 
  group_by(date) %>%
  summarize(length(datetime))
#look at dst
datelength2010[datelength2010$date == '2010-03-14',]
datelength2010[datelength2010$date == '2010-11-07',]

#create dummy timestamp so there are no blanks
alltimes_2010 <- as.data.frame(seq.POSIXt(as.POSIXct('2010-01-01 00:00', tz=buoy_tz), as.POSIXct('2010-12-31 23:50', tz=buoy_tz), '10 min')) %>% 
  rename("datetime" = !!names(.[1]))

buoy2010_L1 <- buoy2010_L1 %>% 
  right_join(., alltimes_2010) %>% 
  arrange(datetime)

#clean up workspace
rm(datelength2010, alltimes_2010)

#### thermistors ####
buoy2010_temp_vert <- buoy2010_L1 %>%
  select(datetime, all_of(alltemp2007)) %>%
  pivot_longer(names_to = 'variable', values_to = 'value', -datetime) %>% 
  mutate(variable = factor(variable, levels=alltemp2007))

# ggplot(subset(buoy2010_temp_vert,
#               subset=(datetime >=as.POSIXct('2010-01-01', tz=buoy_tz) &
#                         datetime < as.POSIXct('2011-01-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='2010 buoy temp data raw',
#        x=NULL,
#        y='temp (deg C)') +
#   scale_x_datetime(date_minor_breaks = '1 month') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust

buoy2010_L1 <- buoy2010_L1 %>% 
  mutate_at(vars(alltemp2007),
            ~(case_when(. == -6999 ~ NA_real_,
                           . == -99.9 ~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate(location = 'loon')

buoy2010_temp_vert <- buoy2010_L1 %>%
  select(datetime, all_of(alltemp2007)) %>%
  pivot_longer(names_to = 'variable', values_to = 'value', -datetime) %>% 
  mutate(variable = factor(variable, levels=alltemp2007))

# ggplot(subset(buoy2010_temp_vert,
#               subset=(datetime >=as.POSIXct('2010-01-01', tz=buoy_tz) &
#                         datetime < as.POSIXct('2011-01-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='2010 buoy temp data, NAs recoded',
#        x=NULL,
#        y='temp (deg C)') +
#   scale_x_datetime(date_minor_breaks = '1 month') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# ggplot(subset(buoy2010_temp_vert,
#               subset=(datetime >=as.POSIXct('2010-01-01', tz=buoy_tz) &
#                         datetime < as.POSIXct('2010-02-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Jan 2010, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# #look closer at jan 8 to see if there is any pattern
# ggplot(subset(buoy2010_temp_vert,
#               subset=(datetime >=as.POSIXct('2010-01-08 12:00', tz=buoy_tz) &
#                         datetime < as.POSIXct('2010-01-09', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Jan 2010, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# ggplot(subset(buoy2010_temp_vert,
#               subset=(datetime >=as.POSIXct('2010-01-01', tz=buoy_tz) &
#                         datetime < as.POSIXct('2010-02-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Jan 2010, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   coord_cartesian(ylim=c(-5, 10)) +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_y_continuous(minor_breaks = c(-5:10)) +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust

buoy2010_L1 <- buoy2010_L1 %>% 
  mutate_at(vars(all_of(alltemp2007)),
            ~(case_when(datetime >= as.POSIXct('2010-01-01', tz=buoy_tz) & 
                             datetime < as.POSIXct('2010-02-01', tz=buoy_tz) & (. > 2 | . < -1) ~ NA_real_,
                           TRUE ~ .))) 

buoy2010_temp_vert_b <- buoy2010_L1 %>%
  select(datetime, all_of(alltemp2007)) %>%
  pivot_longer(names_to = 'variable', values_to = 'value', -datetime) %>% 
  mutate(variable = factor(variable, levels=alltemp2007))

# ggplot(subset(buoy2010_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2010-01-01', tz=buoy_tz) &
#                         datetime < as.POSIXct('2010-02-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Jan 2010, NAs recoded, v2',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# #errant data Jan 3, Jan9-10, Jan 11, Jan 13-14, Jan 16, Jan 22-23, Jan 29, 30, 31
# ggplot(subset(buoy2010_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2010-01-03', tz=buoy_tz) &
#                         datetime < as.POSIXct('2010-01-04', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Jan 2010, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# ggplot(subset(buoy2010_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2010-01-08', tz=buoy_tz) &
#                         datetime < as.POSIXct('2010-01-09', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Jan 2010, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# ggplot(subset(buoy2010_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2010-01-09', tz=buoy_tz) &
#                         datetime < as.POSIXct('2010-01-10', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Jan 2010, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# ggplot(subset(buoy2010_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2010-01-11', tz=buoy_tz) &
#                         datetime < as.POSIXct('2010-01-12', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Jan 2010, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# ggplot(subset(buoy2010_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2010-01-13', tz=buoy_tz) &
#                         datetime < as.POSIXct('2010-01-14 12:00', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Jan 2010, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# ggplot(subset(buoy2010_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2010-01-16', tz=buoy_tz) &
#                         datetime < as.POSIXct('2010-01-17 12:00', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Jan 2010, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# ggplot(subset(buoy2010_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2010-01-22 12:00', tz=buoy_tz) &
#                         datetime < as.POSIXct('2010-01-23 16:00', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Jan 2010, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# ggplot(subset(buoy2010_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2010-01-29', tz=buoy_tz) &
#                         datetime < as.POSIXct('2010-01-30', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Jan 2010, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# ggplot(subset(buoy2010_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2010-01-30', tz=buoy_tz) &
#                         datetime < as.POSIXct('2010-01-31', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Jan 2010, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# ggplot(subset(buoy2010_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2010-01-31', tz=buoy_tz) &
#                         datetime < as.POSIXct('2010-02-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Jan 2010, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust

buoy2010_L1 <- buoy2010_L1 %>% 
  mutate_at(vars(all_of(alltemp2007)),
            ~(case_when(datetime >= as.POSIXct('2010-01-03 2:20', tz=buoy_tz) & datetime < as.POSIXct('2010-01-03 12:20', tz=buoy_tz) ~ NA_real_,
                           datetime == as.POSIXct('2010-01-08 8:10', tz=buoy_tz) ~ NA_real_,
                           datetime >= as.POSIXct('2010-01-08 20:30', tz=buoy_tz) & datetime < as.POSIXct('2010-01-09 8:10', tz=buoy_tz) ~ NA_real_,
                           datetime == as.POSIXct('2010-01-11 11:00', tz=buoy_tz) ~ NA_real_,
                           datetime >= as.POSIXct('2010-01-13 14:10', tz=buoy_tz) & datetime < as.POSIXct('2010-01-14 10:30', tz=buoy_tz) ~ NA_real_,
                           datetime >= as.POSIXct('2010-01-16 13:50', tz=buoy_tz) & datetime < as.POSIXct('2010-01-17 4:40', tz=buoy_tz) ~ NA_real_,
                           datetime >= as.POSIXct('2010-01-22 23:00', tz=buoy_tz) & datetime < as.POSIXct('2010-01-23 0:50', tz=buoy_tz) ~ NA_real_,
                           datetime == as.POSIXct('2010-01-23 14:10', tz=buoy_tz) ~ NA_real_,
                           datetime == as.POSIXct('2010-01-23 10:40', tz=buoy_tz) ~ NA_real_,
                           datetime >= as.POSIXct('2010-01-29 4:00', tz=buoy_tz) & datetime < as.POSIXct('2010-01-29 6:50', tz=buoy_tz) ~ NA_real_,
                           datetime >= as.POSIXct('2010-01-30 16:50', tz=buoy_tz) & datetime < as.POSIXct('2010-01-30 19:00', tz=buoy_tz) ~ NA_real_,
                           datetime >= as.POSIXct('2010-01-30 16:50', tz=buoy_tz) & datetime < as.POSIXct('2010-01-30 19:00', tz=buoy_tz) ~ NA_real_,
                           datetime == as.POSIXct('2010-01-31 11:50', tz=buoy_tz) ~ NA_real_,
                           TRUE ~ .))) 

buoy2010_temp_vert_b <- buoy2010_L1 %>%
  select(datetime, all_of(alltemp2007)) %>%
  pivot_longer(names_to = 'variable', values_to = 'value', -datetime) %>% 
  mutate(variable = factor(variable, levels=alltemp2007))

# ggplot(subset(buoy2010_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2010-01-01', tz=buoy_tz) &
#                         datetime < as.POSIXct('2010-02-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Jan 2010, semi clean',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# ggplot(subset(buoy2010_temp_vert,
#               subset=(datetime >=as.POSIXct('2010-02-01', tz=buoy_tz) &
#                         datetime < as.POSIXct('2010-03-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Feb 2010, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust

buoy2010_L1 <- buoy2010_L1 %>%
  mutate_at(vars(alltemp2007),
            ~(case_when(datetime >= as.POSIXct('2010-02-01', tz=buoy_tz) & datetime < as.POSIXct('2010-03-01', tz=buoy_tz) & (. > 2 | . < -1) ~ NA_real_,
                           TRUE ~ .)))

buoy2010_temp_vert_b <- buoy2010_L1 %>%
  select(datetime, all_of(alltemp2007)) %>%
  pivot_longer(names_to = 'variable', values_to = 'value', -datetime) %>% 
  mutate(variable = factor(variable, levels=alltemp2007))

# ggplot(subset(buoy2010_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2010-02-01', tz=buoy_tz) &
#                         datetime < as.POSIXct('2010-03-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Feb 2010, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# #Feb 05, 06, 07, 08, 08-09, 09, 13, 19, 22, 24
# ggplot(subset(buoy2010_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2010-02-05', tz=buoy_tz) &
#                         datetime < as.POSIXct('2010-02-06', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Feb 2010, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# ggplot(subset(buoy2010_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2010-02-06', tz=buoy_tz) &
#                         datetime < as.POSIXct('2010-02-07', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Feb 2010, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# ggplot(subset(buoy2010_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2010-02-07', tz=buoy_tz) &
#                         datetime < as.POSIXct('2010-02-08', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Feb 2010, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# ggplot(subset(buoy2010_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2010-02-08', tz=buoy_tz) &
#                         datetime < as.POSIXct('2010-02-09', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Feb 2010, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# ggplot(subset(buoy2010_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2010-02-08 12:00', tz=buoy_tz) &
#                         datetime < as.POSIXct('2010-02-09 12:00', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Feb 2010, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# ggplot(subset(buoy2010_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2010-02-09', tz=buoy_tz) &
#                         datetime < as.POSIXct('2010-02-10', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Feb 2010, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# ggplot(subset(buoy2010_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2010-02-12', tz=buoy_tz) &
#                         datetime < as.POSIXct('2010-02-13', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Feb 2010, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# ggplot(subset(buoy2010_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2010-02-13', tz=buoy_tz) &
#                         datetime < as.POSIXct('2010-02-14', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Feb 2010, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# ggplot(subset(buoy2010_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2010-02-19', tz=buoy_tz) &
#                         datetime < as.POSIXct('2010-02-20', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Feb 2010, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# ggplot(subset(buoy2010_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2010-02-22', tz=buoy_tz) &
#                         datetime < as.POSIXct('2010-02-23', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Feb 2010, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# ggplot(subset(buoy2010_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2010-02-24', tz=buoy_tz) &
#                         datetime < as.POSIXct('2010-02-25', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Feb 2010, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
buoy2010_L1 <- buoy2010_L1 %>% 
  mutate_at(vars(all_of(alltemp2007)),
            ~(case_when(datetime >= as.POSIXct('2010-02-05 01:30', tz=buoy_tz) & datetime < as.POSIXct('2010-02-05 2:40', tz=buoy_tz) ~ NA_real_,
                           datetime >= as.POSIXct('2010-02-06 01:00', tz=buoy_tz) & datetime < as.POSIXct('2010-02-06 2:30', tz=buoy_tz) ~ NA_real_,
                           datetime == as.POSIXct('2010-02-06 13:00', tz=buoy_tz) ~ NA_real_,
                           datetime >= as.POSIXct('2010-02-06 17:50', tz=buoy_tz) & datetime < as.POSIXct('2010-02-06 19:50', tz=buoy_tz) ~ NA_real_,
                           datetime >= as.POSIXct('2010-02-07 12:30', tz=buoy_tz) & datetime < as.POSIXct('2010-02-07 12:50', tz=buoy_tz) ~ NA_real_,
                           datetime >= as.POSIXct('2010-02-08 03:10', tz=buoy_tz) & datetime < as.POSIXct('2010-02-08 11:50', tz=buoy_tz) ~ NA_real_,
                           datetime >= as.POSIXct('2010-02-08 21:40', tz=buoy_tz) & datetime < as.POSIXct('2010-02-09 8:30', tz=buoy_tz) ~ NA_real_,
                           datetime >= as.POSIXct('2010-02-09 12:30', tz=buoy_tz) & datetime < as.POSIXct('2010-02-09 14:00', tz=buoy_tz) ~ NA_real_,
                           datetime >= as.POSIXct('2010-02-12 6:30', tz=buoy_tz) & datetime < as.POSIXct('2010-02-12 8:30', tz=buoy_tz) ~ NA_real_,
                           datetime >= as.POSIXct('2010-02-13 3:10', tz=buoy_tz) & datetime < as.POSIXct('2010-02-13 9:50', tz=buoy_tz) ~ NA_real_,
                           datetime == as.POSIXct('2010-02-19 8:20', tz=buoy_tz) ~ NA_real_,
                           datetime == as.POSIXct('2010-02-19 20:00', tz=buoy_tz) ~ NA_real_,
                           datetime == as.POSIXct('2010-02-22 11:30', tz=buoy_tz) ~ NA_real_,
                           datetime == as.POSIXct('2010-02-24 10:10', tz=buoy_tz) ~ NA_real_,
                           TRUE ~ .))) 

buoy2010_temp_vert_b <- buoy2010_L1 %>%
  select(datetime, all_of(alltemp2007)) %>%
  pivot_longer(names_to = 'variable', values_to = 'value', -datetime) %>% 
  mutate(variable = factor(variable, levels=alltemp2007))

# ggplot(subset(buoy2010_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2010-02-01', tz=buoy_tz) &
#                         datetime < as.POSIXct('2010-03-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Feb 2010, semi clean',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# ggplot(subset(buoy2010_temp_vert,
#               subset=(datetime >=as.POSIXct('2010-03-01', tz=buoy_tz) &
#                         datetime < as.POSIXct('2010-04-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Mar 2010, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust

buoy2010_L1 <- buoy2010_L1 %>% 
  mutate_at(vars(alltemp2007),
            ~(case_when(datetime >= as.POSIXct('2010-03-01', tz=buoy_tz) & datetime < as.POSIXct('2010-04-01', tz=buoy_tz) & (. > 5 | . < 0) ~ NA_real_,
                           TRUE ~ .))) 

buoy2010_temp_vert_b <- buoy2010_L1 %>%
  select(datetime, all_of(alltemp2007)) %>%
  pivot_longer(names_to = 'variable', values_to = 'value', -datetime) %>% 
  mutate(variable = factor(variable, levels=alltemp2007))

# ggplot(subset(buoy2010_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2010-03-01', tz=buoy_tz) &
#                         datetime < as.POSIXct('2010-04-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Mar 2010, NAs recoded, v2',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust

# #Mar 1, 9,  15, 19, 24, 27, 29
# ggplot(subset(buoy2010_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2010-03-01', tz=buoy_tz) &
#                         datetime < as.POSIXct('2010-03-02', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Mar 2010, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# ggplot(subset(buoy2010_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2010-03-09', tz=buoy_tz) &
#                         datetime < as.POSIXct('2010-03-10', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Mar 2010, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# ggplot(subset(buoy2010_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2010-03-15', tz=buoy_tz) &
#                         datetime < as.POSIXct('2010-03-16', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Mar 2010, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# ggplot(subset(buoy2010_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2010-03-19', tz=buoy_tz) &
#                         datetime < as.POSIXct('2010-03-20', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Mar 2010, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# ggplot(subset(buoy2010_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2010-03-24', tz=buoy_tz) &
#                         datetime < as.POSIXct('2010-03-25', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Mar 2010, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# ggplot(subset(buoy2010_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2010-03-27', tz=buoy_tz) &
#                         datetime < as.POSIXct('2010-03-28', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Mar 2010, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# ggplot(subset(buoy2010_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2010-03-29', tz=buoy_tz) &
#                         datetime < as.POSIXct('2010-03-30', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Mar 2010, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
buoy2010_L1 <- buoy2010_L1 %>% 
  mutate_at(vars(alltemp2007),
            ~(case_when(datetime == as.POSIXct('2010-03-01 4:50', tz=buoy_tz) ~ NA_real_,
                           datetime >= as.POSIXct('2010-03-09 4:00', tz=buoy_tz) & datetime < as.POSIXct('2010-03-09 19:50', tz=buoy_tz) ~ NA_real_,
                           datetime == as.POSIXct('2010-03-15 2:10', tz=buoy_tz) ~ NA_real_,
                           datetime >= as.POSIXct('2010-03-19 13:10', tz=buoy_tz) & datetime < as.POSIXct('2010-03-19 14:40', tz=buoy_tz) ~ NA_real_,
                           datetime == as.POSIXct('2010-03-24 7:30', tz=buoy_tz) ~ NA_real_,
                           datetime == as.POSIXct('2010-03-27 19:10', tz=buoy_tz) ~ NA_real_,
                           datetime == as.POSIXct('2010-03-29 18:00', tz=buoy_tz) ~ NA_real_,
                           datetime == as.POSIXct('2010-03-29 18:50', tz=buoy_tz) ~ NA_real_,
                           TRUE ~ .))) 

buoy2010_temp_vert_b <- buoy2010_L1 %>%
  select(datetime, all_of(alltemp2007)) %>%
  pivot_longer(names_to = 'variable', values_to = 'value', -datetime) %>% 
  mutate(variable = factor(variable, levels=alltemp2007))

# ggplot(subset(buoy2010_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2010-03-01', tz=buoy_tz) &
#                         datetime < as.POSIXct('2010-04-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Mar 2010, semi clean',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# # Mar 07-08; Mar 22-23 consecutive readings without change
# ggplot(subset(buoy2010_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2010-03-21', tz=buoy_tz) &
#                         datetime < as.POSIXct('2010-03-23', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Mar 2010, semi clean',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# ggplot(subset(buoy2010_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2010-03-07 12:00', tz=buoy_tz) &
#                         datetime < as.POSIXct('2010-03-09 12:00', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Mar 2010, semi clean',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust

buoy2010_L1 <- buoy2010_L1 %>% 
  mutate_at(vars(all_of(alltemp2007)),
            ~(case_when(datetime >= as.POSIXct('2010-03-21', tz=buoy_tz) & 
                          datetime < as.POSIXct('2010-03-23', tz=buoy_tz) &
                          is.na(TempC_2p5m) ~ NA_real_,
                        datetime >= as.POSIXct('2010-03-08', tz= buoy_tz) &
                          datetime < as.POSIXct('2010-03-09 04:00', tz=buoy_tz) ~ NA_real_,
                         TRUE ~ .))) 

buoy2010_temp_vert_b <- buoy2010_L1 %>%
  select(datetime, all_of(alltemp2007)) %>%
  pivot_longer(names_to = 'variable', values_to = 'value', -datetime) %>% 
  mutate(variable = factor(variable, levels=alltemp2007))

# ggplot(subset(buoy2010_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2010-03-01', tz=buoy_tz) &
#                         datetime < as.POSIXct('2010-04-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Mar 2010, semi clean',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# ggplot(subset(buoy2010_temp_vert,
#               subset=(datetime >=as.POSIXct('2010-04-01', tz=buoy_tz) &
#                         datetime < as.POSIXct('2010-05-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Apr 2010, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   coord_cartesian(ylim=c(-5, 10)) +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_y_continuous(minor_breaks = c(-5:10)) +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# #buoy not functioning beginning apr 3 through 20 May
# ggplot(subset(buoy2010_temp_vert,
#               subset=(datetime >=as.POSIXct('2010-04-03', tz=buoy_tz) &
#                         datetime < as.POSIXct('2010-04-04', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Apr 2010, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   coord_cartesian(ylim=c(-5, 10)) +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# ggplot(subset(buoy2010_temp_vert,
#               subset=(datetime >=as.POSIXct('2010-05-20', tz=buoy_tz) &
#                         datetime < as.POSIXct('2010-05-21', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='May 2010, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust

buoy2010_L1 <- buoy2010_L1 %>% 
  mutate_at(vars(all_of(alltemp2007)),
            ~(case_when(datetime >= as.POSIXct('2010-04-01', tz=buoy_tz) & 
                          datetime < as.POSIXct('2010-05-01', tz=buoy_tz) & (. > 5 | . < 2.75) ~ NA_real_,
                           datetime >= as.POSIXct('2010-04-03 11:20', tz=buoy_tz) & 
                          datetime < as.POSIXct('2010-05-20 10:20', tz=buoy_tz) ~ NA_real_,
                           TRUE ~ .))) 

buoy2010_temp_vert_b <- buoy2010_L1 %>%
  select(datetime, all_of(alltemp2007)) %>%
  pivot_longer(names_to = 'variable', values_to = 'value', -datetime) %>% 
  mutate(variable = factor(variable, levels=alltemp2007))

#adding flags to data until thermisters replaced in Apr 2010
buoy2010_L1 <- buoy2010_L1 %>% 
  mutate(flag_alltemp = case_when(datetime<=as.POSIXct('2010-04-03', tz=buoy_tz) ~ 'i; s; x',
                               TRUE ~ ''))

# ggplot(subset(buoy2010_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2010-04-01', tz=buoy_tz) &
#                         datetime < as.POSIXct('2010-05-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Apr 2010, NAs recoded v2',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust

# ggplot(subset(buoy2010_temp_vert,
#               subset=(datetime >=as.POSIXct('2010-04-02', tz=buoy_tz) &
#                         datetime < as.POSIXct('2010-04-03', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Apr 2010, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust

buoy2010_L1 <- buoy2010_L1 %>% 
  mutate_at(vars(all_of(alltemp2007)),
            ~(case_when(datetime == as.POSIXct('2010-04-02 10:20', tz=buoy_tz) ~ NA_real_,
                        datetime >= as.POSIXct('2010-04-02 11:00', tz=buoy_tz) &
                          datetime < as.POSIXct('2010-04-02 17:00', tz=buoy_tz) ~ NA_real_,
                           TRUE ~ .))) 

buoy2010_temp_vert_b <- buoy2010_L1 %>%
  select(datetime, all_of(alltemp2007)) %>%
  pivot_longer(names_to = 'variable', values_to = 'value', -datetime) %>% 
  mutate(variable = factor(variable, levels=alltemp2007))

# ggplot(subset(buoy2010_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2010-04-01', tz=buoy_tz) &
#                         datetime < as.POSIXct('2010-05-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Apr 2010, semi clean',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# ggplot(subset(buoy2010_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2010-05-01', tz=buoy_tz) &
#                         datetime < as.POSIXct('2010-06-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='May 2010, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# #blip in 10-13m thermistors
# ggplot(subset(buoy2010_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2010-05-29', tz=buoy_tz) &
#                         datetime < as.POSIXct('2010-05-30', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='May 2010, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
buoy2010_L1 <- buoy2010_L1 %>% 
  mutate_at(vars(TempC_10m, TempC_11m, TempC_13m),
            ~(case_when(datetime == as.POSIXct('2010-05-29 16:30', tz=buoy_tz) ~ NA_real_,
                           TRUE ~ .))) 

buoy2010_temp_vert_b <- buoy2010_L1 %>%
  select(datetime, all_of(alltemp2007)) %>%
  pivot_longer(names_to = 'variable', values_to = 'value', -datetime) %>% 
  mutate(variable = factor(variable, levels=alltemp2007))

# ggplot(subset(buoy2010_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2010-05-29', tz=buoy_tz) &
#                         datetime < as.POSIXct('2010-05-30', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='May 2010, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# ggplot(subset(buoy2010_temp_vert,
#               subset=(datetime >=as.POSIXct('2010-06-01', tz=buoy_tz) &
#                         datetime < as.POSIXct('2010-07-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Jun 2010, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# temp blips Jun  3, 12, 17
# 
# ggplot(subset(buoy2010_temp_vert,
#               subset=(datetime >=as.POSIXct('2010-06-03', tz=buoy_tz) &
#                         datetime < as.POSIXct('2010-06-04', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Jun 2010, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# ggplot(subset(buoy2010_temp_vert,
#               subset=(datetime >=as.POSIXct('2010-06-12', tz=buoy_tz) &
#                         datetime < as.POSIXct('2010-06-13', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Jun 2010, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# ggplot(subset(buoy2010_temp_vert,
#               subset=(datetime >=as.POSIXct('2010-06-17', tz=buoy_tz) &
#                         datetime < as.POSIXct('2010-06-18', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Jun 2010, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# ggplot(subset(buoy2010_temp_vert,
#               subset=(datetime >=as.POSIXct('2010-06-22', tz=buoy_tz) &
#                         datetime < as.POSIXct('2010-06-23', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Jun 2010, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust

buoy2010_L1 <- buoy2010_L1 %>% 
  mutate_at(vars(TempC_10m, TempC_11m, TempC_13m),
            ~(case_when(datetime == as.POSIXct('2010-06-03 12:50', tz=buoy_tz) ~ NA_real_,
                           datetime == as.POSIXct('2010-06-12 15:00', tz=buoy_tz) ~ NA_real_,
                           datetime == as.POSIXct('2010-06-17 2:50', tz=buoy_tz) ~ NA_real_,
                           datetime == as.POSIXct('2010-06-23 6:00', tz=buoy_tz) ~ NA_real_,
                           TRUE ~ .))) 

buoy2010_temp_vert_b <- buoy2010_L1 %>%
  select(datetime, all_of(alltemp2007)) %>%
  pivot_longer(names_to = 'variable', values_to = 'value', -datetime) %>% 
  mutate(variable = factor(variable, levels=alltemp2007))

# ggplot(subset(buoy2010_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2010-06-01', tz=buoy_tz) &
#                         datetime < as.POSIXct('2010-07-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Jun 2010, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust

# ggplot(subset(buoy2010_temp_vert,
#               subset=(datetime >=as.POSIXct('2010-07-01', tz=buoy_tz) &
#                         datetime < as.POSIXct('2010-08-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Jul 2010, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust

# #Jul 2
# ggplot(subset(buoy2010_temp_vert,
#               subset=(datetime >=as.POSIXct('2010-07-02', tz=buoy_tz) &
#                         datetime < as.POSIXct('2010-07-03', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Jul 2010, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# #Jul 9
# ggplot(subset(buoy2010_temp_vert,
#               subset=(datetime >=as.POSIXct('2010-07-09', tz=buoy_tz) &
#                         datetime < as.POSIXct('2010-07-10', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Jul 2010, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# #jul 10 
# ggplot(subset(buoy2010_temp_vert,
#               subset=(datetime >=as.POSIXct('2010-07-10', tz=buoy_tz) &
#                         datetime < as.POSIXct('2010-07-11', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Jul 2010, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# #jul 14
# ggplot(subset(buoy2010_temp_vert,
#               subset=(datetime >=as.POSIXct('2010-07-14', tz=buoy_tz) &
#                         datetime < as.POSIXct('2010-07-15', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Jul 2010, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# #jul 15
# ggplot(subset(buoy2010_temp_vert,
#               subset=(datetime >=as.POSIXct('2010-07-15', tz=buoy_tz) &
#                         datetime < as.POSIXct('2010-07-16', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Jul 2010, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# #jul 23
# ggplot(subset(buoy2010_temp_vert,
#               subset=(datetime >=as.POSIXct('2010-07-23', tz=buoy_tz) &
#                         datetime < as.POSIXct('2010-07-24', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Jul 2010, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust

buoy2010_L1 <- buoy2010_L1 %>% 
  mutate_at(vars(all_of(alltemp2007)),
            ~(case_when(datetime == as.POSIXct('2010-07-09 7:30', tz=buoy_tz) ~ NA_real_,
                           datetime == as.POSIXct('2010-07-10 6:10', tz=buoy_tz) ~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate_at(vars(TempC_10m, TempC_11m, TempC_13m),
            ~(case_when(datetime == as.POSIXct('2010-07-02 4:00', tz=buoy_tz) ~ NA_real_,
                           datetime == as.POSIXct('2010-07-14 9:00', tz=buoy_tz) ~ NA_real_,
                           datetime == as.POSIXct('2010-07-15 22:10', tz=buoy_tz) ~ NA_real_,
                           datetime == as.POSIXct('2010-07-23 12:40', tz=buoy_tz) ~ NA_real_,
                           TRUE ~ .)))

buoy2010_temp_vert_b <- buoy2010_L1 %>%
  select(datetime, all_of(alltemp2007)) %>%
  pivot_longer(names_to = 'variable', values_to = 'value', -datetime) %>% 
  mutate(variable = factor(variable, levels=alltemp2007))

# ggplot(subset(buoy2010_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2010-07-01', tz=buoy_tz) &
#                         datetime < as.POSIXct('2010-08-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Jul 2010, clean',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust

# ggplot(subset(buoy2010_temp_vert,
#               subset=(datetime >=as.POSIXct('2010-08-01', tz=buoy_tz) &
#                         datetime < as.POSIXct('2010-09-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Aug 2010, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# #aug 04
# ggplot(subset(buoy2010_temp_vert,
#               subset=(datetime >=as.POSIXct('2010-08-04', tz=buoy_tz) &
#                         datetime < as.POSIXct('2010-08-05', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Aug 2010, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# #aug 07
# ggplot(subset(buoy2010_temp_vert,
#               subset=(datetime >=as.POSIXct('2010-08-07', tz=buoy_tz) &
#                         datetime < as.POSIXct('2010-08-08', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Aug 2010, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# # aug 14
# ggplot(subset(buoy2010_temp_vert,
#               subset=(datetime >=as.POSIXct('2010-08-14', tz=buoy_tz) &
#                         datetime < as.POSIXct('2010-08-15', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Aug 2010, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# # #buoy visit/ anamolous points aug 21
# ggplot(subset(buoy2010_temp_vert,
#               subset=(datetime >=as.POSIXct('2010-08-21', tz=buoy_tz) &
#                         datetime < as.POSIXct('2010-08-22', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Aug 2010, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# #aug 23
# ggplot(subset(buoy2010_temp_vert,
#               subset=(datetime >=as.POSIXct('2010-08-23', tz=buoy_tz) &
#                         datetime < as.POSIXct('2010-08-24', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Aug 2010, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# #aug 26
# ggplot(subset(buoy2010_temp_vert,
#               subset=(datetime >=as.POSIXct('2010-08-26', tz=buoy_tz) &
#                         datetime < as.POSIXct('2010-08-27', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Aug 2010, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# #aug 29
# ggplot(subset(buoy2010_temp_vert,
#               subset=(datetime >=as.POSIXct('2010-08-29', tz=buoy_tz) &
#                         datetime < as.POSIXct('2010-08-30', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Aug 2010, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust

buoy2010_L1 <- buoy2010_L1 %>% 
  mutate_at(vars(TempC_10m:TempC_13m),
            ~(case_when(datetime == as.POSIXct('2010-08-04 10:50', tz=buoy_tz) ~ NA_real_,
                           datetime == as.POSIXct('2010-08-07 17:20', tz=buoy_tz) ~ NA_real_,
                           datetime == as.POSIXct('2010-08-14 16:30', tz=buoy_tz) ~ NA_real_,
                           datetime == as.POSIXct('2010-08-14 20:30', tz=buoy_tz) ~ NA_real_,
                           datetime == as.POSIXct('2010-08-21 1:10', tz=buoy_tz) ~ NA_real_,
                           datetime == as.POSIXct('2010-08-23 6:20', tz=buoy_tz) ~ NA_real_,
                           datetime == as.POSIXct('2010-08-26 16:10', tz=buoy_tz) ~ NA_real_,
                           datetime == as.POSIXct('2010-08-29 4:10', tz=buoy_tz) ~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate_at(vars(all_of(alltemp2007)),
            ~(case_when(datetime == as.POSIXct('2010-08-21 12:40', tz=buoy_tz) ~ NA_real_,
                           datetime == as.POSIXct('2010-08-21 18:50', tz=buoy_tz) ~ NA_real_,
                           TRUE ~ .))) 

buoy2010_temp_vert_b <- buoy2010_L1 %>%
  select(datetime, all_of(alltemp2007)) %>%
  pivot_longer(names_to = 'variable', values_to = 'value', -datetime) %>% 
  mutate(variable = factor(variable, levels=alltemp2007))

# ggplot(subset(buoy2010_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2010-08-01', tz=buoy_tz) &
#                         datetime < as.POSIXct('2010-09-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Aug 2010, clean',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# ggplot(subset(buoy2010_temp_vert,
#               subset=(datetime >=as.POSIXct('2010-09-01', tz=buoy_tz) &
#                         datetime < as.POSIXct('2010-10-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Sept 2010, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# #aug 6
# ggplot(subset(buoy2010_temp_vert,
#               subset=(datetime >=as.POSIXct('2010-09-06', tz=buoy_tz) &
#                         datetime < as.POSIXct('2010-09-07', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Sept 2010, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# #aug 12
# ggplot(subset(buoy2010_temp_vert,
#               subset=(datetime >=as.POSIXct('2010-09-12', tz=buoy_tz) &
#                         datetime < as.POSIXct('2010-09-13', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Sept 2010, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# #aug 13
# ggplot(subset(buoy2010_temp_vert,
#               subset=(datetime >=as.POSIXct('2010-09-13', tz=buoy_tz) &
#                         datetime < as.POSIXct('2010-09-14', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Sept 2010, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# #AUG 15
# ggplot(subset(buoy2010_temp_vert,
#               subset=(datetime >=as.POSIXct('2010-09-15', tz=buoy_tz) &
#                         datetime < as.POSIXct('2010-09-16', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Sept 2010, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# #AUG 21
# ggplot(subset(buoy2010_temp_vert,
#               subset=(datetime >=as.POSIXct('2010-09-21', tz=buoy_tz) &
#                         datetime < as.POSIXct('2010-09-22', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Sept 2010, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust

buoy2010_L1 <- buoy2010_L1 %>% 
  mutate_at(vars(TempC_10m:TempC_13m),
            ~(case_when(datetime == as.POSIXct('2010-09-06 15:20', tz=buoy_tz) ~ NA_real_,
                           datetime == as.POSIXct('2010-09-06 20:50', tz=buoy_tz) ~ NA_real_,
                           datetime == as.POSIXct('2010-09-12 11:10', tz=buoy_tz) ~ NA_real_,
                           datetime == as.POSIXct('2010-09-13 20:10', tz=buoy_tz) ~ NA_real_,
                           datetime == as.POSIXct('2010-09-15 8:10', tz=buoy_tz) ~ NA_real_,
                           datetime == as.POSIXct('2010-09-21 11:50', tz=buoy_tz) ~ NA_real_,
                           TRUE ~ .))) 

buoy2010_temp_vert_b <- buoy2010_L1 %>%
  select(datetime, all_of(alltemp2007)) %>%
  pivot_longer(names_to = 'variable', values_to = 'value', -datetime) %>% 
  mutate(variable = factor(variable, levels=alltemp2007))

# ggplot(subset(buoy2010_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2010-09-01', tz=buoy_tz) &
#                         datetime < as.POSIXct('2010-10-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Sept 2010, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# ggplot(subset(buoy2010_temp_vert,
#               subset=(datetime >=as.POSIXct('2010-09-30', tz=buoy_tz) &
#                         datetime < as.POSIXct('2010-10-02', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Oct 2010, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# ggplot(subset(buoy2010_temp_vert,
#               subset=(datetime >=as.POSIXct('2010-10-01', tz=buoy_tz) &
#                         datetime < as.POSIXct('2010-11-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Oct 2010, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# #anamolous points on Oct2 and buoy to harbor Oct 25
# ggplot(subset(buoy2010_temp_vert,
#               subset=(datetime >=as.POSIXct('2010-10-02', tz=buoy_tz) &
#                         datetime < as.POSIXct('2010-10-03', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Oct 2010, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# ggplot(subset(buoy2010_temp_vert,
#               subset=(datetime >=as.POSIXct('2010-10-25', tz=buoy_tz) &
#                         datetime < as.POSIXct('2010-10-26', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Oct 2010, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust

buoy2010_L1 <- buoy2010_L1 %>% 
  mutate_at(vars(all_of(alltemp2007)),
            ~(case_when(datetime == as.POSIXct('2010-10-02 15:10', tz=buoy_tz) ~ NA_real_,
                           datetime >= as.POSIXct('2010-10-25 11:40', tz=buoy_tz) & 
                          datetime < as.POSIXct('2011-01-01') ~ NA_real_, #buoy back in water in harbor for a bit, but temp would not be applicable
                           TRUE ~ .)))

buoy2010_temp_vert_b <- buoy2010_L1 %>%
  select(datetime, all_of(alltemp2007)) %>%
  pivot_longer(names_to = 'variable', values_to = 'value', -datetime) %>% 
  mutate(variable = factor(variable, levels=alltemp2007))
# 
# ggplot(subset(buoy2010_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2010-10-01', tz=buoy_tz) &
#                         datetime < as.POSIXct('2010-11-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Oct 2010, clean',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust

# #data gap thru Nov, TESTING IN DEC
# 
# ggplot(subset(buoy2010_temp_vert,
#               subset=(datetime >=as.POSIXct('2010-12-01', tz=buoy_tz) &
#                         datetime < as.POSIXct('2011-01-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Dec 2010, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust

buoy2010_L1 <- buoy2010_L1 %>% 
  mutate_at(vars(all_of(alltemp2007)),
            ~(case_when(datetime >= as.POSIXct('2010-12-01', tz=buoy_tz) ~ NA_real_,
                           TRUE ~ .))) 

buoy2010_temp_vert_b <- buoy2010_L1 %>%
  select(datetime, all_of(alltemp2007)) %>%
  pivot_longer(names_to = 'variable', values_to = 'value', -datetime) %>% 
  mutate(variable = factor(variable, levels=alltemp2007))

ggplot(buoy2010_temp_vert_b,
       aes(x=datetime, y=value, color=as.factor(variable))) +
  geom_point() +
  final_theme +
  labs(title='2010, clean',
       x=NULL,
       y='temp deg C') +
  scale_x_datetime(date_minor_breaks = '1 month') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
                              "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust

#add flag for 11.5 and 13.5 as possibly in sediment
buoy2010_L1 <- buoy2010_L1 %>% 
  mutate(flag_temp11p5m = 'b',
         flag_temp13p5m = 'b',
         flag_temp1m = 's; x')

#add in offline location
buoy2010_L1 <- buoy2010_L1 %>% 
  mutate(location = case_when(datetime >= as.POSIXct('2010-04-21 7:00', tz=buoy_tz) & 
                                datetime < as.POSIXct('2010-05-16 15:40', tz=buoy_tz) ~ 'offline',
                              datetime >= as.POSIXct('2010-10-29 14:00', tz=buoy_tz) &
                                datetime < as.POSIXct('2010-12-14 11:40', tz=buoy_tz) ~ 'offline',   
                              TRUE ~ location)) %>% 
  mutate_at(vars(flag_alltemp, flag_temp1m, flag_temp11p5m, flag_temp13p5m),
            ~case_when(location == 'offline' ~ '',
                               TRUE~.))

#clean up workspace
rm(buoy2010_temp_vert, buoy2010_temp_vert_b)

#correct thermistor depth for offset; add CV
buoy2010_L1 <- buoy2010_L1 %>% 
  rename(waterTemperature_degC_13p5m = TempC_13m,
         waterTemperature_degC_11p5m = TempC_11m,
         waterTemperature_degC_10p5m = TempC_10m,
         waterTemperature_degC_9p5m = TempC_9m,
         waterTemperature_degC_8p5m = TempC_8m,
         waterTemperature_degC_7p5m = TempC_7m,
         waterTemperature_degC_6p5m = TempC_6m,
         waterTemperature_degC_5p5m = TempC_5m,
         waterTemperature_degC_4p5m = TempC_4m,
         waterTemperature_degC_3p5m = TempC_3m,
         waterTemperature_degC_3m = TempC_2p5m,
         waterTemperature_degC_2p5m = TempC_2m,
         waterTemperature_degC_2m = TempC_1p5m,
         waterTemperature_degC_1p5m = TempC_1m,
         waterTemperature_degC_1m = TempC_0p5m,
         waterTemperature_degC_0p5m = TempC_0m)


#### DO sensors ####
range(buoy2010_L1$DOSat, na.rm=T)
range(buoy2010_L1$DOppm, na.rm=T)
range(buoy2010_L1$DOTempC, na.rm=T)

buoy2010_L1 <- buoy2010_L1 %>% 
  mutate(DOTempC = case_when(DOTempC == -6999 ~ NA_real_,
                             TRUE ~ DOTempC))

do_vert <- buoy2010_L1 %>% 
  select(datetime, DOSat, DOppm, DOTempC) %>% 
  gather(variable, value, -datetime)

ggplot(do_vert, aes(x = datetime, y = value)) +
  geom_point() +
  facet_grid(variable~., scales = 'free_y') +
  final_theme +
  labs(title = '2010 DO data raw',
       x = NULL,
       y = NULL) +
  scale_x_datetime(date_minor_breaks = '1 month')


# ggplot(subset(do_vert,
#               subset = (datetime >= as.POSIXct('2010-01-01', tz=buoy_tz) &
#                           datetime < as.POSIXct('2010-02-01', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'jan 2010 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(do_vert,
#               subset = (datetime >= as.POSIXct('2010-02-01', tz=buoy_tz) &
#                           datetime < as.POSIXct('2010-03-01', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'feb 2010 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(do_vert,
#               subset = (datetime >= as.POSIXct('2010-03-01', tz=buoy_tz) &
#                           datetime < as.POSIXct('2010-04-01', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'mar 2010 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(do_vert,
#               subset = (datetime >= as.POSIXct('2010-04-01', tz=buoy_tz) &
#                           datetime < as.POSIXct('2010-05-01', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'apr 2010 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# #buoy out of water beginning apr 21, errant data begins 17th
# ggplot(subset(do_vert,
#               subset = (datetime >= as.POSIXct('2010-04-17', tz=buoy_tz) &
#                           datetime < as.POSIXct('2010-04-18', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'apr 2010 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 hour')

buoy2010_L1 <- buoy2010_L1 %>% 
  mutate_at(vars(DOSat, DOppm, DOTempC),
            ~(case_when(datetime >= as.POSIXct('2010-04-17 16:10', tz=buoy_tz) &
                             datetime < as.POSIXct('2010-04-22', tz=buoy_tz) ~ NA_real_,
                           TRUE ~ .)))
do_vert_b <- buoy2010_L1 %>% 
  select(datetime, DOSat, DOppm, DOTempC) %>% 
  gather(variable, value, -datetime)

# ggplot(subset(do_vert_b,
#               subset = (datetime >= as.POSIXct('2010-04-01', tz=buoy_tz) &
#                           datetime < as.POSIXct('2010-05-01', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'apr 2010 DO data clean',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(do_vert,
#               subset = (datetime >= as.POSIXct('2010-05-01', tz=buoy_tz) &
#                           datetime < as.POSIXct('2010-06-01', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'may 2010 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# #sensors working may 16, buoy to loon 20th, sensors likely out of water until thien
# ggplot(subset(do_vert,
#               subset = (datetime >= as.POSIXct('2010-05-20', tz=buoy_tz) &
#                           datetime < as.POSIXct('2010-05-21', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'may 2010 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 hour')

buoy2010_L1 <- buoy2010_L1 %>% 
  mutate_at(vars(DOSat, DOppm, DOTempC),
            ~(case_when(datetime >= as.POSIXct('2010-05-16', tz=buoy_tz) &
                             datetime < as.POSIXct('2010-05-20 10:20', tz=buoy_tz) ~ NA_real_,
                           TRUE ~ .)))
do_vert_b <- buoy2010_L1 %>% 
  select(datetime, DOSat, DOppm, DOTempC) %>% 
  gather(variable, value, -datetime)

# ggplot(subset(do_vert_b,
#               subset = (datetime >= as.POSIXct('2010-05-01', tz=buoy_tz) &
#                           datetime < as.POSIXct('2010-06-01', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'may 2010 DO data clean',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(do_vert,
#               subset = (datetime >= as.POSIXct('2010-06-01', tz=buoy_tz) &
#                           datetime < as.POSIXct('2010-07-01', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'jun 2010 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(do_vert,
#               subset = (datetime >= as.POSIXct('2010-07-01', tz=buoy_tz) &
#                           datetime < as.POSIXct('2010-08-01', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'jul 2010 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')

# ggplot(subset(do_vert,
#               subset = (datetime >= as.POSIXct('2010-08-01', tz=buoy_tz) &
#                           datetime < as.POSIXct('2010-09-01', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'aug 2010 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')


# #add flag for presumed cleaning Aug 17
# ggplot(subset(do_vert,
#               subset = (datetime >= as.POSIXct('2010-08-17', tz=buoy_tz) &
#                           datetime < as.POSIXct('2010-08-18', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'aug 2010 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 hour')

buoy2010_L1 <- buoy2010_L1 %>% 
  mutate_at(vars(DOSat, DOppm, DOTempC),
            ~(case_when(datetime == as.POSIXct('2010-08-17 13:30', tz=buoy_tz) ~ NA_real_,
                        TRUE ~ .))) %>% 
  mutate(flag_do1p5m = case_when(datetime == as.POSIXct('2010-08-17 13:30', tz=buoy_tz) ~ 'wp',
                                 TRUE ~ ''))
do_vert_b <- buoy2010_L1 %>% 
  select(datetime, DOSat, DOppm, DOTempC) %>% 
  gather(variable, value, -datetime)

# ggplot(subset(do_vert_b,
#               subset = (datetime >= as.POSIXct('2010-08-17', tz=buoy_tz) &
#                           datetime < as.POSIXct('2010-08-18', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'aug 2010 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 hour')
# 
# ggplot(subset(do_vert,
#               subset = (datetime >= as.POSIXct('2010-09-01', tz=buoy_tz) &
#                           datetime < as.POSIXct('2010-10-01', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'sept 2010 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(do_vert,
#               subset = (datetime >= as.POSIXct('2010-10-01', tz=buoy_tz) &
#                           datetime < as.POSIXct('2010-11-01', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'oct 2010 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# #Oct 25 buoy moved to harbor
# ggplot(subset(do_vert,
#               subset = (datetime >= as.POSIXct('2010-10-25', tz=buoy_tz) &
#                           datetime < as.POSIXct('2010-10-26', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'oct 2010 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 hour')

buoy2010_L1 <- buoy2010_L1 %>% 
  mutate_at(vars(DOSat, DOppm, DOTempC),
            ~(case_when(datetime >= as.POSIXct('2010-10-25 11:30', tz=buoy_tz) &
                             datetime < as.POSIXct('2010-10-25 12:50', tz=buoy_tz) ~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate_at(vars(DOSat, DOppm),
          ~(case_when(datetime >= as.POSIXct('2010-10-25 11:50', tz=buoy_tz) &
                           datetime < as.POSIXct('2010-10-25 17:00', tz=buoy_tz) ~ NA_real_,
                         TRUE ~ .))) %>% 
  mutate(location = case_when(datetime >= as.POSIXct('2010-10-25 11:30', tz=buoy_tz) &
                                datetime < as.POSIXct('2010-10-25 12:50', tz=buoy_tz) ~ 'in transit',
                              datetime >= as.POSIXct('2010-10-25 12:50', tz=buoy_tz) ~ 'harbor',
                              TRUE ~ location))
do_vert_b <- buoy2010_L1 %>% 
  select(datetime, DOSat, DOppm, DOTempC, location) %>% 
  gather(variable, value, -datetime, -location)

# ggplot(subset(do_vert_b,
#               subset = (datetime >= as.POSIXct('2010-10-01', tz=buoy_tz) &
#                           datetime < as.POSIXct('2010-11-01', tz=buoy_tz))),
#        aes(x = datetime, y = value, color = location)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'oct 2010 DO data clean',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(do_vert,
#               subset = (datetime >= as.POSIXct('2010-11-01', tz=buoy_tz) &
#                           datetime < as.POSIXct('2010-12-01', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'nov 2010 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(do_vert,
#               subset = (datetime >= as.POSIXct('2010-12-01', tz=buoy_tz) &
#                           datetime < as.POSIXct('2011-01-01', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'dec 2010 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')

#10 days of data - recode to NA- unsure if sensors in/out of water
buoy2010_L1 <- buoy2010_L1 %>%
  mutate_at(vars(DOSat, DOppm, DOTempC),
            ~(case_when(datetime >= as.POSIXct('2010-12-14', tz=buoy_tz) &
                             datetime < as.POSIXct('2010-12-24', tz=buoy_tz) ~ NA_real_,
                           TRUE ~ .)))
do_vert_b <- buoy2010_L1 %>%
  select(datetime, DOSat, DOppm, DOTempC, location) %>%
  gather(variable, value, -datetime, -location)

# ggplot(subset(do_vert_b,
#               subset = (datetime >= as.POSIXct('2010-12-01', tz=buoy_tz) &
#                           datetime < as.POSIXct('2011-01-01', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'dec 2010 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')

# add offline locations
buoy2010_L1 <- buoy2010_L1 %>% 
  mutate(location = case_when(datetime >= as.POSIXct('2010-10-29 13:00', tz=buoy_tz) ~ 'offline',
                              TRUE ~ location))

#add presumed cleanings 
buoy2010_L1 <- buoy2010_L1 %>% 
  mutate(flag_do1p5m = case_when(datetime == as.POSIXct('2010-05-20 10:20', tz=buoy_tz) ~ 'wp',
                              TRUE ~ flag_do1p5m)) %>% 
  mutate(flag_do1p5m = case_when(flag_do1p5m == '' ~ 'x',
                                 TRUE ~ paste('x', flag_do1p5m, sep = '; ')))
unique(buoy2010_L1$flag_do1p5m)

do_vert_b <- buoy2010_L1 %>% 
  select(datetime, DOSat, DOppm, DOTempC, location, flag_do1p5m) %>% 
  gather(variable, value, -datetime, -location, - flag_do1p5m)


ggplot(do_vert_b, aes(x = datetime, y = value, color = location, shape = flag_do1p5m)) +
  geom_point() +
  facet_grid(variable~., scales = 'free_y') +
  final_theme +
  labs(title = '2010 DO data NA values recoded',
       x = NULL,
       y = NULL) +
  scale_x_datetime(date_minor_breaks = '1 month')

#clean up workspace
rm(do_vert, do_vert_b)

#rename with CV
buoy2010_L1 <- buoy2010_L1 %>% 
  rename(oxygenDissolved_mgl_1p5m = DOppm,
         oxygenDissolvedPercentOfSaturation_pct_1p5m = DOSat,
         waterTemperature_DO_degC_1p5m = DOTempC)

#### wind sensors ####
range(buoy2010_L1$InstWindDir, na.rm = T)
range(buoy2010_L1$InstWindSp, na.rm = T)
range(buoy2010_L1$AveWindDir, na.rm = T)
range(buoy2010_L1$AveWindSp, na.rm = T)

#recode na strings
buoy2010_L1 <- buoy2010_L1 %>% 
  mutate(InstWindDir = case_when(InstWindDir==-6999 ~ NA_real_,
                                 TRUE ~ InstWindDir))

#recode when offline or in transit
buoy2010_L1 <- buoy2010_L1 %>% 
  mutate_at(vars(InstWindDir, AveWindDir,InstWindSp, AveWindSp),
            ~ case_when(location == 'offline' | location == 'in transit' ~ NA_real_,
                        TRUE ~.))
wind_vert <- buoy2010_L1 %>% 
  select(datetime, InstWindDir, AveWindDir,InstWindSp, AveWindSp) %>% 
  gather(variable, value, -datetime)

ggplot(wind_vert, aes(x = datetime, y = value)) +
  geom_point() +
  facet_grid(variable~., scales = 'free_y') +
  final_theme +
  labs(title = '2010 wind data NAs recoded',
       x = NULL,
       y = NULL) +
  scale_x_datetime(date_minor_breaks = '1 month')

# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2010-01-01', tz=buoy_tz) &
#                         datetime<as.POSIXct('2010-02-01', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'jan 2010 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# #frozen sensor on Jan 2
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2010-01-02', tz=buoy_tz) &
#                         datetime<as.POSIXct('2010-01-03', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'jan 2010 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 hour')
# 
# #frozen sensor jan 20
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2010-01-20', tz=buoy_tz) &
#                         datetime<as.POSIXct('2010-01-21', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'jan 2010 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 hour')

buoy2010_L1 <- buoy2010_L1 %>% 
  mutate_at(vars(InstWindDir, AveWindDir,InstWindSp, AveWindSp),
            ~(case_when(datetime>=as.POSIXct('2010-01-02 3:50', tz=buoy_tz) &
                             datetime<as.POSIXct('2010-01-02 11:30', tz=buoy_tz)~ NA_real_,
                           datetime>=as.POSIXct('2010-01-20 2:10', tz=buoy_tz) &
                             datetime<as.POSIXct('2010-01-20 7:30', tz=buoy_tz)~ NA_real_,
                           TRUE ~ .)))

wind_vert_b <- buoy2010_L1 %>% 
  select(datetime, InstWindDir, AveWindDir,InstWindSp, AveWindSp) %>% 
  gather(variable, value, -datetime)

# ggplot(subset(wind_vert_b,
#               subset=(datetime>=as.POSIXct('2010-01-01', tz=buoy_tz) &
#                         datetime<as.POSIXct('2010-02-01', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'jan 2010 wind data clean',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2010-02-01', tz=buoy_tz) &
#                         datetime<as.POSIXct('2010-03-01', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'feb 2010 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# #frozen on feb 16
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2010-02-16', tz=buoy_tz) &
#                         datetime<as.POSIXct('2010-02-17', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'feb 2010 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 hour')

buoy2010_L1 <- buoy2010_L1 %>% 
  mutate_at(vars(InstWindDir, AveWindDir,InstWindSp, AveWindSp),
            ~(case_when(datetime>=as.POSIXct('2010-02-16 18:00', tz=buoy_tz) &
                             datetime<as.POSIXct('2010-02-16 21:10', tz=buoy_tz)~ NA_real_,
                           TRUE ~ .)))

wind_vert_b <- buoy2010_L1 %>% 
  select(datetime, InstWindDir, AveWindDir,InstWindSp, AveWindSp) %>% 
  gather(variable, value, -datetime)

# ggplot(subset(wind_vert_b,
#               subset=(datetime>=as.POSIXct('2010-02-01', tz=buoy_tz) &
#                         datetime<as.POSIXct('2010-03-01', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'feb 2010 wind data clean',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2010-03-01', tz=buoy_tz) &
#                         datetime<as.POSIXct('2010-04-01', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'mar 2010 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
ggplot(subset(wind_vert,
              subset=(datetime>=as.POSIXct('2010-03-25', tz=buoy_tz) &
                        datetime<as.POSIXct('2010-03-26', tz=buoy_tz))),
       aes(x = datetime, y = value)) +
  geom_point() +
  facet_grid(variable~., scales = 'free_y') +
  final_theme +
  labs(title = 'mar 2010 wind data NAs recoded',
       x = NULL,
       y = NULL) +
  scale_x_datetime(date_minor_breaks = '1 hour')
# 
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2010-04-01', tz=buoy_tz) &
#                         datetime<as.POSIXct('2010-05-01', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'apr 2010 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2010-05-01', tz=buoy_tz) &
#                         datetime<as.POSIXct('2010-06-01', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'may 2010 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# #wind data looks odd until mid day may 20
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2010-05-20', tz=buoy_tz) &
#                         datetime<as.POSIXct('2010-05-21', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'may 2010 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 hour')

#mar 25 sensor offline/frozen until redeployment in May
buoy2010_L1 <- buoy2010_L1 %>% 
  mutate_at(vars(InstWindDir, AveWindDir,InstWindSp, AveWindSp),
            ~(case_when(datetime>=as.POSIXct('2010-03-25 14:00', tz=buoy_tz) &
                             datetime<as.POSIXct('2010-05-20 09:00', tz=buoy_tz)~ NA_real_,
                           TRUE ~ .)))

wind_vert_b <- buoy2010_L1 %>% 
  select(datetime, InstWindDir, AveWindDir,InstWindSp, AveWindSp) %>% 
  gather(variable, value, -datetime)

# ggplot(subset(wind_vert_b,
#               subset=(datetime>=as.POSIXct('2010-05-01', tz=buoy_tz) &
#                         datetime<as.POSIXct('2010-06-01', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'may 2010 wind data clean',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2010-06-01', tz=buoy_tz) &
#                         datetime<as.POSIXct('2010-07-01', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'june 2010 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2010-07-01', tz=buoy_tz) &
#                         datetime<as.POSIXct('2010-08-01', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'jul 2010 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2010-08-01', tz=buoy_tz) &
#                         datetime<as.POSIXct('2010-09-01', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'aug 2010 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2010-09-01', tz=buoy_tz) &
#                         datetime<as.POSIXct('2010-10-01', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'sept 2010 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2010-10-01', tz=buoy_tz) &
#                         datetime<as.POSIXct('2010-11-01', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'oct 2010 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# #buoy moved to harbor oct25
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2010-10-25', tz=buoy_tz) &
#                         datetime<as.POSIXct('2010-10-26', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'oct 2010 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#     scale_x_datetime(date_minor_breaks = '1 hour')
# 
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2010-11-01', tz=buoy_tz) &
#                         datetime<as.POSIXct('2010-12-01', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'nov 2010 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2010-12-01', tz=buoy_tz) &
#                         datetime<as.POSIXct('2011-01-01', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'dec 2010 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')

#remove data in dec - incomplete
buoy2010_L1 <- buoy2010_L1 %>% 
  mutate_at(vars(InstWindDir, AveWindDir,InstWindSp, AveWindSp),
            ~(case_when(datetime>=as.POSIXct('2010-12-14', tz=buoy_tz) &
                             datetime<as.POSIXct('2010-12-24', tz=buoy_tz)~ NA_real_,
                           TRUE ~ .)))

wind_vert_b <- buoy2010_L1 %>% 
  select(datetime, location, InstWindDir, AveWindDir,InstWindSp, AveWindSp) %>% 
  gather(variable, value, -datetime, -location)

ggplot(wind_vert_b,
       aes(x = datetime, y = value, color = location)) +
  geom_point() +
  facet_grid(variable~., scales = 'free_y') +
  final_theme +
  labs(title = '2010 wind data clean',
       x = NULL,
       y = NULL) +
  scale_x_datetime(date_minor_breaks = '1 month')

#clean up workspace
rm(wind_vert, wind_vert_b)

buoy2010_L1 <- buoy2010_L1 %>% 
  rename(windDirectionInstantaneous_deg = InstWindDir,
         windSpeedInstantaneous_mps = InstWindSp,
         windDirectionAverage_deg = AveWindDir, 
         windSpeedAverage_mps = AveWindSp)

#### PAR sensors ####
range(buoy2010_L1$PAR, na.rm = T)

# ggplot(buoy2010_L1, aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme

#flag and recode values below zero
buoy2010_L1 <-  buoy2010_L1 %>%
  mutate(flag_par = case_when(PAR <0 ~ 'z',
                              TRUE ~ '')) %>% 
  mutate(PAR = case_when(PAR <0 ~ 0,
                         TRUE ~ PAR))
#recode when offline or in transit
buoy2010_L1 <- buoy2010_L1 %>% 
  mutate(PAR = case_when(location == 'offline' | location == 'in transit' ~ NA_real_, 
                         TRUE ~ PAR))

ggplot(buoy2010_L1, aes(x = datetime, y = PAR)) +
  geom_point() +
  final_theme +
  labs(title = '2010 PAR data below 0 recoded',
       x = NULL,
       y = 'PAR (umol/m2/s)') +
  scale_x_datetime(date_minor_breaks = '1 month')

# ggplot(subset(buoy2010_L1,
#               subset = (datetime>=as.POSIXct('2010-01-01', tz=buoy_tz) &
#                           datetime<as.POSIXct('2010-02-01', tz=buoy_tz))),
#        aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'jan 2010 PAR data raw',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2010_L1,
#               subset = (datetime>=as.POSIXct('2010-02-01', tz=buoy_tz) &
#                           datetime<as.POSIXct('2010-03-01', tz=buoy_tz))),
#        aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'feb 2010 PAR data raw',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2010_L1,
#               subset = (datetime>=as.POSIXct('2010-03-01', tz=buoy_tz) &
#                           datetime<as.POSIXct('2010-04-01', tz=buoy_tz))),
#        aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'mar 2010 PAR data raw',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2010_L1,
#               subset = (datetime>=as.POSIXct('2010-04-01', tz=buoy_tz) &
#                           datetime<as.POSIXct('2010-05-01', tz=buoy_tz))),
#        aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'apr 2010 PAR data raw',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2010_L1,
#               subset = (datetime>=as.POSIXct('2010-05-01', tz=buoy_tz) &
#                           datetime<as.POSIXct('2010-06-01', tz=buoy_tz))),
#        aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'may 2010 PAR data raw',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2010_L1,
#               subset = (datetime>=as.POSIXct('2010-05-20', tz=buoy_tz) &
#                           datetime<as.POSIXct('2010-05-21', tz=buoy_tz))),
#        aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'may 2010 PAR data raw',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 hour')

#incomplete data until 20th may / data in may prior recoded to NA
buoy2010_L1 <-  buoy2010_L1 %>%
  mutate(PAR = case_when(datetime>=as.POSIXct('2010-05-01', tz=buoy_tz) &
                           datetime<as.POSIXct('2010-05-20 09:00', tz=buoy_tz) ~ NA_real_,
                         TRUE ~ PAR))

# ggplot(subset(buoy2010_L1,
#               subset = (datetime>=as.POSIXct('2010-05-01', tz=buoy_tz) &
#                           datetime<as.POSIXct('2010-06-01', tz=buoy_tz))),
#        aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'may 2010 PAR data clean',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2010_L1,
#               subset = (datetime>=as.POSIXct('2010-06-01', tz=buoy_tz) &
#                           datetime<as.POSIXct('2010-07-01', tz=buoy_tz))),
#        aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'jun 2010 PAR data raw',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2010_L1,
#               subset = (datetime>=as.POSIXct('2010-07-01', tz=buoy_tz) &
#                           datetime<as.POSIXct('2010-08-01', tz=buoy_tz))),
#        aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'jul 2010 PAR data raw',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2010_L1,
#               subset = (datetime>=as.POSIXct('2010-08-01', tz=buoy_tz) &
#                           datetime<as.POSIXct('2010-09-01', tz=buoy_tz))),
#        aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'aug 2010 PAR data raw',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2010_L1,
#               subset = (datetime>=as.POSIXct('2010-09-01', tz=buoy_tz) &
#                           datetime<as.POSIXct('2010-10-01', tz=buoy_tz))),
#        aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'sept 2010 PAR data raw',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2010_L1,
#               subset = (datetime>=as.POSIXct('2010-10-01', tz=buoy_tz) &
#                           datetime<as.POSIXct('2010-11-01', tz=buoy_tz))),
#        aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'oct 2010 PAR data raw',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# #buoy moved to harbor oct 25
# ggplot(subset(buoy2010_L1,
#               subset = (datetime>=as.POSIXct('2010-10-25', tz=buoy_tz) &
#                           datetime<as.POSIXct('2010-10-26', tz=buoy_tz))),
#        aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'oct 2010 PAR data raw',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2010_L1,
#               subset = (datetime>=as.POSIXct('2010-11-01', tz=buoy_tz) &
#                           datetime<as.POSIXct('2010-12-01', tz=buoy_tz))),
#        aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'nov 2010 PAR data raw',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2010_L1,
#               subset = (datetime>=as.POSIXct('2010-12-01', tz=buoy_tz) &
#                           datetime<as.POSIXct('2011-01-01', tz=buoy_tz))),
#        aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'dec 2010 PAR data raw',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')

#par in dec errant - removing
buoy2010_L1 <-  buoy2010_L1 %>%
  mutate(PAR = case_when(datetime>=as.POSIXct('2010-12-14', tz=buoy_tz) &
                           datetime<as.POSIXct('2010-12-24', tz=buoy_tz) ~ NA_real_,
                         TRUE ~ PAR))

ggplot(buoy2010_L1,
       aes(x = datetime, y = PAR, color = location)) +
  geom_point() +
  final_theme +
  labs(title = '2010 PAR data clean',
       x = NULL,
       y = 'PAR (umol/m2/s)') +
  scale_x_datetime(date_minor_breaks = '1 month')

#rename with CV
buoy2010_L1 <- buoy2010_L1 %>% 
  rename(radiationIncomingPAR_umolm2s = PAR)



#### Air Temp ####
range(buoy2010_L1$AirTempC, na.rm = T)
#recode na strings
buoy2010_L1 <- buoy2010_L1 %>% 
  mutate(AirTempC = case_when(AirTempC == -6999 ~ NA_real_,
                              TRUE ~ AirTempC))
#recode when in transit or offline
buoy2010_L1 <- buoy2010_L1 %>% 
  mutate(AirTempC = case_when(location == 'in transit' | location == 'offline' ~ NA_real_, 
                              TRUE ~ AirTempC))

ggplot(buoy2010_L1, aes(x=datetime, y = AirTempC)) +
  geom_point() +
  final_theme +
  labs(title = '2010 air temp NAs recoded',
       x= NULL,
       y= 'air temp (deg C)') +
  scale_x_datetime(date_minor_breaks = '1 month')

#everything is good except late march and end of 2012

# ggplot(subset(buoy2010_L1,
#               subset=(datetime>=as.POSIXct('2010-03-01', tz=buoy_tz) &
#                         datetime < as.POSIXct('2010-04-01', tz=buoy_tz))),
#        aes(x=datetime, y = AirTempC)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'mar 2010 air temp raw',
#        x= NULL,
#        y= 'air temp (deg C)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2010_L1,
#               subset=(datetime>=as.POSIXct('2010-03-25', tz=buoy_tz) &
#                         datetime < as.POSIXct('2010-03-26', tz=buoy_tz))),
#        aes(x=datetime, y = AirTempC)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'mar 2010 air temp raw',
#        x= NULL,
#        y= 'air temp (deg C)') +
#   scale_x_datetime(date_minor_breaks = '1 hour')
# 
# ggplot(subset(buoy2010_L1,
#               subset=(datetime>=as.POSIXct('2010-03-28', tz=buoy_tz) &
#                         datetime < as.POSIXct('2010-03-29', tz=buoy_tz))),
#        aes(x=datetime, y = AirTempC)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'mar 2010 air temp raw',
#        x= NULL,
#        y= 'air temp (deg C)') +
#   scale_x_datetime(date_minor_breaks = '1 hour')

buoy2010_L1 <- buoy2010_L1 %>% 
  mutate(AirTempC = case_when(datetime >= as.POSIXct('2010-03-25 13:30', tz=buoy_tz) &
                                datetime < as.POSIXct('2010-03-29 20:30', tz=buoy_tz)~ NA_real_,
                              datetime >= as.POSIXct('2010-12-01', tz=buoy_tz) &
                                datetime < as.POSIXct('2010-12-31', tz=buoy_tz) ~ NA_real_,
                              TRUE ~ AirTempC))

# ggplot(subset(buoy2010_L1,
#               subset=(datetime>=as.POSIXct('2010-03-01', tz=buoy_tz) &
#                         datetime < as.POSIXct('2010-04-01', tz=buoy_tz))),
#        aes(x=datetime, y = AirTempC)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'mar 2010 air temp clean',
#        x= NULL,
#        y= 'air temp (deg C)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2010_L1,
#               subset=(datetime>=as.POSIXct('2010-04-01', tz=buoy_tz) &
#                         datetime < as.POSIXct('2010-05-01', tz=buoy_tz))),
#        aes(x=datetime, y = AirTempC)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'apr 2010 air temp raw',
#        x= NULL,
#        y= 'air temp (deg C)') +
#   scale_x_datetime(date_minor_breaks = '1 day')

ggplot(buoy2010_L1, aes(x=datetime, y = AirTempC, color = location)) +
  geom_point() +
  final_theme +
  labs(title = '2010 air temp clean',
       x= NULL,
       y= 'air temp (deg C)') +
  scale_x_datetime(date_minor_breaks = '1 month')

#rename with CV
buoy2010_L1 <- buoy2010_L1 %>% 
  rename(airTemperature_degC = AirTempC)


#### EXPORT L1 FILES ####
colnames(buoy2010_L1)

#export L1 tempstring file
buoy2010_L1 %>%
  select(datetime, location, waterTemperature_degC_0p5m:waterTemperature_degC_13p5m, 
         flag_alltemp, flag_temp11p5m, flag_temp13p5m) %>%
  mutate(datetime = as.character(datetime)) %>%
  write_csv(., file.path(dump_dir,'tempstring/2010_tempstring_L1_v2022.csv'))

# export L1 do file
buoy2010_L1 %>%
  select(datetime, location, 
         oxygenDissolved_mgl_1p5m, oxygenDissolvedPercentOfSaturation_pct_1p5m, waterTemperature_DO_degC_1p5m, 
         flag_do1p5m) %>%
  mutate(datetime = as.character(datetime)) %>%
  write_csv(., file.path(dump_dir, 'do/2010_do_L1_v2022.csv'))

#export L1 met file
buoy2010_L1 %>%
  select(datetime, location, 
         windDirectionInstantaneous_deg, windSpeedInstantaneous_mps,
         windDirectionAverage_deg, windSpeedAverage_mps, 
         radiationIncomingPAR_umolm2s, flag_par,
         airTemperature_degC) %>%
  mutate(datetime = as.character(datetime)) %>%
  write_csv(., file.path(dump_dir, 'met/2010_met_L1_v2022.csv'))
