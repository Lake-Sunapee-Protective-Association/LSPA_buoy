#*****************************************************************
#*      Cary Institute of Ecosystem Studies (Millbrook, NY)      *
#*                      Weathers Lab                             *
#*                                                               *
#* TITLE:   Sunapee_buoy_2007.r                                  *
#* PROJECT: SunapeeBuoy.RProj                                    *
#* AUTHOR:  Bethel Steele                                        *
#*****************************************************************

source('library_func_lists.R')

#point to data directories
raw_dir = 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L0/'
dump_dir = 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/'

#set tz
buoy_tz = 'Etc/GMT+5'

#bring in 2007 buoy raw data
buoy2007_L0 <- read.csv(file.path(raw_dir, 'Sunapee2007_rawData.csv'),
                        col.names = c('datetime', 'AirTempC', 'BattVolt', 'DOSat', 'DOppm', 
                                      'DOSat2', 'PAR', 'BattVolt2', 'RH', 'DOTempC', 
                                      'TempC_0m', 'TempC_0p5m', 'TempC_1m', 'TempC_1p5m', 'TempC_2m', 
                                      'TempC_2p5m', 'TempC_3m', 'TempC_4m', 'TempC_5m', 'TempC_6m',
                                      'TempC_7m', 'TempC_8m', 'TempC_9m', 'TempC_10m', 'TempC_11m',
                                      'TempC_13m', 'InstWindDir', 'AveWindDir', 'AveWindSp', 'InstWindSp'), 
                     skip=1) %>% 
  select(-BattVolt, -DOSat2, -BattVolt2, -AveWindSp, -AveWindDir) %>%  #drop blank columns
  mutate(datetime = as.POSIXct(datetime, format='%Y-%m-%d %H:%M:%S', tz=buoy_tz))

#double check to make sure there are no DST issues
datelength2007 <- buoy2007_L0 %>% 
  mutate(date = format(datetime, '%Y-%m-%d')) %>% 
  group_by(date) %>% 
  summarize(length(datetime))
#look on DST days
#--buoy not deployed in march
datelength2007[datelength2007$date == '2007-11-04',]
#a couple fewer on this day, but not at the right time. Assume no DST observed

#check next day and previous just in case
datelength2007[datelength2007$date == '2007-11-03',]
datelength2007[datelength2007$date == '2007-11-05',]

#make sure all timestamps present and create L1 dataset
#create dummy timestamp so there are no blanks
range(buoy2007_L0$datetime)
alltimes_2007 <- as.data.frame(seq.POSIXt(as.POSIXct('2007-08-27 23:00', tz=buoy_tz), as.POSIXct('2007-12-31 23:50', tz=buoy_tz), '10 min')) %>% 
  rename("datetime" = !!names(.[1]))

buoy2007_L1 <- buoy2007_L0 %>% 
  full_join(., alltimes_2007) %>% 
  arrange(datetime)


#clean up workspace
rm(alltimes_2007, datelength2007)

#### thermistors ####
buoy2007_temp_vert <- buoy2007_L1 %>%
  select(datetime, alltemp2007) %>%
  gather(variable, value, -datetime) %>% 
  mutate(variable = factor(variable, levels = c(alltemp2007)))
  
# ggplot(subset(buoy2007_temp_vert, 
#               subset=(datetime >=as.POSIXct('2007-01-01', tz=buoy_tz) & 
#                         datetime < as.POSIXct('2008-01-01', tz=buoy_tz))), 
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   labs(title='2007 buoy data - raw',
#        x=NULL,
#        y='temp (deg C)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 month') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust

#recode NA values to na; add location as loon
buoy2007_L1 <- buoy2007_L1 %>% 
  mutate_at(vars(all_of(alltemp2007)),
            ~(case_when(. == -6999 ~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate(location = 'loon')

buoy2007_temp_vert <- buoy2007_L1 %>%
  select(datetime, all_of(alltemp2007)) %>%
  pivot_longer(names_to = 'variable', values_to = 'value', -datetime) %>%
  mutate(variable = factor(variable, levels=alltemp2007))
 
ggplot(subset(buoy2007_temp_vert, 
              subset=(datetime >=as.POSIXct('2007-01-01', tz=buoy_tz) & 
                        datetime < as.POSIXct('2008-01-01', tz=buoy_tz))), 
       aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  labs(title = '2007 buoy data, NAs recoded',
       x=NULL,
       y='temp (deg C)') +
  final_theme +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
                              "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust

# #buoy deployed 25Aug2007, data begins on the 27th
# ggplot(subset(buoy2007_temp_vert, subset=(datetime >=as.POSIXct('2007-08-25', tz=buoy_tz) & datetime < as.POSIXct('2007-08-30', tz=buoy_tz))), aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   labs(title = 'Aug/Sept 2007 buoy data, NAs recoded',
#        x='date',
#        y='temp deg C') +
#   final_theme +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) + #so you can adjust
#   scale_x_datetime(date_minor_breaks = '1 hour')

# 10-13m all reading about the same temp - hung up?

# #sept 6 buoy visit
# ggplot(subset(buoy2007_temp_vert, subset=(datetime >=as.POSIXct('2007-09-06', tz=buoy_tz) & datetime < as.POSIXct('2007-09-07', tz=buoy_tz))), aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   labs(title = 'Aug/Sept 2007 buoy data, NAs recoded',
#        x='date',
#        y='temp deg C') +
#   final_theme +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) + #so you can adjust
#   scale_x_datetime(date_minor_breaks = '1 hour')

#recode data when out of water; add flags for sensors hung up
buoy2007_L1 <- buoy2007_L1 %>% 
  mutate_at(vars(all_of(alltemp2007)),
            ~(case_when(datetime>=as.POSIXct('2007-09-06 13:30', tz=buoy_tz) & 
                              datetime < as.POSIXct('2007-09-06 16:00') ~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate(flag_temp9p5m = case_when(datetime>=as.POSIXct('2007-09-06 16:00', tz=buoy_tz) ~ 'd', #sensors hung up
                           TRUE ~ ''),
         flag_temp10p5m = case_when(datetime>=as.POSIXct('2007-09-06 16:00', tz=buoy_tz) ~ 'd', #sensors hung up
                               TRUE ~ ''),
         flag_temp11p5m = case_when(datetime>=as.POSIXct('2007-09-06 16:00', tz=buoy_tz) ~ 'd', #sensors hung up
                               TRUE ~ ''),
         flag_temp13p5m = case_when(datetime>=as.POSIXct('2007-09-06 16:00', tz=buoy_tz) ~ 'd', #sensors hung up
                               TRUE ~ '')) %>% 
  mutate(flag_temp11p5m = case_when(datetime<as.POSIXct('2007-09-06 13:30', tz=buoy_tz) ~ 'd', #sensors hung up
                                TRUE ~ flag_temp11p5m),
         flag_temp13p5m = case_when(datetime>=as.POSIXct('2007-09-06 13:30', tz=buoy_tz) ~ 'd', #sensors hung up
                                TRUE ~ flag_temp13p5m))

buoy2007_temp_vert_b <- buoy2007_L1 %>%
  select(datetime, all_of(alltemp2007)) %>%
  pivot_longer(names_to = 'variable', values_to = 'value', -datetime) %>%
  mutate(variable = factor(variable, levels=alltemp2007))

# ggplot(subset(buoy2007_temp_vert_b, subset=(datetime >=as.POSIXct('2007-08-01', tz=buoy_tz) &
#                                               datetime < as.POSIXct('2007-09-01', tz=buoy_tz))), aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   labs(title = 'Aug 2007 buoy data, clean',
#        x='date',
#        y='temp deg C') +
#   final_theme +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) + #so you can adjust
#   scale_x_datetime(date_minor_breaks = ('1 day'))

# ggplot(subset(buoy2007_temp_vert_b, subset=(datetime >=as.POSIXct('2007-09-01', tz=buoy_tz) &
#                                               datetime < as.POSIXct('2007-09-15', tz=buoy_tz))), aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   labs(title = 'Sept 2007 buoy data, clean',
#        x='date',
#        y='temp deg C') +
#   final_theme +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) + #so you can adjust
#   scale_x_datetime(date_minor_breaks = ('1 day'))

# ggplot(subset(buoy2007_temp_vert_b, subset=(datetime >=as.POSIXct('2007-09-15', tz=buoy_tz) & 
#                                               datetime < as.POSIXct('2007-10-01', tz=buoy_tz))), aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   labs(title = 'Sept 2007 buoy data, clean',
#        x='date',
#        y='temp deg C') +
#   final_theme +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) + #so you can adjust
#   scale_x_datetime(date_minor_breaks = ('1 day'))
# 
# ggplot(subset(buoy2007_temp_vert, subset=(datetime >=as.POSIXct('2007-10-01', tz=buoy_tz) & datetime < as.POSIXct('2007-11-01', tz=buoy_tz))), aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   labs(title = 'Oct 2007 buoy data, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   final_theme +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) + #so you can adjust
#   scale_x_datetime(date_minor_breaks = ('1 day'))
# 
# #Oct 3
# ggplot(subset(buoy2007_temp_vert, subset=(datetime >=as.POSIXct('2007-10-03', tz=buoy_tz) & datetime < as.POSIXct('2007-10-04', tz=buoy_tz))), aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   labs(title = 'Oct 2007 buoy data, NAs recoded',
#        x=NULL,
#        y='temp (deg C)') +
#   final_theme +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) + #so you can adjust
#   scale_x_datetime(date_minor_breaks = ('1 hour'))
# 
# #oct 5
# ggplot(subset(buoy2007_temp_vert, subset=(datetime >=as.POSIXct('2007-10-05', tz=buoy_tz) & datetime < as.POSIXct('2007-10-06', tz=buoy_tz))), aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   labs(title = 'Oct 2007 buoy data, NAs recoded',
#        x='date',
#        y='temp deg C') +
#   final_theme +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) + #so you can adjust
#   scale_x_datetime(date_minor_breaks = ('1 hour'))
# 
# #Oct 23 anamolous point
# ggplot(subset(buoy2007_temp_vert, subset=(datetime >=as.POSIXct('2007-10-23', tz=buoy_tz) & datetime < as.POSIXct('2007-10-24', tz=buoy_tz))), aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   labs(title = 'Oct 2007 buoy data, NAs recoded',
#        x='date',
#        y='temp deg C') +
#   final_theme +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) + #so you can adjust
#   scale_x_datetime(date_minor_breaks = ('1 hour'))

buoy2007_L1 <- buoy2007_L1 %>% 
  mutate(TempC_8m = case_when(datetime==as.POSIXct('2007-10-23 10:50', tz=buoy_tz) ~ NA_real_, #outlier
                           TRUE ~ TempC_8m)) 
buoy2007_temp_vert_b <- buoy2007_L1 %>%
  select(datetime, all_of(alltemp2007)) %>%
  pivot_longer(names_to = 'variable', values_to = 'value', -datetime) %>%
  mutate(variable = factor(variable, levels=alltemp2007))

# ggplot(subset(buoy2007_temp_vert_b, subset=(datetime >=as.POSIXct('2007-10-01', tz=buoy_tz) & datetime < as.POSIXct('2007-11-01', tz=buoy_tz))), aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   labs(title = 'Oct 2007 buoy data, clean',
#        x=NULL,
#        y='temp (deg C)') +
#   final_theme +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) + #so you can adjust
#   scale_x_datetime(date_minor_breaks = ('1 day'))
# 
# ggplot(subset(buoy2007_temp_vert, subset=(datetime >=as.POSIXct('2007-11-01', tz=buoy_tz) & datetime < as.POSIXct('2007-12-01', tz=buoy_tz))), aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   labs(title = 'Nov 2007 buoy data, NAs recoded',
#        x=NULL,
#        y='temp (deg C)') +
#   final_theme +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) + #so you can adjust
#   scale_x_datetime(date_minor_breaks = ('1 day'))
# 
# ggplot(subset(buoy2007_temp_vert, subset=(datetime >=as.POSIXct('2007-12-01', tz=buoy_tz) & datetime < as.POSIXct('2008-01-01', tz=buoy_tz))), aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   labs(title = 'Dec 2007 buoy data, NAs recoded',
#        x=NULL,
#        y='temp (deg C)') +
#   final_theme +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) + #so you can adjust
#   scale_x_datetime(date_minor_breaks = ('1 day'))
# 
# ggplot(subset(buoy2007_temp_vert, subset=(datetime >=as.POSIXct('2007-12-05', tz=buoy_tz) & datetime < as.POSIXct('2007-12-06', tz=buoy_tz))), aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   labs(title = 'Dec 2007 buoy data, NAs recoded',
#        x='date',
#        y='temp deg C') +
#   final_theme +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) + #so you can adjust
#   scale_x_datetime(date_minor_breaks = ('1 hour'))

#dec 5 18:20 8m anomolous
buoy2007_L1 <- buoy2007_L1 %>% 
  mutate(TempC_8m = case_when(datetime == as.POSIXct('2007-12-05 18:20', tz=buoy_tz) ~ NA_real_, #outlier
                           TRUE ~ TempC_8m))
buoy2007_temp_vert_b <- buoy2007_L1 %>%
  select(datetime, all_of(alltemp2007)) %>%
  pivot_longer(names_to = 'variable', values_to = 'value', -datetime) %>%
  mutate(variable = factor(variable, levels=alltemp2007))

# ggplot(subset(buoy2007_temp_vert_b, subset=(datetime >=as.POSIXct('2007-12-01', tz=buoy_tz) & datetime < as.POSIXct('2008-01-01', tz=buoy_tz))), aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   labs(title = 'Dec 2007 buoy data, clean',
#        x=NULL,
#        y='temp (deg C)') +
#   final_theme +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) + #so you can adjust
#   scale_x_datetime(date_minor_breaks = ('1 day'))

ggplot(subset(buoy2007_temp_vert_b, subset=(datetime >=as.POSIXct('2007-01-01', tz=buoy_tz) & datetime < as.POSIXct('2008-01-01', tz=buoy_tz))), aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  labs(title = '2007 buoy data, clean',
       x=NULL,
       y='temp (deg C)') +
  final_theme +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
                              "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) + #so you can adjust
  scale_x_datetime(date_minor_breaks = ('1 month'))


#clean up workspace
rm(buoy2007_temp_vert, buoy2007_temp_vert_b)

#correct thermistor depth for offset; add CV
buoy2007_L1 <- buoy2007_L1 %>% 
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

#add flag for 11.5 and 13.5 as possibly in sediment
buoy2007_L1 <- buoy2007_L1 %>% 
  mutate(flag_temp11p5m = case_when(flag_temp11p5m == '' ~ 'b',
                                flag_temp11p5m != '' ~ paste(flag_temp11p5m, 'b', sep = '; '),
                                TRUE ~ flag_temp11p5m),
         flag_temp13p5m = case_when(flag_temp13p5m == '' ~ 'b',
                                    flag_temp13p5m != '' ~ paste(flag_temp13p5m, 'b', sep = '; '),
                                TRUE ~ flag_temp13p5m))
         

#### DO sensors ####
range(buoy2007_L1$DOSat, na.rm=T)
range(buoy2007_L1$DOppm, na.rm=T)
range(buoy2007_L1$DOTempC, na.rm=T)

do_vert <- buoy2007_L1 %>% 
  select(datetime, all_of(upDO)) %>% 
  pivot_longer(names_to = 'variable', values_to = 'value', -datetime)

ggplot(do_vert, aes(x = datetime, y = value)) +
  geom_point() +
  facet_grid(variable~., scales = 'free_y') +
  final_theme +
  labs(title = '2007 DO data raw',
       x = NULL,
       y = NULL) +
  scale_x_datetime(date_minor_breaks = '1 month')

# ggplot(subset(do_vert, 
#               subset = (datetime >= as.POSIXct('2007-08-25', tz=buoy_tz) & 
#                           datetime < as.POSIXct('2007-10-01', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'aug/sept 2007 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(do_vert, 
#               subset = (datetime >= as.POSIXct('2007-10-01', tz=buoy_tz) & 
#                           datetime < as.POSIXct('2007-11-01', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'oct 2007 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(do_vert, 
#               subset = (datetime >= as.POSIXct('2007-11-01', tz=buoy_tz) & 
#                           datetime < as.POSIXct('2007-12-01', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'nov 2007 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(do_vert, 
#               subset = (datetime >= as.POSIXct('2007-12-01', tz=buoy_tz) & 
#                           datetime < as.POSIXct('2008-01-01', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'dec 2007 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')

#add flag for sensor calibraion unknown
buoy2007_L1 <- buoy2007_L1 %>% 
  mutate(flag_do1p5m = case_when(!is.na(DOppm) ~ 'x',
                                   !is.na(DOSat) ~ 'x',
                                   TRUE ~ ''))

#rename with CV
buoy2007_L1 <- buoy2007_L1 %>% 
  rename(oxygenDissolved_mgl_1p5m = DOppm,
         oxygenDissolvedPercentOfSaturation_pct_1p5m = DOSat,
         waterTemperature_DO_degC_1p5m = DOTempC)

rm(do_vert)

#### wind sensors ####
range(buoy2007_L1$InstWindDir, na.rm = T)
range(buoy2007_L1$InstWindSp, na.rm = T)

wind_vert <- buoy2007_L1 %>% 
  select(datetime, InstWindDir, InstWindSp) %>% 
  pivot_longer(names_to = 'variable', values_to = 'value', -datetime)

ggplot(wind_vert, aes(x = datetime, y = value)) +
  geom_point() +
  facet_grid(variable~., scales = 'free_y') +
  final_theme +
  labs(title = '2007 wind data raw',
       x = NULL,
       y = NULL) +
  scale_x_datetime(date_minor_breaks = '1 month')

# this wind sensor does not have a compass built in, so all data are considered errant

buoy2007_L1 <- buoy2007_L1 %>% 
  mutate(InstWindDir = NA_real_) %>% 
  mutate(flag_winddir = 'e')

# ggplot(subset(wind_vert, 
#               subset=(datetime>=as.POSIXct('2007-08-01', tz=buoy_tz) &
#                         datetime<as.POSIXct('2007-09-01', tz=buoy_tz))), 
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'aug 2007 wind data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(wind_vert, 
#               subset=(datetime>=as.POSIXct('2007-09-01', tz=buoy_tz) &
#                         datetime<as.POSIXct('2007-10-01', tz=buoy_tz))), 
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'sept 2007 wind data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(wind_vert, 
#               subset=(datetime>=as.POSIXct('2007-10-01', tz=buoy_tz) &
#                         datetime<as.POSIXct('2007-11-01', tz=buoy_tz))), 
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'oct 2007 wind data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(wind_vert, 
#               subset=(datetime>=as.POSIXct('2007-11-01', tz=buoy_tz) &
#                         datetime<as.POSIXct('2007-12-01', tz=buoy_tz))), 
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'nov 2007 wind data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(wind_vert, 
#               subset=(datetime>=as.POSIXct('2007-12-01', tz=buoy_tz) &
#                         datetime<as.POSIXct('2008-01-01', tz=buoy_tz))), 
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'dec 2007 wind data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# #sensor stuck dec 31
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2007-12-31', tz=buoy_tz) &
#                         datetime<as.POSIXct('2008-01-01', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'dec 2007 wind data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 hour')

buoy2007_L1 <- buoy2007_L1 %>% 
  mutate_at(vars(InstWindDir, InstWindSp),
            ~(case_when(datetime >= as.POSIXct('2007-12-31 08:30', tz=buoy_tz) & 
                             datetime < as.POSIXct('2007-12-31 15:00', tz=buoy_tz) ~ NA_real_,
                             TRUE ~ .)))
wind_vert_b <- buoy2007_L1 %>% 
  select(datetime, InstWindDir, InstWindSp) %>% 
  pivot_longer(names_to = 'variable', values_to = 'value', -datetime)

# ggplot(subset(wind_vert_b,
#               subset=(datetime>=as.POSIXct('2007-12-01', tz=buoy_tz) &
#                         datetime<as.POSIXct('2008-01-01', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'dec 2007 wind data clean',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')

ggplot(wind_vert_b, aes(x = datetime, y = value)) +
  geom_point() +
  facet_grid(variable~., scales = 'free_y') +
  final_theme +
  labs(title = '2007 wind data clean',
       x = NULL,
       y = NULL) +
  scale_x_datetime(date_minor_breaks = '1 month')

#rename with CV
buoy2007_L1 <- buoy2007_L1 %>% 
  rename(windDirectionInstantaneous_deg = InstWindDir,
         windSpeedInstantaneous_mps = InstWindSp)

rm(wind_vert, wind_vert_b)


####PAR####
range(buoy2007_L1$PAR, na.rm = T)

buoy2007_L1 <-  buoy2007_L1 %>% 
  mutate(flag_par = case_when(PAR <0 ~ 'z',
                              TRUE ~ ''),
         PAR = case_when(PAR <0 ~ 0,
                         TRUE ~ PAR))

ggplot(buoy2007_L1, aes(x = datetime, y = PAR)) +
  geom_point() +
  final_theme +
  labs(title = '2007 PAR data raw',
       x = NULL,
       y = 'PAR (umol/m2/s)') +
  scale_x_datetime(date_minor_breaks = '1 month')

# ggplot(subset(buoy2007_L1,
#               subset = (datetime>=as.POSIXct('2007-08-01', tz=buoy_tz) &
#                           datetime < as.POSIXct('2007-10-01', tz=buoy_tz))), 
#               aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'aug/sept 2007 PAR data raw',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2007_L1,
#               subset = (datetime>=as.POSIXct('2007-10-01', tz=buoy_tz) &
#                           datetime < as.POSIXct('2007-11-01', tz=buoy_tz))),
#        aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'oct 2007 PAR data raw',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2007_L1,
#               subset = (datetime>=as.POSIXct('2007-11-01', tz=buoy_tz) &
#                           datetime < as.POSIXct('2007-12-01', tz=buoy_tz))),
#        aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'nov 2007 PAR data raw',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2007_L1,
#               subset = (datetime>=as.POSIXct('2007-12-01', tz=buoy_tz) &
#                           datetime < as.POSIXct('2008-01-01', tz=buoy_tz))),
#        aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'dec 2007 PAR data raw',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')

#rename with CV
buoy2007_L1 <- buoy2007_L1 %>% 
  rename(radiationIncomingPAR_umolm2s = PAR)

#### Air Temp ####
range(buoy2007_L1$AirTempC, na.rm = T)

ggplot(buoy2007_L1, aes(x=datetime, y = AirTempC)) +
  geom_point() +
  final_theme +
  labs(title = '2007 air temp raw',
       x= NULL,
       y= 'air temp (deg C)') +
  scale_x_datetime(date_minor_breaks = '1 month')

#looks good

#rename with CV
buoy2007_L1 <- buoy2007_L1 %>% 
  rename(airTemperature_degC = AirTempC)


#### Relative Humidity ####
range(buoy2007_L1$RH, na.rm = T)

ggplot(buoy2007_L1, aes(x=datetime, y = RH)) +
  geom_point() +
  final_theme +
  labs(title = '2007 air temp raw',
       x= NULL,
       y= 'air temp (deg C)') +
  scale_x_datetime(date_minor_breaks = '1 month')

#rh is wrong. recode and do not export

#rename with CV
buoy2007_L1 <- buoy2007_L1 %>% 
  mutate(RH = NA_real_)


#### EXPORT L1 FILES ####
#export L1 tempstring file
colnames(buoy2007_L1)
buoy2007_L1 %>%
  select(datetime, location, 
         waterTemperature_degC_0p5m:waterTemperature_degC_13p5m, 
         flag_temp9p5m, flag_temp10p5m, flag_temp11p5m, flag_temp13p5m) %>% 
  mutate(datetime = as.character(datetime)) %>%
  write_csv(., file.path(dump_dir, 'tempstring/2007_tempstring_L1_v2022.csv'))

# export L1 DO file
buoy2007_L1 %>%
  select(datetime, location, 
         oxygenDissolved_mgl_1p5m, oxygenDissolvedPercentOfSaturation_pct_1p5m, waterTemperature_DO_degC_1p5m,
         flag_do1p5m) %>%
  mutate(datetime = as.character(datetime)) %>%
  write_csv(., file.path(dump_dir,'do/2007_do_L1_v2022.csv'))

# export met data
buoy2007_L1 %>%
  select(datetime, location, 
         windDirectionInstantaneous_deg, windSpeedInstantaneous_mps, flag_winddir, 
         radiationIncomingPAR_umolm2s, flag_par, 
         airTemperature_degC) %>%
  mutate(datetime = as.character(datetime)) %>%
  write_csv(., file.path(dump_dir, 'met/2007_met_L1_v2022.csv'))

