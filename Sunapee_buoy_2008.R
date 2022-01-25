#*****************************************************************
#*      Cary Institute of Ecosystem Studies (Millbrook, NY)      *
#*                      Weathers Lab                             *
#*                                                               *
#* TITLE:   Sunapee_buoy_2008.r                                  *
#* PROJECT: SunapeeBuoy.RProj                                    *
#* AUTHOR:  Bethel Steele                                        *
#* SYSTEM:  Lenovo ThinkCentre, Win 10, R 3.6.1, RStudio 1.2.5001*
#* DATE:    01Apr2020                                            *
#* PURPOSE: QAQC and collate buoy data                           *
#*****************************************************************

source('library_func_lists.R')

#point to data directories
raw_dir = 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L0/'
dump_dir = 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/'

#set tz
buoy_tz = 'Etc/GMT+5'

#bring in 2008 buoy raw data
buoy2008_L0 <- read.csv(file.path(raw_dir, 'Sunapee2008_rawData.csv'),
                        col.names = c('datetime', 'AirTempC', 'BattVolt', 'DOSat', 'DOppm', 
                                      'DOSat2', 'PAR', 'BattVolt2', 'RH', 'DOTempC', 
                                      'TempC_0m', 'TempC_0p5m', 'TempC_1m', 'TempC_1p5m', 'TempC_2m', 
                                      'TempC_2p5m', 'TempC_3m', 'TempC_4m', 'TempC_5m', 'TempC_6m',
                                      'TempC_7m', 'TempC_8m', 'TempC_9m', 'TempC_10m', 'TempC_11m',
                                      'TempC_13m', 'InstWindDir', 'AveWindDir', 'AveWindSp', 'InstWindSp'), 
                     skip=1) %>% 
  select(-BattVolt, -DOSat2, -BattVolt2, -AveWindDir, -AveWindSp) %>%  #drop blank columns
  mutate(datetime = as.POSIXct(datetime, format='%Y-%m-%d %H:%M:%S', tz = buoy_tz))

#double check to make sure there are no DST issues
datelength2008 <- buoy2008_L0 %>% 
  mutate(date = format(datetime, '%Y-%m-%d')) %>% 
  group_by(date) %>% 
  summarize(length(datetime))
#look at dst
datelength2008[datelength2008$date == '2008-03-09',]
#dst observed here
datelength2008[datelength2008$date == '2008-11-02',]
#no dst observed here. 

#force into NYtz with dst; convert to utc-5
buoy2008_L1 <- buoy2008_L0 %>% 
  mutate(datetime_instrument = force_tz(datetime, tz = 'America/New_York'),
         datetime = with_tz(datetime_instrument, tz = buoy_tz))

#check again
datelength2008 <- buoy2008_L1 %>% 
  mutate(date = format(datetime, '%Y-%m-%d')) %>% 
  group_by(date) %>%
  summarize(length(datetime))
#look at dst
datelength2008[datelength2008$date == '2008-03-09',] # this is right now
datelength2008[datelength2008$date == '2008-11-02',] # this has a gap, assume this is due to overwrite of dupe data

#make sure all timestamps present and create L1 dataset
#create dummy timestamp so there are no blanks
range(buoy2008_L1$datetime)
alltimes_2008 <- as.data.frame(seq.POSIXt(as.POSIXct('2008-01-01', tz = buoy_tz), as.POSIXct('2008-12-31 23:50', tz = buoy_tz), '10 min')) %>% 
  rename("datetime" = !!names(.[1]))

buoy2008_L1 <- buoy2008_L1 %>% 
  right_join(., alltimes_2008) %>% 
  arrange(datetime)

#clean up workspace
rm(alltimes_2008, datelength2008)


#### thermistors ####
buoy2008_temp_vert <- buoy2008_L1 %>%
  select(datetime, all_of(alltemp2007)) %>%
  pivot_longer(names_to = 'variable', values_to = 'value', -datetime) %>% 
  mutate(variable = factor(variable, levels = c(alltemp2007)))

# ggplot(subset(buoy2008_temp_vert,
#               subset=(datetime >=as.POSIXct('2008-01-01', tz = buoy_tz) &
#                         datetime < as.POSIXct('2009-01-01', tz = buoy_tz))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   labs(title = '2008 buoy temp data - raw',
#        x=NULL,
#        y='temp (deg C)') +
#   final_theme +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust

buoy2008_L1 <- buoy2008_L1 %>% 
  mutate_at(vars(alltemp2007),
            ~(case_when(. == -6999 ~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate(location = 'loon')
buoy2008_temp_vert <- buoy2008_L1 %>%
  select(datetime, all_of(alltemp2007)) %>%
  pivot_longer(names_to = 'variable', values_to = 'value', -datetime) %>% 
  mutate(variable = factor(variable, levels = c(alltemp2007)))

ggplot(subset(buoy2008_temp_vert,
              subset=(datetime >=as.POSIXct('2008-01-01', tz = buoy_tz) &
                        datetime < as.POSIXct('2009-01-01', tz = buoy_tz))),
       aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  labs(title = '2008 buoy temp data - raw',
       x=NULL,
       y='temp (deg C)') +
  final_theme +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
                              "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust

#in 2008, there were some wonky temps associated with NA strings in the 5m line. Recoding to NA at 3 and 4 m when 5m was na, since it seems like there were some communications oddities
buoy2008_L1 <- buoy2008_L1 %>% 
  mutate_at(vars(TempC_3m, TempC_4m),
            ~(case_when(is.na(TempC_5m) & datetime>=as.POSIXct('2008-01-01', tz = buoy_tz) & datetime < as.POSIXct('2009-01-01', tz = buoy_tz) ~ NA_real_,
                           TRUE ~ .)))

buoy2008_temp_vert <- buoy2008_L1 %>%
  select(datetime, all_of(alltemp2007)) %>%
  pivot_longer(names_to = 'variable', values_to = 'value', -datetime) %>% 
  mutate(variable = factor(variable, levels=alltemp2007))

# ggplot(subset(buoy2008_temp_vert,
#               subset=(datetime >=as.POSIXct('2008-01-01', tz = buoy_tz) &
#                         datetime < as.POSIXct('2009-01-01', tz = buoy_tz))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 month') +
#   labs(title = '2008 buoy temp data - NAs recoded',
#        x=NULL,
#        y='temp (deg C)') +
#   final_theme +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# ggplot(subset(buoy2008_temp_vert,
#               subset=(datetime >=as.POSIXct('2008-01-01', tz = buoy_tz) &
#                         datetime < as.POSIXct('2008-02-01', tz = buoy_tz))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   labs(title='Jan 2008, NAs recoded',
#        x=NULL,
#        y='temp (deg C)') +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# ggplot(subset(buoy2008_temp_vert,
#               subset=(datetime >=as.POSIXct('2008-02-01', tz = buoy_tz) &
#                         datetime < as.POSIXct('2008-03-01', tz = buoy_tz))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   labs(title='Feb 2008, NAs recoded',
#        x=NULL,
#        y='temp (deg C)') +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# ggplot(subset(buoy2008_temp_vert,
#               subset=(datetime >=as.POSIXct('2008-03-01', tz = buoy_tz) &
#                         datetime < as.POSIXct('2008-04-01', tz = buoy_tz))),
#        aes(x=datetime, y=value, color=variable)) +
#   labs(title='Mar 2008, NAs recoded',
#        x=NULL,
#        y='temp (deg C)') +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust

#inspect odd balls on Mar 8&25
ggplot(subset(buoy2008_temp_vert,
              subset=(datetime >=as.POSIXct('2008-03-08', tz = buoy_tz) &
                        datetime < as.POSIXct('2008-03-09', tz = buoy_tz))),
       aes(x=datetime, y=value, color=as.factor(variable))) +
  labs(title='Mar 2008, NAs recoded',
       x=NULL,
       y='temp (deg C)') +
  geom_line() +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme +
  theme(legend.position = 'bottom') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
                              "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust

ggplot(subset(buoy2008_temp_vert,
              subset=(datetime >=as.POSIXct('2008-03-25', tz = buoy_tz) &
                        datetime < as.POSIXct('2008-03-26', tz = buoy_tz))),
       aes(x=datetime, y=value, color=as.factor(variable))) +
  labs(title='Mar 2008, NAs recoded',
       x=NULL,
       y='temp (deg C)') +
  geom_line() +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme +
  theme(legend.position = 'bottom') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
                              "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust

buoy2008_L1 <- buoy2008_L1 %>%
  mutate_at(vars(all_of(alltemp2007)),
            ~(case_when(datetime == as.POSIXct('2008-03-08 18:40', tz = buoy_tz) ~ NA_real_,
                           datetime == as.POSIXct('2008-03-25 13:00', tz = buoy_tz) ~ NA_real_,
                           TRUE ~ .)))
buoy2008_temp_vert_b <- buoy2008_L1 %>%
  select(datetime, all_of(alltemp2007)) %>%
  pivot_longer(names_to = 'variable', values_to = 'value', -datetime) %>% 
  mutate(variable = factor(variable, levels=alltemp2007))

# ggplot(subset(buoy2008_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2008-03-01', tz = buoy_tz) &
#                         datetime < as.POSIXct('2008-04-01', tz = buoy_tz))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   labs(title='Mar 2008, clean',
#        x=NULL,
#        y='temp (deg C)') +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# ggplot(subset(buoy2008_temp_vert,
#               subset=(datetime >=as.POSIXct('2008-04-01', tz = buoy_tz) &
#                         datetime < as.POSIXct('2008-05-01', tz = buoy_tz))),
#        aes(x=datetime, y=value, color=variable)) +
#   labs(title='Apr 2008, NAs recoded',
#        x=NULL,
#        y='temp (deg C)') +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# #inspect Apr 1, 15
# ggplot(subset(buoy2008_temp_vert,
#                                    subset=(datetime >=as.POSIXct('2008-04-01', tz = buoy_tz) &
#                                              datetime < as.POSIXct('2008-04-02', tz = buoy_tz))),
#                             aes(x=datetime, y=value, color=as.factor(variable))) +
#   labs(title='Apr 2008, NAs recoded',
#        x=NULL,
#        y='temp (deg C)') +
#   geom_line(size=1.5) +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme +
#   theme(legend.position = 'bottom') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# ggplot(subset(buoy2008_temp_vert,
#                                    subset=(datetime >=as.POSIXct('2008-04-15', tz = buoy_tz) &
#                                              datetime < as.POSIXct('2008-04-16', tz = buoy_tz))),
#                             aes(x=datetime, y=value, color=as.factor(variable))) +
#   labs(title='Apr 2008, NAs recoded',
#        x=NULL,
#        y='temp (deg C)') +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme +
#   theme(legend.position = 'bottom') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust

buoy2008_L1 <- buoy2008_L1 %>%
  mutate(TempC_0p5m = case_when(datetime == as.POSIXct('2008-04-01 19:10', tz= buoy_tz) ~ NA_real_, #outlier
                                TRUE ~ TempC_0p5m)) %>% 
  mutate_at(vars(all_of(alltemp2007)),
            ~(case_when(datetime == as.POSIXct('2008-04-15 17:10', tz = buoy_tz) ~ NA_real_, #glitch in thermistors
                           TRUE ~ .)))
buoy2008_temp_vert_b <- buoy2008_L1 %>%
  select(datetime, all_of(alltemp2007)) %>%
  pivot_longer(names_to = 'variable', values_to = 'value', -datetime) %>% 
  mutate(variable = factor(variable, levels=alltemp2007))


# ggplot(subset(buoy2008_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2008-04-01', tz = buoy_tz) &
#                         datetime < as.POSIXct('2008-05-01', tz = buoy_tz))),
#        aes(x=datetime, y=value, color=(variable))) +
#   labs(title='Apr 2008, clean',
#        x=NULL,
#        y='temp (deg C)') +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# ggplot(subset(buoy2008_temp_vert,
#               subset=(datetime >=as.POSIXct('2008-05-01', tz = buoy_tz) &
#                         datetime < as.POSIXct('2008-06-01', tz = buoy_tz))),
#        aes(x=datetime, y=value, color=(variable))) +
#   labs(title='May 2008, NAs recoded',
#        x=NULL,
#        y='temp (deg C)') +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# #odball may 22/23
# ggplot(subset(buoy2008_temp_vert,
#               subset=(datetime >=as.POSIXct('2008-05-21 12:00', tz = buoy_tz) &
#                         datetime < as.POSIXct('2008-05-22 12:00', tz = buoy_tz))),
#        aes(x=datetime, y=value, color=variable)) +
#   labs(title='May 2008, NAs recoded',
#        x='date',
#        y='temp deg C') +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust

buoy2008_L1<- buoy2008_L1 %>%
  mutate_at(vars(all_of(alltemp2007)),
            ~(case_when(datetime == as.POSIXct('2008-05-21 23:50', tz = buoy_tz) ~ NA_real_, #thermistor glitch
                           TRUE ~ .)))
buoy2008_temp_vert_b <- buoy2008_L1 %>%
  select(datetime, all_of(alltemp2007)) %>%
  pivot_longer(names_to = 'variable', values_to = 'value', -datetime) %>% 
  mutate(variable = factor(variable, levels=alltemp2007))

# ggplot(subset(buoy2008_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2008-05-01', tz = buoy_tz) &
#                         datetime < as.POSIXct('2008-06-01', tz = buoy_tz))),
#        aes(x=datetime, y=value, color=(variable))) +
#   labs(title='May 2008, clean',
#        x=NULL,
#        y='temp (deg C)') +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust

# ggplot(subset(buoy2008_temp_vert,
#               subset=(datetime >=as.POSIXct('2008-06-01', tz = buoy_tz) &
#                         datetime < as.POSIXct('2008-07-01', tz = buoy_tz))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   labs(title='Jun 2008, NAs recoded',
#        x='date',
#        y='temp deg C') +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# #june 21 oddities
# ggplot(subset(buoy2008_temp_vert,
#               subset=(datetime >=as.POSIXct('2008-06-21', tz = buoy_tz) &
#                         datetime < as.POSIXct('2008-06-22', tz = buoy_tz))),
#        aes(x=datetime, y=value, color=variable)) +
#   labs(title='Jun 2008, NAs recoded',
#        x='date',
#        y='temp deg C') +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust

buoy2008_L1<- buoy2008_L1 %>%
  mutate_at(vars(alltemp2007),
            ~(case_when(datetime == as.POSIXct('2008-06-21 7:50', tz = buoy_tz) ~ NA_real_, #glitch in thermistors
                           TRUE ~ .)))

buoy2008_temp_vert_b <- buoy2008_L1 %>%
  select(datetime, all_of(alltemp2007)) %>%
  pivot_longer(names_to = 'variable', values_to = 'value', -datetime) %>% 
  mutate(variable = factor(variable, levels=alltemp2007))

# ggplot(subset(buoy2008_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2008-06-01', tz = buoy_tz) &
#                         datetime < as.POSIXct('2008-07-01', tz = buoy_tz))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   labs(title='Jun 2008, clean',
#        x='date',
#        y='temp deg C') +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# ggplot(subset(buoy2008_temp_vert,
#               subset=(datetime >=as.POSIXct('2008-07-01', tz = buoy_tz) &
#                         datetime < as.POSIXct('2008-08-01', tz = buoy_tz))),
#        aes(x=datetime, y=value, color=(variable))) +
#   labs(title='Jul 2008, NAs recoded',
#        x=NULL,
#        y='temp (deg C)') +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
 #jul 13 inversion/oddities
# ggplot(subset(buoy2008_temp_vert,
#               subset=(datetime >=as.POSIXct('2008-07-13', tz = buoy_tz) &
#                         datetime < as.POSIXct('2008-07-14', tz = buoy_tz))),
#        aes(x=datetime, y=value, color=variable)) +
#   labs(title='Jul 2008, NAs recoded',
#        x='date',
#        y='temp deg C') +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
buoy2008_L1<- buoy2008_L1 %>%
  mutate_at(vars(alltemp2007),
            ~(case_when(datetime == as.POSIXct('2008-07-13 21:40', tz = buoy_tz) ~ NA_real_,
                           TRUE ~ .)))

buoy2008_temp_vert_b <- buoy2008_L1 %>%
  select(datetime, all_of(alltemp2007)) %>%
  pivot_longer(names_to = 'variable', values_to = 'value', -datetime) %>% 
  mutate(variable = factor(variable, levels=alltemp2007))

# ggplot(subset(buoy2008_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2008-07-01', tz = buoy_tz) &
#                         datetime < as.POSIXct('2008-08-01', tz = buoy_tz))),
#        aes(x=datetime, y=value, color=variable)) +
#   labs(title='Jul 2008, clean',
#        x='date',
#        y='temp deg C') +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# ggplot(subset(buoy2008_temp_vert,
#               subset=(datetime >=as.POSIXct('2008-08-01', tz = buoy_tz) &
#                         datetime < as.POSIXct('2008-09-01', tz = buoy_tz))),
#        aes(x=datetime, y=value, color=(variable))) +
#   labs(title='Aug 2008, NAs recoded',
#        x=NULL,
#        y='temp (deg C)') +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjus
# 
# #aug 23 oddities
# ggplot(subset(buoy2008_temp_vert,
#               subset=(datetime >=as.POSIXct('2008-08-23', tz = buoy_tz) &
#                         datetime < as.POSIXct('2008-08-24', tz = buoy_tz))),
#        aes(x=datetime, y=value, color=variable)) +
#   labs(title='Aug 2008, NAs recoded',
#        x='date',
#        y='temp deg C') +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust

buoy2008_L1<- buoy2008_L1 %>%
  mutate_at(vars(alltemp2007),
            ~(case_when(datetime == as.POSIXct('2008-08-23 10:40', tz = buoy_tz) ~ NA_real_, #thermistor glitch
                           TRUE ~ .)))

buoy2008_temp_vert_b <- buoy2008_L1 %>%
  select(datetime, all_of(alltemp2007)) %>%
  pivot_longer(names_to = 'variable', values_to = 'value', -datetime) %>% 
  mutate(variable = factor(variable, levels=alltemp2007))

# ggplot(subset(buoy2008_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2008-08-01', tz = buoy_tz) &
#                         datetime < as.POSIXct('2008-09-01', tz = buoy_tz))),
#        aes(x=datetime, y=value, color=(variable))) +
#   labs(title='Aug 2008, clean',
#        x=NULL,
#        y='temp (deg C)') +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# ggplot(subset(buoy2008_temp_vert,
#               subset=(datetime >=as.POSIXct('2008-09-01', tz = buoy_tz) &
#                         datetime < as.POSIXct('2008-10-01', tz = buoy_tz))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   labs(title='Sept 2008, NAs recoded',
#        x=NULL,
#        y='temp (deg C)') +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# #oddball on Sept 27
# ggplot(subset(buoy2008_temp_vert,
#               subset=(datetime >=as.POSIXct('2008-09-27', tz = buoy_tz) &
#                         datetime < as.POSIXct('2008-09-28', tz = buoy_tz))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   labs(title='Sept 2008, NAs recoded',
#        x='date',
#        y='temp deg C') +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
buoy2008_L1 <- buoy2008_L1 %>%
  mutate_at(vars(alltemp2007),
            ~(case_when(datetime == as.POSIXct('2008-09-27 2:00', tz = buoy_tz) ~ NA_real_,
                           TRUE ~ .)))

buoy2008_temp_vert_b <- buoy2008_L1 %>%
  select(datetime, all_of(alltemp2007)) %>%
  pivot_longer(names_to = 'variable', values_to = 'value', -datetime) %>% 
  mutate(variable = factor(variable, levels=alltemp2007))

# ggplot(subset(buoy2008_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2008-09-01', tz = buoy_tz) &
#                         datetime < as.POSIXct('2008-10-01', tz = buoy_tz))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   labs(title='Sept 2008, clean',
#        x='date',
#        y='temp deg C') +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89",
#                               "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# ggplot(subset(buoy2008_temp_vert,
#               subset=(datetime >=as.POSIXct('2008-10-01', tz = buoy_tz) &
#                         datetime < as.POSIXct('2008-11-01', tz = buoy_tz))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   labs(title='Oct 2008, NAs recoded',
#        x=NULL,
#        y='temp (deg C)') +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjus
# 
# #odd balls Oct 13 and 15
# ggplot(subset(buoy2008_temp_vert,
#               subset=(datetime >=as.POSIXct('2008-10-13', tz = buoy_tz) &
#                         datetime < as.POSIXct('2008-10-14', tz = buoy_tz))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   labs(title='Oct 2008, NAs recoded',
#        x='date',
#        y='temp deg C') +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjus
# ggplot(subset(buoy2008_temp_vert,
#               subset=(datetime >=as.POSIXct('2008-10-15', tz = buoy_tz) &
#                         datetime < as.POSIXct('2008-10-16', tz = buoy_tz))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   labs(title='Oct 2008, NAs recoded',
#        x='date',
#        y='temp deg C') +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjus

buoy2008_L1 <- buoy2008_L1 %>%
  mutate_at(vars(alltemp2007),
            ~(case_when(datetime == as.POSIXct('2008-10-13 09:20', tz = buoy_tz) ~ NA_real_,
                           datetime == as.POSIXct('2008-10-15 12:00', tz = buoy_tz) ~ NA_real_,
                           TRUE ~ .)))

buoy2008_temp_vert_b <- buoy2008_L1 %>%
  select(datetime, all_of(alltemp2007)) %>%
  pivot_longer(names_to = 'variable', values_to = 'value', -datetime) %>% 
  mutate(variable = factor(variable, levels=alltemp2007))

# ggplot(subset(buoy2008_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2008-10-01', tz = buoy_tz) &
#                         datetime < as.POSIXct('2008-11-01', tz = buoy_tz))),
#        aes(x=datetime, y=value, color=as.factor(variable))) +
#   labs(title='Oct 2008, clean',
#        x='date',
#        y='temp deg C') +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjus
# 
# ggplot(subset(buoy2008_temp_vert,
#               subset=(datetime >=as.POSIXct('2008-11-01', tz = buoy_tz) &
#                         datetime < as.POSIXct('2008-12-01', tz = buoy_tz))),
#        aes(x=datetime, y=value, color=(variable))) +
#   labs(title='Nov 2008, NAs recoded',
#        x=NULL,
#        y='temp (deg C)') +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# ggplot(subset(buoy2008_temp_vert,
#               subset=(datetime >=as.POSIXct('2008-11-15 20:00', tz = buoy_tz) &
#                         datetime < as.POSIXct('2008-11-17 4:00', tz = buoy_tz))),
#        aes(x=datetime, y=value, color=(variable))) +
#   labs(title='Nov 2008, NAs recoded',
#        x=NULL,
#        y='temp (deg C)') +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# ggplot(subset(buoy2008_temp_vert,
#               subset=(datetime >=as.POSIXct('2008-12-01', tz = buoy_tz) &
#                         datetime < as.POSIXct('2009-01-01', tz = buoy_tz))),
#        aes(x=datetime, y=value, color=(variable))) +
#   labs(title='Dec 2008, NAs recoded',
#        x=NULL,
#        y='temp (deg C)') +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# ggplot(subset(buoy2008_temp_vert,
#               subset=(datetime >=as.POSIXct('2008-12-29', tz = buoy_tz) &
#                         datetime < as.POSIXct('2008-12-30', tz = buoy_tz))),
#        aes(x=datetime, y=value, color=(variable))) +
#   labs(title='Dec 2008, NAs recoded',
#        x=NULL,
#        y='temp (deg C)') +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# ggplot(subset(buoy2008_temp_vert,
#               subset=(datetime >=as.POSIXct('2008-12-30', tz = buoy_tz) &
#                         datetime < as.POSIXct('2008-12-31', tz = buoy_tz))),
#        aes(x=datetime, y=value, color=(variable))) +
#   labs(title='Dec 2008, NAs recoded',
#        x=NULL,
#        y='temp (deg C)') +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# ggplot(subset(buoy2008_temp_vert,
#               subset=(datetime >=as.POSIXct('2008-12-31', tz = buoy_tz) &
#                         datetime < as.POSIXct('2009-01-01', tz = buoy_tz))),
#        aes(x=datetime, y=value, color=(variable))) +
#   labs(title='Dec 2008, NAs recoded',
#        x=NULL,
#        y='temp (deg C)') +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust

#add flag for intermittent temps starting Dec 29
buoy2008_L1 <- buoy2008_L1 %>%
  mutate(flag_alltemp = case_when(datetime >= as.POSIXct('2008-12-29', tz = buoy_tz) ~ 'i',
                               TRUE ~ ''))

buoy2008_temp_vert_b <- buoy2008_L1 %>%
  select(datetime, location, all_of(alltemp2007)) %>%
  pivot_longer(names_to = 'variable', values_to = 'value', -c(datetime, location)) %>% 
  mutate(variable = factor(variable, levels=alltemp2007))

# ggplot(subset(buoy2008_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2008-12-01', tz = buoy_tz) &
#                         datetime < as.POSIXct('2009-01-01', tz = buoy_tz))),
#        aes(x=datetime, y=value, color=(variable))) +
#   labs(title='Dec 2008, clean',
#        x=NULL,
#        y='temp (deg C)') +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
ggplot(subset(buoy2008_temp_vert_b,
              subset=(datetime >=as.POSIXct('2008-01-01', tz = buoy_tz) &
                        datetime < as.POSIXct('2009-01-01', tz = buoy_tz))),
       aes(x=datetime, y=value, color=as.factor(variable))) +
  geom_point() +
  labs(title='2008 buoy temp data, clean',
       x=NULL,
       y='temp (deg C)') +
  scale_x_datetime(date_minor_breaks = '1 month') +
  final_theme +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
                              "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust

#clean up workspace
rm(buoy2008_temp_vert, buoy2008_temp_vert_b)

#10-13 hung up all year - temps never separated in summer; unclear if 9.5 is at proper depth
buoy2008_L1 <- buoy2008_L1 %>% 
  mutate(flag_temp9p5m = 'd',
         flag_temp10p5m = 'd',
         flag_temp11p5m = 'd',
         flag_temp13p5m = 'd')

#add flag for 11.5 and 13.5 as possibly in sediment
buoy2008_L1 <- buoy2008_L1 %>% 
  mutate(flag_temp11p5m = case_when(flag_temp11p5m == '' ~ 'b',
                                    flag_temp11p5m != '' ~ paste(flag_temp11p5m, 'b', sep = '; '),
                                    TRUE ~ flag_temp11p5m),
         flag_temp13p5m = case_when(flag_temp13p5m == '' ~ 'b',
                                    flag_temp13p5m != '' ~ paste(flag_temp13p5m, 'b', sep = '; '),
                                    TRUE ~ flag_temp13p5m))


#correct thermistor depth for offset; add CV
buoy2008_L1 <- buoy2008_L1 %>% 
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
range(buoy2008_L1$DOSat, na.rm=T)
range(buoy2008_L1$DOppm, na.rm=T)
range(buoy2008_L1$DOTempC, na.rm=T)

#recode na strings
buoy2008_L1 <- buoy2008_L1 %>% 
  mutate(DOTempC = case_when(DOTempC == -6999 ~ NA_real_,
                             TRUE ~ DOTempC))

do_vert <- buoy2008_L1 %>% 
  select(datetime, DOSat, DOppm, DOTempC) %>% 
  pivot_longer(names_to = 'variable', values_to = 'value', -datetime)

# ggplot(do_vert, aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = '2008 DO data, na's recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 month')

# ggplot(subset(do_vert,
#               subset = (datetime >= as.POSIXct('2008-01-01', tz = buoy_tz) &
#                           datetime < as.POSIXct('2008-02-01', tz = buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'jan 2008 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(do_vert,
#               subset = (datetime >= as.POSIXct('2008-02-01', tz = buoy_tz) &
#                           datetime < as.POSIXct('2008-03-01', tz = buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'feb 2008 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
#data looks errant on feb 18
ggplot(subset(do_vert,
              subset = (datetime >= as.POSIXct('2008-02-18', tz = buoy_tz) &
                          datetime < as.POSIXct('2008-02-19', tz = buoy_tz))),
       aes(x = datetime, y = value)) +
  geom_point() +
  facet_grid(variable~., scales = 'free_y') +
  final_theme +
  labs(title = 'feb 2008 DO data raw',
       x = NULL,
       y = NULL) +
  scale_x_datetime(date_minor_breaks = '1 hour')

#flag data as suspicious
buoy2008_L1 <- buoy2008_L1 %>% 
  mutate(flag_do1p5m = case_when(datetime >= as.POSIXct('2008-02-18 9:40', tz = buoy_tz) &
                             datetime < as.POSIXct('2008-02-18 18:00') ~ 's',
                           TRUE ~ ''))

# ggplot(subset(do_vert,
#               subset = (datetime >= as.POSIXct('2008-03-01', tz = buoy_tz) &
#                           datetime < as.POSIXct('2008-04-01', tz = buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'mar 2008 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(do_vert,
#               subset = (datetime >= as.POSIXct('2008-04-01', tz = buoy_tz) &
#                           datetime < as.POSIXct('2008-05-01', tz = buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'apr 2008 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(do_vert,
#               subset = (datetime >= as.POSIXct('2008-05-01', tz = buoy_tz) &
#                           datetime < as.POSIXct('2008-06-01', tz = buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'may 2008 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(do_vert,
#               subset = (datetime >= as.POSIXct('2008-06-01', tz = buoy_tz) &
#                           datetime < as.POSIXct('2008-07-01', tz = buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'jun 2008 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(do_vert,
#               subset = (datetime >= as.POSIXct('2008-06-09', tz = buoy_tz) &
#                           datetime < as.POSIXct('2008-06-10', tz = buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'jun 2008 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 hour')

buoy2008_L1 <- buoy2008_L1 %>% 
  mutate(flag_do1p5m = case_when(datetime == as.POSIXct('2008-06-09 8:30', tz = buoy_tz) & flag_do1p5m == '' ~ 'w',
                                datetime == as.POSIXct('2008-06-09 8:30', tz = buoy_tz) & flag_do1p5m != '' ~ paste('w', flag_do1p5m, sep = '; '),
                                TRUE ~ flag_do1p5m))
 
# ggplot(subset(do_vert,
#               subset = (datetime >= as.POSIXct('2008-07-01', tz = buoy_tz) &
#                           datetime < as.POSIXct('2008-08-01', tz = buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'jul 2008 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(do_vert,
#               subset = (datetime >= as.POSIXct('2008-08-01', tz = buoy_tz) &
#                           datetime < as.POSIXct('2008-09-01', tz = buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'aug 2008 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(do_vert,
#               subset = (datetime >= as.POSIXct('2008-09-01', tz = buoy_tz) &
#                           datetime < as.POSIXct('2008-10-01', tz = buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'sept 2008 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(do_vert,
#               subset = (datetime >= as.POSIXct('2008-10-01', tz = buoy_tz) &
#                           datetime < as.POSIXct('2008-11-01', tz = buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'oct 2008 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(do_vert,
#               subset = (datetime >= as.POSIXct('2008-11-01', tz = buoy_tz) &
#                           datetime < as.POSIXct('2008-12-01', tz = buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'nov 2008 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(do_vert,
#               subset = (datetime >= as.POSIXct('2008-12-01', tz = buoy_tz) &
#                           datetime < as.POSIXct('2009-01-01', tz = buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'dec 2008 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')

#start seeing intermittent readings dec 29 - adding flag of intermittent do data from then through the end of the month
buoy2008_L1 <- buoy2008_L1 %>% 
  mutate(flag_do1p5m = case_when(datetime >= as.POSIXct('2008-12-29', tz = buoy_tz) & flag_do1p5m == '' ~ 'i',
                                TRUE ~ paste(flag_do1p5m, 'i', sep ='; ')))
do_vert_b <- buoy2008_L1 %>% 
  select(datetime, DOSat, DOppm, DOTempC, flag_do1p5m) %>% 
  pivot_longer(names_to = 'variable', values_to = 'value', -c(datetime, flag_do1p5m))

# ggplot(do_vert_b,
#        aes(x = datetime, y = value, color = flag_do1p5m)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = '2008 DO data clean',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 month')
 
#add in not calibrated flag
buoy2008_L1 <-  buoy2008_L1 %>% 
  mutate(flag_do1p5m = case_when(flag_do1p5m == '' ~ 'x',
                                TRUE ~ paste(flag_do1p5m, 'x', sep= '; ')))
unique(buoy2008_L1$flag_do1p5m)

do_vert_b <- buoy2008_L1 %>% 
  select(datetime, DOSat, DOppm, DOTempC, flag_do1p5m) %>% 
  pivot_longer(names_to = 'variable', values_to = 'value', -c(datetime, flag_do1p5m))

ggplot(do_vert_b,
       aes(x = datetime, y = value, color = flag_do1p5m)) +
  geom_point() +
  facet_grid(variable~., scales = 'free_y') +
  final_theme +
  labs(title = '2008 DO data clean',
       x = NULL,
       y = NULL) +
  scale_x_datetime(date_minor_breaks = '1 month')

#rename with CV
buoy2008_L1 <- buoy2008_L1 %>% 
  rename(oxygenDissolved_mgl_1p5m = DOppm,
         oxygenDissolvedPercentOfSaturation_pct_1p5m = DOSat,
         waterTemperature_DO_degC_1p5m = DOTempC)

rm(do_vert, do_vert_b)

#### wind sensors ####
range(buoy2008_L1$InstWindDir, na.rm = T)
range(buoy2008_L1$InstWindSp, na.rm = T)

#recode na strings
buoy2008_L1 <- buoy2008_L1 %>% 
  mutate(InstWindDir = case_when(InstWindDir==-6999 ~ NA_real_,
                                 TRUE ~ InstWindDir))

wind_vert <- buoy2008_L1 %>% 
  select(datetime, InstWindDir, InstWindSp) %>% 
  pivot_longer(names_to = 'variable', values_to = 'value', -datetime)

# ggplot(wind_vert, aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = '2008 wind data na's recoded,
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 month')

# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2008-01-01', tz = buoy_tz) &
#                         datetime<as.POSIXct('2008-02-01', tz = buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'jan 2008 wind data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2008-02-01', tz = buoy_tz) &
#                         datetime<as.POSIXct('2008-03-01', tz = buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'feb 2008 wind data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# #Feb 13 errant readings
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2008-02-13', tz = buoy_tz) &
#                         datetime<as.POSIXct('2008-02-14', tz = buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'feb 2008 wind data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 hour')

buoy2008_L1 <- buoy2008_L1 %>% 
  mutate_at(vars(InstWindDir, InstWindSp),
            ~(case_when(datetime >= as.POSIXct('2008-02-13 10:40', tz = buoy_tz) & 
                          datetime < as.POSIXct('2008-02-13 18:20', tz = buoy_tz) ~ NA_real_,
                           TRUE ~ .)))
wind_vert_b <- buoy2008_L1 %>% 
  select(datetime, InstWindDir, InstWindSp) %>% 
  pivot_longer(names_to = 'variable', values_to = 'value', -datetime)

# ggplot(subset(wind_vert_b,
#               subset=(datetime>=as.POSIXct('2008-02-01', tz = buoy_tz) &
#                         datetime<as.POSIXct('2008-03-01', tz = buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'feb 2008 wind data clean',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2008-03-01', tz = buoy_tz) &
#                         datetime<as.POSIXct('2008-04-01', tz = buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'mar 2008 wind data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# 
# #Mar 28
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2008-03-28', tz = buoy_tz) &
#                         datetime<as.POSIXct('2008-03-29', tz = buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'mar 2008 wind data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 hour')

buoy2008_L1 <- buoy2008_L1 %>% 
  mutate_at(vars(InstWindDir, InstWindSp),
            ~(case_when(datetime >= as.POSIXct('2008-03-28 3:40', tz = buoy_tz) & 
                          datetime < as.POSIXct('2008-03-28 11:00', tz = buoy_tz) ~ NA_real_,
                           TRUE ~ .)))
wind_vert_b <- buoy2008_L1 %>% 
  select(datetime, InstWindDir, InstWindSp) %>% 
  pivot_longer(names_to = 'variable', values_to = 'value', -datetime)

# ggplot(subset(wind_vert_b,
#               subset=(datetime>=as.POSIXct('2008-03-01', tz = buoy_tz) &
#                         datetime<as.POSIXct('2008-04-01', tz = buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'mar 2008 wind data clean',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2008-04-01', tz = buoy_tz) &
#                         datetime<as.POSIXct('2008-05-01', tz = buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'apr 2008 wind data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2008-05-01', tz = buoy_tz) &
#                         datetime<as.POSIXct('2008-06-01', tz = buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'may 2008 wind data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2008-06-01', tz = buoy_tz) &
#                         datetime<as.POSIXct('2008-07-01', tz = buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'june 2008 wind data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2008-07-01', tz = buoy_tz) &
#                         datetime<as.POSIXct('2008-08-01', tz = buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'july 2008 wind data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2008-08-01', tz = buoy_tz) &
#                         datetime<as.POSIXct('2008-09-01', tz = buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'aug 2008 wind data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2008-09-01', tz = buoy_tz) &
#                         datetime<as.POSIXct('2008-10-01', tz = buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'sept 2008 wind data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2008-10-01', tz = buoy_tz) &
#                         datetime<as.POSIXct('2008-11-01', tz = buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'oct 2008 wind data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2008-11-01', tz = buoy_tz) &
#                         datetime<as.POSIXct('2008-12-01', tz = buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'nov 2008 wind data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2008-12-01', tz = buoy_tz) &
#                         datetime<as.POSIXct('2009-01-01', tz = buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'dec 2008 wind data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')

# #sensor stuck dec 12
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2008-12-11 18:00', tz = buoy_tz) &
#                         datetime<as.POSIXct('2008-12-13', tz = buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'dec 2008 wind data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 hour')
# 
# #dec 25
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2008-12-24 18:00', tz = buoy_tz) &
#                         datetime<as.POSIXct('2008-12-27', tz = buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'dec 2008 wind data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 hour')
# 
# #dec 27-eoy
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2008-12-27', tz = buoy_tz) &
#                         datetime<=as.POSIXct('2008-12-31', tz = buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'dec 2008 wind data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 hour')

buoy2008_L1 <- buoy2008_L1 %>% 
  mutate_at(vars(InstWindDir, InstWindSp),
            ~(case_when(datetime >= as.POSIXct('2008-12-12 0:00', tz = buoy_tz) & 
                          datetime < as.POSIXct('2008-12-12 11:50', tz = buoy_tz) ~ NA_real_,
                        datetime >= as.POSIXct('2008-12-24', tz = buoy_tz) & 
                          datetime <= as.POSIXct('2008-12-31 ', tz = buoy_tz) &
                          is.na(InstWindDir) ~ NA_real_,
                        datetime >= as.Date('2008-12-31') ~ NA_real_,
                           TRUE ~ .)))
wind_vert_b <- buoy2008_L1 %>% 
  select(datetime, InstWindDir, InstWindSp) %>% 
  gather(variable, value, -datetime)

# ggplot(subset(wind_vert_b,
#               subset=(datetime>=as.POSIXct('2008-12-01', tz = buoy_tz) &
#                         datetime<as.POSIXct('2009-01-01', tz = buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'dec 2008 wind data clean',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')

ggplot(wind_vert_b, aes(x = datetime, y = value)) +
  geom_point() +
  facet_grid(variable~., scales = 'free_y') +
  final_theme +
  labs(title = '2008 wind data clean',
       x = NULL,
       y = NULL) +
  scale_x_datetime(date_minor_breaks = '1 month')

#flag for errant direction data
buoy2008_L1 <- buoy2008_L1 %>% 
  mutate(flag_winddir = 'e')

#rename with CV
buoy2008_L1 <- buoy2008_L1 %>% 
  rename(windDirection_deg = InstWindDir,
         windSpeed_mps = InstWindSp)

rm(wind_vert, wind_vert_b)


#### PAR ####

range(buoy2008_L1$PAR, na.rm = T)

buoy2008_L1 <-  buoy2008_L1 %>% 
  mutate(flag_par = case_when(PAR <0 ~ 'z',
                              TRUE ~ ''),
         PAR = case_when(PAR <0 ~ 0,
                         TRUE ~ PAR))

ggplot(buoy2008_L1, aes(x = datetime, y = PAR)) +
  geom_point() +
  final_theme +
  labs(title = '2008 PAR data raw',
       x = NULL,
       y = 'PAR (umol/m2/s)') +
  scale_x_datetime(date_minor_breaks = '1 month')

# ggplot(subset(buoy2008_L1,
#               subset = (datetime>=as.POSIXct('2008-01-01', tz = buoy_tz) &
#                           datetime<as.POSIXct('2008-02-01', tz = buoy_tz))), 
#        aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'jan 2008 PAR data raw',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2008_L1,
#               subset = (datetime>=as.POSIXct('2008-02-01', tz = buoy_tz) &
#                           datetime<as.POSIXct('2008-03-01', tz = buoy_tz))), 
#        aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'feb 2008 PAR data raw',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2008_L1,
#               subset = (datetime>=as.POSIXct('2008-03-01', tz = buoy_tz) &
#                           datetime<as.POSIXct('2008-04-01', tz = buoy_tz))), 
#        aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'mar 2008 PAR data raw',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2008_L1,
#               subset = (datetime>=as.POSIXct('2008-04-01', tz = buoy_tz) &
#                           datetime<as.POSIXct('2008-05-01', tz = buoy_tz))), 
#        aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'apr 2008 PAR data raw',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2008_L1,
#               subset = (datetime>=as.POSIXct('2008-05-01', tz = buoy_tz) &
#                           datetime<as.POSIXct('2008-06-01', tz = buoy_tz))), 
#        aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'may 2008 PAR data raw',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2008_L1,
#               subset = (datetime>=as.POSIXct('2008-06-01', tz = buoy_tz) &
#                           datetime<as.POSIXct('2008-07-01', tz = buoy_tz))), 
#        aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'jun 2008 PAR data raw',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2008_L1,
#               subset = (datetime>=as.POSIXct('2008-07-01', tz = buoy_tz) &
#                           datetime<as.POSIXct('2008-08-01', tz = buoy_tz))), 
#        aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'july 2008 PAR data raw',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2008_L1,
#               subset = (datetime>=as.POSIXct('2008-08-01', tz = buoy_tz) &
#                           datetime<as.POSIXct('2008-09-01', tz = buoy_tz))), 
#        aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'aug 2008 PAR data raw',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2008_L1,
#               subset = (datetime>=as.POSIXct('2008-09-01', tz = buoy_tz) &
#                           datetime<as.POSIXct('2008-10-01', tz = buoy_tz))), 
#        aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'sept 2008 PAR data raw',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2008_L1,
#               subset = (datetime>=as.POSIXct('2008-10-01', tz = buoy_tz) &
#                           datetime<as.POSIXct('2008-11-01', tz = buoy_tz))), 
#        aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'oct 2008 PAR data raw',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2008_L1,
#               subset = (datetime>=as.POSIXct('2008-11-01', tz = buoy_tz) &
#                           datetime<as.POSIXct('2008-12-01', tz = buoy_tz))), 
#        aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'nov 2008 PAR data raw',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2008_L1,
#               subset = (datetime>=as.POSIXct('2008-12-01', tz = buoy_tz) &
#                           datetime<as.POSIXct('2009-01-01', tz = buoy_tz))),
#        aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'dec 2008 PAR data raw',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
#dec 29 to eoy intermittent
buoy2008_L1 <-  buoy2008_L1 %>% 
  mutate(flag_par = case_when(flag_par == '' & datetime >= as.POSIXct('2008-12-29', tz = buoy_tz) ~ 'i',
                              flag_par != '' & datetime >= as.POSIXct('2008-12-29', tz = buoy_tz) ~ paste(flag_par, 'i', sep = '; '),
                              TRUE ~ flag_par))
unique(buoy2008_L1$flag_par)

# ggplot(subset(buoy2008_L1,
#               subset = (datetime>=as.POSIXct('2008-12-01', tz = buoy_tz) &
#                           datetime<as.POSIXct('2009-01-01', tz = buoy_tz))), 
#        aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'dec 2008 PAR data clean',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')

ggplot(subset(buoy2008_L1,
              subset = (datetime>=as.POSIXct('2008-01-01', tz = buoy_tz) &
                          datetime<as.POSIXct('2009-01-01', tz = buoy_tz))), 
       aes(x = datetime, y = PAR)) +
  geom_point() +
  final_theme +
  labs(title = '2008 PAR data clean',
       x = NULL,
       y = 'PAR (umol/m2/s)') +
  scale_x_datetime(date_minor_breaks = '1 month')

#rename with CV
buoy2008_L1 <- buoy2008_L1 %>% 
  rename(radiationIncomingPAR_umolm2s = PAR)

#### Air Temp ####
range(buoy2008_L1$AirTempC, na.rm = T)

ggplot(buoy2008_L1, aes(x=datetime, y = AirTempC)) +
  geom_point() +
  final_theme +
  labs(title = '2008 air temp raw',
       x= NULL,
       y= 'air temp (deg C)') +
  scale_x_datetime(date_minor_breaks = '1 month')

# ggplot(subset(buoy2008_L1,
#               subset=(datetime>=as.POSIXct('2008-12-01', tz = buoy_tz) &
#                         datetime < as.POSIXct('2009-01-01', tz = buoy_tz))),
#        aes(x=datetime, y = AirTempC)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'dec 2008 air temp raw',
#        x= NULL,
#        y= 'air temp (deg C)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# #dec 24-25
# ggplot(subset(buoy2008_L1,
#               subset=(datetime>=as.POSIXct('2008-12-24 12:00', tz = buoy_tz) &
#                         datetime < as.POSIXct('2008-12-25 12:00', tz = buoy_tz))),
#        aes(x=datetime, y = AirTempC)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'dec 2008 air temp raw',
#        x= NULL,
#        y= 'air temp (deg C)') +
#   scale_x_datetime(date_minor_breaks = '1 hour')
# 
# ggplot(subset(buoy2008_L1,
#               subset=(datetime>=as.POSIXct('2008-12-29', tz = buoy_tz) &
#                         datetime < as.POSIXct('2008-12-30', tz = buoy_tz))),
#        aes(x=datetime, y = AirTempC)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'dec 2008 air temp raw',
#        x= NULL,
#        y= 'air temp (deg C)') +
#   scale_x_datetime(date_minor_breaks = '1 hour')

buoy2008_L1 <- buoy2008_L1 %>% 
  mutate(AirTempC = case_when(datetime >= as.POSIXct('2008-12-24 22:00', tz = buoy_tz) &
                                datetime < as.POSIXct('2008-12-25', tz = buoy_tz) ~ NA_real_,
                              datetime == as.POSIXct('2008-12-25 1:30', tz = buoy_tz) ~ NA_real_,
                              TRUE ~ AirTempC))

#add flag for dec 29 on as intermittent
buoy2008_L1 <- buoy2008_L1 %>% 
  mutate(flag_airtemp = case_when(datetime >= as.POSIXct('2008-12-29', tz = buoy_tz) ~ 'i',
                                  TRUE ~ ''))

ggplot(buoy2008_L1, aes(x = datetime, y = AirTempC)) +
  geom_point() +
  final_theme +
  labs(title = '2008 air temp clean',
       x= NULL,
       y= 'air temp (deg C)') +
  scale_x_datetime(date_minor_breaks = '1 month')

#rename with CV
buoy2008_L1 <- buoy2008_L1 %>% 
  rename(airTemperature_degC = AirTempC)


#### Relative Humidity ####
range(buoy2008_L1$RH, na.rm = T)

ggplot(buoy2008_L1, aes(x=datetime, y = RH)) +
  geom_point() +
  final_theme +
  labs(title = '2008 relative humidity raw',
       x= NULL,
       y= 'relative humidity (percent)') +
  scale_x_datetime(date_minor_breaks = '1 month')

#not relative humidity. recode and do not export
buoy2008_L1 <- buoy2008_L1 %>% 
  mutate(RH = NA_real_)


#### EXPORT L1 FILES ####
colnames(buoy2008_L1)

#export L1 tempstring file
buoy2008_L1 %>%
  select(datetime, location, 
         waterTemperature_degC_0p5m:waterTemperature_degC_13p5m, 
         flag_temp9p5m, flag_temp10p5m, flag_temp11p5m, flag_temp13p5m) %>% 
  mutate(datetime = as.character(datetime)) %>%
  write_csv(., file.path(dump_dir, 'tempstring/2008_tempstring_L1_v2022.csv'))

# export L1 DO file
buoy2008_L1 %>%
  select(datetime, location, 
         oxygenDissolved_mgl_1p5m, oxygenDissolvedPercentOfSaturation_pct_1p5m, waterTemperature_DO_degC_1p5m,
         flag_do1p5m) %>%
  mutate(datetime = as.character(datetime)) %>%
  write_csv(., file.path(dump_dir,'do/2008_do_L1_v2022.csv'))

# export met data
buoy2008_L1 %>%
  select(datetime, location, 
         windDirection_deg, windSpeed_mps, flag_winddir, 
         radiationIncomingPAR_umolm2s, flag_par, 
         airTemperature_degC, flag_airtemp) %>%
  mutate(datetime = as.character(datetime)) %>%
  write_csv(., file.path(dump_dir, 'met/2008_met_L1_v2022.csv'))
