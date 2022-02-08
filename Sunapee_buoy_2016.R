#*****************************************************************
#*      Cary Institute of Ecosystem Studies (Millbrook, NY)      *
#*                                                               *
#* TITLE:   Sunapee_buoy_2016.r                                  *
#* AUTHOR:  Bethel Steele                                        *
#* PROJECT: Lake Sunapee Buoy Data Cleaning                      *
#*****************************************************************

source('library_func_lists.R')

#point to data directories
raw_dir = 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L0/'
dump_dir = 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/'

#set tz
buoy_tz = 'Etc/GMT+5'

#bring in 2016 buoy raw data
buoy2016_L0 <- read.csv(file.path(raw_dir, '2016 Buoy Data.csv'))

#### format data ####
buoy2016_L0 <- buoy2016_L0  %>%
  rename(DOLowTempC = 'DOLoTempC',
         AveWindSp = 'WindSpdAv',
         AveWindDir = 'WindVect',
         MaxWindSp = 'MaxWind') %>% 
  mutate(hour = Hr.Min%/%100,
         minutes = Hr.Min%%100,
         time = paste(hour, minutes, sep=':')) %>% #break out time from Hr.Min, create time column
  mutate(date = as.Date(paste(Day, Year, sep = '-'), format='%j-%Y'), #create date in ymd format
         datetime = as.POSIXct(paste(date, time, sep=' '), format='%Y-%m-%d %H:%M', tz=buoy_tz)) %>%  #tibble forces to UTC, so coerce for our uses.
  select(-hour, -minutes, -Hr.Min, -Year, -Day, -time, -ArrayID) %>% #remove unnecessary columns
  rowid_to_column('rowid')

#plot the battery levels
ggplot(buoy2016_L0, aes(x = datetime, y = LoggerBatV)) +
  geom_point()

ggplot(buoy2016_L0, aes(x = datetime, y = RadioBatV)) +
  geom_point()
#these look fine

#double check to make sure there are no DST issues
datelength2016 <- buoy2016_L0 %>% 
  mutate(date = format(datetime, '%Y-%m-%d')) %>% 
  group_by(date) %>% 
  summarize(length(datetime))

#check dst dates
datelength2016[datelength2016$date == '2016-03-13',]
#dst observed here
datelength2016[datelength2016$date == '2016-11-06',]
#dst observed on the 07th
datelength2016[datelength2016$date == '2016-11-07',]

# see where dst occurs
buoy2016_L0 %>% 
  filter(datetime >= as.POSIXct('2016-03-13', tz=buoy_tz) & 
           datetime < as.POSIXct('2016-03-14', tz=buoy_tz)) %>% 
  select(datetime, rowid)
buoy2016_L0 %>% 
  filter(datetime >= as.POSIXct('2016-11-07', tz=buoy_tz) & 
           datetime < as.POSIXct('2016-11-08', tz=buoy_tz)) %>% 
  select(datetime, rowid)


#DST observed at odd times - 03-08-15 23:00; 11-02-15 00:00
buoy2016_L1 <- buoy2016_L0 %>% 
  mutate(datetime.instrument = datetime)
buoy2016_L1a <- buoy2016_L1 %>% 
  filter(rowid <= 10503)
buoy2016_L1b <- buoy2016_L1 %>% 
  filter(rowid > 10503 & rowid <= 44774)
buoy2016_L1c <- buoy2016_L1 %>% 
  filter(rowid > 44774)
#apply time math on middle section
buoy2016_L1b <- buoy2016_L1b %>% 
  mutate(datetime = datetime-hours(1))

#rejoin and force tz as buoytz
buoy2016_L1 <- full_join(buoy2016_L1a, buoy2016_L1b) %>% 
  full_join(., buoy2016_L1c) %>% 
  mutate(datetime = as.POSIXct(datetime, tz = buoy_tz))

#double check to make sure there are no DST issues
datelength2016 <- buoy2016_L1 %>% 
  mutate(date = format(datetime, '%Y-%m-%d')) %>% 
  group_by(date) %>% 
  summarize(length(datetime))

datelength2016[datelength2016$date == '2016-03-13',]
datelength2016[datelength2016$date == '2016-11-06',]
datelength2016[datelength2016$date == '2016-11-07',]
#all good!

# add in all date time options in L1 data set
alltimes_2016 <- as.data.frame(seq.POSIXt(as.POSIXct('2016-01-01 00:00', tz=buoy_tz), as.POSIXct('2016-12-31 23:50', tz=buoy_tz), '10 min')) %>% 
  rename("datetime" = !!names(.[1]))

buoy2016_L1 <- buoy2016_L1 %>% 
  right_join(., alltimes_2016) %>% 
  arrange(datetime)


####thermistors####
buoy2016_vert_temp <- buoy2016_L1 %>% 
  select(datetime, all_of(alltemp2011)) %>% 
  pivot_longer(names_to = 'variable', values_to = 'value', -datetime) %>% 
  mutate(variable = factor(variable, levels=alltemp))

# ggplot(subset(buoy2016_vert_temp, 
#               subset=(datetime>=as.POSIXct('2016-01-01', tz=buoy_tz) & datetime < as.POSIXct('2017-01-01', tz=buoy_tz))), 
#        aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='2016, raw', x='date', y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme

buoy2016_L1 <- buoy2016_L1 %>% 
  mutate_at(vars(TempC_0m:TempC_9m),
            ~(case_when(. == -6999 ~ NA_real_,
                           . >= 555.4 ~ NA_real_,
                           TRUE ~ .)))

buoy2016_vert_temp <- buoy2016_L1 %>%
  select(datetime, all_of(alltemp2011)) %>% 
  pivot_longer(names_to = 'variable', values_to = 'value', -datetime) %>% 
  mutate(variable = factor(variable, levels=alltemp))

# ggplot(subset(buoy2016_vert_temp, subset=(datetime>=as.POSIXct('2016-01-01', tz=buoy_tz) & datetime < as.POSIXct('2017-01-01', tz=buoy_tz))), aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='2016, NA values recoded', x='date', y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme

#look at 0m thermistor
ggplot(buoy2016_L1, aes(x = datetime, y = TempC_0m)) +
  geom_point()

hist(buoy2016_L1$TempC_0m)

#see if we can filter out some of the noise
buoy2016_L1 <- buoy2016_L1 %>% 
  mutate(TempC_0m = case_when(TempC_0m >30 ~ NA_real_,
                              TempC_0m == 9| TempC_0m == 15 |TempC_0m <=5 ~ NA_real_,
                              TRUE ~ TempC_0m))

ggplot(buoy2016_L1, aes(x = datetime, y = TempC_0m)) +
  geom_point()

# add flag for 0m as intermittent
buoy2016_L1 <- buoy2016_L1 %>% 
  mutate(flag_temp0p5m = 'i')

buoy2016_vert_temp <- buoy2016_L1 %>%
  select(datetime, all_of(alltemp2011)) %>% 
  pivot_longer(names_to = 'variable', values_to = 'value', -datetime) %>% 
  mutate(variable = factor(variable, levels=alltemp))

ggplot(subset(buoy2016_vert_temp, subset=(datetime>=as.POSIXct('2016-01-01', tz=buoy_tz) & datetime < as.POSIXct('2017-01-01', tz=buoy_tz))), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='2016, 0m recoded', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 month')

# ggplot(subset(buoy2016_vert_temp, subset=(datetime>=as.POSIXct('2016-04-01', tz=buoy_tz) & datetime < as.POSIXct('2016-05-01', tz=buoy_tz))), aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='Apr 2016', x='date', y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2016_vert_temp, subset=(datetime>=as.POSIXct('2016-05-01', tz=buoy_tz) & datetime < as.POSIXct('2016-06-01', tz=buoy_tz))), aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='May 2016', x='date', y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme+
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# #recode therm data to NA prior to move to loon - the depths are not correct - suspect temp not in water. 5-3-16 11am
# 
# ggplot(subset(buoy2016_vert_temp, subset=(datetime>=as.POSIXct('2016-05-03', tz=buoy_tz) & datetime < as.POSIXct('2016-05-04', tz=buoy_tz))), aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='May move 2016', x='date', y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour')

buoy2016_L1 <- buoy2016_L1 %>%
  mutate_at(vars(TempC_0m:TempC_9m),
            ~(case_when(datetime <= as.POSIXct('2016-05-03 11:00', tz=buoy_tz) ~ NA_real_,
                           TRUE ~ .)))

buoy2016_vert_temp_L1 <- buoy2016_L1 %>%
  select(datetime, all_of(alltemp2011)) %>% 
  pivot_longer(names_to = 'variable', values_to = 'value', -datetime) %>% 
  mutate(variable = factor(variable, levels=alltemp))

# ggplot(subset(buoy2016_vert_temp_L1, subset=(datetime>=as.POSIXct('2016-05-01', tz=buoy_tz) & datetime < as.POSIXct('2016-06-01', tz=buoy_tz))), aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='May 2016, v2', x='date', y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# #errant data on may 18 and 19
# ggplot(subset(buoy2016_vert_temp_L1, subset=(datetime>=as.POSIXct('2016-05-18', tz=buoy_tz) & datetime < as.POSIXct('2016-05-19', tz=buoy_tz))), aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='mid May 2016', x='date', y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour')
# 
# ggplot(subset(buoy2016_vert_temp_L1, subset=(datetime>=as.POSIXct('2016-05-19', tz=buoy_tz) & datetime < as.POSIXct('2016-05-20', tz=buoy_tz))), aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='mid May 2016', x='date', y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme+
#   scale_x_datetime(date_minor_breaks = '1 hour')
 
buoy2016_L1 <- buoy2016_L1 %>% 
  mutate_at(vars(TempC_0m:TempC_9m),
            ~(case_when(datetime == as.POSIXct('2016-05-18 10:00', tz=buoy_tz) ~ NA_real_,
                           datetime == as.POSIXct('2016-05-19 4:10', tz=buoy_tz) ~ NA_real_,
                           TRUE ~ .)))
buoy2016_vert_temp_L1 <- buoy2016_L1 %>%
  select(datetime, all_of(alltemp2011)) %>% 
  pivot_longer(names_to = 'variable', values_to = 'value', -datetime) %>% 
  mutate(variable = factor(variable, levels=alltemp))


# ggplot(subset(buoy2016_vert_temp_L1, subset=(datetime>=as.POSIXct('2016-05-01', tz=buoy_tz) & datetime < as.POSIXct('2016-06-01', tz=buoy_tz))), aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='May 2016, v3', x='date', y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2016_vert_temp, subset=(datetime>=as.POSIXct('2016-06-01', tz=buoy_tz) & datetime < as.POSIXct('2016-07-01', tz=buoy_tz))), aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='June 2016', x='date', y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme+
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# #buoy visit June 7 8:10
# ggplot(subset(buoy2016_vert_temp, subset=(datetime>=as.POSIXct('2016-06-07', tz=buoy_tz) & datetime < as.POSIXct('2016-06-08', tz=buoy_tz))), aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='June 7 2016', x='date', y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme
# #looks okay - a little wonky at 6m, but good otherwise
# 
# #buoy visit June 14 8:15
# ggplot(subset(buoy2016_vert_temp, subset=(datetime>=as.POSIXct('2016-06-14', tz=buoy_tz) & datetime < as.POSIXct('2016-06-15', tz=buoy_tz))), aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='June 14 2016', x='date', y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme+
#   scale_x_datetime(date_minor_breaks = '1 hour')
# #looks fine, no artifacts
# 
# #buoy visit June 15 10:50a
# ggplot(subset(buoy2016_vert_temp, subset=(datetime>=as.POSIXct('2016-06-15', tz=buoy_tz) & datetime < as.POSIXct('2016-06-16', tz=buoy_tz))), aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='June 15 2016', x='date', y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme+
#   scale_x_datetime(date_minor_breaks = '1 hour')

#remove artifacts
buoy2016_L1 <- buoy2016_L1 %>% 
  mutate_at(vars(TempC_0m:TempC_9m),
            ~(case_when(datetime == as.POSIXct('2016-06-15 10:00', tz=buoy_tz) ~ NA_real_,
                           TRUE ~ .)))
buoy2016_vert_temp_L1 <- buoy2016_L1 %>%
  select(datetime, all_of(alltemp2011)) %>% 
  pivot_longer(names_to = 'variable', values_to = 'value', -datetime) %>% 
  mutate(variable = factor(variable, levels=alltemp))

# ggplot(subset(buoy2016_vert_temp_L1, subset=(datetime>=as.POSIXct('2016-06-15 09:00', tz=buoy_tz) & datetime < as.POSIXct('2016-06-15 13:00', tz=buoy_tz))), aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='June 15 2016, v2', x='date', y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme+
#   scale_x_datetime(date_minor_breaks = '1 hour')
# 
# #buoy visit June 17 8:30
# ggplot(subset(buoy2016_vert_temp, subset=(datetime>=as.POSIXct('2016-06-17', tz=buoy_tz) & datetime < as.POSIXct('2016-06-18', tz=buoy_tz))), aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='June 17 2016', x='date', y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme+
#   scale_x_datetime(date_minor_breaks = '1 hour')
# #looks fine, no artifacts
# 
# #buoy visit June 20 8:15
# ggplot(subset(buoy2016_vert_temp, subset=(datetime>=as.POSIXct('2016-06-20', tz=buoy_tz) & datetime < as.POSIXct('2016-06-21', tz=buoy_tz))), aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='June 20 2016', x='date', y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme+
#   scale_x_datetime(date_minor_breaks = '1 hour')
# #looks fine, no artifacts
# 
# ggplot(subset(buoy2016_vert_temp, subset=(datetime>=as.POSIXct('2016-07-01', tz=buoy_tz) & datetime < as.POSIXct('2016-08-01', tz=buoy_tz))), aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='July 2016', x='date', y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme+
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2016_vert_temp, subset=(datetime>=as.POSIXct('2016-08-01', tz=buoy_tz) & datetime < as.POSIXct('2016-09-01', tz=buoy_tz))), aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='Aug 2016', x='date', y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme+
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2016_vert_temp, subset=(datetime>=as.POSIXct('2016-09-01', tz=buoy_tz) & datetime < as.POSIXct('2016-10-01', tz=buoy_tz))), aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='Sept 2016', x='date', y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme+
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2016_vert_temp, subset=(datetime>=as.POSIXct('2016-10-01', tz=buoy_tz) & datetime < as.POSIXct('2016-11-01', tz=buoy_tz))), aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='Oct 2016', x='date', y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme+
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# #buoy moved to harbor Oct 12 9:30
# ggplot(subset(buoy2016_vert_temp, subset=(datetime>=as.POSIXct('2016-10-12', tz=buoy_tz) & datetime < as.POSIXct('2016-10-13', tz=buoy_tz))), aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='Oct 12 2016', x='date', y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme+
#   scale_x_datetime(date_minor_breaks = '1 hour')
# #no artifacts of buoy move
# 
# ggplot(subset(buoy2016_vert_temp_L1, 
#               subset=(datetime>=as.POSIXct('2016-01-01', tz=buoy_tz) & 
#                         datetime < as.POSIXct('2017-01-01', tz=buoy_tz))), 
#        aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='2016, clean', x='date', y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme

# recode that wonky 0m point after move
buoy2016_L1 <- buoy2016_L1 %>% 
  mutate(TempC_0m = case_when(datetime >as.Date('2016-11-01') ~ NA_real_,
                              TRUE ~ TempC_0m))
buoy2016_vert_temp_L1 <- buoy2016_L1 %>%
  select(datetime, all_of(alltemp2011)) %>% 
  pivot_longer(names_to = 'variable', values_to = 'value', -datetime) %>% 
  mutate(variable = factor(variable, levels=alltemp))

ggplot(subset(buoy2016_vert_temp_L1, 
              subset=(datetime>=as.POSIXct('2016-01-01', tz=buoy_tz) & 
                        datetime < as.POSIXct('2017-01-01', tz=buoy_tz))), 
       aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='2016, clean', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  final_theme

#correct for thermistor offset and add CV
buoy2016_L1 <- buoy2016_L1 %>% 
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

rm(buoy2016_vert_temp, buoy2016_vert_temp_L1)

####DO####
buoy2016_vert_do <- buoy2016_L1 %>% 
  select(datetime, all_of(upDO), all_of(lowDO)) %>%
  pivot_longer(names_to = 'variable', values_to = 'value', -datetime)

# ggplot(subset(buoy2016_vert_do, 
#               subset=(datetime>=as.POSIXct('2016-01-01', tz=buoy_tz) & datetime < as.POSIXct('2017-01-01', tz=buoy_tz))), 
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='2016, raw', x='date', y='') +
#   final_theme

buoy2016_L1 <- buoy2016_L1 %>% 
  mutate_at(vars(all_of(upDO), all_of(lowDO)),
            ~(case_when(. == -6999 ~ NA_real_,
                           TRUE ~ .)))
buoy2016_vert_do <- buoy2016_L1 %>% 
  select(datetime, all_of(upDO), all_of(lowDO)) %>%
  pivot_longer(names_to = 'variable', values_to = 'value', -datetime)

ggplot(subset(buoy2016_vert_do, 
              subset=(datetime>=as.POSIXct('2016-01-01', tz=buoy_tz) & datetime < as.POSIXct('2017-01-01', tz=buoy_tz))), 
       aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title='2016, NA values recoded', x='date', y='') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 month')

# #remove data prior to sensor deployment 4-19-16 10:00
# ggplot(subset(buoy2016_vert_do,
#               subset=(datetime>=as.POSIXct('2016-04-19', tz=buoy_tz) & datetime < as.POSIXct('2016-04-20', tz=buoy_tz))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='Apr 19 2016', x='date', y='') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour')

buoy2016_L1 <- buoy2016_L1 %>% 
  mutate_at(vars(all_of(upDO)),
            ~(case_when(datetime <= as.POSIXct('2016-04-19 10:30') ~ NA_real_, #equilibration required more time after deployment
                           TRUE ~ .))) %>% 
  mutate(flag_do1p5m = case_when(datetime == as.POSIXct('2016-04-19 10:40') ~ 'c',
                                   TRUE  ~ ''))
#low do no at proper depth, will recode to NA once at buoy move

buoy2016_vert_do_L1 <- buoy2016_L1 %>% 
  select(datetime, all_of(upDO), all_of(lowDO)) %>%
  pivot_longer(names_to = 'variable', values_to = 'value', -datetime)

# ggplot(subset(buoy2016_vert_do_L1,
#               subset=(datetime>=as.POSIXct('2016-04-01', tz=buoy_tz) & datetime < as.POSIXct('2016-05-01', tz=buoy_tz))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='apr 2016, clean', x='date', y='') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2016_vert_do_L1,
#               subset=(datetime>=as.POSIXct('2016-05-01', tz=buoy_tz) & datetime < as.POSIXct('2016-06-01', tz=buoy_tz))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='may 2016, clean', x='date', y='') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2016_vert_do_L1,
#               subset=(datetime>=as.POSIXct('2016-05-03', tz=buoy_tz) & datetime < as.POSIXct('2016-05-04', tz=buoy_tz))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='may 2016, clean', x='date', y='') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour')

buoy2016_L1 <- buoy2016_L1 %>% 
  mutate(flag_do10p5m = '') %>% 
  mutate_at(vars(all_of(upDO)),
            ~(case_when(datetime >= as.POSIXct('2016-05-03 9:10', tz=buoy_tz) & 
                          datetime < as.POSIXct('2016-05-03 11:00', tz=buoy_tz) ~ NA_real_, #equilibration required more time after deployment
                           TRUE ~ .))) %>% 
  mutate_at(vars(DOSat, DOppm),
            ~(case_when(datetime >= as.POSIXct('2016-05-03 11:00', tz=buoy_tz) & 
                          datetime < as.POSIXct('2016-05-03 11:40', tz=buoy_tz) ~ NA_real_, #equilibration required more time after deployment
                           TRUE ~ .))) %>% 
  mutate_at(vars(all_of(lowDO)),
            ~(case_when(datetime < as.POSIXct('2016-05-03 11:00', tz=buoy_tz) ~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate(location = case_when(datetime < as.POSIXct('2016-05-03 9:10', tz=buoy_tz) ~ 'harbor',
                              datetime >= as.POSIXct('2016-05-03 9:10', tz=buoy_tz) & 
                                datetime < as.POSIXct('2016-05-03 11:00', tz=buoy_tz) ~ 'in transit',
                              TRUE ~ 'loon')) %>% 
  mutate(flag_do1p5m = case_when(datetime == as.POSIXct('2016-05-03 11:00', tz=buoy_tz) ~ 'w',
                                   TRUE ~ flag_do1p5m)) %>% 
  mutate(flag_do10p5m = case_when(datetime == as.POSIXct('2016-05-03 11:00', tz=buoy_tz) ~ 'c',
                                   TRUE ~ flag_do10p5m))
  
buoy2016_vert_do_L1 <- buoy2016_L1 %>% 
  select(datetime, all_of(upDO), all_of(lowDO), location) %>%
  pivot_longer(names_to = 'variable', values_to = 'value', -c(datetime, location))

# ggplot(subset(buoy2016_vert_do_L1,
#               subset=(datetime>=as.POSIXct('2016-05-01', tz=buoy_tz) & datetime < as.POSIXct('2016-06-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color = location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='may 2016, clean', x='date', y='') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_colorblind()
# 
# # May 18 and 21
# ggplot(subset(buoy2016_vert_do_L1,
#               subset=(datetime>=as.POSIXct('2016-05-18', tz=buoy_tz) & datetime < as.POSIXct('2016-05-19', tz=buoy_tz))),
#        aes(x=datetime, y=value, color = location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='may 2016, clean', x='date', y='') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy2016_vert_do_L1,
#               subset=(datetime>=as.POSIXct('2016-05-19', tz=buoy_tz) & datetime < as.POSIXct('2016-05-20', tz=buoy_tz))),
#        aes(x=datetime, y=value, color = location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='may 2016, clean', x='date', y='') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy2016_vert_do_L1,
#               subset=(datetime>=as.POSIXct('2016-05-21', tz=buoy_tz) & datetime < as.POSIXct('2016-05-22', tz=buoy_tz))),
#        aes(x=datetime, y=value, color = location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='may 2016, clean', x='date', y='') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_colorblind()

#may 18 11:00 low do
#may 19 5:10 low do
#may 21 14:10 low do
buoy2016_L1 <- buoy2016_L1 %>% 
  mutate_at(vars(lowDO),
            ~(case_when(datetime == as.POSIXct('2016-05-18 10:00', tz=buoy_tz) ~ NA_real_,
                           datetime == as.POSIXct('2016-05-19 4:10', tz=buoy_tz) ~ NA_real_,
                           datetime == as.POSIXct('2016-05-21 13:10', tz=buoy_tz) ~ NA_real_,
                           TRUE ~ .))) 
buoy2016_vert_do_L1 <- buoy2016_L1 %>% 
  select(datetime, all_of(upDO), all_of(lowDO), location) %>%
  pivot_longer(names_to = 'variable', values_to = 'value', -c(datetime, location))

# ggplot(subset(buoy2016_vert_do_L1,
#               subset=(datetime>=as.POSIXct('2016-05-01', tz=buoy_tz) & datetime < as.POSIXct('2016-06-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color = location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='may 2016, clean', x='date', y='') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy2016_vert_do_L1,
#               subset=(datetime>=as.POSIXct('2016-06-01', tz=buoy_tz) & datetime < as.POSIXct('2016-07-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color = location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='jun 2016, NAs recoded', x='date', y='') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_colorblind()

#do low sat errant when below 25% - recode sat and ppm at those times
buoy2016_L1 <- buoy2016_L1 %>% 
  mutate_at(vars(DOLowPPM, DOLowSat),
            ~(case_when(datetime >= as.POSIXct('2016-06-01', tz=buoy_tz) & 
                          datetime < as.POSIXct('2016-06-14', tz=buoy_tz) & DOLowSat<25 ~ NA_real_,
                           TRUE ~ .))) 
buoy2016_vert_do_L1 <- buoy2016_L1 %>% 
  select(datetime, all_of(upDO), all_of(lowDO), location) %>%
  pivot_longer(names_to = 'variable', values_to = 'value', -c(datetime, location))

# ggplot(subset(buoy2016_vert_do_L1,
#               subset=(datetime>=as.POSIXct('2016-06-01', tz=buoy_tz) & datetime < as.POSIXct('2016-07-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color = location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='jun 2016, clean', x='date', y='') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_colorblind()

#do data intermittent from may 22 until jun 13 - flag as such
buoy2016_L1 <- buoy2016_L1 %>% 
  mutate_at(vars(flag_do1p5m, flag_do10p5m),
            ~(case_when(datetime >= as.POSIXct('2016-05-22', tz=buoy_tz) & datetime < as.POSIXct('2016-06-14', tz=buoy_tz) ~ 'i',
                           TRUE ~ .))) 

# #do cleaned Jun 7
# ggplot(subset(buoy2016_vert_do_L1,
#               subset=(datetime>=as.POSIXct('2016-06-07', tz=buoy_tz) & datetime < as.POSIXct('2016-06-08', tz=buoy_tz))),
#        aes(x=datetime, y=value, color = location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='jun 2016, clean', x='date', y='') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_colorblind()

buoy2016_L1 <- buoy2016_L1 %>% 
  mutate_at(vars(flag_do1p5m, flag_do10p5m),
            ~(case_when(datetime == as.POSIXct('2016-06-07 7:20', tz=buoy_tz) ~ 'i; w',
                           TRUE ~ .))) 

# ggplot(subset(buoy2016_vert_do_L1,
#               subset=(datetime>=as.POSIXct('2016-07-01', tz=buoy_tz) & datetime < as.POSIXct('2016-08-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color = location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='jul 2016, clean', x='date', y='') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy2016_vert_do_L1,
#               subset=(datetime>=as.POSIXct('2016-08-01', tz=buoy_tz) & datetime < as.POSIXct('2016-09-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color = location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='aug 2016, clean', x='date', y='') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy2016_vert_do_L1,
#               subset=(datetime>=as.POSIXct('2016-09-01', tz=buoy_tz) & datetime < as.POSIXct('2016-10-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color = location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='sept 2016, clean', x='date', y='') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy2016_vert_do_L1,
#               subset=(datetime>=as.POSIXct('2016-10-01', tz=buoy_tz) & datetime < as.POSIXct('2016-11-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color = location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='oct 2016, clean', x='date', y='') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy2016_vert_do_L1,
#               subset=(datetime>=as.POSIXct('2016-10-12', tz=buoy_tz) & datetime < as.POSIXct('2016-10-13', tz=buoy_tz))),
#        aes(x=datetime, y=value, color = location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='oct 2016, clean', x='date', y='') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_colorblind()

#remove buoy move artifacts of move and add location flag
buoy2016_L1 <- buoy2016_L1 %>% 
  mutate_at(vars(all_of(upDO), all_of(lowDO)),
            ~(case_when(datetime >= as.POSIXct('2016-10-12 8:50', tz=buoy_tz) & datetime < as.POSIXct('2016-10-12 11:40', tz=buoy_tz) ~ NA_real_, #equilibration required more time after deployment
                           TRUE ~ .))) %>% 
  mutate(location = case_when(datetime >= as.POSIXct('2016-10-12 8:50', tz=buoy_tz) & datetime < as.POSIXct('2016-10-12 10:00', tz=buoy_tz) ~ 'in transit',
                              datetime >= as.POSIXct('2016-10-12 10:00', tz=buoy_tz) ~ 'harbor',
                              TRUE ~ location)) %>% 
  mutate(flag_do1p5m = case_when(datetime == as.POSIXct('2016-10-12 10:00', tz=buoy_tz) ~ 'wp',
                                   TRUE ~ flag_do1p5m))
buoy2016_vert_do_L1 <- buoy2016_L1 %>% 
  select(datetime, all_of(upDO), all_of(lowDO), location) %>%
  pivot_longer(names_to = 'variable', values_to = 'value', -c(datetime, location))

# ggplot(subset(buoy2016_vert_do_L1,
#               subset=(datetime>=as.POSIXct('2016-10-01', tz=buoy_tz) & datetime < as.POSIXct('2016-11-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color = location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='oct 2016, clean', x='date', y='') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy2016_vert_do_L1,
#               subset=(datetime>=as.POSIXct('2016-11-01', tz=buoy_tz) & datetime < as.POSIXct('2016-12-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color = location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='nov 2016, clean', x='date', y='') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy2016_vert_do_L1,
#               subset=(datetime>=as.POSIXct('2016-12-01', tz=buoy_tz) & datetime < as.POSIXct('2017-01-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color = location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='dec 2016, clean', x='date', y='') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_colorblind()

ggplot(buoy2016_vert_do_L1,
       aes(x=datetime, y=value, color = location)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title='2016, clean', x='date', y='') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 month') +
  scale_color_colorblind()

buoy2016_vert_updo <- buoy2016_L1 %>% 
  select(datetime, all_of(upDO), flag_do1p5m, location) %>% 
  pivot_longer(names_to = 'variable', values_to = 'value', -c(datetime, location, flag_do1p5m))

ggplot(buoy2016_vert_updo,
       aes(x=datetime, y=value, color = flag_do1p5m, shape = location)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title='2016, clean', x='date', y='') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 month') +
  scale_color_colorblind()


buoy2016_vert_lowdo <- buoy2016_L1 %>% 
  select(datetime, all_of(lowDO), flag_do10p5m, location) %>% 
  pivot_longer(names_to = 'variable', values_to = 'value', -c(datetime, location, flag_do10p5m))

ggplot(buoy2016_vert_lowdo,
       aes(x=datetime, y=value, color = flag_do10p5m, shape = location)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title='2016, clean', x='date', y='') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 month') +
  scale_color_colorblind()

#rename with CV
buoy2016_L1 <- buoy2016_L1 %>% 
  rename(oxygenDissolved_mgl_1p5m = DOppm,
         oxygenDissolvedPercentOfSaturation_pct_1p5m = DOSat,
         waterTemperature_DO_degC_1p5m = DOTempC,
         oxygenDissolved_mgl_10p5m = DOLowPPM,
         oxygenDissolvedPercentOfSaturation_pct_10p5m = DOLowSat,
         waterTemperature_DO_degC_10p5m = DOLowTempC)

rm(buoy2016_vert_do, buoy2016_vert_do_L1, buoy2016_vert_updo, buoy2016_vert_lowdo)


#### wind data ####
range(buoy2016_L1$AveWindSp, na.rm = T)
range(buoy2016_L1$AveWindDir, na.rm = T)
range(buoy2016_L1$MaxWindSp, na.rm = T)
range(buoy2016_L1$MaxWindDir, na.rm = T)

buoy2016_vert_wind <- buoy2016_L1 %>%
  select(datetime, location, AveWindDir, AveWindSp, MaxWindDir, MaxWindSp) %>%
  pivot_longer(names_to = 'variable', values_to = 'value', -c(datetime, location))

ggplot(buoy2016_vert_wind, 
  aes(x=datetime, y=value, col=location)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title='2016, raw', x='date', y='') +
  scale_color_colorblind() +
  final_theme

# ggplot(subset(buoy2016_vert_wind,
#               subset=(datetime>=as.POSIXct('2016-01-01', tz=buoy_tz) & datetime < as.POSIXct('2016-02-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='Jan 2016', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2016_vert_wind,
#               subset=(datetime>=as.POSIXct('2016-01-18', tz=buoy_tz) & datetime < as.POSIXct('2016-01-19', tz=buoy_tz))),
#        aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='Jan 2016', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour')

# flag jan 18 wind event as suspect
buoy2016_L1 <- buoy2016_L1 %>% 
  mutate(flag_allwind = (case_when(datetime >= as.POSIXct('2016-01-18 3:20', tz=buoy_tz) &
                             datetime < as.POSIXct('2016-01-18 9:50', tz=buoy_tz)  ~ 's',
                           TRUE ~ '')))
buoy2016_vert_wind_L1 <- buoy2016_L1 %>%
  select(datetime, location, AveWindDir, AveWindSp, MaxWindDir, MaxWindSp) %>%
  pivot_longer(names_to = 'variable', values_to = 'value', -c(datetime, location))


# ggplot(subset(buoy2016_vert_wind,
#               subset=(datetime>=as.POSIXct('2016-02-01', tz=buoy_tz) & datetime < as.POSIXct('2016-03-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='feb 2016', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# #forzen sesnor feb09
# ggplot(subset(buoy2016_vert_wind,
#               subset=(datetime>=as.POSIXct('2016-02-09', tz=buoy_tz) & datetime < as.POSIXct('2016-02-10', tz=buoy_tz))),
#        aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='Jan 2016', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour')
# 
# #frozen sensor feb 24
# ggplot(subset(buoy2016_vert_wind,
#               subset=(datetime>=as.POSIXct('2016-02-24', tz=buoy_tz) & datetime < as.POSIXct('2016-02-25', tz=buoy_tz))),
#        aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='Jan 2016', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour')

#select by feb 9 and 24 & max wind =0
buoy2016_L1 <- buoy2016_L1 %>% 
  mutate_at(vars(AveWindDir, AveWindSp, MaxWindDir, MaxWindSp),
            ~(case_when(datetime >= as.POSIXct('2016-02-09 00:10', tz=buoy_tz) &
                             datetime < as.POSIXct('2016-02-09 12:30', tz=buoy_tz) ~ NA_real_,
                           datetime >= as.POSIXct('2016-02-24 11:40', tz=buoy_tz) &
                             datetime < as.POSIXct('2016-02-24 17:40', tz=buoy_tz) ~ NA_real_,
                           TRUE ~ .)))
buoy2016_vert_wind_L1 <- buoy2016_L1 %>%
  select(datetime, location, AveWindDir, AveWindSp, MaxWindDir, MaxWindSp) %>%
  pivot_longer(names_to = 'variable', values_to = 'value', -c(datetime, location))

# ggplot(subset(buoy2016_vert_wind_L1,
#               subset=(datetime>=as.POSIXct('2016-02-01', tz=buoy_tz) & datetime < as.POSIXct('2016-03-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='feb 2016', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2016_vert_wind, 
#               subset=(datetime>=as.POSIXct('2016-03-01', tz=buoy_tz) & datetime < as.POSIXct('2016-04-01', tz=buoy_tz))), 
#        aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='mar 2016', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2016_vert_wind,
#               subset=(datetime>=as.POSIXct('2016-03-25', tz=buoy_tz) & datetime < as.POSIXct('2016-03-26', tz=buoy_tz))),
#        aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='mar 2016', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour')

buoy2016_L1 <- buoy2016_L1 %>% 
  mutate_at(vars(AveWindDir, AveWindSp, MaxWindDir, MaxWindSp),
            ~(case_when(datetime >= as.POSIXct('2016-03-25 8:10', tz=buoy_tz) &
                             datetime < as.POSIXct('2016-03-25 11:10', tz=buoy_tz) &
                             MaxWindSp == 0 ~ NA_real_,
                           TRUE ~ .)))
buoy2016_vert_wind_L1 <- buoy2016_L1 %>%
  select(datetime, location, AveWindDir, AveWindSp, MaxWindDir, MaxWindSp) %>%
  pivot_longer(names_to = 'variable', values_to = 'value', -c(datetime, location))

# ggplot(subset(buoy2016_vert_wind_L1,
#               subset=(datetime>=as.POSIXct('2016-03-01', tz=buoy_tz) & datetime < as.POSIXct('2016-04-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='mar 2016', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')

# ggplot(subset(buoy2016_vert_wind,
#               subset=(datetime>=as.POSIXct('2016-04-01', tz=buoy_tz) & datetime < as.POSIXct('2016-05-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='apr 2016', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2016_vert_wind,
#               subset=(datetime>=as.POSIXct('2016-05-01', tz=buoy_tz) & datetime < as.POSIXct('2016-06-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='may 2016', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')

buoy2016_L1 <- buoy2016_L1 %>% 
  mutate_at(vars(AveWindDir, AveWindSp, MaxWindDir, MaxWindSp),
            ~(case_when(location == 'in transit' ~ NA_real_,
                           TRUE ~ .)))
buoy2016_vert_wind_L1 <- buoy2016_L1 %>%
  select(datetime, location, AveWindDir, AveWindSp, MaxWindDir, MaxWindSp) %>%
  pivot_longer(names_to = 'variable', values_to = 'value', -c(datetime, location))

# ggplot(subset(buoy2016_vert_wind_L1,
#               subset=(datetime>=as.POSIXct('2016-05-01', tz=buoy_tz) & datetime < as.POSIXct('2016-06-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='may 2016', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2016_vert_wind,
#               subset=(datetime>=as.POSIXct('2016-06-01', tz=buoy_tz) & datetime < as.POSIXct('2016-07-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='jun 2016', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2016_vert_wind,
#               subset=(datetime>=as.POSIXct('2016-07-01', tz=buoy_tz) & datetime < as.POSIXct('2016-08-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='jul 2016', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2016_vert_wind,
#               subset=(datetime>=as.POSIXct('2016-08-01', tz=buoy_tz) & datetime < as.POSIXct('2016-09-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='aug 2016', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2016_vert_wind,
#               subset=(datetime>=as.POSIXct('2016-09-01', tz=buoy_tz) & datetime < as.POSIXct('2016-10-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='sept 2016', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2016_vert_wind_L1,
#               subset=(datetime>=as.POSIXct('2016-10-01', tz=buoy_tz) & datetime < as.POSIXct('2016-11-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='oct 2016', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2016_vert_wind,
#               subset=(datetime>=as.POSIXct('2016-11-01', tz=buoy_tz) & datetime < as.POSIXct('2016-12-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='nov 2016', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2016_vert_wind,
#               subset=(datetime>=as.POSIXct('2016-12-01', tz=buoy_tz) & datetime < as.POSIXct('2017-01-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='dec 2016', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# #sensor frozen 12/08 and 12/29
# ggplot(subset(buoy2016_vert_wind,
#               subset=(datetime>=as.POSIXct('2016-12-18', tz=buoy_tz) & datetime < as.POSIXct('2016-12-19', tz=buoy_tz))),
#        aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='dec 2016', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour')
# 
# ggplot(subset(buoy2016_vert_wind, 
#               subset=(datetime>=as.POSIXct('2016-12-29 12:00', tz=buoy_tz) & datetime < as.POSIXct('2016-12-31', tz=buoy_tz))), 
#        aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='dec 2016', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour')

buoy2016_L1 <- buoy2016_L1 %>% 
  mutate_at(vars(AveWindDir, AveWindSp, MaxWindDir, MaxWindSp),
            ~(case_when(datetime >= as.POSIXct('2016-12-18 3:40', tz=buoy_tz) &
                             datetime < as.POSIXct('2016-12-18 7:40', tz=buoy_tz) &
                             MaxWindSp == 0 ~ NA_real_,
                           datetime >= as.POSIXct('2016-12-29 19:40', tz=buoy_tz) &
                             datetime < as.POSIXct('2016-12-30 10:50', tz=buoy_tz) &
                             MaxWindSp == 0 ~ NA_real_,
                           TRUE ~ .)))
buoy2016_vert_wind_L1 <- buoy2016_L1 %>%
  select(datetime, location, AveWindDir, AveWindSp, MaxWindDir, MaxWindSp) %>%
  pivot_longer(names_to = 'variable', values_to = 'value', -c(datetime, location))

# ggplot(subset(buoy2016_vert_wind_L1,
#               subset=(datetime>=as.POSIXct('2016-12-01', tz=buoy_tz) & datetime < as.POSIXct('2017-01-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, col=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='dec 2016', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')

ggplot(buoy2016_vert_wind_L1,
       aes(x=datetime, y=value, col=location)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title='2016, qaqc', x='date', y='') +
  scale_color_colorblind() +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 day')

#rename with CV
buoy2016_L1 <- buoy2016_L1 %>% 
  rename(windDirectionAverage_deg = AveWindDir,
         windSpeedAverage_mps = AveWindSp,
         windGustDirection_deg = MaxWindDir,
         windGustSpeed_mps = MaxWindSp)

rm(buoy2016_vert_wind, buoy2016_vert_wind_L1)

# ####chlorophyll sensor####
## SENSOR DOES NOT FUNCTION PROPERLY ####
# # buoy2016_vert_chla <- buoy2016_L1 %>%
# #   select(datetime, location, chla) %>%
# #   gather(variable, value, -datetime, -location)
# # 
# # ggplot(buoy2016_vert_chla, 
# #        aes(x=datetime, y=value, col=location)) +
# #   geom_point() +
# #   facet_grid(variable ~ ., scales = 'free_y') +
# #   labs(title='2016', x='date', y='') +
# #   scale_color_colorblind() +
# #   final_theme
# 
# buoy2016_L1 <- buoy2016_L1 %>% 
#   mutate_at(vars(chla),
#             ~(case_when(. == -6999 ~ NA_real_,
#                            TRUE ~ .))) %>% 
#   mutate(chla_flag = case_when(Chlor_UGL > 10 ~ 's',
#                                TRUE ~ '')) %>% 
#   mutate(Chlor_UGL = case_when(Chlor_UGL<0 ~ 0,
#                                TRUE ~ Chlor_UGL))
# buoy2016_vert_chla <- buoy2016_L1 %>%
#   select(datetime, location, chla, chla_flag) %>%
#   gather(variable, value, -datetime, -location, -chla_flag)
# 
# ggplot(buoy2016_vert_chla, 
#        aes(x=datetime, y=value, col=location, shape = chla_flag)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='2016', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme
# 
# # ggplot(subset(buoy2016_vert_chla,
# #               subset=(datetime >= as.POSIXct('2016-04-01', tz=buoy_tz) & datetime < as.POSIXct('2016-05-01', tz=buoy_tz))),
# #        aes(x=datetime, y=value, col=location)) +
# #   geom_point() +
# #   facet_grid(variable ~ ., scales = 'free_y') +
# #   labs(title='2016', x='date', y='') +
# #   scale_color_colorblind() +
# #   final_theme +
# #   scale_x_datetime(date_minor_breaks = '1 day')
# #     
# # ggplot(subset(buoy2016_vert_chla,
# #               subset=(datetime >= as.POSIXct('2016-04-19', tz=buoy_tz) & datetime < as.POSIXct('2016-04-20', tz=buoy_tz))),
# #        aes(x=datetime, y=value, col=location)) +
# #   geom_point() +
# #   facet_grid(variable ~ ., scales = 'free_y') +
# #   labs(title='2016', x='date', y='') +
# #   scale_color_colorblind() +
# #   final_theme +
# #   scale_x_datetime(date_minor_breaks = '1 hour')
# 
# buoy2016_L1 <- buoy2016_L1 %>% 
#   mutate_at(vars(chla),
#             ~(case_when(datetime < as.POSIXct('2016-04-19 11:20', tz=buoy_tz) ~ NA_real_,
#                            TRUE ~ .))) %>% 
#   mutate(chla_flag = case_when(datetime == as.POSIXct('2016-04-19 11:20', tz=buoy_tz) ~ 'c',
#                                TRUE ~ '')) %>% 
#   mutate(cond_flag = case_when(datetime == as.POSIXct('2016-04-19 11:20', tz=buoy_tz) ~ 'c',
#                                TRUE ~ ''))
# buoy2016_vert_chla_L1 <- buoy2016_L1 %>%
#   select(datetime, location, chla) %>%
#   gather(variable, value, -datetime, -location)
# 
# # ggplot(subset(buoy2016_vert_chla_L1,
# #               subset=(datetime >= as.POSIXct('2016-04-01', tz=buoy_tz) & datetime < as.POSIXct('2016-05-01', tz=buoy_tz))),
# #        aes(x=datetime, y=value, col=location)) +
# #   geom_point() +
# #   facet_grid(variable ~ ., scales = 'free_y') +
# #   labs(title='2016', x='date', y='') +
# #   scale_color_colorblind() +
# #   final_theme +
# #   scale_x_datetime(date_minor_breaks = '1 day')
# # 
# # ggplot(subset(buoy2016_vert_chla,
# #               subset=(datetime >= as.POSIXct('2016-05-01', tz=buoy_tz) & datetime < as.POSIXct('2016-06-01', tz=buoy_tz))),
# #        aes(x=datetime, y=value, col=location)) +
# #   geom_point() +
# #   facet_grid(variable ~ ., scales = 'free_y') +
# #   labs(title='2016', x='date', y='') +
# #   scale_color_colorblind() +
# #   final_theme +
# #   scale_x_datetime(date_minor_breaks = '1 day')
# 
# buoy2016_L1 <- buoy2016_L1 %>% 
#   mutate_at(vars(chla),
#             ~(case_when(location == 'in transit' ~ NA_real_,
#                            TRUE ~ .))) 
# buoy2016_vert_chla_L1 <- buoy2016_L1 %>%
#   select(datetime, location, chla) %>%
#   gather(variable, value, -datetime, -location)
# 
# # ggplot(subset(buoy2016_vert_chla_L1,
# #               subset=(datetime >= as.POSIXct('2016-05-01', tz=buoy_tz) & datetime < as.POSIXct('2016-06-01', tz=buoy_tz))),
# #        aes(x=datetime, y=value, col=location)) +
# #   geom_point() +
# #   facet_grid(variable ~ ., scales = 'free_y') +
# #   labs(title='2016', x='date', y='') +
# #   scale_color_colorblind() +
# #   final_theme +
# #   scale_x_datetime(date_minor_breaks = '1 day')
# # 
# # #spec cond taken out early // check before buoy move
# # ggplot(subset(buoy2016_vert_chla_L1,
# #               subset=(datetime >= as.POSIXct('2016-05-03', tz=buoy_tz) & datetime < as.POSIXct('2016-05-04', tz=buoy_tz))),
# #        aes(x=datetime, y=value, col=location)) +
# #   geom_point() +
# #   facet_grid(variable ~ ., scales = 'free_y') +
# #   labs(title='2016', x='date', y='') +
# #   scale_color_colorblind() +
# #   final_theme +
# #   scale_x_datetime(date_minor_breaks = '1 hour')
# 
# buoy2016_L1 <- buoy2016_L1 %>% 
#   mutate(SpecCond = case_when(datetime >= as.POSIXct('2016-05-03 9:00', tz=buoy_tz) & datetime < as.POSIXct('2016-05-03 11:00') ~ NA_real_,
#                                TRUE ~ SpecCond)) %>% 
#   mutate_at(vars(cond_flag, chla_flag),
#             ~(case_when(datetime == as.POSIXct('2016-05-03 11:00') ~ 'w',
#                            TRUE ~ .)))
# buoy2016_vert_chla_L1 <- buoy2016_L1 %>%
#   select(datetime, location, chla) %>%
#   gather(variable, value, -datetime, -location)
# 
# # ggplot(subset(buoy2016_vert_chla_L1,
# #               subset=(datetime >= as.POSIXct('2016-05-01', tz=buoy_tz) & datetime < as.POSIXct('2016-06-01', tz=buoy_tz))),
# #        aes(x=datetime, y=value, col=location)) +
# #   geom_point() +
# #   facet_grid(variable ~ ., scales = 'free_y') +
# #   labs(title='2016', x='date', y='') +
# #   scale_color_colorblind() +
# #   final_theme +
# #   scale_x_datetime(date_minor_breaks = '1 day')
# # 
# # ggplot(subset(buoy2016_vert_chla,
# #               subset=(datetime >= as.POSIXct('2016-06-01', tz=buoy_tz) & datetime < as.POSIXct('2016-07-01', tz=buoy_tz))),
# #        aes(x=datetime, y=value, col=location)) +
# #   geom_point() +
# #   facet_grid(variable ~ ., scales = 'free_y') +
# #   labs(title='2016', x='date', y='') +
# #   scale_color_colorblind() +
# #   final_theme +
# #   scale_x_datetime(date_minor_breaks = '1 day')
# # 
# # #do cleaned Jun 7
# # ggplot(subset(buoy2016_vert_chla,
# #               subset=(datetime>=as.POSIXct('2016-06-07', tz=buoy_tz) & datetime < as.POSIXct('2016-06-08', tz=buoy_tz))),
# #        aes(x=datetime, y=value, color = location)) +
# #   geom_point() +
# #   facet_grid(variable ~ ., scales = 'free_y') +
# #   labs(title='jun 2016, clean', x='date', y='') +
# #   final_theme +
# #   scale_x_datetime(date_minor_breaks = '1 hour') +
# #   scale_color_colorblind()
# 
# #data intermittent from may 22 until jun 13 - flag as such; sensors impacted with do cleaning
# buoy2016_L1 <- buoy2016_L1 %>% 
#   mutate_at(vars(chla_flag, cond_flag),
#             ~(case_when(datetime >= as.POSIXct('2016-05-22', tz=buoy_tz) & datetime < as.POSIXct('2016-06-14', tz=buoy_tz) ~ 'i',
#                            TRUE ~ .))) %>% 
#   mutate_at(vars(chla),
#             ~(case_when(datetime == as.POSIXct('2016-06-07 8:20', tz=buoy_tz) ~ NA_real_,
#                            TRUE ~ .))) 
# buoy2016_vert_chla_L1 <- buoy2016_L1 %>%
#   select(datetime, location, chla) %>%
#   gather(variable, value, -datetime, -location)
# 
# # ggplot(subset(buoy2016_vert_chla_L1,
# #               subset=(datetime >= as.POSIXct('2016-06-01', tz=buoy_tz) & datetime < as.POSIXct('2016-07-01', tz=buoy_tz))),
# #        aes(x=datetime, y=value, col=location)) +
# #   geom_point() +
# #   facet_grid(variable ~ ., scales = 'free_y') +
# #   labs(title='2016', x='date', y='') +
# #   scale_color_colorblind() +
# #   final_theme +
# #   scale_x_datetime(date_minor_breaks = '1 day')
# # 
# # ggplot(subset(buoy2016_vert_chla,
# #               subset=(datetime >= as.POSIXct('2016-07-01', tz=buoy_tz) & datetime < as.POSIXct('2016-08-01', tz=buoy_tz))),
# #        aes(x=datetime, y=value, col=location)) +
# #   geom_point() +
# #   facet_grid(variable ~ ., scales = 'free_y') +
# #   labs(title='2016', x='date', y='') +
# #   scale_color_colorblind() +
# #   final_theme +
# #   scale_x_datetime(date_minor_breaks = '1 day')
# # 
# # ggplot(subset(buoy2016_vert_chla,
# #               subset=(datetime >= as.POSIXct('2016-08-01', tz=buoy_tz) & datetime < as.POSIXct('2016-09-01', tz=buoy_tz))),
# #        aes(x=datetime, y=value, col=location)) +
# #   geom_point() +
# #   facet_grid(variable ~ ., scales = 'free_y') +
# #   labs(title='2016', x='date', y='') +
# #   scale_color_colorblind() +
# #   final_theme +
# #   scale_x_datetime(date_minor_breaks = '1 day')
# # 
# # ggplot(subset(buoy2016_vert_chla,
# #               subset=(datetime >= as.POSIXct('2016-09-01', tz=buoy_tz) & datetime < as.POSIXct('2016-10-01', tz=buoy_tz))),
# #        aes(x=datetime, y=value, col=location)) +
# #   geom_point() +
# #   facet_grid(variable ~ ., scales = 'free_y') +
# #   labs(title='2016', x='date', y='') +
# #   scale_color_colorblind() +
# #   final_theme +
# #   scale_x_datetime(date_minor_breaks = '1 day')
# # 
# # ggplot(subset(buoy2016_vert_chla,
# #               subset=(datetime >= as.POSIXct('2016-10-01', tz=buoy_tz) & datetime < as.POSIXct('2016-11-01', tz=buoy_tz))),
# #        aes(x=datetime, y=value, col=location)) +
# #   geom_point() +
# #   facet_grid(variable ~ ., scales = 'free_y') +
# #   labs(title='2016', x='date', y='') +
# #   scale_color_colorblind() +
# #   final_theme +
# #   scale_x_datetime(date_minor_breaks = '1 day')
# 
# buoy2016_L1 <- buoy2016_L1 %>% 
#   mutate_at(vars(chla),
#             ~(case_when(location == 'in transit' ~ NA_real_,
#                            TRUE ~ .))) 
# 
# buoy2016_vert_chla_L1 <- buoy2016_L1 %>%
#   select(datetime, location, chla, chla_flag) %>%
#   gather(variable, value, -datetime, -location, -chla_flag)
# 
# # ggplot(subset(buoy2016_vert_chla,
# #               subset=(datetime >= as.POSIXct('2016-10-01', tz=buoy_tz) & datetime < as.POSIXct('2016-11-01', tz=buoy_tz))),
# #        aes(x=datetime, y=value, col=location, shape = chla_flag)) +
# #   geom_point() +
# #   facet_grid(variable ~ ., scales = 'free_y') +
# #   labs(title='2016', x='date', y='') +
# #   scale_color_colorblind() +
# #   final_theme +
# #   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(buoy2016_vert_chla_L1,
#        aes(x=datetime, y=value, col=location, shape = chla_flag)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='2016', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 month')
# 
# #chlorRFU should be linear with ugl. recode to na, because that data stream is labeled incorrectly.
# 
# rm(buoy2016_vert_chla, buoy2016_vert_chla_L1)



#### air temp & rh ####
range(buoy2016_L1$AirTempC, na.rm = T)
range(buoy2016_L1$RelHum, na.rm = T)

buoy2016_L1 <- buoy2016_L1 %>% 
  mutate_at(vars(AirTempC, RelHum),
            ~ case_when(location == 'in transit' ~ NA_real_,
                        location == 'offline' ~ NA_real_,
                        TRUE ~.))

air_vert <- buoy2016_L1 %>% 
  select(datetime, AirTempC, RelHum, location) %>% 
  pivot_longer(names_to = 'variable', values_to = 'value', -c(datetime, location))

ggplot(air_vert, aes(x=datetime, y = value)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  final_theme +
  labs(title = '2016 air temp raw',
       x= NULL,
       y= 'air temp (deg C)') +
  scale_x_datetime(date_minor_breaks = '1 month')

#no NA values to recode

# ggplot(subset(air_vert,
#               subset=(datetime>=as.POSIXct('2016-01-01', tz=buoy_tz) & datetime<as.POSIXct('2016-02-01', tz=buoy_tz))),
#        aes(x=datetime, value, color = location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'jan 2016 air temp', x=NULL, y='Air temp (deg C)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')

# ggplot(subset(air_vert,
#               subset=(datetime>=as.POSIXct('2016-02-01', tz=buoy_tz) & datetime<as.POSIXct('2016-03-01', tz=buoy_tz))),
#        aes(x=datetime, value, color = location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'feb 2016 air temp', x=NULL, y='Air temp (deg C)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(air_vert,
#               subset=(datetime>=as.POSIXct('2016-03-01', tz=buoy_tz) & datetime<as.POSIXct('2016-04-01', tz=buoy_tz))),
#        aes(x=datetime, value, color = location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'mar 2016 air temp', x=NULL, y='Air temp (deg C)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(air_vert,
#               subset=(datetime>=as.POSIXct('2016-04-01', tz=buoy_tz) & datetime<as.POSIXct('2016-05-01', tz=buoy_tz))),
#        aes(x=datetime, value, color = location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'apr 2016 air temp', x=NULL, y='Air temp (deg C)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(air_vert,
#               subset=(datetime>=as.POSIXct('2016-05-01', tz=buoy_tz) & datetime<as.POSIXct('2016-06-01', tz=buoy_tz))),
#        aes(x=datetime, value, color = location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'may 2016 air temp', x=NULL, y='Air temp (deg C)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(air_vert,
#               subset=(datetime>=as.POSIXct('2016-06-01', tz=buoy_tz) & datetime<as.POSIXct('2016-07-01', tz=buoy_tz))),
#        aes(x=datetime, value, color = location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'jun 2016 air temp', x=NULL, y='Air temp (deg C)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(air_vert,
#               subset=(datetime>=as.POSIXct('2016-07-01', tz=buoy_tz) & datetime<as.POSIXct('2016-08-01', tz=buoy_tz))),
#        aes(x=datetime, value, color = location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'jul 2016 air temp', x=NULL, y='Air temp (deg C)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(air_vert,
#               subset=(datetime>=as.POSIXct('2016-08-01', tz=buoy_tz) & datetime<as.POSIXct('2016-09-01', tz=buoy_tz))),
#        aes(x=datetime, value, color = location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'aug 2016 air temp', x=NULL, y='Air temp (deg C)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(air_vert,
#               subset=(datetime>=as.POSIXct('2016-09-01', tz=buoy_tz) & datetime<as.POSIXct('2016-10-01', tz=buoy_tz))),
#        aes(x=datetime, value, color = location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'sept 2016 air temp', x=NULL, y='Air temp (deg C)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(air_vert,
#               subset=(datetime>=as.POSIXct('2016-10-01', tz=buoy_tz) & datetime<as.POSIXct('2016-11-01', tz=buoy_tz))),
#        aes(x=datetime, value, color = location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'oct 2016 air temp', x=NULL, y='Air temp (deg C)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(air_vert,
#               subset=(datetime>=as.POSIXct('2016-11-01', tz=buoy_tz) & datetime<as.POSIXct('2016-12-01', tz=buoy_tz))),
#        aes(x=datetime, value, color = location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'nov 2016 air temp', x=NULL, y='Air temp (deg C)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(air_vert,
#               subset=(datetime>=as.POSIXct('2016-12-01', tz=buoy_tz) & datetime<as.POSIXct('2017-01-01', tz=buoy_tz))),
#        aes(x=datetime, value, color = location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'dec 2016 air temp', x=NULL, y='Air temp (deg C)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')

ggplot(air_vert,
       aes(x=datetime, value, color = location)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title = '2016 air temp', x=NULL, y='Air temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 month')

#add RH flags for supersaturated 
buoy2016_L1 <- buoy2016_L1 %>% 
  mutate(flag_rh = case_when(RelHum >100 ~ 's',
                             TRUE ~ ''))

#rename with CV
buoy2016_L1 <- buoy2016_L1 %>% 
  rename(relativeHumidity_perc = RelHum,
         airTemperature_degC = AirTempC)

rm(air_vert)



#### PAR ####
range(buoy2016_L1$PAR, na.rm = T)

#recode when in transit
buoy2016_L1 <-  buoy2016_L1 %>% 
  mutate(PAR = case_when(location == 'in transit' ~ NA_real_,
                         TRUE ~ PAR))

#replace negative values with 0 and flag
buoy2016_L1 <-  buoy2016_L1 %>% 
  mutate(flag_par = case_when(PAR < 0 ~ 'z',
                              TRUE ~ '')) %>% 
  mutate(PAR = case_when(PAR < 0 ~ 0,
                         TRUE ~ PAR))

ggplot(buoy2016_L1,
       aes(x=datetime, y=PAR, col=location)) +
  geom_point() +
  labs(title='2016', x=NULL, y='') +
  scale_color_colorblind() +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 month')

# ggplot(subset(buoy2016_L1, 
#               subset=(datetime>=as.POSIXct('2016-01-01', tz=buoy_tz) & datetime < as.POSIXct('2016-02-01', tz=buoy_tz))), 
#        aes(x=datetime, y=PAR, col=location)) +
#   geom_point() +
#   labs(title='2016', x=NULL, y='PAR') +
#   scale_color_colorblind() +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2016_L1, 
#               subset=(datetime>=as.POSIXct('2016-02-01', tz=buoy_tz) & datetime < as.POSIXct('2016-03-01', tz=buoy_tz))), 
#        aes(x=datetime, y=PAR, col=location)) +
#   geom_point() +
#   labs(title='2016', x=NULL, y='PAR') +
#   scale_color_colorblind() +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2016_L1, 
#               subset=(datetime>=as.POSIXct('2016-03-01', tz=buoy_tz) & datetime < as.POSIXct('2016-04-01', tz=buoy_tz))), 
#        aes(x=datetime, y=PAR, col=location)) +
#   geom_point() +
#   labs(title='2016', x=NULL, y='PAR') +
#   scale_color_colorblind() +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2016_L1, 
#               subset=(datetime>=as.POSIXct('2016-04-01', tz=buoy_tz) & datetime < as.POSIXct('2016-05-01', tz=buoy_tz))), 
#        aes(x=datetime, y=PAR, col=location)) +
#   geom_point() +
#   labs(title='2016', x=NULL, y='PAR') +
#   scale_color_colorblind() +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2016_L1, 
#               subset=(datetime>=as.POSIXct('2016-05-01', tz=buoy_tz) & datetime < as.POSIXct('2016-06-01', tz=buoy_tz))), 
#        aes(x=datetime, y=PAR, col=location)) +
#   geom_point() +
#   labs(title='2016', x=NULL, y='PAR') +
#   scale_color_colorblind() +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
#
# ggplot(subset(buoy2016_L1, 
#               subset=(datetime>=as.POSIXct('2016-06-01', tz=buoy_tz) & datetime < as.POSIXct('2016-07-01', tz=buoy_tz))), 
#        aes(x=datetime, y=PAR, col=location)) +
#   geom_point() +
#   labs(title='2016', x=NULL, y='PAR') +
#   scale_color_colorblind() +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2016_L1, 
#               subset=(datetime>=as.POSIXct('2016-07-01', tz=buoy_tz) & datetime < as.POSIXct('2016-08-01', tz=buoy_tz))), 
#        aes(x=datetime, y=PAR, col=location)) +
#   geom_point() +
#   labs(title='2016', x=NULL, y='PAR') +
#   scale_color_colorblind() +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2016_L1, 
#               subset=(datetime>=as.POSIXct('2016-08-01', tz=buoy_tz) & datetime < as.POSIXct('2016-09-01', tz=buoy_tz))), 
#        aes(x=datetime, y=PAR, col=location)) +
#   geom_point() +
#   labs(title='2016', x=NULL, y='PAR') +
#   scale_color_colorblind() +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2016_L1, 
#               subset=(datetime>=as.POSIXct('2016-09-01', tz=buoy_tz) & datetime < as.POSIXct('2016-10-01', tz=buoy_tz))), 
#        aes(x=datetime, y=PAR, col=location)) +
#   geom_point() +
#   labs(title='2016', x=NULL, y='PAR') +
#   scale_color_colorblind() +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2016_L1, 
#               subset=(datetime>=as.POSIXct('2016-10-01', tz=buoy_tz) & datetime < as.POSIXct('2016-11-01', tz=buoy_tz))), 
#        aes(x=datetime, y=PAR, col=location)) +
#   geom_point() +
#   labs(title='2016', x=NULL, y='PAR') +
#   scale_color_colorblind() +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2016_L1, 
#               subset=(datetime>=as.POSIXct('2016-11-01', tz=buoy_tz) & datetime < as.POSIXct('2016-12-01', tz=buoy_tz))), 
#        aes(x=datetime, y=PAR, col=location)) +
#   geom_point() +
#   labs(title='2016', x=NULL, y='PAR') +
#   scale_color_colorblind() +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2016_L1, 
#               subset=(datetime>=as.POSIXct('2016-12-01', tz=buoy_tz) & datetime < as.POSIXct('2017-01-01', tz=buoy_tz))), 
#        aes(x=datetime, y=PAR, col=location)) +
#   geom_point() +
#   labs(title='2016', x=NULL, y='PAR') +
#   scale_color_colorblind() +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')

#no recoding necessary

#rename with CV
buoy2016_L1 <- buoy2016_L1 %>% 
  rename(radiationIncomingPAR_umolm2s = PAR)


#### EXPORT L1 DATA STREAMS ####

colnames(buoy2016_L1)

#export L1 tempstring file
buoy2016_L1 %>%
  select(datetime, location, waterTemperature_degC_0p5m:waterTemperature_degC_9p5m, flag_temp0p5m) %>%
  mutate(datetime = as.character(datetime)) %>%
  write_csv(., file.path(dump_dir,'tempstring/2016_tempstring_L1_v2022.csv'))

# export L1 do file
buoy2016_L1 %>%
  select(datetime, location, 
         oxygenDissolved_mgl_1p5m, oxygenDissolvedPercentOfSaturation_pct_1p5m, waterTemperature_DO_degC_1p5m, 
         flag_do1p5m,
         oxygenDissolved_mgl_10p5m, oxygenDissolvedPercentOfSaturation_pct_10p5m, waterTemperature_DO_degC_10p5m, 
         flag_do10p5m) %>%
  mutate(datetime = as.character(datetime)) %>%
  write_csv(., file.path(dump_dir, 'do/2016_do_L1_v2022.csv'))

#export L1 met file
buoy2016_L1 %>%
  select(datetime, location, 
         windSpeedAverage_mps:windGustDirection_deg, flag_allwind,
         radiationIncomingPAR_umolm2s, flag_par,
         airTemperature_degC, relativeHumidity_perc, flag_rh) %>%
  mutate(datetime = as.character(datetime)) %>%
  write_csv(., file.path(dump_dir, 'met/2016_met_L1_v2022.csv'))
