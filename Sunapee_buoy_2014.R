#*****************************************************************
#*      Cary Institute of Ecosystem Studies (Millbrook, NY)      *
#*                                                               *
#* TITLE:   Sunapee_buoy_2014.r                                  *
#* AUTHOR:  Bethel Steele                                        *
#* PROJECT: Lake Sunapee Buoy Data Cleaning                      *
#*****************************************************************

source('library_func_lists.R')

#point to data directories
raw_dir = 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L0/'
dump_dir = 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/'

#set tz
buoy_tz = 'Etc/GMT+5'

#bring in 2014 buoy raw data
buoy2014_L0 <- read.csv(file.path(raw_dir, '2014 Buoy Data.csv'), 
                     col.names = c('ArrayID', 'Year', 'Day', 'Hr.Min', 'DOTempC', 
                                   'DOSat', 'DOppm', 'TempC_0m', 'TempC_1m', 'TempC_2m',
                                   'TempC_3m', 'TempC_4m', 'TempC_5m', 'TempC_6m', 'TempC_7m',
                                   'TempC_8m', 'TempC_9m', 'TempC_10m', 'AirTempC', 'RH',
                                   'PAR', 'InstWindSp', 'InstWindDir', 'AveWindSp', 'AveWindDir', 
                                   'MaxWindSp', 'MaxWindDir', 'LoggerBatV', 'RadioBatV', 'IntLgBxTempC', 
                                   'Heading', 'DOLowTempC', 'DOLowSat', 'DOLowPPM'),
                     skip = 1) %>% 
  select(-ArrayID, -IntLgBxTempC) 

####format data####
# format date and time in buoy data
buoy2014_L0 <- buoy2014_L0 %>% 
  mutate(hour = Hr.Min%/%100,
         minutes = Hr.Min%%100,
         minutes = replace(minutes, minutes=='0', '00'),
         time = paste(hour, minutes, sep=':')) %>% #break out time from Hr.Min, create time column
  mutate(date = as.Date(paste(Day, Year, sep = '-'), format='%j-%Y'), #create date in ymd format
         datetime = as.POSIXct(paste(date, time, sep=' '), format='%Y-%m-%d %H:%M', tz=buoy_tz)) %>%   #create datetime stamp must state UTC so that daylight savings doesn't get messy
  select(-hour, -minutes, -Hr.Min, -Year, -Day, -time, -date) #remove unnecessary columns
  
#plot the battery levels
ggplot(buoy2014_L0, aes(x = datetime, y = LoggerBatV)) +
  geom_point()

ggplot(buoy2014_L0, aes(x = datetime, y = RadioBatV)) +
  geom_point()
#rough at beginning of year and mid feb when the PAR dies

#double check to make sure there are no DST issues
datelength2014 <- buoy2014_L0 %>% 
  mutate(date = format(datetime, '%Y-%m-%d')) %>% 
  group_by(date) %>% 
  summarize(length(datetime))

#check dst dates
datelength2014[datelength2014$date == '2014-03-09',]
#dst observed here
datelength2014[datelength2014$date == '2014-11-02',]
#dst no here, but is in 11-03
datelength2014[datelength2014$date == '2014-11-03',]

#add rowid
buoy2014_L1 <- buoy2014_L0 %>% 
  rowid_to_column() %>% 
  mutate(datetime_instrument = datetime)
#locate march issue
buoy2014_L1 %>% 
  select(rowid, datetime) %>% 
  filter(datetime >= as.Date('2014-03-09') & datetime < as.Date('2014-03-10'))
buoy2014_L1a <- buoy2014_L1 %>% 
  filter(rowid <= 9774)
head(buoy2014_L1a)
#locate november issue
buoy2014_L1 %>% 
  select(rowid, datetime) %>% 
  filter(datetime >= as.Date('2014-11-03') & datetime < as.Date('2014-11-04'))
#do time math to the middle section
buoy2014_L1b <-  buoy2014_L1 %>% 
  filter(rowid > 9774 & rowid < 34946) %>% 
  mutate(datetime = datetime - hours(1))
head(buoy2014_L1b)
buoy2014_L1c <-  buoy2014_L1 %>% 
  filter(rowid >= 34946)
head(buoy2014_L1c)

buoy2014_L1 <- full_join(buoy2014_L1a, buoy2014_L1b) %>% 
  full_join(., buoy2014_L1c) %>% 
  mutate(datetime = as.POSIXct(datetime, tz = buoy_tz))

#check again for dst
datelength2014 <- buoy2014_L1 %>% 
  mutate(date = format(datetime, '%Y-%m-%d')) %>% 
  group_by(date) %>% 
  summarize(length(datetime))
datelength2014 %>% 
  arrange(`length(datetime)`)
datelength2014 %>% 
  arrange(-`length(datetime)`)
#these look good.

#add all dates/times to record
alltimes_2014 <- as.data.frame(seq.POSIXt(as.POSIXct('2014-01-01 00:00', tz=buoy_tz), as.POSIXct('2014-12-31 23:50', tz=buoy_tz), '10 min')) %>% 
  rename("datetime" = !!names(.[1]))

buoy2014_L1 <- buoy2014_L1 %>% 
  right_join(., alltimes_2014) %>% 
  arrange(datetime)

#clean up workspace
rm(alltimes_2014,datelength2014, buoy2014_L1a, buoy2014_L1b, buoy2014_L1c)


####plot  and clean thermistor data####

#initial data visualization
buoy2014_vert_temp <- buoy2014_L1 %>%
  select(datetime, all_of(alltemp)) %>%
  pivot_longer(names_to = 'variable', values_to = 'value', -datetime) %>% 
  mutate(variable = factor(variable, levels=alltemp))

# ggplot(buoy2014_vert_temp, 
#        aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='2014 thermisters - raw', 
#        x=NULL, 
#        y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme

#na values are -6999, 555.4 and 0 in thermisters
buoy2014_L1 <- buoy2014_L1 %>% 
  mutate_at(vars(TempC_0m:TempC_10m), 
            ~(case_when(.==-6999 ~ NA_real_, 
                           .==555.4 ~ NA_real_, 
                           .==0 ~ NA_real_, 
                           TRUE ~ .)))

#### L0.5 plot - no NA values of -6999, 0 or 555.4
buoy2014_vert_temp <- buoy2014_L1 %>%
  select(datetime, all_of(alltemp)) %>%
  pivot_longer(names_to = 'variable', values_to = 'value', -datetime) %>% 
  mutate(variable = factor(variable, levels=c(alltemp)))

ggplot(buoy2014_vert_temp, 
       aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='2014 thermisters - NA values recoded', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 month')


# #buoy deployed June 9, 2014 15:00
# ggplot(subset(buoy2014_vert_temp,
#               subset=(datetime>=as.POSIXct('2014-06-01',tz=buoy_tz) & datetime < as.POSIXct('2014-07-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='June 2014',
#        x='date',
#        y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2014_vert_temp,
#               subset=(datetime>=as.POSIXct('2014-07-01', tz=buoy_tz) & datetime < as.POSIXct('2014-08-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='July 2014',
#        x='date',
#        y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# #thermistors intermittent - add flag
# ggplot(subset(buoy2014_vert_temp,
#               subset=(datetime>=as.POSIXct('2014-06-09',tz=buoy_tz) & datetime < as.POSIXct('2014-07-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='June 2014',
#        x='date',
#        y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')

buoy2014_L1 <- buoy2014_L1 %>% 
  mutate(flag_alltemp = case_when(datetime >=as.POSIXct('2014-06-09',tz=buoy_tz) & datetime < as.POSIXct('2014-07-01', tz=buoy_tz) ~ 'i',
                               TRUE ~ ''))

# ggplot(subset(buoy2014_vert_temp,
#               subset=(datetime>=as.POSIXct('2014-07-24', tz=buoy_tz) & datetime < as.POSIXct('2014-07-25', tz=buoy_tz))),
#               aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='July 24 2014 buoy visit - L0', x='date', y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour')

buoy2014_L1 <- buoy2014_L1 %>% 
  mutate_at(vars(all_of(alltemp)), 
            ~(case_when(datetime>=as.POSIXct('2014-07-24 8:00', tz=buoy_tz) & datetime<=as.POSIXct('2014-07-24 8:40', tz=buoy_tz) ~ NA_real_,
                           TRUE ~ .)))

buoy2014_vert_temp_L1 <- buoy2014_L1 %>%
  select(datetime, all_of(alltemp)) %>%
  pivot_longer(names_to = 'variable', values_to = 'value', -datetime) %>% 
  mutate(variable = factor(variable, levels=c(alltemp)))

# ggplot(subset(buoy2014_vert_temp_L1,
#               subset=(datetime>= as.POSIXct('2014-07-01', tz=buoy_tz) & datetime < as.POSIXct('2014-08-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='July 2014  L1', x='date', y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')

#flag data from 10m line from here forward - seems that it is programmed wrong or is hung up above the 9m thermistor
buoy2014_L1 <- buoy2014_L1 %>% 
  mutate(flag_temp10p5m = case_when(datetime>=as.POSIXct('2014-07-24 8:40', tz=buoy_tz) & datetime<=as.POSIXct('2014-12-31 23:50', tz=buoy_tz) ~ 'd',
                               TRUE ~ ''))

# ggplot(subset(buoy2014_vert_temp,
#               subset=(datetime>=as.POSIXct('2014-08-01', tz=buoy_tz) & datetime < as.POSIXct('2014-09-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='Aug 2014',
#        x='date',
#        y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# #august 13 odd temp behavior
# ggplot(subset(buoy2014_vert_temp,
#               subset=(datetime>=as.POSIXct('2014-08-13', tz=buoy_tz) & datetime < as.POSIXct('2014-08-14', tz=buoy_tz))),
#        aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='Aug 2014',
#        x='date',
#        y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour')
# 
# #just looks like early mixing - seen also in low do
# 
# 
# ggplot(subset(buoy2014_vert_temp,
#               subset=(datetime>=as.POSIXct('2014-09-01', tz=buoy_tz) & datetime < as.POSIXct('2014-10-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='Sept 2014',
#        x='date',
#        y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2014_vert_temp,
#               subset=(datetime>=as.POSIXct('2014-10-01', tz=buoy_tz) & datetime < as.POSIXct('2014-11-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='Oct 2014',
#        x='date',
#        y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# #Oct 14 buoy moved to harbor Oct 14 10:20
# ggplot(subset(buoy2014_vert_temp,
#               subset=(datetime>=as.POSIXct('2014-10-14', tz=buoy_tz) & datetime < as.POSIXct('2014-10-15', tz=buoy_tz))),
#        aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='Oct 2014',
#        x='date',
#        y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour')

#remove from Oct 14 10:10
buoy2014_L1 <- buoy2014_L1 %>% 
  mutate_at(vars(all_of(alltemp)),
            ~(case_when(datetime>=as.POSIXct('2014-10-14 09:00', tz=buoy_tz) ~ NA_real_,
                           TRUE ~ .)))

buoy2014_vert_temp_L1 <- buoy2014_L1 %>%
  select(datetime, all_of(alltemp), flag_alltemp) %>%
  pivot_longer(names_to = 'variable', values_to = 'value', -c(datetime, flag_alltemp)) %>% 
  mutate(variable = factor(variable, levels=c(alltemp)))

# ggplot(subset(buoy2014_vert_temp_L1,
#               subset=(datetime>=as.POSIXct('2014-10-01', tz=buoy_tz) & datetime < as.POSIXct('2014-11-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='Oct 2014',
#        x='date',
#        y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')

ggplot(buoy2014_vert_temp_L1, 
       aes(x=datetime, y=value, col=variable, shape = flag_alltemp)) +
  geom_point() +
  labs(title='2014 thermisters - NA values recoded', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 month')

#clean up workspace
rm(buoy2014_vert_temp, buoy2014_vert_temp_L1)

#correct for thermistor offset
buoy2014_L1 <- buoy2014_L1 %>% 
  rename(waterTemperature_degC_10p5m = TempC_10m,
         waterTemperature_degC_9p5m = TempC_9m,
         waterTemperature_degC_8p5m = TempC_8m,
         waterTemperature_degC_7p5m = TempC_7m,
         waterTemperature_degC_6p5m = TempC_6m,
         waterTemperature_degC_5p5m = TempC_5m,
         waterTemperature_degC_4p5m = TempC_4m,
         waterTemperature_degC_3p5m = TempC_3m,
         waterTemperature_degC_2p5m = TempC_2m,
         waterTemperature_degC_1p5m = TempC_1m,
         waterTemperature_degC_0p5m = TempC_0m)

####DO data####

# add offset amount from logger at start of data recording
deepsat1 = -73.7
deepppm1 = -6.7
shalsat1 = -16.5
shalppm1 = -1.72

buoy2014_vert_do <- buoy2014_L1 %>%
  select(datetime, all_of(upDO), all_of(lowDO)) %>%
  pivot_longer(names_to = 'variable', values_to = 'value', -datetime)

# ggplot(buoy2014_vert_do, 
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='2014 do probes - raw', 
#        x=NULL, 
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 month')

buoy2014_L1 <- buoy2014_L1 %>% 
  mutate_at(vars(all_of(upDO), all_of(lowDO)), 
            ~(case_when(.==555.4 ~ NA_real_,
                        .< 0 ~ NA_real_,
                           TRUE ~ .)))%>% 
  mutate(flag_do1p5m = '',
         flag_do10p5m = '')

buoy2014_vert_do <- buoy2014_L1 %>%
  select(datetime, all_of(upDO), all_of(lowDO)) %>%
  pivot_longer(names_to = 'variable', values_to = 'value', -datetime)

ggplot(buoy2014_vert_do, 
       aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title='2014 do probes - NA values recoded', 
       x=NULL, 
       y=NULL) +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 month')

# ggplot(subset(buoy2014_vert_do,
#               subset = (datetime>= as.POSIXct('2014-01-01', tz=buoy_tz) & datetime < as.POSIXct('2014-02-01', tz=buoy_tz))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='jan 2014 do - NA values recoded',
#        x=NULL,
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2014_vert_do,
#               subset = (datetime>= as.POSIXct('2014-02-01', tz=buoy_tz) & datetime < as.POSIXct('2014-03-01', tz=buoy_tz))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='feb 2014 do - NA values recoded',
#        x=NULL,
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2014_vert_do,
#               subset = (datetime>= as.POSIXct('2014-03-01', tz=buoy_tz) & datetime < as.POSIXct('2014-04-01', tz=buoy_tz))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='mar 2014 do - NA values recoded',
#        x=NULL,
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2014_vert_do,
#               subset = (datetime>= as.POSIXct('2014-04-01', tz=buoy_tz) & datetime < as.POSIXct('2014-05-01', tz=buoy_tz))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='apr 2014 do - NA values recoded',
#        x=NULL,
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')

# #data offline begining apr 7
# ggplot(subset(buoy2014_vert_do,
#               subset = (datetime>= as.POSIXct('2014-04-07', tz=buoy_tz) & datetime < as.POSIXct('2014-04-08', tz=buoy_tz))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='apr 2014 do - NA values recoded',
#        x=NULL,
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour')
# 
# 
# ggplot(subset(buoy2014_vert_do,
#               subset = (datetime>= as.POSIXct('2014-05-01', tz=buoy_tz) & datetime < as.POSIXct('2014-06-01', tz=buoy_tz))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='jan 2014 do - NA values recoded',
#        x=NULL,
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2014_vert_do,
#               subset = (datetime>= as.POSIXct('2014-06-01', tz=buoy_tz) & datetime < as.POSIXct('2014-07-01', tz=buoy_tz))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='jun 2014 do - NA values recoded',
#        x=NULL,
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# #buoy online jun 9 at loon
# ggplot(subset(buoy2014_vert_do,
#               subset = (datetime>= as.POSIXct('2014-06-09', tz=buoy_tz) & datetime < as.POSIXct('2014-06-10', tz=buoy_tz))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='jun 2014 do - NA values recoded',
#        x=NULL,
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour')

buoy2014_L1 <- buoy2014_L1 %>% 
  mutate(location = case_when(datetime < as.POSIXct('2014-06-09', tz=buoy_tz) ~ 'harbor',
                              datetime >= as.POSIXct('2014-06-09 15:00', tz=buoy_tz) ~ 'loon',
                              TRUE ~ NA_character_))

# ggplot(subset(buoy2014_vert_do,
#               subset = (datetime>= as.POSIXct('2014-07-01', tz=buoy_tz) & datetime < as.POSIXct('2014-08-01', tz=buoy_tz))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='jul 2014 do - NA values recoded',
#        x=NULL,
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# #all do data on old offset until Jul 28; low do kaputt 07-17 through 07-28
# ggplot(subset(buoy2014_vert_do,
#               subset = (datetime>= as.POSIXct('2014-07-17', tz=buoy_tz) & datetime < as.POSIXct('2014-07-18', tz=buoy_tz))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='jul 2014 do - NA values recoded',
#        x=NULL,
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour')
# 
# ggplot(subset(buoy2014_vert_do,
#               subset = (datetime>= as.POSIXct('2014-07-28', tz=buoy_tz) & datetime < as.POSIXct('2014-07-29', tz=buoy_tz))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='jul 2014 do - NA values recoded',
#        x=NULL,
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour')

#recode low do data 2014-07-17 08:30 through 2014-07-28 10:30
buoy2014_L1 <- buoy2014_L1 %>% 
  mutate_at(all_of(lowDO),
            ~case_when(datetime >= as.POSIXct('2014-07-17 08:30', tz = buoy_tz) &
                         datetime < as.POSIXct('2014-07-28 10:30', tz= buoy_tz) ~ NA_real_,
                       TRUE ~ .))

buoy2014_vert_do <- buoy2014_L1 %>%
  select(datetime, all_of(upDO), all_of(lowDO)) %>%
  pivot_longer(names_to = 'variable', values_to = 'value', -datetime)

# ggplot(subset(buoy2014_vert_do,
#               subset = (datetime>= as.POSIXct('2014-07-01', tz=buoy_tz) & datetime < as.POSIXct('2014-08-01', tz=buoy_tz))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='jul 2014 do - NA values recoded',
#        x=NULL,
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')

# add offset and recalculate data
buoy2014_L1 <- buoy2014_L1 %>% 
  #save offset in column
  mutate(offval_do10p5_mgl = case_when(datetime < as.POSIXct('2014-07-28 10:30', tz=buoy_tz) ~ deepppm1,
                                       TRUE ~ 0),
         offval_do10p5_sat = case_when(datetime < as.POSIXct('2014-07-28 10:30', tz=buoy_tz) ~ deepsat1,
                                       TRUE ~ 0),
         offval_do1p5_mgl = case_when(datetime < as.POSIXct('2014-07-28 10:30', tz=buoy_tz) ~ shalppm1,
                                      TRUE ~ 0),
         offval_do1p5_sat = case_when(datetime < as.POSIXct('2014-07-28 10:30', tz=buoy_tz) ~ shalsat1,
                                      TRUE ~ 0)) %>% 
  #copy data from program to new column during offset period
  mutate(oxygenDissolved_mgl_1p5m_withoffval = case_when(datetime < as.POSIXct('2014-07-28 10:30', tz=buoy_tz)~ DOppm,
                                                         TRUE ~ NA_real_),
         oxygenDissolvedPercentOfSaturation_pct_1p5m_withoffval = case_when(datetime < as.POSIXct('2014-07-28 10:30', tz=buoy_tz) ~ DOSat,
                                                                            TRUE ~ NA_real_),
         oxygenDissolved_mgl_10p5m_withoffval = case_when(datetime < as.POSIXct('2014-07-28 10:30', tz=buoy_tz) ~ DOLowPPM,
                                                          TRUE ~ NA_real_),
         oxygenDissolvedPercentOfSaturation_pct_10p5m_withoffval = case_when(datetime < as.POSIXct('2014-07-28 10:30', tz=buoy_tz) ~ DOLowSat,
                                                                             TRUE ~ NA_real_)) %>% 
  #add flags for calculated from offset
  mutate(flag_do1p5m = case_when(datetime < as.POSIXct('2014-07-28 10:30', tz=buoy_tz) ~ 'o',
                                 TRUE ~ flag_do1p5m)) %>%
  mutate(flag_do10p5m = case_when(datetime < as.POSIXct('2014-07-28 10:30', tz=buoy_tz) ~ 'o',
                                  TRUE ~ flag_do10p5m)) %>%  
  #calculate data without offset
  mutate(DOppm = case_when(flag_do1p5m == 'o' ~ oxygenDissolved_mgl_1p5m_withoffval - offval_do1p5_mgl,
                           TRUE~DOppm),
         DOSat = case_when(flag_do1p5m == 'o' ~oxygenDissolvedPercentOfSaturation_pct_1p5m_withoffval - offval_do1p5_sat,
                           TRUE~DOSat),
         DOLowPPM = case_when(flag_do10p5m == 'o' ~ oxygenDissolved_mgl_10p5m_withoffval - offval_do10p5_mgl,
                              TRUE~DOLowPPM),
         DOLowSat = case_when(flag_do10p5m == 'o'~oxygenDissolvedPercentOfSaturation_pct_10p5m_withoffval - offval_do10p5_sat,
                              TRUE~DOLowSat))

buoy2014_vert_do_L1 <- buoy2014_L1 %>%
  select(datetime, all_of(upDO), all_of(lowDO)) %>%
  pivot_longer(names_to = 'variable', values_to = 'value', -datetime)

# ggplot(subset(buoy2014_vert_do_L1,
#               subset = (datetime>= as.POSIXct('2014-07-28', tz=buoy_tz) & datetime < as.POSIXct('2014-07-29', tz=buoy_tz))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='jul 2014 do - NA values recoded',
#        x=NULL,
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour')

# #not sure why, but there's some oddball timing in the upper do on the 28th
buoy2014_L1 <- buoy2014_L1 %>% 
  mutate(DOppm = case_when(datetime >= as.POSIXct('2014-07-28 10:30', tz= buoy_tz) &
                             datetime < as.POSIXct('2014-07-28 13:40', tz= buoy_tz) ~ NA_real_,
                           TRUE ~ DOppm),
         DOSat = case_when(datetime >= as.POSIXct('2014-07-28 10:00', tz= buoy_tz) &
                             datetime < as.POSIXct('2014-07-28 10:30', tz= buoy_tz) ~ NA_real_,
                           TRUE ~ DOSat))
buoy2014_vert_do_L1 <- buoy2014_L1 %>%
  select(datetime, all_of(upDO), all_of(lowDO)) %>%
  pivot_longer(names_to = 'variable', values_to = 'value', -datetime)

# ggplot(subset(buoy2014_vert_do_L1,
#               subset = (datetime>= as.POSIXct('2014-07-28', tz=buoy_tz) & datetime < as.POSIXct('2014-07-29', tz=buoy_tz))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='jul 2014 do - NA values recoded',
#        x=NULL,
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour')
# 
# ggplot(subset(buoy2014_vert_do_L1,
#               subset = (datetime>= as.POSIXct('2014-07-01', tz=buoy_tz) & datetime < as.POSIXct('2014-08-01', tz=buoy_tz))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='jul 2014 do - qaqc',
#        x=NULL,
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2014_vert_do,
#               subset = (datetime>= as.POSIXct('2014-08-01', tz=buoy_tz) & datetime < as.POSIXct('2014-09-01', tz=buoy_tz))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='aug 2014 do - NA values recoded',
#        x=NULL,
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# #august 13 is early mixing - seen also in thermistors
# 
# ggplot(subset(buoy2014_vert_do,
#               subset = (datetime>= as.POSIXct('2014-09-01', tz=buoy_tz) & datetime < as.POSIXct('2014-10-01', tz=buoy_tz))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='sept 2014 do - NA values recoded',
#        x=NULL,
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# #upper do given a new offset sept 18
# ggplot(subset(buoy2014_vert_do,
#               subset = (datetime>= as.POSIXct('2014-09-18', tz=buoy_tz) & datetime < as.POSIXct('2014-09-19', tz=buoy_tz))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='sept 2014 do - NA values recoded',
#        x=NULL,
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour')

#recode the errant obsat 15:50
buoy2014_L1 <- buoy2014_L1 %>% 
  mutate_at(vars(all_of(upDO), all_of(lowDO)),
            ~ case_when(datetime == as.POSIXct('2014-09-18 15:50', tz= buoy_tz) ~ NA_real_,
                        TRUE ~ .))


# add offset amount from logger at start of data recording
shalsat2 = -20
shalppm2 = -2

# add offset and recalculate data
buoy2014_L1 <- buoy2014_L1 %>% 
  #save offset in column
  mutate(offval_do1p5_mgl = case_when(datetime >= as.POSIXct('2014-09-18 16:00', tz=buoy_tz) ~ shalppm2,
                                      TRUE ~ offval_do1p5_mgl),
         offval_do1p5_sat = case_when(datetime >= as.POSIXct('2014-09-18 16:00', tz=buoy_tz) ~ shalsat2,
                                      TRUE ~ offval_do1p5_sat)) %>% 
  #copy data from program to new column during offset period
  mutate(oxygenDissolved_mgl_1p5m_withoffval = case_when(datetime >= as.POSIXct('2014-09-18 16:00', tz=buoy_tz)~ DOppm,
                                                         TRUE ~ oxygenDissolved_mgl_1p5m_withoffval),
         oxygenDissolvedPercentOfSaturation_pct_1p5m_withoffval = case_when(datetime >= as.POSIXct('2014-09-18 16:00', tz=buoy_tz) ~ DOSat,
                                                                            TRUE ~ oxygenDissolvedPercentOfSaturation_pct_1p5m_withoffval)) %>% 
  #add flags for calculated from offset
  mutate(flag_do1p5m = case_when(datetime >= as.POSIXct('2014-09-18 16:00', tz=buoy_tz) ~ 'o',
                                 TRUE ~ flag_do1p5m)) %>%
  #calculate data without offset
  mutate(DOppm = case_when(flag_do1p5m == 'o' ~ oxygenDissolved_mgl_1p5m_withoffval - offval_do1p5_mgl,
                           TRUE~DOppm),
         DOSat = case_when(flag_do1p5m == 'o' ~ oxygenDissolvedPercentOfSaturation_pct_1p5m_withoffval - offval_do1p5_sat,
                           TRUE~DOSat))

buoy2014_L1 <- buoy2014_L1 %>% 
  mutate(upper_do_flag = case_when(datetime >= as.POSIXct('2014-07-28 10:00', tz=buoy_tz) & datetime < as.POSIXct('2014-09-18 16:00', tz=buoy_tz) ~ 'm',
                           TRUE ~ NA_character_)) %>% 
  mutate(DOppm = case_when(datetime >= as.POSIXct('2014-07-28 10:00', tz=buoy_tz) & datetime < as.POSIXct('2014-07-28 13:40', tz=buoy_tz) ~ NA_real_,
                           TRUE ~ DOppm))

buoy2014_vert_do_L1 <- buoy2014_L1 %>%
  select(datetime, all_of(upDO), all_of(lowDO)) %>%
  pivot_longer(names_to = 'variable', values_to = 'value', -datetime)

# ggplot(subset(buoy2014_vert_do_L1,
#               subset = (datetime>= as.POSIXct('2014-09-01', tz=buoy_tz) & datetime < as.POSIXct('2014-10-01', tz=buoy_tz))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='sept 2014 do - clean',
#        x=NULL,
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2014_vert_do,
#               subset = (datetime>= as.POSIXct('2014-10-01', tz=buoy_tz) & datetime < as.POSIXct('2014-11-01', tz=buoy_tz))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='oct 2014 do - NA values recoded',
#        x=NULL,
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# #buoy moved to harbor 10-14
# ggplot(subset(buoy2014_vert_do,
#               subset = (datetime>= as.POSIXct('2014-10-14', tz=buoy_tz) & datetime < as.POSIXct('2014-10-15', tz=buoy_tz))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='oct 2014 do - NA values recoded',
#        x=NULL,
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour')

buoy2014_L1 <- buoy2014_L1 %>% 
  mutate(location = case_when(datetime >= as.POSIXct('2014-10-14 09:00', tz=buoy_tz) & datetime < as.POSIXct('2014-10-14 10:10', tz=buoy_tz)~ 'in transit',
                              datetime >= as.POSIXct('2014-10-14 10:10', tz=buoy_tz) ~ 'harbor',
                              TRUE ~ location)) %>% 
  mutate_at(vars(all_of(upDO)),
            ~(case_when(location == 'in transit' ~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate_at(vars(all_of(lowDO)),
            ~(case_when(datetime >= as.POSIXct('2014-10-14 09:00', tz=buoy_tz) ~ NA_real_,
                           TRUE ~ .)))

buoy2014_vert_do_L1 <- buoy2014_L1 %>%
  select(datetime, all_of(upDO), all_of(lowDO)) %>%
  pivot_longer(names_to = 'variable', values_to = 'value', -datetime)

# ggplot(subset(buoy2014_vert_do_L1,
#               subset = (datetime>= as.POSIXct('2014-10-01', tz=buoy_tz) & datetime < as.POSIXct('2014-11-01', tz=buoy_tz))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='oct 2014 do - clean',
#        x=NULL,
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2014_vert_do,
#               subset = (datetime>= as.POSIXct('2014-11-01', tz=buoy_tz) & datetime < as.POSIXct('2014-12-01', tz=buoy_tz))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='nov 2014 do - NA values recoded',
#        x=NULL,
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2014_vert_do,
#               subset = (datetime>= as.POSIXct('2014-12-01', tz=buoy_tz) & datetime < as.POSIXct('2015-01-01', tz=buoy_tz))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title='dec 2014 do - NA values recoded',
#        x=NULL,
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')

buoy2014_vert_updo <- buoy2014_L1 %>%
  select(datetime, all_of(upDO), flag_do1p5m, location) %>%
  pivot_longer(names_to = 'variable', values_to = 'value', -c(datetime, location, flag_do1p5m))

ggplot(buoy2014_vert_updo, 
       aes(x=datetime, y=value, color = flag_do1p5m, shape = location)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title='2014 upper do probes - clean', 
       x=NULL, 
       y=NULL) +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 month')

buoy2014_vert_lowdo <- buoy2014_L1 %>%
  select(datetime,all_of(lowDO), flag_do10p5m, location) %>%
  pivot_longer(names_to = 'variable', values_to = 'value', -c(datetime, location, flag_do10p5m))

ggplot(buoy2014_vert_lowdo, 
       aes(x=datetime, y=value, color = flag_do10p5m, shape = location)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title='2014 lower do probes - clean', 
       x=NULL, 
       y=NULL) +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 month')

# add presumed cleaning flags
buoy2014_L1 <- buoy2014_L1 %>% 
  mutate(flag_do10p5m = case_when(flag_do10p5m == '' & datetime == as.POSIXct('2014-06-09 15:00', tz=buoy_tz) ~ 'wp',
                                  flag_do10p5m != '' & datetime == as.POSIXct('2014-06-09 15:00', tz=buoy_tz) ~ paste(flag_do10p5m, 'wp', sep = '; '),
                                   TRUE ~ flag_do10p5m)) %>% 
  mutate(flag_do1p5m = case_when(flag_do1p5m == '' & datetime == as.POSIXct('2014-06-09 15:00', tz=buoy_tz) ~ 'wp',
                                 flag_do1p5m != '' & datetime == as.POSIXct('2014-06-09 15:00', tz=buoy_tz) ~ paste(flag_do1p5m, 'wp', sep = '; '),
                                 flag_do1p5m == '' & datetime == as.POSIXct('2014-10-14 10:20', tz=buoy_tz) ~ 'wp',
                                flag_do1p5m != ''  & datetime == as.POSIXct('2014-10-14 10:20', tz=buoy_tz) ~ paste(flag_do1p5m, 'wp', sep = '; '),
                                   TRUE ~ flag_do1p5m))

#add flag for no calibration on record
buoy2014_L1 <- buoy2014_L1 %>% 
  mutate_at(vars(flag_do1p5m, flag_do10p5m),
         ~ case_when(. == '' ~ 'x',
                     . != '' ~ paste('x', ., sep = '; ')))

rm(buoy2014_vert_do, buoy2014_vert_do_L1, buoy2014_vert_updo, buoy2014_vert_lowdo)

#rename with CV
buoy2014_L1 <- buoy2014_L1 %>% 
  rename(oxygenDissolved_mgl_1p5m = DOppm,
         oxygenDissolvedPercentOfSaturation_pct_1p5m = DOSat,
         waterTemperature_DO_degC_1p5m = DOTempC,
         oxygenDissolved_mgl_10p5m = DOLowPPM,
         oxygenDissolvedPercentOfSaturation_pct_10p5m = DOLowSat,
         waterTemperature_DO_degC_10p5m = DOLowTempC)

# recode offset values where raw values have been recoded
buoy2014_L1 <- buoy2014_L1 %>% 
  mutate(oxygenDissolved_mgl_1p5m_withoffval = case_when(is.na(oxygenDissolved_mgl_1p5m) ~ NA_real_,
                                                         TRUE ~ oxygenDissolved_mgl_1p5m_withoffval),
         oxygenDissolved_mgl_10p5m_withoffval = case_when(is.na(oxygenDissolved_mgl_10p5m) ~ NA_real_,
                                                          TRUE ~ oxygenDissolved_mgl_10p5m_withoffval),
         oxygenDissolvedPercentOfSaturation_pct_1p5m_withoffval = case_when(is.na(oxygenDissolvedPercentOfSaturation_pct_1p5m) ~ NA_real_,
                                                                            TRUE ~ oxygenDissolvedPercentOfSaturation_pct_1p5m_withoffval),
         oxygenDissolvedPercentOfSaturation_pct_10p5m_withoffval = case_when(is.na(oxygenDissolvedPercentOfSaturation_pct_10p5m) ~ NA_real_,
                                                                             TRUE ~ oxygenDissolvedPercentOfSaturation_pct_10p5m_withoffval))

#### PAR ####

#replace negative values with 0
range(buoy2014_L1$PAR, na.rm = T)

buoy2014_L1 <- buoy2014_L1 %>% 
  mutate(flag_par = case_when(PAR < 0 ~ 'z',
                              TRUE ~ ''))%>% 
  mutate(PAR = case_when(PAR < 0 ~ 0,
                         TRUE ~ PAR))

#recode when in transit
buoy2014_L1 <- buoy2014_L1 %>% 
  mutate(PAR = case_when(location == 'in transit' ~ NA_real_,
                       TRUE ~ PAR))

ggplot(buoy2014_L1,
       aes(x=datetime, y=PAR, color = location)) +
  geom_point() +
  labs(title='2014 PAR', x=NULL, y='PAR (uE*m-2*s-1)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 month')

# ggplot(subset(buoy2014_L1,
#               subset=(datetime >= as.POSIXct('2014-01-01', tz=buoy_tz) & datetime < as.POSIXct('2014-02-01', tz=buoy_tz))),
#        aes(x=datetime, y=PAR, color = location)) +
#   geom_point() +
#   labs(title='jan 2014 PAR', x=NULL, y='PAR (uE*m-2*s-1)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2014_L1,
#               subset=(datetime >= as.POSIXct('2014-02-01', tz=buoy_tz) & datetime < as.POSIXct('2014-03-01', tz=buoy_tz))),
#        aes(x=datetime, y=PAR, color = location)) +
#   geom_point() +
#   labs(title='feb 2014 PAR', x=NULL, y='PAR (uE*m-2*s-1)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')

#par likely obscured feb 13-19
buoy2014_L1 <- buoy2014_L1 %>% 
  mutate(flag_par = case_when(flag_par == '' & datetime >= as.POSIXct('2014-02-13', tz=buoy_tz) & datetime < as.POSIXct('2014-02-20', tz=buoy_tz) ~ 'o',
                              flag_par != '' & datetime >= as.POSIXct('2014-02-13', tz=buoy_tz) & datetime < as.POSIXct('2014-02-20', tz=buoy_tz) ~ paste('o', flag_par, sep = '; '),
                              TRUE ~ flag_par))

# ggplot(subset(buoy2014_L1,
#               subset=(datetime >= as.POSIXct('2014-03-01', tz=buoy_tz) & datetime < as.POSIXct('2014-04-01', tz=buoy_tz))),
#        aes(x=datetime, y=PAR, color = location)) +
#   geom_point() +
#   labs(title='mar 2014 PAR', x=NULL, y='PAR (uE*m-2*s-1)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2014_L1,
#               subset=(datetime >= as.POSIXct('2014-04-01', tz=buoy_tz) & datetime < as.POSIXct('2014-05-01', tz=buoy_tz))),
#        aes(x=datetime, y=PAR, color = location)) +
#   geom_point() +
#   labs(title='apr 2014 PAR', x=NULL, y='PAR (uE*m-2*s-1)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')

#par errant mar 28-apr 8 recode to na, data errant after that - recode from mar 28 forward
buoy2014_L1 <- buoy2014_L1 %>% 
  mutate(PAR = case_when(datetime >= as.POSIXct('2014-03-28', tz=buoy_tz) ~ NA_real_,
                              TRUE ~ PAR))

ggplot(buoy2014_L1,
       aes(x=datetime, y=PAR, color = PAR_flag, shape = location)) +
  geom_point() +
  labs(title='2014 PAR clean', x=NULL, y='PAR (uE*m-2*s-1)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 month')

#rename with CV
buoy2014_L1 <- buoy2014_L1 %>% 
  rename(radiationIncomingPAR_umolm2s = PAR)

####Wind data ####
range(buoy2014_L1$InstWindSp, na.rm = T)
range(buoy2014_L1$InstWindDir, na.rm = T)
range(buoy2014_L1$AveWindSp, na.rm = T)
range(buoy2014_L1$AveWindDir, na.rm = T)
range(buoy2014_L1$MaxWindSp, na.rm = T)
range(buoy2014_L1$MaxWindDir, na.rm = T)

#recode wind data when in transit 
buoy2014_L1 <- buoy2014_L1 %>% 
  mutate_at(vars(InstWindSp, InstWindDir, AveWindSp, AveWindDir, MaxWindSp, MaxWindDir),
            ~case_when(location == 'in transit'~NA_real_,
                       TRUE ~ .))

buoy2014_vert_wind <- buoy2014_L1 %>% 
  select(datetime, location, InstWindSp, InstWindDir, AveWindSp, AveWindDir, MaxWindSp, MaxWindDir) %>% 
  pivot_longer(names_to = 'variable', values_to = 'value' , -c(datetime, location))

ggplot(buoy2014_vert_wind, 
       aes(x=datetime, y=value, color=location)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='2014, raw', x='date', y=' ') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 month')

# ggplot(subset(buoy2014_vert_wind,
#               subset=(datetime >= as.POSIXct('2014-01-01', tz=buoy_tz) & datetime < as.POSIXct('2014-02-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='jan 2014, raw', x='date', y=' ') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ##sensor frozen until Jan 5
# ggplot(subset(buoy2014_vert_wind,
#               subset=(datetime >= as.POSIXct('2014-01-05', tz=buoy_tz) & datetime < as.POSIXct('2014-01-06', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='jan 2014, raw', x='date', y=' ') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour')

buoy2014_L1 <- buoy2014_L1 %>% 
  mutate_at(vars(InstWindSp, InstWindDir, AveWindSp, AveWindDir, MaxWindSp, MaxWindDir),
            ~(case_when(datetime<as.POSIXct('2014-01-05 12:40', tz=buoy_tz) ~ NA_real_,
                           TRUE ~ .)))
buoy2014_vert_wind_L1 <- buoy2014_L1 %>% 
  select(datetime, location, InstWindSp, InstWindDir, AveWindSp, AveWindDir, MaxWindSp, MaxWindDir) %>% 
  pivot_longer(names_to = 'variable', values_to = 'value' , -c(datetime, location))

# ggplot(subset(buoy2014_vert_wind_L1,
#               subset=(datetime >= as.POSIXct('2014-01-01', tz=buoy_tz) & datetime < as.POSIXct('2014-02-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='jan 2014, clean', x='date', y=' ') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2014_vert_wind,
#               subset=(datetime >= as.POSIXct('2014-02-01', tz=buoy_tz) & datetime < as.POSIXct('2014-03-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='feb 2014, raw', x='date', y=' ') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# #sensor frozen feb 14
# ggplot(subset(buoy2014_vert_wind,
#               subset=(datetime >= as.POSIXct('2014-02-14', tz=buoy_tz) & datetime < as.POSIXct('2014-02-15', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='feb 2014, raw', x='date', y=' ') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour')
# 
# #and on the 18/19th
# ggplot(subset(buoy2014_vert_wind,
#               subset=(datetime >= as.POSIXct('2014-02-18 18:00', tz=buoy_tz) & datetime < as.POSIXct('2014-02-19 18:00', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='feb 2014, raw', x='date', y=' ') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour')

buoy2014_L1 <- buoy2014_L1 %>% 
  mutate_at(vars(InstWindSp, InstWindDir, AveWindSp, AveWindDir, MaxWindSp, MaxWindDir),
            ~(case_when(datetime>=as.POSIXct('2014-02-14 4:50', tz=buoy_tz) & datetime<as.POSIXct('2014-02-14 12:50', tz=buoy_tz) & MaxWindSp == 0 ~ NA_real_,
                           datetime>=as.POSIXct('2014-02-19 1:30', tz=buoy_tz) & datetime<as.POSIXct('2014-02-19 8:00', tz=buoy_tz) & MaxWindSp == 0 ~ NA_real_,
                           TRUE ~ .)))
buoy2014_vert_wind_L1 <- buoy2014_L1 %>% 
  select(datetime, location, InstWindSp, InstWindDir, AveWindSp, AveWindDir, MaxWindSp, MaxWindDir) %>% 
  pivot_longer(names_to = 'variable', values_to = 'value' , -c(datetime, location))

# ggplot(subset(buoy2014_vert_wind_L1,
#               subset=(datetime >= as.POSIXct('2014-02-01', tz=buoy_tz) & datetime < as.POSIXct('2014-03-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='feb 2014, clean', x='date', y=' ') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2014_vert_wind,
#               subset=(datetime >= as.POSIXct('2014-03-01', tz=buoy_tz) & datetime < as.POSIXct('2014-04-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='mar 2014, raw', x='date', y=' ') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2014_vert_wind,
#               subset=(datetime >= as.POSIXct('2014-04-01', tz=buoy_tz) & datetime < as.POSIXct('2014-05-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='apr 2014, raw', x='date', y=' ') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2014_vert_wind,
#               subset=(datetime >= as.POSIXct('2014-05-01', tz=buoy_tz) & datetime < as.POSIXct('2014-06-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='may 2014, raw', x='date', y=' ') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2014_vert_wind,
#               subset=(datetime >= as.POSIXct('2014-06-01', tz=buoy_tz) & datetime < as.POSIXct('2014-07-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='jun 2014, raw', x='date', y=' ') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2014_vert_wind,
#               subset=(datetime >= as.POSIXct('2014-07-01', tz=buoy_tz) & datetime < as.POSIXct('2014-08-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='jul 2014, raw', x='date', y=' ') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2014_vert_wind,
#               subset=(datetime >= as.POSIXct('2014-08-01', tz=buoy_tz) & datetime < as.POSIXct('2014-09-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='aug 2014, raw', x='date', y=' ') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2014_vert_wind,
#               subset=(datetime >= as.POSIXct('2014-09-01', tz=buoy_tz) & datetime < as.POSIXct('2014-10-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='sept 2014, raw', x='date', y=' ') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2014_vert_wind,
#               subset=(datetime >= as.POSIXct('2014-10-01', tz=buoy_tz) & datetime < as.POSIXct('2014-11-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='oct 2014, raw', x='date', y=' ') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
#
# ggplot(subset(buoy2014_vert_wind_L1,
#               subset=(datetime >= as.POSIXct('2014-10-01', tz=buoy_tz) & datetime < as.POSIXct('2014-11-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='oct 2014, clean', x='date', y=' ') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2014_vert_wind,
#               subset=(datetime >= as.POSIXct('2014-11-01', tz=buoy_tz) & datetime < as.POSIXct('2014-12-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='nov 2014, raw', x='date', y=' ') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# #sensor frozen nov26 through nov 30
# ggplot(subset(buoy2014_vert_wind,
#               subset=(datetime >= as.POSIXct('2014-11-26', tz=buoy_tz) & datetime < as.POSIXct('2014-11-27', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='nov 2014, raw', x='date', y=' ') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour')
# 
# ggplot(subset(buoy2014_vert_wind,
#               subset=(datetime >= as.POSIXct('2014-11-30', tz=buoy_tz) & datetime < as.POSIXct('2014-12-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='nov 2014, raw', x='date', y=' ') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour')

buoy2014_L1 <- buoy2014_L1 %>% 
  mutate_at(vars(InstWindSp, InstWindDir, AveWindSp, AveWindDir, MaxWindSp, MaxWindDir),
            ~(case_when(datetime>=as.POSIXct('2014-11-26 12:30', tz=buoy_tz) & datetime<as.POSIXct('2014-11-30 9:00', tz=buoy_tz) ~ NA_real_,
                           TRUE ~ .)))
buoy2014_vert_wind_L1 <- buoy2014_L1 %>% 
  select(datetime, location, InstWindSp, InstWindDir, AveWindSp, AveWindDir, MaxWindSp, MaxWindDir) %>% 
  pivot_longer(names_to = 'variable', values_to = 'value' , -c(datetime, location))

# ggplot(subset(buoy2014_vert_wind_L1,
#               subset=(datetime >= as.POSIXct('2014-11-01', tz=buoy_tz) & datetime < as.POSIXct('2014-12-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='nov 2014, clean', x='date', y=' ') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2014_vert_wind,
#               subset=(datetime >= as.POSIXct('2014-12-01', tz=buoy_tz) & datetime < as.POSIXct('2015-01-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='dec 2014, raw', x='date', y=' ') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# #sensor frozen dec 11/12
# ggplot(subset(buoy2014_vert_wind,
#               subset=(datetime >= as.POSIXct('2014-12-11 9:00', tz=buoy_tz) & datetime < as.POSIXct('2014-12-12 18:00', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='dec 2014, raw', x='date', y=' ') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour')

buoy2014_L1 <- buoy2014_L1 %>% 
  mutate_at(vars(InstWindSp, InstWindDir, AveWindSp, AveWindDir, MaxWindSp, MaxWindDir),
            ~(case_when(datetime>=as.POSIXct('2014-12-11 14:30', tz=buoy_tz) & datetime<as.POSIXct('2014-12-12 14:10', tz=buoy_tz) ~ NA_real_,
                           TRUE ~ .)))
buoy2014_vert_wind_L1 <- buoy2014_L1 %>% 
  select(datetime, location, InstWindSp, InstWindDir, AveWindSp, AveWindDir, MaxWindSp, MaxWindDir) %>% 
  pivot_longer(names_to = 'variable', values_to = 'value' , -c(datetime, location))

# ggplot(subset(buoy2014_vert_wind_L1,
#               subset=(datetime >= as.POSIXct('2014-12-01', tz=buoy_tz) & datetime < as.POSIXct('2015-01-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='dec 2014, clean', x='date', y=' ') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')

ggplot(buoy2014_vert_wind_L1, 
       aes(x=datetime, y=value, color=location)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='2014, clean', x='date', y=' ') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 month')

rm(buoy2014_vert_wind, buoy2014_vert_wind_L1)

#rename with CV
buoy2014_L1 <- buoy2014_L1 %>% 
  rename(windDirectionInstantaneous_deg =InstWindDir, 
         windSpeedInstantaneous_mps = InstWindSp,
         windDirectionAverage_deg = AveWindDir,
         windSpeedAverage_mps = AveWindSp,
         windGustDirection_deg = MaxWindDir,
         windGustSpeed_mps = MaxWindSp)

#### Air Temp data ####
range(buoy2014_L1$AirTempC, na.rm = T)
range(buoy2014_L1$RH, na.rm = T)

buoy2014_L1 <- buoy2014_L1 %>% 
  mutate_at(vars(AirTempC, RH),
            ~ case_when(location == 'in transit' ~ NA_real_, 
                        TRUE ~.))

air_vert <- buoy2014_L1 %>% 
  select(datetime, AirTempC, RH, location) %>% 
  pivot_longer(names_to = 'variable', values_to = 'value', -c(datetime, location))

ggplot(air_vert, aes(x=datetime, y = value)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  final_theme +
  labs(title = '2014 air temp raw',
       x= NULL,
       y= 'air temp (deg C)') +
  scale_x_datetime(date_minor_breaks = '1 month')

# ggplot(subset(air_vert,
#               subset=(datetime>=as.POSIXct('2014-01-01', tz=buoy_tz) &
#                         datetime < as.POSIXct('2014-02-01', tz=buoy_tz))),
#        aes(x=datetime, y = value)) +
#   facet_grid(variable~., scales = 'free_y') +
#   geom_point() +
#   final_theme +
#   labs(title = 'jan2014 air temp raw',
#        x= NULL,
#        y= 'air temp (deg C)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(air_vert,
#               subset=(datetime>=as.POSIXct('2014-02-01', tz=buoy_tz) &
#                         datetime < as.POSIXct('2014-03-01', tz=buoy_tz))),
#        aes(x=datetime, y = value)) +
#   facet_grid(variable~., scales = 'free_y') +
#   geom_point() +
#   final_theme +
#   labs(title = 'feb 2014 air temp raw',
#        x= NULL,
#        y= 'air temp (deg C)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(air_vert,
#               subset=(datetime>=as.POSIXct('2014-03-01', tz=buoy_tz) &
#                         datetime < as.POSIXct('2014-04-01', tz=buoy_tz))),
#        aes(x=datetime, y = value)) +
#   facet_grid(variable~., scales = 'free_y') +
#   geom_point() +
#   final_theme +
#   labs(title = 'mar 2014 air temp raw',
#        x= NULL,
#        y= 'air temp (deg C)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(air_vert,
#               subset=(datetime>=as.POSIXct('2014-04-01', tz=buoy_tz) &
#                         datetime < as.POSIXct('2014-05-01', tz=buoy_tz))),
#        aes(x=datetime, y = value)) +
#   facet_grid(variable~., scales = 'free_y') +
#   geom_point() +
#   final_theme +
#   labs(title = ' apr 2014 air temp raw',
#        x= NULL,
#        y= 'air temp (deg C)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(air_vert,
#               subset=(datetime>=as.POSIXct('2014-05-01', tz=buoy_tz) &
#                         datetime < as.POSIXct('2014-06-01', tz=buoy_tz))),
#        aes(x=datetime, y = value)) +
#   facet_grid(variable~., scales = 'free_y') +
#   geom_point() +
#   final_theme +
#   labs(title = 'may 2014 air temp raw',
#        x= NULL,
#        y= 'air temp (deg C)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(air_vert,
#               subset=(datetime>=as.POSIXct('2014-06-01', tz=buoy_tz) &
#                         datetime < as.POSIXct('2014-07-01', tz=buoy_tz))),
#        aes(x=datetime, y = value)) +
#   facet_grid(variable~., scales = 'free_y') +
#   geom_point() +
#   final_theme +
#   labs(title = 'jun 2014 air temp raw',
#        x= NULL,
#        y= 'air temp (deg C)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(air_vert,
#               subset=(datetime>=as.POSIXct('2014-07-01', tz=buoy_tz) &
#                         datetime < as.POSIXct('2014-08-01', tz=buoy_tz))),
#        aes(x=datetime, y = value)) +
#   facet_grid(variable~., scales = 'free_y') +
#   geom_point() +
#   final_theme +
#   labs(title = 'jul 2014 air temp raw',
#        x= NULL,
#        y= 'air temp (deg C)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(air_vert,
#               subset=(datetime>=as.POSIXct('2014-08-01', tz=buoy_tz) &
#                         datetime < as.POSIXct('2014-09-01', tz=buoy_tz))),
#        aes(x=datetime, y = value)) +
#   facet_grid(variable~., scales = 'free_y') +
#   geom_point() +
#   final_theme +
#   labs(title = 'aug 2014 air temp raw',
#        x= NULL,
#        y= 'air temp (deg C)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(air_vert,
#               subset=(datetime>=as.POSIXct('2014-09-01', tz=buoy_tz) &
#                         datetime < as.POSIXct('2014-10-01', tz=buoy_tz))),
#        aes(x=datetime, y = value)) +
#   facet_grid(variable~., scales = 'free_y') +
#   geom_point() +
#   final_theme +
#   labs(title = 'sept 2014 air temp raw',
#        x= NULL,
#        y= 'air temp (deg C)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(air_vert,
#               subset=(datetime>=as.POSIXct('2014-10-01', tz=buoy_tz) &
#                         datetime < as.POSIXct('2014-11-01', tz=buoy_tz))),
#        aes(x=datetime, y = value)) +
#   facet_grid(variable~., scales = 'free_y') +
#   geom_point() +
#   final_theme +
#   labs(title = 'oct 2014 air temp raw',
#        x= NULL,
#        y= 'air temp (deg C)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(air_vert,
#               subset=(datetime>=as.POSIXct('2014-11-01', tz=buoy_tz) &
#                         datetime < as.POSIXct('2014-12-01', tz=buoy_tz))),
#        aes(x=datetime, y = value)) +
#   facet_grid(variable~., scales = 'free_y') +
#   geom_point() +
#   final_theme +
#   labs(title = 'nov 2014 air temp raw',
#        x= NULL,
#        y= 'air temp (deg C)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(air_vert,
#               subset=(datetime>=as.POSIXct('2014-12-01', tz=buoy_tz) &
#                         datetime < as.POSIXct('2015-01-01', tz=buoy_tz))),
#        aes(x=datetime, y = value)) +
#   facet_grid(variable~., scales = 'free_y') +
#   geom_point() +
#   final_theme +
#   labs(title = 'jan2014 air temp raw',
#        x= NULL,
#        y= 'air temp (deg C)') +
#   scale_x_datetime(date_minor_breaks = '1 day')

ggplot(air_vert, aes(x=datetime, 
                        y = value,
                        color = location)) +
  facet_grid(variable~., scales = 'free_y') +
  geom_point() +
  final_theme +
  labs(title = '2014 air temp clean',
       x= NULL,
       y= 'air temp (deg C)') +
  scale_x_datetime(date_minor_breaks = '1 month')

#add RH flags for supersaturated 
buoy2014_L1 <- buoy2014_L1 %>% 
  mutate(flag_rh = case_when(RH >100 ~ 's',
                             TRUE ~ ''))

#rename with CV
buoy2014_L1 <- buoy2014_L1 %>% 
  rename(relativeHumidity_perc = RH,
         airTemperature_degC = AirTempC)

rm(air_vert)


#### export L1 data ####
#add 'unknown for buoy location during overwrittend data time
buoy2014_L1 <- buoy2014_L1 %>% 
  mutate(location = case_when(datetime >= as.POSIXct('2014-04-17 14:00', tz=buoy_tz) & datetime < as.POSIXct('2014-06-09 15:00', tz=buoy_tz) ~ 'unknown',
                              TRUE ~ location)) 

colnames(buoy2014_L1)

#export L1 tempstring file
buoy2014_L1 %>%
  select(datetime, location, waterTemperature_degC_0p5m:waterTemperature_degC_10p5m, flag_alltemp, flag_temp10p5m) %>%
  mutate(datetime = as.character(datetime)) %>%
  write_csv(., file.path(dump_dir,'tempstring/2014_tempstring_L1_v2022.csv'))

# export L1 do file
buoy2014_L1 %>%
  select(datetime, location, 
         oxygenDissolved_mgl_1p5m, oxygenDissolvedPercentOfSaturation_pct_1p5m, waterTemperature_DO_degC_1p5m, 
         flag_do1p5m,
         oxygenDissolved_mgl_10p5m, oxygenDissolvedPercentOfSaturation_pct_10p5m, waterTemperature_DO_degC_10p5m, 
         flag_do10p5m,
         offval_do1p5_mgl, offval_do1p5_sat, offval_do10p5_mgl, offval_do10p5_sat,
         oxygenDissolved_mgl_1p5m_withoffval, oxygenDissolvedPercentOfSaturation_pct_1p5m_withoffval,
         oxygenDissolved_mgl_10p5m_withoffval, oxygenDissolvedPercentOfSaturation_pct_10p5m_withoffval) %>%
  mutate(datetime = as.character(datetime)) %>%
  write_csv(., file.path(dump_dir, 'do/2014_do_L1_v2022.csv'))

#export L1 met file
buoy2014_L1 %>%
  select(datetime, location, 
         windSpeedInstantaneous_mps:windGustDirection_deg,
         radiationIncomingPAR_umolm2s, flag_par,
         airTemperature_degC, relativeHumidity_perc, flag_rh) %>%
  mutate(datetime = as.character(datetime)) %>%
  write_csv(., file.path(dump_dir, 'met/2014_met_L1_v2022.csv'))
