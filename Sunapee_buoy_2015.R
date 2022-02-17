#*****************************************************************
#*      Cary Institute of Ecosystem Studies (Millbrook, NY)      *
#*                                                               *
#* TITLE:   Sunapee_buoy_2015.r                                  *
#* AUTHOR:  Bethel Steele                                        *
#* PROJECT: Lake Sunapee Buoy Data Cleaning                      *
#*****************************************************************

source('library_func_lists.R')

#point to data directories
raw_dir = 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L0/'
dump_dir = 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/'

#set tz
buoy_tz = 'Etc/GMT+5'

#bring in 2015 buoy raw data
buoy2015_L0 <- read.csv(file.path(raw_dir, '2015 Buoy Data.csv'))


#### format data ####
buoy2015_L0 <- buoy2015_L0 %>%
  rename(DOLowTempC = 'DOLoTempC',
         InstWindSp = 'WindSpd',
         InstWindDir = 'CorrWind',
         AveWindSp = 'WindSpdAv',
         AveWindDir = 'WindVect',
         MaxWindSp = 'MaxWind') %>% 
  mutate(hour = Hr.Min%/%100,
         minutes = Hr.Min%%100,
         time = paste(hour, minutes, sep=':')) %>% #break out time from Hr.Min, create time column
  mutate(date = as.Date(paste(Day, Year, sep = '-'), format='%j-%Y'), #create date in ymd format
         datetime = as.POSIXct(paste(date, time, sep=' '), format='%Y-%m-%d %H:%M', tz=buoy_tz)) %>%  #tibble forces to UTC, so coerce for our uses.
  select(-hour, -minutes, -Hr.Min, -time, TempC_10m) %>%  #remove unnecessary columns -- 10m not in use
  rowid_to_column(var = 'rowid')

#plot the battery levels
ggplot(buoy2015_L0, aes(x = datetime, y = LoggerBatV)) +
  geom_point()

ggplot(buoy2015_L0, aes(x = datetime, y = RadioBatV)) +
  geom_point()
#these look fine

#double check to make sure there are no DST issues
datelength2015 <- buoy2015_L0 %>% 
  mutate(date = format(datetime, '%Y-%m-%d')) %>% 
  group_by(date) %>% 
  summarize(length(datetime))

#check dst dates
datelength2015[datelength2015$date == '2015-03-08',]
#dst observed here
datelength2015[datelength2015$date == '2015-11-01',]
#dst observed on the second
datelength2015[datelength2015$date == '2015-11-02',]

# see where dst occurs
buoy2015_L0 %>% 
  filter(datetime >= as.POSIXct('2015-03-08', tz=buoy_tz) & 
           datetime < as.POSIXct('2015-03-09', tz=buoy_tz)) %>% 
  select(datetime, rowid)
buoy2015_L0 %>% 
  filter(datetime >= as.POSIXct('2015-11-02', tz=buoy_tz) & 
           datetime < as.POSIXct('2015-11-03', tz=buoy_tz)) %>% 
  select(datetime, rowid)


#DST observed at odd times - 03-08-15 23:00; 11-02-15 00:00
buoy2015_L1 <- buoy2015_L0 %>% 
  mutate(datetime.instrument = datetime)
buoy2015_L1a <- buoy2015_L1 %>% 
  filter(rowid <= 9633)
buoy2015_L1b <- buoy2015_L1 %>% 
  filter(rowid > 9633 & rowid <= 37879)
buoy2015_L1c <- buoy2015_L1 %>% 
  filter(rowid > 37879)
#apply time math on middle section
buoy2015_L1b <- buoy2015_L1b %>% 
  mutate(datetime = datetime-hours(1))

#rejoin and force tz as buoytz
buoy2015_L1 <- full_join(buoy2015_L1a, buoy2015_L1b) %>% 
  full_join(., buoy2015_L1c) %>% 
  mutate(datetime = as.POSIXct(datetime, tz = buoy_tz))

#double check to make sure there are no DST issues
datelength2015 <- buoy2015_L1 %>% 
  mutate(date = format(datetime, '%Y-%m-%d')) %>% 
  group_by(date) %>% 
  summarize(length(datetime))

#check dst dates
datelength2015[datelength2015$date == '2015-03-08',]
#dst observed here
datelength2015[datelength2015$date == '2015-11-01',]
#dst observed on the second
datelength2015[datelength2015$date == '2015-11-02',]


#add all dates/times to record
alltimes_2015 <- as.data.frame(seq.POSIXt(as.POSIXct('2015-01-01 00:00', tz=buoy_tz), as.POSIXct('2015-12-31 23:50', tz=buoy_tz), '10 min')) %>% 
  rename("datetime" = !!names(.[1]))

buoy2015_L1 <- buoy2015_L1 %>% 
  right_join(., alltimes_2015) %>% 
  arrange(datetime)

#clean up workspace
rm(alltimes_2015, alltimes_2015b, alltimes_2015c, datelength2015, buoy2015_L1a, buoy2015_L1b, buoy2015_L1c)


#### 2015 thermistors - remove/replace NA values ####
buoy2015_vert_temp <- buoy2015_L1 %>%
  select(datetime, all_of(alltemp)) %>%
  pivot_longer(names_to = 'variable', values_to = 'value', -datetime) %>% 
  mutate(variable = factor(variable, levels=alltemp))

# ggplot(buoy2015_vert_temp,
#        aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='2015, raw',
#        x=NULL,
#        y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme

# remove 0m and 10m completely
buoy2015_L1 <- buoy2015_L1 %>% 
  mutate(TempC_0m = NA_real_,
         TempC_10m = NA_real_)

buoy2015_L1 <- buoy2015_L1 %>% 
  mutate_at(vars(all_of(alltemp)),
            ~(case_when(. == -6999 ~ NA_real_,
                           . == 1215 ~ NA_real_,
                           . == 555.4 ~ NA_real_,
                           . == 0 ~ NA_real_,
                           TRUE ~ .)))

buoy2015_vert_temp <- buoy2015_L1 %>%
  select(datetime, all_of(alltemp)) %>%
  pivot_longer(names_to = 'variable', values_to = 'value', -datetime) %>% 
  mutate(variable = factor(variable, levels=alltemp))

# ggplot(buoy2015_vert_temp,
#        aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='2015, NAs recoded',
#        x=NULL,
#        y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 month')
#
# 
# ggplot(subset(buoy2015_vert_temp,
#               subset=(datetime >= as.POSIXct('2015-06-01', tz=buoy_tz) & datetime < as.POSIXct('2015-07-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='jun 2015, NAs recoded',
#        x=NULL,
#        y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme+
#     scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2015_vert_temp,
#               subset=(datetime >= as.POSIXct('2015-06-11', tz=buoy_tz) & datetime < as.POSIXct('2015-06-12', tz=buoy_tz))),
#        aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='jun 2015, NAs recoded',
#        x=NULL,
#        y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme+
#   scale_x_datetime(date_minor_breaks = '1 hour')

buoy2015_L1 <- buoy2015_L1 %>% 
  mutate_at(vars(alltemp),
            ~(case_when(datetime<as.POSIXct('2015-06-11 9:00', tz=buoy_tz) ~ NA_real_,
                           TRUE ~ .))) 

buoy2015_vert_temp_L1 <- buoy2015_L1 %>%
  select(datetime, all_of(alltemp)) %>%
  pivot_longer(names_to = 'variable', values_to = 'value', -datetime) %>% 
  mutate(variable = factor(variable, levels=alltemp))

# ggplot(subset(buoy2015_vert_temp_L1,
#               subset=(datetime >= as.POSIXct('2015-06-01', tz=buoy_tz) & datetime < as.POSIXct('2015-07-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='jun 2015, clean',
#        x=NULL,
#        y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme+
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2015_vert_temp,
#               subset=(datetime >= as.POSIXct('2015-07-01', tz=buoy_tz) & datetime < as.POSIXct('2015-08-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='jul 2015, NAs recoded',
#        x=NULL,
#        y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme+
#   scale_x_datetime(date_minor_breaks = '1 day')

#add intermittent flag for jun 25 through jul 10
buoy2015_L1 <- buoy2015_L1 %>% 
  mutate_at(vars(alltemp),
            ~(case_when(datetime>=as.POSIXct('2015-06-25', tz=buoy_tz) & datetime<=as.POSIXct('2015-07-10', tz=buoy_tz) ~ NA_real_,
                               TRUE ~ .)))
buoy2015_vert_temp_L1 <- buoy2015_L1 %>%
  select(datetime, all_of(alltemp)) %>%
  pivot_longer(names_to = 'variable', values_to = 'value', -datetime) %>% 
  mutate(variable = factor(variable, levels=alltemp))

ggplot(buoy2015_vert_temp_L1,
       aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='2015, clean, with flags',
       x=NULL,
       y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 month')

#correct for thermistor offset and add CV
buoy2015_L1 <- buoy2015_L1 %>% 
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

rm(buoy2015_vert_temp, buoy2015_vert_temp_L1)

#### DO ####

# add offset amount from logger at start of data recording
shalsat = -20
shalppm = -2

buoy2015_vert_do <- buoy2015_L1 %>%
  select(datetime, all_of(upDO), all_of(lowDO)) %>%
  pivot_longer(names_to = 'variable', values_to = 'value', -datetime)

# ggplot(buoy2015_vert_do,
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='2015, raw', x='date', y='') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 month')

# remove NA values
buoy2015_L1 <- buoy2015_L1 %>% 
  mutate_at(vars(all_of(upDO), all_of(lowDO)),
            ~(case_when(. == -6999 ~ NA_real_,
                           . == 0 ~ NA_real_,
                           TRUE ~ .)))

buoy2015_vert_do <- buoy2015_L1 %>%
  select(datetime, all_of(upDO), all_of(lowDO)) %>%
  pivot_longer(names_to = 'variable', values_to = 'value', -datetime)

ggplot(buoy2015_vert_do, 
       aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='2015, NAs recoded', x='date', y='') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 month')

buoy2015_L1 <- buoy2015_L1 %>%
  mutate_at(vars(all_of(lowDO)),
            ~(case_when(datetime>=as.POSIXct('2015-01-01', tz=buoy_tz) & datetime<as.POSIXct('2015-06-01', tz=buoy_tz) ~ NA_real_,
                           TRUE ~ .)))
buoy2015_vert_do <- buoy2015_L1 %>%
  select(datetime, all_of(upDO), all_of(lowDO)) %>%
  pivot_longer(names_to = 'variable', values_to = 'value', -datetime)

# ggplot(subset(buoy2015_vert_do,
#               subset=(datetime >= as.POSIXct('2015-01-01', tz=buoy_tz) & datetime < as.POSIXct('2015-02-01', tz=buoy_tz))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='jan 2015, NAs recoded',
#        x=NULL,
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2015_vert_do,
#               subset=(datetime >= as.POSIXct('2015-02-01', tz=buoy_tz) & datetime < as.POSIXct('2015-03-01', tz=buoy_tz))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='feb 2015, NAs recoded',
#        x=NULL,
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2015_vert_do,
#               subset=(datetime >= as.POSIXct('2015-03-01', tz=buoy_tz) & datetime < as.POSIXct('2015-04-01', tz=buoy_tz))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='mar 2015, NAs recoded',
#        x=NULL,
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2015_vert_do,
#               subset=(datetime >= as.POSIXct('2015-04-01', tz=buoy_tz) & datetime < as.POSIXct('2015-05-01', tz=buoy_tz))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='apr 2015, NAs recoded',
#        x=NULL,
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# #errant do April 10
# ggplot(subset(buoy2015_vert_do,
#               subset=(datetime >= as.POSIXct('2015-04-10', tz=buoy_tz) & datetime < as.POSIXct('2015-04-11', tz=buoy_tz))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='apr 2015, NAs recoded',
#        x=NULL,
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour')
# 
# #errant do April 19
# ggplot(subset(buoy2015_vert_do,
#               subset=(datetime >= as.POSIXct('2015-04-19', tz=buoy_tz) & datetime < as.POSIXct('2015-04-20', tz=buoy_tz))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='apr 2015, NAs recoded',
#        x=NULL,
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour')
# 
# #buoy gets pulled Apr 22
# ggplot(subset(buoy2015_vert_do,
#               subset=(datetime >= as.POSIXct('2015-04-22', tz=buoy_tz) & datetime < as.POSIXct('2015-04-23', tz=buoy_tz))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='apr 2015, NAs recoded',
#        x=NULL,
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour')

#buoy offline for repair
buoy2015_L1 <- buoy2015_L1 %>% 
  mutate_at(vars(all_of(upDO)),
            ~(case_when(datetime>=as.POSIXct('2015-04-10 04:20', tz=buoy_tz) & datetime<=as.POSIXct('2015-04-10 06:10', tz=buoy_tz) ~ NA_real_,
                        datetime>=as.POSIXct('2015-04-19 20:30', tz=buoy_tz) & datetime<=as.POSIXct('2015-04-19 21:20', tz=buoy_tz) ~ NA_real_,
                        datetime>=as.POSIXct('2015-04-22 10:00', tz=buoy_tz) & datetime<=as.POSIXct('2015-04-22 10:10', tz=buoy_tz) ~ NA_real_,
                        TRUE ~ .))) %>% 
  mutate(location = case_when(datetime<as.POSIXct('2015-04-22 10:00', tz=buoy_tz) ~ 'harbor',
                              TRUE ~ 'offline'))

#deal with offset -- this is carry over from 2014
# add offset and recalculate data
buoy2015_L1 <- buoy2015_L1 %>% 
  #save offset in column
  mutate(offval_do1p5_mgl = case_when(datetime<as.POSIXct('2015-04-22 10:00', tz=buoy_tz) ~ shalppm,
                                      TRUE ~ 0),
         offval_do1p5_sat = case_when(datetime<as.POSIXct('2015-04-22 10:00', tz=buoy_tz)~ shalsat,
                                      TRUE ~ 0)) %>% 
  #copy data from program to new column during offset period
  mutate(oxygenDissolved_mgl_1p5m_withoffval = case_when(datetime<as.POSIXct('2015-04-22 10:00', tz=buoy_tz)~ DOppm,
                                                         TRUE ~ NA_real_),
         oxygenDissolvedPercentOfSaturation_pct_1p5m_withoffval = case_when(datetime<as.POSIXct('2015-04-22 10:00', tz=buoy_tz) ~ DOSat,
                                                                            TRUE ~ NA_real_)) %>% 
  #add flags for calculated from offset
  mutate(flag_do1p5m = case_when(datetime<as.POSIXct('2015-04-22 10:00', tz=buoy_tz) ~ 'o',
                                 TRUE ~ '')) %>%
  #calculate data without offset
  mutate(DOppm = case_when(flag_do1p5m == 'o' ~ oxygenDissolved_mgl_1p5m_withoffval - offval_do1p5_mgl,
                           TRUE~DOppm),
         DOSat = case_when(flag_do1p5m == 'o' ~ oxygenDissolvedPercentOfSaturation_pct_1p5m_withoffval - offval_do1p5_sat,
                           TRUE~DOSat))

buoy2015_vert_do_L1 <- buoy2015_L1 %>%
  select(datetime, all_of(upDO), all_of(lowDO)) %>%
  pivot_longer(names_to = 'variable', values_to = 'value', -datetime)

# ggplot(subset(buoy2015_vert_do_L1,
#               subset=(datetime >= as.POSIXct('2015-04-01', tz=buoy_tz) & datetime < as.POSIXct('2015-05-01', tz=buoy_tz))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='apr 2015, clean',
#        x=NULL,
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2015_vert_do,
#               subset=(datetime >= as.POSIXct('2015-05-01', tz=buoy_tz) & datetime < as.POSIXct('2015-06-01', tz=buoy_tz))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='may 2015, NAs recoded',
#        x=NULL,
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2015_vert_do,
#               subset=(datetime >= as.POSIXct('2015-06-01', tz=buoy_tz) & datetime < as.POSIXct('2015-07-01', tz=buoy_tz))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='jun 2015, NAs recoded',
#        x=NULL,
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# #buoy in harbor, testing equipment
# ggplot(subset(buoy2015_vert_do,
#               subset=(datetime >= as.POSIXct('2015-06-01', tz=buoy_tz) & datetime < as.POSIXct('2015-06-02', tz=buoy_tz))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='jun 2015, NAs recoded',
#        x=NULL,
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour')
# 
# # buoy moves to loon
# ggplot(subset(buoy2015_vert_do,
#               subset=(datetime >= as.POSIXct('2015-06-11', tz=buoy_tz) & datetime < as.POSIXct('2015-06-12', tz=buoy_tz))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='jun 2015, NAs recoded',
#        x=NULL,
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour')

buoy2015_L1 <- buoy2015_L1 %>% 
  mutate_at(vars(all_of(upDO), all_of(lowDO)),
            ~(case_when(datetime>=as.POSIXct('2015-06-01', tz=buoy_tz) & datetime<as.POSIXct('2015-06-11 09:10', tz=buoy_tz) ~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate_at(vars(DOLowSat, DOLowPPM),
            ~(case_when(datetime>=as.POSIXct('2015-06-01', tz=buoy_tz) & datetime<as.POSIXct('2015-06-11 10:10', tz=buoy_tz) ~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate(location = case_when(datetime>=as.POSIXct('2015-06-01', tz=buoy_tz) &datetime<as.POSIXct('2015-06-11 8:10', tz=buoy_tz) ~ 'harbor',
                              datetime>=as.POSIXct('2015-06-11 8:10', tz=buoy_tz) & datetime<as.POSIXct('2015-06-11 9:10', tz=buoy_tz) ~ 'in transit',
                              datetime>=as.POSIXct('2015-06-11 9:10', tz=buoy_tz) ~ 'loon',
                              TRUE ~ location))%>% 
  mutate(flag_do1p5m = case_when(datetime==as.POSIXct('2015-06-11 9:00', tz=buoy_tz) & flag_do1p5m == '' ~ 'wp',
                                 datetime==as.POSIXct('2015-06-11 9:00', tz=buoy_tz) & flag_do1p5m != '' ~ paste('wp', flag_do1p5m, sep = '; '),
                                   TRUE ~ flag_do1p5m)) %>% 
  mutate(flag_do10p5m = case_when(datetime==as.POSIXct('2015-06-11 9:00', tz=buoy_tz) ~ 'wp',
                                   TRUE ~ ''))

buoy2015_vert_do_L1 <- buoy2015_L1 %>%
  select(datetime, all_of(upDO), all_of(lowDO)) %>%
  pivot_longer(names_to = 'variable', values_to = 'value', -datetime)

# ggplot(subset(buoy2015_vert_do_L1,
#               subset=(datetime >= as.POSIXct('2015-06-01', tz=buoy_tz) & datetime < as.POSIXct('2015-07-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color = location)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='jun 2015, clean',
#        x=NULL,
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2015_vert_do,
#               subset=(datetime >= as.POSIXct('2015-07-01', tz=buoy_tz) & datetime < as.POSIXct('2015-08-01', tz=buoy_tz))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='jul 2015, NAs recoded',
#        x=NULL,
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2015_vert_do,
#               subset=(datetime >= as.POSIXct('2015-08-01', tz=buoy_tz) & datetime < as.POSIXct('2015-09-01', tz=buoy_tz))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='aug 2015, NAs recoded',
#        x=NULL,
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# # aug 13 buoy vis; sensors cleaned
# ggplot(subset(buoy2015_vert_do,
#               subset=(datetime >= as.POSIXct('2015-08-13', tz=buoy_tz) & datetime < as.POSIXct('2015-08-14', tz=buoy_tz))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='aug 2015, NAs recoded',
#        x=NULL,
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour')

#August 13th visit
buoy2015_L1 <-  buoy2015_L1 %>% 
  mutate_at(vars(all_of(lowDO), all_of(upDO)),
            ~(case_when(datetime==as.POSIXct('2015-08-13 09:00', tz=buoy_tz) ~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate(flag_do1p5m = case_when(datetime==as.POSIXct('2015-08-13 09:00', tz=buoy_tz) & flag_do1p5m == '' ~ 'w',
                                 datetime==as.POSIXct('2015-08-13 09:00', tz=buoy_tz) & flag_do1p5m != '' ~ paste('w', flag_do1p5m, sep = '; '),
                                   TRUE ~ flag_do1p5m)) %>% 
  mutate(flag_do10p5m = case_when(datetime==as.POSIXct('2015-08-13 09:00', tz=buoy_tz) & flag_do10p5m == '' ~ 'w',
                                  datetime==as.POSIXct('2015-08-13 09:00', tz=buoy_tz) & flag_do10p5m != '' ~ paste('w', flag_do10p5m, sep = '; '),
                                  TRUE ~ flag_do10p5m))

buoy2015_vert_do_L1 <- buoy2015_L1 %>%
  select(datetime, all_of(upDO), all_of(lowDO), location) %>%
  pivot_longer(names_to = 'variable', values_to = 'value', -c(datetime, location))

# ggplot(subset(buoy2015_vert_do_L1,
#               subset=(datetime >= as.POSIXct('2015-08-01', tz=buoy_tz) & datetime < as.POSIXct('2015-09-01', tz=buoy_tz))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='aug 2015, clean',
#        x=NULL,
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2015_vert_do,
#               subset=(datetime >= as.POSIXct('2015-09-01', tz=buoy_tz) & datetime < as.POSIXct('2015-10-01', tz=buoy_tz))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='sept 2015, NAs recoded',
#        x=NULL,
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2015_vert_do,
#               subset=(datetime >= as.POSIXct('2015-09-19', tz=buoy_tz) & datetime < as.POSIXct('2015-09-20', tz=buoy_tz))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='sept 2015, NAs recoded',
#        x=NULL,
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour')

#errant point around the 19th
buoy2015_L1 <- buoy2015_L1 %>% 
  mutate_at(vars(all_of(upDO)),
            ~(case_when(datetime==as.POSIXct('2015-09-19 4:50', tz=buoy_tz) ~ NA_real_,
                           TRUE ~ .)))

buoy2015_vert_do_L1 <- buoy2015_L1 %>%
  select(datetime, all_of(upDO), all_of(lowDO)) %>%
  pivot_longer(names_to = 'variable', values_to = 'value', -datetime)

# ggplot(subset(buoy2015_vert_do_L1,
#               subset=(datetime >= as.POSIXct('2015-09-01', tz=buoy_tz) & datetime < as.POSIXct('2015-10-01', tz=buoy_tz))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='sept 2015, clean',
#        x=NULL,
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2015_vert_do,
#               subset=(datetime >= as.POSIXct('2015-10-01', tz=buoy_tz) & datetime < as.POSIXct('2015-11-01', tz=buoy_tz))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='oct 2015, NAs recoded',
#        x=NULL,
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2015_vert_do,
#               subset=(datetime >= as.POSIXct('2015-10-08', tz=buoy_tz) & datetime < as.POSIXct('2015-10-09', tz=buoy_tz))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='oct 2015, NAs recoded',
#        x=NULL,
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour')

#remove artifacts of buoy movement and data thereafter, update location data
buoy2015_L1 <- buoy2015_L1 %>% 
  mutate_at(vars(all_of(upDO), all_of(lowDO)),
            ~(case_when(datetime>=as.POSIXct('2015-10-08 09:40', tz=buoy_tz) ~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate(location = case_when(datetime>=as.POSIXct('2015-10-08 09:40', tz=buoy_tz) & buoy2015_L1$datetime<as.POSIXct('2015-10-08 10:30', tz=buoy_tz) ~ 'in transit',
                              datetime>=as.POSIXct('2015-10-08 10:30', tz=buoy_tz) ~ 'harbor',
                              TRUE ~ location))

buoy2015_vert_do_L1 <- buoy2015_L1 %>%
  select(datetime, all_of(upDO), all_of(lowDO)) %>%
  pivot_longer(names_to = 'variable', values_to = 'value', -datetime)

# ggplot(subset(buoy2015_vert_do_L1,
#               subset=(datetime >= as.POSIXct('2015-10-01', tz=buoy_tz) & datetime < as.POSIXct('2015-11-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color = location)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='oct 2015, clean',
#        x=NULL,
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2015_vert_do,
#               subset=(datetime >= as.POSIXct('2015-11-01', tz=buoy_tz) & datetime < as.POSIXct('2015-12-01', tz=buoy_tz))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='nov 2015, NAs recoded',
#        x=NULL,
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2015_vert_do,
#               subset=(datetime >= as.POSIXct('2015-12-01', tz=buoy_tz) & datetime < as.POSIXct('2016-01-01', tz=buoy_tz))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='dec 2015, NAs recoded',
#        x=NULL,
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')


buoy2015_vert_updo <- buoy2015_L1 %>%
  select(datetime, location, all_of(upDO), flag_do1p5m) %>%
  pivot_longer(names_to = 'variable', values_to = 'value', -c(datetime, location, flag_do1p5m))

ggplot(buoy2015_vert_updo, 
       aes(x=datetime, y=value, color = flag_do1p5m, shape = location)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='2015, clean', x='date', y='') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 month')

buoy2015_vert_lowdo <- buoy2015_L1 %>%
  select(datetime, location, all_of(lowDO), flag_do10p5m) %>%
  pivot_longer(names_to = 'variable', values_to = 'value', -c(datetime, location, flag_do10p5m))

ggplot(buoy2015_vert_lowdo, 
       aes(x=datetime, y=value, color = flag_do10p5m, shape = location)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='2015, clean', x='date', y='') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 month')

# add flag for no calibration on record
buoy2015_L1 <- buoy2015_L1 %>% 
  mutate_at(vars(flag_do1p5m, flag_do10p5m),
         ~ case_when(. == '' ~ 'x',
                     . != '' ~ paste('x', ., sep = '; ')))

#rename with CV
buoy2015_L1 <- buoy2015_L1 %>% 
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
         oxygenDissolvedPercentOfSaturation_pct_1p5m_withoffval = case_when(is.na(oxygenDissolvedPercentOfSaturation_pct_1p5m) ~ NA_real_,
                                                                            TRUE ~ oxygenDissolvedPercentOfSaturation_pct_1p5m_withoffval))

rm(buoy2015_vert_do, buoy2015_vert_do_L1, buoy2015_vert_updo, buoy2015_vert_lowdo)

####PAR ####
range(buoy2015_L1$PAR, na.rm = T)

#recode when in transit
buoy2015_L1 <-  buoy2015_L1 %>% 
  mutate(PAR = case_when(location == 'in transit' ~ NA_real_,
                         TRUE ~ PAR))

#replace negative values with 0 and flag
buoy2015_L1 <-  buoy2015_L1 %>% 
  mutate(flag_par = case_when(PAR < 0 ~ 'z',
                              TRUE ~ '')) %>% 
  mutate(PAR = case_when(PAR < 0 ~ 0,
                         TRUE ~ PAR))

ggplot(buoy2015_L1,
       aes(x=datetime, y=PAR, color = location)) +
  geom_point() +
  labs(title='2015, raw', 
       x=NULL, 
       y='PAR (uE*m-2*s-1)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 month')

# #par not functioning until early jun 
# ggplot(subset(buoy2015_L1,
#               subset=(datetime>=as.POSIXct('2015-06-01', tz=buoy_tz) & datetime<as.POSIXct('2015-07-01', tz=buoy_tz))),
#        aes(x=datetime, y=PAR, color = location)) +
#   geom_point() +
#   labs(title='jun2015, raw',
#        x=NULL,
#        y='PAR (uE*m-2*s-1)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')

buoy2015_L1 <-  buoy2015_L1 %>% 
  mutate(PAR = case_when(datetime<as.POSIXct('2015-06-05', tz=buoy_tz) ~ NA_real_,
                         TRUE ~ PAR)) %>% 
  mutate(flag_par =case_when(datetime<as.POSIXct('2015-06-05', tz=buoy_tz) ~ '',
                             TRUE ~ flag_par))

# ggplot(buoy2015_L1,
#        aes(x=datetime, y=PAR, color = location)) +
#   geom_point() +
#   labs(title='2015, partial clean',
#        x=NULL,
#        y='PAR (uE*m-2*s-1)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 month')
# 
# ggplot(buoy2015_L1,
#        aes(x=datetime, y=PAR, color = location)) +
#   geom_point() +
#   labs(title='2015, partial clean', 
#        x=NULL, 
#        y='PAR (uE*m-2*s-1)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 month')
# 
# ggplot(subset(buoy2015_L1,
#               subset=(datetime>=as.POSIXct('2015-07-01', tz=buoy_tz) & datetime<as.POSIXct('2015-08-01', tz=buoy_tz))),
#        aes(x=datetime, y=PAR, color = location)) +
#   geom_point() +
#   labs(title='jul 2015, raw',
#        x=NULL,
#        y='PAR (uE*m-2*s-1)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2015_L1,
#               subset=(datetime>=as.POSIXct('2015-08-01', tz=buoy_tz) & datetime<as.POSIXct('2015-09-01', tz=buoy_tz))),
#        aes(x=datetime, y=PAR, color = location)) +
#   geom_point() +
#   labs(title='aug 2015, raw',
#        x=NULL,
#        y='PAR (uE*m-2*s-1)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2015_L1,
#               subset=(datetime>=as.POSIXct('2015-09-01', tz=buoy_tz) & datetime<as.POSIXct('2015-10-01', tz=buoy_tz))),
#        aes(x=datetime, y=PAR, color = location)) +
#   geom_point() +
#   labs(title='sept 2015, raw',
#        x=NULL,
#        y='PAR (uE*m-2*s-1)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2015_L1,
#               subset=(datetime>=as.POSIXct('2015-10-01', tz=buoy_tz) & datetime<as.POSIXct('2015-11-01', tz=buoy_tz))),
#        aes(x=datetime, y=PAR, color = location)) +
#   geom_point() +
#   labs(title='oct 2015, raw',
#        x=NULL,
#        y='PAR (uE*m-2*s-1)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2015_L1,
#               subset=(datetime>=as.POSIXct('2015-11-01', tz=buoy_tz) & datetime<as.POSIXct('2015-12-01', tz=buoy_tz))),
#        aes(x=datetime, y=PAR, color = location)) +
#   geom_point() +
#   labs(title='nov 2015, raw',
#        x=NULL,
#        y='PAR (uE*m-2*s-1)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2015_L1,
#               subset=(datetime>=as.POSIXct('2015-12-01', tz=buoy_tz) & datetime<as.POSIXct('2016-01-01', tz=buoy_tz))),
#        aes(x=datetime, y=PAR, color = location)) +
#   geom_point() +
#   labs(title='dec 2015, raw',
#        x=NULL,
#        y='PAR (uE*m-2*s-1)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')

ggplot(buoy2015_L1,
       aes(x=datetime, y=PAR, color = location)) +
  geom_point() +
  labs(title='2015, clean', 
       x=NULL, 
       y='PAR (uE*m-2*s-1)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 month')

#rename with CV
buoy2015_L1 <- buoy2015_L1 %>% 
  rename(radiationIncomingPAR_umolm2s = PAR)


####Wind data ####
range(buoy2015_L1$InstWindSp, na.rm = T)
range(buoy2015_L1$InstWindDir, na.rm = T)
range(buoy2015_L1$AveWindSp, na.rm = T)
range(buoy2015_L1$AveWindDir, na.rm = T)
range(buoy2015_L1$MaxWindSp, na.rm = T)
range(buoy2015_L1$MaxWindDir, na.rm = T)

#recode wind data when in transit 
buoy2015_L1 <- buoy2015_L1 %>% 
  mutate_at(vars(InstWindSp, InstWindDir, AveWindSp, AveWindDir, MaxWindSp, MaxWindDir),
            ~case_when(location == 'in transit'~NA_real_,
                       location == 'offline' ~ NA_real_,
                       TRUE ~ .))

buoy2015_vert_wind <- buoy2015_L1 %>%
  select(datetime, location, InstWindSp, InstWindDir, AveWindSp, AveWindDir, MaxWindSp, MaxWindDir) %>%
  pivot_longer(names_to = 'variable', values_to = 'value', -c(datetime, location))

ggplot(buoy2015_vert_wind, 
       aes(x=datetime, y=value, color=location)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='2015, raw', 
       x=NULL, 
       y=NULL) +
  final_theme

# ggplot(subset(buoy2015_vert_wind,
#               subset=(datetime>=as.POSIXct('2015-01-01', tz=buoy_tz) & datetime<as.POSIXct('2015-02-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='jan 2015, raw',
#        x=NULL,
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# #frozen sensor jan 4
# ggplot(subset(buoy2015_vert_wind,
#               subset=(datetime>=as.POSIXct('2015-01-04', tz=buoy_tz) & datetime<as.POSIXct('2015-01-05', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='jan 2015, raw',
#        x=NULL,
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour')

buoy2015_L1 <- buoy2015_L1 %>% 
  mutate_at(vars(InstWindSp, InstWindDir, AveWindSp, AveWindDir, MaxWindSp, MaxWindDir),
            ~(case_when(datetime>=as.POSIXct('2015-01-04 8:40', tz=buoy_tz) & datetime<=as.POSIXct('2015-01-04 9:30', tz=buoy_tz) ~ NA_real_,
                           TRUE ~ .)))

buoy2015_vert_wind_L1 <- buoy2015_L1 %>%
  select(datetime, location, InstWindSp, InstWindDir, AveWindSp, AveWindDir, MaxWindSp, MaxWindDir) %>%
  pivot_longer(names_to = 'variable', values_to = 'value', -c(datetime, location))

# ggplot(subset(buoy2015_vert_wind_L1,
#               subset=(datetime>=as.POSIXct('2015-01-01', tz=buoy_tz) & datetime<as.POSIXct('2015-02-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='jan 2015, raw',
#        x=NULL,
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2015_vert_wind,
#               subset=(datetime>=as.POSIXct('2015-02-01', tz=buoy_tz) & datetime<as.POSIXct('2015-03-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='feb 2015, raw',
#        x=NULL,
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# # frozen sensor jan14-15
# ggplot(subset(buoy2015_vert_wind,
#               subset=(datetime>=as.POSIXct('2015-02-14 18:00', tz=buoy_tz) & datetime<as.POSIXct('2015-02-16', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='feb 2015, raw',
#        x=NULL,
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour')
# 
# #frozen sensor feb 18-19
# ggplot(subset(buoy2015_vert_wind,
#               subset=(datetime>=as.POSIXct('2015-02-18 18:00', tz=buoy_tz) & datetime<as.POSIXct('2015-02-20', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='feb 2015, raw',
#        x=NULL,
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour')

buoy2015_L1 <- buoy2015_L1 %>% 
  mutate_at(vars(InstWindSp, InstWindDir, AveWindSp, AveWindDir, MaxWindSp, MaxWindDir),
            ~(case_when(datetime>=as.POSIXct('2015-02-15 00:50', tz=buoy_tz) & datetime<as.POSIXct('2015-02-15 14:10', tz=buoy_tz) ~ NA_real_,
                           datetime>=as.POSIXct('2015-02-18 22:40', tz=buoy_tz) & datetime<as.POSIXct('2015-02-19 11:00', tz=buoy_tz) ~ NA_real_,
                           TRUE ~ .)))

buoy2015_vert_wind_L1 <- buoy2015_L1 %>%
  select(datetime, location, InstWindSp, InstWindDir, AveWindSp, AveWindDir, MaxWindSp, MaxWindDir) %>%
  pivot_longer(names_to = 'variable', values_to = 'value', -c(datetime, location))

# ggplot(subset(buoy2015_vert_wind_L1,
#               subset=(datetime>=as.POSIXct('2015-02-01', tz=buoy_tz) & datetime<as.POSIXct('2015-03-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='feb 2015, clean',
#        x=NULL,
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2015_vert_wind,
#               subset=(datetime>=as.POSIXct('2015-03-01', tz=buoy_tz) & datetime<as.POSIXct('2015-04-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='mar 2015, raw',
#        x=NULL,
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# #frozen sensor march 14-16
# ggplot(subset(buoy2015_vert_wind,
#               subset=(datetime>=as.POSIXct('2015-03-14', tz=buoy_tz) & datetime<as.POSIXct('2015-03-16', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='mar 2015, raw',
#        x=NULL,
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour')

buoy2015_L1 <- buoy2015_L1 %>% 
  mutate_at(vars(InstWindSp, InstWindDir, AveWindSp, AveWindDir, MaxWindSp, MaxWindDir),
            ~(case_when(datetime>=as.POSIXct('2015-03-14 12:00', tz=buoy_tz) & 
                          datetime<as.POSIXct('2015-03-15 12:00', tz=buoy_tz) &
                          MaxWindSp == 0 ~ NA_real_,
                        TRUE ~ .)))

buoy2015_vert_wind_L1 <- buoy2015_L1 %>%
  select(datetime, location, InstWindSp, InstWindDir, AveWindSp, AveWindDir, MaxWindSp, MaxWindDir) %>%
  pivot_longer(names_to = 'variable', values_to = 'value', -c(datetime, location))

# ggplot(subset(buoy2015_vert_wind_L1,
#               subset=(datetime>=as.POSIXct('2015-03-01', tz=buoy_tz) & datetime<as.POSIXct('2015-04-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='mar 2015, raw',
#        x=NULL,
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2015_vert_wind,
#               subset=(datetime>=as.POSIXct('2015-04-01', tz=buoy_tz) & datetime<as.POSIXct('2015-05-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='apr 2015, raw',
#        x=NULL,
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# #frozen sensor apr 10
# ggplot(subset(buoy2015_vert_wind,
#               subset=(datetime>=as.POSIXct('2015-04-10', tz=buoy_tz) & datetime<as.POSIXct('2015-04-11', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='apr 2015, raw',
#        x=NULL,
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour')

buoy2015_L1 <- buoy2015_L1 %>% 
  mutate_at(vars(InstWindSp, InstWindDir, AveWindSp, AveWindDir, MaxWindSp, MaxWindDir),
            ~(case_when(datetime>=as.POSIXct('2015-04-10 05:00', tz=buoy_tz) & 
                          datetime<as.POSIXct('2015-04-10 09:00', tz=buoy_tz) &
                          MaxWindSp == 0 ~ NA_real_,
                        TRUE ~ .)))

buoy2015_vert_wind_L1 <- buoy2015_L1 %>%
  select(datetime, location, InstWindSp, InstWindDir, AveWindSp, AveWindDir, MaxWindSp, MaxWindDir) %>%
  pivot_longer(names_to = 'variable', values_to = 'value', -c(datetime, location))

# ggplot(subset(buoy2015_vert_wind_L1,
#               subset=(datetime>=as.POSIXct('2015-04-01', tz=buoy_tz) & datetime<as.POSIXct('2015-05-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='apr 2015, raw',
#        x=NULL,
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2015_vert_wind,
#               subset=(datetime>=as.POSIXct('2015-05-01', tz=buoy_tz) & datetime<as.POSIXct('2015-06-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='may 2015, raw',
#        x=NULL,
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2015_vert_wind,
#               subset=(datetime>=as.POSIXct('2015-06-01', tz=buoy_tz) & datetime<as.POSIXct('2015-07-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='jun 2015, raw',
#        x=NULL,
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2015_vert_wind,
#               subset=(datetime>=as.POSIXct('2015-06-04', tz=buoy_tz) & datetime<as.POSIXct('2015-06-05', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='jun 2015, raw',
#        x=NULL,
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour')


buoy2015_L1 <- buoy2015_L1 %>% 
  mutate_at(vars(InstWindSp, InstWindDir, AveWindSp, AveWindDir, MaxWindSp, MaxWindDir),
            ~(case_when(datetime>=as.POSIXct('2015-06-01 00:00', tz=buoy_tz) & datetime<=as.POSIXct('2015-06-04 08:10', tz=buoy_tz) ~ NA_real_,
                           TRUE ~ .)))

buoy2015_vert_wind_L1 <- buoy2015_L1 %>%
  select(datetime, InstWindSp, InstWindDir, AveWindSp, AveWindDir, MaxWindSp, MaxWindDir, location) %>%
  pivot_longer(names_to = 'variable', values_to = 'value', -c(datetime, location))

# ggplot(subset(buoy2015_vert_wind_L1,
#               subset=(datetime>=as.POSIXct('2015-06-01', tz=buoy_tz) & datetime<as.POSIXct('2015-07-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='jun 2015, clean',
#        x=NULL,
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')

# ggplot(subset(buoy2015_vert_wind,
#               subset=(datetime>=as.POSIXct('2015-07-01', tz=buoy_tz) & datetime<as.POSIXct('2015-08-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='jul 2015, raw',
#        x=NULL,
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2015_vert_wind,
#               subset=(datetime>=as.POSIXct('2015-08-01', tz=buoy_tz) & datetime<as.POSIXct('2015-09-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='jul 2015, raw',
#        x=NULL,
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2015_vert_wind,
#               subset=(datetime>=as.POSIXct('2015-09-01', tz=buoy_tz) & datetime<as.POSIXct('2015-10-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='sept 2015, raw',
#        x=NULL,
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2015_vert_wind,
#               subset=(datetime>=as.POSIXct('2015-10-01', tz=buoy_tz) & datetime<as.POSIXct('2015-11-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='oct 2015, raw',
#        x=NULL,
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2015_vert_wind,
#               subset=(datetime>=as.POSIXct('2015-11-01', tz=buoy_tz) & datetime<as.POSIXct('2015-12-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='nov 2015, raw',
#        x=NULL,
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2015_vert_wind,
#               subset=(datetime>=as.POSIXct('2015-12-01', tz=buoy_tz) & datetime<as.POSIXct('2016-01-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='dec 2015, raw',
#        x=NULL,
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# #frozen sesnor dec 30
# ggplot(subset(buoy2015_vert_wind,
#               subset=(datetime>=as.POSIXct('2015-12-30', tz=buoy_tz) & datetime<as.POSIXct('2015-12-31', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='dec 2015, raw',
#        x=NULL,
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour')
# 
# ggplot(subset(buoy2015_vert_wind,
#               subset=(datetime>=as.POSIXct('2015-12-31', tz=buoy_tz) & datetime<as.POSIXct('2016-01-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='dec 2015, raw',
#        x=NULL,
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour')


buoy2015_L1 <- buoy2015_L1 %>% 
  mutate_at(vars(InstWindSp, InstWindDir, AveWindSp, AveWindDir, MaxWindSp, MaxWindDir),
            ~(case_when(datetime>=as.POSIXct('2015-12-30', tz=buoy_tz) & 
                          datetime<as.POSIXct('2015-12-31 7:20', tz=buoy_tz) & 
                          MaxWindSp == 0 ~ NA_real_,
                           TRUE ~ .)))
buoy2015_vert_wind_L1 <- buoy2015_L1 %>%
  select(datetime, InstWindSp, InstWindDir, AveWindSp, AveWindDir, MaxWindSp, MaxWindDir, location) %>%
  pivot_longer(names_to = 'variable', values_to = 'value', -c(datetime, location))


# ggplot(subset(buoy2015_vert_wind_L1,
#               subset=(datetime>=as.POSIXct('2015-12-01', tz=buoy_tz) & datetime<as.POSIXct('2016-01-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='dec 2015, clean',
#        x=NULL,
#        y=NULL) +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')

ggplot(buoy2015_vert_wind_L1,
       aes(x=datetime, y=value, color=location)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='2015, clean',
       x=NULL,
       y=NULL) +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 month')

rm(buoy2015_vert_wind, buoy2015_vert_wind_L1)

#rename with CV
buoy2015_L1 <- buoy2015_L1 %>% 
  rename(windDirectionInstantaneous_deg =InstWindDir, 
         windSpeedInstantaneous_mps = InstWindSp,
         windDirectionAverage_deg = AveWindDir,
         windSpeedAverage_mps = AveWindSp,
         windGustDirection_deg = MaxWindDir,
         windGustSpeed_mps = MaxWindSp)

# ####chla data ####
## SENSOR NEVER FUNCTIONED CORRECTLY ####
# buoy2015_vert_chla <- buoy2015_L1 %>%
#   select(datetime, chla) %>%
#   gather(variable, value, -datetime)
# 
# #recode cond readings to NA for enitre year
# buoy2015_L1 <- buoy2015_L1 %>% 
#   mutate(SpecCond = NA_real_) %>% 
#   mutate_at(vars(Chlor_RFU, Chlor_UGL),
#             ~(case_when(. == -6999 ~ NA_real_,
#                            . == 6999 ~ NA_real_,
#                            TRUE ~ .)))
# 
# buoy2015_vert_chla <- buoy2015_L1 %>%
#   select(datetime, chla) %>%
#   gather(variable, value, -datetime)
# 
# # ggplot(buoy2015_vert_chla, aes(x=datetime, y = value)) +
# #   geom_point() +
# #   facet_grid(variable ~ .) +
# #   final_theme +
# #   labs(title = 'chl-a and cond 2015, NA values recoded',
# #        x=NULL,
# #        y=NULL) +
# #   scale_x_datetime(date_minor_breaks = '1 month')
# # 
# # ggplot(subset(buoy2015_vert_chla, 
# #               subset = (datetime>=as.POSIXct('2015-06-01', tz=buoy_tz) & datetime< as.POSIXct('2015-07-01', tz=buoy_tz))),
# #               aes(x=datetime, y = value)) +
# #   geom_point() +
# #   facet_grid(variable ~ .) +
# #   final_theme +
# #   labs(title = 'chl-a and cond jun 2015, NA values recoded',
# #        x=NULL,
# #        y=NULL) +
# #   scale_x_datetime(date_minor_breaks = '1 day')
# 
# #recode data before move to loon
# buoy2015_L1 <- buoy2015_L1 %>% 
#   mutate_at(vars(Chlor_RFU, Chlor_UGL),
#             ~(case_when(datetime< as.POSIXct('2015-06-11 10:10', tz=buoy_tz) ~ NA_real_,
#                            TRUE ~ .))) %>% 
#   mutate(chla_flag = case_when(datetime == as.POSIXct('2015-06-11 10:10', tz=buoy_tz) ~ 'cp',
#                                TRUE ~ ''))
# 
# #recode - data as 0
# buoy2015_L1 <- buoy2015_L1 %>% 
#   mutate(Chlor_UGL = case_when(Chlor_UGL < 0 ~ 0,
#                                TRUE ~ Chlor_UGL))
# 
# buoy2015_vert_chla_L1 <- buoy2015_L1 %>%
#   select(datetime, chla) %>%
#   gather(variable, value, -datetime)
# 
# # ggplot(subset(buoy2015_vert_chla_L1, 
# #               subset = (datetime>=as.POSIXct('2015-06-01', tz=buoy_tz) & datetime< as.POSIXct('2015-07-01', tz=buoy_tz))),
# #        aes(x=datetime, y = value)) +
# #   geom_point() +
# #   facet_grid(variable ~ .) +
# #   final_theme +
# #   labs(title = 'chl-a and cond jun 2015, NA values recoded',
# #        x=NULL,
# #        y=NULL) +
# #   scale_x_datetime(date_minor_breaks = '1 day')
# # 
# # ggplot(subset(buoy2015_vert_chla, 
# #               subset = (datetime>=as.POSIXct('2015-07-01', tz=buoy_tz) & datetime< as.POSIXct('2015-08-01', tz=buoy_tz))),
# #        aes(x=datetime, y = value)) +
# #   geom_point() +
# #   facet_grid(variable ~ ., scales = 'free_y') +
# #   final_theme +
# #   labs(title = 'chl-a and cond jul 2015, NA values recoded',
# #        x=NULL,
# #        y=NULL) +
# #   scale_x_datetime(date_minor_breaks = '1 day')
# # 
# # #recode outlier on jul 16 
# # ggplot(subset(buoy2015_vert_chla, 
# #               subset = (datetime>=as.POSIXct('2015-07-16', tz=buoy_tz) & datetime< as.POSIXct('2015-07-17', tz=buoy_tz))),
# #        aes(x=datetime, y = value)) +
# #   geom_point() +
# #   facet_grid(variable ~ .) +
# #   final_theme +
# #   labs(title = 'chl-a and cond jul 2015, NA values recoded',
# #        x=NULL,
# #        y=NULL) +
# #   scale_x_datetime(date_minor_breaks = '1 hour')
# 
# buoy2015_L1 <- buoy2015_L1 %>% 
#   mutate_at(vars(Chlor_RFU, Chlor_UGL),
#             ~(case_when(datetime==as.POSIXct('2015-07-16 21:20', tz=buoy_tz) ~ NA_real_,
#                            TRUE ~ .))) %>% 
#   mutate(chla_flag = case_when(datetime >= as.POSIXct('2015-07-23', tz=buoy_tz) ~ 's',
#                                TRUE ~ chla_flag))
# buoy2015_vert_chla_L1 <- buoy2015_L1 %>%
#   select(datetime, chla) %>%
#   gather(variable, value, -datetime)
# 
# # ggplot(subset(buoy2015_vert_chla_L1, 
# #               subset = (datetime>=as.POSIXct('2015-07-01', tz=buoy_tz) & datetime< as.POSIXct('2015-08-01', tz=buoy_tz))),
# #        aes(x=datetime, y = value)) +
# #   geom_point() +
# #   facet_grid(variable ~ ., scales = 'free_y') +
# #   final_theme +
# #   labs(title = 'chl-a and cond jul 2015, NA values recoded',
# #        x=NULL,
# #        y=NULL) +
# #   scale_x_datetime(date_minor_breaks = '1 day')
# # 
# # ggplot(subset(buoy2015_vert_chla, 
# #               subset = (datetime>=as.POSIXct('2015-08-01', tz=buoy_tz) & datetime< as.POSIXct('2015-09-01', tz=buoy_tz))),
# #        aes(x=datetime, y = value)) +
# #   geom_point() +
# #   facet_grid(variable ~ ., scales = 'free_y') +
# #   final_theme +
# #   labs(title = 'chl-a and cond aug 2015, NA values recoded',
# #        x=NULL,
# #        y=NULL) +
# #   scale_x_datetime(date_minor_breaks = '1 day')
# # 
# # ggplot(subset(buoy2015_vert_chla, 
# #               subset = (datetime>=as.POSIXct('2015-08-13', tz=buoy_tz) & datetime< as.POSIXct('2015-08-14', tz=buoy_tz))),
# #        aes(x=datetime, y = value)) +
# #   geom_point() +
# #   facet_grid(variable ~ .) +
# #   final_theme +
# #   labs(title = 'chl-a and cond jun 2015, NA values recoded',
# #        x=NULL,
# #        y=NULL) +
# #   scale_x_datetime(date_minor_breaks = '1 hour')
# 
# buoy2015_L1 <- buoy2015_L1 %>% 
#   mutate_at(vars(Chlor_RFU, Chlor_UGL),
#             ~(case_when(datetime>=as.POSIXct('2015-08-13 10:10', tz=buoy_tz) ~ NA_real_,
#                            TRUE ~ .))) %>% 
#   mutate(chla_flag = case_when(datetime >= as.POSIXct('2015-08-13 10:10', tz=buoy_tz) ~ '',
#                                TRUE ~ chla_flag))
# 
# buoy2015_vert_chla_L1 <- buoy2015_L1 %>%
#   select(datetime, chla, chla_flag) %>%
#   gather(variable, value, -datetime, -chla_flag)
# 
# # ggplot(subset(buoy2015_vert_chla_L1, 
# #               subset = (datetime>=as.POSIXct('2015-08-01', tz=buoy_tz) & datetime< as.POSIXct('2015-09-01', tz=buoy_tz))),
# #        aes(x=datetime, y = value)) +
# #   geom_point() +
# #   facet_grid(variable ~ ., scales = 'free_y') +
# #   final_theme +
# #   labs(title = 'chl-a and cond aug 2015, NA values recoded',
# #        x=NULL,
# #        y=NULL) +
# #   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(buoy2015_vert_chla_L1, 
#        aes(x=datetime, y = value, color = chla_flag)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   final_theme +
#   labs(title = 'chl-a and cond 2015, clean with flags',
#        x=NULL,
#        y=NULL) +
#   scale_x_datetime(date_minor_breaks = '1 month') +
#   scale_color_colorblind()
# 
# rm(buoy2015_vert_chla, buoy2015_vert_chla_L1)

#### air temp & rh ####
range(buoy2015_L1$AirTempC, na.rm = T)
range(buoy2015_L1$RelHum, na.rm = T)

buoy2015_L1 <- buoy2015_L1 %>% 
  mutate_at(vars(AirTempC, RelHum),
            ~ case_when(location == 'in transit' ~ NA_real_,
                        location == 'offline' ~ NA_real_,
                        TRUE ~.))

air_vert <- buoy2015_L1 %>% 
  select(datetime, AirTempC, RelHum, location) %>% 
  pivot_longer(names_to = 'variable', values_to = 'value', -c(datetime, location))

ggplot(air_vert, aes(x=datetime, y = value)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  final_theme +
  labs(title = '2015 air temp raw',
       x= NULL,
       y= 'air temp (deg C)') +
  scale_x_datetime(date_minor_breaks = '1 month')

#no NA values to recode

# ggplot(subset(air_vert,
#               subset=(datetime>=as.POSIXct('2015-01-01', tz=buoy_tz) & datetime<as.POSIXct('2015-02-01', tz=buoy_tz))),
#        aes(x=datetime, value, color = location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'jan 2015 air temp', x=NULL, y='Air temp (deg C)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(air_vert,
#               subset=(datetime>=as.POSIXct('2015-02-01', tz=buoy_tz) & datetime<as.POSIXct('2015-03-01', tz=buoy_tz))),
#        aes(x=datetime, value, color = location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'feb 2015 air temp', x=NULL, y='Air temp (deg C)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(air_vert,
#               subset=(datetime>=as.POSIXct('2015-03-01', tz=buoy_tz) & datetime<as.POSIXct('2015-04-01', tz=buoy_tz))),
#        aes(x=datetime, value, color = location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'mar 2015 air temp', x=NULL, y='Air temp (deg C)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(air_vert,
#               subset=(datetime>=as.POSIXct('2015-04-01', tz=buoy_tz) & datetime<as.POSIXct('2015-05-01', tz=buoy_tz))),
#        aes(x=datetime, value, color = location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'apr 2015 air temp', x=NULL, y='Air temp (deg C)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
 
#data gap

# ggplot(subset(air_vert,
#               subset=(datetime>=as.POSIXct('2015-06-01', tz=buoy_tz) & datetime<as.POSIXct('2015-07-01', tz=buoy_tz))),
#        aes(x=datetime, value, color = location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'jun 2015 air temp', x=NULL, y='Air temp (deg C)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(air_vert,
#               subset=(datetime>=as.POSIXct('2015-06-04', tz=buoy_tz) & datetime<as.POSIXct('2015-06-05', tz=buoy_tz))),
#        aes(x=datetime, value, color = location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'jun 2015 air temp', x=NULL, y='Air temp (deg C)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour')

#recode data prior to data columns being fixed and data during buoy move
buoy2015_L1 <- buoy2015_L1 %>% 
  mutate_at(vars(AirTempC,RelHum),
            ~case_when(datetime>=as.POSIXct('2015-06-01 00:00', tz=buoy_tz) & datetime<=as.POSIXct('2015-06-04 08:00', tz=buoy_tz) ~ NA_real_,
                              TRUE ~ .))
air_vert <- buoy2015_L1 %>% 
  select(datetime, AirTempC, RelHum, location) %>% 
  pivot_longer(names_to = 'variable', values_to = 'value', -c(datetime, location))

# ggplot(subset(air_vert,
#               subset=(datetime>=as.POSIXct('2015-06-01', tz=buoy_tz) & datetime<as.POSIXct('2015-07-01', tz=buoy_tz))),
#        aes(x=datetime, value, color = location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'jun 2015 air temp', x=NULL, y='Air temp (deg C)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(air_vert,
#               subset=(datetime>=as.POSIXct('2015-07-01', tz=buoy_tz) & datetime<as.POSIXct('2015-08-01', tz=buoy_tz))),
#        aes(x=datetime, value, color = location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'jul 2015 air temp', x=NULL, y='Air temp (deg C)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(air_vert,
#               subset=(datetime>=as.POSIXct('2015-08-01', tz=buoy_tz) & datetime<as.POSIXct('2015-09-01', tz=buoy_tz))),
#        aes(x=datetime, value, color = location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'aug 2015 air temp', x=NULL, y='Air temp (deg C)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(air_vert,
#               subset=(datetime>=as.POSIXct('2015-09-01', tz=buoy_tz) & datetime<as.POSIXct('2015-10-01', tz=buoy_tz))),
#        aes(x=datetime, value, color = location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'sept 2015 air temp', x=NULL, y='Air temp (deg C)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(air_vert,
#               subset=(datetime>=as.POSIXct('2015-10-01', tz=buoy_tz) & datetime<as.POSIXct('2015-11-01', tz=buoy_tz))),
#        aes(x=datetime, value, color = location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'oct 2015 air temp', x=NULL, y='Air temp (deg C)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(air_vert,
#               subset=(datetime>=as.POSIXct('2015-11-01', tz=buoy_tz) & datetime<as.POSIXct('2015-12-01', tz=buoy_tz))),
#        aes(x=datetime, value, color = location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'nov 2015 air temp', x=NULL, y='Air temp (deg C)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(air_vert,
#               subset=(datetime>=as.POSIXct('2015-12-01', tz=buoy_tz) & datetime<as.POSIXct('2016-01-01', tz=buoy_tz))),
#        aes(x=datetime, value, color = location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'dec 2015 air temp', x=NULL, y='Air temp (deg C)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')

#add RH flags for supersaturated 
buoy2015_L1 <- buoy2015_L1 %>% 
  mutate(flag_rh = case_when(RelHum >100 ~ 's',
                             TRUE ~ ''))

#rename with CV
buoy2015_L1 <- buoy2015_L1 %>% 
  rename(relativeHumidity_perc = RelHum,
         airTemperature_degC = AirTempC)

rm(air_vert)


#### EXPORT L1 DATA ####

colnames(buoy2015_L1)

#export L1 tempstring file
buoy2015_L1 %>%
  select(datetime, location, waterTemperature_degC_1p5m:waterTemperature_degC_9p5m) %>%
  mutate(datetime = as.character(datetime)) %>%
  write_csv(., file.path(dump_dir,'tempstring/2015_tempstring_L1_v2022.csv'))

# export L1 do file
buoy2015_L1 %>%
  select(datetime, location, 
         oxygenDissolved_mgl_1p5m, oxygenDissolvedPercentOfSaturation_pct_1p5m, waterTemperature_DO_degC_1p5m, 
         flag_do1p5m,
         oxygenDissolved_mgl_10p5m, oxygenDissolvedPercentOfSaturation_pct_10p5m, waterTemperature_DO_degC_10p5m, 
         flag_do10p5m,
         offval_do1p5_mgl, offval_do1p5_sat, 
         oxygenDissolved_mgl_1p5m_withoffval, oxygenDissolvedPercentOfSaturation_pct_1p5m_withoffval) %>%
  mutate(datetime = as.character(datetime)) %>%
  write_csv(., file.path(dump_dir, 'do/2015_do_L1_v2022.csv'))

#export L1 met file
buoy2015_L1 %>%
  select(datetime, location, 
         windSpeedInstantaneous_mps:windGustDirection_deg,
         radiationIncomingPAR_umolm2s, flag_par,
         airTemperature_degC, relativeHumidity_perc, flag_rh) %>%
  mutate(datetime = as.character(datetime)) %>%
  write_csv(., file.path(dump_dir, 'met/2015_met_L1_v2022.csv'))
  
