#*****************************************************************
#*      Cary Institute of Ecosystem Studies (Millbrook, NY)      *
#*                                                               *
#* TITLE:   Sunapee_buoy_2017.r                                  *
#* AUTHOR:  Bethel Steele                                        *
#* PROJECT: Lake Sunapee Buoy Data Cleaning                      *
#*****************************************************************

source('library_func_lists.R')

#point to data directories
raw_dir = 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L0/'
dump_dir = 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/'

#set tz
buoy_tz = 'Etc/GMT+5'

#bring in 2017 buoy raw data
buoy2017_L0 <- read.csv(file.path(raw_dir, '2017 Buoy Data.csv'))
                        
#### format data ####
buoy2017_L0 <- buoy2017_L0  %>%
  rename(DOLowTempC = 'DOLoTempC',
         AveWindSp = 'WindSpdAv',
         AveWindDir = 'WindVect',
         MaxWindSp = 'MaxWind') %>% 
  mutate(hour = Hr.Min%/%100,
         minutes = Hr.Min%%100,
         time = paste(hour, minutes, sep=':')) %>% #break out time from Hr.Min, create time column
  mutate(date = as.Date(paste(Day, Year, sep = '-'), format='%j-%Y'), #create date in ymd format
         datetime = as.POSIXct(paste(date, time, sep=' '), format='%Y-%m-%d %H:%M', tz=buoy_tz)) %>%  #tibble forces to UTC, so coerce for our uses.
  select(-hour, -minutes, -Hr.Min, -Year, -Day, -time, -ArrayID) %>%  #remove unnecessary columns
  rownames_to_column(var ='rowid')

#plot the battery levels
ggplot(buoy2017_L0, aes(x = datetime, y = LoggerBatV)) +
  geom_point()

ggplot(buoy2017_L0, aes(x = datetime, y = RadioBatV)) +
  geom_point()
# looks fine until end of year; documented issues in log

#double check to make sure there are no DST issues
datelength2017 <- buoy2017_L0 %>% 
  mutate(date = format(datetime, '%Y-%m-%d')) %>% 
  group_by(date) %>% 
  summarize(length(datetime))

#check dst dates
datelength2017[datelength2017$date == '2017-03-12',]
#no data
datelength2017[datelength2017$date == '2017-11-05',]
#dst observed on the 6th
datelength2017[datelength2017$date == '2017-11-06',]

# see where dst occurs
buoy2017_L0 %>% 
  filter(datetime >= as.POSIXct('2017-03-01', tz=buoy_tz) & 
           datetime < as.POSIXct('2017-03-08', tz=buoy_tz)) %>% 
  select(datetime, rowid)

buoy2017_L0 %>% 
  filter(datetime >= as.POSIXct('2017-11-06', tz=buoy_tz) & 
           datetime < as.POSIXct('2017-11-07', tz=buoy_tz)) %>% 
  select(datetime, rowid)


#DST observed at odd times - 03-08-15 23:00; 11-02-15 00:00
buoy2017_L1 <- buoy2017_L0 %>% 
  mutate(datetime.instrument = datetime,
         rowid = as.numeric(rowid))
buoy2017_L1a <- buoy2017_L1 %>% 
  filter(rowid <= 8994)
buoy2017_L1b <- buoy2017_L1 %>% 
  filter(rowid > 8994 & rowid <= 39451)
buoy2017_L1c <- buoy2017_L1 %>% 
  filter(rowid > 39451)
#apply time math on middle section
buoy2017_L1b <- buoy2017_L1b %>% 
  mutate(datetime = datetime-hours(1))

#rejoin and force tz as buoytz
buoy2017_L1 <- full_join(buoy2017_L1a, buoy2017_L1b) %>% 
  full_join(., buoy2017_L1c) %>% 
  mutate(datetime = as.POSIXct(datetime, tz = buoy_tz))

#double check to make sure there are no DST issues
datelength2017 <- buoy2017_L1 %>% 
  mutate(date = format(datetime, '%Y-%m-%d')) %>% 
  group_by(date) %>% 
  summarize(length(datetime))

datelength2017[datelength2017$date == '2017-11-05',]
datelength2017[datelength2017$date == '2017-11-06',]
#all good!

# add in all date time options in L1 data set
alltimes_2017 <- as.data.frame(seq.POSIXt(as.POSIXct('2017-01-01 00:00', tz=buoy_tz), as.POSIXct('2017-12-31 23:50', tz=buoy_tz), '10 min')) %>% 
  rename("datetime" = !!names(.[1]))

buoy2017_L1 <- buoy2017_L1 %>% 
  right_join(., alltimes_2017) %>% 
  arrange(datetime)


#clean up workspace
rm(alltimes_2017, datelength2017, buoy2017_L1a, buoy2017_L1b, buoy2017_L1c)



####THERMISTORS####
buoy2017_therm_vert <- buoy2017_L1 %>% 
  select(datetime, all_of(alltemp2011)) %>% 
  pivot_longer(names_to = 'variable', values_to = 'value', -datetime) %>% 
  mutate(variable = factor(variable, levels=alltemp))

# ggplot(buoy2017_therm_vert, aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 month') +
#   final_theme

buoy2017_L1 <- buoy2017_L1 %>% 
  mutate_at(vars(all_of(alltemp2011)),
            ~(case_when(. == 1215 ~ NA_real_,
                           . == 555.4 ~ NA_real_,
                           . == -6999 ~ NA_real_,
                           TRUE ~ .)))

buoy2017_therm_vert <- buoy2017_L1 %>% 
  select(datetime, all_of(alltemp2011)) %>% 
  pivot_longer(names_to = 'variable', values_to = 'value', -datetime) %>% 
  mutate(variable = factor(variable, levels=alltemp))

# ggplot(buoy2017_therm_vert,
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 month') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme

#tease out issues with 0m again
ggplot(buoy2017_L1, aes(x = datetime, y = TempC_0m)) +
  geom_point()

#looks like only end of record is 'good', but here goes nothing:
buoy2017_L1 <- buoy2017_L1 %>% 
  mutate(TempC_0m = case_when(TempC_0m >30 ~ NA_real_,
                              TempC_0m == 9| TempC_0m == 15 |TempC_0m <=5 ~ NA_real_,
                              TRUE ~ TempC_0m))

ggplot(buoy2017_L1, aes(x = datetime, y = TempC_0m)) +
  geom_point()

#can work with that! add a flag for intermittent data
buoy2017_L1 <- buoy2017_L1 %>% 
  mutate(flag_temp_0p85m = 'i')

buoy2017_therm_vert <- buoy2017_L1 %>% 
  select(datetime, all_of(alltemp2011)) %>% 
  pivot_longer(names_to = 'variable', values_to = 'value', -datetime) %>% 
  mutate(variable = factor(variable, levels=alltemp))

# ggplot(subset(buoy2017_therm_vert,
#               subset=(datetime >= as.POSIXct('2017-05-01', tz=buoy_tz) & datetime < as.POSIXct('2017-06-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme
# 
# #buoy offline until 17th, was in the water for a day before mooring broke; offline again until 14jun
# ggplot(subset(buoy2017_therm_vert,
#               subset=(datetime >= as.POSIXct('2017-05-17', tz=buoy_tz) & datetime < as.POSIXct('2017-05-18', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme
# ggplot(subset(buoy2017_therm_vert,
#               subset=(datetime >= as.POSIXct('2017-05-19', tz=buoy_tz) & datetime < as.POSIXct('2017-05-20', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme
 
buoy2017_L1 <- buoy2017_L1 %>% 
  mutate_at(vars(all_of(alltemp2011)),
            ~(case_when(datetime < as.POSIXct('2017-05-17 10:10', tz=buoy_tz) ~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate(TempC_0m = case_when(datetime >= as.POSIXct('2017-05-19 08:00') &
                                datetime <as.Date('2017-06-01') ~ NA_real_,
                              TRUE ~ TempC_0m))
buoy2017_therm_vert_L1 <- buoy2017_L1 %>% 
  select(datetime, all_of(alltemp2011)) %>% 
  pivot_longer(names_to = 'variable', values_to = 'value', -datetime) %>% 
  mutate(variable = factor(variable, levels=alltemp))

# ggplot(subset(buoy2017_therm_vert_L1,
#               subset=(datetime >= as.POSIXct('2017-05-01', tz=buoy_tz) & datetime < as.POSIXct('2017-06-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme
# 
# ggplot(subset(buoy2017_therm_vert_L1,
#               subset=(datetime >= as.POSIXct('2017-06-01', tz=buoy_tz) & datetime < as.POSIXct('2017-07-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme
#
# #only 8 and 9m reporting until jul 7
# ggplot(subset(buoy2017_therm_vert_L1,
#               subset=(datetime >= as.POSIXct('2017-07-01', tz=buoy_tz) & datetime < as.POSIXct('2017-08-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme

buoy2017_L1 <- buoy2017_L1 %>% 
  mutate_at(vars(TempC_0m:TempC_7m),
            ~(case_when(datetime >= as.POSIXct('2017-06-14', tz=buoy_tz) & datetime < as.POSIXct('2017-07-24', tz=buoy_tz) ~ NA_real_,
                           TRUE ~ .)))
buoy2017_therm_vert_L1 <- buoy2017_L1 %>% 
  select(datetime, all_of(alltemp2011)) %>% 
  pivot_longer(names_to = 'variable', values_to = 'value', -datetime) %>% 
  mutate(variable = factor(variable, levels=alltemp))

# ggplot(subset(buoy2017_therm_vert_L1,
#               subset=(datetime >= as.POSIXct('2017-07-01', tz=buoy_tz) & datetime < as.POSIXct('2017-08-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme
# 
# ggplot(subset(buoy2017_therm_vert_L1,
#               subset=(datetime >= as.POSIXct('2017-07-28', tz=buoy_tz) & datetime < as.POSIXct('2017-07-29', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme
# 
# ggplot(subset(buoy2017_therm_vert_L1,
#               subset=(datetime >= as.POSIXct('2017-08-01', tz=buoy_tz) & datetime < as.POSIXct('2017-09-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme
# 
# #presumed visit aug 16
# ggplot(subset(buoy2017_therm_vert_L1,
#               subset=(datetime >= as.POSIXct('2017-08-16', tz=buoy_tz) & datetime < as.POSIXct('2017-08-17', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme

buoy2017_L1 <- buoy2017_L1 %>% 
  mutate_at(vars(all_of(alltemp2011)),
            ~(case_when(datetime == as.POSIXct('2017-08-16 10:40', tz=buoy_tz) ~ NA_real_,
                           TRUE ~ .)))
buoy2017_therm_vert_L1 <- buoy2017_L1 %>% 
  select(datetime, all_of(alltemp2011)) %>% 
  pivot_longer(names_to = 'variable', values_to = 'value', -datetime) %>% 
  mutate(variable = factor(variable, levels=alltemp))

# ggplot(subset(buoy2017_therm_vert_L1,
#               subset=(datetime >= as.POSIXct('2017-08-01', tz=buoy_tz) & datetime < as.POSIXct('2017-09-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme
# 
# ggplot(subset(buoy2017_therm_vert_L1,
#               subset=(datetime >= as.POSIXct('2017-09-01', tz=buoy_tz) & datetime < as.POSIXct('2017-10-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme
# 
# ggplot(subset(buoy2017_therm_vert_L1,
#               subset=(datetime >= as.POSIXct('2017-10-01', tz=buoy_tz) & datetime < as.POSIXct('2017-11-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme
# 
# #buoy moved for winter
# ggplot(subset(buoy2017_therm_vert_L1,
#               subset=(datetime >= as.POSIXct('2017-10-19', tz=buoy_tz) & datetime < as.POSIXct('2017-10-20', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme

buoy2017_L1 <- buoy2017_L1 %>% 
  mutate_at(vars(all_of(alltemp2011)),
            ~(case_when(datetime >= as.POSIXct('2017-10-19 9:00', tz=buoy_tz) ~ NA_real_,
                           TRUE ~ .)))
buoy2017_therm_vert_L1 <- buoy2017_L1 %>% 
  select(datetime, all_of(alltemp2011)) %>% 
  pivot_longer(names_to = 'variable', values_to = 'value', -datetime) %>% 
  mutate(variable = factor(variable, levels=alltemp))


ggplot(buoy2017_therm_vert_L1,
       aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  scale_x_datetime(date_minor_breaks = '1 month') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  final_theme

#correct column names for sensor offset from surface
buoy2017_L1 <- buoy2017_L1 %>% 
  rename(waterTemperature_degC_9p85m = TempC_9m,
         waterTemperature_degC_8p85m = TempC_8m,
         waterTemperature_degC_7p85m = TempC_7m,
         waterTemperature_degC_6p85m = TempC_6m,
         waterTemperature_degC_5p85m = TempC_5m,
         waterTemperature_degC_4p85m = TempC_4m,
         waterTemperature_degC_3p85m = TempC_3m,
         waterTemperature_degC_2p85m = TempC_2m,
         waterTemperature_degC_1p85m = TempC_1m,
         waterTemperature_degC_0p85m = TempC_0m)

rm(buoy2017_therm_vert, buoy2017_therm_vert_L1)


#### DO ####
buoy2017_do_vert <- buoy2017_L1 %>% 
  select(datetime, all_of(upDO), all_of(lowDO)) %>%
  pivot_longer(names_to = 'variable', values_to = 'value', -datetime)

# ggplot(buoy2017_do_vert, aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 month') +
#   final_theme

buoy2017_L1 <- buoy2017_L1 %>% 
  mutate_at(vars(upDO, lowDO),
            ~(case_when(. == -6999 ~ NA_real_,
                           . == 0 ~ NA_real_,
                           TRUE ~ .)))

buoy2017_do_vert <- buoy2017_L1 %>% 
  select(datetime, all_of(upDO), all_of(lowDO)) %>%
  pivot_longer(names_to = 'variable', values_to = 'value', -datetime)

ggplot(buoy2017_do_vert, aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  scale_x_datetime(date_minor_breaks = '1 month') +
  final_theme

# ggplot(subset(buoy2017_do_vert,
#               subset=(datetime >= as.POSIXct('2017-01-01', tz=buoy_tz) & datetime < as.POSIXct('2017-02-01', tz=buoy_tz))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme
# 
# ggplot(subset(buoy2017_do_vert,
#               subset=(datetime >= as.POSIXct('2017-02-01', tz=buoy_tz) & datetime < as.POSIXct('2017-03-01', tz=buoy_tz))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme
# 
# ggplot(subset(buoy2017_do_vert,
#               subset=(datetime >= as.POSIXct('2017-03-01', tz=buoy_tz) & datetime < as.POSIXct('2017-04-01', tz=buoy_tz))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme
# 
# ggplot(subset(buoy2017_do_vert,
#               subset=(datetime >= as.POSIXct('2017-03-06', tz=buoy_tz) & datetime < as.POSIXct('2017-03-07', tz=buoy_tz))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme
# 
# ggplot(subset(buoy2017_L1,
#               subset=(datetime >= as.POSIXct('2017-04-11', tz=buoy_tz) & datetime < as.POSIXct('2017-04-12', tz=buoy_tz))),
#        aes(x=datetime, y=PAR)) +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme

#data gap - buoy out of water
buoy2017_L1 <- buoy2017_L1 %>% 
  mutate(location = case_when(datetime < as.POSIXct('2017-03-06 10:10') ~ 'harbor',
                              is.na(rowid) ~ 'offline',
                              TRUE ~ 'harbor'))
  

# ggplot(subset(buoy2017_do_vert,
#               subset=(datetime >= as.POSIXct('2017-04-01', tz=buoy_tz) & datetime < as.POSIXct('2017-05-01', tz=buoy_tz))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme
# 
# ggplot(subset(buoy2017_do_vert,
#               subset=(datetime >= as.POSIXct('2017-04-28', tz=buoy_tz) & datetime < as.POSIXct('2017-04-29', tz=buoy_tz))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme

#remove errant data - will deal with low do when buoy moves
buoy2017_L1 <- buoy2017_L1 %>% 
  mutate_at(vars(all_of(upDO)),
            ~(case_when(datetime >= as.POSIXct('2017-04-20', tz=buoy_tz) & datetime < as.POSIXct('2017-04-28 15:00', tz=buoy_tz) ~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate(flag_do1p5m = case_when(datetime == as.POSIXct('2017-04-28 15:00', tz=buoy_tz) ~ 'wp',
                                   TRUE ~ ''))

buoy2017_do_vert_L1 <- buoy2017_L1 %>% 
  select(datetime, all_of(upDO), all_of(lowDO)) %>%
  pivot_longer(names_to = 'variable', values_to = 'value', -datetime)

# ggplot(subset(buoy2017_do_vert_L1,
#               subset=(datetime >= as.POSIXct('2017-04-01', tz=buoy_tz) & datetime < as.POSIXct('2017-05-01', tz=buoy_tz))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme
# 
# ggplot(subset(buoy2017_do_vert_L1,
#               subset=(datetime >= as.POSIXct('2017-05-01', tz=buoy_tz) & datetime < as.POSIXct('2017-06-01', tz=buoy_tz))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme
# 
# ggplot(subset(buoy2017_do_vert_L1,
#               subset=(datetime >= as.POSIXct('2017-05-17', tz=buoy_tz) & datetime < as.POSIXct('2017-05-18', tz=buoy_tz))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme

#buoy move 5-17
buoy2017_L1 <- buoy2017_L1 %>% 
  mutate_at(vars(all_of(upDO)),
            ~(case_when(datetime >= as.POSIXct('2017-05-17 8:20', tz=buoy_tz) & datetime < as.POSIXct('2017-05-17 9:50', tz=buoy_tz) ~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate(location = case_when(datetime >= as.POSIXct('2017-05-17 8:20', tz=buoy_tz) & datetime < as.POSIXct('2017-05-17 9:50', tz=buoy_tz) ~ 'in transit',
                              datetime >= as.POSIXct('2017-05-17 9:50', tz=buoy_tz) ~ 'loon',
                              TRUE ~ location))

buoy2017_do_vert_L1 <- buoy2017_L1 %>% 
  select(datetime, all_of(upDO), all_of(lowDO)) %>%
  pivot_longer(names_to = 'variable', values_to = 'value', -datetime)

# ggplot(subset(buoy2017_do_vert_L1,
#               subset=(datetime >= as.POSIXct('2017-05-18', tz=buoy_tz) & datetime < as.POSIXct('2017-05-19', tz=buoy_tz))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme
# 
# #buoy move 5-19
# ggplot(subset(buoy2017_do_vert_L1,
#               subset=(datetime >= as.POSIXct('2017-05-19', tz=buoy_tz) & datetime < as.POSIXct('2017-05-20', tz=buoy_tz))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme
# 
# ggplot(subset(buoy2017_do_vert_L1,
#               subset=(datetime >= as.POSIXct('2017-05-23', tz=buoy_tz) & datetime < as.POSIXct('2017-05-24', tz=buoy_tz))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme
# 
# ggplot(subset(buoy2017_do_vert_L1,
#               subset=(datetime >= as.POSIXct('2017-06-01', tz=buoy_tz) & datetime < as.POSIXct('2017-07-01', tz=buoy_tz))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme
# 
# ggplot(subset(buoy2017_do_vert_L1,
#               subset=(datetime >= as.POSIXct('2017-06-14', tz=buoy_tz) & datetime < as.POSIXct('2017-06-15', tz=buoy_tz))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme

buoy2017_L1 <- buoy2017_L1 %>% 
  mutate_at(vars(all_of(upDO)),
            ~(case_when(datetime >= as.POSIXct('2017-05-19 8:00', tz=buoy_tz) & datetime < as.POSIXct('2017-05-23 12:50', tz=buoy_tz) ~ NA_real_,
                           datetime >= as.POSIXct('2017-06-14 11:00', tz=buoy_tz) & datetime < as.POSIXct('2017-06-14 13:40', tz=buoy_tz) ~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate_at(vars(all_of(lowDO)),
            ~(case_when(datetime < as.POSIXct('2017-06-14 14:30', tz=buoy_tz)~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate(location = case_when(datetime >= as.POSIXct('2017-05-19 8:00', tz=buoy_tz) & datetime < as.POSIXct('2017-05-19 8:50', tz=buoy_tz) ~ 'in transit',
                              datetime >= as.POSIXct('2017-05-19 8:50', tz=buoy_tz) & datetime < as.POSIXct('2017-05-23 12:50', tz=buoy_tz) ~ 'harbor, water sensors offline',
                              datetime >= as.POSIXct('2017-05-23 12:50', tz=buoy_tz) & datetime < as.POSIXct('2017-06-14 11:00', tz=buoy_tz)~ 'harbor', 
                              datetime >= as.POSIXct('2017-06-14 11:00', tz=buoy_tz) & datetime < as.POSIXct('2017-06-14 12:10', tz=buoy_tz) ~ 'in transit',
                              datetime >= as.POSIXct('2017-06-14 12:10', tz=buoy_tz) ~ 'loon',
                              TRUE ~ location)) %>% 
  mutate(flag_do1p5m = case_when(datetime == as.POSIXct('2017-05-23 12:50', tz=buoy_tz) ~ 'wp',
                                   datetime == as.POSIXct('2017-06-14 12:10', tz=buoy_tz) ~ 'wp',
                                   TRUE ~ flag_do1p5m)) %>% 
  mutate(flag_do10p5m = case_when(datetime == as.POSIXct('2017-06-14 12:10', tz=buoy_tz) ~ 'wp',
                                   TRUE ~ ''))
buoy2017_do_vert_L1 <- buoy2017_L1 %>% 
  select(datetime, all_of(upDO), all_of(lowDO), location) %>%
  pivot_longer(names_to = 'variable', values_to = 'value', -c(datetime, location))

# ggplot(subset(buoy2017_do_vert_L1,
#               subset=(datetime >= as.POSIXct('2017-05-01', tz=buoy_tz) & datetime < as.POSIXct('2017-07-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color = location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme
# 
# ggplot(subset(buoy2017_do_vert_L1,
#               subset=(datetime >= as.POSIXct('2017-06-01', tz=buoy_tz) & datetime < as.POSIXct('2017-07-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color = location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme
# 
# ggplot(subset(buoy2017_do_vert_L1,
#               subset=(datetime >= as.POSIXct('2017-07-01', tz=buoy_tz) & datetime < as.POSIXct('2017-08-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color = location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme
# 
# ggplot(subset(buoy2017_do_vert_L1,
#               subset=(datetime >= as.POSIXct('2017-06-01', tz=buoy_tz) & datetime < as.POSIXct('2017-08-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color = location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme


#flag do data as intermittent beginning jun 26 through jul 2
buoy2017_L1 <- buoy2017_L1 %>% 
  mutate_at(vars(flag_do1p5m, flag_do10p5m),
            ~ case_when(datetime >= as.POSIXct('2017-06-26', tz=buoy_tz) & 
                                   datetime < as.POSIXct('2017-07-03', tz=buoy_tz) &
                          . == '' ~ 'i',
                        datetime >= as.POSIXct('2017-06-26', tz=buoy_tz) & 
                          datetime < as.POSIXct('2017-07-03', tz=buoy_tz) &
                          . != '' ~ paste('i', ., sep = '; '),
                           TRUE ~ .))
buoy2017_do_vert_L1 <- buoy2017_L1 %>% 
  select(datetime, all_of(upDO), all_of(lowDO), location) %>%
  pivot_longer(names_to = 'variable', values_to = 'value', -c(datetime, location))

# ggplot(subset(buoy2017_do_vert_L1,
#               subset=(datetime >= as.POSIXct('2017-08-01', tz=buoy_tz) & datetime < as.POSIXct('2017-09-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color = location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme
# 
# ggplot(subset(buoy2017_do_vert_L1,
#               subset=(datetime >= as.POSIXct('2017-09-01', tz=buoy_tz) & datetime < as.POSIXct('2017-10-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color = location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme
# 
# ggplot(subset(buoy2017_do_vert_L1,
#               subset=(datetime >= as.POSIXct('2017-10-01', tz=buoy_tz) & datetime < as.POSIXct('2017-11-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color = location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme
# 
# ggplot(subset(buoy2017_do_vert_L1,
#               subset=(datetime >= as.POSIXct('2017-10-19', tz=buoy_tz) & datetime < as.POSIXct('2017-10-20', tz=buoy_tz))),
#        aes(x=datetime, y=value, color = location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme

#upper do starts to fail oct 9
buoy2017_L1 <- buoy2017_L1 %>% 
  mutate(flag_do1p5m = case_when(datetime >= as.Date('2017-10-09') ~ 'sf',
                                 TRUE ~ flag_do1p5m))
buoy2017_L1 <- buoy2017_L1 %>% 
  mutate_at(vars(all_of(lowDO), all_of(upDO)),
            ~(case_when(datetime >= as.POSIXct('2017-10-19 8:20', tz=buoy_tz) ~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate(location = case_when(datetime >= as.POSIXct('2017-10-19 8:20', tz=buoy_tz) & 
                                datetime < as.POSIXct('2017-10-19 9:30', tz=buoy_tz) ~ 'in transit',
                              datetime >= as.POSIXct('2017-10-19 9:30', tz=buoy_tz) ~ 'harbor',
                              TRUE ~ location))
buoy2017_do_vert_L1 <- buoy2017_L1 %>%
  select(datetime, location, all_of(lowDO), all_of(upDO)) %>%
  pivot_longer(names_to = 'variable', values_to = 'value', -c(datetime, location))

ggplot(buoy2017_do_vert_L1,
       aes(x=datetime, y=value, color=location)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  scale_x_datetime(date_minor_breaks = '1 month') +
  labs(title = 'do 2017, clean') +
  final_theme +
  scale_color_colorblind()

#add flag for no calibration on record
buoy2017_L1 <- buoy2017_L1 %>% 
  mutate_at(vars(flag_do1p5m, flag_do10p5m),
            ~ case_when(. == '' ~ 'x',
                        . != '' ~ paste('x', ., sep = '; ')))

buoy2017_updo_vert_L1 <- buoy2017_L1 %>%
  select(datetime, location, all_of(upDO), flag_do1p5m) %>%
  pivot_longer(names_to = 'variable', values_to = 'value', -c(datetime, location, flag_do1p5m))

ggplot(buoy2017_updo_vert_L1,
       aes(x=datetime, y=value, color=flag_do1p5m, shape = location)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  scale_x_datetime(date_minor_breaks = '1 month') +
  labs(title = 'do 2017, clean') +
  final_theme +
  scale_color_colorblind()

buoy2017_lowdo_vert_L1 <- buoy2017_L1 %>%
  select(datetime, location, all_of(lowDO), flag_do10p5m) %>%
  pivot_longer(names_to = 'variable', values_to = 'value', -c(datetime, location, flag_do10p5m))

ggplot(buoy2017_lowdo_vert_L1,
       aes(x=datetime, y=value, color=location, shape = flag_do10p5m)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  scale_x_datetime(date_minor_breaks = '1 month') +
  labs(title = 'do 2017, clean') +
  final_theme +
  scale_color_colorblind()

#rename with CV
buoy2017_L1 <- buoy2017_L1 %>% 
  rename(oxygenDissolved_mgl_1p5m = DOppm,
         oxygenDissolvedPercentOfSaturation_pct_1p5m = DOSat,
         waterTemperature_DO_degC_1p5m = DOTempC,
         oxygenDissolved_mgl_10p5m = DOLowPPM,
         oxygenDissolvedPercentOfSaturation_pct_10p5m = DOLowSat,
         waterTemperature_DO_degC_10p5m = DOLowTempC)

rm(buoy2017_do_vert, buoy2017_do_vert_L1, buoy2017_updo_vert_L1, buoy2017_lowdo_vert_L1)


# ####CHLA####
## SENSOR NEVER FUNCTIONS PROPERLY ####
# # buoy2017_chla_vert <- buoy2017_L1 %>% 
# #   select(datetime, location, chla) %>%
# #   gather(variable, value, -datetime, -location)
# # 
# # ggplot(buoy2017_chla_vert, aes(x=datetime, y=value)) +
# #   geom_point() +
# #   facet_grid(variable ~ ., scales = 'free_y') +
# #   scale_x_datetime(date_minor_breaks = '1 month') +
# #   final_theme
# 
# #recode NA
# buoy2017_L1 <- buoy2017_L1 %>% 
#   mutate_at(vars(chla),
#             ~(case_when(. == -6999 ~ NA_real_,
#                            TRUE ~ .))) %>% 
#   mutate(Chlor_UGL = case_when(Chlor_UGL < 0 ~ 0,
#                                Chlor_UGL == 587 ~ NA_real_,
#                                TRUE ~ Chlor_UGL)) %>% 
#   mutate(SpecCond = case_when(SpecCond == 6999 ~ NA_real_,
#                               TRUE ~ SpecCond)) 
# buoy2017_chla_vert <- buoy2017_L1 %>% 
#   select(datetime, location, chla) %>%
#   gather(variable, value, -datetime, -location)
# 
# ggplot(buoy2017_chla_vert, aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 month') +
#   final_theme
# 
# # ggplot(subset(buoy2017_chla_vert, subset=(datetime>=as.POSIXct('2017-04-01', tz=buoy_tz) & datetime < as.POSIXct('2017-05-01', tz=buoy_tz))),
# #        aes(x=datetime, y=value)) +
# #   geom_point() +
# #   facet_grid(variable ~ ., scales = 'free_y') +
# #   labs(title = 'chla apr 2017, NAs recoded') +
# #   scale_x_datetime(date_minor_breaks = '1 day') +
# #   final_theme
# # 
# # #sensor online and in water apr 28
# # ggplot(subset(buoy2017_chla_vert, subset=(datetime>=as.POSIXct('2017-04-28', tz=buoy_tz) & datetime < as.POSIXct('2017-04-29', tz=buoy_tz))),
# #        aes(x=datetime, y=value)) +
# #   geom_point() +
# #   facet_grid(variable ~ ., scales = 'free_y') +
# #   labs(title = 'chla apr 2017, NAs recoded') +
# #   scale_x_datetime(date_minor_breaks = '1 hour') +
# #   final_theme
# 
# buoy2017_L1 <- buoy2017_L1 %>% 
#   mutate_at(vars(chla),
#             ~(case_when(datetime < as.POSIXct('2017-04-28 15:00', tz=buoy_tz)~ NA_real_,
#                            TRUE ~ .)))  %>% 
#   mutate(chla_flag = case_when(datetime == as.POSIXct('2017-04-28 15:00', tz=buoy_tz)~ 'wp',
#                                TRUE ~ '')) %>% 
#   mutate(cond_flag = case_when(datetime == as.POSIXct('2017-04-28 15:00', tz=buoy_tz)~ 'wp',
#                                TRUE ~ ''))
# 
# buoy2017_chla_vert_b <- buoy2017_L1 %>% 
#   select(datetime, location, chla) %>%
#   gather(variable, value, -datetime, -location)
# 
# # ggplot(subset(buoy2017_chla_vert_b, subset=(datetime>=as.POSIXct('2017-04-01', tz=buoy_tz) & datetime < as.POSIXct('2017-05-01', tz=buoy_tz))),
# #        aes(x=datetime, y=value, color=location)) +
# #   geom_point() +
# #   facet_grid(variable ~ ., scales = 'free_y') +
# #   labs(title = 'chla apr 2017, clean') +
# #   scale_x_datetime(date_minor_breaks = '1 day') +
# #   final_theme +
# #   scale_color_colorblind()
# # 
# # ggplot(subset(buoy2017_chla_vert, subset=(datetime>=as.POSIXct('2017-05-01', tz=buoy_tz) & datetime < as.POSIXct('2017-06-01', tz=buoy_tz))),
# #        aes(x=datetime, y=value, color = location)) +
# #   geom_point() +
# #   facet_grid(variable ~ ., scales = 'free_y') +
# #   labs(title = 'chla apr 2017, NAs recoded') +
# #   scale_x_datetime(date_minor_breaks = '1 day') +
# #   final_theme
# # 
# # #may 16 chla and cond probe removed for calibration
# # ggplot(subset(buoy2017_chla_vert, subset=(datetime>=as.POSIXct('2017-05-16', tz=buoy_tz) & datetime < as.POSIXct('2017-05-17', tz=buoy_tz))),
# #        aes(x=datetime, y=value, color = location)) +
# #   geom_point() +
# #   facet_grid(variable ~ ., scales = 'free_y') +
# #   labs(title = 'chla may 2017, NAs recoded') +
# #   scale_x_datetime(date_minor_breaks = '1 hour') +
# #   final_theme
# # 
# # #may 17 sonde reattached, buoy moved to loon
# # ggplot(subset(buoy2017_chla_vert, subset=(datetime>=as.POSIXct('2017-05-17', tz=buoy_tz) & datetime < as.POSIXct('2017-05-18', tz=buoy_tz))),
# #        aes(x=datetime, y=value, color = location)) +
# #   geom_point() +
# #   facet_grid(variable ~ ., scales = 'free_y') +
# #   labs(title = 'chla may 2017, clean') +
# #   scale_x_datetime(date_minor_breaks = '1 hour') +
# #   final_theme
# # 
# # #may 19 buoy back to harbor, sensors offline
# # ggplot(subset(buoy2017_chla_vert, subset=(datetime>=as.POSIXct('2017-05-19', tz=buoy_tz) & datetime < as.POSIXct('2017-05-20', tz=buoy_tz))),
# #        aes(x=datetime, y=value, color = location)) +
# #   geom_point() +
# #   facet_grid(variable ~ ., scales = 'free_y') +
# #   labs(title = 'chla may 2017, NAs recoded') +
# #   scale_x_datetime(date_minor_breaks = '1 hour') +
# #   final_theme
# # 
# # #may 23 sensors back online
# # ggplot(subset(buoy2017_chla_vert, subset=(datetime>=as.POSIXct('2017-05-23', tz=buoy_tz) & datetime < as.POSIXct('2017-05-24', tz=buoy_tz))),
# #        aes(x=datetime, y=value, color = location)) +
# #   geom_point() +
# #   facet_grid(variable ~ ., scales = 'free_y') +
# #   labs(title = 'chla may 2017, NAs recoded') +
# #   scale_x_datetime(date_minor_breaks = '1 hour') +
# #   final_theme
# # 
# # #starting may 31, all sensors measuring only integers
# # ggplot(subset(buoy2017_chla_vert, subset=(datetime>=as.POSIXct('2017-05-31', tz=buoy_tz) & datetime < as.POSIXct('2017-06-01', tz=buoy_tz))),
# #        aes(x=datetime, y=value, color = location)) +
# #   geom_point() +
# #   facet_grid(variable ~ ., scales = 'free_y') +
# #   labs(title = 'chla may 2017, NAs recoded') +
# #   scale_x_datetime(date_minor_breaks = '1 hour') +
# #   final_theme
# 
# buoy2017_L1 <- buoy2017_L1 %>% 
#   mutate_at(vars(chla),
#             ~(case_when(datetime >= as.POSIXct('2017-05-16 12:50', tz=buoy_tz) & datetime < as.POSIXct('2017-05-16 13:10', tz=buoy_tz)~ NA_real_,
#                            location == 'in transit' ~ NA_real_,
#                           location == 'harbor, water sensors offline' ~NA_real_,
#                            TRUE ~ .))) %>% 
#   mutate(cond_flag = case_when(datetime == as.POSIXct('2017-05-17 8:20') ~ 'ct',
#                                cond_flag == '' & !is.na(SpecCond) & datetime>as.POSIXct('2017-05-17', tz=buoy_tz) ~ 't',
#                                TRUE ~ cond_flag)) %>% 
#   mutate(chla_flag = case_when(!is.na(Chlor_UGL) & datetime>as.POSIXct('2017-05-31 11:00', tz=buoy_tz) ~ 't',
#                                         TRUE ~ chla_flag))
# 
# buoy2017_chla_vert_b <- buoy2017_L1 %>% 
#   select(datetime, location, chla) %>%
#   gather(variable, value, -datetime, -location)
# 
# # ggplot(subset(buoy2017_chla_vert_b, subset=(datetime>=as.POSIXct('2017-05-01', tz=buoy_tz) & datetime < as.POSIXct('2017-06-01', tz=buoy_tz))),
# #        aes(x=datetime, y=value, color=location)) +
# #   geom_point() +
# #   facet_grid(variable ~ ., scales = 'free_y') +
# #   labs(title = 'chla may 2017, clean') +
# #   scale_x_datetime(date_minor_breaks = '1 day') +
# #   final_theme +
# #   scale_color_colorblind()
# # 
# # ggplot(subset(buoy2017_chla_vert_b, subset=(datetime>=as.POSIXct('2017-06-01', tz=buoy_tz) & datetime < as.POSIXct('2017-07-01', tz=buoy_tz))),
# #        aes(x=datetime, y=value, color = location)) +
# #   geom_point() +
# #   facet_grid(variable ~ ., scales = 'free_y') +
# #   labs(title = 'chla jun 2017, NAs recoded') +
# #   scale_x_datetime(date_minor_breaks = '1 day') +
# #   final_theme
# # 
# # #jun 14 buoy back to loon
# # ggplot(subset(buoy2017_chla_vert_b, subset=(datetime>=as.POSIXct('2017-06-14', tz=buoy_tz) & datetime < as.POSIXct('2017-06-15', tz=buoy_tz))),
# #        aes(x=datetime, y=value, color = location)) +
# #   geom_point() +
# #   facet_grid(variable ~ ., scales = 'free_y') +
# #   labs(title = 'chla jun 2017, NAs recoded') +
# #   scale_x_datetime(date_minor_breaks = '1 hour') +
# #   final_theme
# 
# buoy2017_L1 <- buoy2017_L1 %>% 
#   mutate_at(vars(chla),
#             ~(case_when(datetime >= as.POSIXct('2017-06-14 13:00', tz=buoy_tz) & datetime < as.POSIXct('2017-06-14 14:10', tz=buoy_tz)~ NA_real_,
#                            TRUE ~ .))) %>% 
#   mutate_at(vars(chla_flag, cond_flag),
#             ~(case_when(datetime==as.POSIXct('2017-06-14 14:20', tz=buoy_tz) ~ 'twp',
#                            TRUE ~ .)))
# buoy2017_chla_vert_b <- buoy2017_L1 %>% 
#   select(datetime, location, chla) %>%
#   gather(variable, value, -datetime, -location)
# 
# # ggplot(subset(buoy2017_chla_vert_b, subset=(datetime>=as.POSIXct('2017-06-01', tz=buoy_tz) & datetime < as.POSIXct('2017-07-01', tz=buoy_tz))),
# #        aes(x=datetime, y=value, color=location)) +
# #   geom_point() +
# #   facet_grid(variable ~ ., scales = 'free_y') +
# #   labs(title = 'chla jun 2017, clean') +
# #   scale_x_datetime(date_minor_breaks = '1 day') +
# #   final_theme +
# #   scale_color_colorblind()
# # 
# # ggplot(subset(buoy2017_chla_vert, subset=(datetime>=as.POSIXct('2017-07-01', tz=buoy_tz) & datetime < as.POSIXct('2017-08-01', tz=buoy_tz))),
# #        aes(x=datetime, y=value, color = location)) +
# #   geom_point() +
# #   facet_grid(variable ~ ., scales = 'free_y') +
# #   labs(title = 'chla jul 2017, NAs recoded') +
# #   scale_x_datetime(date_minor_breaks = '1 day') +
# #   final_theme
# # 
# # ggplot(subset(buoy2017_chla_vert, subset=(datetime>=as.POSIXct('2017-08-01', tz=buoy_tz) & datetime < as.POSIXct('2017-09-01', tz=buoy_tz))),
# #        aes(x=datetime, y=value, color = location)) +
# #   geom_point() +
# #   facet_grid(variable ~ ., scales = 'free_y') +
# #   labs(title = 'chla aug 2017, NAs recoded') +
# #   scale_x_datetime(date_minor_breaks = '1 day') +
# #   final_theme
# # 
# # ggplot(subset(buoy2017_chla_vert, subset=(datetime>=as.POSIXct('2017-09-01', tz=buoy_tz) & datetime < as.POSIXct('2017-10-01', tz=buoy_tz))),
# #        aes(x=datetime, y=value, color =location)) +
# #   geom_point() +
# #   facet_grid(variable ~ ., scales = 'free_y') +
# #   labs(title = 'chla sept 2017, NAs recoded') +
# #   scale_x_datetime(date_minor_breaks = '1 day') +
# #   final_theme
# # 
# # ggplot(subset(buoy2017_chla_vert, subset=(datetime>=as.POSIXct('2017-10-01', tz=buoy_tz) & datetime < as.POSIXct('2017-11-01', tz=buoy_tz))),
# #        aes(x=datetime, y=value, color = location)) +
# #   geom_point() +
# #   facet_grid(variable ~ ., scales = 'free_y') +
# #   labs(title = 'chla oct 2017, NAs recoded') +
# #   scale_x_datetime(date_minor_breaks = '1 day') +
# #   final_theme
# # 
# # #oct 19 buoy moved to harbor
# #   ggplot(subset(buoy2017_chla_vert_b, subset=(datetime>=as.POSIXct('2017-10-19', tz=buoy_tz) & datetime < as.POSIXct('2017-10-20', tz=buoy_tz))),
# #        aes(x=datetime, y=value, color=location)) +
# #   geom_point() +
# #   facet_grid(variable ~ ., scales = 'free_y') +
# #   labs(title = 'chla oct 2017, NAs recoded') +
# #   scale_x_datetime(date_minor_breaks = '1 hour') +
# #   final_theme +
# #   scale_color_colorblind()
# # 
# # #oct 23 sonde back online
# # ggplot(subset(buoy2017_chla_vert, subset=(datetime>=as.POSIXct('2017-10-23', tz=buoy_tz) & datetime < as.POSIXct('2017-10-24', tz=buoy_tz))),
# #        aes(x=datetime, y=value)) +
# #   geom_point() +
# #   facet_grid(variable ~ ., scales = 'free_y') +
# #   labs(title = 'chla oct 2017, NAs recoded') +
# #   scale_x_datetime(date_minor_breaks = '1 hour') +
# #   final_theme
# 
# buoy2017_L1 <- buoy2017_L1 %>% 
#   mutate_at(vars(chla),
#             ~(case_when(datetime >= as.POSIXct('2017-10-19 9:30', tz=buoy_tz) & datetime < as.POSIXct('2017-10-23 14:00', tz=buoy_tz)~ NA_real_,
#                            TRUE ~ .))) 
# buoy2017_chla_vert_b <- buoy2017_L1 %>% 
#   select(datetime, location, chla) %>%
#   gather(variable, value, -datetime, -location)
# 
# # ggplot(subset(buoy2017_chla_vert_b, subset=(datetime>=as.POSIXct('2017-10-01', tz=buoy_tz) & datetime < as.POSIXct('2017-11-01', tz=buoy_tz))),
# #        aes(x=datetime, y=value, color=location)) +
# #   geom_point() +
# #   facet_grid(variable ~ ., scales = 'free_y') +
# #   labs(title = 'chla oct 2017, clean') +
# #   scale_x_datetime(date_minor_breaks = '1 day') +
# #   final_theme +
# #   scale_color_colorblind()
# # 
# # ggplot(subset(buoy2017_chla_vert, subset=(datetime>=as.POSIXct('2017-11-01', tz=buoy_tz) & datetime < as.POSIXct('2017-12-01', tz=buoy_tz))),
# #        aes(x=datetime, y=value, color=location)) +
# #   geom_point() +
# #   facet_grid(variable ~ ., scales = 'free_y') +
# #   labs(title = 'chla nov 2017, NAs recoded') +
# #   scale_x_datetime(date_minor_breaks = '1 day') +
# #   final_theme +
# #   scale_color_colorblind()
# # 
# # ggplot(subset(buoy2017_chla_vert, subset=(datetime>=as.POSIXct('2017-11-13', tz=buoy_tz) & datetime < as.POSIXct('2017-11-14', tz=buoy_tz))),
# #        aes(x=datetime, y=value, color=location)) +
# #   geom_point() +
# #   facet_grid(variable ~ ., scales = 'free_y') +
# #   labs(title = 'chla nov 2017, NAs recoded') +
# #   scale_x_datetime(date_minor_breaks = '1 hour') +
# #   final_theme +
# #   scale_color_colorblind()
# 
# #sonde removed nov20 - but going to end spec cond at same time as other data streams here
# buoy2017_L1 <- buoy2017_L1 %>% 
#   mutate_at(vars(chla),
#             ~(case_when(datetime >= as.POSIXct('2017-11-13 12:00', tz=buoy_tz) ~ NA_real_,
#                            TRUE ~ .))) 
# buoy2017_chla_vert_b <- buoy2017_L1 %>% 
#   select(datetime, location, chla) %>%
#   gather(variable, value, -datetime, -location)
# 
# # ggplot(subset(buoy2017_chla_vert_b, subset=(datetime>=as.POSIXct('2017-11-01', tz=buoy_tz) & datetime < as.POSIXct('2017-12-01', tz=buoy_tz))), 
# #        aes(x=datetime, y=value, color=location)) +
# #   geom_point() +
# #   facet_grid(variable ~ ., scales = 'free_y') +
# #   labs(title = 'chla nov 2017, clean') +
# #   scale_x_datetime(date_minor_breaks = '1 day') +
# #   final_theme +
# #   scale_color_colorblind()
# 
# #add flag as suspect above 10 ugl
# buoy2017_L1 <- buoy2017_L1 %>% 
#   mutate(chla_flag = case_when(Chlor_UGL>10 ~ paste(chla_flag, 's', sep = ''),
#                                TRUE ~ chla_flag))
# 
# #plot with flags
# buoy2017_chla_b <- buoy2017_L1 %>% 
#   select(datetime, location, Chlor_UGL, chla_flag) %>%
#   gather(variable, value, -datetime, -location, -chla_flag)
# 
# ggplot(buoy2017_chla_b, 
#        aes(x=datetime, y=value, color=location, shape = chla_flag)) +
#   geom_point() +
#   labs(title = 'chla 2017, clean') +
#   scale_x_datetime(date_minor_breaks = '1 month') +
#   final_theme +
#   scale_color_colorblind()
# 
# buoy2017_cond_b <- buoy2017_L1 %>% 
#   select(datetime, location, SpecCond, cond_flag) %>%
#   gather(variable, value, -datetime, -location, -cond_flag)
# 
# ggplot(buoy2017_cond_b, 
#        aes(x=datetime, y=value, color=location, shape = cond_flag)) +
#   geom_point() +
#   labs(title = 'chla 2017, clean') +
#   scale_x_datetime(date_minor_breaks = '1 month') +
#   final_theme +
#   scale_color_colorblind()
# 
# rm(buoy2017_chla_b, buoy2017_chla_vert, buoy2017_chla_vert_b, buoy2017_cond_b)


####wind####
range(buoy2017_L1$AveWindSp, na.rm = T)
range(buoy2017_L1$AveWindDir, na.rm = T)
range(buoy2017_L1$MaxWindSp, na.rm = T)
range(buoy2017_L1$MaxWindDir, na.rm = T)

#recode offline/in transit
buoy2017_L1 <- buoy2017_L1 %>% 
  mutate_at(vars(AveWindDir, AveWindSp, MaxWindDir, MaxWindSp),
            ~(case_when(location == 'in transit' ~ NA_real_,
                        TRUE ~ .)))

buoy_wind_vert <- buoy2017_L1 %>% 
  select(datetime, AveWindDir, AveWindSp, MaxWindDir, MaxWindSp, location) %>% 
  pivot_longer(names_to = 'variable', values_to = 'value', -c(datetime, location))

ggplot(buoy_wind_vert, 
       aes(x=datetime, y=value, color=location)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title = 'wind 2017, NAs recoded') +
  scale_x_datetime(date_minor_breaks = '1 month') +
  final_theme +
  scale_color_colorblind()

# ggplot(subset(buoy_wind_vert,
#               subset=(datetime >= as.POSIXct('2017-01-01', tz=buoy_tz) & datetime < as.POSIXct('2017-02-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'jan wind 2017, NAs recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# #sensor frozen jan 4
# ggplot(subset(buoy_wind_vert, 
#               subset=(datetime >= as.POSIXct('2017-01-04', tz=buoy_tz) & datetime < as.POSIXct('2017-01-05', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'jan wind 2017, NAs recoded') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme +
#   scale_color_colorblind()
# 
# #sensor frozen jan 17-19
# ggplot(subset(buoy_wind_vert, 
#               subset=(datetime >= as.POSIXct('2017-01-17', tz=buoy_tz) & datetime < as.POSIXct('2017-01-18 3:00', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'jan wind 2017, NAs recoded') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy_wind_vert, 
#               subset=(datetime >= as.POSIXct('2017-01-19', tz=buoy_tz) & datetime < as.POSIXct('2017-01-20', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'jan wind 2017, NAs recoded') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme +
#   scale_color_colorblind()

buoy2017_L1 <- buoy2017_L1 %>% 
  mutate_at(vars(AveWindDir, AveWindSp, MaxWindDir, MaxWindSp),
            ~(case_when(datetime >= as.POSIXct('2017-01-04 3:50', tz=buoy_tz) & 
                          datetime < as.POSIXct('2017-01-04 8:20', tz=buoy_tz) ~ NA_real_,
                           datetime >= as.POSIXct('2017-01-17 23:50', tz=buoy_tz) & 
                          datetime < as.POSIXct('2017-01-19 10:10', tz=buoy_tz) ~ NA_real_,
                           TRUE ~ .)))

buoy_wind_vert_L1 <- buoy2017_L1 %>% 
  select(datetime, AveWindDir, AveWindSp, MaxWindDir, MaxWindSp, location) %>% 
  pivot_longer(names_to = 'variable', values_to = 'value', -c(datetime, location))

# ggplot(subset(buoy_wind_vert_L1,
#               subset=(datetime >= as.POSIXct('2017-01-01', tz=buoy_tz) & datetime < as.POSIXct('2017-02-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'jan wind 2017, clean') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy_wind_vert,
#               subset=(datetime >= as.POSIXct('2017-02-01', tz=buoy_tz) & datetime < as.POSIXct('2017-03-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'feb wind 2017, NAs recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# #sensor frozen feb 9
# ggplot(subset(buoy_wind_vert, 
#               subset=(datetime >= as.POSIXct('2017-02-09', tz=buoy_tz) & datetime < as.POSIXct('2017-02-10', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'feb wind 2017, NAs recoded') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme +
#   scale_color_colorblind()
# 
# #sensor frozen feb 13
# ggplot(subset(buoy_wind_vert, 
#               subset=(datetime >= as.POSIXct('2017-02-13', tz=buoy_tz) & datetime < as.POSIXct('2017-02-14', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'feb wind 2017, NAs recoded') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme +
#   scale_color_colorblind()
# 
# #sensor frozen feb 15-17
# ggplot(subset(buoy_wind_vert, 
#               subset=(datetime >= as.POSIXct('2017-02-15', tz=buoy_tz) & datetime < as.POSIXct('2017-02-16', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'feb wind 2017, NAs recoded') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme +
#   scale_color_colorblind()
# ggplot(subset(buoy_wind_vert, 
#               subset=(datetime >= as.POSIXct('2017-02-17', tz=buoy_tz) & datetime < as.POSIXct('2017-02-18', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'feb wind 2017, NAs recoded') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme +
#   scale_color_colorblind()

buoy2017_L1 <- buoy2017_L1 %>% 
  mutate_at(vars(AveWindDir, AveWindSp, MaxWindDir, MaxWindSp),
            ~(case_when(datetime >= as.POSIXct('2017-02-09 8:50', tz=buoy_tz) & 
                          datetime < as.POSIXct('2017-02-09 13:50', tz=buoy_tz) ~ NA_real_,
                           datetime >= as.POSIXct('2017-02-13 4:10', tz=buoy_tz) & 
                          datetime < as.POSIXct('2017-02-13 9:10', tz=buoy_tz) ~ NA_real_,
                           datetime >= as.POSIXct('2017-02-15 19:10', tz=buoy_tz) & 
                          datetime < as.POSIXct('2017-02-17 9:20', tz=buoy_tz) ~ NA_real_,
                           TRUE ~ .)))

buoy_wind_vert_L1 <- buoy2017_L1 %>% 
  select(datetime, AveWindDir, AveWindSp, MaxWindDir, MaxWindSp, location) %>% 
  pivot_longer(names_to = 'variable', values_to = 'value', -c(datetime, location))

# ggplot(subset(buoy_wind_vert_L1,
#               subset=(datetime >= as.POSIXct('2017-02-01', tz=buoy_tz) & datetime < as.POSIXct('2017-03-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'feb wind 2017, clean') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy_wind_vert,
#               subset=(datetime >= as.POSIXct('2017-03-01', tz=buoy_tz) & datetime < as.POSIXct('2017-04-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'mar wind 2017, NAs recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy_wind_vert,
#               subset=(datetime >= as.POSIXct('2017-04-01', tz=buoy_tz) & datetime < as.POSIXct('2017-05-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'apr wind 2017, NAs recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()

#buoy not back on the water in harbor until the 21st - recode data before that (as well as other environmental vars)
buoy2017_L1 <- buoy2017_L1 %>% 
  mutate_at(vars(AveWindDir, AveWindSp, MaxWindDir, MaxWindSp, PAR, RelHum, AirTempC),
            ~case_when(datetime >= as.Date('2017-04-01') &
                                    datetime < as.Date('2017-04-22') ~ NA_real_,
                                  TRUE ~ .))
buoy_wind_vert_L1 <- buoy2017_L1 %>% 
  select(datetime, AveWindDir, AveWindSp, MaxWindDir, MaxWindSp, location) %>% 
  pivot_longer(names_to = 'variable', values_to = 'value', -c(datetime, location))

# ggplot(subset(buoy_wind_vert_L1,
#               subset=(datetime >= as.POSIXct('2017-04-01', tz=buoy_tz) & datetime < as.POSIXct('2017-05-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'apr wind 2017, NAs recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy_wind_vert,
#               subset=(datetime >= as.POSIXct('2017-05-01', tz=buoy_tz) & datetime < as.POSIXct('2017-06-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'may wind 2017, NAs recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# #may 14th - sensor frozen?
# ggplot(subset(buoy_wind_vert,
#               subset=(datetime >= as.POSIXct('2017-05-14', tz=buoy_tz) & datetime < as.POSIXct('2017-05-15', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'may wind 2017, NAs recoded') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme +
#   scale_color_colorblind()

buoy2017_L1 <- buoy2017_L1 %>% 
  mutate_at(vars(AveWindDir, AveWindSp, MaxWindDir, MaxWindSp),
            ~case_when(datetime >= as.POSIXct('2017-05-14 3:30', tz= buoy_tz) &
                         datetime < as.POSIXct('2017-05-14 07:50',tz = buoy_tz) ~ NA_real_,
                       TRUE ~ .))
buoy_wind_vert_L1 <- buoy2017_L1 %>% 
  select(datetime, AveWindDir, AveWindSp, MaxWindDir, MaxWindSp, location) %>% 
  pivot_longer(names_to = 'variable', values_to = 'value', -c(datetime, location))

# ggplot(subset(buoy_wind_vert_L1,
#               subset=(datetime >= as.POSIXct('2017-05-01', tz=buoy_tz) & datetime < as.POSIXct('2017-06-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'may wind 2017, NAs recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy_wind_vert,
#               subset=(datetime >= as.POSIXct('2017-06-01', tz=buoy_tz) & datetime < as.POSIXct('2017-07-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'jun wind 2017, NAs recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy_wind_vert,
#               subset=(datetime >= as.POSIXct('2017-07-01', tz=buoy_tz) & datetime < as.POSIXct('2017-08-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'jul wind 2017, NAs recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy_wind_vert,
#               subset=(datetime >= as.POSIXct('2017-08-01', tz=buoy_tz) & datetime < as.POSIXct('2017-09-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'aug wind 2017, NAs recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy_wind_vert,
#               subset=(datetime >= as.POSIXct('2017-09-01', tz=buoy_tz) & datetime < as.POSIXct('2017-10-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'sept wind 2017, NAs recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy_wind_vert,
#               subset=(datetime >= as.POSIXct('2017-10-01', tz=buoy_tz) & datetime < as.POSIXct('2017-11-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'oct wind 2017, NAs recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy_wind_vert,
#               subset=(datetime >= as.POSIXct('2017-11-01', tz=buoy_tz) & datetime < as.POSIXct('2017-12-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'nov wind 2017, NAs recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy_wind_vert,
#               subset=(datetime >= as.POSIXct('2017-12-01', tz=buoy_tz) & datetime < as.POSIXct('2018-01-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'dec wind 2017, NAs recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# #sensor frozen dec 9-10
# ggplot(subset(buoy_wind_vert, 
#               subset=(datetime >= as.POSIXct('2017-12-09 12:00', tz=buoy_tz) & datetime < as.POSIXct('2017-12-11', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'dec wind 2017, NAs recoded') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme +
#   scale_color_colorblind()
# 
# #sensor frozen dec 23 - 26
# ggplot(subset(buoy_wind_vert, 
#               subset=(datetime >= as.POSIXct('2017-12-23', tz=buoy_tz) & datetime < as.POSIXct('2017-12-24', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'dec wind 2017, NAs recoded') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme +
#   scale_color_colorblind()
# ggplot(subset(buoy_wind_vert, 
#               subset=(datetime >= as.POSIXct('2017-12-26', tz=buoy_tz) & datetime < as.POSIXct('2017-12-27', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'dec wind 2017, NAs recoded') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme +
#   scale_color_colorblind()

buoy2017_L1 <- buoy2017_L1 %>% 
  mutate_at(vars(AveWindDir, AveWindSp, MaxWindDir, MaxWindSp),
            ~(case_when(datetime >= as.POSIXct('2017-12-09 22:40', tz=buoy_tz) & 
                          datetime < as.POSIXct('2017-12-10 15:30', tz=buoy_tz) ~ NA_real_,
                           datetime >= as.POSIXct('2017-12-23 10:50', tz=buoy_tz) & datetime < 
                          as.POSIXct('2017-12-26 14:30', tz=buoy_tz) ~ NA_real_,
                        datetime >= as.POSIXct('2017-12-26 18:00', tz = buoy_tz) &
                          datetime < as.Date('2017-12-28') & MaxWindSp == 0 ~ NA_real_,
                           TRUE ~ .)))

buoy_wind_vert_L1 <- buoy2017_L1 %>% 
  select(datetime, AveWindDir, AveWindSp, MaxWindDir, MaxWindSp, location) %>% 
  pivot_longer(names_to = 'variable', values_to = 'value', -c(datetime, location))

# ggplot(subset(buoy_wind_vert_L1,
#               subset=(datetime >= as.POSIXct('2017-12-01', tz=buoy_tz) & datetime < as.POSIXct('2018-01-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'dec wind 2017, clean') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()

# add a suspect flag to wind data from dec 23 through end of year - battery voltage is low and it looks like it's interfering here
buoy2017_L1 <- buoy2017_L1 %>% 
  mutate(flag_allwind = case_when(datetime >= as.Date('2017-12-23') ~ 'sb',
                                  TRUE ~ ''))
buoy_wind_vert_L1 <- buoy2017_L1 %>% 
  select(datetime, AveWindDir, AveWindSp, MaxWindDir, MaxWindSp, location, flag_allwind) %>% 
  pivot_longer(names_to = 'variable', values_to = 'value', -c(datetime, location, flag_allwind))

ggplot(buoy_wind_vert_L1,
       aes(x=datetime, y=value, color=location, shape = flag_allwind)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title = 'wind 2017, clean') +
  scale_x_datetime(date_minor_breaks = '1 month') +
  final_theme +
  scale_color_colorblind()

#rename with CV
buoy2017_L1 <- buoy2017_L1 %>% 
  rename(windDirectionAverage_deg = AveWindDir,
         windSpeedAverage_mps = AveWindSp,
         windGustDirection_deg = MaxWindDir,
         windGustSpeed_mps = MaxWindSp)

rm(buoy_wind_vert, buoy_wind_vert_L1)

####PAR####
range(buoy2017_L1$PAR, na.rm = T)

#recode when in transit
buoy2017_L1 <-  buoy2017_L1 %>% 
  mutate(PAR = case_when(location == 'in transit' ~ NA_real_,
                         TRUE ~ PAR))

#replace negative values with 0 and flag
buoy2017_L1 <-  buoy2017_L1 %>% 
  mutate(flag_par = case_when(PAR < 0 ~ 'z',
                              TRUE ~ '')) %>% 
  mutate(PAR = case_when(PAR < 0 ~ 0,
                         TRUE ~ PAR))

ggplot(buoy2017_L1,
       aes(x=datetime, y=PAR, color=location)) +
  geom_point() +
  labs(title = 'PAR 2017, NA recoded') +
  scale_x_datetime(date_minor_breaks = '1 month') +
  final_theme +
  scale_color_colorblind()

# ggplot(subset(buoy2017_L1,
#               subset=(datetime >= as.POSIXct('2017-01-01', tz=buoy_tz) & datetime < as.POSIXct('2017-02-01', tz=buoy_tz))),
#        aes(x=datetime, y=PAR, color=location)) +
#   geom_point() +
#   labs(title = 'PAR 2017, NA recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy2017_L1,
#               subset=(datetime >= as.POSIXct('2017-02-01', tz=buoy_tz) & datetime < as.POSIXct('2017-03-01', tz=buoy_tz))),
#        aes(x=datetime, y=PAR, color=location)) +
#   geom_point() +
#   labs(title = 'PAR 2017, NA recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()

#par sensor obscured here
buoy2017_L1 <- buoy2017_L1 %>% 
  mutate(flag_par = case_when(flag_par == '' & datetime >= as.POSIXct('2017-02-09', tz=buoy_tz) & 
                                datetime < as.POSIXct('2017-02-18', tz=buoy_tz) ~ 'o',
                              flag_par != '' & datetime >= as.POSIXct('2017-02-09', tz=buoy_tz) & 
                                datetime < as.POSIXct('2017-02-18', tz=buoy_tz) ~ paste('o', flag_par, sep = '; '),
                              TRUE ~ flag_par))

# ggplot(subset(buoy2017_L1,
#               subset=(datetime >= as.POSIXct('2017-03-01', tz=buoy_tz) & datetime < as.POSIXct('2017-04-01', tz=buoy_tz))),
#        aes(x=datetime, y=PAR, color=location)) +
#   geom_point() +
#   labs(title = 'PAR 2017, NA recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy2017_L1,
#               subset=(datetime >= as.POSIXct('2017-04-01', tz=buoy_tz) & datetime < as.POSIXct('2017-05-01', tz=buoy_tz))),
#        aes(x=datetime, y=PAR, color=location)) +
#   geom_point() +
#   labs(title = 'PAR 2017, NA recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()

# ggplot(subset(buoy2017_L1,
#               subset=(datetime >= as.POSIXct('2017-05-01', tz=buoy_tz) & datetime < as.POSIXct('2017-06-01', tz=buoy_tz))),
#        aes(x=datetime, y=PAR, color=location)) +
#   geom_point() +
#   labs(title = 'PAR 2017, NA recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy2017_L1,
#               subset=(datetime >= as.POSIXct('2017-06-01', tz=buoy_tz) & datetime < as.POSIXct('2017-07-01', tz=buoy_tz))),
#        aes(x=datetime, y=PAR, color=location)) +
#   geom_point() +
#   labs(title = 'PAR 2017, NA recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy2017_L1,
#               subset=(datetime >= as.POSIXct('2017-07-01', tz=buoy_tz) & datetime < as.POSIXct('2017-08-01', tz=buoy_tz))),
#        aes(x=datetime, y=PAR, color=location)) +
#   geom_point() +
#   labs(title = 'PAR 2017, NA recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy2017_L1,
#               subset=(datetime >= as.POSIXct('2017-08-01', tz=buoy_tz) & datetime < as.POSIXct('2017-09-01', tz=buoy_tz))),
#        aes(x=datetime, y=PAR, color=location)) +
#   geom_point() +
#   labs(title = 'PAR 2017, NA recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()

# ggplot(subset(buoy2017_L1,
#               subset=(datetime >= as.POSIXct('2017-09-01', tz=buoy_tz) & datetime < as.POSIXct('2017-10-01', tz=buoy_tz))),
#        aes(x=datetime, y=PAR, color=location)) +
#   geom_point() +
#   labs(title = 'PAR 2017, NA recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy2017_L1,
#               subset=(datetime >= as.POSIXct('2017-10-01', tz=buoy_tz) & datetime < as.POSIXct('2017-11-01', tz=buoy_tz))),
#        aes(x=datetime, y=PAR, color=location)) +
#   geom_point() +
#   labs(title = 'PAR 2017, NA recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy2017_L1,
#               subset=(datetime >= as.POSIXct('2017-11-01', tz=buoy_tz) & datetime < as.POSIXct('2017-12-01', tz=buoy_tz))),
#        aes(x=datetime, y=PAR, color=location)) +
#   geom_point() +
#   labs(title = 'PAR 2017, NA recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy2017_L1,
#               subset=(datetime >= as.POSIXct('2017-12-01', tz=buoy_tz) & datetime < as.POSIXct('2018-01-01', tz=buoy_tz))),
#        aes(x=datetime, y=PAR, color=location)) +
#   geom_point() +
#   labs(title = 'PAR 2017, NA recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()

ggplot(buoy2017_L1,
       aes(x=datetime, y=PAR, color=location, shape = flag_par)) +
  geom_point() +
  labs(title = 'PAR 2017, NA recoded') +
  scale_x_datetime(date_minor_breaks = '1 month') +
  final_theme +
  scale_color_colorblind()

#rename with CV
buoy2017_L1 <- buoy2017_L1 %>% 
  rename(radiationIncomingPAR_umolm2s = PAR)

#### Air temp and rel humitidy ####
range(buoy2017_L1$AirTempC, na.rm = T)
range(buoy2017_L1$RelHum, na.rm = T)

buoy2017_L1 <- buoy2017_L1 %>% 
  mutate_at(vars(AirTempC, RelHum),
            ~ case_when(location == 'in transit' ~ NA_real_,
                        TRUE ~.))

air_vert <- buoy2017_L1 %>% 
  select(datetime, AirTempC, RelHum, location) %>% 
  pivot_longer(names_to = 'variable', values_to = 'value', -c(datetime, location))

ggplot(air_vert, aes(x=datetime, y = value)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  final_theme +
  labs(title = '2017 air temp raw',
       x= NULL,
       y= 'air temp (deg C)') +
  scale_x_datetime(date_minor_breaks = '1 month')

#no NA values to recode

# ggplot(subset(air_vert,
#               subset=(datetime>=as.POSIXct('2017-01-01', tz=buoy_tz) & datetime<as.POSIXct('2017-02-01', tz=buoy_tz))),
#        aes(x=datetime, value, color = location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'jan 2017 air temp', x=NULL, y='Air temp (deg C)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(air_vert,
#               subset=(datetime>=as.POSIXct('2017-02-01', tz=buoy_tz) & datetime<as.POSIXct('2017-03-01', tz=buoy_tz))),
#        aes(x=datetime, value, color = location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'feb 2017 air temp', x=NULL, y='Air temp (deg C)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(air_vert,
#               subset=(datetime>=as.POSIXct('2017-03-01', tz=buoy_tz) & datetime<as.POSIXct('2017-04-01', tz=buoy_tz))),
#        aes(x=datetime, value, color = location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'mar 2017 air temp', x=NULL, y='Air temp (deg C)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(air_vert,
#               subset=(datetime>=as.POSIXct('2017-04-01', tz=buoy_tz) & datetime<as.POSIXct('2017-05-01', tz=buoy_tz))),
#        aes(x=datetime, value, color = location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'apr 2017 air temp', x=NULL, y='Air temp (deg C)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(air_vert,
#               subset=(datetime>=as.POSIXct('2017-05-01', tz=buoy_tz) & datetime<as.POSIXct('2017-06-01', tz=buoy_tz))),
#        aes(x=datetime, value, color = location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'may 2017 air temp', x=NULL, y='Air temp (deg C)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(air_vert,
#               subset=(datetime>=as.POSIXct('2017-06-01', tz=buoy_tz) & datetime<as.POSIXct('2017-07-01', tz=buoy_tz))),
#        aes(x=datetime, value, color = location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'jun 2017 air temp', x=NULL, y='Air temp (deg C)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(air_vert,
#               subset=(datetime>=as.POSIXct('2017-07-01', tz=buoy_tz) & datetime<as.POSIXct('2017-08-01', tz=buoy_tz))),
#        aes(x=datetime, value, color = location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'jul 2017 air temp', x=NULL, y='Air temp (deg C)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(air_vert,
#               subset=(datetime>=as.POSIXct('2017-08-01', tz=buoy_tz) & datetime<as.POSIXct('2017-09-01', tz=buoy_tz))),
#        aes(x=datetime, value, color = location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'aug 2017 air temp', x=NULL, y='Air temp (deg C)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(air_vert,
#               subset=(datetime>=as.POSIXct('2017-09-01', tz=buoy_tz) & datetime<as.POSIXct('2017-10-01', tz=buoy_tz))),
#        aes(x=datetime, value, color = location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'sept 2017 air temp', x=NULL, y='Air temp (deg C)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(air_vert,
#               subset=(datetime>=as.POSIXct('2017-10-01', tz=buoy_tz) & datetime<as.POSIXct('2017-11-01', tz=buoy_tz))),
#        aes(x=datetime, value, color = location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'oct 2017 air temp', x=NULL, y='Air temp (deg C)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(air_vert,
#               subset=(datetime>=as.POSIXct('2017-11-01', tz=buoy_tz) & datetime<as.POSIXct('2017-12-01', tz=buoy_tz))),
#        aes(x=datetime, value, color = location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'nov 2017 air temp', x=NULL, y='Air temp (deg C)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(air_vert,
#               subset=(datetime>=as.POSIXct('2017-12-01', tz=buoy_tz) & datetime<as.POSIXct('2017-01-01', tz=buoy_tz))),
#        aes(x=datetime, value, color = location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'dec 2017 air temp', x=NULL, y='Air temp (deg C)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')

ggplot(air_vert,
       aes(x=datetime, value, color = location)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title = '2017 air temp', x=NULL, y='Air temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 month')

#add RH flags for supersaturated 
buoy2017_L1 <- buoy2017_L1 %>% 
  mutate(flag_rh = case_when(RelHum >100 ~ 's',
                             TRUE ~ ''))

#rename with CV
buoy2017_L1 <- buoy2017_L1 %>% 
  rename(relativeHumidity_perc = RelHum,
         airTemperature_degC = AirTempC)

rm(air_vert)


#### EXPORT L1 DATA STREAMS ####

colnames(buoy2017_L1)

#export L1 tempstring file
buoy2017_L1 %>%
  select(datetime, location, waterTemperature_degC_0p85m:waterTemperature_degC_9p85m, flag_temp0p85m) %>%
  mutate(datetime = as.character(datetime)) %>%
  write_csv(., file.path(dump_dir,'tempstring/2017_tempstring_L1_v2022.csv'))

# export L1 do file
buoy2017_L1 %>%
  select(datetime, location, 
         oxygenDissolved_mgl_1p5m, oxygenDissolvedPercentOfSaturation_pct_1p5m, waterTemperature_DO_degC_1p5m, 
         flag_do1p5m,
         oxygenDissolved_mgl_10p5m, oxygenDissolvedPercentOfSaturation_pct_10p5m, waterTemperature_DO_degC_10p5m, 
         flag_do10p5m) %>%
  mutate(datetime = as.character(datetime)) %>%
  write_csv(., file.path(dump_dir, 'do/2017_do_L1_v2022.csv'))

#export L1 met file
buoy2017_L1 %>%
  select(datetime, location, 
         windSpeedAverage_mps:windGustDirection_deg, flag_allwind,
         radiationIncomingPAR_umolm2s, flag_par,
         airTemperature_degC, relativeHumidity_perc, flag_rh) %>%
  mutate(datetime = as.character(datetime)) %>%
  write_csv(., file.path(dump_dir, 'met/2017_met_L1_v2022.csv'))
