#*****************************************************************
#*      Cary Institute of Ecosystem Studies (Millbrook, NY)      *
#*                                                               *
#* TITLE:   Sunapee_buoy_2018.r                                  *
#* AUTHOR:  Bethel Steele                                        *
#* PROJECT: Lake Sunapee Buoy Data Cleaning                      *
#*****************************************************************

source('library_func_lists.R')

#point to data directories
raw_dir = 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L0/'
dump_dir = 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/'

#set tz
buoy_tz = 'Etc/GMT+5'

#bring in  buoy raw data
buoy2018_L0 <- read.csv(file.path(raw_dir, '2018 Buoy Data.csv'))

#### format data ####
buoy2018_L0 <- buoy2018_L0  %>%
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
ggplot(buoy2018_L0, aes(x = datetime, y = LoggerBatV)) +
  geom_point()

ggplot(buoy2018_L0, aes(x = datetime, y = RadioBatV)) +
  geom_point()
# issues at beginning of year; will have to see if it impacts wind data like at end of year 2017

#double check to make sure there are no DST issues
datelength2018 <- buoy2018_L0 %>% 
  mutate(date = format(datetime, '%Y-%m-%d')) %>% 
  group_by(date) %>% 
  summarize(length(datetime))

#check dst dates
datelength2018[datelength2018$date == '2018-03-11',]
#no data
datelength2018[datelength2018$date == '2018-11-04',]
#dst observed on the 5th
datelength2018[datelength2018$date == '2018-11-05',]

# see where dst occurs
buoy2018_L0 %>% 
  filter(datetime >= as.POSIXct('2018-03-11', tz=buoy_tz) & 
           datetime < as.POSIXct('2018-03-12', tz=buoy_tz)) %>% 
  select(datetime, rowid)

buoy2018_L0 %>% 
  filter(datetime >= as.POSIXct('2018-11-05', tz=buoy_tz) & 
           datetime < as.POSIXct('2018-11-06', tz=buoy_tz)) %>% 
  select(datetime, rowid)


#DST observed at odd times need to do time math
buoy2018_L1 <- buoy2018_L0 %>% 
  mutate(datetime.instrument = datetime,
         rowid = as.numeric(rowid))
buoy2018_L1a <- buoy2018_L1 %>% 
  filter(rowid <= 10071)
buoy2018_L1b <- buoy2018_L1 %>% 
  filter(rowid > 10071 & rowid <= 44334)
buoy2018_L1c <- buoy2018_L1 %>% 
  filter(rowid > 44334)
#apply time math on middle section
buoy2018_L1b <- buoy2018_L1b %>% 
  mutate(datetime = datetime-hours(1))

#rejoin and force tz as buoytz
buoy2018_L1 <- full_join(buoy2018_L1a, buoy2018_L1b) %>% 
  full_join(., buoy2018_L1c) %>% 
  mutate(datetime = as.POSIXct(datetime, tz = buoy_tz))

#double check to make sure there are no DST issues
datelength2018 <- buoy2018_L1 %>% 
  mutate(date = format(datetime, '%Y-%m-%d')) %>% 
  group_by(date) %>% 
  summarize(length(datetime))

#check dst dates
datelength2018[datelength2018$date == '2018-03-11',]
datelength2018[datelength2018$date == '2018-11-04',]
datelength2018[datelength2018$date == '2018-11-05',]
#looks good

# add in all date time options in L1 data set
alltimes_2018 <- as.data.frame(seq.POSIXt(as.POSIXct('2018-01-01 00:00', tz=buoy_tz), as.POSIXct('2018-12-31 23:50', tz=buoy_tz), '10 min')) %>% 
  rename("datetime" = !!names(.[1]))

buoy2018_L1 <- buoy2018_L1 %>% 
  right_join(., alltimes_2018) %>% 
  arrange(datetime)

#clean up workspace
rm(alltimes_2018, datelength2018, buoy2018_L1a, buoy2018_L1b, buoy2018_L1c)


####THERMISTORS####
buoy2018_therm_vert <- buoy2018_L1 %>% 
  select(datetime, all_of(alltemp2011)) %>% 
  pivot_longer(names_to = 'variable', values_to = 'value', -datetime) %>% 
  mutate(variable = factor(variable, levels=alltemp))

# ggplot(buoy2018_therm_vert, aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 month') +
#   final_theme

buoy2018_L1 <- buoy2018_L1 %>% 
  mutate_at(vars(all_of(alltemp2011)),
            ~(case_when(. == -6999 ~ NA_real_,
                           TRUE ~ .)))

buoy2018_therm_vert <- buoy2018_L1 %>% 
  select(datetime, all_of(alltemp2011)) %>% 
  pivot_longer(names_to = 'variable', values_to = 'value', -datetime) %>% 
  mutate(variable = factor(variable, levels=alltemp))

ggplot(buoy2018_therm_vert, aes(x=datetime, y=value, color = variable)) +
  geom_point() +
  scale_x_datetime(date_minor_breaks = '1 month') +
    scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                                "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
    final_theme

# ggplot(subset(buoy2018_therm_vert,
#               subset=(datetime >= as.POSIXct('2018-05-01', tz=buoy_tz) & datetime < as.POSIXct('2018-06-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme
# 
# #buoy deployment
# ggplot(subset(buoy2018_therm_vert,
#               subset=(datetime >= as.POSIXct('2018-05-21', tz=buoy_tz) & datetime < as.POSIXct('2018-05-22', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme

buoy2018_L1 <- buoy2018_L1 %>% 
  mutate_at(vars(all_of(alltemp2011)),
            ~(case_when(datetime < as.POSIXct('2018-05-21 8:30', tz=buoy_tz) ~ NA_real_,
                           TRUE ~ .))) 

#look at 2m temp alone - looks like it is only reporting integers
unique(buoy2018_L1$TempC_2m)

#remove all 2m data - only integers reported
buoy2018_L1 <- buoy2018_L1 %>% 
  mutate(TempC_2m = NA_real_)
buoy2018_therm_vert_L1 <- buoy2018_L1 %>% 
  select(datetime, all_of(alltemp2011)) %>% 
  pivot_longer(names_to = 'variable', values_to = 'value', -datetime) %>% 
  mutate(variable = factor(variable, levels=alltemp))

# ggplot(subset(buoy2018_therm_vert_L1,
#               subset=(datetime >= as.POSIXct('2018-05-01', tz=buoy_tz) & datetime < as.POSIXct('2018-06-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme
# 
# ggplot(subset(buoy2018_therm_vert_L1,
#               subset=(datetime >= as.POSIXct('2018-06-01', tz=buoy_tz) & datetime < as.POSIXct('2018-07-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme
# 
# ggplot(subset(buoy2018_therm_vert_L1,
#               subset=(datetime >= as.POSIXct('2018-07-01', tz=buoy_tz) & datetime < as.POSIXct('2018-08-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme
# 
# ggplot(subset(buoy2018_therm_vert_L1,
#               subset=(datetime >= as.POSIXct('2018-08-01', tz=buoy_tz) & datetime < as.POSIXct('2018-09-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme
# 
# ggplot(subset(buoy2018_therm_vert_L1,
#               subset=(datetime >= as.POSIXct('2018-09-01', tz=buoy_tz) & datetime < as.POSIXct('2018-10-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme
# 
# ggplot(subset(buoy2018_therm_vert_L1,
#               subset=(datetime >= as.POSIXct('2018-10-01', tz=buoy_tz) & datetime < as.POSIXct('2018-11-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme
# 
# #buoy moved for winter
# ggplot(subset(buoy2018_therm_vert_L1,
#               subset=(datetime >= as.POSIXct('2018-10-19', tz=buoy_tz) & datetime < as.POSIXct('2018-10-20', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme

buoy2018_L1 <- buoy2018_L1 %>% 
  mutate_at(vars(all_of(alltemp2011)),
            ~(case_when(datetime >= as.POSIXct('2018-10-19 9:30', tz=buoy_tz) ~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate(flag_temp0p85m = case_when(datetime >= as.POSIXct('2018-05-21 8:30', tz=buoy_tz) & 
                                 datetime < as.POSIXct('2018-10-19 9:30', tz=buoy_tz) ~ 'a',
                          TRUE ~ ''))

buoy2018_therm_vert_L1 <- buoy2018_L1 %>% 
  select(datetime, all_of(alltemp2011)) %>% 
  pivot_longer(names_to = 'variable', values_to = 'value', -datetime) %>% 
  mutate(variable = factor(variable, levels=alltemp))

ggplot(buoy2018_therm_vert_L1,
       aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  scale_x_datetime(date_minor_breaks = '1 month') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  final_theme

#correct column names for sensor offset from surface
buoy2018_L1 <- buoy2018_L1 %>% 
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

rm(buoy2018_therm_vert, buoy2018_therm_vert_L1)


#### DO ####
buoy2018_do_vert <- buoy2018_L1 %>% 
  select(datetime, all_of(upDO), all_of(lowDO)) %>%
  pivot_longer(names_to = 'variable', values_to = 'value', -datetime)

ggplot(buoy2018_do_vert, aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  scale_x_datetime(date_minor_breaks = '1 month') +
  final_theme

buoy2018_L1 <- buoy2018_L1 %>% 
  mutate_at(vars(all_of(upDO), all_of(lowDO)),
            ~(case_when(. == -6999 ~ NA_real_,
                           . == 0 ~ NA_real_,
                           TRUE ~ .)))

#no updo data from buoy
buoy2018_L1 <- buoy2018_L1 %>% 
  mutate(DOTempC = NA_real_,
         DOSat = NA_real_,
         DOppm = NA_real_)

buoy2018_do_vert_L1 <- buoy2018_L1 %>% 
  select(datetime, all_of(lowDO)) %>%
  pivot_longer(names_to = 'variable', values_to = 'value', -datetime)

ggplot(buoy2018_do_vert_L1, aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  scale_x_datetime(date_minor_breaks = '1 month') +
  final_theme


# ggplot(subset(buoy2018_do_vert_L1,
#               subset=(datetime >= as.POSIXct('2018-05-01', tz=buoy_tz) & datetime < as.POSIXct('2018-06-01', tz=buoy_tz))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme
# 
# ggplot(subset(buoy2018_do_vert_L1,
#               subset=(datetime >= as.POSIXct('2018-05-21', tz=buoy_tz) & datetime < as.POSIXct('2018-05-22', tz=buoy_tz))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme

#buoy move 5-21
buoy2018_L1 <- buoy2018_L1 %>% 
  mutate_at(vars(lowDO),
            ~(case_when(datetime < as.POSIXct('2018-05-21 09:00', tz=buoy_tz) ~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate(location = case_when(datetime >= as.POSIXct('2018-05-21 8:30', tz=buoy_tz) ~ 'loon',
                              datetime >= as.POSIXct('2018-05-21 7:30', tz=buoy_tz) & 
                                datetime < as.POSIXct('2018-05-21 8:30', tz=buoy_tz) ~ 'in transit',
                              TRUE ~ 'harbor')) %>% 
  mutate(flag_do10p5m = case_when(datetime == as.POSIXct('2018-05-21 9:00', tz=buoy_tz) ~ 'cp',
                                TRUE ~ ''))

buoy2018_do_vert_L1 <- buoy2018_L1 %>% 
  select(datetime, all_of(lowDO), location) %>%
  pivot_longer(names_to = 'variable', values_to = 'value', -c(datetime, location))

# ggplot(subset(buoy2018_do_vert_L1,
#               subset=(datetime >= as.POSIXct('2018-05-01', tz=buoy_tz) & datetime < as.POSIXct('2018-06-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color = location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme
# 
# ggplot(subset(buoy2018_do_vert_L1,
#               subset=(datetime >= as.POSIXct('2018-06-01', tz=buoy_tz) & datetime < as.POSIXct('2018-07-01', tz=buoy_tz))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme
# 
# #june 20-26 do errant
# ggplot(subset(buoy2018_do_vert_L1,
#               subset=(datetime >= as.POSIXct('2018-06-20', tz=buoy_tz) & datetime < as.POSIXct('2018-06-21', tz=buoy_tz))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme
# 
# ggplot(subset(buoy2018_do_vert_L1,
#               subset=(datetime >= as.POSIXct('2018-06-26', tz=buoy_tz) & datetime < as.POSIXct('2018-06-27', tz=buoy_tz))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme

buoy2018_L1 <- buoy2018_L1 %>% 
  mutate(DOLowTempC = case_when(datetime == as.POSIXct('2018-06-20 7:10', tz=buoy_tz) ~ NA_real_,
                                datetime == as.POSIXct('2018-06-26 7:50', tz=buoy_tz) ~ NA_real_,
                                TRUE ~ DOLowTempC)) %>% 
  mutate_at(vars(DOLowPPM, DOLowSat),
            ~case_when(datetime >= as.POSIXct('2018-06-20 7:10', tz=buoy_tz) & 
                         datetime < as.POSIXct('2018-06-20 7:50', tz = buoy_tz) ~ NA_real_,
                       datetime >= as.POSIXct('2018-06-26 07:50', tz = buoy_tz) &
                         datetime < as.POSIXct('2018-06-26 08:30', tz=buoy_tz)~ NA_real_,
                       TRUE ~ .)) %>% 
  mutate(flag_do10p5m =case_when(flag_do10p5m == '' & 
                                   datetime >= as.POSIXct('2018-06-20 7:10', tz=buoy_tz) & 
                                   datetime < as.POSIXct('2018-06-26 8:30', tz=buoy_tz) ~ 're',
                                 flag_do10p5m != '' & 
                                   datetime >= as.POSIXct('2018-06-20 7:10', tz=buoy_tz) & 
                                   datetime < as.POSIXct('2018-06-26 8:30', tz=buoy_tz) ~ paste('re', flag_do10p5m, sep = '; '),
                                 TRUE ~ flag_do10p5m)) 
buoy2018_do_vert_L1 <- buoy2018_L1 %>% 
  select(datetime, all_of(lowDO), location, flag_do10p5m) %>%
  pivot_longer(names_to = 'variable', values_to = 'value', -c(datetime, location, flag_do10p5m))

# ggplot(subset(buoy2018_do_vert_L1,
#               subset=(datetime >= as.POSIXct('2018-06-01', tz=buoy_tz) & datetime < as.POSIXct('2018-07-01', tz=buoy_tz))),
#        aes(x=datetime, y=value,color = flag_do10p5m)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme
# 
# ggplot(subset(buoy2018_do_vert_L1,
#               subset=(datetime >= as.POSIXct('2018-07-01', tz=buoy_tz) & datetime < as.POSIXct('2018-08-01', tz=buoy_tz))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme
# 
# ggplot(subset(buoy2018_do_vert_L1,
#               subset=(datetime >= as.POSIXct('2018-08-01', tz=buoy_tz) & datetime < as.POSIXct('2018-09-01', tz=buoy_tz))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme
# 
# ggplot(subset(buoy2018_do_vert_L1,
#               subset=(datetime >= as.POSIXct('2018-09-01', tz=buoy_tz) & datetime < as.POSIXct('2018-10-01', tz=buoy_tz))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme
# 
# #oddball 9/10
# ggplot(subset(buoy2018_do_vert_L1,
#               subset=(datetime >= as.POSIXct('2018-09-10', tz=buoy_tz) & datetime < as.POSIXct('2018-09-11', tz=buoy_tz))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme

buoy2018_L1 <- buoy2018_L1 %>% 
  mutate_at(vars(lowDO),
            ~(case_when(datetime == as.POSIXct('2018-09-10 18:20', tz=buoy_tz) ~ NA_real_,
                           TRUE ~ .)))
buoy2018_do_vert_L1 <- buoy2018_L1 %>% 
  select(datetime, all_of(lowDO)) %>%
  pivot_longer(names_to = 'variable', values_to = 'value', -datetime)

# ggplot(subset(buoy2018_do_vert_L1,
#               subset=(datetime >= as.POSIXct('2018-09-01', tz=buoy_tz) & datetime < as.POSIXct('2018-10-01', tz=buoy_tz))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme
# 
# ggplot(subset(buoy2018_do_vert_L1,
#               subset=(datetime >= as.POSIXct('2018-10-01', tz=buoy_tz) & datetime < as.POSIXct('2018-11-01', tz=buoy_tz))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme
# 
# #buoy to harbor 10-19
# ggplot(subset(buoy2018_do_vert_L1,
#               subset=(datetime >= as.POSIXct('2018-10-19', tz=buoy_tz) & datetime < as.POSIXct('2018-10-20', tz=buoy_tz))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme

buoy2018_L1 <- buoy2018_L1 %>% 
  mutate_at(vars(lowDO),
            ~(case_when(datetime >= as.POSIXct('2018-10-19 9:30', tz=buoy_tz) ~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate(location = case_when(datetime >= as.POSIXct('2018-10-19 9:30', tz=buoy_tz) ~ 'in transit',
                              TRUE ~ location))

#add flag for no calibration on record
buoy2018_L1 <- buoy2018_L1 %>% 
  mutate(flag_do10p5m = case_when(flag_do10p5m == '' ~ 'x', 
                                  flag_do10p5m != '' ~ paste('x', flag_do10p5m, sep = '; '),
                                  TRUE ~ flag_do10p5m))
buoy2018_do_vert_L1 <- buoy2018_L1 %>%
  select(datetime, all_of(lowDO), location, flag_do10p5m) %>%
  pivot_longer(names_to = 'variable', values_to = 'value', -c(datetime, location, flag_do10p5m))

ggplot(buoy2018_do_vert_L1,
       aes(x=datetime, y=value, color = flag_do10p5m, shape = location)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  scale_x_datetime(date_minor_breaks = '1 month') +
  labs(title = 'do 2018, clean') +
  final_theme +
  scale_color_colorblind()

#rename with CV
buoy2018_L1 <- buoy2018_L1 %>% 
  rename(oxygenDissolved_mgl_10p5m = DOLowPPM,
         oxygenDissolvedPercentOfSaturation_pct_10p5m = DOLowSat,
         waterTemperature_DO_degC_10p5m = DOLowTempC)

rm(buoy2018_do_vert, buoy2018_do_vert_L1)


####CHLA####
## sensor never functions properly ####
# buoy2018_chla_vert <- buoy2018_L1 %>%
#   select(datetime, chla) %>%
#   gather(variable, value, -datetime)
#
# ggplot(buoy2018_chla_vert, aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 month') +
#   final_theme

#chla sensor not working
buoy2018_L1 <- buoy2018_L1 %>%
  mutate(Chlor_RFU = is.na(Chlor_RFU),
         Chlor_UGL = is.na(Chlor_UGL),
         SpecCond = is.na(SpecCond))



####wind####
range(buoy2018_L1$AveWindSp, na.rm = T)
range(buoy2018_L1$AveWindDir, na.rm = T)
range(buoy2018_L1$MaxWindSp, na.rm = T)
range(buoy2018_L1$MaxWindDir, na.rm = T)

#recode offline/in transit prior to buoy move back to harbor
buoy2018_L1 <- buoy2018_L1 %>% 
  mutate_at(vars(AveWindDir, AveWindSp, MaxWindDir, MaxWindSp),
            ~(case_when(location == 'in transit' & datetime < as.Date('2018-10-18')~ NA_real_,
                        TRUE ~ .)))

buoy_wind_vert <- buoy2018_L1 %>%
  select(datetime, AveWindDir, AveWindSp, MaxWindDir, MaxWindSp, location) %>% 
  pivot_longer(names_to = 'variable', values_to = 'value', -c(datetime, location))

ggplot(buoy_wind_vert,
       aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title = 'wind 2018, raw') +
  scale_x_datetime(date_minor_breaks = '1 month') +
  final_theme +
  scale_color_colorblind()

# ggplot(subset(buoy_wind_vert,
#               subset=(datetime >= as.POSIXct('2018-01-01', tz=buoy_tz) & datetime < as.POSIXct('2018-02-01', tz=buoy_tz))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'jan wind 2018') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# #sensor frozen jan 17-18
# ggplot(subset(buoy_wind_vert,
#               subset=(datetime >= as.POSIXct('2018-01-17 12:00', tz=buoy_tz) & datetime < as.POSIXct('2018-01-18 16:00', tz=buoy_tz))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'jan wind 2018, NAs recoded') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme +
#   scale_color_colorblind()
# 
#sensor frozen jan 23
ggplot(subset(buoy_wind_vert,
              subset=(datetime >= as.POSIXct('2018-01-23', tz=buoy_tz) & datetime < as.POSIXct('2018-01-24', tz=buoy_tz))),
       aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title = 'jan wind 2018, NAs recoded') +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme +
  scale_color_colorblind()
# #data prior and directly after to this freeze are suspect flagging as such


buoy2018_L1 <- buoy2018_L1 %>% 
  mutate_at(vars(AveWindDir, AveWindSp, MaxWindDir, MaxWindSp),
            ~(case_when(datetime >= as.POSIXct('2018-01-17 13:40', tz=buoy_tz) & 
                          datetime < as.POSIXct('2018-01-18 14:30', tz=buoy_tz) & MaxWindSp == 0 ~ NA_real_,
                           datetime >= as.POSIXct('2018-01-23 6:10', tz=buoy_tz) & 
                          datetime < as.POSIXct('2018-01-23 22:00', tz=buoy_tz) ~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate(flag_allwind = case_when(datetime >= as.POSIXct('2018-01-22 11:00', tz= buoy_tz) & 
                                    datetime < as.POSIXct('2018-01-24', tz=buoy_tz) ~ 's',
                                  TRUE ~ ''))

buoy_wind_vert_L1 <- buoy2018_L1 %>% 
  select(datetime, AveWindDir, AveWindSp, MaxWindDir, MaxWindSp, location, flag_allwind) %>% 
  pivot_longer(names_to = 'variable', values_to = 'value', -c(datetime, location, flag_allwind))

# ggplot(subset(buoy_wind_vert_L1,
#               subset=(datetime >= as.POSIXct('2018-01-01', tz=buoy_tz) & datetime < as.POSIXct('2018-02-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color = flag_allwind)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'jan wind 2018, clean') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# 
# ggplot(subset(buoy_wind_vert_L1,
#               subset=(datetime >= as.POSIXct('2018-02-01', tz=buoy_tz) & datetime < as.POSIXct('2018-03-01', tz=buoy_tz))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'feb wind 2018') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# #sensor frozen feb 2
# ggplot(subset(buoy_wind_vert,
#               subset=(datetime >= as.POSIXct('2018-02-02', tz=buoy_tz) & datetime < as.POSIXct('2018-02-03', tz=buoy_tz))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme +
#   scale_color_colorblind()
# 
# #sensor frozen feb 18
# ggplot(subset(buoy_wind_vert,
#               subset=(datetime >= as.POSIXct('2018-02-18', tz=buoy_tz) & datetime < as.POSIXct('2018-02-19', tz=buoy_tz))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme +
#   scale_color_colorblind()

buoy2018_L1 <- buoy2018_L1 %>% 
  mutate_at(vars(AveWindDir, AveWindSp, MaxWindDir, MaxWindSp),
            ~(case_when(datetime >= as.POSIXct('2018-02-02 4:30', tz=buoy_tz) & 
                          datetime < as.POSIXct('2018-02-02 12:50', tz=buoy_tz)  & MaxWindSp == 0  ~ NA_real_,
                           datetime >= as.POSIXct('2018-02-18 4:30', tz=buoy_tz) & 
                          datetime < as.POSIXct('2018-02-18 10:30', tz=buoy_tz) & MaxWindSp == 0  ~ NA_real_,
                           TRUE ~ .)))

buoy_wind_vert_L1 <- buoy2018_L1 %>% 
  select(datetime, AveWindDir, AveWindSp, MaxWindDir, MaxWindSp, location) %>% 
  pivot_longer(names_to = 'variable', values_to = 'value', -c(datetime, location))

# ggplot(subset(buoy_wind_vert_L1,
#               subset=(datetime >= as.POSIXct('2018-02-01', tz=buoy_tz) & datetime < as.POSIXct('2018-03-01', tz=buoy_tz))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'feb wind 2018') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy_wind_vert_L1,
#               subset=(datetime >= as.POSIXct('2018-03-01', tz=buoy_tz) & datetime < as.POSIXct('2018-04-01', tz=buoy_tz))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'mar wind 2018') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# #frozenmar08 - mar10
# ggplot(subset(buoy_wind_vert_L1,
#               subset=(datetime >= as.POSIXct('2018-03-08', tz=buoy_tz) & datetime < as.POSIXct('2018-03-09', tz=buoy_tz))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'mar wind 2018') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme +
#   scale_color_colorblind()
# ggplot(subset(buoy_wind_vert_L1,
#               subset=(datetime >= as.POSIXct('2018-03-10', tz=buoy_tz) & datetime < as.POSIXct('2018-03-11', tz=buoy_tz))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'mar wind 2018') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme +
#   scale_color_colorblind()
# 
# #frozen mar13 - mar14
# ggplot(subset(buoy_wind_vert_L1,
#               subset=(datetime >= as.POSIXct('2018-03-13 12:00', tz=buoy_tz) & datetime < as.POSIXct('2018-03-14 16:00', tz=buoy_tz))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'mar wind 2018') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme +
#   scale_color_colorblind()

buoy2018_L1 <- buoy2018_L1 %>% 
  mutate_at(vars(AveWindDir, AveWindSp, MaxWindDir, MaxWindSp),
            ~(case_when(datetime >= as.POSIXct('2018-03-08 5:00', tz=buoy_tz) & 
                          datetime < as.POSIXct('2018-03-10 14:30', tz=buoy_tz)  & MaxWindSp == 0 ~ NA_real_,
                           datetime >= as.POSIXct('2018-03-13 20:20', tz=buoy_tz) & 
                          datetime < as.POSIXct('2018-03-14 13:20', tz=buoy_tz)  & MaxWindSp == 0 ~ NA_real_,
                           TRUE ~ .)))

buoy_wind_vert_L1 <- buoy2018_L1 %>% 
  select(datetime, AveWindDir, AveWindSp, MaxWindDir, MaxWindSp, location) %>% 
  pivot_longer(names_to = 'variable', values_to = 'value', -c(datetime, location))

# ggplot(subset(buoy_wind_vert_L1,
#               subset=(datetime >= as.POSIXct('2018-03-01', tz=buoy_tz) & datetime < as.POSIXct('2018-04-01', tz=buoy_tz))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'mar wind 2018') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy_wind_vert_L1,
#               subset=(datetime >= as.POSIXct('2018-04-01', tz=buoy_tz) & datetime < as.POSIXct('2018-05-01', tz=buoy_tz))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'apr wind 2018') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy_wind_vert_L1,
#               subset=(datetime >= as.POSIXct('2018-05-01', tz=buoy_tz) & datetime < as.POSIXct('2018-06-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color = location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'may wind 2018') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# #may 21 buoy moved to loon
# ggplot(subset(buoy_wind_vert_L1,
#               subset=(datetime >= as.POSIXct('2018-05-21', tz=buoy_tz) & datetime < as.POSIXct('2018-05-22', tz=buoy_tz))),
#        aes(x=datetime, y=value, color = location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'may wind 2018') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy_wind_vert_L1,
#               subset=(datetime >= as.POSIXct('2018-06-01', tz=buoy_tz) & datetime < as.POSIXct('2018-07-01', tz=buoy_tz))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'june wind 2018') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy_wind_vert_L1,
#               subset=(datetime >= as.POSIXct('2018-07-01', tz=buoy_tz) & datetime < as.POSIXct('2018-08-01', tz=buoy_tz))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'july wind 2018') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy_wind_vert_L1,
#               subset=(datetime >= as.POSIXct('2018-08-01', tz=buoy_tz) & datetime < as.POSIXct('2018-09-01', tz=buoy_tz))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'august wind 2018') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy_wind_vert_L1,
#               subset=(datetime >= as.POSIXct('2018-09-01', tz=buoy_tz) & datetime < as.POSIXct('2018-10-01', tz=buoy_tz))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'sept wind 2018') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy_wind_vert_L1,
#               subset=(datetime >= as.POSIXct('2018-10-01', tz=buoy_tz) & datetime < as.POSIXct('2018-11-01', tz=buoy_tz))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'oct wind 2018') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# #oct 19 buoy to harbor
# ggplot(subset(buoy_wind_vert_L1,
#               subset=(datetime >= as.POSIXct('2018-10-19', tz=buoy_tz) & datetime < as.POSIXct('2018-10-20', tz=buoy_tz))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'oct wind 2018') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme +
#   scale_color_colorblind()

buoy2018_L1 <- buoy2018_L1 %>% 
  mutate(location = case_when(datetime >= as.POSIXct('2018-10-19 10:30', tz=buoy_tz) ~ 'harbor',
                              TRUE ~ location)) %>% 
  mutate_at(vars(AveWindDir, AveWindSp, MaxWindDir, MaxWindSp),
            ~(case_when(location == 'in transit' ~ NA_real_,
                           TRUE ~ .)))
buoy_wind_vert_L1 <- buoy2018_L1 %>% 
  select(datetime, AveWindDir, AveWindSp, MaxWindDir, MaxWindSp, location) %>% 
  pivot_longer(names_to = 'variable', values_to = 'value', -c(datetime, location))

# ggplot(subset(buoy_wind_vert_L1,
#               subset=(datetime >= as.POSIXct('2018-10-01', tz=buoy_tz) & datetime < as.POSIXct('2018-11-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color = location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'oct wind 2018') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy_wind_vert_L1,
#               subset=(datetime >= as.POSIXct('2018-11-01', tz=buoy_tz) & datetime < as.POSIXct('2018-12-01', tz=buoy_tz))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'nov wind 2018') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# #frozen sensors nov 20-22
# ggplot(subset(buoy_wind_vert_L1,
#               subset=(datetime >= as.POSIXct('2018-11-20', tz=buoy_tz) & datetime < as.POSIXct('2018-11-21', tz=buoy_tz))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'nov wind 2018') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme +
#   scale_color_colorblind()
# ggplot(subset(buoy_wind_vert_L1,
#               subset=(datetime >= as.POSIXct('2018-11-22', tz=buoy_tz) & datetime < as.POSIXct('2018-11-23', tz=buoy_tz))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'nov wind 2018') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme +
#   scale_color_colorblind()
# 
# #frozen sensors nov 26-28
# ggplot(subset(buoy_wind_vert_L1,
#               subset=(datetime >= as.POSIXct('2018-11-26', tz=buoy_tz) & datetime < as.POSIXct('2018-11-27', tz=buoy_tz))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'nov wind 2018') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme +
#   scale_color_colorblind()
# ggplot(subset(buoy_wind_vert_L1,
#               subset=(datetime >= as.POSIXct('2018-11-28', tz=buoy_tz) & datetime < as.POSIXct('2018-11-29', tz=buoy_tz))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'nov wind 2018') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme +
#   scale_color_colorblind()

buoy2018_L1 <- buoy2018_L1 %>% 
  mutate_at(vars(AveWindDir, AveWindSp, MaxWindDir, MaxWindSp),
            ~(case_when(datetime >= as.POSIXct('2018-11-20 7:00', tz=buoy_tz) & 
                          datetime < as.POSIXct('2018-11-22 9:50', tz=buoy_tz)  & MaxWindSp == 0 ~ NA_real_,
                           datetime >= as.POSIXct('2018-11-26 23:10', tz=buoy_tz) & 
                          datetime < as.POSIXct('2018-11-28 13:00', tz=buoy_tz)  & MaxWindSp == 0 ~ NA_real_,
                           TRUE ~ .)))

buoy_wind_vert_L1 <- buoy2018_L1 %>% 
  select(datetime, AveWindDir, AveWindSp, MaxWindDir, MaxWindSp, location) %>% 
  pivot_longer(names_to = 'variable', values_to = 'value', -c(datetime, location))

# ggplot(subset(buoy_wind_vert_L1,
#               subset=(datetime >= as.POSIXct('2018-11-01', tz=buoy_tz) & datetime < as.POSIXct('2018-12-01', tz=buoy_tz))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'nov wind 2018') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy_wind_vert_L1,
#               subset=(datetime >= as.POSIXct('2018-12-01', tz=buoy_tz) & datetime < as.POSIXct('2019-01-01', tz=buoy_tz))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'dec wind 2018') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()

# #sensor frozen dec 28-29
# ggplot(subset(buoy_wind_vert_L1,
#               subset=(datetime >= as.POSIXct('2018-12-28', tz=buoy_tz) & datetime < as.POSIXct('2018-12-29', tz=buoy_tz))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'dec wind 2018') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme +
#   scale_color_colorblind()
# ggplot(subset(buoy_wind_vert_L1,
#               subset=(datetime >= as.POSIXct('2018-12-29', tz=buoy_tz) & datetime < as.POSIXct('2018-12-30', tz=buoy_tz))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'dec wind 2018') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme +
#   scale_color_colorblind()

buoy2018_L1 <- buoy2018_L1 %>% 
  mutate_at(vars(AveWindDir, AveWindSp, MaxWindDir, MaxWindSp),
            ~(case_when(datetime >= as.POSIXct('2018-12-28 13:00', tz=buoy_tz) & 
                          datetime < as.POSIXct('2018-12-29 2:40', tz=buoy_tz)  & MaxWindSp == 0 ~ NA_real_,
                           TRUE ~ .)))

buoy_wind_vert_L1 <- buoy2018_L1 %>% 
  select(datetime, AveWindDir, AveWindSp, MaxWindDir, MaxWindSp, location) %>% 
  pivot_longer(names_to = 'variable', values_to = 'value', -c(datetime, location))

# ggplot(subset(buoy_wind_vert_L1,
#               subset=(datetime >= as.POSIXct('2018-12-01', tz=buoy_tz) & datetime < as.POSIXct('2019-01-01', tz=buoy_tz))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'dec wind 2018') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()

ggplot(buoy_wind_vert_L1,
       aes(x=datetime, y=value, color=location)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title = 'wind 2018, clean') +
  scale_x_datetime(date_minor_breaks = '1 month') +
  final_theme +
  scale_color_colorblind()

#rename with CV
buoy2018_L1 <- buoy2018_L1 %>% 
  rename(windDirectionAverage_deg = AveWindDir,
         windSpeedAverage_mps = AveWindSp,
         windGustDirection_deg = MaxWindDir,
         windGustSpeed_mps = MaxWindSp)

rm(buoy_wind_vert, buoy_wind_vert_L1)

####PAR####
range(buoy2018_L1$PAR, na.rm = T)

#recode when in transit
buoy2018_L1 <-  buoy2018_L1 %>% 
  mutate(PAR = case_when(location == 'in transit' ~ NA_real_,
                         TRUE ~ PAR))

#recode when in transit or offline
buoy2018_L1 <- buoy2018_L1 %>% 
  mutate(flag_par = case_when(PAR < 0 ~ 'z',
                              TRUE ~ '')) %>% 
  mutate(PAR = case_when(PAR < 0 ~ 0,
                         TRUE ~ PAR)) 
ggplot(buoy2018_L1,
       aes(x=datetime, y=PAR, color=location)) +
  geom_point() +
  labs(title = 'PAR 2018, NA recoded') +
  scale_x_datetime(date_minor_breaks = '1 month') +
  final_theme +
  scale_color_colorblind()

# ggplot(subset(buoy2018_L1,
#               subset=(datetime >= as.POSIXct('2018-01-01', tz=buoy_tz) & datetime < as.POSIXct('2018-02-01', tz=buoy_tz))),
#        aes(x=datetime, y=PAR, color=location)) +
#   geom_point() +
#   labs(title = 'PAR 2018, NA recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()

#add flag for likely obscured jan 4; jan 17-19
buoy2018_L1 <- buoy2018_L1 %>%
  mutate(flag_par = case_when(flag_par == '' &
                                datetime >= as.POSIXct('2018-01-04', tz = buoy_tz) &
                                datetime < as.POSIXct('2018-01-05', tz= buoy_tz) ~ 'o',
                              flag_par != '' &
                                datetime >= as.POSIXct('2018-01-04', tz = buoy_tz) &
                                datetime < as.POSIXct('2018-01-05', tz= buoy_tz) ~ paste('o', flag_par, sep = '; '),
                              TRUE ~ flag_par))

# ggplot(subset(buoy2018_L1,
#               subset=(datetime >= as.POSIXct('2018-02-01', tz=buoy_tz) & datetime < as.POSIXct('2018-03-01', tz=buoy_tz))),
#        aes(x=datetime, y=PAR, color=location)) +
#   geom_point() +
#   labs(title = 'PAR 2018, NA recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy2018_L1,
#               subset=(datetime >= as.POSIXct('2018-03-01', tz=buoy_tz) & datetime < as.POSIXct('2018-04-01', tz=buoy_tz))),
#        aes(x=datetime, y=PAR, color=location)) +
#   geom_point() +
#   labs(title = 'PAR 2018, NA recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()

#add flag for likely obscured mar 02; 08-10; 13-16
buoy2018_L1 <- buoy2018_L1 %>%
  mutate(flag_par = case_when(flag_par == '' &
                                datetime >= as.POSIXct('2018-03-02', tz = buoy_tz) &
                                datetime < as.POSIXct('2018-03-03', tz= buoy_tz) ~ 'o',
                              flag_par == '' &
                                datetime >= as.POSIXct('2018-03-08', tz = buoy_tz) &
                                datetime < as.POSIXct('2018-03-10', tz= buoy_tz) ~ 'o',
                              flag_par != '' &
                                datetime >= as.POSIXct('2018-03-13', tz = buoy_tz) &
                                datetime < as.POSIXct('2018-03-16', tz= buoy_tz) ~ paste('o', flag_par, sep = '; '),
                              TRUE ~ flag_par))

# ggplot(subset(buoy2018_L1,
#               subset=(datetime >= as.POSIXct('2018-04-01', tz=buoy_tz) & datetime < as.POSIXct('2018-05-01', tz=buoy_tz))),
#        aes(x=datetime, y=PAR, color=location)) +
#   geom_point() +
#   labs(title = 'PAR 2018, NA recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy2018_L1,
#               subset=(datetime >= as.POSIXct('2018-05-01', tz=buoy_tz) & datetime < as.POSIXct('2018-06-01', tz=buoy_tz))),
#        aes(x=datetime, y=PAR, color=location)) +
#   geom_point() +
#   labs(title = 'PAR 2018, NA recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
#
# ggplot(subset(buoy2018_L1,
#               subset=(datetime >= as.POSIXct('2018-06-01', tz=buoy_tz) & datetime < as.POSIXct('2018-07-01', tz=buoy_tz))),
#        aes(x=datetime, y=PAR, color=location)) +
#   geom_point() +
#   labs(title = 'PAR 2018, NA recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy2018_L1,
#               subset=(datetime >= as.POSIXct('2018-07-01', tz=buoy_tz) & datetime < as.POSIXct('2018-08-01', tz=buoy_tz))),
#        aes(x=datetime, y=PAR, color=location)) +
#   geom_point() +
#   labs(title = 'PAR 2018, NA recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy2018_L1,
#               subset=(datetime >= as.POSIXct('2018-08-01', tz=buoy_tz) & datetime < as.POSIXct('2018-09-01', tz=buoy_tz))),
#        aes(x=datetime, y=PAR, color=location)) +
#   geom_point() +
#   labs(title = 'PAR 2018, NA recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy2018_L1,
#               subset=(datetime >= as.POSIXct('2018-09-01', tz=buoy_tz) & datetime < as.POSIXct('2018-10-01', tz=buoy_tz))),
#        aes(x=datetime, y=PAR, color=location)) +
#   geom_point() +
#   labs(title = 'PAR 2018, NA recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy2018_L1,
#               subset=(datetime >= as.POSIXct('2018-10-01', tz=buoy_tz) & datetime < as.POSIXct('2018-11-01', tz=buoy_tz))),
#        aes(x=datetime, y=PAR, color=location)) +
#   geom_point() +
#   labs(title = 'PAR 2018, NA recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy2018_L1,
#               subset=(datetime >= as.POSIXct('2018-11-01', tz=buoy_tz) & datetime < as.POSIXct('2018-12-01', tz=buoy_tz))),
#        aes(x=datetime, y=PAR, color=location)) +
#   geom_point() +
#   labs(title = 'PAR 2018, NA recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy2018_L1,
#               subset=(datetime >= as.POSIXct('2018-12-01', tz=buoy_tz) & datetime < as.POSIXct('2019-01-01', tz=buoy_tz))),
#        aes(x=datetime, y=PAR, color=location)) +
#   geom_point() +
#   labs(title = 'PAR 2018, NA recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
#
#add flag for nighttime PAR errors begininig Jul 23
buoy2018_L1 <- buoy2018_L1 %>% 
  mutate(flag_par = case_when(flag_par == '' & datetime >= as.POSIXct('2018-07-23', tz= buoy_tz) ~ 'n',
                              flag_par != '' &datetime >= as.POSIXct('2018-07-23', tz= buoy_tz) ~ paste('n', flag_par, sep = '; '),
                              TRUE ~ flag_par)) 

ggplot(buoy2018_L1,
       aes(x=datetime, y=PAR, color = flag_par, shape = location)) +
  geom_point() +
  labs(title = 'PAR 2018, NA recoded') +
  scale_x_datetime(date_minor_breaks = '1 month') +
  final_theme +
  scale_color_colorblind()

#rename with CV
buoy2018_L1 <- buoy2018_L1 %>% 
  rename(radiationIncomingPAR_umolm2s = PAR)

#### Air temp ####
range(buoy2018_L1$AirTempC, na.rm = T)
range(buoy2018_L1$RelHum, na.rm = T)

buoy2018_L1 <- buoy2018_L1 %>% 
  mutate_at(vars(AirTempC, RelHum),
            ~ case_when(location == 'in transit' ~ NA_real_,
                        TRUE ~.))

air_vert <- buoy2018_L1 %>% 
  select(datetime, AirTempC, RelHum, location) %>% 
  pivot_longer(names_to = 'variable', values_to = 'value', -c(datetime, location))

ggplot(air_vert, aes(x=datetime, y = value)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  final_theme +
  labs(title = '2018 air temp raw',
       x= NULL,
       y= 'air temp (deg C)') +
  scale_x_datetime(date_minor_breaks = '1 month')

#no NA values to recode

# ggplot(subset(air_vert,
#               subset=(datetime>=as.POSIXct('2018-01-01', tz=buoy_tz) & datetime<as.POSIXct('2018-02-01', tz=buoy_tz))),
#        aes(x=datetime, value, color = location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'jan 2018 air temp', x=NULL, y='Air temp (deg C)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(air_vert,
#               subset=(datetime>=as.POSIXct('2018-02-01', tz=buoy_tz) & datetime<as.POSIXct('2018-03-01', tz=buoy_tz))),
#        aes(x=datetime, value, color = location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'feb 2018 air temp', x=NULL, y='Air temp (deg C)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(air_vert,
#               subset=(datetime>=as.POSIXct('2018-03-01', tz=buoy_tz) & datetime<as.POSIXct('2018-04-01', tz=buoy_tz))),
#        aes(x=datetime, value, color = location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'mar 2018 air temp', x=NULL, y='Air temp (deg C)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(air_vert,
#               subset=(datetime>=as.POSIXct('2018-04-01', tz=buoy_tz) & datetime<as.POSIXct('2018-05-01', tz=buoy_tz))),
#        aes(x=datetime, value, color = location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'apr 2018 air temp', x=NULL, y='Air temp (deg C)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(air_vert,
#               subset=(datetime>=as.POSIXct('2018-05-01', tz=buoy_tz) & datetime<as.POSIXct('2018-06-01', tz=buoy_tz))),
#        aes(x=datetime, value, color = location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'may 2018 air temp', x=NULL, y='Air temp (deg C)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(air_vert,
#               subset=(datetime>=as.POSIXct('2018-06-01', tz=buoy_tz) & datetime<as.POSIXct('2018-07-01', tz=buoy_tz))),
#        aes(x=datetime, value, color = location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'jun 2018 air temp', x=NULL, y='Air temp (deg C)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(air_vert,
#               subset=(datetime>=as.POSIXct('2018-07-01', tz=buoy_tz) & datetime<as.POSIXct('2018-08-01', tz=buoy_tz))),
#        aes(x=datetime, value, color = location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'jul 2018 air temp', x=NULL, y='Air temp (deg C)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(air_vert,
#               subset=(datetime>=as.POSIXct('2018-08-01', tz=buoy_tz) & datetime<as.POSIXct('2018-09-01', tz=buoy_tz))),
#        aes(x=datetime, value, color = location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'aug 2018 air temp', x=NULL, y='Air temp (deg C)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(air_vert,
#               subset=(datetime>=as.POSIXct('2018-09-01', tz=buoy_tz) & datetime<as.POSIXct('2018-10-01', tz=buoy_tz))),
#        aes(x=datetime, value, color = location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'sept 2018 air temp', x=NULL, y='Air temp (deg C)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(air_vert,
#               subset=(datetime>=as.POSIXct('2018-10-01', tz=buoy_tz) & datetime<as.POSIXct('2018-11-01', tz=buoy_tz))),
#        aes(x=datetime, value, color = location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'oct 2018 air temp', x=NULL, y='Air temp (deg C)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(air_vert,
#               subset=(datetime>=as.POSIXct('2018-11-01', tz=buoy_tz) & datetime<as.POSIXct('2018-12-01', tz=buoy_tz))),
#        aes(x=datetime, value, color = location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'nov 2018 air temp', x=NULL, y='Air temp (deg C)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(air_vert,
#               subset=(datetime>=as.POSIXct('2018-12-01', tz=buoy_tz) & datetime<as.POSIXct('2019-01-01', tz=buoy_tz))),
#        aes(x=datetime, value, color = location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'dec 2018 air temp', x=NULL, y='Air temp (deg C)') +
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
buoy2018_L1 <- buoy2018_L1 %>% 
  mutate(flag_rh = case_when(RelHum >100 ~ 's',
                             TRUE ~ ''))

#rename with CV
buoy2018_L1 <- buoy2018_L1 %>% 
  rename(relativeHumidity_perc = RelHum,
         airTemperature_degC = AirTempC)

rm(air_vert)

#### EXPORT L1 DATA STREAMS ####

colnames(buoy2018_L1)

#export L1 tempstring file
buoy2018_L1 %>%
  select(datetime, location, waterTemperature_degC_0p85m:waterTemperature_degC_9p85m, flag_temp0p85m) %>%
  mutate(datetime = as.character(datetime)) %>%
  write_csv(., file.path(dump_dir,'tempstring/2018_tempstring_L1_v2022.csv'))

# export L1 do file
buoy2018_L1 %>%
  select(datetime, location, 
         oxygenDissolved_mgl_10p5m, oxygenDissolvedPercentOfSaturation_pct_10p5m, waterTemperature_DO_degC_10p5m, 
         flag_do10p5m) %>%
  mutate(datetime = as.character(datetime)) %>%
  write_csv(., file.path(dump_dir, 'do/2018_do_L1_v2022.csv'))

#export L1 met file
buoy2018_L1 %>%
  select(datetime, location, 
         windSpeedAverage_mps:windGustDirection_deg, flag_allwind,
         radiationIncomingPAR_umolm2s, flag_par,
         airTemperature_degC, relativeHumidity_perc, flag_rh) %>%
  mutate(datetime = as.character(datetime)) %>%
  write_csv(., file.path(dump_dir, 'met/2018_met_L1_v2022.csv'))


