#*****************************************************************
#*      Cary Institute of Ecosystem Studies (Millbrook, NY)      *
#*                                                               *
#* TITLE:   Sunapee_buoy_2019.r                                  *
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
buoy2019_L0 <- read.csv(file.path(raw_dir, '2019 Buoy Data.csv'))

#### format data ####
buoy2019_L0 <- buoy2019_L0  %>%
  rename(DOLowTempC = 'DOLoTempC',
         AveWindSp = 'WindSpdAv',
         AveWindDir = 'WindVect',
         MaxWindSp = 'MaxWind') %>% 
  mutate(hour = Hr.Min%/%100,
         minutes = Hr.Min%%100,
         time = paste(hour, minutes, sep=':')) %>% #break out time from Hr.Min, create time column
  mutate(date = as.Date(paste(Day, Year, sep = '-'), format='%j-%Y'), #create date in ymd format
         datetime = as.POSIXct(paste(date, time, sep=' '), format='%Y-%m-%d %H:%M', tz=buoy_tz)) %>%  
  select(-hour, -minutes, -Hr.Min, -Year, -Day, -time, -ArrayID) %>% #remove unnecessary columns
  rownames_to_column(var ='rowid')

#plot the battery levels
ggplot(buoy2019_L0, aes(x = datetime, y = LoggerBatV)) +
  geom_point()

ggplot(buoy2019_L0, aes(x = datetime, y = RadioBatV)) +
  geom_point()
# looks fine 

#double check to make sure there are no DST issues
datelength2019 <- buoy2019_L0 %>% 
  mutate(date = format(datetime, '%Y-%m-%d')) %>% 
  group_by(date) %>% 
  summarize(length(datetime))

#check dst dates
datelength2019[datelength2019$date == '2019-03-10',]
#dst observed
datelength2019[datelength2019$date == '2019-11-03',]
#dst observed on the 4th
datelength2019[datelength2019$date == '2019-11-04',]


# see where dst occurs
buoy2019_L0 %>% 
  filter(datetime >= as.POSIXct('2019-03-10', tz=buoy_tz) & 
           datetime < as.POSIXct('2019-03-11', tz=buoy_tz)) %>% 
  select(datetime, rowid)

buoy2019_L0 %>% 
  filter(datetime >= as.POSIXct('2019-11-04', tz=buoy_tz) & 
           datetime < as.POSIXct('2019-11-05', tz=buoy_tz)) %>% 
  select(datetime, rowid)


#DST observed at odd times - 03-08-15 23:00; 11-02-15 00:00
buoy2019_L1 <- buoy2019_L0 %>% 
  mutate(datetime.instrument = datetime,
         rowid = as.numeric(rowid))
buoy2019_L1a <- buoy2019_L1 %>% 
  filter(rowid <= 9927)
buoy2019_L1b <- buoy2019_L1 %>% 
  filter(rowid > 9927 & rowid <= 44195)
buoy2019_L1c <- buoy2019_L1 %>% 
  filter(rowid > 44195)
#apply time math on middle section
buoy2019_L1b <- buoy2019_L1b %>% 
  mutate(datetime = datetime-hours(1))

#rejoin and force tz as buoytz
buoy2019_L1 <- full_join(buoy2019_L1a, buoy2019_L1b) %>% 
  full_join(., buoy2019_L1c) %>% 
  mutate(datetime = as.POSIXct(datetime, tz = buoy_tz))

#double check to make sure there are no DST issues
datelength2019 <- buoy2019_L1 %>% 
  mutate(date = format(datetime, '%Y-%m-%d')) %>% 
  group_by(date) %>% 
  summarize(length(datetime))

datelength2019[datelength2019$date == '2019-03-10',]
datelength2019[datelength2019$date == '2019-11-03',]
datelength2019[datelength2019$date == '2019-11-04',]
#all good!

# add in all date time options in L1 data set
alltimes_2019 <- as.data.frame(seq.POSIXt(as.POSIXct('2019-01-01 00:00', tz=buoy_tz), as.POSIXct('2019-12-31 23:50', tz=buoy_tz), '10 min')) %>% 
  rename("datetime" = !!names(.[1]))

buoy2019_L1 <- buoy2019_L1 %>% 
  right_join(., alltimes_2019) %>% 
  arrange(datetime)

#clean up workspace
rm(alltimes_2019, datelength2019, buoy2019_L1a, buoy2019_L1b, buoy2019_L1c)

####THERMISTORS####
buoy2019_therm_vert <- buoy2019_L1 %>% 
  select(datetime, all_of(alltemp2011)) %>%
  pivot_longer(names_to = 'variable', values_to = 'value', -datetime) %>% 
  mutate(variable = factor(variable, levels=alltemp))

# #plot to see
# ggplot(buoy2019_therm_vert, aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 month') +
#   final_theme

#recode NA strings of -6999
buoy2019_L1 <- buoy2019_L1 %>% 
  mutate_at(vars(all_of(alltemp2011)),
            ~case_when(. == -6999 ~ NA_real_,
                           TRUE ~ .))
buoy2019_therm_vert <- buoy2019_L1 %>% 
  select(datetime, all_of(alltemp2011)) %>%
  pivot_longer(names_to = 'variable', values_to = 'value', -datetime) %>% 
  mutate(variable = factor(variable, levels=alltemp))

# #plot together to see relative values
# ggplot(buoy2019_therm_vert,
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 month') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme

#look at 0m only 
ggplot(buoy2019_L1, aes(x = datetime, y = TempC_0m)) +
  geom_point()
#probably some usable data in there; see if we can get it out

buoy2019_L1 <- buoy2019_L1 %>% 
  mutate(TempC_0m = case_when(TempC_0m >30 ~ NA_real_,
                              TempC_0m < 10 ~ NA_real_, 
                              TempC_0m == 15 ~ NA_real_,
                              TRUE ~ TempC_0m))

ggplot(buoy2019_L1, aes(x = datetime, y = TempC_0m)) +
  geom_point()

#add intermittent flag for 0m data
buoy2019_L1 <- buoy2019_L1 %>% 
  mutate(flag_temp0p75m = 'i')

buoy2019_therm_vert <- buoy2019_L1 %>% 
  select(datetime, all_of(alltemp2011)) %>%
  pivot_longer(names_to = 'variable', values_to = 'value', -datetime) %>% 
  mutate(variable = factor(variable, levels=alltemp))

# #plot together to see relative values
# ggplot(buoy2019_therm_vert,
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 month') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme

#recode data prior to May 1 (predeployment testing)
buoy2019_L1 <- buoy2019_L1 %>% 
  mutate_at(vars(alltemp2011),
            ~ case_when(datetime < as.POSIXct('2019-05-01', tz=buoy_tz) ~ NA_real_,
                        TRUE ~ .))
buoy2019_therm_vert <- buoy2019_L1 %>% 
  select(datetime, all_of(alltemp2011)) %>%
  pivot_longer(names_to = 'variable', values_to = 'value', -datetime) %>% 
  mutate(variable = factor(variable, levels=alltemp))

#plot together to see relative values, without pre-deployment blip
ggplot(buoy2019_therm_vert,
       aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  scale_x_datetime(date_minor_breaks = '1 month') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  final_theme

# plot by 2-week period, save plot.

# set time period of interest:
start_date = '2019-05-15'
end_date = '2019-11-15'

#create a list of weeks during time period of interest
biweekly_2019 <- seq(as.Date(start_date), as.Date(end_date), '2 weeks')  %>% 
  as.data.frame(.) %>% 
  dplyr::rename(date = '.') 

# #plot data in 2-week iterations
# for(i in 1:(nrow(biweekly_2019)-1)) {
#   chunk <- buoy2019_L1 %>% 
#     filter(datetime >= as.POSIXct(biweekly_2019$date[i], tz= buoy_tz) &
#              datetime < as.POSIXct(biweekly_2019$date[i+1], tz=buoy_tz))
#   chunk_vert <- chunk %>% 
#     select(datetime, all_of(alltemp2011)) %>%
#     gather(variable, value, -datetime)
#   ggplot(chunk_vert, aes(x = datetime, y = value, color = variable)) +
#     geom_point() +
#     scale_x_datetime(date_minor_breaks = '1 day') +
#     scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                                 "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#     final_theme +
#     labs(x = NULL,
#          y = 'water temperature (degrees C)',
#          title = paste0('LSPA thermistors ', biweekly_2019$date[i], ' - ', biweekly_2019$date[i+1]))
#   ggsave(filename = paste0('graphs/2019/L0_therm_biweekly_', biweekly_2019$date[i], '.jpg'), height = 6, width =8, units = 'in', dpi = 300)
# }

#data look great - need to check deployment May 23 and removal, Nov 5

#buoy deployment
deployment = '2019-05-23'
# ggplot(subset(buoy2019_therm_vert,
#               subset=(datetime >= as.POSIXct(deployment, tz=buoy_tz) & datetime < (as.POSIXct(deployment, tz=buoy_tz) + days(1)))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme
#just add location information
buoy2019_L1 <- buoy2019_L1 %>% 
  mutate(location = case_when(datetime >= as.POSIXct('2019-05-23 09:30', tz=buoy_tz) ~ 'loon',
                              datetime <as.POSIXct('2019-05-23 09:30', tz=buoy_tz) ~ 'harbor',
                              TRUE ~ ''))
buoy2019_therm_vert_L1 <- buoy2019_L1 %>% 
  select(datetime, all_of(alltemp2011), location) %>%
  gather(variable, value, -datetime, -location)

#buoy removal 
removal = '2019-11-05'
# ggplot(subset(buoy2019_therm_vert,
#               subset=(datetime >= as.POSIXct(removal, tz=buoy_tz) & datetime < (as.POSIXct(removal, tz=buoy_tz) + days(1)))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme
# 
#recode data after 10:30 (inclusive), update location to in-transit. loon will be added by do string
buoy2019_L1 <- buoy2019_L1 %>% 
  mutate_at(vars(alltemp2011),
            ~ case_when(datetime >= as.POSIXct('2019-11-05 10:30', tz=buoy_tz) ~ NA_real_,
                        TRUE ~ .)) %>% 
  mutate(location = case_when(datetime >= as.POSIXct('2019-11-05 10:30', tz=buoy_tz) ~ 'in transit',
                              TRUE ~ location))
buoy2019_therm_vert_L1 <- buoy2019_L1 %>% 
  select(datetime, alltemp2011, location) %>%
  gather(variable, value, -datetime, -location)

# ggplot(subset(buoy2019_therm_vert_L1,
#               subset=(datetime >= as.POSIXct(removal, tz=buoy_tz) & datetime < (as.POSIXct(removal, tz=buoy_tz) + days(1)))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme
# 
ggplot(buoy2019_therm_vert_L1,
       aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  scale_x_datetime(date_minor_breaks = '1 month') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  final_theme


#correct column names for sensor offset
buoy2019_L1 <- buoy2019_L1 %>% 
  rename(waterTemperature_degC_9p75m = TempC_9m,
         waterTemperature_degC_8p75m = TempC_8m,
         waterTemperature_degC_7p75m = TempC_7m,
         waterTemperature_degC_6p75m = TempC_6m,
         waterTemperature_degC_5p75m = TempC_5m,
         waterTemperature_degC_4p75m = TempC_4m,
         waterTemperature_degC_3p75m = TempC_3m,
         waterTemperature_degC_2p75m = TempC_2m,
         waterTemperature_degC_1p75m = TempC_1m,
         waterTemperature_degC_0p75m = TempC_0m)

#clean up workspace
rm(buoy2019_therm_vert, buoy2019_therm_vert_L1)

#### DO ####
buoy2019_do_vert <- buoy2019_L1 %>% 
  select(datetime, all_of(upDO), all_of(lowDO), location) %>%
  gather(variable, value, -datetime, -location)

# ggplot(buoy2019_do_vert, aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 month') +
#   final_theme

#recode NA values and data prior to buoy move to loon as well as those data after the move from loon back to harbor in dec
buoy2019_L1 <- buoy2019_L1 %>% 
  mutate_at(vars(all_of(upDO), all_of(lowDO)),
            ~(case_when(. == -6999 ~ NA_real_,
                           location == 'harbor' ~ NA_real_,
                           location == 'in transit' ~ NA_real_,
                           TRUE ~ .))) 

buoy2019_do_vert_L1 <- buoy2019_L1 %>%
  select(datetime, all_of(upDO), all_of(lowDO)) %>%
  gather(variable, value, -datetime)

ggplot(buoy2019_do_vert_L1, aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  scale_x_datetime(date_minor_breaks = '1 month') +
  final_theme

# #look at the DO data on a biweekly basis
# for(i in 1:(nrow(biweekly_2019)-1)) {
#   chunk <- buoy2019_L1 %>% 
#     filter(datetime >= as.POSIXct(biweekly_2019$date[i], tz= buoy_tz) &
#              datetime < as.POSIXct(biweekly_2019$date[i+1], tz=buoy_tz))
#   chunk_vert <- chunk %>% 
#     select(datetime, all_of(upDO), all_of(lowDO)) %>%
#     gather(variable, value, -datetime)
#   ggplot(chunk_vert, aes(x = datetime, y = value)) +
#     geom_point() +
#     facet_grid(variable ~ ., scales = 'free_y') +
#     scale_x_datetime(date_minor_breaks = '1 day') +
#     scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                                 "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#     final_theme +
#     labs(x = NULL,
#          y = NULL,
#          title = paste0('LSPA DO sensors ', biweekly_2019$date[i], ' - ', biweekly_2019$date[i+1]))
#   ggsave(filename = paste0('graphs/2019/L0_do_biweekly_', biweekly_2019$date[i], '.jpg'), height = 6, width =8, units = 'in', dpi = 300)
# }

# # May 23
# #buoy deployment 
# ggplot(subset(buoy2019_do_vert_L1,
#               subset=(datetime >= as.POSIXct(deployment, tz=buoy_tz) & datetime < (as.POSIXct(deployment, tz=buoy_tz) + days(1)))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme

#just one datetime that needs to be recoded
buoy2019_L1 <- buoy2019_L1 %>% 
mutate_at(vars(all_of(lowDO), all_of(upDO)),
          ~(case_when(datetime == as.POSIXct('2019-05-23 9:30', tz=buoy_tz) ~ NA_real_,
                         TRUE ~ .))) %>% 
  mutate(flag_do0p25m= case_when(datetime == as.POSIXct('2019-05-23 9:30', tz=buoy_tz) ~ 'c',
                        TRUE ~ ''),
         flag_do10m= case_when(datetime == as.POSIXct('2019-05-23 9:30', tz=buoy_tz) ~ 'c',
                                   TRUE ~ ''))

buoy2019_do_vert_L1 <- buoy2019_L1 %>% 
  select(datetime, all_of(lowDO), all_of(upDO)) %>%
  gather(variable, value, -datetime)

# ggplot(subset(buoy2019_do_vert_L1,
#               subset=(datetime >= as.POSIXct(deployment, tz=buoy_tz) & datetime < (as.POSIXct(deployment, tz=buoy_tz) + days(1)))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme

# Jun 17
date_look = '2019-06-17'
# ggplot(subset(buoy2019_do_vert_L1,
#               subset=(datetime >= as.POSIXct(date_look, tz=buoy_tz) & datetime < (as.POSIXct(date_look, tz=buoy_tz) + days(1)))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme

#just one datetime that needs to be recoded in low do
buoy2019_L1 <- buoy2019_L1 %>% 
  mutate_at(vars(all_of(lowDO)),
            ~(case_when(datetime == as.POSIXct('2019-06-17 9:10', tz=buoy_tz) ~ NA_real_,
                           TRUE ~ .)))

buoy2019_do_vert_L1 <- buoy2019_L1 %>% 
  select(datetime, all_of(upDO), all_of(lowDO)) %>%
  gather(variable, value, -datetime)

# ggplot(subset(buoy2019_do_vert_L1,
#               subset=(datetime >= as.POSIXct(date_look, tz=buoy_tz) & datetime < (as.POSIXct(date_look, tz=buoy_tz) + days(1)))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme

#Jul13
date_look = '2019-07-13'
# ggplot(subset(buoy2019_do_vert_L1,
#               subset=(datetime >= as.POSIXct(date_look, tz=buoy_tz) & datetime < (as.POSIXct(date_look, tz=buoy_tz) + days(1)))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme

#just one datetime that needs to be recoded in low do
buoy2019_L1 <- buoy2019_L1 %>% 
  mutate_at(vars(all_of(lowDO)),
            ~(case_when(datetime == as.POSIXct('2019-07-13 11:50', tz=buoy_tz) ~ NA_real_,
                           TRUE ~ .)))

buoy2019_do_vert_L1 <- buoy2019_L1 %>% 
  select(datetime, all_of(upDO), all_of(lowDO)) %>%
  gather(variable, value, -datetime)

# ggplot(subset(buoy2019_do_vert_L1,
#               subset=(datetime >= as.POSIXct(date_look, tz=buoy_tz) & datetime < (as.POSIXct(date_look, tz=buoy_tz) + days(1)))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme

#Jul24pm
date_look = '2019-07-24'
# ggplot(subset(buoy2019_do_vert_L1,
#               subset=(datetime >= as.POSIXct(paste(date_look, '12:00', sep = ' '), tz=buoy_tz) & datetime < (as.POSIXct(paste(date_look, '12:00', sep = ' '), tz=buoy_tz) + days(1)))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme

#This actually looks okay, eve if a little odd. Leaving as-is.

#aug 15
date_look = '2019-08-15'
# ggplot(subset(buoy2019_do_vert_L1,
#               subset=(datetime >= as.POSIXct(date_look, tz=buoy_tz) & datetime < (as.POSIXct(date_look, tz=buoy_tz) + days(1)))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme

#just two rows need to be recoded
buoy2019_L1 <- buoy2019_L1 %>% 
  mutate_at(vars(all_of(lowDO)),
            ~(case_when(datetime >= as.POSIXct('2019-08-15 19:20', tz=buoy_tz) & 
                             datetime < as.POSIXct('2019-08-15 19:40', tz=buoy_tz) ~ NA_real_,
                           TRUE ~ .)))

buoy2019_do_vert_L1 <- buoy2019_L1 %>% 
  select(datetime, all_of(upDO), all_of(lowDO)) %>%
  gather(variable, value, -datetime)

# ggplot(subset(buoy2019_do_vert_L1,
#               subset=(datetime >= as.POSIXct(date_look, tz=buoy_tz) & datetime < (as.POSIXct(date_look, tz=buoy_tz) + days(1)))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme

#aug19
date_look = '2019-08-19'
# ggplot(subset(buoy2019_do_vert_L1,
#               subset=(datetime >= as.POSIXct(date_look, tz=buoy_tz) & datetime < (as.POSIXct(date_look, tz=buoy_tz) + days(1)))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme

#just one datetime that needs to be recoded in low do
buoy2019_L1 <- buoy2019_L1 %>% 
  mutate_at(vars(all_of(lowDO)),
            ~(case_when(datetime == as.POSIXct('2019-08-19 8:10', tz=buoy_tz) ~ NA_real_,
                           TRUE ~ .)))

buoy2019_do_vert_L1 <- buoy2019_L1 %>% 
  select(datetime, all_of(upDO), all_of(lowDO)) %>%
  gather(variable, value, -datetime)

# ggplot(subset(buoy2019_do_vert_L1,
#               subset=(datetime >= as.POSIXct(date_look, tz=buoy_tz) & datetime < (as.POSIXct(date_look, tz=buoy_tz) + days(1)))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme

#sep24
date_look = '2019-09-24'
# ggplot(subset(buoy2019_do_vert_L1,
#               subset=(datetime >= as.POSIXct(date_look, tz=buoy_tz) & datetime < (as.POSIXct(date_look, tz=buoy_tz) + days(1)))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme

#just one datetime that needs to be recoded in low do
buoy2019_L1 <- buoy2019_L1 %>% 
  mutate_at(vars(all_of(lowDO)),
            ~(case_when(datetime == as.POSIXct('2019-09-24 12:00', tz=buoy_tz) ~ NA_real_,
                           TRUE ~ .)))

buoy2019_do_vert_L1 <- buoy2019_L1 %>% 
  select(datetime, all_of(upDO), all_of(lowDO)) %>%
  gather(variable, value, -datetime)

# ggplot(subset(buoy2019_do_vert_L1,
#               subset=(datetime >= as.POSIXct(date_look, tz=buoy_tz) & datetime < (as.POSIXct(date_look, tz=buoy_tz) + days(1)))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme

#oct11
date_look = '2019-10-11'
# ggplot(subset(buoy2019_do_vert_L1,
#               subset=(datetime >= as.POSIXct(date_look, tz=buoy_tz) & datetime < (as.POSIXct(date_look, tz=buoy_tz) + days(1)))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme

#just one datetime that needs to be recoded in low do
buoy2019_L1 <- buoy2019_L1 %>% 
  mutate_at(vars(all_of(lowDO)),
            ~(case_when(datetime == as.POSIXct('2019-10-11 08:40', tz=buoy_tz) ~ NA_real_,
                           TRUE ~ .)))

buoy2019_do_vert_L1 <- buoy2019_L1 %>% 
  select(datetime, all_of(upDO), all_of(lowDO)) %>%
  gather(variable, value, -datetime)

# ggplot(subset(buoy2019_do_vert_L1,
#               subset=(datetime >= as.POSIXct(date_look, tz=buoy_tz) & datetime < (as.POSIXct(date_look, tz=buoy_tz) + days(1)))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme

#nov 5
date_look = '2019-11-05'
# ggplot(subset(buoy2019_do_vert_L1,
#               subset=(datetime >= as.POSIXct(date_look, tz=buoy_tz) & datetime < (as.POSIXct(date_look, tz=buoy_tz) + days(1)))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme

#just one datetime that needs to be recoded in low do
buoy2019_L1 <- buoy2019_L1 %>% 
  mutate_at(vars(all_of(lowDO), all_of(upDO)),
            ~(case_when(datetime >= as.POSIXct('2019-11-05 10:30', tz=buoy_tz)  ~ NA_real_,
                           TRUE ~ .)))

buoy2019_do_vert_L1 <- buoy2019_L1 %>% 
  select(datetime, all_of(upDO), all_of(lowDO)) %>%
  gather(variable, value, -datetime)

# ggplot(subset(buoy2019_do_vert_L1,
#               subset=(datetime >= as.POSIXct(date_look, tz=buoy_tz) & datetime < (as.POSIXct(date_look, tz=buoy_tz) + days(1)))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme


ggplot(buoy2019_do_vert_L1, aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  scale_x_datetime(date_minor_breaks = '1 month') +
  final_theme

#clean up workspace
rm(buoy2020_do_vert, buoy2020_do_vert_L1, docheckppm, dochecksat)

#rename with CV
buoy2019_L1 <- buoy2019_L1 %>% 
  rename(oxygenDissolved_mgl_0p25m = DOppm,
         oxygenDissolvedPercentOfSaturation_pct_0p25m = DOSat,
         waterTemperature_DO_degC_0p25m = DOTempC,
         oxygenDissolved_mgl_10m = DOLowPPM,
         oxygenDissolvedPercentOfSaturation_pct_10m = DOLowSat,
         waterTemperature_DO_degC_10m = DOLowTempC)


# ####CHLA####
# buoy2019_chla_vert <- buoy2019_L1 %>%
#   select(datetime, all_of(chla)) %>%
#   gather(variable, value, -datetime)
# 
# ggplot(buoy2019_chla_vert, aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 month') +
#   final_theme

#chla sensor not working
buoy2019_L1 <- buoy2019_L1 %>% 
  mutate(Chlor_RFU = NA_real_,
         Chlor_UGL = NA_real_,
         SpecCond = NA_real_)

#clean up workspace
rm(buoy2019_chla_vert)

####wind####
buoy_wind_vert <- buoy2019_L1 %>%
  select(datetime, all_of(wind)) %>%
  gather(variable, value, -datetime)

# ggplot(buoy_wind_vert,
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'wind 2019, raw') +
#   scale_x_datetime(date_minor_breaks = '1 month') +
#   final_theme +
#   scale_color_colorblind()

# set time period of interest:
start_date = '2019-01-01'
end_date = '2020-01-01'

#create a list of months during time period of interest
monthly_2019 <- seq(as.Date(start_date), as.Date(end_date), 'month')  %>% 
  as.data.frame(.) %>% 
  dplyr::rename(date = '.') %>% 
  filter(date< as.Date('2019-12-31')) %>% 
  add_row(date = as.Date('2020-01-01'))
# 
# #plot data in monthly iterations
# for(i in 1:(nrow(monthly_2019)-1)) {
#   chunk <- buoy2019_L1 %>% 
#     filter(datetime >= as.POSIXct(monthly_2019$date[i], tz= buoy_tz) &
#              datetime < as.POSIXct(monthly_2019$date[i+1], tz=buoy_tz))
#   chunk_vert <- chunk %>% 
#     select(datetime, all_of(wind)) %>%
#     gather(variable, value, -datetime)
#   ggplot(chunk_vert, aes(x = datetime, y = value, color = variable)) +
#     geom_point() +
#     scale_x_datetime(date_minor_breaks = '1 day') +
#     facet_grid(variable ~ ., scales = 'free_y') +
#     scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                                 "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#     final_theme +
#     labs(x = NULL,
#          y = NULL,
#          title = paste0('LSPA wind variables ', monthly_2019$date[i], ' - ', monthly_2019$date[i+1]))
#   ggsave(filename = paste0('graphs/2019/L0_wind_monthly_', monthly_2019$date[i], '.jpg'), height = 8, width =10, units = 'in', dpi = 300)
# }

#sensor frozen jan 9-14
date_look_start <- '2019-01-09'
date_look_end <- '2019-01-14'

# ggplot(subset(buoy_wind_vert,
#               subset=(datetime >= as.POSIXct(date_look_start, tz=buoy_tz) & datetime < (as.POSIXct(date_look_start, tz=buoy_tz) + days(1)))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'jan wind 2019, NAs recoded') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme +
#   scale_color_colorblind()

# ggplot(subset(buoy_wind_vert,
#               subset=(datetime >= (as.POSIXct(date_look_end, tz=buoy_tz) - days(1)) & datetime < as.POSIXct(date_look_end, tz=buoy_tz))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'jan wind 2019, NAs recoded') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme +
#   scale_color_colorblind()

buoy2019_L1 <- buoy2019_L1 %>% 
  mutate_at(vars(all_of(wind)),
            ~(case_when(datetime >= as.POSIXct('2019-01-09 5:10', tz=buoy_tz) & 
                          datetime < as.POSIXct('2019-01-13 12:30', tz=buoy_tz) & MaxWindSp == 0 ~ NA_real_,
                           TRUE ~ .)))

buoy_wind_vert_L1 <- buoy2019_L1 %>% 
  select(datetime, all_of(wind)) %>% 
  gather(variable, value, -datetime)

# ggplot(subset(buoy_wind_vert_L1,
#               subset=(datetime >= as.POSIXct(date_look_start, tz=buoy_tz) & datetime < (as.POSIXct(date_look_end, tz=buoy_tz)))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'jan wind 2019, NAs recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()

#Feb 29
date_look <- '2019-02-28'

# ggplot(subset(buoy_wind_vert,
#               subset=(datetime >= as.POSIXct(date_look, tz=buoy_tz) & datetime < (as.POSIXct(date_look, tz=buoy_tz) +days(1)))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme +
#   scale_color_colorblind()

buoy2019_L1 <- buoy2019_L1 %>% 
  mutate_at(vars(all_of(wind)),
            ~(case_when(datetime >= as.POSIXct('2019-02-28 4:30', tz=buoy_tz) & 
                          datetime < as.POSIXct('2019-02-28 10:20', tz=buoy_tz) & MaxWindSp ==0 ~ NA_real_,
                           TRUE ~ .)))

buoy_wind_vert_L1 <- buoy2019_L1 %>% 
  select(datetime, all_of(wind)) %>% 
  gather(variable, value, -datetime)

# ggplot(subset(buoy_wind_vert_L1,
#               subset=(datetime >= as.POSIXct(date_look, tz=buoy_tz) & datetime < (as.POSIXct(date_look, tz=buoy_tz) +days(1)))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme +
#   scale_color_colorblind()


#mar04
date_look <- '2019-03-04'

# ggplot(subset(buoy_wind_vert,
#               subset=(datetime >= as.POSIXct(date_look, tz=buoy_tz) & datetime < (as.POSIXct(date_look, tz=buoy_tz) +days(1)))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme +
#   scale_color_colorblind()

buoy2019_L1 <- buoy2019_L1 %>% 
  mutate_at(vars(all_of(wind)),
            ~(case_when(datetime >= as.POSIXct('2019-03-04 5:20', tz=buoy_tz) & datetime < as.POSIXct('2019-03-04 9:00', tz=buoy_tz) & MaxWindSp ==0 ~ NA_real_,
                           TRUE ~ .)))

buoy_wind_vert_L1 <- buoy2019_L1 %>% 
  select(datetime, all_of(wind)) %>% 
  gather(variable, value, -datetime)

# ggplot(subset(buoy_wind_vert_L1,
#               subset=(datetime >= as.POSIXct(date_look, tz=buoy_tz) & datetime < (as.POSIXct(date_look, tz=buoy_tz) +days(1)))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme +
#   scale_color_colorblind()

# # buoy deployment
# ggplot(subset(buoy_wind_vert_L1,
#               subset=(datetime >= as.POSIXct(deployment, tz=buoy_tz) & datetime < (as.POSIXct(deployment, tz=buoy_tz) +days(1)))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme +
#   scale_color_colorblind()

buoy2019_L1 <- buoy2019_L1 %>% 
  mutate(location = case_when(datetime < as.POSIXct('2019-05-23 7:20', tz=buoy_tz) ~ 'harbor',
                              datetime >= as.POSIXct('2019-05-23 7:20', tz=buoy_tz) & datetime < as.POSIXct('2019-05-23 9:30', tz=buoy_tz) ~ 'in transit',
                              TRUE ~ location)) 
buoy_wind_vert_L1 <- buoy2019_L1 %>% 
  select(datetime, all_of(wind), location) %>% 
  gather(variable, value, -datetime, -location)
# 
# ggplot(subset(buoy_wind_vert_L1,
#               subset=(datetime >= as.POSIXct(deployment, tz=buoy_tz) & datetime < (as.POSIXct(deployment, tz=buoy_tz) +days(1)))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme +
#   scale_color_colorblind()

#removal
# ggplot(subset(buoy_wind_vert_L1,
#               subset=(datetime >= as.POSIXct(removal, tz=buoy_tz) & datetime < (as.POSIXct(removal, tz=buoy_tz) +days(1)))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme +
#   scale_color_colorblind()

buoy2019_L1 <- buoy2019_L1 %>% 
  mutate(location = case_when(datetime >= as.POSIXct('2019-11-05 10:30', tz=buoy_tz) & datetime < as.POSIXct('2019-11-05 12:00', tz=buoy_tz) ~ 'in transit',
                              datetime >= as.POSIXct('2019-11-05 12:00', tz=buoy_tz) ~ 'harbor',
                              TRUE ~ location)) %>% 
  mutate_at(vars(all_of(wind)),
            ~(case_when(location == 'in transit' ~ NA_real_,
                           TRUE ~ .)))
buoy_wind_vert_L1 <- buoy2019_L1 %>% 
  select(datetime, all_of(wind), location) %>% 
  gather(variable, value, -datetime, -location)

# ggplot(subset(buoy_wind_vert_L1,
#               subset=(datetime >= as.POSIXct(removal, tz=buoy_tz) & datetime < (as.POSIXct(removal, tz=buoy_tz) +days(1)))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme +
#   scale_color_colorblind()


#Dec 30-31
date_look_start <- '2019-12-30'
date_look_end <- '2020-01-01'

# ggplot(subset(buoy_wind_vert,
#               subset=(datetime >= as.POSIXct(date_look_start, tz=buoy_tz) & datetime < (as.POSIXct(date_look_start, tz=buoy_tz) + days(1)))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'jan wind 2019, NAs recoded') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy_wind_vert,
#               subset=(datetime >= (as.POSIXct(date_look_end, tz=buoy_tz) - days(1)) & datetime < as.POSIXct(date_look_end, tz=buoy_tz))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'jan wind 2019, NAs recoded') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme +
#   scale_color_colorblind()
# 
buoy2019_L1 <- buoy2019_L1 %>% 
  mutate_at(vars(all_of(wind)),
            ~(case_when(datetime >= as.POSIXct('2019-12-30 06:00', tz=buoy_tz) & datetime < as.POSIXct('2019-12-31 08:00', tz=buoy_tz) & MaxWindSp == 0 ~ NA_real_,
                           TRUE ~ .)))

buoy_wind_vert_L1 <- buoy2019_L1 %>% 
  select(datetime, all_of(wind)) %>% 
  gather(variable, value, -datetime)

# ggplot(subset(buoy_wind_vert_L1,
#               subset=(datetime >= as.POSIXct(date_look_start, tz=buoy_tz) & datetime < (as.POSIXct(date_look_end, tz=buoy_tz)))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'wind 2019, NAs recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()
# 
#rename with CV
buoy2019_L1 <- buoy2019_L1 %>% 
  rename(windDirectionAverage_deg = AveWindDir,
         windSpeedAverage_mps = AveWindSp,
         windGustDirection_deg = MaxWindDir,
         windGustSpeed_mps = MaxWindSp)

#Clean up workspace
rm(buoy_wind_vert, buoy_wind_vert_L1)

# ###PAR####
#recode when in transit or offline and recode less than zero to 0
buoy2019_L1 <- buoy2019_L1 %>% 
  mutate(flag_par = case_when(PAR < 0 ~ 'z',
                              TRUE ~ '')) %>% 
  mutate(PAR = case_when(location == 'in transit' ~ NA_real_,
                         PAR < 0 ~ 0,
                         TRUE ~ PAR)) 

for(i in 1:(nrow(monthly_2019)-1)) {
  chunk <- buoy2019_L1 %>%
    filter(datetime >= as.POSIXct(monthly_2019$date[i], tz= buoy_tz) &
             datetime < as.POSIXct(monthly_2019$date[i+1], tz=buoy_tz))
  chunk_vert <- chunk %>%
    select(datetime, PAR)
  ggplot(chunk_vert, aes(x = datetime, y = PAR)) +
    geom_point() +
    scale_x_datetime(date_minor_breaks = '1 day') +
    scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                                "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
    final_theme +
    labs(x = NULL,
         y = NULL,
         title = paste0('LSPA PAR ', monthly_2019$date[i], ' - ', monthly_2019$date[i+1]))
  ggsave(filename = paste0('graphs/2019/L0_par_monthly_', monthly_2019$date[i], '.jpg'), height = 3, width =8, units = 'in', dpi = 300)
}

#add flag for night time issues; par likely obscured jan 9-20, add flag
buoy2019_L1 <- buoy2019_L1 %>% 
  mutate(flag_par = case_when(flag_par == '' ~ 'n',
                              flag_par != '' ~ paste('n', flag_par, sep = '; ')),
         flag_par = case_when(flag_par == '' & 
                                datetime >= as.POSIXct('2019-01-09', tz=buoy_tz) &
                                datetime < as.POSIXct('2019-01-20', tz=buoy_tz) ~ 'o',
                              flag_par != '' & 
                                datetime >= as.POSIXct('2019-01-09', tz=buoy_tz) &
                                datetime < as.POSIXct('2019-01-20', tz=buoy_tz) ~ paste('o', flag_par, sep = '; '),
                              TRUE ~ flag_par))
unique(buoy2019_L1$flag_par)

ggplot(buoy2019_L1,
       aes(x=datetime, y=PAR, color=location, shape = flag_par)) +
  geom_point() +
  labs(title = 'PAR 2019, NA recoded') +
  scale_x_datetime(date_minor_breaks = '1 month') +
  final_theme +
  scale_color_colorblind()

#rename with CV
buoy2019_L1 <- buoy2019_L1 %>% 
  rename(radiationIncomingPAR_umolm2s = PAR)


#### Air temp and rel humidity####

#recode when in transit or offline
buoy2019_L1 <- buoy2019_L1 %>% 
  mutate_at(vars(AirTempC, RelHum),
            ~case_when(location == 'in transit' ~ NA_real_,
                       TRUE ~ .))

# for(i in 1:(nrow(monthly_2019)-1)) {
#   chunk <- buoy2019_L1 %>% 
#     filter(datetime >= as.POSIXct(monthly_2019$date[i], tz= buoy_tz) &
#              datetime < as.POSIXct(monthly_2019$date[i+1], tz=buoy_tz))
#   chunk_vert <- chunk %>% 
#     select(datetime, AirTempC, RelHum, location) %>% 
#     pivot_longer(names_to = 'variable', values_to = 'value', -c(datetime, location))
#   ggplot(chunk_vert, aes(x = datetime, y = value, color = location)) +
#     geom_point() +
#     facet_grid(variable ~ ., scales = 'free_y') +
#     scale_x_datetime(date_minor_breaks = '1 day') +
#     scale_color_colorblind() +
#     final_theme +
#     labs(x = NULL,
#          y = NULL,
#          title = paste0('LSPA Air Temperature and RH ', monthly_2019$date[i], ' - ', monthly_2019$date[i+1]))
#   ggsave(filename = paste0('graphs/2019/L0_airtempRH_monthly_', monthly_2019$date[i], '.jpg'), height = 3, width =8, units = 'in', dpi = 300)
# }

#look good
#add RH flags for supersaturated 
buoy2019_L1 <- buoy2019_L1 %>% 
  mutate(flag_rh = case_when(RelHum >100 ~ 's',
                             TRUE ~ ''))

#rename with CV
buoy2019_L1 <- buoy2019_L1 %>% 
  rename(relativeHumidity_perc = RelHum,
         airTemperature_degC = AirTempC)

#### EXPORT L1 DATA STREAMS ####
colnames(buoy2019_L1)

#export L1 tempstring file
buoy2019_L1 %>%
  select(datetime, location, waterTemperature_degC_0p75m:waterTemperature_degC_9p75m, flag_temp0p75m) %>%
  mutate(datetime = as.character(datetime)) %>%
  write_csv(., file.path(dump_dir,'tempstring/2019_tempstring_L1_v2022.csv'))

# export L1 do file
buoy2019_L1 %>%
  select(datetime, location, 
         oxygenDissolved_mgl_0p25m, oxygenDissolvedPercentOfSaturation_pct_0p25m, waterTemperature_DO_degC_0p25m, 
         flag_do0p25m,
         oxygenDissolved_mgl_10m, oxygenDissolvedPercentOfSaturation_pct_10m, waterTemperature_DO_degC_10m, 
         flag_do10m) %>%
  mutate(datetime = as.character(datetime)) %>%
  write_csv(., file.path(dump_dir, 'do/2019_do_L1_v2022.csv'))

#export L1 met file
buoy2019_L1 %>%
  select(datetime, location, 
         windSpeedAverage_mps:windGustDirection_deg,
         radiationIncomingPAR_umolm2s, flag_par,
         airTemperature_degC, relativeHumidity_perc, flag_rh) %>%
  mutate(datetime = as.character(datetime)) %>%
  write_csv(., file.path(dump_dir, 'met/2019_met_L1_v2022.csv'))
