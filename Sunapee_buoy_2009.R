#*****************************************************************
#*      Cary Institute of Ecosystem Studies (Millbrook, NY)      *
#*                                                               *
#* TITLE:   Sunapee_buoy_2009.r                                  *
#* AUTHOR:  Bethel Steele                                        *
#* SYSTEM:  Lenovo ThinkCentre, Win 10, R 3.5.2, RStudio 1.1.383 *
#* PROJECT: Lake Sunapee Buoy Data Cleaning                      *
#* PURPOSE: subset data for met/compare with L1                  *
#* LAST MODIFIED: 05Sept2019 to create vertical dataset for      *
#*          master collation                                     *
#*****************************************************************

source('library_func_lists.R')

#point to data directories
raw_dir = 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L0/'
dump_dir = 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/'

#set tz
buoy_tz = 'Etc/GMT+5'

#bring in 2009 buoy raw data
buoy2009_L0 <- read.csv(file.path(raw_dir, 'Sunapee2009_rawData.csv'),
                        col.names = c('datetime', 'AirTempC', 'DOppm', 'DOSat', 'DOSat2', 
                                      'PAR', 'DOTempC', 'TempC_0m', 'TempC_0p5m', 'TempC_1m', 
                                      'TempC_1p5m', 'TempC_2m', 'TempC_2p5m', 'TempC_3m', 'TempC_4m', 
                                      'TempC_5m', 'TempC_6m', 'TempC_7m', 'TempC_8m', 'TempC_9m', 
                                      'TempC_10m', 'TempC_11m', 'TempC_13m', 'AveWindDir', 'InstWindDir', 
                                      'InstWindSp', 'AveWindSp'), 
                        skip=1) %>% 
  select(-DOSat2) %>%  #drop blank columns
  mutate(datetime = as.POSIXct(datetime, format='%Y-%m-%d %H:%M:%S', tz=buoy_tz))

#double check to make sure there are no DST issues
datelength2009 <- buoy2009_L0 %>% 
  mutate(date = format(datetime, '%Y-%m-%d')) %>% 
  group_by(date) %>% 
  summarize(length(datetime))

#check dst dates
datelength2009[datelength2009$date == '2009-03-08',]
#dst observed here
datelength2009[datelength2009$date == '2009-11-01',]
#no dst observed here. 

#force into NYtz with dst; convert to utc-5
buoy2009_L1 <- buoy2009_L0 %>% 
  mutate(datetime_instrument = force_tz(datetime, tz = 'America/New_York'),
         datetime = with_tz(datetime_instrument, tz = buoy_tz))

#check again
datelength2009 <- buoy2009_L1 %>% 
  mutate(date = format(datetime, '%Y-%m-%d')) %>% 
  group_by(date) %>%
  summarize(length(datetime))
#look at dst
datelength2009[datelength2009$date == '2009-03-08',]
datelength2009[datelength2009$date == '2009-11-01',]


#create dummy timestamp so there are no blanks
alltimes_2009 <- as.data.frame(seq.POSIXt(as.POSIXct('2009-01-01 00:00', tz=buoy_tz), as.POSIXct('2009-12-31 23:50', tz=buoy_tz), '10 min')) %>% 
  rename("datetime" = !!names(.[1]))

buoy2009_L1 <- buoy2009_L1 %>% 
  right_join(., alltimes_2009) %>% 
  arrange(datetime)

#clean up workspace
rm(alltimes_2009, datelength2009)


#### thermistors ####
buoy2009_temp_vert <- buoy2009_L1 %>%
  select(datetime, all_of(alltemp2007)) %>%
  pivot_longer(names_to = 'variable', values_to = 'value', -datetime) %>% 
  mutate(variable = factor(variable, levels=alltemp2007))

# ggplot(subset(buoy2009_temp_vert,
#               subset=(datetime >=as.POSIXct('2009-01-01', tz=buoy_tz) &
#                         datetime < as.POSIXct('2010-01-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='2009 buoy temp raw',
#        x=NULL,
#        y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89",
#                               "#005180", "#0081cc", "#66c7ff")) #so you can adjust

buoy2009_L1 <- buoy2009_L1 %>% 
  mutate_at(vars(all_of(alltemp2007)),
            ~(case_when(. == -6999 ~ NA_real_,
                           . == -99.9 ~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate(location = 'loon')

buoy2009_temp_vert <- buoy2009_L1 %>%
  select(datetime, all_of(alltemp2007)) %>%
  pivot_longer(names_to = 'variable', values_to = 'value', -datetime) %>% 
  mutate(variable = factor(variable, levels=alltemp2007))

# ggplot(subset(buoy2009_temp_vert,
#               subset=(datetime >=as.POSIXct('2009-01-01', tz=buoy_tz) &
#                         datetime < as.POSIXct('2010-01-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='2009 buoy temp NAs recoded',
#        x=NULL,
#        y='temp (deg C)') +
#   scale_x_datetime(date_minor_breaks = '1 month') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# ggplot(subset(buoy2009_temp_vert,
#               subset=(datetime >=as.POSIXct('2009-01-01', tz=buoy_tz) &
#                         datetime < as.POSIXct('2009-02-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Jan 2009, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust

buoy2009_L1 <- buoy2009_L1 %>% 
  mutate(flag_alltemp = case_when(datetime < as.POSIXct('2009-01-15', tz=buoy_tz) ~ 'i',
                               TRUE ~ ''))
buoy2009_temp_vert_b <- buoy2009_L1 %>%
  select(datetime, all_of(alltemp2007)) %>%
  pivot_longer(names_to = 'variable', values_to = 'value', -datetime) %>% 
  mutate(variable = factor(variable, levels=alltemp2007))

# ggplot(subset(buoy2009_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2009-01-01', tz=buoy_tz) &
#                         datetime < as.POSIXct('2009-02-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Jan 2009, clean',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# ggplot(subset(buoy2009_temp_vert,
#               subset=(datetime >=as.POSIXct('2009-02-01', tz=buoy_tz) &
#                         datetime < as.POSIXct('2009-03-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Feb 2009, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# #data gap until jul
# ggplot(subset(buoy2009_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2009-07-28', tz=buoy_tz) &
#                         datetime < as.POSIXct('2009-09-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Jul/Aug 2009, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust

buoy2009_L1 <- buoy2009_L1 %>% 
  mutate_at(vars(all_of(alltemp2007)),
            ~case_when(datetime > as.Date('2009-02-01') & datetime < as.Date('2009-03-01') ~ NA_real_, 
                       TRUE ~ .))
buoy2009_temp_vert_b <- buoy2009_L1 %>%
  select(datetime, all_of(alltemp2007)) %>%
  pivot_longer(names_to = 'variable', values_to = 'value', -datetime) %>% 
  mutate(variable = factor(variable, levels=alltemp2007))

# #look at beginning of record
# ggplot(subset(buoy2009_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2009-07-28', tz=buoy_tz) &
#                         datetime < as.POSIXct('2009-07-29', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Jul/Aug 2009, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# ggplot(subset(buoy2009_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2009-07-24', tz=buoy_tz) &
#                         datetime < as.POSIXct('2009-08-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Jul/Aug 2009, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# ggplot(subset(buoy2009_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2009-08-01', tz=buoy_tz) &
#                         datetime < as.POSIXct('2009-08-08', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Jul/Aug 2009, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# ggplot(subset(buoy2009_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2009-08-08', tz=buoy_tz) &
#                         datetime < as.POSIXct('2009-08-15', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Jul/Aug 2009, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# #anomalous points Aug 11,13 and 24
# ggplot(subset(buoy2009_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2009-08-11', tz=buoy_tz) &
#                         datetime < as.POSIXct('2009-08-12', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Jul/Aug 2009, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# ggplot(subset(buoy2009_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2009-08-12', tz=buoy_tz) &
#                         datetime < as.POSIXct('2009-08-13', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Jul/Aug 2009, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# ggplot(subset(buoy2009_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2009-08-13', tz=buoy_tz) &
#                         datetime < as.POSIXct('2009-08-14', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Jul/Aug 2009, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust

# ggplot(subset(buoy2009_temp_vert,
#               subset=(datetime >=as.POSIXct('2009-08-24', tz=buoy_tz) &
#                         datetime < as.POSIXct('2009-08-25', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Jul/Aug 2009, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# ggplot(subset(buoy2009_temp_vert,
#               subset=(datetime >=as.POSIXct('2009-08-31', tz=buoy_tz) &
#                         datetime < as.POSIXct('2009-09-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Jul/Aug 2009, clean',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# ggplot(subset(buoy2009_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2009-08-01 12:00', tz=buoy_tz) &
#                         datetime < as.POSIXct('2009-08-02 12:00', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Jul/Aug 2009, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# ggplot(subset(buoy2009_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2009-08-26', tz=buoy_tz) &
#                         datetime < as.POSIXct('2009-08-27', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Jul/Aug 2009, clean',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# ggplot(subset(buoy2009_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2009-08-31', tz=buoy_tz) &
#                         datetime < as.POSIXct('2009-09-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Jul/Aug 2009, clean',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust

buoy2009_L1 <- buoy2009_L1 %>% 
  mutate_at(vars(TempC_0p5m:TempC_4m), #temp shifts
            ~(case_when(is.na(TempC_5m) ~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate_at(vars(TempC_6m:TempC_13m),
            ~(case_when(TempC_13m > TempC_11m & datetime >= as.POSIXct('2009-07-28', tz=buoy_tz) & datetime < as.POSIXct('2009-10-02 12:00', tz=buoy_tz)~ NA_real_, # temp shifts
                           TempC_13m > TempC_6m & datetime >= as.POSIXct('2009-07-28', tz=buoy_tz) & datetime < as.POSIXct('2009-10-02 12:00', tz=buoy_tz)~ NA_real_, # temp shifts
                           TRUE ~ .))) %>% 
  mutate_at(vars(alltemp2007),
            ~(case_when(datetime >= as.POSIXct('2009-02-01', tz=buoy_tz) & datetime < as.POSIXct('2009-07-28', tz=buoy_tz) ~ NA_real_,
                           TempC_8m < 7.5 & datetime > as.POSIXct('2009-08-01', tz=buoy_tz) & datetime < as.POSIXct('2009-09-01', tz=buoy_tz) ~ NA_real_, #temp shifts
                           TRUE ~ .))) %>% 
  mutate_at(vars(TempC_8m:TempC_11m),
            ~(case_when(is.na(TempC_13m)~ NA_real_, #temp shifts
                           TRUE ~ .))) %>% 
  mutate(TempC_13m = case_when(is.na(TempC_1m) ~ NA_real_, #temp shifts
                               TRUE ~ TempC_13m)) %>% 
  mutate_at(vars(alltemp2007),
            ~(case_when(datetime == as.POSIXct('2009-08-31 10:40', tz=buoy_tz) ~ NA_real_, 
                           TRUE ~ .)))
  
buoy2009_temp_vert_b <- buoy2009_L1 %>%
  select(datetime, all_of(alltemp2007)) %>%
  pivot_longer(names_to = 'variable', values_to = 'value', -datetime) %>% 
  mutate(variable = factor(variable, levels=alltemp2007))

# ggplot(subset(buoy2009_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2009-07-28', tz=buoy_tz) &
#                         datetime < as.POSIXct('2009-09-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Jul/Aug 2009, clean',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# ggplot(subset(buoy2009_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2009-09-01', tz=buoy_tz) &
#                         datetime < as.POSIXct('2009-10-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Sept 2009, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# ggplot(subset(buoy2009_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2009-10-01', tz=buoy_tz) &
#                         datetime < as.POSIXct('2009-11-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Oct 2009, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# ggplot(subset(buoy2009_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2009-11-01', tz=buoy_tz) &
#                         datetime < as.POSIXct('2009-12-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Nov 2009, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# #Nov 16
# ggplot(subset(buoy2009_temp_vert,
#               subset=(datetime >=as.POSIXct('2009-11-16', tz=buoy_tz) &
#                         datetime < as.POSIXct('2009-11-17', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Nov 2009, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
buoy2009_L1 <- buoy2009_L1 %>% 
  mutate_at(vars(alltemp2007),
            ~(case_when(datetime == as.POSIXct('2009-11-16 6:50', tz=buoy_tz) ~ NA_real_,
                           TRUE ~ .))) 

buoy2009_temp_vert_b <- buoy2009_L1 %>%
  select(datetime, all_of(alltemp2007)) %>%
  pivot_longer(names_to = 'variable', values_to = 'value', -datetime) %>% 
  mutate(variable = factor(variable, levels=alltemp2007))

# ggplot(subset(buoy2009_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2009-11-01', tz=buoy_tz) &
#                         datetime < as.POSIXct('2009-12-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Nov 2009, clean',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# ggplot(subset(buoy2009_temp_vert,
#               subset=(datetime >=as.POSIXct('2009-12-01', tz=buoy_tz) &
#                         datetime < as.POSIXct('2010-01-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Dec 2009, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# # #Dec 8, 12, 17, 23/24
# ggplot(subset(buoy2009_temp_vert,
#               subset=(datetime >=as.POSIXct('2009-12-08', tz=buoy_tz) &
#                         datetime < as.POSIXct('2009-12-09', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Dec 2009, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# ggplot(subset(buoy2009_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2009-12-12', tz=buoy_tz) &
#                         datetime < as.POSIXct('2009-12-13', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Dec 2009, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# ggplot(subset(buoy2009_temp_vert,
#               subset=(datetime >=as.POSIXct('2009-12-17', tz=buoy_tz) &
#                         datetime < as.POSIXct('2009-12-18', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Dec 2009, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# ggplot(subset(buoy2009_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2009-12-21', tz=buoy_tz) &
#                         datetime < as.POSIXct('2009-12-22', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Dec 2009, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# ggplot(subset(buoy2009_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2009-12-22', tz=buoy_tz) &
#                         datetime < as.POSIXct('2009-12-23', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Dec 2009, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# ggplot(subset(buoy2009_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2009-12-23', tz=buoy_tz) &
#                         datetime < as.POSIXct('2009-12-24', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Dec 2009, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# ggplot(subset(buoy2009_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2009-12-24', tz=buoy_tz) &
#                         datetime < as.POSIXct('2009-12-25', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Dec 2009, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# ggplot(subset(buoy2009_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2009-12-29', tz=buoy_tz) &
#                         datetime < as.POSIXct('2009-12-30', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Dec 2009, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust


buoy2009_L1 <- buoy2009_L1 %>% 
  mutate_at(vars(all_of(alltemp2007)),
            ~(case_when(datetime == as.POSIXct('2009-12-08 23:20', tz=buoy_tz) ~ NA_real_, #anamolous shift
                           datetime >= as.POSIXct('2009-12-12 4:50', tz=buoy_tz) & datetime < as.POSIXct('2009-12-12 9:00', tz=buoy_tz) ~ NA_real_, #flatline
                           datetime >= as.POSIXct('2009-12-17 4:00', tz=buoy_tz) & datetime < as.POSIXct('2009-12-17 5:20', tz=buoy_tz) ~ NA_real_, #flatline
                           datetime >= as.POSIXct('2009-12-17 12:20', tz=buoy_tz) & datetime < as.POSIXct('2009-12-17 15:00', tz=buoy_tz) ~ NA_real_, #flatline
                           datetime >= as.POSIXct('2009-12-21 19:30', tz=buoy_tz) & datetime < as.POSIXct('2009-12-22 6:10', tz=buoy_tz) ~ NA_real_, #flatline
                           datetime >= as.POSIXct('2009-12-22 19:00', tz=buoy_tz) & datetime < as.POSIXct('2009-12-22 22:00', tz=buoy_tz) ~ NA_real_, #flatline
                           datetime >= as.POSIXct('2009-12-29 9:50', tz=buoy_tz) & datetime < as.POSIXct('2009-12-29 11:20', tz=buoy_tz) ~ NA_real_, #flatline
                           is.na(TempC_1p5m) ~ NA_real_, #anamolous shift
                           TRUE ~ .))) 

buoy2009_temp_vert_b <- buoy2009_L1 %>%
  select(datetime, all_of(alltemp2007)) %>%
  pivot_longer(names_to = 'variable', values_to = 'value', -datetime) %>% 
  mutate(variable = factor(variable, levels=alltemp2007))

# ggplot(subset(buoy2009_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2009-12-01', tz=buoy_tz) &
#                         datetime < as.POSIXct('2010-01-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='Dec 2009, clean',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# ggplot(subset(buoy2009_temp_vert_b,
#               subset=(datetime >=as.POSIXct('2009-01-01', tz=buoy_tz) &
#                         datetime < as.POSIXct('2010-01-01', tz=buoy_tz))),
#        aes(x=datetime, y=value, color=(variable))) +
#   geom_point() +
#   final_theme +
#   labs(title='2009, clean',
#        x=NULL,
#        y='temp deg C') +
#   scale_x_datetime(date_minor_breaks = '1 month') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust

buoy2009_L1 <- buoy2009_L1 %>% 
  mutate(flag_alltemp = case_when(flag_alltemp == '' & datetime>=as.POSIXct('2009-12-12', tz=buoy_tz) ~ 'i',
                                  flag_alltemp != '' &datetime>=as.POSIXct('2009-12-12', tz=buoy_tz) ~ paste('i', flag_alltemp, sep = '; '),
                               TRUE ~ flag_alltemp))

#add flag for 11.5 and 13.5 as possibly in sediment
buoy2009_L1 <- buoy2009_L1 %>% 
  mutate(flag_temp11p5m = 'b',
         flag_temp13p5m = 'b')

#add flag prior to jan 15 for 9-13 tangled and at wrong depth
buoy2009_L1 <- buoy2009_L1 %>% 
  mutate(flag_temp9p5m = case_when(!is.na(TempC_9m) & datetime <= as.Date('2009-01-15') ~ 'd',
                                   TRUE ~ ''),
         flag_temp10p5m = case_when(!is.na(TempC_10m) & datetime <= as.Date('2009-01-15') ~ 'd',
                                   TRUE ~ ''),
         flag_temp11p5m = case_when(!is.na(TempC_11m) & flag_temp11p5m == '' & datetime <= as.Date('2009-01-15') ~ 'd',
                                    !is.na(TempC_11m) & flag_temp11p5m != '' & datetime <= as.Date('2009-01-15') ~ paste(flag_temp11p5m, 'd', sep = '; '),
                                    TRUE ~ flag_temp11p5m),
         flag_temp13p5m = case_when(!is.na(TempC_13m) & flag_temp13p5m == '' & datetime <= as.Date('2009-01-15') ~ 'd',
                                    !is.na(TempC_13m) & flag_temp13p5m != '' & datetime <= as.Date('2009-01-15') ~ paste(flag_temp13p5m, 'd', sep = '; '),
                                    TRUE ~ flag_temp13p5m))

buoy2009_temp_vert_b <- buoy2009_L1 %>%
  select(datetime, all_of(alltemp2007)) %>%
  pivot_longer(names_to = 'variable', values_to = 'value', -datetime) %>% 
  mutate(variable = factor(variable, levels=alltemp2007))

ggplot(subset(buoy2009_temp_vert_b,
              subset=(datetime >=as.POSIXct('2009-01-01', tz=buoy_tz) &
                        datetime < as.POSIXct('2010-01-01', tz=buoy_tz))),
       aes(x=datetime, y=value, color=(variable))) +
  geom_point() +
  final_theme +
  labs(title='2009, clean',
       x=NULL,
       y='temp deg C') +
  scale_x_datetime(date_minor_breaks = '1 month') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
                              "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust

#add in offline location
buoy2009_L1 <- buoy2009_L1 %>% 
  mutate(location = case_when(datetime >= as.POSIXct('2009-05-08', tz=buoy_tz) & 
                                datetime < as.POSIXct('2009-07-28 13:30', tz=buoy_tz) ~ 'offline',   
                              TRUE ~ location)) %>% 
  mutate_at(vars(flag_alltemp, flag_temp9p5m, flag_temp10p5m, flag_temp11p5m, flag_temp13p5m),
            ~case_when(location == 'offline' ~ '',
                               TRUE~.))

#clean up workspace
rm(buoy2009_temp_vert, buoy2009_temp_vert_b)

#correct thermistor depth for offset; add CV
buoy2009_L1 <- buoy2009_L1 %>% 
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
range(buoy2009_L1$DOSat, na.rm=T)
range(buoy2009_L1$DOppm, na.rm=T)
range(buoy2009_L1$DOTempC, na.rm=T)

buoy2009_L1 <- buoy2009_L1 %>% 
  mutate(DOTempC = case_when(DOTempC == -6999 ~ NA_real_,
                             TRUE ~ DOTempC))

do_vert <- buoy2009_L1 %>% 
  select(datetime, DOSat, DOppm, DOTempC) %>% 
  pivot_longer(names_to = 'variable', values_to = 'value', -datetime)

# ggplot(do_vert, aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = '2009 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 month')
# 
# ggplot(subset(do_vert,
#               subset = (datetime >= as.POSIXct('2009-01-01', tz=buoy_tz) &
#                           datetime < as.POSIXct('2009-02-01', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'jan 2009 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')

#start seeing intermittent readings dec 29 - adding flag of intermittent do data from then throught the end of the month
buoy2009_L1 <- buoy2009_L1 %>% 
  mutate(flag_do1p5m = case_when(datetime < as.POSIXct('2009-01-15', tz=buoy_tz) & !is.na(DOTempC) ~ 'i',
                             TRUE ~ ''))

# ggplot(subset(do_vert,
#               subset = (datetime >= as.POSIXct('2009-02-01', tz=buoy_tz) &
#                           datetime < as.POSIXct('2009-03-01', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'feb 2009 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
#data before feb 23 recoded to NA
buoy2009_L1 <- buoy2009_L1 %>% 
  mutate_at(vars(DOSat, DOppm, DOTempC),
            ~(case_when(datetime >= as.POSIXct('2009-02-01', tz=buoy_tz) &
                             datetime < as.POSIXct('2009-02-23', tz=buoy_tz) ~ NA_real_,
                             TRUE ~ .)))
do_vert_b <- buoy2009_L1 %>% 
  select(datetime, DOSat, DOppm, DOTempC) %>% 
  pivot_longer(names_to = 'variable', values_to = 'value', -datetime)

# ggplot(subset(do_vert_b,
#               subset = (datetime >= as.POSIXct('2009-02-01', tz=buoy_tz) &
#                           datetime < as.POSIXct('2009-03-01', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'feb 2009 DO data clean',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(do_vert,
#               subset = (datetime >= as.POSIXct('2009-03-01', tz=buoy_tz) &
#                           datetime < as.POSIXct('2009-04-01', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'mar 2009 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(do_vert,
#               subset = (datetime >= as.POSIXct('2009-04-01', tz=buoy_tz) &
#                           datetime < as.POSIXct('2009-05-01', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'apr 2009 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# #errant readings beginning apr 5
# ggplot(subset(do_vert,
#               subset = (datetime >= as.POSIXct('2009-04-05', tz=buoy_tz) &
#                           datetime < as.POSIXct('2009-04-06', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'apr 2009 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 hour')

buoy2009_L1 <- buoy2009_L1 %>% 
  mutate_at(vars(DOSat, DOppm, DOTempC),
            ~(case_when(datetime >= as.POSIXct('2009-04-05 0:00', tz=buoy_tz) &
                             datetime < as.POSIXct('2009-04-16', tz=buoy_tz) ~ NA_real_,
                           TRUE ~ .)))
do_vert_b <- buoy2009_L1 %>% 
  select(datetime, DOSat, DOppm, DOTempC) %>% 
  pivot_longer(names_to = 'variable', values_to = 'value', -datetime)

# ggplot(subset(do_vert_b,
#               subset = (datetime >= as.POSIXct('2009-04-01', tz=buoy_tz) &
#                           datetime < as.POSIXct('2009-05-01', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'apr 2009 DO data clean',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(do_vert,
#               subset = (datetime >= as.POSIXct('2009-05-01', tz=buoy_tz) &
#                           datetime < as.POSIXct('2009-06-01', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'may 2009 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(do_vert,
#               subset = (datetime >= as.POSIXct('2009-06-01', tz=buoy_tz) &
#                           datetime < as.POSIXct('2009-07-01', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'jun 2009 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(do_vert,
#               subset = (datetime >= as.POSIXct('2009-07-01', tz=buoy_tz) &
#                           datetime < as.POSIXct('2009-08-01', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'jul 2009 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(do_vert,
#               subset = (datetime >= as.POSIXct('2009-08-01', tz=buoy_tz) &
#                           datetime < as.POSIXct('2009-09-01', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'aug 2009 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(do_vert,
#               subset = (datetime >= as.POSIXct('2009-09-01', tz=buoy_tz) &
#                           datetime < as.POSIXct('2009-10-01', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'sept 2009 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(do_vert,
#               subset = (datetime >= as.POSIXct('2009-10-01', tz=buoy_tz) &
#                           datetime < as.POSIXct('2009-11-01', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'oct 2009 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(do_vert,
#               subset = (datetime >= as.POSIXct('2009-11-01', tz=buoy_tz) &
#                           datetime < as.POSIXct('2009-12-01', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'nov 2009 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(do_vert,
#               subset = (datetime >= as.POSIXct('2009-12-01', tz=buoy_tz) &
#                           datetime < as.POSIXct('2010-01-01', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'dec 2009 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')

# ggplot(do_vert_b,
#        aes(x = datetime, y = value, color = location)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = '2009 DO data clean',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 month')

# add presumed cleaning flags and possible non-calibration flags
buoy2009_L1 <-  buoy2009_L1 %>%
  mutate(flag_do1p5m = case_when(flag_do1p5m == '' & datetime == as.POSIXct('2009-07-28 12:30', tz=buoy_tz) ~ 'wp',
                                 flag_do1p5m != '' & datetime == as.POSIXct('2009-07-28 12:30', tz=buoy_tz) ~ paste('wp', flag_do1p5m, sep = '; '),
                                 TRUE ~ flag_do1p5m)) %>% 
  mutate(flag_do1p5m = case_when(flag_do1p5m == '' ~ 'x',
                                 flag_do1p5m != ''  ~ paste('x', flag_do1p5m, sep = '; '),
                                   TRUE ~ flag_do1p5m)) %>% 
  mutate(flag_do1p5m = case_when(location == 'offline' ~ '',
                                   TRUE ~ flag_do1p5m))
  
do_vert_b <- buoy2009_L1 %>% 
  select(datetime, location, DOSat, DOppm, DOTempC, flag_do1p5m) %>% 
  pivot_longer(names_to = 'variable', values_to = 'value', -c(datetime, location, flag_do1p5m))

ggplot(do_vert_b,
       aes(x = datetime, y = value, shape = location, color = flag_do1p5m)) +
  geom_point() +
  facet_grid(variable~., scales = 'free_y') +
  final_theme +
  labs(title = '2009 DO data clean',
       x = NULL,
       y = NULL) +
  scale_x_datetime(date_minor_breaks = '1 month')

rm(do_vert, do_vert_b)

#rename with CV
buoy2009_L1 <- buoy2009_L1 %>% 
  rename(oxygenDissolved_mgl_1p5m = DOppm,
         oxygenDissolvedPercentOfSaturation_pct_1p5m = DOSat,
         waterTemperature_DO_degC_1p5m = DOTempC)


#### wind sensors ####
range(buoy2009_L1$InstWindDir, na.rm = T)
range(buoy2009_L1$InstWindSp, na.rm = T)
range(buoy2009_L1$AveWindDir, na.rm = T)
range(buoy2009_L1$AveWindSp, na.rm = T)

buoy2009_L1 <- buoy2009_L1 %>% 
  mutate(InstWindDir = case_when(InstWindDir==-6999 ~ NA_real_,
                                 TRUE ~ InstWindDir))

wind_vert <- buoy2009_L1 %>% 
  select(datetime, InstWindDir, AveWindDir,InstWindSp, AveWindSp) %>% 
  gather(variable, value, -datetime)

ggplot(wind_vert, aes(x = datetime, y = value)) +
  geom_point() +
  facet_grid(variable~., scales = 'free_y') +
  final_theme +
  labs(title = '2009 wind data raw',
       x = NULL,
       y = NULL) +
  scale_x_datetime(date_minor_breaks = '1 month')

# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2009-01-01', tz=buoy_tz) &
#                         datetime<as.POSIXct('2009-02-01', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'jan 2009 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2009-02-01', tz=buoy_tz) &
#                         datetime<as.POSIXct('2009-03-01', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'feb 2009 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
 
#wind offline until feb
buoy2009_L1 <- buoy2009_L1 %>% 
  mutate_at(vars(InstWindSp, InstWindDir),
            ~(case_when(datetime<as.Date('2009-02-18') ~ NA_real_,
                           TRUE ~ .)))

wind_vert_b <- buoy2009_L1 %>% 
  select(datetime, InstWindDir, AveWindDir,InstWindSp, AveWindSp) %>% 
  gather(variable, value, -datetime)

# ggplot(subset(wind_vert_b,
#               subset=(datetime>=as.POSIXct('2009-01-01', tz=buoy_tz) &
#                         datetime<as.POSIXct('2009-03-01', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'feb 2009 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2009-03-01', tz=buoy_tz) &
#                         datetime<as.POSIXct('2009-04-01', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'mar 2009 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2009-04-01', tz=buoy_tz) &
#                         datetime<as.POSIXct('2009-05-01', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'apr 2009 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2009-05-01', tz=buoy_tz) &
#                         datetime<as.POSIXct('2009-06-01', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'may 2009 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2009-06-01', tz=buoy_tz) &
#                         datetime<as.POSIXct('2009-07-01', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'jun 2009 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2009-07-01', tz=buoy_tz) &
#                         datetime<as.POSIXct('2009-08-01', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'july 2009 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2009-08-01', tz=buoy_tz) &
#                         datetime<as.POSIXct('2009-09-01', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'aug 2009 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2009-09-01', tz=buoy_tz) &
#                         datetime<as.POSIXct('2009-10-01', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'sept 2009 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2009-10-01', tz=buoy_tz) &
#                         datetime<as.POSIXct('2009-11-01', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'oct 2009 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2009-11-01', tz=buoy_tz) &
#                         datetime<as.POSIXct('2009-12-01', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'nov 2009 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# # look at nov 27-28
# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2009-11-27 12:00', tz=buoy_tz) &
#                         datetime<as.POSIXct('2009-11-28 18:00', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'sept 2009 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 hour')
# # odd to have so many 0 readings, but not a sensor frozen issue, leaving in

#flag as suspect
buoy2009_L1 <- buoy2009_L1 %>% 
  mutate(flag_allwind = case_when(datetime >= as.Date('2009-11-27') &
                                    datetime < as.Date('2009-11-29') &
                                    InstWindSp == 0 ~ 's',
                                  TRUE ~ ''))

# ggplot(subset(wind_vert,
#               subset=(datetime>=as.POSIXct('2009-12-01', tz=buoy_tz) &
#                         datetime<as.POSIXct('2010-01-01', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'sept 2009 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# #dec 26 sensor frozen
# ggplot(subset(wind_vert_b,
#               subset=(datetime>=as.POSIXct('2009-12-26 00:00', tz=buoy_tz) &
#                         datetime<as.POSIXct('2009-12-28 00:00', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'sept 2009 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 hour')

buoy2009_L1 <- buoy2009_L1 %>%
  mutate_at(vars(InstWindSp, InstWindDir, AveWindSp, AveWindDir),
            ~(case_when(datetime>=as.POSIXct('2009-12-26 19:40', tz=buoy_tz) &
                             datetime<as.POSIXct('2009-12-26 21:50', tz=buoy_tz) ~ NA_real_,
                           TRUE ~ .)))

wind_vert_b <- buoy2009_L1 %>% 
  select(datetime, InstWindDir, AveWindDir,InstWindSp, AveWindSp, location) %>% 
  pivot_longer(names_to = 'variable', values_to = 'value', -c(datetime, location))

# ggplot(subset(wind_vert_b,
#               subset=(datetime>=as.POSIXct('2009-12-01', tz=buoy_tz) &
#                         datetime<as.POSIXct('2010-01-01', tz=buoy_tz))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'sept 2009 wind data NAs recoded',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')

# add flag to wind direction prior to jul 28 when buoy online
buoy2009_L1 <- buoy2009_L1 %>% 
  mutate(flag_winddir = case_when(datetime < as.POSIXct('2009-07-28', tz=buoy_tz) & !is.na(InstWindDir) ~ 'e',
                                   TRUE ~ ''))
wind_vert_b <- buoy2009_L1 %>% 
  select(datetime, InstWindDir, AveWindDir,InstWindSp, AveWindSp, location, flag_winddir, flag_allwind) %>% 
  pivot_longer(names_to = 'variable', values_to = 'value', -c(datetime, location, flag_winddir, flag_allwind))

ggplot(wind_vert_b,
       aes(x = datetime, y = value, color = flag_winddir)) +
  geom_point() +
  facet_grid(variable~., scales = 'free_y') +
  final_theme +
  labs(title = '2009 wind data clean',
       x = NULL,
       y = NULL) +
  scale_x_datetime(date_minor_breaks = '1 month')

# ggplot(wind_vert_b,
#        aes(x = datetime, y = value, color = flag_allwind)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = '2009 wind data clean',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 month')

buoy2009_L1 <- buoy2009_L1 %>% 
  rename(windDirectionInstantaneous_deg = InstWindDir,
         windSpeedInstantaneous_mps = InstWindSp,
         windDirectionAverage_deg = AveWindDir, 
         windSpeedAverage_mps = AveWindSp)

rm(wind_vert, wind_vert_b)

#### PAR ####
range(buoy2009_L1$PAR, na.rm = T)

# ggplot(buoy2009_L1, aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme

buoy2009_L1 <-  buoy2009_L1 %>%
  mutate(flag_par = case_when(PAR <0 ~ 'z',
                         TRUE ~ ''),
         PAR = case_when(PAR <0 ~ 0,
                         TRUE ~ PAR))

ggplot(buoy2009_L1, aes(x = datetime, y = PAR)) +
  geom_point() +
  final_theme +
  labs(title = '2009 PAR data below 0 recoded',
       x = NULL,
       y = 'PAR (umol/m2/s)') +
  scale_x_datetime(date_minor_breaks = '1 month')

# ggplot(subset(buoy2009_L1,
#               subset = (datetime>=as.POSIXct('2009-01-01', tz=buoy_tz) &
#                           datetime<as.POSIXct('2009-02-01', tz=buoy_tz))),
#        aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'jan 2009 PAR data raw',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')

#jan data incomplete. flagging as intermittent
buoy2009_L1 <-  buoy2009_L1 %>%
  mutate(flag_par = case_when(datetime<as.POSIXct('2009-01-15', tz=buoy_tz) & flag_par == '' & !is.na(PAR) ~ 'i',
                              datetime<as.POSIXct('2009-01-15', tz=buoy_tz) & flag_par != '' & !is.na(PAR) ~ paste('i', flag_par, sep = '; '),
                              TRUE ~ flag_par))

# ggplot(subset(buoy2009_L1,
#               subset = (datetime>=as.POSIXct('2009-02-01', tz=buoy_tz) &
#                           datetime<as.POSIXct('2009-03-01', tz=buoy_tz))),
#        aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'feb 2009 PAR data raw',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')

#incomplete data through feb 23 midday
buoy2009_L1 <-  buoy2009_L1 %>%
  mutate(PAR = case_when(datetime>=as.POSIXct('2009-02-01', tz=buoy_tz) & datetime<as.POSIXct('2009-02-17', tz=buoy_tz) ~ NA_real_,
                         TRUE ~ PAR))

# ggplot(subset(buoy2009_L1,
#               subset = (datetime>=as.POSIXct('2009-03-01', tz=buoy_tz) &
#                           datetime<as.POSIXct('2009-04-01', tz=buoy_tz))),
#        aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'mar 2009 PAR data raw',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2009_L1,
#               subset = (datetime>=as.POSIXct('2009-04-01', tz=buoy_tz) &
#                           datetime<as.POSIXct('2009-05-01', tz=buoy_tz))),
#        aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'apr 2009 PAR data raw',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2009_L1,
#               subset = (datetime>=as.POSIXct('2009-05-01', tz=buoy_tz) &
#                           datetime<as.POSIXct('2009-06-01', tz=buoy_tz))),
#        aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'may 2009 PAR data raw',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2009_L1,
#               subset = (datetime>=as.POSIXct('2009-06-01', tz=buoy_tz) &
#                           datetime<as.POSIXct('2009-07-01', tz=buoy_tz))),
#        aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'jun 2009 PAR data raw',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2009_L1,
#               subset = (datetime>=as.POSIXct('2009-07-01', tz=buoy_tz) &
#                           datetime<as.POSIXct('2009-08-01', tz=buoy_tz))),
#        aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'jul 2009 PAR data raw',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2009_L1,
#               subset = (datetime>=as.POSIXct('2009-08-01', tz=buoy_tz) &
#                           datetime<as.POSIXct('2009-09-01', tz=buoy_tz))),
#        aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'aug 2009 PAR data raw',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2009_L1,
#               subset = (datetime>=as.POSIXct('2009-09-01', tz=buoy_tz) &
#                           datetime<as.POSIXct('2009-10-01', tz=buoy_tz))),
#        aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'sept 2009 PAR data raw',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2009_L1,
#               subset = (datetime>=as.POSIXct('2009-10-01', tz=buoy_tz) &
#                           datetime<as.POSIXct('2009-11-01', tz=buoy_tz))),
#        aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'oct 2009 PAR data raw',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2009_L1,
#               subset = (datetime>=as.POSIXct('2009-11-01', tz=buoy_tz) &
#                           datetime<as.POSIXct('2009-12-01', tz=buoy_tz))),
#        aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'nov 2009 PAR data raw',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2009_L1,
#               subset = (datetime>=as.POSIXct('2009-12-01', tz=buoy_tz) &
#                           datetime<as.POSIXct('2010-01-01', tz=buoy_tz))),
#        aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'dec 2009 PAR data raw',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
 
ggplot(buoy2009_L1, aes(x = datetime, y = PAR)) +
  geom_point() +
  final_theme +
  labs(title = '2009 PAR data clean',
       x = NULL,
       y = 'PAR (umol/m2/s)') +
  scale_x_datetime(date_minor_breaks = '1 month')

#rename with CV
buoy2009_L1 <- buoy2009_L1 %>% 
  rename(radiationIncomingPAR_umolm2s = PAR)


#### Air Temp ####
range(buoy2009_L1$AirTempC, na.rm = T)

ggplot(buoy2009_L1, aes(x=datetime, y = AirTempC)) +
  geom_point() +
  final_theme +
  labs(title = '2009 air temp raw',
       x= NULL,
       y= 'air temp (deg C)') +
  scale_x_datetime(date_minor_breaks = '1 month')

# data intermittent - recode Feb intermittent data

# ggplot(subset(buoy2009_L1,
#               subset=(datetime>=as.POSIXct('2009-01-01', tz=buoy_tz) &
#                         datetime < as.POSIXct('2009-02-01', tz=buoy_tz))),
#        aes(x=datetime, y = AirTempC)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'feb 2009 air temp raw',
#        x= NULL,
#        y= 'air temp (deg C)') +
#   scale_x_datetime(date_minor_breaks = '1 day')

#flag through jan 15 as intermittent
buoy2009_L1 <- buoy2009_L1 %>% 
  mutate(flag_airtemp = case_when(datetime < as.POSIXct('2009-01-15', tz=buoy_tz) & !is.na(AirTempC) ~ 'i',
                                  TRUE ~ ''))

# ggplot(subset(buoy2009_L1,
#               subset=(datetime>=as.POSIXct('2009-02-01', tz=buoy_tz) &
#                         datetime < as.POSIXct('2009-03-01', tz=buoy_tz))),
#        aes(x=datetime, y = AirTempC)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'feb 2009 air temp raw',
#        x= NULL,
#        y= 'air temp (deg C)') +
#   scale_x_datetime(date_minor_breaks = '1 day')

buoy2009_L1 <- buoy2009_L1 %>% 
  mutate(AirTempC = case_when(datetime >= as.POSIXct('2009-02-06', tz=buoy_tz) &
                                datetime < as.POSIXct('2009-02-17', tz=buoy_tz) ~ NA_real_,
                              TRUE ~ AirTempC))

ggplot(buoy2009_L1, aes(x=datetime, y = AirTempC, color = flag_airtemp)) +
  geom_point() +
  final_theme +
  labs(title = '2009 air temp clean',
       x= NULL,
       y= 'air temp (deg C)') +
  scale_x_datetime(date_minor_breaks = '1 month')

#rename with CV
buoy2009_L1 <- buoy2009_L1 %>% 
  rename(airTemperature_degC = AirTempC)


#### EXPORT L1 FILES ####
colnames(buoy2009_L1)

#export L1 tempstring file
buoy2009_L1 %>%
  select(datetime, location, waterTemperature_degC_0p5m:waterTemperature_degC_13p5m, 
         flag_alltemp, flag_temp9p5m, flag_temp10p5m, flag_temp11p5m, flag_temp13p5m) %>%
  mutate(datetime = as.character(datetime)) %>%
  write_csv(., file.path(dump_dir,'tempstring/2009_tempstring_L1_v2022.csv'))

# export L1 do file
buoy2009_L1 %>%
  select(datetime, location, oxygenDissolved_mgl_1p5m, oxygenDissolvedPercentOfSaturation_pct_1p5m, waterTemperature_DO_degC_1p5m, 
         flag_do1p5m) %>%
  mutate(datetime = as.character(datetime)) %>%
  write_csv(., file.path(dump_dir, 'do/2009_do_L1_v2022.csv'))

#export L1 met file
buoy2009_L1 %>%
  select(datetime, location, 
         windDirectionInstantaneous_deg, windSpeedInstantaneous_mps,
         windDirectionAverage_deg, windSpeedAverage_mps, flag_allwind, flag_winddir, 
         radiationIncomingPAR_umolm2s, flag_par,
         airTemperature_degC, flag_airtemp) %>%
  mutate(datetime = as.character(datetime)) %>%
  write_csv(., file.path(dump_dir, 'met/2009_met_L1_v2022.csv'))
