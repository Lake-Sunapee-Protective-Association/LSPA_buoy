#*****************************************************************
#*      Cary Institute of Ecosystem Studies (Millbrook, NY)      *
#*                                                               *
#* TITLE:   Sunapee_buoy_2007.r                                  *
#* AUTHOR:  Bethel Steele                                        *
#* SYSTEM:  Lenovo ThinkCentre, Win 10, R 3.4.2, RStudio 1.1.383 *
#* PROJECT: Lake Sunapee Buoy Data Cleaning                      *
#* PURPOSE: subset data for met/compare with L1                  *
#*****************************************************************

#bring in 2007 buoy raw data
buoy2007_L0 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L0/Sunapee2007_rawData.csv',
                        col_names = c('datetime', 'AirTempC', 'BattVolt', 'DOSat', 'DOppm', 
                                      'DOSat2', 'PAR', 'BattVolt2', 'RH', 'DOTempC', 
                                      'TempC_0m', 'TempC_0p5m', 'TempC_1m', 'TempC_1p5m', 'TempC_2m', 
                                      'TempC_2p5m', 'TempC_3m', 'TempC_4m', 'TempC_5m', 'TempC_6m',
                                      'TempC_7m', 'TempC_8m', 'TempC_9m', 'TempC_10m', 'TempC_11m',
                                      'TempC_13m', 'InstWindDir', 'AveWindDir', 'AveWindSp', 'InstWindSp'), 
                     col_types = 'cnnnnnnnnnnnnnnnnnnnnnnnnnnnnn',
                     skip=1) %>% 
  select(-BattVolt, -DOSat2, -BattVolt2, -AveWindSp, -AveWindDir) %>%  #drop blank columns
  mutate(datetime = as.POSIXct(datetime, format='%Y-%m-%d %H:%M:%S', tz='UTC'))

#bring in 2007 LMP data for comparison
LMP2007 <- read_xlsx('C:/Users/steeleb/Dropbox/Lake Sunapee/long term Sunapee data/raw data/LMP files/LMP 2017/DO.xlsx', 
                     sheet='DO',
                     col_types = c('text', 'text', 'numeric', 'guess', 'numeric', 
                                   'numeric', 'numeric', 'numeric', 'numeric' ,'numeric',
                                   'text', 'text', 'numeric', 'text')) %>% 
  mutate(DATE = as.Date(DATE, format='%Y-%m-%d')) %>%  #format date
  filter(DATE >= '2007-01-01' & DATE < '2008-01-01', #filter for 2016 only
         STATION == 210)

LMP2007_temp <- LMP2007 %>% 
  select(DATE, DEPTH, TEMP) %>% 
  filter(DEPTH<=14) %>% 
  rename(variable = 'DEPTH', 
         value = 'TEMP',
         datetime = 'DATE') %>% 
  mutate(source = 'LMP',
         datetime = as.POSIXct(paste(datetime, '12:00', sep=' '), format = '%Y-%m-%d %H:%M', tz='UTC'),
         variable = as.factor(variable),
         variable = case_when(variable == '0.5' ~ 'TempC_0p5m',
                              variable == '1' ~ 'TempC_1m',
                              variable == '2' ~ 'TempC_2m',
                              variable == '3' ~ 'TempC_3m',
                              variable == '4' ~ 'TempC_4m',
                              variable == '5' ~ 'TempC_5m',
                              variable == '6' ~ 'TempC_6m',
                              variable == '7' ~ 'TempC_7m',
                              variable == '8' ~ 'TempC_8m',
                              variable == '9' ~ 'TempC_9m',
                              variable == '10' ~ 'TempC_10m',
                              variable == '11' ~ 'TempC_11m',
                              variable == '12' ~ 'TempC_12m',
                              variable == '13' ~ 'TempC_13m',
                              variable == '14' ~ 'TempC_14m'))


#### thermisters ####
buoy2007_temp_vert <- buoy2007_L0 %>%
  select(datetime, alltemp2007) %>%
  gather(variable, value, -datetime) %>% 
  mutate(variable = factor(variable, levels = c(alltemp2007)))
  
ggplot(subset(buoy2007_temp_vert, 
              subset=(datetime >=as.POSIXct('2007-01-01', tz='UTC') & 
                        datetime < as.POSIXct('2008-01-01', tz='UTC'))), 
       aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  labs(title='2007 buoy data - raw',
       x=NULL,
       y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 month') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
                              "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust

buoy2007_L1 <- buoy2007_L0 %>% 
  mutate_at(vars(alltemp2007),
            funs(case_when(. == -6999 ~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate(location = 'loon')

buoy2007_temp_vert <- buoy2007_L1 %>%
  select(datetime, alltemp2007) %>%
  gather(variable, value, -datetime) %>%
  mutate(variable = factor(variable, levels=alltemp2007))
 
ggplot(subset(buoy2007_temp_vert, 
              subset=(datetime >=as.POSIXct('2007-01-01', tz='UTC') & 
                        datetime < as.POSIXct('2008-01-01', tz='UTC'))), 
       aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  labs(title = '2007 buoy data, NAs recoded',
       x=NULL,
       y='temp (deg C)') +
  final_theme +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
                              "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust

ggplot(subset(buoy2007_temp_vert, 
              subset=(datetime >=as.POSIXct('2007-08-01', tz='UTC') & 
                        datetime < as.POSIXct('2007-10-01', tz='UTC'))), 
       aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  labs(title = 'Aug/Sept 2007 buoy data, NAs recoded',
       x=NULL,
       y='temp deg C') +
  final_theme +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
                              "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) + #so you can adjust
  scale_x_datetime(date_minor_breaks = ('1 day'))


#buoy deployed 25Aug2007, data begins on the 27th
ggplot(subset(buoy2007_temp_vert, subset=(datetime >=as.POSIXct('2007-08-25', tz='UTC') & datetime < as.POSIXct('2007-08-30', tz='UTC'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  labs(title = 'Aug/Sept 2007 buoy data, NAs recoded',
       x='date',
       y='temp deg C') +
  final_theme +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
                              "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) + #so you can adjust
  scale_x_datetime(date_minor_breaks = '1 hour')

#sept 6 buoy visit
ggplot(subset(buoy2007_temp_vert, subset=(datetime >=as.POSIXct('2007-09-06', tz='UTC') & datetime < as.POSIXct('2007-09-07', tz='UTC'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  labs(title = 'Aug/Sept 2007 buoy data, NAs recoded',
       x='date',
       y='temp deg C') +
  final_theme +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
                              "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) + #so you can adjust
  scale_x_datetime(date_minor_breaks = '1 hour')

buoy2007_L1 <- buoy2007_L1 %>% 
  mutate_at(vars(alltemp2007),
            funs(case_when(datetime>=as.POSIXct('2007-09-06 13:30', tz='UTC') & datetime < as.POSIXct('2007-09-06 16:00') ~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate_at(vars(TempC_10m, TempC_11m, TempC_13m),
            funs(case_when(datetime>=as.POSIXct('2007-09-06 16:00', tz='UTC') ~ NA_real_,
                           TRUE ~ .)))
buoy2007_temp_vert_b <- buoy2007_L1 %>%
  select(datetime, alltemp2007) %>%
  gather(variable, value, -datetime) %>%
  mutate(variable = factor(variable, levels=alltemp2007))

ggplot(subset(buoy2007_temp_vert_b, subset=(datetime >=as.POSIXct('2007-08-01', tz='UTC') & datetime < as.POSIXct('2007-10-01', tz='UTC'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  labs(title = 'Aug/Sept 2007 buoy data, clean',
       x='date',
       y='temp deg C') +
  final_theme +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
                              "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) + #so you can adjust
  scale_x_datetime(date_minor_breaks = ('1 day'))

ggplot(subset(buoy2007_temp_vert, subset=(datetime >=as.POSIXct('2007-10-01', tz='UTC') & datetime < as.POSIXct('2007-11-01', tz='UTC'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  labs(title = 'Oct 2007 buoy data, NAs recoded',
       x=NULL,
       y='temp deg C') +
  final_theme +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
                              "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) + #so you can adjust
  scale_x_datetime(date_minor_breaks = ('1 day'))

ggplot(subset(buoy2007_temp_vert, subset=(datetime >=as.POSIXct('2007-10-03', tz='UTC') & datetime < as.POSIXct('2007-10-04', tz='UTC'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  labs(title = 'Oct 2007 buoy data, NAs recoded',
       x=NULL,
       y='temp (deg C)') +
  final_theme +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
                              "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) + #so you can adjust
  scale_x_datetime(date_minor_breaks = ('1 hour'))

ggplot(subset(buoy2007_temp_vert, subset=(datetime >=as.POSIXct('2007-10-05', tz='UTC') & datetime < as.POSIXct('2007-10-06', tz='UTC'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  labs(title = 'Oct 2007 buoy data, NAs recoded',
       x='date',
       y='temp deg C') +
  final_theme +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
                              "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) + #so you can adjust
  scale_x_datetime(date_minor_breaks = ('1 hour'))

#Oct 23 anamolous point
ggplot(subset(buoy2007_temp_vert, subset=(datetime >=as.POSIXct('2007-10-23', tz='UTC') & datetime < as.POSIXct('2007-10-24', tz='UTC'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  labs(title = 'Oct 2007 buoy data, NAs recoded',
       x='date',
       y='temp deg C') +
  final_theme +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
                              "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) + #so you can adjust
  scale_x_datetime(date_minor_breaks = ('1 hour'))

buoy2007_L1 <- buoy2007_L1 %>% 
  mutate(TempC_8m = case_when(datetime==as.POSIXct('2007-10-23 10:50', tz='UTC') ~ NA_real_,
                           TRUE ~ TempC_8m)) 
buoy2007_temp_vert_b <- buoy2007_L1 %>%
  select(datetime, alltemp2007) %>%
  gather(variable, value, -datetime) %>%
  mutate(variable = factor(variable, levels=alltemp2007))

ggplot(subset(buoy2007_temp_vert_b, subset=(datetime >=as.POSIXct('2007-10-01', tz='UTC') & datetime < as.POSIXct('2007-11-01', tz='UTC'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  labs(title = 'Oct 2007 buoy data, clean',
       x=NULL,
       y='temp (deg C)') +
  final_theme +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
                              "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) + #so you can adjust
  scale_x_datetime(date_minor_breaks = ('1 day'))

ggplot(subset(buoy2007_temp_vert, subset=(datetime >=as.POSIXct('2007-11-01', tz='UTC') & datetime < as.POSIXct('2007-12-01', tz='UTC'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  labs(title = 'Nov 2007 buoy data, NAs recoded',
       x=NULL,
       y='temp (deg C)') +
  final_theme +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
                              "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) + #so you can adjust
  scale_x_datetime(date_minor_breaks = ('1 day'))

ggplot(subset(buoy2007_temp_vert_b, subset=(datetime >=as.POSIXct('2007-11-01', tz='UTC') & datetime < as.POSIXct('2007-12-01', tz='UTC'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  labs(title = 'Nov 2007 buoy data, clean',
       x=NULL,
       y='temp (deg C)') +
  final_theme +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
                              "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) + #so you can adjust
  scale_x_datetime(date_minor_breaks = ('1 day'))

ggplot(subset(buoy2007_temp_vert, subset=(datetime >=as.POSIXct('2007-12-01', tz='UTC') & datetime < as.POSIXct('2008-01-01', tz='UTC'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  labs(title = 'Dec 2007 buoy data, NAs recoded',
       x=NULL,
       y='temp (deg C)') +
  final_theme +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
                              "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) + #so you can adjust
  scale_x_datetime(date_minor_breaks = ('1 day'))

ggplot(subset(buoy2007_temp_vert, subset=(datetime >=as.POSIXct('2007-12-05', tz='UTC') & datetime < as.POSIXct('2007-12-06', tz='UTC'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  labs(title = 'Dec 2007 buoy data, NAs recoded',
       x='date',
       y='temp deg C') +
  final_theme +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
                              "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) + #so you can adjust
  scale_x_datetime(date_minor_breaks = ('1 hour'))

#dec 5 18:20 all data anomolous
buoy2007_L1 <- buoy2007_L1 %>% 
  mutate(TempC_8m = case_when(datetime == as.POSIXct('2007-12-05 18:20', tz='UTC') ~ NA_real_,
                           TRUE ~ TempC_8m))
buoy2007_temp_vert_b <- buoy2007_L1 %>%
  select(datetime, alltemp2007) %>%
  gather(variable, value, -datetime) %>%
  mutate(variable = factor(variable, levels=alltemp2007))

ggplot(subset(buoy2007_temp_vert_b, subset=(datetime >=as.POSIXct('2007-12-01', tz='UTC') & datetime < as.POSIXct('2008-01-01', tz='UTC'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  labs(title = 'Dec 2007 buoy data, clean',
       x=NULL,
       y='temp (deg C)') +
  final_theme +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
                              "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) + #so you can adjust
  scale_x_datetime(date_minor_breaks = ('1 day'))

ggplot(subset(buoy2007_temp_vert_b, subset=(datetime >=as.POSIXct('2007-01-01', tz='UTC') & datetime < as.POSIXct('2008-01-01', tz='UTC'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  labs(title = '2007 buoy data, clean',
       x=NULL,
       y='temp (deg C)') +
  final_theme +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
                              "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) + #so you can adjust
  scale_x_datetime(date_minor_breaks = ('1 month'))

#reality check with LMP
buoy_LMP_2007 <- buoy2007_temp_vert_b %>%
  mutate(source='buoy') %>%
  full_join(., LMP2007_temp) %>% 
  mutate(variable = factor(variable, levels = alltemp2007LMP))
unique(LMP2007_temp$datetime)

#June 11, Jul 16, aug 14, sept 11
ggplot(subset(buoy_LMP_2007,
              subset=(datetime>=as.POSIXct('2007-06-11', tz='UTC') &
                        datetime<as.POSIXct('2007-06-12', tz='UTC'))),
       aes(x=datetime, y=value, color=as.factor(variable), shape = source)) +
  geom_point() +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
                              "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust

ggplot(subset(buoy_LMP_2007,
              subset=(datetime>=as.POSIXct('2007-07-16', tz='UTC') &
                        datetime<as.POSIXct('2007-07-17', tz='UTC'))),
       aes(x=datetime, y=value, color=as.factor(variable), shape = source)) +
  geom_point() +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
                              "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust

ggplot(subset(buoy_LMP_2007,
              subset=(datetime>=as.POSIXct('2007-08-14', tz='UTC') &
                        datetime<as.POSIXct('2007-08-15', tz='UTC'))),
       aes(x=datetime, y=value, color=as.factor(variable), shape = source)) +
  geom_point() +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
                              "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust

ggplot(subset(buoy_LMP_2007,
              subset=(datetime>=as.POSIXct('2007-09-11', tz='UTC') &
                        datetime<as.POSIXct('2007-09-12', tz='UTC'))),
       aes(x=datetime, y=value, color=variable, shape = source)) +
  geom_point() +
  final_theme +
  labs(title = 'temp reality check\n2007 buoy and LMP site 210 data\nSeptember 11, 2007',
       x=NULL,
       y='temp (deg C)') +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
                              "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust

#export L1 tempstring file
buoy2007_L1 %>%
  select(datetime, TempC_0m, TempC_0p5m, TempC_1m, TempC_1p5m, TempC_2m, TempC_2p5m, TempC_3m, TempC_4m, TempC_5m, TempC_6m, TempC_7m, TempC_8m, TempC_9m, TempC_10m, TempC_11m, TempC_13m) %>%
  mutate(datetime = as.character(datetime)) %>%
  write_csv(., 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2007_tempstring_L1.csv')

#clean up workspace
rm(buoy_LMP_2007, buoy2007_temp_vert, buoy2007_temp_vert_b, LMP2007_temp)


#### DO sensors ####





# buoy2007_temp_vert_DR <- buoy2007_DR %>%
#   select(datetime, alltemp2007p) %>%
#   gather(variable, value, -datetime) %>%
#   mutate(variable = factor(variable, levels=alltemp2007p))
# 
# ggplot(subset(buoy2007_temp_vert_DR, subset=(datetime >=as.POSIXct('2007-01-01', tz='UTC') &
#                                                datetime < as.POSIXct('2008-01-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   final_theme
# 
# buoy2007_temp_comp <- buoy2007_temp_vert_DR %>% 
#   mutate(source = 'DR') %>% 
#   full_join(., buoy2007_temp_vert) %>% 
#   mutate(source = case_when(is.na(source) ~ 'L0',
#                             TRUE ~ source),
#          variable = factor(variable, levels=alltemp2007p))
# 
# ggplot(subset(buoy2007_temp_comp, subset=(datetime >=as.POSIXct('2007-08-01', tz='UTC') & 
#                                             datetime < as.POSIXct('2007-09-01', tz='UTC'))), 
#        aes(x=datetime, y=value, color=variable, size=source, shape=source)) +
#   geom_point() +
#   facet_grid(variable ~. ) +
#   final_theme
# 
# ggplot(subset(buoy2007_temp_comp, subset=(datetime >=as.POSIXct('2007-09-01', tz='UTC') & 
#                                             datetime < as.POSIXct('2007-10-01', tz='UTC'))), 
#        aes(x=datetime, y=value, color=variable, size=source, shape=source)) +
#   geom_point() +
#   facet_grid(variable ~. ) +
#   final_theme
# 
# ggplot(subset(buoy2007_temp_comp, subset=(datetime >=as.POSIXct('2007-10-01', tz='UTC') & 
#                                             datetime < as.POSIXct('2007-11-01', tz='UTC'))), 
#        aes(x=datetime, y=value, color=variable, size=source, shape=source)) +
#   geom_point() +
#   facet_grid(variable ~. ) +
#   final_theme
# 
# ggplot(subset(buoy2007_temp_comp, subset=(datetime >=as.POSIXct('2007-11-01', tz='UTC') & 
#                                             datetime < as.POSIXct('2007-12-01', tz='UTC'))), 
#        aes(x=datetime, y=value, color=variable, size=source, shape=source)) +
#   geom_point() +
#   facet_grid(variable ~. ) +
#   final_theme
# 
# ggplot(subset(buoy2007_temp_comp, subset=(datetime >=as.POSIXct('2007-12-01', tz='UTC') & 
#                                             datetime < as.POSIXct('2008-01-01', tz='UTC'))), 
#        aes(x=datetime, y=value, color=variable, size=source, shape=source)) +
#   geom_point() +
#   facet_grid(variable ~. ) +
#   final_theme
