#*****************************************************************
#*      Cary Institute of Ecosystem Studies (Millbrook, NY)      *
#*                                                               *
#* TITLE:   Sunapee_buoy_2007.r                                  *
#* AUTHOR:  Bethel Steele                                        *
#* SYSTEM:  Lenovo ThinkCentre, Win 10, R 3.5.2, RStudio 1.1.383 *
#* PROJECT: Lake Sunapee Buoy Data Cleaning                      *
#* PURPOSE: subset data for met/compare with L1                  *
#* LAST MODIFIED: 05Sept2019 to create vertical dataset for      *
#*          master collation                                     *
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

# #bring in 2007 LMP data for comparison
# LMP2007 <- read_xlsx('C:/Users/steeleb/Dropbox/Lake Sunapee/long term Sunapee data/raw data/LMP files/LMP 2017/DO.xlsx', 
#                      sheet='DO',
#                      col_types = c('text', 'text', 'numeric', 'guess', 'numeric', 
#                                    'numeric', 'numeric', 'numeric', 'numeric' ,'numeric',
#                                    'text', 'text', 'numeric', 'text')) %>% 
#   mutate(DATE = as.Date(DATE, format='%Y-%m-%d')) %>%  #format date
#   filter(DATE >= '2007-01-01' & DATE < '2008-01-01', #filter for 2016 only
#          STATION == 210)
# 
# LMP2007_temp <- LMP2007 %>% 
#   select(DATE, DEPTH, TEMP) %>% 
#   filter(DEPTH<=14) %>% 
#   rename(variable = 'DEPTH', 
#          value = 'TEMP',
#          datetime = 'DATE') %>% 
#   mutate(source = 'LMP',
#          datetime = as.POSIXct(paste(datetime, '12:00', sep=' '), format = '%Y-%m-%d %H:%M', tz='UTC'),
#          variable = as.factor(variable),
#          variable = case_when(variable == '0.5' ~ 'TempC_0p5m',
#                               variable == '1' ~ 'TempC_1m',
#                               variable == '2' ~ 'TempC_2m',
#                               variable == '3' ~ 'TempC_3m',
#                               variable == '4' ~ 'TempC_4m',
#                               variable == '5' ~ 'TempC_5m',
#                               variable == '6' ~ 'TempC_6m',
#                               variable == '7' ~ 'TempC_7m',
#                               variable == '8' ~ 'TempC_8m',
#                               variable == '9' ~ 'TempC_9m',
#                               variable == '10' ~ 'TempC_10m',
#                               variable == '11' ~ 'TempC_11m',
#                               variable == '12' ~ 'TempC_12m',
#                               variable == '13' ~ 'TempC_13m',
#                               variable == '14' ~ 'TempC_14m'))

#make sure all timestamps present and create L1 dataset
#create dummy timestamp so there are no blanks
alltimes_2007 <- as.data.frame(seq.POSIXt(as.POSIXct('2007-08-27 23:00', tz='UTC'), as.POSIXct('2007-12-31 23:50', tz='UTC'), '10 min')) %>% 
  rename("datetime" = !!names(.[1]))

buoy2007_L1 <- buoy2007_L0 %>% 
  right_join(., alltimes_2007) %>% 
  arrange(datetime)


#### thermisters ####
buoy2007_temp_vert <- buoy2007_L1 %>%
  select(datetime, alltemp2007) %>%
  gather(variable, value, -datetime) %>% 
  mutate(variable = factor(variable, levels = c(alltemp2007)))
  
# ggplot(subset(buoy2007_temp_vert, 
#               subset=(datetime >=as.POSIXct('2007-01-01', tz='UTC') & 
#                         datetime < as.POSIXct('2008-01-01', tz='UTC'))), 
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   labs(title='2007 buoy data - raw',
#        x=NULL,
#        y='temp (deg C)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 month') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust

buoy2007_L1 <- buoy2007_L1 %>% 
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

# ggplot(subset(buoy2007_temp_vert, 
#               subset=(datetime >=as.POSIXct('2007-08-01', tz='UTC') & 
#                         datetime < as.POSIXct('2007-10-01', tz='UTC'))), 
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   labs(title = 'Aug/Sept 2007 buoy data, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   final_theme +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) + #so you can adjust
#   scale_x_datetime(date_minor_breaks = ('1 day'))
# 
# 
# #buoy deployed 25Aug2007, data begins on the 27th
# ggplot(subset(buoy2007_temp_vert, subset=(datetime >=as.POSIXct('2007-08-25', tz='UTC') & datetime < as.POSIXct('2007-08-30', tz='UTC'))), aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   labs(title = 'Aug/Sept 2007 buoy data, NAs recoded',
#        x='date',
#        y='temp deg C') +
#   final_theme +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) + #so you can adjust
#   scale_x_datetime(date_minor_breaks = '1 hour')
# 
# #sept 6 buoy visit
# ggplot(subset(buoy2007_temp_vert, subset=(datetime >=as.POSIXct('2007-09-06', tz='UTC') & datetime < as.POSIXct('2007-09-07', tz='UTC'))), aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   labs(title = 'Aug/Sept 2007 buoy data, NAs recoded',
#        x='date',
#        y='temp deg C') +
#   final_theme +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) + #so you can adjust
#   scale_x_datetime(date_minor_breaks = '1 hour')

buoy2007_L1 <- buoy2007_L1 %>% 
  mutate_at(vars(alltemp2007),
            funs(case_when(datetime>=as.POSIXct('2007-09-06 13:30', tz='UTC') & datetime < as.POSIXct('2007-09-06 16:00') ~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate(temp_flag = case_when(datetime>=as.POSIXct('2007-09-06 16:00', tz='UTC') ~ '9.5d, 10.5d, 11.5d, 13.5d',
                           TRUE ~ ''))
buoy2007_temp_vert_b <- buoy2007_L1 %>%
  select(datetime, alltemp2007) %>%
  gather(variable, value, -datetime) %>%
  mutate(variable = factor(variable, levels=alltemp2007))

# ggplot(subset(buoy2007_temp_vert_b, subset=(datetime >=as.POSIXct('2007-08-01', tz='UTC') & datetime < as.POSIXct('2007-10-01', tz='UTC'))), aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   labs(title = 'Aug/Sept 2007 buoy data, clean',
#        x='date',
#        y='temp deg C') +
#   final_theme +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) + #so you can adjust
#   scale_x_datetime(date_minor_breaks = ('1 day'))
# 
# ggplot(subset(buoy2007_temp_vert, subset=(datetime >=as.POSIXct('2007-10-01', tz='UTC') & datetime < as.POSIXct('2007-11-01', tz='UTC'))), aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   labs(title = 'Oct 2007 buoy data, NAs recoded',
#        x=NULL,
#        y='temp deg C') +
#   final_theme +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) + #so you can adjust
#   scale_x_datetime(date_minor_breaks = ('1 day'))
# 
# ggplot(subset(buoy2007_temp_vert, subset=(datetime >=as.POSIXct('2007-10-03', tz='UTC') & datetime < as.POSIXct('2007-10-04', tz='UTC'))), aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   labs(title = 'Oct 2007 buoy data, NAs recoded',
#        x=NULL,
#        y='temp (deg C)') +
#   final_theme +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) + #so you can adjust
#   scale_x_datetime(date_minor_breaks = ('1 hour'))
# 
# ggplot(subset(buoy2007_temp_vert, subset=(datetime >=as.POSIXct('2007-10-05', tz='UTC') & datetime < as.POSIXct('2007-10-06', tz='UTC'))), aes(x=datetime, y=value, color=variable)) +
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
# ggplot(subset(buoy2007_temp_vert, subset=(datetime >=as.POSIXct('2007-10-23', tz='UTC') & datetime < as.POSIXct('2007-10-24', tz='UTC'))), aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   labs(title = 'Oct 2007 buoy data, NAs recoded',
#        x='date',
#        y='temp deg C') +
#   final_theme +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) + #so you can adjust
#   scale_x_datetime(date_minor_breaks = ('1 hour'))

buoy2007_L1 <- buoy2007_L1 %>% 
  mutate(TempC_8m = case_when(datetime==as.POSIXct('2007-10-23 10:50', tz='UTC') ~ NA_real_,
                           TRUE ~ TempC_8m)) 
buoy2007_temp_vert_b <- buoy2007_L1 %>%
  select(datetime, alltemp2007) %>%
  gather(variable, value, -datetime) %>%
  mutate(variable = factor(variable, levels=alltemp2007))

# ggplot(subset(buoy2007_temp_vert_b, subset=(datetime >=as.POSIXct('2007-10-01', tz='UTC') & datetime < as.POSIXct('2007-11-01', tz='UTC'))), aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   labs(title = 'Oct 2007 buoy data, clean',
#        x=NULL,
#        y='temp (deg C)') +
#   final_theme +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) + #so you can adjust
#   scale_x_datetime(date_minor_breaks = ('1 day'))
# 
# ggplot(subset(buoy2007_temp_vert, subset=(datetime >=as.POSIXct('2007-11-01', tz='UTC') & datetime < as.POSIXct('2007-12-01', tz='UTC'))), aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   labs(title = 'Nov 2007 buoy data, NAs recoded',
#        x=NULL,
#        y='temp (deg C)') +
#   final_theme +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) + #so you can adjust
#   scale_x_datetime(date_minor_breaks = ('1 day'))
# 
# ggplot(subset(buoy2007_temp_vert_b, subset=(datetime >=as.POSIXct('2007-11-01', tz='UTC') & datetime < as.POSIXct('2007-12-01', tz='UTC'))), aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   labs(title = 'Nov 2007 buoy data, clean',
#        x=NULL,
#        y='temp (deg C)') +
#   final_theme +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) + #so you can adjust
#   scale_x_datetime(date_minor_breaks = ('1 day'))
# 
# ggplot(subset(buoy2007_temp_vert, subset=(datetime >=as.POSIXct('2007-12-01', tz='UTC') & datetime < as.POSIXct('2008-01-01', tz='UTC'))), aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   labs(title = 'Dec 2007 buoy data, NAs recoded',
#        x=NULL,
#        y='temp (deg C)') +
#   final_theme +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) + #so you can adjust
#   scale_x_datetime(date_minor_breaks = ('1 day'))
# 
# ggplot(subset(buoy2007_temp_vert, subset=(datetime >=as.POSIXct('2007-12-05', tz='UTC') & datetime < as.POSIXct('2007-12-06', tz='UTC'))), aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   labs(title = 'Dec 2007 buoy data, NAs recoded',
#        x='date',
#        y='temp deg C') +
#   final_theme +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) + #so you can adjust
#   scale_x_datetime(date_minor_breaks = ('1 hour'))

#dec 5 18:20 all data anomolous
buoy2007_L1 <- buoy2007_L1 %>% 
  mutate(TempC_8m = case_when(datetime == as.POSIXct('2007-12-05 18:20', tz='UTC') ~ NA_real_,
                           TRUE ~ TempC_8m))
buoy2007_temp_vert_b <- buoy2007_L1 %>%
  select(datetime, alltemp2007) %>%
  gather(variable, value, -datetime) %>%
  mutate(variable = factor(variable, levels=alltemp2007))

# ggplot(subset(buoy2007_temp_vert_b, subset=(datetime >=as.POSIXct('2007-12-01', tz='UTC') & datetime < as.POSIXct('2008-01-01', tz='UTC'))), aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   labs(title = 'Dec 2007 buoy data, clean',
#        x=NULL,
#        y='temp (deg C)') +
#   final_theme +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) + #so you can adjust
#   scale_x_datetime(date_minor_breaks = ('1 day'))

ggplot(subset(buoy2007_temp_vert_b, subset=(datetime >=as.POSIXct('2007-01-01', tz='UTC') & datetime < as.POSIXct('2008-01-01', tz='UTC'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  labs(title = '2007 buoy data, clean',
       x=NULL,
       y='temp (deg C)') +
  final_theme +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
                              "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) + #so you can adjust
  scale_x_datetime(date_minor_breaks = ('1 month'))

# #reality check with LMP
# buoy_LMP_2007 <- buoy2007_temp_vert_b %>%
#   mutate(source='buoy') %>%
#   full_join(., LMP2007_temp) %>% 
#   mutate(variable = factor(variable, levels = alltemp2007LMP))
# unique(LMP2007_temp$datetime)
# 
# #June 11, Jul 16, aug 14, sept 11
# ggplot(subset(buoy_LMP_2007,
#               subset=(datetime>=as.POSIXct('2007-06-11', tz='UTC') &
#                         datetime<as.POSIXct('2007-06-12', tz='UTC'))),
#        aes(x=datetime, y=value, color=as.factor(variable), shape = source)) +
#   geom_point() +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# ggplot(subset(buoy_LMP_2007,
#               subset=(datetime>=as.POSIXct('2007-07-16', tz='UTC') &
#                         datetime<as.POSIXct('2007-07-17', tz='UTC'))),
#        aes(x=datetime, y=value, color=as.factor(variable), shape = source)) +
#   geom_point() +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# ggplot(subset(buoy_LMP_2007,
#               subset=(datetime>=as.POSIXct('2007-08-14', tz='UTC') &
#                         datetime<as.POSIXct('2007-08-15', tz='UTC'))),
#        aes(x=datetime, y=value, color=as.factor(variable), shape = source)) +
#   geom_point() +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# ggplot(subset(buoy_LMP_2007,
#               subset=(datetime>=as.POSIXct('2007-09-11', tz='UTC') &
#                         datetime<as.POSIXct('2007-09-12', tz='UTC'))),
#        aes(x=datetime, y=value, color=variable, shape = source)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'temp reality check\n2007 buoy and LMP site 210 data\nSeptember 11, 2007',
#        x=NULL,
#        y='temp (deg C)') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 

#clean up workspace
rm(buoy_LMP_2007, buoy2007_temp_vert, buoy2007_temp_vert_b, LMP2007_temp)


#### DO sensors ####
range(buoy2007_L1$DOSat, na.rm=T)
range(buoy2007_L1$DOppm, na.rm=T)
range(buoy2007_L1$DOTempC, na.rm=T)

do_vert <- buoy2007_L1 %>% 
  select(datetime, upDO) %>% 
  gather(variable, value, -datetime)

ggplot(do_vert, aes(x = datetime, y = value)) +
  geom_point() +
  facet_grid(variable~., scales = 'free_y') +
  final_theme +
  labs(title = '2007 DO data raw',
       x = NULL,
       y = NULL) +
  scale_x_datetime(date_minor_breaks = '1 month')

# ggplot(subset(do_vert, 
#               subset = (datetime >= as.POSIXct('2007-08-25', tz='UTC') & 
#                           datetime < as.POSIXct('2007-10-01', tz='UTC'))),
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
#               subset = (datetime >= as.POSIXct('2007-10-01', tz='UTC') & 
#                           datetime < as.POSIXct('2007-11-01', tz='UTC'))),
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
#               subset = (datetime >= as.POSIXct('2007-11-01', tz='UTC') & 
#                           datetime < as.POSIXct('2007-12-01', tz='UTC'))),
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
#               subset = (datetime >= as.POSIXct('2007-12-01', tz='UTC') & 
#                           datetime < as.POSIXct('2008-01-01', tz='UTC'))),
#        aes(x = datetime, y = value)) +
#   geom_point() +
#   facet_grid(variable~., scales = 'free_y') +
#   final_theme +
#   labs(title = 'dec 2007 DO data raw',
#        x = NULL,
#        y = NULL) +
#   scale_x_datetime(date_minor_breaks = '1 day')

buoy2007_L1 <- buoy2007_L1 %>% 
  mutate(upper_do_flag = 'x')

rm(do_vert)

#### wind sensors ####
range(buoy2007_L1$InstWindDir, na.rm = T)
range(buoy2007_L1$InstWindSp, na.rm = T)

wind_vert <- buoy2007_L1 %>% 
  select(datetime, InstWindDir, InstWindSp) %>% 
  gather(variable, value, -datetime)

ggplot(wind_vert, aes(x = datetime, y = value)) +
  geom_point() +
  facet_grid(variable~., scales = 'free_y') +
  final_theme +
  labs(title = '2007 wind data raw',
       x = NULL,
       y = NULL) +
  scale_x_datetime(date_minor_breaks = '1 month')

# ggplot(subset(wind_vert, 
#               subset=(datetime>=as.POSIXct('2007-08-01', tz='UTC') &
#                         datetime<as.POSIXct('2007-09-01', tz='UTC'))), 
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
#               subset=(datetime>=as.POSIXct('2007-09-01', tz='UTC') &
#                         datetime<as.POSIXct('2007-10-01', tz='UTC'))), 
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
#               subset=(datetime>=as.POSIXct('2007-10-01', tz='UTC') &
#                         datetime<as.POSIXct('2007-11-01', tz='UTC'))), 
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
#               subset=(datetime>=as.POSIXct('2007-11-01', tz='UTC') &
#                         datetime<as.POSIXct('2007-12-01', tz='UTC'))), 
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
#               subset=(datetime>=as.POSIXct('2007-12-01', tz='UTC') &
#                         datetime<as.POSIXct('2008-01-01', tz='UTC'))), 
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
#               subset=(datetime>=as.POSIXct('2007-12-31', tz='UTC') &
#                         datetime<as.POSIXct('2008-01-01', tz='UTC'))), 
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
            funs(case_when(datetime >= as.POSIXct('2007-12-31 08:30', tz='UTC') & datetime < as.POSIXct('2007-12-31 14:50', tz='UTC') ~ NA_real_,
                             TRUE ~ .)))
wind_vert_b <- buoy2007_L1 %>% 
  select(datetime, InstWindDir, InstWindSp) %>% 
  gather(variable, value, -datetime)

# ggplot(subset(wind_vert_b, 
#               subset=(datetime>=as.POSIXct('2007-12-01', tz='UTC') &
#                         datetime<as.POSIXct('2008-01-01', tz='UTC'))), 
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

#add errant wind dir flag
buoy2007_L1 <- buoy2007_L1 %>% 
  mutate(wind_dir_flag = 'e')

rm(wind_vert, wind_vert_b)


####PAR####
range(buoy2007_L1$PAR, na.rm = T)

buoy2007_L1 <-  buoy2007_L1 %>% 
  mutate(PAR = case_when(PAR <0 ~ 0,
                         TRUE ~ PAR))

ggplot(buoy2007_L1, aes(x = datetime, y = PAR)) +
  geom_point() +
  final_theme +
  labs(title = '2007 PAR data raw',
       x = NULL,
       y = 'PAR (umol/m2/s)') +
  scale_x_datetime(date_minor_breaks = '1 month')

# ggplot(subset(buoy2007_L1,
#               subset = (datetime>=as.POSIXct('2007-08-01', tz='UTC') &
#                           datetime < as.POSIXct('2007-10-01', tz='UTC'))), 
#               aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'aug/sept 2007 PAR data raw',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2007_L1,
#               subset = (datetime>=as.POSIXct('2007-10-01', tz='UTC') &
#                           datetime < as.POSIXct('2007-11-01', tz='UTC'))), 
#        aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'oct 2007 PAR data raw',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2007_L1,
#               subset = (datetime>=as.POSIXct('2007-11-01', tz='UTC') &
#                           datetime < as.POSIXct('2007-12-01', tz='UTC'))),
#        aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'nov 2007 PAR data raw',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')
# 
# ggplot(subset(buoy2007_L1,
#               subset = (datetime>=as.POSIXct('2007-12-01', tz='UTC') &
#                           datetime < as.POSIXct('2008-01-01', tz='UTC'))),
#        aes(x = datetime, y = PAR)) +
#   geom_point() +
#   final_theme +
#   labs(title = 'dec 2007 PAR data raw',
#        x = NULL,
#        y = 'PAR (umol/m2/s)') +
#   scale_x_datetime(date_minor_breaks = '1 day')


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



#### EXPORT L1 FILES ####

#add offline to buoy location
buoy2007_L1 <- buoy2007_L1 %>% 
  mutate(location = case_when(datetime >= as.POSIXct('2007-10-03 13:30', tz='UTC') &
                                datetime < as.POSIXct('2007-10-05 1:50') ~ 'offline',
                              datetime >= as.POSIXct('2007-12-13 15:10', tz='UTC') &
                                datetime < as.POSIXct('2007-12-20 2:20') ~ 'offline',
                              TRUE ~ location)) %>% 
  mutate_at(vars(temp_flag, upper_do_flag, wind_dir_flag),
            funs(case_when(location == 'offline' ~ '',
                           TRUE ~ .)))

#export L1 tempstring file
buoy2007_L1 %>%
  select(datetime, location, TempC_0m, TempC_0p5m, TempC_1m, TempC_1p5m, TempC_2m, TempC_2p5m, TempC_3m, TempC_4m, TempC_5m, TempC_6m, TempC_7m, TempC_8m, TempC_9m, TempC_10m, TempC_11m, TempC_13m, temp_flag) %>%
  rename(TempC_13p5m = 'TempC_13m',
         TempC_11p5m = 'TempC_11m',
         TempC_10p5m = 'TempC_10m',
         TempC_9p5m = 'TempC_9m',
         TempC_8p5m = 'TempC_8m',
         TempC_7p5m = 'TempC_7m',
         TempC_6p5m = 'TempC_6m',
         TempC_5p5m = 'TempC_5m',
         TempC_4p5m = 'TempC_4m',
         TempC_3p5m = 'TempC_3m',
         TempC_3m = 'TempC_2p5m',
         TempC_2p5m = 'TempC_2m',
         TempC_2m = 'TempC_1p5m',
         TempC_1p5m = 'TempC_1m',
         TempC_1m = 'TempC_0p5m',
         TempC_0p5m = 'TempC_0m') %>%
  mutate(datetime = as.character(datetime)) %>%
  write_csv(., 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2007_tempstring_L1.csv')

#crete vertical dataset
buoy_2007_L1_vert <- buoy2007_L1 %>%
  select(datetime, location, TempC_0m, TempC_0p5m, TempC_1m, TempC_1p5m, TempC_2m, TempC_2p5m, TempC_3m, TempC_4m, TempC_5m, TempC_6m, TempC_7m, TempC_8m, TempC_9m, TempC_10m, TempC_11m, TempC_13m, temp_flag) %>%
  rename(TempC_13p5m = 'TempC_13m',
         TempC_11p5m = 'TempC_11m',
         TempC_10p5m = 'TempC_10m',
         TempC_9p5m = 'TempC_9m',
         TempC_8p5m = 'TempC_8m',
         TempC_7p5m = 'TempC_7m',
         TempC_6p5m = 'TempC_6m',
         TempC_5p5m = 'TempC_5m',
         TempC_4p5m = 'TempC_4m',
         TempC_3p5m = 'TempC_3m',
         TempC_3m = 'TempC_2p5m',
         TempC_2p5m = 'TempC_2m',
         TempC_2m = 'TempC_1p5m',
         TempC_1p5m = 'TempC_1m',
         TempC_1m = 'TempC_0p5m',
         TempC_0p5m = 'TempC_0m') %>%
  gather(depth_m, temp_degC, -datetime, -location, -temp_flag) %>% 
  mutate(depth_m = case_when(grepl(pattern = '_0p5m', x = depth_m) ~ '0.5',
                           grepl(pattern ='_1m', x = depth_m) ~ '1',
                           grepl(pattern = '_1p5m', x = depth_m) ~ '1.5',
                           grepl(pattern = '_2m', x = depth_m) ~ '2',
                           grepl(pattern = '_2p5m', x = depth_m) ~ '2.5',
                           grepl(pattern = '_3m', x = depth_m) ~ '3',
                           grepl(pattern = '_3p5m', x = depth_m) ~ '3.5',
                           grepl(pattern = '_4p5m', x = depth_m) ~ '4.5',
                           grepl(pattern = '_5p5m', x = depth_m) ~ '5.5',
                           grepl(pattern = '_6p5m', x = depth_m) ~ '6.5',
                           grepl(pattern = '_7p5m', x = depth_m) ~ '7.5',
                           grepl(pattern = '_8p5m', x = depth_m) ~ '8.5',
                           grepl(pattern = '_9p5m', x = depth_m) ~ '9.5',
                           grepl(pattern = '_10p5m', x = depth_m) ~ '10.5',
                           grepl(pattern = '_11p5m', x = depth_m) ~ '11.5',
                           grepl(pattern = '_13p5m', x = depth_m) ~ '13.5',
                           TRUE ~ NA_character_)) %>% 
  mutate(depth_m = as.numeric(depth_m))

#parse out flags
unique(buoy_2007_L1_vert$temp_flag)

buoy_2007_L1_vert <- buoy_2007_L1_vert %>% 
  mutate(temp_flag = case_when(temp_flag != '' & (depth_m == 9.5 | depth_m == 10.5 | depth_m == 11.5 | depth_m == 13.5) & !is.na(temp_degC) ~ 'd',
                               TRUE ~ NA_character_))

#add flags to 11.5-13.5 as possibly in sediment
buoy_2007_L1_vert <-   buoy_2007_L1_vert %>% 
  mutate(temp_flag = case_when(!is.na(temp_degC) & depth_m >= 11.5 & is.na(temp_flag) ~ 'b',
                               !is.na(temp_degC) & depth_m >= 11.5 & !is.na(temp_flag) ~ paste(temp_flag, 'b', sep = ', '),
                               TRUE ~ temp_flag))
                                                                                              
unique(buoy_2007_L1_vert$temp_flag)                               

# #plot to check
# ggplot(buoy_2007_L1_vert, aes(x = datetime, y = temp_degC, color = as.factor(depth_m))) +
#   geom_point() +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
# 
# ggplot(buoy_2007_L1_vert, aes(x = datetime, y = temp_degC, color = as.factor(depth_m))) +
#   geom_point(aes(shape = temp_flag)) +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
  
#order by date, depth
buoy_2007_L1_vert <- buoy_2007_L1_vert %>% 
  arrange(datetime, depth_m)

# buoy_2007_L1_vert %>%
#   mutate(datetime = as.character(datetime)) %>%
#   write_csv(., 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2007_tempstring_vert_L1.csv')
# 
# # export L1 DO file
# buoy2007_L1 %>%
#   select(datetime, location, DOSat, DOppm, DOTempC, upper_do_flag) %>%
#   mutate(datetime = as.character(datetime)) %>%
#   write_csv(., 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2007_do_L1.csv')
# 
# # export wind data
# buoy2007_L1 %>%
#   select(datetime, location, InstWindDir, InstWindSp, wind_dir_flag) %>%
#   mutate(datetime = as.character(datetime)) %>%
#   rename(WindSp_ms = 'InstWindSp',
#          WindDir_deg = 'InstWindDir') %>%
#   write_csv(., 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2007_wind_L1.csv')
# 
# # export PAR data
# buoy2007_L1 %>%
#   select(datetime, location, PAR) %>%
#   mutate(datetime = as.character(datetime)) %>%
#   rename(PAR_umolm2s = 'PAR') %>%
#   write_csv(., 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2007_PAR_L1.csv')
# 
# # export air temp data
# buoy2007_L1 %>%
#   select(datetime, location, AirTempC) %>%
#   mutate(datetime = as.character(datetime)) %>%
#   rename(AirTemp_degC = 'AirTempC') %>%
#   write_csv(., 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2007_AirTemp_L1.csv')
