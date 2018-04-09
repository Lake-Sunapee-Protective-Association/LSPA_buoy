#*****************************************************************
#*      Cary Institute of Ecosystem Studies (Millbrook, NY)      *
#*                                                               *
#* TITLE:   Sunapee_buoy_2014.r                                  *
#* AUTHOR:  Bethel Steele                                        *
#* SYSTEM:  Lenovo ThinkCentre, Win 10, R 3.4.2, RStudio 1.1.383 *
#* DATE:    09Oct2017                                            *
#* PROJECT: Lake Sunapee Buoy Data Cleaning                      *
#* PURPOSE: create L0 and L1 data for buoy data 2014 using       *
#*          similar methods to CCC and DR                        *
#*****************************************************************


#bring in 2014 buoy raw data
buoy2014_L0 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L0/2014 Buoy Data.csv', 
                     col_types = 'iiiinnnnnnnnnnnnnnnnnnnnnnnnnnnnnn') %>% 
  rename(Hr.Min = 'Hr/Min')


#bring in 2014 LMP data for comparison
LMP2014 <- read_xlsx('C:/Users/steeleb/Dropbox/Lake Sunapee/long term Sunapee data/raw data/LMP files/LMP 2015/LMPDO_2015.xlsx', sheet='LMPDO') %>% 
  mutate(DATE = as.Date(DATE, format='%Y-%m-%d')) %>% #format date
  filter(DATE>='2014-01-01' & DATE<'2015-01-01',  #subset for 2014 data only
         STATION==210) #subset for 210 only (closest to buoy)
str(LMP2014)
#fix time on 2014-05-27
ix=which(LMP2014$DATE=='2014-05-27')
LMP2014$TIME[ix]=900


####format data####
# format date and time in buoy data
buoy2014_L0 <- buoy2014_L0 %>% 
  mutate(hour = Hr.Min%/%100,
         minutes = Hr.Min%%100,
         minutes = replace(minutes, minutes=='0', '00'),
         time = paste(hour, minutes, sep=':')) %>% #break out time from Hr.Min, create time column
  mutate(date = as.Date(paste(Day, Year, sep = '-'), format='%j-%Y'), #create date in ymd format
         datetime = as.POSIXct(paste(date, time, sep=' '), format='%Y-%m-%d %H:%M', tz='UTC')) %>%  #create datetime stamp must state UTC so that daylight savings doesn't get messy
  select(-hour, -minutes, -Hr.Min, -ArrayID, -Year, -Day, -time, -date, -LoggerBatV, -RadioBatV, -IntLgBxTempC) #remove unnecessary columns
  
str(buoy2014_L0)

# format date and time in LMP data
LMP2014 <- LMP2014 %>% 
  mutate(hour = TIME%/%100,
         minutes = TIME%%100,
         hour = replace(hour, hour==1, 13), #force data in 24h time
         minutes = replace(minutes, minutes=='0', '00'), #force into two digits
         time = paste(hour, minutes, sep=':')) %>% #break out time from TIME, create time column
  select(-hour, -minutes, -TIME) %>% #remove unnecessary columns
  mutate(datetime = as.POSIXct(paste(DATE, time, sep=' '), format='%Y-%m-%d %H:%M', tz='UTC')) #create datetime stamp must state UTC so that daylight savings doesn't get messy
str(LMP2014)

#subset LMP for truthing data
LMP2014_temp <- LMP2014 %>% 
  select(datetime, DEPTH, TEMP) %>% 
  filter(DEPTH <= 12) %>% 
  rename(value = 'TEMP') %>% 
  mutate(variable = case_when(DEPTH == 0.5 ~ 'TempC_0p5m',
                           DEPTH == 1 ~ 'TempC_1m',
                           DEPTH == 2 ~ 'TempC_2m',
                           DEPTH == 3 ~ 'TempC_3m',
                           DEPTH == 4 ~ 'TempC_4m',
                           DEPTH == 5 ~ 'TempC_5m',
                           DEPTH == 6 ~ 'TempC_6m',
                           DEPTH == 7 ~ 'TempC_7m',
                           DEPTH == 8 ~ 'TempC_8m',
                           DEPTH == 9 ~ 'TempC_9m',
                           DEPTH == 10 ~ 'TempC_10m',
                           DEPTH == 11 ~ 'TempC_11m',
                           DEPTH == 12 ~ 'TempC_12m',
                           TRUE ~ ''),
         source = 'LMP')

LMP2014_upDO <- LMP2014 %>% 
  select(datetime, DEPTH, DO, PCNTSAT) %>% 
  filter(DEPTH<=2) %>% 
  rename(depth = 'DEPTH',
         DOppm = 'DO', 
         DOSat = 'PCNTSAT') %>% 
  gather(variable, value, -datetime, -depth) %>% 
  mutate(source='LMP')
LMP2014_lowDO <- LMP2014 %>% 
  select(datetime, DEPTH, DO, PCNTSAT) %>% 
  filter(DEPTH>=9 & DEPTH<=12) %>% 
  rename(depth = 'DEPTH',
         DOLowPPM = 'DO', 
         DOLowSat = 'PCNTSAT') %>% 
  gather(variable, value, -datetime, -depth) %>% 
  mutate(source='LMP')


####plot thermister data####
buoy2014_L1 <- buoy2014_L0

#initial data visualization
buoy2014_vert_temp <- buoy2014_L1 %>%
  select(datetime, alltemp) %>%
  gather(variable, value, -datetime) %>%
  mutate(variable = factor(variable, levels = alltemp))

ggplot(subset(buoy2014_vert_temp, subset=(datetime>='2014-01-01' & 
                                            datetime < '2015-01-01')), 
       aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='2014 thermisters - raw', 
       x=NULL, 
       y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  final_theme

#buoy moved to loon June 9, 2014 16:00, thermisters active

#data gap early July

#buoy moved to harbor Oct 14 10:20, thermisters inactive


####2014 thermisters (and DO temp) - remove/replace NA values ####
#na values are -6999, 555.4 and 0 in thermisters
buoy2014_L1 <- buoy2014_L1 %>% 
  mutate_at(vars(TempC_0m:TempC_10m, DOTempC, DOLoTempC), 
            funs(case_when(.==-6999 ~ NA_real_, 
                           .==555.4 ~ NA_real_, 
                           .==0 ~ NA_real_, 
                           TRUE ~ .)))

#### L0.5 plot - no NA values of -6999, 0 or 555.4
buoy2014_vert_temp <- buoy2014_L1 %>%
  select(datetime, alltemp) %>%
  gather(variable, value, -datetime) %>%
  mutate(variable = factor(variable, levels=c(alltemp)))

ggplot(subset(buoy2014_vert_temp, subset=(datetime>='2014-01-01' & datetime < '2015-01-01')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='2014 thermisters - NA values recoded', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  final_theme


#buoy deployed June 9, 2014 16:00
ggplot(subset(buoy2014_vert_temp, subset=(datetime>='2014-06-01' & datetime < '2014-07-01')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='June 2014', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  final_theme


#no data earlyJuly

ggplot(subset(buoy2014_vert_temp, subset=(datetime>='2014-07-01' & datetime < '2014-08-01')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='July 2014', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  final_theme

ggplot(subset(buoy2014_vert_temp, subset=(datetime>='2014-08-01' & datetime < '2014-09-01')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='August 2014', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  final_theme

ggplot(subset(buoy2014_vert_temp, subset=(datetime>='2014-09-01' & datetime < '2014-10-01')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='Sept 2014', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  final_theme

ggplot(subset(buoy2014_vert_temp, subset=(datetime>='2014-10-01' & datetime < '2014-11-01')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='October 2014', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  final_theme

#buoy moved to harbor Oct 14 10:20



####2014 thermisters - compare with LMP####
# buoy2014_vert_temp_L1 <- buoy2014_L1 %>% 
#   select(datetime, TempC_0m, TempC_1m, TempC_2m, TempC_3m, TempC_4m, TempC_5m, TempC_6m, TempC_7m, TempC_8m, TempC_9m, TempC_10m) %>% 
#   gather(variable, value, -datetime) %>% 
#   separate(variable, into=c('sensor', 'depth'), sep='_', remove=F) %>% 
#   mutate(depth = as.numeric(str_extract(depth, '\\d')),
#          value = as.numeric(value))
#   
# 
# buoy2014_temp_LMP_L1 <- buoy2014_vert_temp_L1 %>% 
#   full_join(., LMP2014_temp, by=c('datetime' = 'datetime', 'depth' = 'DEPTH')) %>% 
#   mutate(depth = factor(depth, levels=c('0', '0.5', '1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12')),
#          variable = factor(variable, levels=c('TempC_0m', 'TempC_1m', 'TempC_2m', 'TempC_3m', 'TempC_4m', 'TempC_5m', 'TempC_6m', 'TempC_7m', 'TempC_8m', 'TempC_9m', 'TempC_10m')))
# 
# 
# #plot buoy temp against LMP sonde values
# ggplot(subset(buoy2014_temp_LMP_L1, subset=(datetime>='2014-06-08' & datetime < '2014-06-10')), aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='June 9 buoy and LMP', x='date', y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   geom_point(aes(x=datetime, y=TEMP, shape=depth), color='black', size=4) +
#   scale_shape_manual(values=1:nlevels(buoy2014_temp_LMP_L1$depth))+
#   final_theme
# 
# ggplot(subset(buoy2014_temp_LMP_L1, subset=(datetime>='2014-08-18' & datetime < '2014-08-20')), aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='August 19 buoy and LMP', x='date', y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   geom_point(aes(x=datetime, y=TEMP, shape=depth), color='black', size=4) +
#   scale_shape_manual(values=1:nlevels(buoy2014_temp_LMP_L1$depth))+
#   final_theme
# 
# ggplot(subset(buoy2014_temp_LMP_L1, subset=(datetime>='2014-09-14' & datetime < '2014-09-16')), aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='September 15 buoy and LMP', x='date', y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   geom_point(aes(x=datetime, y=TEMP, shape=depth), color='black', size=4) +
#   scale_shape_manual(values=1:nlevels(buoy2014_temp_LMP_L1$depth))+
#   final_theme


####2014 thermisters - compare with do temp####
# buoy2014_lodotemp_L1 <- buoy2014_L1 %>% 
#   select(datetime, DOLoTempC) %>% 
#   mutate(depth = 10,
#          source = 'do sensor')
# 
# buoy2014_temp_lodo_L1 <- buoy2014_vert_temp_L1 %>% 
#   full_join(., buoy2014_lodotemp_L1, by=c('datetime', 'depth')) %>% 
#   mutate(depth = factor(depth, levels=c('0', '0.5', '1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12')),
#          variable = factor(variable, levels=c('TempC_0m', 'TempC_1m', 'TempC_2m', 'TempC_3m', 'TempC_4m', 'TempC_5m', 'TempC_6m', 'TempC_7m', 'TempC_8m', 'TempC_9m', 'TempC_10m')))
# 
# #plot buoy temp against do temps
# ggplot(subset(buoy2014_temp_lodo_L1, subset=(datetime>='2014-06-01' & datetime < '2014-10-15')), aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='2014 thermistors and lo do temp', x='date', y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   geom_point(aes(x=datetime, y=DOLoTempC), color='red') +
#   coord_cartesian(ylim=c(5,30)) +
#   final_theme

####2014 thermisters - clean data####
# buoy2014_vert_temp_L1 <- buoy2014_L1 %>% 
#   select(datetime, TempC_0m, TempC_1m, TempC_2m, TempC_3m, TempC_4m, TempC_5m, TempC_6m, TempC_7m, TempC_8m, TempC_9m, TempC_10m) %>% 
#   gather(variable, value, -datetime) %>% 
#   separate(variable, into=c('sensor', 'depth'), sep='_', remove=F) %>% 
#   mutate(depth = as.numeric(str_extract(depth, '\\d')),
#          value = as.numeric(value),
#          depth = factor(depth, levels=c('0', '0.5', '1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12')),
#          variable = factor(variable, levels=c('TempC_0m', 'TempC_1m', 'TempC_2m', 'TempC_3m', 'TempC_4m', 'TempC_5m', 'TempC_6m', 'TempC_7m', 'TempC_8m', 'TempC_9m', 'TempC_10m')))
# 
# 
# #July 24
# ggplot(subset(buoy2014_vert_temp, subset=(datetime>='2014-07-16' & datetime < '2014-08-01')), aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='late July 2014', x='date', y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme
# 
# ggplot(subset(buoy2014_vert_temp, subset=(datetime>='2014-07-24' & datetime < '2014-07-25')), aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='July 24 2014 buoy visit - L0', x='date', y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme

buoy2014_L1 <- buoy2014_L1 %>% 
  mutate_at(vars(alltemp), 
            funs(case_when(datetime>=as.POSIXct('2014-07-24 9:00', tz='UTC') & datetime<=as.POSIXct('2014-07-24 9:40', tz='UTC') ~ NA_real_,
                           TRUE ~ .)))
# buoy2014_vert_temp_L1 <- buoy2014_L1 %>% 
#   select(datetime, TempC_0m, TempC_1m, TempC_2m, TempC_3m, TempC_4m, TempC_5m, TempC_6m, TempC_7m, TempC_8m, TempC_9m, TempC_10m) %>% 
#   gather(variable, value, -datetime) %>% 
#   separate(variable, into=c('sensor', 'depth'), sep='_', remove=F) %>% 
#   mutate(depth = as.numeric(str_extract(depth, '\\d')),
#          value = as.numeric(value),
#          depth = factor(depth, levels=c('0', '0.5', '1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12')),
#          variable = factor(variable, levels=c('TempC_0m', 'TempC_1m', 'TempC_2m', 'TempC_3m', 'TempC_4m', 'TempC_5m', 'TempC_6m', 'TempC_7m', 'TempC_8m', 'TempC_9m', 'TempC_10m')))
# 
# ggplot(subset(buoy2014_vert_temp_L1, subset=(datetime>='2014-07-24' & datetime < '2014-07-25')), aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='July 24 2014 buoy visit - L1', x='date', y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme

#remove data from 10m line from here forward - seems that it is programmed wrong or is hung up above the 9m thermistor
buoy2014_L1 <- buoy2014_L1 %>% 
  mutate(TempC_10m = case_when(datetime>=as.POSIXct('2014-07-24 9:40', tz='UTC') & datetime<=as.POSIXct('2014-12-31 23:50', tz='UTC') ~ NA_real_,
                               TRUE ~ TempC_10m))

# buoy2014_vert_temp_L1 <- buoy2014_L1 %>% 
#   select(datetime, TempC_0m, TempC_1m, TempC_2m, TempC_3m, TempC_4m, TempC_5m, TempC_6m, TempC_7m, TempC_8m, TempC_9m, TempC_10m) %>% 
#   gather(variable, value, -datetime) %>% 
#   separate(variable, into=c('sensor', 'depth'), sep='_', remove=F) %>% 
#   mutate(depth = as.numeric(str_extract(depth, '\\d')),
#          value = as.numeric(value),
#          depth = factor(depth, levels=c('0', '0.5', '1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12')),
#          variable = factor(variable, levels=c('TempC_0m', 'TempC_1m', 'TempC_2m', 'TempC_3m', 'TempC_4m', 'TempC_5m', 'TempC_6m', 'TempC_7m', 'TempC_8m', 'TempC_9m', 'TempC_10m')))
# 
# ggplot(subset(buoy2014_vert_temp_L1, subset=(datetime>='2014-07-01' & datetime < '2014-08-01')), aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='July 2014 - L1', x='date', y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme


# #August 13
# ggplot(subset(buoy2014_vert_temp, subset=(datetime>='2014-08-01' & datetime < '2014-08-16')), aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='early August 2014', x='date', y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme
# 
# ggplot(subset(buoy2014_vert_temp, subset=(datetime>='2014-08-13' & datetime < '2014-08-14')), aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='August 13 zoom', x='date', y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme

#remove all point from 15:20 until 21:20
buoy2014_L1 <- buoy2014_L1 %>% 
  mutate_at(vars(alltemp),
            funs(case_when(datetime>=as.POSIXct('2014-08-13 15:20', tz='UTC') & datetime<=as.POSIXct('2014-08-13 21:20', tz='UTC') ~ NA_real_,
                               TRUE ~ .)))

# buoy2014_vert_temp_L1 <- buoy2014_L1 %>% 
#   select(datetime, TempC_0m, TempC_1m, TempC_2m, TempC_3m, TempC_4m, TempC_5m, TempC_6m, TempC_7m, TempC_8m, TempC_9m, TempC_10m) %>% 
#   gather(variable, value, -datetime) %>% 
#   separate(variable, into=c('sensor', 'depth'), sep='_', remove=F) %>% 
#   mutate(depth = as.numeric(str_extract(depth, '\\d')),
#          value = as.numeric(value),
#          depth = factor(depth, levels=c('0', '0.5', '1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12')),
#          variable = factor(variable, levels=c('TempC_0m', 'TempC_1m', 'TempC_2m', 'TempC_3m', 'TempC_4m', 'TempC_5m', 'TempC_6m', 'TempC_7m', 'TempC_8m', 'TempC_9m', 'TempC_10m')))
# 
# ggplot(subset(buoy2014_vert_temp_L1, subset=(datetime>='2014-08-01' & datetime < '2014-09-01')), aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='August 2014 - L1', x='date', y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme
# 
# ggplot(subset(buoy2014_vert_temp_L1, subset=(datetime>='2014-09-01' & datetime < '2014-10-01')), aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='September 2014 - L1', x='date', y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme
# 
# #Oct 14 buoy moved to harbor Oct 14 10:20
# ggplot(subset(buoy2014_vert_temp_L1, subset=(datetime>='2014-10-01' & datetime < '2014-11-01')), aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='October 2014 - L1', x='date', y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme

#remove from Oct 14 10:10
buoy2014_L1 <- buoy2014_L1 %>% 
  mutate_at(vars(alltemp),
            funs(case_when(datetime>=as.POSIXct('2014-10-14 10:10', tz='UTC') ~ NA_real_,
                           TRUE ~ .)))
buoy2014_vert_temp_L1 <- buoy2014_L1 %>%
  select(datetime, TempC_0m, TempC_1m, TempC_2m, TempC_3m, TempC_4m, TempC_5m, TempC_6m, TempC_7m, TempC_8m, TempC_9m, TempC_10m) %>%
  gather(variable, value, -datetime) %>%
  separate(variable, into=c('sensor', 'depth'), sep='_', remove=F) %>%
  mutate(depth = as.numeric(str_extract(depth, '\\d')),
         value = as.numeric(value),
         depth = factor(depth, levels=c('0', '0.5', '1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12')),
         variable = factor(variable, levels=c('TempC_0m', 'TempC_1m', 'TempC_2m', 'TempC_3m', 'TempC_4m', 'TempC_5m', 'TempC_6m', 'TempC_7m', 'TempC_8m', 'TempC_9m', 'TempC_10m')))
# 
# ggplot(subset(buoy2014_vert_temp_L1, subset=(datetime>='2014-10-01' & datetime < '2014-11-01')), aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='October 2014 - L1', x='date', y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme
# 
# ggplot(subset(buoy2014_vert_temp_L1, subset=(datetime>=as.POSIXct('2014-01-01', tz='UTC') & datetime < as.POSIXct('2015-01-01', tz='UTC'))), aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='2014, Level 1 data', x='date', y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme


#reality check with LMP
buoy_LMP_2014 <- buoy2014_vert_temp_L1 %>%
  mutate(source='buoy') %>%
  full_join(., LMP2014_temp)
unique(LMP2014_temp$datetime)

#May 27, June 9, July 22, Aug 19, Sept 15
ggplot(subset(buoy_LMP_2014,
              subset=(datetime>=as.POSIXct('2014-05-27', tz='UTC') &
                        datetime<as.POSIXct('2014-05-28', tz='UTC'))),
       aes(x=datetime, y=value, color=as.factor(variable), shape = source)) +
  geom_point() +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
                              "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
ggplot(subset(buoy_LMP_2014,
              subset=(datetime>=as.POSIXct('2014-06-09', tz='UTC') &
                        datetime<as.POSIXct('2014-06-10', tz='UTC'))),
              aes(x=datetime, y=value, color=as.factor(variable), shape = source)) +
  geom_point() +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
                              "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust

ggplot(subset(buoy_LMP_2014,
              subset=(datetime>=as.POSIXct('2014-07-22', tz='UTC') &
                        datetime<as.POSIXct('2014-07-23', tz='UTC'))),
       aes(x=datetime, y=value, color=as.factor(variable), shape = source)) +
  geom_point() +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
                              "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust

ggplot(subset(buoy_LMP_2014,
              subset=(datetime>=as.POSIXct('2014-08-19', tz='UTC') &
                        datetime<as.POSIXct('2014-08-20', tz='UTC'))),
       aes(x=datetime, y=value, color=as.factor(variable), shape = source)) +
  geom_point() +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
                              "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust

ggplot(subset(buoy_LMP_2014,
              subset=(datetime>=as.POSIXct('2014-09-15', tz='UTC') &
                        datetime<as.POSIXct('2014-09-16', tz='UTC'))),
       aes(x=datetime, y=value, color=as.factor(variable), shape = source)) +
  geom_point() +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
                              "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust

#export L1 tempstring file
buoy2014_L1 %>% 
  select(datetime, TempC_0m, TempC_1m, TempC_2m, TempC_3m, TempC_4m, TempC_5m, TempC_6m, TempC_7m, TempC_8m, TempC_9m, TempC_10m) %>% 
  mutate(datetime = as.character(datetime)) %>% 
  write_csv(., 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2014_tempstring_L1.csv')

#clean up workspace
rm(buoy_LMP_2014, buoy2014_temp_vert, buoy2014_vert_temp_L1, LMP2014_temp)

####Upper DO data####
# buoy2014_vert_updo <- buoy2014_L1 %>% 
#   select(datetime, upDO) %>% 
#   gather(variable, value, -datetime)
# 
# ggplot(subset(buoy2014_vert_updo, subset=(datetime>=as.POSIXct('2014-01-01', tz='UTC') & datetime < as.POSIXct('2015-01-01', tz='UTC'))), aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='2014, raw', x='date', y='') +
#   final_theme

##### 2014 upper DO - replace NA values with NA ####
buoy2014_L1 <- buoy2014_L1 %>% 
  mutate_at(vars(DOSat, DOppm), 
            funs(case_when(.==0 ~ NA_real_, 
                           TRUE ~ .)))

# #transform data to vertical for display
buoy2014_vert_updo <- buoy2014_L1 %>%
  select(datetime, upDO) %>%
  gather(variable, value, -datetime)

# ggplot(subset(buoy2014_vert_updo, subset=(datetime>=as.POSIXct('2014-01-01', tz='UTC') & datetime < as.POSIXct('2015-01-01', tz='UTC'))), aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='2014, na strings recoded', x='date', y='') +
#   final_theme
# 
# ggplot(subset(buoy2014_vert_updo, subset=(datetime>=as.POSIXct('2014-01-01', tz='UTC') & datetime < as.POSIXct('2014-02-01', tz='UTC'))), aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='January 2014', x='date', y='') +
#   final_theme
# 
# ggplot(subset(buoy2014_vert_updo, subset=(datetime>=as.POSIXct('2014-02-01', tz='UTC') & datetime < as.POSIXct('2014-03-01', tz='UTC'))), aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='February 2014', x='date', y='') +
#   final_theme
# 
# ggplot(subset(buoy2014_vert_updo, subset=(datetime>=as.POSIXct('2014-03-01', tz='UTC') & datetime < as.POSIXct('2014-04-01', tz='UTC'))), aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='March 2014', x='date', y='') +
#   final_theme
# 
# ggplot(subset(buoy2014_vert_updo, subset=(datetime>=as.POSIXct('2014-04-01', tz='UTC') & datetime < as.POSIXct('2014-05-01', tz='UTC'))), aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='April 2014', x='date', y='') +
#   final_theme
# 
# #data gap until june
# 
# ggplot(subset(buoy2014_vert_updo, subset=(datetime>=as.POSIXct('2014-06-01', tz='UTC') & datetime < as.POSIXct('2014-07-01', tz='UTC'))), aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='June 2014', x='date', y='') +
#   final_theme
# 
# ggplot(subset(buoy2014_vert_updo, subset=(datetime>=as.POSIXct('2014-07-01', tz='UTC') & datetime < as.POSIXct('2014-08-01', tz='UTC'))), aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='July 2014', x='date', y='') +
#   final_theme
# 
# ggplot(subset(buoy2014_vert_updo, subset=(datetime>=as.POSIXct('2014-08-01', tz='UTC') & datetime < as.POSIXct('2014-09-01', tz='UTC'))), aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='August 2014', x='date', y='') +
#   final_theme
# 
# ggplot(subset(buoy2014_vert_updo, subset=(datetime>=as.POSIXct('2014-09-01', tz='UTC') & datetime < as.POSIXct('2014-10-01', tz='UTC'))), aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='September 2014', x='date', y='') +
#   final_theme
# 
# ggplot(subset(buoy2014_vert_updo, subset=(datetime>=as.POSIXct('2014-10-01', tz='UTC') & datetime < as.POSIXct('2014-11-01', tz='UTC'))), aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='October 2014', x='date', y='') +
#   final_theme
# 
# ggplot(subset(buoy2014_vert_updo, subset=(datetime>=as.POSIXct('2014-11-01', tz='UTC') & datetime < as.POSIXct('2014-12-01', tz='UTC'))), aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='November 2014', x='date', y='') +
#   final_theme
# 
# ggplot(subset(buoy2014_vert_updo, subset=(datetime>=as.POSIXct('2014-12-01', tz='UTC') & datetime < as.POSIXct('2015-01-01', tz='UTC'))), aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='December 2014', x='date', y='') +
#   final_theme

####2014 upper DO - compare with LMP####
# buoy2014_updo_LMP_L1 <- buoy2014_L1 %>% 
#   select(datetime, upDO) %>% 
#   gather(variable, value, -datetime) %>% 
#   mutate(source='buoy',
#          depth = 0.5) %>% 
#   full_join(., LMP2014_upDO) %>% 
#   mutate(depth = factor(depth, levels=c('0', '0.5', '1', '2')))
# 
# #plot buoy temp against LMP sonde values
# ggplot(subset(buoy2014_updo_LMP_L1, subset=(datetime>='2014-06-08' & datetime < '2014-06-10')), aes(x=datetime, y=value, col=source, shape=source)) +
#   geom_point(size=3) +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='June 9 buoy and LMP', x='date', y='') +
#   scale_color_manual(values=c("#000000", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme
# 
# ggplot(subset(buoy2014_updo_LMP_L1, subset=(datetime>='2014-07-21' & datetime < '2014-07-23')), aes(x=datetime, y=value, col=source, shape=source)) +
#   geom_point(size=3) +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='Jul 22 buoy and LMP', x='date', y='') +
#   scale_color_manual(values=c("#000000", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme
# 
# ggplot(subset(buoy2014_updo_LMP_L1, subset=(datetime>='2014-08-18' & datetime < '2014-08-20')), aes(x=datetime, y=value, col=source, shape=source)) +
#   geom_point(size=3) +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='August 19 buoy and LMP', x='date', y='') +
#   scale_color_manual(values=c("#000000", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme
# 
# ggplot(subset(buoy2014_updo_LMP_L1, subset=(datetime>='2014-09-14' & datetime < '2014-09-16')), aes(x=datetime, y=value, col=source, shape=source)) +
#   geom_point(size=3) +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='September 15 buoy and LMP', x='date', y='') +
#   scale_color_manual(values=c("#000000", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme
# 
# ggplot(subset(buoy2014_updo_LMP_L1, subset=(datetime>='2014-01-01' & datetime < '2015-01-01')), aes(x=datetime, y=value, col=source, shape=source)) +
#   geom_point(size=3) +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='2014 buoy and LMP', x='date', y='') +
#   scale_color_manual(values=c("#000000",  "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme

  
####2014 upper do - clean data####
# buoy2014_vert_updo_L1 <-  buoy2014_L1 %>%
#   select(datetime, upDO) %>%
#   gather(variable, value, -datetime)

#data logger reset. no data until 6/9 - that is also when the buoy was moved to the deep spot
buoy2014_L1 <- buoy2014_L1 %>% 
  mutate(location = case_when(datetime <= as.POSIXct('2014-06-09 15:00', tz='UTC') ~ 'harbor', 
                              TRUE ~ 'loon island'))

# ggplot(subset(buoy2014_vert_updo, subset=(datetime>=as.POSIXct('2014-07-28', tz='UTC') & datetime < as.POSIXct('2014-07-29', tz='UTC'))), aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='late July jump 2014', x='date', y='') +
#   final_theme
# 
# ggplot(subset(buoy2014_vert_updo, subset=(datetime>=as.POSIXct('2014-09-18', tz='UTC') & datetime < as.POSIXct('2014-09-19', tz='UTC'))), aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='September jump 2014', x='date', y='') +
#   final_theme

#remove do sat and ppm from Jul 28 Until Sept 17
buoy2014_L1 <- buoy2014_L1 %>% 
  mutate_at(vars(c(DOSat, DOppm)),
            funs(case_when(datetime>=as.POSIXct('2014-07-28 11:10', tz='UTC') & datetime<=as.POSIXct('2014-09-18 16:40', tz='UTC') ~ NA_real_,
                           TRUE ~ .)))

# buoy2014_vert_updo_L1 <-  buoy2014_L1 %>% 
#   select(datetime, upDO) %>% 
#   gather(variable, value, -datetime)
# 
# 
# ggplot(subset(buoy2014_vert_updo_L1, subset=(datetime>=as.POSIXct('2014-07-28', tz='UTC') & datetime < as.POSIXct('2014-07-29', tz='UTC'))), aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='late July jump 2014, v2', x='date', y='') +
#   final_theme
# 
# ggplot(subset(buoy2014_vert_updo_L1, subset=(datetime>=as.POSIXct('2014-09-18', tz='UTC') & datetime < as.POSIXct('2014-09-19', tz='UTC'))), aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='September jump 2014, v2', x='date', y='') +
#   final_theme
# 
# ggplot(subset(buoy2014_vert_updo_L1, subset=(datetime>=as.POSIXct('2014-07-01', tz='UTC') & datetime < as.POSIXct('2014-08-01', tz='UTC'))), aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='July 2014, v2', x='date', y='') +
#   final_theme
# 
# ggplot(subset(buoy2014_vert_updo_L1, subset=(datetime>=as.POSIXct('2014-08-01', tz='UTC') & datetime < as.POSIXct('2014-09-01', tz='UTC'))), aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='August 2014, v2', x='date', y='') +
#   final_theme
# 
# ggplot(subset(buoy2014_vert_updo_L1, subset=(datetime>=as.POSIXct('2014-09-01', tz='UTC') & datetime < as.POSIXct('2014-10-01', tz='UTC'))), aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='September 2014, v2', x='date', y='') +
#   final_theme

#NA for move to harbor and assign location
buoy2014_L1 <- buoy2014_L1 %>% 
  mutate_at(vars(upDO),
            funs(case_when(datetime>=as.POSIXct('2014-10-14 10:00', tz='UTC') & datetime<=as.POSIXct('2014-10-14 11:30', tz='UTC') ~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate(location = case_when(datetime>=as.POSIXct('2014-10-14 11:00', tz='UTC') & datetime<as.POSIXct('2015-01-01', tz='UTC') ~ 'harbor',
                              TRUE ~ location))

# buoy2014_vert_updo_L1 <-  buoy2014_L1 %>% 
#   select(datetime, location, upDO) %>% 
#   gather(variable, value, -datetime, -location)
# 
# 
# ggplot(subset(buoy2014_vert_updo_L1, subset=(datetime>=as.POSIXct('2014-10-01', tz='UTC') & datetime < as.POSIXct('2014-11-01', tz='UTC'))), aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='October 2014, v2', x='date', y='') +
#   scale_color_manual(values=c("#000000",  "#ffbf00")) +
#   final_theme
# 
# 
# ggplot(subset(buoy2014_vert_updo_L1, subset=(datetime>=as.POSIXct('2014-01-01', tz='UTC') & datetime < as.POSIXct('2015-01-01', tz='UTC'))), aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='2014, clean', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme





#### plot Lower DO ####

##transform data to vertical for display
buoy2014_vert_lowdo <-  buoy2014_L1 %>% 
  select(datetime, location, lowDO) %>% 
  gather(variable, value, -datetime, -location)

ggplot(subset(buoy2014_vert_lowdo, subset=(datetime>=as.POSIXct('2014-01-01', tz='UTC') & datetime < as.POSIXct('2015-01-01', tz='UTC'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='2014, raw', x='date', y='') +
  final_theme




#### 2014 lower DO - replace NA values with NA ####
buoy2014_L1 <- buoy2014_L1 %>% 
  mutate_at(vars(DOLoTempC, DOLowSat, DOLowPPM), 
            funs(case_when(.==0 ~ NA_real_, 
                           .==-6999 ~ NA_real_,
                           TRUE ~ .)))

##transform data to vertical for display
buoy2014_vert_lowdo <-  buoy2014_L1 %>% 
  select(datetime, location, lowDO) %>% 
  gather(variable, value, -datetime, -location)

ggplot(subset(buoy2014_vert_lowdo, subset=(datetime>=as.POSIXct('2014-01-01', tz='UTC') & datetime < as.POSIXct('2015-01-01', tz='UTC'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='2014, NA strings removed', x='date', y='') +
  final_theme


####2014 lower DO - compare with LMP####

buoy2014_lowdo_LMP_L1 <-  buoy2014_L1 %>% 
  select(datetime, location, lowDO) %>% 
  gather(variable, value, -datetime, -location) %>% 
  mutate(source = 'buoy', 
         depth = 10) %>% 
  full_join(., LMP2014_lowDO) %>% 
  mutate(depth = factor(depth, levels=c('9', '10', '11', '12'))) 

#plot buoy temp against LMP sonde values
ggplot(subset(buoy2014_lowdo_LMP_L1, subset=(datetime>='2014-07-21' & datetime < '2014-07-23')), aes(x=datetime, y=value, col=source, shape=source)) +
  geom_point(size=3) +
  facet_grid(variable~., scales='free_y') +
  labs(title='Jul 22 buoy and LMP', x='date', y='') +
  scale_color_manual(values=c("#000000", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  final_theme

ggplot(subset(buoy2014_lowdo_LMP_L1, subset=(datetime>='2014-08-18' & datetime < '2014-08-20')), aes(x=datetime, y=value, col=source, shape=source)) +
  geom_point(size=3) +
  facet_grid(variable~., scales='free_y') +
  labs(title='August 19 buoy and LMP', x='date', y='') +
  scale_color_manual(values=c("#000000", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  final_theme

ggplot(subset(buoy2014_lowdo_LMP_L1, subset=(datetime>='2014-09-14' & datetime < '2014-09-16')), aes(x=datetime, y=value, col=source, shape=source)) +
  geom_point(size=3) +
  facet_grid(variable~., scales='free_y') +
  labs(title='September 15 buoy and LMP', x='date', y='') +
  scale_color_manual(values=c("#000000", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  final_theme

ggplot(subset(buoy2014_lowdo_LMP_L1, subset=(datetime>='2014-01-01' & datetime < '2015-01-01')), aes(x=datetime, y=value, col=source, shape=source)) +
  geom_point(size=3) +
  facet_grid(variable~., scales='free_y') +
  labs(title='2014 buoy and LMP', x='date', y='') +
  scale_color_manual(values=c("#000000",  "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  final_theme



####2014 lower do - clean data ####
buoy2014_lowdo_L1 <- buoy2014_L1 %>% 
  select(datetime, location, lowDO) %>% 
  gather(variable, value, -datetime, -location)

#remove all sat and ppm data until Jul 28 - it is not correct
ggplot(subset(buoy2014_vert_lowdo_L1, subset=(datetime>=as.POSIXct('2014-07-28', tz='UTC') & datetime < as.POSIXct('2014-07-29', tz='UTC'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='late July 2014', x='date', y='') +
  final_theme

ix=which(buoy2014_L1$datetime>=as.POSIXct('2014-01-01', tz='UTC') & buoy2014_L1$datetime<=as.POSIXct('2014-07-28 11:20', tz='UTC'))
for (i in c('DOLowSat', 'DOLowPPM')) {buoy2014_L1 [ix,i]=NA}

buoy2014_vert_lowdo_L1 <- buoy2014_L1 %>% 
  select(datetime, location, lowDO) %>% 
  gather(variable, value, -datetime, -location)

ggplot(subset(buoy2014_vert_lowdo_L1, subset=(datetime>=as.POSIXct('2014-01-01', tz='UTC') & datetime < as.POSIXct('2015-01-01', tz='UTC'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='2014, pre-july removed', x='date', y='') +
  final_theme

#temp data okay until the flat line and spike
ggplot(subset(buoy2014_vert_lowdo_L1, subset=(datetime>=as.POSIXct('2014-06-16', tz='UTC') & datetime < as.POSIXct('2014-07-01', tz='UTC'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='late June 2014', x='date', y='') +
  final_theme

ggplot(subset(buoy2014_vert_lowdo_L1, subset=(datetime>=as.POSIXct('2014-07-01', tz='UTC') & datetime < as.POSIXct('2014-07-16', tz='UTC'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='early July 2014', x='date', y='') +
  final_theme

ggplot(subset(buoy2014_vert_lowdo_L1, subset=(datetime>=as.POSIXct('2014-07-16', tz='UTC') & datetime < as.POSIXct('2014-08-01', tz='UTC'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='early July 2014', x='date', y='') +
  final_theme

ggplot(subset(buoy2014_vert_lowdo_L1, subset=(datetime>=as.POSIXct('2014-07-17 06:00', tz='UTC') & datetime < as.POSIXct('2014-07-17 12:00', tz='UTC'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='late July 2014, v2', x='date', y='') +
  final_theme

ggplot(subset(buoy2014_vert_lowdo_L1, subset=(datetime>=as.POSIXct('2014-07-28', tz='UTC') & datetime < as.POSIXct('2014-07-29', tz='UTC'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='late July 2014, v2', x='date', y='') +
  final_theme


ix=which(buoy2014_L1$datetime>=as.POSIXct('2014-07-17 7:50', tz='UTC') & buoy2014_L1$datetime<=as.POSIXct('2014-07-28 11:20', tz='UTC'))
for (i in c('DOLoTempC')) {buoy2014_L1 [ix,i]=NA}

buoy2014_vert_lowdo_L1 <- buoy2014_L1 %>% 
  select(datetime, location, lowDO) %>% 
  gather(variable, value, -datetime, -location)


ggplot(subset(buoy2014_vert_lowdo_L1, subset=(datetime>=as.POSIXct('2014-07-17', tz='UTC') & datetime < as.POSIXct('2014-07-18', tz='UTC'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='late July 2014, v2', x='date', y='') +
  final_theme

ggplot(subset(buoy2014_vert_lowdo_L1, subset=(datetime>=as.POSIXct('2014-07-28', tz='UTC') & datetime < as.POSIXct('2014-07-29', tz='UTC'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='late July 2014, v2', x='date', y='') +
  final_theme

ggplot(subset(buoy2014_vert_lowdo_L1, subset=(datetime>=as.POSIXct('2014-07-16', tz='UTC') & datetime < as.POSIXct('2014-08-01', tz='UTC'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='late July 2014, v2', x='date', y='') +
  final_theme

ggplot(subset(buoy2014_vert_lowdo_L1, subset=(datetime>=as.POSIXct('2014-01-01', tz='UTC') & datetime < as.POSIXct('2015-01-01', tz='UTC'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='2014', x='date', y='') +
  final_theme


# presumed buoy visit on Aug 13
ggplot(subset(buoy2014_vert_lowdo, subset=(datetime>=as.POSIXct('2014-08-01', tz='UTC') & datetime < as.POSIXct('2014-08-16', tz='UTC'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='early August 2014', x='date', y='') +
  final_theme

ggplot(subset(buoy2014_vert_lowdo, subset=(datetime>=as.POSIXct('2014-08-13', tz='UTC') & datetime < as.POSIXct('2014-08-14', tz='UTC'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='August 13 2014', x='date', y='') +
  final_theme

ggplot(subset(buoy2014_vert_lowdo, subset=(datetime>=as.POSIXct('2014-08-13 12:00', tz='UTC') & datetime < as.POSIXct('2014-08-14', tz='UTC'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='August 13 2014', x='date', y='') +
  final_theme

ix=which(buoy2014_L1$datetime>=as.POSIXct('2014-08-13 16:00', tz='UTC') & buoy2014_L1$datetime<=as.POSIXct('2014-08-13 20:20', tz='UTC'))
for (i in lowDO) {buoy2014_L1 [ix,i]=NA}

buoy2014_vert_lowdo_L1 <- buoy2014_L1 %>% 
  select(datetime, location, lowDO) %>% 
  gather(variable, value, -datetime, -location)


ggplot(subset(buoy2014_vert_lowdo_L1, subset=(datetime>=as.POSIXct('2014-08-01', tz='UTC') & datetime < as.POSIXct('2014-08-16', tz='UTC'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='early August 2014, v2', x='date', y='') +
  final_theme

ggplot(subset(buoy2014_vert_lowdo_L1, subset=(datetime>=as.POSIXct('2014-08-13', tz='UTC') & datetime < as.POSIXct('2014-08-14', tz='UTC'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='August 13 2014', x='date', y='') +
  final_theme

ggplot(subset(buoy2014_vert_lowdo_L1, subset=(datetime>=as.POSIXct('2014-09-16', tz='UTC') & datetime < as.POSIXct('2014-10-01', tz='UTC'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='late September 2014, v2', x='date', y='') +
  final_theme

ggplot(subset(buoy2014_vert_lowdo, subset=(datetime>=as.POSIXct('2014-10-01', tz='UTC') & datetime < as.POSIXct('2014-10-16', tz='UTC'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='early October 2014', x='date', y='') +
  final_theme

# buoy moved to harbor.
ggplot(subset(buoy2014_vert_lowdo, subset=(datetime>=as.POSIXct('2014-10-14', tz='UTC') & datetime < as.POSIXct('2014-10-15', tz='UTC'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='October 14, buoy to harbor', x='date', y='') +
  final_theme

ix=which(buoy2014_L1$datetime>=as.POSIXct('2014-10-14 10:00', tz='UTC') & buoy2014_L1$datetime<as.POSIXct('2015-01-01', tz='UTC'))
for (i in lowDO) {buoy2014_L1[ix,i]=NA}

buoy2014_vert_lowdo_L1 <- buoy2014_L1 %>% 
  select(datetime, location, lowDO) %>% 
  gather(variable, value, -datetime, -location)


ggplot(subset(buoy2014_vert_lowdo_L1, subset=(datetime>=as.POSIXct('2014-10-01', tz='UTC') & datetime < as.POSIXct('2014-10-16', tz='UTC'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='early October 2014, v2', x='date', y='') +
  final_theme


ggplot(subset(buoy2014_vert_lowdo_L1, subset=(datetime>=as.POSIXct('2014-01-01', tz='UTC') & datetime < as.POSIXct('2015-01-01', tz='UTC'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='2014, clean', x='date', y='') +
  final_theme


#### plotPAR ####
# ggplot(subset(buoy2014_L1, subset=(datetime>=as.POSIXct('2014-01-01', tz='UTC') & datetime<as.POSIXct('2015-01-01', tz='UTC'))), aes(x=datetime, y=PAR)) +
#   geom_point() + 
#   labs(title='2014, raw', x='date', y='PAR (uE*m-2*s-1)') +
#   final_theme
# 
# ggplot(subset(buoy2014_L1, subset=(datetime>=as.POSIXct('2014-01-01', tz='UTC') & datetime<as.POSIXct('2014-01-16', tz='UTC'))), aes(x=datetime, y=PAR)) +
#   geom_point() + 
#   labs(title='early January 2014', x='date', y='PAR (uE*m-2*s-1)') +
#   final_theme
# 
# ggplot(subset(buoy2014_L1, subset=(datetime>=as.POSIXct('2014-01-16', tz='UTC') & datetime<as.POSIXct('2014-02-01', tz='UTC'))), aes(x=datetime, y=PAR)) +
#   geom_point() + 
#   labs(title='late January 2014', x='date', y='PAR (uE*m-2*s-1)') +
#   final_theme
# 
# ggplot(subset(buoy2014_L1, subset=(datetime>=as.POSIXct('2014-02-01', tz='UTC') & datetime<as.POSIXct('2014-02-14', tz='UTC'))), aes(x=datetime, y=PAR)) +
#   geom_point() + 
#   labs(title='early February 2014', x='date', y='PAR (uE*m-2*s-1)') +
#   final_theme
# 
# ggplot(subset(buoy2014_L1, subset=(datetime>=as.POSIXct('2014-02-14', tz='UTC') & datetime<as.POSIXct('2014-03-01', tz='UTC'))), aes(x=datetime, y=PAR)) +
#   geom_point() + 
#   labs(title='late February 2014', x='date', y='PAR (uE*m-2*s-1)') +
#   final_theme
# 
# ggplot(subset(buoy2014_L1, subset=(datetime>=as.POSIXct('2014-03-01', tz='UTC') & datetime<as.POSIXct('2014-03-16', tz='UTC'))), aes(x=datetime, y=PAR)) +
#   geom_point() + 
#   labs(title='early March 2014', x='date', y='PAR (uE*m-2*s-1)') +
#   final_theme
# 
# ggplot(subset(buoy2014_L1, subset=(datetime>=as.POSIXct('2014-03-16', tz='UTC') & datetime<as.POSIXct('2014-04-01', tz='UTC'))), aes(x=datetime, y=PAR)) +
#   geom_point() + 
#   labs(title='late March 2014', x='date', y='PAR (uE*m-2*s-1)') +
#   final_theme
# 
# ggplot(subset(buoy2014_L1, subset=(datetime>=as.POSIXct('2014-04-01', tz='UTC') & datetime<as.POSIXct('2014-04-16', tz='UTC'))), aes(x=datetime, y=PAR)) +
#   geom_point() + 
#   labs(title='early April 2014', x='date', y='PAR (uE*m-2*s-1)') +
#   final_theme
# 
# ggplot(subset(buoy2014_L1, subset=(datetime>=as.POSIXct('2014-06-01', tz='UTC') & datetime<as.POSIXct('2014-06-16', tz='UTC'))), aes(x=datetime, y=PAR)) +
#   geom_point() + 
#   labs(title='early June 2014', x='date', y='PAR (uE*m-2*s-1)') +
#   final_theme
# 
# ggplot(subset(buoy2014_L1, subset=(datetime>=as.POSIXct('2014-06-16', tz='UTC') & datetime<as.POSIXct('2014-07-01', tz='UTC'))), aes(x=datetime, y=PAR)) +
#   geom_point() + 
#   labs(title='late June 2014', x='date', y='PAR (uE*m-2*s-1)') +
#   final_theme
# 
# ggplot(subset(buoy2014_L1, subset=(datetime>=as.POSIXct('2014-07-01', tz='UTC') & datetime<as.POSIXct('2014-07-16', tz='UTC'))), aes(x=datetime, y=PAR)) +
#   geom_point() + 
#   labs(title='early July 2014', x='date', y='PAR (uE*m-2*s-1)') +
#   final_theme
# 
# ggplot(subset(buoy2014_L1, subset=(datetime>=as.POSIXct('2014-07-16', tz='UTC') & datetime<as.POSIXct('2014-08-01', tz='UTC'))), aes(x=datetime, y=PAR)) +
#   geom_point() + 
#   labs(title='late July 2014', x='date', y='PAR (uE*m-2*s-1)') +
#   final_theme
# 
# ggplot(subset(buoy2014_L1, subset=(datetime>=as.POSIXct('2014-08-01', tz='UTC') & datetime<as.POSIXct('2014-08-16', tz='UTC'))), aes(x=datetime, y=PAR)) +
#   geom_point() + 
#   labs(title='early August 2014', x='date', y='PAR (uE*m-2*s-1)') +
#   final_theme
# 
# ggplot(subset(buoy2014_L1, subset=(datetime>=as.POSIXct('2014-08-16', tz='UTC') & datetime<as.POSIXct('2014-09-01', tz='UTC'))), aes(x=datetime, y=PAR)) +
#   geom_point() + 
#   labs(title='late August 2014', x='date', y='PAR (uE*m-2*s-1)') +
#   final_theme
# 
# ggplot(subset(buoy2014_L1, subset=(datetime>=as.POSIXct('2014-09-01', tz='UTC') & datetime<as.POSIXct('2014-09-16', tz='UTC'))), aes(x=datetime, y=PAR)) +
#   geom_point() + 
#   labs(title='early September 2014', x='date', y='PAR (uE*m-2*s-1)') +
#   final_theme
# 
# ggplot(subset(buoy2014_L1, subset=(datetime>=as.POSIXct('2014-09-16', tz='UTC') & datetime<as.POSIXct('2014-10-01', tz='UTC'))), aes(x=datetime, y=PAR)) +
#   geom_point() + 
#   labs(title='late September 2014', x='date', y='PAR (uE*m-2*s-1)') +
#   final_theme
# 
# ggplot(subset(buoy2014_L1, subset=(datetime>=as.POSIXct('2014-10-01', tz='UTC') & datetime<as.POSIXct('2014-10-16', tz='UTC'))), aes(x=datetime, y=PAR)) +
#   geom_point() + 
#   labs(title='early October 2014', x='date', y='PAR (uE*m-2*s-1)') +
#   final_theme
# 
# ggplot(subset(buoy2014_L1, subset=(datetime>=as.POSIXct('2014-10-16', tz='UTC') & datetime<as.POSIXct('2014-11-01', tz='UTC'))), aes(x=datetime, y=PAR)) +
#   geom_point() + 
#   labs(title='late October 2014', x='date', y='PAR (uE*m-2*s-1)') +
#   final_theme
# 
# ggplot(subset(buoy2014_L1, subset=(datetime>=as.POSIXct('2014-11-01', tz='UTC') & datetime<as.POSIXct('2014-11-16', tz='UTC'))), aes(x=datetime, y=PAR)) +
#   geom_point() + 
#   labs(title='early November 2014', x='date', y='PAR (uE*m-2*s-1)') +
#   final_theme
# 
# ggplot(subset(buoy2014_L1, subset=(datetime>=as.POSIXct('2014-11-16', tz='UTC') & datetime<as.POSIXct('2014-12-01', tz='UTC'))), aes(x=datetime, y=PAR)) +
#   geom_point() + 
#   labs(title='late November 2014', x='date', y='PAR (uE*m-2*s-1)') +
#   final_theme
# 
# ggplot(subset(buoy2014_L1, subset=(datetime>=as.POSIXct('2014-12-01', tz='UTC') & datetime<as.POSIXct('2014-12-16', tz='UTC'))), aes(x=datetime, y=PAR)) +
#   geom_point() + 
#   labs(title='early December 2014', x='date', y='PAR (uE*m-2*s-1)') +
#   final_theme
# 
# ggplot(subset(buoy2014_L1, subset=(datetime>=as.POSIXct('2014-12-16', tz='UTC') & datetime<as.POSIXct('2015-01-01', tz='UTC'))), aes(x=datetime, y=PAR)) +
#   geom_point() + 
#   labs(title='late December 2014', x='date', y='PAR (uE*m-2*s-1)') +
#   final_theme


####Plot par - clean data ####

#replace negative values with 0
range(buoy2014_L1$PAR)
ix=which(buoy2014_L1$PAR<0)
buoy2014_L1$PAR[ix]=0

ggplot(subset(buoy2014_L1, subset=(datetime>=as.POSIXct('2014-01-01', tz='UTC') & datetime<as.POSIXct('2015-01-01', tz='UTC'))), aes(x=datetime, y=PAR)) +
  geom_point() +
  labs(title='2014, negative PAR to 0', x='date', y='PAR (uE*m-2*s-1)') +
  final_theme


#low values during the day on the 26th - leaving as is.
ggplot(subset(buoy2014_L1, subset=(datetime>=as.POSIXct('2014-03-16', tz='UTC') & datetime<as.POSIXct('2014-04-01', tz='UTC'))), aes(x=datetime, y=PAR)) +
  geom_point() +
  labs(title='late March 2014', x='date', y='PAR (uE*m-2*s-1)') +
  final_theme


#remove data after midnight on the 27th
ix=which(buoy2014_L1$datetime>=as.POSIXct('2014-03-28', tz='UTC') & buoy2014_L1$datetime<as.POSIXct('2015-01-01', tz='UTC'))
buoy2014_L1$PAR[ix]=NA

ggplot(subset(buoy2014_L1, subset=(datetime>=as.POSIXct('2014-01-01', tz='UTC') & datetime<as.POSIXct('2015-01-01', tz='UTC'))), aes(x=datetime, y=PAR)) +
  geom_point() +
  labs(title='2014, level 1', x='date', y='PAR (uE*m-2*s-1)') +
  final_theme




####Wind data ####
buoy2014_vert_wind <- buoy2014_L1 %>% 
  select(datetime, wind) %>% 
  gather(variable, value , -datetime)

### plot wind - clean data####
ggplot(subset(buoy2014_vert_wind, subset=(datetime>=as.POSIXct('2014-01-01', tz='UTC') & datetime<as.POSIXct('2015-01-01', tz='UTC'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='2014, raw', x='date', y=' ') +
  final_theme

ggplot(subset(buoy2014_vert_wind, subset=(datetime>=as.POSIXct('2014-01-01', tz='UTC') & datetime<as.POSIXct('2014-01-16', tz='UTC'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='early January 2014', x='date', y=' ') +
  final_theme

##errant readings until Jan 5
ggplot(subset(buoy2014_vert_wind, subset=(datetime>=as.POSIXct('2014-01-05', tz='UTC') & datetime<as.POSIXct('2014-01-06', tz='UTC'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='early January 2014', x='date', y=' ') +
  final_theme

ix=which(buoy2014_L1$datetime>=as.POSIXct('2014-01-01', tz='UTC') & buoy2014_L1$datetime<=as.POSIXct('2014-01-05 12:30', tz='UTC'))
for (i in wind) {buoy2014_L1[ix, i]= NA}
buoy2014_vert_wind_L1 <- buoy2014_L1 %>% 
  select(datetime, wind) %>% 
  gather(variable, value , -datetime)

ggplot(subset(buoy2014_vert_wind_L1, subset=(datetime>=as.POSIXct('2014-01-01', tz='UTC') & datetime<as.POSIXct('2014-01-16', tz='UTC'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='early January 2014, v2', x='date', y=' ') +
  final_theme


ggplot(subset(buoy2014_vert_wind, subset=(datetime>=as.POSIXct('2014-01-16', tz='UTC') & datetime<as.POSIXct('2014-02-01', tz='UTC'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='late January 2014', x='date', y=' ') +
  final_theme

ggplot(subset(buoy2014_vert_wind, subset=(datetime>=as.POSIXct('2014-02-01', tz='UTC') & datetime<as.POSIXct('2014-02-14', tz='UTC'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='early February 2014', x='date', y=' ') +
  final_theme

ggplot(subset(buoy2014_vert_wind, subset=(datetime>=as.POSIXct('2014-02-14', tz='UTC') & datetime<as.POSIXct('2014-03-01', tz='UTC'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='late February 2014', x='date', y=' ') +
  final_theme

## 0 values followed by >30m/s
ggplot(subset(buoy2014_vert_wind, subset=(datetime>=as.POSIXct('2014-02-14', tz='UTC') & datetime<as.POSIXct('2014-02-15', tz='UTC'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='late February 2014', x='date', y=' ') +
  final_theme

ix=which(buoy2014_L1$datetime>=as.POSIXct('2014-02-14 4:50', tz='UTC') & buoy2014_L1$datetime<=as.POSIXct('2014-02-14 12:40', tz='UTC'))
for (i in wind) {buoy2014_L1[ix, i]= NA}
buoy2014_vert_wind_L1 <- buoy2014_L1 %>% 
  select(datetime, wind) %>% 
  gather(variable, value , -datetime)

ggplot(subset(buoy2014_vert_wind_L1, subset=(datetime>=as.POSIXct('2014-02-14', tz='UTC') & datetime<as.POSIXct('2014-03-01', tz='UTC'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='late February 2014, v2', x='date', y=' ') +
  final_theme

ggplot(subset(buoy2014_vert_wind_L1, subset=(datetime>=as.POSIXct('2014-02-19', tz='UTC') & datetime<as.POSIXct('2014-02-20', tz='UTC'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='late February 2014, v2', x='date', y=' ') +
  final_theme

ix=which(buoy2014_L1$datetime>=as.POSIXct('2014-02-19', tz='UTC') & buoy2014_L1$datetime<=as.POSIXct('2014-02-19 9:20', tz='UTC'))
for (i in wind) {buoy2014_L1[ix, i]= NA}
buoy2014_vert_wind_L1 <- buoy2014_L1 %>% 
  select(datetime, wind) %>% 
  gather(variable, value , -datetime)

ggplot(subset(buoy2014_vert_wind_L1, subset=(datetime>=as.POSIXct('2014-02-14', tz='UTC') & datetime<as.POSIXct('2014-03-01', tz='UTC'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='late February 2014, v3', x='date', y=' ') +
  final_theme


ggplot(subset(buoy2014_vert_wind, subset=(datetime>=as.POSIXct('2014-03-01', tz='UTC') & datetime<as.POSIXct('2014-03-16', tz='UTC'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='early March 2014', x='date', y=' ') +
  final_theme

ggplot(subset(buoy2014_vert_wind, subset=(datetime>=as.POSIXct('2014-03-16', tz='UTC') & datetime<as.POSIXct('2014-04-01', tz='UTC'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='late March 2014', x='date', y=' ') +
  final_theme

ggplot(subset(buoy2014_vert_wind, subset=(datetime>=as.POSIXct('2014-04-01', tz='UTC') & datetime<as.POSIXct('2014-04-16', tz='UTC'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='early April 2014', x='date', y=' ') +
  final_theme

#data gap until June

ggplot(subset(buoy2014_vert_wind, subset=(datetime>=as.POSIXct('2014-06-01', tz='UTC') & datetime<as.POSIXct('2014-06-16', tz='UTC'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='early June 2014', x='date', y=' ') +
  final_theme

ggplot(subset(buoy2014_vert_wind, subset=(datetime>=as.POSIXct('2014-06-16', tz='UTC') & datetime<as.POSIXct('2014-07-01', tz='UTC'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='late June 2014', x='date', y=' ') +
  final_theme

ggplot(subset(buoy2014_vert_wind, subset=(datetime>=as.POSIXct('2014-07-01', tz='UTC') & datetime<as.POSIXct('2014-07-16', tz='UTC'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='early July 2014', x='date', y=' ') +
  final_theme

ggplot(subset(buoy2014_vert_wind, subset=(datetime>=as.POSIXct('2014-07-16', tz='UTC') & datetime<as.POSIXct('2014-08-01', tz='UTC'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='late July 2014', x='date', y=' ') +
  final_theme

ggplot(subset(buoy2014_vert_wind, subset=(datetime>=as.POSIXct('2014-08-01', tz='UTC') & datetime<as.POSIXct('2014-08-16', tz='UTC'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='early August 2014', x='date', y=' ') +
  final_theme

ggplot(subset(buoy2014_vert_wind, subset=(datetime>=as.POSIXct('2014-08-16', tz='UTC') & datetime<as.POSIXct('2014-09-01', tz='UTC'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='late August 2014', x='date', y=' ') +
  final_theme

ggplot(subset(buoy2014_vert_wind, subset=(datetime>=as.POSIXct('2014-09-01', tz='UTC') & datetime<as.POSIXct('2014-09-16', tz='UTC'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='early September 2014', x='date', y=' ') +
  final_theme

ggplot(subset(buoy2014_vert_wind, subset=(datetime>=as.POSIXct('2014-09-16', tz='UTC') & datetime<as.POSIXct('2014-10-01', tz='UTC'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='late September 2014', x='date', y=' ') +
  final_theme

ggplot(subset(buoy2014_vert_wind, subset=(datetime>=as.POSIXct('2014-10-01', tz='UTC') & datetime<as.POSIXct('2014-10-16', tz='UTC'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='early October 2014', x='date', y=' ') +
  final_theme


#remove buoy move artifacts
ix=which(buoy2014_L1$datetime>=as.POSIXct('2014-10-14 10:00', tz='UTC') & buoy2014_L1$datetime<=as.POSIXct('2014-10-14 11:30', tz='UTC'))
for (i in wind) {buoy2014_L1[ix, i]= NA}
buoy2014_vert_wind_L1 <- buoy2014_L1 %>% 
  select(datetime, wind) %>% 
  gather(variable, value , -datetime)

ggplot(subset(buoy2014_vert_wind_L1, subset=(datetime>=as.POSIXct('2014-10-01', tz='UTC') & datetime<as.POSIXct('2014-10-16', tz='UTC'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='early October 2014, v2', x='date', y=' ') +
  final_theme


ggplot(subset(buoy2014_vert_wind, subset=(datetime>=as.POSIXct('2014-10-16', tz='UTC') & datetime<as.POSIXct('2014-11-01', tz='UTC'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='late October 2014', x='date', y=' ') +
  final_theme

ggplot(subset(buoy2014_vert_wind, subset=(datetime>=as.POSIXct('2014-11-01', tz='UTC') & datetime<as.POSIXct('2014-11-16', tz='UTC'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='early November 2014', x='date', y=' ') +
  final_theme

ggplot(subset(buoy2014_vert_wind, subset=(datetime>=as.POSIXct('2014-11-16', tz='UTC') & datetime<as.POSIXct('2014-12-01', tz='UTC'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='late Novebmer 2014', x='date', y=' ') +
  final_theme

## errant data around Nov 26-30
ggplot(subset(buoy2014_vert_wind, subset=(datetime>=as.POSIXct('2014-11-26', tz='UTC') & datetime<as.POSIXct('2014-11-27', tz='UTC'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='late Novebmer 2014', x='date', y=' ') +
  final_theme

ggplot(subset(buoy2014_vert_wind, subset=(datetime>=as.POSIXct('2014-11-30', tz='UTC') & datetime<as.POSIXct('2014-12-1', tz='UTC'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='late Novebmer 2014', x='date', y=' ') +
  final_theme

## errant wind values nov 26-30
ix=which(buoy2014_L1$datetime>=as.POSIXct('2014-11-26 12:20', tz='UTC') & buoy2014_L1$datetime<=as.POSIXct('2014-11-30 8:50', tz='UTC'))
for (i in wind) {buoy2014_L1[ix, i]= NA}
buoy2014_vert_wind_L1 <- buoy2014_L1 %>% 
  select(datetime, wind) %>% 
  gather(variable, value , -datetime)

ggplot(subset(buoy2014_vert_wind_L1, subset=(datetime>=as.POSIXct('2014-11-16', tz='UTC') & datetime<as.POSIXct('2014-12-01', tz='UTC'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='late Novebmer 2014, v2', x='date', y=' ') +
  final_theme


ggplot(subset(buoy2014_vert_wind, subset=(datetime>=as.POSIXct('2014-12-01', tz='UTC') & datetime<as.POSIXct('2014-12-16', tz='UTC'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='early December 2014', x='date', y=' ') +
  final_theme

#errant data around dec 11
ggplot(subset(buoy2014_vert_wind, subset=(datetime>=as.POSIXct('2014-12-11 6:00', tz='UTC') & datetime<as.POSIXct('2014-12-11 12:00', tz='UTC'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='early December 2014', x='date', y=' ') +
  final_theme
ggplot(subset(buoy2014_vert_wind, subset=(datetime>=as.POSIXct('2014-12-12 12:00', tz='UTC') & datetime<as.POSIXct('2014-12-12 18:00', tz='UTC'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='early December 2014', x='date', y=' ') +
  final_theme

## errant values dec 11/12
ix=which(buoy2014_L1$datetime>=as.POSIXct('2014-12-11 9:20', tz='UTC') & buoy2014_L1$datetime<=as.POSIXct('2014-12-12 14:00', tz='UTC'))
for (i in wind) {buoy2014_L1[ix, i]= NA}
buoy2014_vert_wind_L1 <- buoy2014_L1 %>% 
  select(datetime, wind) %>% 
  gather(variable, value , -datetime)

ggplot(subset(buoy2014_vert_wind_L1, subset=(datetime>=as.POSIXct('2014-12-01', tz='UTC') & datetime<as.POSIXct('2014-12-16', tz='UTC'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='early December 2014, v2', x='date', y=' ') +
  final_theme

ggplot(subset(buoy2014_vert_wind, subset=(datetime>=as.POSIXct('2014-12-16', tz='UTC') & datetime<as.POSIXct('2015-01-01', tz='UTC'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='late December 2014', x='date', y=' ') +
  final_theme

ggplot(subset(buoy2014_vert_wind_L1, subset=(datetime>=as.POSIXct('2014-01-01', tz='UTC') & datetime<as.POSIXct('2015-01-01', tz='UTC'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='2014, clean', x='date', y=' ') +
  final_theme

#### export L1 data ####
#remove temp and humidity data, since that has not been proofed (see previous version for some truthing)
buoy2014_L1_notemphum <- buoy2014_L1 %>% 
  select(-AirTempC, -RelHum)

setwd('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1')
write.csv(buoy2014_L1_notemphum, 'buoy2014_L1_03Jan2018.csv', row.names = F)


