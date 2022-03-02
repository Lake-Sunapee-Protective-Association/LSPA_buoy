#*****************************************************************
#*      Cary Institute of Ecosystem Studies (Millbrook, NY)      *
#*                                                               *
#* TITLE:   Sunapee_buoy_2015.r                                  *
#* AUTHOR:  Bethel Steele                                        *
#* PROJECT: Lake Sunapee Buoy Data Cleaning                      *
#*****************************************************************

source('library_func_lists.R')

#bring in summer 2015 hobo raw data
hobo2015 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L0/2015 Summer Hobo.csv',
                      col_types = 'icnnnnnnnnnnnnnnnnnn')

head(hobo2015)
tail(hobo2015)

hobo_tz = 'America/New_York'

#format date of hobo sensors
hobo2015 <- hobo2015 %>% 
  rename(Date.Time = 'Date/Time') %>% 
  mutate(datetime = as.POSIXct(Date.Time, format='%m/%d/%Y %H:%M', tz=hobo_tz)) %>% 
  select(-Date.Time)
head(hobo2015)

#### hobo line ####
hobo_vert <- hobo2015 %>%
  select(datetime, TempC_1m, TempC_2m, TempC_3m, TempC_4m, TempC_5m, TempC_6m, TempC_7m, TempC_8m, TempC_9m) %>%
  gather(variable, value, -datetime)

ggplot(subset(hobo_vert, subset=(datetime>='2015-01-01' & datetime < '2016-01-01')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='hobo temp sensor summer 2015', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  final_theme

# ggplot(subset(hobo_vert, subset=(datetime>='2015-08-01' & datetime < '2015-09-01')), aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='hobo temp aug 2015', x='date', y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme
# 
# ggplot(subset(hobo_vert, subset=(datetime>='2015-09-01' & datetime < '2015-10-01')), aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='hobo temp sept 2015', x='date', y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme

tail(hobo2015)

#add CV
hobo2015 <- hobo2015 %>%
  select(datetime, TempC_1m, TempC_2m, TempC_3m, TempC_4m, TempC_5m, TempC_6m, TempC_7m, TempC_8m, TempC_9m) %>%
  rename(waterTemperature_degC_0p5m = 'TempC_1m',
         waterTemperature_degC_1p5m = 'TempC_2m',
         waterTemperature_degC_2p5m = 'TempC_3m',
         waterTemperature_degC_3p5m = 'TempC_4m',
         waterTemperature_degC_4p5m = 'TempC_5m',
         waterTemperature_degC_5p5m = 'TempC_6m',
         waterTemperature_degC_6p5m = 'TempC_7m',
         waterTemperature_degC_7p5m = 'TempC_8m',
         waterTemperature_degC_8p5m = 'TempC_9m') 

#convert to GMT+5
hobo2015 <- hobo2015 %>%
  rename(datetime.local = datetime) %>% 
  mutate(datetime = with_tz(datetime.local, tz = 'Etc/GMT+5'))

#export L1 tempstring file
hobo2015 %>%
  select(-datetime.local) %>% 
  mutate(datetime = as.character(datetime)) %>%
  mutate(location = 'loon') %>% 
  write_csv(., 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/tempstring/2015_hobotempstring_L1_v2022.csv') 
#clean up workspace
rm(hobo_vert)




