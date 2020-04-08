#### read in all air temp files ####

airtemp_07 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/met/2007_AirTemp_L1.csv',
                       col_types = 'Tcn')
colnames(airtemp_07)

airtemp_08 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/met/2008_AirTemp_L1.csv',
                       col_types = 'Tcnc')
colnames(airtemp_08)
                       
airtemp_09 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/met/2009_AirTemp_L1.csv',
                       col_types = 'Tcnc')
colnames(airtemp_09)

airtemp_10 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/met/2010_AirTemp_L1.csv',
                       col_types = 'Tcn')
colnames(airtemp_10)

airtemp_11 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/met/2011_AirTemp_L1.csv',
                       col_types = 'Tcn')
colnames(airtemp_11)

airtemp_12 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/met/2012_AirTemp_L1.csv',
                       col_types = 'Tcn')
colnames(airtemp_12)

airtemp_13 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/met/2013_AirTemp_L1.csv',
                       col_types = 'Tcn')
colnames(airtemp_13)

airtemp_14 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/met/2014_AirTemp_L1.csv',
                       col_types = 'Tcn')
colnames(airtemp_14)
airtemp_14 <- airtemp_14 %>% 
  rename(AirTemp_degC = AirTempC)
colnames(airtemp_14)

airtemp_15 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/met/2015_AirTemp_L1.csv',
                       col_types = 'Tnc')
colnames(airtemp_15)
airtemp_15 <- airtemp_15 %>% 
  rename(AirTemp_degC = AirTempC)
colnames(airtemp_15)

airtemp_16 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/met/2016_AirTemp_L1.csv',
                       col_types = 'Tnc')
colnames(airtemp_16)
airtemp_16 <- airtemp_16 %>% 
  rename(AirTemp_degC = AirTempC)
colnames(airtemp_16)

airtemp_17 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/met/2017_AirTemp_L1.csv',
                       col_types = 'Tnc')
colnames(airtemp_17)
airtemp_17 <- airtemp_17 %>% 
  rename(AirTemp_degC = AirTempC)
colnames(airtemp_17)

airtemp_18 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/met/2018_AirTemp_L1.csv',
                       col_types = 'Tnc')
colnames(airtemp_18)
airtemp_18 <- airtemp_18 %>% 
  rename(AirTemp_degC = AirTempC)
colnames(airtemp_18)

airtemp_e19 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/met/2019_airtemp_L1.csv',
                       col_types = 'Tnc')
colnames(airtemp_e19)
airtemp_e19 <- airtemp_e19 %>% 
  rename(AirTemp_degC = AirTempC)
colnames(airtemp_e19)



#### collate files into one ####
airtemp_07e19 <- full_join(airtemp_07, airtemp_08) %>% 
  full_join(., airtemp_09) %>% 
  full_join(., airtemp_10) %>% 
  full_join(., airtemp_11) %>% 
  full_join(., airtemp_12) %>% 
  full_join(., airtemp_13) %>% 
  full_join(., airtemp_14) %>% 
  full_join(., airtemp_15) %>% 
  full_join(., airtemp_16) %>% 
  full_join(., airtemp_17) %>% 
  full_join(., airtemp_18) %>% 
  full_join(., airtemp_e19) %>% 
  mutate(AirTemp_degC = case_when(location == 'offline' ~ NA_real_,
                                  location == 'in transit' ~ NA_real_,
                                  TRUE ~ AirTemp_degC))
colnames(airtemp_07e19)
unique(airtemp_07e19$airtemp_flag)

ggplot(airtemp_07e19, aes(x=datetime, y=AirTemp_degC, color = location)) +
  geom_point()+
  scale_color_colorblind()


#### export file ####
airtemp_07e19 %>% 
  mutate(datetime = as.character(datetime)) %>% 
  write_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/record collations/met/2007-e2019_airtemp_L1.csv')
