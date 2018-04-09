# temp string collation

#bring in L1 thermistor files
buoy2007 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2007_tempstring_L1.csv',
                     col_types = 'cnnnnnnnnnnnnnnnn') %>% 
  mutate(datetime = as.POSIXct(datetime, tz='EST'))
buoy2008 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2008_tempstring_L1.csv',
                     col_types = 'cnnnnnnnnnnnnnnnc') %>% 
  mutate(datetime = as.POSIXct(datetime, tz='EST'))
buoy2009 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2009_tempstring_L1.csv',
         col_types = 'cnnnnnnnnnnnnnnnnc') %>% 
  mutate(datetime = as.POSIXct(datetime, tz='EST'))
buoy2010 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2010_tempstring_L1.csv',
         col_types = 'cnnnnnnnnnnnnnnnnc') %>% 
  mutate(datetime = as.POSIXct(datetime, tz='EST'))
buoy2011 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2011_tempstring_L1.csv',
         col_types = 'cnnnnnnnnnn') %>% 
  mutate(datetime = as.POSIXct(datetime, tz='EST'))
buoy2012 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2012_tempstring_L1.csv',
         col_types = 'cnnnnnnnnnn') %>% 
  mutate(datetime = as.POSIXct(datetime, tz='EST'))
buoy2013 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2013_tempstring_L1.csv',
         col_types = 'cnnnnnnnnnn') %>% 
  mutate(datetime = as.POSIXct(datetime, tz='EST'))
buoy2014 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2014_tempstring_L1.csv',
         col_types = 'cnnnnnnnnnnn') %>% 
  mutate(datetime = as.POSIXct(datetime, tz='EST'))
hobo1415 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2014-2015_hobotempstring_L1.csv',
         col_types = 'cnnnnnnnnn') %>% 
  mutate(datetime = as.POSIXct(datetime, tz='EST'))
buoy_2015 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2015_tempstring_L1.csv',
         col_types = 'cnnnnnnnnnnn') %>% 
  mutate(datetime = as.POSIXct(datetime, tz='EST'))
hobo2015 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2015_hobotempstring_L1.csv',
         col_types = 'cnnnnnnnnn') %>% 
  mutate(datetime = as.POSIXct(datetime, tz='EST'))
hobo1516 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2015-2016_hobotempstring_L1.csv', 
         col_types = 'cnnnnnnnnn') %>% 
  mutate(datetime =as.POSIXct(datetime, tz='EST'))
buoy2016 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2016_tempstring_L1.csv',
         col_types = 'cnnnnnnnnnn') %>% 
  mutate(datetime = as.POSIXct(datetime, tz='EST'))
hobo1617 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2016-2017_hobotempstring_L1.csv',
         col_types = 'cnnnnnnnnn') %>% 
  mutate(datetime = as.POSIXct(datetime, tz='EST'))
buoy2017 <- read_csv('c:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2017_tempstring_L1.csv',
         col_types = 'cnnnnnnnnnn')

#find first and last date with data
buoy2007v <- buoy2007 %>% 
  gather(variable, value, -datetime) %>% 
  mutate(variable = factor(variable, levels=alltemp2007))
ggplot(buoy2007v, aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  scale_x_datetime(date_minor_breaks = '1 month') +
  final_theme
