#R version: 3.6.1
#RStudio version: 1.2.5001

#bring in L1 temp data
temp07 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/tempstring/2007_tempstring_L1.csv')
temp08 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/tempstring/2008_tempstring_L1.csv')
temp09 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/tempstring/2009_tempstring_L1.csv')
temp10 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/tempstring/2010_tempstring_L1.csv',
                   col_types = 'Tcnnnnnnnnnnnnnnnnc')
temp11 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/tempstring/2011_tempstring_L1.csv',
                   col_types = 'Tcnnnnnnnnnn')
temp12 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/tempstring/2012_tempstring_L1.csv',
                   col_types = 'Tcnnnnnnnnnn')
temp13 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/tempstring/2013_tempstring_L1.csv',
                   col_types = 'Tcnnnnnnnnnn')
temp14 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/tempstring/2014_tempstring_L1.csv',
                   col_types = 'Tcnnnnnnnnnnnc')
temp15 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/tempstring/2015_tempstring_L1.csv',
                   col_types = 'Tnnnnnnnnnnc')
temp16 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/tempstring/2016_tempstring_L1.csv',
                   col_types = 'Tnnnnnnnnnnc')
temp17 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/tempstring/2017_tempstring_L1_corrdepth.csv',
                   col_types = 'Tnnnnnnnnnnc')
temp18 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/tempstring/2018_tempstring_L1.csv',
                   col_types = 'Tnnnnnnnnnnc')
temp19 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/tempstring/2019_tempstring_L1_corrdepths.csv',
                   col_types = 'Tnnnnnnnnnnc')

#collate together
alltemp_07e19 <- full_join(temp07, temp08) %>% 
  full_join(., temp09) %>% 
  full_join(., temp10) %>% 
  full_join(., temp11) %>% 
  full_join(., temp12) %>% 
  full_join(., temp13) %>% 
  full_join(., temp14) %>% 
  full_join(., temp15) %>% 
  full_join(., temp16) %>% 
  full_join(., temp17) %>% 
  full_join(., temp18) %>% 
  full_join(., temp19) %>% 
  select(datetime, location, 
         TempC_0p5m, TempC_0p75m, TempC_0p85m, TempC_1m,
         TempC_1p5m, TempC_1p75m, TempC_1p85m, TempC_2m,
         TempC_2p5m, TempC_2p75m, TempC_2p85m, TempC_3m,
         TempC_3p5m, TempC_3p75m, TempC_3p85m, 
         TempC_4p5m, TempC_4p75m, TempC_4p85m, 
         TempC_5p5m, TempC_5p75m, TempC_5p85m, 
         TempC_6p5m, TempC_6p75m, TempC_6p85m, 
         TempC_7p5m, TempC_7p75m, TempC_7p85m, 
         TempC_8p5m, TempC_8p75m, TempC_8p85m, 
         TempC_9p5m, TempC_9p75m, TempC_9p85m, 
         TempC_10p5m, TempC_11p5m, TempC_13p5m,
         temp_flag)

alltemp_07e19 %>% 
  mutate(datetime = as.character(datetime)) %>% 
  write_csv(., 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/record collations/tempstring/2007-e2019_buoy_templine_v07April2020.csv')
  
  
  
  
  
  
