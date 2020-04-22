#R version: 3.6.1
#RStudio version: 1.2.5001

#bring in L1 tempdo data
tempdo_1415 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/winter/2014-2015_hobotempstring_L1.csv',
                        col_types = 'Tnnnnnnnnn')
tempdo_1415

tempdo_1516 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/winter/2015-2016_hobotempstring_L1.csv',
                        col_types = 'Tnnnnnnnnn')
tempdo_1516

tempdo_1617 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/winter/2016-2017_hobotempstring_L1.csv',
                        col_types = 'Tnnnnnnnnn')
tempdo_1617

tempdo_1718 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/winter/2017-2018_hobotempstringdo_L1.csv',
                        col_types = 'Tnnnnnnnnnnn')
tempdo_1718

tempdo_1819 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/winter/2018-2019_hobotempstringdo_L1.csv',
                        col_types = 'Tnnnnnnnnnnn')
tempdo_1819



#collate together
winter_tempdo_14_19 <- full_join(tempdo_1415, tempdo_1516) %>% 
  full_join(., tempdo_1617) %>% 
  full_join(., tempdo_1718) %>% 
  full_join(., tempdo_1819) %>% 
  select(datetime,  
         do_ppm,
         TempC_1p5m,  
         TempC_2p5m,  
         TempC_3p5m,  
         TempC_4p5m,  
         TempC_5p5m,  
         TempC_6p5m,  
         TempC_7p5m,  
         TempC_8p5m,  
         TempC_9p5m,  
         TempC_10p5m)


winter_tempdo_14_19 %>% 
  mutate(datetime = as.character(datetime)) %>% 
  write_csv(., 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/record collations/winter/2014-2019_winterhobodotemp_L1.csv')
  
  
  
  
  
  
