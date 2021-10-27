#R version: 3.6.1
#RStudio version: 1.2.5001

source('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/programs/library_func_lists.R')

#bring in L1 temp data
temp07 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/tempstring/2007_tempstring_L1.csv')
head(temp07)

temp08 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/tempstring/2008_tempstring_L1.csv')
head(temp08)

temp09 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/tempstring/2009_tempstring_L1.csv')
head(temp09)

temp10 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/tempstring/2010_tempstring_L1.csv',
                   col_types = 'Tcnnnnnnnnnnnnnnnnc')
head(temp10)

temp11 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/tempstring/2011_tempstring_L1.csv',
                   col_types = 'Tcnnnnnnnnnn')
head(temp11)

temp12 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/tempstring/2012_tempstring_L1.csv',
                   col_types = 'Tcnnnnnnnnnn')
head(temp12)

temp13 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/tempstring/2013_tempstring_L1.csv',
                   col_types = 'Tcnnnnnnnnnn')
head(temp13)

temp14 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/tempstring/2014_tempstring_L1.csv',
                   col_types = 'Tcnnnnnnnnnnnc')
head(temp14)

temp15 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/tempstring/2015_tempstring_L1.csv',
                   col_types = 'Tnnnnnnnnnnc')
head(temp15)

temp16 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/tempstring/2016_tempstring_L1.csv',
                   col_types = 'Tnnnnnnnnnnc')
head(temp16)

temp17 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/tempstring/2017_tempstring_L1_corrdepth.csv',
                   col_types = 'Tnnnnnnnnnnc')
head(temp17)

temp18 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/tempstring/2018_tempstring_L1.csv',
                   col_types = 'Tnnnnnnnnnncc')
head(temp18)

temp19 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/tempstring/2019_tempstring_L1_corrdepths.csv',
                   col_types = 'Tnnnnnnnnnnc')
head(temp19)

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

#clean up flags for eml
alltemp_07e19 <- alltemp_07e19 %>% 
  mutate(temp_flag = gsub(pattern = ', ', replace = '; ', x = temp_flag)) %>% 
  mutate(temp_flag = gsub(pattern = ',', replace = '; ', x = temp_flag)) %>% 
  mutate(temp_flag = case_when(is.na(temp_flag) ~ '',
                               TRUE ~ temp_flag))
unique(alltemp_07e19$temp_flag)

#clean up location for eml
unique(alltemp_07e19$location)
alltemp_07e19 <- alltemp_07e19 %>% 
  mutate(location = case_when(location == 'harbor, water sensors offline' ~ 'harbor',
                              TRUE ~ location))
unique(alltemp_07e19$location)  

alltemp_07e19 %>% 
  mutate(datetime = as.character(datetime)) %>% 
  write_csv(., 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/record collations/tempstring/2007-e2019_buoy_templine_v22April2020.csv')
  
  
  
  
  
  
