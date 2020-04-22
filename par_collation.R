source('library_func_lists.R')

#### collate PAR data ####
par_07 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/met/2007_PAR_L1.csv',
                   col_types = 'Tcnc')
colnames(par_07)

par_08 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/met/2008_PAR_L1.csv',
                   col_types = 'Tcnc')
colnames(par_08)

par_09 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/met/2009_PAR_L1.csv',
                   col_types = 'Tcnc')
colnames(par_09)

par_10 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/met/2010_PAR_L1.csv',
                   col_types = 'Tcnc')
colnames(par_10)

par_11 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/met/2011_PAR_L1.csv',
                   col_types = 'Tcnc')
colnames(par_11)

par_12 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/met/2012_PAR_L1.csv',
                   col_types = 'Tcnc')
colnames(par_12)

par_13 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/met/2013_PAR_L1.csv',
                   col_types = 'Tcnc')
colnames(par_13)

par_14 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/met/2014_PAR_L1.csv',
                   col_types = 'Tcnc')
colnames(par_14)
par_14 <- par_14 %>% 
  rename(PAR_umolm2s = PAR)
colnames(par_14)

par_15 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/met/2015_PAR_L1.csv',
                   col_types = 'Tncc')
colnames(par_15)
par_15 <- par_15 %>% 
  rename(PAR_umolm2s = PAR)
colnames(par_15)

par_16 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/met/2016_PAR_L1.csv',
                   col_types = 'Tncc')
colnames(par_16)
par_16 <- par_16 %>% 
  rename(PAR_umolm2s = PAR)
colnames(par_16)

par_17 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/met/2017_PAR_L1.csv',
                   col_types = 'Tncc')
colnames(par_17)
par_17 <- par_17 %>% 
  rename(PAR_umolm2s = PAR)
colnames(par_17)

par_18 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/met/2018_PAR_L1.csv',
                   col_types = 'Tncc')
colnames(par_18)
par_18 <- par_18 %>% 
  rename(PAR_umolm2s = PAR)
colnames(par_18)

par_e19 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/met/2019_PAR_L1.csv',
                   col_types = 'Tncc')
colnames(par_e19)
par_e19 <- par_e19 %>% 
  rename(PAR_umolm2s = PAR)
colnames(par_e19)




#### collate into one file ####
par_07e19 <- full_join(par_07, par_08) %>% 
  full_join(., par_09) %>% 
  full_join(., par_10) %>% 
  full_join(., par_11) %>% 
  full_join(., par_12) %>% 
  full_join(., par_13) %>% 
  full_join(., par_14) %>% 
  full_join(., par_15) %>% 
  full_join(., par_16) %>% 
  full_join(., par_17) %>% 
  full_join(., par_18) %>% 
  full_join(., par_e19) %>% 
  mutate(PAR_umolm2s = case_when(location == 'in transit' ~ NA_real_,
                                 location == 'offline' ~ NA_real_,
                                 TRUE ~ PAR_umolm2s)) 
colnames(par_07e19)
unique(par_07e19$PAR_flag)

ggplot(par_07e19, aes(x=datetime, y=PAR_umolm2s, color =location))+
  geom_point() +
  scale_color_colorblind()
  
#edit location for EML
unique(par_07e19$location)
par_07e19 <- par_07e19 %>% 
  mutate(location = case_when(location == 'harbor, water sensors offline' ~ 'harbor',
                              TRUE ~ location))

#edit flags for EML
unique(par_07e19$PAR_flag)
par_07e19 <- par_07e19 %>% 
  mutate(PAR_flag = gsub(pattern = ', ', replacement = '; ', PAR_flag),
         PAR_flag = case_when(is.na(PAR_flag) ~ '',
                              TRUE ~ PAR_flag))

#### export file ####
par_07e19 %>% 
  mutate(datetime = as.character(datetime)) %>% 
  write_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/record collations/met/2007-e2019_PAR_L1.csv')


