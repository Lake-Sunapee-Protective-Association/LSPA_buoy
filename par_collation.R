#### collate PAR data ####
par_07 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2007_PAR_L1.csv',
                   col_types = 'Tcn')
colnames(par_07)

par_08 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2008_PAR_L1.csv',
                   col_types = 'Tcn')
colnames(par_08)

par_09 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2009_PAR_L1.csv',
                   col_types = 'Tcnc')
colnames(par_09)

par_10 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2010_PAR_L1.csv',
                   col_types = 'Tcn')
colnames(par_10)

par_11 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2011_PAR_L1.csv',
                   col_types = 'Tcnc')
colnames(par_11)

par_12 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2012_PAR_L1.csv',
                   col_types = 'Tcn')
colnames(par_12)

par_13 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2013_PAR_L1.csv',
                   col_types = 'Tcn')
colnames(par_13)

par_14 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2014_PAR_L1.csv',
                   col_types = 'Tcnc')
colnames(par_14)
par_14 <- par_14 %>% 
  rename(PAR_umolm2s = PAR)
colnames(par_14)

par_15 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2015_PAR_L1.csv',
                   col_types = 'Tnc')
colnames(par_15)
par_15 <- par_15 %>% 
  rename(PAR_umolm2s = PAR)
colnames(par_15)

par_16 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2016_PAR_L1.csv',
                   col_types = 'Tnc')
colnames(par_16)
par_16 <- par_16 %>% 
  rename(PAR_umolm2s = PAR)
colnames(par_16)

par_17 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2017_PAR_L1.csv',
                   col_types = 'Tncc')
colnames(par_17)
par_17 <- par_17 %>% 
  rename(PAR_umolm2s = PAR)
colnames(par_17)


#### collate into one file ####
par_0717 <- full_join(par_07, par_08) %>% 
  full_join(., par_09) %>% 
  full_join(., par_10) %>% 
  full_join(., par_11) %>% 
  full_join(., par_12) %>% 
  full_join(., par_13) %>% 
  full_join(., par_14) %>% 
  full_join(., par_15) %>% 
  full_join(., par_16) %>% 
  full_join(., par_17) %>% 
  mutate(PAR_umolm2s = case_when(location == 'in transit' ~ NA_real_,
                                 location == 'offline' ~ NA_real_,
                                 TRUE ~ PAR_umolm2s)) %>% 
  mutate(PAR_umolm2s = case_when(PAR_umolm2s < 0 ~ 0,
                                 TRUE ~ PAR_umolm2s))
colnames(par_0717)
unique(par_0717$PAR_flag)

ggplot(par_0717, aes(x=datetime, y=PAR_umolm2s, color =location))+
  geom_point() +
  final_theme+
  scale_color_colorblind()
  

#### export file ####
par_0717 %>% 
  mutate(datetime = as.character(datetime)) %>% 
  write_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2007-2017_PAR_L1.csv')


