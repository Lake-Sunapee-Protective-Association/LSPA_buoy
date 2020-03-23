#R version: 3.5.2
#RStudio version: 1.1.463

#updated 05July2019 to include data through May 2019
#updated 04Sept2019 to remove some wonky points in 2008 temp line data; correct the 2018 temp flags to represent the correct depth associated with them

# temp string collation
#bring in previously-collated data
alltemp0717 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2007-2017_fulltemprecord_L1.csv',
                        col_types = 'Tcnnnnnnnnnnnnnnnnccnnnnnnnnnn')

#correct summer 2015 thermistors to be NA Jun25-Jul10 (exported correctly after this collation)
alltemp0717 <- alltemp0717 %>% 
  mutate_at(vars(alltemp2019),
            funs(case_when(datetime>=as.POSIXct('2015-06-25', tz='UTC') & datetime<=as.POSIXct('2015-07-10', tz='UTC') ~ NA_real_,
                           TRUE ~ .))) %>% 
  select(datetime, location, alltemp2019, temp_flag, source)


alltemp0717v <- alltemp0717 %>% 
  gather(variable, value, -datetime, -location, -temp_flag) %>% 
  mutate(value = as.numeric(value))%>% 
  filter(!is.na(value)) 
str(alltemp0717v)
str(alltemp0717)

ggplot(alltemp0717v, aes(x=datetime, y = value, color = variable)) +
  geom_point()

#bring in data post fall 2017
hobo_1718 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2017-2018_hobotempstringdo_L1.csv',
                      col_types = 'Tnnnnnnnnnn') %>% 
  mutate(location = 'loon',
         source = 'hobo') %>% 
  select(-do_ppm)
str(hobo_1718)

# therm_18 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2018_tempstring_L1.csv',
#                     col_types = 'Tnnnnnnnnnncc') %>%
#   filter(datetime >= as.POSIXct('2018-01-01', tz='UTC') & datetime < as.POSIXct('2019-01-01', tz='UTC'))%>%
#   mutate(source = 'thermistors') %>%
#   mutate(temp_flag = case_when(datetime >= as.POSIXct('2018-05-21 9:30', tz='UTC') & datetime < as.POSIXct('2018-10-19 10:30', tz='UTC') ~ '0.85a',
#                              TRUE ~ NA_character_))

str(therm_18)
unique(therm_18$temp_flag)

hobo_1819 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2018-2019_hobotempstringdo_L1.csv',
                      col_types = 'Tnnnnnnnnnnn') %>% 
  mutate(location = 'loon',
         source = 'hobo') %>% 
  select(-do_ppm)
str(hobo_1819)






####collate buoy temp data together ####
buoy_record_temp <- full_join(alltemp0717, hobo_1718) %>% 
  full_join(., therm_18) %>% 
  full_join(., hobo_1819) %>% 
  mutate(temp_flag = case_when(is.na(temp_flag) ~ '',
                               TRUE ~ temp_flag)) %>% 
  arrange(datetime) %>% 
  mutate_at(vars(TempC_0p5m:TempC_13p5m),
            funs(case_when(location == 'in transit' ~ NA_real_,
                           location == 'offline' ~ NA_real_,
                           TRUE ~ .))) 

unique(buoy_record_temp$source)

buoy_record_temp_v <- buoy_record_temp %>% 
  gather(variable, value, -datetime, -location, -temp_flag, -source)

ggplot(buoy_record_temp_v,
       aes(x = datetime, y = value, color = variable, shape = source)) +
  geom_point() +
  final_theme 

buoy_record_temp %>%
  mutate(datetime = as.character(datetime)) %>%
  write_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2007-e2019_completerecord_L1.csv')


#### subset thermistors only ####
thermistor_buoy <- buoy_record_temp %>% 
  filter(source == 'thermistors') %>% 
  mutate(datetime = as.POSIXct(datetime, fomrat = '%Y-%m-%d %H:%M:%S', tz='UTC'))

thermistor_buoy_v <- thermistor_buoy %>% 
  gather(variable, value, -datetime, -location, -source) %>% 
  mutate(value = as.numeric(value))
str(thermistor_buoy_v)

ggplot(thermistor_buoy_v,
       aes(x = datetime, y = value, color = variable)) +
  geom_point() +
  final_theme +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
                              "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust

thermistor_buoy%>% 
  mutate(datetime = as.character(datetime)) %>% 
  select(-source) %>% 
  write_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2007-e2019_thermistorsonly_L1.csv')

thermistor_buoy %>% 
  filter(datetime < as.POSIXct('2019-01-01', tz='UTC')) %>% 
  mutate(datetime = as.character(datetime)) %>% 
  select(-source) %>% 
  write_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2007-2018_thermistorsonly_L1.csv')


#### subset hobo data (winter plus one summer) ####
winter_buoy <- buoy_record_temp %>% 
  filter(source == 'hobo')

winter_buoy_v <- winter_buoy %>% 
  gather(variable, value, -datetime, -location, -source) %>% 
  mutate(value = as.numeric(value))

ggplot(winter_buoy_v,
       aes(x = datetime, y = value, color = variable)) +
  geom_point() +
  final_theme 

winter_buoy %>% 
  mutate(datetime = as.character(datetime)) %>% 
  select(-source) %>% 
  write_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2014-e2019_hobotempstring_L1.csv')

winter_buoy %>% 
  filter(datetime < as.POSIXct('2019-01-01', tz='UTC')) %>% 
  mutate(datetime = as.character(datetime)) %>% 
  select(-source) %>% 
  write_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2014-2018_hobotempstring_L1.csv')




  
  
  
  
  
  
