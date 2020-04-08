# temp string collation

# make sure the years are jan-dec, check for dupes - in 14-17, daylight savings showed up in an odd spot in november (not the appropriate hour - so just deleting second obs)
wtr2007 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2007_tempstring_L1.csv',
                    col_types = 'Tcnnnnnnnnnnnnnnnnc') %>% 
  filter(datetime >= as.POSIXct('2007-01-01', tz='UTC') & datetime < as.POSIXct('2008-01-01', tz='UTC'))%>% 
  mutate(source = 'thermistors')
str(wtr2007)

wtr2008 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2008_tempstring_L1.csv',
                    col_types = 'Tcnnnnnnnnnnnnnnnnc') %>% 
  filter(datetime >= as.POSIXct('2008-01-01', tz='UTC') & datetime < as.POSIXct('2009-01-01', tz='UTC'))%>% 
  mutate(source = 'thermistors')
str(wtr2008)

wtr2009 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2009_tempstring_L1.csv',
                    col_types = 'Tcnnnnnnnnnnnnnnnnc') %>% 
  filter(datetime >= as.POSIXct('2009-01-01', tz='UTC') & datetime < as.POSIXct('2010-01-01', tz='UTC'))%>% 
  mutate(source = 'thermistors')
str(wtr2009)

wtr2010 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2010_tempstring_L1.csv',
                    col_types = 'Tcnnnnnnnnnnnnnnnnc') %>% 
  filter(datetime >= as.POSIXct('2010-01-01', tz='UTC') & datetime < as.POSIXct('2011-01-01', tz='UTC'))%>% 
  mutate(source = 'thermistors')
str(wtr2010)

wtr2011 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2011_tempstring_L1.csv',
                    col_types = 'Tcnnnnnnnnnn') %>% 
  filter(datetime >= as.POSIXct('2011-01-01', tz='UTC') & datetime < as.POSIXct('2012-01-01', tz='UTC'))%>% 
  mutate(source = 'thermistors')
str(wtr2011)

wtr2012 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2012_tempstring_L1.csv',
                    col_types = 'Tcnnnnnnnnnn') %>% 
  filter(datetime >= as.POSIXct('2012-01-01', tz='UTC') & datetime < as.POSIXct('2013-01-01', tz='UTC')) %>% 
  mutate(source = 'thermistors')
str(wtr2012)

wtr2013 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2013_tempstring_L1.csv',
                    col_types = 'Tcnnnnnnnnnn') %>% 
  filter(datetime >= as.POSIXct('2013-01-01', tz='UTC') & datetime < as.POSIXct('2014-01-01', tz='UTC'))%>% 
  mutate(source = 'thermistors')
str(wtr2013)

wtr2014 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2014_tempstring_L1.csv',
                    col_types = 'Tcnnnnnnnnnnnc') %>% 
  filter(datetime >= as.POSIXct('2014-01-01', tz='UTC') & datetime < as.POSIXct('2015-01-01', tz='UTC'))%>% 
  mutate(source = 'thermistors')
str(wtr2014)

wtr_w14 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2014-2015_hobotempstring_L1.csv',
                    col_types = 'Tnnnnnnnnn') %>% 
  mutate(location = 'loon',
         source = 'hobo')
str(wtr_w14)

wtr2015hobo <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2015_hobotempstring_L1.csv',
                        col_types = 'Tnnnnnnnnn') %>% 
  mutate(location = 'loon',
         source = 'hobo')
str(wtr2015hobo)

wtr2015 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2015_tempstring_L1.csv',
                    col_types = 'Tnnnnnnnnnnncc') %>%  
  filter(datetime >= as.POSIXct('2015-01-01', tz='UTC') & datetime < as.POSIXct('2016-01-01', tz='UTC'))%>% 
  mutate(source = 'thermistors')
str(wtr2015)

wtr_w15 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2015-2016_hobotempstring_L1.csv')%>% 
  mutate(location = 'loon',
         source = 'hobo')
str(wtr_w15)

wtr2016 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2016_tempstring_L1.csv',
                    col_types = 'Tnnnnnnnnnnc') %>%
  filter(datetime >= as.POSIXct('2016-01-01', tz='UTC') & datetime < as.POSIXct('2017-01-01', tz='UTC'))%>% 
  mutate(source = 'thermistors')
str(wtr2016)

wtr_w16 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2016-2017_hobotempstring_L1.csv',
                    col_types = 'Tnnnnnnnnn') %>% 
  mutate(location = 'loon',
         source = 'hobo')
str(wtr_w16)

wtr2017 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2017_tempstring_L1_corrdepth.csv',
                    col_types = 'Tnnnnnnnnnnc') %>%
  filter(datetime >= as.POSIXct('2017-01-01', tz='UTC') & datetime < as.POSIXct('2018-01-01', tz='UTC'))%>% 
  mutate(source = 'thermistors')
str(wtr2017)


####collate buoy temp data together ####
buoy_record_temp <- full_join(wtr2007, wtr2008) %>% 
  full_join(., wtr2009) %>% 
  full_join(., wtr2010) %>% 
  full_join(., wtr2011) %>% 
  full_join(., wtr2012) %>% 
  full_join(., wtr2013) %>% 
  full_join(., wtr2014) %>% 
  full_join(., wtr2015) %>% 
  full_join(., wtr2016) %>% 
  full_join(., wtr2017) %>% 
  select(datetime, location,
         TempC_0p5m, TempC_0p85m, 
         TempC_1p5m, TempC_1p85m, 
         TempC_2p5m, TempC_2p85m, 
         TempC_3p5m, TempC_3p85m, 
         TempC_4p5m, TempC_4p85m, 
         TempC_5p5m, TempC_5p85m, 
         TempC_6p5m, TempC_6p85m, 
         TempC_7p5m, TempC_7p85m, 
         TempC_8p5m, TempC_8p85m, 
         TempC_9p5m, TempC_9p85m, 
         TempC_10p5m, TempC_11p5m, 
         TempC_13p5m, temp_flag,
         source) %>% 
  mutate(temp_flag = case_when(is.na(temp_flag) ~ '',
                               TRUE ~ temp_flag)) %>% 
  arrange(datetime) %>% 
  mutate(temp_flag = case_when(temp_flag == 'q' ~ 's',
                               temp_flag == '1q' ~ '1s',
                               TRUE ~ temp_flag)) %>% 
  mutate_at(vars(TempC_0p5m:TempC_13p5m),
            funs(case_when(location == 'in transit' ~ NA_real_,
                           location == 'offline' ~ NA_real_,
                           TRUE ~ .)))

unique(buoy_record_temp$source)

buoy_record_temp_v <- buoy_record_temp %>% 
  gather(variable, value, -datetime, -location, -temp_flag, -source)

# ggplot(buoy_record_temp_v,
#        aes(x = datetime, y = value, color = variable)) +
#   geom_point() +
#   final_theme +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust


#add 11.5 and 13.5 flags for possibly in sediment
buoy_record_temp <- buoy_record_temp %>% 
  mutate(temp_flag = case_when(temp_flag==''~ NA_character_,
                               !is.na(TempC_11p5m) & is.na(temp_flag) ~ '11.5b',
                               !is.na(TempC_11p5m) & !is.na(temp_flag) ~ paste(temp_flag, '11.5b', sep = ', '),
                               !is.na(TempC_13p5m) & is.na(temp_flag) ~ '13.5b',
                               !is.na(TempC_13p5m) & !is.na(temp_flag) ~ paste(temp_flag, '13.5b', sep = ', ')))

unique(buoy_record_temp$temp_flag)

buoy_record_temp %>%
  mutate(datetime = as.character(datetime)) %>%
  write_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2007-2017_tempstring_L1.csv')


#### buoy summer 2015 hobo ####
wtr2015hobo %>% 
  mutate(datetime = as.character(datetime)) %>% 
  write_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2015summerhobo_L1.csv')



#### buoy winter data ####
winter_buoy <- full_join(wtr_w14, wtr_w15) %>% 
  full_join(., wtr_w16)

winter_buoy_v <- winter_buoy %>% 
  gather(variable, value, -datetime, -location, -source)

# ggplot(winter_buoy_v,
#        aes(x = datetime, y = value, color = variable)) +
#   geom_point() +
#   final_theme +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust

winter_buoy %>% 
  mutate(datetime = as.character(datetime)) %>% 
  select(-source) %>% 
  write_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2014-2016_wintertempstring_L1.csv')

###trim data that overlap for full record####
head(wtr_w14)
wtr2014 <- wtr2014 %>%
  filter(datetime < as.POSIXct('2014-10-14 12:00', tz='UTC'))

tail(wtr_w14)
wtr2015 <- wtr2015 %>%
  filter(datetime > as.POSIXct('2015-06-11 8:30', tz='UTC'))

head(wtr2015hobo)
tail(wtr2015hobo)
wtr2015 <- wtr2015 %>%
  filter(datetime < as.POSIXct('2015-08-07 14:15', tz='UTC') | datetime > as.POSIXct('2015-09-28 14:15', tz='UTC'))

head(wtr_w15)
wtr2015 <- wtr2015 %>%
  filter(datetime < as.POSIXct('2015-10-08 12:00', tz='UTC'))

tail(wtr_w15)
head(wtr_w16)
wtr2016 <- wtr2016 %>%
  filter(datetime > as.POSIXct('2016-05-03 11:00', tz='UTC') | datetime < as.POSIXct('2016-10-12 12:00'))

tail(wtr_w16)
wtr2017 <- wtr2017 %>%
  filter(datetime > as.POSIXct('2017-05-17 10:15', tz='UTC'))

####collate full temp data together ####
full_record_temp <- full_join(wtr2007, wtr2008) %>%
  full_join(., wtr2009) %>%
  full_join(., wtr2010) %>%
  full_join(., wtr2011) %>%
  full_join(., wtr2012) %>%
  full_join(., wtr2013) %>%
  full_join(., wtr2014) %>%
  full_join(., wtr_w14) %>%
  full_join(., wtr2015) %>%
  full_join(., wtr2015hobo) %>%
  full_join(., wtr_w15) %>%
  full_join(., wtr2016) %>%
  full_join(., wtr_w16) %>%
  full_join(., wtr2017) %>%
  mutate(temp_flag = case_when(is.na(temp_flag) ~ '',
                               TRUE ~ temp_flag)) %>%
  arrange(datetime) %>%
  mutate(temp_flag = case_when(temp_flag == 'q' ~ 's',  #recode q as s
                               temp_flag == '1q' ~ '1s',
                               TRUE ~ temp_flag))%>%
  mutate_at(vars(TempC_0p5m:TempC_13p5m),
            funs(case_when(location == 'in transit' ~ NA_real_,
                           location == 'offline' ~ NA_real_,
                           TRUE ~ .)))
full_record_temp <- full_record_temp %>% 
  mutate(temp_flag = case_when(temp_flag==''~ NA_character_,
                               !is.na(TempC_11p5m) & is.na(temp_flag) ~ '11.5b',
                               !is.na(TempC_11p5m) & !is.na(temp_flag) ~ paste(temp_flag, '11.5b', sep = ', '),
                               !is.na(TempC_13p5m) & is.na(temp_flag) ~ '13.5b',
                               !is.na(TempC_13p5m) & !is.na(temp_flag) ~ paste(temp_flag, '13.5b', sep = ', ')))


full_record_temp_v <- full_record_temp %>%
  gather(variable, value, -datetime, -location, -temp_flag, -source)


# ggplot(subset(full_record_temp_v,
#               subset = datetime < as.POSIXct('2010-01-01', tz='UTC')),
#        aes(x = datetime, y = value, color = variable, shape = temp_flag)) +
#   geom_point() +
#   final_theme +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
#
# ggplot(subset(full_record_temp_v,
#               subset = (datetime >= as.POSIXct('2009-01-01', tz='UTC') & datetime < as.POSIXct('2011-01-01', tz='UTC'))),
#        aes(x = datetime, y = value, color = variable, shape = temp_flag)) +
#   geom_point() +
#   final_theme +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
#
# ggplot(subset(full_record_temp_v,
#               subset = (datetime >= as.POSIXct('2010-01-01', tz='UTC') & datetime < as.POSIXct('2012-01-01', tz='UTC'))),
#        aes(x = datetime, y = value, color = variable, shape = temp_flag)) +
#   geom_point() +
#   final_theme +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
#
# ggplot(subset(full_record_temp_v,
#               subset = (datetime >= as.POSIXct('2011-01-01', tz='UTC') & datetime < as.POSIXct('2013-01-01', tz='UTC'))),
#        aes(x = datetime, y = value, color = variable, shape = temp_flag)) +
#   geom_point() +
#   final_theme +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
#
# ggplot(subset(full_record_temp_v,
#               subset = (datetime >= as.POSIXct('2012-01-01', tz='UTC') & datetime < as.POSIXct('2014-01-01', tz='UTC'))),
#        aes(x = datetime, y = value, color = variable, shape = temp_flag)) +
#   geom_point() +
#   final_theme +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
#
# ggplot(subset(full_record_temp_v,
#               subset = (datetime >= as.POSIXct('2013-01-01', tz='UTC') & datetime < as.POSIXct('2015-01-01', tz='UTC'))),
#        aes(x = datetime, y = value, color = variable, shape = temp_flag)) +
#   geom_point() +
#   final_theme +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
#
# ggplot(subset(full_record_temp_v,
#               subset = (datetime >= as.POSIXct('2014-01-01', tz='UTC') & datetime < as.POSIXct('2016-01-01', tz='UTC'))),
#        aes(x = datetime, y = value, color = variable, shape = temp_flag)) +
#   geom_point() +
#   final_theme +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
#
# ggplot(subset(full_record_temp_v,
#               subset = (datetime >= as.POSIXct('2015-01-01', tz='UTC') & datetime < as.POSIXct('2017-01-01', tz='UTC'))),
#        aes(x = datetime, y = value, color = variable, shape = temp_flag)) +
#   geom_point() +
#   final_theme +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
#
# ggplot(subset(full_record_temp_v,
#               subset = (datetime >= as.POSIXct('2016-01-01', tz='UTC') & datetime < as.POSIXct('2018-01-01', tz='UTC'))),
#        aes(x = datetime, y = value, color = variable, shape = temp_flag)) +
#   geom_point() +
#   final_theme +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust
#
# ggplot(full_record_temp_v,
#        aes(x = datetime, y = value, color = variable, shape = temp_flag)) +
#   geom_point() +
#   final_theme +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust

ggplot(full_record_temp_v,
       aes(x = datetime, y = value, color = variable, shape = temp_flag)) +
  geom_point() +
  facet_grid(location ~ .) +
  final_theme +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
                              "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust

full_record_temp %>%
  mutate(datetime = as.character(datetime)) %>%
  write_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2007-2017_fulltemprecord_L1.csv')


  
  
  
  
  
  
