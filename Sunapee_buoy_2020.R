#*****************************************************************
#*      Cary Institute of Ecosystem Studies (Millbrook, NY)      *
#*                                                               *
#* TITLE:   Sunapee_buoy_2020.r                                  *
#* AUTHOR:  Bethel Steele                                        *
#* PROJECT: Lake Sunapee Buoy Data Cleaning                      *
#*****************************************************************

source('library_func_lists.R')

#point to data directories
raw_dir = 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L0/'
dump_dir = 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/'

#set tz
buoy_tz = 'Etc/GMT+5'

#bring in  buoy raw data
buoy2020_L0 <- read.csv(file.path(raw_dir, '2020 Buoy Data.csv'))

#### format data ####
buoy2020_L0 <- buoy2020_L0  %>%
  rename(DOLowTempC = 'DOLoTempC',
         AveWindSp = 'WindSpdAv',
         AveWindDir = 'WindVect',
         MaxWindSp = 'MaxWind') %>% 
  mutate(hour = Hr.Min%/%100,
         minutes = Hr.Min%%100,
         time = paste(hour, minutes, sep=':')) %>% #break out time from Hr.Min, create time column
  mutate(date = as.Date(paste(Day, Year, sep = '-'), format='%j-%Y'), #create date in ymd format
         datetime = as.POSIXct(paste(date, time, sep=' '), format='%Y-%m-%d %H:%M', tz=buoy_tz)) %>%  
  select(-hour, -minutes, -Hr.Min, -Year, -Day, -time, -ArrayID) %>% #remove unnecessary columns
  rownames_to_column(var ='rowid')
head(buoy2020_L0)


#plot the battery levels
ggplot(buoy2020_L0, aes(x = datetime, y = LoggerBatV)) +
  geom_point()

ggplot(buoy2020_L0, aes(x = datetime, y = RadioBatV)) +
  geom_point()
# looks fine 

#double check to make sure there are no DST issues
datelength2020 <- buoy2020_L0 %>% 
  mutate(date = format(datetime, '%Y-%m-%d')) %>% 
  group_by(date) %>% 
  summarize(length(datetime))

#check dst dates
datelength2020[datelength2020$date == '2020-03-08',]
#dst observed
datelength2020[datelength2020$date == '2020-11-01',]
#dst observed on the 2nd
datelength2020[datelength2020$date == '2020-11-02',]

# see where dst occurs
buoy2020_L0 %>% 
  filter(datetime >= as.POSIXct('2020-03-08', tz=buoy_tz) & 
           datetime < as.POSIXct('2020-03-09', tz=buoy_tz)) %>% 
  select(datetime, rowid)

buoy2020_L0 %>% 
  filter(datetime >= as.POSIXct('2020-11-02', tz=buoy_tz) & 
           datetime < as.POSIXct('2020-11-03', tz=buoy_tz)) %>% 
  select(datetime, rowid)


#DST observed at odd times - 03-08-15 23:00; 11-02-15 00:00
buoy2020_L1 <- buoy2020_L0 %>% 
  mutate(datetime.instrument = datetime,
         rowid = as.numeric(rowid))
buoy2020_L1a <- buoy2020_L1 %>% 
  filter(rowid <= 9432)
buoy2020_L1b <- buoy2020_L1 %>% 
  filter(rowid > 9432 & rowid <= 43531)
buoy2020_L1c <- buoy2020_L1 %>% 
  filter(rowid > 43531)
#apply time math on middle section
buoy2020_L1b <- buoy2020_L1b %>% 
  mutate(datetime = datetime-hours(1))

#rejoin and force tz as buoytz
buoy2020_L1 <- full_join(buoy2020_L1a, buoy2020_L1b) %>% 
  full_join(., buoy2020_L1c) %>% 
  mutate(datetime = as.POSIXct(datetime, tz = buoy_tz))

#double check to make sure there are no DST issues
datelength2020 <- buoy2020_L1 %>% 
  mutate(date = format(datetime, '%Y-%m-%d')) %>% 
  group_by(date) %>% 
  summarize(length(datetime))

datelength2020[datelength2020$date == '2020-03-08',]
datelength2020[datelength2020$date == '2020-11-01',]
datelength2020[datelength2020$date == '2020-11-02',]
#all good!

# add in all date time options in L1 data set
alltimes_2020 <- as.data.frame(seq.POSIXt(as.POSIXct('2020-01-01 00:00', tz=buoy_tz), as.POSIXct('2020-12-31 23:50', tz=buoy_tz), '10 min')) %>% 
  rename("datetime" = !!names(.[1]))

buoy2020_L1 <- buoy2020_L1 %>% 
  right_join(., alltimes_2020) %>% 
  arrange(datetime)

####THERMISTORS####
buoy2020_therm_vert <- buoy2020_L1 %>% 
  select(datetime, all_of(alltemp2011)) %>%
  gather(variable, value, -datetime)

# #plot to see
# ggplot(buoy2020_therm_vert, aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 month') +
#   final_theme

#recode NA strings of -6999
buoy2020_L1 <- buoy2020_L1 %>% 
  mutate_at(vars(all_of(alltemp2011)),
            ~case_when(. < -5000 ~ NA_real_, #note there were some oddball NA values this year, had to change to accomodate that. 
                           TRUE ~ .))
buoy2020_therm_vert <- buoy2020_L1 %>% 
  select(datetime, all_of(alltemp2011)) %>%
  gather(variable, value, -datetime)

# #plot together to see relative values
# ggplot(buoy2020_therm_vert,
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 month') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme

#look at 0m only 
ggplot(buoy2020_L1, aes(x = datetime, y = TempC_0m)) +
  geom_point()
#probably some usable data in there; see if we can get it out

buoy2020_L1 <- buoy2020_L1 %>% 
  mutate(TempC_0m = case_when(TempC_0m >30 ~ NA_real_,
                              TempC_0m <= 5 ~ NA_real_,
                              TempC_0m == 15 ~ NA_real_,
                              TempC_0m == 9 ~ NA_real_,
                              TRUE ~ TempC_0m))

ggplot(buoy2020_L1, aes(x = datetime, y = TempC_0m)) +
  geom_point()

#add intermittent flag for 0m data
buoy2020_L1 <- buoy2020_L1 %>% 
  mutate(flag_temp_0p75m = 'i')

# sensor testing april 30;; deployed 3 May
buoy2020_L1 <- buoy2020_L1 %>% 
  mutate_at(vars(alltemp2011),
            ~ case_when(datetime < as.POSIXct('2020-05-03', tz=buoy_tz) ~ NA_real_,
                        TRUE ~ .)) %>% 
  mutate(TempC_0m = case_when(TempC_0m>200 ~ NA_real_, #one wonky value that messes with visualization.
                              TRUE ~ TempC_0m))
buoy2020_therm_vert <- buoy2020_L1 %>% 
  select(datetime, all_of(alltemp2011)) %>%
  gather(variable, value, -datetime)

#plot together to see relative values, without pre-deployment blip 
ggplot(buoy2020_therm_vert,
       aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  scale_x_datetime(date_minor_breaks = '1 month') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  final_theme

# plot by 2-week period, save plot.

# set time period of interest:
start_date = '2020-01-01'
end_date = '2021-01-01'

#create a list of weeks during time period of interest
weekly_2020 <- seq(as.Date(start_date), as.Date(end_date), '2 weeks')  %>% 
  as.data.frame(.) %>% 
  dplyr::rename(date = '.') %>% 
  filter(date< as.Date('2020-12-30')) %>% 
  add_row(date = as.Date('2021-01-01'))

# #plot data in 2-week iterations
# for(i in 1:(nrow(weekly_2020)-1)) {
#   chunk <- buoy2020_L1 %>% 
#     filter(datetime >= as.POSIXct(weekly_2020$date[i], tz= buoy_tz) &
#              datetime < as.POSIXct(weekly_2020$date[i+1], tz=buoy_tz))
#   chunk_vert <- chunk %>% 
#     select(datetime, all_of(alltemp2011)) %>%
#     gather(variable, value, -datetime)
#   ggplot(chunk_vert, aes(x = datetime, y = value, color = variable)) +
#     geom_point() +
#     scale_x_datetime(date_minor_breaks = '1 day') +
#     scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                                 "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#     final_theme +
#     labs(x = NULL,
#          y = 'water temperature (degrees C)',
#          title = paste0('LSPA thermistors ', weekly_2020$date[i], ' - ', weekly_2020$date[i+1]))
#   ggsave(filename = paste0('graphs/2020/L0_therm_biweekly_', weekly_2020$date[i], '.jpg'), height = 8, width =10, units = 'in', dpi = 300)
# }

#buoy deployment
deployment = '2020-05-03'
# ggplot(subset(buoy2020_therm_vert,
#               subset=(datetime >= as.POSIXct(deployment, tz=buoy_tz) & datetime < (as.POSIXct(deployment, tz=buoy_tz) + days(1)))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme

# recode data prior to loon; add location information
buoy2020_L1 <- buoy2020_L1 %>% 
  mutate(location = case_when(datetime >= as.POSIXct('2020-05-03 10:20', tz=buoy_tz) ~ 'loon',
                              datetime < as.POSIXct('2020-05-03 10:20', tz=buoy_tz) ~ 'harbor',
                              TRUE ~ '')) %>% 
  mutate_at(vars(all_of(alltemp2011)),
            ~ case_when(location == 'harbor' ~ NA_real_,
                        TRUE ~ .))
buoy2020_therm_vert_L1 <- buoy2020_L1 %>% 
  select(datetime, all_of(alltemp2011), location) %>%
  gather(variable, value, -datetime, -location)

# ggplot(subset(buoy2020_therm_vert_L1,
#               subset=(datetime >= as.POSIXct(deployment, tz=buoy_tz) & datetime < (as.POSIXct(deployment, tz=buoy_tz) + days(1)))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme

#Jun 2 - Jun 3
look_date_start = '2020-06-02'
look_date_end = '2020-06-03'

# ggplot(subset(buoy2020_therm_vert_L1,
#               subset=(datetime >= as.POSIXct(look_date_start, tz=buoy_tz) & datetime < (as.POSIXct(look_date_end, tz=buoy_tz) + days(1)))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme

# recode chunk of intermittent data
buoy2020_L1 <- buoy2020_L1 %>% 
  mutate_at(vars(all_of(alltemp2011)),
            ~ case_when(datetime >= as.Date(look_date_start) & datetime < as.POSIXct(paste(look_date_end, '10:30', sep = ' '), tz = buoy_tz) ~ NA_real_,
                        TRUE ~ .))
buoy2020_therm_vert_L1 <- buoy2020_L1 %>% 
  select(datetime, all_of(alltemp2011), location) %>%
  gather(variable, value, -datetime, -location)

# ggplot(subset(buoy2020_therm_vert_L1,
#               subset=(datetime >= as.POSIXct(look_date_start, tz=buoy_tz) & datetime < (as.POSIXct(look_date_end, tz=buoy_tz) + days(1)))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme

# May 30/31 stray temp reading
look_date = '2020-05-30'
# ggplot(subset(buoy2020_therm_vert_L1,
#               subset=(datetime >= as.POSIXct(look_date, tz=buoy_tz) & datetime < (as.POSIXct(look_date, tz=buoy_tz) + days(1)))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme

buoy2020_L1 <- buoy2020_L1 %>% 
  mutate_at(vars(all_of(alltemp2011)),
            ~ case_when(datetime >= as.Date(look_date) & 
                          datetime < (as.POSIXct(paste(look_date, '00:00'), tz = buoy_tz) + days(2)) ~ NA_real_,
                        TRUE ~ .))
buoy2020_therm_vert_L1 <- buoy2020_L1 %>% 
  select(datetime, all_of(alltemp2011), location) %>%
  gather(variable, value, -datetime, -location)

# ggplot(subset(buoy2020_therm_vert_L1,
#               subset=(datetime >= as.POSIXct(look_date, tz=buoy_tz) & datetime < (as.POSIXct(look_date, tz=buoy_tz) + days(1)))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme

#mid sept stray 0m mpoint 19-20
look_date = '2020-09-19'
# ggplot(subset(buoy2020_therm_vert_L1,
#               subset=(datetime >= as.POSIXct(look_date, tz=buoy_tz) & datetime < (as.POSIXct(look_date, tz=buoy_tz) + days(1)))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme

buoy2020_L1 <- buoy2020_L1 %>% 
  mutate(TempC_0m = case_when(datetime == as.POSIXct(paste(look_date, '23:40'), tz = buoy_tz) ~ NA_real_,
                        TRUE ~ TempC_0m))
buoy2020_therm_vert_L1 <- buoy2020_L1 %>% 
  select(datetime, all_of(alltemp2011), location) %>%
  gather(variable, value, -datetime, -location)

# ggplot(subset(buoy2020_therm_vert_L1,
#               subset=(datetime >= as.POSIXct(look_date, tz=buoy_tz) & datetime < (as.POSIXct(look_date, tz=buoy_tz) + days(1)))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme

#beg of oct 0m point 4
look_date = '2020-10-04'
# ggplot(subset(buoy2020_therm_vert_L1,
#               subset=(datetime >= as.POSIXct(look_date, tz=buoy_tz) & datetime < (as.POSIXct(look_date, tz=buoy_tz) + days(1)))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme

buoy2020_L1 <- buoy2020_L1 %>% 
  mutate(TempC_0m = case_when(datetime == as.POSIXct(paste(look_date, '04:40'), tz = buoy_tz) ~ NA_real_,
                              TRUE ~ TempC_0m))
buoy2020_therm_vert_L1 <- buoy2020_L1 %>% 
  select(datetime, all_of(alltemp2011), location) %>%
  gather(variable, value, -datetime, -location)
# ggplot(subset(buoy2020_therm_vert_L1,
#               subset=(datetime >= as.POSIXct(look_date, tz=buoy_tz) & datetime < (as.POSIXct(look_date, tz=buoy_tz) + days(1)))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme

#buoy removal 
removal = '2020-10-05'
# ggplot(subset(buoy2020_therm_vert,
#               subset=(datetime >= as.POSIXct(removal, tz=buoy_tz) & datetime < (as.POSIXct(removal, tz=buoy_tz) + days(1)))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme

#recode data after 10:30 (inclusive), update location to in-transit. loon will be added by do string
buoy2020_L1 <- buoy2020_L1 %>% 
  mutate_at(vars(all_of(alltemp2011)),
            ~ case_when(datetime >= as.POSIXct('2020-10-05 9:00', tz=buoy_tz) & datetime < as.Date('2020-10-08') ~ NA_real_,
                        TRUE ~ .)) %>% 
  mutate(location = case_when(datetime >= as.POSIXct('2020-10-05 9:00', tz=buoy_tz) ~ 'in transit',
                              TRUE ~ location))
buoy2020_therm_vert_L1 <- buoy2020_L1 %>% 
  select(datetime, alltemp2011, location) %>%
  gather(variable, value, -datetime, -location)

# ggplot(subset(buoy2020_therm_vert_L1,
#               subset=(datetime >= as.POSIXct(removal, tz=buoy_tz) & datetime < (as.POSIXct(removal, tz=buoy_tz) + days(1)))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme

# install of 1m temp 
winter_temp_add = '2020-10-08'
# ggplot(subset(buoy2020_therm_vert_L1,
#               subset=(datetime >= as.POSIXct(winter_temp_add, tz=buoy_tz) & datetime < (as.POSIXct(winter_temp_add, tz=buoy_tz) + days(1)))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme

buoy2020_L1 <- buoy2020_L1 %>% 
  mutate_at(vars(all_of(alltemp2011)),
            ~ case_when(datetime >= as.POSIXct('2020-10-05 9:00', tz=buoy_tz) & datetime < as.POSIXct('2020-10-08 10:50', tz= buoy_tz) ~ NA_real_,
                        TRUE ~ .)) 
buoy2020_therm_vert_L1 <- buoy2020_L1 %>% 
  select(datetime, all_of(alltemp2011), location) %>%
  gather(variable, value, -datetime, -location)

# ggplot(subset(buoy2020_therm_vert_L1,
#               subset=(datetime >= as.POSIXct(winter_temp_add, tz=buoy_tz) & datetime < (as.POSIXct(winter_temp_add, tz=buoy_tz) + days(1)))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme

ggplot(buoy2020_therm_vert_L1,
       aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  scale_x_datetime(date_minor_breaks = '1 month') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  final_theme

# for(i in 1:(nrow(weekly_2020)-1)) {
#   chunk <- buoy2020_L1 %>% 
#     filter(datetime >= as.POSIXct(weekly_2020$date[i], tz= buoy_tz) &
#              datetime < as.POSIXct(weekly_2020$date[i+1], tz=buoy_tz))
#   chunk_vert <- chunk %>% 
#     select(datetime, all_of(alltemp2011)) %>%
#     gather(variable, value, -datetime)
#   ggplot(chunk_vert, aes(x = datetime, y = value, color = variable)) +
#     geom_point() +
#     scale_x_datetime(date_minor_breaks = '1 day') +
#     scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                                 "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#     final_theme +
#     labs(x = NULL,
#          y = 'water temperature (degrees C)',
#          title = paste0('LSPA thermistors ', weekly_2020$date[i], ' - ', weekly_2020$date[i+1]))
#   ggsave(filename = paste0('graphs/2020/L1_bitherm_weekly_', weekly_2020$date[i], '.jpg'), height = 8, width =10, units = 'in', dpi = 300)
# }


#correct column names for sensor offset
buoy2020_L1 <- buoy2020_L1 %>% 
  rename(waterTemperature_degC_9p75m = TempC_9m,
         waterTemperature_degC_8p75m = TempC_8m,
         waterTemperature_degC_7p75m = TempC_7m,
         waterTemperature_degC_6p75m = TempC_6m,
         waterTemperature_degC_5p75m = TempC_5m,
         waterTemperature_degC_4p75m = TempC_4m,
         waterTemperature_degC_3p75m = TempC_3m,
         waterTemperature_degC_2p75m = TempC_2m,
         waterTemperature_degC_1p75m = TempC_1m,
         waterTemperature_degC_0p75m = TempC_0m)

#clean up workspace
rm(buoy2020_therm_vert, buoy2020_therm_vert_L1)

#### DO ####
buoy2020_do_vert <- buoy2020_L1 %>% 
  select(datetime, all_of(upDO), all_of(lowDO), location) %>%
  gather(variable, value, -datetime, -location)

# ggplot(buoy2020_do_vert, aes(x=datetime, y=value, color = location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 month') +
#   final_theme

#recode NA values 
buoy2020_L1 <- buoy2020_L1 %>% 
  mutate_at(vars(all_of(upDO), all_of(lowDO)),
            funs(case_when(. == -6999 ~ NA_real_,
                           TRUE ~ .))) 

buoy2020_do_vert_L1 <- buoy2020_L1 %>%
  select(datetime, all_of(upDO), all_of(lowDO), location) %>%
  gather(variable, value, -datetime, -location)

ggplot(buoy2020_do_vert_L1, aes(x=datetime, y=value, color = location)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  scale_x_datetime(date_minor_breaks = '1 month') +
  final_theme

# #look at the DO data on a biweekly basis
# for(i in 1:(nrow(weekly_2020)-1)) {
#   chunk <- buoy2020_L1 %>% 
#     filter(datetime >= as.POSIXct(weekly_2020$date[i], tz= buoy_tz) &
#              datetime < as.POSIXct(weekly_2020$date[i+1], tz=buoy_tz))
#   chunk_vert <- chunk %>% 
#     select(datetime, all_of(upDO), all_of(lowDO)) %>%
#     gather(variable, value, -datetime)
#   ggplot(chunk_vert, aes(x = datetime, y = value)) +
#     geom_point() +
#     facet_grid(variable ~ ., scales = 'free_y') +
#     scale_x_datetime(date_minor_breaks = '1 day') +
#     scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                                 "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#     final_theme +
#     labs(x = NULL,
#          y = NULL,
#          title = paste0('LSPA DO sensors ', weekly_2020$date[i], ' - ', weekly_2020$date[i+1]))
#   ggsave(filename = paste0('graphs/2020/L0_do_biweekly_', weekly_2020$date[i], '.jpg'), height = 8, width =10, units = 'in', dpi = 300)
# }

#april 30, upper do/lower DO in water, lower has issues
look_date = '2020-04-30'
# ggplot(subset(buoy2020_do_vert_L1,
#               subset=(datetime >= as.POSIXct(look_date, tz=buoy_tz) & datetime < (as.POSIXct(look_date, tz=buoy_tz) + days(1)))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme
# hard to tell if the do is in water (by data and operation log), need to recode everything prior to deployment at loon.

# #buoy deployment 
# ggplot(subset(buoy2020_do_vert_L1,
#               subset=(datetime >= as.POSIXct(deployment, tz=buoy_tz) & datetime < (as.POSIXct(deployment, tz=buoy_tz) + days(1)))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme

#recode prior to deployment - allow for 3 hours of equilibration; recode all of upDO until Aug 6
buoy2020_L1 <- buoy2020_L1 %>% 
mutate_at(vars(all_of(lowDO), all_of(upDO)),
          ~ case_when(datetime < (as.POSIXct(paste(deployment, '10:00', sep = ' '), tz=buoy_tz) + hours(3)) ~ NA_real_,
                         TRUE ~ .)) %>% 
  mutate_at(vars(all_of(lowDO)),
            ~ case_when(datetime < as.Date('2020-08-06') ~ NA_real_,
                        TRUE ~ .)) %>% 
  mutate(flag_do1p5m = case_when(datetime == (as.POSIXct(paste(deployment, '10:00', sep = ' '), tz=buoy_tz) + hours(3)) ~ 'c',
                                   TRUE ~ ''),
         flag_do10p5m = case_when(datetime == (as.POSIXct(paste(deployment, '10:00', sep = ' '), tz=buoy_tz) + hours(3)) ~ 'c',
                                   TRUE ~ ''))

buoy2020_do_vert_L1 <- buoy2020_L1 %>% 
  select(datetime, all_of(lowDO), all_of(upDO)) %>%
  gather(variable, value, -datetime)

# ggplot(subset(buoy2020_do_vert_L1,
#               subset=(datetime >= as.POSIXct(deployment, tz=buoy_tz) & datetime < (as.POSIXct(deployment, tz=buoy_tz) + days(1)))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme

#may 24
look_date = '2020-05-24'
# ggplot(subset(buoy2020_do_vert_L1,
#               subset=(datetime >= as.POSIXct(look_date, tz=buoy_tz) & datetime < (as.POSIXct(look_date, tz=buoy_tz) + days(1)))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme

buoy2020_L1 <- buoy2020_L1 %>% 
  mutate_at(vars(all_of(upDO)),
            ~ case_when(datetime >= as.POSIXct(paste(look_date, '11:00', sep = ' '), tz= buoy_tz) &
                          datetime < as.POSIXct(paste(look_date, '19:50', sep = ' '), tz= buoy_tz) ~ NA_real_,
                        TRUE ~ .))

buoy2020_do_vert_L1 <- buoy2020_L1 %>% 
  select(datetime, all_of(lowDO), all_of(upDO)) %>%
  gather(variable, value, -datetime)

# ggplot(subset(buoy2020_do_vert_L1,
#               subset=(datetime >= as.POSIXct(look_date, tz=buoy_tz) & datetime < (as.POSIXct(look_date, tz=buoy_tz) + days(1)))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme

#june 5
look_date = '2020-06-05'
# ggplot(subset(buoy2020_do_vert_L1,
#               subset=(datetime >= as.POSIXct(look_date, tz=buoy_tz) & datetime < (as.POSIXct(look_date, tz=buoy_tz) + days(1)))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme

buoy2020_L1 <- buoy2020_L1 %>% 
  mutate_at(vars(all_of(upDO)),
            ~ case_when(datetime >= as.POSIXct(paste(look_date, '11:00', sep = ' '), tz= buoy_tz) &
                          datetime < as.POSIXct(paste(look_date, '12:00', sep = ' '), tz= buoy_tz) ~ NA_real_,
                        TRUE ~ .))

buoy2020_do_vert_L1 <- buoy2020_L1 %>% 
  select(datetime, all_of(lowDO), all_of(upDO)) %>%
  gather(variable, value, -datetime)

# ggplot(subset(buoy2020_do_vert_L1,
#               subset=(datetime >= as.POSIXct(look_date, tz=buoy_tz) & datetime < (as.POSIXct(look_date, tz=buoy_tz) + days(1)))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme

#august 6
look_date = '2020-08-06'
# ggplot(subset(buoy2020_do_vert_L1,
#               subset=(datetime >= as.POSIXct(look_date, tz=buoy_tz) & datetime < (as.POSIXct(look_date, tz=buoy_tz) + days(1)))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme

buoy2020_L1 <- buoy2020_L1 %>% 
  mutate_at(vars(all_of(upDO)),
            ~ case_when(datetime == as.POSIXct(paste(look_date, '11:30', sep = ' '), tz= buoy_tz) ~ NA_real_,
                        TRUE ~ .)) %>% 
  mutate_at(vars(all_of(lowDO)),
            ~ case_when(datetime < as.POSIXct(paste(look_date, '12:20', sep = ' '), tz=buoy_tz) ~ NA_real_,
                        TRUE ~ .)) %>% 
  mutate(flag_do10p5m = case_when(datetime == as.POSIXct(paste(look_date, '12:20', sep = ' '), tz=buoy_tz) ~ 'x; cp',
                                   datetime > as.POSIXct(paste(look_date, '12:20', sep = ' '), tz=buoy_tz) ~ 'x',
                                   TRUE ~ flag_do10p5m))

buoy2020_do_vert_L1 <- buoy2020_L1 %>% 
  select(datetime, all_of(lowDO), all_of(upDO)) %>%
  gather(variable, value, -datetime)

# ggplot(subset(buoy2020_do_vert_L1,
#               subset=(datetime >= as.POSIXct(look_date, tz=buoy_tz) & datetime < (as.POSIXct(look_date, tz=buoy_tz) + days(1)))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme

#note that saturation is REALLY high for the low DO here.

#aug 12
look_date = '2020-08-12'
# ggplot(subset(buoy2020_do_vert_L1,
#               subset=(datetime >= as.POSIXct(look_date, tz=buoy_tz) & datetime < (as.POSIXct(look_date, tz=buoy_tz) + days(1)))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme

buoy2020_L1 <- buoy2020_L1 %>% 
  mutate_at(vars(all_of(upDO)),
            ~ case_when(datetime >= as.POSIXct(paste(look_date, '4:00', sep = ' '), tz= buoy_tz) &
                          datetime < as.POSIXct(paste(look_date, '5:00', sep = ' '), tz= buoy_tz) ~ NA_real_,
                        TRUE ~ .))

buoy2020_do_vert_L1 <- buoy2020_L1 %>% 
  select(datetime, all_of(lowDO), all_of(upDO)) %>%
  gather(variable, value, -datetime)

# ggplot(subset(buoy2020_do_vert_L1,
#               subset=(datetime >= as.POSIXct(look_date, tz=buoy_tz) & datetime < (as.POSIXct(look_date, tz=buoy_tz) + days(1)))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme

#aug 19-aug 20 upper do has a big jump at night
look_date_start = '2020-08-19'
look_date_end = '2020-08-20'

# ggplot(subset(buoy2020_do_vert_L1,
#               subset=(datetime >= as.POSIXct(look_date_start, tz=buoy_tz) & datetime < (as.POSIXct(look_date_end, tz=buoy_tz) + days(1)))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme

buoy2020_L1 <- buoy2020_L1 %>% 
  mutate_at(vars(all_of(upDO)),
            ~ case_when(datetime >= as.POSIXct(paste(look_date_start, '21:30', sep = ' '), tz= buoy_tz) &
                          datetime < as.POSIXct(paste(look_date_end, '12:00', sep = ' '), tz= buoy_tz) ~ NA_real_,
                        TRUE ~ .))

buoy2020_do_vert_L1 <- buoy2020_L1 %>% 
  select(datetime, all_of(lowDO), all_of(upDO)) %>%
  gather(variable, value, -datetime)

# ggplot(subset(buoy2020_do_vert_L1,
#               subset=(datetime >= as.POSIXct(look_date_start, tz=buoy_tz) & datetime < (as.POSIXct(look_date_end, tz=buoy_tz) + days(1)))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme

# removal
# ggplot(subset(buoy2020_do_vert_L1,
#               subset=(datetime >= as.POSIXct(removal, tz=buoy_tz) & datetime < (as.POSIXct(removal, tz=buoy_tz) + days(1)))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme

buoy2020_L1 <- buoy2020_L1 %>% 
  mutate_at(vars(all_of(upDO), all_of(lowDO)),
            ~ case_when(datetime >= as.POSIXct(paste(removal, '09:30', sep = ' '), tz= buoy_tz) &
                          datetime < as.Date(winter_temp_add) ~ NA_real_,
                        TRUE ~ .))

buoy2020_do_vert_L1 <- buoy2020_L1 %>% 
  select(datetime, all_of(lowDO), all_of(upDO)) %>%
  gather(variable, value, -datetime)

# ggplot(subset(buoy2020_do_vert_L1,
#               subset=(datetime >= as.POSIXct(removal, tz=buoy_tz) & datetime < (as.POSIXct(removal, tz=buoy_tz) + days(1)))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  final_theme

# winter upper oct 8

# ggplot(subset(buoy2020_do_vert_L1,
#               subset=(datetime >= as.POSIXct(winter_temp_add, tz=buoy_tz) & datetime < (as.POSIXct(winter_temp_add, tz=buoy_tz) + days(1)))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme

buoy2020_L1 <- buoy2020_L1 %>% 
  mutate_at(vars(all_of(upDO), all_of(lowDO)),
            ~ case_when(datetime >= as.POSIXct(paste(winter_temp_add, '00:00', sep = ' '), tz= buoy_tz) &
                          datetime < as.POSIXct(paste(winter_temp_add, '10:50', sep = ' '), tz=buoy_tz) ~ NA_real_,
                        TRUE ~ .))

buoy2020_do_vert_L1 <- buoy2020_L1 %>% 
  select(datetime, all_of(lowDO), all_of(upDO)) %>%
  gather(variable, value, -datetime)

# ggplot(subset(buoy2020_do_vert_L1,
#               subset=(datetime >= as.POSIXct(winter_temp_add, tz=buoy_tz) & datetime < (as.POSIXct(winter_temp_add, tz=buoy_tz) + days(1)))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme

# #plot whole year to see if there are overarching issues
# ggplot(buoy2020_do_vert_L1, aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 month') +
#   final_theme


#upper do saturation/ppm is wrong from early/mid aug through mid/late aug
look_date_start = '2020-08-12'
look_date_end = '2020-08-21'

# ggplot(subset(buoy2020_do_vert_L1,
#               subset=(datetime >= as.POSIXct(look_date_start, tz=buoy_tz) & datetime < (as.POSIXct(look_date_end, tz=buoy_tz) + days(1)))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme
# 
# ggplot(subset(buoy2020_do_vert_L1,
#               subset=(datetime >= as.POSIXct(look_date_start, tz=buoy_tz) & datetime < (as.POSIXct(look_date_start, tz=buoy_tz) + days(1)))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme

buoy2020_L1 <- buoy2020_L1 %>% 
  mutate_at(vars(all_of(upDO)),
            ~ case_when(datetime >= as.POSIXct(paste(look_date_start, '5:00', sep = ' '), tz= buoy_tz) &
                          datetime < as.Date(look_date_end) ~ NA_real_,
                        TRUE ~ .))

buoy2020_do_vert_L1 <- buoy2020_L1 %>% 
  select(datetime, all_of(lowDO), all_of(upDO)) %>%
  gather(variable, value, -datetime)

# ggplot(subset(buoy2020_do_vert_L1,
#               subset=(datetime >= as.POSIXct(look_date_start, tz=buoy_tz) & datetime < (as.POSIXct(look_date_end, tz=buoy_tz) + days(1)))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#   final_theme

#also recode last blip of data
buoy2020_L1 <- buoy2020_L1 %>% 
  mutate_at(vars(all_of(upDO)),
            ~ case_when(datetime >= as.POSIXct('2020-12-30', tz= buoy_tz) ~ NA_real_,
                        TRUE ~ .))
buoy2020_do_vert_L1 <- buoy2020_L1 %>% 
  select(datetime, all_of(lowDO), all_of(upDO), location) %>%
  gather(variable, value, -datetime, -location)

#plot whole year to see if there are overarching issues
ggplot(buoy2020_do_vert_L1, aes(x=datetime, y=value, color = location)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  scale_x_datetime(date_minor_breaks = '1 month') +
  final_theme

#print biweekly L1
for(i in 1:(nrow(weekly_2020)-1)) {
  chunk <- buoy2020_L1 %>% 
    filter(datetime >= as.POSIXct(weekly_2020$date[i], tz= buoy_tz) &
             datetime < as.POSIXct(weekly_2020$date[i+1], tz=buoy_tz))
  chunk_vert <- chunk %>% 
    select(datetime, all_of(upDO), all_of(lowDO)) %>%
    gather(variable, value, -datetime)
  ggplot(chunk_vert, aes(x = datetime, y = value)) +
    geom_point() +
    facet_grid(variable ~ ., scales = 'free_y') +
    scale_x_datetime(date_minor_breaks = '1 day') +
    scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                                "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
    final_theme +
    labs(x = NULL,
         y = NULL,
         title = paste0('LSPA DO sensors ', weekly_2020$date[i], ' - ', weekly_2020$date[i+1]))
  ggsave(filename = paste0('graphs/2020/L1_do_biweekly_', weekly_2020$date[i], '.jpg'), height = 8, width =10, units = 'in', dpi = 300)
}

#rename with CV
buoy2020_L1 <- buoy2020_L1 %>% 
  rename(oxygenDissolved_mgl_1p5m = DOppm,
         oxygenDissolvedPercentOfSaturation_pct_1p5m = DOSat,
         waterTemperature_DO_degC_1p5m = DOTempC,
         oxygenDissolved_mgl_10p5m = DOLowPPM,
         oxygenDissolvedPercentOfSaturation_pct_10p5m = DOLowSat,
         waterTemperature_DO_degC_10p5m = DOLowTempC)

#clean up workspace
rm(buoy2020_do_vert, buoy2020_do_vert_L1)

####CHLA####
# buoy2020_chla_vert <- buoy2020_L1 %>%
#   select(datetime, all_of(chla)) %>%
#   gather(variable, value, -datetime)
# 
# ggplot(buoy2020_chla_vert, aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 month') +
#   final_theme
# 
#chla sensor not working
buoy2020_L1 <- buoy2020_L1 %>% 
  mutate(Chlor_RFU = NA_real_,
         Chlor_UGL = NA_real_,
         SpecCond = NA_real_)

# #clean up workspace
# rm(buoy2020_chla_vert)

####wind####
buoy_wind_vert <- buoy2020_L1 %>%
  select(datetime, all_of(wind), location) %>%
  gather(variable, value, -datetime, -location)

ggplot(buoy_wind_vert,
       aes(x=datetime, y=value, color = location)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title = 'wind 2020, raw') +
  scale_x_datetime(date_minor_breaks = '1 month') +
  final_theme +
  scale_color_colorblind()


#plot data in 2-week iterations
for(i in 1:(nrow(weekly_2020)-1)) {
  chunk <- buoy2020_L1 %>% 
    filter(datetime >= as.POSIXct(weekly_2020$date[i], tz= buoy_tz) &
             datetime < as.POSIXct(weekly_2020$date[i+1], tz=buoy_tz))
  chunk_vert <- chunk %>% 
    select(datetime, all_of(wind)) %>%
    gather(variable, value, -datetime)
  ggplot(chunk_vert, aes(x = datetime, y = value, color = variable)) +
    geom_point() +
    scale_x_datetime(date_minor_breaks = '1 day') +
    facet_grid(variable ~ ., scales = 'free_y') +
    scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                                "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
    final_theme +
    labs(x = NULL,
         y = NULL,
         title = paste0('LSPA wind variables ', weekly_2020$date[i], ' - ', weekly_2020$date[i+1]))
  ggsave(filename = paste0('graphs/2020/L0_wind_biweekly_', weekly_2020$date[i], '.jpg'), height = 8, width =10, units = 'in', dpi = 300)
}

#jan 7
look_date = '2020-01-07'
# ggplot(subset(buoy_wind_vert,
#               subset=(datetime >= as.POSIXct(look_date, tz=buoy_tz) & datetime < (as.POSIXct(look_date, tz=buoy_tz) + days(1)))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'jan wind 2020, NAs recoded') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme +
#   scale_color_colorblind()

buoy2020_L1 <- buoy2020_L1 %>% 
  mutate_at(vars(all_of(wind)),
            ~ (case_when(datetime >= as.POSIXct(paste(look_date, '1:30', sep = ' '), tz=buoy_tz) & 
                             datetime < as.POSIXct(paste(look_date, '3:20', sep = ' '), tz=buoy_tz) & 
                             MaxWindSp == 0 ~ NA_real_,
                           TRUE ~ .)))

buoy_wind_vert_L1 <- buoy2020_L1 %>% 
  select(datetime, all_of(wind)) %>% 
  gather(variable, value, -datetime)

# ggplot(subset(buoy_wind_vert_L1,
#               subset=(datetime >= as.POSIXct(look_date, tz=buoy_tz) & datetime < (as.POSIXct(look_date, tz=buoy_tz)+days(1)))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'jan wind 2020, NAs recoded') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme +
#   scale_color_colorblind()

#jan 16
look_date = '2020-01-16'
# ggplot(subset(buoy_wind_vert,
#               subset=(datetime >= as.POSIXct(look_date, tz=buoy_tz) & datetime < (as.POSIXct(look_date, tz=buoy_tz) + days(1)))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'jan wind 2020, NAs recoded') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme +
#   scale_color_colorblind()

buoy2020_L1 <- buoy2020_L1 %>% 
  mutate_at(vars(all_of(wind)),
            ~ (case_when(datetime >= as.POSIXct(paste(look_date, '5:50', sep = ' '), tz=buoy_tz) & 
                           datetime < as.POSIXct(paste(look_date, '11:00', sep = ' '), tz=buoy_tz) & 
                           MaxWindSp == 0 ~ NA_real_,
                         TRUE ~ .)))

buoy_wind_vert_L1 <- buoy2020_L1 %>% 
  select(datetime, all_of(wind)) %>% 
  gather(variable, value, -datetime)

# ggplot(subset(buoy_wind_vert_L1,
#               subset=(datetime >= as.POSIXct(look_date, tz=buoy_tz) & datetime < (as.POSIXct(look_date, tz=buoy_tz)+days(1)))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'jan wind 2020, NAs recoded') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme +
#   scale_color_colorblind()

#feb7-9
look_date_start = '2020-02-07'
look_date_end = '2020-02-09'
# ggplot(subset(buoy_wind_vert,
#               subset=(datetime >= as.POSIXct(look_date_start, tz=buoy_tz) & datetime < (as.POSIXct(look_date_start, tz=buoy_tz) + days(1)))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'jan wind 2020, NAs recoded') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme +
#   scale_color_colorblind()

# ggplot(subset(buoy_wind_vert,
#               subset=(datetime >= (as.POSIXct(look_date_end, tz=buoy_tz)) & datetime < (as.POSIXct(look_date_end, tz=buoy_tz)+ days(1)))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'jan wind 2020, NAs recoded') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme +
#   scale_color_colorblind()

buoy2020_L1 <- buoy2020_L1 %>% 
  mutate_at(vars(all_of(wind)),
            ~ (case_when(datetime >= as.POSIXct(paste(look_date_start, '4:40', sep = ' '), tz=buoy_tz) & 
                           datetime < as.POSIXct(paste(look_date_end, '9:40', sep = ' '), tz=buoy_tz) & MaxWindSp == 0 ~ NA_real_,
                           TRUE ~ .)))

buoy_wind_vert_L1 <- buoy2020_L1 %>% 
  select(datetime, all_of(wind)) %>% 
  gather(variable, value, -datetime)

# ggplot(subset(buoy_wind_vert_L1,
#               subset=(datetime >= as.POSIXct(look_date_start, tz=buoy_tz) & datetime < (as.POSIXct(look_date_end, tz=buoy_tz)))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'jan wind 2020, NAs recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()

#feb 13
look_date = '2020-02-13'
# ggplot(subset(buoy_wind_vert,
#               subset=(datetime >= as.POSIXct(look_date, tz=buoy_tz) & datetime < (as.POSIXct(look_date, tz=buoy_tz) + days(1)))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'jan wind 2020, NAs recoded') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme +
#   scale_color_colorblind()

buoy2020_L1 <- buoy2020_L1 %>% 
  mutate_at(vars(all_of(wind)),
            ~ (case_when(datetime >= as.POSIXct(paste(look_date, '6:00', sep = ' '), tz=buoy_tz) & 
                           datetime < as.POSIXct(paste(look_date, '12:40', sep = ' '), tz=buoy_tz) & 
                           MaxWindSp == 0 ~ NA_real_,
                         TRUE ~ .)))

buoy_wind_vert_L1 <- buoy2020_L1 %>% 
  select(datetime, all_of(wind)) %>% 
  gather(variable, value, -datetime)

# ggplot(subset(buoy_wind_vert_L1,
#               subset=(datetime >= as.POSIXct(look_date, tz=buoy_tz) & datetime < (as.POSIXct(look_date, tz=buoy_tz)+days(1)))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'jan wind 2020, NAs recoded') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme +
#   scale_color_colorblind()


#mar 19
look_date = '2020-03-19'
# ggplot(subset(buoy_wind_vert,
#               subset=(datetime >= as.POSIXct(look_date, tz=buoy_tz) & datetime < (as.POSIXct(look_date, tz=buoy_tz) + days(1)))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'jan wind 2020, NAs recoded') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme +
#   scale_color_colorblind()

buoy2020_L1 <- buoy2020_L1 %>% 
  mutate_at(vars(all_of(wind)),
            ~ (case_when(datetime >= as.POSIXct(paste(look_date, '7:00', sep = ' '), tz=buoy_tz) & 
                           datetime < as.POSIXct(paste(look_date, '9:00', sep = ' '), tz=buoy_tz) & 
                           MaxWindSp == 0 ~ NA_real_,
                         TRUE ~ .)))

buoy_wind_vert_L1 <- buoy2020_L1 %>% 
  select(datetime, all_of(wind), location) %>% 
  gather(variable, value, -datetime, -location)

# ggplot(subset(buoy_wind_vert_L1,
#               subset=(datetime >= as.POSIXct(look_date, tz=buoy_tz) & datetime < (as.POSIXct(look_date, tz=buoy_tz)+days(1)))),
#        aes(x=datetime, y=value, color =location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   labs(title = 'jan wind 2020, NAs recoded') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme +
#   scale_color_colorblind()

# buoy deployment
# ggplot(subset(buoy_wind_vert_L1,
#               subset=(datetime >= as.POSIXct(deployment, tz=buoy_tz) & datetime < (as.POSIXct(deployment, tz=buoy_tz) +days(1)))),
#        aes(x=datetime, y=value, color = location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme +
#   scale_color_colorblind()

buoy2020_L1 <- buoy2020_L1 %>% 
  mutate(location = case_when(datetime >= as.POSIXct(paste(deployment, '8:00', sep = ' '), tz=buoy_tz) & 
                                datetime < as.POSIXct(paste(deployment, '9:20', sep = ' '), tz=buoy_tz) ~ 'in transit',
                              TRUE ~ location))  
buoy_wind_vert_L1 <- buoy2020_L1 %>% 
  select(datetime, all_of(wind), location) %>% 
  gather(variable, value, -datetime, -location)

# ggplot(subset(buoy_wind_vert_L1,
#               subset=(datetime >= as.POSIXct(deployment, tz=buoy_tz) & datetime < (as.POSIXct(deployment, tz=buoy_tz) +days(1)))),
#        aes(x=datetime, y=value, color = location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme +
#   scale_color_colorblind()

#removal
# ggplot(subset(buoy_wind_vert_L1,
#               subset=(datetime >= as.POSIXct(removal, tz=buoy_tz) & datetime < (as.POSIXct(removal, tz=buoy_tz) +days(1)))),
#        aes(x=datetime, y=value, color = location)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme +
#   scale_color_colorblind()

buoy2020_L1 <- buoy2020_L1 %>% 
  mutate(location = case_when(datetime >= as.POSIXct(paste(removal, '9:00', sep = ' '), tz=buoy_tz) & 
                                datetime < as.POSIXct(paste(removal, '13:00', sep = ' '), tz=buoy_tz) ~ 'in transit',
                              datetime >= as.POSIXct(paste(removal, '13:00', sep = ' '), tz=buoy_tz) ~ 'harbor',
                              TRUE ~ location)) %>% 
  mutate_at(vars(all_of(wind)),
            funs(case_when(location == 'in transit' ~ NA_real_,
                           TRUE ~ .)))
buoy_wind_vert_L1 <- buoy2020_L1 %>% 
  select(datetime, all_of(wind), location) %>% 
  gather(variable, value, -datetime, -location)

# ggplot(subset(buoy_wind_vert_L1,
#               subset=(datetime >= as.POSIXct(removal, tz=buoy_tz) & datetime < (as.POSIXct(removal, tz=buoy_tz) +days(1)))),
  #      aes(x=datetime, y=value, color = location)) +
  # geom_point() +
  # facet_grid(variable ~ ., scales = 'free_y') +
  # scale_x_datetime(date_minor_breaks = '1 hour') +
  # final_theme +
  # scale_color_colorblind()

#check data
ggplot(buoy_wind_vert_L1, 
       aes(x=datetime, y=value, color = location)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme +
  scale_color_colorblind()

#recode the last blip at end of year
buoy2020_L1 <- buoy2020_L1 %>% 
  mutate_at(vars(all_of(wind)),
            ~ case_when(datetime >= as.POSIXct('2020-12-30', tz=buoy_tz) ~ NA_real_,
                        TRUE ~ .))

#plot data in 2-week iterations
for(i in 1:(nrow(weekly_2020)-1)) {
  chunk <- buoy2020_L1 %>% 
    filter(datetime >= as.POSIXct(weekly_2020$date[i], tz= buoy_tz) &
             datetime < as.POSIXct(weekly_2020$date[i+1], tz=buoy_tz))
  chunk_vert <- chunk %>% 
    select(datetime, all_of(wind), location) %>%
    gather(variable, value, -datetime, -location)
  ggplot(chunk_vert, aes(x = datetime, y = value, color = location)) +
    geom_point() +
    scale_x_datetime(date_minor_breaks = '1 day') +
    facet_grid(variable ~ ., scales = 'free_y') +
    scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                                "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
    final_theme +
    labs(x = NULL,
         y = NULL,
         title = paste0('LSPA wind variables ', weekly_2020$date[i], ' - ', weekly_2020$date[i+1]))
  ggsave(filename = paste0('graphs/2020/L1_wind_biweekly_', weekly_2020$date[i], '.jpg'), height = 8, width =10, units = 'in', dpi = 300)
}

#Clean up workspace
rm(buoy_wind_vert, buoy_wind_vert_L1)

#rename with CV
buoy2020_L1 <- buoy2020_L1 %>% 
  rename(windDirectionAverage_deg = AveWindDir,
         windSpeedAverage_mps = AveWindSp,
         windGustDirection_deg = MaxWindDir,
         windGustSpeed_mps = MaxWindSp)


# ###PAR####
#recode when in transit, recode negative par to 0
buoy2020_L1 <- buoy2020_L1 %>% 
  mutate(flag_par = case_when(PAR < 0 ~ 'z',
                              TRUE ~ '')) %>% 
  mutate(PAR = case_when(location == 'in transit' ~ NA_real_,
                         PAR < 0 ~ 0,
                         TRUE ~ PAR)) 

#quick check
ggplot(buoy2020_L1, aes(x = datetime, y = PAR)) +
  geom_point()

# for(i in 1:(nrow(weekly_2020)-1)) {
#   chunk <- buoy2020_L1 %>% 
#     filter(datetime >= as.POSIXct(weekly_2020$date[i], tz= buoy_tz) &
#              datetime < as.POSIXct(weekly_2020$date[i+1], tz=buoy_tz))
#   chunk_vert <- chunk %>% 
#     select(datetime, PAR, location) 
#   ggplot(chunk_vert, aes(x = datetime, y = PAR, color = location)) +
#     geom_point() +
#     scale_x_datetime(date_minor_breaks = '1 day') +
#     scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                                 "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
#     final_theme +
#     labs(x = NULL,
#          y = NULL,
#          title = paste0('LSPA PAR ', weekly_2020$date[i], ' - ', weekly_2020$date[i+1]))
#   ggsave(filename = paste0('graphs/2020/L0_par_biweekly_', weekly_2020$date[i], '.jpg'), height = 3, width =8, units = 'in', dpi = 300)
# }

#nothing egregious week-by-week, but night par is still occuring all year, especially after low par days
#add flag for night time par values
buoy2020_L1 <- buoy2020_L1 %>% 
  mutate(flag_par = case_when(flag_par=='' ~ 'n',
                              flag_par!='' ~ paste('n', flag_par, sep = '; ')))

#look at whole year to see if par obscured at any time by snow
ggplot(buoy2020_L1, aes(x = datetime, y = PAR)) +
  geom_point() +
  final_theme

#look at Jan and Feb
look_date_start = '2020-01-01'
look_date_end = '2020-02-01'

# ggplot(subset(buoy2020_L1,
#               subset=(datetime >= as.POSIXct(look_date_start, tz=buoy_tz) & datetime < (as.POSIXct(look_date_end, tz=buoy_tz)))),
#        aes(x=datetime, y=PAR)) +
#   geom_point() +
#   labs(title = 'jan par 2020, NAs recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()

look_date_start = '2020-02-01'
look_date_end = '2020-03-01'

# ggplot(subset(buoy2020_L1,
#               subset=(datetime >= as.POSIXct(look_date_start, tz=buoy_tz) & datetime < (as.POSIXct(look_date_end, tz=buoy_tz)))),
#        aes(x=datetime, y=PAR)) +
#   geom_point() +
#   labs(title = 'feb par 2020, NAs recoded') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme +
#   scale_color_colorblind()

#par likely obscured feb 06-11 and 13 add flag
buoy2020_L1 <- buoy2020_L1 %>% 
  mutate(flag_par = case_when(flag_par =='' & datetime >= as.POSIXct('2020-02-06', tz=buoy_tz) &
                                datetime < as.POSIXct('2020-02-12', tz=buoy_tz) ~ 'o',
                              flag_par !='' & datetime >= as.POSIXct('2020-02-06', tz=buoy_tz) &
                                datetime < as.POSIXct('2020-02-12', tz=buoy_tz) ~ paste('o', flag_par, sep = '; '),
                              flag_par =='' & datetime >= as.POSIXct('2020-02-12', tz=buoy_tz) &
                                datetime < as.POSIXct('2020-02-13', tz=buoy_tz) ~ 'o',
                              flag_par !='' & datetime >= as.POSIXct('2020-02-12', tz=buoy_tz) &
                                datetime < as.POSIXct('2020-02-13', tz=buoy_tz) ~ paste('o', flag_par, sep = '; '),
                              TRUE ~ flag_par))
unique(buoy2020_L1$flag_par)

ggplot(buoy2020_L1,
       aes(x=datetime, y=PAR, color=location, shape = flag_par)) +
  geom_point() +
  labs(title = 'PAR 2020, NA recoded') +
  scale_x_datetime(date_minor_breaks = '1 month') +
  final_theme +
  scale_color_colorblind()

#recode blip at end of record
buoy2020_L1 <- buoy2020_L1 %>% 
  mutate(PAR = case_when(datetime >= as.POSIXct('2020-12-30', tz= buoy_tz) ~ NA_real_,
                         TRUE ~ PAR))

for(i in 1:(nrow(weekly_2020)-1)) {
  chunk <- buoy2020_L1 %>%
    filter(datetime >= as.POSIXct(weekly_2020$date[i], tz= buoy_tz) &
             datetime < as.POSIXct(weekly_2020$date[i+1], tz=buoy_tz))
  chunk_vert <- chunk %>%
    select(datetime, PAR, location)
  ggplot(chunk_vert, aes(x = datetime, y = PAR, color = location)) +
    geom_point() +
    scale_x_datetime(date_minor_breaks = '1 day') +
    scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                                "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
    final_theme +
    labs(x = NULL,
         y = NULL,
         title = paste0('LSPA PAR ', weekly_2020$date[i], ' - ', weekly_2020$date[i+1]))
  ggsave(filename = paste0('graphs/2020/L1_par_biweekly_', weekly_2020$date[i], '.jpg'), height = 3, width =8, units = 'in', dpi = 300)
}

#rename with CV
buoy2020_L1 <- buoy2020_L1 %>% 
  rename(radiationIncomingPAR_umolm2s = PAR)


#### Air temp & RH ####

#recode when in transit or offline
buoy2020_L1 <- buoy2020_L1 %>% 
  mutate_at(vars(AirTempC, RelHum),
            ~case_when(location == 'in transit' ~ NA_real_,
                              TRUE ~ .))

airvert <- buoy2020_L1 %>% 
  select(datetime, AirTempC, RelHum, location) %>%
  pivot_longer(names_to = 'variable', values_to = 'value', -c(datetime, location))
ggplot(airvert, aes(x = datetime, y = value, color = location)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  scale_x_datetime(date_minor_breaks = '1 month') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  final_theme +
  labs(x = NULL,
       y = NULL,
       title = paste0('LSPA Air Temperature ', weekly_2020$date[i], ' - ', weekly_2020$date[i+1]))

for(i in 1:(nrow(weekly_2020)-1)) {
  chunk <- buoy2020_L1 %>% 
    filter(datetime >= as.POSIXct(weekly_2020$date[i], tz= buoy_tz) &
             datetime < as.POSIXct(weekly_2020$date[i+1], tz=buoy_tz))
  chunk_vert <- chunk %>% 
    select(datetime, AirTempC, RelHum, location) %>%
    pivot_longer(names_to = 'variable', values_to = 'value', -c(datetime, location))
  ggplot(chunk_vert, aes(x = datetime, y = value, color = location)) +
    geom_point() +
    facet_grid(variable ~ ., scales = 'free_y') +
    scale_x_datetime(date_minor_breaks = '1 day') +
    scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                                "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
    final_theme +
    labs(x = NULL,
         y = NULL,
         title = paste0('LSPA Air Temperature ', weekly_2020$date[i], ' - ', weekly_2020$date[i+1]))
  ggsave(filename = paste0('graphs/2020/L0_airtemp_biweekly_', weekly_2020$date[i], '.jpg'), height = 3, width =8, units = 'in', dpi = 300)
}

#look good
#add RH flags for supersaturated 
buoy2020_L1 <- buoy2020_L1 %>% 
  mutate(flag_rh = case_when(RelHum >100 ~ 's',
                             TRUE ~ ''))

#recode blip at the end
buoy2020_L1 <- buoy2020_L1 %>% 
  mutate_at(vars(AirTempC, RelHum),
            ~ case_when(datetime >= as.POSIXct('2020-12-30', tz= buoy_tz) ~ NA_real_,
                         TRUE ~ .))

#rename with CV
buoy2020_L1 <- buoy2020_L1 %>% 
  rename(relativeHumidity_perc = RelHum,
         airTemperature_degC = AirTempC)

unique(buoy2020_L1$location)

#### EXPORT L1 DATA STREAMS ####
colnames(buoy2020_L1)

#make sure data are recoded during offline/in transit period
buoy2020_L1 <- buoy2020_L1 %>% 
  mutate_at(vars(waterTemperature_degC_0p75m:waterTemperature_degC_9p75m,
            oxygenDissolved_mgl_1p5m,oxygenDissolvedPercentOfSaturation_pct_1p5m, waterTemperature_DO_degC_1p5m,
            windDirectionInstantaneous_deg, windSpeedInstantaneous_mps,
            windDirectionAverage_deg, windSpeedAverage_mps, 
            radiationIncomingPAR_umolm2s,
            airTemperature_degC),
            ~ case_when(location == 'offline' ~ NA_real_,
            location == 'in transit' ~ NA_real_,
            TRUE ~ .)))


#export L1 tempstring file
buoy2020_L1 %>%
  select(datetime, location, waterTemperature_degC_0p75m:waterTemperature_degC_9p75m, flag_temp_0p75m) %>%
  mutate(datetime = as.character(datetime)) %>%
  write_csv(., file.path(dump_dir,'tempstring/2020_tempstring_L1_v2022.csv'))

# export L1 do file
buoy2020_L1 %>%
  select(datetime, location, 
         oxygenDissolved_mgl_1p5m, oxygenDissolvedPercentOfSaturation_pct_1p5m, waterTemperature_DO_degC_1p5m, 
         flag_do1p5m,
         oxygenDissolved_mgl_10p5m, oxygenDissolvedPercentOfSaturation_pct_10p5m, waterTemperature_DO_degC_10p5m, 
         flag_do10p5m) %>%
  mutate(datetime = as.character(datetime)) %>%
  write_csv(., file.path(dump_dir, 'do/2020_do_L1_v2022.csv'))

#export L1 met file
buoy2020_L1 %>%
  select(datetime, location, 
         windSpeedAverage_mps:windGustDirection_deg,
         radiationIncomingPAR_umolm2s, flag_par,
         airTemperature_degC, relativeHumidity_perc, flag_rh) %>%
  mutate(datetime = as.character(datetime)) %>%
  write_csv(., file.path(dump_dir, 'met/2020_met_L1_v2022.csv'))