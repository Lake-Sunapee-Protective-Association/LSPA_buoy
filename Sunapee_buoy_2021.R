#*****************************************************************
#*      Cary Institute of Ecosystem Studies (Millbrook, NY)      *
#*                                                               *
#* TITLE:   Sunapee_buoy_2021.r                                  *
#* AUTHOR:  Bethel Steele                                        *
#* SYSTEM:  Lenovo ThinkCentre, Win 10, R 4.0.3, RStudio 1.1.383 *
#* DATE:    16Jun2021                                            *
#* PROJECT: Lake Sunapee Buoy Data Cleaning                      *
#* PURPOSE: create L0 and L1 data for buoy data 2021 using       *
#*          similar methods to CCC and DR                        *
#*****************************************************************

source('library_func_lists.R')

#point to directories
data_dir = 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L0/'
log_dir = 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/operation notes/'

# store time zones
buoya_tz = 'Etc/GMT+5'
buoyb_tz = 'Etc/GMT+5'

log_tz = 'America/New_York'

# BRING IN RAW DATA ----

## first part of year, buoy 1.0 ----
buoy2021a_L0 <- read_xlsx(file.path(data_dir, '2021_BuoyData.xlsx'),
                          col_types = c('text', 'text', 'text', 'text', 
                                        'numeric', 'numeric', 'numeric', 'numeric', 
                                        'numeric', 'numeric', 'numeric', 'numeric', 
                                        'numeric', 'numeric', 'numeric', 'numeric', 
                                        'numeric', 'numeric', 'numeric', 'numeric', 
                                        'numeric', 'numeric', 'numeric', 'numeric', 
                                        'numeric', 'numeric', 'numeric', 'numeric', 
                                        'numeric', 'numeric', 'numeric', 'numeric', 
                                        'numeric', 'numeric'))
head(buoy2021a_L0)

## second part of year  (incomplete) ----
buoy2021b_wq_L0 <- read.csv(file.path(data_dir, 'SUNP_buoy_wq_p2021.csv'),
                            col.names = buoy2021,
                            skip = 3,
                            na.strings = 'NAN')
head(buoy2021b_wq_L0)

buoy2021b_met_L0 <- read.csv(file.path(data_dir, 'SUNP_buoy_met_p2021.csv'),
                             col.names = met2021,
                             skip = 3,
                             na.strings = 'NAN')
head(buoy2021b_met_L0)

## bring in log info ----
loga <- read_xlsx(file.path(log_dir, 'LS Buoy Operation Log - BGS master.xlsx'),
                  sheet = 'Log') %>% 
  filter(Year == 2021)

logb <- read_xlsx(file.path(log_dir, 'SUNP_MaintenanceLog_p2021.xlsx'),
                 sheet = 'MaintenanceLog')


# FORMAT DATA AND DEAL WITH TIMEZONES ----

## 2021a ----

### buoy data ----
buoy2021a_L0 <- buoy2021a_L0  %>%
  rename(Hr.Min = 'Hr/Min',
         DOLowTempC = 'DOLoTempC',
         AveWindSp = 'WindSpdAv',
         AveWindDir = 'WindVect',
         MaxWindSp = 'MaxWind') %>% 
  mutate(Hr.Min = as.integer(Hr.Min)) %>% 
  mutate(hour = Hr.Min%/%100,
         minutes = Hr.Min%%100,
         time = paste(hour, minutes, sep=':')) %>% #break out time from Hr.Min, create time column
  mutate(date = as.Date(paste(Day, Year, sep = '-'), format='%j-%Y'), #create date in ymd format
         datetime = as.POSIXct(paste(date, time, sep=' '), format='%Y-%m-%d %H:%M', tz=buoya_tz)) %>%  
  select(-hour, -minutes, -Hr.Min, -Year, -Day, -time, -ArrayID) %>% #remove unnecessary columns
  rownames_to_column(var ='rowid')
head(buoy2021a_L0)

#double check total number of observations for each day (should only be 144 or less to confirm no DST)
datetimetable <- buoy2021a_L0 %>% 
  group_by(date) %>% 
  summarize(n = length(datetime))

#looks like there are 6 fewer instances of datetime in 3/08 and 6 additional on 11/02
buoy2021a_L0[buoy2021a_L0$date == '2021-03-17',]$datetime
#data gap on 3/17 is actually at 23:00-00:00

# does observe DST, but at the wrong times. pull out by rowid, force time change for middle section.
buoy2021a_L0 <- buoy2021a_L0 %>% 
  mutate(instrument_datetime = datetime) 
beginning <- buoy2021a_L0 %>% 
  filter(datetime <= as.POSIXct('2021-03-17 23:00', tz=buoya_tz)) #in EST
middle <- buoy2021a_L0 %>% 
  filter(datetime > as.POSIXct('2021-03-17 23:00', tz=buoya_tz)) #in EDT; this is the row id that is the second occurrance of 0:00-1:00
middle <- middle %>% 
  mutate(datetime = instrument_datetime - hours(1)) #get rid of DST
#rejoin all data
buoy2021a_L0 <- full_join(beginning, middle) %>% 
  arrange(datetime)

# add in all date time stamps in L1 data set
last_time <- last(buoy2021a_L0$datetime)
alltimes_2021 <- as.data.frame(seq.POSIXt(as.POSIXct('2021-01-01 00:00', tz=buoya_tz), as.POSIXct(last_time, tz=buoya_tz), '10 min')) %>%
  rename("datetime" = !!names(.[1])) %>% 
  rowid_to_column('index')
buoy2021a_L1 <- buoy2021a_L0 %>%
  right_join(., alltimes_2021) %>%
  arrange(datetime)
#add flag for missing data from buoy
buoy2021a_L1 <- buoy2021a_L1 %>% 
  mutate(buoyoffline = case_when(is.na(rowid) ~ 'T',
                                 TRUE ~ 'F')) %>% 
  select(-rowid, -index) %>% 
  mutate(location = case_when(buoyoffline == 'T' ~ 'offline',
                              TRUE ~ NA_character_)) %>% 
  select(-buoyoffline) %>% 
  select(-all_of(chla))

#clean up workspace
rm(alltimes_2021, beginning, middle, datetimetable)

#recode -6999 to na
buoy2021a_L1 <- buoy2021a_L1 %>% 
  mutate_at(vars(all_of(alltemp2011), all_of(upDO), all_of(lowDO), all_of(wind), all_of(air), PAR),
            ~ case_when(. == -6999 ~ NA_real_, TRUE ~ .))

### log ----
loga <- loga %>% 
  rename(date = Date, 
         time = `hr/min`) %>% 
  mutate(time = format(time, '%H:%M')) %>% 
  mutate(datetime = as.POSIXct(paste(date, time), format = '%Y-%m-%d %H:%M', tz = log_tz))

#create start and end timestamps
loga <- loga%>% 
  mutate(timestamp = floor_date(datetime, '10 minutes'))

# change to buoy tz
loga <- loga %>% 
  mutate(timestamp = with_tz(timestamp, buoya_tz))

## 2021b ----

### wq ----
buoy2021b_wq_L1 <- buoy2021b_wq_L0 %>% 
  mutate(datetime = as.POSIXct(datetime, tz = buoyb_tz),
         instrument_datetime = datetime,
         date = as.Date(instrument_datetime)) %>% 
  rownames_to_column('rowid_wq') %>% 
  select(-index)

#record does not extend to DST, so no need to check.
# #double check total number of observations for each day (should only be 144 or less to confirm no DST)
# datetimetable <- buoy2021b_wq_L0 %>% 
#   group_by(date) %>% 
#   summarize(n = length(datetime))
# #looks good
# #double check DST dates
# buoy2021b_wq_L0[buoy2021b_wq_L0$date == '2021-11-07',]$datetime

### met ----
buoy2021b_met_L1 <- buoy2021b_met_L0 %>% 
  mutate(datetime = as.POSIXct(datetime, tz = buoyb_tz),
         instrument_datetime = datetime,
         date = as.Date(instrument_datetime)) %>% 
  rownames_to_column('rowid_met') %>% 
  select(-index)

#remove std column and dupe wind column
buoy2021b_met_L1 <- buoy2021b_met_L1 %>% 
  select(-STD_winddir, -AveWindSp2)

### log ----
logb <- logb %>% 
  mutate_at(vars(TIMESTAMP_start, TIMESTAMP_end),
            ~ force_tz(., tz = log_tz)) %>% #define timezone
  mutate_at(vars(TIMESTAMP_start, TIMESTAMP_end),
            ~ with_tz(., tz = buoyb_tz)) #change tz to buoy tz

#set floor and ceiling for times
logb <- logb %>% 
  mutate(TIMESTAMP_start = floor_date(TIMESTAMP_start, '10 minutes'),
         TIMESTAMP_end = ceiling_date(TIMESTAMP_end, '10 minutes'))

#add end of year to nas in end time
logb <- logb %>% 
  mutate(TIMESTAMP_end = case_when(is.na(TIMESTAMP_end) ~ as.POSIXct('2022-01-01', tz = log_tz),
                                   TRUE ~ TIMESTAMP_end))

# add flags to log
logb <- logb %>% 
  mutate(flag = case_when(grepl('clean', notes, ignore.case = T) & 
                            (instrument == 'EXO' | instrument == 'do') ~ 'w', #clean flag
                          grepl('calibrate', notes, ignore.case = T) &
                            (instrument == 'EXO' | instrument == 'do') ~ 'c', #calibrate flag
                          grepl('sensor cap', notes, ignore.case = T) &
                            (instrument == 'EXO' | instrument == 'do') ~ 'r', #sensor cap change flag
                          grepl('removed', notes, ignore.case = T) ~ 'R', #sensor removal
                          grepl('move', notes, ignore.case = T) ~ 'v',
                          TRUE ~ 'o'))  # sensor out of water


# LOOK AT BATTERY ----
ggplot(buoy2021a_L1, aes(x = datetime, y = LoggerBatV)) +
  geom_point()
ggplot(buoy2021a_L1, aes(x = datetime, y = RadioBatV)) +
  geom_point()

ggplot(buoy2021b_wq_L1, aes(x = datetime, y = LoggerBatV)) +
  geom_point()
ggplot(buoy2021b_wq_L1, aes(x = datetime, y = RadioBatV)) +
  geom_point()

ggplot(buoy2021b_wq_L1, aes(x = datetime, y = EXO_batt_V)) +
  geom_point()

# 2021a ----

## thermistors ----
buoy2021a_therm_vert <- buoy2021a_L1 %>% 
  select(datetime, all_of(alltemp2011)) %>%
  pivot_longer(names_to = 'variable', 
               values_to = 'value', 
               -datetime)

#plot to see
ggplot(buoy2021a_therm_vert, aes(x=datetime, y=value, color = variable)) +
  geom_point() +
  scale_x_datetime(date_minor_breaks = '1 month') +
  final_theme

#truncate record at removal data from log
end_of_buoy1p0 <- loga$timestamp[grepl('garage',loga$`Item Description`)]

buoy2021a_L1 <- buoy2021a_L1 %>% 
  mutate_at(vars(all_of(alltemp2011), all_of(upDO), all_of(lowDO), all_of(wind), all_of(air), PAR),
            ~ case_when(instrument_datetime >= end_of_buoy1p0 ~ NA_real_,
                        TRUE ~ .))

buoy2021a_therm_vert <- buoy2021a_L1 %>% 
  select(datetime, all_of(alltemp2011)) %>%
  pivot_longer(names_to = 'variable', 
               values_to = 'value', 
               -datetime)

#plot to see
ggplot(buoy2021a_therm_vert, aes(x=datetime, y=value, color = variable)) +
  geom_point() +
  scale_x_datetime(date_minor_breaks = '1 month') +
  final_theme

#correct column names for sensor offset
buoy2021a_L1 <- buoy2021a_L1 %>% 
  rename(TempC_9p75m = 'TempC_9m',
         TempC_8p75m = 'TempC_8m',
         TempC_7p75m = 'TempC_7m',
         TempC_6p75m = 'TempC_6m',
         TempC_5p75m = 'TempC_5m',
         TempC_4p75m = 'TempC_4m',
         TempC_3p75m = 'TempC_3m',
         TempC_2p75m = 'TempC_2m',
         TempC_1p75m = 'TempC_1m',
         TempC_0p75m = 'TempC_0m') 

#clean up workspace
rm(buoy2021a_therm_vert)

# add location flag here
buoy2021a_L1 <- buoy2021a_L1 %>% 
  mutate(location = case_when(is.na(location) ~ 'harbor',
                              datetime >= end_of_buoy1p0 ~ 'offline',
                              TRUE ~ location))

## DO ----
buoy2021a_do_vert <- buoy2021a_L1 %>% 
  select(datetime, all_of(upDO), all_of(lowDO), location) %>%
  pivot_longer(names_to = 'variable', 
               values_to = 'value', 
               -c(datetime, location))

ggplot(buoy2021a_do_vert, aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  scale_x_datetime(date_minor_breaks = '1 month') +
  final_theme

rm(buoy2021a_do_vert)

## wind ----
buoya_wind_vert <- buoy2021a_L1 %>%
  select(datetime, all_of(wind)) %>%
  pivot_longer(names_to ='variable', 
               values_to = 'value', 
               -datetime)

ggplot(buoya_wind_vert,
       aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title = 'wind 2021, raw') +
  scale_x_datetime(date_minor_breaks = '1 month') +
  final_theme +
  scale_color_colorblind()

months = seq.Date(as.Date('2021-01-01'), as.Date('2021-03-01'), 'month')
#plot data in monthly iterations
for(i in 1:length(months)) {
  chunk <- buoy2021a_L1 %>% 
    filter(datetime >= as.POSIXct(months[i], tz= buoya_tz) &
             datetime < (as.POSIXct(months[i], tz=buoya_tz) + dmonths(1)))
  chunk_vert <- chunk %>% 
    select(datetime, all_of(wind)) %>%
    pivot_longer(names_to = 'variable', 
                 values_to = 'value', 
                 -datetime)
  ggplot(chunk_vert, aes(x = datetime, y = value, color = variable)) +
    geom_point() +
    scale_x_datetime(date_minor_breaks = '1 day') +
    facet_grid(variable ~ ., scales = 'free_y') +
    scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                                "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
    final_theme +
    labs(x = NULL,
         y = NULL,
         title = paste0('LSPA wind variables ', format(months[i], '%B')))
  ggsave(filename = paste0('graphs/2021/L0_wind_monthly_', months[i], '.jpg'), height = 8, width =10, units = 'in', dpi = 300)
}


#jan 2
look_date = '2021-01-02'

ggplot(subset(buoya_wind_vert,
        subset=(datetime >= as.POSIXct(look_date, tz=buoya_tz) & datetime < (as.POSIXct(look_date, tz=buoya_tz) + days(1)))),
  aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title = 'jan wind 2021, NAs recoded') +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme +
  scale_color_colorblind()

buoy2021a_L1 <- buoy2021a_L1 %>% 
  mutate(flag_wind = case_when(datetime >= as.POSIXct(paste(look_date, '11:30', sep = ' '), tz=buoya_tz) & 
                           datetime < as.POSIXct(paste(look_date, '14:00', sep = ' '), tz=buoya_tz) & 
                           MaxWindSp == 0 ~ 'f',
                         TRUE ~ ''))

buoya_wind_vert_L1 <- buoy2021a_L1 %>% 
  select(datetime, flag_wind, all_of(wind)) %>% 
  pivot_longer(names_to = 'variable', values_to='value', -c(datetime, flag_wind))

ggplot(subset(buoya_wind_vert_L1,
              subset=(datetime >= as.POSIXct(look_date, tz=buoya_tz) & datetime < (as.POSIXct(look_date, tz=buoya_tz)+days(1)))),
       aes(x=datetime, y=value, color = flag_wind)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title = 'jan wind 2021, NAs recoded') +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme +
  scale_color_colorblind()

#feb 16
look_date = '2021-02-16'

ggplot(subset(buoya_wind_vert,
              subset=(datetime >= as.POSIXct(look_date, tz=buoya_tz) & datetime < (as.POSIXct(look_date, tz=buoya_tz) + days(1)))),
       aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title = 'jan wind 2021, NAs recoded') +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme +
  scale_color_colorblind()

buoy2021a_L1 <- buoy2021a_L1 %>% 
  mutate(flag_wind = case_when(datetime >= as.POSIXct(paste(look_date, '8:00', sep = ' '), tz=buoya_tz) & 
                                 datetime < as.POSIXct(paste(look_date, '14:30', sep = ' '), tz=buoya_tz) & 
                                 MaxWindSp == 0 ~ 'f',
                               TRUE ~ flag_wind)) %>% 
  mutate_at(vars(all_of(wind)),
            ~ case_when(datetime >= as.POSIXct(paste(look_date, '8:00', sep = ' '), tz=buoya_tz) & 
                          datetime < as.POSIXct(paste(look_date, '13:00', sep = ' '), tz=buoya_tz) & 
                          MaxWindSp == 0 ~ NA_real_,
                        TRUE ~ .))

buoya_wind_vert_L1 <- buoy2021a_L1 %>% 
  select(datetime, flag_wind, all_of(wind)) %>% 
  pivot_longer(names_to = 'variable', values_to='value', -c(datetime, flag_wind))

ggplot(subset(buoya_wind_vert_L1,
              subset=(datetime >= as.POSIXct(look_date, tz=buoya_tz) & datetime < (as.POSIXct(look_date, tz=buoya_tz)+days(1)))),
       aes(x=datetime, y=value, color = flag_wind)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title = 'feb wind 2021, NAs recoded') +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme +
  scale_color_colorblind()

## PAR ----
#plot data in monthly iterations
for(i in 1:length(months)) {
  chunk <- buoy2021a_L1 %>% 
    filter(datetime >= as.POSIXct(months[i], tz= buoya_tz) &
             datetime < (as.POSIXct(months[i], tz=buoya_tz) + dmonths(1)))
  chunk_vert <- chunk %>% 
    select(datetime, PAR) 
  ggplot(chunk_vert, aes(x = datetime, y = PAR)) +
    geom_point() +
    scale_x_datetime(date_minor_breaks = '1 day') +
    final_theme +
    labs(x = NULL,
         y = NULL,
         title = paste0('LSPA PAR ', format(months[i], '%B')))
  ggsave(filename = paste0('graphs/2021/L0_par_monthly_', months[i], '.jpg'), height = 8, width =10, units = 'in', dpi = 300)
}


#nothing egregious monthly, but night par is still occuring all year, especially after low par days

#recode when in transit, recode negative par to 0; add flag for night time par values
buoy2021a_L1 <- buoy2021a_L1 %>% 
  mutate(flag_PAR = case_when(PAR < 0 ~ 'z',
                         TRUE ~ '')) %>% 
  mutate(PAR = case_when(PAR < 0 ~ 0,
                         TRUE ~ PAR)) %>% 
  mutate(flag_PAR = case_when(flag_PAR == '' ~ 'n',
                              flag_PAR != '' ~ paste('n', flag_PAR, sep = ', ')))

#looks like PAR obscured Mar 28
buoy2021a_L1 <- buoy2021a_L1 %>% 
  mutate(flag_PAR = case_when(flag_PAR == '' & datetime >= as.POSIXct('2021-03-28', tz='UTC') &
                                datetime < as.POSIXct('2021-03-29', tz='UTC') ~ 'o',
                              flag_PAR != '' & datetime >= as.POSIXct('2021-03-28', tz='UTC') &
                                datetime < as.POSIXct('2021-03-29', tz='UTC') ~ paste('o', flag_PAR, sep = ', '),
                              TRUE ~ flag_PAR))
unique(buoy2021a_L1$flag_PAR)

ggplot(buoy2021a_L1,
       aes(x=datetime, y=PAR, color = flag_PAR)) +
  geom_point() +
  labs(title = 'PAR 2021, NA recoded') +
  scale_x_datetime(date_minor_breaks = '1 month') +
  final_theme +
  scale_color_colorblind()


## Air temp and RH ----
for(i in 1:length(months)) {
  chunk <- buoy2021a_L1 %>% 
    filter(datetime >= as.POSIXct(months[i], tz= buoya_tz) &
             datetime < (as.POSIXct(months[i], tz=buoya_tz) + dmonths(1)))
  chunk_vert <- chunk %>% 
    select(datetime, AirTempC, RelHum) %>% 
  pivot_longer(names_to = 'variable', 
                 values_to = 'value', 
                 -datetime)
  ggplot(chunk_vert, aes(x = datetime, y = value, color = variable)) +
    geom_point() +
    scale_x_datetime(date_minor_breaks = '1 day') +
    facet_grid(variable ~ ., scales = 'free_y') +
    scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                                "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
    final_theme +
    labs(x = NULL,
         y = NULL,
         title = paste0('LSPA air temp and RH ', format(months[i], '%B')))
  ggsave(filename = paste0('graphs/2021/L0_airRH_monthly_', months[i], '.jpg'), height = 8, width =10, units = 'in', dpi = 300)
}

#these look fine

# 2021b wq ----

#add flags from log

for(l in 1:nrow(logb)){
  if (logb$instrument[l] == 'EXO') {
  buoy2021b_wq_L1 <- buoy2021b_wq_L1 %>% 
    mutate(flag_exo = case_when(instrument_datetime >= logb$TIMESTAMP_start[l] &
                                  instrument_datetime <= logb$TIMESTAMP_end[l] ~ logb$flag[l],
                                TRUE ~ ''))
  } else if (logb$instrument[l] == 'do' | logb$instrument[l] == 'DO') {
    buoy2021b_wq_L1 <- buoy2021b_wq_L1 %>% 
      mutate(flag_do = case_when(instrument_datetime >= logb$TIMESTAMP_start[l] &
                                   instrument_datetime <= logb$TIMESTAMP_end[l] ~ logb$flag[l], 
                                 TRUE ~ ''))
  } else if (logb$instrument[l] == 'temp') {
    buoy2021b_wq_L1 <- buoy2021b_wq_L1 %>% 
      mutate(flag_temp = case_when(instrument_datetime >= logb$TIMESTAMP_start[l] &
                                     instrument_datetime <= logb$TIMESTAMP_end[l] ~ logb$flag[l],
                                   TRUE ~ ''))
  } else if (logb$data_table[l] == 'met') {
    buoy2021b_wq_L1 <- buoy2021b_wq_L1 %>% 
      mutate(flag_met = case_when(instrument_datetime >= logb$TIMESTAMP_start[l] &
                                     instrument_datetime <= logb$TIMESTAMP_end[l] ~ logb$flag[l],
                                   TRUE ~ ''))
  } else {
  }
}

## thermistors ----

#recode as necessary from 









# EXPORT L1 DATA STREAMS ----
colnames(buoy2021_L1)

#export L1 tempstring file
buoy2021_L1 %>%
  select(datetime, TempC_0p75m:TempC_9p75m, location) %>%
  mutate(datetime = as.character(datetime)) %>%
  write_csv(., 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/tempstring/2021_tempstring_L1_corrdepths.csv')

#export l1 do file
buoy2021_L1 %>%
  select(datetime, all_of(upDO), all_of(lowDO), upper_do_flag, lower_do_flag, location) %>%
  mutate(datetime = as.character(datetime)) %>%
  write_csv(., 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/do/2021_do_L1.csv')

#export l1 par file
buoy2021_L1 %>%
  select(datetime, PAR, PAR_flag, location) %>%
  mutate(datetime = as.character(datetime)) %>%
  write_csv(., 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/met/2021_PAR_L1.csv')

#export l1 wind
buoy2021_L1 %>%
  select(datetime, all_of(wind), location) %>%
  mutate(datetime = as.character(datetime)) %>%
  write_csv(., 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/met/2021_wind_L1.csv')

#export l1 air temp file
buoy2021_L1 %>%
  select(datetime, AirTempC, location) %>%
  mutate(datetime = as.character(datetime)) %>%
  write_csv(., 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/met/2021_airtemp_L1.csv')


