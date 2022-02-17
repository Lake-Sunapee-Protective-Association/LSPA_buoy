#*****************************************************************
#*      Cary Institute of Ecosystem Studies (Millbrook, NY)      *
#*                                                               *
#* TITLE:   Sunapee_buoy2_2021.r                                  *
#* AUTHOR:  Bethel Steele                                        *
#* DATE:    16Jun2021                                            *
#* PROJECT: Lake Sunapee Buoy Data Cleaning                      *
#*****************************************************************

source('library_func_lists.R')

#point to directories
data_dir = 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L0/2021/'
log_dir = 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/operation notes/'
dump_dir = 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/'

# store time zones
buoyb_tz = 'Etc/GMT+5'

log_tz = 'America/New_York'

# BRING IN RAW DATA ----

## second part of year ----
buoy2021b_wq_L0 <- read.csv(file.path(data_dir, 'SUNP_buoy_wq.csv'),
                            col.names = buoy2021,
                            skip = 3,
                            na.strings = 'NAN')
head(buoy2021b_wq_L0)

buoy2021b_met_L0 <- read.csv(file.path(data_dir, 'SUNP_buoy_met.csv'),
                             col.names = met2021,
                             skip = 3,
                             na.strings = 'NAN')
head(buoy2021b_met_L0)

## bring in log info ----

logb <- read_xlsx(file.path(log_dir, 'SUNP_MaintenanceLog_2021.xlsx'),
                  sheet = 'MaintenanceLog')


# FORMAT DATA AND DEAL WITH TIMEZONES ----

## 2021b ----

### wq ----
buoy2021b_wq_L1 <- buoy2021b_wq_L0 %>% 
  mutate(datetime = as.POSIXct(datetime),
         datetime = force_tz(datetime, tz = buoyb_tz),
         date = format(datetime, '%Y-%m-%d')) %>% 
  filter(datetime < as.POSIXct('2022-01-01', tz = buoyb_tz)) %>% 
  rownames_to_column('rowid_wq') %>% 
  select(-index)

#remove the EXO date and time columns
buoy2021b_wq_L1 <- buoy2021b_wq_L1 %>% 
  select(-EXO_date, -EXO_time)


#double check total number of observations for each day (should only be 144 or less to confirm no DST)
datetimetable <- buoy2021b_wq_L1 %>%
  group_by(date) %>%
  summarize(n = length(datetime)) %>% 
  arrange(n)
head(datetimetable, n = 10)
tail(datetimetable)

#double check DST dates
buoy2021b_wq_L1[buoy2021b_wq_L1$datetime >= as.Date('2021-11-07') & buoy2021b_wq_L1$datetime < as.Date('2021-11-08'),]$datetime
#and the following day
buoy2021b_wq_L1[buoy2021b_wq_L1$datetime >= as.Date('2021-11-08') & buoy2021b_wq_L1$datetime < as.Date('2021-11-09'),]$datetime

#look at date with 136 records
buoy2021b_wq_L1[buoy2021b_wq_L1$datetime >= as.Date('2021-06-03') & buoy2021b_wq_L1$datetime < as.Date('2021-06-04'),]$datetime
#missing some time around noon, no notes in maintenance log

#look at date with 145 records
buoy2021b_wq_L1[buoy2021b_wq_L1$date == as.Date('2021-12-06'),]$datetime
#duplicate is 2021-12-06 14:10:00, check record to see if identical
dupedate <- buoy2021b_wq_L1[buoy2021b_wq_L1$date == as.Date('2021-12-06'),]
#data are identical
buoy2021b_wq_L1 <- buoy2021b_wq_L1[buoy2021b_wq_L1$rowid_wq != 27532,] #remove the dupe row

# get list of all datetimes from start of this record to end
first_time <- first(buoy2021b_wq_L1$datetime)
alltimes_2021 <- as.data.frame(seq.POSIXt(as.POSIXct(first_time, tz=buoyb_tz), 
                                          as.POSIXct('2021-12-31 23:50', tz=buoyb_tz), '10 min')) %>%
  rename("datetime" = !!names(.[1])) %>% 
  rowid_to_column('index')
buoy2021b_wq_L1 <- buoy2021b_wq_L1 %>%
  right_join(., alltimes_2021) %>%
  arrange(datetime)
#add flag for missing data from buoy
buoy2021b_wq_L1 <- buoy2021b_wq_L1 %>% 
  mutate(location = case_when(is.na(rowid_wq) ~ 'offline',
                              TRUE ~ NA_character_)) 


### met ----
buoy2021b_met_L1 <- buoy2021b_met_L0 %>% 
  mutate(datetime = as.POSIXct(datetime),
         datetime = force_tz(datetime, tz = buoyb_tz),
         date = format(datetime, '%Y-%m-%d')) %>% 
  filter(datetime < as.Date('2022-01-01')) %>% 
  rownames_to_column('rowid_met') %>% 
  select(-index)

ggplot(buoy2021b_met_L1, aes(x = AveWindSp, y = AveWindSp2)) +
  geom_point()

#remove std column and dupe wind column
buoy2021b_met_L1 <- buoy2021b_met_L1 %>% 
  select(-STD_winddir, -AveWindSp2)

#look at time 
#double check total number of observations for each day (should only be 1440 or less to confirm no DST)
datetimetable <- buoy2021b_met_L1 %>%
  group_by(date) %>%
  summarize(n = length(datetime)) %>% 
  arrange(n)
head(datetimetable, n = 10)
tail(datetimetable)

#look at date with 1450 records
buoy2021b_met_L1[buoy2021b_met_L1$date == as.Date('2021-12-06'),]$datetime

#duplicate is 2021-12-06 14:10:00-14:20, check record to see if identical
dupedate <- buoy2021b_met_L1[buoy2021b_met_L1$datetime >= as.POSIXct('2021-12-06 14:10', tz = buoyb_tz) &
                               buoy2021b_met_L1$datetime <= as.POSIXct('2021-12-06 14:20', tz = buoyb_tz),]

#data are identical for those 10 minutes: remove those data
buoy2021b_met_L1 <- buoy2021b_met_L1 %>% 
  filter(rowid_met < 267925 |
           rowid_met >= 267935)

#can't join the two together, as the timesteps are different.

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
  mutate(TIMESTAMP_end = case_when(is.na(TIMESTAMP_end) ~ as.POSIXct('2022-01-01', tz = buoyb_tz),
                                   TRUE ~ TIMESTAMP_end))

# add flags to log
logb <- logb %>% 
  mutate(flag = case_when(grepl('clean', notes, ignore.case = T) & 
                            (instrument == 'EXO' | instrument == 'do' | instrument == 'DO') ~ 'w', #clean flag
                          grepl('calibrate', notes, ignore.case = T) &
                            (instrument == 'EXO' | instrument == 'do'| instrument == 'DO') ~ 'c', #calibrate flag
                          grepl('sensor cap', notes, ignore.case = T) &
                            (instrument == 'EXO' | instrument == 'do'| instrument == 'DO') ~ 'r', #sensor cap change flag
                          grepl('removed', notes, ignore.case = T) ~ 'R', #sensor removal
                          grepl('move', notes, ignore.case = T) ~ 'v',
                          grepl('RH', notes, ignore.case = T) ~ 'r',
                          TRUE ~ 'o'))  # sensor out of water


# LOOK AT BATTERY ----

ggplot(buoy2021b_wq_L1, aes(x = datetime, y = LoggerBatV)) +
  geom_point()
ggplot(buoy2021b_wq_L1, aes(x = datetime, y = RadioBatV)) +
  geom_point()
ggplot(buoy2021b_wq_L1, aes(x = datetime, y = EXO_batt_V)) +
  geom_point()
#these look good

# 2021b wq ----

#add flags from log
#initiate columns for wq file
buoy2021b_wq_L1$flag_allexo = ''
buoy2021b_wq_L1$flag_do10m = ''
buoy2021b_wq_L1$flag_alltemp = ''

for(l in 1:nrow(logb)){
  if (logb$instrument[l] == 'EXO') {
    buoy2021b_wq_L1 <- buoy2021b_wq_L1 %>% 
      mutate(flag_allexo = case_when(datetime >= logb$TIMESTAMP_start[l] &
                                       datetime <= logb$TIMESTAMP_end[l] ~ logb$flag[l],
                                     TRUE ~ flag_allexo))
  } else if (logb$instrument[l] == 'do' | logb$instrument[l] == 'DO') {
    buoy2021b_wq_L1 <- buoy2021b_wq_L1 %>% 
      mutate(flag_do10m = case_when(datetime >= logb$TIMESTAMP_start[l] &
                                        datetime <= logb$TIMESTAMP_end[l] ~ logb$flag[l], 
                                      TRUE ~ flag_do10m))
  } else if (logb$instrument[l] == 'temp') {
    buoy2021b_wq_L1 <- buoy2021b_wq_L1 %>% 
      mutate(flag_alltemp = case_when(datetime >= logb$TIMESTAMP_start[l] &
                                        datetime <= logb$TIMESTAMP_end[l] ~ logb$flag[l],
                                      TRUE ~ flag_alltemp))
  } 
}

#and for met file
buoy2021b_met_L1$flag_allmet = ''

for(l in 1:nrow(logb)){
  if (logb$data_table[l] == 'met') {
    buoy2021b_met_L1 <- buoy2021b_met_L1 %>% 
      mutate(flag_allmet = case_when(datetime >= logb$TIMESTAMP_start[l] &
                                       datetime <= logb$TIMESTAMP_end[l] ~ logb$flag[l],
                                     TRUE ~ flag_allmet))
  } else {
  }
}


## thermistors ----

#recode as necessary from log
unique(buoy2021b_wq_L1$flag_alltemp)
buoy2021b_wq_L1 <- buoy2021b_wq_L1 %>% 
  mutate_at(vars(all_of(alltemp)),
            ~ case_when(flag_alltemp == 'o' ~ NA_real_, 
                        flag_alltemp == 'R' ~ NA_real_, #all flags indicate that sensors were out of the water
                        TRUE ~ .))

buoyb_therm_vert <- buoy2021b_wq_L1 %>% 
  select(datetime, flag_alltemp, all_of(alltemp)) %>% 
  pivot_longer(names_to = 'variable',
               values_to = 'values', 
               -c(datetime,flag_alltemp))

#recode record prior to buoy deploy
redeploy = as.POSIXct('2021-06-07', tz = buoyb_tz)

buoy2021b_wq_L1 <- buoy2021b_wq_L1 %>% 
  mutate_at(vars(all_of(alltemp)),
            ~ case_when(datetime <= redeploy ~ NA_real_,
                        TRUE ~ .))

buoyb_therm_vert <- buoy2021b_wq_L1 %>% 
  select(datetime, flag_alltemp, all_of(alltemp)) %>% 
  pivot_longer(names_to = 'variable',
               values_to = 'values', 
               -c(datetime,flag_alltemp))

ggplot(buoyb_therm_vert, aes(x = datetime, y = values, color = variable)) +
  geom_point() +
  final_theme

#look at data on a monthly basis
months = c(seq.Date(as.Date('2021-06-01'), as.Date('2022-01-01'), 'month'))

#plot data in monthly iterations
for(i in 1:length(months)) {
  chunk <- buoy2021b_wq_L1 %>% 
    filter(datetime >= as.POSIXct(months[i], tz= buoyb_tz) &
             datetime < (as.POSIXct(months[i], tz=buoyb_tz) + dmonths(1)))
  chunk_vert <- chunk %>% 
    select(datetime, all_of(alltemp)) %>%
    pivot_longer(names_to = 'variable', 
                 values_to = 'value', 
                 -datetime)
  ggplot(chunk_vert, aes(x = datetime, y = value, color = variable)) +
    geom_point() +
    scale_x_datetime(date_minor_breaks = '1 day') +
    scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                                "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
    final_theme +
    labs(x = NULL,
         y = NULL,
         title = paste0('LSPA temp variables ', format(months[i], '%B')))
  ggsave(filename = paste0('graphs/2021/L0_temp_monthly_', months[i], '.jpg'), height = 8, width =10, units = 'in', dpi = 300)
}

#deployment on Jun 7
buoyb_therm_vert %>% 
  filter(datetime >= as.POSIXct('2021-06-07 00:00', tz = buoyb_tz)&
           datetime < as.POSIXct('2021-06-08 00:00', tz= buoyb_tz)) %>% 
  ggplot(., aes(x = datetime, y = values, color = variable)) +
  geom_point() +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme

redeploy_exact = as.POSIXct('2021-06-07 9:30', tz= buoyb_tz)

unique(buoy2021b_wq_L1$location)
buoy2021b_wq_L1 <- buoy2021b_wq_L1 %>% 
  mutate_at(vars(all_of(alltemp)),
            ~ case_when(datetime <= redeploy_exact ~ NA_real_,
                        TRUE ~ .)) %>% 
  mutate(location = case_when(datetime > redeploy_exact ~ 'loon',
                              TRUE ~ 'harbor'))

buoyb_therm_vert <- buoy2021b_wq_L1 %>% 
  select(datetime, flag_alltemp, all_of(alltemp)) %>% 
  pivot_longer(names_to = 'variable',
               values_to = 'values', 
               -c(datetime,flag_alltemp))

buoyb_therm_vert %>% 
  filter(datetime >= as.POSIXct('2021-06-07 00:00', tz = buoyb_tz)&
           datetime < as.POSIXct('2021-06-08 00:00', tz= buoyb_tz)) %>% 
  ggplot(., aes(x = datetime, y = values, color = variable)) +
  geom_point() +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme

#Jul 25
buoyb_therm_vert %>% 
  filter(datetime >= as.POSIXct('2021-07-25 00:00', tz = buoyb_tz)&
           datetime < as.POSIXct('2021-07-26 00:00', tz= buoyb_tz)) %>% 
  ggplot(., aes(x = datetime, y = values, color = variable)) +
  geom_point() +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme

#looks fine up close

rm(buoyb_therm_vert)


## exo ----
#recode as necessary from log
unique(buoy2021b_wq_L1$flag_allexo)

buoy2021b_wq_L1 <- buoy2021b_wq_L1 %>% 
  mutate_at(vars(all_of(exo), all_of(exoinfo)),
            ~ case_when(flag_allexo != '' ~ NA_real_,# all flags should be recoded
                        TRUE ~ .))

#recode record prior to buoy deploy
buoy2021b_wq_L1 <- buoy2021b_wq_L1 %>% 
  mutate_at(vars(all_of(exo), all_of(exoinfo)),
            ~ case_when(datetime < redeploy_exact ~ NA_real_,
                        TRUE ~ .))

buoyb_exo_vert <- buoy2021b_wq_L1 %>% 
  select(datetime, flag_allexo, all_of(exo)) %>% 
  pivot_longer(names_to = 'variable',
               values_to = 'values', 
               -c(datetime,flag_allexo))

ggplot(buoyb_exo_vert, aes(x = datetime, y = values, shape = flag_allexo)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  final_theme

#exo actually not functioning until Jun 30 visit
buoy2021b_wq_L1 <- buoy2021b_wq_L1 %>% 
  mutate_at(vars(all_of(exo), all_of(exoinfo)),
            ~ case_when(datetime < as.POSIXct('2021-06-30 10:30:00', tz=buoyb_tz) ~ NA_real_,
                        TRUE ~ .)) 

#plot exodepth to see if there are artifacts of maintenece
ggplot(buoy2021b_wq_L1, aes(x = datetime, y = EXO_depth_m)) +
  geom_point()

buoyb_exo_vert <- buoy2021b_wq_L1 %>% 
  select(datetime, flag_allexo, all_of(exo)) %>% 
  pivot_longer(names_to = 'variable',
               values_to = 'values', 
               -c(datetime,flag_allexo))

ggplot(buoyb_exo_vert, aes(x = datetime, y = values, shape = flag_allexo)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  final_theme

#end of record oct 19
oct19 <- as.POSIXct('2021-10-19 00:00', tz=buoyb_tz)
buoyb_exo_vert %>% 
  filter(datetime >= oct19 &
           datetime < (oct19 +days(1))) %>% 
  ggplot(., aes(x = datetime, y = values, color = variable)) +
  geom_point() +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme

exo_eor <- as.POSIXct('2021-10-19 07:30', tz= buoyb_tz)

buoy2021b_wq_L1 <- buoy2021b_wq_L1 %>% 
  mutate_at(vars(all_of(exo), all_of(exoinfo)),
            ~ case_when(datetime >= exo_eor ~ NA_real_,
                        TRUE ~ .))

buoyb_exo_vert <- buoy2021b_wq_L1 %>% 
  select(datetime, flag_allexo, all_of(exo)) %>% 
  pivot_longer(names_to = 'variable',
               values_to = 'values', 
               -c(datetime,flag_allexo))

ggplot(buoyb_exo_vert, aes(x = datetime, y = values, shape = flag_allexo)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  final_theme

# plot monthly
for(i in 1:length(months)) {
  chunk <- buoy2021b_wq_L1 %>% 
    filter(datetime >= as.POSIXct(months[i], tz= buoyb_tz) &
             datetime < (as.POSIXct(months[i], tz=buoyb_tz) + dmonths(1)))
  chunk_vert <- chunk %>% 
    select(datetime, all_of(exo)) %>%
    pivot_longer(names_to = 'variable', 
                 values_to = 'value', 
                 -datetime)
  ggplot(chunk_vert, aes(x = datetime, y = value)) +
    geom_point() +
    facet_grid(variable ~ ., scales = 'free_y') +
    scale_x_datetime(date_minor_breaks = '1 day') +
    final_theme +
    labs(x = NULL,
         y = NULL,
         title = paste0('LSPA exo variables ', format(months[i], '%B')))
  ggsave(filename = paste0('graphs/2021/L0_exo_monthly_', months[i], '.jpg'), height = 8, width =10, units = 'in', dpi = 300)
}

#recode do prior to 2021-07-20 10:20:00 - hole in DO membrane
doerr_date <- as.POSIXct('2021-07-20 10:20:00', tz=buoyb_tz)

buoy2021b_wq_L1 <- buoy2021b_wq_L1 %>% 
  mutate_at(all_of(exodo),
            ~ case_when(datetime < doerr_date ~ NA_real_,
                        TRUE ~ .))

#add clean and calibrate for do (calibration only pertinent to do)
buoy2021b_wq_L1 <- buoy2021b_wq_L1 %>% 
  mutate(flag_exodo = case_when(flag_allexo == 'c' ~ 'c', 
                                flag_allexo == 'w' ~ 'w',
                                TRUE ~ '')) 

exo_cal_datetime = as.POSIXct('2021-06-30 10:30:00', tz = buoyb_tz)
#add calibration at beginning of season
buoy2021b_wq_L1 <- buoy2021b_wq_L1 %>% 
  mutate(flag_exodo = case_when(datetime == exo_cal_datetime ~ 'c', 
                                TRUE ~ flag_exodo))
                                

# flag algae for bdl;
buoy2021b_wq_L1 <- buoy2021b_wq_L1 %>% 
  mutate(flag_exobga = case_when(BGAPC_RFU < 0 ~ 'z',
                                    TRUE ~ ''),
         flag_exobga = case_when(flag_exobga == '' & datetime == exo_cal_datetime ~ 'c',
                                    flag_exobga != '' & datetime == exo_cal_datetime ~ paste0('c; ', flag_exobga),
                                    flag_exobga == '' & flag_allexo == 'w' ~ 'w',
                                    flag_exobga != '' & flag_allexo == 'w' ~ paste0('w; ', flag_exobga),
                                    TRUE ~ flag_exobga))

# drop bgapc ugl
buoy2021b_wq_L1$BGAPC_UGL = NA_real_

#recode chlor ugl to nareal
buoy2021b_wq_L1$Chlor_UGL = NA_real_

buoyb_exo_vert <- buoy2021b_wq_L1 %>% 
  select(datetime, flag_allexo, all_of(exo)) %>% 
  pivot_longer(names_to = 'variable',
               values_to = 'values', 
               -c(datetime,flag_allexo))

ggplot(buoyb_exo_vert, aes(x = datetime, y = values)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  final_theme

### BGA ----

#calculate rolling average and rolling SD

buoy2021b_wq_L1$BGA_stdev = zoo::rollapply(buoy2021b_wq_L1$BGAPC_RFU, width=3, sd, align='center', partial=F, fill=NA)
range(buoy2021b_wq_L1$BGA_stdev, na.rm = T)

bga_sd = sd(buoy2021b_wq_L1$BGAPC_RFU, na.rm = T)
bga_sd

#range of data isn't even 2*SD, so only flag the few SD outliers as 'suspect'

ggplot(buoy2021b_wq_L1, aes(x = datetime, y = BGAPC_RFU, color = BGA_stdev)) +
  geom_point()
hist(buoy2021b_wq_L1$BGA_stdev)

buoy2021b_wq_L1 <- buoy2021b_wq_L1 %>% 
  mutate(flag_exobga = case_when(flag_exobga == '' & BGA_stdev > 0.06 ~ 's',
                                    flag_exobga != '' & BGA_stdev > 0.06 ~ paste0('s; ', flag_exobga),
                                    TRUE ~ flag_exobga))

ggplot(buoy2021b_wq_L1, aes(x = datetime, y = BGAPC_RFU, color = BGA_stdev, shape = flag_exobga)) +
  geom_point()

### chla ----

#calculate rolling average and rolling SD

buoy2021b_wq_L1$chla_stdev = zoo::rollapply(buoy2021b_wq_L1$Chlor_RFU, width=3, sd, align='center', partial=F, fill=NA)
range(buoy2021b_wq_L1$chla_stdev, na.rm = T)

chla_sd = sd(buoy2021b_wq_L1$Chlor_RFU, na.rm = T)
chla_sd

ggplot(buoy2021b_wq_L1, aes(x = datetime, y = Chlor_RFU, color = chla_stdev)) +
  geom_point() +
  geom_point(data = subset(buoy2021b_wq_L1,
                           chla_stdev >1),
             aes(x = datetime, y = Chlor_RFU), color = 'red')

hist(buoy2021b_wq_L1$chla_stdev)

#recode outlier and run again
buoy2021b_wq_L1 <- buoy2021b_wq_L1 %>% 
  mutate(Chlor_RFU = case_when(Chlor_RFU >2.9 ~ NA_real_,
                               TRUE ~ Chlor_RFU))

buoy2021b_wq_L1$chla_stdev = zoo::rollapply(buoy2021b_wq_L1$Chlor_RFU, width=3, sd, align='center', partial=F, fill=NA)
range(buoy2021b_wq_L1$chla_stdev, na.rm = T)

chla_sd = sd(buoy2021b_wq_L1$Chlor_RFU, na.rm = T)
chla_sd

#none are >4 sd
ggplot(buoy2021b_wq_L1, aes(x = datetime, y = Chlor_RFU, color = chla_stdev)) +
  geom_point() +
  geom_point(data = subset(buoy2021b_wq_L1,
                           chla_stdev > 4*chla_sd),
             aes(x = datetime, y = Chlor_RFU), color = 'red')

hist(buoy2021b_wq_L1$chla_stdev)


# plot monthly
for(i in 1:length(months)) {
  chunk <- buoy2021b_wq_L1 %>% 
    filter(datetime >= as.POSIXct(months[i], tz= buoyb_tz) &
             datetime < (as.POSIXct(months[i], tz=buoyb_tz) + dmonths(1)))
  ggplot(chunk, aes(x = datetime, y = BGAPC_RFU, color = flag_exobga)) +
    geom_point() +
    scale_x_datetime(date_minor_breaks = '1 day') +
    final_theme +
    labs(x = NULL,
         y = NULL,
         title = paste0('LSPA exo BGA ', format(months[i], '%B')))
  ggsave(filename = paste0('graphs/2021/L0p5_exobga_monthly_', months[i], '.jpg'), height = 8, width =10, units = 'in', dpi = 300)
  ggplot(chunk, aes(x = datetime, y = Chlor_RFU)) +
    geom_point() +
    scale_x_datetime(date_minor_breaks = '1 day') +
    final_theme +
    labs(x = NULL,
         y = NULL,
         title = paste0('LSPA exo chlorophyll ', format(months[i], '%B')))
  ggsave(filename = paste0('graphs/2021/L0p5_exochla_monthly_', months[i], '.jpg'), height = 8, width =10, units = 'in', dpi = 300)
}

#add flag to last 3 days before cleaning as suspect
clean_dt = as.POSIXct('2021-09-20 11:00:00', tz=buoyb_tz)
buoy2021b_wq_L1 <- buoy2021b_wq_L1 %>% 
  mutate(flag_exochl = case_when(datetime >= clean_dt - days(3) & datetime < clean_dt & Chlor_RFU > 0.75 ~ 'sf',
                                    TRUE ~ ''))

#add flag to few days before removal as suspect
removal = as.POSIXct('2021-10-19 08:30:00', tz = buoyb_tz)
buoy2021b_wq_L1 <- buoy2021b_wq_L1 %>% 
  mutate(flag_exochl = case_when(datetime >=  removal - days(3) & datetime < removal & Chlor_RFU > 0.75 ~ 'sf',
                                    TRUE ~ flag_exochl))

ggplot(buoy2021b_wq_L1, aes(x = datetime, y = Chlor_RFU, color = flag_exochl)) +
  geom_point() 

#remove SD cols
buoy2021b_wq_L1 <- buoy2021b_wq_L1 %>% 
  select(-BGA_stdev, -chla_stdev)

# flag cleaning
buoy2021b_wq_L1 <- buoy2021b_wq_L1 %>% 
  mutate(flag_exochl = case_when(flag_exochl == '' & datetime == exo_cal_datetime ~ 'c',
                           flag_exochl != '' & datetime == exo_cal_datetime ~ paste0('c; ', flag_exochl),
                           flag_exochl == '' & flag_allexo == 'w' ~ 'w',
                           flag_exochl != '' & flag_allexo == 'w' ~ paste0('w; ', flag_exochl),
                           TRUE ~ flag_exochl))

unique(buoy2021b_wq_L1$flag_exochl)



### fdom ----
buoyb_fdom_vert <- buoy2021b_wq_L1 %>% 
  select(datetime, all_of(exofdom)) %>% 
  pivot_longer(names_to = 'variable',
               values_to = 'value', 
               -datetime)

ggplot(buoyb_fdom_vert, aes(x = datetime, y = value)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y')

# plot monthly
for(i in 1:length(months)) {
  chunk <- buoy2021b_wq_L1 %>% 
    filter(datetime >= as.POSIXct(months[i], tz= buoyb_tz) &
             datetime < (as.POSIXct(months[i], tz=buoyb_tz) + dmonths(1)))
  ggplot(chunk, aes(x = datetime, y = fDOM_RFU)) +
    geom_point() +
    scale_x_datetime(date_minor_breaks = '1 day') +
    final_theme +
    labs(x = NULL,
         y = NULL,
         title = paste0('LSPA exo BGA ', format(months[i], '%B')))
  ggsave(filename = paste0('graphs/2021/L0p5_exofdom_monthly_', months[i], '.jpg'), height = 8, width =10, units = 'in', dpi = 300)
}

buoy2021b_wq_L1 <- buoy2021b_wq_L1 %>% 
  mutate(flag_exofdom = case_when(datetime == exo_cal_datetime ~ 'c',
                                  flag_allexo == 'w' ~ 'w',
                                    TRUE ~ ''))

# add calibration flag for TDS and cond
buoy2021b_wq_L1 <- buoy2021b_wq_L1 %>% 
  mutate(flag_exotds = case_when(datetime == exo_cal_datetime ~ 'c',
                                 flag_allexo == 'w' ~ 'w',
                                 TRUE ~ ''),
         flag_exocond = case_when(datetime == exo_cal_datetime ~ 'c',
                                  flag_allexo == 'w' ~ 'w',
                                  TRUE ~ ''))


### plot all data together monthly ----
buoyb_exo_vert <- buoy2021b_wq_L1 %>% 
  select(datetime, flag_allexo, all_of(exo)) %>% 
  pivot_longer(names_to = 'variable',
               values_to = 'values', 
               -c(datetime,flag_allexo))

# plot monthly
for(i in 1:length(months)) {
  chunk <- buoy2021b_wq_L1 %>% 
    filter(datetime >= as.POSIXct(months[i], tz= buoyb_tz) &
             datetime < (as.POSIXct(months[i], tz=buoyb_tz) + dmonths(1)))
  chunk_vert <- chunk %>% 
    select(datetime, all_of(exo)) %>%
    pivot_longer(names_to = 'variable', 
                 values_to = 'value', 
                 -datetime)
  ggplot(chunk_vert, aes(x = datetime, y = value)) +
    geom_point() +
    facet_grid(variable ~ ., scales = 'free_y') +
    scale_x_datetime(date_minor_breaks = '1 day') +
    final_theme +
    labs(x = NULL,
         y = NULL,
         title = paste0('LSPA exo variables ', format(months[i], '%B')))
  ggsave(filename = paste0('graphs/2021/L1_exo_monthly_', months[i], '.jpg'), height = 8, width =10, units = 'in', dpi = 300)
}

#clean up workspace
rm(buoyb_exo_vert, buoyb_fdom_vert, chunk, chunk_vert)

## DO ----
buoy_do_vert <- buoy2021b_wq_L1 %>% 
  select(datetime, flag_do10m, all_of(lowDO)) %>% 
  pivot_longer(names_to = 'variable', 
               values_to =  'value', 
               -c(datetime,flag_do10m))

# recode where flagged
unique(buoy2021b_wq_L1$flag_do10m) # all flags are out of water

#move low do to up do where flag == v
buoy2021b_wq_L1 <- buoy2021b_wq_L1 %>% 
  mutate(DOTempC_harbor = case_when(flag_do10m == 'v' & datetime > as.Date('2021-10-15') ~ DOLowTempC,
                                    TRUE ~ NA_real_),
         DOSat_harbor = case_when(flag_do10m == 'v' & datetime > as.Date('2021-10-15') ~ DOLowSat,
                                  TRUE ~ NA_real_),
         DOppm_harbor = case_when(flag_do10m == 'v' & datetime > as.Date('2021-10-15') ~ DOLowPPM,
                                  TRUE ~ NA_real_))

buoy2021b_wq_L1 <- buoy2021b_wq_L1 %>% 
  mutate_at(vars(all_of(lowDO)),
            ~ case_when(flag_do10m != '' ~ NA_real_,
                        TRUE ~ .))

#add flag for calibration prior to deploy in June
buoy2021b_wq_L1 <- buoy2021b_wq_L1 %>% 
  mutate(flag_do10m = case_when(datetime == redeploy_exact +minutes(10) ~ 'c',
                                  TRUE ~ flag_do10m))

#remove beginning of record
buoy2021b_wq_L1 <- buoy2021b_wq_L1 %>% 
  mutate_at(vars(all_of(lowDO)),
            ~ case_when(datetime <= redeploy_exact ~ NA_real_,
                        TRUE ~ .))

#remove end of record
buoy2021b_wq_L1 <- buoy2021b_wq_L1 %>% 
  mutate_at(vars(all_of(lowDO)),
            ~ case_when(datetime >= exo_eor ~ NA_real_,
                        TRUE ~ .))

buoy_do_vert <- buoy2021b_wq_L1 %>% 
  select(datetime, flag_do10m, all_of(lowDO)) %>% 
  pivot_longer(names_to = 'variable', 
               values_to =  'value', 
               -c(datetime,flag_do10m))

ggplot(buoy_do_vert, aes(x = datetime, y = value, color = flag_do10m))+
  geom_point()+
  facet_grid(variable ~., scales = 'free_y')

# plot monthly
for(i in 1:length(months)) {
  chunk <- buoy_do_vert %>% 
    filter(datetime >= as.POSIXct(months[i], tz= buoyb_tz) &
             datetime < (as.POSIXct(months[i], tz=buoyb_tz) + dmonths(1)))
  ggplot(chunk, aes(x = datetime, y = value)) +
    geom_point() +
    facet_grid(variable ~ ., scales = 'free_y') +
    scale_x_datetime(date_minor_breaks = '1 day') +
    final_theme +
    labs(x = NULL,
         y = NULL,
         title = paste0('LSPA low DO ', format(months[i], '%B')))
  ggsave(filename = paste0('graphs/2021/L1_LowDO_monthly_', months[i], '.jpg'), height = 8, width =10, units = 'in', dpi = 300)
}

#clean up workspace
rm(buoy_do_vert, chunk)

## Upper DO in harbor ----
harbordo_vert <- buoy2021b_wq_L1 %>% 
  select(datetime, DOSat_harbor, DOppm_harbor, DOTempC_harbor) %>% 
  pivot_longer(names_to = 'variable', values_to = 'value', -datetime)

ggplot(harbordo_vert, aes(x = datetime, y = value)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  scale_x_datetime(minor_breaks = '1 month')

# need to correct for beginning of record
harbordo_vert %>% 
  filter(datetime > as.POSIXct('2021-10-19', tz = buoyb_tz) & datetime < as.POSIXct('2021-10-20', tz = buoyb_tz)) %>% 
  ggplot(., aes(x = datetime, y = value)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  scale_x_datetime(minor_breaks = '1 hour')

buoy2021b_wq_L1 <- buoy2021b_wq_L1 %>% 
  mutate_at(vars(DOSat_harbor, DOppm_harbor, DOTempC_harbor),
            ~case_when(datetime < as.POSIXct('2021-10-19 09:30', tz =buoyb_tz) ~ NA_real_,
                       TRUE ~ .))
harbordo_vert <- buoy2021b_wq_L1 %>% 
  select(datetime, DOSat_harbor, DOppm_harbor, DOTempC_harbor) %>% 
  pivot_longer(names_to = 'variable', values_to = 'value', -datetime)

# plot to check
harbordo_vert %>% 
  filter(datetime > as.POSIXct('2021-10-19', tz = buoyb_tz) & datetime < as.POSIXct('2021-10-20', tz = buoyb_tz)) %>% 
  ggplot(., aes(x = datetime, y = value)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  scale_x_datetime(minor_breaks = '1 hour')

ggplot(harbordo_vert, aes(x = datetime, y = value)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  scale_x_datetime(minor_breaks = '1 month')

#add location information
buoy2021b_wq_L1 <- buoy2021b_wq_L1 %>% 
  mutate(location = case_when(datetime >= exo_eor & datetime < as.POSIXct('2021-10-19 09:30', tz =buoyb_tz) ~ 'in transit',
                              datetime >= as.POSIXct('2021-10-19 09:30', tz =buoyb_tz) ~ 'harbor', 
                              TRUE ~ location))

#double check location
ggplot(buoy2021b_wq_L1, aes(x = datetime, y = location)) +
  geom_point()

#filter to loon and beyond only
buoy2021b_wq_L1 <- buoy2021b_wq_L1 %>% 
  filter(datetime > redeploy_exact)

# 2021 Met data ----

# filter data from launch
buoy2021b_met_L1 <- buoy2021b_met_L1 %>% 
  filter(datetime >= redeploy_exact) 

# get location from wq data and apply to met
first_loon = first(buoy2021b_wq_L1$datetime[buoy2021b_wq_L1$location == 'loon'])
first_intransit = first(buoy2021b_wq_L1$datetime[buoy2021b_wq_L1$location == 'in transit'])
first_harbor = first(buoy2021b_wq_L1$datetime[buoy2021b_wq_L1$location == 'harbor'])

buoy2021b_met_L1 <- buoy2021b_met_L1 %>% 
  mutate(location = case_when(datetime >= first_loon & datetime < first_intransit ~ 'loon',
                              datetime >= first_intransit & datetime < first_harbor ~ 'in transit',
                              datetime >= first_harbor ~ 'harbor',
                              TRUE ~ ''))

# recode when in transit
buoy2021b_met_L1 <- buoy2021b_met_L1 %>% 
  mutate_at(all_of(met),
            ~ case_when(location == 'in transit' ~ NA_real_,
                        TRUE ~ .))

#look at flags
unique(buoy2021b_met_L1$flag_allmet)

#only r needs to be recoded - this is when the rh chip was changed
buoy2021b_met_L1 <- buoy2021b_met_L1 %>% 
  mutate_at(all_of(met),
            ~ case_when(flag_allmet == 'r' ~ NA_real_,
                        TRUE ~ .))


#plotting all of the met data at same time takes too long - only plot by data group

## air temp and rel hum ----

buoy_temp_vert <- buoy2021b_met_L1 %>% 
  select(datetime, all_of(air), location) %>% 
  pivot_longer(names_to = 'variable', 
               values_to =  'value', 
               -c(datetime, location))

# plot monthly
for(i in 1:length(months)) {
  chunk <- buoy_temp_vert %>% 
    filter(datetime >= as.POSIXct(months[i], tz= buoyb_tz) &
             datetime < (as.POSIXct(months[i], tz=buoyb_tz) + dmonths(1)))
  ggplot(chunk, aes(x = datetime, y = value, color = location)) +
    geom_point() +
    facet_grid(variable ~ ., scales = 'free_y') +
    scale_x_datetime(date_minor_breaks = '1 day') +
    final_theme +
    labs(x = NULL,
         y = NULL,
         title = paste0('LSPA buoy Air Temp RH ', format(months[i], '%B')))
  ggsave(filename = paste0('graphs/2021/L0_airRH_monthly_', months[i], '.jpg'), height = 8, width =10, units = 'in', dpi = 300)
}

#data look good

## wind ----
buoy_wind_vert <- buoy2021b_met_L1 %>% 
  select(datetime, all_of(wind), location) %>% 
  pivot_longer(names_to = 'variable', 
               values_to =  'value', 
               -c(datetime, location))

# plot monthly
for(i in 1:length(months)) {
  chunk <- buoy_wind_vert %>% 
    filter(datetime >= as.POSIXct(months[i], tz= buoyb_tz) &
             datetime < (as.POSIXct(months[i], tz=buoyb_tz) + dmonths(1)))
  ggplot(chunk, aes(x = datetime, y = value, color = location)) +
    geom_point() +
    facet_grid(variable ~ ., scales = 'free_y') +
    scale_x_datetime(date_minor_breaks = '1 day') +
    final_theme +
    labs(x = NULL,
         y = NULL,
         title = paste0('LSPA buoy wind ', format(months[i], '%B')))
  ggsave(filename = paste0('graphs/2021/L0_wind_monthly_', months[i], '.jpg'), height = 8, width =10, units = 'in', dpi = 300)
}

#sensor frozen nov 26- nov29
buoy2021b_met_L1 <- buoy2021b_met_L1 %>% 
  mutate_at(all_of(wind),
            ~case_when(datetime >= as.POSIXct('2021-11-26', tz = buoyb_tz) &
                         datetime < as.POSIXct('2021-11-30', tz = buoyb_tz) &
                         MaxWindSp == 0 ~ NA_real_, 
                       TRUE ~ .))

#sensor frozen dec 25- dec29
buoy2021b_met_L1 <- buoy2021b_met_L1 %>% 
  mutate_at(all_of(wind),
            ~case_when(datetime >= as.POSIXct('2021-12-25', tz = buoyb_tz) &
                         datetime < as.POSIXct('2021-12-30', tz = buoyb_tz) &
                         MaxWindSp == 0 ~ NA_real_, 
                       TRUE ~ .))

buoy_wind_vert <- buoy2021b_met_L1 %>% 
  select(datetime, all_of(wind), location) %>% 
  pivot_longer(names_to = 'variable', 
               values_to =  'value', 
               -c(datetime, location))

# plot monthly
for(i in 1:length(months)) {
  chunk <- buoy_wind_vert %>% 
    filter(datetime >= as.POSIXct(months[i], tz= buoyb_tz) &
             datetime < (as.POSIXct(months[i], tz=buoyb_tz) + dmonths(1)))
  ggplot(chunk, aes(x = datetime, y = value, color = location)) +
    geom_point() +
    facet_grid(variable ~ ., scales = 'free_y') +
    scale_x_datetime(date_minor_breaks = '1 day') +
    final_theme +
    labs(x = NULL,
         y = NULL,
         title = paste0('LSPA buoy wind ', format(months[i], '%B')))
  ggsave(filename = paste0('graphs/2021/L1_wind_monthly_', months[i], '.jpg'), height = 8, width =10, units = 'in', dpi = 300)
}

## PAR ----
range(buoy2021b_met_L1$PAR_ave_umolpspm2, na.rm = T)
range(buoy2021b_met_L1$PAR_tot_mmolpm2, na.rm = T)

buoy_par_vert <- buoy2021b_met_L1 %>% 
  select(datetime, all_of(par2021), location) %>% 
  pivot_longer(names_to = 'variable', 
               values_to =  'value', 
               -c(datetime, location))

ggplot(buoy_par_vert, aes(x = datetime, y = value, color = location)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  scale_x_datetime(date_minor_breaks = '1 month') +
  final_theme 


# plot monthly
for(i in 1:length(months)) {
  chunk <- buoy_par_vert %>% 
    filter(datetime >= as.POSIXct(months[i], tz= buoyb_tz) &
             datetime < (as.POSIXct(months[i], tz=buoyb_tz) + dmonths(1)))
  ggplot(chunk, aes(x = datetime, y = value, color = location)) +
    geom_point() +
    facet_grid(variable ~ ., scales = 'free_y') +
    scale_x_datetime(date_minor_breaks = '1 day') +
    final_theme +
    labs(x = NULL,
         y = NULL,
         title = paste0('LSPA buoy PAR ', format(months[i], '%B')))
  ggsave(filename = paste0('graphs/2021/L0_PAR_monthly_', months[i], '.jpg'), height = 8, width =10, units = 'in', dpi = 300)
}

#looks like sensor obscured end of nov and end-mid dec

#nov 27- nov 30
#dec 25- 28

buoy2021b_met_L1 <- buoy2021b_met_L1 %>% 
  mutate(flag_par = case_when(datetime >= as.POSIXct('2021-11-27', tz =buoyb_tz) &
                                datetime <= as.POSIXct('2021-12-01', tz = buoyb_tz) ~ 'o', 
                              datetime >= as.POSIXct('2021-12-25', tz =buoyb_tz) &
                                datetime <= as.POSIXct('2021-12-29', tz = buoyb_tz) ~ 'o', 
                              TRUE ~ ''))

# still have par >0 at night issue. flagging all PAR data 
buoy2021b_met_L1 <- buoy2021b_met_L1 %>% 
  mutate(flag_par = case_when(flag_par == '' ~ 'n',
                              TRUE ~ paste0('n; ', flag_par)))

buoy_par_vert <- buoy2021b_met_L1 %>% 
  select(datetime, all_of(par2021), location, flag_par) %>% 
  pivot_longer(names_to = 'variable', 
               values_to =  'value', 
               -c(datetime, location, flag_par))

# plot monthly
for(i in 1:length(months)) {
  chunk <- buoy_par_vert %>% 
    filter(datetime >= as.POSIXct(months[i], tz= buoyb_tz) &
             datetime < (as.POSIXct(months[i], tz=buoyb_tz) + dmonths(1)))
  ggplot(chunk, aes(x = datetime, y = value, color = flag_par, shape = location)) +
    geom_point() +
    facet_grid(variable ~ ., scales = 'free_y') +
    scale_x_datetime(date_minor_breaks = '1 day') +
    final_theme +
    labs(x = NULL,
         y = NULL,
         title = paste0('LSPA buoy PAR ', format(months[i], '%B')))
  ggsave(filename = paste0('graphs/2021/L1_PAR_monthly_', months[i], '.jpg'), height = 8, width =10, units = 'in', dpi = 300)
}

# Convert to CV ----
colnames(buoy2021b_wq_L1)
buoy2021b_wq_L1 <- buoy2021b_wq_L1 %>% 
  rename(waterTemperature_degC_0p1m = TempC_0m,
         waterTemperature_degC_1m = TempC_1m,
         waterTemperature_degC_2m = TempC_2m,
         waterTemperature_degC_3m = TempC_3m,
         waterTemperature_degC_4m = TempC_4m,
         waterTemperature_degC_5m = TempC_5m,
         waterTemperature_degC_6m = TempC_6m,
         waterTemperature_degC_7m = TempC_7m,
         waterTemperature_degC_8m = TempC_8m,
         waterTemperature_degC_9m = TempC_9m,
         waterTemperature_degC_10m = TempC_10m,
         oxygenDissolved_mgl_1m = DOppm,
         oxygenDissolvedPercentOfSaturation_pct_1m = DOSat,
         waterTemperature_EXO_degC_1m = EXOTempC,
         fluorescenceDissolvedOrganicMatter_RFU_1m = fDOM_RFU,
         blue_GreenAlgae_Cyanobacteria_Phycocyanin_RFU_1m = BGAPC_RFU,
         chlorophyllFluorescence_RFU_1m = Chlor_RFU,
         solidsTotalDissolved_mgl_1m = TDS_mgl,
         electricalConductivity_mScm_1m = Cond,
         specificConductance_mScm_1m = SpecCond,
         oxygenDissolved_mgl_0p75m = DOppm_harbor,
         oxygenDissolvedPercentOfSaturation_pct_0p75m = DOSat_harbor,
         waterTemperature_DO_degC_0p75m = DOTempC_harbor,
         oxygenDissolved_mgl_10m = DOLowPPM,
         oxygenDissolvedPercentOfSaturation_pct_10m = DOLowSat,
         waterTemperature_DO_degC_10m = DOLowTempC)

colnames(buoy2021b_met_L1)
buoy2021b_met_L1 <- buoy2021b_met_L1 %>% 
  rename(winddirectionInstantaneous_deg = WindDir,
         windDirectionAverage_deg = AveWindDir,
         windSpeedAverage_mps = AveWindSp,
         windGustDirection_deg = MaxWindDir,
         windGustSpeed_mps = MaxWindSp,
         relativeHumidity_perc = RelHum,
         airTemperature_degC = AirTempC,
         radiationIncomingPARTotal_mmolm2 = PAR_tot_mmolpm2,
         radiationIncomingPARAverage_mmolm2s = PAR_ave_umolpspm2)


# EXPORT L1 DATA STREAMS ----

## Water Quality data ----
colnames(buoy2021b_wq_L1)


#export L1 tempstring file
unique(buoy2021b_wq_L1$flag_alltemp) #these are fine to drop
buoy2021b_wq_L1 %>%
  select(datetime, waterTemperature_degC_0p1m:waterTemperature_degC_10m, location) %>%
  mutate(datetime = as.character(datetime)) %>%
  write_csv(., file.path(dump_dir, 'tempstring/2021_tempstring_L1_v2022.csv'))

#export l1 exo file
unique(buoy2021b_wq_L1$flag_exochl)
unique(buoy2021b_wq_L1$flag_exobga)
unique(buoy2021b_wq_L1$flag_exofdom)
unique(buoy2021b_wq_L1$flag_exocond)

buoy2021b_wq_L1 %>%
  select(datetime, 
         waterTemperature_EXO_degC_1m:oxygenDissolved_mgl_1m, flag_exodo,
         chlorophyllFluorescence_RFU_1m, flag_exochl,
         blue_GreenAlgae_Cyanobacteria_Phycocyanin_RFU_1m, flag_exobga,
         fluorescenceDissolvedOrganicMatter_RFU_1m, flag_exofdom,
         solidsTotalDissolved_mgl_1m, flag_exotds,
         electricalConductivity_mScm_1m,
         specificConductance_mScm_1m, flag_exocond,
         location) %>%
  mutate(datetime = as.character(datetime)) %>%
  write_csv(., file.path(dump_dir, 'exo/2021_exo_L1_v2022.csv'))

#export l1 do file
unique(buoy2021b_wq_L1$flag_do10m)
#recode v to '', not applicable to data
buoy2021b_wq_L1 <- buoy2021b_wq_L1 %>% 
  mutate(flag_do10m = case_when(flag_do10m == 'v' ~ '',
                                TRUE ~ flag_do10m))
buoy2021b_wq_L1 %>%
  select(datetime, 
         waterTemperature_DO_degC_0p75m:oxygenDissolved_mgl_0p75m,
         waterTemperature_DO_degC_10m:oxygenDissolved_mgl_10m,
         flag_do10m, 
         location) %>%
  mutate(datetime = as.character(datetime)) %>%
  write_csv(., file.path(dump_dir, 'do/2021_do_L1_v2022.csv'))


## Met Data ----
colnames(buoy2021b_met_L1)

unique(buoy2021b_met_L1$flag_allmet)
unique(buoy2021b_met_L1$flag_par)

# apply the 'r' flag from all met to rh
buoy2021b_met_L1 <- buoy2021b_met_L1 %>% 
  mutate(flag_rh = case_when(flag_allmet == 'r' ~ 'r',
                             TRUE ~ ''))

#export l1 met file
buoy2021b_met_L1 %>%
  select(datetime, 
         airTemperature_degC, relativeHumidity_perc, flag_rh,
         radiationIncomingPARAverage_mmolm2s, radiationIncomingPARTotal_mmolm2, flag_par,
         winddirectionInstantaneous_deg:windDirectionAverage_deg,
         location) %>%
  mutate(datetime = as.character(datetime)) %>%
  write_csv(., file.path(dump_dir, 'met/2021b_met_L1_v2022.csv'))
