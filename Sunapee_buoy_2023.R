#*****************************************************************
#*      Cary Institute of Ecosystem Studies (Millbrook, NY)      *
#*                                                               *
#* TITLE:   Sunapee_buoy_2023.r                                  *
#* AUTHOR:  Bethel Steele                                        *
#* DATE:    05Aug2024                                            *
#* PROJECT: Lake Sunapee Buoy Data Cleaning                      *
#*****************************************************************

source('library_func_lists.R')

# #point to directories
data_dir = 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L0/2023/'
log_dir = 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/operation notes/'
dump_dir = 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/'

if (dir.exists(data_dir) == FALSE) {
  dir.create(data_dir)
} 
if (dir.exists(log_dir) == FALSE) {
  dir.create(log_dir)
}
if (dir.exists(dump_dir) == FALSE) {
  dir.create(dump_dir)
}

# store time zones
buoy_tz = 'Etc/GMT+5'

log_tz = 'America/New_York'

# BRING IN RAW DATA ----

## GitHub Processes ----

if(!file.exists(file.path(data_dir, 'buoy_met_L0_2023.csv'))){
  ## download file from GH - this is a live file, we'll save a version of the file locally that has been filtered for 2023 only
  download.file('https://raw.githubusercontent.com/FLARE-forecast/SUNP-data/sunp-buoy-data/SUNP_buoy_met.csv',
                file.path(data_dir, 'buoy_met.csv'),
                method = 'curl')
  download.file('https://raw.githubusercontent.com/FLARE-forecast/SUNP-data/sunp-buoy-data/SUNP_buoy_wq.csv',
                file.path(data_dir, 'buoy_wq.csv'),
                method = 'curl')
  
  ## read, filter and save file ----
  buoy2023_met_L0 = read.csv(file.path(data_dir, 'buoy_met.csv'),
                             col.names = met2021, 
                             skip = 3,
                             na.strings = 'NAN') %>% 
    mutate(datetime = ymd_hms(datetime),
           datetime = force_tz(datetime, buoy_tz)) %>% 
    filter(datetime >= force_tz(ymd_hms('2023-01-01 00:00:00'), buoy_tz) &
             datetime <= force_tz(ymd_hms('2023-12-31 23:59:59'), buoy_tz))
  write.csv(buoy2023_met_L0, file.path(data_dir, 'buoy_met_L0_2023.csv'), row.names = F)
  unlink(file.path(data_dir, 'buoy_met.csv'))
  
  buoy2023_wq_L0 <- read.csv(file.path(data_dir, 'buoy_wq.csv'),
                             col.names = buoy2021,
                             skip = 3,
                             na.strings = 'NAN') %>% 
    mutate(datetime = ymd_hms(datetime),
           datetime = force_tz(datetime, buoy_tz)) %>% 
    filter(datetime >= force_tz(ymd_hms('2023-01-01 00:00:00'), buoy_tz) &
             datetime <= force_tz(ymd_hms('2023-12-31 23:59:59'), buoy_tz))
  write.csv(buoy2023_wq_L0, file.path(data_dir, 'buoy_wq_L0_2023.csv'), row.names = F)
  unlink(file.path(data_dir, 'buoy_wq.csv'))
} else {
  buoy2023_met_L0 = read_csv(file.path(data_dir, 'buoy_met_L0_2023.csv')) %>% 
    mutate(datetime = force_tz(as.POSIXct(datetime), tzone = buoy_tz))
  buoy2023_wq_L0 = read_csv(file.path(data_dir, 'buoy_wq_L0_2023.csv')) %>% 
    mutate(datetime = force_tz(as.POSIXct(datetime), tzone = buoy_tz))
}

## bring in log info ----

log <- read_xlsx(file.path(log_dir, 'SUNP_MaintenanceLog_2023.xlsx'),
                 sheet = 'MaintenanceLog')

### log ----
log <- log %>% 
  mutate_at(vars(TIMESTAMP_start, TIMESTAMP_end),
            ~ force_tz(., tz = log_tz)) %>% #define timezone
  mutate_at(vars(TIMESTAMP_start, TIMESTAMP_end),
            ~ with_tz(., tz = buoy_tz)) #change tz to buoy tz

#set floor and ceiling for times
log <- log %>% 
  mutate(TIMESTAMP_start = floor_date(TIMESTAMP_start, '10 minutes'),
         TIMESTAMP_end = ceiling_date(TIMESTAMP_end, '10 minutes'))

#add end of year to nas in end time
log <- log %>% 
  mutate(TIMESTAMP_end = case_when(is.na(TIMESTAMP_end) ~ as.POSIXct('2024-01-01', tz = buoy_tz),
                                   TRUE ~ TIMESTAMP_end))

# add flags to log
log <- log %>% 
  mutate(flag = case_when(grepl('calibrate', notes, ignore.case = T) &
                            (instrument == 'EXO' | instrument == 'do'| instrument == 'DO') ~ 'c', #calibrate flag
                          grepl('clean', notes, ignore.case = T) & 
                            (instrument == 'EXO' | instrument == 'do' | instrument == 'DO') ~ 'w', #clean flag
                          grepl('sensor cap', notes, ignore.case = T) &
                            (instrument == 'EXO' | instrument == 'do'| instrument == 'DO') ~ 'r', #sensor cap change flag
                          grepl('replace', notes, ignore.case = T) &
                            (instrument == 'EXO' | instrument == 'do'| instrument == 'DO') ~ 'r', #sensor cap change flag
                          grepl('removed', notes, ignore.case = T) ~ 'R', #sensor removal
                          grepl(' move', notes, ignore.case = T) ~ 'v',
                          grepl('RH', notes, ignore.case = T) ~ 'r',
                          grepl('head', notes, ignore.case = T & instrument == 'PAR') ~ 'h', #sensor head replaced
                          TRUE ~ 'o'))  # sensor out of water

# FORMAT DATA AND DEAL WITH TIMEZONES ----

## 2023 ----

### wq ----
buoy2023_wq_L1 <- buoy2023_wq_L0 %>% 
  mutate(date = as_date(datetime)) %>% 
  rownames_to_column('rowid_wq') %>% 
  select(-index)

#remove the EXO date and time columns
buoy2023_wq_L1 <- buoy2023_wq_L1 %>% 
  select(-EXO_date, -EXO_time)


#double check total number of observations for each day (should only be 144 or less to confirm no DST)
datetimetable <- buoy2023_wq_L1 %>%
  mutate(date = lubridate::as_date(datetime)) %>% 
  group_by(date) %>%
  summarize(n = length(datetime)) %>% 
  arrange(n)
head(datetimetable, n = 10)
tail(datetimetable)

# a few sporradic NA rows on the 7th, remove from record
buoy2023_wq_L1 <- buoy2023_wq_L1 %>% 
  filter(date <= ymd('2023-11-06'))

# get list of all datetimes from start of this record to end
first_time <- first(buoy2023_wq_L1$datetime)
alltimes_2023 <- as.data.frame(seq.POSIXt(as.POSIXct(first_time, tz=buoy_tz), 
                                          as.POSIXct('2023-11-06 23:50', tz=buoy_tz), '10 min')) %>%
  rename("datetime" = !!names(.[1])) %>% 
  rowid_to_column('index')
buoy2023_wq_L1 <- buoy2023_wq_L1 %>%
  right_join(., alltimes_2023) %>%
  arrange(datetime)

#add flag for missing data from buoy
buoy2023_wq_L1 <- buoy2023_wq_L1 %>% 
  mutate(location = case_when(is.na(rowid_wq) ~ 'offline',
                              TRUE ~ NA_character_)) 


### met ----
buoy2023_met_L1 <- buoy2023_met_L0 %>% 
  mutate(date = as_date(datetime)) %>% 
  rownames_to_column('rowid_met') %>% 
  select(-index)

#check to see if these are the same
ggplot(buoy2023_met_L1, aes(x = AveWindSp, y = AveWindSp2)) +
  geom_point()

# look at names and summary of columns
names(buoy2023_met_L1)
summary(buoy2023_met_L1)

# remove std column and dupe wind column
buoy2023_met_L1 <- buoy2023_met_L1 %>% 
  select(-STD_winddir, -AveWindSp2)

#look at time 
#double check total number of observations for each day (should only be 1440 or less to confirm no DST)
datetimetable <- buoy2023_met_L1 %>%
  mutate(date = as_date(datetime)) %>% 
  group_by(date) %>%
  summarize(n = length(datetime)) %>% 
  arrange(n)
head(datetimetable, n = 10)
tail(datetimetable)

# same as wq - remove rows after 2024-11-06
buoy2023_met_L1 <- buoy2023_met_L1 %>% 
  filter(date <= ymd('2024-11-06'))


# LOOK AT BATTERY ----

ggplot(buoy2023_wq_L1, aes(x = datetime, y = LoggerBatV)) +
  geom_point()
#battery very low in late January, need to look at met data then
ggplot(buoy2023_wq_L1, aes(x = datetime, y = RadioBatV)) +
  geom_point()
# similar dip in battery in January.
ggplot(buoy2023_wq_L1, aes(x = datetime, y = EXO_batt_V)) +
  geom_point()
# exo battery is fine.

# add flags 2023 wq and met ----

# add flags from log
# initiate columns for wq file
buoy2023_wq_L1$flag_allexo = ''
buoy2023_wq_L1$flag_do10m = ''
buoy2023_wq_L1$flag_alltemp = ''

for(l in 1:nrow(log)){
  if (log$instrument[l] == 'EXO') {
    buoy2023_wq_L1 <- buoy2023_wq_L1 %>% 
      mutate(flag_allexo = case_when(datetime >= log$TIMESTAMP_start[l] &
                                       datetime <= log$TIMESTAMP_end[l] ~ log$flag[l],
                                     TRUE ~ flag_allexo))
  } else if (log$instrument[l] == 'do' | log$instrument[l] == 'DO') {
    buoy2023_wq_L1 <- buoy2023_wq_L1 %>% 
      mutate(flag_do10m = case_when(datetime >= log$TIMESTAMP_start[l] &
                                      datetime <= log$TIMESTAMP_end[l] ~ log$flag[l], 
                                    TRUE ~ flag_do10m))
  } else if (log$instrument[l] == 'temp') {
    buoy2023_wq_L1 <- buoy2023_wq_L1 %>% 
      mutate(flag_alltemp = case_when(datetime >= log$TIMESTAMP_start[l] &
                                        datetime <= log$TIMESTAMP_end[l] ~ log$flag[l],
                                      TRUE ~ flag_alltemp))
  } 
}

#and for met file
buoy2023_met_L1$flag_allmet = ''

for(l in 1:nrow(log)){
  if (log$data_table[l] == 'met' | log$data_table[l] == 'all met') {
    buoy2023_met_L1 <- buoy2023_met_L1 %>% 
      mutate(flag_allmet = case_when(datetime >= log$TIMESTAMP_start[l] &
                                       datetime <= log$TIMESTAMP_end[l] ~ log$flag[l],
                                     TRUE ~ flag_allmet))
  } else if (log$data_table[l] == 'PAR') {
    buoy2023_met_L1 <- buoy2023_met_L1 %>% 
      mutate(flag_allmet = case_when(datetime >= log$TIMESTAMP_start[l] &
                                       datetime <= log$TIMESTAMP_end[l] ~ log$flag[l],
                                     TRUE ~ flag_allmet))
  }
}


# CLEAN 2023 WQ ----

## thermistors ----

# recode as necessary from log
unique(buoy2023_wq_L1$flag_alltemp)
# no flags to recode

buoy_therm_vert <- buoy2023_wq_L1 %>% 
  select(datetime, flag_alltemp, all_of(alltemp)) %>% 
  pivot_longer(names_to = 'variable',
               values_to = 'values', 
               -c(datetime,flag_alltemp))

# recode record prior to buoy deploy
redeploy = force_tz(ymd_hms("2023-05-15 10:30:00"), buoy_tz)

# need to find remove date
buoy_therm_vert %>% 
  filter(between(datetime, ymd('2023-11-01'), ymd('2023-11-07'))) %>% 
  ggplot(., aes(x = datetime, y = values, color = variable)) +
  geom_point() +
  scale_x_datetime(date_minor_breaks = '1 day')

buoy_therm_vert %>% 
  filter(between(datetime, ymd('2023-11-06'), ymd('2023-11-07'))) %>% 
  ggplot(., aes(x = datetime, y = values, color = variable)) +
  geom_point() +
  scale_x_datetime(date_minor_breaks = '1 hour')

# removal 2024-11-06 09:00
remove = ymd_hms("2023-11-06 09:00:00", tz = buoy_tz)

buoy2023_wq_L1 <- buoy2023_wq_L1 %>% 
  mutate(across(all_of(alltemp),
                ~ case_when(datetime <= redeploy ~ NA_real_,
                            datetime >= remove ~ NA_real_,
                            TRUE ~ .)))

buoyb_therm_vert <- buoy2023_wq_L1 %>% 
  select(datetime, flag_alltemp, all_of(alltemp)) %>% 
  pivot_longer(names_to = 'variable',
               values_to = 'values', 
               -c(datetime,flag_alltemp)) %>% 
  filter(!is.na(values))

ggplot(buoyb_therm_vert, aes(x = datetime, y = values, color = variable)) +
  geom_point() +
  final_theme

#look at data on a bi-monthly basis
two_weeks = c(seq.Date(as.Date('2023-01-01'), as.Date('2024-01-01'), '2 weeks'))

#plot data in bi-monthly iterations
for(i in 1:length(two_weeks)) {
  chunk <- buoyb_therm_vert %>% 
    filter(datetime >= force_tz(ymd(two_weeks[i]), buoy_tz) &
             datetime < force_tz((ymd(two_weeks[i]) + weeks(2)), tz=buoy_tz))
  if (nrow(chunk) > 0) {
    ggplot(chunk_vert, aes(x = datetime, y = values, color = variable)) +
      geom_point() +
      scale_x_datetime(date_minor_breaks = '1 day') +
      scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                                  "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
      final_theme +
      labs(x = NULL,
           y = NULL,
           title = paste0('LSPA temp variables ', format(two_weeks[i], '%B')))
    ggsave(filename = paste0('graphs/2023/L0_temp_2wk_', two_weeks[i], '.jpg'), 
           height = 8, width =10, units = 'in', dpi = 300)
  }
}

unique(buoy2023_wq_L1$location)

buoy2023_wq_L1 <- buoy2023_wq_L1 %>% 
  mutate_at(vars(all_of(alltemp)),
            ~ case_when(datetime < redeploy ~ NA_real_,
                        TRUE ~ .)) %>% 
  mutate(location = if_else(datetime >= redeploy, 'loon', location))

buoyb_therm_vert <- buoy2023_wq_L1 %>% 
  select(datetime, flag_alltemp, all_of(alltemp)) %>% 
  pivot_longer(names_to = 'variable',
               values_to = 'values', 
               -c(datetime,flag_alltemp))

# September 7/8 messy thermistors
buoyb_therm_vert %>% 
  filter(datetime >= as.POSIXct('2023-09-07 00:00', tz = buoy_tz)&
           datetime < as.POSIXct('2023-09-09 00:00', tz= buoy_tz)) %>% 
  ggplot(., aes(x = datetime, y = values, color = variable)) +
  geom_point() +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme
# this looks fine - there was a heavy storm documented in the area that night

rm(buoyb_therm_vert)


#plot data in bi-monthly iterations
for(i in 1:length(two_weeks)) {
  chunk <- buoyb_therm_vert %>% 
    filter(datetime >= force_tz(ymd(two_weeks[i]), buoy_tz) &
             datetime < force_tz((ymd(two_weeks[i]) + weeks(2)), tz=buoy_tz))
  if (nrow(chunk) > 0) {
    ggplot(chunk_vert, aes(x = datetime, y = values, color = variable)) +
      geom_point() +
      scale_x_datetime(date_minor_breaks = '1 day') +
      scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                                  "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
      final_theme +
      labs(x = NULL,
           y = NULL,
           title = paste0('LSPA temp variables ', format(two_weeks[i], '%B')))
    ggsave(filename = paste0('graphs/2023/L1_temp_2wk_', two_weeks[i], '.jpg'), 
           height = 8, width = 10, units = 'in', dpi = 300)
  }
}

## exo ----

#recode as necessary from log
unique(buoy2023_wq_L1$flag_allexo)

buoy2023_wq_L1 <- buoy2023_wq_L1 %>% 
  mutate_at(vars(all_of(exo), all_of(exoinfo)),
            ~ case_when(flag_allexo != '' ~ NA_real_,# all flags should be recoded
                        TRUE ~ .))

# quick look at exo sensor temp
ggplot(buoy2023_wq_L1, aes(x = datetime, y = EXOTempC)) +
  geom_point()
ggplot(buoy2023_wq_L1, aes(x = datetime, y = DOLowTempC)) +
  geom_point()
# some issue in May/Jun and again at re-deploy of sensor

#recode record prior to buoy deploy
buoy2023_wq_L1 <- buoy2023_wq_L1 %>% 
  mutate_at(vars(all_of(exo), all_of(exoinfo)),
            ~ case_when(datetime < redeploy ~ NA_real_,
                        TRUE ~ .))

buoyb_exo_vert <- buoy2023_wq_L1 %>% 
  select(datetime, flag_allexo, all_of(exo)) %>% 
  pivot_longer(names_to = 'variable',
               values_to = 'values', 
               -c(datetime,flag_allexo)) %>% 
  filter(!is.na(values))

ggplot(buoyb_exo_vert, aes(x = datetime, y = values, shape = flag_allexo)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  final_theme

#plot exodepth to see if there are artifacts of maintenance
ggplot(buoy2023_wq_L1, aes(x = datetime, y = EXO_depth_m)) +
  geom_point()
# Jul and Oct maintenance artifacts

buoyb_exo_vert <- buoy2023_wq_L1 %>% 
  select(datetime, flag_allexo, all_of(exo)) %>% 
  pivot_longer(names_to = 'variable',
               values_to = 'values', 
               -c(datetime,flag_allexo)) %>% 
  filter(!is.na(values))

ggplot(buoyb_exo_vert, aes(x = datetime, y = values, shape = flag_allexo)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  final_theme

#end of record oct 17
buoyb_exo_vert %>% 
  filter(datetime >= as_date(remove) &
           datetime < (as_date(remove) +days(1))) %>% 
  ggplot(., aes(x = datetime, y = values, color = variable)) +
  geom_point() +
  facet_grid(variable ~ ., scales = "free_y") +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme

# looks like the same timing as thermistors for removal
exo_eor <- remove

buoy2023_wq_L1 <- buoy2023_wq_L1 %>% 
  mutate_at(vars(all_of(exo), all_of(exoinfo)),
            ~ case_when(datetime >= exo_eor ~ NA_real_,
                        TRUE ~ .))

# add location as offline starting at exo_eor
buoy2023_wq_L1 <- buoy2023_wq_L1 %>% 
  mutate(location = if_else(datetime >= exo_eor, "offline", location))

buoyb_exo_vert <- buoy2023_wq_L1 %>% 
  select(datetime, flag_allexo, all_of(exo)) %>% 
  pivot_longer(names_to = 'variable',
               values_to = 'values', 
               -c(datetime,flag_allexo)) %>% 
  filter(!is.na(values))

ggplot(buoyb_exo_vert, aes(x = datetime, y = values, shape = flag_allexo)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  final_theme

# plot bi-weekly
for(i in 1:length(two_weeks)) {
  chunk <- buoyb_exo_vert %>% 
    filter(datetime >= force_tz(as.POSIXct(two_weeks[i]), tzone = buoy_tz) &
             datetime < (force_tz(as.POSIXct(two_weeks[i]), tzone = buoy_tz) + weeks(2)))
  if (nrow(chunk) > 0) {
    ggplot(chunk, aes(x = datetime, y = values)) +
      geom_point() +
      facet_grid(variable ~ ., scales = 'free_y') +
      scale_x_datetime(date_minor_breaks = '1 day') +
      final_theme +
      labs(x = NULL,
           y = NULL,
           title = paste0('LSPA exo variables ', format(two_weeks[i], '%B')))
    ggsave(filename = paste0('graphs/2023/L0_exo_2wks_', two_weeks[i], '.jpg'), height = 8, width =10, units = 'in', dpi = 300)
  }
}

#### chla flier on May 26 ----
# flag chla > 1 on May 26
inspect = as_date('2023-05-26')

buoy2023_wq_L1 <- buoy2023_wq_L1 %>% 
  mutate(flag_exochl = if_else(Chlor_RFU > 1 & 
                                 datetime >= inspect &
                                 datetime < (inspect + days(1)),
                               's',
                               ''))

#### chla flier on Jul 12 ----

inspect = as_date('2023-07-12')
buoyb_exo_vert %>% 
  filter(datetime >= inspect &
           datetime < (inspect + days(1))) %>% 
  ggplot(., aes(x = datetime, y = values, color = variable)) +
  geom_point() +
  facet_grid(variable ~ ., scales = "free_y") +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme
recode_dt = '2023-07-12 13:50'

buoy2023_wq_L1 <- buoy2023_wq_L1 %>% 
  mutate_at(vars(all_of(exochla)),
            ~ case_when(datetime == as.POSIXct(recode_dt, tz = buoy_tz) ~ NA_real_,
                        TRUE ~ .))

buoyb_exo_vert <- buoy2023_wq_L1 %>% 
  select(datetime, flag_allexo, all_of(exo)) %>% 
  pivot_longer(names_to = 'variable',
               values_to = 'values', 
               -c(datetime,flag_allexo)) %>% 
  filter(!is.na(values))

buoyb_exo_vert %>% 
  filter(datetime >= inspect &
           datetime < (inspect + days(1))) %>% 
  ggplot(., aes(x = datetime, y = values, shape = flag_allexo)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  final_theme

# flag chlor high value on this date > 2
buoy2023_wq_L1 <- buoy2023_wq_L1 %>% 
  mutate(flag_exochl = if_else(Chlor_RFU > 2 & 
                                 datetime >= inspect &
                                 datetime < (inspect + days(1)),
                               's',
                               flag_exochl))



#### bga flier jul 16 ----
inspect = as_date('2023-07-16')
buoyb_exo_vert %>% 
  filter(datetime >= inspect &
           datetime < (inspect + days(1))) %>% 
  ggplot(., aes(x = datetime, y = values, color = variable)) +
  geom_point() +
  facet_grid(variable ~ ., scales = "free_y") +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme
recode_dt = '2023-07-16 20:30'

buoy2023_wq_L1 <- buoy2023_wq_L1 %>% 
  mutate_at(vars(all_of(exobga)),
            ~ case_when(datetime == as.POSIXct(recode_dt, tz = buoy_tz) ~ NA_real_,
                        TRUE ~ .))

buoyb_exo_vert <- buoy2023_wq_L1 %>% 
  select(datetime, flag_allexo, all_of(exo)) %>% 
  pivot_longer(names_to = 'variable',
               values_to = 'values', 
               -c(datetime,flag_allexo)) %>% 
  filter(!is.na(values))

buoyb_exo_vert %>% 
  filter(datetime >= inspect &
           datetime < (inspect + days(1))) %>% 
  ggplot(., aes(x = datetime, y = values, shape = flag_allexo)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  final_theme

#### chla flier jul 18 ----

inspect = as_date('2023-07-18')
buoyb_exo_vert %>% 
  filter(datetime >= inspect &
           datetime < (inspect + days(1))) %>% 
  ggplot(., aes(x = datetime, y = values, color = variable)) +
  geom_point() +
  facet_grid(variable ~ ., scales = "free_y") +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme
recode_dt = '2023-07-18 14:40'

buoy2023_wq_L1 <- buoy2023_wq_L1 %>% 
  mutate_at(vars(all_of(exochla)),
            ~ case_when(datetime == as.POSIXct(recode_dt, tz = buoy_tz) ~ NA_real_,
                        TRUE ~ .))

# flag bga > 0.2 as suspect on this date
buoy2023_wq_L1 <- buoy2023_wq_L1 %>% 
  mutate(flag_exobga = if_else(BGAPC_RFU > 0.2 & 
                                 datetime >= inspect &
                                 datetime < (inspect + days(1)),
                               's',
                               ''))

buoyb_exo_vert <- buoy2023_wq_L1 %>% 
  select(datetime, flag_allexo, all_of(exo)) %>% 
  pivot_longer(names_to = 'variable',
               values_to = 'values', 
               -c(datetime,flag_allexo)) %>% 
  filter(!is.na(values))

buoyb_exo_vert %>% 
  filter(datetime >= inspect &
           datetime < (inspect + days(1))) %>% 
  ggplot(., aes(x = datetime, y = values, shape = flag_allexo)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  final_theme

# flag chlor rfu > 3
buoy2023_wq_L1 <- buoy2023_wq_L1 %>% 
  mutate(flag_exochl = if_else(Chlor_RFU > 3 & 
                                 datetime >= inspect &
                                 datetime < (inspect + days(1)),
                               's',
                               flag_exochl))

# flag bga > 0.3 as suspect on this date
buoy2023_wq_L1 <- buoy2023_wq_L1 %>% 
  mutate(flag_exobga = if_else(BGAPC_RFU > 0.3 & 
                                 datetime >= inspect &
                                 datetime < (inspect + days(1)),
                               's',
                               flag_exobga))

#### bga flag aug 14/15 > 0.1 suspect ----

inspect = as_date('2023-08-14')
buoyb_exo_vert %>% 
  filter(datetime >= inspect &
           datetime < (inspect + days(2))) %>% 
  ggplot(., aes(x = datetime, y = values, color = variable)) +
  geom_point() +
  facet_grid(variable ~ ., scales = "free_y") +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme

buoy2023_wq_L1 <- buoy2023_wq_L1 %>% 
  mutate(flag_exobga = if_else(BGAPC_RFU > 0.1 & 
                                 datetime >= inspect &
                                 datetime < (inspect + days(1)),
                               's',
                               flag_exobga))



#### oct 2 cleaning ----
inspect = as_date("2023-10-02")

buoyb_exo_vert %>% 
  filter(datetime >= inspect &
           datetime < (inspect + days(1))) %>% 
  ggplot(., aes(x = datetime, y = values, color = variable)) +
  geom_point() +
  facet_grid(variable ~ ., scales = "free_y") +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme

recode_dt = "2023-10-02 10:30"

buoy2023_wq_L1 <- buoy2023_wq_L1 %>% 
  mutate_at(vars(all_of(exo), all_of(exoinfo)),
            ~ case_when(datetime == as.POSIXct(recode_dt, tz = buoy_tz) ~ NA_real_,
                        TRUE ~ .))

#### bga flag oct 19 > 0.2 suspect ----

inspect = as_date('2023-10-19')

buoyb_exo_vert %>% 
  filter(datetime >= inspect &
           datetime < (inspect + days(1))) %>% 
  ggplot(., aes(x = datetime, y = values, color = variable)) +
  geom_point() +
  facet_grid(variable ~ ., scales = "free_y") +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme

buoy2023_wq_L1 <- buoy2023_wq_L1 %>% 
  mutate(flag_exobga = if_else(BGAPC_RFU > 0.2 & 
                                 datetime >= inspect &
                                 datetime < (inspect + days(1)),
                               's',
                               flag_exobga))

#### add general flags ----

#add clean and calibrate for do (calibration only pertinent to do)
buoy2023_wq_L1 <- buoy2023_wq_L1 %>% 
  mutate(flag_exodo = case_when(flag_allexo == 'c' ~ 'c', 
                                flag_allexo == 'w' ~ 'w',
                                TRUE ~ '')) 

#add calibration at beginning of season
buoy2023_wq_L1 <- buoy2023_wq_L1 %>% 
  mutate(flag_exodo = if_else(datetime == redeploy, 'c', flag_exodo))


# flag algae for bdl;
buoy2023_wq_L1 <- buoy2023_wq_L1 %>% 
  mutate(flag_exobga = if_else(BGAPC_RFU < 0, 'z', flag_exobga),
         flag_exobga = case_when(flag_exobga == '' & datetime == redeploy+minutes(10) ~ 'c',
                                 flag_exobga != '' & datetime == redeploy+minutes(10)  ~ paste0('c; ', flag_exobga),
                                 flag_exobga == '' & flag_allexo == 'w' ~ 'w',
                                 flag_exobga != '' & flag_allexo == 'w' ~ paste0('w; ', flag_exobga),
                                 TRUE ~ flag_exobga))

# drop bgapc ugl and chlor ugl
buoy2023_wq_L1 <- buoy2023_wq_L1 %>% 
  select(-BGAPC_UGL, -Chlor_UGL)

buoyb_exo_vert <- buoy2023_wq_L1 %>% 
  select(datetime, flag_allexo, any_of(exo)) %>% 
  pivot_longer(names_to = 'variable',
               values_to = 'values', 
               -c(datetime,flag_allexo)) %>% 
  filter(!is.na(values))

ggplot(buoyb_exo_vert, aes(x = datetime, y = values)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  final_theme

ggplot(buoy2023_wq_L1, aes(x = datetime, y = Chlor_RFU, color = flag_exochl)) +
  geom_point() +
  final_theme
ggplot(buoy2023_wq_L1, aes(x = datetime, y = BGAPC_RFU, color = flag_exobga)) +
  geom_point() +
  final_theme

### BGA ----

#calculate rolling average and rolling SD

buoy2023_wq_L1$BGA_stdev = zoo::rollapply(buoy2023_wq_L1$BGAPC_RFU, width=3, sd, align='center', partial=F, fill=NA)
range(buoy2023_wq_L1$BGA_stdev, na.rm = T)

bga_sd = sd(buoy2023_wq_L1$BGAPC_RFU, na.rm = T)
bga_sd

#flag 3*SD
buoy2023_wq_L1 <- buoy2023_wq_L1 %>% 
  mutate(flag_exobga = case_when(flag_exobga == '' & BGA_stdev > 3*bga_sd ~ 's',
                                 flag_exobga != '' & !grepl("s", flag_exobga) & BGA_stdev > 3*bga_sd ~ paste0('s; ', flag_exobga),
                                 TRUE ~ flag_exobga))

ggplot(buoy2023_wq_L1, aes(x = datetime, y = BGAPC_RFU, color = BGA_stdev, shape = flag_exobga)) +
  geom_point()

### chla ----

#calculate rolling average and rolling SD

buoy2023_wq_L1$chla_stdev = zoo::rollapply(buoy2023_wq_L1$Chlor_RFU, width=3, sd, align='center', partial=F, fill=NA)
range(buoy2023_wq_L1$chla_stdev, na.rm = T)

chla_sd = sd(buoy2023_wq_L1$Chlor_RFU, na.rm = T)
chla_sd

#flag 3*SD
buoy2023_wq_L1 <- buoy2023_wq_L1 %>% 
  mutate(flag_exochl = case_when(flag_exochl == '' & chla_stdev > 3*chla_sd ~ 's',
                                 flag_exochl != '' & !grepl("s", flag_exochl) & chla_stdev > 3*chla_sd ~ paste0('s; ', flag_exochl),
                                 TRUE ~ flag_exochl))


ggplot(buoy2023_wq_L1, aes(x = datetime, y = Chlor_RFU, color = chla_stdev, shape = flag_exochl)) +
  geom_point()

#remove SD cols
buoy2023_wq_L1 <- buoy2023_wq_L1 %>% 
  select(-BGA_stdev, -chla_stdev)

# flag cleaning
buoy2023_wq_L1 <- buoy2023_wq_L1 %>% 
  mutate(flag_exochl = case_when(flag_exochl == '' & datetime == redeploy+minutes(10)  ~ 'c',
                                 flag_exochl != '' & datetime == redeploy+minutes(10)  ~ paste0('c; ', flag_exochl),
                                 flag_exochl == '' & flag_allexo == 'w' ~ 'w',
                                 flag_exochl != '' & flag_allexo == 'w' ~ paste0('w; ', flag_exochl),
                                 TRUE ~ flag_exochl))

unique(buoy2023_wq_L1$flag_exochl)



### fdom ----
buoyb_fdom_vert <- buoy2023_wq_L1 %>% 
  select(datetime, all_of(exofdom)) %>% 
  pivot_longer(names_to = 'variable',
               values_to = 'value', 
               -datetime) %>% 
  filter(!is.na(value))

ggplot(buoyb_fdom_vert, aes(x = datetime, y = value)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y')

buoy2023_wq_L1 <- buoy2023_wq_L1 %>% 
  mutate(flag_exofdom = case_when(datetime == redeploy + minutes(10) ~ 'c',
                                  flag_allexo == 'w' ~ 'w',
                                  TRUE ~ ''))

# add calibration flag for TDS and cond
buoy2023_wq_L1 <- buoy2023_wq_L1 %>% 
  mutate(flag_exotds = case_when(datetime == redeploy+minutes(10)  ~ 'c',
                                 flag_allexo == 'w' ~ 'w',
                                 TRUE ~ ''),
         flag_exocond = case_when(datetime == redeploy+minutes(10)  ~ 'c',
                                  flag_allexo == 'w' ~ 'w',
                                  TRUE ~ ''))

ggplot(buoy2023_wq_L1, aes(x = datetime, y = TDS_mgl, color = flag_exotds)) +
  geom_point() +
  final_theme

ggplot(buoy2023_wq_L1, aes(x = datetime, y = Cond, color = flag_exocond)) +
  geom_point() +
  final_theme


### plot all data together bi-monthly ----
buoyb_exo_vert <- buoy2023_wq_L1 %>% 
  select(datetime, flag_allexo, any_of(exo)) %>% 
  pivot_longer(names_to = 'variable',
               values_to = 'values', 
               -c(datetime,flag_allexo)) %>% 
  filter(!is.na(values))

# plot bi-monthly
for(i in 1:length(two_weeks)) {
  chunk <- buoyb_exo_vert %>% 
    filter(datetime >= as.POSIXct(two_weeks[i], tz= buoy_tz) &
             datetime < (as.POSIXct(two_weeks[i], tz=buoy_tz) + weeks(2)))
  if (nrow(chunk) > 0) {
    ggplot(chunk, aes(x = datetime, y = values)) +
      geom_point() +
      facet_grid(variable ~ ., scales = 'free_y') +
      scale_x_datetime(date_minor_breaks = '1 day') +
      final_theme +
      labs(x = NULL,
           y = NULL,
           title = paste0('LSPA exo variables ', format(two_weeks[i], '%B')))
    ggsave(filename = paste0('graphs/2023/L1_exo_2wks_', two_weeks[i], '.jpg'), height = 8, width =10, units = 'in', dpi = 300)
  }
}

#clean up workspace
rm(buoyb_exo_vert, buoyb_fdom_vert, chunk, chunk_vert)

## DO ----

#low do is actually at 1m until move to loon
buoy2023_wq_L1 <- buoy2023_wq_L1 %>% 
  mutate(DOTempC_harbor = if_else(datetime < redeploy, DOLowTempC, NA_real_),
         DOLowTempC = if_else(datetime > redeploy, DOLowTempC, NA_real_),
         DOSat_harbor = if_else(datetime < redeploy, DOLowSat, NA_real_),
         DOLowSat = if_else(datetime > redeploy, DOLowSat, NA_real_),
         DOppm_harbor = if_else(datetime < redeploy, DOLowPPM, NA_real_),
         DOLowPPM = if_else(datetime > redeploy, DOLowPPM, NA_real_))

buoy_do_vert <- buoy2023_wq_L1 %>% 
  select(datetime, flag_do10m, all_of(lowDO)) %>% 
  pivot_longer(names_to = 'variable', 
               values_to =  'value', 
               -c(datetime,flag_do10m))

# recode where flagged
unique(buoy2023_wq_L1$flag_do10m) # all flags are out of water

buoy2023_wq_L1 <- buoy2023_wq_L1 %>% 
  mutate_at(vars(all_of(lowDO)),
            ~ case_when(flag_do10m != '' ~ NA_real_,
                        TRUE ~ .))

#add flag for calibration prior to deploy in April
buoy2023_wq_L1 <- buoy2023_wq_L1 %>% 
  mutate(flag_do10m = case_when(datetime == redeploy +minutes(10) ~ 'c',
                                TRUE ~ flag_do10m))

buoy_do_vert <- buoy2023_wq_L1 %>% 
  select(datetime, flag_do10m, all_of(lowDO)) %>% 
  pivot_longer(names_to = 'variable', 
               values_to =  'value', 
               -c(datetime,flag_do10m)) %>% 
  filter(!is.na(value))

ggplot(buoy_do_vert, aes(x = datetime, y = value, color = flag_do10m))+
  geom_point()+
  facet_grid(variable ~., scales = 'free_y')

# plot bi-monthly
for(i in 1:length(two_weeks)) {
  chunk <- buoy_do_vert %>% 
    filter(datetime >= as.POSIXct(two_weeks[i], tz= buoy_tz) &
             datetime < (as.POSIXct(two_weeks[i], tz=buoy_tz) + weeks(2)))
  if (nrow(chunk) > 0) {
    ggplot(chunk, aes(x = datetime, y = value)) +
      geom_point() +
      facet_grid(variable ~ ., scales = 'free_y') +
      scale_x_datetime(date_minor_breaks = '1 day') +
      final_theme +
      labs(x = NULL,
           y = NULL,
           title = paste0('LSPA low DO ', format(two_weeks[i], '%B')))
    ggsave(filename = paste0('graphs/2023/L0_LowDO_2wks_', two_weeks[i], '.jpg'), height = 8, width =10, units = 'in', dpi = 300)
  }
}

#### Jun 30 failure - recode all after first 0 value through eod jul 10 ----
inspect = as_date('2023-06-30')

buoyb_lodo_vert <- buoy2023_wq_L1 %>% 
  select(datetime, all_of(lowDO)) %>% 
  pivot_longer(names_to = 'variable',
               values_to = 'values', 
               -c(datetime)) %>% 
  filter(!is.na(values))

buoyb_lodo_vert %>% 
  filter(datetime >= inspect - days(2) &
           datetime < (inspect + days(2))) %>% 
  ggplot(., aes(x = datetime, y = values)) +
  geom_point() +
  scale_x_datetime(minor_breaks = "1 hour") +
  facet_grid(variable ~ ., scales = 'free_y') +
  final_theme

recode_dt = '2023-06-30 01:00'

buoy2023_wq_L1 <- buoy2023_wq_L1 %>% 
  mutate_at(vars(all_of(lowDO)), 
            ~ if_else(datetime >= ymd_hm(recode_dt, tz = buoy_tz) &
                        datetime <= as_date('2023-07-11'),
                      NA_real_,
                      .))

buoyb_lodo_vert <- buoy2023_wq_L1 %>% 
  select(datetime, all_of(lowDO)) %>% 
  pivot_longer(names_to = 'variable',
               values_to = 'values', 
               -c(datetime)) %>% 
  filter(!is.na(values))

buoyb_lodo_vert %>% 
  filter(datetime >= inspect - days(2) &
           datetime < (inspect + days(2))) %>% 
  ggplot(., aes(x = datetime, y = values)) +
  geom_point() +
  scale_x_datetime(minor_breaks = "1 hour") +
  facet_grid(variable ~ ., scales = 'free_y') +
  final_theme

# and recode the outlier prior to failure
recode_dt = ymd_hm('2023-06-29 16:10', tz = buoy_tz)

buoy2023_wq_L1 <- buoy2023_wq_L1 %>% 
  mutate_at(vars(DOLowPPM, DOLowSat), 
            ~ if_else(datetime == recode_dt,
                      NA_real_,
                      .))

buoyb_lodo_vert <- buoy2023_wq_L1 %>% 
  select(datetime, all_of(lowDO)) %>% 
  pivot_longer(names_to = 'variable',
               values_to = 'values', 
               -c(datetime)) %>% 
  filter(!is.na(values))

buoyb_lodo_vert %>% 
  filter(datetime >= inspect - days(2) &
           datetime < (inspect + days(2))) %>% 
  ggplot(., aes(x = datetime, y = values)) +
  geom_point() +
  scale_x_datetime(minor_breaks = "1 hour") +
  facet_grid(variable ~ ., scales = 'free_y') +
  final_theme

#### redeploy aug 10 ----
inspect = as_date('2023-08-10')

buoyb_lodo_vert %>% 
  filter(datetime >= inspect &
           datetime < (inspect + days(1))) %>% 
  ggplot(., aes(x = datetime, y = values)) +
  geom_point() +
  scale_x_datetime(minor_breaks = "1 hour") +
  facet_grid(variable ~ ., scales = 'free_y') +
  final_theme

recode_dt = ymd_hm("2023-08-10 08:40", tz = buoy_tz)

buoy2023_wq_L1 <- buoy2023_wq_L1 %>% 
  mutate_at(vars(all_of(lowDO)), 
            ~ if_else(datetime >= inspect &
                        datetime < recode_dt,
                      NA_real_,
                      .))

buoyb_lodo_vert <- buoy2023_wq_L1 %>% 
  select(datetime, all_of(lowDO)) %>% 
  pivot_longer(names_to = 'variable',
               values_to = 'values', 
               -c(datetime)) %>% 
  filter(!is.na(values))

buoyb_lodo_vert %>% 
  filter(datetime >= inspect &
           datetime < (inspect + days(1))) %>% 
  ggplot(., aes(x = datetime, y = values)) +
  geom_point() +
  scale_x_datetime(minor_breaks = "1 hour") +
  facet_grid(variable ~ ., scales = 'free_y') +
  final_theme

#### oct 14 failure - recode all after first 0 value through end of record ----
inspect = as_date('2023-10-14')

buoyb_lodo_vert %>% 
  filter(datetime >= inspect &
           datetime < (inspect + days(1))) %>% 
  ggplot(., aes(x = datetime, y = values)) +
  geom_point() +
  scale_x_datetime(minor_breaks = "1 hour") +
  facet_grid(variable ~ ., scales = 'free_y') +
  final_theme

recode_dt = ymd_hm("2023-10-14 16:00", tz = buoy_tz)

buoy2023_wq_L1 <- buoy2023_wq_L1 %>% 
  mutate_at(vars(all_of(lowDO)), 
            ~ if_else(datetime >= recode_dt &
                        datetime <= as_date('2023-12-31'),
                      NA_real_,
                      .))

buoyb_lodo_vert <- buoy2023_wq_L1 %>% 
  select(datetime, all_of(lowDO)) %>% 
  pivot_longer(names_to = 'variable',
               values_to = 'values', 
               -c(datetime)) %>% 
  filter(!is.na(values))

buoyb_lodo_vert %>% 
  filter(datetime >= inspect &
           datetime < (inspect + days(1))) %>% 
  ggplot(., aes(x = datetime, y = values)) +
  geom_point() +
  scale_x_datetime(minor_breaks = "1 hour") +
  facet_grid(variable ~ ., scales = 'free_y') +
  final_theme


# plot bi-monthly
for(i in 1:length(two_weeks)) {
  chunk <- buoyb_lodo_vert %>% 
    filter(datetime >= as.POSIXct(two_weeks[i], tz= buoy_tz) &
             datetime < (as.POSIXct(two_weeks[i], tz=buoy_tz) + weeks(2)))
  if (nrow(chunk) > 0) {
    ggplot(chunk, aes(x = datetime, y = values)) +
      geom_point() +
      facet_grid(variable ~ ., scales = 'free_y') +
      scale_x_datetime(date_minor_breaks = '1 day') +
      final_theme +
      labs(x = NULL,
           y = NULL,
           title = paste0('LSPA low DO ', format(two_weeks[i], '%B')))
    ggsave(filename = paste0('graphs/2023/L1_LowDO_2wks_', two_weeks[i], '.jpg'), height = 8, width =10, units = 'in', dpi = 300)
  }
}


#clean up workspace
rm(buoy_do_vert, chunk)

## Upper DO in harbor ----
harbordo_vert <- buoy2023_wq_L1 %>% 
  select(datetime, DOSat_harbor, DOppm_harbor, DOTempC_harbor) %>% 
  pivot_longer(names_to = 'variable', values_to = 'value', -datetime) %>% 
  filter(!is.na(value))

ggplot(harbordo_vert, aes(x = datetime, y = value)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  scale_x_datetime(minor_breaks = '1 month')

# need to remove data during deployment
harbordo_vert %>% 
  filter(datetime > as.Date(redeploy) & 
           datetime <= as.Date(redeploy) + days(1)) %>% 
  ggplot(., aes(x = datetime, y = value)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  scale_x_datetime(minor_breaks = '1 hour')


buoy2023_wq_L1 <- buoy2023_wq_L1 %>% 
  mutate_at(vars(DOSat_harbor, DOppm_harbor, DOTempC_harbor),
            ~case_when(datetime >= as.POSIXct('2023-05-15 8:00', tz = buoy_tz) &
                         datetime < redeploy ~ NA_real_,
                       TRUE ~ .))

# add 'in transit' during the time from recode to redeploy
buoy2023_wq_L1 <- buoy2023_wq_L1 %>% 
  mutate(location = case_when(datetime >= as.POSIXct('2023-05-15 8:00', tz = buoy_tz) &
                              datetime < redeploy ~ "in transit",
                              datetime <  as.POSIXct('2023-05-15 8:00', tz = buoy_tz) ~ "harbor",
                            .default = location))

# plot to check
harbordo_vert %>% 
  filter(datetime > redeploy - days(5) & datetime < redeploy + days(5)) %>% 
  ggplot(., aes(x = datetime, y = value)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  scale_x_datetime(minor_breaks = '1 hour')

ggplot(harbordo_vert, aes(x = datetime, y = value)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  scale_x_datetime(minor_breaks = '1 month')


#double check location
ggplot(buoy2023_wq_L1, aes(x = datetime, y = location)) +
  geom_point()


# 2022 MET DATA ----

# get location from wq data and apply to met
first_loon = first(buoy2023_wq_L1$datetime[buoy2023_wq_L1$location == 'loon'])
first_intransit = first(buoy2023_wq_L1$datetime[buoy2023_wq_L1$location == 'in transit'])
first_offline = first(buoy2023_wq_L1$datetime[buoy2023_wq_L1$location == 'offline'])

buoy2023_met_L1 <- buoy2023_met_L1 %>% 
  mutate(location = case_when(datetime < first_intransit ~ 'harbor',
                              datetime >= first_intransit & datetime < first_loon ~ 'in transit',
                              datetime >= first_loon & datetime < first_offline ~ 'loon',
                              TRUE ~ ''))

# recode when in transit
buoy2023_met_L1 <- buoy2023_met_L1 %>% 
  mutate_at(all_of(met),
            ~ case_when(location == 'in transit' ~ NA_real_,
                        TRUE ~ .))

# look at flags
unique(buoy2023_met_L1$flag_allmet)

# no flags

# plotting all of the met data at same time takes too long - only plot by data group

## air temp and rel hum ----
buoy_temp_vert <- buoy2023_met_L1 %>% 
  select(datetime, all_of(air), location) %>% 
  pivot_longer(names_to = 'variable', 
               values_to =  'value', 
               -c(datetime, location)) %>% 
  filter(!is.na(value))

# plot bi-monthly
for(i in 1:length(two_weeks)) {
  chunk <- buoy_temp_vert %>% 
    filter(datetime >= as.POSIXct(two_weeks[i], tz= buoy_tz) &
             datetime < (as.POSIXct(two_weeks[i], tz=buoy_tz) + weeks(2)))
  if(nrow(chunk>0)){
    ggplot(chunk, aes(x = datetime, y = value, color = location)) +
      geom_point() +
      facet_grid(variable ~ ., scales = 'free_y') +
      scale_x_datetime(date_minor_breaks = '1 day') +
      final_theme +
      labs(x = NULL,
           y = NULL,
           title = paste0('LSPA buoy Air Temp RH ', format(two_weeks[i], '%B')))
    ggsave(filename = paste0('graphs/2023/L0_airRH_2wks_', two_weeks[i], '.jpg'), height = 8, width =10, units = 'in', dpi = 300)
  }
}

# just need to recode from beginning of offline time to eoy
buoy2023_met_L1 <- buoy2023_met_L1 %>% 
  mutate_at(vars(all_of(met)),
            ~ if_else(datetime >= first_offline, NA_real_, .))


## wind ----
buoy_wind_vert <- buoy2023_met_L1 %>% 
  select(datetime, all_of(wind), location) %>% 
  pivot_longer(names_to = 'variable', 
               values_to =  'value', 
               -c(datetime, location))

# plot monthly
for(i in 1:length(two_weeks)) {
  chunk <- buoy_wind_vert %>% 
    filter(datetime >= as.POSIXct(two_weeks[i], tz= buoy_tz) &
             datetime < (as.POSIXct(two_weeks[i], tz=buoy_tz) + weeks(2)))
  if(nrow(chunk>0)){
    ggplot(chunk, aes(x = datetime, y = value, color = location)) +
      geom_point() +
      facet_grid(variable ~ ., scales = 'free_y') +
      scale_x_datetime(date_minor_breaks = '1 day') +
      final_theme +
      labs(x = NULL,
           y = NULL,
           title = paste0('LSPA buoy wind ', format(two_weeks[i], '%B')))
    ggsave(filename = paste0('graphs/2023/L0_wind_monthly_', two_weeks[i], '.jpg'), height = 8, width =10, units = 'in', dpi = 300)
  }
}


#### Jan 3 ----
inspect = ymd('2023-01-03', tz = buoy_tz)

buoy_wind_vert %>% 
  filter(datetime >= inspect - hours(12) &
           datetime <= inspect + days(1) + hours(12)) %>% 
  ggplot(., aes(x = datetime, y = value)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme

buoy2023_met_L1 <- buoy2023_met_L1 %>% 
  mutate_at(all_of(wind),
            ~case_when(datetime >= inspect &
                         datetime < inspect + days(1) &
                         MaxWindSp == 0 ~ NA_real_, 
                       TRUE ~ .))

buoy_wind_vert <- buoy2023_met_L1 %>% 
  select(datetime, all_of(wind), location) %>% 
  pivot_longer(names_to = 'variable', 
               values_to =  'value', 
               -c(datetime, location))

buoy_wind_vert %>% 
  filter(datetime >= inspect - hours(12) &
           datetime <= inspect + days(1) + hours(12)) %>% 
  ggplot(., aes(x = datetime, y = value)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme


#### Jan 6 ----
inspect = ymd('2023-01-06', tz = buoy_tz)

buoy_wind_vert %>% 
  filter(datetime >= inspect - hours(12) &
           datetime <= inspect + days(1) + hours(12)) %>% 
  ggplot(., aes(x = datetime, y = value)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme

buoy2023_met_L1 <- buoy2023_met_L1 %>% 
  mutate_at(all_of(wind),
            ~case_when(datetime >= inspect &
                         datetime < inspect + days(1) &
                         MaxWindSp == 0 ~ NA_real_, 
                       TRUE ~ .))

buoy_wind_vert <- buoy2023_met_L1 %>% 
  select(datetime, all_of(wind), location) %>% 
  pivot_longer(names_to = 'variable', 
               values_to =  'value', 
               -c(datetime, location))

buoy_wind_vert %>% 
  filter(datetime >= inspect - hours(12) &
           datetime <= inspect + days(1) + hours(12)) %>% 
  ggplot(., aes(x = datetime, y = value)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme

#### Jan 23-25 ----

inspect = ymd('2023-01-23', tz = buoy_tz)

buoy_wind_vert %>% 
  filter(datetime >= inspect - hours(12) &
           datetime <= inspect + days(3) + hours(12)) %>% 
  ggplot(., aes(x = datetime, y = value)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme

buoy2023_met_L1 <- buoy2023_met_L1 %>% 
  mutate_at(all_of(wind),
            ~case_when(datetime >= inspect - hours(12) &
                         datetime <= inspect + days(3) + hours(12) &
                         MaxWindSp == 0 ~ NA_real_, 
                       TRUE ~ .))

buoy_wind_vert <- buoy2023_met_L1 %>% 
  select(datetime, all_of(wind), location) %>% 
  pivot_longer(names_to = 'variable', 
               values_to =  'value', 
               -c(datetime, location))

buoy_wind_vert %>% 
  filter(datetime >= inspect - hours(12) &
           datetime <= inspect + days(3) + hours(12)) %>% 
  ggplot(., aes(x = datetime, y = value)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme

#### Mar 13-15 ----

inspect = ymd('2023-03-13', tz = buoy_tz)

buoy_wind_vert %>% 
  filter(datetime >= inspect - hours(12) &
           datetime <= inspect + days(3) + hours(12)) %>% 
  ggplot(., aes(x = datetime, y = value)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme

buoy2023_met_L1 <- buoy2023_met_L1 %>% 
  mutate_at(all_of(wind),
            ~case_when(datetime >= inspect - hours(12) &
                         datetime <= inspect + days(3) + hours(12) &
                         MaxWindSp == 0 ~ NA_real_, 
                       TRUE ~ .))

buoy_wind_vert <- buoy2023_met_L1 %>% 
  select(datetime, all_of(wind), location) %>% 
  pivot_longer(names_to = 'variable', 
               values_to =  'value', 
               -c(datetime, location))

buoy_wind_vert %>% 
  filter(datetime >= inspect - hours(12) &
           datetime <= inspect + days(3) + hours(12)) %>% 
  ggplot(., aes(x = datetime, y = value)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme

# flag data right before recode

buoy2023_met_L1 <- buoy2023_met_L1 %>% 
  mutate(flag_wind = if_else(datetime >= inspect + days(1) &
                               datetime <= inspect + days(2) &
                               MaxWindSp > 20, 's', ''))


#### Apr 1 ----

inspect = ymd('2023-04-01', tz = buoy_tz)

buoy_wind_vert %>% 
  filter(datetime >= inspect - hours(12) &
           datetime <= inspect + days(1) + hours(12)) %>% 
  ggplot(., aes(x = datetime, y = value)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme

buoy2023_met_L1 <- buoy2023_met_L1 %>% 
  mutate_at(all_of(wind),
            ~case_when(datetime >= inspect &
                         datetime <= inspect + days(1) &
                         MaxWindSp == 0 ~ NA_real_, 
                       TRUE ~ .))

buoy_wind_vert <- buoy2023_met_L1 %>% 
  select(datetime, all_of(wind), location) %>% 
  pivot_longer(names_to = 'variable', 
               values_to =  'value', 
               -c(datetime, location))

buoy_wind_vert %>% 
  filter(datetime >= inspect - hours(12) &
           datetime <= inspect + days(1) + hours(12)) %>% 
  ggplot(., aes(x = datetime, y = value)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme

#### Flag Apr 27 ----

inspect = ymd("2023-04-27", tz = buoy_tz)


buoy_wind_vert %>% 
  filter(datetime >= inspect - hours(12) &
           datetime <= inspect + days(1) + hours(12)) %>% 
  ggplot(., aes(x = datetime, y = value)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme

buoy2023_met_L1 <- buoy2023_met_L1 %>% 
  mutate(flag_wind = if_else(datetime >= inspect &
                         datetime <= inspect + days(1) &
                         MaxWindSp > 10, 's', flag_wind))


# plot bi-monthly
for(i in 1:length(two_weeks)) {
  chunk <- buoy_wind_vert %>% 
    filter(datetime >= as.POSIXct(two_weeks[i], tz= buoy_tz) &
             datetime < (as.POSIXct(two_weeks[i], tz=buoy_tz) + weeks(2)))
  if(nrow(chunk)>0){
    ggplot(chunk, aes(x = datetime, y = value, color = location)) +
      geom_point() +
      facet_grid(variable ~ ., scales = 'free_y') +
      scale_x_datetime(date_minor_breaks = '1 day') +
      final_theme +
      labs(x = NULL,
           y = NULL,
           title = paste0('LSPA buoy wind ', format(two_weeks[i], '%B')))
    ggsave(filename = paste0('graphs/2023/L1_wind_monthly_', two_weeks[i], '.jpg'), height = 8, width =10, units = 'in', dpi = 300)
  }
}

## PAR ----
range(buoy2023_met_L1$PAR_ave_umolpspm2, na.rm = T)
range(buoy2023_met_L1$PAR_tot_mmolpm2, na.rm = T)

buoy_par_vert <- buoy2023_met_L1 %>% 
  select(datetime, all_of(par2021), location) %>% 
  pivot_longer(names_to = 'variable', 
               values_to =  'value', 
               -c(datetime, location)) %>% 
  filter(!is.na(value))

ggplot(buoy_par_vert, aes(x = datetime, y = value, color = location)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  scale_x_datetime(date_minor_breaks = '1 month') +
  final_theme 


# plot bi-monthly
for(i in 1:length(two_weeks)) {
  chunk <- buoy_par_vert %>% 
    filter(datetime >= as.POSIXct(two_weeks[i], tz= buoy_tz) &
             datetime < (as.POSIXct(two_weeks[i], tz=buoy_tz) + weeks(2)))
  if (nrow(chunk) > 0) {
    ggplot(chunk, aes(x = datetime, y = value, color = location)) +
      geom_point() +
      facet_grid(variable ~ ., scales = 'free_y') +
      scale_x_datetime(date_minor_breaks = '1 day') +
      final_theme +
      labs(x = NULL,
           y = NULL,
           title = paste0('LSPA buoy PAR ', format(two_weeks[i], '%B')))
    ggsave(filename = paste0('graphs/2023/L0_PAR_monthly_', two_weeks[i], '.jpg'), height = 8, width =10, units = 'in', dpi = 300)
  }
}

#data look pretty good, but we still have night time par issues
buoy2023_met_L1$flag_par = ''
# still have par >0 at night issue. flagging all PAR data 
buoy2023_met_L1 <- buoy2023_met_L1 %>% 
  mutate(flag_par = case_when(flag_par == '' ~ 'n',
                              TRUE ~ paste0('n; ', flag_par)))

buoy_par_vert <- buoy2023_met_L1 %>% 
  select(datetime, all_of(par2022), location, flag_par) %>% 
  pivot_longer(names_to = 'variable', 
               values_to =  'value', 
               -c(datetime, location, flag_par))

# plot monthly
for(i in 1:length(two_weeks)) {
  chunk <- buoy_par_vert %>% 
    filter(datetime >= as.POSIXct(two_weeks[i], tz= buoy_tz) &
             datetime < (as.POSIXct(two_weeks[i], tz=buoy_tz) + dtwo_weeks(1)))
  ggplot(chunk, aes(x = datetime, y = value, color = flag_par, shape = location)) +
    geom_point() +
    facet_grid(variable ~ ., scales = 'free_y') +
    scale_x_datetime(date_minor_breaks = '1 day') +
    final_theme +
    labs(x = NULL,
         y = NULL,
         title = paste0('LSPA buoy PAR ', format(two_weeks[i], '%B')))
  ggsave(filename = paste0('graphs/2022/L1_PAR_monthly_', two_weeks[i], '.jpg'), height = 8, width =10, units = 'in', dpi = 300)
}

# Convert to CV ----
colnames(buoy2023_wq_L1)
buoy2023_wq_L1 <- buoy2023_wq_L1 %>% 
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

colnames(buoy2023_met_L1)
buoy2023_met_L1 <- buoy2023_met_L1 %>% 
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
colnames(buoy2023_wq_L1)


#export L1 tempstring file
unique(buoy2023_wq_L1$flag_alltemp) #these are fine to drop
buoy2023_wq_L1 %>%
  select(datetime, waterTemperature_degC_0p1m:waterTemperature_degC_10m, location) %>%
  mutate(datetime = as.character(datetime)) %>%
  write_csv(., file.path(dump_dir, 'tempstring/2023_tempstring_L1_v2024.csv'))

#export l1 exo file
unique(buoy2023_wq_L1$flag_exochl)
unique(buoy2023_wq_L1$flag_exobga)
unique(buoy2023_wq_L1$flag_exofdom)
unique(buoy2023_wq_L1$flag_exocond)

buoy2023_wq_L1 %>%
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
  write_csv(., file.path(dump_dir, 'exo/2023_exo_L1_v2024.csv'))

#export l1 do file
unique(buoy2023_wq_L1$flag_do10m)
#recode v to '', not applicable to data
buoy2023_wq_L1 <- buoy2023_wq_L1 %>% 
  mutate(flag_do10m = case_when(flag_do10m == 'v' ~ '',
                                TRUE ~ flag_do10m))
buoy2023_wq_L1 %>%
  select(datetime, 
         waterTemperature_DO_degC_0p75m:oxygenDissolved_mgl_0p75m,
         waterTemperature_DO_degC_10m:oxygenDissolved_mgl_10m,
         flag_do10m, 
         location) %>%
  mutate(datetime = as.character(datetime)) %>%
  write_csv(., file.path(dump_dir, 'do/2023_do_L1_v2024.csv'))


## Met Data ----
colnames(buoy2023_met_L1)

unique(buoy2023_met_L1$flag_allmet)
unique(buoy2023_met_L1$flag_par)

# apply the 'h' flag from all met to flag_par
buoy2023_met_L1 <- buoy2023_met_L1 %>% 
  mutate(flag_par = case_when(flag_allmet == 'h' ~ paste(flag_par, 'h', sep = '; '),
                              TRUE ~ flag_par))

#export l1 met file
buoy2023_met_L1 %>%
  select(datetime, 
         airTemperature_degC, relativeHumidity_perc, 
         radiationIncomingPARAverage_mmolm2s, radiationIncomingPARTotal_mmolm2, flag_par,
         winddirectionInstantaneous_deg:windDirectionAverage_deg,
         location) %>%
  mutate(datetime = as.character(datetime)) %>%
  write_csv(., file.path(dump_dir, 'met/2023_met_L1_v2024.csv'))
