#*****************************************************************
#*      Cary Institute of Ecosystem Studies (Millbrook, NY)      *
#*                                                               *
#* TITLE:   Sunapee_buoy_2022.r                                  *
#* AUTHOR:  Bethel Steele                                        *
#* DATE:    05Feb2023                                            *
#* PROJECT: Lake Sunapee Buoy Data Cleaning                      *
#*****************************************************************

source('library_func_lists.R')

# #point to directories
data_dir = 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L0/2022/'
log_dir = 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/operation notes/'
dump_dir = 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/'

# store time zones
buoy_tz = 'Etc/GMT+5'

log_tz = 'America/New_York'

# BRING IN RAW DATA ----

## GitHub Processes ----

## download file from GH - this is a live file, we'll save a version of the file locally that has been filtered for 2023 only
download.file('https://raw.githubusercontent.com/FLARE-forecast/SUNP-data/sunp-buoy-data/SUNP_buoy_met.csv',
              file.path(data_dir, 'buoy_met.csv'),
              method = 'curl')
download.file('https://raw.githubusercontent.com/FLARE-forecast/SUNP-data/sunp-buoy-data/SUNP_buoy_wq.csv',
              file.path(data_dir, 'buoy_wq.csv'),
              method = 'curl')

## read, filter and save file ----
buoy2022_met_L0 = read.csv(file.path(data_dir, 'buoy_met.csv'),
                        col.names = met2022, 
                        skip = 3,
                        na.strings = 'NAN') %>% 
  mutate(datetime = ymd_hms(datetime),
         datetime = force_tz(datetime, buoy_tz)) %>% 
  filter(datetime >= force_tz(ymd_hms('2022-01-01 00:00:00'), buoy_tz) &
           datetime <= force_tz(ymd_hms('2022-12-31 23:59:59'), buoy_tz))
write.csv(buoy2022_met_L0, file.path(data_dir, 'buoy_met_L0_2022.csv'), row.names = F)
unlink(file.path(data_dir, 'buoy_met.csv'))

buoy2022_wq_L0 <- read.csv(file.path(data_dir, 'buoy_wq.csv'),
                            col.names = buoy2022,
                            skip = 3,
                            na.strings = 'NAN') %>% 
  mutate(datetime = ymd_hms(datetime),
         datetime = force_tz(datetime, buoy_tz)) %>% 
  filter(datetime >= force_tz(ymd_hms('2022-01-01 00:00:00'), buoy_tz) &
           datetime <= force_tz(ymd_hms('2022-12-31 23:59:59'), buoy_tz))
write.csv(buoy2022_wq_L0, file.path(data_dir, 'buoy_wq_L0_2022.csv'), row.names = F)
unlink(file.path(data_dir, 'buoy_wq.csv'))

## bring in log info ----

log <- read_xlsx(file.path(log_dir, 'SUNP_MaintenanceLog_2022.xlsx'),
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
  mutate(TIMESTAMP_end = case_when(is.na(TIMESTAMP_end) ~ as.POSIXct('2023-01-01', tz = buoy_tz),
                                   TRUE ~ TIMESTAMP_end))

# add flags to log
log <- log %>% 
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

# FORMAT DATA AND DEAL WITH TIMEZONES ----

## 2022 ----

### wq ----
buoy2022_wq_L1 <- buoy2022_wq_L0 %>% 
  mutate(date = as_date(datetime)) %>% 
  rownames_to_column('rowid_wq') %>% 
  select(-index)

#remove the EXO date and time columns
buoy2022_wq_L1 <- buoy2022_wq_L1 %>% 
  select(-EXO_date, -EXO_time)


#double check total number of observations for each day (should only be 144 or less to confirm no DST)
datetimetable <- buoy2022_wq_L1 %>%
  mutate(date = lubridate::as_date(datetime)) %>% 
  group_by(date) %>%
  summarize(n = length(datetime)) %>% 
  arrange(n)
head(datetimetable, n = 10)
tail(datetimetable)

#look at date with 145 records
buoy2022_wq_L1[buoy2022_wq_L1$date == as_date('2022-12-05'),]$datetime
#duplicate is 2022-12-05 8:50, check record to see if identical
dupedate <- buoy2022_wq_L1[buoy2022_wq_L1$date == as.POSIXct('2022-12-05 8:50', buoy_tz),]
#data are identical
buoy2022_wq_L1 <- buoy2022_wq_L1[buoy2022_wq_L1$rowid_wq != 48727,] #remove the dupe row

# get list of all datetimes from start of this record to end
first_time <- first(buoy2022_wq_L1$datetime)
alltimes_2022 <- as.data.frame(seq.POSIXt(as.POSIXct(first_time, tz=buoy_tz), 
                                          as.POSIXct('2022-12-31 23:50', tz=buoy_tz), '10 min')) %>%
  rename("datetime" = !!names(.[1])) %>% 
  rowid_to_column('index')
buoy2022_wq_L1 <- buoy2022_wq_L1 %>%
  right_join(., alltimes_2022) %>%
  arrange(datetime)
#add flag for missing data from buoy
buoy2022_wq_L1 <- buoy2022_wq_L1 %>% 
  mutate(location = case_when(is.na(rowid_wq) ~ 'offline',
                              TRUE ~ NA_character_)) 


### met ----
buoy2022_met_L1 <- buoy2022_met_L0 %>% 
  mutate(date = as_date(datetime)) %>% 
  rownames_to_column('rowid_met') %>% 
  select(-index)

#check to see if these are the same
ggplot(buoy2022_met_L1, aes(x = AveWindSp, y = AveWindSp2)) +
  geom_point()

#remove std column and dupe wind column
buoy2022_met_L1 <- buoy2022_met_L1 %>% 
  select(-STD_winddir, -AveWindSp2)

#look at time 
#double check total number of observations for each day (should only be 1440 or less to confirm no DST)
datetimetable <- buoy2022_met_L1 %>%
  mutate(date = as_date(datetime)) %>% 
  group_by(date) %>%
  summarize(n = length(datetime)) %>% 
  arrange(n)
head(datetimetable, n = 10)
tail(datetimetable)

#look at date with 1450 records
buoy2022_met_L1[buoy2022_met_L1$date == as_date('2022-12-05'),]$datetime

#duplicate is 2022-12-06 14:10:00-14:20, check record to see if identical
dupedate <- buoy2022_met_L1[buoy2022_met_L1$datetime >= as.POSIXct('2022-12-05 8:50', tz = buoy_tz) &
                               buoy2022_met_L1$datetime < as.POSIXct('2022-12-05 9:01', tz = buoy_tz),]

#data are identical for those 10 minutes: remove those data
buoy2022_met_L1 <- buoy2022_met_L1 %>% 
  filter(rowid_met < 487262 |
           rowid_met > 487271)

# LOOK AT BATTERY ----

ggplot(buoy2022_wq_L1, aes(x = datetime, y = LoggerBatV)) +
  geom_point()
ggplot(buoy2022_wq_L1, aes(x = datetime, y = RadioBatV)) +
  geom_point()
ggplot(buoy2022_wq_L1, aes(x = datetime, y = EXO_batt_V)) +
  geom_point()
#battery is a bit low in January, but battery really only impacts underwater sensors

# 2022 wq ----

#add flags from log
#initiate columns for wq file
buoy2022_wq_L1$flag_allexo = ''
buoy2022_wq_L1$flag_do10m = ''
buoy2022_wq_L1$flag_alltemp = ''

for(l in 1:nrow(log)){
  if (log$instrument[l] == 'EXO') {
    buoy2022_wq_L1 <- buoy2022_wq_L1 %>% 
      mutate(flag_allexo = case_when(datetime >= log$TIMESTAMP_start[l] &
                                       datetime <= log$TIMESTAMP_end[l] ~ log$flag[l],
                                     TRUE ~ flag_allexo))
  } else if (log$instrument[l] == 'do' | log$instrument[l] == 'DO') {
    buoy2022_wq_L1 <- buoy2022_wq_L1 %>% 
      mutate(flag_do10m = case_when(datetime >= log$TIMESTAMP_start[l] &
                                        datetime <= log$TIMESTAMP_end[l] ~ log$flag[l], 
                                      TRUE ~ flag_do10m))
  } else if (log$instrument[l] == 'temp') {
    buoy2022_wq_L1 <- buoy2022_wq_L1 %>% 
      mutate(flag_alltemp = case_when(datetime >= log$TIMESTAMP_start[l] &
                                        datetime <= log$TIMESTAMP_end[l] ~ log$flag[l],
                                      TRUE ~ flag_alltemp))
  } 
}

#and for met file
buoy2022_met_L1$flag_allmet = ''

for(l in 1:nrow(log)){
  if (log$data_table[l] == 'met') {
    buoy2022_met_L1 <- buoy2022_met_L1 %>% 
      mutate(flag_allmet = case_when(datetime >= log$TIMESTAMP_start[l] &
                                       datetime <= log$TIMESTAMP_end[l] ~ log$flag[l],
                                     TRUE ~ flag_allmet))
  } else {
  }
}


## thermistors ----

#recode as necessary from log
unique(buoy2022_wq_L1$flag_alltemp)
buoy2022_wq_L1 <- buoy2022_wq_L1 %>% 
  mutate_at(vars(all_of(alltemp)),
            ~ case_when(flag_alltemp == 'o' ~ NA_real_, 
                        flag_alltemp == 'R' ~ NA_real_, #all flags indicate that sensors were out of the water
                        TRUE ~ .))

buoy_therm_vert <- buoy2022_wq_L1 %>% 
  select(datetime, flag_alltemp, all_of(alltemp)) %>% 
  pivot_longer(names_to = 'variable',
               values_to = 'values', 
               -c(datetime,flag_alltemp))

#need to find deployment date
buoy_therm_vert %>% 
  filter(between(datetime, ymd('2022-04-20'), ymd('2022-05-01'))) %>% 
  ggplot(., aes(x = datetime, y = values, color = variable)) +
  geom_point() +
  scale_x_datetime(date_minor_breaks = '1 day')
buoy_therm_vert %>% 
  filter(between(datetime, ymd('2022-05-01'), ymd('2022-05-15'))) %>% 
  ggplot(., aes(x = datetime, y = values, color = variable)) +
  geom_point() +
  scale_x_datetime(date_minor_breaks = '1 day')
#looks like deployment was on April 26?

#recode record prior to buoy deploy
redeploy = force_tz(ymd('2022-04-26'), buoy_tz)

buoy2022_wq_L1 <- buoy2022_wq_L1 %>% 
  mutate_at(vars(all_of(alltemp)),
            ~ case_when(datetime <= redeploy ~ NA_real_,
                        TRUE ~ .))

buoyb_therm_vert <- buoy2022_wq_L1 %>% 
  select(datetime, flag_alltemp, all_of(alltemp)) %>% 
  pivot_longer(names_to = 'variable',
               values_to = 'values', 
               -c(datetime,flag_alltemp))

ggplot(buoyb_therm_vert, aes(x = datetime, y = values, color = variable)) +
  geom_point() +
  final_theme

#look at data on a monthly basis
months = c(seq.Date(as.Date('2022-01-01'), as.Date('2023-01-01'), 'month'))

#plot data in monthly iterations
for(i in 1:length(months)) {
  chunk <- buoy2022_wq_L1 %>% 
    filter(datetime >= force_tz(ymd(months[i]), buoy_tz) &
             datetime < force_tz((ymd(months[i]) + dmonths(1)), tz=buoy_tz))
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
  ggsave(filename = paste0('graphs/2022/L0_temp_monthly_', months[i], '.jpg'), 
         height = 8, width =10, units = 'in', dpi = 300)
}

#deployment on April 26
buoyb_therm_vert %>% 
  filter(datetime >= as.POSIXct('2022-04-26 00:00', tz = buoy_tz)&
           datetime < as.POSIXct('2022-04-27 00:00', tz= buoy_tz)) %>% 
  ggplot(., aes(x = datetime, y = values, color = variable)) +
  geom_point() +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme

redeploy_exact = as.POSIXct('2022-04-26 11:00', tz= buoy_tz)

unique(buoy2022_wq_L1$location)
buoy2022_wq_L1 <- buoy2022_wq_L1 %>% 
  mutate_at(vars(all_of(alltemp)),
            ~ case_when(datetime <= redeploy_exact ~ NA_real_,
                        TRUE ~ .)) %>% 
  mutate(location = case_when(datetime > redeploy_exact ~ 'loon',
                              TRUE ~ 'harbor'))

buoyb_therm_vert <- buoy2022_wq_L1 %>% 
  select(datetime, flag_alltemp, all_of(alltemp)) %>% 
  pivot_longer(names_to = 'variable',
               values_to = 'values', 
               -c(datetime,flag_alltemp))

buoyb_therm_vert %>% 
  filter(datetime >= as.POSIXct('2022-04-26 00:00', tz = buoy_tz)&
           datetime < as.POSIXct('2022-04-27 00:00', tz= buoy_tz)) %>% 
  ggplot(., aes(x = datetime, y = values, color = variable)) +
  geom_point() +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme

#Aug 4/5
buoyb_therm_vert %>% 
  filter(datetime >= as.POSIXct('2022-08-04 00:00', tz = buoy_tz)&
           datetime < as.POSIXct('2022-08-07 00:00', tz= buoy_tz)) %>% 
  ggplot(., aes(x = datetime, y = values, color = variable)) +
  geom_point() +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme

#messy, but likely a wind event

rm(buoyb_therm_vert)


## exo ----
#recode as necessary from log
unique(buoy2022_wq_L1$flag_allexo)

buoy2022_wq_L1 <- buoy2022_wq_L1 %>% 
  mutate_at(vars(all_of(exo), all_of(exoinfo)),
            ~ case_when(flag_allexo != '' ~ NA_real_,# all flags should be recoded
                        TRUE ~ .))

# quick look at temp
ggplot(buoy2022_wq_L1, aes(x = datetime, y = EXOTempC)) +
  geom_point()
ggplot(buoy2022_wq_L1, aes(x = datetime, y = DOLowTempC)) +
  geom_point()

#recode record prior to buoy deploy
buoy2022_wq_L1 <- buoy2022_wq_L1 %>% 
  mutate_at(vars(all_of(exo), all_of(exoinfo)),
            ~ case_when(datetime < redeploy_exact ~ NA_real_,
                        TRUE ~ .))

buoyb_exo_vert <- buoy2022_wq_L1 %>% 
  select(datetime, flag_allexo, all_of(exo)) %>% 
  pivot_longer(names_to = 'variable',
               values_to = 'values', 
               -c(datetime,flag_allexo))

ggplot(buoyb_exo_vert, aes(x = datetime, y = values, shape = flag_allexo)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  final_theme

#plot exodepth to see if there are artifacts of maintenece
ggplot(buoy2022_wq_L1, aes(x = datetime, y = EXO_depth_m)) +
  geom_point()

buoyb_exo_vert <- buoy2022_wq_L1 %>% 
  select(datetime, flag_allexo, all_of(exo)) %>% 
  pivot_longer(names_to = 'variable',
               values_to = 'values', 
               -c(datetime,flag_allexo))

ggplot(buoyb_exo_vert, aes(x = datetime, y = values, shape = flag_allexo)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  final_theme

#end of record oct 17
oct17 <- as.POSIXct('2022-10-17 00:00', tz=buoy_tz)
buoyb_exo_vert %>% 
  filter(datetime >= oct17 &
           datetime < (oct17 +days(1))) %>% 
  ggplot(., aes(x = datetime, y = values, color = variable)) +
  geom_point() +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme

exo_eor <- as.POSIXct('2022-10-17 9:00', tz= buoy_tz)

buoy2022_wq_L1 <- buoy2022_wq_L1 %>% 
  mutate_at(vars(all_of(exo), all_of(exoinfo)),
            ~ case_when(datetime >= exo_eor ~ NA_real_,
                        TRUE ~ .))

buoyb_exo_vert <- buoy2022_wq_L1 %>% 
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
  chunk <- buoy2022_wq_L1 %>% 
    filter(datetime >= as.POSIXct(months[i], tz= buoy_tz) &
             datetime < (as.POSIXct(months[i], tz=buoy_tz) + dmonths(1)))
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
  ggsave(filename = paste0('graphs/2022/L0_exo_monthly_', months[i], '.jpg'), height = 8, width =10, units = 'in', dpi = 300)
}

# need to look closer at the times reported for Jun and Sept for cleaning, they didn't get caught in the script
jun22 = as.Date('2022-06-22')
buoyb_exo_vert %>% 
  filter(datetime >= jun22 &
           datetime < (jun22 +days(1))) %>% 
  ggplot(., aes(x = datetime, y = values, color = variable)) +
  geom_point() +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme
jun22 = '2022-06-22 8:40'

buoy2022_wq_L1 <- buoy2022_wq_L1 %>% 
  mutate_at(vars(all_of(exo), all_of(exoinfo)),
            ~ case_when(datetime == as.POSIXct(jun22, tz = buoy_tz) ~ NA_real_,
                        TRUE ~ .))

sept20 = as.Date('2022-09-20')
buoyb_exo_vert %>% 
  filter(datetime >= sept20 &
           datetime < (sept20 +days(1))) %>% 
  ggplot(., aes(x = datetime, y = values, color = variable)) +
  geom_point() +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme
sept20 = '2022-09-20 12:20:00'

buoy2022_wq_L1 <- buoy2022_wq_L1 %>% 
  mutate_at(vars(all_of(exo), all_of(exoinfo)),
            ~ case_when(datetime == as.POSIXct(sept20, tz = buoy_tz) ~ NA_real_,
                        TRUE ~ .))


#add clean and calibrate for do (calibration only pertinent to do)
buoy2022_wq_L1 <- buoy2022_wq_L1 %>% 
  mutate(flag_exodo = case_when(flag_allexo == 'c' ~ 'c', 
                                flag_allexo == 'w' ~ 'w',
                                TRUE ~ '')) 

#add calibration at beginning of season
buoy2022_wq_L1 <- buoy2022_wq_L1 %>% 
  mutate(flag_exodo = case_when(datetime == redeploy_exact ~ 'c', 
                                TRUE ~ flag_exodo))
                                

# flag algae for bdl;
buoy2022_wq_L1 <- buoy2022_wq_L1 %>% 
  mutate(flag_exobga = case_when(BGAPC_RFU < 0 ~ 'z',
                                    TRUE ~ ''),
         flag_exobga = case_when(flag_exobga == '' & datetime == redeploy_exact+minutes(10) ~ 'c',
                                    flag_exobga != '' & datetime == redeploy_exact+minutes(10)  ~ paste0('c; ', flag_exobga),
                                    flag_exobga == '' & flag_allexo == 'w' ~ 'w',
                                    flag_exobga != '' & flag_allexo == 'w' ~ paste0('w; ', flag_exobga),
                                    TRUE ~ flag_exobga))

# drop bgapc ugl
buoy2022_wq_L1$BGAPC_UGL = NA_real_

#recode chlor ugl to nareal
buoy2022_wq_L1$Chlor_UGL = NA_real_

buoyb_exo_vert <- buoy2022_wq_L1 %>% 
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

buoy2022_wq_L1$BGA_stdev = zoo::rollapply(buoy2022_wq_L1$BGAPC_RFU, width=3, sd, align='center', partial=F, fill=NA)
range(buoy2022_wq_L1$BGA_stdev, na.rm = T)

bga_sd = sd(buoy2022_wq_L1$BGAPC_RFU, na.rm = T)
bga_sd

#flag 3*SD
buoy2022_wq_L1 <- buoy2022_wq_L1 %>% 
  mutate(flag_exobga = case_when(flag_exobga == '' & BGA_stdev > 3*bga_sd ~ 's',
                                    flag_exobga != '' & BGA_stdev > 3*bga_sd ~ paste0('s; ', flag_exobga),
                                    TRUE ~ flag_exobga))

ggplot(buoy2022_wq_L1, aes(x = datetime, y = BGAPC_RFU, color = BGA_stdev, shape = flag_exobga)) +
  geom_point()

### chla ----

#calculate rolling average and rolling SD

buoy2022_wq_L1$chla_stdev = zoo::rollapply(buoy2022_wq_L1$Chlor_RFU, width=3, sd, align='center', partial=F, fill=NA)
range(buoy2022_wq_L1$chla_stdev, na.rm = T)

chla_sd = sd(buoy2022_wq_L1$Chlor_RFU, na.rm = T)
chla_sd

#flag 3*SD
buoy2022_wq_L1 <- buoy2022_wq_L1 %>% 
  mutate(flag_exochl = case_when(chla_stdev > 3*chla_sd ~ 's',
                                 TRUE ~ ''))


ggplot(buoy2022_wq_L1, aes(x = datetime, y = Chlor_RFU, color = chla_stdev, shape = flag_exochl)) +
  geom_point()


# plot monthly
for(i in 1:length(months)) {
  chunk <- buoy2022_wq_L1 %>% 
    filter(datetime >= as.POSIXct(months[i], tz= buoy_tz) &
             datetime < (as.POSIXct(months[i], tz=buoy_tz) + dmonths(1)))
  ggplot(chunk, aes(x = datetime, y = BGAPC_RFU, color = flag_exobga)) +
    geom_point() +
    scale_x_datetime(date_minor_breaks = '1 day') +
    final_theme +
    labs(x = NULL,
         y = NULL,
         title = paste0('LSPA exo BGA ', format(months[i], '%B')))
  ggsave(filename = paste0('graphs/2022/L0p5_exobga_monthly_', months[i], '.jpg'), height = 8, width =10, units = 'in', dpi = 300)
  ggplot(chunk, aes(x = datetime, y = Chlor_RFU, color = flag_exochl)) +
    geom_point() +
    scale_x_datetime(date_minor_breaks = '1 day') +
    final_theme +
    labs(x = NULL,
         y = NULL,
         title = paste0('LSPA exo chlorophyll ', format(months[i], '%B')))
  ggsave(filename = paste0('graphs/2022/L0p5_exochla_monthly_', months[i], '.jpg'), height = 8, width =10, units = 'in', dpi = 300)
}


#remove SD cols
buoy2022_wq_L1 <- buoy2022_wq_L1 %>% 
  select(-BGA_stdev, -chla_stdev)

# flag cleaning
buoy2022_wq_L1 <- buoy2022_wq_L1 %>% 
  mutate(flag_exochl = case_when(flag_exochl == '' & datetime == redeploy_exact+minutes(10)  ~ 'c',
                           flag_exochl != '' & datetime == redeploy_exact+minutes(10)  ~ paste0('c; ', flag_exochl),
                           flag_exochl == '' & flag_allexo == 'w' ~ 'w',
                           flag_exochl != '' & flag_allexo == 'w' ~ paste0('w; ', flag_exochl),
                           TRUE ~ flag_exochl))

unique(buoy2022_wq_L1$flag_exochl)



### fdom ----
buoyb_fdom_vert <- buoy2022_wq_L1 %>% 
  select(datetime, all_of(exofdom)) %>% 
  pivot_longer(names_to = 'variable',
               values_to = 'value', 
               -datetime)

ggplot(buoyb_fdom_vert, aes(x = datetime, y = value)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y')

# plot monthly
for(i in 1:length(months)) {
  chunk <- buoy2022_wq_L1 %>% 
    filter(datetime >= as.POSIXct(months[i], tz= buoy_tz) &
             datetime < (as.POSIXct(months[i], tz=buoy_tz) + dmonths(1)))
  ggplot(chunk, aes(x = datetime, y = fDOM_RFU)) +
    geom_point() +
    scale_x_datetime(date_minor_breaks = '1 day') +
    final_theme +
    labs(x = NULL,
         y = NULL,
         title = paste0('LSPA exo fDOM ', format(months[i], '%B')))
  ggsave(filename = paste0('graphs/2022/L0p5_exofdom_monthly_', months[i], '.jpg'), height = 8, width =10, units = 'in', dpi = 300)
}

buoy2022_wq_L1 <- buoy2022_wq_L1 %>% 
  mutate(flag_exofdom = case_when(datetime == redeploy_exact +minutes(10) ~ 'c',
                                  flag_allexo == 'w' ~ 'w',
                                    TRUE ~ ''))

# add calibration flag for TDS and cond
buoy2022_wq_L1 <- buoy2022_wq_L1 %>% 
  mutate(flag_exotds = case_when(datetime == redeploy_exact+minutes(10)  ~ 'c',
                                 flag_allexo == 'w' ~ 'w',
                                 TRUE ~ ''),
         flag_exocond = case_when(datetime == redeploy_exact+minutes(10)  ~ 'c',
                                  flag_allexo == 'w' ~ 'w',
                                  TRUE ~ ''))


### plot all data together monthly ----
buoyb_exo_vert <- buoy2022_wq_L1 %>% 
  select(datetime, flag_allexo, all_of(exo)) %>% 
  pivot_longer(names_to = 'variable',
               values_to = 'values', 
               -c(datetime,flag_allexo))

# plot monthly
for(i in 1:length(months)) {
  chunk <- buoy2022_wq_L1 %>% 
    filter(datetime >= as.POSIXct(months[i], tz= buoy_tz) &
             datetime < (as.POSIXct(months[i], tz=buoy_tz) + dmonths(1)))
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
  ggsave(filename = paste0('graphs/2022/L1_exo_monthly_', months[i], '.jpg'), height = 8, width =10, units = 'in', dpi = 300)
}

#clean up workspace
rm(buoyb_exo_vert, buoyb_fdom_vert, chunk, chunk_vert)

## DO ----
#low do is actually at 1m until move to loon
buoy2022_wq_L1 <- buoy2022_wq_L1 %>% 
  mutate(DOTempC_harbor = if_else(datetime < redeploy_exact, DOLowTempC, NA_real_),
         DOLowTempC = if_else(datetime > redeploy_exact, DOLowTempC, NA_real_),
         DOSat_harbor = if_else(datetime < redeploy_exact, DOLowSat, NA_real_),
         DOLowSat = if_else(datetime > redeploy_exact, DOLowSat, NA_real_),
         DOppm_harbor = if_else(datetime < redeploy_exact, DOLowPPM, NA_real_),
         DOLowPPM = if_else(datetime > redeploy_exact, DOLowPPM, NA_real_))

buoy_do_vert <- buoy2022_wq_L1 %>% 
  select(datetime, flag_do10m, all_of(lowDO)) %>% 
  pivot_longer(names_to = 'variable', 
               values_to =  'value', 
               -c(datetime,flag_do10m))

# recode where flagged
unique(buoy2022_wq_L1$flag_do10m) # all flags are out of water

#move low do to up do where flag == v
buoy2022_wq_L1 <- buoy2022_wq_L1 %>% 
  mutate(DOTempC_harbor = if_else(flag_do10m == 'v' & datetime > as.Date('2022-10-15'), 
                                  DOLowTempC,
                                  DOTempC_harbor),
         DOLowTempC = if_else(is.na(DOTempC_harbor), 
                             DOLowTempC,
                             NA_real_),
         DOSat_harbor = if_else(flag_do10m == 'v' & datetime > as.Date('2022-10-15'),
                                DOLowSat,
                                DOSat_harbor),
         DOLowSat = if_else(is.na(DOSat_harbor),
                           DOLowSat,
                           NA_real_),
         DOppm_harbor = if_else(flag_do10m == 'v' & datetime > as.Date('2022-10-17'),
                                DOLowPPM,
                                DOppm_harbor),
         DOLowPPM = if_else(is.na(DOppm_harbor),
                            DOLowPPM,
                            NA_real_))

buoy2022_wq_L1 <- buoy2022_wq_L1 %>% 
  mutate_at(vars(all_of(lowDO)),
            ~ case_when(flag_do10m != '' ~ NA_real_,
                        TRUE ~ .))

#add flag for calibration prior to deploy in April
buoy2022_wq_L1 <- buoy2022_wq_L1 %>% 
  mutate(flag_do10m = case_when(datetime == redeploy_exact +minutes(10) ~ 'c',
                                  TRUE ~ flag_do10m))

buoy_do_vert <- buoy2022_wq_L1 %>% 
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
    filter(datetime >= as.POSIXct(months[i], tz= buoy_tz) &
             datetime < (as.POSIXct(months[i], tz=buoy_tz) + dmonths(1)))
  ggplot(chunk, aes(x = datetime, y = value)) +
    geom_point() +
    facet_grid(variable ~ ., scales = 'free_y') +
    scale_x_datetime(date_minor_breaks = '1 day') +
    final_theme +
    labs(x = NULL,
         y = NULL,
         title = paste0('LSPA low DO ', format(months[i], '%B')))
  ggsave(filename = paste0('graphs/2022/L1_LowDO_monthly_', months[i], '.jpg'), height = 8, width =10, units = 'in', dpi = 300)
}

#clean up workspace
rm(buoy_do_vert, chunk)

## Upper DO in harbor ----
harbordo_vert <- buoy2022_wq_L1 %>% 
  select(datetime, DOSat_harbor, DOppm_harbor, DOTempC_harbor) %>% 
  pivot_longer(names_to = 'variable', values_to = 'value', -datetime)

ggplot(harbordo_vert, aes(x = datetime, y = value)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  scale_x_datetime(minor_breaks = '1 month')

# need to correct for beginning of record -- actually begins on the 25th
harbordo_vert %>% 
  filter(datetime > as.Date(redeploy_exact)-days(5) & 
           datetime < as.Date(redeploy_exact) + days(5)) %>% 
  ggplot(., aes(x = datetime, y = value)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  scale_x_datetime(minor_breaks = '1 hour')

harbordo_vert %>% 
  filter(datetime > as.Date(redeploy_exact)-days(1) & 
           datetime < as.Date(redeploy_exact)) %>% 
  ggplot(., aes(x = datetime, y = value)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  scale_x_datetime(minor_breaks = '1 hour')

buoy2022_wq_L1 <- buoy2022_wq_L1 %>% 
  mutate_at(vars(DOSat_harbor, DOppm_harbor, DOTempC_harbor),
            ~case_when(datetime >= as.POSIXct('2022-04-25 10:00', tz =buoy_tz) &
                         datetime < redeploy_exact ~ NA_real_,
                       TRUE ~ .))
#and move to harbor
harbordo_vert %>% 
  filter(datetime > as.POSIXct('2022-10-17', tz = buoy_tz) & 
           datetime < as.POSIXct('2022-10-18', tz = buoy_tz)) %>% 
  ggplot(., aes(x = datetime, y = value)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  scale_x_datetime(minor_breaks = '1 hour')

buoy2022_wq_L1 <- buoy2022_wq_L1 %>% 
  mutate_at(vars(DOSat_harbor, DOppm_harbor, DOTempC_harbor),
            ~case_when(datetime > as.POSIXct('2022-10-17 00:00', tz =buoy_tz) &
                         datetime < as.POSIXct('2022-10-17 14:00')~ NA_real_,
                       TRUE ~ .))
harbordo_vert <- buoy2022_wq_L1 %>% 
  select(datetime, DOSat_harbor, DOppm_harbor, DOTempC_harbor) %>% 
  pivot_longer(names_to = 'variable', values_to = 'value', -datetime)

# plot to check
harbordo_vert %>% 
  filter(datetime > as.POSIXct('2022-04-25', tz = buoy_tz) & datetime < as.POSIXct('2022-04-27', tz = buoy_tz)) %>% 
  ggplot(., aes(x = datetime, y = value)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  scale_x_datetime(minor_breaks = '1 hour')
harbordo_vert %>% 
  filter(datetime > as.POSIXct('2022-10-17', tz = buoy_tz) & datetime < as.POSIXct('2022-10-18', tz = buoy_tz)) %>% 
  ggplot(., aes(x = datetime, y = value)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  scale_x_datetime(minor_breaks = '1 hour')

ggplot(harbordo_vert, aes(x = datetime, y = value)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  scale_x_datetime(minor_breaks = '1 month')

#add location information
buoy2022_wq_L1 <- buoy2022_wq_L1 %>% 
  mutate(location = case_when(datetime >= exo_eor & datetime < as.POSIXct('2022-10-17 14:00', tz =buoy_tz) ~ 'in transit',
                              datetime >= as.POSIXct('2022-10-17 14:00', tz =buoy_tz) ~ 'harbor', 
                              TRUE ~ location))

#double check location
ggplot(buoy2022_wq_L1, aes(x = datetime, y = location)) +
  geom_point()


# 2022 Met data ----

# filter data from launch
buoy2022_met_L1 <- buoy2022_met_L1 %>% 
  filter(datetime >= redeploy_exact) 

# get location from wq data and apply to met
first_loon = first(buoy2022_wq_L1$datetime[buoy2022_wq_L1$location == 'loon'])
first_intransit = first(buoy2022_wq_L1$datetime[buoy2022_wq_L1$location == 'in transit'])
first_harbor = first(buoy2022_wq_L1$datetime[buoy2022_wq_L1$location == 'harbor'])

buoy2022_met_L1 <- buoy2022_met_L1 %>% 
  mutate(location = case_when(datetime >= first_loon & datetime < first_intransit ~ 'loon',
                              datetime >= first_intransit & datetime < first_harbor ~ 'in transit',
                              datetime >= first_harbor ~ 'harbor',
                              TRUE ~ ''))

# recode when in transit
buoy2022_met_L1 <- buoy2022_met_L1 %>% 
  mutate_at(all_of(met),
            ~ case_when(location == 'in transit' ~ NA_real_,
                        TRUE ~ .))

#look at flags
unique(buoy2022_met_L1$flag_allmet)

#only r needs to be recoded - this is when the rh chip was changed
buoy2022_met_L1 <- buoy2022_met_L1 %>% 
  mutate_at(all_of(met),
            ~ case_when(flag_allmet == 'r' ~ NA_real_,
                        TRUE ~ .))


#plotting all of the met data at same time takes too long - only plot by data group

## air temp and rel hum ----

buoy_temp_vert <- buoy2022_met_L1 %>% 
  select(datetime, all_of(air), location) %>% 
  pivot_longer(names_to = 'variable', 
               values_to =  'value', 
               -c(datetime, location))

# plot monthly
for(i in 1:length(months)) {
  chunk <- buoy_temp_vert %>% 
    filter(datetime >= as.POSIXct(months[i], tz= buoy_tz) &
             datetime < (as.POSIXct(months[i], tz=buoy_tz) + dmonths(1)))
  ggplot(chunk, aes(x = datetime, y = value, color = location)) +
    geom_point() +
    facet_grid(variable ~ ., scales = 'free_y') +
    scale_x_datetime(date_minor_breaks = '1 day') +
    final_theme +
    labs(x = NULL,
         y = NULL,
         title = paste0('LSPA buoy Air Temp RH ', format(months[i], '%B')))
  ggsave(filename = paste0('graphs/2022/L0_airRH_monthly_', months[i], '.jpg'), height = 8, width =10, units = 'in', dpi = 300)
}

#data look good

## wind ----
buoy_wind_vert <- buoy2022_met_L1 %>% 
  select(datetime, all_of(wind), location) %>% 
  pivot_longer(names_to = 'variable', 
               values_to =  'value', 
               -c(datetime, location))

# plot monthly
for(i in 1:length(months)) {
  chunk <- buoy_wind_vert %>% 
    filter(datetime >= as.POSIXct(months[i], tz= buoy_tz) &
             datetime < (as.POSIXct(months[i], tz=buoy_tz) + dmonths(1)))
  ggplot(chunk, aes(x = datetime, y = value, color = location)) +
    geom_point() +
    facet_grid(variable ~ ., scales = 'free_y') +
    scale_x_datetime(date_minor_breaks = '1 day') +
    final_theme +
    labs(x = NULL,
         y = NULL,
         title = paste0('LSPA buoy wind ', format(months[i], '%B')))
  ggsave(filename = paste0('graphs/2022/L0_wind_monthly_', months[i], '.jpg'), height = 8, width =10, units = 'in', dpi = 300)
}

#sensor frozen nov 26- nov29
buoy2022_met_L1 <- buoy2022_met_L1 %>% 
  mutate_at(all_of(wind),
            ~case_when(datetime >= as.POSIXct('2022-11-26', tz = buoy_tz) &
                         datetime < as.POSIXct('2022-11-30', tz = buoy_tz) &
                         MaxWindSp == 0 ~ NA_real_, 
                       TRUE ~ .))

#sensor frozen dec 25- dec29
buoy2022_met_L1 <- buoy2022_met_L1 %>% 
  mutate_at(all_of(wind),
            ~case_when(datetime >= as.POSIXct('2022-12-25', tz = buoy_tz) &
                         datetime < as.POSIXct('2022-12-30', tz = buoy_tz) &
                         MaxWindSp == 0 ~ NA_real_, 
                       TRUE ~ .))

buoy_wind_vert <- buoy2022_met_L1 %>% 
  select(datetime, all_of(wind), location) %>% 
  pivot_longer(names_to = 'variable', 
               values_to =  'value', 
               -c(datetime, location))

# plot monthly
for(i in 1:length(months)) {
  chunk <- buoy_wind_vert %>% 
    filter(datetime >= as.POSIXct(months[i], tz= buoy_tz) &
             datetime < (as.POSIXct(months[i], tz=buoy_tz) + dmonths(1)))
  ggplot(chunk, aes(x = datetime, y = value, color = location)) +
    geom_point() +
    facet_grid(variable ~ ., scales = 'free_y') +
    scale_x_datetime(date_minor_breaks = '1 day') +
    final_theme +
    labs(x = NULL,
         y = NULL,
         title = paste0('LSPA buoy wind ', format(months[i], '%B')))
  ggsave(filename = paste0('graphs/2022/L1_wind_monthly_', months[i], '.jpg'), height = 8, width =10, units = 'in', dpi = 300)
}

## PAR ----
range(buoy2022_met_L1$PAR_ave_umolpspm2, na.rm = T)
range(buoy2022_met_L1$PAR_tot_mmolpm2, na.rm = T)

buoy_par_vert <- buoy2022_met_L1 %>% 
  select(datetime, all_of(par2022), location) %>% 
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
    filter(datetime >= as.POSIXct(months[i], tz= buoy_tz) &
             datetime < (as.POSIXct(months[i], tz=buoy_tz) + dmonths(1)))
  ggplot(chunk, aes(x = datetime, y = value, color = location)) +
    geom_point() +
    facet_grid(variable ~ ., scales = 'free_y') +
    scale_x_datetime(date_minor_breaks = '1 day') +
    final_theme +
    labs(x = NULL,
         y = NULL,
         title = paste0('LSPA buoy PAR ', format(months[i], '%B')))
  ggsave(filename = paste0('graphs/2022/L0_PAR_monthly_', months[i], '.jpg'), height = 8, width =10, units = 'in', dpi = 300)
}

#looks like sensor obscured end of nov and end-mid dec

#nov 27- nov 30
#dec 25- 28

buoy2022_met_L1 <- buoy2022_met_L1 %>% 
  mutate(flag_par = case_when(datetime >= as.POSIXct('2022-11-27', tz =buoy_tz) &
                                datetime <= as.POSIXct('2022-12-01', tz = buoy_tz) ~ 'o', 
                              datetime >= as.POSIXct('2022-12-25', tz =buoy_tz) &
                                datetime <= as.POSIXct('2022-12-29', tz = buoy_tz) ~ 'o', 
                              TRUE ~ ''))

# still have par >0 at night issue. flagging all PAR data 
buoy2022_met_L1 <- buoy2022_met_L1 %>% 
  mutate(flag_par = case_when(flag_par == '' ~ 'n',
                              TRUE ~ paste0('n; ', flag_par)))

buoy_par_vert <- buoy2022_met_L1 %>% 
  select(datetime, all_of(par2022), location, flag_par) %>% 
  pivot_longer(names_to = 'variable', 
               values_to =  'value', 
               -c(datetime, location, flag_par))

# plot monthly
for(i in 1:length(months)) {
  chunk <- buoy_par_vert %>% 
    filter(datetime >= as.POSIXct(months[i], tz= buoy_tz) &
             datetime < (as.POSIXct(months[i], tz=buoy_tz) + dmonths(1)))
  ggplot(chunk, aes(x = datetime, y = value, color = flag_par, shape = location)) +
    geom_point() +
    facet_grid(variable ~ ., scales = 'free_y') +
    scale_x_datetime(date_minor_breaks = '1 day') +
    final_theme +
    labs(x = NULL,
         y = NULL,
         title = paste0('LSPA buoy PAR ', format(months[i], '%B')))
  ggsave(filename = paste0('graphs/2022/L1_PAR_monthly_', months[i], '.jpg'), height = 8, width =10, units = 'in', dpi = 300)
}

# Convert to CV ----
colnames(buoy2022_wq_L1)
buoy2022_wq_L1 <- buoy2022_wq_L1 %>% 
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

colnames(buoy2022_met_L1)
buoy2022_met_L1 <- buoy2022_met_L1 %>% 
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
colnames(buoy2022_wq_L1)


#export L1 tempstring file
unique(buoy2022_wq_L1$flag_alltemp) #these are fine to drop
buoy2022_wq_L1 %>%
  select(datetime, waterTemperature_degC_0p1m:waterTemperature_degC_10m, location) %>%
  mutate(datetime = as.character(datetime)) %>%
  write_csv(., file.path(dump_dir, 'tempstring/2022_tempstring_L1_v2022.csv'))

#export l1 exo file
unique(buoy2022_wq_L1$flag_exochl)
unique(buoy2022_wq_L1$flag_exobga)
unique(buoy2022_wq_L1$flag_exofdom)
unique(buoy2022_wq_L1$flag_exocond)

buoy2022_wq_L1 %>%
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
  write_csv(., file.path(dump_dir, 'exo/2022_exo_L1_v2022.csv'))

#export l1 do file
unique(buoy2022_wq_L1$flag_do10m)
#recode v to '', not applicable to data
buoy2022_wq_L1 <- buoy2022_wq_L1 %>% 
  mutate(flag_do10m = case_when(flag_do10m == 'v' ~ '',
                                TRUE ~ flag_do10m))
buoy2022_wq_L1 %>%
  select(datetime, 
         waterTemperature_DO_degC_0p75m:oxygenDissolved_mgl_0p75m,
         waterTemperature_DO_degC_10m:oxygenDissolved_mgl_10m,
         flag_do10m, 
         location) %>%
  mutate(datetime = as.character(datetime)) %>%
  write_csv(., file.path(dump_dir, 'do/2022_do_L1_v2022.csv'))


## Met Data ----
colnames(buoy2022_met_L1)

unique(buoy2022_met_L1$flag_allmet)
unique(buoy2022_met_L1$flag_par)

# apply the 'r' flag from all met to rh
buoy2022_met_L1 <- buoy2022_met_L1 %>% 
  mutate(flag_rh = case_when(flag_allmet == 'r' ~ 'r',
                             TRUE ~ ''))

#export l1 met file
buoy2022_met_L1 %>%
  select(datetime, 
         airTemperature_degC, relativeHumidity_perc, flag_rh,
         radiationIncomingPARAverage_mmolm2s, radiationIncomingPARTotal_mmolm2, flag_par,
         winddirectionInstantaneous_deg:windDirectionAverage_deg,
         location) %>%
  mutate(datetime = as.character(datetime)) %>%
  write_csv(., file.path(dump_dir, 'met/2022b_met_L1_v2022.csv'))
