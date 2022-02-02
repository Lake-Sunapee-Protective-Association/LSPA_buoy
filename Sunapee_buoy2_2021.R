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
data_dir = 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L0/2021/'
log_dir = 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/operation notes/'

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
         date = as.Date(datetime)) %>% 
  filter(datetime < as.Date('2022-01-01')) %>% 
  rownames_to_column('rowid_wq') %>% 
  select(-index)

#remove the EXO date and time columns
buoy2021b_wq_L1 <- buoy2021b_wq_L1 %>% 
  select(-EXO_date, -EXO_time)

# record does not extend to DST, so no need to check.
#double check total number of observations for each day (should only be 144 or less to confirm no DST)
datetimetable <- buoy2021b_wq_L1 %>%
  group_by(date) %>%
  summarize(n = length(datetime))
#looks good
#double check DST dates
buoy2021b_wq_L1[buoy2021b_wq_L1$date == as.Date('2021-11-07'),]$datetime
#look at date with 145 records
buoy2021b_wq_L1[buoy2021b_wq_L1$date == as.Date('2021-12-06'),]$datetime
#duplicate is 2021-12-06 14:10:00, check record to see if identical
dupedate <- buoy2021b_wq_L1[buoy2021b_wq_L1$date == as.Date('2021-12-06'),]
buoy2021b_wq_L1 <- buoy2021b_wq_L1[buoy2021b_wq_L1$rowid_wq != 27532,] #remove the dupe row


### met ----
buoy2021b_met_L1 <- buoy2021b_met_L0 %>% 
  mutate(datetime = as.POSIXct(datetime),
         datetime = force_tz(datetime, tz = buoyb_tz),
         date = as.Date(datetime)) %>% 
  filter(datetime < as.Date('2022-01-01')) %>% 
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
  mutate(TIMESTAMP_end = case_when(is.na(TIMESTAMP_end) ~ as.POSIXct('2022-01-01', tz = buoyb_tz),
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
                          grepl('RH', notes, ignore.case = T) ~ 'r',
                          TRUE ~ 'o'))  # sensor out of water


# LOOK AT BATTERY ----

ggplot(buoy2021b_wq_L1, aes(x = datetime, y = LoggerBatV)) +
  geom_point()
ggplot(buoy2021b_wq_L1, aes(x = datetime, y = RadioBatV)) +
  geom_point()
ggplot(buoy2021b_wq_L1, aes(x = datetime, y = EXO_batt_V)) +
  geom_point()


# 2021b wq ----

#add flags from log
#initiate columns
buoy2021b_wq_L1$flag_exo = ''
buoy2021b_wq_L1$flag_do = ''
buoy2021b_wq_L1$flag_temp = ''
buoy2021b_wq_L1$flag_met = ''

for(l in 1:nrow(logb)){
  if (logb$instrument[l] == 'EXO') {
  buoy2021b_wq_L1 <- buoy2021b_wq_L1 %>% 
    mutate(flag_exo = case_when(datetime >= logb$TIMESTAMP_start[l] &
                                  datetime <= logb$TIMESTAMP_end[l] ~ logb$flag[l],
                                TRUE ~ flag_exo))
  } else if (logb$instrument[l] == 'do' | logb$instrument[l] == 'DO') {
    buoy2021b_wq_L1 <- buoy2021b_wq_L1 %>% 
      mutate(flag_do = case_when(datetime >= logb$TIMESTAMP_start[l] &
                                   datetime <= logb$TIMESTAMP_end[l] ~ logb$flag[l], 
                                 TRUE ~ flag_do))
  } else if (logb$instrument[l] == 'temp') {
    buoy2021b_wq_L1 <- buoy2021b_wq_L1 %>% 
      mutate(flag_temp = case_when(datetime >= logb$TIMESTAMP_start[l] &
                                     datetime <= logb$TIMESTAMP_end[l] ~ logb$flag[l],
                                   TRUE ~ flag_temp))
  } else if (logb$data_table[l] == 'met') {
    buoy2021b_wq_L1 <- buoy2021b_wq_L1 %>% 
      mutate(flag_met = case_when(datetime >= logb$TIMESTAMP_start[l] &
                                     datetime <= logb$TIMESTAMP_end[l] ~ logb$flag[l],
                                   TRUE ~ flag_met))
  } else {
  }
}

## thermistors ----

#recode as necessary from log
unique(buoy2021b_wq_L1$flag_temp)
buoy2021b_wq_L1 <- buoy2021b_wq_L1 %>% 
  mutate_at(vars(all_of(alltemp)),
            ~ case_when(flag_temp == 'o' ~ NA_real_, 
                        flag_temp == 'R' ~ NA_real_, #all flags indicate that sensors were out of the water
                        TRUE ~ .))

buoyb_therm_vert <- buoy2021b_wq_L1 %>% 
  select(datetime, flag_temp, all_of(alltemp)) %>% 
  pivot_longer(names_to = 'variable',
               values_to = 'values', 
               -c(datetime,flag_temp))

#recode record prior to buoy deploy
redeploy = as.POSIXct('2021-06-07', tz = buoyb_tz)

buoy2021b_wq_L1 <- buoy2021b_wq_L1 %>% 
  mutate_at(vars(all_of(alltemp)),
            ~ case_when(datetime <= redeploy ~ NA_real_,
                        TRUE ~ .))

buoyb_therm_vert <- buoy2021b_wq_L1 %>% 
  select(datetime, flag_temp, all_of(alltemp)) %>% 
  pivot_longer(names_to = 'variable',
               values_to = 'values', 
               -c(datetime,flag_temp))

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

buoy2021b_wq_L1 <- buoy2021b_wq_L1 %>% 
  mutate_at(vars(all_of(alltemp)),
            ~ case_when(datetime <= redeploy_exact ~ NA_real_,
                        TRUE ~ .)) %>% 
  mutate(location = case_when(datetime > redeploy_exact ~ 'loon',
                              TRUE ~ 'offline'))

buoyb_therm_vert <- buoy2021b_wq_L1 %>% 
  select(datetime, flag_temp, all_of(alltemp)) %>% 
  pivot_longer(names_to = 'variable',
               values_to = 'values', 
               -c(datetime,flag_temp))

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
unique(buoy2021b_wq_L1$flag_exo)
buoy2021b_wq_L1 <- buoy2021b_wq_L1 %>% 
  mutate_at(vars(all_of(exo), all_of(exoinfo)),
            ~ case_when(flag_exo != '' ~ NA_real_,# all flags should be recoded
                        TRUE ~ .))

#recode record prior to buoy deploy
buoy2021b_wq_L1 <- buoy2021b_wq_L1 %>% 
  mutate_at(vars(all_of(exo), all_of(exoinfo)),
            ~ case_when(datetime < redeploy_exact ~ NA_real_,
                        TRUE ~ .))

buoyb_exo_vert <- buoy2021b_wq_L1 %>% 
  select(datetime, flag_exo, all_of(exo)) %>% 
  pivot_longer(names_to = 'variable',
               values_to = 'values', 
               -c(datetime,flag_exo))

ggplot(buoyb_exo_vert, aes(x = datetime, y = values, shape = flag_exo)) +
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
  select(datetime, flag_exo, all_of(exo)) %>% 
  pivot_longer(names_to = 'variable',
               values_to = 'values', 
               -c(datetime,flag_exo))

ggplot(buoyb_exo_vert, aes(x = datetime, y = values, shape = flag_exo)) +
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
  select(datetime, flag_exo, all_of(exo)) %>% 
  pivot_longer(names_to = 'variable',
               values_to = 'values', 
               -c(datetime,flag_exo))

ggplot(buoyb_exo_vert, aes(x = datetime, y = values, shape = flag_exo)) +
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

#flag do prior to 2021-07-20 10:20:00 as likely miscalibrated
miscal_date <- as.POSIXct('2021-07-20 10:20:00', tz=buoyb_tz)

buoy2021b_wq_L1 <- buoy2021b_wq_L1 %>% 
  mutate(flag_exodo = case_when(datetime < miscal_date & !is.na(DOppm) ~ 'm',
                                flag_exo == 'c' ~ 'c', 
                                flag_exo == 'w' ~ 'w',
                                TRUE ~ '')) 

# flag algae for bdl
buoy2021b_wq_L1 <- buoy2021b_wq_L1 %>% 
  mutate(flag_bga_rfu = case_when(BGAPC_RFU < 0 ~ 'z',
                              TRUE ~ ''),
         flag_bga_ugl = case_when(BGAPC_UGL < 0 ~ 'z',
                                  TRUE ~ '')) 

# recode all of BGAPC_UGL, all are negative values
buoy2021b_wq_L1 <- buoy2021b_wq_L1 %>% 
  mutate(BGAPC_UGL = case_when(BGAPC_UGL < 0 ~ 0,
                              TRUE ~ BGAPC_UGL),
         BGAPC_RFU = case_when(BGAPC_RFU < 0 ~ 0,
                               TRUE ~ BGAPC_RFU)) 

buoyb_exo_vert <- buoy2021b_wq_L1 %>% 
  select(datetime, flag_exo, all_of(exo)) %>% 
  pivot_longer(names_to = 'variable',
               values_to = 'values', 
               -c(datetime,flag_exo))

ggplot(buoyb_exo_vert, aes(x = datetime, y = values)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  final_theme

### BGA ----

#calculate rolling average and rolling SD

buoy2021b_wq_L1$BGA_stdev = zoo::rollapply(buoy2021b_wq_L1$BGAPC_RFU, width=3, sd, align='center', partial=F, fill=NA)
range(buoy2021b_wq_L1$BGA_stdev, na.rm = T)

range(buoy2021b_wq_L1$BGAPC_RFU, na.rm = T)

#range of data isn't even 2*SD, so only flag the few SD outliers as 'suspect'

ggplot(buoy2021b_wq_L1, aes(x = datetime, y = BGAPC_RFU, color = BGA_stdev)) +
  geom_point()

buoy2021b_wq_L1 <- buoy2021b_wq_L1 %>% 
  mutate(flag_bga_rfu = case_when(flag_bga_rfu == '' & BGA_stdev > 0.06 & BGAPC_RFU > 0.1 ~ 's',
                                  flag_bga_rfu != '' & BGA_stdev > 0.06 & BGAPC_RFU > 0.1 ~ paste0(flag_bga_rfu, ', s'),
                                 TRUE ~ flag_bga_rfu))

### chla ----

#calculate rolling average and rolling SD

buoy2021b_wq_L1$chla_stdev = zoo::rollapply(buoy2021b_wq_L1$Chlor_RFU, width=3, sd, align='center', partial=F, fill=NA)
range(buoy2021b_wq_L1$chla_stdev, na.rm = T)

chla_sd = sd(buoy2021b_wq_L1$Chlor_RFU, na.rm = T)
chla_sd

ggplot(buoy2021b_wq_L1, aes(x = datetime, y = Chlor_RFU, color = chla_stdev)) +
  geom_point()

#recode > 4SD, flag

buoy2021b_wq_L1 <- buoy2021b_wq_L1 %>% 
  mutate(flag_chla_rfu = case_when(chla_stdev > 4*chla_sd & Chlor_RFU > 2 ~ 'x',
                               TRUE ~ '')) %>% 
  mutate_at(vars(Chlor_RFU, Chlor_UGL),
            ~ case_when(chla_stdev > 4*chla_sd & Chlor_RFU > 2 ~ NA_real_,
                               TRUE ~ .))

#check again after outlier removal
ggplot(buoy2021b_wq_L1, aes(x = datetime, y = Chlor_RFU, color = flag_chla_rfu)) +
  geom_point()

buoy2021b_wq_L1$chla_stdev = zoo::rollapply(buoy2021b_wq_L1$Chlor_RFU, width=3, sd, align='center', partial=F, fill=NA)
range(buoy2021b_wq_L1$chla_stdev, na.rm = T)

chla_sd = sd(buoy2021b_wq_L1$Chlor_RFU, na.rm = T)

ggplot(buoy2021b_wq_L1, aes(x = datetime, y = chla_stdev)) +
  geom_point()

#flag for fouling between cleanings 09-20 and 08-16; huge jump in chla and bga when cleaed on 9-20
buoy2021b_wq_L1 <- buoy2021b_wq_L1 %>% 
  mutate_at(vars(flag_chla, flag_bga),
            ~ case_when(datetime >= as.POSIXct('2021-08-16 11:10:00', tz=buoyb_tz) &
                                 datetime <= as.POSIXct('2021-09-20 10:40:00', tz = buoyb_tz) &
                                 . == '' ~ 'f',
                               datetime >= as.POSIXct('2021-08-16 11:10:00', tz=buoyb_tz) &
                                 datetime <= as.POSIXct('2021-09-20 10:40:00', tz = buoyb_tz) &
                                 . != '' ~ paste0(., ', f'),
                               TRUE ~ .))

# plot monthly
for(i in 1:length(months)) {
  chunk <- buoy2021b_wq_L1 %>% 
    filter(datetime >= as.POSIXct(months[i], tz= buoyb_tz) &
             datetime < (as.POSIXct(months[i], tz=buoyb_tz) + dmonths(1)))
  ggplot(chunk, aes(x = datetime, y = BGAPC_RFU, color = flag_bga)) +
    geom_point() +
    scale_x_datetime(date_minor_breaks = '1 day') +
    final_theme +
    labs(x = NULL,
         y = NULL,
         title = paste0('LSPA exo BGA ', format(months[i], '%B')))
  ggsave(filename = paste0('graphs/2021/L0p5_exobga_monthly_', months[i], '.jpg'), height = 8, width =10, units = 'in', dpi = 300)
  ggplot(chunk, aes(x = datetime, y = Chlor_RFU, color = flag_chla)) +
    geom_point() +
    scale_x_datetime(date_minor_breaks = '1 day') +
    final_theme +
    labs(x = NULL,
         y = NULL,
         title = paste0('LSPA exo chlorophyll ', format(months[i], '%B')))
  ggsave(filename = paste0('graphs/2021/L0p5_exochla_monthly_', months[i], '.jpg'), height = 8, width =10, units = 'in', dpi = 300)
}

#remove SD cols
buoy2021b_wq_L1 <- buoy2021b_wq_L1 %>% 
  select(-BGA_stdev, -chla_stdev)

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

### plot all data together monthly ----
buoyb_exo_vert <- buoy2021b_wq_L1 %>% 
  select(datetime, flag_exo, all_of(exo)) %>% 
  pivot_longer(names_to = 'variable',
               values_to = 'values', 
               -c(datetime,flag_exo))

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
  select(datetime, flag_do, all_of(lowDO)) %>% 
  pivot_longer(names_to = 'variable', 
               values_to =  'value', 
               -c(datetime,flag_do))

# recode where flagged
unique(buoy2021b_wq_L1$flag_do) # all flags are out of water

#move low do to up do where flag == v
buoy2021b_wq_L1 <- buoy2021b_wq_L1 %>% 
  mutate(DOTempC = case_when(flag_do == 'v' & datetime > as.Date('2021-10-15') ~ DOLowTempC,
                             TRUE ~ NA_real_),
         DOSat = case_when(flag_do == 'v' & datetime > as.Date('2021-10-15') ~ DOLowSat,
                           TRUE ~ NA_real_),
         DOppm = case_when(flag_do == 'v' & datetime > as.Date('2021-10-15') ~ DOLowPPM,
                   TRUE ~ NA_real_))

buoy2021b_wq_L1 <- buoy2021b_wq_L1 %>% 
  mutate_at(vars(all_of(lowDO)),
            ~ case_when(flag_do != '' ~ NA_real_,
                        TRUE ~ .))

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
  select(datetime, flag_do, all_of(lowDO)) %>% 
  pivot_longer(names_to = 'variable', 
               values_to =  'value', 
               -c(datetime,flag_do))

ggplot(buoy_do_vert, aes(x = datetime, y = value, color = flag_do))+
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

## Upper DO ----

# will qaqc this next round, in mean time truncate entire record at buoy move

buoy2021b_wq_L1 <- buoy2021b_wq_L1 %>% 
  filter(datetime <= exo_eor)


# 2021 Met data ----

# recode before launch and add loction
buoy2021b_met_L1 <- buoy2021b_met_L1 %>% 
  mutate_at(all_of(met),
            ~case_when(datetime <= redeploy_exact ~ NA_real_,
                       TRUE ~ .)) %>% 
  mutate(location = case_when(datetime > redeploy_exact ~ 'loon',
                              TRUE ~ 'offline'))

#truncate at end of exo record
buoy2021b_met_L1 <- buoy2021b_met_L1 %>% 
  filter(datetime <= exo_eor)

buoy_met_vert <- buoy2021b_met_L1 %>% 
  select(datetime, all_of(met)) %>% 
  pivot_longer(names_to = 'variable', 
               values_to =  'value', 
               -c(datetime))

ggplot(buoy_met_vert, aes(x = datetime, y = value))+
  geom_point()+
  facet_grid(variable ~., scales = 'free_y')

# plot monthly
for(i in 1:length(months)) {
  chunk <- buoy_met_vert %>% 
    filter(datetime >= as.POSIXct(months[i], tz= buoyb_tz) &
             datetime < (as.POSIXct(months[i], tz=buoyb_tz) + dmonths(1)))
  ggplot(chunk, aes(x = datetime, y = value)) +
    geom_point() +
    facet_grid(variable ~ ., scales = 'free_y') +
    scale_x_datetime(date_minor_breaks = '1 day') +
    final_theme +
    labs(x = NULL,
         y = NULL,
         title = paste0('LSPA buoy met ', format(months[i], '%B')))
  ggsave(filename = paste0('graphs/2021/L0_met_monthly_', months[i], '.jpg'), height = 8, width =10, units = 'in', dpi = 300)
}

## air temp and rel hum ----

buoy_temp_vert <- buoy2021b_met_L1 %>% 
  select(datetime, all_of(air)) %>% 
  pivot_longer(names_to = 'variable', 
               values_to =  'value', 
               -c(datetime))

# plot monthly
for(i in 1:length(months)) {
  chunk <- buoy_temp_vert %>% 
    filter(datetime >= as.POSIXct(months[i], tz= buoyb_tz) &
             datetime < (as.POSIXct(months[i], tz=buoyb_tz) + dmonths(1)))
  ggplot(chunk, aes(x = datetime, y = value)) +
    geom_point() +
    facet_grid(variable ~ ., scales = 'free_y') +
    scale_x_datetime(date_minor_breaks = '1 day') +
    final_theme +
    labs(x = NULL,
         y = NULL,
         title = paste0('LSPA buoy Air Temp RH ', format(months[i], '%B')))
  ggsave(filename = paste0('graphs/2021/L0_airRH_monthly_', months[i], '.jpg'), height = 8, width =10, units = 'in', dpi = 300)
}

## wind ----

buoy_wind_vert <- buoy2021b_met_L1 %>% 
  select(datetime, all_of(wind)) %>% 
  pivot_longer(names_to = 'variable', 
               values_to =  'value', 
               -c(datetime))

# plot monthly
for(i in 1:length(months)) {
  chunk <- buoy_wind_vert %>% 
    filter(datetime >= as.POSIXct(months[i], tz= buoyb_tz) &
             datetime < (as.POSIXct(months[i], tz=buoyb_tz) + dmonths(1)))
  ggplot(chunk, aes(x = datetime, y = value)) +
    geom_point() +
    facet_grid(variable ~ ., scales = 'free_y') +
    scale_x_datetime(date_minor_breaks = '1 day') +
    final_theme +
    labs(x = NULL,
         y = NULL,
         title = paste0('LSPA buoy wind ', format(months[i], '%B')))
  ggsave(filename = paste0('graphs/2021/L0_wind_monthly_', months[i], '.jpg'), height = 8, width =10, units = 'in', dpi = 300)
}


## PAR ----

buoy_par_vert <- buoy2021b_met_L1 %>% 
  select(datetime, all_of(par2021)) %>% 
  pivot_longer(names_to = 'variable', 
               values_to =  'value', 
               -c(datetime))

# plot monthly
for(i in 1:length(months)) {
  chunk <- buoy_par_vert %>% 
    filter(datetime >= as.POSIXct(months[i], tz= buoyb_tz) &
             datetime < (as.POSIXct(months[i], tz=buoyb_tz) + dmonths(1)))
  ggplot(chunk, aes(x = datetime, y = value)) +
    geom_point() +
    facet_grid(variable ~ ., scales = 'free_y') +
    scale_x_datetime(date_minor_breaks = '1 day') +
    final_theme +
    labs(x = NULL,
         y = NULL,
         title = paste0('LSPA buoy PAR ', format(months[i], '%B')))
  ggsave(filename = paste0('graphs/2021/L0_PAR_monthly_', months[i], '.jpg'), height = 8, width =10, units = 'in', dpi = 300)
}

# still have par >0 at night issue. flagging all PAR data 

buoy2021b_met_L1 <- buoy2021b_met_L1 %>% 
  mutate(flag_PAR = case_when(!is.na(PAR_tot_mmolpm2) ~ 'n',
                              TRUE ~ ''))



# EXPORT L1 DATA STREAMS ----

## Water Quality data ----
colnames(buoy2021b_wq_L1)

#export L1 tempstring file
unique(buoy2021b_wq_L1$flag_temp)
buoy2021b_wq_L1 %>%
  select(datetime, all_of(all_temp), location, flag_temp) %>%
  mutate(datetime = as.character(datetime)) %>%
  write_csv(., 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/tempstring/2021_tempstring_L1_corrdepths.csv')

#export l1 exo file
unique(buoy2021b_wq_L1$flag_exo)
unique(buoy2021b_wq_L1$flag_chla)
unique(buoy2021b_wq_L1$flag_bga)
buoy2021b_wq_L1 %>%
  select(datetime, all_of(exoinfo), all_of(exo), flag_exo, flag_chla, flag_bga, location) %>%
  mutate(datetime = as.character(datetime)) %>%
  write_csv(., 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/do/2021_do_L1.csv')

#export l1 do file
buoy2021b_wq_L1 %>%
  select(datetime, all_of(upDO), all_of(lowDO), upper_do_flag, lower_do_flag, location) %>%
  mutate(datetime = as.character(datetime)) %>%
  write_csv(., 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/do/2021_do_L1.csv')


## Met Data ----
colnames(buoy2021b_met_L1)

#export l1 par file
buoy2021_L1 %>%
  select(datetime, all_of(par2021), PAR_flag, location) %>%
  mutate(datetime = as.character(datetime)) %>%
  write_csv(., 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/met/2021_PAR_L1.csv')

#export l1 wind
buoy2021_L1 %>%
  select(datetime, all_of(wind2021), location) %>%
  mutate(datetime = as.character(datetime)) %>%
  write_csv(., 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/met/2021_wind_L1.csv')

#export l1 air temp file
buoy2021_L1 %>%
  select(datetime, AirTempC, location) %>%
  mutate(datetime = as.character(datetime)) %>%
  write_csv(., 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/met/2021_airtemp_L1.csv')

