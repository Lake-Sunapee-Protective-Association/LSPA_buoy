source('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/programs/library_func_lists.R')

#### HOBO DO ####

hobo_tz = 'America/New_York'
hobo_updo <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L0/2018_Summer_U26_DO.csv',
                      skip = 2,
                      col_types = c('icnnccccc'),
                      col_names = c('x', 'datetime', 'do_mgl', 'temp_degC', 'x2', 'x3', 'x4', 'x5', 'x6')) %>% 
  select(datetime, do_mgl, temp_degC) %>% 
  mutate(datetime = as.POSIXct(datetime, tz=hobo_tz, format = '%m/%d/%y %I:%M:%S %p'))

hobo_updo_vert <- hobo_updo %>% 
  gather(variable, value, -datetime)

# ggplot(hobo_updo_vert,
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 month') +
#   labs(title = 'do 2018') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(hobo_updo_vert,
#               (subset = datetime >= as.POSIXct('2018-10-01', tz=hobo_tz))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   labs(title = 'do 2018') +
#   final_theme +
#   scale_color_colorblind()
# 
# #sensor removed oct 12
# ggplot(subset(hobo_updo_vert,
#               (subset = datetime >= as.POSIXct('2018-10-12', tz=hobo_tz))),
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   labs(title = 'do 2018') +
#   final_theme +
#   scale_color_colorblind()

hobo_updo_L1 <- hobo_updo %>% 
  mutate_at(vars(do_mgl, temp_degC),
            funs(case_when(datetime >= as.POSIXct('2018-10-12 10:30', tz=hobo_tz) ~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate(hobodo_flag = case_when(datetime == as.POSIXct('2018-05-23 12:00:00', tz=hobo_tz) ~ 'c',
                               TRUE ~ ''))

hobo_updo_vert_L1 <- hobo_updo_L1 %>% 
  gather(variable, value, -datetime, -hobodo_flag)

# ggplot(hobo_updo_vert_L1,
#        aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable ~ ., scales = 'free_y') +
#   scale_x_datetime(date_minor_breaks = '1 month') +
#   labs(title = 'do 2018') +
#   final_theme +
#   scale_color_colorblind()

# add CV
colnames(hobo_updo_L1)
hobo_updo_L1 <- hobo_updo_L1 %>% 
  rename(datetime.local = datetime,
         oxygenDissolved_mgl_0p25m = do_mgl,
         waterTemperature_degC_0p25m = temp_degC,
         flag_do0p25m = hobodo_flag)

#convert time to UTC-5
hobo_updo_L1 <- hobo_updo_L1 %>% 
  mutate(datetime = with_tz(datetime.local, tz = 'Etc/GMT+5'))

#export l1 hobo do file
hobo_updo_L1 %>%
  select(-datetime.local) %>% 
  mutate(datetime = as.character(datetime)) %>%
  mutate(location = 'loon') %>% 
  write_csv(., 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/do/2018_hobodo_L1_v2022.csv')
  