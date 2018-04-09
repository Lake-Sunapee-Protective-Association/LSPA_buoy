#*****************************************************************
#*      Cary Institute of Ecosystem Studies (Millbrook, NY)      *
#*                                                               *
#* TITLE:   Sunapee_DO_Temp_L0_L1.r                              *
#* AUTHOR:  Bethel Steele                                        *
#* SYSTEM:  Lenovo W530, Win 7, R 3.2.2                          *
#* DATE:    14Oct2016                                            *
#* PROJECT: Lake Sunapee DataOne Submission                      *
#* PURPOSE: collate Level 0 and Level 1 data from thermisters    *
#*          and DO sensor from Sunapee Buoy for submission to    *
#*          DataOne                                              *
#* LAST UPDATED: 17Jan2017                                       *
#* BY: B. Steele                                                 *
#*****************************************************************

library(gdata)

#### Level One Data ####
varnames1 <- c('DateTime', 'AirTemp_degC', 'DatalogBat_V', 'DOConc_1m_per', 'DOConc_1m_mgL', 
               'DOSat_1m_per', 'PAR_uEsecm2', 'RadioBat_V', 'RelHum_per', 'DOSensorTemp_1m_degC', 
               'Temp_0m_degC', 'Temp_0p5m_degC', 'Temp_1m_degC', 'Temp_1p5m_degC', 'Temp_2m_degC', 
               'Temp_2p5m_degC', 'Temp_3m_degC', 'Temp_4m_degC', 'Temp_5m_degC', 'Temp_6m_degC',
               'Temp_7m_degC', 'Temp_8m_degC', 'Temp_9m_degC', 'Temp_10m_degC', 'Temp_11m_degC',
               'Temp_13m_degC', 'WindDir_deg', 'WindDir_WindVector_deg', 'WindSp_WindVector_msec', 'WindSp_msec')

varnames2 <- c('DateTime', 'AirTemp_degC', 'DOConc_1m_mgL', 'DOConc_1m_per', 
               'DOSat_1m_per', 'PAR_uEsecm2', 'DOSensorTemp_1m_degC', 
               'Temp_0m_degC', 'Temp_0p5m_degC', 'Temp_1m_degC', 'Temp_1p5m_degC', 'Temp_2m_degC', 
               'Temp_2p5m_degC', 'Temp_3m_degC', 'Temp_4m_degC', 'Temp_5m_degC', 'Temp_6m_degC',
               'Temp_7m_degC', 'Temp_8m_degC', 'Temp_9m_degC', 'Temp_10m_degC', 'Temp_11m_degC',
               'Temp_13m_degC', 'WindDir_WindVector_deg', 'WindDir_deg', 'WindSp_msec', 'WindSp_WindVector_msec')

varnames3 <- c('DateTime', 'AirTemp_degC', 'DOConc_1m_mgL', 'DOConc_1m_per', 
               'DOSat_1m_per', 'PAR_uEsecm2', 'DOSensorTemp_1m_degC', 
               'Temp_0m_degC', 'Temp_0p5m_degC', 'Temp_1m_degC', 'Temp_1p5m_degC', 'Temp_2m_degC', 
               'Temp_2p5m_degC', 'Temp_3m_degC', 'Temp_4m_degC', 'Temp_5m_degC', 'Temp_6m_degC',
               'Temp_7m_degC', 'Temp_8m_degC', 'Temp_9m_degC', 'Temp_10m_degC', 'Temp_11m_degC',
               'Temp_13m_degC', 'WindDir_WindVector_deg', 'WindDir_deg', 'WindSp_msec', 'WindSp_WindVector_msec')

varnames4 <- c('DateTime', 'AirTemp_degC', 'DOConc_1m_per', 'DOConc_1m_mgL', 
               'DOSat_1m_per', 'PAR_uEsecm2', 'RelHum_per', 'DOSensorTemp_1m_degC', 
               'Temp_0m_degC', 'Temp_0p5m_degC', 'Temp_1m_degC', 'Temp_1p5m_degC', 'Temp_2m_degC', 
               'Temp_2p5m_degC', 'Temp_3m_degC', 'Temp_4m_degC', 'Temp_5m_degC', 'Temp_6m_degC',
               'Temp_7m_degC', 'Temp_8m_degC', 'Temp_9m_degC', 'Temp_10m_degC', 'Temp_11m_degC',
               'Temp_13m_degC', 'WindDir_deg', 'WindDir_WindVector_deg', 'WindSp_WindVector_msec', 'WindSp_msec')

varnames5 <- c("Station", "Year", "Day", "hr_min", "DOSensorTemp_1m_degC",      
               "DOConc_1m_per", "DOConc_1m_mgL", "Temp_1m_degC", "Temp_2m_degC", "Temp_3m_degC",       
               "Temp_4m_degC", "Temp_5m_degC", "Temp_6m_degC", "Temp_7m_degC", "Temp_8m_degC",       
               "Temp_9m_degC", "Temp_10m_degC", "AirTemp_degC", "RelHum_per", "PAR_uEsecm2",         
               "WindSp_msec", "WindDir_deg", 'WindSp_Ave_msec', "WindDir_WindVector_deg", "WindSp_Max_msec",    
               "WindDir_Max_deg", "DatalogBat_V", "RadioBat_V", "LoggerTemp_degC", "BuoyHeading_deg",     
               "unk_1", "DOSensorTemp_10m_degC", "DOConc_10m_per", "DOConc_10m_mgL", "Date",        
               "Time", "DateTime", "dateTimeDiff", "Gap")


setwd('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/L0 07-13 temp do from CCC and DR')
L0_buoy_dotemp_07 <- read.csv('Sunapee2007_rawData.csv', col.names = varnames1) 
L0_buoy_dotemp_07$DateTime <- as.POSIXct(strptime(L0_buoy_dotemp_07$DateTime, '%Y-%m-%d %H:%M:%S'))
L0_buoy_dotemp_07 <- subset(L0_buoy_dotemp_07, subset=(DateTime < '2008-01-01 00:00:00'))

L0_buoy_dotemp_08 <- read.csv('Sunapee2008_rawData.csv', col.names=varnames1) 
L0_buoy_dotemp_08$DateTime <- as.POSIXct(strptime(L0_buoy_dotemp_08$DateTime, '%Y-%m-%d %H:%M:%S'))

L0_buoy_dotemp_09 <- read.csv('Sunapee2009_rawData.csv', col.names = varnames2) 
L0_buoy_dotemp_09$DateTime <- as.POSIXct(strptime(L0_buoy_dotemp_09$DateTime, '%Y-%m-%d %H:%M:%S'))

L0_buoy_dotemp_10 <- read.csv('Sunapee2010_rawData.csv', col.names = varnames3) 
L0_buoy_dotemp_10$DateTime <- as.POSIXct(strptime(L0_buoy_dotemp_10$DateTime, '%Y-%m-%d %H:%M:%S'))

L0_buoy_dotemp_11 <- read.csv('Sunapee2011_rawData.csv', col.names = varnames3) 
L0_buoy_dotemp_11$DateTime <- as.POSIXct(strptime(L0_buoy_dotemp_11$DateTime, '%Y-%m-%d %H:%M:%S'))

L0_buoy_dotemp_12 <- read.csv('Sunapee2012_rawData.csv', col.names = varnames4) 
L0_buoy_dotemp_12$DateTime <- as.POSIXct(strptime(L0_buoy_dotemp_12$DateTime, '%m/%d/%Y %H:%M'))

L0_buoy_dotemp_13 <- read.csv('Sunapee2013_rawData.csv', col.names = varnames5) 
L0_buoy_dotemp_13$DateTime <- as.POSIXct(strptime(L0_buoy_dotemp_13$DateTime, '%Y-%m-%d %H:%M'))
L0_buoy_dotemp_13 <- subset(L0_buoy_dotemp_13, select=c("Station", "Year", "Day", "hr_min", "DOSensorTemp_1m_degC",      
                                                        "DOConc_1m_per", "DOConc_1m_mgL", "Temp_1m_degC", "Temp_2m_degC", "Temp_3m_degC",       
                                                        "Temp_4m_degC", "Temp_5m_degC", "Temp_6m_degC", "Temp_7m_degC", "Temp_8m_degC",       
                                                        "Temp_9m_degC", "Temp_10m_degC", "AirTemp_degC", "RelHum_per", "PAR_uEsecm2",         
                                                        "WindSp_msec", "WindDir_deg", 'WindSp_Ave_msec', "WindDir_WindVector_deg", "WindSp_Max_msec",    
                                                        "WindDir_Max_deg", "DatalogBat_V", "RadioBat_V", "LoggerTemp_degC", "BuoyHeading_deg",     
                                                        "DOSensorTemp_10m_degC", "DOConc_10m_per", "DOConc_10m_mgL"))


#merge L0 buoy data
L0_buoy_all <- merge(L0_buoy_dotemp_07, L0_buoy_dotemp_08, all=T)
L0_buoy_all <- merge(L0_buoy_all, L0_buoy_dotemp_09, all=T)
L0_buoy_all <- merge(L0_buoy_all, L0_buoy_dotemp_10, all=T)
L0_buoy_all <- merge(L0_buoy_all, L0_buoy_dotemp_11, all=T)
L0_buoy_all <- merge(L0_buoy_all, L0_buoy_dotemp_12, all=T)
L0_buoy_all <- merge(L0_buoy_all, L0_buoy_dotemp_13, all=T)

#reorganize columns
L0_buoy_all <- subset(L0_buoy_all, select=c("DateTime", "Station", "Year", "Day", "hr_min",
                                            "AirTemp_degC", "PAR_uEsecm2", "RelHum_per", "WindDir_deg","WindDir_WindVector_deg",
                                            "WindSp_msec", "WindSp_WindVector_msec", "BuoyHeading_deg", "WindSp_Ave_msec", 'WindSp_Max_msec', 
                                            "WindDir_Max_deg", "DOConc_1m_mgL", "DOConc_1m_per", "DOSat_1m_per", 
                                            "DOSensorTemp_1m_degC", "DOConc_10m_mgL", "DOConc_10m_per", "DOSensorTemp_10m_degC", "Temp_0m_degC", 
                                            "Temp_0p5m_degC", "Temp_1m_degC", "Temp_1p5m_degC", "Temp_2m_degC", "Temp_2p5m_degC", 
                                            "Temp_3m_degC", "Temp_4m_degC", "Temp_5m_degC", "Temp_6m_degC", "Temp_7m_degC", 
                                            "Temp_8m_degC", "Temp_9m_degC", "Temp_10m_degC", "Temp_11m_degC", "Temp_13m_degC", 
                                            "RadioBat_V", "DatalogBat_V", "LoggerTemp_degC"))


L0_buoy_all <- L0_buoy_all[with(L0_buoy_all, order(DateTime, Year, Day, hr_min)), ]

write.csv(L0_buoy_all, 'L0_buoy_comp_18Jan2017.csv', row.names = F)





####Level 1/2 data ####
varnames6 <- c('DateTime', 'PAR_umolm2sec', 'Wind_sp_ms', 'DOSensorTemp_1m_degC', 'DO_mgL_1m', 
               'Temp_0m_degC', 'Temp_0p5m_degC', 'Temp_1m_degC', 'Temp_1p5m_degC', 'Temp_2m_degC', 
               'Temp_2p5_degC', 'Temp_3m_degC', 'Temp_3p5m', 'Temp_4m_degC', 'Temp_5m_degC', 
               'Temp_6m_degC', 'Temp_7m_degC', 'Temp_8m_degC', 'Temp_9m_degC', 'Temp_10m_degC', 
               'Temp_11m_degC', 'Temp_12m_degC', 'Temp_13m_degC', 'Temp_14m_degC', 'AirTemp_degC', 
               'WindDir_deg')
varnames7 <- c('DateTime', 'PAR_umolm2sec', 'Wind_sp_ms', 'DOSensorTemp_1m_degC', 'DO_mgL_1m', 
               'Temp_1m_degC', 'Temp_1p5m_degC', 'Temp_2m_degC', 'Temp_4m_degC', 'Temp_5m_degC', 
               'Temp_6m_degC', 'Temp_7m_degC', 'Temp_8m_degC', 'Temp_9m_degC', 'Temp_10m_degC', 
               'Temp_11m_degC', 'Temp_12m_degC', 'Temp_14m_degC', 'AirTemp_degC', 'WindDir_deg')

varnames8 <- c('DateTime', 'PAR_umolm2sec', 'Wind_sp_ms', 'DOSensorTemp_1m_degC', 'DO_mgL_1m', 
               'Temp_0m_degC', 'Temp_0p5m_degC', 'Temp_1m_degC', 'Temp_1p5m_degC', 'Temp_2m_degC', 
               'Temp_2p5_degC', 'Temp_3m_degC', 'Temp_4m_degC', 'Temp_5m_degC', 'Temp_6m_degC', 
               'Temp_7m_degC', 'Temp_8m_degC', 'Temp_9m_degC', 'Temp_10m_degC', 'Temp_11m_degC', 
               'Temp_13m_degC', 'AirTemp_degC', 'WindDir_deg')

varnames9 <- c('DateTime', 'PAR_umolm2sec', 'Wind_sp_ms', 'DOSensorTemp_1m_degC', 'DO_mgL_1m', 
               'Temp_0p5m_degC', 'Temp_1m_degC', 'Temp_1p5m_degC', 'Temp_2m_degC', 'Temp_2p5_degC', 
               'Temp_3m_degC', 'Temp_4m_degC', 'Temp_5m_degC', 'Temp_6m_degC', 'Temp_7m_degC', 
               'Temp_8m_degC', 'Temp_9m_degC', 'Temp_10m_degC', 'Temp_11m_degC', 'Temp_13m_degC', 
               'AirTemp_degC', 'WindDir_deg')

varnames10 <- c('DateTime', 'PAR_umolm2sec', 'Wind_sp_ms', 'DOSensorTemp_1m_degC', 'DO_mgL_1m', 
               'Temp_0m_degC', 'Temp_1m_degC', 'Temp_2m_degC', 'Temp_3m_degC', 'Temp_4m_degC', 
               'Temp_5m_degC', 'Temp_6m_degC', 'Temp_7m_degC', 'Temp_8m_degC', 'Temp_9m_degC', 
               'AirTemp_degC', 'WindDir_deg')

varnames11 <- c('DateTime', 'PAR_umolm2sec', 'Wind_sp_ms', 'DOSensorTemp_1m_degC', 'DO_mgL_1m', 
                'Temp_1m_degC', 'Temp_2m_degC', 'Temp_3m_degC', 'Temp_4m_degC', 'Temp_5m_degC', 
                'Temp_6m_degC', 'Temp_7m_degC', 'Temp_8m_degC', 'Temp_9m_degC', 'Temp_10m_degC', 
                'AirTemp_degC', 'WindDir_deg')

setwd('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/L1 07-13 temp do from CCC and DR')
L1_buoy_dotemp_07 <- read.csv('Sunapee_2007-LSPA-Export.csv', col.names = varnames6) 
L1_buoy_dotemp_08 <- read.csv('Sunapee_2008-LSPA-Export.csv', col.names = varnames7) 
L1_buoy_dotemp_09 <- read.csv('Sunapee_2009-LSPA-Export.csv', col.names = varnames8) 
L1_buoy_dotemp_10 <- read.csv('Sunapee_2010-LSPA-Export.csv', col.names = varnames9) 
L1_buoy_dotemp_11 <- read.csv('Sunapee_2011-LSPA-Export.csv', col.names = varnames10) 
L1_buoy_dotemp_12 <- read.csv('Sunapee_2012-LSPA-Export.csv', col.names = varnames10) 
L1_buoy_dotemp_13 <- read.csv('Sunapee_2013-LSPA-Export.csv', col.names = varnames11) 

# merge data
L1_buoy_all <- merge(L1_buoy_dotemp_07, L1_buoy_dotemp_08, all=T)
L1_buoy_all <- merge(L1_buoy_all, L1_buoy_dotemp_09, all=T)
L1_buoy_all <- merge(L1_buoy_all, L1_buoy_dotemp_10, all=T)
L1_buoy_all <- merge(L1_buoy_all, L1_buoy_dotemp_11, all=T)
L1_buoy_all <- merge(L1_buoy_all, L1_buoy_dotemp_12, all=T)
L1_buoy_all <- merge(L1_buoy_all, L1_buoy_dotemp_13, all=T)

#format date
L1_buoy_all$DateTime <- as.POSIXct(strptime(L1_buoy_all$DateTime, '%Y-%m-%d %H:%M:%S'))

#replace DO mg/L data with NA for invalid dates (20May - 15Oct)
L1_buoy_all$DO_mgL_1m [L1_buoy_all$DateTime > '2007-10-15 23:59:59' & L1_buoy_all$DateTime < '2008-05-20 00:00:00'] = NA
L1_buoy_all$DO_mgL_1m [L1_buoy_all$DateTime > '2008-10-15 23:59:59' & L1_buoy_all$DateTime < '2009-05-20 00:00:00'] = NA
L1_buoy_all$DO_mgL_1m [L1_buoy_all$DateTime > '2009-10-15 23:59:59' & L1_buoy_all$DateTime < '2010-05-20 00:00:00'] = NA
L1_buoy_all$DO_mgL_1m [L1_buoy_all$DateTime > '2010-10-15 23:59:59' & L1_buoy_all$DateTime < '2011-05-20 00:00:00'] = NA
L1_buoy_all$DO_mgL_1m [L1_buoy_all$DateTime > '2011-10-15 23:59:59' & L1_buoy_all$DateTime < '2012-05-20 00:00:00'] = NA
L1_buoy_all$DO_mgL_1m [L1_buoy_all$DateTime > '2012-10-15 23:59:59' & L1_buoy_all$DateTime < '2013-05-20 00:00:00'] = NA
L1_buoy_all$DO_mgL_1m [L1_buoy_all$DateTime > '2012-10-15 23:59:59'] = NA

#reorganize columns
L1_buoy_all <- subset(L1_buoy_all, select=c("DateTime", "PAR_umolm2sec", "AirTemp_degC", "WindDir_deg", "Wind_sp_ms", 
                                            "DOSensorTemp_1m_degC", "DO_mgL_1m", "Temp_0m_degC", "Temp_0p5m_degC", "Temp_1m_degC", 
                                            "Temp_1p5m_degC", "Temp_2m_degC", "Temp_2p5_degC", "Temp_3m_degC", "Temp_3p5m", 
                                            "Temp_4m_degC", "Temp_5m_degC", "Temp_6m_degC", "Temp_7m_degC", "Temp_8m_degC", 
                                            "Temp_9m_degC", "Temp_10m_degC", "Temp_11m_degC", "Temp_12m_degC", "Temp_13m_degC", 
                                            "Temp_14m_degC"))

L1_buoy_all <- L1_buoy_all[with(L1_buoy_all, order(DateTime)), ]

write.csv(L1_buoy_all, 'L1_buoy_comp_24Oct16.csv', row.names = F)

