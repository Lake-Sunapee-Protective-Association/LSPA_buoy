#*****************************************************************
#*      Cary Institute of Ecosystem Studies (Millbrook, NY)      *
#*                      Weathers Lab                             *
#*                                                               *
#* TITLE:   library_func_lists.R                                 *
#* PROJECT: SunapeeBuoy.RProj                                    *
#* AUTHOR:  Bethel Steele                                        *
#* SYSTEM:  Lenovo ThinkCentre, Win 10, R 3.6.1, RStudio 1.2.5001*
#* DATE:    01Apr2020                                            *
#* PURPOSE: QAQC and collate buoy data                           *
#*****************************************************************

library(tidyverse)
library(ggthemes)
library(readxl)
library(lubridate)

# ##functions
# substrRight <- function(x, n){
#   substr(x, nchar(x)-n+1, nchar(x))
# }

##ggplot grom
final_theme=theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        plot.title=element_text(size=16, face='bold', hjust=0.5))

#create column name lists
buoy2021 = c('datetime','index', 'LoggerBatV', 'LoggerTempC', 'RadioBatV',
             "TempC_0m", "TempC_1m", "TempC_2m", "TempC_3m", "TempC_4m", 
             "TempC_5m", "TempC_6m", "TempC_7m", "TempC_8m", "TempC_9m", 
             "TempC_10m",
             'EXO_date', 'EXO_time', 'EXOTempC', 'Cond', 'SpecCond', 'TDS_mgl', 
             'DOSat', 'DOppm','Chlor_RFU', 'Chlor_UGL', 'BGAPC_RFU', 'BGAPC_UGL',
             'fDOM_RFU', 'fDOM_QSU', 'EXO_pres_psia', 'EXO_depth_m', 'EXO_batt_V', 
             'EXO_cablepower_V', 'EXO_wiper_V',
             "DOLowTempC", "DOLowSat", "DOLowPPM")
met2021 = c('datetime', 'index', 'LoggerBatV', 'LoggerTempC', 'RadioBatV',
            'PAR_umolpspm2', 'PAR_mmolpm2', 'AirTempC', 'RelHum',
            'heading_deg', 'WindDir', 'AveWindSp','MaxWindDir', 'MaxWindSp',
            'AveWindSp2', 'AveWindDir', 'STD_winddir')

##create lists of sensors for data cleaning
buoyinfo <- c('datetime','index', 'LoggerBatV', 'LoggerTempC', 'RadioBatV')  
metinfo <- c('datetime', 'index', 'LoggerBatV', 'LoggerTempC', 'RadioBatV')

alltemp <- c("TempC_0m", "TempC_1m", "TempC_2m", "TempC_3m", "TempC_4m", "TempC_5m", "TempC_6m", "TempC_7m", "TempC_8m", "TempC_9m", "TempC_10m")
alltemp2011 <- c("TempC_0m", "TempC_1m", "TempC_2m", "TempC_3m", "TempC_4m", "TempC_5m", "TempC_6m", "TempC_7m", "TempC_8m", "TempC_9m")
alltemp2007 <- c("TempC_0m", "TempC_0p5m", "TempC_1m", "TempC_1p5m", "TempC_2m", "TempC_2p5m", "TempC_3m", "TempC_4m", "TempC_5m", "TempC_6m", "TempC_7m", "TempC_8m", "TempC_9m", "TempC_10m", "TempC_11m", "TempC_13m")
alltemp2011.5 <- c("TempC_0p5m", "TempC_1p5m", "TempC_2p5m", "TempC_3p5m", "TempC_4p5m", "TempC_5p5m", "TempC_6p5m", "TempC_7p5m", "TempC_8p5m", 
                   "TempC_9p5m")
alltemp2019 <-   c('TempC_0p5m', 'TempC_0p85m', 
                   'TempC_1p5m', 'TempC_1p85m', 
                   'TempC_2p5m', 'TempC_2p85m', 
                   'TempC_3p5m', 'TempC_3p85m', 
                   'TempC_4p5m', 'TempC_4p85m', 
                   'TempC_5p5m', 'TempC_5p85m', 
                   'TempC_6p5m', 'TempC_6p85m', 
                   'TempC_7p5m', 'TempC_7p85m', 
                   'TempC_8p5m', 'TempC_8p85m', 
                   'TempC_9p5m', 'TempC_9p85m', 
                   'TempC_10p5m', 'TempC_11p5m', 
                   'TempC_13p5m') 

upDO <- c("DOTempC", "DOSat", "DOppm")
lowDO <- c("DOLowTempC", "DOLowSat", "DOLowPPM")
chla <- c('Chlor_RFU', 'Chlor_UGL', 'SpecCond')

exo <- c('EXOTempC', 'Cond', 'SpecCond', 'TDS_mgl', 
         'DOSat', 'DOppm','Chlor_RFU', 'Chlor_UGL', 'BGAPC_RFU', 'BGAPC_UGL',
         'fDOM_RFU', 'fDOM_QSU')
exoinfo <- c('EXO_date', 'EXO_time','EXO_pres_psia', 'EXO_depth_m', 'EXO_batt_V', 
             'EXO_cablepower_V', 'EXO_wiper_V')
cond <- c('Cond', 'SpecCond')
exodo <- c('DOSat', 'DOppm')
exoalgae <- c('Chlor_RFU', 'Chlor_UGL', 'BGAPC_RFU', 'BGAPC_UGL')
exofdom <- c('fDOM_RFU', 'fDOM_QSU')
exochla <- c('Chlor_RFU', 'Chlor_UGL')
exobga <- c('BGAPC_RFU', 'BGAPC_UGL')

air <-  c('AirTempC', 'RelHum')
wind <- c('AveWindDir', 'AveWindSp', 'MaxWindDir', 'MaxWindSp')
wind2021 <- c('WindDir', 'AveWindSp','MaxWindDir', 'MaxWindSp', 'AveWindDir')
par2021 <- c('PAR_umolpspm2', 'PAR_mmolpm2')

hobotemp <- c('TempC_1m_hobo', 'TempC_2m_hobo', 'TempC_3m_hobo', 'TempC_4m_hobo', 'TempC_5m_hobo', 'TempC_6m_hobo', 'TempC_7m_hobo', 'TempC_8m_hobo', 'TempC_9m_hobo')
hobotemp_17 <- c("TempC_1m", "TempC_2m", "TempC_3m", "TempC_4m", "TempC_5m", "TempC_6m", "TempC_8m", "TempC_9m", "TempC_10m")
