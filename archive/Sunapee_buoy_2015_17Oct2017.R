#*****************************************************************
#*      Cary Institute of Ecosystem Studies (Millbrook, NY)      *
#*                                                               *
#* TITLE:   Sunapee_buoy_2015_11Oct2017.r                        *
#* AUTHOR:  Bethel Steele                                        *
#* SYSTEM:  Lenovo ThinkCentre, Win 10, R 3.4.2, RStudio 1.1.383 *
#* DATE:    11Oct2017                                            *
#* PROJECT: Lake Sunapee Buoy Data Cleaning                      *
#* PURPOSE: create L0 and L1 data for buoy data 2015 using       *
#*          similar methods to CCC and DR                        *
#* UPDATED: 17Oct2017                                            *
#* UPDATES: include new LMP file from LSPA                       *
#* PREVIOUS VERSION: 'Sunapee_buoy_2015_11Oct2017.R'             *
#*                   'Sunapee_buoy_2014-2016_07Aug2017.R'        *
#*****************************************************************

library(gdata)
library(ggplot2)
library(ggthemes)
library(reshape2)

#bring in 2015 buoy raw data
setwd('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L0')
buoy2015 <- read.csv('2015 Buoy Data.csv')
hobo2015 <- read.csv('2015 Summer Hobo.csv')

#bring in 2017 LMP data for comparison
setwd('C:/Users/steeleb/Dropbox/Lake Sunapee/long term Sunapee data/raw data/LMP files/LMP 2017')
LMP2015 <- read.xls('DO.xlsx', sheet='DO', header=T)
#format date
LMP2015$DATE <- as.Date(LMP2015$DATE, format='%d-%b-%y')
#subset for 2015 data only
LMP2015 <- subset(LMP2015, subset=(DATE>='2015-01-01' & DATE<'2016-01-01'))
#subset for 210 only (closest to buoy)
LMP2015 <- subset(LMP2015, subset=STATION==210)

##functions
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

final_theme=theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        plot.title=element_text(size=16, face='bold', hjust=0.5))

#create lists
alltemp <- c("TempC_0m", "TempC_1m", "TempC_2m", "TempC_3m", "TempC_4m", "TempC_5m", "TempC_6m", "TempC_7m", "TempC_8m", "TempC_9m", "TempC_10m")
upDO <- c("DOTempC", "DOSat", "DOppm")
lowDO <- c("DOLoTempC", "DOLowSat", "DOLowPPM")
allwatersensors <- c("TempC_0m", "TempC_1m", "TempC_2m", "TempC_3m", "TempC_4m", "TempC_5m", "TempC_6m", "TempC_7m", "TempC_8m", "TempC_9m", "TempC_10m", 
                     "DOTempC", "DOSat", "DOppm", "DOLoTempC", "DOLowSat", "DOLowPPM")
allmet <- c("AirTempC", "RelHum", "PAR", "WindSpd", "CorrWind", "WindSpdAv", "WindVect", "MaxWind", "MaxWindDir")
wind <- c('WindSpdAv', 'WindVect', 'MaxWind', 'MaxWindDir', 'CorrWind', 'WindSpd')

#### format data ####
buoy2015$minutes <- substrRight(buoy2015$Hr.Min, 2)
ix=which(buoy2015$minutes=='0')
buoy2015$minutes[ix]='00'

for (i in 1:nrow(buoy2015)) {
  if(nchar(buoy2015$Hr.Min[i]) ==4) { #if the number of characters is 4
    buoy2015$hour [i] <- substr(buoy2015$Hr.Min [i], 1,2) #then select the first two characters
    } else #assign it to the column
      if(nchar(buoy2015$Hr.Min [i]) ==3) { #if the number of characters is 3
      buoy2015$hour [i] <- substr(buoy2015$Hr.Min [i], 1, 1) #then select the first character
      } else #assign it to the column
        if(nchar(buoy2015$Hr.Min [i]) <2) { #if the number of characters is 2
        buoy2015$hour [i] <- '00'} #assign 00 to the column
}

buoy2015$time <- paste(buoy2015$hour, buoy2015$minutes, sep=':')

buoy2015$date <- as.Date(paste(buoy2015$Day, buoy2015$Year, sep = '-'), '%j-%Y')
buoy2015$datetime <- as.POSIXct(paste(buoy2015$date, buoy2015$time, sep=' '), format='%Y-%m-%d %H:%M', tz='GMT') #must state GMT so that daylight savings doesn't get messy

str(buoy2015)

#fix times in LMP database
ix=which(LMP2015$TIME==99)
LMP2015$TIME[ix]=909
ix=which(LMP2015$TIME==98)
LMP2015$TIME[ix]=908

# format time in LMP data
LMP2015$minutes <- substrRight(LMP2015$TIME, 2)


for (i in 1:nrow(LMP2015)) {
  if(is.na(LMP2015$TIME[i])) {
    LMP2015$hour[i] <-NA
  } else
  if(nchar(LMP2015$TIME[i])==4) { #if the number of characters is 4
    LMP2015$hour [i] <- substr(LMP2015$TIME [i], 1,2) #then select the first two characters
  } 
  else #assign it to the column
    if(nchar(LMP2015$TIME [i]) ==3) { #if the number of characters is 3
      LMP2015$hour [i] <- substr(LMP2015$TIME [i], 1, 1) #then select the first character
    } else is.na(LMP2015$hour)
}

#fix time stamp to 24 hour clock for hours <3
LMP2015$hour <- as.numeric(LMP2015$hour)
ix=which(LMP2015$hour<=3)
LMP2015$hour[ix] = LMP2015$hour[ix]+12

#create time from hour minutes
LMP2015$time <- paste(LMP2015$hour, LMP2015$minutes, sep=':')

#assign time of 9:30 to september readings (assumed)
ix=which(LMP2015$DATE=='2015-09-21')
LMP2015$time[ix]='9:30'

#create datetime stamp from date and time
LMP2015$datetime <- as.POSIXct(paste(LMP2015$DATE, LMP2015$time, sep=' '), format='%Y-%m-%d %H:%M', tz='GMT') #must state GMT so that daylight savings doesn't get messy

str(LMP2015)

#subset LMP for truthing data
LMP2015_temp <- subset(LMP2015, select=c('datetime', 'DEPTH', 'TEMP'))
LMP2015_temp <- subset(LMP2015_temp, subset=(DEPTH<=12))
LMP2015_temp <- rename.vars(LMP2015_temp, from=c('DEPTH', 'TEMP'), to=c('depth', 'value'))
LMP2015_temp$source <- 'LMP'
  
LMP2015_upDO <- subset(LMP2015, select=c('datetime', 'DEPTH', 'DO', 'PCNTSAT'))
LMP2015_upDO <- subset(LMP2015_upDO, subset=(DEPTH<=2))
LMP2015_upDO <- rename.vars(LMP2015_upDO, from=c('DEPTH', 'DO', 'PCNTSAT'), to=c('depth', 'DOppm', 'DOSat'))
LMP2015_upDO <- melt(LMP2015_upDO, id=c('datetime', 'depth'))
LMP2015_upDO$source <- 'LMP'

LMP2015_lowDO <- subset(LMP2015, select=c('datetime', 'DEPTH', 'DO', 'PCNTSAT'))
LMP2015_lowDO <- subset(LMP2015_lowDO, subset=(DEPTH<=12 & DEPTH>=9))
LMP2015_lowDO <- rename.vars(LMP2015_lowDO, from=c('DEPTH', 'DO', 'PCNTSAT'), to=c('depth', 'DOLowPPM', 'DOLowSat'))
LMP2015_lowDO <- melt(LMP2015_lowDO, id=c('datetime', 'depth'))
LMP2015_lowDO$source <- 'LMP'


buoy2015_L1 <- buoy2015

#### plot Thermisters ####
##transform data to vertical for display
buoy2015_temp <- subset(buoy2015_L1, select=c('datetime', "TempC_0m", "TempC_1m", "TempC_2m", "TempC_3m", "TempC_4m", "TempC_5m", "TempC_6m", "TempC_7m", "TempC_8m", "TempC_9m", "TempC_10m"))
buoy2015_vert_temp <- melt(buoy2015_temp, id='datetime')

# ggplot(subset(buoy2015_vert_temp, subset=(datetime>='2015-01-01' & datetime < '2016-01-01')), aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='2015, raw', x='date', y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +   
#   final_theme
# 
# ggplot(subset(buoy2015_vert_temp, subset=(datetime>='2015-06-01' & datetime < '2015-06-16')), aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='early June 2015', x='date', y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +   
#   final_theme
# 
# ggplot(subset(buoy2015_vert_temp, subset=(datetime>='2015-06-16' & datetime < '2015-07-01')), aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='late June 2015', x='date', y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +   
#   final_theme
# 
# ggplot(subset(buoy2015_vert_temp, subset=(datetime>='2015-07-01' & datetime < '2015-07-16')), aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='early July 2015', x='date', y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +   
#   final_theme
# 
# ggplot(subset(buoy2015_vert_temp, subset=(datetime>='2015-07-16' & datetime < '2015-08-01')), aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='late July 2015', x='date', y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +   
#   final_theme

#temp line removed Jul 22 13:00

####2015 thermisters - remove/replace NA values ####
#print range of each variable and address obvious issues#
range(buoy2015_L1$TempC_0m, na.rm = T)
ix=which(buoy2015_L1$TempC_0m==-6999)
buoy2015_L1$TempC_0m[ix]=NA
ix=which(buoy2015_L1$TempC_0m==1215)
buoy2015_L1$TempC_0m[ix]=NA
ix=which(buoy2015_L1$TempC_0m==555.4)
buoy2015_L1$TempC_0m[ix]=NA

range(buoy2015_L1$TempC_1m, na.rm = T)
ix=which(buoy2015_L1$TempC_1m==555.4)
buoy2015_L1$TempC_1m[ix]=NA
ix=which(buoy2015_L1$TempC_1m==0)
buoy2015_L1$TempC_1m[ix]=NA

range(buoy2015_L1$TempC_2m, na.rm = T)
ix=which(buoy2015_L1$TempC_2m==555.4)
buoy2015_L1$TempC_2m[ix]=NA
ix=which(buoy2015_L1$TempC_2m==0)
buoy2015_L1$TempC_2m[ix]=NA

range(buoy2015_L1$TempC_3m, na.rm = T)
ix=which(buoy2015_L1$TempC_3m==555.4)
buoy2015_L1$TempC_3m[ix]=NA
ix=which(buoy2015_L1$TempC_3m==0)
buoy2015_L1$TempC_3m[ix]=NA

range(buoy2015_L1$TempC_4m, na.rm = T)
ix=which(buoy2015_L1$TempC_4m==555.4)
buoy2015_L1$TempC_4m[ix]=NA
ix=which(buoy2015_L1$TempC_4m==0)
buoy2015_L1$TempC_4m[ix]=NA

range(buoy2015_L1$TempC_5m, na.rm = T)
ix=which(buoy2015_L1$TempC_5m==555.4)
buoy2015_L1$TempC_5m[ix]=NA
ix=which(buoy2015_L1$TempC_5m==0)
buoy2015_L1$TempC_5m[ix]=NA

range(buoy2015_L1$TempC_6m, na.rm = T)
ix=which(buoy2015_L1$TempC_6m==555.4)
buoy2015_L1$TempC_6m[ix]=NA
ix=which(buoy2015_L1$TempC_6m==0)
buoy2015_L1$TempC_6m[ix]=NA

range(buoy2015_L1$TempC_7m, na.rm = T)
ix=which(buoy2015_L1$TempC_7m==555.4)
buoy2015_L1$TempC_7m[ix]=NA
ix=which(buoy2015_L1$TempC_7m==0)
buoy2015_L1$TempC_7m[ix]=NA

range(buoy2015_L1$TempC_8m, na.rm = T)
ix=which(buoy2015_L1$TempC_8m==-6999)
buoy2015_L1$TempC_8m[ix]=NA
ix=which(buoy2015_L1$TempC_8m==555.4)
buoy2015_L1$TempC_8m[ix]=NA
ix=which(buoy2015_L1$TempC_8m==0)
buoy2015_L1$TempC_8m[ix]=NA

range(buoy2015_L1$TempC_9m, na.rm = T)
ix=which(buoy2015_L1$TempC_9m==0)
buoy2015_L1$TempC_9m[ix]=NA
ix=which(buoy2015_L1$TempC_9m==555.4)
buoy2015_L1$TempC_9m[ix]=NA

range(buoy2015_L1$TempC_10m, na.rm = T)
ix=which(buoy2015_L1$TempC_10m==-6999)
buoy2015_L1$TempC_10m[ix]=NA

##transform data to vertical for display
buoy2015_temp <- subset(buoy2015_L1, select=c('datetime', "TempC_0m", "TempC_1m", "TempC_2m", "TempC_3m", "TempC_4m", "TempC_5m", "TempC_6m", "TempC_7m", "TempC_8m", "TempC_9m", "TempC_10m"))
buoy2015_vert_temp <- melt(buoy2015_temp, id='datetime')

# ggplot(subset(buoy2015_vert_temp, subset=(datetime>='2015-01-01' & datetime < '2016-01-01')), aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='2015, NA values replaced with NA', x='date', y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +   
#   final_theme
# 
# ggplot(subset(buoy2015_vert_temp, subset=(datetime>='2015-06-01' & datetime < '2015-06-16')), aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='early June 2015, NA values replaced with NA', x='date', y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +   
#   final_theme
# 
# ggplot(subset(buoy2015_vert_temp, subset=(datetime>='2015-06-16' & datetime < '2015-07-01')), aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='late June 2015, NA values replaced with NA', x='date', y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +   
#   final_theme
# 
# ggplot(subset(buoy2015_vert_temp, subset=(datetime>='2015-07-01' & datetime < '2015-07-16')), aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='early July 2015, NA values replaced with NA', x='date', y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +   
#   final_theme
# 
# ggplot(subset(buoy2015_vert_temp, subset=(datetime>='2015-07-16' & datetime < '2015-08-01')), aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='late July 2015, NA values replaced with NA', x='date', y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +   
#   final_theme

####2015 thermisters compare with LMP####
buoy2015_temp_L1 <- subset(buoy2015_L1, select=c('datetime', "TempC_0m", "TempC_1m", "TempC_2m", "TempC_3m", "TempC_4m", "TempC_5m", "TempC_6m", "TempC_7m", "TempC_8m", "TempC_9m", "TempC_10m"))
buoy2015_vert_temp_L1 <- melt(buoy2015_temp_L1, id='datetime')
buoy2015_vert_temp_L1$depth [buoy2015_vert_temp_L1$variable=='TempC_0m'] = 0
buoy2015_vert_temp_L1$depth [buoy2015_vert_temp_L1$variable=='TempC_1m'] = 1
buoy2015_vert_temp_L1$depth [buoy2015_vert_temp_L1$variable=='TempC_2m'] = 2
buoy2015_vert_temp_L1$depth [buoy2015_vert_temp_L1$variable=='TempC_3m'] = 3
buoy2015_vert_temp_L1$depth [buoy2015_vert_temp_L1$variable=='TempC_4m'] = 4
buoy2015_vert_temp_L1$depth [buoy2015_vert_temp_L1$variable=='TempC_5m'] = 5
buoy2015_vert_temp_L1$depth [buoy2015_vert_temp_L1$variable=='TempC_6m'] = 6
buoy2015_vert_temp_L1$depth [buoy2015_vert_temp_L1$variable=='TempC_7m'] = 7
buoy2015_vert_temp_L1$depth [buoy2015_vert_temp_L1$variable=='TempC_8m'] = 8
buoy2015_vert_temp_L1$depth [buoy2015_vert_temp_L1$variable=='TempC_9m'] = 9
buoy2015_vert_temp_L1$depth [buoy2015_vert_temp_L1$variable=='TempC_10m'] = 10
buoy2015_vert_temp_L1$source <- 'buoy'

buoy2015_temp_LMP_L1 <- merge(buoy2015_vert_temp_L1, LMP2015_temp, all=T)
buoy2015_temp_LMP_L1$depth <- factor(buoy2015_temp_LMP_L1$depth, levels=c('0', '0.5', '1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12'))

# #plot buoy temp against LMP sonde values
# ggplot(subset(buoy2015_temp_LMP_L1, subset=(datetime>='2015-06-22' & datetime < '2015-06-23')), aes(x=datetime, y=value, col=depth, shape=source)) +
#   geom_point(size=2) +
#   coord_cartesian(ylim=c(5,25)) +
#   labs(title='June 22 buoy and LMP', x='date', y='') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +
#   final_theme
# 
# ggplot(subset(buoy2015_temp_LMP_L1, subset=(datetime>='2015-07-19' & datetime < '2015-07-21')), aes(x=datetime, y=value, col=depth, shape=source)) +
#   geom_point(size=2) +
#   coord_cartesian(ylim=c(5,25)) +
#   labs(title='July 20 buoy and LMP', x='date', y='') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +
#   final_theme
# 
# ggplot(subset(buoy2015_temp_LMP_L1, subset=(datetime>='2015-08-09' & datetime < '2015-08-11')), aes(x=datetime, y=value, col=depth, shape=source)) +
#   geom_point(size=2) +
#   coord_cartesian(ylim=c(5,25)) +
#   labs(title='August 10 buoy and LMP', x='date', y='') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +
#   final_theme
# 
# ggplot(subset(buoy2015_temp_LMP_L1, subset=(datetime>='2015-09-20' & datetime < '2015-09-22')), aes(x=datetime, y=value, col=depth, shape=source)) +
#   geom_point(size=2) +
#   coord_cartesian(ylim=c(5,25)) +
#   labs(title='August 10 buoy and LMP', x='date', y='') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +
#   final_theme

####2015 thermisters - data cleaning####
# remove 0m completely
buoy2015_L1$TempC_0m <- NA
buoy2015_temp_L1 <- subset(buoy2015_L1, select=c('datetime', "TempC_0m", "TempC_1m", "TempC_2m", "TempC_3m", "TempC_4m", "TempC_5m", "TempC_6m", "TempC_7m", "TempC_8m", "TempC_9m", "TempC_10m"))
buoy2015_vert_temp_L1 <- melt(buoy2015_temp_L1, id='datetime')

#buoy moved to June 11 11:00
# ggplot(subset(buoy2015_vert_temp_L1, subset=(datetime>='2015-06-01' & datetime < '2015-06-16')), aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='early June 2015, v2', x='date', y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +   
#   final_theme
# 
# ggplot(subset(buoy2015_vert_temp, subset=(datetime>='2015-06-11' & datetime < '2015-06-11 12:00')), aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='June 11 2015, buoy to summer location', x='date', y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +   
#   final_theme

ix=which(buoy2015_L1$datetime>=as.POSIXct('2015-01-01', tz='GMT') & buoy2015_L1$datetime<=as.POSIXct('2015-06-11 9:50', tz='GMT'))
for (i in alltemp) {buoy2015_L1[ix,i]=NA}
buoy2015_temp_L1 <- subset(buoy2015_L1, select=c('datetime', "TempC_0m", "TempC_1m", "TempC_2m", "TempC_3m", "TempC_4m", "TempC_5m", "TempC_6m", "TempC_7m", "TempC_8m", "TempC_9m", "TempC_10m"))
buoy2015_vert_temp_L1 <- melt(buoy2015_temp_L1, id='datetime')

# ggplot(subset(buoy2015_vert_temp_L1, subset=(datetime>='2015-06-11' & datetime < '2015-06-11 12:00')), aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='June 11 2015, buoy to summer location, L1', x='date', y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +   
#   final_theme
# 
# ggplot(subset(buoy2015_vert_temp_L1, subset=(datetime>='2015-06-01' & datetime < '2015-06-16')), aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='early June 2015, v3', x='date', y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +   
#   final_theme
# 
# #remove data after initial break in data
# ggplot(subset(buoy2015_vert_temp_L1, subset=(datetime>='2015-06-24 18:00' & datetime < '2015-06-25 6:00')), aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='late June 2015, v2', x='date', y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +   
#   final_theme

#remove data after 3am until end of year
ix=which(buoy2015_L1$datetime>=as.POSIXct('2015-06-25 3:00', tz='GMT') & buoy2015_L1$datetime<as.POSIXct('2016-01-01', tz='GMT'))
for (i in alltemp) {buoy2015_L1[ix,i]=NA}
buoy2015_temp_L1 <- subset(buoy2015_L1, select=c('datetime', "TempC_0m", "TempC_1m", "TempC_2m", "TempC_3m", "TempC_4m", "TempC_5m", "TempC_6m", "TempC_7m", "TempC_8m", "TempC_9m", "TempC_10m"))
buoy2015_vert_temp_L1 <- melt(buoy2015_temp_L1, id='datetime')

# ggplot(subset(buoy2015_vert_temp_L1, subset=(datetime>='2015-06-16' & datetime < '2015-07-01')), aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='late June 2015, v3', x='date', y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +   
#   final_theme
# 
# ggplot(subset(buoy2015_vert_temp_L1, subset=(datetime>=as.POSIXct('2015-01-01', tz='GMT') & datetime < as.POSIXct('2016-01-01', tz='GMT'))), aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='2015, clean', x='date', y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +   
#   final_theme


#### hobo line ####
hobo2015$datetime <- as.POSIXct(hobo2015$Date.Time, format='%m/%d/%Y %H:%M', tz='GMT')

hobo_temp <- subset(hobo2015, select=c('datetime', 'TempC_1m',  'TempC_2m', 'TempC_3m', 'TempC_4m', 'TempC_5m', 'TempC_6m', 'TempC_7m', 'TempC_8m', 'TempC_9m'))

hobo_vert <- melt(hobo_temp, id='datetime')

####2015 hobo line####
# ggplot(subset(hobo_vert, subset=(datetime>='2015-01-01' & datetime < '2016-01-01')), aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='hobo temp sensor summer 2015', x='date', y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +   
#   final_theme
# 
# ggplot(subset(hobo_vert, subset=(datetime>='2015-08-01' & datetime < '2015-08-16')), aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='hobo temp early aug 2015', x='date', y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +   
#   final_theme
# 
# ggplot(subset(hobo_vert, subset=(datetime>='2015-08-16' & datetime < '2015-09-01')), aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='hobo temp late aug 2015', x='date', y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +   
#   final_theme
# 
# ggplot(subset(hobo_vert, subset=(datetime>='2015-09-01' & datetime < '2015-09-16')), aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='hobo temp early sept 2015', x='date', y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +   
#   final_theme
# 
# ggplot(subset(hobo_vert, subset=(datetime>='2015-09-16' & datetime < '2015-10-01')), aes(x=datetime, y=value, col=variable)) +
#   geom_point() +
#   labs(title='hobo temp late sept 2015', x='date', y='temp (deg C)') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
#                               "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +   
#   final_theme
# 

hobo_temp <- rename.vars(hobo_temp, from=c('TempC_1m',  'TempC_2m', 'TempC_3m', 'TempC_4m', 'TempC_5m', 'TempC_6m', 'TempC_7m', 'TempC_8m', 'TempC_9m'),
                         to=c('HoboTempC_1m',  'HoboTempC_2m', 'HoboTempC_3m', 'HoboTempC_4m', 'HoboTempC_5m', 'HoboTempC_6m', 'HoboTempC_7m', 'HoboTempC_8m', 'HoboTempC_9m'))

buoy2015_L1 <- merge(buoy2015_L1, hobo_temp, by='datetime', all=T)


hobotherm_temp <- subset(buoy2015_L1, select=c('datetime', 'TempC_0m', 'TempC_1m', 'HoboTempC_1m', 'HoboTempC_2m', 'TempC_2m', 'TempC_3m','HoboTempC_3m', 
                                               'TempC_4m', 'HoboTempC_4m','TempC_5m', 'HoboTempC_5m', 'TempC_6m', 'HoboTempC_6m','TempC_7m', 'HoboTempC_7m',  
                                               'TempC_8m', 'HoboTempC_8m', 'TempC_9m', 'HoboTempC_9m', 'TempC_10m'))

hobotherm_temp_vert <- melt(hobotherm_temp, id='datetime')

hobotherm_temp_vert$depth [hobotherm_temp_vert$variable=='TempC_0m'] =0
hobotherm_temp_vert$depth [hobotherm_temp_vert$variable=='TempC_1m'] =1
hobotherm_temp_vert$depth [hobotherm_temp_vert$variable=='TempC_2m'] =2
hobotherm_temp_vert$depth [hobotherm_temp_vert$variable=='TempC_3m'] =3
hobotherm_temp_vert$depth [hobotherm_temp_vert$variable=='TempC_4m'] =4
hobotherm_temp_vert$depth [hobotherm_temp_vert$variable=='TempC_5m'] =5
hobotherm_temp_vert$depth [hobotherm_temp_vert$variable=='TempC_6m'] =6
hobotherm_temp_vert$depth [hobotherm_temp_vert$variable=='TempC_7m'] =7
hobotherm_temp_vert$depth [hobotherm_temp_vert$variable=='TempC_8m'] =8
hobotherm_temp_vert$depth [hobotherm_temp_vert$variable=='TempC_9m'] =9
hobotherm_temp_vert$depth [hobotherm_temp_vert$variable=='TempC_10m'] =10
hobotherm_temp_vert$depth [hobotherm_temp_vert$variable=='HoboTempC_1m'] =1
hobotherm_temp_vert$depth [hobotherm_temp_vert$variable=='HoboTempC_2m'] =2
hobotherm_temp_vert$depth [hobotherm_temp_vert$variable=='HoboTempC_3m'] =3
hobotherm_temp_vert$depth [hobotherm_temp_vert$variable=='HoboTempC_4m'] =4
hobotherm_temp_vert$depth [hobotherm_temp_vert$variable=='HoboTempC_5m'] =5
hobotherm_temp_vert$depth [hobotherm_temp_vert$variable=='HoboTempC_6m'] =6
hobotherm_temp_vert$depth [hobotherm_temp_vert$variable=='HoboTempC_7m'] =7
hobotherm_temp_vert$depth [hobotherm_temp_vert$variable=='HoboTempC_8m'] =8
hobotherm_temp_vert$depth [hobotherm_temp_vert$variable=='HoboTempC_9m'] =9
hobotherm_temp_vert$source [hobotherm_temp_vert$variable=='TempC_0m' |
                              hobotherm_temp_vert$variable=='TempC_1m' |
                              hobotherm_temp_vert$variable=='TempC_2m' |
                              hobotherm_temp_vert$variable=='TempC_3m' |
                              hobotherm_temp_vert$variable=='TempC_4m' |
                              hobotherm_temp_vert$variable=='TempC_5m' |
                              hobotherm_temp_vert$variable=='TempC_6m' |
                              hobotherm_temp_vert$variable=='TempC_7m' |
                              hobotherm_temp_vert$variable=='TempC_8m' |
                              hobotherm_temp_vert$variable=='TempC_9m' |
                              hobotherm_temp_vert$variable=='TempC_10m' ] = 'thermistor'
hobotherm_temp_vert$source [hobotherm_temp_vert$variable=='HoboTempC_1m' |
                              hobotherm_temp_vert$variable=='HoboTempC_2m' |
                              hobotherm_temp_vert$variable=='HoboTempC_3m' |
                              hobotherm_temp_vert$variable=='HoboTempC_4m' |
                              hobotherm_temp_vert$variable=='HoboTempC_5m' |
                              hobotherm_temp_vert$variable=='HoboTempC_6m' |
                              hobotherm_temp_vert$variable=='HoboTempC_7m' |
                              hobotherm_temp_vert$variable=='HoboTempC_8m' |
                              hobotherm_temp_vert$variable=='HoboTempC_9m' ] = 'hobo'

str(hobotherm_temp_vert)
hobotherm_temp_vert$depth <- factor(hobotherm_temp_vert$depth, levels=c('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '10'))

####2015 hobo + thermistor line####
# ggplot(subset(hobotherm_temp_vert, subset=(datetime>='2015-01-01' & datetime < '2016-01-01')), aes(x=datetime, y=value, col=depth, shape=source)) +
#  geom_point() +
#  labs(title='thermistor + hoboline 2015', x='date', y='temp (deg C)') +
#  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                              "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +
#   final_theme

####2015 hobo and LMP####
hobo_vert$depth [hobo_vert$variable=='TempC_1m'] = 1
hobo_vert$depth [hobo_vert$variable=='TempC_2m'] = 2
hobo_vert$depth [hobo_vert$variable=='TempC_3m'] = 3
hobo_vert$depth [hobo_vert$variable=='TempC_4m'] = 4
hobo_vert$depth [hobo_vert$variable=='TempC_5m'] = 5
hobo_vert$depth [hobo_vert$variable=='TempC_6m'] = 6
hobo_vert$depth [hobo_vert$variable=='TempC_7m'] = 7
hobo_vert$depth [hobo_vert$variable=='TempC_8m'] = 8
hobo_vert$depth [hobo_vert$variable=='TempC_9m'] = 9
hobo_vert$source <- 'buoy'

buoy2015_hobo_LMP_L1 <- merge(hobo_vert, LMP2015_temp, all=T)
buoy2015_hobo_LMP_L1$depth <- factor(buoy2015_hobo_LMP_L1$depth, levels=c('0', '0.5', '1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12'))

# #plot buoy temp against LMP sonde values
# ggplot(subset(buoy2015_hobo_LMP_L1, subset=(datetime>='2015-08-09' & datetime < '2015-08-11')), aes(x=datetime, y=value, col=depth, shape=source)) +
#   geom_point(size=2) +
#   labs(title='August 10 hobo and LMP', x='date', y='') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +
#   final_theme
# 
# ggplot(subset(buoy2015_hobo_LMP_L1, subset=(datetime>='2015-07-19' & datetime < '2015-07-21')), aes(x=datetime, y=value, col=depth, shape=source)) +
#   geom_point(size=2) +
#   labs(title='July 20 buoy and LMP', x='date', y='') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +
#   final_theme
# 
# ggplot(subset(buoy2015_hobo_LMP_L1, subset=(datetime>='2015-08-09' & datetime < '2015-08-11')), aes(x=datetime, y=value, col=depth, shape=source)) +
#   geom_point(size=2) +
#   coord_cartesian(ylim=c(5,25)) +
#   labs(title='August 10 buoy and LMP', x='date', y='') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +
#   final_theme
# 
# ggplot(subset(buoy2015_hobo_LMP_L1, subset=(datetime>='2015-09-20' & datetime < '2015-09-22')), aes(x=datetime, y=value, col=depth, shape=source)) +
#   geom_point(size=2) +
#   coord_cartesian(ylim=c(5,25)) +
#   labs(title='September 21 buoy and LMP', x='date', y='') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
#                               "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +
#   final_theme


####plot Upper DO ####
##transform data to vertical for display
buoy2015_updo <- subset(buoy2015_L1, select=c('datetime', upDO))
buoy2015_vert_updo <- melt(buoy2015_updo, id='datetime')

# ggplot(subset(buoy2015_vert_updo, subset=(datetime>=as.POSIXct('2015-01-01', tz='GMT') & datetime < as.POSIXct('2016-01-01', tz='GMT'))), aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='2015, raw', x='date', y='') +
#   final_theme
# 
# ggplot(subset(buoy2015_vert_updo, subset=(datetime>=as.POSIXct('2015-01-01', tz='GMT') & datetime < as.POSIXct('2015-01-16', tz='GMT'))), aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='early January 2015', x='date', y='') +
#   final_theme
# 
# ggplot(subset(buoy2015_vert_updo, subset=(datetime>=as.POSIXct('2015-01-16', tz='GMT') & datetime < as.POSIXct('2015-02-01', tz='GMT'))), aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='late January 2015', x='date', y='') +
#   final_theme
# 
# ggplot(subset(buoy2015_vert_updo, subset=(datetime>=as.POSIXct('2015-02-01', tz='GMT') & datetime < as.POSIXct('2015-02-14', tz='GMT'))), aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='early February 2015', x='date', y='') +
#   final_theme
# 
# ggplot(subset(buoy2015_vert_updo, subset=(datetime>=as.POSIXct('2015-02-14', tz='GMT') & datetime < as.POSIXct('2015-03-01', tz='GMT'))), aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='late February 2015', x='date', y='') +
#   final_theme
# 
# ggplot(subset(buoy2015_vert_updo, subset=(datetime>=as.POSIXct('2015-03-01', tz='GMT') & datetime < as.POSIXct('2015-03-16', tz='GMT'))), aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='early March 2015', x='date', y='') +
#   final_theme
# 
# ggplot(subset(buoy2015_vert_updo, subset=(datetime>=as.POSIXct('2015-03-16', tz='GMT') & datetime < as.POSIXct('2015-04-01', tz='GMT'))), aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='late March 2015', x='date', y='') +
#   final_theme
# 
# ggplot(subset(buoy2015_vert_updo, subset=(datetime>=as.POSIXct('2015-04-01', tz='GMT') & datetime < as.POSIXct('2015-04-16', tz='GMT'))), aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='early April 2015', x='date', y='') +
#   final_theme
# 
# ggplot(subset(buoy2015_vert_updo, subset=(datetime>=as.POSIXct('2015-04-16', tz='GMT') & datetime < as.POSIXct('2015-05-01', tz='GMT'))), aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='late April 2015', x='date', y='') +
#   final_theme
# 
# #data gap april to june
# 
# ggplot(subset(buoy2015_vert_updo, subset=(datetime>=as.POSIXct('2015-06-01', tz='GMT') & datetime < as.POSIXct('2015-06-16', tz='GMT'))), aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='early June 2015', x='date', y='') +
#   final_theme
# 
# ggplot(subset(buoy2015_vert_updo, subset=(datetime>=as.POSIXct('2015-06-16', tz='GMT') & datetime < as.POSIXct('2015-07-01', tz='GMT'))), aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='late June 2015', x='date', y='') +
#   final_theme
# 
# ggplot(subset(buoy2015_vert_updo, subset=(datetime>=as.POSIXct('2015-07-01', tz='GMT') & datetime < as.POSIXct('2015-07-16', tz='GMT'))), aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='early July 2015', x='date', y='') +
#   final_theme
# 
# ggplot(subset(buoy2015_vert_updo, subset=(datetime>=as.POSIXct('2015-07-16', tz='GMT') & datetime < as.POSIXct('2015-08-01', tz='GMT'))), aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='late July 2015', x='date', y='') +
#   final_theme
# 
# ggplot(subset(buoy2015_vert_updo, subset=(datetime>=as.POSIXct('2015-08-01', tz='GMT') & datetime < as.POSIXct('2015-08-16', tz='GMT'))), aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='early August 2015', x='date', y='') +
#   final_theme
# 
# ggplot(subset(buoy2015_vert_updo, subset=(datetime>=as.POSIXct('2015-08-16', tz='GMT') & datetime < as.POSIXct('2015-09-01', tz='GMT'))), aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='late August 2015', x='date', y='') +
#   final_theme
# 
# ggplot(subset(buoy2015_vert_updo, subset=(datetime>=as.POSIXct('2015-09-01', tz='GMT') & datetime < as.POSIXct('2015-09-16', tz='GMT'))), aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='early September 2015', x='date', y='') +
#   final_theme
# 
# ggplot(subset(buoy2015_vert_updo, subset=(datetime>=as.POSIXct('2015-09-16', tz='GMT') & datetime < as.POSIXct('2015-10-01', tz='GMT'))), aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='late September 2015', x='date', y='') +
#   final_theme
# 
# ggplot(subset(buoy2015_vert_updo, subset=(datetime>=as.POSIXct('2015-10-01', tz='GMT') & datetime < as.POSIXct('2015-10-16', tz='GMT'))), aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='early October 2015', x='date', y='') +
#   final_theme

#####upper do - remove/replace NA####
#print range of each variable and address obvious issues#
range(buoy2015_L1$DOTempC, na.rm = T)
ix=which(buoy2015_L1$DOTempC==-6999)
buoy2015_L1$DOTempC[ix]=NA
ix=which(buoy2015_L1$DOTempC==0)
buoy2015_L1$DOTempC[ix]=NA

range(buoy2015_L1$DOSat, na.rm=T)
ix=which(buoy2015_L1$DOSat==-6999)
buoy2015_L1$DOSat[ix]=NA
ix=which(buoy2015_L1$DOSat==0)
buoy2015_L1$DOSat[ix]=NA

range(buoy2015_L1$DOppm, na.rm = T)
ix=which(buoy2015_L1$DOppm==-6999)
buoy2015_L1$DOppm[ix]=NA
ix=which(buoy2015_L1$DOppm==0)
buoy2015_L1$DOppm[ix]=NA

buoy2015_updo_L1 <- subset(buoy2015_L1, select=c('datetime', upDO))
buoy2015_vert_updo_L1 <- melt(buoy2015_updo_L1, id='datetime')
  
# ggplot(subset(buoy2015_vert_updo_L1, subset=(datetime>=as.POSIXct('2015-01-01', tz='GMT') & datetime < as.POSIXct('2016-01-01', tz='GMT'))), aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='2015, NA strings replace with NA', x='date', y='') +
#   final_theme
# 
# ggplot(subset(buoy2015_vert_updo_L1, subset=(datetime>=as.POSIXct('2015-06-01', tz='GMT') & datetime < as.POSIXct('2015-06-16', tz='GMT'))), aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='early June 2015, NA strings replace with NA', x='date', y='') +
#   final_theme

#### 2015 upper DO - clean data ####
#remove artifacts of buoy move/going off line
# ggplot(subset(buoy2015_vert_updo_L1, subset=(datetime>=as.POSIXct('2015-04-22 6:00', tz='GMT') & datetime < as.POSIXct('2015-05-01 16:00', tz='GMT'))), aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='late April 2015', x='date', y='') +
#   final_theme

ix=which(buoy2015_L1$datetime>=as.POSIXct('2015-04-22 11:00', tz='GMT') & buoy2015_L1$datetime<=as.POSIXct('2015-04-22 11:10', tz='GMT'))
for (i in upDO) {buoy2015_L1 [ix,i] = NA}
buoy2015_updo_L1 <- subset(buoy2015_L1, select=c('datetime', upDO))
buoy2015_vert_updo_L1 <- melt(buoy2015_updo_L1, id='datetime')

# ggplot(subset(buoy2015_vert_updo_L1, subset=(datetime>=as.POSIXct('2015-04-22 6:00', tz='GMT') & datetime < as.POSIXct('2015-05-01 16:00', tz='GMT'))), aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='late April 2015', x='date', y='') +
#   final_theme
# 
# ggplot(subset(buoy2015_vert_updo_L1, subset=(datetime>=as.POSIXct('2015-04-16', tz='GMT') & datetime < as.POSIXct('2015-05-01', tz='GMT'))), aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='late April 2015, v2', x='date', y='') +
#   final_theme

# ggplot(subset(buoy2015_vert_updo_L1, subset=(datetime>=as.POSIXct('2015-06-01', tz='GMT') & datetime < as.POSIXct('2015-06-16', tz='GMT'))), aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='early June 2015', x='date', y='') +
#   final_theme
# 
# #remove data from 1st to 3rd, before sensors correctly reporting, then short data gap until the 4th
# ggplot(subset(buoy2015_vert_updo_L1, subset=(datetime>=as.POSIXct('2015-06-03', tz='GMT') & datetime < as.POSIXct('2015-06-05', tz='GMT'))), aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='early June 2015', x='date', y='') +
#   final_theme

ix=which(buoy2015_L1$datetime>=as.POSIXct('2015-06-01', tz='GMT') & buoy2015_L1$datetime<=as.POSIXct('2015-06-04 10:00', tz='GMT'))
for (i in upDO) {buoy2015_L1 [ix,i] = NA}
buoy2015_updo_L1 <- subset(buoy2015_L1, select=c('datetime', upDO))
buoy2015_vert_updo_L1 <- melt(buoy2015_updo_L1, id='datetime')

# ggplot(subset(buoy2015_vert_updo_L1, subset=(datetime>=as.POSIXct('2015-06-04', tz='GMT') & datetime < as.POSIXct('2015-06-04 12:00', tz='GMT'))), aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='early June 2015', x='date', y='') +
#   final_theme
# 
# ggplot(subset(buoy2015_vert_updo_L1, subset=(datetime>=as.POSIXct('2015-06-01', tz='GMT') & datetime < as.POSIXct('2015-06-16', tz='GMT'))), aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='early June 2015, v2', x='date', y='') +
#   final_theme

# #remove artifacts of buoy move
# ggplot(subset(buoy2015_vert_updo_L1, subset=(datetime>=as.POSIXct('2015-06-11', tz='GMT') & datetime < as.POSIXct('2015-06-12', tz='GMT'))), aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='early June 2015', x='date', y='') +
#   final_theme

ix=which(buoy2015_L1$datetime>=as.POSIXct('2015-06-11 9:50', tz='GMT') & buoy2015_L1$datetime<=as.POSIXct('2015-06-11 10:00', tz='GMT'))
for (i in upDO) {buoy2015_L1 [ix,i] = NA}
buoy2015_updo_L1 <- subset(buoy2015_L1, select=c('datetime', upDO))
buoy2015_vert_updo_L1 <- melt(buoy2015_updo_L1, id='datetime')

# moved to deep spot on jun 11
buoy2015_L1$location <- as.character('')
ix=which(buoy2015_L1$datetime>=as.POSIXct('2015-01-01', tz='GMT') & buoy2015_L1$datetime<as.POSIXct('2015-06-11 9:50', tz='GMT'))
buoy2015_L1$location [ix] = 'harbor'
ix=which(buoy2015_L1$datetime>=as.POSIXct('2015-06-11 10:10', tz='GMT') & buoy2015_L1$datetime<as.POSIXct('2016-01-01', tz='GMT'))
buoy2015_L1$location [ix] = 'deep spot'

buoy2015_updo_L1 <- subset(buoy2015_L1, select=c('datetime', 'location', upDO))
buoy2015_vert_updo_L1 <- melt(buoy2015_updo_L1, id=c('datetime', 'location'))

# ggplot(subset(buoy2015_vert_updo_L1, subset=(datetime>=as.POSIXct('2015-06-01', tz='GMT') & datetime < as.POSIXct('2015-06-16', tz='GMT'))), aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='early June 2015, v3', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme



# ggplot(subset(buoy2015_vert_updo_L1, subset=(datetime>=as.POSIXct('2015-09-16', tz='GMT') & datetime < as.POSIXct('2015-10-01', tz='GMT'))), aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='late September 2015', x='date', y='') +
#   final_theme
# 
# #errant point around the 19th
# ggplot(subset(buoy2015_vert_updo_L1, subset=(datetime>=as.POSIXct('2015-09-19', tz='GMT') & datetime < as.POSIXct('2015-09-20', tz='GMT'))), aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='late September 2015', x='date', y='') +
#   final_theme

ix=which(buoy2015_L1$datetime==as.POSIXct('2015-09-19 5:50', tz='GMT'))
for (i in upDO) {buoy2015_L1 [ix,i] = NA}
buoy2015_updo_L1 <- subset(buoy2015_L1, select=c('datetime', 'location', upDO))
buoy2015_vert_updo_L1 <- melt(buoy2015_updo_L1, id=c('datetime', 'location'))

# ggplot(subset(buoy2015_vert_updo_L1, subset=(datetime>=as.POSIXct('2015-09-19', tz='GMT') & datetime < as.POSIXct('2015-09-20', tz='GMT'))), aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='late September 2015', x='date', y='') +
#   final_theme
# 
# ggplot(subset(buoy2015_vert_updo_L1, subset=(datetime>=as.POSIXct('2015-09-16', tz='GMT') & datetime < as.POSIXct('2015-10-01', tz='GMT'))), aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='late September 2015, v2', x='date', y='') +
#   final_theme


# ggplot(subset(buoy2015_vert_updo_L1, subset=(datetime>=as.POSIXct('2015-10-01', tz='GMT') & datetime < as.POSIXct('2015-10-16', tz='GMT'))), aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='early October 2015', x='date', y='') +
#   final_theme
# 
# # buoy moved to harbor. 
# ggplot(subset(buoy2015_vert_updo_L1, subset=(datetime>=as.POSIXct('2015-10-08', tz='GMT') & datetime < as.POSIXct('2015-10-09', tz='GMT'))), aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='October 08, buoy to harbor', x='date', y='') +
#   final_theme

#remove artifacts of buoy movement
ix=which(buoy2015_L1$datetime>=as.POSIXct('2015-10-08 10:40', tz='GMT') & buoy2015_L1$datetime<=as.POSIXct('2015-10-08 11:20', tz='GMT'))
for (i in upDO) {buoy2015_L1[ix,i]=NA}

ix=which(buoy2015_L1$datetime>=as.POSIXct('2015-10-08 11:20', tz='GMT') & buoy2015_L1$datetime<as.POSIXct('2016-01-01', tz='GMT'))
buoy2015_L1$location [ix] = 'harbor'

buoy2015_updo_L1 <- subset(buoy2015_L1, select=c('datetime', 'location', upDO))
buoy2015_vert_updo_L1 <- melt(buoy2015_updo_L1, id=c('datetime', 'location'))

# ggplot(subset(buoy2015_vert_updo_L1, subset=(datetime>=as.POSIXct('2015-10-01', tz='GMT') & datetime < as.POSIXct('2015-10-16', tz='GMT'))), aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='early October 2015, v2', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme

#remove data after buoy move -error in sensor
ix=which(buoy2015_L1$datetime>=as.POSIXct('2015-10-08 11:30', tz='GMT') & buoy2015_L1$datetime<=as.POSIXct('2015-12-31 23:50', tz='GMT'))
for (i in upDO) {buoy2015_L1[ix,i]=NA}
buoy2015_updo_L1 <- subset(buoy2015_L1, select=c('datetime', 'location', upDO))
buoy2015_vert_updo_L1 <- melt(buoy2015_updo_L1, id=c('datetime', 'location'))

# ggplot(subset(buoy2015_vert_updo_L1, subset=(datetime>=as.POSIXct('2015-10-01', tz='GMT') & datetime < as.POSIXct('2015-10-16', tz='GMT'))), aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='early October 2015, v3', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme



# ggplot(subset(buoy2015_vert_updo_L1, subset=(datetime>=as.POSIXct('2015-01-01', tz='GMT') & datetime < as.POSIXct('2016-01-01', tz='GMT'))), aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='2015, clean', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme



#### plot Lower DO ####
buoy2015_lowdo <- subset(buoy2015_L1, select=c('datetime', lowDO))
buoy2015_vert_lowdo <- melt(buoy2015_lowdo, id='datetime')

# ggplot(subset(buoy2015_vert_lowdo, subset=(datetime>=as.POSIXct('2015-01-01', tz='GMT') & datetime < as.POSIXct('2016-01-01', tz='GMT'))), aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='2015, raw', x='date', y='') +
#   final_theme
# 
# ggplot(subset(buoy2015_vert_lowdo, subset=(datetime>=as.POSIXct('2015-06-01', tz='GMT') & datetime < as.POSIXct('2015-06-16', tz='GMT'))), aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='early June 2015', x='date', y='') +
#   final_theme
# 
# ggplot(subset(buoy2015_vert_lowdo, subset=(datetime>=as.POSIXct('2015-06-16', tz='GMT') & datetime < as.POSIXct('2015-07-01', tz='GMT'))), aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='late June 2015', x='date', y='') +
#   final_theme
# 
# ggplot(subset(buoy2015_vert_lowdo, subset=(datetime>=as.POSIXct('2015-07-01', tz='GMT') & datetime < as.POSIXct('2015-07-16', tz='GMT'))), aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='early July 2015', x='date', y='') +
#   final_theme
# 
# ggplot(subset(buoy2015_vert_lowdo, subset=(datetime>=as.POSIXct('2015-07-16', tz='GMT') & datetime < as.POSIXct('2015-08-01', tz='GMT'))), aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='late July 2015', x='date', y='') +
#   final_theme
# 
# ggplot(subset(buoy2015_vert_lowdo, subset=(datetime>=as.POSIXct('2015-08-01', tz='GMT') & datetime < as.POSIXct('2015-08-16', tz='GMT'))), aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='early August 2015', x='date', y='') +
#   final_theme
# 
# ggplot(subset(buoy2015_vert_lowdo, subset=(datetime>=as.POSIXct('2015-08-16', tz='GMT') & datetime < as.POSIXct('2015-09-01', tz='GMT'))), aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='late August 2015', x='date', y='') +
#   final_theme
# 
# ggplot(subset(buoy2015_vert_lowdo, subset=(datetime>=as.POSIXct('2015-09-01', tz='GMT') & datetime < as.POSIXct('2015-09-16', tz='GMT'))), aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='early September 2015', x='date', y='') +
#   final_theme
# 
# ggplot(subset(buoy2015_vert_lowdo, subset=(datetime>=as.POSIXct('2015-09-16', tz='GMT') & datetime < as.POSIXct('2015-10-01', tz='GMT'))), aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='late September 2015', x='date', y='') +
#   final_theme
# 
# ggplot(subset(buoy2015_vert_lowdo, subset=(datetime>=as.POSIXct('2015-10-01', tz='GMT') & datetime < as.POSIXct('2015-10-16', tz='GMT'))), aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='early October 2015', x='date', y='') +
#   final_theme



#print range of each variable and address obvious issues#
range(buoy2015_L1$DOLoTempC, na.rm = T)
ix=which(buoy2015_L1$DOLoTempC==-6999)
buoy2015_L1$DOLoTempC[ix]=NA
ix=which(buoy2015_L1$DOLoTempC==0)
buoy2015_L1$DOLoTempC[ix]=NA

range(buoy2015_L1$DOLowSat, na.rm=T)
ix=which(buoy2015_L1$DOLowSat==-6999)
buoy2015_L1$DOLowSat[ix]=NA
ix=which(buoy2015_L1$DOLowSat==0)
buoy2015_L1$DOLowSat[ix]=NA

range(buoy2015_L1$DOLowPPM, na.rm = T)
ix=which(buoy2015_L1$DOLowPPM==-6999)
buoy2015_L1$DOLowPPM[ix]=NA
ix=which(buoy2015_L1$DOLowPPM==0)
buoy2015_L1$DOLowPPM[ix]=NA

##transform data to vertical for display
buoy2015_lowdo <- subset(buoy2015_L1, select=c('datetime', lowDO))
buoy2015_vert_lowdo <- melt(buoy2015_lowdo, id='datetime')

# ggplot(subset(buoy2015_vert_lowdo, subset=(datetime>=as.POSIXct('2015-01-01', tz='GMT') & datetime < as.POSIXct('2016-01-01', tz='GMT'))), aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='2015, NA strings replaced with NA', x='date', y='') +
#   final_theme
# 
# ggplot(subset(buoy2015_vert_lowdo, subset=(datetime>=as.POSIXct('2015-06-01', tz='GMT') & datetime < as.POSIXct('2015-06-16', tz='GMT'))), aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='early June 2015, NA strings replaced with NA', x='date', y='') +
#   final_theme


####2015 lower do - clean data####
buoy2015_lowdo_L1 <- subset(buoy2015_L1, select=c('datetime', lowDO))
buoy2015_vert_lowdo_L1 <- melt(buoy2015_lowdo_L1, id='datetime')

# ggplot(subset(buoy2015_vert_lowdo, subset=(datetime>=as.POSIXct('2015-06-01', tz='GMT') & datetime < as.POSIXct('2015-06-16', tz='GMT'))), aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='early June 2015', x='date', y='') +
#   final_theme
# 
# #remove artifacts of buoy move
# ggplot(subset(buoy2015_vert_lowdo, subset=(datetime>=as.POSIXct('2015-06-11', tz='GMT') & datetime < as.POSIXct('2015-06-12', tz='GMT'))), aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='early June 2015', x='date', y='') +
#   final_theme

#note, that time ends later because of equilibration of do readings
ix=which(buoy2015_L1$datetime>=as.POSIXct('2015-01-01', tz='GMT') & buoy2015_L1$datetime<as.POSIXct('2015-06-11 11:10', tz='GMT'))
for (i in lowDO) {buoy2015_L1 [ix,i] = NA}
buoy2015_lowdo_L1 <- subset(buoy2015_L1, select=c('datetime', lowDO))
buoy2015_vert_lowdo_L1 <- melt(buoy2015_lowdo_L1, id='datetime')


# ggplot(subset(buoy2015_vert_lowdo_L1, subset=(datetime>=as.POSIXct('2015-06-01', tz='GMT') & datetime < as.POSIXct('2015-06-16', tz='GMT'))), aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='early June 2015, v2', x='date', y='') +
#   final_theme


# ggplot(subset(buoy2015_vert_lowdo, subset=(datetime>=as.POSIXct('2015-08-01', tz='GMT') & datetime < as.POSIXct('2015-08-16', tz='GMT'))), aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='early August 2015', x='date', y='') +
#   final_theme
# 
# ggplot(subset(buoy2015_vert_lowdo, subset=(datetime>=as.POSIXct('2015-08-13', tz='GMT') & datetime < as.POSIXct('2015-08-14', tz='GMT'))), aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='early August 2015', x='date', y='') +
#   final_theme

#August 13th visit
ix=which(buoy2015_L1$datetime==as.POSIXct('2015-08-13 10:00', tz='GMT'))
for (i in lowDO) {buoy2015_L1 [ix,i] = NA}
buoy2015_lowdo_L1 <- subset(buoy2015_L1, select=c('datetime', lowDO))
buoy2015_vert_lowdo_L1 <- melt(buoy2015_lowdo_L1, id='datetime')

# ggplot(subset(buoy2015_vert_lowdo_L1, subset=(datetime>=as.POSIXct('2015-08-01', tz='GMT') & datetime < as.POSIXct('2015-08-16', tz='GMT'))), aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='early August 2015, v2', x='date', y='') +
#   final_theme


# ggplot(subset(buoy2015_vert_lowdo, subset=(datetime>=as.POSIXct('2015-10-01', tz='GMT') & datetime < as.POSIXct('2015-10-16', tz='GMT'))), aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='early October 2015', x='date', y='') +
#   final_theme
# 
# # buoy moved to harbor. 
# ggplot(subset(buoy2015_vert_lowdo, subset=(datetime>=as.POSIXct('2015-10-08', tz='GMT') & datetime < as.POSIXct('2015-10-09', tz='GMT'))), aes(x=datetime, y=value)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='October 08, buoy to harbor', x='date', y='') +
#   final_theme
# 
#remove artifacts of buoy movement and data after move - all errant
ix=which(buoy2015_L1$datetime>=as.POSIXct('2015-10-08 10:40', tz='GMT') & buoy2015_L1$datetime<as.POSIXct('2016-01-01', tz='GMT'))
for (i in lowDO) {buoy2015_L1[ix,i]=NA}
buoy2015_lowdo_L1 <- subset(buoy2015_L1, select=c('datetime', 'location', lowDO))
buoy2015_vert_lowdo_L1 <- melt(buoy2015_lowdo_L1, id=c('datetime', 'location'))

# ggplot(subset(buoy2015_vert_lowdo_L1, subset=(datetime>=as.POSIXct('2015-10-01', tz='GMT') & datetime < as.POSIXct('2015-10-16', tz='GMT'))), aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='early October 2015, v2', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme
# 
# ggplot(subset(buoy2015_vert_lowdo_L1, subset=(datetime>=as.POSIXct('2015-01-01', tz='GMT') & datetime < as.POSIXct('2016-01-01', tz='GMT'))), aes(x=datetime, y=value, color=location)) +
#   geom_point() +
#   facet_grid(variable~., scales='free_y') +
#   labs(title='2015, clean', x='date', y='') +
#   scale_color_colorblind() +
#   final_theme

#### meteorology ####
range(buoy2015_L1$AirTempC, na.rm = T)
range(buoy2015_L1$RelHum, na.rm = T)
range(buoy2015_L1$PAR, na.rm = T)
range(buoy2015_L1$WindSpdAv, na.rm = T)
range(buoy2015_L1$WindVect, na.rm = T)
range(buoy2015_L1$MaxWind, na.rm = T)
range(buoy2015_L1$MaxWindDir, na.rm = T)

####PAR ####
#replace negative values with 0
ix=which(buoy2015_L1$PAR<0)
buoy2015_L1$PAR[ix]=0


####2015####
#ggplot(subset(buoy2015_L1, subset=(datetime>=as.POSIXct('2015-01-01', tz='GMT') & datetime<as.POSIXct('2016-01-01', tz='GMT'))), aes(x=datetime, y=PAR)) +
  geom_point() + 
  labs(title='2015, raw', x='date', y='PAR (uE*m-2*s-1)') +
  final_theme

#par comes back online June 2015

#ggplot(subset(buoy2015_L1, subset=(datetime>=as.POSIXct('2015-06-01', tz='GMT') & datetime<as.POSIXct('2015-06-16', tz='GMT'))), aes(x=datetime, y=PAR)) +
  geom_point() + 
  labs(title='early June 2015', x='date', y='PAR (uE*m-2*s-1)') +
  final_theme

#need to remove data before PAR functioning correctly
#ggplot(subset(buoy2015_L1, subset=(datetime>=as.POSIXct('2015-06-04', tz='GMT') & datetime<as.POSIXct('2015-06-05', tz='GMT'))), aes(x=datetime, y=PAR)) +
  geom_point() + 
  labs(title='early June 2015', x='date', y='PAR (uE*m-2*s-1)') +
  final_theme

ix=which(buoy2015_L1$datetime>=as.POSIXct('2015-01-01', tz='GMT') & buoy2015_L1$datetime<=as.POSIXct('2015-06-04 9:00', tz='GMT'))
buoy2015_L1$PAR[ix]=NA

#ggplot(subset(buoy2015_L1, subset=(datetime>=as.POSIXct('2015-06-04', tz='GMT') & datetime<as.POSIXct('2015-06-05', tz='GMT'))), aes(x=datetime, y=PAR)) +
  geom_point() + 
  labs(title='early June 2015', x='date', y='PAR (uE*m-2*s-1)') +
  final_theme

#ggplot(subset(buoy2015_L1, subset=(datetime>=as.POSIXct('2015-06-01', tz='GMT') & datetime<as.POSIXct('2015-06-16', tz='GMT'))), aes(x=datetime, y=PAR)) +
  geom_point() + 
  labs(title='early June 2015, v2', x='date', y='PAR (uE*m-2*s-1)') +
  final_theme


#ggplot(subset(buoy2015_L1, subset=(datetime>=as.POSIXct('2015-06-16', tz='GMT') & datetime<as.POSIXct('2015-07-01', tz='GMT'))), aes(x=datetime, y=PAR)) +
  geom_point() + 
  labs(title='late June 2015', x='date', y='PAR (uE*m-2*s-1)') +
  final_theme

#ggplot(subset(buoy2015_L1, subset=(datetime>=as.POSIXct('2015-07-01', tz='GMT') & datetime<as.POSIXct('2015-07-16', tz='GMT'))), aes(x=datetime, y=PAR)) +
  geom_point() + 
  labs(title='early July 2015', x='date', y='PAR (uE*m-2*s-1)') +
  final_theme

#ggplot(subset(buoy2015_L1, subset=(datetime>=as.POSIXct('2015-07-16', tz='GMT') & datetime<as.POSIXct('2015-08-01', tz='GMT'))), aes(x=datetime, y=PAR)) +
  geom_point() + 
  labs(title='late July 2015', x='date', y='PAR (uE*m-2*s-1)') +
  final_theme

#ggplot(subset(buoy2015_L1, subset=(datetime>=as.POSIXct('2015-08-01', tz='GMT') & datetime<as.POSIXct('2015-08-16', tz='GMT'))), aes(x=datetime, y=PAR)) +
  geom_point() + 
  labs(title='early August 2015', x='date', y='PAR (uE*m-2*s-1)') +
  final_theme

#ggplot(subset(buoy2015_L1, subset=(datetime>=as.POSIXct('2015-08-16', tz='GMT') & datetime<as.POSIXct('2015-09-01', tz='GMT'))), aes(x=datetime, y=PAR)) +
  geom_point() + 
  labs(title='late August 2015', x='date', y='PAR (uE*m-2*s-1)') +
  final_theme

#ggplot(subset(buoy2015_L1, subset=(datetime>=as.POSIXct('2015-09-01', tz='GMT') & datetime<as.POSIXct('2015-09-16', tz='GMT'))), aes(x=datetime, y=PAR)) +
  geom_point() + 
  labs(title='early Sept 2015', x='date', y='PAR (uE*m-2*s-1)') +
  final_theme

#ggplot(subset(buoy2015_L1, subset=(datetime>=as.POSIXct('2015-09-16', tz='GMT') & datetime<as.POSIXct('2015-10-01', tz='GMT'))), aes(x=datetime, y=PAR)) +
  geom_point() + 
  labs(title='late Sept 2015', x='date', y='PAR (uE*m-2*s-1)') +
  final_theme

#ggplot(subset(buoy2015_L1, subset=(datetime>=as.POSIXct('2015-10-01', tz='GMT') & datetime<as.POSIXct('2015-10-16', tz='GMT'))), aes(x=datetime, y=PAR)) +
  geom_point() + 
  labs(title='early Oct 2015', x='date', y='PAR (uE*m-2*s-1)') +
  final_theme

#ggplot(subset(buoy2015_L1, subset=(datetime>=as.POSIXct('2015-10-16', tz='GMT') & datetime<as.POSIXct('2015-11-01', tz='GMT'))), aes(x=datetime, y=PAR)) +
  geom_point() + 
  labs(title='late Oct 2015', x='date', y='PAR (uE*m-2*s-1)') +
  final_theme

#ggplot(subset(buoy2015_L1, subset=(datetime>=as.POSIXct('2015-11-01', tz='GMT') & datetime<as.POSIXct('2015-11-16', tz='GMT'))), aes(x=datetime, y=PAR)) +
  geom_point() + 
  labs(title='early November 2015', x='date', y='PAR (uE*m-2*s-1)') +
  final_theme

#ggplot(subset(buoy2015_L1, subset=(datetime>=as.POSIXct('2015-11-16', tz='GMT') & datetime<as.POSIXct('2015-12-01', tz='GMT'))), aes(x=datetime, y=PAR)) +
  geom_point() + 
  labs(title='late November 2015', x='date', y='PAR (uE*m-2*s-1)') +
  final_theme

#ggplot(subset(buoy2015_L1, subset=(datetime>=as.POSIXct('2015-12-01', tz='GMT') & datetime<as.POSIXct('2015-12-16', tz='GMT'))), aes(x=datetime, y=PAR)) +
  geom_point() + 
  labs(title='early December 2015', x='date', y='PAR (uE*m-2*s-1)') +
  final_theme

#ggplot(subset(buoy2015_L1, subset=(datetime>=as.POSIXct('2015-12-16', tz='GMT') & datetime<as.POSIXct('2016-01-01', tz='GMT'))), aes(x=datetime, y=PAR)) +
  geom_point() + 
  labs(title='late December 2015', x='date', y='PAR (uE*m-2*s-1)') +
  final_theme

#ggplot(subset(buoy2015_L1, subset=(datetime>=as.POSIXct('2015-01-01', tz='GMT') & datetime<as.POSIXct('2016-01-01', tz='GMT'))), aes(x=datetime, y=PAR)) +
  geom_point() + 
  labs(title='2015, clean', x='date', y='PAR (uE*m-2*s-1)') +
  final_theme






####Wind data ####
buoy2015_wind <- subset(buoy2015_L1, select=c('datetime', wind))
buoy2015_vert_wind <- melt(buoy2015_wind, id='datetime')



####2015####
#ggplot(subset(buoy2015_vert_wind, subset=(datetime>=as.POSIXct('2015-01-01', tz='GMT') & datetime<as.POSIXct('2016-01-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='2015, raw', x='date', y=' ') +
  final_theme

#ggplot(subset(buoy2015_vert_wind, subset=(datetime>=as.POSIXct('2015-01-01', tz='GMT') & datetime<as.POSIXct('2015-01-16', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() +   
  facet_grid(variable~., scales='free_y') +
  labs(title='early January 2015', x='date', y=' ') +
  final_theme

#ggplot(subset(buoy2015_vert_wind, subset=(datetime>=as.POSIXct('2015-01-16', tz='GMT') & datetime<as.POSIXct('2015-02-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='late January 2015', x='date', y=' ') +
  final_theme

#ggplot(subset(buoy2015_vert_wind, subset=(datetime>=as.POSIXct('2015-02-01', tz='GMT') & datetime<as.POSIXct('2015-02-14', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='early February 2015', x='date', y=' ') +
  final_theme

#ggplot(subset(buoy2015_vert_wind, subset=(datetime>=as.POSIXct('2015-02-14', tz='GMT') & datetime<as.POSIXct('2015-03-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='late February 2015', x='date', y=' ') +
  final_theme

## flat lines on feb 15 and 19

#ggplot(subset(buoy2015_vert_wind, subset=(datetime>=as.POSIXct('2015-02-15', tz='GMT') & datetime<as.POSIXct('2015-02-16', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='late February 2015', x='date', y=' ') +
  final_theme
#ggplot(subset(buoy2015_vert_wind, subset=(datetime>=as.POSIXct('2015-02-18 18:00', tz='GMT') & datetime<as.POSIXct('2015-02-19 12:00', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='late February 2015', x='date', y=' ') +
  final_theme

ix=which(buoy2015_L1$datetime>=as.POSIXct('2015-02-15 00:40', tz='GMT') & buoy2015_L1$datetime<=as.POSIXct('2015-02-15 14:00', tz='GMT'))
for (i in wind) {buoy2015_L1[ix, i]= NA}
ix=which(buoy2015_L1$datetime>=as.POSIXct('2015-02-18 22:40', tz='GMT') & buoy2015_L1$datetime<=as.POSIXct('2015-02-19 10:50', tz='GMT'))
for (i in wind) {buoy2015_L1[ix, i]= NA}
buoy2015_wind_L1 <- subset(buoy2015_L1, select=c('datetime', wind))
buoy2015_vert_wind_L1 <- melt(buoy2015_wind_L1, id='datetime')

#ggplot(subset(buoy2015_vert_wind_L1, subset=(datetime>=as.POSIXct('2015-02-14', tz='GMT') & datetime<as.POSIXct('2015-03-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='late February 2015, v2', x='date', y=' ') +
  final_theme


#ggplot(subset(buoy2015_vert_wind, subset=(datetime>=as.POSIXct('2015-03-01', tz='GMT') & datetime<as.POSIXct('2015-03-16', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='early March 2015', x='date', y=' ') +
  final_theme

#ggplot(subset(buoy2015_vert_wind, subset=(datetime>=as.POSIXct('2015-03-16', tz='GMT') & datetime<as.POSIXct('2015-04-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='late March 2015', x='date', y=' ') +
  final_theme

#ggplot(subset(buoy2015_vert_wind, subset=(datetime>=as.POSIXct('2015-04-01', tz='GMT') & datetime<as.POSIXct('2015-04-16', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='early April 2015', x='date', y=' ') +
  final_theme

#ggplot(subset(buoy2015_vert_wind, subset=(datetime>=as.POSIXct('2015-04-16', tz='GMT') & datetime<as.POSIXct('2015-05-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='late April 2015', x='date', y=' ') +
  final_theme


#data gap until June

#ggplot(subset(buoy2015_vert_wind, subset=(datetime>=as.POSIXct('2015-06-01', tz='GMT') & datetime<as.POSIXct('2015-06-16', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='early June 2015', x='date', y=' ') +
  final_theme

#errant data beginning of June
#ggplot(subset(buoy2015_vert_wind, subset=(datetime>=as.POSIXct('2015-06-01', tz='GMT') & datetime<as.POSIXct('2015-06-05', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='early June 2015', x='date', y=' ') +
  final_theme

ix=which(buoy2015_L1$datetime>=as.POSIXct('2015-06-01 00:00', tz='GMT') & buoy2015_L1$datetime<=as.POSIXct('2015-06-04 10:00', tz='GMT'))
for (i in wind) {buoy2015_L1[ix, i]= NA}
buoy2015_wind_L1 <- subset(buoy2015_L1, select=c('datetime', wind))
buoy2015_vert_wind_L1 <- melt(buoy2015_wind_L1, id='datetime')

#ggplot(subset(buoy2015_vert_wind_L1, subset=(datetime>=as.POSIXct('2015-06-01', tz='GMT') & datetime<as.POSIXct('2015-06-16', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='early June 2015, v2', x='date', y=' ') +
  final_theme



#corr wind and windspd out 
wind2 <- c('WindSpdAv', 'WindVect', 'MaxWind', 'MaxWindDir')
buoy2015_wind <- subset(buoy2015_L1, select=c('datetime', wind2))
buoy2015_vert_wind <- melt(buoy2015_wind, id='datetime')


#ggplot(subset(buoy2015_vert_wind, subset=(datetime>=as.POSIXct('2015-06-16', tz='GMT') & datetime<as.POSIXct('2015-07-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='late June 2015', x='date', y=' ') +
  final_theme

#ggplot(subset(buoy2015_vert_wind, subset=(datetime>=as.POSIXct('2015-07-01', tz='GMT') & datetime<as.POSIXct('2015-07-16', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='early July 2015', x='date', y=' ') +
  final_theme

#ggplot(subset(buoy2015_vert_wind, subset=(datetime>=as.POSIXct('2015-07-16', tz='GMT') & datetime<as.POSIXct('2015-08-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='late July 2015', x='date', y=' ') +
  final_theme

#ggplot(subset(buoy2015_vert_wind, subset=(datetime>=as.POSIXct('2015-08-01', tz='GMT') & datetime<as.POSIXct('2015-08-16', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='early August 2015', x='date', y=' ') +
  final_theme

#ggplot(subset(buoy2015_vert_wind, subset=(datetime>=as.POSIXct('2015-08-16', tz='GMT') & datetime<as.POSIXct('2015-09-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='late August 2015', x='date', y=' ') +
  final_theme

#ggplot(subset(buoy2015_vert_wind, subset=(datetime>=as.POSIXct('2015-09-01', tz='GMT') & datetime<as.POSIXct('2015-09-16', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='early September 2015', x='date', y=' ') +
  final_theme

#ggplot(subset(buoy2015_vert_wind, subset=(datetime>=as.POSIXct('2015-09-16', tz='GMT') & datetime<as.POSIXct('2015-10-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='late September 2015', x='date', y=' ') +
  final_theme

#ggplot(subset(buoy2015_vert_wind, subset=(datetime>=as.POSIXct('2015-10-01', tz='GMT') & datetime<as.POSIXct('2015-10-16', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='early October 2015', x='date', y=' ') +
  final_theme

#ggplot(subset(buoy2015_vert_wind, subset=(datetime>=as.POSIXct('2015-10-16', tz='GMT') & datetime<as.POSIXct('2015-11-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='late October 2015', x='date', y=' ') +
  final_theme

#ggplot(subset(buoy2015_vert_wind, subset=(datetime>=as.POSIXct('2015-11-01', tz='GMT') & datetime<as.POSIXct('2015-11-16', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='early November 2015', x='date', y=' ') +
  final_theme

#ggplot(subset(buoy2015_vert_wind, subset=(datetime>=as.POSIXct('2015-11-16', tz='GMT') & datetime<as.POSIXct('2015-12-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='late Novebmer 2015', x='date', y=' ') +
  final_theme

#ggplot(subset(buoy2015_vert_wind, subset=(datetime>=as.POSIXct('2015-12-01', tz='GMT') & datetime<as.POSIXct('2015-12-16', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='early December 2015', x='date', y=' ') +
  final_theme

#ggplot(subset(buoy2015_vert_wind, subset=(datetime>=as.POSIXct('2015-12-16', tz='GMT') & datetime<as.POSIXct('2016-01-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='late December 2015', x='date', y=' ') +
  final_theme

#errant wind data 30-31
#ggplot(subset(buoy2015_vert_wind, subset=(datetime>=as.POSIXct('2015-12-29 12:00', tz='GMT') & datetime<as.POSIXct('2016-01-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='late December 2015', x='date', y=' ') +
  final_theme
#ggplot(subset(buoy2015_vert_wind, subset=(datetime>=as.POSIXct('2015-12-29 12:00', tz='GMT') & datetime<as.POSIXct('2015-12-30', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='late December 2015', x='date', y=' ') +
  final_theme
#ggplot(subset(buoy2015_vert_wind, subset=(datetime>=as.POSIXct('2015-12-31', tz='GMT') & datetime<as.POSIXct('2016-01-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='late December 2015', x='date', y=' ') +
  final_theme

ix=which(buoy2015_L1$datetime>=as.POSIXct('2015-12-29 17:30', tz='GMT') & buoy2015_L1$datetime<=as.POSIXct('2015-12-31 7:10', tz='GMT'))
for (i in wind) {buoy2015_L1[ix, i]= NA}
buoy2015_wind_L1 <- subset(buoy2015_L1, select=c('datetime', wind2))
buoy2015_vert_wind_L1 <- melt(buoy2015_wind_L1, id='datetime')

#ggplot(subset(buoy2015_vert_wind_L1, subset=(datetime>=as.POSIXct('2015-12-16', tz='GMT') & datetime<as.POSIXct('2016-01-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='late December 2015, v2', x='date', y=' ') +
  final_theme

buoy2015_wind_L1 <- subset(buoy2015_L1, select=c('datetime', wind))
buoy2015_vert_wind_L1 <- melt(buoy2015_wind_L1, id='datetime')

#ggplot(subset(buoy2015_vert_wind_L1, subset=(datetime>=as.POSIXct('2015-01-01', tz='GMT') & datetime<as.POSIXct('2016-01-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='2015, clean', x='date', y=' ') +
  final_theme





####chla data ####
chla <- c('Chlor_RFU', 'Chlor_UGL', 'SpecCond')

range(buoy2015_L1$Chlor_RFU, na.rm = T)
ix=which(buoy2015_L1$Chlor_RFU==-6999)
buoy2015_L1$Chlor_RFU[ix]=NA
ix=which(buoy2015_L1$Chlor_RFU==-3.3)
buoy2015_L1$Chlor_RFU[ix]=NA

range(buoy2015_L1$Chlor_UGL, na.rm = T)
ix=which(buoy2015_L1$Chlor_UGL==-6999)
buoy2015_L1$Chlor_UGL[ix]=NA
ix=which(buoy2015_L1$Chlor_UGL==6999)
buoy2015_L1$Chlor_UGL[ix]=NA
ix=which(buoy2015_L1$Chlor_UGL==183)
buoy2015_L1$Chlor_UGL[ix]=NA

range(buoy2015_L1$SpecCond, na.rm = T)
ix=which(buoy2015_L1$SpecCond==-6999)
buoy2015_L1$SpecCond[ix]=NA

buoy2015_chla <- subset(buoy2015_L1, select=c('datetime', chla))
buoy2015_vert_chla <- melt(buoy2015_chla, id='datetime')


####2015####
ggplot(subset(buoy2015_vert_chla, subset=(datetime>=as.POSIXct('2015-01-01', tz='GMT') & datetime<as.POSIXct('2016-01-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='2015, raw', x='date', y=' ') +
  final_theme

ggplot(subset(buoy2015_vert_wind, subset=(datetime>=as.POSIXct('2015-01-01', tz='GMT') & datetime<as.POSIXct('2015-01-16', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
geom_point() +   
  facet_grid(variable~., scales='free_y') +
  labs(title='early January 2015', x='date', y=' ') +
  final_theme

ggplot(subset(buoy2015_vert_wind, subset=(datetime>=as.POSIXct('2015-01-16', tz='GMT') & datetime<as.POSIXct('2015-02-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='late January 2015', x='date', y=' ') +
  final_theme

ggplot(subset(buoy2015_vert_wind, subset=(datetime>=as.POSIXct('2015-02-01', tz='GMT') & datetime<as.POSIXct('2015-02-14', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='early February 2015', x='date', y=' ') +
  final_theme

ggplot(subset(buoy2015_vert_wind, subset=(datetime>=as.POSIXct('2015-02-14', tz='GMT') & datetime<as.POSIXct('2015-03-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='late February 2015', x='date', y=' ') +
  final_theme

ggplot(subset(buoy2015_vert_wind, subset=(datetime>=as.POSIXct('2015-03-01', tz='GMT') & datetime<as.POSIXct('2015-03-16', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='early March 2015', x='date', y=' ') +
  final_theme

ggplot(subset(buoy2015_vert_wind, subset=(datetime>=as.POSIXct('2015-03-16', tz='GMT') & datetime<as.POSIXct('2015-04-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='late March 2015', x='date', y=' ') +
  final_theme

ggplot(subset(buoy2015_vert_wind, subset=(datetime>=as.POSIXct('2015-04-01', tz='GMT') & datetime<as.POSIXct('2015-04-16', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='early April 2015', x='date', y=' ') +
  final_theme

ggplot(subset(buoy2015_vert_wind, subset=(datetime>=as.POSIXct('2015-04-16', tz='GMT') & datetime<as.POSIXct('2015-05-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='late April 2015', x='date', y=' ') +
  final_theme


#data gap until June

ggplot(subset(buoy2015_vert_wind, subset=(datetime>=as.POSIXct('2015-06-01', tz='GMT') & datetime<as.POSIXct('2015-06-16', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='early June 2015', x='date', y=' ') +
  final_theme

ggplot(subset(buoy2015_vert_wind, subset=(datetime>=as.POSIXct('2015-06-16', tz='GMT') & datetime<as.POSIXct('2015-07-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='late June 2015', x='date', y=' ') +
  final_theme

ggplot(subset(buoy2015_vert_wind, subset=(datetime>=as.POSIXct('2015-07-01', tz='GMT') & datetime<as.POSIXct('2015-07-16', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='early July 2015', x='date', y=' ') +
  final_theme

ggplot(subset(buoy2015_vert_wind, subset=(datetime>=as.POSIXct('2015-07-16', tz='GMT') & datetime<as.POSIXct('2015-08-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='late July 2015', x='date', y=' ') +
  final_theme

ggplot(subset(buoy2015_vert_wind, subset=(datetime>=as.POSIXct('2015-08-01', tz='GMT') & datetime<as.POSIXct('2015-08-16', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='early August 2015', x='date', y=' ') +
  final_theme

ggplot(subset(buoy2015_vert_wind, subset=(datetime>=as.POSIXct('2015-08-16', tz='GMT') & datetime<as.POSIXct('2015-09-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='late August 2015', x='date', y=' ') +
  final_theme

ggplot(subset(buoy2015_vert_wind, subset=(datetime>=as.POSIXct('2015-09-01', tz='GMT') & datetime<as.POSIXct('2015-09-16', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='early September 2015', x='date', y=' ') +
  final_theme

ggplot(subset(buoy2015_vert_wind, subset=(datetime>=as.POSIXct('2015-09-16', tz='GMT') & datetime<as.POSIXct('2015-10-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='late September 2015', x='date', y=' ') +
  final_theme

ggplot(subset(buoy2015_vert_wind, subset=(datetime>=as.POSIXct('2015-10-01', tz='GMT') & datetime<as.POSIXct('2015-10-16', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='early October 2015', x='date', y=' ') +
  final_theme

ggplot(subset(buoy2015_vert_wind, subset=(datetime>=as.POSIXct('2015-10-16', tz='GMT') & datetime<as.POSIXct('2015-11-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='late October 2015', x='date', y=' ') +
  final_theme

ggplot(subset(buoy2015_vert_wind, subset=(datetime>=as.POSIXct('2015-11-01', tz='GMT') & datetime<as.POSIXct('2015-11-16', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='early November 2015', x='date', y=' ') +
  final_theme

ggplot(subset(buoy2015_vert_wind, subset=(datetime>=as.POSIXct('2015-11-16', tz='GMT') & datetime<as.POSIXct('2015-12-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='late Novebmer 2015', x='date', y=' ') +
  final_theme

ggplot(subset(buoy2015_vert_wind, subset=(datetime>=as.POSIXct('2015-12-01', tz='GMT') & datetime<as.POSIXct('2015-12-16', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='early December 2015', x='date', y=' ') +
  final_theme

ggplot(subset(buoy2015_vert_wind, subset=(datetime>=as.POSIXct('2015-12-16', tz='GMT') & datetime<as.POSIXct('2016-01-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='late December 2015', x='date', y=' ') +
  final_theme

ggplot(subset(buoy2015_vert_wind_L1, subset=(datetime>=as.POSIXct('2015-01-01', tz='GMT') & datetime<as.POSIXct('2016-01-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='2015, clean', x='date', y=' ') +
  final_theme

