#*****************************************************************
#*      Cary Institute of Ecosystem Studies (Millbrook, NY)      *
#*                                                               *
#* TITLE:   Sunapee_buoy_2014-2016_07Aug2017.r                   *
#* AUTHOR:  Bethel Steele                                        *
#* SYSTEM:  Lenovo ThinkCentre, Win 10, R 3.4.0                  *
#* DATE:    07Aug2017                                            *
#* PROJECT: Lake Sunapee Buoy Data Cleaning                      *
#* PURPOSE: create L0 and L1 data for buoy data 2014-2016 using  *
#*          similar methods to CCC and DR                        *
#* LAST UPDATED:                                                 *
#* BY:                                                           *
#*****************************************************************

library(gdata)
library(ggplot2)
library(ggthemes)
library(reshape2)

#bring in 2014-2016 raw data
setwd('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors')

buoy2014 <- read.csv('2014 Buoy Data.csv')
buoy2015 <- read.csv('2015 Buoy Data.csv')
hobo2015 <- read.csv('2015 Summer Hobo.csv')
buoy2016 <- read.csv('2016 Buoy Data.csv')

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

# merge all buoy data together
buoy1416 <- merge(buoy2014, buoy2015, all=T)
buoy1416 <- merge(buoy1416, buoy2016, all=T)

#format various date to datetime
buoy1416$minutes <- substrRight(buoy1416$Hr.Min, 2)
ix=which(buoy1416$minutes=='0')
buoy1416$minutes[ix]='00'

for (i in 1:nrow(buoy1416)) {
  if(nchar(buoy1416$Hr.Min[i]) ==4) { #if the number of characters is 4
    buoy1416$hour [i] <- substr(buoy1416$Hr.Min [i], 1,2) #then select the first two characters
    } else #assign it to the column
      if(nchar(buoy1416$Hr.Min [i]) ==3) { #if the number of characters is 3
      buoy1416$hour [i] <- substr(buoy1416$Hr.Min [i], 1, 1) #then select the first character
      } else #assign it to the column
        if(nchar(buoy1416$Hr.Min [i]) <2) { #if the number of characters is 2
        buoy1416$hour [i] <- '00'} #assign 00 to the column
}

buoy1416$time <- paste(buoy1416$hour, buoy1416$minutes, sep=':')

buoy1416$date <- as.Date(paste(buoy1416$Day, buoy1416$Year, sep = '-'), '%j-%Y')
buoy1416$datetime <- as.POSIXct(paste(buoy1416$date, buoy1416$time, sep=' '), format='%Y-%m-%d %H:%M', tz='GMT') #must state GMT so that daylight savings doesn't get messy

str(buoy1416)

#export L0 buoy data
#write.csv(buoy1416, 'L0_buoy_14-16.csv', row.names = F)

buoy1416_L1 <- buoy1416

####Thermisters ####
#print range of each variable and address obvious issues#
range(buoy1416_L1$TempC_0m, na.rm = T)
ix=which(buoy1416_L1$TempC_0m==-6999)
buoy1416_L1$TempC_0m[ix]=NA
ix=which(buoy1416_L1$TempC_0m>=50)
buoy1416_L1$TempC_0m[ix]=NA

range(buoy1416_L1$TempC_1m, na.rm = T)
ix=which(buoy1416_L1$TempC_1m>=50)
buoy1416_L1$TempC_1m[ix]=NA

range(buoy1416_L1$TempC_2m, na.rm = T)
ix=which(buoy1416_L1$TempC_2m>=50)
buoy1416_L1$TempC_2m[ix]=NA

range(buoy1416_L1$TempC_3m, na.rm = T)
ix=which(buoy1416_L1$TempC_3m>=50)
buoy1416_L1$TempC_3m[ix]=NA

range(buoy1416_L1$TempC_4m, na.rm = T)
ix=which(buoy1416_L1$TempC_4m>=50)
buoy1416_L1$TempC_4m[ix]=NA

range(buoy1416_L1$TempC_5m, na.rm = T)
ix=which(buoy1416_L1$TempC_5m>=50)
buoy1416_L1$TempC_5m[ix]=NA

range(buoy1416_L1$TempC_6m, na.rm = T)
ix=which(buoy1416_L1$TempC_6m>=50)
buoy1416_L1$TempC_6m[ix]=NA

range(buoy1416_L1$TempC_7m, na.rm = T)
ix=which(buoy1416_L1$TempC_7m>=50)
buoy1416_L1$TempC_7m[ix]=NA

range(buoy1416_L1$TempC_8m, na.rm = T)
ix=which(buoy1416_L1$TempC_8m==-6999)
buoy1416_L1$TempC_8m[ix]=NA
ix=which(buoy1416_L1$TempC_8m>=50)
buoy1416_L1$TempC_8m[ix]=NA

range(buoy1416_L1$TempC_9m, na.rm = T)
ix=which(buoy1416_L1$TempC_9m==-6999)
buoy1416_L1$TempC_9m[ix]=NA
ix=which(buoy1416_L1$TempC_9m>=50)
buoy1416_L1$TempC_9m[ix]=NA

range(buoy1416_L1$TempC_10m, na.rm = T)
ix=which(buoy1416_L1$TempC_10m==-6999)
buoy1416_L1$TempC_10m[ix]=NA
ix=which(buoy1416_L1$TempC_10m>=50)
buoy1416_L1$TempC_10m[ix]=NA

##transform data to vertical for display
buoy1416_temp <- subset(buoy1416_L1, select=c('datetime', "TempC_0m", "TempC_1m", "TempC_2m", "TempC_3m", "TempC_4m", "TempC_5m", "TempC_6m", "TempC_7m", "TempC_8m", "TempC_9m", "TempC_10m"))
buoy1416_vert_temp <- melt(buoy1416_temp, id='datetime')

####2014 thermisters####

#buoy deployed June 9, 2014 16:00
ggplot(subset(buoy1416_vert_temp, subset=(datetime>='2014-06-01' & datetime < '2014-06-16')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='early June 2014', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +   
  final_theme

ggplot(subset(buoy1416_vert_temp, subset=(datetime>='2014-06-16' & datetime < '2014-07-01')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='late June 2014', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +   
  final_theme

#no data early July

ggplot(subset(buoy1416_vert_temp, subset=(datetime>='2014-07-16' & datetime < '2014-08-01')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='late July 2014', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +   
  final_theme

ggplot(subset(buoy1416_vert_temp, subset=(datetime>='2014-08-01' & datetime < '2014-08-16')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='early August 2014', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +   
  final_theme

ggplot(subset(buoy1416_vert_temp, subset=(datetime>='2014-08-16' & datetime < '2014-09-01')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='late August 2014', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +   
  final_theme

ggplot(subset(buoy1416_vert_temp, subset=(datetime>='2014-09-01' & datetime < '2014-09-16')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='early Sept 2014', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +   
  final_theme

ggplot(subset(buoy1416_vert_temp, subset=(datetime>='2014-09-16' & datetime < '2014-10-01')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='late Sept 2014', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +   
  final_theme

ggplot(subset(buoy1416_vert_temp, subset=(datetime>='2014-10-01' & datetime < '2014-10-16')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='early October 2014', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +   
  final_theme

#buoy moved to harbor Oct 14 10:20

####2015 thermisters ####
#buoy moved to June 11 11:00
ggplot(subset(buoy1416_vert_temp, subset=(datetime>='2015-06-01' & datetime < '2015-06-16')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='early June 2015', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +   
  final_theme

#ggplot(subset(buoy1416_vert_temp, subset=(datetime>='2015-06-16' & datetime < '2015-07-01')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='late June 2015', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +   
  final_theme

#ggplot(subset(buoy1416_vert_temp, subset=(datetime>='2015-07-01' & datetime < '2015-07-16')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='early July 2015', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +   
  final_theme

#ggplot(subset(buoy1416_vert_temp, subset=(datetime>='2015-07-16' & datetime < '2015-08-01')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='late July 2015', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +   
  final_theme

#temp line removed Jul 22 13:00


####2016 thermisters####

#templine installed in Harbor 4/19/2016 10:00a
#ggplot(subset(buoy1416_vert_temp, subset=(datetime>='2016-04-16' & datetime < '2016-05-01')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='late April 2016', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +   
  final_theme

#buoy moved to summer location 5-3-2016
#ggplot(subset(buoy1416_vert_temp, subset=(datetime>='2016-05-01' & datetime < '2016-05-16')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='early May 2016', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +   
  final_theme

#ggplot(subset(buoy1416_vert_temp, subset=(datetime>='2016-05-16' & datetime < '2016-06-01')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='late May 2016', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +   
  final_theme

#ggplot(subset(buoy1416_vert_temp, subset=(datetime>='2016-06-01' & datetime < '2016-06-16')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='early June 2016', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +   
  final_theme

#ggplot(subset(buoy1416_vert_temp, subset=(datetime>='2016-06-16' & datetime < '2016-07-01')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='late June 2016', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +   
  final_theme

#ggplot(subset(buoy1416_vert_temp, subset=(datetime>='2016-07-01' & datetime < '2016-07-16')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='early July 2016', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +   
  final_theme

#ggplot(subset(buoy1416_vert_temp, subset=(datetime>='2016-07-16' & datetime < '2016-08-01')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='late July 2016', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +   
  final_theme

#ggplot(subset(buoy1416_vert_temp, subset=(datetime>='2016-08-01' & datetime < '2016-08-16')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='early August 2016', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +   
  final_theme

#ggplot(subset(buoy1416_vert_temp, subset=(datetime>='2016-08-16' & datetime < '2016-09-01')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='late August 2016', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +   
  final_theme

#ggplot(subset(buoy1416_vert_temp, subset=(datetime>='2016-09-01' & datetime < '2016-09-16')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='early Sept 2016', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +   
  final_theme

#ggplot(subset(buoy1416_vert_temp, subset=(datetime>='2016-09-16' & datetime < '2016-10-01')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='late Sept 2016', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +   
  final_theme

#ggplot(subset(buoy1416_vert_temp, subset=(datetime>='2016-10-01' & datetime < '2016-10-16')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='early October 2016', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +   
  final_theme

#buoy moved to harbor Oct 12 9:30

####2014 thermisters - data cleaning####
buoy1416_temp_L1 <- subset(buoy1416_L1, select=c('datetime', "TempC_0m", "TempC_1m", "TempC_2m", "TempC_3m", "TempC_4m", "TempC_5m", "TempC_6m", "TempC_7m", "TempC_8m", "TempC_9m", "TempC_10m"))
buoy1416_vert_temp_L1 <- melt(buoy1416_temp_L1, id='datetime')

#ggplot(subset(buoy1416_vert_temp, subset=(datetime>=as.POSIXct('2014-01-01', tz='GMT') & datetime < as.POSIXct('2015-01-01', tz='GMT'))), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='2014, raw', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +   
  final_theme

#July 24
#ggplot(subset(buoy1416_vert_temp, subset=(datetime>='2014-07-16' & datetime < '2014-08-01')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='late July 2014', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +   
  final_theme

#ggplot(subset(buoy1416_vert_temp, subset=(datetime>='2014-07-24' & datetime < '2014-07-25')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='July 24 2014 buoy visit - L0', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +   
  final_theme

ix=which(buoy1416_L1$datetime>=as.POSIXct('2014-07-24 9:00', tz='GMT') & buoy1416_L1$datetime<=as.POSIXct('2014-07-24 9:40', tz='GMT'))
for (i in alltemp) {buoy1416_L1[ix,i]=NA}

#ggplot(subset(buoy1416_vert_temp_L1, subset=(datetime>='2014-07-24' & datetime < '2014-07-25')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='July 24 2014 buoy visit - L1', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +   
  final_theme


#July 28
#ggplot(subset(buoy1416_vert_temp_L1, subset=(datetime>='2014-07-16' & datetime < '2014-08-01')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='late July 2014 - L1', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +   
  final_theme

ix=which(buoy1416_L1$datetime==as.POSIXct('2014-07-28 11:10:00', tz='GMT'))
for (i in alltemp) {buoy1416_L1[ix,i]=NA}

#ggplot(subset(buoy1416_vert_temp_L1, subset=(datetime>='2014-07-16' & datetime < '2014-08-01')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='late July 2014 - L1 v2', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +   
  final_theme


#August 13
#ggplot(subset(buoy1416_vert_temp, subset=(datetime>='2014-08-01' & datetime < '2014-08-16')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='early August 2014', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +   
  final_theme

#ggplot(subset(buoy1416_vert_temp, subset=(datetime>='2014-08-13' & datetime < '2014-08-14')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='August 13 zoom', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +   
  final_theme


#odd datapoints around 16.5 in the 10m thermister
#ggplot(subset(buoy1416_vert_temp, subset=(datetime>='2014-08-16' & datetime < '2014-09-01')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='late August 2014', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +   
  final_theme

#ggplot(subset(buoy1416_vert_temp, subset=(datetime>='2014-08-16' & datetime < '2014-09-01')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='late August 2014 - 15-18 degrees C', x='date', y='temp (deg C)') +
  coord_cartesian(ylim=c(15, 18)) +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +   
  final_theme


#oddballs 2014-09-18 16:50:00
#ggplot(subset(buoy1416_vert_temp, subset=(datetime>='2014-09-16' & datetime < '2014-10-01')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='late Sept 2014', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +   
  final_theme

ix=which(buoy1416_L1$datetime==as.POSIXct('2014-09-18 16:50:00', tz='GMT'))
for (i in alltemp) {buoy1416_L1[ix,i]=NA}

#ggplot(subset(buoy1416_vert_temp_L1, subset=(datetime>='2014-09-16' & datetime < '2014-10-01')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='late Sept 2014', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +   
  final_theme


#Oct 14 buoy moved to harbor Oct 14 10:20
#ggplot(subset(buoy1416_vert_temp, subset=(datetime>='2014-10-01' & datetime < '2014-10-16')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='early October 2014', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +   
  final_theme

#ggplot(subset(buoy1416_vert_temp, subset=(datetime>='2014-10-14' & datetime < '2014-10-14 12:00')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='October 14 2014 buoy move', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +   
  final_theme

#remove from 10:10
ix=which(buoy1416_L1$datetime==as.POSIXct('2014-10-14 10:10', tz='GMT'))
for (i in alltemp) {buoy1416_L1[ix,i]=NA}

#ggplot(subset(buoy1416_vert_temp_L1, subset=(datetime>='2014-10-14' & datetime < '2014-10-14 12:00')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='October 14 2014 buoy move', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +   
  final_theme

#ggplot(subset(buoy1416_vert_temp_L1, subset=(datetime>='2014-10-01' & datetime < '2014-10-16')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='early October 2014', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +   
  final_theme

#ggplot(subset(buoy1416_vert_temp_L1, subset=(datetime>=as.POSIXct('2014-01-01', tz='GMT') & datetime < as.POSIXct('2015-01-01', tz='GMT'))), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='2014, clean', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +   
  final_theme



####2015 thermisters - data cleaning####
buoy1416_temp_L1 <- subset(buoy1416_L1, select=c('datetime', "TempC_0m", "TempC_1m", "TempC_2m", "TempC_3m", "TempC_4m", "TempC_5m", "TempC_6m", "TempC_7m", "TempC_8m", "TempC_9m", "TempC_10m"))
buoy1416_vert_temp_L1 <- melt(buoy1416_temp_L1, id='datetime')

#ggplot(subset(buoy1416_vert_temp, subset=(datetime>=as.POSIXct('2015-01-01', tz='GMT') & datetime < as.POSIXct('2016-01-01', tz='GMT'))), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='2015, raw', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +   
  final_theme

#buoy moved to June 11 11:00
#ggplot(subset(buoy1416_vert_temp, subset=(datetime>='2015-06-01' & datetime < '2015-06-16')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='early June 2015', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +   
  final_theme

#ggplot(subset(buoy1416_vert_temp, subset=(datetime>='2015-06-11' & datetime < '2015-06-11 12:00')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='June 11 2015, buoy to summer location', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +   
  final_theme

ix=which(buoy1416_L1$datetime>=as.POSIXct('2015-01-01', tz='GMT') & buoy1416_L1$datetime<=as.POSIXct('2015-06-11 9:50', tz='GMT'))
for (i in alltemp) {buoy1416_L1[ix,i]=NA}

#ggplot(subset(buoy1416_vert_temp_L1, subset=(datetime>='2015-06-11' & datetime < '2015-06-11 12:00')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='June 11 2015, buoy to summer location, L1', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +   
  final_theme

#ggplot(subset(buoy1416_vert_temp_L1, subset=(datetime>='2015-06-01' & datetime < '2015-06-16')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='early June 2015, v2', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +   
  final_theme

# adress issues in 0m thermister - remove anything below 10 deg C
ix=which(buoy1416_L1$datetime>=as.POSIXct('2015-01-01', tz='GMT') & buoy1416_L1$datetime<as.POSIXct('2016-01-01', tz='GMT') & buoy1416_L1$TempC_0m<10)
buoy1416_L1$TempC_0m[ix]=NA

#ggplot(subset(buoy1416_vert_temp_L1, subset=(datetime>='2015-06-01' & datetime < '2015-06-16')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='early June 2015, v3', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +   
  final_theme

#0m issues
#ggplot(subset(buoy1416_vert_temp_L1, subset=(datetime>='2015-06-16' & datetime < '2015-07-01')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='late June 2015, v2', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +   
  final_theme

#remove data after initial break in data
#ggplot(subset(buoy1416_vert_temp_L1, subset=(datetime>='2015-06-24 18:00' & datetime < '2015-06-25 6:00')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='late June 2015, v2', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +   
  final_theme

#remove data after 3am until end of year
ix=which(buoy1416_L1$datetime>=as.POSIXct('2015-06-25 3:00', tz='GMT') & buoy1416_L1$datetime<as.POSIXct('2016-01-01', tz='GMT'))
for (i in alltemp) {buoy1416_L1[ix,i]=NA}

#and the odd data point in the 0m
ix=which(buoy1416_L1$datetime>=as.POSIXct('2015-06-25', tz='GMT') & buoy1416_L1$datetime<as.POSIXct('2015-06-26', tz='GMT') & buoy1416_L1$TempC_0m>25)
buoy1416_L1$TempC_0m[ix]=NA

#ggplot(subset(buoy1416_vert_temp_L1, subset=(datetime>='2015-06-16' & datetime < '2015-07-01')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='late June 2015, v3', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +   
  final_theme

#ggplot(subset(buoy1416_vert_temp_L1, subset=(datetime>=as.POSIXct('2015-01-01', tz='GMT') & datetime < as.POSIXct('2016-01-01', tz='GMT'))), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='2015, clean', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +   
  final_theme



####2016 thermisters - data cleaning####
buoy1416_temp_L1 <- subset(buoy1416_L1, select=c('datetime', "TempC_0m", "TempC_1m", "TempC_2m", "TempC_3m", "TempC_4m", "TempC_5m", "TempC_6m", "TempC_7m", "TempC_8m", "TempC_9m", "TempC_10m"))
buoy1416_vert_temp_L1 <- melt(buoy1416_temp_L1, id='datetime')

#ggplot(subset(buoy1416_vert_temp, subset=(datetime>=as.POSIXct('2016-01-01', tz='GMT') & datetime < as.POSIXct('2017-01-01', tz='GMT'))), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='2016, raw', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +   
  final_theme

#buoy moved to summer location 5-3-2016
#ggplot(subset(buoy1416_vert_temp, subset=(datetime>='2016-05-01' & datetime < '2016-05-16')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='early May 2016', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +   
  final_theme

#remove all data before 5-3-16
#ggplot(subset(buoy1416_vert_temp, subset=(datetime>=as.POSIXct('2016-05-03', tz='GMT') & datetime < as.POSIXct('2016-05-04', tz='GMT'))), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='May 3 2016', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +   
  final_theme

#ggplot(subset(buoy1416_vert_temp, subset=(datetime>=as.POSIXct('2016-05-03 8:00', tz='GMT') & datetime < as.POSIXct('2016-05-03 16:00', tz='GMT'))), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='May 3 2016', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +   
  final_theme

#remove through 11:50
ix=which(buoy1416_L1$datetime>=as.POSIXct('2016-01-01', tz='GMT') & buoy1416_L1$datetime<=as.POSIXct('2016-05-03 11:50', tz='GMT'))
for (i in alltemp) {buoy1416_L1[ix,i]=NA}

#ggplot(subset(buoy1416_vert_temp_L1, subset=(datetime>=as.POSIXct('2016-05-03', tz='GMT') & datetime < as.POSIXct('2016-05-04', tz='GMT'))), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='May 3 2016, clean', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +   
  final_theme

#ggplot(subset(buoy1416_vert_temp_L1, subset=(datetime>='2016-05-01' & datetime < '2016-05-16')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='early May 2016, v2', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +   
  final_theme

#remove 0m <=5 deg for entire year and the 15deg point in this view
ix=which(buoy1416_L1$datetime>=as.POSIXct('2016-01-01', tz='GMT') & buoy1416_L1$datetime<as.POSIXct('2017-01-01', tz='GMT') & buoy1416_L1$TempC_0m<=5)
buoy1416_L1$TempC_0m[ix]=NA
ix=which(buoy1416_L1$datetime>=as.POSIXct('2016-05-01', tz='GMT') & buoy1416_L1$datetime<as.POSIXct('2016-05-16', tz='GMT') & buoy1416_L1$TempC_0m==15)
buoy1416_L1$TempC_0m[ix]=NA

#ggplot(subset(buoy1416_vert_temp_L1, subset=(datetime>='2016-05-01' & datetime < '2016-05-16')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='early May 2016, v3', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +   
  final_theme

#errant points in 0m transducer
#ggplot(subset(buoy1416_vert_temp_L1, subset=(datetime>='2016-05-16' & datetime < '2016-06-01')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='late May 2016', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +   
  final_theme

#15, 9 from 5/16 forward
ix=which(buoy1416_L1$datetime>=as.POSIXct('2016-05-16', tz='GMT') & buoy1416_L1$datetime<as.POSIXct('2017-01-01', tz='GMT') & (buoy1416_L1$TempC_0m==9|buoy1416_L1$TempC_0m==15))
buoy1416_L1$TempC_0m[ix]=NA

#ggplot(subset(buoy1416_vert_temp_L1, subset=(datetime>='2016-05-16' & datetime < '2016-06-01')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='late May 2016, v2', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +   
  final_theme

# all thermisters issue on 2016-05-18 11:00:00
ix=which(buoy1416_L1$datetime==as.POSIXct('2016-05-18 11:00:00', tz='GMT'))
for (i in alltemp) {buoy1416_L1[ix,i]=NA}

#all thermisters errant 2016-05-19 05:10:00
ix=which(buoy1416_L1$datetime==as.POSIXct('2016-05-19 05:10:00', tz='GMT'))
for (i in alltemp) {buoy1416_L1[ix,i]=NA}

#ggplot(subset(buoy1416_vert_temp_L1, subset=(datetime>='2016-05-16' & datetime < '2016-06-01')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='late May 2016, v3', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +   
  final_theme

#errant 0m readings now through end of 2016
#ggplot(subset(buoy1416_vert_temp_L1, subset=(datetime>='2016-06-01' & datetime < '2016-06-16')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='early June 2016', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +   
  final_theme

ix=which(buoy1416_L1$datetime>=as.POSIXct('2016-06-01', tz='GMT') & buoy1416_L1$datetime<as.POSIXct('2017-01-01', tz='GMT') & (buoy1416_L1$TempC_0m>30))
buoy1416_L1$TempC_0m[ix]=NA

#ggplot(subset(buoy1416_vert_temp_L1, subset=(datetime>='2016-06-01' & datetime < '2016-06-16')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='early June 2016, v2', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +   
  final_theme

# 6/14 and 15 thermister line out of water
#ggplot(subset(buoy1416_vert_temp_L1, subset=(datetime>=as.POSIXct('2016-06-15', tz='GMT') & datetime < as.POSIXct('2016-06-16', tz='GMT'))), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='June 15 thermister redeployment', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +   
  final_theme

# remove 6-14-16 8:10 to 6-15-16 11:30
ix=which(buoy1416_L1$datetime>=as.POSIXct('2016-06-14 8:10', tz='GMT') & buoy1416_L1$datetime<=as.POSIXct('2016-06-15 11:30', tz='GMT'))
for (i in alltemp) {buoy1416_L1[ix,i]=NA}

#ggplot(subset(buoy1416_vert_temp_L1, subset=(datetime>='2016-06-01' & datetime < '2016-06-16')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='early June 2016, v3', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +   
  final_theme

#errant 0m readings (already removed)
#ggplot(subset(buoy1416_vert_temp_L1, subset=(datetime>='2016-06-16' & datetime < '2016-07-01')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='late June 2016, v2', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +   
  final_theme

#errant 0m readings (already removed)
#ggplot(subset(buoy1416_vert_temp_L1, subset=(datetime>='2016-07-01' & datetime < '2016-07-16')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='early July 2016, v2', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +   
  final_theme

#errant 0m readings (already removed)
#ggplot(subset(buoy1416_vert_temp_L1, subset=(datetime>='2016-07-16' & datetime < '2016-08-01')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='late July 2016, v2', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +   
  final_theme

#errant 0m readings (already removed)
#ggplot(subset(buoy1416_vert_temp_L1, subset=(datetime>='2016-08-01' & datetime < '2016-08-16')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='early August 2016, v2', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +   
  final_theme

#errant 0m readings (already removed)
#ggplot(subset(buoy1416_vert_temp_L1, subset=(datetime>='2016-08-16' & datetime < '2016-09-01')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='late August 2016, v2', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +   
  final_theme

#additional errant point at 0m 2016-08-18 07:10:00
ix=which(buoy1416_L1$datetime==as.POSIXct('2016-08-18 07:10:00', tz='GMT'))
buoy1416_L1$TempC_0m[ix]=NA

#ggplot(subset(buoy1416_vert_temp_L1, subset=(datetime>='2016-08-16' & datetime < '2016-09-01')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='late August 2016, v3', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +   
  final_theme

#errant 0m readings (already removed)
#ggplot(subset(buoy1416_vert_temp_L1, subset=(datetime>='2016-09-01' & datetime < '2016-09-16')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='early Sept 2016, v2', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +   
  final_theme

#errant 0m readings (already removed)
#ggplot(subset(buoy1416_vert_temp_L1, subset=(datetime>='2016-09-16' & datetime < '2016-10-01')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='late Sept 2016, v2', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +   
  final_theme

#errant 0m readings (already removed)
#ggplot(subset(buoy1416_vert_temp_L1, subset=(datetime>='2016-10-01' & datetime < '2016-10-16')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='early October 2016, v2', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +   
  final_theme

#buoy moved to harbor Oct 12 9:30
ix=which(buoy1416_L1$datetime>=as.POSIXct('2016-10-12 9:40', tz='GMT'))
for (i in alltemp) {buoy1416_L1 [ix, i] = NA}

#ggplot(subset(buoy1416_vert_temp_L1, subset=(datetime>=as.POSIXct('2016-01-01', tz='GMT') & datetime < as.POSIXct('2017-01-01', tz='GMT'))), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='2016, clean', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +   
  final_theme


#### hobo line ####
hobo2015$datetime <- as.POSIXct(hobo2015$Date.Time, format='%m/%d/%Y %H:%M', tz='GMT')

hobo_temp <- subset(hobo2015, select=c('datetime', 'TempC_1m',  'TempC_2m', 'TempC_3m', 'TempC_4m', 'TempC_5m', 'TempC_6m', 'TempC_7m', 'TempC_8m', 'TempC_9m'))

hobo_vert <- melt(hobo_temp, id='datetime')

####2015 hobo line####
#ggplot(subset(hobo_vert, subset=(datetime>='2015-01-01' & datetime < '2016-01-01')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='hobo temp sensor summer 2015', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +   
  final_theme

#ggplot(subset(hobo_vert, subset=(datetime>='2015-08-01' & datetime < '2015-08-16')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='hobo temp early aug 2015', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +   
  final_theme

#ggplot(subset(hobo_vert, subset=(datetime>='2015-08-16' & datetime < '2015-09-01')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='hobo temp late aug 2015', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +   
  final_theme

#ggplot(subset(hobo_vert, subset=(datetime>='2015-09-01' & datetime < '2015-09-16')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='hobo temp early sept 2015', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +   
  final_theme

#ggplot(subset(hobo_vert, subset=(datetime>='2015-09-16' & datetime < '2015-10-01')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='hobo temp late sept 2015', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +   
  final_theme


hobo_temp <- rename.vars(hobo_temp, from=c('TempC_1m',  'TempC_2m', 'TempC_3m', 'TempC_4m', 'TempC_5m', 'TempC_6m', 'TempC_7m', 'TempC_8m', 'TempC_9m'),
                         to=c('HoboTempC_1m',  'HoboTempC_2m', 'HoboTempC_3m', 'HoboTempC_4m', 'HoboTempC_5m', 'HoboTempC_6m', 'HoboTempC_7m', 'HoboTempC_8m', 'HoboTempC_9m'))

buoy1416_L1 <- merge(buoy1416_L1, hobo_temp, by='datetime', all=T)

####Upper DO ####
#print range of each variable and address obvious issues#
range(buoy1416_L1$DOTempC, na.rm = T)
ix=which(buoy1416_L1$DOTempC==-6999)
buoy1416_L1$DOTempC[ix]=NA

range(buoy1416_L1$DOSat, na.rm=T)
ix=which(buoy1416_L1$DOSat==-6999)
buoy1416_L1$DOSat[ix]=NA

range(buoy1416_L1$DOppm, na.rm = T)
ix=which(buoy1416_L1$DOppm==-6999)
buoy1416_L1$DOppm[ix]=NA

##transform data to vertical for display
buoy1416_updo <- subset(buoy1416_L1, select=c('datetime', upDO))
buoy1416_vert_updo <- melt(buoy1416_updo, id='datetime')

####2014 upper do####
buoy1416_updo_L1 <- subset(buoy1416_L1, select=c('datetime', upDO))
buoy1416_vert_updo_L1 <- melt(buoy1416_updo_L1, id='datetime')

#ggplot(subset(buoy1416_vert_updo, subset=(datetime>=as.POSIXct('2014-01-01', tz='GMT') & datetime < as.POSIXct('2015-01-01', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='2014, raw', x='date', y='') +
  final_theme

#ggplot(subset(buoy1416_vert_updo, subset=(datetime>=as.POSIXct('2014-01-01', tz='GMT') & datetime < as.POSIXct('2014-01-16', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='early January 2014', x='date', y='') +
  final_theme

#ggplot(subset(buoy1416_vert_updo, subset=(datetime>=as.POSIXct('2014-01-16', tz='GMT') & datetime < as.POSIXct('2014-02-01', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='late January 2014', x='date', y='') +
  final_theme

#ggplot(subset(buoy1416_vert_updo, subset=(datetime>=as.POSIXct('2014-02-01', tz='GMT') & datetime < as.POSIXct('2014-02-14', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='early February 2014', x='date', y='') +
  final_theme

#ggplot(subset(buoy1416_vert_updo, subset=(datetime>=as.POSIXct('2014-02-14', tz='GMT') & datetime < as.POSIXct('2014-03-01', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='late February 2014', x='date', y='') +
  final_theme

#ggplot(subset(buoy1416_vert_updo, subset=(datetime>=as.POSIXct('2014-03-01', tz='GMT') & datetime < as.POSIXct('2014-03-16', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='early March 2014', x='date', y='') +
  final_theme

#ggplot(subset(buoy1416_vert_updo, subset=(datetime>=as.POSIXct('2014-03-16', tz='GMT') & datetime < as.POSIXct('2014-04-01', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='late March 2014', x='date', y='') +
  final_theme

#ggplot(subset(buoy1416_vert_updo, subset=(datetime>=as.POSIXct('2014-04-01', tz='GMT') & datetime < as.POSIXct('2014-04-16', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='early April 2014', x='date', y='') +
  final_theme

#data logger reset. no data until 6/9 - that is also when the buoy was moved to the deep spot
buoy1416_L1$location <- as.character('')
ix=which(buoy1416_L1$datetime>=as.POSIXct('2014-01-01', tz='GMT') & buoy1416_L1$datetime<as.POSIXct('2014-06-09', tz='GMT'))
buoy1416_L1$location [ix] = 'harbor'

#ggplot(subset(buoy1416_vert_updo, subset=(datetime>=as.POSIXct('2014-06-01', tz='GMT') & datetime < as.POSIXct('2014-06-16', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='early June 2014', x='date', y='') +
  final_theme

#ggplot(subset(buoy1416_vert_updo, subset=(datetime>=as.POSIXct('2014-06-16', tz='GMT') & datetime < as.POSIXct('2014-07-01', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='late June 2014', x='date', y='') +
  final_theme

#ggplot(subset(buoy1416_vert_updo, subset=(datetime>=as.POSIXct('2014-07-01', tz='GMT') & datetime < as.POSIXct('2014-07-16', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='early July 2014', x='date', y='') +
  final_theme

#ggplot(subset(buoy1416_vert_updo, subset=(datetime>=as.POSIXct('2014-07-16', tz='GMT') & datetime < as.POSIXct('2014-08-01', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='late July 2014', x='date', y='') +
  final_theme

#ggplot(subset(buoy1416_vert_updo, subset=(datetime>=as.POSIXct('2014-08-01', tz='GMT') & datetime < as.POSIXct('2014-08-16', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='early August 2014', x='date', y='') +
  final_theme

#ggplot(subset(buoy1416_vert_updo, subset=(datetime>=as.POSIXct('2014-08-16', tz='GMT') & datetime < as.POSIXct('2014-09-01', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='late August 2014', x='date', y='') +
  final_theme

#ggplot(subset(buoy1416_vert_updo, subset=(datetime>=as.POSIXct('2014-09-01', tz='GMT') & datetime < as.POSIXct('2014-09-16', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='early September 2014', x='date', y='') +
  final_theme

#ggplot(subset(buoy1416_vert_updo, subset=(datetime>=as.POSIXct('2014-09-16', tz='GMT') & datetime < as.POSIXct('2014-10-01', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='late September 2014', x='date', y='') +
  final_theme

#Sept 19 errant point
#ggplot(subset(buoy1416_vert_updo, subset=(datetime>=as.POSIXct('2014-09-18 15:00:00', tz='GMT') & datetime < as.POSIXct('2014-09-19', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='late September 2014', x='date', y='') +
  final_theme

ix=which(buoy1416_L1$datetime==as.POSIXct('2014-09-18 16:50', tz='GMT'))
for (i in upDO) {buoy1416_L1 [ix,i]=NA}

#ggplot(subset(buoy1416_vert_updo_L1, subset=(datetime>=as.POSIXct('2014-09-16', tz='GMT') & datetime < as.POSIXct('2014-10-01', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='late September 2014, v2', x='date', y='') +
  final_theme

#ggplot(subset(buoy1416_vert_updo, subset=(datetime>=as.POSIXct('2014-10-01', tz='GMT') & datetime < as.POSIXct('2014-10-16', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='early October 2014', x='date', y='') +
  final_theme

# buoy moved to harbor. 
#ggplot(subset(buoy1416_vert_updo, subset=(datetime>=as.POSIXct('2014-10-14', tz='GMT') & datetime < as.POSIXct('2014-10-15', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='October 14, buoy to harbor', x='date', y='') +
  final_theme

ix=which(buoy1416_L1$datetime>=as.POSIXct('2014-10-14 10:00', tz='GMT') & buoy1416_L1$datetime<=as.POSIXct('2014-10-14 11:30', tz='GMT'))
for (i in upDO) {buoy1416_L1[ix,i]=NA}

ix=which(buoy1416_L1$datetime>=as.POSIXct('2014-06-09', tz='GMT') & buoy1416_L1$datetime<as.POSIXct('2014-10-14 10:10', tz='GMT'))
buoy1416_L1$location [ix] = 'deep spot'

ix=which(buoy1416_L1$datetime>=as.POSIXct('2014-10-14 10:10', tz='GMT') & buoy1416_L1$datetime<as.POSIXct('2015-01-01', tz='GMT'))
buoy1416_L1$location [ix] = 'harbor'


#ggplot(subset(buoy1416_vert_updo_L1, subset=(datetime>=as.POSIXct('2014-10-01', tz='GMT') & datetime < as.POSIXct('2014-10-16', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='early October 2014, v2', x='date', y='') +
  final_theme


#ggplot(subset(buoy1416_vert_updo, subset=(datetime>=as.POSIXct('2014-10-16', tz='GMT') & datetime < as.POSIXct('2014-11-01', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='late October 2014', x='date', y='') +
  final_theme

#ggplot(subset(buoy1416_vert_updo, subset=(datetime>=as.POSIXct('2014-11-01', tz='GMT') & datetime < as.POSIXct('2014-11-16', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='early November 2014', x='date', y='') +
  final_theme

#ggplot(subset(buoy1416_vert_updo, subset=(datetime>=as.POSIXct('2014-11-16', tz='GMT') & datetime < as.POSIXct('2014-12-01', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='late November 2014', x='date', y='') +
  final_theme

#ggplot(subset(buoy1416_vert_updo, subset=(datetime>=as.POSIXct('2014-12-01', tz='GMT') & datetime < as.POSIXct('2014-12-16', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='early December 2014', x='date', y='') +
  final_theme

#ggplot(subset(buoy1416_vert_updo, subset=(datetime>=as.POSIXct('2014-12-16', tz='GMT') & datetime < as.POSIXct('2015-01-01', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='late December 2014', x='date', y='') +
  final_theme


buoy1416_updo_L1 <- subset(buoy1416_L1, select=c('datetime', upDO, 'location'))
buoy1416_vert_updo_L1 <- melt(buoy1416_updo_L1, id=c('datetime', 'location'))

#ggplot(subset(buoy1416_vert_updo_L1, subset=(datetime>=as.POSIXct('2014-01-01', tz='GMT') & datetime < as.POSIXct('2015-01-01', tz='GMT'))), aes(x=datetime, y=value, color=location)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='2014, clean', x='date', y='') +
  scale_color_colorblind() +
  final_theme

####2015 upper do####
buoy1416_updo_L1 <- subset(buoy1416_L1, select=c('datetime', upDO))
buoy1416_vert_updo_L1 <- melt(buoy1416_updo_L1, id='datetime')

#ggplot(subset(buoy1416_vert_updo, subset=(datetime>=as.POSIXct('2015-01-01', tz='GMT') & datetime < as.POSIXct('2016-01-01', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='2015, raw', x='date', y='') +
  final_theme

#ggplot(subset(buoy1416_vert_updo, subset=(datetime>=as.POSIXct('2015-01-01', tz='GMT') & datetime < as.POSIXct('2015-01-16', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='early January 2015', x='date', y='') +
  final_theme

#errant points around jan 13/14
#ggplot(subset(buoy1416_vert_updo, subset=(datetime>=as.POSIXct('2015-01-13', tz='GMT') & datetime < as.POSIXct('2015-01-14', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='early January 2015', x='date', y='') +
  final_theme

#errant points on Jan 13 1:40-2
ix=which(buoy1416_L1$datetime>=as.POSIXct('2015-01-13 13:40', tz='GMT') & buoy1416_L1$datetime<=as.POSIXct('2015-01-13 14:00', tz='GMT'))
for (i in upDO) {buoy1416_L1 [ix,i] = NA}

#ggplot(subset(buoy1416_vert_updo_L1, subset=(datetime>=as.POSIXct('2015-01-13', tz='GMT') & datetime < as.POSIXct('2015-01-14', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='early January 2015', x='date', y='') +
  final_theme

#ggplot(subset(buoy1416_vert_updo_L1, subset=(datetime>=as.POSIXct('2015-01-01', tz='GMT') & datetime < as.POSIXct('2015-01-16', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='early January 2015, v2', x='date', y='') +
  final_theme


#ggplot(subset(buoy1416_vert_updo, subset=(datetime>=as.POSIXct('2015-01-16', tz='GMT') & datetime < as.POSIXct('2015-02-01', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='late January 2015', x='date', y='') +
  final_theme

#ggplot(subset(buoy1416_vert_updo, subset=(datetime>=as.POSIXct('2015-02-01', tz='GMT') & datetime < as.POSIXct('2015-02-14', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='early February 2015', x='date', y='') +
  final_theme

#ggplot(subset(buoy1416_vert_updo, subset=(datetime>=as.POSIXct('2015-02-14', tz='GMT') & datetime < as.POSIXct('2015-03-01', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='late February 2015', x='date', y='') +
  final_theme

#ggplot(subset(buoy1416_vert_updo, subset=(datetime>=as.POSIXct('2015-03-01', tz='GMT') & datetime < as.POSIXct('2015-03-16', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='early March 2015', x='date', y='') +
  final_theme

#ggplot(subset(buoy1416_vert_updo, subset=(datetime>=as.POSIXct('2015-03-16', tz='GMT') & datetime < as.POSIXct('2015-04-01', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='late March 2015', x='date', y='') +
  final_theme

#ggplot(subset(buoy1416_vert_updo, subset=(datetime>=as.POSIXct('2015-04-01', tz='GMT') & datetime < as.POSIXct('2015-04-16', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='early April 2015', x='date', y='') +
  final_theme

#ggplot(subset(buoy1416_vert_updo, subset=(datetime>=as.POSIXct('2015-04-16', tz='GMT') & datetime < as.POSIXct('2015-05-01', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='late April 2015', x='date', y='') +
  final_theme

#remove artifacts of buoy move/going off line
#ggplot(subset(buoy1416_vert_updo, subset=(datetime>=as.POSIXct('2015-04-22 6:00', tz='GMT') & datetime < as.POSIXct('2015-05-01 16:00', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='late April 2015', x='date', y='') +
  final_theme

ix=which(buoy1416_L1$datetime>=as.POSIXct('2015-04-22 11:00', tz='GMT') & buoy1416_L1$datetime<=as.POSIXct('2015-04-22 11:10', tz='GMT'))
for (i in upDO) {buoy1416_L1 [ix,i] = NA}

#ggplot(subset(buoy1416_vert_updo_L1, subset=(datetime>=as.POSIXct('2015-04-22 6:00', tz='GMT') & datetime < as.POSIXct('2015-05-01 16:00', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='late April 2015', x='date', y='') +
  final_theme

#ggplot(subset(buoy1416_vert_updo_L1, subset=(datetime>=as.POSIXct('2015-04-16', tz='GMT') & datetime < as.POSIXct('2015-05-01', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='late April 2015, v2', x='date', y='') +
  final_theme

#ggplot(subset(buoy1416_vert_updo, subset=(datetime>=as.POSIXct('2015-06-01', tz='GMT') & datetime < as.POSIXct('2015-06-16', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='early June 2015', x='date', y='') +
  final_theme

#remove data from 1st to 3rd, before sensors correctly reporting, then short data gap until the 4th
#ggplot(subset(buoy1416_vert_updo, subset=(datetime>=as.POSIXct('2015-06-03', tz='GMT') & datetime < as.POSIXct('2015-06-05', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='early June 2015', x='date', y='') +
  final_theme

ix=which(buoy1416_L1$datetime>=as.POSIXct('2015-06-01', tz='GMT') & buoy1416_L1$datetime<=as.POSIXct('2015-06-04 10:00', tz='GMT'))
for (i in upDO) {buoy1416_L1 [ix,i] = NA}

#ggplot(subset(buoy1416_vert_updo_L1, subset=(datetime>=as.POSIXct('2015-06-04', tz='GMT') & datetime < as.POSIXct('2015-06-04 12:00', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='early June 2015', x='date', y='') +
  final_theme

#ggplot(subset(buoy1416_vert_updo_L1, subset=(datetime>=as.POSIXct('2015-06-01', tz='GMT') & datetime < as.POSIXct('2015-06-16', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='early June 2015, v2', x='date', y='') +
  final_theme

#remove artifacts of buoy move
#ggplot(subset(buoy1416_vert_updo, subset=(datetime>=as.POSIXct('2015-06-11', tz='GMT') & datetime < as.POSIXct('2015-06-12', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='early June 2015', x='date', y='') +
  final_theme

ix=which(buoy1416_L1$datetime>=as.POSIXct('2015-06-11 9:50', tz='GMT') & buoy1416_L1$datetime<=as.POSIXct('2015-06-11 10:00', tz='GMT'))
for (i in upDO) {buoy1416_L1 [ix,i] = NA}

# moved to deep spot on jun 11
ix=which(buoy1416_L1$datetime>=as.POSIXct('2015-01-01', tz='GMT') & buoy1416_L1$datetime<as.POSIXct('2015-06-11 10:00', tz='GMT'))
buoy1416_L1$location [ix] = 'harbor'

buoy1416_updo_L1 <- subset(buoy1416_L1, select=c('datetime', 'location', upDO))
buoy1416_vert_updo_L1 <- melt(buoy1416_updo_L1, id=c('datetime', 'location'))

#ggplot(subset(buoy1416_vert_updo_L1, subset=(datetime>=as.POSIXct('2015-06-01', tz='GMT') & datetime < as.POSIXct('2015-06-16', tz='GMT'))), aes(x=datetime, y=value, color=location)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='early June 2015', x='date', y='') +
  scale_color_colorblind() +
  final_theme


#ggplot(subset(buoy1416_vert_updo, subset=(datetime>=as.POSIXct('2015-06-16', tz='GMT') & datetime < as.POSIXct('2015-07-01', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='late June 2015', x='date', y='') +
  final_theme

#ggplot(subset(buoy1416_vert_updo, subset=(datetime>=as.POSIXct('2015-07-01', tz='GMT') & datetime < as.POSIXct('2015-07-16', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='early July 2015', x='date', y='') +
  final_theme

#ggplot(subset(buoy1416_vert_updo, subset=(datetime>=as.POSIXct('2015-07-16', tz='GMT') & datetime < as.POSIXct('2015-08-01', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='late July 2015', x='date', y='') +
  final_theme

#ggplot(subset(buoy1416_vert_updo, subset=(datetime>=as.POSIXct('2015-08-01', tz='GMT') & datetime < as.POSIXct('2015-08-16', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='early August 2015', x='date', y='') +
  final_theme

#ggplot(subset(buoy1416_vert_updo, subset=(datetime>=as.POSIXct('2015-08-13', tz='GMT') & datetime < as.POSIXct('2015-08-14', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='early August 2015', x='date', y='') +
  final_theme


#ggplot(subset(buoy1416_vert_updo, subset=(datetime>=as.POSIXct('2015-08-16', tz='GMT') & datetime < as.POSIXct('2015-09-01', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='late August 2015', x='date', y='') +
  final_theme

#ggplot(subset(buoy1416_vert_updo, subset=(datetime>=as.POSIXct('2015-09-01', tz='GMT') & datetime < as.POSIXct('2015-09-16', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='early September 2015', x='date', y='') +
  final_theme

#ggplot(subset(buoy1416_vert_updo, subset=(datetime>=as.POSIXct('2015-09-16', tz='GMT') & datetime < as.POSIXct('2015-10-01', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='late September 2015', x='date', y='') +
  final_theme

#errant point around the 19th
#ggplot(subset(buoy1416_vert_updo, subset=(datetime>=as.POSIXct('2015-09-19', tz='GMT') & datetime < as.POSIXct('2015-09-20', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='late September 2015', x='date', y='') +
  final_theme

ix=which(buoy1416_L1$datetime==as.POSIXct('2015-09-19 5:50', tz='GMT'))
for (i in upDO) {buoy1416_L1 [ix,i] = NA}

#ggplot(subset(buoy1416_vert_updo_L1, subset=(datetime>=as.POSIXct('2015-09-19', tz='GMT') & datetime < as.POSIXct('2015-09-20', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='late September 2015', x='date', y='') +
  final_theme

#ggplot(subset(buoy1416_vert_updo_L1, subset=(datetime>=as.POSIXct('2015-09-16', tz='GMT') & datetime < as.POSIXct('2015-10-01', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='late September 2015, v2', x='date', y='') +
  final_theme


#ggplot(subset(buoy1416_vert_updo, subset=(datetime>=as.POSIXct('2015-10-01', tz='GMT') & datetime < as.POSIXct('2015-10-16', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='early October 2015', x='date', y='') +
  final_theme

# buoy moved to harbor. 
#ggplot(subset(buoy1416_vert_updo, subset=(datetime>=as.POSIXct('2015-10-08', tz='GMT') & datetime < as.POSIXct('2015-10-09', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='October 08, buoy to harbor', x='date', y='') +
  final_theme

#remove artifacts of buoy movement
ix=which(buoy1416_L1$datetime>=as.POSIXct('2015-10-08 10:40', tz='GMT') & buoy1416_L1$datetime<=as.POSIXct('2015-10-08 11:20', tz='GMT'))
for (i in upDO) {buoy1416_L1[ix,i]=NA}

ix=which(buoy1416_L1$datetime>=as.POSIXct('2015-06-11 10:00', tz='GMT') & buoy1416_L1$datetime<as.POSIXct('2015-10-08 10:40', tz='GMT'))
buoy1416_L1$location [ix] = 'deep spot'

ix=which(buoy1416_L1$datetime>=as.POSIXct('2015-10-08 11:20', tz='GMT') & buoy1416_L1$datetime<as.POSIXct('2016-01-01', tz='GMT'))
buoy1416_L1$location [ix] = 'harbor'

buoy1416_updo_L1 <- subset(buoy1416_L1, select=c('datetime', 'location', upDO))
buoy1416_vert_updo_L1 <- melt(buoy1416_updo_L1, id=c('datetime', 'location'))

#ggplot(subset(buoy1416_vert_updo_L1, subset=(datetime>=as.POSIXct('2015-10-01', tz='GMT') & datetime < as.POSIXct('2015-10-16', tz='GMT'))), aes(x=datetime, y=value, color=location)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='early October 2015, v2', x='date', y='') +
  scale_color_colorblind() +
  final_theme

#remove data after buoy move -error in sensor
ix=which(buoy1416_L1$datetime>=as.POSIXct('2015-10-08 11:30', tz='GMT') & buoy1416_L1$datetime<=as.POSIXct('2015-12-31 23:50', tz='GMT'))
for (i in upDO) {buoy1416_L1[ix,i]=NA}

#ggplot(subset(buoy1416_vert_updo_L1, subset=(datetime>=as.POSIXct('2015-10-01', tz='GMT') & datetime < as.POSIXct('2015-10-16', tz='GMT'))), aes(x=datetime, y=value, color=location)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='early October 2015, v3', x='date', y='') +
  scale_color_colorblind() +
  final_theme


#ggplot(subset(buoy1416_vert_updo_L1, subset=(datetime>=as.POSIXct('2015-01-01', tz='GMT') & datetime < as.POSIXct('2016-01-01', tz='GMT'))), aes(x=datetime, y=value, color=location)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='2015, clean', x='date', y='') +
  scale_color_colorblind() +
  final_theme

####2016 upper do####
buoy1416_updo_L1 <- subset(buoy1416_L1, select=c('datetime', upDO))
buoy1416_vert_updo_L1 <- melt(buoy1416_updo_L1, id='datetime')

#ggplot(subset(buoy1416_vert_updo, subset=(datetime>=as.POSIXct('2016-01-01', tz='GMT') & datetime < as.POSIXct('2017-01-01', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='2016, raw', x='date', y='') +
  final_theme


#ggplot(subset(buoy1416_vert_updo, subset=(datetime>=as.POSIXct('2016-04-16', tz='GMT') & datetime < as.POSIXct('2016-05-01', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='late April 2016', x='date', y='') +
  final_theme

#remove data prior to sensor deployment and artifacts of sensor install
#ggplot(subset(buoy1416_vert_updo, subset=(datetime>=as.POSIXct('2016-04-19', tz='GMT') & datetime < as.POSIXct('2016-04-20', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='late April 2016', x='date', y='') +
  final_theme

#prior to deployment
ix=which(buoy1416_L1$datetime>=as.POSIXct('2016-01-01', tz='GMT') & buoy1416_L1$datetime<=as.POSIXct('2016-04-19 10:00', tz='GMT'))
for (i in upDO) {buoy1416_L1 [ix,i] = NA}

#artifacts of install
ix=which(buoy1416_L1$datetime>=as.POSIXct('2016-04-19 10:10', tz='GMT') & buoy1416_L1$datetime<=as.POSIXct('2016-04-19 11:20', tz='GMT'))
for (i in upDO) {buoy1416_L1 [ix,i] = NA}

#ggplot(subset(buoy1416_vert_updo_L1, subset=(datetime>=as.POSIXct('2016-04-19', tz='GMT') & datetime < as.POSIXct('2016-04-20', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='late April 2016', x='date', y='') +
  final_theme

#ggplot(subset(buoy1416_vert_updo_L1, subset=(datetime>=as.POSIXct('2016-04-16', tz='GMT') & datetime < as.POSIXct('2016-05-01', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='late April 2016, v2', x='date', y='') +
  final_theme


#ggplot(subset(buoy1416_vert_updo, subset=(datetime>=as.POSIXct('2016-05-01', tz='GMT') & datetime < as.POSIXct('2016-05-16', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='early May 2016', x='date', y='') +
  final_theme

#May 3 buoy moved to deep spot
#ggplot(subset(buoy1416_vert_updo, subset=(datetime>=as.POSIXct('2016-05-03', tz='GMT') & datetime < as.POSIXct('2016-05-04', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='early May 2016', x='date', y='') +
  final_theme

ix=which(buoy1416_L1$datetime>=as.POSIXct('2016-05-03 10:10', tz='GMT') & buoy1416_L1$datetime<=as.POSIXct('2016-05-03 12:30', tz='GMT'))
for (i in upDO) {buoy1416_L1 [ix,i] = NA}

#ggplot(subset(buoy1416_vert_updo_L1, subset=(datetime>=as.POSIXct('2016-05-03', tz='GMT') & datetime < as.POSIXct('2016-05-04', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='early May 2016', x='date', y='') +
  final_theme

#add buoy location
ix=which(buoy1416_L1$datetime>=as.POSIXct('2016-01-01', tz='GMT') & buoy1416_L1$datetime<as.POSIXct('2016-05-03 10:10', tz='GMT'))
buoy1416_L1$location [ix] = 'harbor'

buoy1416_updo_L1 <- subset(buoy1416_L1, select=c('datetime', 'location', upDO))
buoy1416_vert_updo_L1 <- melt(buoy1416_updo_L1, id=c('datetime', 'location'))

#ggplot(subset(buoy1416_vert_updo_L1, subset=(datetime>=as.POSIXct('2016-05-01', tz='GMT') & datetime < as.POSIXct('2016-05-16', tz='GMT'))), aes(x=datetime, y=value, color=location)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='early May 2016', x='date', y='') +
  scale_color_colorblind() +
  final_theme


#ggplot(subset(buoy1416_vert_updo, subset=(datetime>=as.POSIXct('2016-05-16', tz='GMT') & datetime < as.POSIXct('2016-06-01', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='late May 2016', x='date', y='') +
  final_theme

#presumed errant data before gap
#ggplot(subset(buoy1416_vert_updo, subset=(datetime>=as.POSIXct('2016-05-21', tz='GMT') & datetime < as.POSIXct('2016-05-22', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='late May 2016', x='date', y='') +
  final_theme

#ggplot(subset(buoy1416_vert_updo, subset=(datetime>=as.POSIXct('2016-05-25', tz='GMT') & datetime < as.POSIXct('2016-05-26', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='late May 2016', x='date', y='') +
  final_theme

ix=which(buoy1416_L1$datetime>=as.POSIXct('2016-05-21 10:40', tz='GMT') & buoy1416_L1$datetime<as.POSIXct('2016-05-22', tz='GMT'))
for (i in upDO) {buoy1416_L1 [ix,i] = NA}

#ggplot(subset(buoy1416_vert_updo_L1, subset=(datetime>=as.POSIXct('2016-05-21', tz='GMT') & datetime < as.POSIXct('2016-05-22', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='late May 2016', x='date', y='') +
  final_theme

#ggplot(subset(buoy1416_vert_updo_L1, subset=(datetime>=as.POSIXct('2016-05-16', tz='GMT') & datetime < as.POSIXct('2016-06-01', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='late May 2016, v2', x='date', y='') +
  final_theme

#ggplot(subset(buoy1416_vert_updo, subset=(datetime>=as.POSIXct('2016-06-01', tz='GMT') & datetime < as.POSIXct('2016-06-16', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='early June 2016', x='date', y='') +
  final_theme

#remove points at beginning of first data gap
#ggplot(subset(buoy1416_vert_updo, subset=(datetime>=as.POSIXct('2016-06-03', tz='GMT') & datetime < as.POSIXct('2016-06-04', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='early June 2016', x='date', y='') +
  final_theme

ix=which(buoy1416_L1$datetime>=as.POSIXct('2016-06-03 9:00', tz='GMT') & buoy1416_L1$datetime<=as.POSIXct('2016-06-03 12:00', tz='GMT'))
for (i in upDO) {buoy1416_L1 [ix,i] = NA}

#ggplot(subset(buoy1416_vert_updo_L1, subset=(datetime>=as.POSIXct('2016-06-01', tz='GMT') & datetime < as.POSIXct('2016-06-16', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='early June 2016', x='date', y='') +
  final_theme

#remove artifacts on June 7 from do cleaning
#ggplot(subset(buoy1416_vert_updo, subset=(datetime>=as.POSIXct('2016-06-07', tz='GMT') & datetime < as.POSIXct('2016-06-08', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='early June 2016', x='date', y='') +
  final_theme

ix=which(buoy1416_L1$datetime>=as.POSIXct('2016-06-07 6:00', tz='GMT') & buoy1416_L1$datetime<=as.POSIXct('2016-06-07 7:00', tz='GMT'))
for (i in upDO) {buoy1416_L1 [ix,i] = NA}

#ggplot(subset(buoy1416_vert_updo_L1, subset=(datetime>=as.POSIXct('2016-06-01', tz='GMT') & datetime < as.POSIXct('2016-06-16', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='early June 2016', x='date', y='') +
  final_theme

#couple of points on june 10
#ggplot(subset(buoy1416_vert_updo, subset=(datetime>=as.POSIXct('2016-06-10', tz='GMT') & datetime < as.POSIXct('2016-06-11', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='early June 2016', x='date', y='') +
  final_theme

ix=which(buoy1416_L1$datetime>=as.POSIXct('2016-06-10', tz='GMT') & buoy1416_L1$datetime<as.POSIXct('2016-06-11', tz='GMT'))
for (i in upDO) {buoy1416_L1 [ix,i] = NA}

#ggplot(subset(buoy1416_vert_updo_L1, subset=(datetime>=as.POSIXct('2016-06-01', tz='GMT') & datetime < as.POSIXct('2016-06-16', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='early June 2016, v2', x='date', y='') +
  final_theme


#ggplot(subset(buoy1416_vert_updo, subset=(datetime>=as.POSIXct('2016-06-16', tz='GMT') & datetime < as.POSIXct('2016-07-01', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='late June 2016', x='date', y='') +
  final_theme

#ggplot(subset(buoy1416_vert_updo, subset=(datetime>=as.POSIXct('2016-07-01', tz='GMT') & datetime < as.POSIXct('2016-07-16', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='early July 2016', x='date', y='') +
  final_theme

#ggplot(subset(buoy1416_vert_updo, subset=(datetime>=as.POSIXct('2016-07-16', tz='GMT') & datetime < as.POSIXct('2016-08-01', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='late July 2016', x='date', y='') +
  final_theme

#ggplot(subset(buoy1416_vert_updo, subset=(datetime>=as.POSIXct('2016-08-01', tz='GMT') & datetime < as.POSIXct('2016-08-16', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='early August 2016', x='date', y='') +
  final_theme

#ggplot(subset(buoy1416_vert_updo, subset=(datetime>=as.POSIXct('2016-08-13', tz='GMT') & datetime < as.POSIXct('2016-08-14', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='early August 2016', x='date', y='') +
  final_theme


#ggplot(subset(buoy1416_vert_updo, subset=(datetime>=as.POSIXct('2016-08-16', tz='GMT') & datetime < as.POSIXct('2016-09-01', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='late August 2016', x='date', y='') +
  final_theme

#ggplot(subset(buoy1416_vert_updo, subset=(datetime>=as.POSIXct('2016-09-01', tz='GMT') & datetime < as.POSIXct('2016-09-16', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='early September 2016', x='date', y='') +
  final_theme

#ggplot(subset(buoy1416_vert_updo, subset=(datetime>=as.POSIXct('2016-09-16', tz='GMT') & datetime < as.POSIXct('2016-10-01', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='late September 2016', x='date', y='') +
  final_theme

#ggplot(subset(buoy1416_vert_updo, subset=(datetime>=as.POSIXct('2016-10-01', tz='GMT') & datetime < as.POSIXct('2016-10-16', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='early October 2016', x='date', y='') +
  final_theme

# buoy moved to harbor 10/12 
#ggplot(subset(buoy1416_vert_updo, subset=(datetime>=as.POSIXct('2016-10-12', tz='GMT') & datetime < as.POSIXct('2016-10-13', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='October 08, buoy to harbor', x='date', y='') +
  final_theme

#remove artifacts of buoy movement
ix=which(buoy1416_L1$datetime>=as.POSIXct('2016-10-12 9:50', tz='GMT') & buoy1416_L1$datetime<=as.POSIXct('2016-10-12 11:40', tz='GMT'))
for (i in upDO) {buoy1416_L1[ix,i]=NA}

ix=which(buoy1416_L1$datetime>=as.POSIXct('2016-05-03 12:30', tz='GMT') & buoy1416_L1$datetime<as.POSIXct('2016-10-12 9:50', tz='GMT'))
buoy1416_L1$location [ix] = 'deep spot'

ix=which(buoy1416_L1$datetime>=as.POSIXct('2016-10-12 11:50', tz='GMT') & buoy1416_L1$datetime<as.POSIXct('2017-01-01', tz='GMT'))
buoy1416_L1$location [ix] = 'harbor'

buoy1416_updo_L1 <- subset(buoy1416_L1, select=c('datetime', 'location', upDO))
buoy1416_vert_updo_L1 <- melt(buoy1416_updo_L1, id=c('datetime', 'location'))

#ggplot(subset(buoy1416_vert_updo_L1, subset=(datetime>=as.POSIXct('2016-10-01', tz='GMT') & datetime < as.POSIXct('2016-10-16', tz='GMT'))), aes(x=datetime, y=value, color=location)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='early October 2016, v2', x='date', y='') +
  scale_color_colorblind() +
  final_theme

#ggplot(subset(buoy1416_vert_updo, subset=(datetime>=as.POSIXct('2016-10-16', tz='GMT') & datetime < as.POSIXct('2016-11-01', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='late October 2016', x='date', y='') +
  final_theme

#ggplot(subset(buoy1416_vert_updo, subset=(datetime>=as.POSIXct('2016-11-01', tz='GMT') & datetime < as.POSIXct('2016-11-16', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='early November 2016', x='date', y='') +
  final_theme

#ggplot(subset(buoy1416_vert_updo, subset=(datetime>=as.POSIXct('2016-11-16', tz='GMT') & datetime < as.POSIXct('2016-12-01', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='late November 2016', x='date', y='') +
  final_theme

#ggplot(subset(buoy1416_vert_updo, subset=(datetime>=as.POSIXct('2016-12-01', tz='GMT') & datetime < as.POSIXct('2016-12-16', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='early December 2016', x='date', y='') +
  final_theme

#ggplot(subset(buoy1416_vert_updo, subset=(datetime>=as.POSIXct('2016-12-16', tz='GMT') & datetime < as.POSIXct('2017-01-01', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='late December 2016', x='date', y='') +
  final_theme


#ggplot(subset(buoy1416_vert_updo_L1, subset=(datetime>=as.POSIXct('2016-01-01', tz='GMT') & datetime < as.POSIXct('2017-01-01', tz='GMT'))), aes(x=datetime, y=value, color=location)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='2016, clean', x='date', y='') +
  scale_color_colorblind() +
  final_theme


####Lower DO ####
#print range of each variable and address obvious issues#
range(buoy1416_L1$DOLoTempC, na.rm = T)
ix=which(buoy1416_L1$DOLoTempC==-6999)
buoy1416_L1$DOLoTempC[ix]=NA

range(buoy1416_L1$DOLowSat, na.rm=T)
ix=which(buoy1416_L1$DOLowSat==-6999)
buoy1416_L1$DOLowSat[ix]=NA

range(buoy1416_L1$DOLowPPM, na.rm = T)
ix=which(buoy1416_L1$DOLowPPM==-6999)
buoy1416_L1$DOLowPPM[ix]=NA

##transform data to vertical for display
buoy1416_lowdo <- subset(buoy1416_L1, select=c('datetime', lowDO))
buoy1416_vert_lowdo <- melt(buoy1416_lowdo, id='datetime')

####2014 lower do####
buoy1416_lowdo_L1 <- subset(buoy1416_L1, select=c('datetime', lowDO))
buoy1416_vert_lowdo_L1 <- melt(buoy1416_lowdo_L1, id='datetime')

#ggplot(subset(buoy1416_vert_lowdo, subset=(datetime>=as.POSIXct('2014-01-01', tz='GMT') & datetime < as.POSIXct('2015-01-01', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='2014, raw', x='date', y='') +
  final_theme


#ggplot(subset(buoy1416_vert_lowdo, subset=(datetime>=as.POSIXct('2014-06-16', tz='GMT') & datetime < as.POSIXct('2014-07-01', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='late June 2014', x='date', y='') +
  final_theme

#remove the artifacts of deployment
#ggplot(subset(buoy1416_vert_lowdo, subset=(datetime>=as.POSIXct('2014-06-23', tz='GMT') & datetime < as.POSIXct('2014-06-24', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='late June 2014', x='date', y='') +
  final_theme

ix=which(buoy1416_L1$datetime>=as.POSIXct('2014-06-23 14:10', tz='GMT') & buoy1416_L1$datetime<=as.POSIXct('2014-06-23 14:50', tz='GMT'))
for (i in lowDO) {buoy1416_L1 [ix,i]=NA}

#ggplot(subset(buoy1416_vert_lowdo_L1, subset=(datetime>=as.POSIXct('2014-06-23', tz='GMT') & datetime < as.POSIXct('2014-06-24', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='late June 2014', x='date', y='') +
  final_theme

#ggplot(subset(buoy1416_vert_lowdo_L1, subset=(datetime>=as.POSIXct('2014-06-16', tz='GMT') & datetime < as.POSIXct('2014-07-01', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='late June 2014, v2', x='date', y='') +
  final_theme


#ggplot(subset(buoy1416_vert_lowdo, subset=(datetime>=as.POSIXct('2014-07-01', tz='GMT') & datetime < as.POSIXct('2014-07-16', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='early July 2014', x='date', y='') +
  final_theme

#ggplot(subset(buoy1416_vert_lowdo, subset=(datetime>=as.POSIXct('2014-07-16', tz='GMT') & datetime < as.POSIXct('2014-08-01', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='late July 2014', x='date', y='') +
  final_theme

#remove flat lines through until Jul 28
#ggplot(subset(buoy1416_vert_lowdo, subset=(datetime>=as.POSIXct('2014-07-17', tz='GMT') & datetime < as.POSIXct('2014-07-18', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='late July 2014', x='date', y='') +
  final_theme

#ggplot(subset(buoy1416_vert_lowdo, subset=(datetime>=as.POSIXct('2014-07-28', tz='GMT') & datetime < as.POSIXct('2014-07-29', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='late July 2014', x='date', y='') +
  final_theme

ix=which(buoy1416_L1$datetime>=as.POSIXct('2014-07-17 9:10', tz='GMT') & buoy1416_L1$datetime<=as.POSIXct('2014-07-28 11:20', tz='GMT'))
for (i in lowDO) {buoy1416_L1 [ix,i]=NA}

#ggplot(subset(buoy1416_vert_lowdo_L1, subset=(datetime>=as.POSIXct('2014-07-16', tz='GMT') & datetime < as.POSIXct('2014-08-01', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='late July 2014, v2', x='date', y='') +
  final_theme


#ggplot(subset(buoy1416_vert_lowdo, subset=(datetime>=as.POSIXct('2014-08-01', tz='GMT') & datetime < as.POSIXct('2014-08-16', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='early August 2014', x='date', y='') +
  final_theme

#ggplot(subset(buoy1416_vert_lowdo, subset=(datetime>=as.POSIXct('2014-08-16', tz='GMT') & datetime < as.POSIXct('2014-09-01', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='late August 2014', x='date', y='') +
  final_theme

#ggplot(subset(buoy1416_vert_lowdo, subset=(datetime>=as.POSIXct('2014-09-01', tz='GMT') & datetime < as.POSIXct('2014-09-16', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='early September 2014', x='date', y='') +
  final_theme

#ggplot(subset(buoy1416_vert_lowdo, subset=(datetime>=as.POSIXct('2014-09-16', tz='GMT') & datetime < as.POSIXct('2014-10-01', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='late September 2014', x='date', y='') +
  final_theme

#Sept 19 errant point
#ggplot(subset(buoy1416_vert_lowdo, subset=(datetime>=as.POSIXct('2014-09-18 15:00:00', tz='GMT') & datetime < as.POSIXct('2014-09-19', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='late September 2014', x='date', y='') +
  final_theme

ix=which(buoy1416_L1$datetime==as.POSIXct('2014-09-18 16:50', tz='GMT'))
for (i in lowDO) {buoy1416_L1 [ix,i]=NA}

#ggplot(subset(buoy1416_vert_lowdo_L1, subset=(datetime>=as.POSIXct('2014-09-16', tz='GMT') & datetime < as.POSIXct('2014-10-01', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='late September 2014, v2', x='date', y='') +
  final_theme


#ggplot(subset(buoy1416_vert_lowdo, subset=(datetime>=as.POSIXct('2014-10-01', tz='GMT') & datetime < as.POSIXct('2014-10-16', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='early October 2014', x='date', y='') +
  final_theme

# buoy moved to harbor. 
#ggplot(subset(buoy1416_vert_lowdo, subset=(datetime>=as.POSIXct('2014-10-14', tz='GMT') & datetime < as.POSIXct('2014-10-15', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='October 14, buoy to harbor', x='date', y='') +
  final_theme

ix=which(buoy1416_L1$datetime>=as.POSIXct('2014-10-14 10:00', tz='GMT') & buoy1416_L1$datetime<as.POSIXct('2015-01-01', tz='GMT'))
for (i in lowDO) {buoy1416_L1[ix,i]=NA}


#ggplot(subset(buoy1416_vert_lowdo_L1, subset=(datetime>=as.POSIXct('2014-10-01', tz='GMT') & datetime < as.POSIXct('2014-10-16', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='early October 2014, v2', x='date', y='') +
  final_theme


#ggplot(subset(buoy1416_vert_lowdo_L1, subset=(datetime>=as.POSIXct('2014-01-01', tz='GMT') & datetime < as.POSIXct('2015-01-01', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='2014, clean', x='date', y='') +
  final_theme

####2015 lower do####
buoy1416_lowdo_L1 <- subset(buoy1416_L1, select=c('datetime', lowDO))
buoy1416_vert_lowdo_L1 <- melt(buoy1416_lowdo_L1, id='datetime')

#ggplot(subset(buoy1416_vert_lowdo, subset=(datetime>=as.POSIXct('2015-01-01', tz='GMT') & datetime < as.POSIXct('2016-01-01', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='2015, raw', x='date', y='') +
  final_theme

#ggplot(subset(buoy1416_vert_lowdo, subset=(datetime>=as.POSIXct('2015-06-01', tz='GMT') & datetime < as.POSIXct('2015-06-16', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='early June 2015', x='date', y='') +
  final_theme

#remove artifacts of buoy move
#ggplot(subset(buoy1416_vert_lowdo, subset=(datetime>=as.POSIXct('2015-06-11', tz='GMT') & datetime < as.POSIXct('2015-06-12', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='early June 2015', x='date', y='') +
  final_theme

#note, that time ends later because of equilibration of do readings
ix=which(buoy1416_L1$datetime>=as.POSIXct('2015-01-01', tz='GMT') & buoy1416_L1$datetime<as.POSIXct('2015-06-11 11:10', tz='GMT'))
for (i in lowDO) {buoy1416_L1 [ix,i] = NA}


#ggplot(subset(buoy1416_vert_lowdo_L1, subset=(datetime>=as.POSIXct('2015-06-01', tz='GMT') & datetime < as.POSIXct('2015-06-16', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='early June 2015, v2', x='date', y='') +
  final_theme


#ggplot(subset(buoy1416_vert_lowdo, subset=(datetime>=as.POSIXct('2015-06-16', tz='GMT') & datetime < as.POSIXct('2015-07-01', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='late June 2015', x='date', y='') +
  final_theme

#ggplot(subset(buoy1416_vert_lowdo, subset=(datetime>=as.POSIXct('2015-07-01', tz='GMT') & datetime < as.POSIXct('2015-07-16', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='early July 2015', x='date', y='') +
  final_theme

#ggplot(subset(buoy1416_vert_lowdo, subset=(datetime>=as.POSIXct('2015-07-16', tz='GMT') & datetime < as.POSIXct('2015-08-01', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='late July 2015', x='date', y='') +
  final_theme

#ggplot(subset(buoy1416_vert_lowdo, subset=(datetime>=as.POSIXct('2015-08-01', tz='GMT') & datetime < as.POSIXct('2015-08-16', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='early August 2015', x='date', y='') +
  final_theme

#ggplot(subset(buoy1416_vert_lowdo, subset=(datetime>=as.POSIXct('2015-08-13', tz='GMT') & datetime < as.POSIXct('2015-08-14', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='early August 2015', x='date', y='') +
  final_theme


#ggplot(subset(buoy1416_vert_lowdo, subset=(datetime>=as.POSIXct('2015-08-16', tz='GMT') & datetime < as.POSIXct('2015-09-01', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='late August 2015', x='date', y='') +
  final_theme

#ggplot(subset(buoy1416_vert_lowdo, subset=(datetime>=as.POSIXct('2015-09-01', tz='GMT') & datetime < as.POSIXct('2015-09-16', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='early September 2015', x='date', y='') +
  final_theme

#ggplot(subset(buoy1416_vert_lowdo, subset=(datetime>=as.POSIXct('2015-09-16', tz='GMT') & datetime < as.POSIXct('2015-10-01', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='late September 2015', x='date', y='') +
  final_theme

#errant point around the 19th
#ggplot(subset(buoy1416_vert_lowdo, subset=(datetime>=as.POSIXct('2015-09-19', tz='GMT') & datetime < as.POSIXct('2015-09-20', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='late September 2015', x='date', y='') +
  final_theme

ix=which(buoy1416_L1$datetime==as.POSIXct('2015-09-19 5:50', tz='GMT'))
for (i in lowdo) {buoy1416_L1 [ix,i] = NA}

#ggplot(subset(buoy1416_vert_lowdo_L1, subset=(datetime>=as.POSIXct('2015-09-19', tz='GMT') & datetime < as.POSIXct('2015-09-20', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='late September 2015', x='date', y='') +
  final_theme

#ggplot(subset(buoy1416_vert_lowdo_L1, subset=(datetime>=as.POSIXct('2015-09-16', tz='GMT') & datetime < as.POSIXct('2015-10-01', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='late September 2015, v2', x='date', y='') +
  final_theme


#ggplot(subset(buoy1416_vert_lowdo, subset=(datetime>=as.POSIXct('2015-10-01', tz='GMT') & datetime < as.POSIXct('2015-10-16', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='early October 2015', x='date', y='') +
  final_theme

# buoy moved to harbor. 
#ggplot(subset(buoy1416_vert_lowdo, subset=(datetime>=as.POSIXct('2015-10-08', tz='GMT') & datetime < as.POSIXct('2015-10-09', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='October 08, buoy to harbor', x='date', y='') +
  final_theme

#remove artifacts of buoy movement
ix=which(buoy1416_L1$datetime>=as.POSIXct('2015-10-08 10:40', tz='GMT') & buoy1416_L1$datetime<=as.POSIXct('2015-10-08 11:20', tz='GMT'))
for (i in lowdo) {buoy1416_L1[ix,i]=NA}

#ggplot(subset(buoy1416_vert_lowdo_L1, subset=(datetime>=as.POSIXct('2015-10-01', tz='GMT') & datetime < as.POSIXct('2015-10-16', tz='GMT'))), aes(x=datetime, y=value, color=location)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='early October 2015, v2', x='date', y='') +
  scale_color_colorblind() +
  final_theme

#remove data after buoy move -error in sensor
ix=which(buoy1416_L1$datetime>=as.POSIXct('2015-10-08 11:30', tz='GMT') & buoy1416_L1$datetime<=as.POSIXct('2015-12-31 23:50', tz='GMT'))
for (i in lowdo) {buoy1416_L1[ix,i]=NA}

#ggplot(subset(buoy1416_vert_lowdo_L1, subset=(datetime>=as.POSIXct('2015-10-01', tz='GMT') & datetime < as.POSIXct('2015-10-16', tz='GMT'))), aes(x=datetime, y=value, color=location)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='early October 2015, v3', x='date', y='') +
  scale_color_colorblind() +
  final_theme


#ggplot(subset(buoy1416_vert_lowdo_L1, subset=(datetime>=as.POSIXct('2015-01-01', tz='GMT') & datetime < as.POSIXct('2016-01-01', tz='GMT'))), aes(x=datetime, y=value, color=location)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='2015, clean', x='date', y='') +
  scale_color_colorblind() +
  final_theme

####2016 lower do####
buoy1416_lowdo_L1 <- subset(buoy1416_L1, select=c('datetime', lowDO))
buoy1416_vert_lowdo_L1 <- melt(buoy1416_lowdo_L1, id='datetime')

#ggplot(subset(buoy1416_vert_lowdo, subset=(datetime>=as.POSIXct('2016-01-01', tz='GMT') & datetime < as.POSIXct('2017-01-01', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='2016, raw', x='date', y='') +
  final_theme


#ggplot(subset(buoy1416_vert_lowdo, subset=(datetime>=as.POSIXct('2016-04-16', tz='GMT') & datetime < as.POSIXct('2016-05-01', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='late April 2016', x='date', y='') +
  final_theme

#ggplot(subset(buoy1416_vert_lowdo, subset=(datetime>=as.POSIXct('2016-05-01', tz='GMT') & datetime < as.POSIXct('2016-05-16', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='early May 2016', x='date', y='') +
  final_theme

#May 3 buoy moved to deep spot
#ggplot(subset(buoy1416_vert_lowdo, subset=(datetime>=as.POSIXct('2016-05-03', tz='GMT') & datetime < as.POSIXct('2016-05-04', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='early May 2016', x='date', y='') +
  final_theme

ix=which(buoy1416_L1$datetime>=as.POSIXct('2016-01-01', tz='GMT') & buoy1416_L1$datetime<=as.POSIXct('2016-05-03 11:50', tz='GMT'))
for (i in lowDO) {buoy1416_L1 [ix,i] = NA}

#ggplot(subset(buoy1416_vert_lowdo_L1, subset=(datetime>=as.POSIXct('2016-05-03', tz='GMT') & datetime < as.POSIXct('2016-05-04', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='early May 2016', x='date', y='') +
  final_theme

#ggplot(subset(buoy1416_vert_lowdo_L1, subset=(datetime>=as.POSIXct('2016-05-01', tz='GMT') & datetime < as.POSIXct('2016-05-16', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='early May 2016, v2', x='date', y='') +
  final_theme


#ggplot(subset(buoy1416_vert_lowdo, subset=(datetime>=as.POSIXct('2016-05-16', tz='GMT') & datetime < as.POSIXct('2016-06-01', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='late May 2016', x='date', y='') +
  final_theme

ix=which(buoy1416_L1$DOLoTempC<8 & buoy1416_L1$datetime >= as.POSIXct('2016-05-16', tz='GMT') & buoy1416_L1$datetime <= as.POSIXct('2016-06-01', tz='GMT'))
for (i in lowDO) {buoy1416_L1 [ix,i] = NA}
ix=which(buoy1416_L1$DOLoTempC>50 & buoy1416_L1$datetime >= as.POSIXct('2016-05-16', tz='GMT') & buoy1416_L1$datetime <= as.POSIXct('2016-06-01', tz='GMT'))
for (i in lowDO) {buoy1416_L1 [ix,i] = NA}

#ggplot(subset(buoy1416_vert_lowdo_L1, subset=(datetime>=as.POSIXct('2016-05-16', tz='GMT') & datetime < as.POSIXct('2016-06-01', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='late May 2016', x='date', y='') +
  final_theme

#ggplot(subset(buoy1416_vert_lowdo, subset=(datetime>=as.POSIXct('2016-05-21', tz='GMT') & datetime < as.POSIXct('2016-05-22', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='late May 2016', x='date', y='') +
  final_theme

ix=which(buoy1416_L1$datetime>=as.POSIXct('2016-05-21 14:10', tz='GMT') & buoy1416_L1$datetime<as.POSIXct('2016-05-21 14:30', tz='GMT'))
for (i in lowDO) {buoy1416_L1 [ix,i] = NA}

#ggplot(subset(buoy1416_vert_lowdo_L1, subset=(datetime>=as.POSIXct('2016-05-21', tz='GMT') & datetime < as.POSIXct('2016-05-22', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='late May 2016', x='date', y='') +
  final_theme

#ggplot(subset(buoy1416_vert_lowdo_L1, subset=(datetime>=as.POSIXct('2016-05-16', tz='GMT') & datetime < as.POSIXct('2016-06-01', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='late May 2016, v2', x='date', y='') +
  final_theme


#ggplot(subset(buoy1416_vert_lowdo, subset=(datetime>=as.POSIXct('2016-06-01', tz='GMT') & datetime < as.POSIXct('2016-06-16', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='early June 2016', x='date', y='') +
  final_theme

#remove points at beginning of first data gap
#ggplot(subset(buoy1416_vert_lowdo, subset=(datetime>=as.POSIXct('2016-06-01', tz='GMT') & datetime < as.POSIXct('2016-06-09', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='early June 2016', x='date', y='') +
  final_theme

#ggplot(subset(buoy1416_vert_lowdo, subset=(datetime>=as.POSIXct('2016-06-03', tz='GMT') & datetime < as.POSIXct('2016-06-04', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='early June 2016', x='date', y='') +
  final_theme

ix=which(buoy1416_L1$datetime>=as.POSIXct('2016-06-01', tz='GMT') & buoy1416_L1$datetime<=as.POSIXct('2016-06-09', tz='GMT') & buoy1416_L1$DOLowSat<75)
for (i in lowDO) {buoy1416_L1 [ix,i] = NA}

ix=which(buoy1416_L1$datetime>=as.POSIXct('2016-06-03 09:00', tz='GMT') & buoy1416_L1$datetime<=as.POSIXct('2016-06-03 12:00', tz='GMT'))
for (i in lowDO) {buoy1416_L1 [ix,i] = NA}


#ggplot(subset(buoy1416_vert_lowdo_L1, subset=(datetime>=as.POSIXct('2016-06-01', tz='GMT') & datetime < as.POSIXct('2016-06-16', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='early June 2016', x='date', y='') +
  final_theme

#couple of points on june 10
#ggplot(subset(buoy1416_vert_lowdo, subset=(datetime>=as.POSIXct('2016-06-10', tz='GMT') & datetime < as.POSIXct('2016-06-11', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='early June 2016', x='date', y='') +
  final_theme

ix=which(buoy1416_L1$datetime>=as.POSIXct('2016-06-10', tz='GMT') & buoy1416_L1$datetime<as.POSIXct('2016-06-11', tz='GMT'))
for (i in lowDO) {buoy1416_L1 [ix,i] = NA}

#ggplot(subset(buoy1416_vert_lowdo_L1, subset=(datetime>=as.POSIXct('2016-06-01', tz='GMT') & datetime < as.POSIXct('2016-06-16', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='early June 2016, v2', x='date', y='') +
  final_theme


#ggplot(subset(buoy1416_vert_lowdo, subset=(datetime>=as.POSIXct('2016-06-16', tz='GMT') & datetime < as.POSIXct('2016-07-01', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='late June 2016', x='date', y='') +
  final_theme

#ggplot(subset(buoy1416_vert_lowdo, subset=(datetime>=as.POSIXct('2016-07-01', tz='GMT') & datetime < as.POSIXct('2016-07-16', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='early July 2016', x='date', y='') +
  final_theme

#ggplot(subset(buoy1416_vert_lowdo, subset=(datetime>=as.POSIXct('2016-07-16', tz='GMT') & datetime < as.POSIXct('2016-08-01', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='late July 2016', x='date', y='') +
  final_theme

#ggplot(subset(buoy1416_vert_lowdo, subset=(datetime>=as.POSIXct('2016-08-01', tz='GMT') & datetime < as.POSIXct('2016-08-16', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='early August 2016', x='date', y='') +
  final_theme

#ggplot(subset(buoy1416_vert_lowdo, subset=(datetime>=as.POSIXct('2016-08-16', tz='GMT') & datetime < as.POSIXct('2016-09-01', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='late August 2016', x='date', y='') +
  final_theme

#ggplot(subset(buoy1416_vert_lowdo, subset=(datetime>=as.POSIXct('2016-09-01', tz='GMT') & datetime < as.POSIXct('2016-09-16', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='early September 2016', x='date', y='') +
  final_theme

#ggplot(subset(buoy1416_vert_lowdo, subset=(datetime>=as.POSIXct('2016-09-16', tz='GMT') & datetime < as.POSIXct('2016-10-01', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='late September 2016', x='date', y='') +
  final_theme

#ggplot(subset(buoy1416_vert_lowdo, subset=(datetime>=as.POSIXct('2016-10-01', tz='GMT') & datetime < as.POSIXct('2016-10-16', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='early October 2016', x='date', y='') +
  final_theme


#ggplot(subset(buoy1416_vert_lowdo_L1, subset=(datetime>=as.POSIXct('2016-01-01', tz='GMT') & datetime < as.POSIXct('2017-01-01', tz='GMT'))), aes(x=datetime, y=value)) +
  geom_point() +
  facet_grid(variable~., scales='free_y') +
  labs(title='2016, clean', x='date', y='') +
  final_theme

#### meteorology ####
range(buoy1416_L1$AirTempC, na.rm = T)
range(buoy1416_L1$RelHum, na.rm = T)
range(buoy1416_L1$PAR, na.rm = T)
range(buoy1416_L1$WindSpdAv, na.rm = T)
range(buoy1416_L1$WindVect, na.rm = T)
range(buoy1416_L1$MaxWind, na.rm = T)
range(buoy1416_L1$MaxWindDir, na.rm = T)

####PAR ####
#replace negative values with 0
ix=which(buoy1416_L1$PAR<0)
buoy1416_L1$PAR[ix]=0

####2014####
#ggplot(subset(buoy1416_L1, subset=(datetime>=as.POSIXct('2014-01-01', tz='GMT') & datetime<as.POSIXct('2015-01-01', tz='GMT'))), aes(x=datetime, y=PAR)) +
  geom_point() + 
  labs(title='2014, raw', x='date', y='PAR (uE*m-2*s-1)') +
  final_theme

#ggplot(subset(buoy1416_L1, subset=(datetime>=as.POSIXct('2014-01-01', tz='GMT') & datetime<as.POSIXct('2014-01-16', tz='GMT'))), aes(x=datetime, y=PAR)) +
  geom_point() + 
  labs(title='early January 2014', x='date', y='PAR (uE*m-2*s-1)') +
  final_theme

#ggplot(subset(buoy1416_L1, subset=(datetime>=as.POSIXct('2014-01-16', tz='GMT') & datetime<as.POSIXct('2014-02-01', tz='GMT'))), aes(x=datetime, y=PAR)) +
  geom_point() + 
  labs(title='late January 2014', x='date', y='PAR (uE*m-2*s-1)') +
  final_theme

#ggplot(subset(buoy1416_L1, subset=(datetime>=as.POSIXct('2014-02-01', tz='GMT') & datetime<as.POSIXct('2014-02-14', tz='GMT'))), aes(x=datetime, y=PAR)) +
  geom_point() + 
  labs(title='early February 2014', x='date', y='PAR (uE*m-2*s-1)') +
  final_theme

#ggplot(subset(buoy1416_L1, subset=(datetime>=as.POSIXct('2014-02-14', tz='GMT') & datetime<as.POSIXct('2014-03-01', tz='GMT'))), aes(x=datetime, y=PAR)) +
  geom_point() + 
  labs(title='late February 2014', x='date', y='PAR (uE*m-2*s-1)') +
  final_theme

#ggplot(subset(buoy1416_L1, subset=(datetime>=as.POSIXct('2014-03-01', tz='GMT') & datetime<as.POSIXct('2014-03-16', tz='GMT'))), aes(x=datetime, y=PAR)) +
  geom_point() + 
  labs(title='early March 2014', x='date', y='PAR (uE*m-2*s-1)') +
  final_theme

#ggplot(subset(buoy1416_L1, subset=(datetime>=as.POSIXct('2014-03-16', tz='GMT') & datetime<as.POSIXct('2014-04-01', tz='GMT'))), aes(x=datetime, y=PAR)) +
  geom_point() + 
  labs(title='late March 2014', x='date', y='PAR (uE*m-2*s-1)') +
  final_theme

#low values during the day on the 26th
#ggplot(subset(buoy1416_L1, subset=(datetime>=as.POSIXct('2014-03-26', tz='GMT') & datetime<as.POSIXct('2014-03-27', tz='GMT'))), aes(x=datetime, y=PAR)) +
  geom_point() + 
  labs(title='late March 2014', x='date', y='PAR (uE*m-2*s-1)') +
  final_theme

ix=which(buoy1416_L1$datetime>=as.POSIXct('2014-03-26 12:00', tz='GMT') & buoy1416_L1$datetime<=as.POSIXct('2014-03-26 15:00', tz='GMT') & buoy1416_L1$PAR<10)
buoy1416_L1$PAR[ix]=NA

#ggplot(subset(buoy1416_L1, subset=(datetime>=as.POSIXct('2014-03-26', tz='GMT') & datetime<as.POSIXct('2014-03-27', tz='GMT'))), aes(x=datetime, y=PAR)) +
  geom_point() + 
  labs(title='late March 2014', x='date', y='PAR (uE*m-2*s-1)') +
  final_theme

#ggplot(subset(buoy1416_L1, subset=(datetime>=as.POSIXct('2014-03-16', tz='GMT') & datetime<as.POSIXct('2014-04-01', tz='GMT'))), aes(x=datetime, y=PAR)) +
  geom_point() + 
  labs(title='late March 2014, v2', x='date', y='PAR (uE*m-2*s-1)') +
  final_theme


#ggplot(subset(buoy1416_L1, subset=(datetime>=as.POSIXct('2014-04-01', tz='GMT') & datetime<as.POSIXct('2014-04-16', tz='GMT'))), aes(x=datetime, y=PAR)) +
  geom_point() + 
  labs(title='early April 2014', x='date', y='PAR (uE*m-2*s-1)') +
  final_theme

#remove data after midnight on the 27th
ix=which(buoy1416_L1$datetime>=as.POSIXct('2014-03-28', tz='GMT') & buoy1416_L1$datetime<as.POSIXct('2015-01-01', tz='GMT'))
buoy1416_L1$PAR[ix]=NA

#ggplot(subset(buoy1416_L1, subset=(datetime>=as.POSIXct('2014-03-16', tz='GMT') & datetime<as.POSIXct('2014-04-01', tz='GMT'))), aes(x=datetime, y=PAR)) +
  geom_point() + 
  labs(title='late March 2014, v3', x='date', y='PAR (uE*m-2*s-1)') +
  final_theme

#ggplot(subset(buoy1416_L1, subset=(datetime>=as.POSIXct('2014-01-01', tz='GMT') & datetime<as.POSIXct('2015-01-01', tz='GMT'))), aes(x=datetime, y=PAR)) +
  geom_point() + 
  labs(title='2014, clean', x='date', y='PAR (uE*m-2*s-1)') +
  final_theme


####2015####
#ggplot(subset(buoy1416_L1, subset=(datetime>=as.POSIXct('2015-01-01', tz='GMT') & datetime<as.POSIXct('2016-01-01', tz='GMT'))), aes(x=datetime, y=PAR)) +
  geom_point() + 
  labs(title='2015, raw', x='date', y='PAR (uE*m-2*s-1)') +
  final_theme

#par comes back online June 2015

#ggplot(subset(buoy1416_L1, subset=(datetime>=as.POSIXct('2015-06-01', tz='GMT') & datetime<as.POSIXct('2015-06-16', tz='GMT'))), aes(x=datetime, y=PAR)) +
  geom_point() + 
  labs(title='early June 2015', x='date', y='PAR (uE*m-2*s-1)') +
  final_theme

#need to remove data before PAR functioning correctly
#ggplot(subset(buoy1416_L1, subset=(datetime>=as.POSIXct('2015-06-04', tz='GMT') & datetime<as.POSIXct('2015-06-05', tz='GMT'))), aes(x=datetime, y=PAR)) +
  geom_point() + 
  labs(title='early June 2015', x='date', y='PAR (uE*m-2*s-1)') +
  final_theme

ix=which(buoy1416_L1$datetime>=as.POSIXct('2015-01-01', tz='GMT') & buoy1416_L1$datetime<=as.POSIXct('2015-06-04 9:00', tz='GMT'))
buoy1416_L1$PAR[ix]=NA

#ggplot(subset(buoy1416_L1, subset=(datetime>=as.POSIXct('2015-06-04', tz='GMT') & datetime<as.POSIXct('2015-06-05', tz='GMT'))), aes(x=datetime, y=PAR)) +
  geom_point() + 
  labs(title='early June 2015', x='date', y='PAR (uE*m-2*s-1)') +
  final_theme

#ggplot(subset(buoy1416_L1, subset=(datetime>=as.POSIXct('2015-06-01', tz='GMT') & datetime<as.POSIXct('2015-06-16', tz='GMT'))), aes(x=datetime, y=PAR)) +
  geom_point() + 
  labs(title='early June 2015, v2', x='date', y='PAR (uE*m-2*s-1)') +
  final_theme


#ggplot(subset(buoy1416_L1, subset=(datetime>=as.POSIXct('2015-06-16', tz='GMT') & datetime<as.POSIXct('2015-07-01', tz='GMT'))), aes(x=datetime, y=PAR)) +
  geom_point() + 
  labs(title='late June 2015', x='date', y='PAR (uE*m-2*s-1)') +
  final_theme

#ggplot(subset(buoy1416_L1, subset=(datetime>=as.POSIXct('2015-07-01', tz='GMT') & datetime<as.POSIXct('2015-07-16', tz='GMT'))), aes(x=datetime, y=PAR)) +
  geom_point() + 
  labs(title='early July 2015', x='date', y='PAR (uE*m-2*s-1)') +
  final_theme

#ggplot(subset(buoy1416_L1, subset=(datetime>=as.POSIXct('2015-07-16', tz='GMT') & datetime<as.POSIXct('2015-08-01', tz='GMT'))), aes(x=datetime, y=PAR)) +
  geom_point() + 
  labs(title='late July 2015', x='date', y='PAR (uE*m-2*s-1)') +
  final_theme

#ggplot(subset(buoy1416_L1, subset=(datetime>=as.POSIXct('2015-08-01', tz='GMT') & datetime<as.POSIXct('2015-08-16', tz='GMT'))), aes(x=datetime, y=PAR)) +
  geom_point() + 
  labs(title='early August 2015', x='date', y='PAR (uE*m-2*s-1)') +
  final_theme

#ggplot(subset(buoy1416_L1, subset=(datetime>=as.POSIXct('2015-08-16', tz='GMT') & datetime<as.POSIXct('2015-09-01', tz='GMT'))), aes(x=datetime, y=PAR)) +
  geom_point() + 
  labs(title='late August 2015', x='date', y='PAR (uE*m-2*s-1)') +
  final_theme

#ggplot(subset(buoy1416_L1, subset=(datetime>=as.POSIXct('2015-09-01', tz='GMT') & datetime<as.POSIXct('2015-09-16', tz='GMT'))), aes(x=datetime, y=PAR)) +
  geom_point() + 
  labs(title='early Sept 2015', x='date', y='PAR (uE*m-2*s-1)') +
  final_theme

#ggplot(subset(buoy1416_L1, subset=(datetime>=as.POSIXct('2015-09-16', tz='GMT') & datetime<as.POSIXct('2015-10-01', tz='GMT'))), aes(x=datetime, y=PAR)) +
  geom_point() + 
  labs(title='late Sept 2015', x='date', y='PAR (uE*m-2*s-1)') +
  final_theme

#ggplot(subset(buoy1416_L1, subset=(datetime>=as.POSIXct('2015-10-01', tz='GMT') & datetime<as.POSIXct('2015-10-16', tz='GMT'))), aes(x=datetime, y=PAR)) +
  geom_point() + 
  labs(title='early Oct 2015', x='date', y='PAR (uE*m-2*s-1)') +
  final_theme

#ggplot(subset(buoy1416_L1, subset=(datetime>=as.POSIXct('2015-10-16', tz='GMT') & datetime<as.POSIXct('2015-11-01', tz='GMT'))), aes(x=datetime, y=PAR)) +
  geom_point() + 
  labs(title='late Oct 2015', x='date', y='PAR (uE*m-2*s-1)') +
  final_theme

#ggplot(subset(buoy1416_L1, subset=(datetime>=as.POSIXct('2015-11-01', tz='GMT') & datetime<as.POSIXct('2015-11-16', tz='GMT'))), aes(x=datetime, y=PAR)) +
  geom_point() + 
  labs(title='early November 2015', x='date', y='PAR (uE*m-2*s-1)') +
  final_theme

#ggplot(subset(buoy1416_L1, subset=(datetime>=as.POSIXct('2015-11-16', tz='GMT') & datetime<as.POSIXct('2015-12-01', tz='GMT'))), aes(x=datetime, y=PAR)) +
  geom_point() + 
  labs(title='late November 2015', x='date', y='PAR (uE*m-2*s-1)') +
  final_theme

#ggplot(subset(buoy1416_L1, subset=(datetime>=as.POSIXct('2015-12-01', tz='GMT') & datetime<as.POSIXct('2015-12-16', tz='GMT'))), aes(x=datetime, y=PAR)) +
  geom_point() + 
  labs(title='early December 2015', x='date', y='PAR (uE*m-2*s-1)') +
  final_theme

#ggplot(subset(buoy1416_L1, subset=(datetime>=as.POSIXct('2015-12-16', tz='GMT') & datetime<as.POSIXct('2016-01-01', tz='GMT'))), aes(x=datetime, y=PAR)) +
  geom_point() + 
  labs(title='late December 2015', x='date', y='PAR (uE*m-2*s-1)') +
  final_theme

#ggplot(subset(buoy1416_L1, subset=(datetime>=as.POSIXct('2015-01-01', tz='GMT') & datetime<as.POSIXct('2016-01-01', tz='GMT'))), aes(x=datetime, y=PAR)) +
  geom_point() + 
  labs(title='2015, clean', x='date', y='PAR (uE*m-2*s-1)') +
  final_theme


####2016####
#ggplot(subset(buoy1416_L1, subset=(datetime>=as.POSIXct('2016-01-01', tz='GMT') & datetime<as.POSIXct('2017-01-01', tz='GMT'))), aes(x=datetime, y=PAR)) +
  geom_point() + 
  labs(title='2016, raw', x='date', y='PAR (uE*m-2*s-1)') +
  final_theme

#ggplot(subset(buoy1416_L1, subset=(datetime>=as.POSIXct('2016-01-01', tz='GMT') & datetime<as.POSIXct('2016-01-16', tz='GMT'))), aes(x=datetime, y=PAR)) +
  geom_point() + 
  labs(title='early January 2016', x='date', y='PAR (uE*m-2*s-1)') +
  final_theme

#ggplot(subset(buoy1416_L1, subset=(datetime>=as.POSIXct('2016-01-16', tz='GMT') & datetime<as.POSIXct('2016-02-01', tz='GMT'))), aes(x=datetime, y=PAR)) +
  geom_point() + 
  labs(title='late January 2016', x='date', y='PAR (uE*m-2*s-1)') +
  final_theme

#ggplot(subset(buoy1416_L1, subset=(datetime>=as.POSIXct('2016-02-01', tz='GMT') & datetime<as.POSIXct('2016-02-14', tz='GMT'))), aes(x=datetime, y=PAR)) +
  geom_point() + 
  labs(title='early February 2016', x='date', y='PAR (uE*m-2*s-1)') +
  final_theme

#ggplot(subset(buoy1416_L1, subset=(datetime>=as.POSIXct('2016-02-14', tz='GMT') & datetime<as.POSIXct('2016-03-01', tz='GMT'))), aes(x=datetime, y=PAR)) +
  geom_point() + 
  labs(title='late February 2016', x='date', y='PAR (uE*m-2*s-1)') +
  final_theme

#ggplot(subset(buoy1416_L1, subset=(datetime>=as.POSIXct('2016-03-01', tz='GMT') & datetime<as.POSIXct('2016-03-16', tz='GMT'))), aes(x=datetime, y=PAR)) +
  geom_point() + 
  labs(title='early March 2016', x='date', y='PAR (uE*m-2*s-1)') +
  final_theme

#ggplot(subset(buoy1416_L1, subset=(datetime>=as.POSIXct('2016-03-16', tz='GMT') & datetime<as.POSIXct('2016-04-01', tz='GMT'))), aes(x=datetime, y=PAR)) +
  geom_point() + 
  labs(title='late March 2016', x='date', y='PAR (uE*m-2*s-1)') +
  final_theme

#ggplot(subset(buoy1416_L1, subset=(datetime>=as.POSIXct('2016-04-01', tz='GMT') & datetime<as.POSIXct('2016-04-16', tz='GMT'))), aes(x=datetime, y=PAR)) +
  geom_point() + 
  labs(title='early April 2016', x='date', y='PAR (uE*m-2*s-1)') +
  final_theme

#ggplot(subset(buoy1416_L1, subset=(datetime>=as.POSIXct('2016-04-16', tz='GMT') & datetime<as.POSIXct('2016-05-01', tz='GMT'))), aes(x=datetime, y=PAR)) +
  geom_point() + 
  labs(title='late April 2016', x='date', y='PAR (uE*m-2*s-1)') +
  final_theme

#ggplot(subset(buoy1416_L1, subset=(datetime>=as.POSIXct('2016-05-01', tz='GMT') & datetime<as.POSIXct('2016-05-16', tz='GMT'))), aes(x=datetime, y=PAR)) +
  geom_point() + 
  labs(title='early May 2016', x='date', y='PAR (uE*m-2*s-1)') +
  final_theme

#ggplot(subset(buoy1416_L1, subset=(datetime>=as.POSIXct('2016-05-16', tz='GMT') & datetime<as.POSIXct('2016-06-01', tz='GMT'))), aes(x=datetime, y=PAR)) +
  geom_point() + 
  labs(title='late May 2016', x='date', y='PAR (uE*m-2*s-1)') +
  final_theme

#ggplot(subset(buoy1416_L1, subset=(datetime>=as.POSIXct('2016-06-01', tz='GMT') & datetime<as.POSIXct('2016-06-16', tz='GMT'))), aes(x=datetime, y=PAR)) +
  geom_point() + 
  labs(title='early June 2016', x='date', y='PAR (uE*m-2*s-1)') +
  final_theme

#ggplot(subset(buoy1416_L1, subset=(datetime>=as.POSIXct('2016-06-16', tz='GMT') & datetime<as.POSIXct('2016-07-01', tz='GMT'))), aes(x=datetime, y=PAR)) +
  geom_point() + 
  labs(title='late June 2016', x='date', y='PAR (uE*m-2*s-1)') +
  final_theme

#ggplot(subset(buoy1416_L1, subset=(datetime>=as.POSIXct('2016-07-01', tz='GMT') & datetime<as.POSIXct('2016-07-16', tz='GMT'))), aes(x=datetime, y=PAR)) +
  geom_point() + 
  labs(title='early July 2016', x='date', y='PAR (uE*m-2*s-1)') +
  final_theme

#ggplot(subset(buoy1416_L1, subset=(datetime>=as.POSIXct('2016-07-16', tz='GMT') & datetime<as.POSIXct('2016-08-01', tz='GMT'))), aes(x=datetime, y=PAR)) +
  geom_point() + 
  labs(title='late July 2016', x='date', y='PAR (uE*m-2*s-1)') +
  final_theme

#ggplot(subset(buoy1416_L1, subset=(datetime>=as.POSIXct('2016-08-01', tz='GMT') & datetime<as.POSIXct('2016-08-16', tz='GMT'))), aes(x=datetime, y=PAR)) +
  geom_point() + 
  labs(title='early August 2016', x='date', y='PAR (uE*m-2*s-1)') +
  final_theme

#ggplot(subset(buoy1416_L1, subset=(datetime>=as.POSIXct('2016-08-16', tz='GMT') & datetime<as.POSIXct('2016-09-01', tz='GMT'))), aes(x=datetime, y=PAR)) +
  geom_point() + 
  labs(title='late August 2016', x='date', y='PAR (uE*m-2*s-1)') +
  final_theme

#ggplot(subset(buoy1416_L1, subset=(datetime>=as.POSIXct('2016-09-01', tz='GMT') & datetime<as.POSIXct('2016-09-16', tz='GMT'))), aes(x=datetime, y=PAR)) +
  geom_point() + 
  labs(title='early Sept 2016', x='date', y='PAR (uE*m-2*s-1)') +
  final_theme

#ggplot(subset(buoy1416_L1, subset=(datetime>=as.POSIXct('2016-09-16', tz='GMT') & datetime<as.POSIXct('2016-10-01', tz='GMT'))), aes(x=datetime, y=PAR)) +
  geom_point() + 
  labs(title='late Sept 2016', x='date', y='PAR (uE*m-2*s-1)') +
  final_theme

#ggplot(subset(buoy1416_L1, subset=(datetime>=as.POSIXct('2016-10-01', tz='GMT') & datetime<as.POSIXct('2016-10-16', tz='GMT'))), aes(x=datetime, y=PAR)) +
  geom_point() + 
  labs(title='early Oct 2016', x='date', y='PAR (uE*m-2*s-1)') +
  final_theme

#ggplot(subset(buoy1416_L1, subset=(datetime>=as.POSIXct('2016-10-16', tz='GMT') & datetime<as.POSIXct('2016-11-01', tz='GMT'))), aes(x=datetime, y=PAR)) +
  geom_point() + 
  labs(title='late Oct 2016', x='date', y='PAR (uE*m-2*s-1)') +
  final_theme

#ggplot(subset(buoy1416_L1, subset=(datetime>=as.POSIXct('2016-11-01', tz='GMT') & datetime<as.POSIXct('2016-11-16', tz='GMT'))), aes(x=datetime, y=PAR)) +
  geom_point() + 
  labs(title='early November 2016', x='date', y='PAR (uE*m-2*s-1)') +
  final_theme

#ggplot(subset(buoy1416_L1, subset=(datetime>=as.POSIXct('2016-11-16', tz='GMT') & datetime<as.POSIXct('2016-12-01', tz='GMT'))), aes(x=datetime, y=PAR)) +
  geom_point() + 
  labs(title='late November 2016', x='date', y='PAR (uE*m-2*s-1)') +
  final_theme

#ggplot(subset(buoy1416_L1, subset=(datetime>=as.POSIXct('2016-12-01', tz='GMT') & datetime<as.POSIXct('2016-12-16', tz='GMT'))), aes(x=datetime, y=PAR)) +
  geom_point() + 
  labs(title='early December 2016', x='date', y='PAR (uE*m-2*s-1)') +
  final_theme

#ggplot(subset(buoy1416_L1, subset=(datetime>=as.POSIXct('2016-12-16', tz='GMT') & datetime<as.POSIXct('2017-01-01', tz='GMT'))), aes(x=datetime, y=PAR)) +
  geom_point() + 
  labs(title='late December 2016', x='date', y='PAR (uE*m-2*s-1)') +
  final_theme


####Air Temp and Rel Humidity ####
buoy1416_temphum <- subset(buoy1416_L1, select=c('datetime', 'AirTempC', 'RelHum'))
buoy1416_vert_temphum <- melt(buoy1416_temphum, id='datetime')

####2014####
#ggplot(subset(buoy1416_vert_temphum, subset=(datetime>=as.POSIXct('2014-01-01', tz='GMT') & datetime<as.POSIXct('2015-01-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='2014, raw', x='date', y=' ') +
  final_theme

#ggplot(subset(buoy1416_vert_temphum, subset=(datetime>=as.POSIXct('2014-01-01', tz='GMT') & datetime<as.POSIXct('2014-01-16', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() +   
  facet_grid(variable~., scales='free_y') +
  labs(title='early January 2014', x='date', y=' ') +
  final_theme

#ggplot(subset(buoy1416_vert_temphum, subset=(datetime>=as.POSIXct('2014-01-16', tz='GMT') & datetime<as.POSIXct('2014-02-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='late January 2014', x='date', y=' ') +
  final_theme

#ggplot(subset(buoy1416_vert_temphum, subset=(datetime>=as.POSIXct('2014-02-01', tz='GMT') & datetime<as.POSIXct('2014-02-14', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='early February 2014', x='date', y=' ') +
  final_theme

#ggplot(subset(buoy1416_vert_temphum, subset=(datetime>=as.POSIXct('2014-02-14', tz='GMT') & datetime<as.POSIXct('2014-03-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='late February 2014', x='date', y=' ') +
  final_theme

#ggplot(subset(buoy1416_vert_temphum, subset=(datetime>=as.POSIXct('2014-03-01', tz='GMT') & datetime<as.POSIXct('2014-03-16', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='early March 2014', x='date', y=' ') +
  final_theme

#ggplot(subset(buoy1416_vert_temphum, subset=(datetime>=as.POSIXct('2014-03-16', tz='GMT') & datetime<as.POSIXct('2014-04-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='late March 2014', x='date', y=' ') +
  final_theme

#ggplot(subset(buoy1416_vert_temphum, subset=(datetime>=as.POSIXct('2014-04-01', tz='GMT') & datetime<as.POSIXct('2014-04-16', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='early April 2014', x='date', y=' ') +
  final_theme

#data gap until June

#ggplot(subset(buoy1416_vert_temphum, subset=(datetime>=as.POSIXct('2014-06-01', tz='GMT') & datetime<as.POSIXct('2014-06-16', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='early June 2014', x='date', y=' ') +
  final_theme

#ggplot(subset(buoy1416_vert_temphum, subset=(datetime>=as.POSIXct('2014-06-16', tz='GMT') & datetime<as.POSIXct('2014-07-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='late June 2014', x='date', y=' ') +
  final_theme

#ggplot(subset(buoy1416_vert_temphum, subset=(datetime>=as.POSIXct('2014-07-01', tz='GMT') & datetime<as.POSIXct('2014-07-16', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='early July 2014', x='date', y=' ') +
  final_theme

#ggplot(subset(buoy1416_vert_temphum, subset=(datetime>=as.POSIXct('2014-07-16', tz='GMT') & datetime<as.POSIXct('2014-08-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='late July 2014', x='date', y=' ') +
  final_theme

#ggplot(subset(buoy1416_vert_temphum, subset=(datetime>=as.POSIXct('2014-07-23', tz='GMT') & datetime<as.POSIXct('2014-07-24', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='late July 2014', x='date', y=' ') +
  final_theme

#ggplot(subset(buoy1416_vert_temphum, subset=(datetime>=as.POSIXct('2014-08-01', tz='GMT') & datetime<as.POSIXct('2014-08-16', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='early August 2014', x='date', y=' ') +
  final_theme

#ggplot(subset(buoy1416_vert_temphum, subset=(datetime>=as.POSIXct('2014-08-16', tz='GMT') & datetime<as.POSIXct('2014-09-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='late August 2014', x='date', y=' ') +
  final_theme

#ggplot(subset(buoy1416_vert_temphum, subset=(datetime>=as.POSIXct('2014-09-01', tz='GMT') & datetime<as.POSIXct('2014-09-16', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='early September 2014', x='date', y=' ') +
  final_theme

#ggplot(subset(buoy1416_vert_temphum, subset=(datetime>=as.POSIXct('2014-09-16', tz='GMT') & datetime<as.POSIXct('2014-10-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='late September 2014', x='date', y=' ') +
  final_theme

#ggplot(subset(buoy1416_vert_temphum, subset=(datetime>=as.POSIXct('2014-10-01', tz='GMT') & datetime<as.POSIXct('2014-10-16', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='early October 2014', x='date', y=' ') +
  final_theme

#ggplot(subset(buoy1416_vert_temphum, subset=(datetime>=as.POSIXct('2014-10-16', tz='GMT') & datetime<as.POSIXct('2014-11-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='late October 2014', x='date', y=' ') +
  final_theme

#ggplot(subset(buoy1416_vert_temphum, subset=(datetime>=as.POSIXct('2014-11-01', tz='GMT') & datetime<as.POSIXct('2014-11-16', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='early November 2014', x='date', y=' ') +
  final_theme

#ggplot(subset(buoy1416_vert_temphum, subset=(datetime>=as.POSIXct('2014-11-16', tz='GMT') & datetime<as.POSIXct('2014-12-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='late Novebmer 2014', x='date', y=' ') +
  final_theme

#ggplot(subset(buoy1416_vert_temphum, subset=(datetime>=as.POSIXct('2014-12-01', tz='GMT') & datetime<as.POSIXct('2014-12-16', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='early December 2014', x='date', y=' ') +
  final_theme

#ggplot(subset(buoy1416_vert_temphum, subset=(datetime>=as.POSIXct('2014-12-16', tz='GMT') & datetime<as.POSIXct('2015-01-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='late December 2014', x='date', y=' ') +
  final_theme



#ggplot(subset(buoy1416_vert_temphum, subset=(datetime>=as.POSIXct('2014-01-01', tz='GMT') & datetime<as.POSIXct('2015-01-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  labs(title='2014, clean', x='date', y=' ') +
  final_theme


####2015####
#ggplot(subset(buoy1416_vert_temphum, subset=(datetime>=as.POSIXct('2015-01-01', tz='GMT') & datetime<as.POSIXct('2016-01-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='2015, raw', x='date', y=' ') +
  final_theme

#ggplot(subset(buoy1416_vert_temphum, subset=(datetime>=as.POSIXct('2015-01-01', tz='GMT') & datetime<as.POSIXct('2015-01-16', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() +   
  facet_grid(variable~., scales='free_y') +
  labs(title='early January 2015', x='date', y=' ') +
  final_theme

#ggplot(subset(buoy1416_vert_temphum, subset=(datetime>=as.POSIXct('2015-01-16', tz='GMT') & datetime<as.POSIXct('2015-02-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='late January 2015', x='date', y=' ') +
  final_theme

#ggplot(subset(buoy1416_vert_temphum, subset=(datetime>=as.POSIXct('2015-02-01', tz='GMT') & datetime<as.POSIXct('2015-02-14', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='early February 2015', x='date', y=' ') +
  final_theme

#ggplot(subset(buoy1416_vert_temphum, subset=(datetime>=as.POSIXct('2015-02-14', tz='GMT') & datetime<as.POSIXct('2015-03-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='late February 2015', x='date', y=' ') +
  final_theme

#ggplot(subset(buoy1416_vert_temphum, subset=(datetime>=as.POSIXct('2015-03-01', tz='GMT') & datetime<as.POSIXct('2015-03-16', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='early March 2015', x='date', y=' ') +
  final_theme

#ggplot(subset(buoy1416_vert_temphum, subset=(datetime>=as.POSIXct('2015-03-16', tz='GMT') & datetime<as.POSIXct('2015-04-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='late March 2015', x='date', y=' ') +
  final_theme

#ggplot(subset(buoy1416_vert_temphum, subset=(datetime>=as.POSIXct('2015-04-01', tz='GMT') & datetime<as.POSIXct('2015-04-16', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='early April 2015', x='date', y=' ') +
  final_theme

#ggplot(subset(buoy1416_vert_temphum, subset=(datetime>=as.POSIXct('2015-04-16', tz='GMT') & datetime<as.POSIXct('2015-05-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='late April 2015', x='date', y=' ') +
  final_theme


#data gap until June

#ggplot(subset(buoy1416_vert_temphum, subset=(datetime>=as.POSIXct('2015-06-01', tz='GMT') & datetime<as.POSIXct('2015-06-16', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='early June 2015', x='date', y=' ') +
  final_theme

#ggplot(subset(buoy1416_vert_temphum, subset=(datetime>=as.POSIXct('2015-06-16', tz='GMT') & datetime<as.POSIXct('2015-07-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='late June 2015', x='date', y=' ') +
  final_theme

#ggplot(subset(buoy1416_vert_temphum, subset=(datetime>=as.POSIXct('2015-07-01', tz='GMT') & datetime<as.POSIXct('2015-07-16', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='early July 2015', x='date', y=' ') +
  final_theme

#ggplot(subset(buoy1416_vert_temphum, subset=(datetime>=as.POSIXct('2015-07-16', tz='GMT') & datetime<as.POSIXct('2015-08-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='late July 2015', x='date', y=' ') +
  final_theme

##errant temp/rh data on Jul 19
#ggplot(subset(buoy1416_vert_temphum, subset=(datetime>=as.POSIXct('2015-07-19 12:00', tz='GMT') & datetime<as.POSIXct('2015-07-20', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='late July 2015', x='date', y=' ') +
  final_theme

ix=which(buoy1416_L1$datetime>=as.POSIXct('2015-07-19 19:50', tz='GMT') & buoy1416_L1$datetime<=as.POSIXct('2015-07-19 21:20', tz='GMT'))
for (i in c('AirTempC', 'RelHum')) {buoy1416_L1[ix, i]= NA}
buoy1416_temphum_L1 <- subset(buoy1416_L1, select=c('datetime', 'AirTempC', 'RelHum'))
buoy1416_vert_temphum_L1 <- melt(buoy1416_temphum_L1, id='datetime')

#ggplot(subset(buoy1416_vert_temphum_L1, subset=(datetime>=as.POSIXct('2015-07-16', tz='GMT') & datetime<as.POSIXct('2015-08-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='late July 2015, v2', x='date', y=' ') +
  final_theme


s#ggplot(subset(buoy1416_vert_temphum, subset=(datetime>=as.POSIXct('2015-08-01', tz='GMT') & datetime<as.POSIXct('2015-08-16', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='early August 2015', x='date', y=' ') +
  final_theme

#ggplot(subset(buoy1416_vert_temphum, subset=(datetime>=as.POSIXct('2015-08-16', tz='GMT') & datetime<as.POSIXct('2015-09-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='late August 2015', x='date', y=' ') +
  final_theme

#ggplot(subset(buoy1416_vert_temphum, subset=(datetime>=as.POSIXct('2015-09-01', tz='GMT') & datetime<as.POSIXct('2015-09-16', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='early September 2015', x='date', y=' ') +
  final_theme

#ggplot(subset(buoy1416_vert_temphum, subset=(datetime>=as.POSIXct('2015-09-16', tz='GMT') & datetime<as.POSIXct('2015-10-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='late September 2015', x='date', y=' ') +
  final_theme

#ggplot(subset(buoy1416_vert_temphum, subset=(datetime>=as.POSIXct('2015-10-01', tz='GMT') & datetime<as.POSIXct('2015-10-16', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='early October 2015', x='date', y=' ') +
  final_theme

#ggplot(subset(buoy1416_vert_temphum, subset=(datetime>=as.POSIXct('2015-10-16', tz='GMT') & datetime<as.POSIXct('2015-11-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='late October 2015', x='date', y=' ') +
  final_theme

#ggplot(subset(buoy1416_vert_temphum, subset=(datetime>=as.POSIXct('2015-11-01', tz='GMT') & datetime<as.POSIXct('2015-11-16', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='early November 2015', x='date', y=' ') +
  final_theme

#ggplot(subset(buoy1416_vert_temphum, subset=(datetime>=as.POSIXct('2015-11-16', tz='GMT') & datetime<as.POSIXct('2015-12-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='late Novebmer 2015', x='date', y=' ') +
  final_theme

#ggplot(subset(buoy1416_vert_temphum, subset=(datetime>=as.POSIXct('2015-12-01', tz='GMT') & datetime<as.POSIXct('2015-12-16', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='early December 2015', x='date', y=' ') +
  final_theme

#ggplot(subset(buoy1416_vert_temphum, subset=(datetime>=as.POSIXct('2015-12-16', tz='GMT') & datetime<as.POSIXct('2016-01-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='late December 2015', x='date', y=' ') +
  final_theme



#ggplot(subset(buoy1416_vert_temphum_L1, subset=(datetime>=as.POSIXct('2015-01-01', tz='GMT') & datetime<as.POSIXct('2016-01-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='2015, clean', x='date', y=' ') +
  final_theme


####2016####
#ggplot(subset(buoy1416_vert_temphum, subset=(datetime>=as.POSIXct('2016-01-01', tz='GMT') & datetime<as.POSIXct('2017-01-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='2016, raw', x='date', y=' ') +
  final_theme

#ggplot(subset(buoy1416_vert_temphum, subset=(datetime>=as.POSIXct('2016-01-01', tz='GMT') & datetime<as.POSIXct('2016-01-16', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() +   
  facet_grid(variable~., scales='free_y') +
  labs(title='early January 2016', x='date', y=' ') +
  final_theme

#ggplot(subset(buoy1416_vert_temphum, subset=(datetime>=as.POSIXct('2016-01-16', tz='GMT') & datetime<as.POSIXct('2016-02-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='late January 2016', x='date', y=' ') +
  final_theme

#ggplot(subset(buoy1416_vert_temphum, subset=(datetime>=as.POSIXct('2016-02-01', tz='GMT') & datetime<as.POSIXct('2016-02-14', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='early February 2016', x='date', y=' ') +
  final_theme

#ggplot(subset(buoy1416_vert_temphum, subset=(datetime>=as.POSIXct('2016-02-14', tz='GMT') & datetime<as.POSIXct('2016-03-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='late February 2016', x='date', y=' ') +
  final_theme

#ggplot(subset(buoy1416_vert_temphum, subset=(datetime>=as.POSIXct('2016-03-01', tz='GMT') & datetime<as.POSIXct('2016-03-16', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='early March 2016', x='date', y=' ') +
  final_theme

#ggplot(subset(buoy1416_vert_temphum, subset=(datetime>=as.POSIXct('2016-03-16', tz='GMT') & datetime<as.POSIXct('2016-04-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='late March 2016', x='date', y=' ') +
  final_theme

#ggplot(subset(buoy1416_vert_temphum, subset=(datetime>=as.POSIXct('2016-04-01', tz='GMT') & datetime<as.POSIXct('2016-04-16', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='early April 2016', x='date', y=' ') +
  final_theme

#ggplot(subset(buoy1416_vert_temphum, subset=(datetime>=as.POSIXct('2016-04-16', tz='GMT') & datetime<as.POSIXct('2016-05-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='late April 2016', x='date', y=' ') +
  final_theme

#ggplot(subset(buoy1416_vert_temphum, subset=(datetime>=as.POSIXct('2016-05-01', tz='GMT') & datetime<as.POSIXct('2016-05-16', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='early May 2016', x='date', y=' ') +
  final_theme

#ggplot(subset(buoy1416_vert_temphum, subset=(datetime>=as.POSIXct('2016-05-16', tz='GMT') & datetime<as.POSIXct('2016-06-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='late May 2016', x='date', y=' ') +
  final_theme

#ggplot(subset(buoy1416_vert_temphum, subset=(datetime>=as.POSIXct('2016-06-01', tz='GMT') & datetime<as.POSIXct('2016-06-16', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='early June 2016', x='date', y=' ') +
  final_theme

#ggplot(subset(buoy1416_vert_temphum, subset=(datetime>=as.POSIXct('2016-06-16', tz='GMT') & datetime<as.POSIXct('2016-07-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='late June 2016', x='date', y=' ') +
  final_theme

#ggplot(subset(buoy1416_vert_temphum, subset=(datetime>=as.POSIXct('2016-07-01', tz='GMT') & datetime<as.POSIXct('2016-07-16', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='early July 2016', x='date', y=' ') +
  final_theme

##errant temp jump on Jul 1
#ggplot(subset(buoy1416_vert_temphum, subset=(datetime>=as.POSIXct('2016-07-01 12:00', tz='GMT') & datetime<as.POSIXct('2016-07-02 6:00', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='early July 2016', x='date', y=' ') +
  final_theme

ix=which(buoy1416_L1$datetime>=as.POSIXct('2016-07-01 20:00', tz='GMT') & buoy1416_L1$datetime<=as.POSIXct('2016-07-01 21:20', tz='GMT'))
for (i in c('AirTempC', 'RelHum')) {buoy1416_L1[ix, i]= NA}
buoy1416_temphum_L1 <- subset(buoy1416_L1, select=c('datetime', 'AirTempC', 'RelHum'))
buoy1416_vert_temphum_L1 <- melt(buoy1416_temphum_L1, id='datetime')

#ggplot(subset(buoy1416_vert_temphum_L1, subset=(datetime>=as.POSIXct('2016-07-01', tz='GMT') & datetime<as.POSIXct('2016-07-16', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='early July 2016, v2', x='date', y=' ') +
  final_theme

 

#ggplot(subset(buoy1416_vert_temphum, subset=(datetime>=as.POSIXct('2016-07-16', tz='GMT') & datetime<as.POSIXct('2016-08-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='late July 2016', x='date', y=' ') +
  final_theme


#ggplot(subset(buoy1416_vert_temphum, subset=(datetime>=as.POSIXct('2016-07-23', tz='GMT') & datetime<as.POSIXct('2016-07-24', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='early July 2016', x='date', y=' ') +
  final_theme

#ggplot(subset(buoy1416_vert_temphum, subset=(datetime>=as.POSIXct('2016-08-01', tz='GMT') & datetime<as.POSIXct('2016-08-16', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='early August 2016', x='date', y=' ') +
  final_theme

#ggplot(subset(buoy1416_vert_temphum, subset=(datetime>=as.POSIXct('2016-08-16', tz='GMT') & datetime<as.POSIXct('2016-09-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='late August 2016', x='date', y=' ') +
  final_theme

#ggplot(subset(buoy1416_vert_temphum, subset=(datetime>=as.POSIXct('2016-09-01', tz='GMT') & datetime<as.POSIXct('2016-09-16', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='early September 2016', x='date', y=' ') +
  final_theme

#ggplot(subset(buoy1416_vert_temphum, subset=(datetime>=as.POSIXct('2016-09-16', tz='GMT') & datetime<as.POSIXct('2016-10-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='late September 2016', x='date', y=' ') +
  final_theme

#ggplot(subset(buoy1416_vert_temphum, subset=(datetime>=as.POSIXct('2016-10-01', tz='GMT') & datetime<as.POSIXct('2016-10-16', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='early October 2016', x='date', y=' ') +
  final_theme

#ggplot(subset(buoy1416_vert_temphum, subset=(datetime>=as.POSIXct('2016-10-16', tz='GMT') & datetime<as.POSIXct('2016-11-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='late October 2016', x='date', y=' ') +
  final_theme

##errant rh points on Oct 19/20
#ggplot(subset(buoy1416_vert_temphum, subset=(datetime>=as.POSIXct('2016-10-18 12:00', tz='GMT') & datetime<as.POSIXct('2016-10-19 6:00', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='Oct 19/20 2016', x='date', y=' ') +
  final_theme

ix=which(buoy1416_L1$datetime>=as.POSIXct('2016-10-18', tz='GMT') & buoy1416_L1$datetime<=as.POSIXct('2016-10-19', tz='GMT') & buoy1416_L1$RelHum>120)
for (i in 'RelHum') {buoy1416_L1[ix, i]= NA}
buoy1416_temphum_L1 <- subset(buoy1416_L1, select=c('datetime', 'AirTempC', 'RelHum'))
buoy1416_vert_temphum_L1 <- melt(buoy1416_temphum_L1, id='datetime')

#ggplot(subset(buoy1416_vert_temphum_L1, subset=(datetime>=as.POSIXct('2016-10-18 12:00', tz='GMT') & datetime<as.POSIXct('2016-10-19 6:00', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='Oct 19/20 2016', x='date', y=' ') +
  final_theme

#ggplot(subset(buoy1416_vert_temphum_L1, subset=(datetime>=as.POSIXct('2016-10-16', tz='GMT') & datetime<as.POSIXct('2016-11-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='late October 2016, v2', x='date', y=' ') +
  final_theme



#ggplot(subset(buoy1416_vert_temphum, subset=(datetime>=as.POSIXct('2016-11-01', tz='GMT') & datetime<as.POSIXct('2016-11-16', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='early November 2016', x='date', y=' ') +
  final_theme

#ggplot(subset(buoy1416_vert_temphum, subset=(datetime>=as.POSIXct('2016-11-16', tz='GMT') & datetime<as.POSIXct('2016-12-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='late November 2016', x='date', y=' ') +
  final_theme

#ggplot(subset(buoy1416_vert_temphum, subset=(datetime>=as.POSIXct('2016-12-01', tz='GMT') & datetime<as.POSIXct('2016-12-16', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='early December 2016', x='date', y=' ') +
  final_theme

#ggplot(subset(buoy1416_vert_temphum, subset=(datetime>=as.POSIXct('2016-12-16', tz='GMT') & datetime<as.POSIXct('2017-01-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='late December 2016', x='date', y=' ') +
  final_theme



#ggplot(subset(buoy1416_vert_temphum_L1, subset=(datetime>=as.POSIXct('2016-01-01', tz='GMT') & datetime<as.POSIXct('2017-01-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='2016, clean', x='date', y=' ') +
  final_theme






####Wind data ####
wind <- c('WindSpdAv', 'WindVect', 'MaxWind', 'MaxWindDir', 'CorrWind', 'WindSpd')
buoy1416_wind <- subset(buoy1416_L1, select=c('datetime', wind))
buoy1416_vert_wind <- melt(buoy1416_wind, id='datetime')

####2014####
#ggplot(subset(buoy1416_vert_wind, subset=(datetime>=as.POSIXct('2014-01-01', tz='GMT') & datetime<as.POSIXct('2015-01-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='2014, raw', x='date', y=' ') +
  final_theme

#ggplot(subset(buoy1416_vert_wind, subset=(datetime>=as.POSIXct('2014-01-01', tz='GMT') & datetime<as.POSIXct('2014-01-16', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() +   
  facet_grid(variable~., scales='free_y') +
  labs(title='early January 2014', x='date', y=' ') +
  final_theme

##errant readings until Jan 5
#ggplot(subset(buoy1416_vert_wind, subset=(datetime>=as.POSIXct('2014-01-05', tz='GMT') & datetime<as.POSIXct('2014-01-06', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() +   
  facet_grid(variable~., scales='free_y') +
  labs(title='early January 2014', x='date', y=' ') +
  final_theme

ix=which(buoy1416_L1$datetime>=as.POSIXct('2014-01-01', tz='GMT') & buoy1416_L1$datetime<=as.POSIXct('2014-01-05 12:30', tz='GMT'))
for (i in wind) {buoy1416_L1[ix, i]= NA}
buoy1416_wind_L1 <- subset(buoy1416_L1, select=c('datetime', wind))
buoy1416_vert_wind_L1 <- melt(buoy1416_wind_L1, id='datetime')

#ggplot(subset(buoy1416_vert_wind_L1, subset=(datetime>=as.POSIXct('2014-01-01', tz='GMT') & datetime<as.POSIXct('2014-01-16', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() +   
  facet_grid(variable~., scales='free_y') +
  labs(title='early January 2014, v2', x='date', y=' ') +
  final_theme


#ggplot(subset(buoy1416_vert_wind, subset=(datetime>=as.POSIXct('2014-01-16', tz='GMT') & datetime<as.POSIXct('2014-02-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='late January 2014', x='date', y=' ') +
  final_theme

#ggplot(subset(buoy1416_vert_wind, subset=(datetime>=as.POSIXct('2014-02-01', tz='GMT') & datetime<as.POSIXct('2014-02-14', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='early February 2014', x='date', y=' ') +
  final_theme

#ggplot(subset(buoy1416_vert_wind, subset=(datetime>=as.POSIXct('2014-02-14', tz='GMT') & datetime<as.POSIXct('2014-03-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='late February 2014', x='date', y=' ') +
  final_theme

## 0 values followed by >30m/s
#ggplot(subset(buoy1416_vert_wind, subset=(datetime>=as.POSIXct('2014-02-14', tz='GMT') & datetime<as.POSIXct('2014-02-15', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='late February 2014', x='date', y=' ') +
  final_theme

ix=which(buoy1416_L1$datetime>=as.POSIXct('2014-02-14 4:50', tz='GMT') & buoy1416_L1$datetime<=as.POSIXct('2014-02-14 12:40', tz='GMT'))
for (i in wind) {buoy1416_L1[ix, i]= NA}
ix=which(buoy1416_L1$datetime>=as.POSIXct('2014-02-14', tz='GMT') & buoy1416_L1$datetime<=as.POSIXct('2014-02-15', tz='GMT') & buoy1416_L1$MaxWind>30)
for (i in wind) {buoy1416_L1[ix, i]= NA}
buoy1416_wind_L1 <- subset(buoy1416_L1, select=c('datetime', wind))
buoy1416_vert_wind_L1 <- melt(buoy1416_wind_L1, id='datetime')

#ggplot(subset(buoy1416_vert_wind_L1, subset=(datetime>=as.POSIXct('2014-02-14', tz='GMT') & datetime<as.POSIXct('2014-03-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='late February 2014, v2', x='date', y=' ') +
  final_theme

#ggplot(subset(buoy1416_vert_wind_L1, subset=(datetime>=as.POSIXct('2014-02-19', tz='GMT') & datetime<as.POSIXct('2014-02-20', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='late February 2014, v2', x='date', y=' ') +
  final_theme

ix=which(buoy1416_L1$datetime>=as.POSIXct('2014-02-19', tz='GMT') & buoy1416_L1$datetime<=as.POSIXct('2014-02-19 12:00', tz='GMT') & buoy1416_L1$MaxWind==0)
for (i in wind) {buoy1416_L1[ix, i]= NA}
buoy1416_wind_L1 <- subset(buoy1416_L1, select=c('datetime', wind))
buoy1416_vert_wind_L1 <- melt(buoy1416_wind_L1, id='datetime')

#ggplot(subset(buoy1416_vert_wind_L1, subset=(datetime>=as.POSIXct('2014-02-14', tz='GMT') & datetime<as.POSIXct('2014-03-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='late February 2014, v3', x='date', y=' ') +
  final_theme


#ggplot(subset(buoy1416_vert_wind, subset=(datetime>=as.POSIXct('2014-03-01', tz='GMT') & datetime<as.POSIXct('2014-03-16', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='early March 2014', x='date', y=' ') +
  final_theme

#ggplot(subset(buoy1416_vert_wind, subset=(datetime>=as.POSIXct('2014-03-16', tz='GMT') & datetime<as.POSIXct('2014-04-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='late March 2014', x='date', y=' ') +
  final_theme

#ggplot(subset(buoy1416_vert_wind, subset=(datetime>=as.POSIXct('2014-04-01', tz='GMT') & datetime<as.POSIXct('2014-04-16', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='early April 2014', x='date', y=' ') +
  final_theme

#data gap until June

#ggplot(subset(buoy1416_vert_wind, subset=(datetime>=as.POSIXct('2014-06-01', tz='GMT') & datetime<as.POSIXct('2014-06-16', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='early June 2014', x='date', y=' ') +
  final_theme

#ggplot(subset(buoy1416_vert_wind, subset=(datetime>=as.POSIXct('2014-06-16', tz='GMT') & datetime<as.POSIXct('2014-07-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='late June 2014', x='date', y=' ') +
  final_theme

#ggplot(subset(buoy1416_vert_wind, subset=(datetime>=as.POSIXct('2014-07-01', tz='GMT') & datetime<as.POSIXct('2014-07-16', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='early July 2014', x='date', y=' ') +
  final_theme

#ggplot(subset(buoy1416_vert_wind, subset=(datetime>=as.POSIXct('2014-07-16', tz='GMT') & datetime<as.POSIXct('2014-08-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='late July 2014', x='date', y=' ') +
  final_theme

#ggplot(subset(buoy1416_vert_wind, subset=(datetime>=as.POSIXct('2014-08-01', tz='GMT') & datetime<as.POSIXct('2014-08-16', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='early August 2014', x='date', y=' ') +
  final_theme

#ggplot(subset(buoy1416_vert_wind, subset=(datetime>=as.POSIXct('2014-08-16', tz='GMT') & datetime<as.POSIXct('2014-09-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='late August 2014', x='date', y=' ') +
  final_theme

#ggplot(subset(buoy1416_vert_wind, subset=(datetime>=as.POSIXct('2014-09-01', tz='GMT') & datetime<as.POSIXct('2014-09-16', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='early September 2014', x='date', y=' ') +
  final_theme

#ggplot(subset(buoy1416_vert_wind, subset=(datetime>=as.POSIXct('2014-09-16', tz='GMT') & datetime<as.POSIXct('2014-10-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='late September 2014', x='date', y=' ') +
  final_theme

#ggplot(subset(buoy1416_vert_wind, subset=(datetime>=as.POSIXct('2014-10-01', tz='GMT') & datetime<as.POSIXct('2014-10-16', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='early October 2014', x='date', y=' ') +
  final_theme

  #ggplot(subset(buoy1416_vert_wind, subset=(datetime>=as.POSIXct('2014-10-16', tz='GMT') & datetime<as.POSIXct('2014-11-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='late October 2014', x='date', y=' ') +
  final_theme

#ggplot(subset(buoy1416_vert_wind, subset=(datetime>=as.POSIXct('2014-10-24', tz='GMT') & datetime<as.POSIXct('2014-10-25', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='late October 2014', x='date', y=' ') +
  final_theme

#ggplot(subset(buoy1416_vert_wind, subset=(datetime>=as.POSIXct('2014-11-01', tz='GMT') & datetime<as.POSIXct('2014-11-16', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='early November 2014', x='date', y=' ') +
  final_theme

#ggplot(subset(buoy1416_vert_wind, subset=(datetime>=as.POSIXct('2014-11-16', tz='GMT') & datetime<as.POSIXct('2014-12-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='late Novebmer 2014', x='date', y=' ') +
  final_theme

## errant data around Nov 26-30
#ggplot(subset(buoy1416_vert_wind, subset=(datetime>=as.POSIXct('2014-11-26', tz='GMT') & datetime<as.POSIXct('2014-11-27', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='late Novebmer 2014', x='date', y=' ') +
  final_theme

#ggplot(subset(buoy1416_vert_wind, subset=(datetime>=as.POSIXct('2014-11-30', tz='GMT') & datetime<as.POSIXct('2014-12-1', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='late Novebmer 2014', x='date', y=' ') +
  final_theme

## errant wind values nov 26-30
ix=which(buoy1416_L1$datetime>=as.POSIXct('2014-11-26 12:20', tz='GMT') & buoy1416_L1$datetime<=as.POSIXct('2014-11-30 8:50', tz='GMT'))
for (i in wind) {buoy1416_L1[ix, i]= NA}
buoy1416_wind_L1 <- subset(buoy1416_L1, select=c('datetime', wind))
buoy1416_vert_wind_L1 <- melt(buoy1416_wind_L1, id='datetime')

#ggplot(subset(buoy1416_vert_wind_L1, subset=(datetime>=as.POSIXct('2014-11-16', tz='GMT') & datetime<as.POSIXct('2014-12-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='late Novebmer 2014, v2', x='date', y=' ') +
  final_theme


#ggplot(subset(buoy1416_vert_wind, subset=(datetime>=as.POSIXct('2014-12-01', tz='GMT') & datetime<as.POSIXct('2014-12-16', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='early December 2014', x='date', y=' ') +
  final_theme

#errant data around dec 11
#ggplot(subset(buoy1416_vert_wind, subset=(datetime>=as.POSIXct('2014-12-11 6:00', tz='GMT') & datetime<as.POSIXct('2014-12-11 12:00', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='early December 2014', x='date', y=' ') +
  final_theme
#ggplot(subset(buoy1416_vert_wind, subset=(datetime>=as.POSIXct('2014-12-12 12:00', tz='GMT') & datetime<as.POSIXct('2014-12-12 18:00', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='early December 2014', x='date', y=' ') +
  final_theme

## errant values dec 11/12
ix=which(buoy1416_L1$datetime>=as.POSIXct('2014-12-11 9:20', tz='GMT') & buoy1416_L1$datetime<=as.POSIXct('2014-12-12 14:00', tz='GMT'))
for (i in wind) {buoy1416_L1[ix, i]= NA}
buoy1416_wind_L1 <- subset(buoy1416_L1, select=c('datetime', wind))
buoy1416_vert_wind_L1 <- melt(buoy1416_wind_L1, id='datetime')

#ggplot(subset(buoy1416_vert_wind_L1, subset=(datetime>=as.POSIXct('2014-12-01', tz='GMT') & datetime<as.POSIXct('2014-12-16', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='early December 2014, v2', x='date', y=' ') +
  final_theme


#ggplot(subset(buoy1416_vert_wind, subset=(datetime>=as.POSIXct('2014-12-16', tz='GMT') & datetime<as.POSIXct('2015-01-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='late December 2014', x='date', y=' ') +
  final_theme


#ggplot(subset(buoy1416_vert_wind_L1, subset=(datetime>=as.POSIXct('2014-01-01', tz='GMT') & datetime<as.POSIXct('2015-01-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='2014, clean', x='date', y=' ') +
  final_theme


####2015####
#ggplot(subset(buoy1416_vert_wind, subset=(datetime>=as.POSIXct('2015-01-01', tz='GMT') & datetime<as.POSIXct('2016-01-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='2015, raw', x='date', y=' ') +
  final_theme

#ggplot(subset(buoy1416_vert_wind, subset=(datetime>=as.POSIXct('2015-01-01', tz='GMT') & datetime<as.POSIXct('2015-01-16', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() +   
  facet_grid(variable~., scales='free_y') +
  labs(title='early January 2015', x='date', y=' ') +
  final_theme

#ggplot(subset(buoy1416_vert_wind, subset=(datetime>=as.POSIXct('2015-01-16', tz='GMT') & datetime<as.POSIXct('2015-02-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='late January 2015', x='date', y=' ') +
  final_theme

#ggplot(subset(buoy1416_vert_wind, subset=(datetime>=as.POSIXct('2015-02-01', tz='GMT') & datetime<as.POSIXct('2015-02-14', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='early February 2015', x='date', y=' ') +
  final_theme

#ggplot(subset(buoy1416_vert_wind, subset=(datetime>=as.POSIXct('2015-02-14', tz='GMT') & datetime<as.POSIXct('2015-03-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='late February 2015', x='date', y=' ') +
  final_theme

## flat lines on feb 15 and 19

#ggplot(subset(buoy1416_vert_wind, subset=(datetime>=as.POSIXct('2015-02-15', tz='GMT') & datetime<as.POSIXct('2015-02-16', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='late February 2015', x='date', y=' ') +
  final_theme
#ggplot(subset(buoy1416_vert_wind, subset=(datetime>=as.POSIXct('2015-02-18 18:00', tz='GMT') & datetime<as.POSIXct('2015-02-19 12:00', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='late February 2015', x='date', y=' ') +
  final_theme

ix=which(buoy1416_L1$datetime>=as.POSIXct('2015-02-15 00:40', tz='GMT') & buoy1416_L1$datetime<=as.POSIXct('2015-02-15 14:00', tz='GMT'))
for (i in wind) {buoy1416_L1[ix, i]= NA}
ix=which(buoy1416_L1$datetime>=as.POSIXct('2015-02-18 22:40', tz='GMT') & buoy1416_L1$datetime<=as.POSIXct('2015-02-19 10:50', tz='GMT'))
for (i in wind) {buoy1416_L1[ix, i]= NA}
buoy1416_wind_L1 <- subset(buoy1416_L1, select=c('datetime', wind))
buoy1416_vert_wind_L1 <- melt(buoy1416_wind_L1, id='datetime')

#ggplot(subset(buoy1416_vert_wind_L1, subset=(datetime>=as.POSIXct('2015-02-14', tz='GMT') & datetime<as.POSIXct('2015-03-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='late February 2015, v2', x='date', y=' ') +
  final_theme


#ggplot(subset(buoy1416_vert_wind, subset=(datetime>=as.POSIXct('2015-03-01', tz='GMT') & datetime<as.POSIXct('2015-03-16', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='early March 2015', x='date', y=' ') +
  final_theme

#ggplot(subset(buoy1416_vert_wind, subset=(datetime>=as.POSIXct('2015-03-16', tz='GMT') & datetime<as.POSIXct('2015-04-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='late March 2015', x='date', y=' ') +
  final_theme

#ggplot(subset(buoy1416_vert_wind, subset=(datetime>=as.POSIXct('2015-04-01', tz='GMT') & datetime<as.POSIXct('2015-04-16', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='early April 2015', x='date', y=' ') +
  final_theme

#ggplot(subset(buoy1416_vert_wind, subset=(datetime>=as.POSIXct('2015-04-16', tz='GMT') & datetime<as.POSIXct('2015-05-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='late April 2015', x='date', y=' ') +
  final_theme


#data gap until June

#ggplot(subset(buoy1416_vert_wind, subset=(datetime>=as.POSIXct('2015-06-01', tz='GMT') & datetime<as.POSIXct('2015-06-16', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='early June 2015', x='date', y=' ') +
  final_theme

#errant data beginning of June
#ggplot(subset(buoy1416_vert_wind, subset=(datetime>=as.POSIXct('2015-06-01', tz='GMT') & datetime<as.POSIXct('2015-06-05', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='early June 2015', x='date', y=' ') +
  final_theme

ix=which(buoy1416_L1$datetime>=as.POSIXct('2015-06-01 00:00', tz='GMT') & buoy1416_L1$datetime<=as.POSIXct('2015-06-04 10:00', tz='GMT'))
for (i in wind) {buoy1416_L1[ix, i]= NA}
buoy1416_wind_L1 <- subset(buoy1416_L1, select=c('datetime', wind))
buoy1416_vert_wind_L1 <- melt(buoy1416_wind_L1, id='datetime')

#ggplot(subset(buoy1416_vert_wind_L1, subset=(datetime>=as.POSIXct('2015-06-01', tz='GMT') & datetime<as.POSIXct('2015-06-16', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='early June 2015, v2', x='date', y=' ') +
  final_theme



#corr wind and windspd out 
wind2 <- c('WindSpdAv', 'WindVect', 'MaxWind', 'MaxWindDir')
buoy1416_wind <- subset(buoy1416_L1, select=c('datetime', wind2))
buoy1416_vert_wind <- melt(buoy1416_wind, id='datetime')


#ggplot(subset(buoy1416_vert_wind, subset=(datetime>=as.POSIXct('2015-06-16', tz='GMT') & datetime<as.POSIXct('2015-07-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='late June 2015', x='date', y=' ') +
  final_theme

#ggplot(subset(buoy1416_vert_wind, subset=(datetime>=as.POSIXct('2015-07-01', tz='GMT') & datetime<as.POSIXct('2015-07-16', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='early July 2015', x='date', y=' ') +
  final_theme

#ggplot(subset(buoy1416_vert_wind, subset=(datetime>=as.POSIXct('2015-07-16', tz='GMT') & datetime<as.POSIXct('2015-08-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='late July 2015', x='date', y=' ') +
  final_theme

#ggplot(subset(buoy1416_vert_wind, subset=(datetime>=as.POSIXct('2015-08-01', tz='GMT') & datetime<as.POSIXct('2015-08-16', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='early August 2015', x='date', y=' ') +
  final_theme

#ggplot(subset(buoy1416_vert_wind, subset=(datetime>=as.POSIXct('2015-08-16', tz='GMT') & datetime<as.POSIXct('2015-09-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='late August 2015', x='date', y=' ') +
  final_theme

#ggplot(subset(buoy1416_vert_wind, subset=(datetime>=as.POSIXct('2015-09-01', tz='GMT') & datetime<as.POSIXct('2015-09-16', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='early September 2015', x='date', y=' ') +
  final_theme

#ggplot(subset(buoy1416_vert_wind, subset=(datetime>=as.POSIXct('2015-09-16', tz='GMT') & datetime<as.POSIXct('2015-10-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='late September 2015', x='date', y=' ') +
  final_theme

#ggplot(subset(buoy1416_vert_wind, subset=(datetime>=as.POSIXct('2015-10-01', tz='GMT') & datetime<as.POSIXct('2015-10-16', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='early October 2015', x='date', y=' ') +
  final_theme

#ggplot(subset(buoy1416_vert_wind, subset=(datetime>=as.POSIXct('2015-10-16', tz='GMT') & datetime<as.POSIXct('2015-11-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='late October 2015', x='date', y=' ') +
  final_theme

#ggplot(subset(buoy1416_vert_wind, subset=(datetime>=as.POSIXct('2015-11-01', tz='GMT') & datetime<as.POSIXct('2015-11-16', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='early November 2015', x='date', y=' ') +
  final_theme

#ggplot(subset(buoy1416_vert_wind, subset=(datetime>=as.POSIXct('2015-11-16', tz='GMT') & datetime<as.POSIXct('2015-12-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='late Novebmer 2015', x='date', y=' ') +
  final_theme

#ggplot(subset(buoy1416_vert_wind, subset=(datetime>=as.POSIXct('2015-12-01', tz='GMT') & datetime<as.POSIXct('2015-12-16', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='early December 2015', x='date', y=' ') +
  final_theme

#ggplot(subset(buoy1416_vert_wind, subset=(datetime>=as.POSIXct('2015-12-16', tz='GMT') & datetime<as.POSIXct('2016-01-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='late December 2015', x='date', y=' ') +
  final_theme

#errant wind data 30-31
#ggplot(subset(buoy1416_vert_wind, subset=(datetime>=as.POSIXct('2015-12-29 12:00', tz='GMT') & datetime<as.POSIXct('2016-01-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='late December 2015', x='date', y=' ') +
  final_theme
#ggplot(subset(buoy1416_vert_wind, subset=(datetime>=as.POSIXct('2015-12-29 12:00', tz='GMT') & datetime<as.POSIXct('2015-12-30', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='late December 2015', x='date', y=' ') +
  final_theme
#ggplot(subset(buoy1416_vert_wind, subset=(datetime>=as.POSIXct('2015-12-31', tz='GMT') & datetime<as.POSIXct('2016-01-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='late December 2015', x='date', y=' ') +
  final_theme

ix=which(buoy1416_L1$datetime>=as.POSIXct('2015-12-29 17:30', tz='GMT') & buoy1416_L1$datetime<=as.POSIXct('2015-12-31 7:10', tz='GMT'))
for (i in wind) {buoy1416_L1[ix, i]= NA}
buoy1416_wind_L1 <- subset(buoy1416_L1, select=c('datetime', wind2))
buoy1416_vert_wind_L1 <- melt(buoy1416_wind_L1, id='datetime')

#ggplot(subset(buoy1416_vert_wind_L1, subset=(datetime>=as.POSIXct('2015-12-16', tz='GMT') & datetime<as.POSIXct('2016-01-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='late December 2015, v2', x='date', y=' ') +
  final_theme

buoy1416_wind_L1 <- subset(buoy1416_L1, select=c('datetime', wind))
buoy1416_vert_wind_L1 <- melt(buoy1416_wind_L1, id='datetime')

#ggplot(subset(buoy1416_vert_wind_L1, subset=(datetime>=as.POSIXct('2015-01-01', tz='GMT') & datetime<as.POSIXct('2016-01-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='2015, clean', x='date', y=' ') +
  final_theme


####2016####
buoy1416_wind <- subset(buoy1416_L1, select=c('datetime', wind2))
buoy1416_vert_wind <- melt(buoy1416_wind, id='datetime')
  
#ggplot(subset(buoy1416_vert_wind, subset=(datetime>=as.POSIXct('2016-01-01', tz='GMT') & datetime<as.POSIXct('2017-01-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='2016, raw', x='date', y=' ') +
  final_theme

#ggplot(subset(buoy1416_vert_wind, subset=(datetime>=as.POSIXct('2016-01-01', tz='GMT') & datetime<as.POSIXct('2016-01-16', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() +   
  facet_grid(variable~., scales='free_y') +
  labs(title='early January 2016', x='date', y=' ') +
  final_theme

#ggplot(subset(buoy1416_vert_wind, subset=(datetime>=as.POSIXct('2016-01-16', tz='GMT') & datetime<as.POSIXct('2016-02-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='late January 2016', x='date', y=' ') +
  final_theme

#ggplot(subset(buoy1416_vert_wind, subset=(datetime>=as.POSIXct('2016-02-01', tz='GMT') & datetime<as.POSIXct('2016-02-14', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='early February 2016', x='date', y=' ') +
  final_theme

#errant data on feb 9
#ggplot(subset(buoy1416_vert_wind, subset=(datetime>=as.POSIXct('2016-02-09', tz='GMT') & datetime<as.POSIXct('2016-02-10', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='early February 2016', x='date', y=' ') +
  final_theme

ix=which(buoy1416_L1$datetime>=as.POSIXct('2016-02-09 00:10', tz='GMT') & buoy1416_L1$datetime<=as.POSIXct('2016-02-09 12:20', tz='GMT'))
for (i in wind) {buoy1416_L1[ix, i]= NA}
buoy1416_wind_L1 <- subset(buoy1416_L1, select=c('datetime', wind2))
buoy1416_vert_wind_L1 <- melt(buoy1416_wind_L1, id='datetime')

#ggplot(subset(buoy1416_vert_wind_L1, subset=(datetime>=as.POSIXct('2016-02-01', tz='GMT') & datetime<as.POSIXct('2016-02-14', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='early February 2016, v2', x='date', y=' ') +
  final_theme


#ggplot(subset(buoy1416_vert_wind, subset=(datetime>=as.POSIXct('2016-02-14', tz='GMT') & datetime<as.POSIXct('2016-03-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='late February 2016', x='date', y=' ') +
  final_theme

##errant data on feb 24-25
#ggplot(subset(buoy1416_vert_wind, subset=(datetime>=as.POSIXct('2016-02-24', tz='GMT') & datetime<as.POSIXct('2016-02-25', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='late February 2016', x='date', y=' ') +
  final_theme

ix=which(buoy1416_L1$datetime>=as.POSIXct('2016-02-24 11:40', tz='GMT') & buoy1416_L1$datetime<=as.POSIXct('2016-02-24 17:30', tz='GMT'))
for (i in wind) {buoy1416_L1[ix, i]= NA}
buoy1416_wind_L1 <- subset(buoy1416_L1, select=c('datetime', wind2))
buoy1416_vert_wind_L1 <- melt(buoy1416_wind_L1, id='datetime')

#ggplot(subset(buoy1416_vert_wind_L1, subset=(datetime>=as.POSIXct('2016-02-14', tz='GMT') & datetime<as.POSIXct('2016-03-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='late February 2016, v2', x='date', y=' ') +
  final_theme



#ggplot(subset(buoy1416_vert_wind, subset=(datetime>=as.POSIXct('2016-03-01', tz='GMT') & datetime<as.POSIXct('2016-03-16', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='early March 2016', x='date', y=' ') +
  final_theme

#ggplot(subset(buoy1416_vert_wind, subset=(datetime>=as.POSIXct('2016-03-16', tz='GMT') & datetime<as.POSIXct('2016-04-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='late March 2016', x='date', y=' ') +
  final_theme

##Mar 25 errant data
#ggplot(subset(buoy1416_vert_wind, subset=(datetime>=as.POSIXct('2016-03-25', tz='GMT') & datetime<as.POSIXct('2016-03-26', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='late March 2016', x='date', y=' ') +
  final_theme

ix=which(buoy1416_L1$datetime>=as.POSIXct('2016-03-25 9:10', tz='GMT') & buoy1416_L1$datetime<=as.POSIXct('2016-03-25 12:00', tz='GMT'))
for (i in wind) {buoy1416_L1[ix, i]= NA}
buoy1416_wind_L1 <- subset(buoy1416_L1, select=c('datetime', wind2))
buoy1416_vert_wind_L1 <- melt(buoy1416_wind_L1, id='datetime')

#ggplot(subset(buoy1416_vert_wind_L1, subset=(datetime>=as.POSIXct('2016-03-16', tz='GMT') & datetime<as.POSIXct('2016-04-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='late March 2016, v2', x='date', y=' ') +
  final_theme



#ggplot(subset(buoy1416_vert_wind, subset=(datetime>=as.POSIXct('2016-04-01', tz='GMT') & datetime<as.POSIXct('2016-04-16', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='early April 2016', x='date', y=' ') +
  final_theme

#ggplot(subset(buoy1416_vert_wind, subset=(datetime>=as.POSIXct('2016-04-16', tz='GMT') & datetime<as.POSIXct('2016-05-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='late April 2016', x='date', y=' ') +
  final_theme

#ggplot(subset(buoy1416_vert_wind, subset=(datetime>=as.POSIXct('2016-05-01', tz='GMT') & datetime<as.POSIXct('2016-05-16', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='early May 2016', x='date', y=' ') +
  final_theme

#ggplot(subset(buoy1416_vert_wind, subset=(datetime>=as.POSIXct('2016-05-16', tz='GMT') & datetime<as.POSIXct('2016-06-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='late May 2016', x='date', y=' ') +
  final_theme

#ggplot(subset(buoy1416_vert_wind, subset=(datetime>=as.POSIXct('2016-06-01', tz='GMT') & datetime<as.POSIXct('2016-06-16', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='early June 2016', x='date', y=' ') +
  final_theme

#ggplot(subset(buoy1416_vert_wind, subset=(datetime>=as.POSIXct('2016-06-16', tz='GMT') & datetime<as.POSIXct('2016-07-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='late June 2016', x='date', y=' ') +
  final_theme

#ggplot(subset(buoy1416_vert_wind, subset=(datetime>=as.POSIXct('2016-07-01', tz='GMT') & datetime<as.POSIXct('2016-07-16', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='early July 2016', x='date', y=' ') +
  final_theme

#ggplot(subset(buoy1416_vert_wind, subset=(datetime>=as.POSIXct('2016-07-16', tz='GMT') & datetime<as.POSIXct('2016-08-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='late July 2016', x='date', y=' ') +
  final_theme

#ggplot(subset(buoy1416_vert_wind, subset=(datetime>=as.POSIXct('2016-08-01', tz='GMT') & datetime<as.POSIXct('2016-08-16', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='early August 2016', x='date', y=' ') +
  final_theme

#ggplot(subset(buoy1416_vert_wind, subset=(datetime>=as.POSIXct('2016-08-16', tz='GMT') & datetime<as.POSIXct('2016-09-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='late August 2016', x='date', y=' ') +
  final_theme

#ggplot(subset(buoy1416_vert_wind, subset=(datetime>=as.POSIXct('2016-09-01', tz='GMT') & datetime<as.POSIXct('2016-09-16', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='early September 2016', x='date', y=' ') +
  final_theme

#ggplot(subset(buoy1416_vert_wind, subset=(datetime>=as.POSIXct('2016-09-16', tz='GMT') & datetime<as.POSIXct('2016-10-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='late September 2016', x='date', y=' ') +
  final_theme

#ggplot(subset(buoy1416_vert_wind, subset=(datetime>=as.POSIXct('2016-10-01', tz='GMT') & datetime<as.POSIXct('2016-10-16', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='early October 2016', x='date', y=' ') +
  final_theme

#ggplot(subset(buoy1416_vert_wind, subset=(datetime>=as.POSIXct('2016-10-16', tz='GMT') & datetime<as.POSIXct('2016-11-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='late October 2016', x='date', y=' ') +
  final_theme

#ggplot(subset(buoy1416_vert_wind, subset=(datetime>=as.POSIXct('2016-11-01', tz='GMT') & datetime<as.POSIXct('2016-11-16', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='early November 2016', x='date', y=' ') +
  final_theme

#ggplot(subset(buoy1416_vert_wind, subset=(datetime>=as.POSIXct('2016-11-16', tz='GMT') & datetime<as.POSIXct('2016-12-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='late November 2016', x='date', y=' ') +
  final_theme

#ggplot(subset(buoy1416_vert_wind, subset=(datetime>=as.POSIXct('2016-12-01', tz='GMT') & datetime<as.POSIXct('2016-12-16', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='early December 2016', x='date', y=' ') +
  final_theme

#ggplot(subset(buoy1416_vert_wind, subset=(datetime>=as.POSIXct('2016-12-16', tz='GMT') & datetime<as.POSIXct('2017-01-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='late December 2016', x='date', y=' ') +
  final_theme

##errant data on dec 18 and dec 30
#ggplot(subset(buoy1416_vert_wind, subset=(datetime>=as.POSIXct('2016-12-18', tz='GMT') & datetime<as.POSIXct('2016-12-19', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='late December 2016', x='date', y=' ') +
  final_theme
#ggplot(subset(buoy1416_vert_wind, subset=(datetime>=as.POSIXct('2016-12-29 18:00', tz='GMT') & datetime<as.POSIXct('2016-12-30 12:00', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='late December 2016', x='date', y=' ') +
  final_theme

ix=which(buoy1416_L1$datetime>=as.POSIXct('2016-12-18 3:40', tz='GMT') & buoy1416_L1$datetime<=as.POSIXct('2016-12-18 7:40', tz='GMT'))
for (i in wind) {buoy1416_L1[ix, i]= NA}
ix=which(buoy1416_L1$datetime>=as.POSIXct('2016-12-29 20:40', tz='GMT') & buoy1416_L1$datetime<=as.POSIXct('2016-12-30 10:40', tz='GMT'))
for (i in wind) {buoy1416_L1[ix, i]= NA}
buoy1416_wind_L1 <- subset(buoy1416_L1, select=c('datetime', wind2))
buoy1416_vert_wind_L1 <- melt(buoy1416_wind_L1, id='datetime')

#ggplot(subset(buoy1416_vert_wind_L1, subset=(datetime>=as.POSIXct('2016-12-16', tz='GMT') & datetime<as.POSIXct('2017-01-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='late December 2016, v2', x='date', y=' ') +
  final_theme


#ggplot(subset(buoy1416_vert_wind_L1, subset=(datetime>=as.POSIXct('2016-01-01', tz='GMT') & datetime<as.POSIXct('2017-01-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='2016, clean', x='date', y=' ') +
  final_theme





####chla data ####
chla <- c('Chlor_RFU', 'Chlor_UGL', 'SpecCond')

range(buoy1416_L1$Chlor_RFU, na.rm = T)
ix=which(buoy1416_L1$Chlor_RFU==-6999)
buoy1416_L1$Chlor_RFU[ix]=NA
ix=which(buoy1416_L1$Chlor_RFU==-3.3)
buoy1416_L1$Chlor_RFU[ix]=NA

range(buoy1416_L1$Chlor_UGL, na.rm = T)
ix=which(buoy1416_L1$Chlor_UGL==-6999)
buoy1416_L1$Chlor_UGL[ix]=NA
ix=which(buoy1416_L1$Chlor_UGL==6999)
buoy1416_L1$Chlor_UGL[ix]=NA
ix=which(buoy1416_L1$Chlor_UGL==183)
buoy1416_L1$Chlor_UGL[ix]=NA

range(buoy1416_L1$SpecCond, na.rm = T)
ix=which(buoy1416_L1$SpecCond==-6999)
buoy1416_L1$SpecCond[ix]=NA

buoy1416_chla <- subset(buoy1416_L1, select=c('datetime', chla))
buoy1416_vert_chla <- melt(buoy1416_chla, id='datetime')


####2015####
ggplot(subset(buoy1416_vert_chla, subset=(datetime>=as.POSIXct('2015-01-01', tz='GMT') & datetime<as.POSIXct('2016-01-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='2015, raw', x='date', y=' ') +
  final_theme

ggplot(subset(buoy1416_vert_wind, subset=(datetime>=as.POSIXct('2015-01-01', tz='GMT') & datetime<as.POSIXct('2015-01-16', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
geom_point() +   
  facet_grid(variable~., scales='free_y') +
  labs(title='early January 2015', x='date', y=' ') +
  final_theme

ggplot(subset(buoy1416_vert_wind, subset=(datetime>=as.POSIXct('2015-01-16', tz='GMT') & datetime<as.POSIXct('2015-02-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='late January 2015', x='date', y=' ') +
  final_theme

ggplot(subset(buoy1416_vert_wind, subset=(datetime>=as.POSIXct('2015-02-01', tz='GMT') & datetime<as.POSIXct('2015-02-14', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='early February 2015', x='date', y=' ') +
  final_theme

ggplot(subset(buoy1416_vert_wind, subset=(datetime>=as.POSIXct('2015-02-14', tz='GMT') & datetime<as.POSIXct('2015-03-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='late February 2015', x='date', y=' ') +
  final_theme

ggplot(subset(buoy1416_vert_wind, subset=(datetime>=as.POSIXct('2015-03-01', tz='GMT') & datetime<as.POSIXct('2015-03-16', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='early March 2015', x='date', y=' ') +
  final_theme

ggplot(subset(buoy1416_vert_wind, subset=(datetime>=as.POSIXct('2015-03-16', tz='GMT') & datetime<as.POSIXct('2015-04-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='late March 2015', x='date', y=' ') +
  final_theme

ggplot(subset(buoy1416_vert_wind, subset=(datetime>=as.POSIXct('2015-04-01', tz='GMT') & datetime<as.POSIXct('2015-04-16', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='early April 2015', x='date', y=' ') +
  final_theme

ggplot(subset(buoy1416_vert_wind, subset=(datetime>=as.POSIXct('2015-04-16', tz='GMT') & datetime<as.POSIXct('2015-05-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='late April 2015', x='date', y=' ') +
  final_theme


#data gap until June

ggplot(subset(buoy1416_vert_wind, subset=(datetime>=as.POSIXct('2015-06-01', tz='GMT') & datetime<as.POSIXct('2015-06-16', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='early June 2015', x='date', y=' ') +
  final_theme

ggplot(subset(buoy1416_vert_wind, subset=(datetime>=as.POSIXct('2015-06-16', tz='GMT') & datetime<as.POSIXct('2015-07-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='late June 2015', x='date', y=' ') +
  final_theme

ggplot(subset(buoy1416_vert_wind, subset=(datetime>=as.POSIXct('2015-07-01', tz='GMT') & datetime<as.POSIXct('2015-07-16', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='early July 2015', x='date', y=' ') +
  final_theme

ggplot(subset(buoy1416_vert_wind, subset=(datetime>=as.POSIXct('2015-07-16', tz='GMT') & datetime<as.POSIXct('2015-08-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='late July 2015', x='date', y=' ') +
  final_theme

ggplot(subset(buoy1416_vert_wind, subset=(datetime>=as.POSIXct('2015-08-01', tz='GMT') & datetime<as.POSIXct('2015-08-16', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='early August 2015', x='date', y=' ') +
  final_theme

ggplot(subset(buoy1416_vert_wind, subset=(datetime>=as.POSIXct('2015-08-16', tz='GMT') & datetime<as.POSIXct('2015-09-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='late August 2015', x='date', y=' ') +
  final_theme

ggplot(subset(buoy1416_vert_wind, subset=(datetime>=as.POSIXct('2015-09-01', tz='GMT') & datetime<as.POSIXct('2015-09-16', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='early September 2015', x='date', y=' ') +
  final_theme

ggplot(subset(buoy1416_vert_wind, subset=(datetime>=as.POSIXct('2015-09-16', tz='GMT') & datetime<as.POSIXct('2015-10-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='late September 2015', x='date', y=' ') +
  final_theme

ggplot(subset(buoy1416_vert_wind, subset=(datetime>=as.POSIXct('2015-10-01', tz='GMT') & datetime<as.POSIXct('2015-10-16', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='early October 2015', x='date', y=' ') +
  final_theme

ggplot(subset(buoy1416_vert_wind, subset=(datetime>=as.POSIXct('2015-10-16', tz='GMT') & datetime<as.POSIXct('2015-11-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='late October 2015', x='date', y=' ') +
  final_theme

ggplot(subset(buoy1416_vert_wind, subset=(datetime>=as.POSIXct('2015-11-01', tz='GMT') & datetime<as.POSIXct('2015-11-16', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='early November 2015', x='date', y=' ') +
  final_theme

ggplot(subset(buoy1416_vert_wind, subset=(datetime>=as.POSIXct('2015-11-16', tz='GMT') & datetime<as.POSIXct('2015-12-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='late Novebmer 2015', x='date', y=' ') +
  final_theme

ggplot(subset(buoy1416_vert_wind, subset=(datetime>=as.POSIXct('2015-12-01', tz='GMT') & datetime<as.POSIXct('2015-12-16', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='early December 2015', x='date', y=' ') +
  final_theme

ggplot(subset(buoy1416_vert_wind, subset=(datetime>=as.POSIXct('2015-12-16', tz='GMT') & datetime<as.POSIXct('2016-01-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='late December 2015', x='date', y=' ') +
  final_theme

ggplot(subset(buoy1416_vert_wind_L1, subset=(datetime>=as.POSIXct('2015-01-01', tz='GMT') & datetime<as.POSIXct('2016-01-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='2015, clean', x='date', y=' ') +
  final_theme


####2016####
ggplot(subset(buoy1416_vert_chla, subset=(datetime>=as.POSIXct('2016-01-01', tz='GMT') & datetime<as.POSIXct('2017-01-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='2016, raw', x='date', y=' ') +
  final_theme

ggplot(subset(buoy1416_vert_wind, subset=(datetime>=as.POSIXct('2016-01-01', tz='GMT') & datetime<as.POSIXct('2016-01-16', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() +   
  facet_grid(variable~., scales='free_y') +
  labs(title='early January 2016', x='date', y=' ') +
  final_theme

ggplot(subset(buoy1416_vert_wind, subset=(datetime>=as.POSIXct('2016-01-16', tz='GMT') & datetime<as.POSIXct('2016-02-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='late January 2016', x='date', y=' ') +
  final_theme

ggplot(subset(buoy1416_vert_wind, subset=(datetime>=as.POSIXct('2016-02-01', tz='GMT') & datetime<as.POSIXct('2016-02-14', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='early February 2016', x='date', y=' ') +
  final_theme

ggplot(subset(buoy1416_vert_wind, subset=(datetime>=as.POSIXct('2016-02-14', tz='GMT') & datetime<as.POSIXct('2016-03-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='late February 2016', x='date', y=' ') +
  final_theme

ggplot(subset(buoy1416_vert_wind, subset=(datetime>=as.POSIXct('2016-03-01', tz='GMT') & datetime<as.POSIXct('2016-03-16', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='early March 2016', x='date', y=' ') +
  final_theme

ggplot(subset(buoy1416_vert_wind, subset=(datetime>=as.POSIXct('2016-03-16', tz='GMT') & datetime<as.POSIXct('2016-04-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='late March 2016', x='date', y=' ') +
  final_theme

ggplot(subset(buoy1416_vert_wind, subset=(datetime>=as.POSIXct('2016-04-01', tz='GMT') & datetime<as.POSIXct('2016-04-16', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='early April 2016', x='date', y=' ') +
  final_theme

ggplot(subset(buoy1416_vert_wind, subset=(datetime>=as.POSIXct('2016-04-16', tz='GMT') & datetime<as.POSIXct('2016-05-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='late April 2016', x='date', y=' ') +
  final_theme

ggplot(subset(buoy1416_vert_wind, subset=(datetime>=as.POSIXct('2016-05-01', tz='GMT') & datetime<as.POSIXct('2016-05-16', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='early May 2016', x='date', y=' ') +
  final_theme

ggplot(subset(buoy1416_vert_wind, subset=(datetime>=as.POSIXct('2016-05-16', tz='GMT') & datetime<as.POSIXct('2016-06-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='late May 2016', x='date', y=' ') +
  final_theme

ggplot(subset(buoy1416_vert_wind, subset=(datetime>=as.POSIXct('2016-06-01', tz='GMT') & datetime<as.POSIXct('2016-06-16', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='early June 2016', x='date', y=' ') +
  final_theme

ggplot(subset(buoy1416_vert_wind, subset=(datetime>=as.POSIXct('2016-06-16', tz='GMT') & datetime<as.POSIXct('2016-07-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='late June 2016', x='date', y=' ') +
  final_theme

ggplot(subset(buoy1416_vert_wind, subset=(datetime>=as.POSIXct('2016-07-01', tz='GMT') & datetime<as.POSIXct('2016-07-16', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='early July 2016', x='date', y=' ') +
  final_theme

ggplot(subset(buoy1416_vert_wind, subset=(datetime>=as.POSIXct('2016-07-16', tz='GMT') & datetime<as.POSIXct('2016-08-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='late July 2016', x='date', y=' ') +
  final_theme

ggplot(subset(buoy1416_vert_wind, subset=(datetime>=as.POSIXct('2016-08-01', tz='GMT') & datetime<as.POSIXct('2016-08-16', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='early August 2016', x='date', y=' ') +
  final_theme

ggplot(subset(buoy1416_vert_wind, subset=(datetime>=as.POSIXct('2016-08-16', tz='GMT') & datetime<as.POSIXct('2016-09-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='late August 2016', x='date', y=' ') +
  final_theme

ggplot(subset(buoy1416_vert_wind, subset=(datetime>=as.POSIXct('2016-09-01', tz='GMT') & datetime<as.POSIXct('2016-09-16', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='early September 2016', x='date', y=' ') +
  final_theme

ggplot(subset(buoy1416_vert_wind, subset=(datetime>=as.POSIXct('2016-09-16', tz='GMT') & datetime<as.POSIXct('2016-10-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='late September 2016', x='date', y=' ') +
  final_theme

ggplot(subset(buoy1416_vert_wind, subset=(datetime>=as.POSIXct('2016-10-01', tz='GMT') & datetime<as.POSIXct('2016-10-16', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='early October 2016', x='date', y=' ') +
  final_theme

ggplot(subset(buoy1416_vert_wind, subset=(datetime>=as.POSIXct('2016-10-16', tz='GMT') & datetime<as.POSIXct('2016-11-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='late October 2016', x='date', y=' ') +
  final_theme

ggplot(subset(buoy1416_vert_wind, subset=(datetime>=as.POSIXct('2016-11-01', tz='GMT') & datetime<as.POSIXct('2016-11-16', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='early November 2016', x='date', y=' ') +
  final_theme

ggplot(subset(buoy1416_vert_wind, subset=(datetime>=as.POSIXct('2016-11-16', tz='GMT') & datetime<as.POSIXct('2016-12-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='late November 2016', x='date', y=' ') +
  final_theme

ggplot(subset(buoy1416_vert_wind, subset=(datetime>=as.POSIXct('2016-12-01', tz='GMT') & datetime<as.POSIXct('2016-12-16', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='early December 2016', x='date', y=' ') +
  final_theme

ggplot(subset(buoy1416_vert_wind, subset=(datetime>=as.POSIXct('2016-12-16', tz='GMT') & datetime<as.POSIXct('2017-01-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='late December 2016', x='date', y=' ') +
  final_theme



ggplot(subset(buoy1416_vert_wind_L1, subset=(datetime>=as.POSIXct('2016-01-01', tz='GMT') & datetime<as.POSIXct('2017-01-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales='free_y') +
  labs(title='2016, clean', x='date', y=' ') +
  final_theme



buoy1416_lowchla <- subset(buoy1416_L1, select=c('datetime', 'DOTempC', 'Chlor_RFU', 'TempC_0m'))
buoy1416_vert_lowchla <- melt(buoy1416_lowchla, id='datetime')

ggplot(subset(buoy1416_vert_lowchla, subset=(datetime>=as.POSIXct('2016-01-01', tz='GMT') & datetime<as.POSIXct('2017-01-01', tz='GMT'))), aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  labs(title='2016', x='date', y=' ') +
  final_theme

buoy16 <- subset(buoy1416_L1, subset=(datetime>=as.POSIXct('2016-01-01', tz='GMT') & datetime<as.POSIXct('2017-01-01', tz='GMT')))
buoy16 <- remove.vars(buoy16, names=c('ArrayID', 'Year', 'Day', 'Hr.Min', 'minutes', 'hour', 'time', 'date', 'location'))
buoy16_vert <- melt(buoy16, id='datetime')
buoy16_vert <- subset(buoy16_vert, subset=!(is.na(value)))

ggplot(buoy16_vert, aes(x=datetime, y=value, color=variable)) +
  geom_point() + 
  facet_grid(variable~., scales = 'free_y') +
  labs(title='2016', x='date', y=' ') +
  final_theme





