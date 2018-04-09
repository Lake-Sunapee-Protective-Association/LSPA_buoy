#/*****************************************************************/
#/*      Cary Institute of Ecosystem Studies (Millbrook, NY)      */
#/*                                                               */
#/* TITLE:         Sunapee_buoy_data_input.r                      */
#/* AUTHOR:        Amanda Lindsey                                 */
#/* SYSTEM:        Dell Studio, Windows 7, R  2.13.0              */
#/* PROJECT:       Lake Sunapee and Gloeo                         */
#/* PURPOSE:       collate raw buoy datalogger files for analysis */
#/*                in other programs                              */
#/* DATE CREATED:  30Aug2011                                      */
#/* LAST MODIFIED: 03Nov2011                                      */
#/* MODIFIED BY:   AL                                             */
#/*                                                               */
#/*****************************************************************/
#/*                     folder tree structure                     */
#/*                                                               */
#/*                         Lake Sunapee                          */
#/*                               |                               */
#/*                           monitoring                          */
#/*                               |                               */
#/*                           buoy data                           */
#/*                 _________/    |    \_________                 */
#/*                /              |              \                */
#/*  sas program files    raw_from_gleon_site    temp_SAS_output  */
#/*                               |                    |          */
#/*                          *.csv files            exported      */
#/*                                               *.xls files     */
#/*                                                               */
#/*****************************************************************/

# set working directory - change if running from a different location
setwd("//LINDSEYA1/Users/elliotta/Documents/atmdep/Lake Sunapee/monitoring/buoy data")
library(gdata)
# Note: must have perl installed on pc at C:\\perl\bin\perl.exe



#*************************************************
#***               input data                  ***
#*************************************************

# define vabiable names lists
raw_vars1=c("logger", "year", "day", "hrmin","DO_temp_C", "DO_sat", "DO_ppm", "temp0m_C", "temp0p5m_C", "temp1_C", "temp1p5m_C", "temp2_C", "temp2p5m_C", "temp3_C", "temp4_C", "temp5_C", "temp6_C", "temp7_C", "temp8_C", "temp9_C", "temp10_C", "temp11m_C", "temp13m_C", "temp15m_C", "air_temp_C", "RH", "PAR_umolesm2s", "wind_vector", "wind_spd_avg", "temp_anem_C")
# not sure if the wind speed and direction are the mean (as I have placed them here) or better placed with wind_spd and corrwind in the second set of variables

raw_vars2=c("logger", "year", "day", "hrmin","DO_temp_C", "DO_sat", "DO_ppm", "temp1_C", "temp2_C", "temp3_C", "temp4_C", "temp5_C", "temp6_C", "temp7_C", "temp8_C", "temp9_C", "temp10_C", "air_temp_C", "RH", "PAR_umolesm2s", "wind_spd", "corrwind", "wind_spd_avg", "wind_vector", "maxwind", "maxwnddir", "logger_batt", "radiobatt", "unknown", "heading")

# note from SAS program:
# NEED TO DEAL WITH THE CHANGE IN DEPTH DURING THE FALL OF 2007 - EASIEST WAY IS TO SPLIT THE RAW DATA FILES AND READ IN AS DIFFERENT DEPTHS AND JUST HAVE MORE VARIABLES


# AugOct07
AugOct07 <- read.csv("data/SAS_raw_data/Sunapee_buoy_data_27Aug07-02Oct07.csv", header=F, col.names=raw_vars1, na.strings=c("-6999", "."))

# OctNov07
OctNov07 <- read.table("data/SAS_raw_data/Sunapee_buoy_data02Oct07-21Nov07.dat", sep=",", col.names=raw_vars1, na.strings=c("-6999", "."))

# NovDec07
NovDec07 <- read.table("data/SAS_raw_data/Sunapee_buoy_data21Nov-13Dec07.dat", sep=",", col.names=raw_vars1, na.strings=c("-6999", "."))

# Dec07Jan08
Dec07Jan08 <- read.table("data/SAS_raw_data/Sunapee_buoy_data20Dec07-17Jan08.dat", sep=",", col.names=raw_vars1, na.strings=c("-6999", "."))

# Jan08
Jan08 <- read.table("data/SAS_raw_data/Sunapee_buoy_data17Jan-30Jan08.dat", sep=",", col.names=raw_vars1, na.strings=c("-6999", "."))

# Feb08
Feb08 <- read.table("data/SAS_raw_data/Sunapee_buoy_data03Feb-17Feb2008.dat", sep=",", col.names=raw_vars1, na.strings=c("-6999", "."))

# FebMar08
FebMar08 <- read.table("data/SAS_raw_data/Sunapee_buoy_data17Feb-28Mar08.dat", sep=",", col.names=raw_vars1, na.strings=c("-6999", "."))

# Apr08
Apr08 <- read.table("data/SAS_raw_data/Sunapee_buoy_data09Apr-23Apr08.dat", sep=",", col.names=raw_vars1, na.strings=c("-6999", "."))

# May08a
May08a <- read.table("data/SAS_raw_data/Sunapee_buoy_data06May-20May08.dat", sep=",", col.names=raw_vars1, na.strings=c("-6999", "."))

# May08b
May08b <- read.table("data/SAS_raw_data/Sunapee_buoy_data20May-28May08.dat", sep=",", col.names=raw_vars1, na.strings=c("-6999", "."))

# Dec10Nov11
Dec10Nov11 <- read.table("data/SAS_raw_data/Sunapee_LI_14Dec2010_16Nov2011.dat", sep=",", col.names=raw_vars2, na.strings=c("-6999", "."))


#*************************************************
#***         merge data and clean up           ***
#*************************************************

raw <- merge(AugOct07, OctNov07, all=T)
raw <- merge(raw, NovDec07, all=T)
raw <- merge(raw, Dec07Jan08, all=T)
raw <- merge(raw, Jan08, all=T)
raw <- merge(raw, Feb08, all=T)
raw <- merge(raw, FebMar08, all=T)
raw <- merge(raw, Apr08, all=T)
raw <- merge(raw, May08a, all=T)
raw <- merge(raw, May08b, all=T)
raw <- merge(raw, Dec10Nov11, all=T)

# create separate hour variable
raw$hour=as.integer(raw$hrmin / 100)

# create a date variable
raw$date=as.Date(paste(raw$day, raw$year, sep="-"), "%j-%Y")

# temperature data went wacky for a couple time periods, so remove any values greater than 100 degrees C
raw$temp1_C [raw$temp1_C>100] = NA
raw$temp2_C [raw$temp2_C>100] = NA
raw$temp3_C [raw$temp3_C>100] = NA
raw$temp4_C [raw$temp4_C>100] = NA
raw$temp5_C [raw$temp5_C>100] = NA
raw$temp6_C [raw$temp6_C>100] = NA
raw$temp7_C [raw$temp7_C>100] = NA
raw$temp8_C [raw$temp8_C>100] = NA
raw$temp9_C [raw$temp9_C>100] = NA
raw$temp10_C [raw$temp10_C>100] = NA

# some quick index plots
#plot(raw$wind_spd)
#str(raw)



#*************************************************
#*** summarize data for tropical storm Irene   ***
#*************************************************

# create a file of raw data to use for Irene and Lee effects project
# 01Aug2011 - 15Oct2011
raw_Irene_Lee <- subset(raw, subset = c(raw$year==2011 & raw$day>=213 & raw$day<=288))
# export
write.csv(raw_Irene_Lee, file="summary data files/Sunapee_buoy_raw_01Aug15Oct.csv", row.names=F, na="")

# pull data from during and around the storm
raw_Irene <- subset(raw, subset = c(raw$year==2011 & raw$day>=238 & raw$day<=241))

# calculate hourly means
library(doBy)
Irene_hourly <- summaryBy(DO_temp_C + DO_sat + DO_ppm + temp1_C + temp2_C + temp3_C + temp4_C + temp5_C + temp6_C + temp7_C + temp8_C + temp9_C + temp10_C + air_temp_C + RH + PAR_umolesm2s + wind_spd + corrwind + wind_spd_avg + wind_vector + maxwind + maxwnddir ~ year + day + hour, data = raw_Irene, na.rm=TRUE, FUN = mean)

plot(Irene_hourly$wind_spd_avg)
plot(raw_Irene$wind_spd_avg)
plot(raw_Irene$wind_vector)
plot(raw_Irene$maxwind)
plot(raw_Irene$maxwnddir)
plot(raw_Irene$DO_ppm)
plot(raw_Irene$temp1_C)
plot(raw_Irene$temp2_C)
plot(raw_Irene$temp3_C)
plot(raw_Irene$temp4_C)
plot(raw_Irene$temp5_C)
plot(raw_Irene$temp6_C)
plot(raw_Irene$temp7_C)
plot(raw_Irene$temp8_C)
plot(raw_Irene$temp9_C)
plot(raw_Irene$temp10_C)
plot(raw_Irene$air_temp_C)

# export raw data
write.csv(raw_Irene, file="summary data files/raw_Irene_2011.csv", row.names=F, na="")


# create wind rose
library(circular)
# dataset of wind direction and wind speed
Irenewind <- subset(raw_Irene, select = c(wind_vector, wind_spd_avg))
# create wind rose graph
windrose(Irenewind, bins=12, increment=4, main="Wind speed and direction for Aug 26-29")
## Not working quite right - needs more work

