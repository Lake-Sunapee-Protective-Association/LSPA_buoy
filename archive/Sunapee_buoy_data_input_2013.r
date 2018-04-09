#*****************************************************************
#*      Cary Institute of Ecosystem Studies (Millbrook, NY)      *
#*                                                               *
#* TITLE:         Sunapee_buoy_data_input_2013.r                 *
#* AUTHOR:        Amanda Lindsey                                 *
#* SYSTEM:        Dell Studio, Windows 7, R  3.0.2               *
#* PROJECT:       Lake Sunapee buoy                              *
#* PURPOSE:       collate raw buoy datalogger files and export   *
#*                2013 data for CUHASI database                  *
#* DATE CREATED:  05May2014                                      *
#* LAST MODIFIED: 15May2014                                      *
#* MODIFIED BY:   AL                                             *
#*                                                               *
#*****************************************************************
#*                     folder tree structure                     *
#*                                                               *
#*                         Lake Sunapee                          *
#*                               |                               *
#*                           monitoring                          *
#*                               |                               *
#*                           buoy data                           *
#*                 _________/         \_________                 *
#*                /                             \                *
#*      program files                          data              *
#*                                               |               *
#*                                          files from LSPA      *
#*                                                               *
#*                                                               *
#*****************************************************************

# set working directory - change if running from a different location
setwd("//LINDSEYA1/Users/elliotta/Dropbox/Lake Sunapee/monitoring/buoy data")
library(gdata)
# Note: must have perl installed on pc at C:\\perl\bin\perl.exe




#********************************************
# input data --------------

vars1 <- c("logger", "year", "day", "hrmin", "DO1_temp_C", "DO1_sat", "DO1_ppm", "temp1_C", "temp2_C", "temp3_C", "temp4_C", "temp5_C", "temp6_C", "temp7_C", "temp8_C", "temp9_C", "temp10_C", "air_temp_C", "RH_pct", "PAR_umolsm2", "wind_spd_inst_mps", "wind_dir_inst_deg", "wind_spd_avg_mps", "wind_vector_avg_deg", "wind_spd_max_mps", "wind_dir_max_deg", "loggerbatt_v", "radiobatt_v", "logger_temp_C", "heading_deg")
vars2 <- c("logger", "year", "day", "hrmin", "DO1_temp_C", "DO1_sat", "DO1_ppm", "temp1_C", "temp2_C", "temp3_C", "temp4_C", "temp5_C", "temp6_C", "temp7_C", "temp8_C", "temp9_C", "temp10_C", "temp11_C", "air_temp_C", "RH_pct", "PAR_umolsm2", "wind_spd_inst_mps", "wind_dir_inst_deg", "wind_spd_avg_mps", "wind_vector_avg_deg", "wind_spd_max_mps", "wind_dir_max_deg", "loggerbatt_v", "radiobatt_v", "logger_temp_C", "heading_deg", "DO10_temp_C", "DO10_sat", "DO10_ppm")

raw1 <- read.table("data/files from LSPA/Buoy Data.txt", header=F, skip=1, col.names=vars1, sep="", row.names=NULL, stringsAsFactors=F, na.strings=c(-6999))
raw2 <- read.table("data/files from LSPA/buoydata_pt2.txt", header=F, skip=1, col.names=vars2, sep="", row.names=NULL, stringsAsFactors=F, na.strings=c(-6999, 6999))
raw3 <- read.table("data/files from LSPA/buoydata_pt3.txt", header=F, skip=1, col.names=vars2, sep=",", row.names=NULL, stringsAsFactors=F, na.strings=c(-6999, 6999))


#********************************************
# merge and clean up --------------
raw_buoy <- merge(raw1, raw2, all=T)
raw_buoy <- merge(raw_buoy, raw3, all=T)

# create date and datetime variables
raw_buoy$date=as.Date(paste(raw_buoy$day, raw_buoy$year, sep="-"), "%j-%Y")
raw_buoy$hour=as.integer(raw_buoy$hrmin / 100)
raw_buoy$min=raw_buoy$hrmin - (raw_buoy$hour * 100)
raw_buoy$time=paste(raw_buoy$hour, raw_buoy$min, sep=":")
raw_buoy$datetime=as.POSIXlt(paste(raw_buoy$date, raw_buoy$time), "%Y-%m-%d %H:%M", tz="EST")

# set temperature values of 555.4 to missing
raw_buoy$temp1_C [raw_buoy$temp1_C==555.4] = NA
raw_buoy$temp2_C [raw_buoy$temp2_C==555.4] = NA
raw_buoy$temp3_C [raw_buoy$temp3_C==555.4] = NA
raw_buoy$temp4_C [raw_buoy$temp4_C==555.4] = NA
raw_buoy$temp5_C [raw_buoy$temp5_C==555.4] = NA
raw_buoy$temp6_C [raw_buoy$temp6_C==555.4] = NA
raw_buoy$temp7_C [raw_buoy$temp7_C==555.4] = NA
raw_buoy$temp8_C [raw_buoy$temp8_C==555.4] = NA
raw_buoy$temp9_C [raw_buoy$temp9_C==555.4] = NA
raw_buoy$temp10_C [raw_buoy$temp10_C==555.4] = NA
raw_buoy$temp11_C [raw_buoy$temp11_C==555.4] = NA
raw_buoy$air_temp_C [raw_buoy$air_temp_C==555.4] = NA

# for Corinna, use only 2013 data
raw_buoy13 <- subset(raw_buoy, subset=raw_buoy$year==2013)

# rearrange variables for nicer display in 2013 dataset
raw_buoy13 <- subset(raw_buoy13, select=c(logger, year, day, hrmin, DO1_temp_C, DO1_sat, DO1_ppm, DO10_temp_C, DO10_sat, DO10_ppm, temp1_C, temp2_C, temp3_C, temp4_C, temp5_C, temp6_C, temp7_C, temp8_C, temp9_C, temp10_C, temp11_C, air_temp_C, RH_pct, PAR_umolsm2, wind_spd_inst_mps, wind_dir_inst_deg, wind_spd_avg_mps, wind_vector_avg_deg, wind_spd_max_mps, wind_dir_max_deg, loggerbatt_v, radiobatt_v, logger_temp_C, heading_deg))

# export dataset (na=-6999)
write.csv(raw_buoy13, file="data/summary data files/Sunapee_buoy_raw_2013.csv", row.names=F, na="-6999")





#********************************************
# plots for exploring data --------------

pdf(file="data/summary data files/plots/buoy_temp_do_2011_13.pdf")

# temperature data
plot(raw_buoy$datetime, raw_buoy$temp1_C, type="l", col="blue", main="water temp 1m", xlab="date", ylab="temperature (deg C)", ylim=c(0,32))
plot(raw_buoy$datetime, raw_buoy$temp2_C, type="l", col="blue", main="water temp 2m", xlab="date", ylab="temperature (deg C)", ylim=c(0,32))
plot(raw_buoy$datetime, raw_buoy$temp3_C, type="l", col="blue", main="water temp 3m", xlab="date", ylab="temperature (deg C)", ylim=c(0,32))
plot(raw_buoy$datetime, raw_buoy$temp4_C, type="l", col="blue", main="water temp 4m", xlab="date", ylab="temperature (deg C)", ylim=c(0,32))
plot(raw_buoy$datetime, raw_buoy$temp5_C, type="l", col="blue", main="water temp 5m", xlab="date", ylab="temperature (deg C)", ylim=c(0,32))
plot(raw_buoy$datetime, raw_buoy$temp6_C, type="l", col="blue", main="water temp 6m", xlab="date", ylab="temperature (deg C)", ylim=c(0,32))
plot(raw_buoy$datetime, raw_buoy$temp7_C, type="l", col="blue", main="water temp 7m", xlab="date", ylab="temperature (deg C)", ylim=c(0,32))
plot(raw_buoy$datetime, raw_buoy$temp8_C, type="l", col="blue", main="water temp 8m", xlab="date", ylab="temperature (deg C)", ylim=c(0,32))
plot(raw_buoy$datetime, raw_buoy$temp9_C, type="l", col="blue", main="water temp 9m", xlab="date", ylab="temperature (deg C)", ylim=c(0,32))
plot(raw_buoy$datetime, raw_buoy$temp10_C, type="l", col="blue", main="water temp 10m", xlab="date", ylab="temperature (deg C)", ylim=c(0,32))
plot(raw_buoy$datetime, raw_buoy$temp11_C, type="l", col="blue", main="some unknown temp", xlab="date", ylab="temperature (deg C)", ylim=c(0,32))
plot(raw_buoy$datetime, raw_buoy$air_temp_C, type="l", col="blue", main="air temp on buoy", xlab="date", ylab="temperature (deg C)")

# DO probe data
plot(raw_buoy$datetime, raw_buoy$DO1_temp_C, type="l", col="blue", main="DO temp 1m", xlab="date", ylab="temperature (deg C)", ylim=c(0,32))
plot(raw_buoy$datetime, raw_buoy$DO10_temp_C, type="l", col="blue", main="DO temp 10m", xlab="date", ylab="temperature (deg C)", ylim=c(0,32))
plot(raw_buoy$datetime, raw_buoy$DO1_sat, type="l", col="blue", main="DO saturation 1m", xlab="date", ylab="DO sat (percent)")
plot(raw_buoy$datetime, raw_buoy$DO10_sat, type="l", col="blue", main="DO saturation 10m", xlab="date", ylab="DO sat (percent)")
plot(raw_buoy$datetime, raw_buoy$DO1_ppm, type="l", col="blue", main="DO concentration 1m", xlab="date", ylab="DO (ppm)", ylim=c(0,18))
plot(raw_buoy$datetime, raw_buoy$DO10_ppm, type="l", col="blue", main="DO concentration 10m", xlab="date", ylab="DO (ppm)", ylim=c(0,18))
dev.off()

pdf(file="data/summary data files/plots/buoy_met_other_2011_13.pdf")

# met data
plot(raw_buoy$datetime, raw_buoy$RH_pct, type="l", col="blue", main="relative humidity", xlab="date", ylab="RH (precent)")
plot(raw_buoy$datetime, raw_buoy$PAR_umolsm2, type="p", pch=46, col="blue", main="PAR", xlab="date", ylab="PAR (umol/s/m2)")
plot(raw_buoy$datetime, raw_buoy$wind_spd_avg_mps, type="l", col="blue", main="average wind speed", xlab="date", ylab="wind speed (m/sec)", ylim=c(0,50))
plot(raw_buoy$datetime, raw_buoy$wind_vector_avg_deg, type="p", pch=46, col="blue", main="wind direction (average vector)", xlab="date", ylab="wind direction (degrees)")
plot(raw_buoy$datetime, raw_buoy$wind_spd_max_mps, type="l", col="blue", main="max wind speed", xlab="date", ylab="wind speed (m/sec)", ylim=c(0,50))
plot(raw_buoy$datetime, raw_buoy$wind_dir_max_deg, type="p", pch=46, col="blue", main="direction of max wind speed", xlab="date", ylab="wind direction (degrees)")
plot(raw_buoy$datetime, raw_buoy$wind_spd_inst_mps, type="l", col="blue", main="WndSp", xlab="date", ylab="instantaneous wind speed (m/sec)", ylim=c(0,50))
plot(raw_buoy$datetime, raw_buoy$wind_dir_inst_deg, type="p", pch=46, col="blue", main="instantaneous wind direction", xlab="date", ylab="wind direction (degrees)")

# other
plot(raw_buoy$datetime, raw_buoy$loggerbatt_v, type="l", col="blue", main="logger battery level", xlab="date", ylab="battery level (volts)")
plot(raw_buoy$datetime, raw_buoy$radiobatt_v, type="l", col="blue", main="radio battery level", xlab="date", ylab="battery level (volts)")
plot(raw_buoy$datetime, raw_buoy$logger_temp_C, type="l", col="blue", main="logger box temperature", xlab="date", ylab="temperature (degrees C)")
plot(raw_buoy$datetime, raw_buoy$heading_deg, type="p", pch=46, col="blue", main="buoy heading", xlab="date", ylab="heading (degrees)")
dev.off()




#********************************************
# plots of DO for 2013 only --------------

# subset and clean up data a bit
ix=which(as.numeric(format(raw_buoy$datetime, "%Y"))==2013 & as.numeric(format(raw_buoy$datetime, "%m"))>=6 & as.numeric(format(raw_buoy$datetime, "%m"))<=10)
raw_summer2013 <- raw_buoy[ix, ]
# set data to missing that are 0
raw_summer2013$DO10_ppm [raw_summer2013$DO10_ppm<=0] = NA
raw_summer2013$DO10_temp_C [raw_summer2013$DO10_temp_C<=0] = NA
raw_summer2013$DO10_sat [raw_summer2013$DO10_sat<=0] = NA

#find step
ix=which(as.numeric(format(raw_summer2013$datetime, "%m"))==9 & as.numeric(format(raw_summer2013$datetime, "%d"))==25)
test <- raw_summer2013[ix,]

pdf(file="data/summary data files/plots/buoy_do_2013.pdf")

# DO concentration
plot(raw_summer2013$datetime, raw_summer2013$DO1_ppm, type="l", col="blue", main="DO concentration", xlab="2013", ylab="DO (ppm)", ylim=c(7,15))
lines(raw_summer2013$datetime, raw_summer2013$DO10_ppm, col="red")
legend(x="topleft", y="NULL", c("1 m depth","10 m depth"), pch=c(95,95), col=c("blue","red"))
text(x=(as.POSIXlt("2013-05-25", "%Y-%m-%d")), y=13, "step happened during gap in data", pos=4)
text(x=(as.POSIXlt("2013-05-25", "%Y-%m-%d")), y=12.4, "on 2013-09-25 from 14:10 to 15:00", pos=4)
# regression
plot(raw_summer2013$DO1_ppm, raw_summer2013$DO10_ppm, main="DO concentration", xlab="1m DO conc (ppm)", ylab="10m DO conc (ppm)", xlim=c(7, 15), ylim=c(7, 15))
abline(lm(raw_summer2013$DO10_ppm~raw_summer2013$DO1_ppm))

# temp on DO probe
plot(raw_summer2013$datetime, raw_summer2013$DO1_temp_C, type="l", col="blue", main="temperature on DO probe", xlab="2013", ylab="temperature (deg C)", ylim=c(10,28))
lines(raw_summer2013$datetime, raw_summer2013$DO10_temp_C, col="red")
legend(x="topright", y="NULL", c("1 m depth","10 m depth"), pch=c(95,95), col=c("blue","red"))
text(x=(as.POSIXlt("2013-07-01", "%Y-%m-%d")), y=12, "no step on 2013-09-25", pos=4)
# regression
plot(raw_summer2013$DO1_temp_C, raw_summer2013$DO10_temp_C, main="temperature on DO probe", xlab="1m temp (deg C)", ylab="10m temp (dec C)", xlim=c(10,28), ylim=c(10,28))
abline(lm(raw_summer2013$DO10_temp_C~raw_summer2013$DO1_temp_C))

# DO saturation
plot(raw_summer2013$datetime, raw_summer2013$DO1_sat, type="l", col="blue", main="DO saturation", xlab="2013", ylab="DO sat (percent)", ylim=c(70,150))
lines(raw_summer2013$datetime, raw_summer2013$DO10_sat, col="red")
legend(x="topleft", y="NULL", c("1 m depth","10 m depth"), pch=c(95,95), col=c("blue","red"))
# regression
plot(raw_summer2013$DO1_sat, raw_summer2013$DO10_sat, main="DO saturation", xlab="1m sat (percent)", ylab="10m sat (percent)", xlim=c(70,150), ylim=c(70,150))
abline(lm(raw_summer2013$DO10_sat~raw_summer2013$DO1_sat))

dev.off()



# plot(raw1$DO1_temp_C)
# plot(raw1$DO1_sat)
# plot(raw1$DO1_ppm)
# plot(raw1$temp1_C)
# plot(raw1$temp2_C)
# plot(raw1$temp3_C)
# plot(raw1$temp4_C)	
# plot(raw1$temp5_C)
# plot(raw1$temp6_C)
# plot(raw1$temp7_C)
# plot(raw1$temp8_C)
# plot(raw1$temp9_C)
# plot(raw1$temp10_C)
# plot(raw1$air_temp_C)
# plot(raw1$RH_pct)
# plot(raw1$PAR_umolsm2)
# plot(raw1$WndSp)
# plot(raw1$CorrWind)
# plot(raw1$wind_spd_avg_mps)
# plot(raw1$wind_dir_deg)
# plot(raw1$wind_spd_max_mps)
# plot(raw1$wind_dir_max_deg)
# plot(raw1$loggerbatt_v)
# plot(raw1$radiobatt_v)
# plot(raw1$logger_temp_C)
# plot(raw1$heading_deg)
# 
# plot(raw2$DO1_temp_C)
# plot(raw2$DO1_sat)
# plot(raw2$DO1_ppm)
# plot(raw2$temp1_C)
# plot(raw2$temp2_C)
# plot(raw2$temp3_C)
# plot(raw2$temp4_C)  
# plot(raw2$temp5_C)
# plot(raw2$temp6_C)
# plot(raw2$temp7_C)
# plot(raw2$temp8_C)
# plot(raw2$temp9_C)
# plot(raw2$temp10_C)
# plot(raw2$unknown_temp)
# plot(raw2$air_temp_C)
# plot(raw2$RH_pct)
# plot(raw2$PAR_umolsm2)
# plot(raw2$WndSp)
# plot(raw2$CorrWind)
# plot(raw2$wind_spd_avg_mps)
# plot(raw2$wind_dir_deg)
# plot(raw2$wind_spd_max_mps)
# plot(raw2$wind_dir_max_deg)
# plot(raw2$loggerbatt_v)
# plot(raw2$radiobatt_v)
# plot(raw2$logger_temp_C)
# plot(raw2$heading_deg)
# plot(raw2$DO10_temp_C)
# plot(raw2$DO10_sat)
# plot(raw2$DO10_ppm)
# 
# plot(raw3$DO1_temp_C)
# plot(raw3$DO1_sat)
# plot(raw3$DO1_ppm)
# plot(raw3$temp1_C)
# plot(raw3$temp2_C)
# plot(raw3$temp3_C)
# plot(raw3$temp4_C)  
# plot(raw3$temp5_C)
# plot(raw3$temp6_C)
# plot(raw3$temp7_C)
# plot(raw3$temp8_C)
# plot(raw3$temp9_C)
# plot(raw3$temp10_C)
# plot(raw3$unknown_temp)
# plot(raw3$air_temp_C)
# plot(raw3$RH_pct)
# plot(raw3$PAR_umolsm2)
# plot(raw3$WndSp)
# plot(raw3$CorrWind)
# plot(raw3$wind_spd_avg_mps)
# plot(raw3$wind_dir_deg)
# plot(raw3$wind_spd_max_mps)
# plot(raw3$wind_dir_max_deg)
# plot(raw3$loggerbatt_v)
# plot(raw3$radiobatt_v)
# plot(raw3$logger_temp_C)
# plot(raw3$heading_deg)
# plot(raw3$DO10_temp_C)
# plot(raw3$DO10_sat)
# plot(raw3$DO10_ppm)
# 
# 
# 
# levels(as.factor(raw1$temp1_C))

str(raw_buoy)

