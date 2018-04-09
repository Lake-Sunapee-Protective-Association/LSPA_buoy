#/*****************************************************************/
#/*      Cary Institute of Ecosystem Studies (Millbrook, NY)      */
#/*                                                               */
#/* TITLE:         buoy_move_detection.r                          */
#/* AUTHOR:        Amanda Lindsey                                 */
#/* SYSTEM:        Dell Studio, Windows 7, R  2.15.1              */
#/* PROJECT:       Lake Sunapee and Gloeo                         */
#/* PURPOSE:       check buoy data for 2009 for indication of     */
#/*                when it was moved to winter harbor location    */
#/* DATE CREATED:  14Mar2013                                      */
#/* LAST MODIFIED: 14Mar2013                                      */
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
#/*                 _________/         \                          */
#/*                /                    data                      */
#/*           programs                 /    \                     */
#/*              |                           currated 2009        */
#/*         program files                        from Cayelan     */
#/*                                                               */
#/*                                                               */
#/*****************************************************************/

# set working directory - change if running from a different location
setwd("//LINDSEYA1/Users/elliotta/Documents/atmdep/Lake Sunapee/monitoring/buoy data")
library(gdata)
# Note: must have perl installed on pc at C:\\perl\bin\perl.exe



# input temperature profile data
tempprof <- read.table("data/currated 2009 from Cayelan/Sunapee_2009_tempProfile.txt", header=F, sep="", col.names=c("date", "time", "temp0.0", "temp0.5", "temp1.0", "temp1.5", "temp2.0", "temp2.5", "temp3.0", "temp4.0", "temp5.0", "temp6.0", "temp7.0", "temp8.0", "temp9.0", "temp10.0", "temp11.0", "temp13.0"), row.names=NULL, stringsAsFactors=F, skip=1)
# format date and time variables
tempprof$date=as.Date(tempprof$date, "%Y-%m-%d")
tempprof$datetime=as.POSIXlt(paste(tempprof$date, tempprof$time), "%Y-%m-%d %H:%M", tz="EST")
str(tempprof)

# input dissolved oxygen data
do <- read.table("data/currated 2009 from Cayelan/Sunapee_2009_DO.txt", header=F, sep="", col.names=c("date", "time", "DO"), row.names=NULL, stringsAsFactors=F, skip=1)
# format date and time variables
do$date=as.Date(do$date, "%Y-%m-%d")
do$datetime=as.POSIXlt(paste(do$date, do$time), "%Y-%m-%d %H:%M", tz="EST")
str(do)

# input air temperature data
atemp <- read.table("data/currated 2009 from Cayelan/Sunapee_2009_airTemp.txt", header=F, sep="", col.names=c("date", "time", "airtemp_C"), row.names=NULL, stringsAsFactors=F, skip=1)
# format date and time variables
atemp$date=as.Date(atemp$date, "%Y-%m-%d")
atemp$datetime=as.POSIXlt(paste(atemp$date, atemp$time), "%Y-%m-%d %H:%M", tz="EST")
str(atemp)

# input wind direction data
wdir <- read.table("data/currated 2009 from Cayelan/Sunapee_2009_windDir.txt", header=F, sep="", col.names=c("date", "time", "wind_dir"), row.names=NULL, stringsAsFactors=F, skip=1)
# format date and time variables
wdir$date=as.Date(wdir$date, "%Y-%m-%d")
wdir$datetime=as.POSIXlt(paste(wdir$date, wdir$time), "%Y-%m-%d %H:%M", tz="EST")
str(wdir)

# input wind speed data
wspd <- read.table("data/currated 2009 from Cayelan/Sunapee_2009_windSpeed.txt", header=F, sep="", col.names=c("date", "time", "wind_speed"), row.names=NULL, stringsAsFactors=F, skip=1)
# format date and time variables
wspd$date=as.Date(wspd$date, "%Y-%m-%d")
wspd$datetime=as.POSIXlt(paste(wspd$date, wspd$time), "%Y-%m-%d %H:%M", tz="EST")
str(wspd)




# graph September data, select depths to look for incontinuities
tempsep <- subset(tempprof, subset = as.numeric(format(tempprof$datetime, "%m")) == 9)
jpeg(filename="data/currated 2009 from Cayelan/tempSep2009.jpg", width=3000, height=1600, res=300)
plot(tempsep$datetime, tempsep$temp2.0, type="n", col="blue", main="Sep temp profile", xlab="month/day", ylab="temperature (°C) or DO (mg/L)", ylim=c(0,22))
lines(tempsep$datetime, tempsep$temp2.0, col="blue")
lines(tempsep$datetime, tempsep$temp6.0, col="red")
lines(tempsep$datetime, tempsep$temp10.0, col="orange")
lines(tempsep$datetime, tempsep$temp13.0, col="darkgreen")
lines(do$datetime, do$DO, col="purple")
legend(x="bottomright", y="NULL", c("2.0m","6.0m","10.0m","13.0m", "DO"), pch=c(95,95,95,95,95), col=c("blue", "red", "orange", "darkgreen", "purple"))
dev.off()
# wind direction
jpeg(filename="data/currated 2009 from Cayelan/winddirSep2009.jpg", width=3000, height=1600, res=300)
plot(tempsep$datetime, tempsep$temp2.0, type="n", col="blue", main="Sep wind direction", xlab="month/day", ylab="wind direction (degrees)", ylim=c(0,360))
lines(wdir$datetime, wdir$wind_dir, col="blue")
dev.off()
# air temp and wind speed
jpeg(filename="data/currated 2009 from Cayelan/atempSep2009.jpg", width=3000, height=1600, res=300)
plot(tempsep$datetime, tempsep$temp2.0, type="n", col="blue", main="Sep air temp and wind speed", xlab="month/day", ylab="temperature (°C) or wind speed (m/sec)", ylim=c(0,25))
lines(atemp$datetime, atemp$airtemp_C, col="blue")
lines(wspd$datetime, wspd$wind_speed, col="red")
legend(x="topright", y="NULL", c("air temp","wind speed"), pch=c(95,95), col=c("blue", "red"))
dev.off()

# graph October data, select depths to look for incontinuities
tempoct <- subset(tempprof, subset = as.numeric(format(tempprof$datetime, "%m")) == 10)
jpeg(filename="data/currated 2009 from Cayelan/tempOct2009.jpg", width=3000, height=1600, res=300)
plot(tempoct$datetime, tempoct$temp2.0, type="n", col="blue", main="Oct temp profile", xlab="month/day", ylab="temperature (°C) or DO (mg/L)", ylim=c(0,22))
lines(tempoct$datetime, tempoct$temp2.0, col="blue")
lines(tempoct$datetime, tempoct$temp6.0, col="red")
lines(tempoct$datetime, tempoct$temp10.0, col="orange")
lines(tempoct$datetime, tempoct$temp13.0, col="darkgreen")
lines(do$datetime, do$DO, col="purple")
legend(x="bottomright", y="NULL", c("2.0m","6.0m","10.0m","13.0m", "DO"), pch=c(95,95,95,95,95,95), col=c("blue", "red", "orange", "darkgreen", "purple"))
dev.off()
# wind direction
jpeg(filename="data/currated 2009 from Cayelan/winddirOct2009.jpg", width=3000, height=1600, res=300)
plot(tempoct$datetime, tempoct$temp2.0, type="n", col="blue", main="Oct wind direction", xlab="month/day", ylab="wind direction (degrees)", ylim=c(0,360))
lines(wdir$datetime, wdir$wind_dir, col="blue")
dev.off()
# air temp and wind speed
jpeg(filename="data/currated 2009 from Cayelan/atempOct2009.jpg", width=3000, height=1600, res=300)
plot(tempoct$datetime, tempoct$temp2.0, type="n", col="blue", main="Oct air temp and wind speed", xlab="month/day", ylab="temperature (°C) or wind speed (m/sec)", ylim=c(-2,20))
lines(atemp$datetime, atemp$airtemp_C, col="blue")
lines(wspd$datetime, wspd$wind_speed, col="red")
legend(x="topright", y="NULL", c("air temp","wind speed"), pch=c(95,95), col=c("blue", "red"))
dev.off()



# graph November data, select depths to look for incontinuities
tempnov <- subset(tempprof, subset = as.numeric(format(tempprof$datetime, "%m")) == 11)
jpeg(filename="data/currated 2009 from Cayelan/tempNov2009.jpg", width=3000, height=1600, res=300)
plot(tempnov$datetime, tempnov$temp2.0, type="n", col="blue", main="Nov temp profile", xlab="month/day", ylab="temperature (°C) or DO (mg/L)", ylim=c(0,22))
lines(tempnov$datetime, tempnov$temp2.0, col="blue")
lines(tempnov$datetime, tempnov$temp6.0, col="red")
lines(tempnov$datetime, tempnov$temp10.0, col="orange")
lines(tempnov$datetime, tempnov$temp13.0, col="darkgreen")
lines(do$datetime, do$DO, col="purple")
legend(x="bottomleft", y="NULL", c("2.0m","6.0m","10.0m","13.0m", "DO"), pch=c(95,95,95,95,95,95), col=c("blue", "red", "orange", "darkgreen", "purple"))
dev.off()
# wind direction
jpeg(filename="data/currated 2009 from Cayelan/winddirNov2009.jpg", width=3000, height=1600, res=300)
plot(tempnov$datetime, tempnov$temp2.0, type="n", col="blue", main="Nov wind direction", xlab="month/day", ylab="wind direction (degrees)", ylim=c(0,360))
lines(wdir$datetime, wdir$wind_dir, col="blue")
dev.off()
# air temp and wind speed
jpeg(filename="data/currated 2009 from Cayelan/atempNov2009.jpg", width=3000, height=1600, res=300)
plot(tempnov$datetime, tempnov$temp2.0, type="n", col="blue", main="Nov air temp and wind speed", xlab="month/day", ylab="temperature (°C) or wind speed (m/sec)", ylim=c(-2,20))
lines(atemp$datetime, atemp$airtemp_C, col="blue")
lines(wspd$datetime, wspd$wind_speed, col="red")
legend(x="topright", y="NULL", c("air temp","wind speed"), pch=c(95,95), col=c("blue", "red"))
dev.off()


# graph December data, select depths to look for incontinuities
tempdec <- subset(tempprof, subset = as.numeric(format(tempprof$datetime, "%m")) == 12)
jpeg(filename="data/currated 2009 from Cayelan/tempDec2009.jpg", width=3000, height=1600, res=300)
plot(tempdec$datetime, tempdec$temp2.0, type="n", col="blue", main="Dec temp profile", xlab="month/day", ylab="temperature (°C) or DO (mg/L)", ylim=c(0,22))
lines(tempdec$datetime, tempdec$temp2.0, col="blue")
lines(tempdec$datetime, tempdec$temp6.0, col="red")
lines(tempdec$datetime, tempdec$temp10.0, col="orange")
lines(tempdec$datetime, tempdec$temp13.0, col="darkgreen")
lines(do$datetime, do$DO, col="purple")
legend(x="topleft", y="NULL", c("2.0m","6.0m","10.0m","13.0m", "DO"), pch=c(95,95,95,95,95,95), col=c("blue", "red", "orange", "darkgreen", "purple"))
dev.off()
# wind direction
jpeg(filename="data/currated 2009 from Cayelan/winddirDec2009.jpg", width=3000, height=1600, res=300)
plot(tempnov$datetime, tempnov$temp2.0, type="n", col="blue", main="Dec wind direction", xlab="month/day", ylab="wind direction (degrees)", ylim=c(0,360))
lines(wdir$datetime, wdir$wind_dir, col="blue")
dev.off()
# air temp and wind speed
jpeg(filename="data/currated 2009 from Cayelan/atempDec2009.jpg", width=3000, height=1600, res=300)
plot(tempnov$datetime, tempnov$temp2.0, type="n", col="blue", main="Dec air temp and wind speed", xlab="month/day", ylab="temperature (°C) or wind speed (m/sec)", ylim=c(-2,18))
lines(atemp$datetime, atemp$airtemp_C, col="blue")
lines(wspd$datetime, wspd$wind_speed, col="red")
legend(x="topright", y="NULL", c("air temp","wind speed"), pch=c(95,95), col=c("blue", "red"))
dev.off()



# graph August data, select depths to look for incontinuities
tempaug <- subset(tempprof, subset = as.numeric(format(tempprof$datetime, "%m")) == 8)
jpeg(filename="data/currated 2009 from Cayelan/tempAug2009.jpg", width=3000, height=1600, res=300)
plot(tempaug$datetime, tempaug$temp2.0, type="n", col="blue", main="Aug temp profile", xlab="month/day", ylab="temperature (°C) or DO (mg/L)", ylim=c(0,26))
lines(tempaug$datetime, tempaug$temp2.0, col="blue")
lines(tempaug$datetime, tempaug$temp6.0, col="red")
lines(tempaug$datetime, tempaug$temp10.0, col="orange")
lines(tempaug$datetime, tempaug$temp13.0, col="darkgreen")
lines(do$datetime, do$DO, col="purple")
legend(x="bottomright", y="NULL", c("2.0m","6.0m","10.0m","13.0m", "DO"), pch=c(95,95,95,95,95,95), col=c("blue", "red", "orange", "darkgreen", "purple"))
dev.off()






# graph all variables for the "suspect" time periods

# Sept 18-24
# temperature and DO
ix=which(as.numeric(format(tempsep$datetime, "%d")) >= 18 & as.numeric(format(tempsep$datetime, "%d")) <= 24)
tempsep18.24 <- tempsep[ix,]
jpeg(filename="data/currated 2009 from Cayelan/tempsep18-242009.jpg", width=3000, height=1600, res=300)
plot(tempsep18.24$datetime, tempsep18.24$temp2.0, type="n", col="blue", main="Sep 18-24 temp profile and DO", xlab="month/day", ylab="temperature (°C) or DO (mg/L)", ylim=c(8,19))
lines(tempsep18.24$datetime, tempsep18.24$temp2.0, col="blue")
lines(tempsep18.24$datetime, tempsep18.24$temp6.0, col="red")
lines(tempsep18.24$datetime, tempsep18.24$temp10.0, col="orange")
lines(tempsep18.24$datetime, tempsep18.24$temp13.0, col="darkgreen")
lines(do$datetime, do$DO, col="purple")
legend(x="bottomright", y="NULL", c("2.0m","6.0m","10.0m","13.0m", "DO"), pch=c(95,95,95,95,95,95), col=c("blue", "red", "orange", "darkgreen", "purple"))
dev.off()

# wind direction
jpeg(filename="data/currated 2009 from Cayelan/winddirsep18-242009.jpg", width=3000, height=1600, res=300)
plot(tempsep18.24$datetime, tempsep18.24$temp2.0, type="n", col="blue", main="Sep 18-24 wind direction", xlab="month/day", ylab="wind direction (degrees)", ylim=c(0,360))
lines(wdir$datetime, wdir$wind_dir, col="blue")
dev.off()

# air temp and wind speed
jpeg(filename="data/currated 2009 from Cayelan/atempsep18-242009.jpg", width=3000, height=1600, res=300)
plot(tempsep18.24$datetime, tempsep18.24$temp2.0, type="n", col="blue", main="Sep 18-24 air temp and wind speed", xlab="month/day", ylab="temperature (°C) or wind speed (m/sec)", ylim=c(0,25))
lines(atemp$datetime, atemp$airtemp_C, col="blue")
lines(wspd$datetime, wspd$wind_speed, col="red")
legend(x="topleft", y="NULL", c("air temp","wind speed"), pch=c(95,95), col=c("blue", "red"))
dev.off()


# Oct 1-3
# temperature and DO
ix=which(as.numeric(format(tempoct$datetime, "%d")) >= 1 & as.numeric(format(tempoct$datetime, "%d")) <= 3)
tempoct1.3 <- tempoct[ix,]
jpeg(filename="data/currated 2009 from Cayelan/tempOct1-32009.jpg", width=3000, height=1600, res=300)
plot(tempoct1.3$datetime, tempoct1.3$temp2.0, type="n", col="blue", main="Oct 1-3 temp profile and DO", xlab="month/day", ylab="temperature (°C) or DO (mg/L)", ylim=c(8,16))
lines(tempoct1.3$datetime, tempoct1.3$temp2.0, col="blue")
lines(tempoct1.3$datetime, tempoct1.3$temp6.0, col="red")
lines(tempoct1.3$datetime, tempoct1.3$temp10.0, col="orange")
lines(tempoct1.3$datetime, tempoct1.3$temp13.0, col="darkgreen")
lines(do$datetime, do$DO, col="purple")
legend(x="right", y="NULL", c("2.0m","6.0m","10.0m","13.0m", "DO"), pch=c(95,95,95,95,95,95), col=c("blue", "red", "orange", "darkgreen", "purple"))
dev.off()

# wind direction
jpeg(filename="data/currated 2009 from Cayelan/winddirOct1-32009.jpg", width=3000, height=1600, res=300)
plot(tempoct1.3$datetime, tempoct1.3$temp2.0, type="n", col="blue", main="Oct 1-3 wind direction", xlab="month/day", ylab="wind direction (degrees)", ylim=c(0,360))
lines(wdir$datetime, wdir$wind_dir, col="blue")
dev.off()

# air temp and wind speed
jpeg(filename="data/currated 2009 from Cayelan/atempOct1-32009.jpg", width=3000, height=1600, res=300)
plot(tempoct1.3$datetime, tempoct1.3$temp2.0, type="n", col="blue", main="Oct 1-3 air temp and wind speed", xlab="month/day", ylab="temperature (°C) or wind speed (m/sec)", ylim=c(0,15))
lines(atemp$datetime, atemp$airtemp_C, col="blue")
lines(wspd$datetime, wspd$wind_speed, col="red")
legend(x="topleft", y="NULL", c("air temp","wind speed"), pch=c(95,95), col=c("blue", "red"))
dev.off()



# Oct 20-23
# temperature and DO
ix=which(as.numeric(format(tempoct$datetime, "%d")) >= 18 & as.numeric(format(tempoct$datetime, "%d")) <= 24)
tempoct20.23 <- tempoct[ix,]
jpeg(filename="data/currated 2009 from Cayelan/tempOct18-242009.jpg", width=3000, height=1600, res=300)
plot(tempoct20.23$datetime, tempoct20.23$temp2.0, type="n", col="blue", main="Oct 18-24 temp profile and DO", xlab="month/day", ylab="temperature (°C) or DO (mg/L)", ylim=c(8,13))
lines(tempoct20.23$datetime, tempoct20.23$temp2.0, col="blue")
lines(tempoct20.23$datetime, tempoct20.23$temp6.0, col="red")
lines(tempoct20.23$datetime, tempoct20.23$temp10.0, col="orange")
lines(tempoct20.23$datetime, tempoct20.23$temp13.0, col="darkgreen")
lines(do$datetime, do$DO, col="purple")
legend(x="bottomright", y="NULL", c("2.0m","6.0m","10.0m","13.0m", "DO"), pch=c(95,95,95,95,95,95), col=c("blue", "red", "orange", "darkgreen", "purple"))
dev.off()

# wind direction
jpeg(filename="data/currated 2009 from Cayelan/winddirOct18-242009.jpg", width=3000, height=1600, res=300)
plot(tempoct20.23$datetime, tempoct20.23$temp2.0, type="n", col="blue", main="Oct 18-24 wind direction", xlab="month/day", ylab="wind direction (degrees)", ylim=c(0,360))
lines(wdir$datetime, wdir$wind_dir, col="blue")
dev.off()

# air temp and wind speed
jpeg(filename="data/currated 2009 from Cayelan/atempOct18-242009.jpg", width=3000, height=1600, res=300)
plot(tempoct20.23$datetime, tempoct20.23$temp2.0, type="n", col="blue", main="Oct 18-24 air temp and wind speed", xlab="month/day", ylab="temperature (°C) or wind speed (m/sec)", ylim=c(0,17))
lines(atemp$datetime, atemp$airtemp_C, col="blue")
lines(wspd$datetime, wspd$wind_speed, col="red")
legend(x="topleft", y="NULL", c("air temp","wind speed"), pch=c(95,95), col=c("blue", "red"))
dev.off()




# Nov 11-18
# temperature and DO
ix=which(as.numeric(format(tempnov$datetime, "%d")) >= 11 & as.numeric(format(tempnov$datetime, "%d")) <= 28)
tempnov11.18 <- tempnov[ix,]
jpeg(filename="data/currated 2009 from Cayelan/tempNov11-182009.jpg", width=3000, height=1600, res=300)
plot(tempnov11.18$datetime, tempnov11.18$temp2.0, type="n", col="blue", main="Nov 11-18 temp profile and DO", xlab="month/day", ylab="temperature (°C) or DO (mg/L)", ylim=c(6,14.5))
lines(tempnov11.18$datetime, tempnov11.18$temp2.0, col="blue")
lines(tempnov11.18$datetime, tempnov11.18$temp6.0, col="red")
lines(tempnov11.18$datetime, tempnov11.18$temp10.0, col="orange")
lines(tempnov11.18$datetime, tempnov11.18$temp13.0, col="darkgreen")
lines(do$datetime, do$DO, col="purple")
legend(x="bottomleft", y="NULL", c("2.0m","6.0m","10.0m"), pch=c(95,95,95), col=c("blue", "red", "orange"))
legend(x="bottom", y="NULL", c("13.0m", "DO"), pch=c(95,95), col=c("darkgreen", "purple"))
dev.off()

# wind direction
jpeg(filename="data/currated 2009 from Cayelan/winddirNov11-182009.jpg", width=3000, height=1600, res=300)
plot(tempnov11.18$datetime, tempnov11.18$temp2.0, type="n", col="blue", main="Nov 11-18 wind direction", xlab="month/day", ylab="wind direction (degrees)", ylim=c(0,360))
lines(wdir$datetime, wdir$wind_dir, col="blue")
dev.off()

# air temp and wind speed
jpeg(filename="data/currated 2009 from Cayelan/atempNov11-182009.jpg", width=3000, height=1600, res=300)
plot(tempnov11.18$datetime, tempnov11.18$temp2.0, type="n", col="blue", main="Nov 11-18 air temp and wind speed", xlab="month/day", ylab="temperature (°C) or wind speed (m/sec)", ylim=c(-2,15))
lines(atemp$datetime, atemp$airtemp_C, col="blue")
lines(wspd$datetime, wspd$wind_speed, col="red")
legend(x="topleft", y="NULL", c("air temp","wind speed"), pch=c(95,95), col=c("blue", "red"))
dev.off()


# graph Sep 30-Oct 4, select depths to look for incontinuities
ix=which(format(tempprof$datetime, "%m-%d") == "09-30" | format(tempprof$datetime, "%m-%d") == "10-01" | format(tempprof$datetime, "%m-%d") == "10-02" | format(tempprof$datetime, "%m-%d") == "10-03" | format(tempprof$datetime, "%m-%d") == "10-04")
tempsep30.oct4 <- tempprof[ix,]
jpeg(filename="data/currated 2009 from Cayelan/tempSep30-Oct42009.jpg", width=3000, height=1600, res=300)
plot(tempsep30.oct4$datetime, tempsep30.oct4$temp2.0, type="n", col="blue", main="30Sep-04Oct temp profile", xlab="month/day", ylab="temperature (°C) or DO (mg/L)", ylim=c(8,18))
lines(tempsep30.oct4$datetime, tempsep30.oct4$temp2.0, col="blue")
lines(tempsep30.oct4$datetime, tempsep30.oct4$temp6.0, col="red")
lines(tempsep30.oct4$datetime, tempsep30.oct4$temp10.0, col="orange")
lines(tempsep30.oct4$datetime, tempsep30.oct4$temp13.0, col="darkgreen")
lines(do$datetime, do$DO, col="purple")
legend(x="bottomright", y="NULL", c("2.0m","6.0m","10.0m","13.0m", "DO"), pch=c(95,95,95,95,95,95), col=c("blue", "red", "orange", "darkgreen", "purple"))
dev.off()





