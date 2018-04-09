
#Set directory
dirMetab<-"C:/Users/Dave/Google Drive/SunapeeMetabolism2012/"

setwd(dirMetab)


#Read in the manual DO 
dataLSPA <- read.table("LSPA and MISC Files/Sunapee DO 2007 2012 modifications 15Jan2013 dcr.txt",header=T,,sep='\t',colClasses=c(dateTime_rounded="POSIXct"))

names(dataLSPA)

#keep only the rows where site=210
dataLSPA<-dataLSPA[dataLSPA$STATION==210,]

#keep only the shallow measurements
#name will have to be changed
#This one only includes measurements at 1
dataLSPA<-dataLSPA[dataLSPA$DEPTH==1,]

#This one includes 0.5 and 1
#dataLSPA<-dataLSPA[dataLSPA$Depth.is..1.5==1,]

#Keep only the needed columns of the input data set
keep.dataLSPA<-c("DO","Dosat_pct","Temp","dateTime_rounded")
dataLSPA<-dataLSPA[,keep.dataLSPA]

names(dataLSPA)[1]<-"DO.LSPA"
names(dataLSPA)[2]<-"DOSat_pct.LSPA"
names(dataLSPA)[3]<-"Temp.LSPA"

#Eliminate outliers!!!
#Remove any LSPA numbers that are greater than 3 sd above the mean for summer DO readings
dataLSPA<-dataLSPA[dataLSPA$DO.LSPA<(mean(dataLSPA$DO.LSPA) + 3*sd(dataLSPA$DO.LSPA)),]
#Remove any LSPA numbers that are greater than 3 sd above the mean for summer DO readings
dataLSPA<-dataLSPA[dataLSPA$DO.LSPA>(mean(dataLSPA$DO.LSPA) - 3*sd(dataLSPA$DO.LSPA)),]


year.vector<-c(2007,2008,2009,2010,2011,2012)
#debug
#year.index.L<-2

#Loop through all the years and read in DO
for(year.index.L in 1:length(year.vector)){
  #Directory of the data is going to be set based on the year
  dirData<-paste("C:/Users/Dave/Google Drive/SunapeeMetabolism2012/Sunapee",year.vector[year.index.L]," data/Proofed Data/",sep="")
  
  setwd(dirData)
  
  #Create the file name to be output using the year name
  outName<-paste("Sunapee_",year.vector[year.index.L],sep="")
  
  #Put the file names in these temp holders
  DOfile<-paste(outName,'_DO.txt',sep="")
  DOsatFile<-paste(outName,'_DOSaturation.txt',sep="")
  sensorTempFile<-paste(outName,'_sensorTemp.txt',sep="")
  
  if(year.vector[year.index.L]==2007){
    
    ##If it is the first entry, then create the merge data with DO data
    dataDO.L <- read.table(DOfile,header=T,sep='\t',colClasses=c(dateTime="POSIXct"))
    dataDO.Sat <- read.table(DOsatFile,header=T,sep='\t',colClasses=c(dateTime="POSIXct"))
    sensorTemp.C <- read.table(sensorTempFile,header=T,sep='\t',colClasses=c(dateTime="POSIXct"))
    
    
  }else{
    
    #################################################################################
    #Needs to read in correctly to merge... right now this is only reading in date and not time
    
    ##If it is the later than the first entry, then create the temp data with DO data
    temp.DO<-read.table(DOfile,header=T,sep='\t',colClasses=c(dateTime="character"))
    temp.DO$dateTime<-as.POSIXct(temp.DO$dateTime, format = "%Y-%m-%d %H:%M")
    
    temp.Sat<- read.table(DOsatFile,header=T,sep='\t',colClasses=c(dateTime="character"))
    temp.Sat$dateTime<-as.POSIXct(temp.Sat$dateTime, format = "%Y-%m-%d %H:%M")
    
    temp.sensorTemp.C<- read.table(sensorTempFile,header=T,sep='\t',colClasses=c(dateTime="character"))
    temp.sensorTemp.C$dateTime<-as.POSIXct(temp.sensorTemp.C$dateTime, format = "%Y-%m-%d %H:%M")
    
    
    ##merge with previous
    dataDO.L<-rbind(dataDO.L,temp.DO)
    
    ##merge with previous
    dataDO.Sat<-rbind(dataDO.Sat,temp.Sat)
    
    ##merge with previous
    sensorTemp.C<-rbind(sensorTemp.C,temp.sensorTemp.C)
    
    ##End of else for merging the results
  }
  
#End for loop  
}

##################
#Loop through all the years and read in DO that is adjusted
for(year.index.L in 1:length(year.vector)){
  #Directory of the data is going to be set based on the year
  dirData<-paste("C:/Users/Dave/Google Drive/SunapeeMetabolism2012/Sunapee",year.vector[year.index.L]," data/CombedDeep/",sep="")
  
  setwd(dirData)
  
  #Create the file name to be output using the year name
  outName<-paste("Sunapee_",year.vector[year.index.L],sep="")
  
  #Put the file names in these temp holders
  DOfile<-paste(outName,'_CombedDO.txt',sep="")
    
  if(year.vector[year.index.L]==2007){
    
    ##If it is the first entry, then create the merge data with DO data
    dataDO.adj <- read.table(DOfile,header=T,sep='\t',colClasses=c(dateTime="POSIXct"))
       
    
  }else{
    
    #################################################################################
    #Needs to read in correctly to merge... right now this is only reading in date and not time
    
    ##If it is the later than the first entry, then create the temp data with DO data
    temp.DO<-read.table(DOfile,header=T,sep='\t',colClasses=c(dateTime="character"))
    temp.DO$dateTime<-as.POSIXct(temp.DO$dateTime, format = "%Y-%m-%d %H:%M")
    
    ##merge with previous
    dataDO.adj<-rbind(dataDO.adj,temp.DO)
    
    ##End of else for merging the results
  }
  
  #End for loop  
}

#Create function that we will minimize
#This is calculating the sums of squares of residuals between the LSPA DO and the buoy DO +/- a quadratic offset with 3 parameters (a*time^2 + b*time + c) fit
min.RSS.quadratic <- function(data, par) {
  sum(((data$DO.LSPA-(data$DO+par[1]*(data$index)^2+par[2]*data$index+par[3]))^2),na.rm=TRUE)
}

#This is calculating the sums of squares of residuals between the LSPA DO and the buoy DO +/- a quadratic offset with 3 parameters (a*time^2 + b*time + c) fit
min.RSS.linear <- function(data, par) {
  sum(((data$DO.LSPA-(data$DO+par[1]*data$index+par[2]))^2),na.rm=TRUE)
}

min.RSS.constant<- function(data, par) {
  sum((data$DO.LSPA-(data$DO+par[1]))^2,na.rm=TRUE)
}


##DF.Sunapee.offset is a data frame that will contain 
#1. $start.Dates for each deployment
#2. $end.Dates for each deployment
#3. $SS Sums of squares for each deployment from 
#4. $Type (Constant, linear, quadratic) of best fit. Exponential is an option but not built in yet
#5. $PAR.a (quadratic A or NA for others)
#6. $PAR.b (linear term)
#7. $PAR.c (constant term)

start.Dates<-as.POSIXct(strptime(c("2007-08-26 00:00:00","2007-10-04 00:00:00","2009-01-03 00:00:00","2010-01-01 00:00:00","2010-03-24 00:00:00","2011-01-01 00:00:00","2012-01-01 00:00:00"),format="%Y-%m-%d %H:%M"))
end.Dates<-as.POSIXct(strptime(c("2007-10-04 00:00:00","2009-01-03 00:00:00","2010-01-01 00:00:00","2010-03-24 00:00:00","2011-01-01 00:00:00","2012-01-01 00:00:00","2013-01-01 00:00:00"),format="%Y-%m-%d %H:%M"))

#Merge the data frame of start dates and end dates, create NAs for all the others
DF.Sunapee.offset<-data.frame(start.Dates,end.Dates)
  DF.Sunapee.offset$SS<-NA
  DF.Sunapee.offset$Type<-NA
  DF.Sunapee.offset$PAR.a<-NA
  DF.Sunapee.offset$PAR.b<-NA
  DF.Sunapee.offset$PAR.c<-NA
  
##Merge the files together to create a subset 
Merged.DO<-merge(dataDO.L,dataLSPA, by.x="dateTime",by.y="dateTime_rounded",all=TRUE)

#Remove data points with NAs in the dateTime column
Merged.DO<-Merged.DO[!is.na(Merged.DO$dateTime),]

#Create the DO.corrected column
Merged.DO$DO.corrected<-NA

######################################
#Subset all the data only where there is LSPA data
Merged.DO.sub<-Merged.DO[!is.na(Merged.DO$DO.LSPA),]

#Remove all NA values
Merged.DO.sub<-Merged.DO.sub[!is.na(Merged.DO.sub$DO),]

#Subset the adjusted DO for the dates and times that LSPA had data
dataDO.adj.subset<-dataDO.adj[dataDO.adj$dateTime %in% Merged.DO.sub$dateTime,]
names(dataDO.adj.subset)[2]<-"DO.adj"

Merged.DO.sub<-merge(Merged.DO.sub, dataDO.adj.subset, by="dateTime",all=TRUE)

#Graph LSPA vs. original data and adjusted data
plot(Merged.DO.sub$DO~Merged.DO.sub$DO.LSPA)
#abline(lm(Merged.DO.sub$DO~Merged.DO.sub$DO.LSPA))
points(Merged.DO.sub$DO.adj~Merged.DO.sub$DO.LSPA,col="blue",pch=19)
#abline(lm(Merged.DO.sub$DO.adj~Merged.DO.sub$DO.LSPA),col="blue")
abline(0,1,col="red")
abline(lm(Merged.DO.sub$DO.adj~Merged.DO.sub$DO.LSPA))
abline(lm(Merged.DO.sub$DO~Merged.DO.sub$DO.LSPA))

summary(lm(Merged.DO.sub$DO.adj~Merged.DO.sub$DO.LSPA))
summary(lm(Merged.DO.sub$DO~Merged.DO.sub$DO.LSPA))

#correlation
cor(Merged.DO.sub$DO.adj,Merged.DO.sub$DO.LSPA)

#Remove the outlier from the dataset where LSPA DO is <7.5
temp.removeOutlier<-Merged.DO.sub[-8,]
summary(lm(temp.removeOutlier$DO.adj~temp.removeOutlier$DO.LSPA))
cor(temp.removeOutlier$DO.adj,temp.removeOutlier$DO.LSPA)
summary(lm(temp.removeOutlier$DO~temp.removeOutlier$DO.LSPA))
plot(temp.removeOutlier$DO~temp.removeOutlier$DO.LSPA)
#abline(lm(Merged.DO.sub$DO~Merged.DO.sub$DO.LSPA))
points(temp.removeOutlier$DO.adj~temp.removeOutlier$DO.LSPA,col="blue",pch=19)
#abline(lm(Merged.DO.sub$DO.adj~Merged.DO.sub$DO.LSPA),col="blue")
abline(0,1,col="red")
abline(lm(temp.removeOutlier$DO.adj~temp.removeOutlier$DO.LSPA))
abline(lm(temp.removeOutlier$DO~temp.removeOutlier$DO.LSPA))



####################################################
##Here should be the export file details with graphs
setwd(paste(dirMetab,'/LSPA and MISC Files/',sep=""))

#Set up pdf device
pdf(file=paste("DO offset LSPA and buoy data.pdf",sep=""),width=8.5,height=11)
par(mfcol=c(2,1));

#Debug
#index.offset<-2

for(index.offset in 1:length(DF.Sunapee.offset$start.Dates)){
    
  dat.subset=Merged.DO[Merged.DO$dateTime>=DF.Sunapee.offset$start.Dates[index.offset]&Merged.DO$dateTime<DF.Sunapee.offset$end.Dates[index.offset],]
  #There are some weird NAs at the end of the dataset
  
  #Assign an index using the time period
  dat.subset$index<-1:length(dat.subset$dateTime)
  
  #Cut the dataset down to only where there is LSPA data
  dat<-dat.subset[!is.na(dat.subset$DO.LSPA),]
  
  #IF ELSE to do the fitting
  if(length(dat$dateTime)<=7){
    #This loop is for points<2 to only generate a constant offset
    #Optim minimises a function by varying its parameters. The first argument of optim are the parameters I'd like to vary, par in this case; the second argument is the function to be minimised, min.RSS. The tricky bit is to understand how to apply optim to your data. The solution is the ... argument in optim, which allows me to pass other arguments through to min.RSS, here my data. 
    result <- optim(par = c(0), min.RSS.constant, data = dat,method="BFGS")
    DF.Sunapee.offset$SS[index.offset]<-result$value
    DF.Sunapee.offset$Type[index.offset]<-"Constant"
    DF.Sunapee.offset$PAR.c[index.offset]<-result$par[1]
    
    #Update corrected DO data for that time period
    Merged.DO[Merged.DO$dateTime>=DF.Sunapee.offset$start.Dates[index.offset]&Merged.DO$dateTime<DF.Sunapee.offset$end.Dates[index.offset],6]<-dat.subset$DO+DF.Sunapee.offset$PAR.c[index.offset]
    
  #end first if  
  }else if(length(dat$dateTime)<=7&length(dat$dateTime)>2){
    #This loop is for points 3 points to choose between linear and constant to only generate a constant offset
    #Optim minimises a function by varying its parameters. The first argument of optim are the parameters I'd like to vary, par in this case; the second argument is the function to be minimised, min.RSS. The tricky bit is to understand how to apply optim to your data. The solution is the ... argument in optim, which allows me to pass other arguments through to min.RSS, here my data. 
    result.constant <- optim(par = c(0), min.RSS.constant, data = dat,method="BFGS")
    result.linear <-optim(par = c(0,0), min.RSS.linear, data = dat,method="BFGS")
    
      #If else to choose the lowest SS
      if(min(result.constant$value,result.linear$value)==result.constant$value){
        #choose the constant
        DF.Sunapee.offset$SS[index.offset]<-result.constant$value
        DF.Sunapee.offset$Type[index.offset]<-"Constant"
        DF.Sunapee.offset$PAR.c[index.offset]<-result.constant$par[1]
        #Update corrected DO data for that time period
        Merged.DO[Merged.DO$dateTime>=DF.Sunapee.offset$start.Dates[index.offset]&Merged.DO$dateTime<DF.Sunapee.offset$end.Dates[index.offset],6]<-dat.subset$DO+DF.Sunapee.offset$PAR.c[index.offset]
        length(Merged.DO$dateTime)
      }else{
        DF.Sunapee.offset$SS[index.offset]<-result.linear$value
        DF.Sunapee.offset$Type[index.offset]<-"Linear"
        DF.Sunapee.offset$PAR.b[index.offset]<-result.linear$par[1]
        DF.Sunapee.offset$PAR.c[index.offset]<-result.linear$par[2]
        #Update corrected DO data for that time period
        Merged.DO[Merged.DO$dateTime>=DF.Sunapee.offset$start.Dates[index.offset]&Merged.DO$dateTime<DF.Sunapee.offset$end.Dates[index.offset],6]<-dat.subset$DO+DF.Sunapee.offset$PAR.b[index.offset]*dat.subset$index+DF.Sunapee.offset$PAR.c[index.offset]
        
      }
      
    
    
  #End second if  
  }else{
    
    #This loop is for points >4 points to choose between quadratic, linear, constant fits to only generate a constant offset
    #Optim minimises a function by varying its parameters. The first argument of optim are the parameters I'd like to vary, par in this case; the second argument is the function to be minimised, min.RSS. The tricky bit is to understand how to apply optim to your data. The solution is the ... argument in optim, which allows me to pass other arguments through to min.RSS, here my data. 
    result.constant <- optim(par = c(0), min.RSS.constant, data = dat,method="BFGS")
    result.linear <-optim(par = c(0,0), min.RSS.linear, data = dat,method="BFGS")
    result.quadratic <-optim(par = c(0,0,0), min.RSS.quadratic, data = dat,method="BFGS")
    
    
    #If else to choose the lowest SS
    if(min(result.constant$value,result.linear$value)==result.constant$value){
      #choose the constant
      DF.Sunapee.offset$SS[index.offset]<-result.constant$value
      DF.Sunapee.offset$Type[index.offset]<-"Constant"
      DF.Sunapee.offset$PAR.c[index.offset]<-result.constant$par[1]
      #Update corrected DO data for that time period
      Merged.DO[Merged.DO$dateTime>=DF.Sunapee.offset$start.Dates[index.offset]&Merged.DO$dateTime<DF.Sunapee.offset$end.Dates[index.offset],6]<-dat.subset$DO+DF.Sunapee.offset$PAR.c[index.offset]
      
    }else if(min(result.constant$value,result.linear$value)==result.constant$value){
      DF.Sunapee.offset$SS[index.offset]<-result.linear$value
      DF.Sunapee.offset$Type[index.offset]<-"Linear"
      DF.Sunapee.offset$PAR.b[index.offset]<-result.linear$par[1]
      DF.Sunapee.offset$PAR.c[index.offset]<-result.linear$par[2]
      #Update corrected DO data for that time period
      Merged.DO[Merged.DO$dateTime>=DF.Sunapee.offset$start.Dates[index.offset]&Merged.DO$dateTime<DF.Sunapee.offset$end.Dates[index.offset],6]<-dat.subset$DO+DF.Sunapee.offset$PAR.b[index.offset]*dat.subset$index+DF.Sunapee.offset$PAR.c[index.offset]
      
    }
    else{
      DF.Sunapee.offset$SS[index.offset]<-result.quadratic$value
      DF.Sunapee.offset$Type[index.offset]<-"Quadratic"
      DF.Sunapee.offset$PAR.a[index.offset]<-result.quadratic$par[1]
      DF.Sunapee.offset$PAR.b[index.offset]<-result.quadratic$par[2]
      DF.Sunapee.offset$PAR.c[index.offset]<-result.quadratic$par[3]
      Merged.DO[Merged.DO$dateTime>=DF.Sunapee.offset$start.Dates[index.offset]&Merged.DO$dateTime<DF.Sunapee.offset$end.Dates[index.offset],6]<-dat.subset$DO+DF.Sunapee.offset$PAR.a[index.offset]*(dat.subset$index)^2+DF.Sunapee.offset$PAR.b[index.offset]*dat.subset$index+DF.Sunapee.offset$PAR.c[index.offset]
      }
  #End of >3 points else
  }

  
  if(length(dat$dateTime)==0){
    ##Plot blank plot if there is no LSPA data for that time period
    plot(1, type="n", axes=F, xlab="", ylab="", main="NO LSPA DATA")
  }else{
    ############
    #Here plot the LSPA vs. Corrected data
    #Plot the corrected DO time series with the LSPA series
    plot(dat$DO~dat$dateTime,ylim=c(min(dat$DO.LSPA,dat$DO,na.rm=TRUE),max(dat$DO.LSPA,dat$DO,na.rm=TRUE)),cex=2,main="Black=Buoy Data, Red=LSPA data, Blue=Offset data")
    points(dat$DO.LSPA~dat$dateTime, col="red")
    
    #Cut the dataset down to only where there is LSPA data
    dat.temp<-Merged.DO[!is.na(Merged.DO$DO.LSPA),]
    lines(dat.temp[dat.temp$dateTime>=DF.Sunapee.offset$start.Dates[index.offset]&dat.temp$dateTime<DF.Sunapee.offset$end.Dates[index.offset],6]~dat.temp[dat.temp$dateTime>=DF.Sunapee.offset$start.Dates[index.offset]&dat.temp$dateTime<DF.Sunapee.offset$end.Dates[index.offset],1], col="blue")
    
    
  }
  
  #Calculate the subset of sensor Temp
  sensorTemp.subset=sensorTemp.C[sensorTemp.C$dateTime>=DF.Sunapee.offset$start.Dates[index.offset]&sensorTemp.C$dateTime<DF.Sunapee.offset$end.Dates[index.offset],]
  #Convert sensorTemp to Kelvin
  sensorTemp.subset$tempK <- sensorTemp.subset$sensorTemp + 273.15
  
  #Weiss equation
  A1 <- -173.4292;  A2 <- 249.6339;  A3 <- 143.3483;  A4 <- -21.8492
  sensorTemp.subset$DOSat <- exp(((A1 + (A2*100/sensorTemp.subset$tempK) + A3*log(sensorTemp.subset$tempK/100) + A4*(sensorTemp.subset$tempK/100))))
  
  ##
  #Calculate average atmospheric pressure at elevation of lake
  #Using the 'barometric formula' from Wikipedia - should double check
  #Values of Rstar, g0, M are according to US Standard Atmosphere 1976; use ISO or SI instead?
  
  #Constants
  Pb <- 101325        #static pressure, pascals
  Tb <- 288.15        #standard temp, K
  Lb <- -0.0065       #standard temp lapse rate, K m-1
  h <- 333            #elevation above sea level, m
  hb <- 0             #elevation at bottom of atmospheric layer 0, m (note layer 0 extends to 11000 masl)
  Rstar <-  8.31432   #universal gas constant, N m mol-1 K-1 (equiv to J K-1 mol-1)  SI: 8.314472
  g0 <- 9.80665       #acceleration of gravity, m s-1
  M <- 0.0289644      #molar mass of Earth's air, kg mol-1
  
  #Pressure, in Pa (pascals)
  P <- Pb * (Tb/(Tb+Lb*(h-hb)))^(g0*M/(Rstar*Lb))
  # In mmHg
  atmPres <- P*0.00750061683
  
  #Correction for local average atmospheric pressure
  u <- 10^(8.10765 - (1750.286/(235+sensorTemp.subset$tempK)))
  sensorTemp.subset$DOSat <- (sensorTemp.subset$DOSat*((atmPres-u)/(760-u)))   #ml/L
  sensorTemp.subset$DOSat <- sensorTemp.subset$DOSat/1000                      #L/L
  
  #Convert using standard temperature and pressure. 
  #Similar to calculating saturation DO at STP in ml/L, converting to mg?L (at STP),
  #and then doing the above temperature and pressure conversions.
  R <- 0.082057  #L atm deg-1 mol-1
  O2molWt <- 15.999*2
  convFactor <- O2molWt*(1/R)*(1/273.15)*(760/760) #g/L
  sensorTemp.subset$DOSat <- sensorTemp.subset$DOSat*convFactor*1000                   #mg/L
  
  
  #Plot old vs. new time series
  max.DO<-max(Merged.DO[Merged.DO$dateTime>=DF.Sunapee.offset$start.Dates[index.offset]&Merged.DO$dateTime<DF.Sunapee.offset$end.Dates[index.offset],2],Merged.DO[Merged.DO$dateTime>=DF.Sunapee.offset$start.Dates[index.offset]&Merged.DO$dateTime<DF.Sunapee.offset$end.Dates[index.offset],6],sensorTemp.subset$DOSat,na.rm=TRUE)
  min.DO<-min(Merged.DO[Merged.DO$dateTime>=DF.Sunapee.offset$start.Dates[index.offset]&Merged.DO$dateTime<DF.Sunapee.offset$end.Dates[index.offset],2],Merged.DO[Merged.DO$dateTime>=DF.Sunapee.offset$start.Dates[index.offset]&Merged.DO$dateTime<DF.Sunapee.offset$end.Dates[index.offset],6],sensorTemp.subset$DOSat,na.rm=TRUE)
  plot(Merged.DO[Merged.DO$dateTime>=DF.Sunapee.offset$start.Dates[index.offset]&Merged.DO$dateTime<DF.Sunapee.offset$end.Dates[index.offset],2]~Merged.DO[Merged.DO$dateTime>=DF.Sunapee.offset$start.Dates[index.offset]&Merged.DO$dateTime<DF.Sunapee.offset$end.Dates[index.offset],1],ylim=c(min.DO,max.DO),cex=1,main="Black=Buoy Data, Red=LSPA data, Blue=Offset data")
  lines(Merged.DO[Merged.DO$dateTime>=DF.Sunapee.offset$start.Dates[index.offset]&Merged.DO$dateTime<DF.Sunapee.offset$end.Dates[index.offset],6]~Merged.DO[Merged.DO$dateTime>=DF.Sunapee.offset$start.Dates[index.offset]&Merged.DO$dateTime<DF.Sunapee.offset$end.Dates[index.offset],1], col="blue")
  points(dat$DO.LSPA~dat$dateTime, col="red",cex=2)
  #Plot saturation value
  lines(sensorTemp.subset$DOSat~sensorTemp.subset$dateTime,col="green")
  
  
    
#end of for loop  
}


dev.off()


##Export one corrected DO per year to the 

#Set directory to the metabolism folder
setwd(dirMetab)

for(year.index in 1:length(year.vector)){
    
  #generate the folder for each year as 
  #/SunapeeYYYY data/CombedDeep/
  
  folder.file<-paste("Sunapee",year.vector[year.index]," data/CombedDeep/Sunapee_",year.vector[year.index],"_CombedDO.txt",sep="")
  
  #Cut the merged data (Merged.DO) down to the range for each year from 01JanYYYY 0:00 to 31DecYYYY 0:00
  temp1<-Merged.DO[Merged.DO$dateTime>=as.POSIXct(strptime(paste(year.vector[year.index],"-01-01 00:00:00",sep=""),format="%Y-%m-%d %H:%M"))&Merged.DO$dateTime<as.POSIXct(strptime(paste(year.vector[year.index]+1,"-01-01 00:00:00",sep=""),format="%Y-%m-%d %H:%M")),]
  
  #remove any with NA for DO.corrected but a value a value for LSPA data
  temp1<-temp1[!(is.na(temp1$DO.corrected)&!is.na(temp1$DO.LSPA)),]
  
  #keep dateTime and DO.corrected
  keep.temp1<-c("dateTime","DO.corrected")
  temp1<-temp1[keep.temp1]
  
  #rename DO.corrected as DO
  names(temp1)[2]<-"DO"
  
  #Export the file as Sunapee_2007_DO.txt tab delimited
  write.table(temp1,folder.file,row.names=FALSE, quote=FALSE,sep='\t')
  
}






