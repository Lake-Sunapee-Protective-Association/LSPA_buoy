/*
load_buoy_14-16.sas
load the raw buoy data from Geoff Lizotte WITHOUT cleaning first
KLC, created 31 July 2017

doing so that I can calculate rough stability metrics for ESA talk next week
will need revisiting!

modified 1 August 2017: can't have multiple years in a file
    confuses the program
*/


/* *****************************
SET UP SAS
***************************** */

/*clear log and output so only this run appears*/
dm 'Clear log' ;
dm 'Clear Out' ;

/* where the data are*/
LIBNAME rawdata 'GloeoGroup/Data/CrossYearComparisons/BuoyData/raw_data_from_Geoff';

/* where the processed files should go */
LIBNAME doutput 'GloeoGroup/Data/CrossYearComparisons/BuoyData/SAS_output';


OPTIONS pagesize=58 linesize=78;


/* *****************************
LOAD EACH YEAR'S DATA
note that hour/minute is very strange: first digit(s) are the hour, then come the minutes
***************************** */


DATA d2014;
     INFILE 'GloeoGroup/Data/CrossYearComparisons/BuoyData/raw_data_from_Geoff/2014 Buoy Data.csv'
        dlm=',' firstobs=2;
     INPUT arrayID year $ dayofyr $  hourmin $ DOtempC DOsat DOppm
        wtr_0 wtr_1 wtr_2 wtr_3 wtr_4 wtr_5 wtr_6 wtr_7 wtr_8 wtr_9 wtr_10
        AirtempC RelHum PAR
        WindSpd CorrWind
        WindSpdAv WindVect MaxWind MaxWindDir LoggerBatV RadioBatV IntLgBxTemp Heading DOLoTempC DOLowSat DOLowPPM;
RUN;


DATA d2015;
     INFILE 'GloeoGroup/Data/CrossYearComparisons/BuoyData/raw_data_from_Geoff/2015 Buoy Data.csv'
        dlm=',' firstobs=2;
     INPUT arrayID year $ dayofyr $ hourmin $ DOTempC DOsat DOppm
        wtr_0 wtr_1 wtr_2 wtr_3 wtr_4 wtr_5 wtr_6 wtr_7 wtr_8 wtr_9 wtr_10
        AirTempC RelHum PAR
        WindSpd CorrWind
        WindSpdAv WindVect MaxWind MaxWindDir LoggerBatV RadioBatV IntLgBxTemp Heading DOLoTempC DOLowSat DOLowPPM;
RUN;


DATA d2016;
     INFILE 'GloeoGroup/Data/CrossYearComparisons/BuoyData/raw_data_from_Geoff/2016 Buoy Data.csv'
        dlm=',' firstobs=2;
     INPUT arrayID year $ dayofyr $ hourmin $ DOTempC DOsat DOppm
        wtr_0 wtr_1 wtr_2 wtr_3 wtr_4 wtr_5 wtr_6 wtr_7 wtr_8 wtr_9
        AirTempC RelHum PAR
        WindSpdAv WindVect MaxWind MaxWindDir LoggerBatV RadioBatV IntLgBxTemp Heading DOLoTempC DOLowSat DOLowPPM              Chlor_RFU S
RUN;


/* *****************************
APPEND THE DATA TOGETHER
***************************** */

DATA doutput.Buoy1416;

        SET d2014 d2015 d2016;

        /* Remove dayofyr outside the range of interest: 1 June - 1 October (days 152-274) */
        if dayofyr<152 then delete;
        if dayofyr>274 then delete;

        /* buoy thermistor string failed summer 2015 for most of year */
        if year=2015 and dayofyr>176 then delete;

        /* Change -6999 values to missing */
        if wtr_0=-6999 then wtr_0=.;
        if wtr_1=-6999 then wtr_1=.;
        if wtr_2=-6999 then wtr_2=.;
        if wtr_3=-6999 then wtr_3=.;
        if wtr_4=-6999 then wtr_4=.;
        if wtr_5=-6999 then wtr_5=.;
        if wtr_6=-6999 then wtr_6=.;
        if wtr_7=-6999 then wtr_7=.;
        if wtr_8=-6999 then wtr_8=.;
        if wtr_9=-6999 then wtr_9=.;
        if wtr_10=-6999 then wtr_10=.;

        if wtr_0=555.4 then wtr_0=.;
        if wtr_1=555.4 then wtr_1=.;
        if wtr_2=555.4 then wtr_2=.;
        if wtr_3=555.4 then wtr_3=.;
        if wtr_4=555.4 then wtr_4=.;
        if wtr_5=555.4 then wtr_5=.;
        if wtr_6=555.4 then wtr_6=.;
        if wtr_7=555.4 then wtr_7=.;
        if wtr_8=555.4 then wtr_8=.;
        if wtr_9=555.4 then wtr_9=.;
        if wtr_10=555.4 then wtr_10=.;

        if wtr_0=1215 then wtr_0=.;
        if wtr_1=1215 then wtr_1=.;
        if wtr_2=1215 then wtr_2=.;
        if wtr_3=1215 then wtr_3=.;
        if wtr_4=1215 then wtr_4=.;
        if wtr_5=1215 then wtr_5=.;
        if wtr_6=1215 then wtr_6=.;
        if wtr_7=1215 then wtr_7=.;
        if wtr_8=1215 then wtr_8=.;
        if wtr_9=1215 then wtr_9=.;
        if wtr_10=1215 then wtr_10=.;

        /* kludged data cleaning attempt */
        if wtr_0>30 then wtr_0=.;
        if wtr_0<=9 then wtr_0=.;

        /* need to convert year and day of year to a YYYY-MM-DD numeric format
        the SAS function DATEJUL will take YYYYDDD and turn it into a SAS date
        right now we have YYYY and DDD as separate entities, they need to be combined */
        yeardoystr=trim(year) || trim(dayofyr);
        assasdate=input(yeardoystr, julian7.);
        format assasdate yymmdd10.;

        /* need to figure out how to deal with the hour/min variable
        gets read in as a three or four-digit string */
        nchar=length(hourmin); *get length of the string;
        if nchar=3 then minutestr=substr(hourmin,2,2); * get the minutes;
        if nchar=4 then minutestr=substr(hourmin,3,2); * get the minutes;
        if nchar=3 then hourstr='0' || substr(hourmin,1,1);   * get the hours;
        if nchar=4 then hourstr=substr(hourmin,1,2);   * get the hours;
        if nchar=2 then hourstr='00';
        if nchar=2 then minutestr=hourmin;
        timestr= trim(hourstr) || ':' || minutestr;
        if nchar=1 then timestr='00:00';

RUN;

/*
PROC PRINT;
        VAR assasdate timestr temp0-temp10;
RUN;
*/

DATA doutput14;
     SET doutput.Buoy1416;
     IF year ne 2014 then delete;
RUN;
DATA doutput14;
     SET doutput14;
     FILE 'GloeoGroup/Data/CrossYearComparisons/BuoyData/SAS_output/Sunapee14.wtr.txt';
     IF _N_=1 THEN PUT "datetime" '09'x "wtr_0.0" '09'x "wtr_1.0" '09'x "wtr_2.0" '09'x "wtr_3.0" '09'x
        "wtr_4.0" '09'x "wtr_5.0" '09'x "wtr_6.0" '09'x "wtr_7.0" '09'x "wtr_8.0" '09'x "wtr_9.0" '09'x "wtr_10.0" '09'x;
     PUT assasdate timestr '09'x wtr_0 '09'x wtr_1 '09'x wtr_2 '09'x wtr_3 '09'x
        wtr_4 '09'x wtr_5 '09'x wtr_6 '09'x wtr_7 '09'x wtr_8 '09'x wtr_9 '09'x wtr_10 '09'x;
RUN;
DATA doutput14;
     SET doutput14;
     FILE 'GloeoGroup/Data/CrossYearComparisons/BuoyData/SAS_output/Sunapee14.wind.txt';
     IF _n_=1 THEN PUT "DateTime" '09'x "WindSpeed";
     PUT assasdate timestr '09'x WindSpdAv;
RUN;


DATA doutput15;
     SET doutput.Buoy1416;
     IF year ne 2015 then delete;
DATA doutput15;
     SET doutput15;
     FILE 'GloeoGroup/Data/CrossYearComparisons/BuoyData/SAS_output/Sunapee15.wtr.txt';
     IF _N_=1 THEN PUT "datetime" '09'x "wtr_0.0" '09'x "wtr_1.0" '09'x "wtr_2.0" '09'x "wtr_3.0" '09'x
        "wtr_4.0" '09'x "wtr_5.0" '09'x "wtr_6.0" '09'x "wtr_7.0" '09'x "wtr_8.0" '09'x "wtr_9.0" '09'x "wtr_10.0" '09'x;
     PUT assasdate timestr '09'x wtr_0 '09'x wtr_1 '09'x wtr_2 '09'x wtr_3 '09'x
        wtr_4 '09'x wtr_5 '09'x wtr_6 '09'x wtr_7 '09'x wtr_8 '09'x wtr_9 '09'x wtr_10 '09'x;
RUN;

DATA doutput15;
     SET doutput15;
     FILE 'GloeoGroup/Data/CrossYearComparisons/BuoyData/SAS_output/Sunapee15.wind.txt';
     IF _n_=1 THEN PUT "DateTime" '09'x "WindSpeed";
     PUT assasdate timestr '09'x WindSpdAv;
RUN;

DATA doutput16;
     SET doutput.Buoy1416;
     IF year ne 2016 then delete;
DATA doutput16;
     SET doutput16;
     FILE 'GloeoGroup/Data/CrossYearComparisons/BuoyData/SAS_output/Sunapee16.wtr.txt';
     IF _N_=1 THEN PUT "datetime" '09'x "wtr_0.0" '09'x "wtr_1.0" '09'x "wtr_2.0" '09'x "wtr_3.0" '09'x
        "wtr_4.0" '09'x "wtr_5.0" '09'x "wtr_6.0" '09'x "wtr_7.0" '09'x "wtr_8.0" '09'x "wtr_9.0" '09'x "wtr_10.0" '09'x;
     PUT assasdate timestr '09'x wtr_0 '09'x wtr_1 '09'x wtr_2 '09'x wtr_3 '09'x
        wtr_4 '09'x wtr_5 '09'x wtr_6 '09'x wtr_7 '09'x wtr_8 '09'x wtr_9 '09'x wtr_10 '09'x;
RUN;

DATA doutput16;
     SET doutput16;
     FILE 'GloeoGroup/Data/CrossYearComparisons/BuoyData/SAS_output/Sunapee16.wind.txt';
     IF _n_=1 THEN PUT "DateTime" '09'x "WindSpeed";
     PUT assasdate timestr '09'x WindSpdAv;
RUN;
             RUN;






RUN;
