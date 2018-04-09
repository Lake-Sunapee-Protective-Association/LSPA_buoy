/*****************************************************************/
/*        Institute of Ecosystem Studies (Millbrook, NY)         */
/*                                                               */
/* TITLE:   Sunapee_buoy_data_input.sas                          */
/* AUTHOR:  Amanda Elliott                                       */
/* SYSTEM:  Toshiba Tecra, Windows XP    SAS 9.1                 */
/* DATE:    17Oct2007                                            */
/* PROJECT: Lake Sunapee monitoring bouy                         */
/* PURPOSE: Read in raw buoy datalogger data and export as an    */
/*          organized dataset                                    */
/*                                                               */
/*         			                                             */
/*****************************************************************/
/*                     folder tree structure                     */
/*                                                               */
/*                         Lake Sunapee                          */
/*                               |                               */
/*                          monitoring                           */
/*                               |                               */
/*                           buoy data                           */
/*                           /      \                            */
/*                  sas program    SAS_raw_data                  */
/*                                      |                        */
/*                                   raw data files              */
/*                                                               */
/*                                                               */
/*                                                               */
/*****************************************************************/
options ls=100 ps=52 pageno=1; * when page orientation=portrait;
libname buoy "sasdatalibr";



* NEED TO DEAL WITH THE CHANGE IN DEPTH DURING THE FALL OF 2007;
* EASIEST WAY IS TO SPLIT THE RAW DATA FILES AND READ IN AS DIFFERENT
  DEPTHS AND JUST HAVE MORE VARIABLES.;





*******************************************************;
* read in raw bouy data;

title 'raw_buoy';
data raw_Aug_Oct07;
  infile "SAS_raw_data\Sunapee_buoy_data_27Aug07-02Oct07.csv" 
    firstobs=1 dlm=',' n=2;
  input array year julian_day time1 temponDO_C DOpersat DO_ppm temp0m_C
    temp0p5m_C temp1m_C temp1p5m_C temp2m_C temp2p5m_C temp3m_C temp4m_C
    temp5m_C temp6m_C temp7m_C temp8m_C temp9m_C temp10m_C temp11m_C
    temp13m_C temp15m_C temp_air_C relhum_pct PAR_umolesm2s wind_dir_deg
    wind_spd_ms temp_anem_C;
  run;
*proc print data=raw_Aug_Oct07; run;

data raw_Oct_Nov07;
  infile "SAS_raw_data\Sunapee_buoy_data02Oct07-21Nov07.dat" 
    firstobs=1 dlm=',' n=2;
  input array year julian_day time1 temponDO_C DOpersat DO_ppm temp0m_C
    temp0p5m_C temp1m_C temp1p5m_C temp2m_C temp2p5m_C temp3m_C temp4m_C
    temp5m_C temp6m_C temp7m_C temp8m_C temp9m_C temp10m_C temp11m_C
    temp13m_C temp15m_C temp_air_C relhum_pct PAR_umolesm2s wind_dir_deg
    wind_spd_ms temp_anem_C;
  run;
*proc print data=raw_Oct_Nov07; run;

data raw_Nov_Dec07;
  infile "SAS_raw_data\Sunapee_buoy_data21Nov-13Dec07.dat" 
    firstobs=1 dlm=',' n=2;
  input array year julian_day time1 temponDO_C DOpersat DO_ppm temp0m_C
    temp0p5m_C temp1m_C temp1p5m_C temp2m_C temp2p5m_C temp3m_C temp4m_C
    temp5m_C temp6m_C temp7m_C temp8m_C temp9m_C temp10m_C temp11m_C
    temp13m_C temp15m_C temp_air_C relhum_pct PAR_umolesm2s wind_dir_deg
    wind_spd_ms temp_anem_C;
  run;
*proc print data=raw_Nov_Dec07; run;

data raw_Dec07_Jan08;
  infile "SAS_raw_data\Sunapee_buoy_data20Dec07-17Jan08.dat" 
    firstobs=1 dlm=',' n=2;
  input array year julian_day time1 temponDO_C DOpersat DO_ppm temp0m_C
    temp0p5m_C temp1m_C temp1p5m_C temp2m_C temp2p5m_C temp3m_C temp4m_C
    temp5m_C temp6m_C temp7m_C temp8m_C temp9m_C temp10m_C temp11m_C
    temp13m_C temp15m_C temp_air_C relhum_pct PAR_umolesm2s wind_dir_deg
    wind_spd_ms temp_anem_C;
  run;
*proc print data=raw_Dec07_Jan08; run;

data raw_17Jan_30Jan08;
  infile "SAS_raw_data\Sunapee_buoy_data17Jan-30Jan08.dat" 
    firstobs=1 dlm=',' n=2;
  input array year julian_day time1 temponDO_C DOpersat DO_ppm temp0m_C
    temp0p5m_C temp1m_C temp1p5m_C temp2m_C temp2p5m_C temp3m_C temp4m_C
    temp5m_C temp6m_C temp7m_C temp8m_C temp9m_C temp10m_C temp11m_C
    temp13m_C temp15m_C temp_air_C relhum_pct PAR_umolesm2s wind_dir_deg
    wind_spd_ms temp_anem_C;
  run;
*proc print data=raw_17Jan_30Jan08; run;

data raw_03Feb_17Feb08;
  infile "SAS_raw_data\Sunapee_buoy_data03Feb-17Feb2008.dat" 
    firstobs=1 dlm=',' n=2;
  input array year julian_day time1 temponDO_C DOpersat DO_ppm temp0m_C
    temp0p5m_C temp1m_C temp1p5m_C temp2m_C temp2p5m_C temp3m_C temp4m_C
    temp5m_C temp6m_C temp7m_C temp8m_C temp9m_C temp10m_C temp11m_C
    temp13m_C temp15m_C temp_air_C relhum_pct PAR_umolesm2s wind_dir_deg
    wind_spd_ms temp_anem_C;
  run;
*proc print data=raw_03Feb_17Feb08; run;

data raw_17Feb_28Mar08;
  infile "SAS_raw_data\Sunapee_buoy_data17Feb-28Mar08.dat" 
    firstobs=1 dlm=',' n=2;
  input array year julian_day time1 temponDO_C DOpersat DO_ppm temp0m_C
    temp0p5m_C temp1m_C temp1p5m_C temp2m_C temp2p5m_C temp3m_C temp4m_C
    temp5m_C temp6m_C temp7m_C temp8m_C temp9m_C temp10m_C temp11m_C
    temp13m_C temp15m_C temp_air_C relhum_pct PAR_umolesm2s wind_dir_deg
    wind_spd_ms temp_anem_C;
  run;
*proc print data=raw_03Feb_17Feb08; run;

data raw_09Apr_223Apr08;
  infile "SAS_raw_data\Sunapee_buoy_data09Apr-23Apr08.dat" 
    firstobs=1 dlm=',' n=2;
  input array year julian_day time1 temponDO_C DOpersat DO_ppm temp0m_C
    temp0p5m_C temp1m_C temp1p5m_C temp2m_C temp2p5m_C temp3m_C temp4m_C
    temp5m_C temp6m_C temp7m_C temp8m_C temp9m_C temp10m_C temp11m_C
    temp13m_C temp15m_C temp_air_C relhum_pct PAR_umolesm2s wind_dir_deg
    wind_spd_ms temp_anem_C;
  run;
*proc print data=raw_09Apr_223Apr08; run;




data raw_buoy;
  * merge all raw files;
    set raw_Aug_Oct07 raw_Oct_Nov07 raw_Nov_Dec07 raw_Dec07_Jan08
      raw_17Jan_30Jan08 raw_03Feb_17Feb08 raw_17Feb_28Mar08 raw_09Apr_223Apr08;

  * create a julian date from the year and julian day;
	year1000=year*1000;
	juliandate=year1000+julian_day;
	date=datejul(juliandate);
	format date date9.;

  * convert time to a format SAS can deal with;
	LENGTH timestring $ 4;
	timestring=time1;
	hour=120; min=120;
	* 1am to 9:50am;
	if time1>50 and time1<=950 then hourS=substr(timestring,2,1);
	if time1>50 and time1<=950 then minuteS=substr(timestring,3,2);
	* after 10am;
	if time1>950 then hourS=substr(timestring,1,2);
	if time1>950 then minuteS=substr(timestring,3,2);
	hour=hourS; min=minuteS;
	sec=0;
	* midnight to 12:50am;
	if time1=0 then hour=0;
	if time1=0 then min=0;
	if time1>0 and time1<=50 then hour=0;
	if time1>0 and time1<=50 then min=time1;

	datetime=DHMS(date,hour,min,sec);
	format datetime datetime.;
	time=timepart(datetime);
	format time hhmm.;

  * set missing values to missing;
	if temponDO_C=-6999 then temponDO_C='.';
	if DOpersat=-6999 then DOpersat='.';
	if DO_ppm=-6999 then DO_ppm='.';
	if temp0m_C=-6999 then temp0m_C='.';
	if temp0p5m_C=-6999 then temp0p5m_C='.';
	if temp1m_C=-6999 then temp1m_C='.';
	if temp1p5m_C=-6999 then temp1p5m_C='.';
	if temp2m_C=-6999 then temp2m_C='.';
	if temp2p5m_C=-6999 then temp2p5m_C='.';
	if temp3m_C=-6999 then temp3m_C='.';
	if temp4m_C=-6999 then temp4m_C='.';
	if temp5m_C=-6999 then temp5m_C='.';
	if temp6m_C=-6999 then temp6m_C='.';
	if temp7m_C=-6999 then temp7m_C='.';
	if temp8m_C=-6999 then temp8m_C='.';
	if temp9m_C=-6999 then temp9m_C='.';
	if temp10m_C=-6999 then temp10m_C='.';
	if temp11m_C=-6999 then temp11m_C='.';
	if temp13m_C=-6999 then temp13m_C='.';
	if temp15m_C=-6999 then temp15m_C='.';
	if temp_air_C=-6999 then temp_air_C='.';
	if relhum_pct=-6999 then relhum_pct='.';
	if PAR_umolesm2s=-6999 then PAR_umolesm2s='.';
	if wind_dir_deg=-6999 then wind_dir_deg='.';
	if wind_spd_ms=-6999 then wind_spd_ms='.';
	if temp_anem_C=-6999 then temp_anem_C='.';

  drop timestring array hourS minuteS year1000 juliandate time1 sec;
  run;
*proc print data=raw_buoy; run;


*******************************************************;
* create cleaned up dataset of all data;

title 'Sunapee_buoy_data';
proc SQL;
 create table buoy.Sunapee_buoy_data as
   select datetime, date, time, hour, temponDO_C, DOpersat, DO_ppm, temp0m_C,
    temp0p5m_C, temp1m_C, temp1p5m_C, temp2m_C, temp2p5m_C, temp3m_C,
    temp4m_C, temp5m_C, temp6m_C, temp7m_C, temp8m_C, temp9m_C, temp10m_C,
    temp11m_C, temp13m_C, temp15m_C, temp_air_C, PAR_umolesm2s,
    wind_dir_deg, wind_spd_ms, temp_anem_C
   from raw_buoy;
*proc print data=buoy.Sunapee_buoy_data; run;


