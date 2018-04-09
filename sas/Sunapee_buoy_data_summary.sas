/*****************************************************************/
/*        Institute of Ecosystem Studies (Millbrook, NY)         */
/*                                                               */
/* TITLE:   Sunapee_buoy_data_summary.sas                        */
/* AUTHOR:  Amanda Elliott                                       */
/* SYSTEM:  Dell Vostro, Windows Vista, SAS 9.1                  */
/* DATE:    31Jan2008                                            */
/* PROJECT: Lake Sunapee monitoring bouy                         */
/* PURPOSE: Use raw buoy datalogger data as organized in         */
/*          Sunapee_buoy_data_input.sas to calculate summary     */
/*          values for graphing purposes                         */
/*         			                                             */
/*****************************************************************/
/*                     folder tree structure                     */
/*                                                               */
/*                         Lake Sunapee                          */
/*                               |                               */
/*                          monitoring                           */
/*                               |                               */
/*                           buoy data                           */
/*                 _________/    |    \_________                 */
/*                /              |              \                */
/*  sas program files    raw_from_gleon_site    temp_SAS_output  */
/*                               |                    |          */
/*                          *.csv files            exported      */
/*                                               *.xls files     */
/*                                                               */
/*                                                               */
/*****************************************************************/
options ls=100 ps=52 pageno=1; * when page orientation=portrait;
libname buoy "sasdatalibr";


* NEED TO DEAL WITH THE CHANGE IN DEPTH DURING THE FALL OF 2007;
* EASIEST WAY IS TO SPLIT THE RAW DATA FILES AND READ IN AS DIFFERENT
  DEPTHS AND JUST HAVE MORE VARIABLES.;


******************************************************************;
**                        daily averages                        **;
******************************************************************;


*******************************************************;
* calculate daily average temps and DO conc for graphing;

title 'daily_avg_temps';
proc sort data=buoy.Sunapee_buoy_data; by date; run;
proc means data=buoy.Sunapee_buoy_data noprint;
  by date;
  var temp0m_C temp0p5m_C temp1m_C temp1p5m_C temp2m_C temp2p5m_C temp3m_C
    temp4m_C temp5m_C temp6m_C temp7m_C temp8m_C temp9m_C temp10m_C
    temp11m_C temp13m_C temp15m_C DO_ppm;
  output out=daily_avg_temps mean=;
  run;
*proc print data=daily_avg_temps; run;
proc export data=daily_avg_temps
  outfile="temp_SAS_output\daily_avg_temps.xls" replace;
  run;


*******************************************************;
* calculate daily average wind direction;

title 'daily_avg_wd';
data daily_avg_wd1;
  set buoy.Sunapee_buoy_data;
  * convert wind direction from degrees to radians;
  wdrad=(wind_dir_deg*0.017453);
  * calculate the X and Y coordinates of each vector;
  Y=wind_spd_ms*sin(wdrad);
  X=wind_spd_ms*cos(wdrad);
  run;
proc sort data=daily_avg_wd1; by date; run;
proc means data=daily_avg_wd1 noprint;
  by date;
  var Y X;
  output out=mean_XY mean=;
  run;
*proc print data=mean_XY; run;
data daily_avg_wd;
  set mean_XY;
  r=sqrt(((Y*Y)+(X*X)));
  sina=Y/r;
  cosa=X/r;
  a=(arsin(sina));
  b=(arcos(cosa));
  adeg=a*57.29674;
  if sina>0 and cosa>0 then do;
    mean_wd_deg=adeg;
    end;
  if sina<0 and cosa>0 then do;
    mean_wd_deg=360+adeg;
    end;
  if sina>0 and cosa<0 then do;
    mean_wd_deg=180-adeg;
    end;
  if sina<0 and cosa<0 then do;
    mean_wd_deg=180-adeg;
    end;
  keep date _freq_ mean_wd_deg;
  run;
*proc print data=daily_avg_wd; run;


*******************************************************;
* calculate daily average met data;

title 'daily_avg_met';
proc sort data=buoy.Sunapee_buoy_data; by date; run;
proc means data=buoy.Sunapee_buoy_data noprint;
  by date;
  var temp_air_C PAR_umolesm2s wind_spd_ms;
  output out=daily_avg_met1 mean=;
  run;
*proc print data=daily_avg_met1; run;
proc sort data=daily_avg_met1; by date; run;
proc sort data=daily_avg_wd; by date; run;
* merge average met data and wind direction data;
data daily_avg_met;
  merge daily_avg_met1 daily_avg_wd;
    by date;
  run;
proc export data=daily_avg_met
  outfile="temp_SAS_output\daily_avg_met.xls" replace;
  run;


*******************************************************;
* tabulate frequency of daily mean wind direction for rose graphing;

title 'daily_wd_freq';
data daily_wd;
  set daily_avg_wd;
  if 0<mean_wd_deg<11.25 then wd_category=0;
  if 11.25<mean_wd_deg<33.75 then wd_category=22.5;
  if 33.75<mean_wd_deg<56.25 then wd_category=45;
  if 56.25<mean_wd_deg<78.75 then wd_category=67.5;
  if 78.75<mean_wd_deg<101.25 then wd_category=90;
  if 101.25<mean_wd_deg<123.75 then wd_category=112.5;
  if 123.75<mean_wd_deg<146.25 then wd_category=135;
  if 146.25<mean_wd_deg<168.75 then wd_category=157.5;
  if 168.75<mean_wd_deg<191.25 then wd_category=180;
  if 191.25<mean_wd_deg<213.75 then wd_category=202.5;
  if 213.75<mean_wd_deg<236.25 then wd_category=225;
  if 236.25<mean_wd_deg<258.75 then wd_category=247.5;
  if 258.75<mean_wd_deg<281.25 then wd_category=270;
  if 281.25<mean_wd_deg<303.75 then wd_category=292.5;
  if 303.75<mean_wd_deg<326.25 then wd_category=315;
  if 326.25<mean_wd_deg<348.75 then wd_category=337.5;
  if 348.75<mean_wd_deg<360 then wd_category=360;
  run;

proc freq data=daily_wd noprint;
  tables wd_category /out=daily_wd_freq;
  run;
proc print data=daily_wd_freq; run;



******************************************************************;
**                       hourly averages                        **;
******************************************************************;


*******************************************************;
* calculate hourly average temps and DO conc for graphing;

title 'hourly_avg_temps';
proc sort data=buoy.Sunapee_buoy_data; by date hour; run;
proc means data=buoy.Sunapee_buoy_data noprint;
  by date hour;
  var temp0m_C temp0p5m_C temp1m_C temp1p5m_C temp2m_C temp2p5m_C temp3m_C
    temp4m_C temp5m_C temp6m_C temp7m_C temp8m_C temp9m_C temp10m_C
    temp11m_C temp13m_C temp15m_C DO_ppm;
  output out=hourly_avg_temps1 mean=;
  run;
data hourly_avg_temps;
  set hourly_avg_temps1;
  min=0;
  sec=0;
  datetime=DHMS(date,hour,min,sec);
  format datetime datetime.;
  drop date hour min sec _type_ _freq_;
  run;
*proc print data=hourly_avg_temps; run;
proc export data=hourly_avg_temps
  outfile="temp_SAS_output\hourly_avg_temps.xls" replace;
  run;


*******************************************************;
* calculate hourly average wind direction;

title 'hourly_avg_wd';
data hourly_avg_wd1;
  set buoy.Sunapee_buoy_data;
  * if wind speed is recorded as 0, set to a very small number
    so in later calculations, we're not dividing by 0;
  if wind_spd_ms=0 then wind_spd_ms=0.0001;
  * convert wind direction from degrees to radians;
  wdrad=(wind_dir_deg*0.017453);
  * calculate the X and Y coordinates of each vector;
  Y=wind_spd_ms*sin(wdrad);
  X=wind_spd_ms*cos(wdrad);
  run;
proc sort data=hourly_avg_wd1; by date; run;
proc means data=hourly_avg_wd1 noprint;
  by date hour;
  var Y X;
  output out=mean_XY1 mean=;
  run;
proc print data=mean_XY1; run;
data hourly_avg_wd;
  set mean_XY1;
  r=sqrt(((Y*Y)+(X*X)));
  sina=Y/r;
  cosa=X/r;
  a=(arsin(sina));
  b=(arcos(cosa));
  adeg=a*57.29674;
  if sina>0 and cosa>0 then do;
    mean_wd_deg=adeg;
    end;
  if sina<0 and cosa>0 then do;
    mean_wd_deg=360+adeg;
    end;
  if sina>0 and cosa<0 then do;
    mean_wd_deg=180-adeg;
    end;
  if sina<0 and cosa<0 then do;
    mean_wd_deg=180-adeg;
    end;
  keep date _freq_ mean_wd_deg hour;
  run;
*proc print data=hourly_avg_wd; run;


*******************************************************;
* calculate hourly average met data;

title 'hourly_avg_met';
proc sort data=buoy.Sunapee_buoy_data; by date hour; run;
proc means data=buoy.Sunapee_buoy_data noprint;
  by date hour;
  var temp_air_C PAR_umolesm2s wind_spd_ms;
  output out=hourly_avg_met1 mean=;
  run;
*proc print data=hourly_avg_met1; run;
proc sort data=hourly_avg_met1; by date hour; run;
proc sort data=hourly_avg_wd; by date hour; run;
* merge average met data and wind direction data;
data hourly_avg_met;
  merge hourly_avg_met1 hourly_avg_wd;
    by date hour;
  min=0;
  sec=0;
  datetime=DHMS(date,hour,min,sec);
  format datetime datetime.;
  drop date hour min sec _type_ _freq_;
  run;
proc export data=hourly_avg_met
  outfile="temp_SAS_output\hourly_avg_met.xls" replace;
  run;


*******************************************************;
* tabulate frequency of hourly mean wind direction for rose graphing;

title 'hourly_wd_freq';
data hourly_wd;
  set hourly_avg_wd;
  if 0<mean_wd_deg<11.25 then wd_category=0;
  if 11.25<mean_wd_deg<33.75 then wd_category=22.5;
  if 33.75<mean_wd_deg<56.25 then wd_category=45;
  if 56.25<mean_wd_deg<78.75 then wd_category=67.5;
  if 78.75<mean_wd_deg<101.25 then wd_category=90;
  if 101.25<mean_wd_deg<123.75 then wd_category=112.5;
  if 123.75<mean_wd_deg<146.25 then wd_category=135;
  if 146.25<mean_wd_deg<168.75 then wd_category=157.5;
  if 168.75<mean_wd_deg<191.25 then wd_category=180;
  if 191.25<mean_wd_deg<213.75 then wd_category=202.5;
  if 213.75<mean_wd_deg<236.25 then wd_category=225;
  if 236.25<mean_wd_deg<258.75 then wd_category=247.5;
  if 258.75<mean_wd_deg<281.25 then wd_category=270;
  if 281.25<mean_wd_deg<303.75 then wd_category=292.5;
  if 303.75<mean_wd_deg<326.25 then wd_category=315;
  if 326.25<mean_wd_deg<348.75 then wd_category=337.5;
  if 348.75<mean_wd_deg<360 then wd_category=360;
  run;

proc freq data=hourly_wd noprint;
  tables wd_category /out=hourly_wd_freq;
  run;
proc print data=hourly_wd_freq; run;



******************************************************************;
**                     temperature profiles                     **;
******************************************************************;


*******************************************************;
* choose temp values from noon one day per week for profile graphing;

title 'profile_1day_noon';
data profile_1day_noon;
  set buoy.Sunapee_buoy_data;
  dayofweek=weekday(date);
  if dayofweek=4 and time='12:00't;
  run;
*proc print data=profile_1day_noon; run;
proc export data=profile_1day_noon
  outfile="temp_SAS_output\profile_1day_noon.xls" replace;
  run;
