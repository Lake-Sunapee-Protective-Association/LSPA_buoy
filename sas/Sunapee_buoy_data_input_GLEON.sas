/*****************************************************************/
/*        Institute of Ecosystem Studies (Millbrook, NY)         */
/*                                                               */
/* TITLE:   Sunapee_buoy_data_input_GLEON.sas                    */
/* AUTHOR:  Amanda Elliott                                       */
/* SYSTEM:  Dell Vostro, Windows Vista, SAS 9.1                  */
/* DATE:    05May2008                                            */
/* PROJECT: Lake Sunapee monitoring bouy                         */
/* PURPOSE: Read in raw buoy datalogger data as downloaded from  */
/*          the GLEON website and export as an organized         */
/*          dataset for use in Sunapee_buoy_data_summary.sas     */
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

* To obtain raw data:
Go to GLEON data download site: http://dbbadger.gleonrcn.org
Get data from a site
Select Sunapee_LI_Buoy and select all the variables in the order they appear:
	air_temp, DO, PAR, RH, sensor_temp, water_temp (all, ie 0-13),
	wind_direction, wind_speed
Select the date range you want and click "get data" and wait for data to load
Then click "get sparse matrix" and save the csv file in the "raw_from_gleon_site"
	folder;


*** Read in raw data as downloaded above ***;
* Note, if you download different or more data, either change the file name in
  the infile statement or add another data step with another infile and input
  statement and merge;

title 'raw_buoy';
data raw_27Aug07_31Mar08;
  infile "raw_from_gleon_site\Sunapee_raw_gleon_27Aug07_31Mar08.csv" 
    firstobs=2 dlm=', ' n=2; *lrecl=500;
  input date mmddyy10. time hhmmss. drop1 temp_air_C DO_ppm PAR_umolesm2s
    relhum_pct sensor_temp_C temp0m_C temp0p5m_C temp1m_C temp1p5m_C temp2m_C
    temp2p5m_C temp3m_C temp4m_C temp5m_C temp6m_C temp7m_C temp8m_C temp9m_C
    temp10m_C temp11m_C temp13m_C wind_dir_deg wind_spd_ms;
  format date date9.;
  format time time.;
  drop drop1 sensor_temp_C;
  run;
*proc print data=raw_27Aug07_31Mar08; run;

data raw_01Apr_20May08;
  infile "raw_from_gleon_site\Sunapee_raw_gleon_01Apr08_20May08.csv" 
    firstobs=2 dlm=', ' n=2; *lrecl=500;
  input date mmddyy10. time hhmmss. drop1 temp_air_C DO_ppm PAR_umolesm2s
    relhum_pct sensor_temp_C temp0m_C temp0p5m_C temp1m_C temp1p5m_C temp2m_C
    temp2p5m_C temp3m_C temp4m_C temp5m_C temp6m_C temp7m_C temp8m_C temp9m_C
    temp10m_C temp11m_C temp13m_C wind_dir_deg wind_spd_ms;
  format date date9.;
  format time time.;
  drop drop1 sensor_temp_C;
  run;
*proc print data=raw_01Apr_20May08; run;

data raw_20May_28May08;
  infile "raw_from_gleon_site\Sunapee_raw_gleon_20May08_28May08.csv" 
    firstobs=2 dlm=', ' n=2; *lrecl=500;
  input date mmddyy10. time hhmmss. drop1 temp_air_C DO_ppm PAR_umolesm2s
    relhum_pct sensor_temp_C temp0m_C temp0p5m_C temp1m_C temp1p5m_C temp2m_C
    temp2p5m_C temp3m_C temp4m_C temp5m_C temp6m_C temp7m_C temp8m_C temp9m_C
    temp10m_C temp11m_C temp13m_C wind_dir_deg wind_spd_ms;
  format date date9.;
  format time time.;
  drop drop1 sensor_temp_C;
  run;
*proc print data=raw_20May_28May08; run;


* merge raw data files;
data raw_buoy;
  set raw_27Aug07_31Mar08 raw_01Apr_20May08 raw_20May_28May08;
  * set missing values to missing;
	if temp_air_C=-6999 then temp_air_C='.';
	if DO_ppm=-6999 then DO_ppm='.';
	if PAR_umolesm2s=-6999 then PAR_umolesm2s='.';
	if relhum_pct=-6999 then relhum_pct='.';
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
	if wind_dir_deg=-6999 then wind_dir_deg='.';
	if wind_spd_ms=-6999 then wind_spd_ms='.';
  * create various date and time variables;
	hour=hour(time);
	min=minute(time);
	datetime=dhms(date, hour, min, 0);
	format datetime datetime.;
  	drop min;
  * create a bogus variable temp15m_C so this file matches the
	one created in "Sunapee_buoy_data_input.sas" and both can be
	used with "Sunapee_buoy_data_summary.sas";
	format temp15m_C best.;
	temp15m_C='.';
  run;
*proc print data=raw_buoy; run;


*******************************************************;
* create cleaned up dataset of all data;

title 'Sunapee_buoy_data';
proc SQL;
 create table buoy.Sunapee_buoy_data as
   select datetime, date, time, hour, DO_ppm, temp0m_C, temp0p5m_C,
    temp1m_C, temp1p5m_C, temp2m_C, temp2p5m_C, temp3m_C, temp4m_C,
    temp5m_C, temp6m_C, temp7m_C, temp8m_C, temp9m_C, temp10m_C,
    temp11m_C, temp13m_C, temp15m_C, temp_air_C, PAR_umolesm2s,
    wind_dir_deg, wind_spd_ms
   from raw_buoy;
*proc print data=buoy.Sunapee_buoy_data; run;
