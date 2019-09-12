/* CREATE_DISTR_M01_FILES.SAS
    Craig Heither, rev. 02-19-2016

   Program creates the scenario-specific DISTR and M01 files used by Pre-Distribution and Mode Choice based on Eash's
   methodology. 

    M01 file components
   -----------------------
     - Socec data from TG from m01tg.txt.
     - Workers per vehicle at destination (from CTPP) from m01auto.csv.
     - Zone type flag from m01type.csv.
     - Zonal attributes based on the Emme network:
        * Park-&-Ride cost (cents) - based on lot closest to Zone centroid (within 10 miles).
        * Park-&-Ride flag.
        * First wait for bus work trip (minutes).
        * First wait for bus nonwork trip (minutes).
        * First wait for feeder bus work trip (minutes).
        * First wait for feeder bus nonwork trip (minutes).


    DISTR file components
   -----------------------
   All zonal attributes calculated using Emme network data.  See 02-19-2016 revisions below.
     - Mean distance to Metra station, to CTA rail station and to Park-n-Ride (miles) - weighted by subzone households.
        * The distance of the closest rail station to each subzone centroid is determined and the subzone distances are averaged
          to calculate the zonal mean. Calculations are performed separately for Metra, CTA Rail and Park-n-Ride.
          stations. 
		* No longer capped at 19.95 miles.
		* Additional impedance added for zones>1712 (zones outside of CMAP 7 counties and which only contain one subzone).  Since these
          zones only contain one subzone, distances (and standard deviations) to CTA rail stations/Metra stations/park-n-ride lots are 
		  calculated from random points within the zone (taken from data\distr\zone_random_points.shp).  Extra impedance is added by 
		  calculating Manhattan distance rather than Euclidean.
		* If the average zonal distance for CTA rail exceeds 19.95, turn off access (use 999 code).  
     - Standard deviation of distance to Metra station, to CTA rail station and to Park-n-Ride (miles).
        * Calculated as the square root of the sum of (zonal variance + a subzone variance [estimated by Eash to be 0.042]).
     - Type of rail and Park-n-Ride distribution (used by Mode Choice).
        * 101=normal distribution (default), 102=exponential distribution.
     - Minimum bus stop distance (miles).
        * Bus stop buffers incremented by 0.1 miles between 0.1 and 1.1 miles. Determined by the value of the smallest buffer that 
          touches any part of a zone. The value is set to 999 for zones with no stops within 1.1 miles. Calculations are performed 
          separately for bus and feeder bus stops.
     - Maximum bus stop distance (miles).
        * Determined by the value of the smallest buffer that covers at least 97% of the zone. The value is set to 999 for zones
          with a minimum distance of 999 or is capped at 1.1 if the minimum distance is <=1.1.
     - Proportion of zone within minimum walking distance.
        * The area of the zone covered by the minimum distance buffer divided by the area of the zone covered by the maximum
          distance buffer. This value is set to 999 for zones with a minimum distance of 999.

    [Emme vehicle types: 1=mode B, 2=mode E, 3=mode P, 4=mode Q, 5=mode L, 7=mode C, 8=mode M]



    Revisions:
   -----------------------
           04-24-2013: revised .dbf names

           08-06-2013: - corrected distance to Metra/CTA rail to be mean distance weighted by subzone households. 
                       - enhanced error-checking to verify spatial analyses created required input files.               

           08-07-2013: - includes park-n-ride distance to subzones in DISTR (Eash October 2012 change) - Emme 4 version. 

           04-16-2014: - modified park-n-ride cost calculation to syncronize with Eash methodology. 

           08-08-2014: - (Steve Chau) code corrected to exclude non-stops from analysis and accurately apply dwt to start of route. 
		   
		   02-19-2016: - the following changes were made to introduce additional impedance into the DISTR files:
						i. Distance cap of 19.95 miles between CTA rail stations/Metra stations/park-n-ride lots and subzone centroids removed.
						ii. Additional impedance added for zones>1712 (zones outside of CMAP 7 counties and which only contain one subzone).
    						Since these zones only contain one subzone, distances (and standard deviations) to CTA rail stations/Metra stations/park-n-ride lots
							are calculated from random points within the zone (taken from data\distr\zone_random_points.shp).  Extra impedance is added by 
							calculating Manhattan distance rather than Euclidean.
						iii. If the average zonal distance for CTA rail exceeds 19.95, turn off access (use 999 code).
                        
           12-11-2017: - removed household weight from Metra/CTA rail distance variance.
           
           03-07-2018: - extra code to ensure format for "zone" from ArcGIS 10.5 shapefiles (bhdwy_zn, fhdwy_zn, pkrd) is compatible with other datasets.
		   
		   Ferguson 10/2/2018: Updated zone system 09 references to zone system 17.
*/

*=====================================================================;
*       ###   DISTR VARIABLES       ###     ;
%let eash=0.042;                                     *** Eash's estimate of subzone variance;
%let rlvar=101;                                      *** default value for DISTR rail variance;
*=====================================================================;


/* ------------------------------------------------------ */
        *** INPUT FILES ***;
filename in1 "temp\node.txt";
filename in15 "temp\node_midday.txt";
filename in2 "temp\transit.itin";
filename in25 "temp\transit_midday.itin";
filename in3 "..\data\zcentroid_xcoord.txt";
filename in4 "..\data\zcentroid_ycoord.txt";

filename in5 "..\tg\sas\data\m01tg.txt";
filename in6 "..\tg\sas\data\m01auto.csv";
filename in7 "..\tg\sas\data\m01type.csv";
filename in8 "..\tg\fortran\HH_IN.txt";
filename error1 "saserr.txt";
%let pnr=temp\pkrd.dbf;                              *** Park-n-Ride data;  
%let zncntd=temp\tmp_zncntrd_Layer.dbf;              *** zone centroids;  
%let prloc=temp\tmp_pnr_Layer.dbf;                   *** Park-n-Ride locations;  
%let bwait=temp\bhdwy_zn.dbf;                        *** reg bus wait data;        
%let fwait=temp\fhdwy_zn.dbf;                        *** feeder bus wait data;     
%let metra=temp\sz17_1.dbf;                          *** Metra nearest station distance;
%let ctarail=temp\sz17_2.dbf;                        *** CTA Rail nearest station distance; 
%let prdist=temp\sz17_3.dbf;                         *** nearest Park-n-Ride distance; 
%let bacc=temp\tmp_bus_area.dbf;                     *** bus stop accessibility area;
%let fbacc=temp\tmp_feed_area.dbf;                   *** feeder bus stop accessibility area;
%let sz=..\data\distr\sz17_centroids.dbf;            *** subzone shapefile;
%let randpts=..\data\distr\zone_random_points.dbf;   *** zone random points shapefile;
%let subznZn=2977;                                   *** highest zone number containing multiple subzones;

*** Error Checking ***;
%let totalobs=0;
%let sznum=17418;                                    *** total subzones;
%let znnum=3632;                                     *** zone polygons;
/* ------------------------------------------------------ */

*=====================================================================;
*     MODULE 1: CREATE FILES FOR SPATIAL ANALYSIS          ;
*=====================================================================;

%macro setup;
  %if &sysparm=1 %then %do;
       *** -- READ IN RAIL STATION, BUS STOP & ITINERARY DATA -- ***;
        data stops; infile in1 firstobs=2 missover;
           input node pspac pcost xcoord ycoord; proc sort; by node; 

		data stops_mdy; infile in15 firstobs=2 missover;
           input node pspac pcost xcoord ycoord; proc sort; by node; 

        data itin; infile in2 firstobs=2 missover;
           input line $ anode node dwtime veh hdwy; 
            ord=_n_; proc sort; by line ord;

        data itin_mdy; infile in25 firstobs=2 missover;
           input line $ anode node dwtime veh hdwy; 
            ord=_n_; proc sort; by line ord;

       ** Get Stops **;
        data itin; set itin; by line;
           output;
           if first.line then do; node=anode; dwtime=0.01; output; end;    *** ensures start of route kept;
        data itin(where=(dwtime>0)); set itin; drop anode; proc sort; by node;

        data itin_mdy; set itin_mdy; by line;
           output;
           if first.line then do; node=anode; dwtime=0.01; output; end;    *** ensures start of route kept;
        data itin_mdy(where=(dwtime>0)); set itin_mdy; drop anode; proc sort; by node;

        data itin; merge itin (in=hit) stops; by node; if hit;

		data itin_mdy; merge itin_mdy (in=hit) stops_mdy; by node; if hit;
		
		data itin_mdy (drop = hdwy xcoord ycoord dwtime veh ord pspac pcost); set itin_mdy;
		hdwy_mdy = hdwy;
		xcoord_mdy = xcoord;
		ycoord_mdy = ycoord;
		dwtime_mdy = dwtime;
		veh_mdy = veh;
		ord_mdy = ord;
		pspac_mdy = pspac;
		pcost_mdy = pcost;

       *** ## FILE 1. CREATE METRA STATION FILE ## ***;
              **--- Only Metra rail junctions use 49000+ values ---**;
       data metra(keep=node xcoord ycoord); set itin(where=(veh=8 & node<49000));
         proc sort nodupkey; by node;
         proc export data=metra outfile="temp\metra.dbf" dbms=dbf replace;

       *** ## FILE 2. CREATE CTA RAIL STATION FILE ## ***;
              **--- Only CTA rail junctions use 39000+ values ---**;
        data ctarail(keep=node xcoord ycoord); set itin(where=(veh=7 & node<39000));
         proc sort nodupkey; by node;
         proc export data=ctarail outfile="temp\ctarail.dbf" dbms=dbf replace;

       *** ## FILE 3. CREATE REGULAR BUS STOP FILE ## ***;
        data bus; set itin(where=(veh<5)); frequent=120/hdwy;
         proc summary nway; class node; var hdwy; weight frequent; id xcoord ycoord; output out=regbus_am mean=;
        data regbus_am(drop=_type_ _freq_); set regbus_am; hdwy=round(hdwy);

        data bus_mdy; set itin_mdy(where=(veh_mdy<5)); frequent=120/hdwy_mdy;
         proc summary nway; class node; var hdwy_mdy; weight frequent; id xcoord_mdy ycoord_mdy; output out=regbus_mdy mean=;
        data regbus_mdy(drop=_type_ _freq_); set regbus_mdy; hdwy_mdy=round(hdwy_mdy);

		data regbus (drop=xcoord_mdy ycoord_mdy); merge regbus_am regbus_mdy; by node;
		if xcoord = '.' then xcoord=xcoord_mdy;
		if ycoord = '.' then ycoord=ycoord_mdy;
		if hdwy = '.' then hdwy= 888;
		if hdwy_mdy= '.' then hdwy_mdy = 888;
		proc export data=regbus outfile="temp\regbus.dbf" dbms=dbf replace;

       *** ## FILE 4. CREATE FEEDER BUS STOP FILE ## ***;
        data feed; set itin(where=(veh=5)); frequent=120/hdwy;
         proc summary nway; class node; var hdwy; weight frequent; id xcoord ycoord; output out=fdbus_am mean=;
        data fdbus_am(drop=_type_ _freq_); set fdbus_am; hdwy=round(hdwy);

        data feed_mdy; set itin_mdy(where=(veh_mdy=5)); frequent=120/hdwy_mdy;
         proc summary nway; class node; var hdwy_mdy; weight frequent; id xcoord_mdy ycoord_mdy; output out=fdbus_mdy mean=;
        data fdbus_mdy(drop=_type_ _freq_); set fdbus_mdy; hdwy_mdy=round(hdwy_mdy);

		data fdbus (drop=xcoord_mdy ycoord_mdy); merge fdbus_am fdbus_mdy; by node;
		if xcoord = '.' then xcoord=xcoord_mdy;
		if ycoord = '.' then ycoord=ycoord_mdy;
		if hdwy = '.' then hdwy= 888;
		if hdwy_mdy= '.' then hdwy_mdy = 888;
        proc export data=fdbus outfile="temp\feed.dbf" dbms=dbf replace;

       *** ## FILE 5. CREATE PARK-N-RIDE LOCATION FILE ## ***;
        data pnr; set stops(where=(pspac>0 or pcost>0));
         proc export data=pnr outfile="temp\pnr.dbf" dbms=dbf replace;


       *** -- READ IN ZONE CENTROID DATA -- ***;
        data z1(keep=zone xcoord); infile in3 missover dlm=' :';
           input @1 flag $1. @;
           select;
            when (flag in ('a','c','d','t')) delete;
            otherwise input @1 zone j $ xcoord;
           end;
          proc sort; by zone;
        data z2(keep=zone ycoord); infile in4 missover dlm=' :';
           input @1 flag $1. @;
           select;
            when (flag in ('a','c','d','t')) delete;
            otherwise input @1 j $ zone ycoord;
           end;
          proc sort; by zone;

       *** ## FILE 6. CREATE ZONE CENTROID FILE ## ***;
        data zn; merge z1 z2; by zone;
         proc export data=zn outfile="temp\zncntrd.dbf" dbms=dbf replace;

  %end;
%mend setup;
%setup
run;

*=====================================================================;
*     MODULE 2: ASSEMBLE DATA FOR FINAL DISTR AND M01 FILES          ;
*=====================================================================;

%macro assemble;
  %if &sysparm=2 %then %do;

   *** -------------------------------------------- ***;
   ***               M01 File Assembly              ***;
   *** -------------------------------------------- ***;

       *** -- ZONAL BUS WAIT STATISTICS -- ***;
        proc import datafile="&bwait" out=bwait dbms=dbf replace;     **** REGULAR BUS;
           data _null_; set bwait nobs=waitb; call symput('totalobs',left(put(waitb,8.))); run;
           %if %eval(&totalobs)=0 %then %do; data _null_; file error1; put "ERROR: &bwait has &totalobs observations."; %end; %let totalobs=0; run;

        data bwait_am(rename=(zone17=zone)); set bwait(where=(node>0 and hdwy>0 and hdwy<>888));      **keep only good hits;
          ***********hdwy=min(hdwy*0.5,30);      ***set hdwy to max. of 60 & divide by 2 to setup for wait time calc.;
           proc summary nway; class zone; var hdwy; output out=bus_am mean=hwbus; 

        data bwait_mdy(rename=(zone17=zone)); set bwait(where=(node>0 and hdwy_mdy>0 and hdwy_mdy<>888));      **keep only good hits;
		  ***********hdwy_mdy=min(hdwy_mdy*0.5,30);  ***set hdwy to max. of 60 & divide by 2 to setup for wait time calc.;
           proc summary nway; class zone; var hdwy_mdy; output out=bus_mdy mean=hwbus_mdy; 

		data bus(drop=zone); merge bus_am bus_mdy; by zone;
		  zn=put(zone,8.0);
		data bus(drop=zn); set bus; 
          zone=input(zn,8.);		
		  proc sort; by zone;

        proc import datafile="&fwait" out=fwait dbms=dbf replace;     **** FEEDER BUS;
           data _null_; set fwait nobs=waitf; call symput('totalobs',left(put(waitf,8.))); run;
           %if %eval(&totalobs)=0 %then %do; data _null_; file error1 mod; put "ERROR: &fwait has &totalobs observations."; %end; %let totalobs=0; run;

		data fwait_am(rename=(zone17=zone)); set fwait(where=(node>0 and hdwy>0 and hdwy<>888));      **keep only good hits;
          ***********hdwy=min(hdwy*0.5,30);      ***set hdwy to max. of 60 & divide by 2 to setup for wait time calc.;
           proc summary nway; class zone; var hdwy; output out=fbus_am mean=hwfeed; 

        data fwait_mdy(rename=(zone17=zone)); set fwait(where=(node>0 and hdwy_mdy>0 and hdwy_mdy<>888));      **keep only good hits;
		  ***********hdwy_mdy=min(hdwy_mdy*0.5,30);  ***set hdwy to max. of 60 & divide by 2 to setup for wait time calc.;
           proc summary nway; class zone; var hdwy_mdy; output out=fbus_mdy mean=hwfeed_mdy;

		data fbus(drop=zone); merge fbus_am fbus_mdy; by zone;
		  zn=put(zone,8.0);
		data fbus(drop=zn); set fbus; 
          zone=input(zn,8.);		
		  proc sort; by zone;

       *** -- PARK-N-RIDE COST & AVAILABILITY -- ***;
        proc import datafile="&pnr" out=park dbms=dbf replace;
           data _null_; set park nobs=pride; call symput('totalobs',left(put(pride,8.))); run;
           %if %eval(&totalobs)=0 %then %do; data _null_; file error1 mod; put "ERROR: &pnr has &totalobs observations."; %end; %let totalobs=0; run;
        data park(drop=objectid); set park; proc sort; by in_fid;

        proc import datafile="&zncntd" out=znctd dbms=dbf replace;
           data _null_; set znctd nobs=zcent; call symput('totalobs',left(put(zcent,8.))); run;
           %if %eval(&totalobs)=0 %then %do; data _null_; file error1 mod; put "ERROR: &zncntd has &totalobs observations."; %end; %let totalobs=0; run;

        data znctd(keep=zone in_fid); set znctd; id=_n_; in_fid=id-1; proc sort; by in_fid; 
        data park; merge park (in=hit) znctd; by in_fid; if hit; proc sort; by near_fid; 

        proc import datafile="&prloc" out=prloc dbms=dbf replace;
           data _null_; set prloc nobs=locpr; call symput('totalobs',left(put(locpr,8.))); run;
           %if %eval(&totalobs)=0 %then %do; data _null_; file error1 mod; put "ERROR: &prloc has &totalobs observations."; %end; %let totalobs=0; run;

        data prloc(keep=node pspac pcost near_fid); set prloc; 
           id=_n_; near_fid=id-1; proc sort; by near_fid;
        data park; merge park (in=hit) prloc; by near_fid; if hit; proc sort; by zone near_dist;

        data cst(keep=zn pcost pr); set park; by zone near_dist;
          if first.zone; *** parking cost determined by P-n-R lot closest to geographic centroid of zone;
          pr=1;   *** Park-n-Ride flag;
		  zn=put(zone,8.0);
		data cst(drop=zn); set cst;
          zone=input(zn,8.);		
		  proc sort; by zone;

  
       *** -- REMAINING DATA FROM TG DIRECTORY FILES -- ***;
        data tg; infile in5 missover; input zone income; proc sort; by zone;
           data _null_; set tg nobs=tg1; call symput('totalobs',left(put(tg1,8.))); run;
           %if %eval(&totalobs) ne %eval(&znnum) %then %do; data _null_; file error1 mod; put "ERROR: in5 has &totalobs observations (not &znnum)."; %end; %let totalobs=0; run;

        data auto(drop=j1-j2); infile in6 missover dsd firstobs=2; input zone j1 j2 autocc;
          autocc=round(autocc,0.01); proc sort; by zone;
           data _null_; set auto nobs=auto1; call symput('totalobs',left(put(auto1,8.))); run;
           %if %eval(&totalobs) ne %eval(&znnum) %then %do; data _null_; file error1 mod; put "ERROR: in6 has &totalobs observations (not &znnum)."; %end; %let totalobs=0; run;

        data type; infile in7 missover dsd firstobs=4; input zone type; proc sort; by zone;
           data _null_; set type nobs=type1; call symput('totalobs',left(put(type1,8.))); run;
           %if %eval(&totalobs) ne %eval(&znnum) %then %do; data _null_; file error1 mod; put "ERROR: in7 has &totalobs observations (not &znnum)."; %end; %let totalobs=0; run;

        data m01(drop=_type_ _freq_ hwbus_mdy hwfeed_mdy); merge tg auto type bus fbus cst; by zone;
          *** finalize bus wait times ***;
            *- HW wait: set value to 99 if missing & maximum value of 99 **;
           if hwbus=. then hwbus=99; hwbus=round(min(hwbus,99));
           if hwfeed=. then hwfeed=99; hwfeed=round(min(hwfeed,99)); 

            *- HW_miday wait: set value to 99 if missing & maximum value of 99 **;
           if hwbus_mdy=. then hwbus_mdy=99; hobus=round(min(hwbus_mdy,99));
           if hwfeed_mdy=. then hwfeed_mdy=99; hofeed=round(min(hwfeed_mdy,99));
          *** finalize park-n-ride data ***;
           cost=round(max(pcost,0));
           pr=max(pr,0);

   *** -------------------------------------------- ***;
   ***             DISTR File Assembly              ***;
   *** -------------------------------------------- ***;

       *** -- RAIL STATION & PARK-N-RIDE ACCESSIBILITY -- ***;
        proc import datafile="&metra" out=metra dbms=dbf replace;   **** METRA;
           data _null_; set metra nobs=metra1; call symput('totalobs',left(put(metra1,8.))); run;
           %if %eval(&totalobs) ne %eval(&sznum) %then %do; data _null_; file error1 mod; put "ERROR: &metra has &totalobs observations (not &sznum)."; %end; %let totalobs=0; run;

        data metra(keep=subzone17 mdist); set metra;
          format subzone17 8.; subzone17=round(subzone17);
           *****mdist=min(near_dist/5280,19.95);               *** convert feet to miles, cap at 19.95;
		   mdist=near_dist/5280;               *** convert feet to miles, cap removed (rev. 02-19-2016);
          proc sort; by subzone17; 

        proc import datafile="&ctarail" out=crail dbms=dbf replace; **** CTA RAIL;
           data _null_; set crail nobs=crail1; call symput('totalobs',left(put(crail1,8.))); run;
           %if %eval(&totalobs) ne %eval(&sznum) %then %do; data _null_; file error1 mod; put "ERROR: &ctarail has &totalobs observations (not &sznum)."; %end; %let totalobs=0; run;

        data crail(keep=subzone17 cdist); set crail;
          format subzone17 8.; subzone17=round(subzone17);
           *****cdist=min(near_dist/5280,19.95); 
           cdist=near_dist/5280;		      *** convert feet to miles, cap removed (rev. 02-19-2016);
          proc sort; by subzone17; 

        proc import datafile="&prdist" out=parkd dbms=dbf replace; **** Park-n-Ride;
           data _null_; set parkd nobs=prdist1; call symput('totalobs',left(put(prdist1,8.))); run;
           %if %eval(&totalobs) ne %eval(&sznum) %then %do; data _null_; file error1 mod; put "ERROR: &prdist has &totalobs observations (not &sznum)."; %end; %let totalobs=0; run;

        data parkd(keep=subzone17 pkdist); set parkd;
          format subzone17 8.; subzone17=round(subzone17);
           *****pkdist=min(near_dist/5280,19.95);   
          pkdist=min(near_dist/5280,19.95);	 *** convert feet to miles, cap removed (rev. 02-19-2016);	   
          proc sort; by subzone17; 

        proc import datafile="&sz" out=subzone dbms=dbf replace;
           data _null_; set subzone nobs=subzone1; call symput('totalobs',left(put(subzone1,8.))); run;
           %if %eval(&totalobs) ne %eval(&sznum) %then %do; data _null_; file error1 mod; put "ERROR: &sz has &totalobs observations (not &sznum)."; %end; %let totalobs=0; run;

        data subzone(keep=subzone17 zone); set subzone; format subzone17 8.; subzone17=round(subzone17); rename zone17=zone; proc sort; by subzone17;

        data hh(keep=subzone17 hh); infile in8 missover dsd;  *** subzone households;
          format subzone17 8.;
          input subzone17 hh;
             hh=max(hh,1);   *** no zero weights;
            proc sort; by subzone17;
           data _null_; set hh nobs=hh1; call symput('totalobs',left(put(hh1,8.))); run;
           %if %eval(&totalobs) ne %eval(&sznum) %then %do; data _null_; file error1 mod; put "ERROR: in8 has &totalobs observations (not &sznum)."; %end; %let totalobs=0; run;

        data rail; merge subzone metra crail parkd hh; by subzone17;  
           data _null_; set rail nobs=rail1; call symput('totalobs',left(put(rail1,8.))); run;
           %if %eval(&totalobs) ne %eval(&sznum) %then %do; data _null_; file error1 mod; put "ERROR: Merged Subzone file has &totalobs observations (not &sznum)."; %end; %let totalobs=0; run;

        proc summary nway data=rail;
            class zone;
            var mdist cdist pkdist;
            weight hh;
            output out=distr1dist mean(mdist)= mean(cdist)= mean(pkdist)=;
            run;
        proc summary nway data=rail;    *** variance not weighted by hh (NRF 12-11-2017);
            class zone;
            var mdist cdist pkdist;
            output out=distr1var var(mdist)=mvar var(cdist)=cvar var(pkdist)=pvar;
            run;
		data distr1;
            merge distr1dist distr1var;
            by zone;
            run;
		   
    *** ========================================================================================= ***;
         *** -- ADD MORE RAIL & P-n-R IMPEDANCE FROM ZONES OUTSIDE OF CMAP (USE MANHATTAN DISTANCE, NOT EUCLIDEAN) -- ***;		
		 
		   *** -- Get Nearest Metra Station -- ***;	
		proc import datafile="&metra" out=metrasta dbms=dbf replace;   
        data metrasta; set metrasta(where=(zone17>&subznZn)); rename zone17=zone; keep zone17 near_fid; proc sort; by near_fid;   ** just identify nearest Metra station to zone;
		proc import datafile="temp\metra.dbf" out=metraloc dbms=dbf replace;   		
		data metraloc; set metraloc; near_fid=_n_-1; rename xcoord=mxcoord ycoord=mycoord; proc sort; by near_fid;
		data metraloc(drop=node); merge metrasta(in=hit) metraloc; by near_fid; if hit; proc sort nodupkey; by zone near_fid;	

		  *** -- Get Nearest CTA Rail Station -- ***;	
		proc import datafile="&ctarail" out=ctarailsta dbms=dbf replace;   
        data ctarailsta; set ctarailsta(where=(zone17>&subznZn)); rename zone17=zone; keep zone17 near_fid; proc sort; by near_fid;
		proc import datafile="temp\ctarail.dbf" out=ctarailloc dbms=dbf replace;   		
		data ctarailloc; set ctarailloc; near_fid=_n_-1; rename xcoord=cxcoord ycoord=cycoord; proc sort; by near_fid; 
		data ctarailloc(drop=node); merge ctarailsta(in=hit) ctarailloc; by near_fid; if hit; proc sort nodupkey; by zone near_fid;  

		   *** -- Get Nearest Park and Ride Location -- ***;	
		proc import datafile="&prdist" out=pnrsta dbms=dbf replace;   
        data pnrsta; set pnrsta(where=(zone17>&subznZn)); rename zone17=zone; keep zone17 near_fid; proc sort; by near_fid; 
		proc import datafile="temp\pnr.dbf" out=pnrloc dbms=dbf replace;   		
		data pnrloc; set pnrloc; near_fid=_n_-1; rename xcoord=pxcoord ycoord=pycoord; proc sort; by near_fid;
		data pnrloc(drop=node); merge pnrsta(in=hit) pnrloc; by near_fid; if hit; proc sort nodupkey; by zone near_fid; 	

        data comb(drop=near_fid pspac pcost); merge metraloc ctarailloc pnrloc; by zone;

		  *** -- Get Random Points for Each Zone -- ***;		
		proc import datafile="&randpts" out=randpt dbms=dbf replace;		  
		data randpt; set randpt(where=(zone17>&subznZn)); rename zone17=zone; proc sort; by zone;
		data randpt; merge randpt comb(in=hit); by zone; if hit; 
		  *** -- Calculate Manhattan Distances (No Caps) -- ***;			
		  mdist=(abs(point_x-mxcoord) + abs(point_y-mycoord))/5280;	
          cdist=(abs(point_x-cxcoord) + abs(point_y-cycoord))/5280; 
          pkdist=(abs(point_x-pxcoord) + abs(point_y-pycoord))/5280; 
		  		
		proc summary nway data=randpt; class zone; var mdist cdist pkdist;
           output out=distr2 mean(mdist)= var(mdist)=mvar mean(cdist)= var(cdist)=cvar mean(pkdist)= var(pkdist)=pvar;     
    *** ========================================================================================= ***;
	
        data distr1(drop=_type_ _freq_); merge distr1 distr2; by zone;
          format mdist mstdv cdist cstdv pkdist pstdv 5.2;
		  
		  if zone<=&subznZn then do;
		    if mvar=. then mvar=&eash; else mvar=mvar+&eash;
			if cvar=. then cvar=&eash; else cvar=cvar+&eash;
			if pvar=. then pvar=&eash; else pvar=pvar+&eash;
		  end; 
          mstdv=sqrt(mvar); mvar=&rlvar;
          cstdv=sqrt(cvar); cvar=&rlvar;
          pstdv=sqrt(pvar); pvar=&rlvar; 
		  if cdist>19.95 then do;
		    cdist=999; cvar=999; cstdv=999;   *** turn off CTA rail access (rev. 02-19-2016);
		  end; 		  
		  
       *** -- BUS STOP ACCESSIBILITY -- ***;
        proc import datafile="&bacc" out=bus dbms=dbf replace;
           data _null_; set bus nobs=bus1; call symput('totalobs',left(put(bus1,8.))); run;
           %if %eval(&totalobs)=0 %then %do; data _null_; file error1 mod; put "ERROR: &bacc has &totalobs observations."; %end; %let totalobs=0; run;

        data bus(rename=(zone17=zone)); set bus;
               mindist=distance; minarea=f_area/5280**2;     **convert square feet to sqmi;
            *** since intersect areas overlap, must add all final zone pieces together ***;
              proc summary nway; class zone mindist; var minarea; id sqmi; output out=busarea sum=;
           
        proc import datafile="&fbacc" out=feed dbms=dbf replace;
           data _null_; set feed nobs=feed1; call symput('totalobs',left(put(feed1,8.))); run;
           %if %eval(&totalobs)=0 %then %do; data _null_; file error1 mod; put "ERROR: &fbacc has &totalobs observations."; %end; %let totalobs=0; run;

        data feed(rename=(zone17=zone)); set feed;
               fmindist=distance; fminarea=f_area/5280**2;     **convert square feet to sqmi;
              proc summary nway; class zone fmindist; var fminarea; id sqmi; output out=feedarea sum=;
  

         data minbus firstmax maxbus; set busarea; by zone mindist;
           if first.zone then output minbus;                   **smallest buffer to touch zone;
           else if minarea/sqmi>0.97 then output firstmax;     **smallest buffer to cover entire zone;
           else output maxbus;                                 **largest buffer that does not cover zone;
         data firstmax; set firstmax; by zone mindist; if first.zone;
           rename mindist=maxdist minarea=maxarea;
         data maxbus; set maxbus; by zone mindist; if last.zone;
           rename mindist=maxdist minarea=maxarea;
         data busmax; update maxbus firstmax; by zone;         **ensures firstmax overwrites maxbus;
         data bus(drop=_type_ _freq_); merge minbus busmax; by zone;  

          
         data minfeed firstmax maxfeed; set feedarea; by zone fmindist;
           if first.zone then output minfeed;                   
           else if fminarea/sqmi>0.97 then output firstmax;     
           else output maxfeed;                                
         data firstmax; set firstmax; by zone fmindist; if first.zone;
           rename fmindist=fmaxdist fminarea=fmaxarea;
         data maxfeed; set maxfeed; by zone fmindist; if last.zone;
           rename fmindist=fmaxdist fminarea=fmaxarea;
         data feedmax; update maxfeed firstmax; by zone;         
         data feed(drop=_type_ _freq_); merge minfeed feedmax; by zone;  


         data distr; merge distr1 bus feed; by zone;
          *** finalize bus accessibility ***;
           if mindist=. then do; mindist=999; maxdist=999; p3=999; end;
           else do;
              if maxdist=. then do; maxdist=1.1; maxarea=sqmi; end;
              p3=min(round(minarea/maxarea,.001),1);
           end;
         *** finalize bus accessibility ***;
           if fmindist=. then do; fmindist=999; fmaxdist=999; fp3=999; end;
           else do;
              if fmaxdist=. then do; fmaxdist=1.1; fmaxarea=sqmi; end;
              fp3=min(round(fminarea/fmaxarea,.001),1);
           end;

         run;

        %hwm01(MCHW_M01)
        %hwm01(PDHW_M01)
        %hom01(MCHO_M01)
        %hom01(PDHO_M01)
        %hom01(MCNH_M01)
        %hom01(PDNH_M01)
        %hwdistr(MCHW_DISTR)
        %hwdistr(PDHW_DISTR)
        %hodistr(MCHO_DISTR)
        %hodistr(PDHO_DISTR)
        %hodistr(MCNH_DISTR)
        %hodistr(PDNH_DISTR)
         run;

  %end;
%mend assemble;


*=====================================================================;
   *** -------------------------------------------- ***;
   ***           Macros to Write Files              ***;
   *** -------------------------------------------- ***;
%macro hwm01(fl);
    *** Write Out HW Files ***; 
     data print; set m01;
      file "..\&fl..TXT" dsd;
       put zone type cost income pr hwbus hobus hwfeed hofeed autocc;
%mend hwm01;

%macro hom01(fl);
    *** Write Out HO/NH Files ***; 
     data print; set m01;
      if hwbus=99 then hwbus=999; if hobus=99 then hobus=999;
      if hwfeed=99 then hwfeed=999; if hofeed=99 then hofeed=999;
      file "..\&fl..TXT" dsd;
       put zone type cost income pr hwbus hobus hwfeed hofeed;
%mend hom01;

%macro hwdistr(fl);
    *** Write Out HW Files ***; 
     data print; set distr;
      file "..\&fl..TXT" dsd;
       put zone mdist mstdv mvar cdist cstdv cvar mindist maxdist p3 fmindist fmaxdist fp3 pkdist pstdv pvar;
%mend hwdistr;

%macro hodistr(fl);
    *** Write Out HO/NH Files ***; 
     data print; set distr;
      fmindist=999; fmaxdist=999; fp3=999;
      file "..\&fl..TXT" dsd;
       put zone mdist mstdv mvar cdist cstdv cvar mindist maxdist p3 fmindist fmaxdist fp3 pkdist pstdv pvar;
%mend hodistr;

%assemble
*=====================================================================;

run;
