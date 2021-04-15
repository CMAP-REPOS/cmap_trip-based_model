* SUMMARY_TG_RESULTS kww 6/07, 4/09;
* REVISED mas 02/2010;
* IMPORTS TG RESULTS, CREATES SAS LIBRARY, TABULATE SUMMARIES.;

* Heither, revised 05-25-2016: no longer include trip type 34 (home-school
    Ps-As for children 12-15) in P-A files as most of these motorized trips are
	school bus.;
* Ferguson, revised 06-24-2016: changed households input from HH_IN.txt to
    POPSYN_HH.csv and adjusted 'i' variables.;
* Ferguson, revised 01-03-2018: added zonal median household income to summary
    from POPSYN_HH.csv HH income field;
* Ferguson 8/20/2018: Changed subzone09 and zone09 variable names to subzone17
    and zone17;
* Ferguson 9/10/2018: Removed hardcoded zone definition for region=1. Reads
    CMAP flag from subzones17 now included in GEOG_IN.TXT.;


options linesize=132 pagesize=52 /*nocenter nodate nonumber*/ ; 

%let project = tg_c19q3;
%let run = 100_20190701;
%let num_trip_types=49;
*SET THIS VARIABLE equal to 1 if you want to include extra child 12-15 trip types when 
 using the 49 trip type output (set to 0 otherwise);
%let kid_trips=1;
*TO SEPARATE HI AND LO INC PROD AND ATTR, SET THIS EQUAL TO 1 (ZERO OTHERWISE);
%let hilo=1;


*DATASETS;

filename geog_in "..\..\fortran\GEOG_IN.txt";
filename popsynhh "..\..\fortran\POPSYN_HH.csv";
filename gq_in "..\..\fortran\GQ_IN.txt";
filename attr_in "..\..\fortran\ATTR_IN.txt";
filename hh_in "..\..\fortran\HH_IN.txt";   **** NEW!!!;

filename ino1 "..\..\fortran\TRIP&num_trip_types._PA_OUT.TXT";

libname dd1 "..\data";

*TG MODEL INPUT DATA;
data i0; infile geog_in DLM=',' LRECL=400;
 input  subzone17 fips cnty_name $ state $ puma1 puma5 zone17 chicago cbd row_column area cmap;run;

data popsyn; infile popsynhh DLM=',' LRECL=400;
 input  subzone17 hh_type vehicles pums puma5 row_col adults workers children income_index age_index hh_vtype hh_income;
 label  subzone17="subzone"
 		hh_type="household type"
		vehicles="available vehicles in household"
		pums="household PUMS serial number"
		puma5="PUMA5"
		row_col="row-column"
		adults="adults in household"
		workers="workers in household"
		children="children in household"
        income_index="household income quartile index"
        age_index="age of householder index"
        hh_vtype="revised vehicle availability household type"
        hh_income="household income";
		proc sort; by subzone17;
		run;
data inc(keep=subzone17 hh_income zone17);
    merge popsyn(in=hit) i0;
    by subzone17;
    if hit;
    run;
proc summary data=inc nway;
    var hh_income;
    class zone17;
    output out=zmedinc median=zmedinc;
    run;
data zmedinc(keep=zone17 zmedinc);
    set zmedinc;
	label zmedinc="zonal median household income";
	run;
    
proc sql;
create table i1 as
select subzone17,count(hh_type) as households,avg(vehicles) as avg_vehicles,avg(adults) as avg_adults,avg(workers) as avg_workers,avg(children) as avg_children,
       avg(income_index) as avg_income_index,avg(age_index) as avg_age_index
from popsyn
group by subzone17;
quit;

data i1; set i1;
label subzone17="subzone"
      households="households"
      avg_vehicles="available vehicles per household"
      avg_adults="adults per household"
      avg_workers="workers per household"
      avg_children="children per household"
      avg_income_index="income quartile index"
      avg_age_index="age of householder index";

data i2; infile gq_in DLM=',' LRECL=400;
 input subzone17 gq_mil gq_univ gq_16to64 gq_65plus;
 label  subzone17="subzone"
 		gq_mil="military barracks pop"
		gq_univ="college dorm pop"
		gq_16to64="other gq pop age 16 to 64"
		gq_65plus="other gq pop age 65 plus";

data i3; infile attr_in DLM=',' LRECL=400;  
 input subzone17 retail_emp tot_emp hi_earn_share;
 label  subzone17="subzone"
 		retail_emp="retail employment"
		tot_emp="total employment"
		hi_earn_share="hi income worker share";


data i4(drop=j1-j6); infile hh_in DLM=',' LRECL=400;
 input  subzone17 j1-j6 wrkautoms pef;
 label  subzone17="subzone"
		wrkautoms="work auto mode share"
		pef="pedestrian environment factor"; 

 
*TG MODEL OUTPUT DATA;

data tgout; infile ino1;
 INPUT subzone17 1-6 zone17 7-12 trip_type 13-14 hh_prods 15-23 hh_attrs 24-32;
run;

* MACRO to create 1 table for each trip type from the final TG model trip table;
%MACRO m1 (notypes=); *notypes variable is to change the number of trip types (choices are 11 or 49);
	%DO i=1 %TO &notypes;
		data o&i; set tgout; if trip_type=&i; rename hh_prods=op&i hh_attrs=oa&i; drop trip_type; run;
	%END;
%MEND m1;

* MACRO merges all trip types tables into 1 table that has 1 observation per subzone, as well as 
  columns for all productions and attractions for each trip type;
%MACRO m2 (notypes=); *notypes variable to change the number of trip types (choices are 11 or 49);
	%DO i=2 %TO &notypes;
		data o1; merge o1 o&i; by subzone17; run;
		proc datasets library=work; delete o&i; run;
	%END;
%MEND m2;

* CALL MACROS M1 and M2 (these simply reformat the final trip table coming out of the tg model).
  BE SURE TO THAT the global macro variable 'num_trip_types' is correctly set to 11 or 49 to 
  reflect the number of trip types generated by the tg model;
%m1(notypes=&num_trip_types);
%m2(notypes=&num_trip_types);


*LABEL ALL 98 TRIP TYPE VARIABLES TO SOMETHING MEANINGFUL;
*EXPORT THIS TABLE OR SAVE IT AS A SAS DATASET IF IT IS NECESSARY TO KEEP IT AROUND;
data o2; set o1;
IF &num_trip_types=49 THEN
	DO;
		    *CHANGE PA TO OD FORMAT;
		 op8=(op8+oa8)/2;
		 oa8=(op8+oa8)/2;
		 op9=(op9+oa9)/2;
		 oa9=(op9+oa9)/2;
		 op10=(op10+oa10)/2;
		 oa10=(op10+oa10)/2;
		 op38=(op38+oa38)/2;
		 oa38=(op38+oa38)/2;
		 op39=(op39+oa39)/2;
		 oa39=(op39+oa39)/2;
		 op40=(op40+oa40)/2;
		 oa40=(op40+oa40)/2;
	        *LABEL ALL VARIABLES;
	 	 label subzone17="subzone"
		 op1	=	"worker home-based work low-income productions"
		 oa1	=	"worker home-based work low-income attractions"
		 op2	=	"worker home-based work hi-income productions"
		 oa2	=	"worker home-based work hi-income attractions"
		 op3	=	"worker home-based work-related productions"
		 oa3	=	"worker home-based work-related attractions"
		 op4	=	"worker home-based school productions"
		 oa4	=	"worker home-based school attractions"
		 op5	=	"worker home-based nonhome/work at residence productions"
		 oa5	=	"worker home-based nonhome/work at residence attractions"
		 op6	=	"worker home-based nonhome/work not at residence productions"
		 oa6	=	"worker home-based nonhome/work not at residence attractions"
		 op7	=	"worker home-based shop productions"
		 oa7	=	"worker home-based shop attractions"
		 op8	=	"worker work-based nonhome/work at residence productions"
		 oa8	=	"worker work-based nonhome/work at residence attractions"
		 op9	=	"worker work-based nonhome/work not at residence productions"
		 oa9	=	"worker work-based nonhome/work not at residence attractions"
		 op10	=	"worker work-based shop productions"
		 oa10	=	"worker work-based shop attractions"
		 op11	=	"worker work-based work productions"
		 oa11	=	"worker work-based work attractions"
		 op12	=	"worker nonhome/work at residence nonhome/work at residence productions"
		 oa12	=	"worker nonhome/work at residence nonhome/work at residence attractions"
		 op13	=	"worker nonhome/work at residence nonhome/work not at residence productions"
		 oa13	=	"worker nonhome/work at residence nonhome/work not at residence attractions"
		 op14	=	"worker nonhome/work at residence shop productions"
		 oa14	=	"worker nonhome/work at residence shop attractions"
		 op15	=	"worker nonhome/work not at residence nonhome/work at residence productions"
		 oa15	=	"worker nonhome/work not at residence nonhome/work at residence attractions"
		 op16	=	"worker nonhome/work not at residence nonhome/work not at residence productions"
		 oa16	=	"worker nonhome/work not at residence nonhome/work not at residence attractions"
		 op17	=	"worker nonhome/work not at residence shop productions"
		 oa17	=	"worker nonhome/work not at residence shop attractions"
		 op18	=	"worker shop nonhome/work at residence productions"
		 oa18	=	"worker shop nonhome/work at residence attractions"
		 op19	=	"worker shop nonhome/work not at residence productions"
		 oa19	=	"worker shop nonhome/work not at residence attractions"
		 op20	=	"worker shop shop productions"
		 oa20	=	"worker shop shop attractions"
		 op21	=	"non-working adult home-based school productions"
		 oa21	=	"non-working adult home-based school attractions"
		 op22	=	"non-working adult home-based nonhome at residence productions"
		 oa22	=	"non-working adult home-based nonhome at residence attractions"
		 op23	=	"non-working adult home-based nonhome not at residence productions"
		 oa23	=	"non-working adult home-based nonhome not at residence attractions"
		 op24	=	"non-working adult home-based shop productions"
		 oa24	=	"non-working adult home-based shop attractions"
		 op25	=	"non-working adult nonhome at residence nonhome at residence productions"
		 oa25	=	"non-working adult nonhome at residence nonhome at residence attractions"
		 op26	=	"non-working adult nonhome at residence nonhome not at residence productions"
		 oa26	=	"non-working adult nonhome at residence nonhome not at residence attractions"
		 op27	=	"non-working adult nonhome at residence shop productions"
		 oa27	=	"non-working adult nonhome at residence shop attractions"
		 op28	=	"non-working adult nonhome not at residence nonhome at residence productions"
		 oa28	=	"non-working adult nonhome not at residence nonhome at residence attractions"
		 op29	=	"non-working adult nonhome not at residence nonhome not at residence productions"
		 oa29	=	"non-working adult nonhome not at residence nonhome not at residence attractions"
		 op30	=	"non-working adult nonhome not at residence shop productions"
		 oa30	=	"non-working adult nonhome not at residence shop attractions"
		 op31	=	"non-working adult shop nonhome at residence productions"
		 oa31	=	"non-working adult shop nonhome at residence attractions"
		 op32	=	"non-working adult shop nonhome not at residence productions"
		 oa32	=	"non-working adult shop nonhome not at residence attractions"
		 op33	=	"non-working adult shop shop productions"
		 oa33	=	"non-working adult shop shop attractions"
		 op34	=	"child home-based school productions"
		 oa34	=	"child home-based school attractions"
		 op35	=	"child home-based nonhome at residence productions"
		 oa35	=	"child home-based nonhome at residence attractions"
		 op36	=	"child home-based nonhome not at residence productions"
		 oa36	=	"child home-based nonhome not at residence attractions"
		 op37	=	"child home-based shop productions"
		 oa37	=	"child home-based shop attractions"
		 op38	=	"child school nonhome at residence productions"
		 oa38	=	"child school nonhome at residence attractions"
		 op39	=	"child school nonhome not at residence productions"
		 oa39	=	"child school nonhome not at residence attractions"
		 op40	=	"child school shop productions"
		 oa40	=	"child school shop attractions"
		 op41	=	"child nonhome at residence nonhome at residence productions"
		 oa41	=	"child nonhome at residence nonhome at residence attractions"
		 op42	=	"child nonhome at residence nonhome not at residence productions"
		 oa42	=	"child nonhome at residence nonhome not at residence attractions"
		 op43	=	"child nonhome at residence shop productions"
		 oa43	=	"child nonhome at residence shop attractions"
		 op44	=	"child nonhome not at residence nonhome at residence productions"
		 oa44	=	"child nonhome not at residence nonhome at residence attractions"
		 op45	=	"child nonhome not at residence nonhome not at residence productions"
		 oa45	=	"child nonhome not at residence nonhome not at residence attractions"
		 op46	=	"child nonhome not at residence shop productions"
		 oa46	=	"child nonhome not at residence shop attractions"
		 op47	=	"child shop nonhome at residence productions"
		 oa47	=	"child shop nonhome at residence attractions"
		 op48	=	"child shop nonhome not at residence productions"
		 oa48	=	"child shop nonhome not at residence attractions"
		 op49	=	"child shop shop productions"
		 oa49	=	"child shop shop attractions" ;
		
		*INCLUDE EXTRA CHILD 12-15 TRIPS OR NOT;
		IF &kid_trips=1 THEN
			DO;
		 	 	nhp = op8 + op9 + op10 + op11 + op12 + op13 + op14 + op15 + op16 + 
					op17 + op18 + op19 + op20 + op25 + op26 + op27 + op28 + op29 + op30 + 
					op31 + op32 + op33 + op38 + op39 + op40 + op41 + op42 + op43 + op44 + 
					op45 + op46 + op47 + op48 + op49;
			 	nha = oa8 + oa9 + oa10 + oa11 + oa12 + oa13 + oa14 + oa15 + oa16 + 
					oa17 + oa18 + oa19 + oa20 + oa25 + oa26 + oa27 + oa28 + oa29 + oa30 + 
					oa31 + oa32 + oa33 + oa38 + oa39 + oa40 + oa41 + oa42 + oa43 + oa44 + 
					oa45 + oa46 + oa47 + oa48 + oa49;
			END;
		ELSE
			DO;
				nhp = op8 + op9 + op10 + op11 + op12 + op13 + op14 + op15 + op16 + 
					op17 + op18 + op19 + op20 + op25 + op26 + op27 + op28 + op29 + op30 + 
					op31 + op32 + op33 /*+ op38 + op39 + op40 + op41 + op42 + op43 + op44 + 
					op45 + op46 + op47 + op48 + op49*/;
		 	 	nha = oa8 + oa9 + oa10 + oa11 + oa12 + oa13 + oa14 + oa15 + oa16 + 
					oa17 + oa18 + oa19 + oa20 + oa25 + oa26 + oa27 + oa28 + oa29 + oa30 + 
					oa31 + oa32 + oa33 /*+ oa38 + oa39 + oa40 + oa41 + oa42 + oa43 + oa44 + 
					oa45 + oa46 + oa47 + oa48 + oa49*/;
			END;
	 	*COLLAPSE;
	 	IF &hilo=1 THEN 
	 		DO;
				hwplo = op1+((op3+op4+op21)*(op1/(op1+op2))); 	 
				hwphi = op2+((op3+op4+op21)*(op2/(op1+op2)));
		 		hwalo = oa1+((oa3+oa4+oa21)*(oa1/(oa1+oa2)));
				hwahi = oa2+((oa3+oa4+oa21)*(oa2/(oa1+oa2)));
				*hwpoth= op3 + op4;
				*hwaoth= oa3 + oa4;
				***** -- exclude trip type 34 (home-school Ps-As for children 12-15) -- *****;
	 			**hop = op5 + op6 + op7 + op22 + op23 + op24 + op34 + op35 + op36 + op37;
			 	**hoa = oa5 + oa6 + oa7 + oa22 + oa23 + oa24 + oa34 + oa35 + oa36 + oa37;
				hop = op5 + op6 + op7 + op22 + op23 + op24 + op35 + op36 + op37;
			 	hoa = oa5 + oa6 + oa7 + oa22 + oa23 + oa24 + oa35 + oa36 + oa37;
				
				*LABEL COLLAPSED VARIABLES;
	 			label 
				hwplo = "sum of home-based work low income productions"
				hwphi = "sum of home-based work high income productions"
				hwalo = "sum of home-based work low income attractions"
				hwahi = "sum of home-based work high income attractions"
				/*hwpoth = "sum of home-based work misc productions"
				hwaoth = "sum of home-based work misc attractions"*/
				hop = "sum of home-based other productions"
				hoa = "sum of home-based other attractions"
				nhp = "sum of non-home based productions"
				nha = "sum of non-home based attractions" ;
				IF hwplo=. then hwplo=0;
				IF hwalo=. then hwalo=0;
				IF hwphi=. then hwphi=0;
				IF hwahi=. then hwahi=0;
			END;
	 	ELSE
			DO;
		 		hwp = op1 + op2 + op3 + op4 + op21; 	 
		 		hwa = oa1 + oa2 + oa3 + oa4 + oa21;
				***** -- exclude trip type 34 (home-school Ps-As for children 12-15) -- *****;
	 			**hop = op5 + op6 + op7 + op22 + op23 + op24 + op34 + op35 + op36 + op37;
			 	**hoa = oa5 + oa6 + oa7 + oa22 + oa23 + oa24 + oa34 + oa35 + oa36 + oa37;
			 	hop = op5 + op6 + op7 + op22 + op23 + op24 + op35 + op36 + op37;
			 	hoa = oa5 + oa6 + oa7 + oa22 + oa23 + oa24 + oa35 + oa36 + oa37; 
	
				*LABEL COLLAPSED VARIABLES;
				label 
				hwp = "sum of home-based work productions"
				hwa = "sum of home-based work attractions"
				hop = "sum of home-based other productions"
				hoa = "sum of home-based other attractions"
				nhp = "sum of non-home based productions"
				nha = "sum of non-home based attractions" ;
			END;
	 
	END;

*IF 11 TRIP TYPES;
ELSE
	DO;
		*CHANGE PA TO OD FORMAT;
		op4=(op4+oa4)/2;
		oa4=(op4+oa4)/2;
		op5=(op5+oa5)/2;
		oa5=(op5+oa5)/2;

	 	*COLLAPSE;
		hwp = op1;
		hwa = oa1;
		hop = op2 + op3 + op8 + op9 + op11;
		hoa = oa2 + oa3 + oa8 + oa9 + oa11;
		nhp = op4 + op5 + op6 + op7 + op10;
		nha = oa4 + oa5 + oa6 + oa7 + oa10;
	
		*LABEL COLLAPSED VARIABLES;
	 	label 
		hwp = "sum of home-based work productions"
		hwa = "sum of home-based work attractions"
		hop = "sum of home-based other productions"
		hoa = "sum of home-based other attractions"
		nhp = "sum of non-home based productions"
		nha = "sum of non-home based attractions" ;
	END;
run;

* Macro to delete extra fields after p's an a's have been collapsed to more general trip types;
%MACRO m3 (notypes=); *notypes variable to change the number of trip types (choices are 11 or 49);
	data o3; set o2;
	%DO i=1 %TO &notypes;
		drop op&i oa&i; 
	%END;
	run;
%MEND m3;

%m3(notypes=&num_trip_types);

*MERGE;

data a; merge i0 i1 i2 i3 o3 i4; by subzone17;
proc sort data=a;
    by zone17;
    run;
data a;
    merge a(in=hit) zmedinc;
    by zone17;
	run;
proc sort data=a;
    by subzone17;
	run;

*FORMAT;

proc format;
 value fips  
	17007= "Illinois Boone" 
	17031= "Illinois Cook"
	17037= "Illinois DeKalb" 
	17043= "Illinois DuPage" 
	17063= "Illinois Grundy" 
	17089= "Illinois Kane"
	17091= "Illinois Kankakee" 
	17093= "Illinois Kendall" 
	17097= "Illinois Lake" 
	17111= "Illinois McHenry" 
	17197= "Illinois Will" 
    17201= "Illinois Winnebago"
    17141= "Illinois Ogle"
    17099= "Illinois LaSalle" 
    17103= "Illinois Lee"     
	18089= "Indiana Lake" 
	18127= "Indiana Porter" 
    18091= "Indiana LaPorte" 
	55059= "Wisconsin Kenosha"
    55101= "Wisconsin Racine" 
	55127= "Wisconsin Walworth" ;
 value chicago
	1= "yes"
	0= "no" ;
 value cbd
	1= "yes"
	0= "no" ;
 value cmap
    1="yes"
    0="no";

*SAVE;
 
data dd1.tg&run; set a; run;

*SUMMARIZE;



*INPUTS;

data a1; set a;
workers = (max(avg_workers,0)*max(households,0)) + (max(gq_mil,0)*1.00) + (max(gq_univ,0)*0.88) + (max(gq_16to64,0)*0.50) + (max(gq_65plus,0)*0.18);
adults = (max(avg_adults,0)*max(households,0)); children = (max(avg_children,0)*max(households,0));
population = adults + children + gq_mil + gq_univ + gq_16to64 + gq_65plus;

proc summary nway; class cmap fips chicago cbd; var households workers retail_emp tot_emp adults children population;
output out=dd1.summary_tg_results_socec sum=;

proc summary nway; class cmap fips chicago cbd; var households workers retail_emp tot_emp adults children population;
output out=print sum=;

proc sort; by cmap;
proc print label noobs; by cmap;
 format cmap cmap. fips fips. chicago chicago. cbd cbd.;
 format households workers retail_emp tot_emp population adults children comma10.0;
 var cmap fips chicago cbd households workers retail_emp tot_emp population adults children;
 sum households workers retail_emp tot_emp population adults children;
 sumby cmap; 
 title "trip generation inputs *** &project - &run";
 run;



*OUTPUTS;

data a; set a; 
	IF &num_trip_types=49 and &hilo=1 THEN
		DO;
			hwp=sum(hwplo,hwphi);*+ hwpoth;
			hwa=sum(hwalo,hwahi);*+ hwaoth;
		END;
run;	

proc summary nway; class cmap fips chicago cbd; var hwp hwa hop hoa nhp nha ;
output out=dd1.summary_tg_results_tg sum=;

proc summary nway; class cmap fips chicago cbd; var hwp hwa hop hoa nhp nha ;
output out=print sum=;

proc sort; by cmap;
proc print label noobs; by cmap;
 format cmap cmap. fips fips. chicago chicago. cbd cbd.;
 format hwp hwa hop hoa nhp nha  comma10.0;
 var cmap fips chicago cbd hwp hwa hop hoa nhp nha ;
 sum hwp hwa hop hoa nhp nha ;
 sumby cmap;
 title "trip generation outputs *** &project - &run";
 run;
