/*Print the SAS log to file */
PROC PRINTTO Log="Q:\Users\rmaheshwari8\assign_52\log5_2.txt"; run;
/*initialize libnames and macro variables*/

libname comp "Q:\Data-ReadOnly\COMP";
libname crsp "Q:\Data-ReadOnly\CRSP";
libname FEDData "Q:\Users\rmaheshwari8\assign_52";
%let vars=GVKEY CUSIP FYEAR DLC DLTT SCF ;
%let filtervars= indfmt datafmt popsrc fic consol datadate;
%let vars_dsf= date CUSIP PRC SHROUT RET PRC;
%let plotvars= PD_method2 PD1_method1 PD2_method1 PD3_method1 DD_method2 DD1_method1 DD2_method1 DD3_method1;
%let PDVars= PD_method2 PD1_method1 PD2_method1 PD3_method1;
%let DDVars= DD_method2 DD1_method1 DD2_method1 DD3_method1;

/*macro to add fin var . this macro uses a dataset from output statement of a proc means statement*/
%macro addfinvars(input_data,output_data);
proc sql;
	create table &output_data. as
	select a.year,a.PD_method2, a.PD1_method1, a.PD2_method1, a.PD3_method1, a.DD_method2, a.DD1_method1, a.DD2_method1, a.DD3_method1,
		b.B as USREC, 
		c.B as CFSI, 
		d.B as BAAFFM from (select * from &input_data. where _TYPE_=1)  a 
			left join  USREC b on a.year=b.data_year
			left join  CFSI c on a.year=c.data_year
			left join BAAFFM d on  a.year=d.data_year;
quit;
%mend;
/*macro to plot graphs: plot vars & index values*/
%macro plot_macro(input_data,plotvar,Index,titletext);


proc gplot data=&input_data.;
		symbol i=spline v=dot h=0.75;
		axis2 label=(angle=90 "Y Variable Value") minor=(n=1);
		plot (&plotvar. )*year /Overlay legend vaxis=axis2 ;
		plot2 &Index.*year /Overlay legend;
		TITLE &titletext.;
run ;

%mend;

/*Macro to perform analysis for recession and no recession */
%macro USREC_corr(varlist,dataname,analysis_var);
/*Corr materix for all data req for analysis */	data &dataname._0 &dataname._1;
	data &dataname._0 &dataname._1;
	set &dataname.;
		if USREC=0 then output &dataname._0;
		if USREC=1 then output &dataname._1;
	run;
	 
	proc corr data=&dataname._0;
	Title "Correlation Matrix for  &analysis_var.  in  no recession (USREC=0)";
   		var &varlist.;
	run;

	proc corr data=&dataname._1;
	Title "Correlation Matrix for &analysis_var. in Recession (USREC=1)";
   		var &varlist. ;
	run;
%mend;

/*macro which calls all other macros*/
%macro all_analysis(dataname,analysis_var);
/*Corr matrix */
proc corr data=&dataname.;
	Title "Correlation Matrix for  &analysis_var.  overall";
   		var &plotvars. ;
run;
/*USREC analysis*/
%USREC_corr(&plotvars.,&dataname., &analysis_var.);
%plot_macro(&dataname.,&PDVars.,USREC,"PD vars  and USREC plot for &analysis_var." );
%plot_macro(&dataname.,&DDVars.,USREC,"DD vars  and USREC plot for &analysis_var." );

/*BAAFFM analysis*/
%plot_macro(&dataname.,&PDVars.,BAAFFM,"PD vars  and BAAFFM plot for &analysis_var." );
%plot_macro(&dataname.,&DDVars.,BAAFFM,"DD vars  and BAAFFM plot for &analysis_var." );

/*CFSI analysis*/
%plot_macro(&dataname.,&PDVars.,CFSI,"PD vars  and CFSI plot for  &analysis_var." );
%plot_macro(&dataname.,&DDVars.,CFSI,"DD vars  and CFSI plot for   &analysis_var." );

%mend;

/*Read compustatdata*/
data compustatdata ;
	set comp.funda ( keep= &vars. &filtervars. where=((indfmt='INDL') and (datafmt='STD') and (popsrc='D') and (fic='USA') and (consol='C') and (year(datadate)>=1970) and (year(datadate)<=2015)));
	CUSIP=substr(CUSIP,1,8);
	DLC=DLC*1000000;
	dltt=dltt*1000000;
	year=year(datadate);
	new_year=year+1;
	if (DLC ne .) and (DLTT ne .);
run;
/*create F variable in compustat dtata*/
data compustatdata;
	set compustatdata ;
	F= DLC+0.5*DLTT;
	drop DLC DLTT indfmt popsrc fic consol;
run;

/*Read dsf data and initialize vars*/
data dsf ;
	set crsp.dsf (keep= &vars_dsf WHERE=((year(date)>=1970) and (year(date)<=2015)));	
	if (shrout ne .) and not(shrout=0) and (PRC ne .) and not(PRC =0) and (ret ne .);
	abs_price=abs(PRC);
	year=year(date);
	shrout=shrout*1000;
	CUSIP=substr(CUSIP,1,8);
run;  
/*Create E var and drop variables not required further in analysis*/
data dsf ;
	set dsf;
	E=abs_price*shrout;
	drop PRC abs_price shrout;
run;  

/*Calculate annret*/
proc sql;
create table dsf_annual as
	select cusip, year, exp(sum(log(1+ret)))-1 As annret, 
	std(ret)*SQRT(250)as sigmae 
	from (select * from dsf where ret>-1) a /* select only valid returns */
	group by cusip,year;
quit;
/*create a year lag var*/
data dsf_annual;
	set dsf_annual;
	new_year=year+1;
run;
proc sort data=compustatdata;
	by cusip year; 
run;
proc sort data=dsf_annual;
	by cusip year; 
run;
proc sort data=dsf;
	by cusip date;
run;

data dsf_1st_rec;/*keep only 1st record for E*/
	set dsf (Keep=cusip year date E) ;
	by cusip year;
	if (first.year);
run;
/*combine relevant F, E and other data*/
proc sql;
	create table merged_annual_data as
		select a.cusip, a.new_year as year, annret, sigmae,b.E as E ,c.F as F 
			from dsf_annual a 
			join  (select cusip,year, E from dsf_1st_rec ) b on a.cusip=b.cusip and a.new_year=b.year
			join compustatdata c on a.new_year=c.new_year and a.cusip=c.cusip;
quit;
/*delete unnecessary datsets*/
proc datasets ;
   delete dsf compustatdata;
run;


/*import DailyFed rate*/
proc import out=FEDData.DAILYFED datafile="Q://Users//rmaheshwari8//assign_52//DTB3.xls" dbms=xls replace; 
	getnames=no;
	datarow=12;
run;
/* removing NA data of intrate which gets imputed to 0*/ 
data FEDData.DAILYFED;
	set FEDData.DAILYFED;
	if (B ne .) and not(B =0); 
run;

data DailyFED;
	set FEDData.DAILYFED;
	DTB3=B*1.0;
	Label DTB3= 'DTB3';
	data_year=year(A);
run;


/*keeping first valid data for intrate*/
proc sort nodupkey data=DAILYFED ; by data_year;run;


proc sql;
	create table alldata_with_r as
		select a.*, log(1+(b.DTB3/100)) as intrate,b.DTB3 from 
			merged_annual_data a join 
			DAILYFED b on a.year=b.data_year;
quit;

data alldata_with_r;
	set alldata_with_r;
	if (F ne .) and (intrate ne .) and (sigmae ne .) and (E ne .) and not(F = 0) and not(E=0) and not(sigmaE =0) and not(intrate=0);
	V=E+F;
	sigmaV=((E/(E+F))*sigmae) + ((F/(E+F))*(0.05+0.25*sigmae));

run;

/*proc sort data=Riskanalysis_51_merged_data; by year;run;*/

proc sort data=alldata_with_r; by cusip year; run;


/*Predict V & sigmaV using proc model*/

PROC MODEL DATA = alldata_with_r  NOPRINT;
	ENDOGENOUS V sigmaV;
	EXOGENOUS  intrate F SIGMAE E;
	E = V*probnorm((LOG(V/F) + (intrate + ((sigmaV * sigmaV)/2))/sigmaV)) - exp(-intrate)*F*probnorm((LOG(V/F) + (intrate- ((sigmaV * sigmaV)/2))/sigmaV));
	SIGMAE = (V*sigmaV/E) * PROBNORM((LOG(V/F) + (intrate+ ((V * V)/2))/V));
	SOLVE V SIGMAV / OUT=predicted_data SEIDEL MAXITER = 32000 MAXSUBIT = 1000 CONVERGE=1E-4;
	BY CUSIP YEAR;
RUN;

/*Dropping initialized V , sigmaV to replace them with the predicted values*/
data alldata_with_r;
set alldata_with_r;
drop V sigmaV;
run;
/*Adding predicted values*/

proc sql;
create table alldata_with_pred as
select a.*, b.V , b.sigmaV from 
	alldata_with_r a join 
	predicted_data b on a.cusip=b.cusip and a.year=b.year;
run;



data Riskanalysis_52_merged_data;
	set alldata_with_pred;
	if (F ne .) and not(F=0) and E+F ne . and not(E+F=0) and (annret ne .) and (V ne .) and not(V=0) and (sigmaV ne .) and not (sigmav=0);
	DD_method2 = (Log(V/F)+(ANNRET-(sigmaV*sigmaV)/2))/sigmaV; 
	PD_method2 = CDF('NORMAL',-DD_method2);
	Naive1 = ((E/(E+F))*sigmae) + ((F/(E+F))*(0.05+0.25*sigmae)); 
	Naive2 = ((E/(E+F))*sigmae) + ((F/(E+F))*(0.05+0.5*sigmae)); 
	Naive3 = ((E/(E+F))*sigmae) + ((F/(E+F))*(0.25*sigmae));
	DD1_method1 = (Log((E+F)/F)+(ANNRET-(Naive1*Naive1)/2))/Naive1; 
	PD1_method1 = CDF('NORMAL',-DD1_method1); 
	DD2_method1 = (Log((E+F)/F)+(ANNRET-(Naive2*Naive2)/2))/Naive2; 
	PD2_method1 = CDF('NORMAL',-DD2_method1); 
	DD3_method1 = (Log((E+F)/F)+(ANNRET-(Naive3*Naive3)/2))/Naive3; 
	PD3_method1 = CDF('NORMAL',-DD3_method1); 	 	
run; 


/* import other fin datasets Note : Annual data downloaded from website*/
/*USREC*/
proc import out=USREC 
	datafile="Q:/Users/rmaheshwari8/assign_32/USREC.xls" 
	dbms=xls replace; 
	getnames=no;
	datarow=128;
run;
data USREC;
	set USREC;
	Label B= 'USREC';
	data_year=year(A);
run;
/*CFSI*/
proc import out=CFSI 
	datafile="Q:/Users/rmaheshwari8/assign_32/CFSI.xls" 
	dbms=xls replace; 
	getnames=no;
	datarow=12;
run;
data CFSI;
	set CFSI;
	Label B= 'CFSI';
	data_year=year(A);
run;

/*BAAFM*/
proc import out=BAAFFM datafile="Q:/Users/rmaheshwari8/assign_32/BAAFFM.xls" dbms=xls replace; getnames=no;datarow=12;
run;
data BAAFFM;
	set BAAFFM;
	Label B= 'BAAFFM';
	data_year=year(A);
run;
/*SOrt data by year*/
proc sort data=Riskanalysis_52_merged_data; by year;run;


/*add fin var*/
proc sql ;
create table Riskanalysis_52_merged_data_fin as
select a.*, b.B as USREC, c.B as CFSI, d.B as BAAFFM 
from Riskanalysis_52_merged_data a join
	USREC b on a.year=b.data_year join
	CFSI c on a.year=c.data_year join
	BAAFFM d on a.year=d.data_year;
quit;


/*start printing in pdf*/
ods GRAPHICS ON;
ods pdf file="P:\SAS_assignment\assign5\Assign_5_2_results.pdf";

/*print corr matrix*/
proc corr data=Riskanalysis_52_merged_data_fin;
	Title 'Correlation Matrix of PD & DD';
   	var &plotvars.;
run;

proc means  data = Riskanalysis_52_merged_data_fin n mean p25 p50 p75 std min max;
		class USREC;
		var &plotvars.;
		title " Mean analysis by USREC";
run;

/*print means data */
proc means data =Riskanalysis_52_merged_data  n mean p25 p50 p75 std min max ;
		class year;
		var &plotvars. ;
		OUTPUT OUT = data_p25 P25=; 
 	 	OUTPUT OUT = data_p50 P50=; 
 	 	OUTPUT OUT = data_p75 P75=; 
 	 	OUTPUT OUT = data_mean MEAN=; 
		title 'Mean analysis for PD & DD across time';
run;

/*add fin data*/
%addfinvars(data_mean, data_mean_fin);
%addfinvars(data_p25, data_p25_fin);
%addfinvars(data_p50, data_p50_fin);
%addfinvars(data_p75, data_p75_fin);

/*Var analysis in PDF*/
%all_analysis(data_mean_fin,mean);
%all_analysis(data_p25_fin,p25);
%all_analysis(data_p50_fin,p50);
%all_analysis(data_p75_fin,p75);

ods pdf close;
ods GRAPHICS OFF;









