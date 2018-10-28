/*Print the SAS log to file. Update location of log to a location with access */
PROC PRINTTO Log="Q:\Users\rmaheshwari8\assign_52\new_log5_3.txt"; run;
/*initialize libnames and macro variables*/
/*temporary output library: User can update this to a location with access*/
libname tempout1 "Q:\Scratch\Rachit";

libname comp "Q:\Data-ReadOnly\COMP";
libname crsp "Q:\Data-ReadOnly\CRSP";
libname FEDData "Q:\Users\rmaheshwari8\assign_52";
%let vars=GVKEY CUSIP FYEAR DLC DLTT SCF ;
%let filtervars= indfmt datafmt popsrc fic consol datadate;
%let vars_dsf= date CUSIP PRC SHROUT RET PRC;
%let plotvars= PD_method3 PD1_method1 PD2_method1 PD3_method1 DD_method3 DD1_method1 DD2_method1 DD3_method1;
%let PDVars= PD_method3 PD1_method1 PD2_method1 PD3_method1;
%let DDVars= DD_method3 DD1_method1 DD2_method1 DD3_method1;


/*macro to calculate sigmaV for each cusip for a particular year*/
/*sort data alldata_with_r by year before calling this macro for faster processing*/
%macro Year_iterator(var_year);

	data tempout1.alldata_subset;
		set tempout1.alldata_with_r (WHERE=(year=&var_year.));
		by cusip;
		retain t;
		if (F ne .) and (intrate ne .) and (E ne .) and not(F = 0) and not(E=0) and  not (intrate=0);
		V=E+F;
	run;

	/*	sort data by cusip to then initialize t*/
	proc sort data= tempout1.alldata_subset; by cusip; run;

	/*time to maturity will vary for each day of the year. if maturity date is end of year*/
	data tempout1.alldata_subset;
		set tempout1.alldata_subset;
		if (first.cusip) then  t=1.0;
		else  t= t-(1.0/250.0);
		if t<0 then t=0;
	run;

/*	initial sigmaV_table with sigmaV_updated initialized with sigmae*/
	proc sql; 
		create table tempout1.sigmaV_table as
			select distinct cusip,this_sigmae as sigmaV_updated from tempout1.alldata_subset
			order by cusip;
	quit;


	%do i = 1 %to 8;
/*		update sigmaV*/
		proc sql;
		create table tempout1.temp_all as
		select a.*,b.sigmaV_updated as sigmaV from
		tempout1.alldata_subset a join tempout1.sigmaV_table b on a.cusip=b.cusip;
		quit;
		run;
/*		proc model to calculate daily  V*/
		PROC MODEL DATA = tempout1.temp_all Newton CONVERGE=1E-3 maxiter=32000 noprint ;
			DEPENDENT V ;
			INDEPENDENT  intrate F  E sigmaV  t;
			E=(V*probnorm(  (LOG(V/F) + (intrate + ((sigmaV * sigmaV)/2))*t)/(sigmaV*sqrt(t))) )- (exp(-intrate*t)*F*probnorm(  (LOG(V/F) + (intrate -((sigmaV * sigmaV)/2))*t)/(sigmaV*sqrt(t)))) ;
			SOLVE V / OUT=tempout1.predicted_data  ;
			BY CUSIP date ;
		RUN;

/*calculate V_ret*/
		proc  sort data=tempout1.predicted_data ; by cusip date ; run; 

		data tempout1.temp;
			set tempout1.predicted_data;
			by cusip ;
			lag_v=lag(V);
			if not (first.cusip) then V_ret=log(V/lag_V);
			else V_ret=0;
		run;
		 
		data tempout1.temp;
			set tempout1.temp;
			if V_ret ne .;
		run;
/*calculate sigma V*/
		proc sql;
			create table tempout1.sigmaV_table as
			select  CUsip, std(V_ret)*SQRT(250)as sigmaV_updated from tempout1.temp 
			group by cusip;
		quit;

	%end;


	data tempout1.sigmaV_yr_table;
		set tempout1.sigmaV_table ;
		year =&var_year. *1;
		next_year=(&var_year.+1)*1;
	run;
/*append sigmaV to a ayyearly dataset*/
	proc append base=tempout1.all_sigmaV data=tempout1.sigmaV_yr_table; run;
%mend;

/*macro to loop through all years*/

%macro all_iterator();
%do yr=1970 %to 2014;
	%Year_iterator(&yr.);
%end;

%mend;



/*macro to add fin var . this macro uses a dataset from output statement of a proc means statement*/
%macro addfinvars(input_data,output_data);
proc sql;
	create table &output_data. as
	select a.year,a.PD_method3, a.PD1_method1, a.PD2_method1, a.PD3_method1, a.DD_method3, a.DD1_method1, a.DD2_method1, a.DD3_method1,
		b.USREC, 
		c.CFSI, 
		d.BAAFFM from (select * from &input_data. where _TYPE_=1)  a 
			left join  tempout1.USREC b on a.year=b.data_year
			left join  tempout1.CFSI c on a.year=c.data_year
			left join tempout1.BAAFFM d on  a.year=d.data_year;
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
/*	if (CUSIP='26865010' or CUSIP='36720410');*/
run;
/*create F variable in compustat dtata*/
data compustatdata;
	set compustatdata ;
	F= DLC+0.5*DLTT;
	drop DLC DLTT indfmt popsrc fic consol;
run;
proc sort data=compustatdata; by cusip date;run;

/*Read dsf data and initialize vars*/
data dsf ;
	set crsp.dsf (keep= &vars_dsf WHERE=((year(date)>=1970) and (year(date)<=2015)));	
	if (shrout ne .) and not(shrout=0) and (PRC ne .) and not(PRC =0) and (ret ne .);
	abs_price=abs(PRC);
	year=year(date);
	shrout=shrout*1000;
	CUSIP=substr(CUSIP,1,8);
run;  

proc sort data=dsf; by cusip date;run;

/*Create E var and drop variables not required further in analysis*/
data dsf ;
	set dsf;
	E=abs_price*shrout;
	drop PRC abs_price shrout;
run;  


/*Calculate annret*/
proc sql;
create table dsf_annual as
	select cusip, year+1 as next_year,year, exp(sum(log(1+ret)))-1 As annret, 
	std(ret)*SQRT(250)as sigmae 
	from (select * from dsf where ret>-1) a /* select only valid returns */
	group by cusip,year;
quit;

/*all infro from dsf c& compustat. This is current yeaars sigmae and F since this is useful to calculate sigmaV. We will calculate */
proc sql;
create table tempout1.merged_annual_data as
select a.*,c.F, d.sigmae as this_sigmae  from 
tempout1.dsf a join 
tempout1.compustatdata c on a.year=c.year and a.cusip=c.cusip join
tempout1.dsf_annual d  on a.year=d.year and a.cusip=d.cusip ;
quit;

proc sort data=tempout1.merged_annual_data; by cusip date;run;
/*add r*/

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
/*proc sort nodupkey data=DAILYFED ; by data_year;run;*/


proc sql;
	create table tempout1.alldata_with_r as
		select a.*, log(1+(b.DTB3/100)) as intrate from 
			tempout1.merged_annual_data a join 
			tempout1.DAILYFED b on a.date=b.A;

quit;

proc  sort data=tempout1.alldata_with_r ; by year; run; 


/*deleting all_sigmaV if already present since we will be using proc append*/
proc datasets lib=tempout1;
   delete all_sigmaV;
run;


/*calling all iterator to find sigmaV for each year*/
%all_iterator();

/*one record*/
proc  sort data=tempout1.alldata_with_r ; by cusip date; run; 


data tempout1.alldata_1st_rec;/*keep only 1st record for E*/
	set tempout1.alldata_with_r  ;
	by cusip year;
	if (first.year);
	keep cusip year date E   intrate;
run;

/* Lag sigmae, F, annret appropriately */
proc sql;
	create table tempout1.alldata_1st_rec as
	select a.*, b.sigmaV_updated as sigmaV,d.sigmae as prev_sigmae,c.F as prev_F, d.annret as prev_annret from 
	tempout1.alldata_1st_rec a join 
	(select * from tempout1.all_sigmaV where sigmaV_updated>0) b on a.cusip=b.cusip and a.year=b.next_year join
	tempout1.compustatdata c  on a.year=c.new_year and a.cusip=c.cusip join
	tempout1.dsf_annual d  on a.year=d.next_year and a.cusip=d.cusip ;

	quit;
run;


/*initialize V for proc model*/
data tempout1.alldata_1st_rec;
	set tempout1.alldata_1st_rec;
	if ((prev_F ne .) and (intrate ne .) and (sigmaV ne .) and (E ne .) and not(prev_F = 0) and not(E=0) and not(sigmaV =0) and not(intrate=0));
	V=E+prev_F;
run;



proc sort data=tempout1.alldata_1st_rec; by cusip year; run;


/*Predict V  using proc model*/

PROC MODEL noprint DATA = tempout1.alldata_1st_rec Seidel CONVERGE=1E-3 maxiter=32000 noprint ;
	endogenous V ;
	exogenous  intrate prev_F  E sigmaV  ;
	E = V*probnorm((LOG(V/prev_F) + (intrate + ((sigmaV * sigmaV)/2))/sigmaV)) - exp(-intrate)*prev_F*probnorm((LOG(V/prev_F) + (intrate- ((sigmaV * sigmaV)/2))/sigmaV));
		SOLVE V / OUT=tempout1.predicted_all_1st_rec ;
	BY CUSIP year ;
RUN;



/*Dropping initialized V , sigmaV to replace them with the predicted values*/
data tempout1.alldata_1st_rec;
	set tempout1.alldata_1st_rec;
	drop V ;
run;
/*Adding predicted values*/

proc sql;
create table tempout1.alldata_with_pred as
select a.*, b.V  from 
	tempout1.alldata_1st_rec a join 
	tempout1.predicted_all_1st_rec b on a.cusip=b.cusip and a.year=b.year;
quit;
run;


/*method3 & method 1*/
data tempout1.Riskanalysis_52_merged_data;
	set tempout1.alldata_with_pred;
	if (prev_F ne .) and not(prev_F=0) and E+prev_F ne . and not(E+prev_F=0) and (prev_annret ne .) and (V ne .) and not(V=0) and (sigmaV ne .) and not (sigmav=0);
	DD_method3 = (Log(V/prev_F)+(prev_annret-(sigmaV*sigmaV)/2))/sigmaV; 
	PD_method3 = CDF('NORMAL',-DD_method3);
	Naive1 = ((E/(E+prev_F))*prev_sigmae) + ((prev_F/(E+prev_F))*(0.05+0.25*prev_sigmae)); 
	Naive2 = ((E/(E+prev_F))*prev_sigmae) + ((prev_F/(E+prev_F))*(0.05+0.5*prev_sigmae)); 
	Naive3 = ((E/(E+prev_F))*prev_sigmae) + ((prev_F/(E+prev_F))*(0.25*prev_sigmae));
	DD1_method1 = (Log((E+prev_F)/prev_F)+(prev_annret-(Naive1*Naive1)/2))/Naive1; 
	PD1_method1 = CDF('NORMAL',-DD1_method1); 
	DD2_method1 = (Log((E+prev_F)/prev_F)+(prev_annret-(Naive2*Naive2)/2))/Naive2; 
	PD2_method1 = CDF('NORMAL',-DD2_method1); 
	DD3_method1 = (Log((E+prev_F)/prev_F)+(prev_annret-(Naive3*Naive3)/2))/Naive3; 
	PD3_method1 = CDF('NORMAL',-DD3_method1); 	 	
run; 


/* import other fin datasets Note : Annual data downloaded from website*/
/*USREC*/
proc import out=tempout1.USREC 
	datafile="Q:/Users/rmaheshwari8/assign_32/USREC.xls" 
	dbms=xls replace; 
	getnames=no;
	datarow=128;
run;
data tempout1.USREC;
	set tempout1.USREC;
	Label B= 'USREC';
	USREC=B;
	data_year=year(A);
run;
/*CFSI*/
proc import out=tempout1.CFSI 
	datafile="Q:/Users/rmaheshwari8/assign_32/CFSI.xls" 
	dbms=xls replace; 
	getnames=no;
	datarow=12;
run;
data tempout1.CFSI;
	set tempout1.CFSI;
	Label B= 'CFSI';
	CFSI=B;
	data_year=year(A);
run;

/*BAAFM*/
proc import out=tempout1.BAAFFM datafile="Q:/Users/rmaheshwari8/assign_32/BAAFFM.xls" dbms=xls replace; getnames=no;datarow=12;
run;
data tempout1.BAAFFM;
	set tempout1.BAAFFM;
	Label B= 'BAAFFM';
	BAAFFM=B;
	data_year=year(A);
run;
/*SOrt data by year*/
proc sort data=tempout1.Riskanalysis_52_merged_data; by year;run;


/*add fin var*/
proc sql ;
create table tempout1.Riskanalysis_52_merged_data_fin as
select a.*, b.USREC, c.CFSI, d.BAAFFM 
from tempout1.Riskanalysis_52_merged_data a join
	tempout1.USREC b on a.year=b.data_year join
	tempout1.CFSI c on a.year=c.data_year join
	tempout1.BAAFFM d on a.year=d.data_year;
quit;


/*start printing in pdf*/
ods GRAPHICS ON;
ods pdf file="P:\SAS_assignment\assign5\Assign_5_3_results.pdf";

/*print corr matrix*/
proc corr data=tempout1.Riskanalysis_52_merged_data_fin;
	Title 'Correlation Matrix of PD & DD';
   	var &plotvars. ;
run;

proc means  data = tempout1.Riskanalysis_52_merged_data_fin n mean p25 p50 p75 std min max;
		class USREC;
		var &plotvars.;
		title " Mean analysis by USREC";
run;

/*print means data */
proc means data =tempout1.Riskanalysis_52_merged_data  n mean p25 p50 p75 std min max ;
		class year;
		var &plotvars. ;
		OUTPUT OUT = tempout1.data_p25 P25=; 
 	 	OUTPUT OUT = tempout1.data_p50 P50=; 
 	 	OUTPUT OUT = tempout1.data_p75 P75=; 
 	 	OUTPUT OUT = tempout1.data_mean MEAN=; 
		title 'Mean analysis for PD & DD across time';
run;

/*add fin data*/
%addfinvars(tempout1.data_mean, tempout1.data_mean_fin);
%addfinvars(tempout1.data_p25, tempout1.data_p25_fin);
%addfinvars(tempout1.data_p50, tempout1.data_p50_fin);
%addfinvars(tempout1.data_p75, tempout1.data_p75_fin);

/*Var analysis in PDF*/
%all_analysis(tempout1.data_mean_fin,mean);
%all_analysis(tempout1.data_p25_fin,p25);
%all_analysis(tempout1.data_p50_fin,p50);
%all_analysis(tempout1.data_p75_fin,p75);

ods pdf close;
ods GRAPHICS OFF;









