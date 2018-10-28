libname comp "Q:\Data-ReadOnly\COMP";
libname crsp "Q:\Data-ReadOnly\CRSP";

%let vars=GVKEY CUSIP FYEAR DLC DLTT SCF ;
%let filtervars= indfmt datafmt popsrc fic consol datadate;
%let vars_dsf= date CUSIP PRC SHROUT RET PRC;
%let plotvars= DD1 DD2 DD3 PD1 PD2 PD3;
%let PDVars= Pd1 PD2 PD3;
%let DDVars= Dd1 DD2 DD3;
%macro addfinvars(input_data,output_data);
proc sql;
	create table &output_data. as
	select a.year, a.PD1, A.PD2,A.Pd3,a.DD1,a.DD2,a.DD3,
		b.B as USREC, 
		c.B as CFSI, 
		d.B as BAAFFM from (select * from &input_data. where _TYPE_=1)  a 
			left join  USREC b on a.year=b.data_year
			left join  CFSI c on a.year=c.data_year
			left join BAAFFM d on  a.year=d.data_year;
quit;

%mend;
%macro plot_macro(input_data,plotvar,Index,titletext);


proc gplot data=&input_data.;
		symbol i=spline v=dot h=0.75;
		axis2 label=(angle=90 "Y Variable Value") minor=(n=1);
		plot (&plotvar. )*year /Overlay legend vaxis=axis2 ;
		plot2 &Index.*year /Overlay legend;
		TITLE "&titletext.";
run ;

%mend;

%macro USREC_corr(varlist,dataname,analysis_var);

proc means  data = &dataname. n mean p25 p50 p75 std min max;
		class USREC;
		var &varlist.;
/*		output out = &cat.&analysis_var;*/
	run;
/*Corr materix for all data req for analysis */
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
   		var &varlist.;
	run;
%mend;

%macro all_analysis(dataname,analysis_var);
/*UsRec analysis*/
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


data compustatdata ;
	set comp.funda ( keep= &vars. &filtervars. where=((indfmt='INDL') and (datafmt='STD') and (popsrc='D') and (fic='USA') and (consol='C') and (year(datadate)>=1970) and (year(datadate)<=2015)));
	CUSIP=substr(CUSIP,1,8);
	DLC=DLC*1000000;
	dltt=dltt*1000000;
	year=year(datadate);
	new_year=year+1;
run;

data compdata_w_var;
	set compustatdata ;
	F= DLC+0.5*DLTT;
run;


data dsf ;
	set crsp.dsf (keep= &vars_dsf WHERE=((year(date)>=1970) and (year(date)<=2015)));	
	abs_price=abs(PRC);
	year=year(date);
	shrout=shrout*1000;
	CUSIP=substr(CUSIP,1,8);
run;  

data dsf ;
	set dsf;
	E=abs_price*shrout;
run;  

proc sql;
create table dsf_annual as
	select cusip, year, exp(sum(log(1+ret)))-1 As annret, 
	std(ret)*SQRT(250)as sigmae 
	from dsf
	group by cusip,year;
quit;

data dsf_annual;
	set dsf_annual;
	new_year=year+1;
run;
proc sort data=compdata_w_var;
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

proc sql;
	create table merged_annual_data as
		select a.cusip, a.new_year as year, annret, sigmae,b.E as E ,c.F as F 
			from dsf_annual a 
			join  (select cusip,year, E from dsf_1st_rec ) b on a.cusip=b.cusip and a.new_year=b.year
			join compdata_w_var c on a.new_year=c.new_year and a.cusip=c.cusip;/*Need to update fyear to year*/
quit;


data Riskanalysis_51_merged_data;
	set merged_annual_data;
	
	if (F ne .) and not(F=0) and E+F ne . and not(E+F=0) and annret ne .;
	do; 
		Naive1  =  ((E/(E+F))*sigmae) + ((F/(E+F))*(0.05+0.25*sigmae)); 
		Naive2 =  ((E/(E+F))*sigmae) + ((F/(E+F))*(0.05+0.5*sigmae)); 
		Naive3 =  ((E/(E+F))*sigmae) + ((F/(E+F))*(0.25*sigmae));
		DD1 = (Log((E+F)/F)+(ANNRET-(Naive1*Naive1)/2))/Naive1; 
		PD1 = CDF('NORMAL',-DD1); 
		DD2 = (Log((E+F)/F)+(ANNRET-(Naive2*Naive2)/2))/Naive2; 
		PD2 = CDF('NORMAL',-DD2); 
		DD3 = (Log((E+F)/F)+(ANNRET-(Naive3*Naive3)/2))/Naive3; 
		PD3 = CDF('NORMAL',-DD3); 
	end;
run; 


/* import other fin dtaasets NOte : Annual data downloaded from website*/
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

proc sort data=Riskanalysis_51_merged_data; by year;run;

ods GRAPHICS ON;
ods pdf file="P:\SAS_assignment\assign5\Assign5_1_RM.pdf";

/*print means data */
proc means data =Riskanalysis_51_merged_data  n mean p25 p50 p75 std min max ;
		class year;
		var PD1 PD2 PD3 DD1 DD2 DD3 ;
		OUTPUT OUT = data_p25 P25=; 
 	 	OUTPUT OUT = data_p50 P50=; 
 	 	OUTPUT OUT = data_p75 P75=; 
 	 	OUTPUT OUT = data_mean MEAN=; 
		title 'Mean analysis for PD & DD vars across time';
run;

/*print corr matrix */
proc corr data=Riskanalysis_51_merged_data;
	Title 'Correlation Matrix of variablles';
   	var PD1 Pd2 Pd3 DD1 DD2 DD3;
run;

/*add fin data*/
%addfinvars(data_p25, data_p25_fin);
%addfinvars(data_p50, data_p50_fin);
%addfinvars(data_p75, data_p75_fin);

/*fin var analysis*/
%all_analysis(data_p25_fin,p25);
%all_analysis(data_p50_fin,p50);
%all_analysis(data_p75_fin,p75);

ods pdf close;
ods GRAPHICS OFF;









