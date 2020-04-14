/*******************************************************************************
Macro name:  Parallel Analysis macro

Written by:    Robert I. Kabacoff, Ph.D., Management Research Group®, Portland, ME

Creation date:  
As of date:     

Change:        Change layout and convert camel case to all lower case

Purpose:       A macro to conduct parallel analysis 
Summary:       

Reference:  Horn, J. L. (1965). A rationale and test for the number of factors
			in factor analysis. Psychometrika, 30, 179-185.

			Glorfeld, L. W. (1995). An improvement on Horn’s parallel
			analysis methodology for selecting the correct number of factors
			to retain. Educational and Psychological Measurement, 55, 377-
			393.
           
Format:  For example:
         %Polyserial(dat=,out=,uin=uin,formid=form_id,state=state,byform=N,bystate=N);

Required
Parameters: 1. dat - input data; 
 -
************************************************-*********************************/

%macro parallel(data=_LAST_, var=_NUMERIC_, niter=1000, statistic=Median);

	/*--------------------------------------*
	| Macro Parallel |
	| Parameters |
	| data = dataset to be analyzed |
	| (default: _LAST_) |
	| var = variables to be analyzed |
	| (default: _NUMERIC_) |
	| niter= number of simulated datasets |
	| to create (default: 1000) |
	| statistic = statistic used to |
	| summarized eigenvalues |
	| (default: Median. Other |
	| possible values: P90, |
	| P95, P99) |
	| Output |
	| Graph of actual vs. simulated |
	| eigenvalues |
	*--------------------------------------*/
	
	%macro dummy();%mend dummy;

	/* Read-in data */
	data _temp;
		set &data.;
		keep &var.;
	run;

	/* obtain number of observations and variables in dataset */
	ods output attributes=params;
	ods listing close;
	proc contents data=_temp;run;

	ods listing;
	data _null_;
		set params;
		if label2 = 'Observations' then
		call symput('nobs',trim(left(nvalue2)));
		else if label2 = 'Variables' then
		call symput('nvar',trim(left(nvalue2)));
	run;

	/* obtain eigenvalues for actual data */
	proc factor data=_temp nfact=&nvar. noprint
		outstat = e1(where=(_type_ = 'EIGENVAL'));
		var &var.;
	run;

	data e1;
		set e1;
		array a1{&nvar.} &var.;
		array a2{&nvar.} x1-x&nvar.;
		do j = 1 to &nvar.;
			a2{j} = a1{j};
		end;

		keep x1-x&nvar.;
	run;

	/* --- simulate datasets and obtain eigenvalues accordingly ----*/
	%do k = 1 %to &niter.;
		data raw;
			array x {&nvar} x1-x&nvar;
			keep x1-x&nvar;
			do n = 1 to &nobs;
				do i = 1 to &nvar;
					x{i} = rannor(-1);
				end;
				output;
			end;
		run;

		/* use proc factor again to get eigen value */
		proc factor data=raw nfact=&nvar noprint
			outstat=e(where=(_type_ = 'EIGENVAL'));
			var x1-x&nvar;
		run;

		proc append base=eigen
			data=e(keep=x1-x&nvar.);
		run;
	%end;

	/* summarize eigenvalues for simulated datasets */
	proc means data=eigen noprint;
		var x1-x&nvar.;
		output out=simulated(keep=x1-x&nvar.)
		&statistic.= ; /* use median*/
	run;

	proc datasets nolist;delete eigen;run;

	/* Observed datasets */
	proc transpose data=e1 out=e1;run;

	/* All the simulated datasets */
	proc transpose data=simulated out=simulated;run;

	/* --- plot actual vs. simulated eigenvalues --- */
	/* generate plot data point */
	data plotdata;
		length type $ 9;
		position+1;

		if position = (&nvar. + 1) then position = 1;
		set e1(in=a) simulated(in=b);
		if a then type = 'actual';
		if b then type = 'simulated';
		rename col1 = eigenvalue;
	run;

	/* Plot data */
	title height=1.5 "parallel analysis - &statistic simulated eigenvalues";
	title2 height=1 "&nvar variables, &niter iterations, &nobs observations";
	proc print data = plotdata; run;
	symbol1 interpol = join value=diamond height=1 line=1 color=blue;
	symbol2 interpol = join value=circle  height=1 line=3 color=red ;

	proc gplot data = plotdata;
		plot eigenvalue * position = type;
	run;quit;

%mend parallel;

/************************************/
/********         EOF          ******/
/************************************/
