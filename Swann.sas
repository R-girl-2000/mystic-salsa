**Import Swann Basline from transfer SAS file;

proc cimport library= work
infile="\\cas-psych-file.ad.uab.edu\shares\Mrug_727\Paige\Project\Baseline Swann\28762-0001-Data.stc" 
;
run;
**Import Swann V2 from transfer;
proc cimport library= work
infile="\\cas-psych-file.ad.uab.edu\shares\Mrug_727\Paige\Project\V2 Swann\ICPSR_29221\DS0001\29221-0001-Data.stc" 
;
run;
**Import Swann V3 from transfer;
proc cimport library= work
infile="\\cas-psych-file.ad.uab.edu\shares\Mrug_727\Paige\Project\V3 Swann\ICPSR_29701\DS0001\29701-0001-Data.stc"
;
run;
* Create SAS library;
libname data "\\cas-psych-file.ad.uab.edu\shares\Mrug_727\Paige\Project\";
** Save data to SAS Library;
data data.Da28762p1; 
set Da28762p1; 
run;
data data.Da29221p1; 
set Da29221p1; 
run;
data data.Da29701p1; 
set Da29701p1; 
run;
** Create more intutive data labels;
DATA BLSwann; set Da28762p1;
run;
DATA V2Swann; set Da29221p1;
run;
DATA V3Swann; set Da29701p1;
run;
** Save new datasets to DATA (named) library;

data data.BLSwann; 
set BLSwann; 
run;
data data.V2Swann; 
set V2Swann; 
run;
data data.V3Swann;
set V3Swann;
run;
** fix non-matching visit data types;
data blswann_fixed;
set blswann;
drop VISIT;
Visit_1 = 0;
run;
data v2swann_fixed;
set v2swann;
drop VISIT;
Visit_1 = 1;
run;
data V3swann_fixed;
set v3swann;
drop VISIT;
Visit_1 = 3;
run;
data data.blswann_fixed;
set blswann_fixed;
run;
data data.v2swann_fixed;
set v2swann_fixed;
run;
data data.v3swann_fixed;
set v3swann_fixed;
run;
*Sort files by Swann ID to prepare for merge;
proc sort data= BlSwann_fixed;
by SwanID;
run;
proc sort data= V2swann_fixed;
by SwanID;
run;
proc sort data= V3Swann_fixed;
by SwanID;
run;
*create merged long dataset from multiple long data sets;
data Swann_long ;
  set blswann_fixed v2swann_fixed v3swann_fixed ;
  by SwanID;
  age = coalesce(AGE0,AGE1,Age3);
   race = coalesce(RACE);
   meno_status= coalesce (Status0, status1, status3);
   bc_use=coalesce (BCEVER0,BCP11,BCP13);
   length_BC_BL= BCEVMO0;
   nutrition= coalesce(NUTRIRE0,NUTRIRE1,NUTRIRE3);
   bmi= coalesce (BMI0,BMI1,BMI3);
   visit= coalesce (VISIT_1, VISIT_1,VISIT_1);
run;
data data.swann_long;
set swann_Long;
run;
*remove variables we dont want to analyze;
data swann_small;
set swann_long;
keep swanid age race meno_status bc_use length_BC_Bl nutrition bmi visit;
by swanid;
run;
data data.swann_small;
set swann_small;
run;
*Run GLM to predict menopausal status from visit number and bc use;
proc mixed data=swann_small method=ml  ;
  class swanid;
  model meno_status = visit bc_use visit*bc_use /solution ;
  random intercept visit/type=un sub=swanid;
run;
************************************************************************************************************
************************************************************************************************************
** Copied from class, menopause variable decreases from 8 (hysterectomy) 7 (HT use, unknown status), 6 (pregnant or bfing), 5 (pre), 
4 (early peri), 3 (late peri, 2 (natural post, 1 (post by bilateral Salpingo Oophorectomy);
proc mixed data=swann_small method=ml noclprint noinfo covtest;
title 'Model A - unconditional means model';
  class swanid;
  model meno_status = / solution chisq;
 random intercept  / sub=swanid;
run;


**copy, paste and modify to run Model b - unconditional growth model;
**use visit as the time variable;
proc mixed data=swann_small method=ml noclprint noinfo covtest;
title 'Model B - unconditional growth model';
  class swanid;
  model meno_status = visit / solution chisq;
 random intercept visit / sub=swanid;
run;
** getting covariates and correlations;
proc mixed data=faml method=ml noclprint noinfo covtest;
title 'Model B - unconditional growth model-Correlations';
  class cid;
  model socspm = years / solution chisq;
  ** type=un gives us the unstructured residual correlation structure to estimate the effects;
 random intercept years / type=un sub=cid;
run;
proc freq data=faml;
tables years;
run;
** model that will create predicted values;
proc mixed data=faml method=ml noclprint noinfo covtest;
title 'Model B - unconditional growth model-predicted values';
  class cid;
  model socspm = years / solution chisq outpm=pred;
 random intercept years / type=un sub=cid;
run;
** run GLM on predicted data, model = observed social support value vs predicted values;
proc reg data=pred;
model socspm = pred ;
run;
** correlation between social support observed and predicted. have to use predicted data set becuase it is the only one with pred values;
proc corr data=pred;
var socspm pred;
run; 

**add predictors:
age - mage_m01 
marital status - mmarried 
education - MEDUCM01 
stress at 1 mo - ABPSIM01;

*this code will only center the specified variables, SDs will not change;
*variables will be centered in the outputed data set we named std;
proc import datafile='\\cas-psych-file.ad.uab.edu\shares\Mrug_727\Paige\Topic 3\fam_PS.sav' out=fam
dbms=spss replace;
run;

data fam;
set data.fam;
run;

**recode continuous predictors centered at 0;
proc standard data=fam out=std mean=0  ;
var mage_m01 MEDUCM01 ABPSIM01;
run;

proc means data=fam;
var mage_m01 MEDUCM01 ABPSIM01;
run;
proc means data=std;
var mage_m01 MEDUCM01 ABPSIM01;
run;

***checking assumptions;

*functional form at Level 1;
proc sgpanel data = faml; where rand < .01;
  panelby cid /columns=4 rows= 4;
  reg y = socspm x = time;
run;
quit;

*functional form at Level 2;
*demonstrate on 2 predictors, one dichotomous (married) and one continuous (stress at 1mo);
*figure 4.4 on p. 130;
proc sort data = faml out=fig4_4;
 by cid;
run;
*OLS estimates of intercept and slope;
** suppressed all the results, so theres no results printed;
proc reg data = fig4_4 outest=est plots=none noprint;
   by cid;
  model socspm = years;
run;
quit;
*slope is renamed 'pred';
data fig4_4l;
  merge est(rename=(years=pred)) fig4_4;
  by cid;
run;

*correlations of int & slope with Level 2 predictors - married & stress at 1 mo;
** when you use the with statementyou get a matrix with only the ones you asked for;
ods output  PearsonCorr = corr;
proc corr data = fig4_4l nosimple noprob;
  var mmarried cstress;
  with intercept pred;
run;
*putting correlations in a file;
** Dataset called nulll;
data _null_;
  set corr;
  if _n_ = 1 then do;
    call symput('int_married',  put(mmarried, 4.2));
    call symput('int_stress', put(cstress, 4.2));
  end;
  if _n_ = 2 then do;  
    call symput('pred_married',  put(mmarried, 4.2));
    call symput('pred_stress', put(cstress, 4.2));
  end;
run;
*looking at range of int, slope and stress to inform range on axes;
proc means data= fig4_4l N mean min max;
var intercept pred cstress;
run;

*plotting int & slope against predictor married; 
ods html style=journal2;
ods graphics on /width=3in height=3in border=off;
title;
proc sgplot data = fig4_4l;
 xaxis min = -.5 max = 1.5 values=(0,1) VALUESHINT;
 yaxis min = 0 max = 7 label="Intercept";
  format estimate 3.0;
  scatter x = mmarried  y = intercept  /markerattrs = (symbol=circlefilled);
  refline 0 /axis=y;
  inset  "&int_married" /noborder position=topright;
run;
proc sgplot data = fig4_4l;
 xaxis min = -.5 max = 1.5 values=(0,1) VALUESHINT;
 yaxis min = -5 max = 5 label="pred";
  format estimate 3.0;
  scatter x = mmarried y = pred  /markerattrs = (symbol=circlefilled);
  refline 0 /axis=y;
  inset  "&pred_married" /noborder position=topright;
run;
*plotting int & slope against predictor stress values=(0,1,2,3) ;
proc sgplot data = fig4_4l;
 xaxis min = -30 max = 45 VALUESHINT;
 yaxis min = 0 max = 8 label="Intercept";
  format estimate 3.0;
  scatter x = cstress  y = intercept  /markerattrs = (symbol=circlefilled);
  refline 0 /axis=y;
  inset  "&int_stress" /noborder position=topright;
run;
proc sgplot data = fig4_4l ;
 xaxis min = -30 max = 45 VALUESHINT;
 yaxis min = -5 max = 5 label="pred";
  format estimate 3.0;
  scatter x = cstress  y = pred  /markerattrs = (symbol=circlefilled);
  refline 0 /axis=y;
  inset  "&pred_stress" /noborder position=topright;
run;


*checking normality;

*MODEL with stress and marital status;
proc mixed data=faml covtest method=ml noclprint ;
  title2 "Final model for stress";
  class cid;
  model socspm = cstress mmarried years years*cstress years*mmarried / outp=fig4_5 solution chisq;
  random intercept years /type=un sub=cid solution;
  ods output SolutionR = fig4_5a;
run;
**proc contents gives you a list of all the variables in your file;
*normal prob plot for L1 residual, epsilon; 
*with normality tests and histogram;
proc univariate data = fig4_5  normal ;
title 'Level 1 residual';
  var resid;
  qqplot /vref=0 ;
   histogram resid / normal ;
run;
*normal prob plot for L2 residuals for intercept & slopes, zeta0 and zeta1;
**4.5a has the L2 residuals, 4.5 has the L1 residuals;
proc univariate data = fig4_5a  normal ;
title 'Level 2 residual for intercept';
  where effect = "Intercept";
  var estimate;
  qqplot /vref=0 normal;
   histogram estimate / normal ;
run;
**have to run these seperately, since we have two formulas for L2, one with the intercept and one with the slope;
proc univariate data = fig4_5a  normal ;
title 'Level 2 residual for slope';
where effect = "years";
  var estimate;
  qqplot /vref=0 ;
   histogram estimate / normal ;
run;

*standardized residuals;
proc stdize data = fig4_5 out=fig4_5r;
  var resid;
run;
title;
*plot std L1 residuals against cid; 
proc sgplot data = fig4_5r;
  scatter x = cid y = resid /markerattrs = (symbol=circlefilled);
  refline 0 /axis=y;
run;
quit;

*same for L2 intercept residual;
proc stdize data = fig4_5a out=fig4_5rm (rename=(estimate = int));
 where effect = "Intercept";
  var estimate;
run;
proc sgplot data = fig4_5rm;
  format int 3.0;
  scatter x = cid y = int /markerattrs = (symbol=circlefilled);
  refline 0 /axis=y;
run;

*same for L2 slope residual;
proc stdize data = fig4_5a out=fig4_5rm (rename=(estimate=years));
 where effect = "years";
  var estimate;
run;
proc sgplot data = fig4_5rm;
  format years 3.0;
  scatter x = cid y= years /markerattrs = (symbol=circlefilled);
  refline 0 /axis=y;
run;
quit;


**checking homoscedasticity;
*first get ranges fro graphs;
proc means data= fig4_5 N mean min max;
var resid;
run;
*level 1 residual;
proc sgplot data = fig4_5 ;
 xaxis min = -1 max = 5.5;
 yaxis min = -3 max = 2;
  scatter x = years y = resid /markerattrs = (symbol=circlefilled);
  refline 0 /axis=y;
  title 'Level 1 residuals - homoscedasticity';
run;
quit;

*level 2 residuals;
*get ranges for graphs;
proc sort data=fig4_5a; by effect; run;
proc means data= fig4_5a N mean min max;
var estimate;
by effect;
run;

*merge residual data with raw data so we also have Level 2 predictors in file;
*first for intercept residuals;
proc sort data = fig4_5a;
  by cid;
proc sort data = faml;
  by cid;
run;
data fig4_6int;
  merge fig4_5a (where=(effect="Intercept"))
        faml;
  by cid;
run;
proc sgplot data = fig4_6int;  
title 'Level 2 intercept residual';
 xaxis min = -.5 max = 1.5 values=(0,1) VALUESHINT;
 yaxis min = -2 max = 1.5;
  format estimate 3.0;
  scatter x = mmarried y =  estimate /markerattrs = (symbol=circlefilled);
  refline 0 /axis=y;
run;
quit;

proc sgplot data = fig4_6int;  
title 'Level 2 intercept residual';
 xaxis min = -30 max = 45  VALUESHINT;
 yaxis min = -2 max = 1.5;
  format estimate 3.0;
  scatter x = cstress y =  estimate /markerattrs = (symbol=circlefilled);
  refline 0 /axis=y;
run;
quit;

*now merge data sets for slope residuals;
data fig4_6years;
  merge fig4_5a (where=(effect="years"))
        faml;
  by cid;
run;
proc sgplot data = fig4_6years;  
title 'Level 2 slope residual';
 xaxis min = -.5 max = 1.5 values=(0,1) VALUESHINT;
 yaxis min = -.4 max = .2 label="years";
  format estimate 3.0;
  scatter x = mmarried y =  estimate /markerattrs = (symbol=circlefilled);
  refline 0 /axis=y;
run;
quit;

proc sgplot data = fig4_6years;  
 xaxis min = -30 max = 45 VALUESHINT;
 yaxis min = -.4 max = .2 label="years";
  format estimate 3.0;
  scatter x = cstress y =  estimate /markerattrs = (symbol=circlefilled);
  refline 0 /axis=y;
run;
quit;

