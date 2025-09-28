/* Import from SPSS*/
options validvarname=v7 nofmterr;
LIBNAME A '.';

proc import datafile= "C:\Users\pvand\OneDrive - UAB - The University of Alabama at Birmingham\Documents\dataset for Daniel skin color.sav" 
dbms= spss 
out=dataset_for_daniel
REPLACE;
FMTLIB=A.FORMATS_SPSS;
run;
proc contents data= dataset_for_daniel;
run;
proc print data=dataset_for_daniel;
var AT_1_:;
run;
/* Look for missing data*/
proc means data= dataset_for_daniel NMISS;
var AT_17_sunscreen AT_18_Skin_Color AT_1_Gender; *AT_1_Female;
title "missing values" ;
run;
*create female variable;
data dataset_for_daniel (drop= at_1_gender1);
set dataset_for_daniel;
if AT_1_Gender = 2 then AT_1_Female = 1;
else if AT_1_Gender = 1 then AT_1_Female = 0;
else if missing(AT_1_Gender) then AT_1_Female = ".";
label AT_1_Female = "Is Female?";
run;
proc print data= dataset_for_daniel;
var at_1_:;
run;

/* get descriptives*/
proc means data= dataset_for_daniel;
var AT_17_sunscreen AT_18_Skin_Color AT_1_Female;
title;
run;
* Check Skewness and Kurtosis*;
proc univariate data= dataset_for_daniel;
var AT_17_sunscreen AT_18_Skin_Color AT_1_Female;
run;
data dataset_for_daniel;
set dataset_for_daniel;
sunscreen1 = AT_17_sunscreen;
run;
/*check values for sunscreen use*/
proc freq data= dataset_for_daniel;
tables AT_17_sunscreen sunscreen1;
run;

/* create dichotomous sunscreen var*/
data dataset_for_daniel2 (drop = sunscreen1);
set dataset_for_daniel;
if AT_17_sunscreen= 1 then sunscreen=1;
Else if AT_17_sunscreen = 2 then sunscreen=1;
Else if AT_17_sunscreen = 3 then sunscreen =1;
Else if AT_17_sunscreen = 4 then sunscreen=1;
Else if AT_17_sunscreen = 5 then sunscreen=0;
label sunscreen = "When you outside for 15 minutes or more, do
you wear sunscreen?";
run;
proc freq data=dataset_for_daniel2;
table sunscreen;
run;
data dd_LR;
set dataset_for_daniel2;
run;

/* next run assumption tests;
no continuous predictors, so no need for box-tidewell*/

/*check correlations between predictors and outcome (sunscreen use)*/
proc corr data=dd_LR;
   var sunscreen AT_1_Female AT_18_Skin_Color A_5_Years_School_Completed;
run;

/* predictors are not time dependent, so no durbin-watson to check for independence*/

/*check multi-colinaetiry with an MR, can also use corr*/
proc reg data= dd_LR;
model sunscreen = AT_1_Female AT_18_Skin_Color A_5_Years_School_Completed / Vif tol;
output out=res (keep = r) residual=r;
run;
/* tol >0.1, VIF < 10, no multi co*/


/*check outliers via standardized res*/
proc reg data= dd_LR;
model sunscreen = AT_1_Female AT_18_Skin_Color A_5_Years_School_Completed / Vif tol;
output out=dd_LR_stats (keep = r lev cd dffits AT_1_Female AT_18_Skin_Color A_5_Years_School_Completed sunscreen Participant_ID) rstudent=r h=lev cookd=cd dffits=dffits;
run;

proc univariate data=dd_LR_stats;
var r lev cd dffits;
run;

proc print data=dd_LR_stats;
  var r AT_1_Female AT_18_Skin_Color A_5_Years_School_Completed sunscreen Participant_ID ;
  where abs(r)>2;
run;
/*scan for non-zero cell counts*/

proc freq data= dd_lr;
tables AT_1_Female*sunscreen AT_18_Skin_Color*sunscreen A_5_Years_School_Completed*sunscreen;
run;

/*all good, now perform logistic regression*/

proc logistic data= dd_LR ;
	class AT_1_Female AT_18_Skin_Color;
	model sunscreen = AT_1_Female AT_18_Skin_Color A_5_Years_School_Completed/ RSQUARE expb;
	contrast '1 vs 2 of Skin Color' AT_18_Skin_Color 1 -1 0 /estimate;
	contrast '1 vs 3 of Skin Color' AT_18_Skin_Color 1 0 -1 /estimate;
	contrast ' 2 vs 3 of Skin Color' AT_18_Skin_Color 0 1 -1 /estimate;
run;
