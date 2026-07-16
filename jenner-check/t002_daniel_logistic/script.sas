/* Adapted from "dataset for daniel_SAS.sas": the SPSS import from a local
   OneDrive path is replaced by a small inline skin-color survey sample with
   the same variable names/types the script reads. The recodes, assumption
   checks, and PROC LOGISTIC (with CLASS + CONTRASTs) are the author's. */
data dataset_for_daniel;
input Participant_ID AT_1_Gender AT_17_sunscreen AT_18_Skin_Color A_5_Years_School_Completed;
datalines;
1 1 1 1 12
2 2 5 2 16
3 1 3 3 14
4 2 2 1 18
5 1 5 2 12
6 2 1 3 16
7 1 4 1 10
8 2 5 2 20
9 1 2 3 14
10 2 3 1 16
11 1 5 2 12
12 2 1 3 18
13 2 3 1 13
14 1 4 2 15
15 2 5 3 11
16 1 2 1 17
17 2 1 2 19
18 1 5 3 14
19 2 4 1 12
20 1 3 2 16
;
run;

proc contents data= dataset_for_daniel;
run;

/* Look for missing data*/
proc means data= dataset_for_daniel NMISS;
var AT_17_sunscreen AT_18_Skin_Color AT_1_Gender;
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

/* get descriptives*/
proc means data= dataset_for_daniel;
var AT_17_sunscreen AT_18_Skin_Color AT_1_Female;
title;
run;

/* create dichotomous sunscreen var*/
data dataset_for_daniel2 (drop = sunscreen1);
set dataset_for_daniel;
if AT_17_sunscreen= 1 then sunscreen=1;
Else if AT_17_sunscreen = 2 then sunscreen=1;
Else if AT_17_sunscreen = 3 then sunscreen =1;
Else if AT_17_sunscreen = 4 then sunscreen=1;
Else if AT_17_sunscreen = 5 then sunscreen=0;
label sunscreen = "When you outside for 15 minutes or more, do you wear sunscreen?";
run;
proc freq data=dataset_for_daniel2;
table sunscreen;
run;
data dd_LR;
set dataset_for_daniel2;
run;

/*check correlations between predictors and outcome (sunscreen use)*/
proc corr data=dd_LR;
   var sunscreen AT_1_Female AT_18_Skin_Color A_5_Years_School_Completed;
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
