/* Import from SPSS*/
proc import datafile= 'C:\Users\pvand\Downloads\dataset for Daniel skin color[3805].sav' out=dataset_for_daniel;
run;
proc contents data= dataset_for_daniel;
run;
proc print data= dataset_for_daniel;
run;
/* Look for missing data*/
proc means data= dataset_for_daniel NMISS;
var AT_17_sunscreen AT_18_Skin_Color AT_1_Female;
title;
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
proc print data= dataset_for_daniel;
var AT_17_sunscreen;
run;
/* create dichotomous sunscreen var*/
data dataset_for_daniel2;
set dataset_for_daniel;
if AT_17_sunscreen= 'Always' then sunscreen=1;
Else if AT_17_sunscreen = 'Frequently' then sunscreen=1;
Else if AT_17_sunscreen = 'Sometimes' then sunscreen =1;
Else if AT_17_sunscreen = 'Rarely' then sunscreen=1;
Else sunscreen=0;
run;
proc print data=dataset_for_daniel2;
var sunscreen;
run;
/* did not work, investigate*/
proc SQL;
SELECT * FROM dataset_for_daniel2
WHERE AT_17_sunscreen = 'Never';
run;
/* vartype mismatch*/
proc SQL;
SELECT AT_17_sunscreen2 FROM dataset_for_daniel2;
run;
data dataset_for_daniel3;
set dataset_for_daniel2;
AT_17_sunscreen2= put(AT_17_sunscreen, 10.);
run;
proc SQL;
SELECT AT_17_sunscreen2 FROM dataset_for_daniel3;
run;
/* above returned numeric values saved as characters, now we know the varchar 'Never' is saved as the numeric value 5 in the original AT_17_sunscreen var*/
proc SQL;
SELECT AT_17_sunscreen FROM dataset_for_daniel3;
run;
proc print data= dataset_for_daniel3;
var AT_17_sunscreen;
run;
/* create dichotomous sunscreen var*/
data dataset_for_daniel3;
set dataset_for_daniel3;
if AT_17_sunscreen=5  then sunscreen=0;
Else sunscreen=1;
run;
/*check it worked*/
proc SQL;
select sunscreen from dataset_for_daniel3;
run;
libname skincolr 'C:\Users\pvand\OneDrive\Documents\';
data skincolr.dataset_for_daniel3;
set work.dataset_for_daniel3;
run;
/* next run assumption tests*/
