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

