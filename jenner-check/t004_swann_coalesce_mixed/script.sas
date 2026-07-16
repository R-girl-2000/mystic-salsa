/* Adapted from Swann.sas: the PROC CIMPORT transport-file imports from a
   network share are replaced by small inline baseline/V2/V3 samples with the
   source columns the script coalesces (AGE0/AGE1/Age3, Status*, BCEVER0/BCP1*,
   NUTRIRE*, BMI*). The author's visit-fixing, coalesce merge into a long file,
   and PROC MIXED random-effects model run against those samples. */
data blswann; input SwanID AGE0 RACE Status0 BCEVER0 BCEVMO0 NUTRIRE0 BMI0; datalines;
1 50 1 4 0 4 1 25.5
2 41 3 2 0 5 4 26.3
3 47 1 5 1 3 1 34.2
4 41 3 5 1 3 2 20.7
5 44 2 4 0 34 1 28.6
6 45 1 5 0 23 1 28.2
7 42 3 1 0 31 4 31.7
8 54 3 4 1 19 2 31.9
9 47 1 5 1 33 4 33.1
10 54 2 5 0 7 4 22.5
11 50 1 4 1 2 1 31.5
12 50 2 3 1 37 4 21.0;
run;
data v2swann; input SwanID AGE1 status1 BCP11 NUTRIRE1 BMI1; datalines;
1 43 3 1 1 20.9
2 50 5 1 3 30.7
3 52 1 1 3 22.5
4 44 4 0 2 31.5
5 45 2 1 4 33.8
6 56 1 0 4 26.0
7 49 2 1 3 30.6
8 52 4 0 2 21.2
9 45 2 0 1 27.3
10 46 3 1 1 22.2
11 52 5 1 2 30.4
12 42 4 1 4 26.0;
run;
data v3swann; input SwanID Age3 status3 BCP13 NUTRIRE3 BMI3; datalines;
1 46 4 1 1 22.9
2 49 4 0 1 25.1
3 44 1 0 2 28.0
4 54 5 0 1 33.1
5 55 2 1 3 29.0
6 58 1 0 4 34.9
7 57 4 1 3 21.3
8 46 3 1 4 32.4
9 48 5 0 2 34.3
10 54 2 0 3 34.7
11 45 3 1 2 25.3
12 50 5 1 2 29.2;
run;

** fix non-matching visit data types;
data blswann_fixed;
set blswann;
Visit_1 = 0;
run;
data v2swann_fixed;
set v2swann;
Visit_1 = 1;
run;
data V3swann_fixed;
set v3swann;
Visit_1 = 3;
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

*remove variables we dont want to analyze;
data swann_small;
set swann_long;
keep swanid age race meno_status bc_use length_BC_Bl nutrition bmi visit;
by swanid;
run;

*Run GLM to predict menopausal status from visit number and bc use;
proc mixed data=swann_small method=ml  ;
  class swanid;
  model meno_status = visit bc_use visit*bc_use /solution ;
  random intercept visit/type=un sub=swanid;
run;
