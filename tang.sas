libname tangdr 'C:\Users\pvand\OneDrive\Documents\';
run;
data tang_beverage;
infile 'C:\Users\pvand\Desktop\tang_drink.txt';
input NAME $ 1-6 Age $ 7-13 Sex $ 14-15 Purchases 16-19;
run;
/*overwrite existing data*/
data tang_beverage1;
set tang_beverage; 
input NAME $ 1-6 Age $ 7-13 Sex $ 14-15 Purchases 16-19;
datalines;
Ben   teen   M 5
Julia teen   F 2
Henry child  M 4
;
run;
/*add new data*/
data tang_beverage2;
set tang_beverage;
input NAME $ 1-6 Age $ 7-13 Sex $ 14-15 Purchases 16-19;
datalines;
Ben   teen   M 5
Julia teen   F 2
Henry child  M 4
Andrew adult M 3
Hailey adult F 1
Cyndi adult  F 6
;
run;
proc append base=tang_beverage data= tang_beverage2;
run;
/*check that data was actually appended correctly */
proc contents data= tang_beverage;
run;
proc print data= tang_beverage;
run;
/* add money spent*/
data tang_money;
format Date MMDDYY10.;
input  NAME $ Price DOLLAR6. Region $ Date MMDDYY10.;
datalines;
Johnny $4.80 NE 01/10/2003
Max $4.99 SW 02/02/2003
Erika $5.15 W 01/20/2003
Anna $5.75 NW 01/20/2003
Ben $5.00 NE 01/01/2003
Julia $5.10 W 01/02/2003
Henry $4.99 SE 01/01/2003
Andrew $4.80 N 01/05/2003
Hailey $5.00 SE 01/09/2003
Cyndi $5.20  NE 01/15/2003
;
run;
proc print data= tang_money;
run;
proc sort data= tang_beverage;
by NAME;
run; 
proc sort data = tang_money;
by NAME;
run;
Data tang_merge;
merge tang_beverage tang_money;
by NAME;
run;
proc print data= tang_merge;
run;
/* Save to tangdr library for later*/
data tangdr.tang_merge;
set work.tang_merge;
run;

/*create new variable*/
data tang_merge;
set tang_merge;
spent= Price*Purchases;
run;
proc print data= tang_merge;
run;
/* get descriptives for all */
proc univariate data= tang_merge;
var spent price purchases;
Title;
run;
/* get decriptives and test for normality*/
proc univariate Normal data= tang_merge;
var spent price purchases;
Title;
run;
/* plot descriptives*/
proc univariate data=tang_merge;
var spent price purchases;
histogram spent price purchases/NORMAL; /*overlay with normal dist curve*/
probplot spent;
title;
run;
/*run ANOVA*/
proc ANOVA data= tang_merge;
class age;
model spent = age;
means age/Tukey;
title 'spending habits by age';
run;
