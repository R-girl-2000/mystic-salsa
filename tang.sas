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
;
run;
proc print data= tang_money;
run;
/* sort to prepare for merge by name*/
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

