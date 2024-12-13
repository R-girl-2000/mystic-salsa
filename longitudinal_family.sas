***project using class data;
options nofmterr;

*import data;
proc import datafile="\\cas-psych-file.ad.uab.edu\shares\Mrug_727\Paige\Topic 2\fam_PS.sav" out=fam
dbms=spss replace;
run;

*create SAS library;
libname data "\\cas-psych-file.ad.uab.edu\shares\Mrug_727\Paige\Topic 2\";

**save data in SAS library;
data data.fam; 
set fam; 
run;
** check distribution of maternal depression over time;
 proc univariate data = fam normal;
 var MADEPM01 MADEPM06 MADEPM15 MADEPM24 MADEPM36 MADEPM54;
 histogram MADEPM01 MADEPM06 MADEPM15 MADEPM24 MADEPM36 MADEPM54 / midpoints =0 to 7 by 1;;
run; 
** check correlation of maternal depression scores over time;
proc corr data=fam noprob nomiss;
var MADEPM01 MADEPM06 MADEPM15 MADEPM24 MADEPM36 MADEPM54;
run;


*convert wide to long data file;
*longitudinal variables - financial resources, depressive symptoms and social support;
*all are mom report at months 01 through 54; 
DATA faml; SET fam;
  months = 01 ; finrsm = finrsm01; madepm = madepm01; socspm=socspm01; HLTHM=HLTHMM01; HLTHP=HLTHPM01; OUTPUT ;
  months = 06 ; finrsm = finrsm06; madepm = madepm06; socspm=socspm06; HLTHM=HLTHMM06; HLTHP=HLTHPM06; OUTPUT ;
  months = 15 ; finrsm = finrsm15; madepm = madepm15; socspm=socspm15; HLTHM=HLTHMM15; HLTHP= HLTHPM15; OUTPUT ;
  months = 24 ; finrsm = finrsm24; madepm = madepm24; socspm=socspm24; HLTHM= HLTHMM24; HLTHP=HLTHPM24; OUTPUT ;
  months = 36 ; finrsm = finrsm36; madepm = madepm36; socspm=socspm36; HLTHM=HLTHMM36; HLTHP=HLTHPM36; OUTPUT ;
  months = 54 ; finrsm = finrsm54; madepm = madepm54; socspm=socspm54; HlTHM=HLTHMM54; HLTHP=HLTHPM54; OUTPUT ;
DROP finrsm01 finrsm06 finrsm15 finrsm36 finrsm54 
madepm01 madepm06 madepm15 madepm24 madepm36 madepm54
socspm01 socspm06 socspm15 socspm36 socspm54
HLTHMM01 HLTHMM06 HLTHMM15 HLTHMM24 HLTHMM36 HLTHMM54
HLTHPM01 HLTHPM06 HLTHPM15 HLTHPM24 HLTHPM36 HLTHPM54;
RUN;
proc means data=faml;
var CID;
run;

*make sure data are sorted by id and time;
proc sort data=faml;
by cid months;
run;
data data.faml;
set faml;
run;
proc means data= fam; 
var madepm01 madepm06 madepm15 madepm24 madepm36 madepm54 HLTHMM01 HLTHMM06 HLTHMM15 HLTHMM24 HLTHMM36 
HLTHPM01 HLTHPM06 HLTHPM15 HLTHPM24 HLTHPM36 ;
title 'means of maternal dep, and parental health at each time point';
run;

proc means data=faml; var madepm hlthm hlthp months; 
title 'mean of madepm, mothers health and fathers health and months ';
run;
proc freq data=faml;
tables madepm hlthm hlthp;
title 'frequency of mothers depression scores';
run;

proc corr data=fam noprob nomiss;
var madepm01 madepm06 madepm15 madepm24 madepm36 madepm54;
run;
 proc univariate data = faml normal;
 var madepm;
 histogram MADEPM / midpoints =0 to 7 by 1;;
run; 

**plot depressive symptoms;
proc gplot data=faml;
  title 'Mothers Level of Depression Over Time';
  axis1 label=('months') order=(6 to 54 by 6) minor=none;
  axis2 label=(angle=90 'Mom depressive symptoms') order=(0 to 50 by 5) minor=none;
  symbol1 color=black interpol=join value=none width=1 height=1 repeat=5;
 symbol2 color=blue interpol=join value=none width=1 height=1 repeat=5;
  symbol3 color=red interpol=join value=none width=1 height=1 repeat=5;
  symbol4 color=green interpol=join value=none width=1 height=1 repeat=5;
  symbol99 interpol=none repeat=999;
  goptions noborder cback=white ftext="Arial" htext=2 hby=3;
  plot madepm*months=cid /nolegend noframe haxis=axis1 vaxis=axis2;
run;
quit;
*GEE model accounting for dependence, Exchangable;
* model with just time as a predictor;
proc genmod data=faml;
class cid;
model madepm = months/ dist=nb;
repeated subject=cid / type=exch corrw; 
run;
**Center health variables to facilitate interpretation at value=zero. pre-centered range is 1-4;
data data.faml;
set faml;
cHLTHM= HLTHM-3.24179;
cHLTHP= HLTHP-3.31386;
run;
data faml;
set data.faml;
run;

* Model with mothers and father's health;
proc genmod data=faml ;
class cid;
model madepm = months cHLTHM cHLTHP chlthm*months chlthp*months chlthp*chlthm/ dist=nb;
repeated subject=cid / type=exch corrw; 
run;




*** GEE model, unstructured;
proc genmod data=faml;
class cid;
model madepm = months HLTHM HLTHP/ dist=nb;
repeated subject=cid / type=un corrw; 
run;

***autoregressive;
proc genmod data=faml;
class cid;
model madepm = months HLTHM HLTHP / dist=nb;
repeated subject=cid / type=AR corrw; 
run;

***M-dependent(5);
proc genmod data=faml;
class cid;
model madepm = months HLTHM HLTHP / dist=nb;
repeated subject=cid / type=MDEP(5) corrw; 
run;

***getting IRR;
** exchnageable;
proc genmod data=faml;
class cid;
model madepm = months HLTHM HLTHP / dist=nb;
repeated subject=cid / type=exch corrw; 
estimate '1 year' months 12;
estimate 'moms health' HLTHM 4;
estimate 'fathers health' HLTHP 4;
title '';
run;
