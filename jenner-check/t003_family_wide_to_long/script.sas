/* Adapted from longitudinal_family.sas: the SPSS import of "fam_PS.sav" from
   a network share is replaced by a small inline sample of the same wide
   monthly variables (finrsm/madepm/socspm/HLTHMM/HLTHPM at months 01-54).
   The author's wide-to-long reshape (6-timepoint OUTPUT transform) and the
   PROC UNIVARIATE/CORR/MEANS/FREQ descriptives run against that sample. */
data fam;
input CID finrsm01 finrsm06 finrsm15 finrsm24 finrsm36 finrsm54 madepm01 madepm06 madepm15 madepm24 madepm36 madepm54 socspm01 socspm06 socspm15 socspm24 socspm36 socspm54 HLTHMM01 HLTHMM06 HLTHMM15 HLTHMM24 HLTHMM36 HLTHMM54 HLTHPM01 HLTHPM06 HLTHPM15 HLTHPM24 HLTHPM36 HLTHPM54;
datalines;
1 1 1 3 2 2 2 1 1 6 0 0 1 10.9 25.3 1.3 9.9 32.5 27.2 1.7 2.8 3.4 1.0 3.4 3.1 2.0 1.5 3.9 2.0 1.3 1.3
2 3 5 3 1 4 5 1 6 1 4 5 3 35.2 2.3 11.4 14.5 4.0 11.6 1.3 1.8 2.9 2.1 2.1 1.6 1.8 3.8 2.9 2.8 1.5 3.2
3 2 4 4 3 5 2 5 0 3 0 5 6 13.4 10.5 47.1 43.8 15.7 32.8 2.2 3.7 2.4 1.8 1.7 2.7 1.8 2.8 3.7 2.2 1.7 4.0
4 5 4 1 1 1 2 2 6 1 6 6 7 26.5 48.6 43.0 0.6 36.0 34.1 2.6 1.8 2.9 1.3 2.3 2.4 3.9 3.6 1.8 2.5 1.5 3.7
5 3 5 5 2 2 3 2 0 5 7 0 1 46.5 43.9 41.6 15.4 2.9 43.9 3.8 1.3 2.5 1.2 3.3 3.3 1.4 2.4 2.6 1.8 3.6 2.3
6 2 5 2 3 4 3 7 7 1 3 3 1 16.9 29.4 11.5 11.0 3.5 31.6 1.7 3.7 3.6 1.2 1.7 3.0 1.6 1.4 3.8 2.7 2.4 3.4
7 4 2 1 1 4 3 6 6 7 0 1 0 20.1 17.0 43.1 12.4 9.5 22.4 2.3 1.8 1.7 3.8 2.3 3.6 2.7 1.2 4.0 3.5 3.9 3.8
8 2 2 4 4 4 2 6 0 2 6 0 6 13.3 39.2 22.8 21.2 47.9 49.8 2.7 3.2 1.5 1.9 3.9 2.7 2.6 3.2 1.2 2.8 2.5 3.6
9 2 1 5 1 2 1 1 3 6 1 3 0 31.0 21.0 29.2 26.1 46.7 10.2 3.1 1.7 2.2 3.0 1.9 1.9 3.3 1.2 2.4 4.0 4.0 1.2
10 2 5 3 2 3 1 3 5 4 2 7 4 30.6 49.4 32.7 0.4 40.9 15.0 3.0 3.8 1.4 1.3 1.3 2.7 1.8 2.8 3.2 1.6 2.9 1.8
11 4 3 1 1 4 3 0 0 5 2 4 2 37.1 27.6 21.4 0.5 3.8 44.2 3.7 2.6 3.5 2.7 1.4 1.4 1.9 3.7 3.4 3.6 3.7 1.6
12 2 1 3 5 4 5 2 3 2 2 6 0 9.0 46.2 39.1 20.6 33.5 36.8 1.7 1.5 3.1 2.1 1.1 2.4 1.6 3.8 2.0 3.5 3.6 1.7;
run;

** check distribution of maternal depression over time;
proc univariate data = fam normal;
var MADEPM01 MADEPM06 MADEPM15 MADEPM24 MADEPM36 MADEPM54;
run;
** check correlation of maternal depression scores over time;
proc corr data=fam noprob nomiss;
var MADEPM01 MADEPM06 MADEPM15 MADEPM24 MADEPM36 MADEPM54;
run;

*convert wide to long data file;
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

*make sure data are sorted by id and time;
proc sort data=faml;
by cid months;
run;

proc means data=faml; var madepm hlthm hlthp months;
title 'mean of madepm, mothers health and fathers health and months ';
run;
proc freq data=faml;
tables madepm hlthm hlthp;
title 'frequency of mothers depression scores';
run;
