/*Sort and filter on target lesion and target lesion at baseline forms where new tumor lesions identified and tumor assessment performed, rename assessment date and tumor size at baseline for merge*/
/*Final tumor size the sum of multiple identified tumors, only keep one sum per patient tumor assessment*/

proc sort data= data_in.tl out=tl(where=(tl_trperf="Yes") ) nodupkey; 
    by pb_region pb_center pb_patient tl_trdat tl_trlnkid; 
run; 

*rename variables from the baseline dataset so we can merge later;

proc sort data= data_in.tlb out=tlb(where=(tlb_tuyn="Yes") rename= (tlb_tudat=tl_trdat tlb_sumdiam_trorres=tl_sumdiam_trorres tlb_tulnkid = tl_trlnkid )) nodupkey; 
    by pb_region pb_center pb_patient tlb_tudat tlb_tulnkid; 
run; 

*merge follow-up visit dataset with the baseline dataset;

data tlb_tl;
    set tl(keep=tl_sumdiam_trorres tl_trdat tl_trlnkid datapagename foldername cp:) tlb(keep=tl_sumdiam_trorres tl_trdat tl_trlnkid datapagename foldername cp:);
run;

proc sort data = tlb_tl nodupkey;
by pb_patient tl_trdat tl_trlnkid;
run;

*/ Find first instance of each tumor id for each patient. 
Used later to calculate the shrinkage from baselien to first follow-up measurement for the first tumor measured.*/;

data tl_firstvisit;
    set tl(keep= tl_sumdiam_trorres tl_trdat tl_trlnkid datapagename foldername pb_: );
    by pb_region pb_center pb_patient tl_trdat tl_trlnkid;
    if first.tl_trlnkid and first.pb_patient then pb_firstvisit = 1;
    run;
    
*add this new flag variable to the merged dataset, keeping only what is in the merged dataset + this new flag;
    
proc sql;
create table first_visit AS
select a. *, b. pb_firstvisit
from work.tlb_tl as a left join work.tl_firstvisit as b
on a.pb_patient=b.pb_patient and a.tl_trdat= b.tl_trdat and a.tl_trlnkid=b.tl_trlnkid;
quit;


proc sql;
delete 
from first_visit where pb_firstvisit ne 1 and foldername ne "Screening";
quit;

/*Derive percentage change in baseline tumor size
pb_ch_ts_bl means " CP change in tumor size since baseline" */

proc sort data=first_visit;
by pb_region pb_center pb_patient tl_trdat;
run;


/*proc sort data=tlb_tl;
    by pb_region pb_center pb_patient tl_trdat;
run;*/

data data_out.output ;
    set first_visit;
    *set tlb_tl;
    by pb_region pb_center pb_patient;

    retain pb_tl_trorres_scr;

    if first.pb_patient then do;
        pb_tl_trorres_scr = .;
        pb_ch_ts_bl = .;
    end;

    if foldername = "Screening" then pb_tl_trorres_scr = tl_sumdiam_trorres;
    else if foldername = "Tumor Assessment" and not missing(pb_tl_trorres_scr) and pb_firstvisit = 1 then do;
        pb_ch_ts_bl = ((tl_sumdiam_trorres - pb_tl_trorres_scr) / pb_tl_trorres_scr) * 100;
    end;
    
    /*Derive pb_TUM (check for Early Tumor Shrinkage)*/
    if pb_ch_ts_bl le -10  and foldername ne "Screening" then pb_tum="Yes";
    else if pb_ch_ts_bl gt -10  and foldername ne "Screening" then do pb_tum="No";
    end;

run;

data data_out.output;
set data_out.output;
label pb_ch_ts_bl = "CP Change in Tumor Size Since Baseline" pb_tum= "Early Tumor Shrinkage";
run;
