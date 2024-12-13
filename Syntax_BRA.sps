* Encoding: UTF-8.
*PainME

DATASET ACTIVATE DataSet1.
T-TEST GROUPS=PAIN(0 1)
  /MISSING=ANALYSIS
  /VARIABLES=dmPFC PCG1 PCG2 PoCG vmPFC
  /ES DISPLAY(TRUE)
  /CRITERIA=CI(.95).
*PainxControl, compares pain group averages in signal across brain regions with or without control. 

DATASET ACTIVATE DataSet4.
T-TEST GROUPS=PAIN(0 1)
  /MISSING=ANALYSIS
  /VARIABLES=ACC dmPFC IFG IPL PHG SFG ACC_NoC dmPFC_NoC IFG_NoC IPL_NoC PHG_NoC SFG_NoC
  /ES DISPLAY(TRUE)
  /CRITERIA=CI(.95).

*tests ME of control across all groups. 
T-TEST PAIRS=ACC_Control dmPFC_Control WITH ACC_NoC dmPFC_NoC (PAIRED)
  /ES DISPLAY(TRUE) STANDARDIZER(SD)
  /CRITERIA=CI(.9500)
  /MISSING=ANALYSIS.
*PainxPredict, compares pain groups in signficant brain regions during predictable and unpredictable situations. 
DATASET ACTIVATE DataSet5.
T-TEST GROUPS=PAIN('0' '1')
  /MISSING=ANALYSIS
  /VARIABLES=Amygdala_Predict PCG_predict dlPFC_predict Amygdala_UnP PCG_UnP dlPFC_UnP
  /ES DISPLAY(TRUE)
  /CRITERIA=CI(.95).
*Main Effect of Predict.
T-TEST PAIRS=Amygdala_Predict PCG_predict dlPFC_predict WITH Amygdala_UnP PCG_UnP dlPFC_UnP (PAIRED)    
  /ES DISPLAY(TRUE) STANDARDIZER(SD)
  /CRITERIA=CI(.9500)
  /MISSING=ANALYSIS.

*3-way interaction tests, dmPFC first.
DATASET ACTIVATE DataSet7.
T-TEST GROUPS=PAIN('0' '1')
  /MISSING=ANALYSIS
  /VARIABLES=Predict_Control Predict_NoC UnP_Control UnP_NoC
  /ES DISPLAY(TRUE)
  /CRITERIA=CI(.95).


DATASET ACTIVATE DataSet8.
T-TEST GROUPS=PAIN('0' '1')
  /MISSING=ANALYSIS
  /VARIABLES=Predict_Control Predict_NoControl UnP_Control UnP_NoC
  /ES DISPLAY(TRUE)
  /CRITERIA=CI(.95).

DATASET ACTIVATE DataSet9.
T-TEST GROUPS=Pain(0 1)
  /MISSING=ANALYSIS
  /VARIABLES=Predict_Control Predict_NoC UnP_Control UnP_NoC
  /ES DISPLAY(TRUE)
  /CRITERIA=CI(.95).

DATASET ACTIVATE DataSet10.
T-TEST GROUPS=PAIN('0' '1')
  /MISSING=ANALYSIS
  /VARIABLES=predictandcontrol unpredictableControl PredictNocontrol NoControlUnpredictable
  /ES DISPLAY(TRUE)
  /CRITERIA=CI(.95).

DATASET ACTIVATE DataSet1.
T-TEST GROUPS=PAIN('0' '1')
  /MISSING=ANALYSIS
  /VARIABLES=predictandcontrol unpredictableControl PredictNocontrol NoControlUnpredictable
  /ES DISPLAY(TRUE)
  /CRITERIA=CI(.95).

DATASET ACTIVATE DataSet3.
T-TEST GROUPS=PAIN('0' '1')
  /MISSING=ANALYSIS
  /VARIABLES=predictandcontrol unpredictableControl PredictNocontrol NoControlUnpredictable
  /ES DISPLAY(TRUE)
  /CRITERIA=CI(.95).

DATASET ACTIVATE DataSet1.
T-TEST GROUPS=PAIN('0' '1')
  /MISSING=ANALYSIS
  /VARIABLES=predictandcontrol unpredictableControl PredictNocontrol NoControlUnpredictable
  /ES DISPLAY(TRUE)
  /CRITERIA=CI(.95).

DATASET ACTIVATE DataSet4.
T-TEST GROUPS=PAIN('0' '1')
  /MISSING=ANALYSIS
  /VARIABLES=predictandcontrol unpredictableControl PredictNocontrol NoControlUnpredictable
  /ES DISPLAY(TRUE)
  /CRITERIA=CI(.95).

DATASET ACTIVATE DataSet6.
T-TEST GROUPS=PAIN('0' '1')
  /MISSING=ANALYSIS
  /VARIABLES=predictandcontrol unpredictableControl PredictNocontrol NoControlUnpredictable
  /ES DISPLAY(TRUE)
  /CRITERIA=CI(.95).

DATASET ACTIVATE DataSet7.
T-TEST GROUPS=PAIN('0' '1')
  /MISSING=ANALYSIS
  /VARIABLES=predictandcontrol unpredictableControl PredictNocontrol NoControlUnpredictable
  /ES DISPLAY(TRUE)
  /CRITERIA=CI(.95).



DATASET ACTIVATE DataSet8.
T-TEST GROUPS=Pain('0' '1')
  /MISSING=ANALYSIS
  /VARIABLES=Predict_Control Predict_NoC UnP_Control UnP_NoC
  /ES DISPLAY(TRUE)
  /CRITERIA=CI(.95).

DATASET ACTIVATE DataSet9.
T-TEST GROUPS=PAIN('0' '1')
  /MISSING=ANALYSIS
  /VARIABLES=predictandcontrol unpredictableControl PredictNocontrol NoControlUnpredictable
  /ES DISPLAY(TRUE)
  /CRITERIA=CI(.95).

DATASET ACTIVATE DataSet10.
T-TEST GROUPS=PAIN('0' '1')
  /MISSING=ANALYSIS
  /VARIABLES=predictandcontrol unpredictableControl PredictNocontrol NoControlUnpredictable
  /ES DISPLAY(TRUE)
  /CRITERIA=CI(.95).

DATASET ACTIVATE DataSet11.
RECODE Pain ('No'='0') ('Yes'='1').
EXECUTE.

T-TEST GROUPS=Pain('0' '1')
  /MISSING=ANALYSIS
  /VARIABLES=predictandcontrol unpredictableControl PredictNocontrol NoControlUnpredictable
  /ES DISPLAY(TRUE)
  /CRITERIA=CI(.95).
