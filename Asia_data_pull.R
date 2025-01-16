library(tidyverse)
library(summarytools)
library(rstatix)
library(ggpubr)
install.packages("lsr")
install.packages("psych")
library(lsr)
library(psych)

#use summarytools package to get descriptives for all vars

descr<- stby(
  data = DIRECTPull1_AW,
  INDICES = DIRECTPull1_AW$Diet,
  FUN = descr,
  stats = "common" #common descriptives
)
descr

# create difference scores between time 1 & 2 (visits 2 and 5) to test diet effects

DIRECTPull1_AW$weight_diff<- DIRECTPull1_AW$Weight1_V5 - DIRECTPull1_AW$Visit2Weight
DIRECTPull1_AW$BMD_diff<- DIRECTPull1_AW$TotalBoneDensityBMDV4 - DIRECTPull1_AW$TotalBoneDensityBMDbaseline
DIRECTPull1_AW$sf_36_pain_diff<- DIRECTPull1_AW$Visit5Pain - DIRECTPull1_AW$Visit2Pain
DIRECTPull1_AW$BPI_Sev_diff<- DIRECTPull1_AW$PainSeveritySubscaleV5 - DIRECTPull1_AW$Visit2PainSeveritySubscale
DIRECTPull1_AW$BPI_Int_diff <- DIRECTPull1_AW$PainInterferenceSubscaleV5 - DIRECTPull1_AW$Visit2PainInterferenceSubscale
DIRECTPull1_AW$TSK_Diff <- DIRECTPull1_AW$Visit5TotalScore_TSK - DIRECTPull1_AW$Visit2TotalScore_TSK
DIRECTPull1_AW$WOMAC_Tot_Diff <- DIRECTPull1_AW$WOMACTOTALSCORE96V5- DIRECTPull1_AW$Visit2WOMACTOTALSCORE96

#explore distribution of weight_diff by diet. 
DIRECTPull1_AW |>
  group_by(Diet) |> 
  shapiro_test(weight_diff)
ggqqplot(DIRECTPull1_AW, x = "weight_diff", facet.by ="Diet")

#warning message that 1 row contains non-finite outside the scale range. 

#identify weight outliers
DIRECTPull1_AW|> 
  group_by(Diet) |> 
  identify_outliers(weight_diff)
# see that it is recordsids 25 and 110, go find what row those are in the dataset. 
# they are rows 19 and 35. weight_diff data is inside columns 87. Pull all diff scores to see if there are any other outliers there. 

DIRECTPull1_AW[19,87:93] # 344lbs at v2, 126 at V5. -219lbs in 6 weeks. Definitely an error. 
DIRECTPull1_AW[35,87:93] #15 lb weight loss. 


#WOMAC outliers
DIRECTPull1_AW|> 
  group_by(Diet) |> 
  identify_outliers(WOMAC_Tot_Diff)
DIRECTPull1_AW[5,87:93] #record ID 6, row 5, -68 point womac difference

#tsk_outliers
DIRECTPull1_AW|> 
  group_by(Diet) |> 
  identify_outliers(TSK_Diff) #outliers recordid 47,88, 5.
DIRECTPull1_AW[4, c(1,87:93)] # -23 tsk
DIRECTPull1_AW[22,c(1,87:93)] # -27 tsk
DIRECTPull1_AW[30, c(1,87:93)] # - 10 tsk

#identify outliers bpi sev
DIRECTPull1_AW|> 
  group_by(Diet) |> 
  identify_outliers(BPI_Sev_diff) # record ids 62,67,88,159,6
DIRECTPull1_AW[5, c(1, 87:93)] #- 7 BPI sev
DIRECTPull1_AW[24, c(1,87:93)] # 2
DIRECTPull1_AW[27, c(1,87:93)] # -6
DIRECTPull1_AW[30, c(1,87:93)] # -3
DIRECTPull1_AW[40, c(1,87:93)] # 2

#identify outliers in BPI int. 

DIRECTPull1_AW|> 
  group_by(Diet) |> 
  identify_outliers(BPI_Int_diff) # record ids 12, 23, 98, 133
DIRECTPull1_AW[ 9 , c(1,87:93)]

#normality test
DIRECTPull1_AW |>
  group_by(Diet) |> 
  shapiro_test(weight_diff)
ggqqplot(DIRECTPull1_AW, x = "weight_diff", facet.by ="Diet")
# USDA group (diet 1) is not normal.

#t-test
weight.t.test<- DIRECTPull1_AW |> 
  t_test(weight_diff ~ Diet, var.equal = TRUE) |> 
  add_significance()
weight.t.test

BPI_sev.t.test<- DIRECTPull1_AW |> 
  t_test(BPI_Sev_diff ~ Diet, var.equal = TRUE) |> 
  add_significance()
BPI_sev.t.test

BPI_Int.t.test<- DIRECTPull1_AW |> 
  t_test(BPI_Int_diff ~ Diet, var.equal = TRUE) |> 
  add_significance()
BPI_Int.t.test

sf_36.t.test<- DIRECTPull1_AW |> 
  t_test(sf_36_pain_diff ~ Diet, var.equal = TRUE) |> 
  add_significance()
sf_36.t.test

BMD.t.test<- DIRECTPull1_AW |> 
  t_test(BMD_diff ~ Diet, var.equal = TRUE) |> 
  add_significance()
BMD.t.test

Womac_pain.t.test<- DIRECTPull1_AW |> 
  t_test(WOMAC_Tot_Diff ~ Diet, var.equal = TRUE) |> 
  add_significance()
Womac_pain.t.test

tsk.t.test<- DIRECTPull1_AW |> 
  t_test(TSK_Diff ~ Diet, var.equal = TRUE) |> 
  add_significance()
tsk.t.test

# try summarizing a different way

DIRECTPull1_AW |> 
  group_by(Diet) |> 
 reframe(
    weight_diff) |> 
  print(n =40)
#can see that the 9th person has a weight_diff of -219. 

