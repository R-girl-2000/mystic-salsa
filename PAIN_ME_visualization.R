library(ggplot2)
mpg
#displ is the engine displacement in liters.drv is the drive train, front wheel (f), rear wheel (r) or four wheel (4)
ggplot(mpg, aes(x = displ, y = hwy)) + 
  geom_point()


#graph PAINME from FOAM study
ggplot(PainME, aes(x = FOAMID, y = S1_1, color = PAIN))+ 
  geom_point()

#errors, lets fix them
library(tidyverse)
library(dplyr)
sapply(PainME, class)

# identify problem variable, reassign as numeric
PainME$PAIN<- as.numeric(PainME$PAIN)

#filter dataset by pain group
pain_group_s1.df <- filter(PainME,PAIN ==1 )
HC_s1.df <- filter(PainME,PAIN == 0)

#extract and save S1 values from each dataset
pain_group_s1<- pain_group_s1.df$S1_1
HC_s1<-HC_s1.df$S1_1

pain_group_s1
HC_s1

#calcualte average beta weight for S1 area 1 (90 voxels)
ave_pg_S1<- ave(pain_group_s1)
sd_pg_s1<- sd(pain_group_s1)
ave_hc_S1<- ave(HC_s1)
sd_hc_s1<- sd(HC_s1)
ave_pg_S1
#need to decide how to recombine these values with the df, or how to analyze a df. 

#install summary tools to look at descriptive statistics
install.packages('summarytools')
library(summarytools)

descriptives_pg<- descr(pain_group_s1.df)
descriptives_hc<- descr(HC_s1.df)

#try online code
descriptives<- PainME %>%
  group_by(PAIN)%>%
  filter(PAIN == 1) %>%
  summarise(average_s1_1 = mean(S1_1), SD_S1_1 = sd(S1_1)) 
 
  descriptives_2<-  PainME %>%
  group_by(PAIN)%>%
  filter(PAIN == 0) %>%
  summarise(average_s1_1 = mean(S1_1), SD_S1_1 = sd(S1_1))
  
descriptives_3<- data.frame(("pain"= c(descriptives$PAIN, descriptives_2$PAIN)) ,
             ("mean" = c(descriptives$average_s1_1, descriptives_2$average_s1_1)) ,
             ("SD" = c(descriptives$SD_S1_1, descriptives_2$SD_S1_1)))
descriptives_3

# disaster 

# try rbind, binds vectors as rows.
# ?rbind

descriptives_3<- rbind(descriptives, descriptives_2)
descriptives_3

#now try graphing again
average_barchart<- ggplot(descriptives_3, mapping = aes(x = PAIN, y = average_s1_1, color= "darkolivegreen")) +
  geom_col()
average_barchart

meta_plot<- ggplot(PainME, mapping = aes(group = PAIN, y= S1_1, color = "pink", outliers.color = "black" ))+ 
 geom_boxplot()
meta_plot


