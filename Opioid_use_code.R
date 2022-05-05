#https://stats.idre.ucla.edu/r/seminars/survey-data-analysis-with-r/

library(ggplot2)
library(dplyr)
library(tidyverse)
library(Hmisc)
library(survey)
library(jtools)
library(sjPlot)
library(naniar)
library(broom)
library(sjPlot)
library(WeightedROC)
library(caret)
#https://cran.r-project.org/web/packages/naniar/vignettes/naniar-visualisation.html

library("readxl")
ndata=read.csv(file='F:/My Documents/MPH/Biostatistics_projects/2 Survey/adult20.csv')

#summary(as.factor(ndata$PAIFRQ3M_A))
#summary(as.factor(ndata$OPDCHRONIC_A))

#------------------data preparation----------------------
#filtering out refused/dont know/not ascertained from vars chronic pain and opioid use
data<-ndata%>%filter(PAIFRQ3M_A<6)
data<-data%>% filter(OPDCHRONIC_A<6 | is.na(OPDCHRONIC_A))

#creating variables
data$cpain[data$PAIFRQ3M_A<3 | data$PAIFRQ3M_A>4]<-0
data$cpain[data$PAIFRQ3M_A==3 | data$PAIFRQ3M_A==4]<-1
data$cpain<-factor(data$cpain, levels=c(0,1), labels=c("No", "Yes"))

data$opioid[data$OPDCHRONIC_A != 1]<-0
data$opioid[is.na(data$OPDCHRONIC_A)]<-0
data$opioid[data$OPDCHRONIC_A == 1]<-1
data$opioid<-factor(data$opioid, levels=c(0,1), labels=c("No", "Yes"))
#summary(data$opioid)

#visualizing missing data
#opdf<-data.frame(data[,c("OPD12M_A", "OPD3M_A", "OPDACUTE_A","OPDCHRONIC_A",   "OPDFREQ_A")])
#vis_miss(opdf)

#examining the missing data for opioids
summary(as.factor(data$OPDCHRONIC_A))
omis<-data%>% filter(is.na(OPDCHRONIC_A))#28,961 missing values
table(omis$cpain, omis$OPD12M_A)
#5938 of missing values have chronic pain
ocmis<-omis%>%filter(cpain=="Yes")
table(ocmis$OPD12M_A)#600 had opioid in the last 12 months and did not answer about chronic pain


#--------creating variables
#using logistic regression to compare the levels' estimates, and unify levels that have 
#similar estimates 
#--------------------------demographics
names(data)[names(data)=='AGEP_A']<-'age'

data$sex<-data$SEX_A-1
data$sex<-factor(data$sex, levels=c(0,1), labels=c("Male", "Female"))
label(data$sex)<-"Sex"

data$age65<-ifelse(data$age<65, 0, 1)
data$age65<-factor(data$age65, levels=c(0,1), labels=c("under 65", "65+"))
label(data$age65)<-"65+ years old"

#marital status
data$married[data$MARITAL_A==1 | data$MARITAL_A==2]<-1
data$married[data$MARITAL_A>2]<-0
data$married<-factor(data$married, levels=c(0,1), labels=c("No", "Yes"))
#summary(data$married)

#family income group
#data$income_grp<-as.factor(data$INCGRP_A)
#data$income_grp<-factor(data$income_grp, levels=c(1,2,3,4,5), 
 #                       labels=c("$0 to $34,999", "$35,000 to $49,999", "$50,000 to $74,999", "$75,000 to $99,999", "$100,000 or greater"))
#summary(data$income_grp)
#comparing the levels' estimates, and unify levels that have similar estimates
#m<-glm(cpain~income_grp, family=binomial, na.action=na.omit, data)
#summary(m)
#unifying levels 3+4
data$income_grp<-data$INCGRP_A
data$income_grp[data$INCGRP_A==4]<-3
data$income_grp[data$INCGRP_A==5]<-4
data$income_grp<-factor(data$income_grp, levels=c(1,2,3,4), 
                       labels=c("$0 to $34,999", "$35,000 to $49,999", "$50,000 to $99,999", "$100,000 or greater"))
#summary(data$income_grp)

#military service
data$veteran[data$AFVET_A==1]<-1
data$veteran[data$AFVET_A!=1]<-0
data$veteran<-factor(data$veteran, levels=c(0,1), labels=c("No", "Yes"))
#summary(data$veteran)

#----------------economic status
#health insurance coverage
data$uninsured[data$NOTCOV_A == 2]<-0 #insured
data$uninsured[data$NOTCOV_A != 2]<-1 #uninsured
data$uninsured<-factor(data$uninsured, levels=c(0,1), labels=c("No", "Yes"))
#summary(data$uninsured)

#problems paying medical bills
#summary(glm(cpain~as.factor(PAYBLL12M_A), family=binomial, na.action=na.omit, data))
#not ascertained, dont know and refused are not statistically significant, added no "no"
data$paybill[data$PAYBLL12M_A == 1]<-1 #having problems
data$paybill[data$PAYBLL12M_A == 2]<-0 #not having problems
data$paybill[data$PAYBLL12M_A == 7]<-0 
data$paybill[data$PAYBLL12M_A == 8]<-0 #not ascertained
data$paybill[data$PAYBLL12M_A == 9]<-0 
data$paybill<-factor(data$paybill, levels=c(0,1), labels=c("No", "Yes"))
#summary(data$paybill)

#worry about paying medical bills
#summary(glm(cpain~as.factor(PAYWORRY_A), family=binomial, na.action=na.omit, data))
#summary(glm(opioid~as.factor(PAYWORRY_A), family=binomial, na.action=na.omit, data))
#all levels have very similar estimates for chronic pain but different for opioid
#unifying levels 2+3 and creating a binary variable yes/no
data$payworry[data$PAYWORRY_A == 1]<-1
data$payworry[data$PAYWORRY_A == 2 |data$PAYWORRY_A == 3 | data$PAYWORRY_A == 9]<-0 #not worried
data$payworry[data$PAYWORRY_A == 7 ]<-NA #refused
data$payworry<-factor(data$payworry, levels=c(0, 1), labels=c("No", "Yes"))
#summary(data$payworry)

#worry that your food will run out in the last 30 days?
#summary(glm(cpain~as.factor(FDSRUNOUT_A), family=binomial, na.action=na.omit, data))
#summary(glm(opioid~as.factor(FDSRUNOUT_A), family=binomial, na.action=na.omit, data))
data$foodworry[data$FDSRUNOUT_A==1 | data$FDSRUNOUT_A==2]<-1 #worried
data$foodworry[data$FDSRUNOUT_A>2]<- 0#not worried
data$foodworry<-factor(data$foodworry, levels=c(0,1), labels=c("No", "Yes"))
#summary(data$foodworry)

#home ownership status
data$home_owner[data$HOUTENURE_A ==1]<-1 #home owner
data$home_owner[data$HOUTENURE_A!=1]<- 0#not home owner
data$home_owner<-factor(data$home_owner, levels=c(0,1), labels=c("No", "Yes"))
#summary(data$home_owner)


#------------------------disability and functioning
#do you have difficulty walking or climbing stairs?
data$mobility1[data$DIFF_A>4]<-1
data$mobility1[data$DIFF_A==1]<-1
data$mobility1[data$DIFF_A==2]<-2
data$mobility1[data$DIFF_A==3]<-3
data$mobility1[data$DIFF_A==4]<-3
data$mobility1<-factor(data$mobility1, levels=c(1,2,3), labels=c("No difficulty", "Some difficulty", "A lot of difficulty/unable"))
#summary(data$mobility1)
#summary(glm(cpain~mobility1, family=binomial, na.action=na.omit, data))
#summary(m)

#do you use equipment to get around?
data$mobility2[data$EQUIP_A != 1]<-0
data$mobility2[is.na(data$EQUIP_A)]<-0
data$mobility2[data$EQUIP_A == 1]<-1
data$mobility2<-factor(data$mobility2, levels=c(0,1), labels=c("No", "Yes"))
#summary(data$mobility2)

#communication
#unifying levels 3 and 4 due to a small number of observations is level 4 (28, 0.01%)
#data$communicating[is.na(data$COMDIFF_A0)]<-1
data$communicating[data$COMDIFF_A >4]<-1
data$communicating[data$COMDIFF_A ==1]<-1
data$communicating[data$COMDIFF_A ==2]<-2
data$communicating[data$COMDIFF_A ==3]<-3
data$communicating[data$COMDIFF_A ==4]<-3
data$communicating<-factor(data$communicating, levels=c(1,2,3), labels=c("No difficulty", "Some difficulty", "A lot of difficulty/unable"))
#summary(data$communicating)
#summary(glm(cpain~communicating, family=binomial, na.action=na.omit, data))


#do you have difficulty concentrating or remembering?
data$cognitive[data$COGMEMDFF_A >4]<-1
data$cognitive[data$COGMEMDFF_A==1]<-1
data$cognitive[data$COGMEMDFF_A==2]<-2
data$cognitive[data$COGMEMDFF_A==3]<-3
data$cognitive[data$COGMEMDFF_A==4]<-3
data$cognitive<-factor(data$cognitive, levels=c(1,2,3), labels=c("No difficulty", "Some difficulty", "A lot of difficulty/unable"))
#summary(data$cognitive)
#summary(glm(cpain~cognitive, family=binomial, na.action=na.omit, data))

#are you limited in the kind or amount of work you can do?
data$work_limited[data$SOCWRKLIM_A != 1]<-0
data$work_limited[is.na(data$SOCWRKLIM_A)]<-0
data$work_limited[data$SOCWRKLIM_A == 1]<-1
data$work_limited<-factor(data$work_limited, levels=c(0,1), labels=c("No", "Yes"))
#summary(data$work_limited)


#do you have difficulty participating in social activity?
data$participation[data$SOCSCLPAR_A >4]<-1
data$participation[data$SOCSCLPAR_A==1]<-1
data$participation[data$SOCSCLPAR_A==2]<-2
data$participation[data$SOCSCLPAR_A==3]<-3
data$participation[data$SOCSCLPAR_A==4]<-3
data$participation<-factor(data$participation, levels=c(1,2,3), labels=c("No difficulty", "Some difficulty", "A lot of difficulty/unable"))
#summary(data$participation)
#summary(glm(cpain~participation, family=binomial, na.action=na.omit, data))

#Days missed work in the past 12 months due to illness/injury/disability
#98.37% missed between 0-129 days
#only 0.51% , 100 respondents missed more than 130 days
#this variable has highly unbalanced classes and is not sensitive enough

#Worked last week
data$work_lastwk[data$EMPWRKLSWK_A != 1]<-0
data$work_lastwk[is.na(data$EMPWRKLSWK_A)]<-0
data$work_lastwk[data$EMPWRKLSWK_A == 1]<-1
data$work_lastwk<-factor(data$work_lastwk, levels=c(0,1), labels=c("No", "Yes"))
#summary(data$work_lastwk)
#summary(glm(cpain~work_lastwk, family=binomial, na.action=na.omit, data))

#-----------------------Health

#fatigue- In the last 3 months- how often did you feel very tired or exhausted?
data$fatigue[data$FGEFRQTRD_A==1]<-1
data$fatigue[data$FGEFRQTRD_A==2]<-2
data$fatigue[data$FGEFRQTRD_A==3]<-3
data$fatigue[data$FGEFRQTRD_A==4]<-3
data$fatigue[data$FGEFRQTRD_A >4]<-2
data$fatigue<-factor(data$fatigue, levels=c(1,2,3), labels=c("Never", "Some days", "Most days/Every day"))
#summary(data$fatigue)
#summary(glm(cpain~fatigue, family=binomial, na.action=na.omit, data))
#summary(m)

data$health[data$PHSTAT_A==1]<-1
data$health[data$PHSTAT_A==2]<-2
data$health[data$PHSTAT_A==3]<-3
data$health[data$PHSTAT_A==4]<-4
data$health[data$PHSTAT_A==5]<-5
data$health[data$PHSTAT_A >5]<-2
data$health<-factor(data$health, levels=c(1,2,3,4,5), labels=c("Excellent", "very good", "Good", "Fair", "Poor"))
#summary(data$health)
#summary(glm(cpain~health, family=binomial, na.action=na.omit, data))

#accident or injury in the past 3 months
data$injury[is.na(data$ANYINJURY_A)]<-0 #no injury 
data$injury[data$ANYINJURY_A == 1]<-1 #had injury
data$injury[data$ANYINJURY_A != 1]<-0 #no injury 
data$injury<-factor(data$injury, levels=c(0,1), labels=c("No", "Yes"))
#summary(data$injury)

#when was the last time you saw a doctor
#178 missing
#data$last_dr[data$LASTDR_A >3]<-4
#data$last_dr[data$LASTDR_A ==0]<-4
#data$last_dr[data$LASTDR_A==1]<-1
#data$last_dr[data$LASTDR_A==2]<-2
#data$last_dr[data$LASTDR_A==3]<-3
#data$last_dr<-factor(data$last_dr, levels=c(1,2,3,4), labels=c("Less than 12 months", "1-2 years ago", "2-3 years ago", "Over 3 years ago"))
#summary(data$last_dr)
#comparing the levels' estimates, and unify levels that have similar estimates
#m<-glm(cpain~as.factor(last_dr), family=binomial, na.action=na.omit, data)
#summary(m)
#unify all levels (2+3+4) except the first
#have you seen a doctor in the last 12 months? yes/no
data$dr_12months[data$LASTDR_A==1]<-1
data$dr_12months[data$LASTDR_A !=1]<-0
data$dr_12months<-factor(data$dr_12months, levels=c(0,1), labels=c("No", "Yes"))
#summary(data$dr_12months)

#was this a wellness visit?
#224 missing
data$wellness[data$WELLNESS_A == 1]<-1 #wellness visit
data$wellness[data$WELLNESS_A != 1]<-0 
data$wellness[is.na(data$wellness)]<-0 
data$wellness<-factor(data$wellness, levels= c(0,1), labels=c("No", "Yes"))
#summary(data$wellness)



#how many times have you had urgent care in the past 12 months?
#224 missing
#data$urgent_care[data$URGNT12MTC_A ==0]<-0
#data$urgent_care[data$URGNT12MTC_A==1]<-1
#data$urgent_care[data$URGNT12MTC_A==2]<-2
#data$urgent_care[data$URGNT12MTC_A==3]<-2
#data$urgent_care[data$URGNT12MTC_A==4]<-3
#data$urgent_care[data$URGNT12MTC_A==5]<-3
#data$urgent_care[data$URGNT12MTC_A >5]<-0
#data$urgent_care<-factor(data$urgent_care, levels=c(0,1,2,3), labels=c("0 times", "1 time", "2-3 times", "4+ times"))
#summary(data$urgent_care)
#comparing the levels' estimates, and unify levels that have similar estimates
#m<-glm(cpain~as.factor(urgent_care), family=binomial, na.action=na.omit, data)
#summary(m)
#level 1 is not significant, unify it with 0
data$urgent_care[data$URGNT12MTC_A ==0]<-0
data$urgent_care[data$URGNT12MTC_A==1]<-0
data$urgent_care[data$URGNT12MTC_A==2]<-1
data$urgent_care[data$URGNT12MTC_A==3]<-1
data$urgent_care[data$URGNT12MTC_A==4]<-2
data$urgent_care[data$URGNT12MTC_A==5]<-2
data$urgent_care[data$URGNT12MTC_A >5]<-0
data$urgent_care<-factor(data$urgent_care, levels=c(0,1,2), labels=c("0-1 time", "2-3 times", "4+ times"))


#were you hospitalized overnight in the past 12 months?
data$hospitalized[data$HOSPONGT_A == 1]<-1 #hospitalized
data$hospitalized[data$HOSPONGT_A != 1]<-0 
data$hospitalized<-factor(data$hospitalized, levels= c(0,1), labels=c("No", "Yes"))
#summary(data$hospitalized)

#during the past 12 months have you had delayed medical care because of the cost?
#176 missing
data$caredelay[data$MEDDL12M_A == 1]<-1 #delayed care
data$caredelay[data$MEDDL12M_A != 1]<-0 
data$caredelay<-factor(data$caredelay, levels= c(0,1), labels=c("No", "Yes"))
#summary(data$caredelay)

#have you had a flu vaccine in the past 12 months?
data$flu_vaccine[data$SHTFLU12M_A == 1]<-1 #
data$flu_vaccine[data$SHTFLU12M_A != 1]<-0 
data$flu_vaccine<-factor(data$flu_vaccine, levels= c(0,1), labels=c("No", "Yes"))
#summary(data$flu_vaccine)

#--------------- mental health
#anxiety
#how often do you feel worried, nervous or anxious
#483 missing
data$anx_freq[data$ANXFREQ_A==1]<-1
data$anx_freq[data$ANXFREQ_A==2]<-2
data$anx_freq[data$ANXFREQ_A==3]<-3
data$anx_freq[data$ANXFREQ_A==4]<-4
data$anx_freq[data$ANXFREQ_A==5]<-5
data$anx_freq[data$ANXFREQ_A >5]<-4
#data$anx_freq<-factor(data$anx_freq, levels=c(1,2,3,4,5), 
 #                     labels=c("Daily", "Weekly", "Monthly", "A few times a year", "Never"))
#summary(data$anx_freq)
#comparing the levels' estimates, and unify levels that have similar estimates
#summary(glm(cpain~anx_freq, family=binomial, na.action=na.omit, data))
#unify  levels for more than weekly (3+4+5) and levels daily+weekly (1+2)
data$anx_freq[data$ANXFREQ_A==1]<-1
data$anx_freq[data$ANXFREQ_A==2]<-1
data$anx_freq[data$ANXFREQ_A>2]<-0
data$anx_freq<-factor(data$anx_freq, levels=c(0,1), 
                      labels=c("Less often than weekly", "Daily or weekly"))

#Do you take prescription medication for worried/nervous/anxious feelings?
#384 missing
data$anx_med[data$ANXMED_A == 1]<-1 #medication
data$anx_med[data$ANXMED_A != 1]<-0 #no medication
data$anx_med<-factor(data$anx_med, levels= c(0,1), labels=c("No", "Yes"))
#summary(data$anx_med)

#level of feelings the last time you felt worried/nervous/anxious
#9676 missing values, 30%, entered as an additional level 0
data$anx_level[is.na(data$ANXLEVEL_A)]<-0
data$anx_level[data$ANXLEVEL_A ==1]<-1
data$anx_level[data$ANXLEVEL_A==3]<-2
data$anx_level[data$ANXLEVEL_A==2]<-3
data$anx_level[data$ANXLEVEL_A>3]<-1
#data$anx_level<-factor(data$anx_level, levels=c(0, 1,2,3), 
#                      labels=c("No worry", "A little", Between a little and a lot", "A lot"))
#summary(data$anx_level)
#summary(glm(cpain~anx_level, family=binomial, na.action=na.omit, data))
#unify levels "a little" and "between a little and a lot" (1+2)
data$anx_level[is.na(data$ANXLEVEL_A)]<-0 #no worry
data$anx_level[data$ANXLEVEL_A ==1|data$ANXLEVEL_A ==3]<-1 #between a little and a lot
data$anx_level[data$ANXLEVEL_A==2]<-2 #a lot
data$anx_level[data$ANXLEVEL_A>3]<-1
data$anx_level<-factor(data$anx_level, levels=c(0, 1,2), 
                       labels=c("No worry", "Between a little and a lot", "A lot"))

#----depression
#how often do you feel depressed
data$depr_freq[data$DEPFREQ_A ==1]<-1
data$depr_freq[data$DEPFREQ_A==2]<-2
data$depr_freq[data$DEPFREQ_A==3]<-3
data$depr_freq[data$DEPFREQ_A==4]<-4
data$depr_freq[data$DEPFREQ_A==5]<-5
data$depr_freq[data$DEPFREQ_A >5]<-5
#data$depr_freq<-factor(data$depr_freq, levels=c(1,2,3,4,5), 
 #                     labels=c("Daily", "Weekly", "Monthly", "A few times a year", "Never"))
#summary(data$depr_freq)
#summary(glm(cpain~as.factor(depr_freq), family=binomial, na.action=na.omit, data))
#summary(m)
#unify levels for less often than weekly(3+4+5) and daily+weekly (1+2)
data$depr_freq[data$DEPFREQ_A>2]<-0
data$depr_freq[data$DEPFREQ_A==2 |data$DEPFREQ_A==1]<-1
data$depr_freq<-factor(data$depr_freq, levels=c(0,1), 
                      labels=c("Less often than weekly", "Daily or weekly"))

#Do you take prescription medication for depression?
data$depr_med[data$DEPMED_A == 1]<-1 #medication
data$depr_med[data$DEPMED_A != 1]<-0 #no medication
data$depr_med<-factor(data$depr_med, levels= c(0,1), labels=c("No", "Yes"))
#summary(data$depr_med)

#level of feelings the last time you felt depressed
#17223 missing values, 54% entered as an additional level 0
data$depr_level[is.na(data$DEPLEVEL_A)]<-0
data$depr_level[data$DEPLEVEL_A==1]<-1
data$depr_level[data$DEPLEVEL_A==3]<-2
data$depr_level[data$DEPLEVEL_A==2]<-3
data$depr_level[data$DEPLEVEL_A>3]<-1
#data$depr_level<-factor(data$depr_level, levels=c(0, 1,2,3), 
 #                      labels=c("No depression", "A little", "Between a little and a lot", "A lot"))
#summary(data$depr_level)
#summary(glm(cpain~as.factor(depr_level), family=binomial, na.action=na.omit, data))
##unify levels "a little" and "between a little and a lot" (1+2)
data$depr_level[is.na(data$DEPLEVEL_A)]<-0
data$depr_level[data$DEPLEVEL_A==1 | data$DEPLEVEL_A==2]<-1
data$depr_level[data$DEPLEVEL_A==3]<-2
data$depr_level[data$DEPLEVEL_A>3]<-1
data$depr_level<-factor(data$depr_level, levels=c(0, 1,2), 
                       labels=c("No depression", "Between a little and a lot", "A lot"))
#summary(data$depr_level)

#In the last 12 months, have you taken prescription medication for your mental health?
#missing data: 410. 
#data$mhrx[is.na(data$DEPMED_A)]<-0
data$mhrx[data$MHRX_A == 1]<-1 #medication
data$mhrx[data$MHRX_A  != 1]<-0 #no medication
data$mhrx[is.na(data$MHRX_A)]<-0 
data$mhrx<-factor(data$mhrx, levels= c(0,1), labels=c("No", "Yes"))
#summary(data$mhrx)

#in the past 12 months have you delayed getting counseling because of the cost?
#392 missing
data$counseldelay[data$MHTHDLY_A == 1]<-1 #delayed care
data$counseldelay[data$MHTHDLY_A != 1]<-0
data$counseldelay[is.na(data$MHTHDLY_A)]<-0 
data$counseldelay<-factor(data$counseldelay, levels= c(0,1), labels=c("No", "Yes"))
#summary(data$counseldelay)

#in the past 12 months have you forgone counseling because of the cost?
#nearly identical to counseldelay
#data$counselabstain[data$MHTHND_A == 1]<-1 #forgone counselling
#data$counselabstain[data$MHTHND_A != 1]<-0 
#data$counselabstain<-factor(data$counselabstain, levels= c(0,1), labels=c("No", "Yes"))
#summary(data$counselabstain)

#-------------------lifestyle
#have you smoked at least 100 cigarettes in your lifetime?
#582 missing, 2%
data$cig100[data$SMKEV_A == 1]<-1 #smoker
data$cig100[data$SMKEV_A != 1]<-0 
data$cig100<-factor(data$cig100, levels= c(0,1), labels=c("No", "Yes"))
#summary(data$cig100)

#have you had at least one alcoholic drink in your lifetime?
#600 missing, 2%
data$onedrink[data$DRKLIFE_A == 1]<-1 #drinker
data$onedrink[data$DRKLIFE_A != 1]<-0 
data$onedrink<-factor(data$onedrink, levels= c(0,1), labels=c("No", "Yes"))
#summary(data$onedrink)
#summary(glm(cpain~onedrink,family=binomial, na.action=na.omit, cdata))
#est=0.34, intercept = -1.5


#days drank alcohol in the past 12 months
#missing: 3943, 12.5%
#refused/not ascertain or out-of-range were set to the median=2
data$drinks_days<-data$DRK12MN1_A
data$drinks_days[is.na(data$DRK12MN1_A)]<-0
data$drinks_days[data$DRK12MN1_A>365]<-2
#summary(data$drinks_days)
#summary(glm(opioid~drinks_days,family=binomial, na.action=na.omit, data))
#summary(glm(cpain~drinks_days,family=binomial, na.action=na.omit, data))
#non significant predictor, tested on the unweighted data

#how many drinks in average per occasion (in the past 12 months)?
#missing values=9974, 31.6%
#continuous
#refused/not ascertain or out-of-range were set to the median=2
#data$drinks_per_occasion<-data$DRKAVG12M_A
#data$drinks_per_occasion[data$DRKAVG12M_A>20]<-NA
#summary(glm(cpain~drinks_per_occasion,family=binomial, na.action=na.omit, data))
#est=0.02, intercept -1.34, weak predictor

#heavy alcohol drinking DRKHVY12M_A
#10,009 missing values, 32%
#data$drink_heavy<-data$DRKHVY12M_A
data$drink_heavy[is.na(data$DRKHVY12M_A)]<-0
data$drink_heavy[data$DRKHVY12M_A==1]<-1
data$drink_heavy[data$DRKHVY12M_A!=1]<-0
data$drink_heavy<-factor(data$drink_heavy, levels= c(0,1), labels=c("No", "Yes"))
summary(data$drink_heavy)
#summary(glm(opioid~drink_heavy,family=binomial, na.action=na.omit, data))
#summary(glm(cpain~drink_heavy,family=binomial, na.action=na.omit, data))
#significant predictor, tested on the unweighted data

#physical activity
#not ascertained= 1045, coded as 8
#summary(glm(cpain~as.factor(PA18_02R_A), family=binomial, na.action=na.omit, data))
#estimates are very close to intercept and each other. 
#not ascertained is closest to "insufficient" (8+2)
data$sport[data$PA18_02R_A==1]<-1
data$sport[data$PA18_02R_A==2]<-1
data$sport[data$PA18_02R_A==3]<-3
data$sport[data$PA18_02R_A>3]<-2
data$sport<-factor(data$sport, levels=c(1,2,3), 
               labels=c("Inactive","Insufficiently active", "Sufficiently active"))
#summary(data$sport)

#summary(glm(cpain~as.factor(sport), family=binomial, na.action=na.omit, data))
#all levels have approximately the same est, this is not a good predictor


#sunscreen- how often do you use sunscreen
#dont go outside on a sunny day for more than 1 hour was added to "always"
#not ascertained added to 
#dont know/refused added to "never" (largest and most probable choice)
#missing 820
  data$sunscreen[data$SUNSCREEN_A==1]<-1
  data$sunscreen[data$SUNSCREEN_A==2]<-2
  data$sunscreen[data$SUNSCREEN_A==3]<-3
  data$sunscreen[data$SUNSCREEN_A==4]<-4
  data$sunscreen[data$SUNSCREEN_A==5]<-5
  data$sunscreen[data$SUNSCREEN_A==7 | data$SUNSCREEN_A==9]<-5 #dont know/refused added to "never"
  data$sunscreen[data$SUNSCREEN_A==6]<-1
  #data$sunscreen<-factor(data$sunscreen, levels=c(1,2,3,4,5), 
   #                      labels=c("Always", "Most of the time", "Sometimes", "Rarely", "Never"))
  #summary(data$sunscreen)
  #summary(glm(cpain~as.factor(sunscreen), family=binomial, na.action=na.omit, data))
  #summary(m)
  #unify levels most of the time+sometimes(2+3)
  data$sunscreen[data$SUNSCREEN_A==1]<-1
  data$sunscreen[data$SUNSCREEN_A==2]<-2
  data$sunscreen[data$SUNSCREEN_A==3]<-2
  data$sunscreen[data$SUNSCREEN_A==4]<-3
  data$sunscreen[data$SUNSCREEN_A==5]<-3
  data$sunscreen[data$SUNSCREEN_A==7 | data$SUNSCREEN_A==9]<-3 #dont know/refused added to "rarely/never"
  data$sunscreen[data$SUNSCREEN_A==6]<-2
  data$sunscreen<-factor(data$sunscreen, levels=c(1,2,3), 
                         labels=c("Always", "Sometimes", "Rarely or never"))



#diabetes prevention
#responses are inconsistent: only 24% said their doctor advised increasing physical activity,
#yet 42% said they increased their physical activity. same goes for reducing 
#fat or calories. 5.81% were advised to join a weight loss program, and 5.65% did.
#either there is under-reporting of doctors advising weight loss program (which seems likely, the morbid obesity rate in the USA
#is much higher than 6%) or an over-reporting of joining the weight loss program.
#we aggregated doctor's recommendations in regard to diabetes prevention, and 
#created a binary predictor: if a doctor advised on at least one way to prevent diabetes
#we considered it as having a risk factor for diabetes.
#missing=932
data$dm_pa<-data$ADVACTIVE_A
data$dm_pa[data$dm_pa>2]<-2
data$dm_cal<-data$ADVEAT_A
data$dm_cal[data$dm_cal>2]<-2
data$dm_pro<-data$ADVWGTPRG_A
data$dm_pro[data$dm_pro>2]<-2

data$DM_risk<-data$dm_pa+data$dm_cal+data$dm_pro
data$DM_risk[data$DM_risk<6]<-1
data$DM_risk[data$DM_risk==6]<-0
data$DM_risk<-factor(data$DM_risk, levels= c(0,1), labels=c("No", "Yes"))
#summary(data$DM_risk)
#summary(glm(cpain~DM_risk, family=binomial, na.action=na.omit, data))

#sleep
#how many hours do you sleep at night
data$sleep<-data$SLPHOURS_A
data$sleep[data$sleep>24]<-7
#hist(data$sleep)
#summary(data$sleep)
#ggplot(data, aes(x=sleep))+geom_histogram(binwidth = 0.5)
#eliminating outliers
data$sleep[data$sleep>12]<-7
#summary(glm(cpain~sleep, family=binomial, na.action=na.omit, data))
#summary(m)
#est=-0.13, intercept=-0.16
#creating a binary variable for normal (6-8 hours) and abnormal sleep
data$sleep_abnorm<-ifelse((data$sleep<6|data$sleep>8), 1,0)
data$sleep_abnorm<-factor(data$sleep_abnorm, levels= c(0,1), labels=c("No", "Yes"))
#summary(glm(cpain~sleep_abnorm, family=binomial, na.action=na.omit, data))
#est=0.73, intercept=-1.29

#sleep medication
#956 missing
data$sleep_med[data$SLPMED_A==1]<-1
data$sleep_med[data$SLPMED_A==2]<-2
data$sleep_med[data$SLPMED_A==3]<-3
data$sleep_med[data$SLPMED_A==4]<-3
data$sleep_med[data$SLPMED_A>4]<-1
data$sleep_med<-factor(data$sleep_med, levels=c(1,2,3), 
                       labels=c("Never", "Some days", "Most days/every day"))
#summary(data$sleep_med)
#comparing the levels' estimates, and unify levels that have similar estimates
#summary(glm(cpain~sleep_med, family=binomial, na.action=na.omit, data))
#est=0.51, 1.24
#summary(glm(opioid~sleep_med, family=binomial, na.action=na.omit, data))
#est=0.87, 1.67
#all levels have different estimates

#data$sleep_m[data$SLPMED_A == 1]<-0 #not using sleep med
#data$sleep_m[data$SLPMED_A != 1]<-1 
#data$sleep_m[is.na(data$SLPMED_A)]<-0
#data$sleep_m<-factor(data$sleep_m, levels= c(0,1), labels=c("No", "Yes"))
#summary(glm(cpain~sleep_m, family=binomial, na.action=na.omit, data))
#est=0.84
#summary(glm(opioid~sleep_m, family=binomial, na.action=na.omit, data))
#est=1.2
#not better than sleep_med

#social support
#how often do you get the emotional support you need?
#missing data- 14520!!! for both variables
#refused/not ascertained/dont know- usually
#13856 missing values
#summary(glm(cpain~as.factor(support), family=binomial, na.action=na.omit, data))
#unify 1+2, missing values recoded as an additional level
data$support[data$SUPPORT_A==1]<-1
data$support[data$SUPPORT_A==2]<-1
data$support[data$SUPPORT_A==3]<-2
data$support[data$SUPPORT_A==4]<-3
data$support[data$SUPPORT_A==5]<-4
data$support[data$SUPPORT_A>5 | is.na(data$SUPPORT_A)]<-5
data$support<-factor(data$support, levels=c(1,2,3,4,5), 
                     labels=c("Always/Usually", "Sometimes", "Rarely", "Never", "Missing"))
#summary(data$support)
#comparing the levels' estimates, and unify levels that have similar estimates
#summary(glm(cpain~support, family=binomial, na.action=na.omit, data))
#summary(m)


label(data$cpain)<-"Chronic pain"
label(data$opioid)<-"Opioid use"
label(data$age)<-"Age (years)"
label(data$married)<-"Married"
label(data$income_grp)<-"Yearly income"
label(data$veteran)<-"Military service"
label(data$home_owner)<-"Home owner"
label(data$uninsured)<-"uninsured"
label(data$health)<-"Health "
label(data$hospitalized)<-"Hospitalized in the past 12 months"
label(data$DM_risk)<-"At risk for Diabetes"
label(data$dr_12months)<-"Seen a doctor in the past 12 months"
label(data$mobility1)<-"Walking/climbing stairs"
label(data$mobility2)<-"Using equipment for mobility"
label(data$paybill)<-"Difficulty paying medical bills"
label(data$payworry)<-"Worry about paying medical bills"
label(data$foodworry)<-"Worry food will run out"
label(data$caredelay)<-"delayed care due to financial difficulty"
label(data$counseldelay)<-"delayed counsel due to financial difficulty"
label(data$communicating)<-"Communication "
label(data$cognitive)<-"Cognition "
label(data$work_limited)<-"Work limited by pain "
label(data$participation)<-"Social activity "
label(data$work_lastwk)<-"Worked last week"
label(data$fatigue)<-"Fatigue "
label(data$injury)<-"Injury"
label(data$wellness)<-"Had a wellness doctor's visit"
label(data$urgent_care)<-"Urgent care in the past year "
label(data$flu_vaccine)<-"Got a flu vaccine"
label(data$cig100)<-"Smoked at least 100 cigarettes in a lifetime"
label(data$onedrink)<-"Had at leat one drink in a lifetime"
label(data$sport)<-"Physical activity"
label(data$sunscreen)<-"Use sunscreen "
label(data$sleep_abnorm)<-"Abnormal amount of sleep"
label(data$sleep_med)<-"Use sleep medication "
label(data$anx_freq)<-"Anxiety "
label(data$anx_med)<-"Anxiety medication"
label(data$anx_level)<-"Anxiety level "
label(data$depr_freq)<-"Depression "
label(data$depr_med)<-"Depression medication"
label(data$depr_level)<-"Depression level"
label(data$mhrx)<-"Other mental health medication"

#sample description
library(table1)
table1(~ opioid+sex+age+age65+married+income_grp+home_owner+uninsured+veteran+
         health+hospitalized+DM_risk+ mobility1+mobility2
       | cpain, data=data, overall="Total", caption="Demographic and clinical characteristics by chronic pain")


#creating a data frame for model building
#with technical variables, outcome variables and predictors 
vars<-c("PPSU", "PSTRAT",  "WTFA_A", "cpain", "opioid", 
        "sex", "age", "age65", "married", "veteran","income_grp", "home_owner",
        "uninsured", "paybill", "payworry", "foodworry", "caredelay",
        "counseldelay",
        "mobility1", "mobility2", "communicating", "cognitive", "work_limited",
        "participation",  "work_lastwk", 
        "health","fatigue", "injury", "dr_12months", "wellness", "urgent_care", "hospitalized",
        "flu_vaccine", "cig100", "onedrink", "drink_heavy", "sport", "sunscreen", "DM_risk", 
        "sleep_abnorm", "sleep_med", "support", "anx_freq", "anx_med", "anx_level",  "depr_freq", "depr_med", "depr_level", "mhrx")

cdata<-data[,vars]%>%filter(!is.na(data$cpain))

#checking correlations between predictors, dividing the list for convenient visual display
vars1<-c("sex", "age", "age65", "married", "veteran","income_grp", "home_owner",
         "uninsured", "paybill", "payworry", "foodworry", "caredelay",
         "counseldelay",
         "mobility1", "mobility2", "communicating", "cognitive", "work_limited",
         "participation",  "work_lastwk")
vars2<-c("health","fatigue", "injury", "dr_12months", "wellness", "urgent_care", "hospitalized",
         "flu_vaccine", "cig100", "onedrink", "drink_heavy", "sport", "sunscreen", "DM_risk", 
         "sleep_abnorm", "sleep_med", "support", "anx_freq", "anx_med", "anx_level",  "depr_freq", "depr_med", "depr_level", "mhrx")
df<-data[,vars2]

library(ggcorrplot)
model.matrix(~0+., data=df) %>% 
  cor(use="pairwise.complete.obs") %>% 
  ggcorrplot(show.diag = F, type="lower", lab=TRUE, lab_size=2)
#no correlations above 0.65, mostly between levels of the same variable

#creating a survey design
options( survey.lonely.psu = "adjust" )
nhis <- svydesign(id =~PPSU, strata =~PSTRAT,  nest=TRUE, weights=~WTFA_A, data = cdata)

#--------------------descriptive statistics
#continuous variables: age
svymean(~ age, nhis)
svysd(~age,design = nhis, na = TRUE)
svyquantile(~age, design = nhis, na = TRUE, c(.25,.5,.75),ci=TRUE)
svyby(~age, ~cpain, nhis, svymean)

#plots
svyhist(~age, nhis)
svyboxplot(~age~1, nhis, all.outliers=TRUE)
svyboxplot(~age~opioid, nhis, all.outliers=TRUE)

#binary variables
svymean(~sexn, nhis)
#cross-tabulation, 2-way table
svytable(~cpain+sex, design = nhis)
#3 way
svytable(~interaction(sex, uninsured, cpain), design = nhis)
#chi-squared test

#categorical variables
svyciprop(~I(income_grp=="$0 to $34,999"), nhis, method="mean")
svyciprop(~I(income_grp=="$35,000 to $49,999"), nhis, method="mean")


barplt<-svyby(~opioid, ~age, nhis, na = TRUE, svymean)
barplot(barplt,beside=TRUE,legend=TRUE)

barplt<-svyby(~fatigue, ~sex, nhis, na = TRUE, svymean)
barplot(barplt,beside=TRUE,legend=TRUE)
#women suffer more fatigue

#chisquare/logistic regression for categorical variables
svychisq(~sex+cpain, nhis, statistic="adjWald")
svytable(~cpain+sex, design = nhis)
#women are more likely to suffer chronic pain, the risk is increased by 17%

#--------------------------------------Prediction model for Chronic pain----------------
#testing all predictors vs. cpain
#with svychisq or a logistic regression model to identify significant 
#predictors

#t-test
svyttest(age~0, nhis, na = TRUE)
#age mean is 48 years
svyttest(age~sex, nhis, na = TRUE)
#women's mean age is 1.4 years higher than men's and the difference is statistically significant
#probably due to the large sample size
summary(svyglm(age~cpain, design=nhis))
#significant, chronic pain sufferers are 9 years older
summary(svyglm(age~opioid, design=nhis))
##significant, opioid users are 12 years older

#table output for survey
#https://r-survey.r-forge.r-project.org/survey/html/ftable.svystat.html
#https://r-survey.r-forge.r-project.org/survey/html/surveysummary.html

#--------------testing associations of predictors and chronic pain
#t test and logistic regression for continuous variables
svyttest(age~cpain, nhis, na = TRUE)
model <- svyglm(cpain~age, family=quasibinomial, design=nhis, na.action = na.omit)
summary(model)

summary(svyglm(opioid~sleep_abnorm, family=quasibinomial, design=nhis, na.action = na.omit))
#significant

#opioid+
#  health+fatigue+injury+last_dr+wellness+urgent_care+hospitalized+
#  flu_vaccine+cig100+PA+sunscreen+DM_risk+sleep+sleep_med+
#  support+anx_freq+anx_med+anx_level+depr_freq+depr_med+depr_level+mhrx

#demographics
model1 <- svyglm(cpain ~ sex, design = nhis, family = quasibinomial, na.action = na.omit)
model4 <- svyglm(cpain ~ income_grp, design = nhis, family = quasibinomial, na.action = na.omit)
model3 <- svyglm(cpain ~ age65, design = nhis, family = quasibinomial, na.action = na.omit)
model2 <- svyglm(cpain ~ married, design = nhis, family = quasibinomial, na.action = na.omit)
model6 <- svyglm(cpain ~ veteran, design = nhis, family = quasibinomial, na.action = na.omit)
model5 <- svyglm(cpain ~ home_owner, design = nhis, family = quasibinomial, na.action = na.omit)
tidy1 <- tidy(model1, exponentiate=TRUE,conf.int = TRUE)[-1, -c(3, 4)]
tidy2 <- tidy(model2, exponentiate=TRUE,conf.int = TRUE)[-1, -c(3, 4)]
tidy3 <- tidy(model3, exponentiate=TRUE,conf.int = TRUE)[-1, -c(3, 4)]
tidy4 <- tidy(model4, exponentiate=TRUE,conf.int = TRUE)[-1, -c(3, 4)]
tidy5 <- tidy(model5, exponentiate=TRUE,conf.int = TRUE)[-1, -c(3, 4)]
tidy6 <- tidy(model6, exponentiate=TRUE,conf.int = TRUE)[-1, -c(3, 4)]
missing<-c(31124-length(model1$residuals),  31124-length(model2$residuals), 31124-length(model3$residuals),
           31124-length(model4$residuals),31124-length(model4$residuals), 31124-length(model4$residuals), 31124-length(model5$residuals),31124-length(model6$residuals))
knitr::kable(cbind(bind_rows(tidy1, tidy2, tidy3, tidy4, tidy5, tidy6), missing), digits=2,
             caption="Demographic characteristics")

#financial concerns
model1 <- svyglm(cpain ~ uninsured, design = nhis, family = quasibinomial, na.action = na.omit)
model2 <- svyglm(cpain ~ paybill, design = nhis, family = quasibinomial, na.action = na.omit)
model3 <- svyglm(cpain ~ payworry, design = nhis, family = quasibinomial, na.action = na.omit)
model4 <- svyglm(cpain ~ caredelay, design = nhis, family = quasibinomial, na.action = na.omit)
model5 <- svyglm(cpain ~ foodworry, design = nhis, family = quasibinomial, na.action = na.omit)
model6 <- svyglm(cpain ~ counseldelay, design = nhis, family = quasibinomial, na.action = na.omit)
tidy1 <- tidy(model1, exponentiate=TRUE,conf.int = TRUE)[-1, -c(3, 4)]
tidy2 <- tidy(model2, exponentiate=TRUE,conf.int = TRUE)[-1, -c(3, 4)]
tidy3 <- tidy(model3, exponentiate=TRUE,conf.int = TRUE)[-1, -c(3, 4)]
tidy4 <- tidy(model4, exponentiate=TRUE,conf.int = TRUE)[-1, -c(3, 4)]
tidy5 <- tidy(model5, exponentiate=TRUE,conf.int = TRUE)[-1, -c(3, 4)]
tidy6 <- tidy(model6, exponentiate=TRUE,conf.int = TRUE)[-1, -c(3, 4)]
missing<-c(31124-length(model1$residuals),  31124-length(model2$residuals), 31124-length(model3$residuals),31124-length(model4$residuals),31124-length(model5$residuals),31124-length(model6$residuals))
knitr::kable(cbind(bind_rows(tidy1, tidy2, tidy3, tidy4, tidy5, tidy6), missing), digits=2,
             caption="Financial concerns")

#disability and participation
model3 <- svyglm(cpain ~ mobility1, design = nhis, family = quasibinomial, na.action = na.omit)
model4 <- svyglm(cpain ~ mobility2, design = nhis, family = quasibinomial, na.action = na.omit)
model5 <- svyglm(cpain ~ communicating, design = nhis, family = quasibinomial, na.action = na.omit)
model1 <- svyglm(cpain ~ cognitive, design = nhis, family = quasibinomial, na.action = na.omit)
model2 <- svyglm(cpain ~ work_limited, design = nhis, family = quasibinomial, na.action = na.omit)
model6 <- svyglm(cpain ~ participation, design = nhis, family = quasibinomial, na.action = na.omit)
model7 <- svyglm(cpain ~ work_lastwk, design = nhis, family = quasibinomial, na.action = na.omit)
tidy1 <- tidy(model1, exponentiate=TRUE,conf.int = TRUE)[-1, -c(3, 4)]
tidy2 <- tidy(model2, exponentiate=TRUE,conf.int = TRUE)[-1, -c(3, 4)]
tidy3 <- tidy(model3, exponentiate=TRUE,conf.int = TRUE)[-1, -c(3, 4)]
tidy4 <- tidy(model4, exponentiate=TRUE,conf.int = TRUE)[-1, -c(3, 4)]
tidy5 <- tidy(model5, exponentiate=TRUE,conf.int = TRUE)[-1, -c(3, 4)]
tidy6 <- tidy(model6, exponentiate=TRUE,conf.int = TRUE)[-1, -c(3, 4)]
tidy7 <- tidy(model7, exponentiate=TRUE,conf.int = TRUE)[-1, -c(3, 4)]
missing<-c(31124-length(model1$residuals),  31124-length(model1$residuals), 
           31124-length(model2$residuals), 31124-length(model3$residuals),31124-length(model3$residuals),
           31124-length(model4$residuals),31124-length(model5$residuals),31124-length(model5$residuals),
           31124-length(model6$residuals), 31124-length(model6$residuals), 31124-length(model6$residuals))
knitr::kable(cbind(bind_rows(tidy1, tidy2, tidy3, tidy4, tidy5, tidy6, tidy7), missing), digits=2,
             caption="Disability and participation")

#health
model1 <- svyglm(cpain ~ health, design = nhis, family = quasibinomial, na.action = na.omit)
model2 <- svyglm(cpain ~ fatigue, design = nhis, family = quasibinomial, na.action = na.omit)
model3 <- svyglm(cpain ~ dr_12months, design = nhis, family = quasibinomial, na.action = na.omit)
model4 <- svyglm(cpain ~ wellness, design = nhis, family = quasibinomial, na.action = na.omit)
model5 <- svyglm(cpain ~ urgent_care, design = nhis, family = quasibinomial, na.action = na.omit)
model6 <- svyglm(cpain ~ hospitalized, design = nhis, family = quasibinomial, na.action = na.omit)
model7 <- svyglm(cpain ~ opioid, design = nhis, family = quasibinomial, na.action = na.omit)
model8 <- svyglm(cpain ~DM_risk , design = nhis, family = quasibinomial, na.action = na.omit)
model9 <- svyglm(cpain ~ sleep_abnorm, design = nhis, family = quasibinomial, na.action = na.omit)
model10 <- svyglm(cpain ~ sleep_med, design = nhis, family = quasibinomial, na.action = na.omit)
tidy1 <- tidy(model1, exponentiate=TRUE,conf.int = TRUE)[-1, -c(3, 4)]
tidy2 <- tidy(model2, exponentiate=TRUE,conf.int = TRUE)[-1, -c(3, 4)]
tidy3 <- tidy(model3, exponentiate=TRUE,conf.int = TRUE)[-1, -c(3, 4)]
tidy4 <- tidy(model4, exponentiate=TRUE,conf.int = TRUE)[-1, -c(3, 4)]
tidy5 <- tidy(model5, exponentiate=TRUE,conf.int = TRUE)[-1, -c(3, 4)]
tidy6 <- tidy(model6, exponentiate=TRUE,conf.int = TRUE)[-1, -c(3, 4)]
tidy7 <- tidy(model7, exponentiate=TRUE,conf.int = TRUE)[-1, -c(3, 4)]
tidy8 <- tidy(model8, exponentiate=TRUE,conf.int = TRUE)[-1, -c(3, 4)]
tidy9 <- tidy(model9, exponentiate=TRUE,conf.int = TRUE)[-1, -c(3, 4)]
tidy10 <- tidy(model10, exponentiate=TRUE,conf.int = TRUE)[-1, -c(3, 4)]
missing<-c(31124-length(model1$residuals),31124-length(model1$residuals),31124-length(model1$residuals),
           31124-length(model1$residuals), 
           31124-length(model2$residuals), 31124-length(model2$residuals),31124-length(model2$residuals),
           31124-length(model3$residuals), 31124-length(model4$residuals),
           31124-length(model5$residuals), 31124-length(model5$residuals),
           31124-length(model6$residuals), 31124-length(model7$residuals),
           31124-length(model8$residuals), 31124-length(model9$residuals),
           31124-length(model10$residuals), 31124-length(model10$residuals))
knitr::kable(cbind(bind_rows(tidy1, tidy2, tidy3, tidy4, tidy5, tidy6, tidy7, tidy8,tidy9, tidy10), missing), digits=2,
             caption="Health")

#Health awareness and lifestyle
#  flu_vaccine+cig100+PA+sunscreen+
model1 <- svyglm(cpain ~ flu_vaccine, design = nhis, family = quasibinomial, na.action = na.omit)
model2 <- svyglm(cpain ~ cig100, design = nhis, family = quasibinomial, na.action = na.omit)
model3 <- svyglm(cpain ~ onedrink, design = nhis, family = quasibinomial, na.action = na.omit)
model6 <- svyglm(cpain ~ drink_heavy, design = nhis, family = quasibinomial, na.action = na.omit)
model4 <- svyglm(cpain ~ sport, design = nhis, family = quasibinomial, na.action = na.omit)
model5 <- svyglm(cpain ~ sunscreen, design = nhis, family = quasibinomial, na.action = na.omit)
tidy1 <- tidy(model1, exponentiate=TRUE,conf.int = TRUE)[-1, -c(3, 4)]
tidy2 <- tidy(model2, exponentiate=TRUE,conf.int = TRUE)[-1, -c(3, 4)]
tidy3 <- tidy(model3, exponentiate=TRUE,conf.int = TRUE)[-1, -c(3, 4)]
tidy4 <- tidy(model4, exponentiate=TRUE,conf.int = TRUE)[-1, -c(3, 4)]
tidy5 <- tidy(model5, exponentiate=TRUE,conf.int = TRUE)[-1, -c(3, 4)]
tidy6 <- tidy(model6, exponentiate=TRUE,conf.int = TRUE)[-1, -c(3, 4)]
missing<-c(31124-length(model1$residuals), 31124-length(model2$residuals), 
           31124-length(model3$residuals), 31124-length(model6$residuals), 
           31124-length(model4$residuals),31124-length(model4$residuals), 
           31124-length(model5$residuals), 31124-length(model5$residuals) )
knitr::kable(cbind(bind_rows(tidy1, tidy2, tidy3, tidy6, tidy4, tidy5), missing), digits=2,
             caption="Health awareness and lifestyle")

#mental health
#  support+anx_freq+anx_med+anx_level+depr_freq+depr_med+depr_level+mhrx
model1 <- svyglm(cpain ~ support, design = nhis, family = quasibinomial, na.action = na.omit)
model2 <- svyglm(cpain ~ anx_freq, design = nhis, family = quasibinomial, na.action = na.omit)
model3 <- svyglm(cpain ~ anx_level, design = nhis, family = quasibinomial, na.action = na.omit)
model4 <- svyglm(cpain ~ anx_med, design = nhis, family = quasibinomial, na.action = na.omit)
model5 <- svyglm(cpain ~ depr_freq, design = nhis, family = quasibinomial, na.action = na.omit)
model6 <- svyglm(cpain ~ depr_level, design = nhis, family = quasibinomial, na.action = na.omit)
model7 <- svyglm(cpain ~ depr_med, design = nhis, family = quasibinomial, na.action = na.omit)
model8 <- svyglm(cpain ~mhrx , design = nhis, family = quasibinomial, na.action = na.omit)
tidy1 <- tidy(model1, exponentiate=TRUE,conf.int = TRUE)[-1, -c(3, 4)]
tidy2 <- tidy(model2, exponentiate=TRUE,conf.int = TRUE)[-1, -c(3, 4)]
tidy3 <- tidy(model3, exponentiate=TRUE,conf.int = TRUE)[-1, -c(3, 4)]
tidy4 <- tidy(model4, exponentiate=TRUE,conf.int = TRUE)[-1, -c(3, 4)]
tidy5 <- tidy(model5, exponentiate=TRUE,conf.int = TRUE)[-1, -c(3, 4)]
tidy6 <- tidy(model6, exponentiate=TRUE,conf.int = TRUE)[-1, -c(3, 4)]
tidy7 <- tidy(model7, exponentiate=TRUE,conf.int = TRUE)[-1, -c(3, 4)]
tidy8 <- tidy(model8, exponentiate=TRUE,conf.int = TRUE)[-1, -c(3, 4)]
missing<-c(31124-length(model1$residuals),31124-length(model1$residuals),
           31124-length(model2$residuals), 
           31124-length(model3$residuals), 31124-length(model3$residuals), 
           31124-length(model4$residuals), 
           31124-length(model5$residuals), 
           31124-length(model6$residuals), 31124-length(model6$residuals),
           31124-length(model7$residuals), 31124-length(model8$residuals))
knitr::kable(cbind(bind_rows(tidy1, tidy2, tidy3, tidy4, tidy5, tidy6, tidy7, tidy8), missing), digits=2,
             caption="Mental health")

#  Interactions
#identifying interactions for the chronic pain model by logistic regression, using 
#socio-demographic factors and relavant predictors
#these interactions were not significant
summary(svyglm(cpain~sex*depr_freq, family=quasibinomial, design=nhis, na.action = na.omit))
summary(svyglm(cpain~sex*anx_freq, family=quasibinomial, design=nhis, na.action = na.omit))
summary(svyglm(cpain~sex*depr_level, family=quasibinomial, design=nhis, na.action = na.omit))
summary(svyglm(cpain~sex*anx_level, family=quasibinomial, design=nhis, na.action = na.omit))
summary(svyglm(cpain~sex*depr_med, family=quasibinomial, design=nhis, na.action = na.omit))
summary(svyglm(cpain~sex*anx_med, family=quasibinomial, design=nhis, na.action = na.omit))
summary(svyglm(cpain~sex*uninsured, family=quasibinomial, design=nhis, na.action = na.omit))
summary(svyglm(cpain~sex*payworry, family=quasibinomial, design=nhis, na.action = na.omit))
summary(svyglm(cpain~sex*caredelay, family=quasibinomial, design=nhis, na.action = na.omit))
summary(svyglm(cpain~sex*paybill, family=quasibinomial, design=nhis, na.action = na.omit))
summary(svyglm(cpain~sex*fatigue, family=quasibinomial, design=nhis, na.action = na.omit))
summary(svyglm(cpain~sex*dr_12months, family=quasibinomial, design=nhis, na.action = na.omit))
summary(svyglm(cpain~sex*injury, family=quasibinomial, design=nhis, na.action = na.omit))
summary(svyglm(cpain~sex*DM_risk, family=quasibinomial, design=nhis, na.action = na.omit))
summary(svyglm(cpain~sex*mobility2, family=quasibinomial, design=nhis, na.action = na.omit))
summary(svyglm(cpain~sex*mobility1, family=quasibinomial, design=nhis, na.action = na.omit))

summary(svyglm(cpain~age65*uninsured, family=quasibinomial, design=nhis, na.action = na.omit))
summary(svyglm(cpain~age65*payworry, family=quasibinomial, design=nhis, na.action = na.omit))
summary(svyglm(cpain~age65*caredelay, family=quasibinomial, design=nhis, na.action = na.omit))
summary(svyglm(cpain~age65*paybill, family=quasibinomial, design=nhis, na.action = na.omit))
summary(svyglm(cpain~age65*fatigue, family=quasibinomial, design=nhis, na.action = na.omit))
summary(svyglm(cpain~age65*dr_12months, family=quasibinomial, design=nhis, na.action = na.omit))
summary(svyglm(cpain~age65*injury, family=quasibinomial, design=nhis, na.action = na.omit))

summary(svyglm(cpain~income_grp*uninsured, family=quasibinomial, design=nhis, na.action = na.omit))
summary(svyglm(cpain~income_grp*payworry, family=quasibinomial, design=nhis, na.action = na.omit))
summary(svyglm(cpain~income_grp*paybill, family=quasibinomial, design=nhis, na.action = na.omit))

summary(svyglm(cpain~uninsured*hospitalized, family=quasibinomial, design=nhis, na.action = na.omit))
summary(svyglm(cpain~uninsured*payworry, family=quasibinomial, design=nhis, na.action = na.omit))
summary(svyglm(cpain~uninsured*paybill, family=quasibinomial, design=nhis, na.action = na.omit))
summary(svyglm(cpain~participation*mobility1, family=quasibinomial, design=nhis, na.action = na.omit))
summary(svyglm(cpain~support*participation, family=quasibinomial, design=nhis, na.action = na.omit))

#visualizing interactions
library(emmeans)
m3<-svyglm(cpain~cognitive*health, family=quasibinomial, design=nhis, na.action = na.omit)
emmip(m3,  cognitive~health, CIs=TRUE, plotit=T)+theme_bw()

m3<-svyglm(cpain~social_activity*age65, family=quasibinomial, design=nhis, na.action = na.omit)
emmip(m3,  social_activity~age65, CIs=TRUE, plotit=T)+theme_bw()

m3<-svyglm(cpain~health*age65, family=quasibinomial, design=nhis, na.action = na.omit)
emmip(m3,  health~age65, CIs=TRUE, plotit=T)+theme_bw()

m3<-svyglm(cpain~cognitive*age65, family=quasibinomial, design=nhis, na.action = na.omit)
emmip(m3,  cognitive~age65, CIs=TRUE, plotit=T)+theme_bw()

m3<-svyglm(cpain~mobility1*age65, family=quasibinomial, design=nhis, na.action = na.omit)
emmip(m3,  mobility1~age65, CIs=TRUE, plotit=T)+theme_bw()
  
  
#significant interactions
summary(svyglm(cpain~sex*health, family=quasibinomial, design=nhis, na.action = na.omit))
summary(svyglm(cpain~sex*opioid, family=quasibinomial, design=nhis, na.action = na.omit))
summary(svyglm(cpain~age65*DM_risk, family=quasibinomial, design=nhis, na.action = na.omit))
summary(svyglm(cpain~age65*mobility2, family=quasibinomial, design=nhis, na.action = na.omit))
summary(svyglm(cpain~age65*mobility1, family=quasibinomial, design=nhis, na.action = na.omit))
summary(svyglm(cpain~income_grp*dr_12months, family=quasibinomial, design=nhis, na.action = na.omit))
summary(svyglm(cpain~income_grp*fatigue, family=quasibinomial, design=nhis, na.action = na.omit))
summary(svyglm(cpain~income_grp*caredelay, family=quasibinomial, design=nhis, na.action = na.omit))
summary(svyglm(cpain~income_grp*cig100, family=quasibinomial, design=nhis, na.action = na.omit))
summary(svyglm(cpain~age65*participation, family=quasibinomial, design=nhis, na.action = na.omit))
summary(svyglm(cpain~age65*cognitive, family=quasibinomial, design=nhis, na.action = na.omit))
summary(svyglm(cpain~age65*work_lastwk, family=quasibinomial, design=nhis, na.action = na.omit))
summary(svyglm(cpain~mobility2*participation, family=quasibinomial, design=nhis, na.action = na.omit))

#most significant interactions (highest estimates)
summary(svyglm(cpain~participation*mobility1, family=quasibinomial, design=nhis, na.action = na.omit))
summary(svyglm(cpain~age65*health, family=quasibinomial, design=nhis, na.action = na.omit))

svychisq(~sex+opioid, nhis, statistic="adjWald")
svytable(~sex+opioid, design = nhis)
#women are more likely to use opioids

svychisq(~cpain+opioid, nhis, statistic="adjWald")
svytable(~cpain+opioid, design = nhis)
#strongly correlated, F statistic=497
#for people with no chronic pain, the odd of opioid use is 0.007
#for people with chronic pain, the odd for opiod use is 0.156
#OR=22, people with chronic pain are 22 times more likely to use opioids


#------------------- cpain model fitting

#model building, testing every predictor and creating a table for display
#https://rstudio-pubs-static.s3.amazonaws.com/268057_9e722577b6c34858b9ccad02e97075bc.html

#all predictors,except support
m0<-(svyglm(cpain~ opioid+sex+age+age65+married+veteran+income_grp+home_owner+uninsured+
              paybill+payworry+caredelay+foodworry+counseldelay+
              mobility1+mobility2+communicating+cognitive+work_limited+participation+work_lastwk+
              health+fatigue+injury+dr_12months+wellness+urgent_care+hospitalized+
              flu_vaccine+cig100+onedrink+drink_heavy+sport+sunscreen+DM_risk+sleep_abnorm+sleep_med+
              anx_freq+anx_med+anx_level+depr_freq+depr_med+depr_level+mhrx,
            family=quasibinomial, design=nhis, na.action = na.omit))
summary(m0)
psrsq(m0, method = c("Nagelkerke"))
#pseudo-R2=0.40


#predictors, except support, (home owner, married, sport and sunscreen)
#including interactions
m1<-(svyglm(cpain~ age65*health+ participation*mobility1+
              opioid+sex+age+veteran+income_grp+uninsured+
              paybill+payworry+foodworry+counseldelay+caredelay+
              mobility2+communicating+cognitive+work_limited+work_lastwk+
              fatigue+injury+dr_12months+wellness+urgent_care+hospitalized+
              flu_vaccine+cig100+onedrink+drink_heavy+DM_risk+sleep_abnorm+sleep_med+
              anx_freq+anx_med+anx_level+depr_freq+depr_med+depr_level+mhrx,
              family=quasibinomial, design=nhis, na.action = na.omit))
summary(m1)
psrsq(m1, method = c("Nagelkerke"))
#summary(m1)

#all predictors+interaction
m2<-(svyglm(cpain~ opioid*sex+age+veteran+income_grp+uninsured+home_owner+married+sport+sunscreen+
              paybill+payworry+foodworry+counseldelay+caredelay+
              age65*mobility1+mobility2+communicating+cognitive+work_limited+participation+work_lastwk+
              sex*health+fatigue+injury+dr_12months+wellness+urgent_care+hospitalized+
              flu_vaccine+cig100+DM_risk+sleep_abnorm+sleep_med+
              anx_freq+anx_med+anx_level+depr_freq+depr_med+depr_level+mhrx,
            family=quasibinomial, design=nhis, na.action = na.omit))

#comparing the models
tab_model(m0, m1, show.ci=FALSE, show.aic = TRUE)
#plot_summs(m0, m1, scale = TRUE)
psrsq(m2, method = c("Nagelkerke"))
#pseudo r2 for m0= 0.399, m1=0.396, m2=0.4


#testing model's performance with a stratified train and test set
set.seed(123)
#creating train and test set stratified by chronic pain
yesdata<-cdata%>% filter(cpain=="Yes")#df of opioid users
nodata<-cdata%>% filter(cpain=="No")# df of non-users

yestraining.samples <- yesdata$cpain %>% createDataPartition(p = 0.8, list = FALSE)
yestrain.data  <- na.omit(yesdata[yestraining.samples, ])
yestest.data <- na.omit(yesdata[-yestraining.samples, ])

notraining.samples <- nodata$cpain %>% createDataPartition(p = 0.8, list = FALSE)
notrain.data  <- na.omit(nodata[notraining.samples, ])
notest.data <- na.omit(nodata[-notraining.samples, ])

train.data<-bind_rows(yestrain.data, notrain.data)
test.data<-bind_rows(yestest.data, notest.data)

train.design<-svydesign(id =~PPSU, strata =~PSTRAT,  nest=TRUE, weights=~WTFA_A, data = train.data)

#model 1- all predictors
m1<-(svyglm(cpain~ opioid+sex+age+age65+married+veteran+income_grp+home_owner+uninsured+
              paybill+payworry+caredelay+foodworry+counseldelay+
              mobility1+mobility2+communicating+cognitive+work_limited+social_activity+
              work_lastwk+
              health+fatigue+injury+dr_12months+wellness+urgent_care+hospitalized+
              flu_vaccine+support+
              cig100+onedrink+drink_heavy+sport+sunscreen+DM_risk+sleep_abnorm+sleep_med+
              anx_freq+anx_med+anx_level+depr_freq+depr_med+depr_level+mhrx,
            family=quasibinomial, design=train.design, na.action = na.omit))
#AUC=0.8436

#deriving predictions for the test set
guess<-predict(m1, newdata=test.data, type="response")
tp.fp<-WeightedROC(guess, test.data$cpain, test.data$WTFA_A)
auc<-WeightedAUC(tp.fp)#0.8436

#plotting the roc curve
ggplot()+ geom_path(aes(FPR, TPR), data=tp.fp)+ xlab("1-Specificity")+
  ylab("Sensitivity")+coord_equal() +ggtitle("Model 1: ROC curve")

#model 2: subset of predictors with interactions
m2<-(svyglm(cpain~age65*mobility1+ sex+age+home_owner+
              veteran+income_grp+uninsured+married+
              paybill+payworry+foodworry+caredelay+counseldelay+
              mobility2+communicating+cognitive+work_limited+social_activity+work_lastwk+
              opioid+health+fatigue+injury+dr_12months+wellness+hospitalized+urgent_care+
              flu_vaccine+cig100+drink_heavy+onedrink+DM_risk+sleep_med+sleep_abnorm+
              anx_freq+anx_med+depr_freq+depr_med,
            family=quasibinomial, design=train.design, na.action = na.omit))
#deriving predictions for the test set
guess1<-predict(m2, newdata=test.data, type="response")
tp.fp1<-WeightedROC(guess1, test.data$cpain, test.data$WTFA_A)
auc1<-WeightedAUC(tp.fp1)#0.846

#plotting the roc curve
ggplot()+ geom_path(aes(FPR, TPR), data=tp.fp1)+ xlab("1-Specificity")+
  ylab("Sensitivity")+coord_equal() +ggtitle("Model 2: ROC curve")


#deriving threshold
error.fun.list <- list(
  FN=function(df)df$FN,
  FP=function(df)df$FP,
  errors=function(df)with(df, FP+FN)
)
all.error.list <- list()
for(error.type in names(error.fun.list)){
  error.fun <- error.fun.list[[error.type]]
  all.error.list[[error.type]] <-
    data.frame(tp.fp, error.type, weighted.error=error.fun(tp.fp))
}
all.error <- do.call(rbind, all.error.list)

error.graph<-all.error%>% filter(all.error$error.type=="errors")
#threshold - by minimal error
thresh<-error.graph$threshold[error.graph$weighted.error==min(error.graph$weighted.error)]
results<-data.frame(guess)
predicted.classes <- ifelse(results$responsnse>thresh, "Yes", "No")

#finding threshold by intersection of FN and FP- minimizing both FN and FP
## The FN/FP columns can be used to plot weighted error as a
## function of threshold.
error.fun.list <- list(
  FN=function(df)df$FN,
  FP=function(df)df$FP,
  errors=function(df)with(df, FP+FN)
)
all.error.list <- list()
for(error.type in names(error.fun.list)){
  error.fun <- error.fun.list[[error.type]]
  all.error.list[[error.type]] <-
    data.frame(tp.fp, error.type, weighted.error=error.fun(tp.fp))
}
all.error <- do.call(rbind, all.error.list)
fp.fn.colors <- c(FP="skyblue",
                  FN="#E41A1C",
                  errors="black")
ggplot()+
  scale_color_manual(values=fp.fn.colors)+
  geom_line(aes(threshold, weighted.error, color=error.type),
            data=all.error)+
  geom_vline(xintercept = 0.31, color = "blue", size=0.5)
#the optimal threshold for minimizing both types of errors  (FN or FP) is 0.31.
#if we can allow more FN we can use a higher threshold, or if we can allow more FP
#we can use a lower threshold

#finding the optimal threshold- the intersection point of the FN and FP graphs
#https://stackoverflow.com/questions/20519431/finding-point-of-intersection-in-r
#thresh_intersection<-all.error$threshold[min(abs(all.error$FN-all.error$FP))]
#threshold by equal errors for FN and FP= 0.056, accuracy is 0.36

#threshold by the minimum of the errors graph
#error.graph<-all.error%>% filter(all.error$error.type=="errors")
#error.graph$threshold[error.graph$threshold>1]<-1

#thresh<-error.graph$threshold[error.graph$weighted.error==min(error.graph$weighted.error)]
#threshold for minimal error=0.568
#83% accuracy
#threshold by minimum FP+FN
thresh<-all.error$threshold[which.min(all.error$FP+all.error$FN)]


#calculating model sensitivity, specificity, balanced accuracy:
results<-data.frame(guess)
predicted.classes <- ifelse(results$response > thresh, "Yes", "No")
confusionMatrix(as.factor(predicted.classes), test.data$cpain)
#obtaining specificity and sensitivity

#threshold by minimal error
thresh<-tp.fp1$threshold[which.min(tp.fp1$FP+tp.fp1$FN)]

#threshold by maximal sensitivity and specificity
thresh1<-tp.fp1$threshold[which.max(tp.fp1$TPR +(1-tp.fp1$FPR))]

#thresholds for sensitivity of 0.85, 0.9 and 0.95
t85<-tp.fp1$threshold[0.848<tp.fp1$TPR & 0.852>tp.fp1$TPR] #0.127
t90<-tp.fp1$threshold[0.898<tp.fp1$TPR & 0.902>tp.fp1$TPR] #0.102
t95<-tp.fp1$threshold[0.948<tp.fp1$TPR & 0.952>tp.fp1$TPR] #0.0783

#calculating model accuracy:
library(MLmetrics)

results<-data.frame(guess1)
predicted.classes1 <- ifelse(results$response > 0.2, "Yes", "No")
conf<-confusionMatrix(as.factor(predicted.classes1), test.data$cpain, positive="Yes")
F1_Score(test.data$cpain, predicted.classes1, positive="Yes")

#prediction with survey model
#https://stats.idre.ucla.edu/r/faq/how-can-i-do-regression-estimation-with-survey-data/
predict(m1, newdata=df, type='response', allow.new.levels = TRUE)

#weightedRoc
#https://cran.r-project.org/web/packages/WeightedROC/WeightedROC.pdf

#optimal threshold DIY
#https://stats.stackexchange.com/questions/123124/how-to-determine-the-optimal-threshold-for-a-classifier-and-generate-roc-curve

#repeated k-fold cross validation
#http://www.sthda.com/english/articles/38-regression-model-validation/157-cross-validation-essentials-in-r/
#function caret::createFolds creates k folds
#createFolds(y, k = 10, list = TRUE, returnTrain = FALSE)

#model interpretation , interpreting and plotting the interaction
export_summs(m2, error_format = "[{conf.low}, {conf.high}]", digits = 2, exp=FALSE, pvals = FALSE)
#library(emmeans)
#emmeans_results <- emmeans(m2, ~ age65*mobility1)
#model is too big for this function
library(effects)
result <- allEffects(m2)
summary(result$`age65:mobility1`)

OR<-round(exp(result$`age65:mobility1`$fit), digits=2)
CI_upper<-round(exp(result$`age65:mobility1`$upper), digits=2)
CI_lower<-round(exp(result$`age65:mobility1`$lower), digits=2)
Age<-c("Under 65", "65+","Under 65", "65+","Under 65", "65+")
Mobility<-c("No difficulty", "Some difficulty", "A lot of difficulty", "No difficulty", "Some difficulty", "A lot of difficulty")
knitr::kable(cbind(Age, Mobility, OR, CI_lower, CI_upper), 
             col.names=c("Age", "Mobility", "OR", "CI lower bound", "CI upper bound"),
             caption="Interaction post-hoc analysis: age 65+ and mobility", align="l")
effects::plot.eff(result)
plot(result$`age65:mobility1`)
#effect(age65:mobility1, m2)

###222222222222222-----------prediction model- opioid use
#logistic regression for imbalanced binary classification.
#weighted logistic regression for imbalanced data
#https://towardsdatascience.com/weighted-logistic-regression-for-imbalanced-dataset-9a5cd88e68b

#sample description by opioid use
library(table1)
table1(~ cpain+sex+age+age65+married+income_grp+veteran+home_owner+uninsured+
         health+hospitalized+DM_risk+ mobility1+mobility2
       | opioid, data=data, overall="Total", caption="Socio-demographic and clinical characteristics by opioid use")

# Predictors
model <- svyglm(opioid~age, family=quasibinomial, design=nhis, na.action = na.omit)
tab_model(model,title="opioid use by age")

#demographics
model0 <- svyglm(opioid ~ sex, design = nhis, family = quasibinomial, na.action = na.omit)
model1 <- svyglm(opioid ~ sex, design = nhis, family = quasibinomial, na.action = na.omit)
model4 <- svyglm(opioid ~ income_grp, design = nhis, family = quasibinomial, na.action = na.omit)
model3 <- svyglm(opioid~ age65, design = nhis, family = quasibinomial, na.action = na.omit)
model2 <- svyglm(opioid ~ married, design = nhis, family = quasibinomial, na.action = na.omit)
model6 <- svyglm(opioid ~ veteran, design = nhis, family = quasibinomial, na.action = na.omit)
model5 <- svyglm(opioid ~ home_owner, design = nhis, family = quasibinomial, na.action = na.omit)
tidy0 <- tidy(model0, exponentiate=TRUE,conf.int = TRUE)[-1, -c(3, 4)]
tidy1 <- tidy(model1, exponentiate=TRUE,conf.int = TRUE)[-1, -c(3, 4)]
tidy2 <- tidy(model2, exponentiate=TRUE,conf.int = TRUE)[-1, -c(3, 4)]
tidy3 <- tidy(model3, exponentiate=TRUE,conf.int = TRUE)[-1, -c(3, 4)]
tidy4 <- tidy(model4, exponentiate=TRUE,conf.int = TRUE)[-1, -c(3, 4)]
tidy5 <- tidy(model5, exponentiate=TRUE,conf.int = TRUE)[-1, -c(3, 4)]
tidy6 <- tidy(model6, exponentiate=TRUE,conf.int = TRUE)[-1, -c(3, 4)]
missing<-c(31124-length(model0$residuals),31124-length(model1$residuals),  31124-length(model2$residuals), 31124-length(model3$residuals),31124-length(model4$residuals),31124-length(model5$residuals),31124-length(model6$residuals))
knitr::kable(cbind(bind_rows(tidy1, tidy2, tidy3, tidy4, tidy5, tidy6), ""), digits=2,
             caption="Socio-demographic characteristics: odds ratios for opioid use", align="l")

model1 <- svyglm(opioid ~ uninsured, design = nhis, family = quasibinomial, na.action = na.omit)
model2 <- svyglm(opioid ~ paybill, design = nhis, family = quasibinomial, na.action = na.omit)
model3 <- svyglm(opioid ~ payworry, design = nhis, family = quasibinomial, na.action = na.omit)
model4 <- svyglm(opioid ~ caredelay, design = nhis, family = quasibinomial, na.action = na.omit)
model6 <- svyglm(opioid ~ foodworry, design = nhis, family = quasibinomial, na.action = na.omit)
model5 <- svyglm(opioid ~ counseldelay, design = nhis, family = quasibinomial, na.action = na.omit)
tidy1 <- tidy(model1, exponentiate=TRUE,conf.int = TRUE)[-1, -c(3, 4)]
tidy2 <- tidy(model2, exponentiate=TRUE,conf.int = TRUE)[-1, -c(3, 4)]
tidy3 <- tidy(model3, exponentiate=TRUE,conf.int = TRUE)[-1, -c(3, 4)]
tidy4 <- tidy(model4, exponentiate=TRUE,conf.int = TRUE)[-1, -c(3, 4)]
tidy5 <- tidy(model5, exponentiate=TRUE,conf.int = TRUE)[-1, -c(3, 4)]
tidy6 <- tidy(model6, exponentiate=TRUE,conf.int = TRUE)[-1, -c(3, 4)]
missing<-c(31124-length(model1$residuals),  31124-length(model2$residuals), 31124-length(model3$residuals),31124-length(model4$residuals),31124-length(model5$residuals),31124-length(model6$residuals))
knitr::kable(cbind(bind_rows(tidy1, tidy2, tidy3, tidy4, tidy5, tidy6), missing), digits=2,
             caption="Financial concerns odds ratios for opioid use", align="l")

model1 <- svyglm(opioid ~ mobility1, design = nhis, family = quasibinomial, na.action = na.omit)
model2 <- svyglm(opioid ~ mobility2, design = nhis, family = quasibinomial, na.action = na.omit)
model3 <- svyglm(opioid ~ communicating, design = nhis, family = quasibinomial, na.action = na.omit)
model4 <- svyglm(opioid ~ cognitive, design = nhis, family = quasibinomial, na.action = na.omit)
model6 <- svyglm(opioid ~ work_limited, design = nhis, family = quasibinomial, na.action = na.omit)
model5 <- svyglm(opioid ~ social_activity, design = nhis, family = quasibinomial, na.action = na.omit)
model7 <- svyglm(opioid ~ work_lastwk, design = nhis, family = quasibinomial, na.action = na.omit)
tidy1 <- tidy(model1, exponentiate=TRUE,conf.int = TRUE)[-1, -c(3, 4)]
tidy2 <- tidy(model2, exponentiate=TRUE,conf.int = TRUE)[-1, -c(3, 4)]
tidy3 <- tidy(model3, exponentiate=TRUE,conf.int = TRUE)[-1, -c(3, 4)]
tidy4 <- tidy(model4, exponentiate=TRUE,conf.int = TRUE)[-1, -c(3, 4)]
tidy5 <- tidy(model5, exponentiate=TRUE,conf.int = TRUE)[-1, -c(3, 4)]
tidy6 <- tidy(model6, exponentiate=TRUE,conf.int = TRUE)[-1, -c(3, 4)]
tidy7 <- tidy(model7, exponentiate=TRUE,conf.int = TRUE)[-1, -c(3, 4)]
missing<-c(31124-length(model1$residuals),  31124-length(model1$residuals), 
           31124-length(model2$residuals), 31124-length(model3$residuals),31124-length(model3$residuals),
           31124-length(model4$residuals), 31124-length(model4$residuals),
           31124-length(model5$residuals),31124-length(model5$residuals),
           31124-length(model6$residuals), 31124-length(model6$residuals))
knitr::kable(cbind(bind_rows(tidy1, tidy2, tidy3, tidy4, tidy5, tidy6, tidy7), missing), digits=2,
             caption="Disability and social_activity: odds ratios for opioid use", align="l")


model1 <- svyglm(opioid ~ health, design = nhis, family = quasibinomial, na.action = na.omit)
model2 <- svyglm(opioid ~ fatigue, design = nhis, family = quasibinomial, na.action = na.omit)
model3 <- svyglm(opioid ~ dr_12months, design = nhis, family = quasibinomial, na.action = na.omit)
model4 <- svyglm(opioid ~ wellness, design = nhis, family = quasibinomial, na.action = na.omit)
model5 <- svyglm(opioid ~ urgent_care, design = nhis, family = quasibinomial, na.action = na.omit)
model6 <- svyglm(opioid ~ hospitalized, design = nhis, family = quasibinomial, na.action = na.omit)
model7 <- svyglm(opioid ~ cpain, design = nhis, family = quasibinomial, na.action = na.omit)
model8 <- svyglm(opioid ~DM_risk , design = nhis, family = quasibinomial, na.action = na.omit)
model9 <- svyglm(opioid ~ sleep_abnorm, design = nhis, family = quasibinomial, na.action = na.omit)
model10 <- svyglm(opioid ~ sleep_med, design = nhis, family = quasibinomial, na.action = na.omit)
model11 <- svyglm(opioid ~ flu_vaccine, design = nhis, family = quasibinomial, na.action = na.omit)
tidy1 <- tidy(model1, exponentiate=TRUE,conf.int = TRUE)[-1, -c(3, 4)]
tidy2 <- tidy(model2, exponentiate=TRUE,conf.int = TRUE)[-1, -c(3, 4)]
tidy3 <- tidy(model3, exponentiate=TRUE,conf.int = TRUE)[-1, -c(3, 4)]
tidy4 <- tidy(model4, exponentiate=TRUE,conf.int = TRUE)[-1, -c(3, 4)]
tidy5 <- tidy(model5, exponentiate=TRUE,conf.int = TRUE)[-1, -c(3, 4)]
tidy6 <- tidy(model6, exponentiate=TRUE,conf.int = TRUE)[-1, -c(3, 4)]
tidy7 <- tidy(model7, exponentiate=TRUE,conf.int = TRUE)[-1, -c(3, 4)]
tidy8 <- tidy(model8, exponentiate=TRUE,conf.int = TRUE)[-1, -c(3, 4)]
tidy9 <- tidy(model9, exponentiate=TRUE,conf.int = TRUE)[-1, -c(3, 4)]
tidy10 <- tidy(model10, exponentiate=TRUE,conf.int = TRUE)[-1, -c(3, 4)]
tidy11 <- tidy(model11, exponentiate=TRUE,conf.int = TRUE)[-1, -c(3, 4)]
missing<-c(31124-length(model1$residuals),31124-length(model1$residuals),31124-length(model1$residuals), 31124-length(model1$residuals), 
           31124-length(model2$residuals),31124-length(model2$residuals),
           31124-length(model3$residuals), 31124-length(model4$residuals),
           31124-length(model5$residuals), 31124-length(model5$residuals),
           31124-length(model6$residuals), 31124-length(model7$residuals),
           31124-length(model8$residuals), 31124-length(model9$residuals),
           31124-length(model10$residuals), 31124-length(model10$residuals),
           31124-length(model11$residuals))
knitr::kable(cbind(bind_rows(tidy1, tidy2, tidy3, tidy4, tidy5, tidy6, tidy7, tidy8,tidy9, tidy10, tidy11), ""), digits=2,
             caption="Health: odds ratios for opioid use", align="l")

#life style
model1 <- svyglm(opioid ~ drink_heavy, design = nhis, family = quasibinomial, na.action = na.omit)
model2 <- svyglm(opioid ~ cig100, design = nhis, family = quasibinomial, na.action = na.omit)
model3 <- svyglm(opioid ~ onedrink, design = nhis, family = quasibinomial, na.action = na.omit)
model4 <- svyglm(opioid ~ sport, design = nhis, family = quasibinomial, na.action = na.omit)
model5 <- svyglm(opioid ~ sunscreen, design = nhis, family = quasibinomial, na.action = na.omit)
tidy1 <- tidy(model1, exponentiate=TRUE,conf.int = TRUE)[-1, -c(3, 4)]
tidy2 <- tidy(model2, exponentiate=TRUE,conf.int = TRUE)[-1, -c(3, 4)]
tidy3 <- tidy(model3, exponentiate=TRUE,conf.int = TRUE)[-1, -c(3, 4)]
tidy4 <- tidy(model4, exponentiate=TRUE,conf.int = TRUE)[-1, -c(3, 4)]
tidy5 <- tidy(model5, exponentiate=TRUE,conf.int = TRUE)[-1, -c(3, 4)]
missing<-c(31124-length(model2$residuals), 
           31124-length(model3$residuals),31124-length(model1$residuals), 
           31124-length(model4$residuals),31124-length(model4$residuals), 
           31124-length(model5$residuals), 31124-length(model5$residuals) )
knitr::kable(cbind(bind_rows(tidy2, tidy3, tidy1, tidy4, tidy5), missing), digits=2,
             caption="life style: odds ratios for opioid use", align="l")


#mental health

model1 <- svyglm(cpain ~ support, design = nhis, family = quasibinomial, na.action = na.omit)
model2 <- svyglm(opioid ~ anx_freq, design = nhis, family = quasibinomial, na.action = na.omit)
model3 <- svyglm(opioid ~ anx_level, design = nhis, family = quasibinomial, na.action = na.omit)
model4 <- svyglm(opioid ~ anx_med, design = nhis, family = quasibinomial, na.action = na.omit)
model5 <- svyglm(opioid ~ depr_freq, design = nhis, family = quasibinomial, na.action = na.omit)
model6 <- svyglm(opioid ~ depr_level, design = nhis, family = quasibinomial, na.action = na.omit)
model7 <- svyglm(opioid ~ depr_med, design = nhis, family = quasibinomial, na.action = na.omit)
model8 <- svyglm(opioid ~mhrx , design = nhis, family = quasibinomial, na.action = na.omit)
tidy1 <- tidy(model1, exponentiate=TRUE,conf.int = TRUE)[-1, -c(3, 4)]
tidy2 <- tidy(model2, exponentiate=TRUE,conf.int = TRUE)[-1, -c(3, 4)]
tidy3 <- tidy(model3, exponentiate=TRUE,conf.int = TRUE)[-1, -c(3, 4)]
tidy4 <- tidy(model4, exponentiate=TRUE,conf.int = TRUE)[-1, -c(3, 4)]
tidy5 <- tidy(model5, exponentiate=TRUE,conf.int = TRUE)[-1, -c(3, 4)]
tidy6 <- tidy(model6, exponentiate=TRUE,conf.int = TRUE)[-1, -c(3, 4)]
tidy7 <- tidy(model7, exponentiate=TRUE,conf.int = TRUE)[-1, -c(3, 4)]
tidy8 <- tidy(model8, exponentiate=TRUE,conf.int = TRUE)[-1, -c(3, 4)]
missing<-c(31124-length(model1$residuals),31124-length(model1$residuals), 31124-length(model1$residuals), 31124-length(model1$residuals),
           31124-length(model2$residuals), 
           31124-length(model3$residuals), 31124-length(model3$residuals), 
           31124-length(model4$residuals), 
           31124-length(model5$residuals), 
           31124-length(model6$residuals), 31124-length(model6$residuals),
           31124-length(model7$residuals), 31124-length(model8$residuals))
knitr::kable(cbind(bind_rows(tidy1, tidy2, tidy3, tidy4, tidy5, tidy6, tidy7, tidy8), missing), digits=2,
             caption="Mental health: odds ratios for opioid use", align="l")





#missing value number in opioid users for sunscreena nd sport
op<-cdata %>% filter(opioid=="Yes")
summary(op$sunscreen) #19
summary(op$sport) #31


#investigating interactions
#non-significant
summary(svyglm(opioid~sex*depr_freq, family=quasibinomial, design=nhis, na.action = na.omit))
summary(svyglm(opioid~sex*anx_freq, family=quasibinomial, design=nhis, na.action = na.omit))
summary(svyglm(opioid~sex*depr_level, family=quasibinomial, design=nhis, na.action = na.omit))
summary(svyglm(opioid~sex*anx_level, family=quasibinomial, design=nhis, na.action = na.omit))
summary(svyglm(opioid~sex*depr_med, family=quasibinomial, design=nhis, na.action = na.omit))
summary(svyglm(opioid~sex*anx_med, family=quasibinomial, design=nhis, na.action = na.omit))
summary(svyglm(opioid~sex*uninsured, family=quasibinomial, design=nhis, na.action = na.omit))
summary(svyglm(opioid~sex*payworry, family=quasibinomial, design=nhis, na.action = na.omit))
summary(svyglm(opioid~sex*caredelay, family=quasibinomial, design=nhis, na.action = na.omit))
summary(svyglm(opioid~sex*paybill, family=quasibinomial, design=nhis, na.action = na.omit))
summary(svyglm(opioid~sex*fatigue, family=quasibinomial, design=nhis, na.action = na.omit))
summary(svyglm(opioid~sex*dr_12months, family=quasibinomial, design=nhis, na.action = na.omit))
summary(svyglm(opioid~sex*injury, family=quasibinomial, design=nhis, na.action = na.omit))
summary(svyglm(opioid~sex*DM_risk, family=quasibinomial, design=nhis, na.action = na.omit))
summary(svyglm(opioid~sex*mobility2, family=quasibinomial, design=nhis, na.action = na.omit))
summary(svyglm(opioid~sex*mobility1, family=quasibinomial, design=nhis, na.action = na.omit))

summary(svyglm(opioid~age65*uninsured, family=quasibinomial, design=nhis, na.action = na.omit))
summary(svyglm(opioid~age65*payworry, family=quasibinomial, design=nhis, na.action = na.omit))
summary(svyglm(opioid~age65*caredelay, family=quasibinomial, design=nhis, na.action = na.omit))
summary(svyglm(opioid~age65*fatigue, family=quasibinomial, design=nhis, na.action = na.omit))
summary(svyglm(opioid~age65*dr_12months, family=quasibinomial, design=nhis, na.action = na.omit))
summary(svyglm(opioid~age65*injury, family=quasibinomial, design=nhis, na.action = na.omit))
summary(svyglm(opioid~age65*cpain, family=quasibinomial, design=nhis, na.action = na.omit))

summary(svyglm(opioid~income_grp*uninsured, family=quasibinomial, design=nhis, na.action = na.omit))
summary(svyglm(opioid~income_grp*payworry, family=quasibinomial, design=nhis, na.action = na.omit))
summary(svyglm(opioid~income_grp*paybill, family=quasibinomial, design=nhis, na.action = na.omit))

summary(svyglm(opioid~uninsured*hospitalized, family=quasibinomial, design=nhis, na.action = na.omit))
summary(svyglm(opioid~uninsured*payworry, family=quasibinomial, design=nhis, na.action = na.omit))
summary(svyglm(opioid~uninsured*paybill, family=quasibinomial, design=nhis, na.action = na.omit))
summary(svyglm(opioid~participation*mobility1, family=quasibinomial, design=nhis, na.action = na.omit))

summary(svyglm(opioid~sex*health, family=quasibinomial, design=nhis, na.action = na.omit))
summary(svyglm(opioid~income_grp*dr_12months, family=quasibinomial, design=nhis, na.action = na.omit))
summary(svyglm(opioid~income_grp*fatigue, family=quasibinomial, design=nhis, na.action = na.omit))
summary(svyglm(opioid~income_grp*cig100, family=quasibinomial, design=nhis, na.action = na.omit))
summary(svyglm(opioid~health*fatigue, family=quasibinomial, design=nhis, na.action = na.omit))
summary(svyglm(opioid~cpain*age65, family=quasibinomial, design=nhis, na.action = na.omit))

#significant interactions
summary(svyglm(opioid~age65*paybill, family=quasibinomial, design=nhis, na.action = na.omit))
summary(svyglm(opioid~age65*DM_risk, family=quasibinomial, design=nhis, na.action = na.omit))
summary(svyglm(opioid~age65*mobility2, family=quasibinomial, design=nhis, na.action = na.omit))
summary(svyglm(opioid~age65*participation, family=quasibinomial, design=nhis, na.action = na.omit))
summary(svyglm(opioid~age65*cognitive, family=quasibinomial, design=nhis, na.action = na.omit))
summary(svyglm(opioid~age65*work_lastwk, family=quasibinomial, design=nhis, na.action = na.omit))
summary(svyglm(opioid~participation*mobility1, family=quasibinomial, design=nhis, na.action = na.omit))
summary(svyglm(opioid~income_grp*caredelay, family=quasibinomial, design=nhis, na.action = na.omit))


#3 most significant interactions
summary(svyglm(opioid~mobility1*health, family=quasibinomial, design=nhis, na.action = na.omit))
#OR=9
summary(svyglm(opioid~age65*mobility1, family=quasibinomial, design=nhis, na.action = na.omit))
#OR ratio=3
summary(svyglm(opioid~age65*health, family=quasibinomial, design=nhis, na.action = na.omit))
#OR ratio=2.3

#visualizing interactions
m3<-svyglm(opioid~ mobility1*age65, family=quasibinomial, design=nhis, na.action = na.omit)
emmip(m3,  mobility1~age65, CIs=TRUE, plotit=T)+theme_bw()

m3<-svyglm(opioid~ mobility1*health, family=quasibinomial, design=nhis, na.action = na.omit)
emmip(m3,  mobility1~health, CIs=TRUE, plotit=T)+theme_bw()

#under 65 have lower odds for excellent-good, but the same as 65+ for fair+poor
m3<-svyglm(opioid~ age65*health, family=quasibinomial, design=nhis, na.action = na.omit)
emmip(m3,  age65~health, CIs=TRUE, plotit=T)+theme_bw()

m3<-svyglm(opioid~ age65*cognitive, family=quasibinomial, design=nhis, na.action = na.omit)
emmip(m3,  age65~cognitive, CIs=TRUE, plotit=T)+theme_bw()

m3<-svyglm(opioid~ age65*social_activity, family=quasibinomial, design=nhis, na.action = na.omit)
emmip(m3,  age65~social_activity, CIs=TRUE, plotit=T)+theme_bw()

m3<-svyglm(opioid~ age65*mobility1, family=quasibinomial, design=nhis, na.action = na.omit)
emmip(m3,  age65~mobility1, CIs=TRUE, plotit=T)+theme_bw()

m3<-svyglm(opioid~ sex*income_grp, family=quasibinomial, design=nhis, na.action = na.omit)
emmip(m3,  sex~income_grp, CIs=TRUE, plotit=T)+theme_bw()

m3<-svyglm(cpain~ age65*mobility2, family=quasibinomial, design=nhis, na.action = na.omit)
emmip(m3,  age65~mobility2, CIs=TRUE, plotit=T)+theme_bw()

m3<-svyglm(cpain~ age65*work_limited, family=quasibinomial, design=nhis, na.action = na.omit)
emmip(m3,  age65~work_limited, CIs=TRUE, plotit=T)+theme_bw()


#------------------stratified train and test split
#library(splitstackshape)
#library(data.table)
#training.sample <- cdata$opioid %>% stratified("opioid", 0.8)
set.seed(123)
#creating train and test set stratified by opioid use
yesdata<-cdata%>% filter(opioid=="Yes")#df of opioid users
nodata<-cdata%>% filter(opioid=="No")# df of non-users

yestraining.samples <- yesdata$opioid %>% createDataPartition(p = 0.8, list = FALSE)
yestrain.data  <- yesdata[yestraining.samples, ]
yestest.data <- na.omit(yesdata[-yestraining.samples, ])

notraining.samples <- nodata$opioid %>% createDataPartition(p = 0.8, list = FALSE)
notrain.data  <- nodata[notraining.samples, ]
notest.data <- na.omit(nodata[-notraining.samples, ])

train.data<-bind_rows(yestrain.data, notrain.data)
test.data<-bind_rows(yestest.data, notest.data)

train.design<-svydesign(id =~PPSU, strata =~PSTRAT,  nest=TRUE, weights=~WTFA_A, data = train.data)
mp<-svyglm(opioid~cpain+sex+age+age65+married+veteran+income_grp+home_owner+uninsured+
             paybill+payworry+caredelay+foodworry+counseldelay+
             mobility1+mobility2+communicating+cognitive+work_limited+participation+work_lastwk+
             health+fatigue+injury+dr_12months+wellness+urgent_care+hospitalized+
             flu_vaccine+cig100+sport+sunscreen+DM_risk+sleep_abnorm+sleep_med+
             anx_freq+anx_med+anx_level+depr_freq+depr_med+depr_level+mhrx,
           family=quasibinomial, design=train.design, na.action = na.omit)

#deriving predictions for the test set
guess<-predict(mp, newdata=test.data, type="response")
tp.fp<-WeightedROC(guess, test.data$opioid, test.data$WTFA_A)
auc<-WeightedAUC(tp.fp)#0.9135

library(MLmetrics)
F1_Score(test.data$opioid, predicted.classes)
#0.97

roc_curve <- ggplot()+
  geom_path(aes(FPR, TPR), data=tp.fp)+
  xlab("Specificity")+
  ylab("Sensitivity")+
  coord_equal()+
  ggtitle("Model 1")
## The FN/FP columns can be used to plot weighted error as a
## function of threshold.
error.fun.list <- list(
  FN=function(df)df$FN,
  FP=function(df)df$FP,
  errors=function(df)with(df, FP+FN)
)
all.error.list <- list()
for(error.type in names(error.fun.list)){
  error.fun <- error.fun.list[[error.type]]
  all.error.list[[error.type]] <-
    data.frame(tp.fp, error.type, weighted.error=error.fun(tp.fp))
}
all.error <- do.call(rbind, all.error.list)
fp.fn.colors <- c(FP="skyblue",
                  FN="#E41A1C",
                  errors="black")
#threshold by minimum FP+FN
thresh<-all.error$threshold[which.min(all.error$FP +all.error$FN)]
#threshold by max(sensitivity+specificity)
thresh<-all.error$threshold[which.max(all.error$TPR +(1-all.error$FPR))]

ggplot()+
  scale_color_manual(values=fp.fn.colors)+
  geom_line(aes(threshold, weighted.error, color=error.type),
            data=all.error)+
  geom_vline(xintercept = thresh, color = "blue", size=0.5)+
  ggtitle("Model 1")

results<-data.frame(guess)
predicted.classes <- ifelse(results$response > thresh, "Yes", "No")
#mean(predicted.classes == test.data$opioid)
#accuracy=0.942
confusionMatrix(as.factor(predicted.classes), test.data$opioid, positive="Yes")
#threshold by max(spe+sen)=0.056, sen=0.85, spe=0.847 balanced accuracy=0.85
#threshold= 0.23, sensitivity= 0.97, specificity=0.4, balanced accuracy=0.68
#threshold=0.1, sensitivity=0.89, specificity=0.77, balanced accuracy=0.83
#summary(mp)$deviance
#AIC(mp)
#F1 score
library(MLmetrics)
F1_Score(test.data$opioid, predicted.classes, positive="Yes")


library(IMP)
IMP::InterConfMatrix(list(data.frame(test.data$opioid, predicted.classes)))

#----------------------model without sunscreen and sport, to minimize na
set.seed(67)
#creating train and test set stratified by opioid use
yesdata<-cdata%>% filter(opioid=="Yes")#df of opioid users
nodata<-cdata%>% filter(opioid=="No")# df of non-users

yestraining.samples <- yesdata$opioid %>% createDataPartition(p = 0.8, list = FALSE)
yestrain.data  <- yesdata[yestraining.samples, ]
yestest.data <- yesdata[-yestraining.samples, ]

notraining.samples <- nodata$opioid %>% createDataPartition(p = 0.8, list = FALSE)
notrain.data  <- nodata[notraining.samples, ]
notest.data <- nodata[-notraining.samples, ]

train.data<-bind_rows(yestrain.data, notrain.data)
test.data<-bind_rows(yestest.data, notest.data)

train.design<-svydesign(id =~PPSU, strata =~PSTRAT,  nest=TRUE, weights=~WTFA_A, data = train.data)
mp<-svyglm(opioid~cpain+sex+age+age65+married+veteran+income_grp+home_owner+uninsured+
             paybill+payworry+caredelay+foodworry+counseldelay+
             mobility1+mobility2+communicating+cognitive+work_limited+participation+work_lastwk+
             health+fatigue+injury+dr_12months+wellness+urgent_care+hospitalized+
             flu_vaccine+cig100+DM_risk+sport+sunscreen+sleep_abnorm+sleep_med+
             anx_freq+anx_med+anx_level+depr_freq+depr_med+depr_level+mhrx,
           family=quasibinomial, design=train.design, na.action = na.omit)

#deriving predictions for the test set
guess<-predict(mp, newdata=test.data, type="response")
tp.fp<-WeightedROC(guess, test.data$opioid, test.data$WTFA_A)
auc<-WeightedAUC(tp.fp)#0.94

ggplot()+ geom_path(aes(FPR, TPR), data=tp.fp)+
  xlab("True positive rate (sensitivity)")+
  ylab("False positive rate")+
  coord_equal()+
  annotate(geom="text", x=0.5, y=0.5, label="AUC=0.94",
           color="black")+  ggtitle("Model 1 ROC curve")
#--------testing a model with interactions
set.seed(67)
#creating train and test set stratified by opioid use
yesdata<-cdata%>% filter(opioid=="Yes")#df of opioid users
nodata<-cdata%>% filter(opioid=="No")# df of non-users

yestraining.samples <- yesdata$opioid %>% createDataPartition(p = 0.8, list = FALSE)
yestrain.data  <- yesdata[yestraining.samples, ]
yestest.data <- na.omit(yesdata[-yestraining.samples, ])

notraining.samples <- nodata$opioid %>% createDataPartition(p = 0.8, list = FALSE)
notrain.data  <- nodata[notraining.samples, ]
notest.data <- na.omit(nodata[-notraining.samples, ])

train.data<-bind_rows(yestrain.data, notrain.data)
test.data<-bind_rows(yestest.data, notest.data)

train.design<-svydesign(id =~PPSU, strata =~PSTRAT,  nest=TRUE, weights=~WTFA_A, data = train.data)
mp<-svyglm(opioid~cpain+sex+age+married+veteran+income_grp+home_owner+uninsured+
             paybill+payworry+caredelay+foodworry+counseldelay+
             mobility1+mobility2+communicating+age65*cognitive+work_limited+participation+work_lastwk+
             health*fatigue+injury+dr_12months+wellness+urgent_care+hospitalized+
             flu_vaccine+cig100+sport+sunscreen+DM_risk+sleep_abnorm+sleep_med+
             anx_freq+anx_med+anx_level+depr_freq+depr_med+depr_level+mhrx,
           family=quasibinomial, design=train.design, na.action = na.omit)

mp<-svyglm(opioid~age65*mobility1+cpain*sex+health+fatigue+
             age+married+veteran+income_grp+home_owner+uninsured+
             paybill+payworry+caredelay+foodworry+counseldelay+
             mobility2+cognitive+communicating+work_limited+participation+work_lastwk+
             injury+dr_12months+wellness+urgent_care+hospitalized+
             flu_vaccine+cig100+sport+sunscreen+DM_risk+sleep_abnorm+sleep_med+
             anx_freq+anx_med+anx_level+depr_freq+depr_med+depr_level+mhrx,
           family=quasibinomial, design=train.design, na.action = na.omit)

mp1<-svyglm(opioid~mobility1+cpain*sex+age65*health+fatigue+
             age+married+veteran+income_grp+home_owner+uninsured+
             paybill+payworry+caredelay+foodworry+counseldelay+
             cognitive+communicating+work_limited+participation*mobility2+work_lastwk+
             injury+dr_12months+wellness+urgent_care+hospitalized+
             flu_vaccine+cig100+sport+sunscreen+DM_risk+sleep_abnorm+sleep_med+
             anx_freq+anx_med+anx_level+depr_freq+depr_med+depr_level+mhrx,
           family=quasibinomial, design=train.design, na.action = na.omit)

#deriving predictions for the test set
guess1<-predict(mp1, newdata=test.data, type="response")
tp.fp1<-WeightedROC(guess1, test.data$opioid, test.data$WTFA_A)
auc1<-WeightedAUC(tp.fp1)#0.94

roc_curve <- ggplot()+
  geom_path(aes(FPR, TPR), data=tp.fp1)+
  xlab("Specificity")+
  ylab("Sensitivity")+
  coord_equal()+ggtitle("Model 2")

error.fun.list <- list(
  FN=function(df)df$FN,
  FP=function(df)df$FP,
  errors=function(df)with(df, FP+FN)
)
all.error.list <- list()
for(error.type in names(error.fun.list)){
  error.fun <- error.fun.list[[error.type]]
  all.error.list[[error.type]] <-
    data.frame(tp.fp, error.type, weighted.error=error.fun(tp.fp))
}
all.error <- do.call(rbind, all.error.list)
fp.fn.colors <- c(FP="skyblue",
                  FN="#E41A1C",
                  errors="black")
#thresh<-all.error$threshold[which.min(all.error$FP +all.error$FN)]
#threshold by max(sensitivity+specificity)
thresh<-all.error$threshold[which.max(all.error$TPR +(1-all.error$FPR))]

ggplot()+
  scale_color_manual(values=fp.fn.colors)+
  geom_line(aes(threshold, weighted.error, color=error.type),
            data=all.error)+
  geom_vline(xintercept = thresh, color = "blue", size=0.5)+
  ggtitle("Model 2")

results<-data.frame(guess1)
predicted.classes <- ifelse(results$response > thresh, "Yes", "No")
confusionMatrix(as.factor(predicted.classes), test.data$opioid, positive="Yes")
#threshold=0.21, spe=0.965 sen=0.47 balanced accuracy=0.718
F1_Score(test.data$opioid, as.factor(predicted.classes), positive="Yes")
#0.41
precision <- posPredValue(as.factor(predicted.classes), test.data$opioid, positive="Yes")
recall <- sensitivity(as.factor(predicted.classes), test.data$opioid, positive="Yes")
F1 <- (2 * precision * recall) / (precision + recall)

#----------------interpretation of the model's coefficients
model <- glm(cpain~age, family=binomial, data=cdata, na.action = na.omit)
model<-svyglm(cpain~age+age65+health, family=quasibinomial, design=nhis, na.action = na.omit)
jtools::summ(model, confint = TRUE, digits = 2, pvals = FALSE, exp=TRUE, model.info = FALSE, model.fit = FALSE)
export_summs(mp, error_format = "[{conf.low}, {conf.high}]", error_pos=c("right"), 
             model.names= c("Odds ratios (95% confidence interval)"),
             confint = TRUE, digits = 2, exp=TRUE, pvals = FALSE, model.info = FALSE, model.fit = FALSE)
jtools::plot_summs(model)

export_summs(..., error_style = c("stderr", "ci", "statistic", "pvalue"),
             error_pos = c("below", "right", "same"), statistics = NULL,
             model.names = NULL, to.word = FALSE, word.file = NULL)








#-----------------------------------links

#logistic regression in rare events data
#https://gking.harvard.edu/files/abs/0s-abs.shtml

#stratified sampling
#https://towardsdatascience.com/stratified-sampling-and-how-to-perform-it-in-r-8b753efde1ef
#use Kfold cv becasue this is a small class?

#https://stats.stackexchange.com/questions/57031/interpreting-interaction-terms-in-logit-regression-with-categorical-variables
