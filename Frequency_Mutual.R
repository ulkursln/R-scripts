#	Check & fix Model Assumptions:
#http://www.bodowinter.com/tutorial/bw_LME_tutorial1.pdf 


##Read Data
mydata<-read.delim("D:\\Folders\\doktora\\TIK6_sonrsi\\Projects\\CreateDataSet\\CreateDataSet\\bin\\Debug\\DataSetsForStatisticalAnalysis\\NumberofGazeAversionsPerMinute.txt", sep= " ", header =T)
names(mydata)
ncol(mydata) #numberofColumns
nrow(mydata)

# I[1] "INTERVIEWERID"     "INTERVIEWEEID"     "GAZEBEHAVIORTYPE"  "FREQUENCY"         "INTERVIEWERGENDER"
#[6] "INTERVIEWEEGENDER"

plot(FREQUENCY~GAZEBEHAVIORTYPE, data=mydata)
#plot(Duration~Role, data=mydata[mydata$SessionID=1,])
####################################################

#(1)Detect outliers (Bu kýsým evaluation partý çýkartmamýzý saðladý ve )
boxplot(mydata$FREQUENCY, horizontal = TRUE)
mydata$FREQUENCY[order(mydata$FREQUENCY,decreasing = T)[1:100]]
nrow(mydata)

mydata_withoutOutlier<-mydata[which(mydata$FREQUENCY <59),]
nrow(mydata_withoutOutlier)
boxplot(mydata_withoutOutlier$FREQUENCY, horizontal = TRUE)
mydata_withoutOutlier_frequency_test=mydata_withoutOutlier
summary(mydata_withoutOutlier_frequency_test)

mydata_withoutOutlier_fq_test_WER_G_F_PG_M = mydata_withoutOutlier_frequency_test[mydata_withoutOutlier_frequency_test$INTERVIEWEEGENDER=='Male' & mydata_withoutOutlier_frequency_test$INTERVIEWERGENDER=='Female',]
summary(mydata_withoutOutlier_fq_test_WER_G_F_PG_M)
meansWER_G_F_PG_M=tapply(mydata_withoutOutlier_fq_test_WER_G_F_PG_M$FREQUENCY, mydata_withoutOutlier_fq_test_WER_G_F_PG_M$GAZEBEHAVIORTYPE,mean)
meansWER_G_F_PG_M

sdsR_WER=tapply(mydata_withoutOutlier_fq_test_WER_G_F_PG_M$FREQUENCY, mydata_withoutOutlier_fq_test_WER_G_F_PG_M$GAZEBEHAVIORTYPE,sd)
R_WER_F=mydata_withoutOutlier_fq_test_WER_G_F_PG_M[which(mydata_withoutOutlier_fq_test_WER_G_F_PG_M$GAZEBEHAVIORTYPE=="WER_A_WEE_A"),]
nrow_R_WER_F=nrow(R_WER_F)
R_WER_M=mydata_withoutOutlier_fq_test_WER_G_F_PG_M[which(mydata_withoutOutlier_fq_test_WER_G_F_PG_M$GAZEBEHAVIORTYPE=="WER_FC_WEE_FC"),]
nrow_R_WER_M=nrow(R_WER_M)
se_R_WER_F= sdsR_WER[1] / sqrt(nrow_R_WER_F)
se_R_WER_M= sdsR_WER[4] / sqrt(nrow_R_WER_M)


mydata_withoutOutlier_fq_test_WEE_M_FCFC = mydata_withoutOutlier_frequency_test[mydata_withoutOutlier_frequency_test$INTERVIEWEEGENDER=='Male' & mydata_withoutOutlier_frequency_test$GAZEBEHAVIORTYPE=='WER_FC_WEE_FC',]
summary(mydata_withoutOutlier_fq_test_WEE_M_FCFC)
meansWEE_M_FCFC=tapply(mydata_withoutOutlier_fq_test_WEE_M_FCFC$FREQUENCY, mydata_withoutOutlier_fq_test_WEE_M_FCFC$INTERVIEWERGENDER,mean)
meansWEE_M_FCFC

sdsR_WER=tapply(mydata_withoutOutlier_fq_test_WEE_M_FCFC$FREQUENCY, mydata_withoutOutlier_fq_test_WEE_M_FCFC$INTERVIEWERGENDER,sd)
R_WER_F=mydata_withoutOutlier_fq_test_WEE_M_FCFC[which(mydata_withoutOutlier_fq_test_WEE_M_FCFC$INTERVIEWERGENDER=="Female"),]
nrow_R_WER_F=nrow(R_WER_F)
R_WER_M=mydata_withoutOutlier_fq_test_WEE_M_FCFC[which(mydata_withoutOutlier_fq_test_WEE_M_FCFC$INTERVIEWERGENDER=="Male"),]
nrow_R_WER_M=nrow(R_WER_M)
se_R_WER_F= sdsR_WER[1] / sqrt(nrow_R_WER_F)
se_R_WER_M= sdsR_WER[2] / sqrt(nrow_R_WER_M)

####################################################
#Find model

##if got lmer convergence warning , check:https://rstudio-pubs-static.s3.amazonaws.com/33653_57fc7b8e5d484c909b615d8633c01d51.html

#bumped up max number of iterations.
# ss <- getME(model_1,c("theta","fixef"))
# m2 <- update(model_1,start=ss,control=lmerControl(optCtrl=list(maxfun=2e4)))
# 
# #Try different optimizer 
# model_1 <- update(model_1,start=ss,control=lmerControl(optimizer="bobyqa",
#                                                  optCtrl=list(maxfun=2e5)))

##Significant difference and lower AIC indicating it's a better model

# if when type (summary) there is sigunlarity problem:
#his indicates that some effects are linear combinations of each other or
#that there's a variance of 0 somewhere. 
#This can also happen when p>>n (more parameters than samples)
library(lme4)
library(nlme)


model_empty=lmer(FREQUENCY ~ 1+ (1|INTERVIEWERID/INTERVIEWEEID), data= mydata_withoutOutlier_frequency_test, REML=FALSE)
#ss <- getME(model_empty,c("theta","fixef"))
#model_empty <- update(model_empty,start=ss,control=lmerControl(optCtrl=list(maxfun=2e4)))

model_1=lmer(FREQUENCY ~ GAZEBEHAVIORTYPE+ (1|INTERVIEWERID/INTERVIEWEEID), data= mydata_withoutOutlier_frequency_test, REML=FALSE)


anova(model_empty, model_1)
summary(model_1)

plot(model_1)

model_2=lmer(FREQUENCY ~ GAZEBEHAVIORTYPE *  INTERVIEWEEGENDER * INTERVIEWERGENDER+(1|INTERVIEWERID/INTERVIEWEEID), data= mydata_withoutOutlier_frequency_test, REML=FALSE)
#ss <- getME(model_2,c("theta","fixef"))
#model_2 <- update(model_2,start=ss,control=lmerControl(optCtrl=list(maxfun=2e4)))
#model_2 <- update(model_2,start=ss,control=lmerControl(optimizer="bobyqa",
 #                                                      optCtrl=list(maxfun=2e5)))
anova(model_empty,model_2)
plot(model_2)
#model_2 is better


model_3=lmer(FREQUENCY ~ GAZEBEHAVIORTYPE +  INTERVIEWEEGENDER + INTERVIEWERGENDER+(1|INTERVIEWERID/INTERVIEWEEID), data= mydata_withoutOutlier_frequency_test, REML=FALSE)
anova(model_2,model_3)
#model_2 is better

model_selected=model_2
plot(model_selected)
library(car)
Anova(model_selected)


####################################################
####Check assumptions
### Here’s	the	conditions	 that	have	 to	be	satisfied	
##in	order	for	the	linear	model	to	be	meaningful:

##Lets check the linearity assumptions:
##Find out the best fitted model
#(1)	Independence
# The	 independence	 assumption	 is	 by	 far	 the	 most	 important	 assumption	 of	 all	statistical	 tests.	
#the	 data	 points	 should	 come	 from	different	subjects.	
#And	each	subject	should	only	contribute	one	data	point, i.e., one row
#You can you	 need	 to	 resolve	 these	non-independencies at	 the	analysis	 stage.	 
#This	is	 where	mixed	models come in handy
##Done





#(1)	Linearity
#This will be checked after model is created, by ploting model : plot(model) and
#review the residual plot ,  If	 there	 were	 a	nonlinear	 or	 curvy	 pattern,	 
#then	 this	would	indicate a	 violation	 of	 the	linearity	assumption.	

# plot(fitted(model_selected),residuals(model_selected))
# plot(model_selected)

plot(model_selected, xlab="Fitted values", ylab = "Residuals" )


require(car)
## Loading required package: car
require(MASS)
qqp(mydata_withoutOutlier_frequency_test$FREQUENCY, "norm",xlab="norm quantiles", ylab = "Frequency")



#(2) Absence	of	collinearity
#When	two	fixed	effects	(two	predictors)	are	correlated	with	each	other,	they	are	
#said	 to	 be	 collinear, think	 about	 which	 one	 is	 the	 most	 meaningful	 
#and	 drop	 the	 others	




#(3) Absence	of	heteroskedasticity
#	 the	 residuals of	 your	 model	need	 to	roughly	have	a	similar	amount	of	deviation	 
# from	your	predicted	values.	
# Again,	 we	 can	 check	 this	 by	 looking	 at	 a	 residual	 plot.
# If the	variance is	not	homoscedastic: e.g.
#it’s	smaller	in	the	lower	range	and	larger	in	the	higher	range,
#	transforming	your	data	often	helps.	Consider	a	log-transform	


plot(model_selected)

#(4) 	Normality	of residuals
# The	 normality	 of	 residuals	 assumption	 is	 the	 one	 that	 is	 least	 important.	
# Especially for that big size data
#Anyway if you want to check: hist(residuals(mdl)) or qqnorm(residuals(mdl))

hist(residuals(model_selected))

#not that better,  but it is not much important especially with large enough data

#(5)Absence	of	influential	data	points
#dfbeta(mdl), #but since we have so much daa without outliers we dont need to check this assumption



summary(model_selected)

####################################################
##Post-hoc tests

library(emmeans)

#https://cran.rstudio.com/web/packages/emmeans/vignettes/interactions.html

# Examining this plot, we see that the “medium” mean is not always higher; so the marginal means, 
# and the way they compare, does not represent what is always the case. Moreover, what is evident in the plot 
# is that the peak for medium-size cars occurs for only one of the two filter types. 
# So it seems more useful to do the comparisons of size separately for each filter type. This is easily done, 
# simply by conditioning on type:

# I[1] "INTERVIEWERID"     "INTERVIEWEEID"     "GAZEBEHAVIORTYPE"  "FREQUENCY"         "INTERVIEWERGENDER"
#[6] "INTERVIEWEEGENDER"

frequency.emm <- emmeans(model_selected, ~ GAZEBEHAVIORTYPE *  INTERVIEWEEGENDER * INTERVIEWERGENDER)
pairs(frequency.emm, simple = "GAZEBEHAVIORTYPE")

##Investgate effect of GAZEBEHAVIORTYPE
emm1 = emmeans(model_selected, spec='GAZEBEHAVIORTYPE')
summary(emm1)
m_tukey = contrast(emm1, method='pairwise')
summary(m_tukey)

emmip(model_selected, GAZEBEHAVIORTYPE ~ INTERVIEWEEGENDER | INTERVIEWERGENDER)
emmip(model_selected, GAZEBEHAVIORTYPE ~ INTERVIEWERGENDER | INTERVIEWEEGENDER)
emm_s.t <- emmeans(model_selected, pairwise ~ GAZEBEHAVIORTYPE | INTERVIEWEEGENDER)   
emm_s.t

emm_s.t <- emmeans(model_selected, pairwise ~  GAZEBEHAVIORTYPE| INTERVIEWERGENDER)   
emm_s.t



#find means, SD by Roles
meansRole=tapply(mydata_withoutOutlier_duration_test$Duration, mydata_withoutOutlier_duration_test$Role,mean)
sdsRole= tapply(mydata_withoutOutlier_duration_test$Duration, mydata_withoutOutlier_duration_test$Role,sd)

boxplot(mydata_withoutOutlier_duration_test$Duration ~ mydata_withoutOutlier_duration_test$Role, main = "Duration by Roles",
        xlab = "Role", ylab = "Duration")

#Duration ~Role when PArtnerGenderFemale
mydata_withoutOutlier_duration_test_PG_F = mydata_withoutOutlier_duration_test[which(mydata_withoutOutlier_duration_test$PartnerGender=="F"),]
meansPG_F=tapply(mydata_withoutOutlier_duration_test_PG_F$Duration, mydata_withoutOutlier_duration_test_PG_F$Role,mean)
sdsPG_F=tapply(mydata_withoutOutlier_duration_test_PG_F$Duration, mydata_withoutOutlier_duration_test_PG_F$Role,sd)
PG_F_interviewer=mydata_withoutOutlier_duration_test_PG_F[which(mydata_withoutOutlier_duration_test_PG_F$Role=="Interviewer"),]
nrow_PG_F_interviewer=nrow(PG_F_interviewer)
PG_F_interviewee=mydata_withoutOutlier_duration_test_PG_F[which(mydata_withoutOutlier_duration_test_PG_F$Role=="Interviewee"),]
nrow_PG_F_interviewee=nrow(PG_F_interviewee)
se_PG_F_interviewee= sdsPG_F[1] / sqrt(nrow_PG_F_interviewee)
se_PG_F_interviewer= sdsPG_F[2] / sqrt(nrow_PG_F_interviewer)


####################################################
####################################################

##Investgate effect of INTERVIEWERGENDER
pairs(frequency.emm, simple = "INTERVIEWERGENDER")
emmip(model_selected, INTERVIEWERGENDER ~ GAZEBEHAVIORTYPE | INTERVIEWEEGENDER)
emmip(model_selected, INTERVIEWERGENDER ~ INTERVIEWEEGENDER | GAZEBEHAVIORTYPE)

emm_s.t <- emmeans(model_selected, pairwise ~ INTERVIEWERGENDER | GAZEBEHAVIORTYPE)   
emm_s.t

emm_s.t <- emmeans(model_selected, pairwise ~ INTERVIEWERGENDER | INTERVIEWEEGENDER)   
emm_s.t


####################################################
####################################################

##Investgate effect of INTERVIEWEEGENDER
pairs(frequency.emm, simple = "INTERVIEWEEGENDER")
emmip(model_selected, INTERVIEWEEGENDER ~ GAZEBEHAVIORTYPE | INTERVIEWERGENDER)
emmip(model_selected, INTERVIEWEEGENDER ~ INTERVIEWERGENDER | GAZEBEHAVIORTYPE)

emm_s.t <- emmeans(model_selected, pairwise ~ INTERVIEWEEGENDER | GAZEBEHAVIORTYPE)   
emm_s.t


emm_s.t <- emmeans(model_selected, pairwise ~ INTERVIEWEEGENDER | INTERVIEWERGENDER)   
emm_s.t


#Duration ~PartnerGender when RoleInterviewerand Gender Female
mydata_withoutOutlier_duration_test_R_WER = mydata_withoutOutlier_duration_test[which(mydata_withoutOutlier_duration_test$Role=="Interviewer" & mydata_withoutOutlier_duration_test$Gender="F"),]
meansR_WER=tapply(mydata_withoutOutlier_duration_test_R_WER$Duration, mydata_withoutOutlier_duration_test_R_WER$PartnerGender,mean)
sdsR_WER=tapply(mydata_withoutOutlier_duration_test_R_WER$Duration, mydata_withoutOutlier_duration_test_R_WER$PartnerGender,sd)
R_WER_F=mydata_withoutOutlier_duration_test_R_WER[which(mydata_withoutOutlier_duration_test_R_WER$PartnerGender=="F"),]
nrow_R_WER_F=nrow(R_WER_F)
R_WER_M=mydata_withoutOutlier_duration_test_R_WER[which(mydata_withoutOutlier_duration_test_R_WER$PartnerGender=="M"),]
nrow_R_WER_M=nrow(R_WER_M)
se_R_WER_F= sdsR_WER[1] / sqrt(nrow_R_WER_F)
se_R_WER_M= sdsR_WER[2] / sqrt(nrow_R_WER_M)

####################################################
####################################################

#Duration ~Gender*PartnerGender 
pairs(duration.emm, simple = list("Role", c("Gender", "PartnerGender")))
emm1 = emmeans(model_selected, ~ Gender* PartnerGender)
summary(emm1)
TUKEY = contrast(emm1, method = 'pairwise')
summary(TUKEY)
# Tuckey test representation :
plot(TUKEY , las=1 , col="brown" )


emmip(model_selected, Gender *PartnerGender ~ Role )

emm_s.t <- emmeans(model_selected, pairwise ~ Gender *PartnerGender | Role)   
emm_s.t



#Duration ~PartnerGender when RoleInterviewer and  and Gender Female
mydata_withoutOutlier_duration_test_R_WER = mydata_withoutOutlier_duration_test[which(mydata_withoutOutlier_duration_test$Role=="Interviewer" & mydata_withoutOutlier_duration_test$Gender="F" ),]
meansR_WER=tapply(mydata_withoutOutlier_duration_test_R_WER$Duration, mydata_withoutOutlier_duration_test_R_WER$PartnerGender,mean)
sdsR_WER=tapply(mydata_withoutOutlier_duration_test_R_WER$Duration, mydata_withoutOutlier_duration_test_R_WER$PartnerGender,sd)
R_WER_F=mydata_withoutOutlier_duration_test_R_WER[which(mydata_withoutOutlier_duration_test_R_WER$PartnerGender=="F"),]
nrow_R_WER_F=nrow(R_WER_F)
R_WER_M=mydata_withoutOutlier_duration_test_R_WER[which(mydata_withoutOutlier_duration_test_R_WER$PartnerGender=="M"),]
nrow_R_WER_M=nrow(R_WER_M)
se_R_WER_F= sdsR_WER[1] / sqrt(nrow_R_WER_F)
se_R_WER_M= sdsR_WER[2] / sqrt(nrow_R_WER_M)


contrast(duration.emm, "consec", simple = "each", combine = FALSE, adjust = "mvt")