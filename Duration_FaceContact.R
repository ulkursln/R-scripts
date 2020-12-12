#	Check & fix Model Assumptions:
#http://www.bodowinter.com/tutorial/bw_LME_tutorial1.pdf 


##Read Data
mydata<-read.delim("D:\\Folders\\doktora\\TIK4\\Analizler\\analysis_for_rawData\\RData\\RData_gazeAversionDurationsExtended.txt", sep= " ", header =T)
names(mydata)
ncol(mydata) #numberofColumns
nrow(mydata)
#"IntervieweeID"          "Duration"          
# "Role"               "Gender"     "PartnerGender"        
#"GendersWithPartner" "InterviewerID"

plot(Duration~Role, data=mydata)
#plot(Duration~Role, data=mydata[mydata$SessionID=1,])
####################################################

#(1)Detect outliers (Bu kýsým evaluation partý çýkartmamýzý saðladý ve )
boxplot(mydata$Duration, horizontal = TRUE)
mydata$Duration[order(mydata$Duration,decreasing = T)[1:1000]]
nrow(mydata)

mydata_withoutOutlier<-mydata[which(mydata$Duration <830),]
nrow(mydata_withoutOutlier)
boxplot(mydata_withoutOutlier$Duration, horizontal = TRUE)
mydata_withoutOutlier_duration_test=mydata_withoutOutlier


##to boxplot duration per role, analysis continue with mydata_withoutOutlier_duration_test
mydata_interviewer=mydata_withoutOutlier[which(mydata_withoutOutlier$Role=="Interviewer"),]
boxplot(mydata_interviewer$Duration, horizontal = TRUE)
mydata_interviewer$Duration[order(mydata_interviewer$Duration,decreasing = T)[1:1000]]
nrow(mydata_interviewer)
mydata_interviewer=mydata_withoutOutlier[which(mydata_withoutOutlier$Duration <700),]
boxplot(mydata_interviewer$Duration, horizontal = TRUE)
mydata_interviewer$Duration[order(mydata_interviewer$Duration,decreasing = T)[1:1000]]
nrow(mydata_interviewer)

mydata_interviewee=mydata_withoutOutlier[which(mydata_withoutOutlier$Role=="Interviewee"),]
boxplot(mydata_interviewee$Duration, horizontal = TRUE)
mydata_interviewee$Duration[order(mydata_interviewee$Duration,decreasing = T)[1:1000]]
nrow(mydata_interviewee)


#find means, SD by Roles
without_outlier=rbind(mydata_interviewer,mydata_interviewee)
nrow(mydata_interviewee)
meansRole=tapply(without_outlier$Duration, without_outlier$Role,mean)
sdsRole= tapply(without_outlier$Duration, without_outlier$Role,sd)

boxplot(without_outlier$Duration ~ without_outlier$Role, main = "Duration by Roles",
        xlab = "Role", ylab = "Duration")

nrow_interviewer=nrow(mydata_interviewer)
nrow_interviewee=nrow(mydata_interviewee)

se_interviewee= sdsRole[1] / sqrt(nrow_interviewee)
se_interviewer= sdsRole[2] / sqrt(nrow_interviewer)
meansRole
se_interviewee
se_interviewer

boxplot(without_outlier$Duration ~ without_outlier$Role, main = "Duration by Roles",
        xlab = "Role", ylab = "Duration")


####################################################
#Screen Data

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

#Duration ~Rolewhen PArtnerGenderMale
mydata_withoutOutlier_duration_test_PG_M = mydata_withoutOutlier_duration_test[which(mydata_withoutOutlier_duration_test$PartnerGender=="M"),]
meansPG_M=tapply(mydata_withoutOutlier_duration_test_PG_M$Duration, mydata_withoutOutlier_duration_test_PG_M$Role,mean)

#find means, SD by Genders
meansGender=tapply(mydata_withoutOutlier_duration_test$Duration, mydata_withoutOutlier_duration_test$Gender,mean)
sdsGender=tapply(mydata_withoutOutlier_duration_test$Duration, mydata_withoutOutlier_duration_test$Gender,sd)
#Duration ~Gender when PArtnerGenderFemale
mydata_withoutOutlier_duration_test_PG_F = mydata_withoutOutlier_duration_test[which(mydata_withoutOutlier_duration_test$PartnerGender=="F"),]
meansPG_F_G=tapply(mydata_withoutOutlier_duration_test_PG_F$Duration, mydata_withoutOutlier_duration_test_PG_F$Gender,mean)
sdsPG_F_G=tapply(mydata_withoutOutlier_duration_test_PG_F$Duration, mydata_withoutOutlier_duration_test_PG_F$Gender,sd)
PG_F_F=mydata_withoutOutlier_duration_test_PG_F[which(mydata_withoutOutlier_duration_test_PG_F$Gender=="F"),]
nrow_PG_F_F=nrow(PG_F_F)
PG_F_M=mydata_withoutOutlier_duration_test_PG_F[which(mydata_withoutOutlier_duration_test_PG_F$Gender=="M"),]
nrow_PG_F_M=nrow(PG_F_M)
se_PG_F_F= sdsPG_F_G[1] / sqrt(nrow_PG_F_F)
se_PG_F_M= sdsPG_F_G[2] / sqrt(nrow_PG_F_M)


#find means, SD by PartnerGender
meansPartnerGender=tapply(mydata_withoutOutlier_duration_test$Duration, mydata_withoutOutlier_duration_test$PartnerGender,mean)
sdsPartnerGender=tapply(mydata_withoutOutlier_duration_test$Duration, mydata_withoutOutlier_duration_test$PartnerGender,sd)

#Duration ~PartnerGender when Gender=Female
mydata_withoutOutlier_duration_test_G_F = mydata_withoutOutlier_duration_test[which(mydata_withoutOutlier_duration_test$Gender=="F"),]
meansG_F_PG=tapply(mydata_withoutOutlier_duration_test_G_F$Duration, mydata_withoutOutlier_duration_test_G_F$PartnerGender,mean)
sdsG_F_G=tapply(mydata_withoutOutlier_duration_test_G_F$Duration, mydata_withoutOutlier_duration_test_G_F$Gender,sd)
G_F_F=mydata_withoutOutlier_duration_test_G_F[which(mydata_withoutOutlier_duration_test_G_F$PartnerGender=="F"),]
nrow_G_F_F=nrow(G_F_F)
G_F_M=mydata_withoutOutlier_duration_test_G_F[which(mydata_withoutOutlier_duration_test_G_F$PartnerGender=="M"),]
nrow_G_F_M=nrow(G_F_M)
se_G_F_F= sdsG_F_G[1] / sqrt(nrow_G_F_F)
se_G_F_M= sdsG_F_G[2] / sqrt(nrow_G_F_M)

#find means, SD by InterviewerID
#install.packages("dplyr")
library(dplyr)
##Just interviewers
mydata_withoutOutlier_duration_test_interviewer = mydata_withoutOutlier_duration_test[which(mydata_withoutOutlier_duration_test$Role=="Interviewer"),]
grp_interviewer <- group_by(mydata_withoutOutlier_duration_test_interviewer, InterviewerID)
summarise(grp_interviewer, mean=mean(Duration), sd=sd(Duration))

##just interviewees
mydata_withoutOutlier_duration_test_interviewee = mydata_withoutOutlier_duration_test[which(mydata_withoutOutlier_duration_test$Role=="Interviewee"),]
grp_interviewee <- group_by(mydata_withoutOutlier_duration_test_interviewee, InterviewerID)
summarise(grp_interviewee, mean=mean(Duration), sd=sd(Duration))


#find means, SD by IntervieweeID
meansSession=tapply(mydata_withoutOutlier_duration_test$Duration, mydata_withoutOutlier_duration_test$IntervieweeID,mean)
sdSessions=tapply(mydata_withoutOutlier_duration_test$Duration, mydata_withoutOutlier_duration_test$IntervieweeID,sd)

#find means,SD by Role * GenderWithPartner
mean_wer_FF= mean(mydata_withoutOutlier_duration_test[mydata_withoutOutlier_duration_test$Role=="Interviewer" & mydata_withoutOutlier_duration_test$GendersWithPartner =="FF",]$Duration)
mean_wer_FM= mean(mydata_withoutOutlier_duration_test[mydata_withoutOutlier_duration_test$Role=="Interviewer" & mydata_withoutOutlier_duration_test$GendersWithPartner =="FM",]$Duration)
mean_wer_MF= mean(mydata_withoutOutlier_duration_test[mydata_withoutOutlier_duration_test$Role=="Interviewer" & mydata_withoutOutlier_duration_test$GendersWithPartner =="MF",]$Duration)
mean_wer_MM= mean(mydata_withoutOutlier_duration_test[mydata_withoutOutlier_duration_test$Role=="Interviewer" & mydata_withoutOutlier_duration_test$GendersWithPartner =="MM",]$Duration)
sd_wer_FF= sd(mydata_withoutOutlier_duration_test[mydata_withoutOutlier_duration_test$Role=="Interviewer" & mydata_withoutOutlier_duration_test$GendersWithPartner =="FF",]$Duration)
sd_wer_FM= sd(mydata_withoutOutlier_duration_test[mydata_withoutOutlier_duration_test$Role=="Interviewer" & mydata_withoutOutlier_duration_test$GendersWithPartner =="FM",]$Duration)
sd_wer_MF= sd(mydata_withoutOutlier_duration_test[mydata_withoutOutlier_duration_test$Role=="Interviewer" & mydata_withoutOutlier_duration_test$GendersWithPartner =="MF",]$Duration)
sd_wer_MM= sd(mydata_withoutOutlier_duration_test[mydata_withoutOutlier_duration_test$Role=="Interviewer" & mydata_withoutOutlier_duration_test$GendersWithPartner =="MM",]$Duration)

mean_wee_FF= mean(mydata_withoutOutlier_duration_test[mydata_withoutOutlier_duration_test$Role=="Interviewee" & mydata_withoutOutlier_duration_test$GendersWithPartner =="FF",]$Duration)
mean_wee_FM= mean(mydata_withoutOutlier_duration_test[mydata_withoutOutlier_duration_test$Role=="Interviewee" & mydata_withoutOutlier_duration_test$GendersWithPartner =="FM",]$Duration)
mean_wee_MF= mean(mydata_withoutOutlier_duration_test[mydata_withoutOutlier_duration_test$Role=="Interviewee" & mydata_withoutOutlier_duration_test$GendersWithPartner =="MF",]$Duration)
mean_wee_MM= mean(mydata_withoutOutlier_duration_test[mydata_withoutOutlier_duration_test$Role=="Interviewee" & mydata_withoutOutlier_duration_test$GendersWithPartner =="MM",]$Duration)
sd_wee_FF= sd(mydata_withoutOutlier_duration_test[mydata_withoutOutlier_duration_test$Role=="Interviewee" & mydata_withoutOutlier_duration_test$GendersWithPartner =="FF",]$Duration)
sd_wee_FM= sd(mydata_withoutOutlier_duration_test[mydata_withoutOutlier_duration_test$Role=="Interviewee" & mydata_withoutOutlier_duration_test$GendersWithPartner =="FM",]$Duration)
sd_wee_MF= sd(mydata_withoutOutlier_duration_test[mydata_withoutOutlier_duration_test$Role=="Interviewee" & mydata_withoutOutlier_duration_test$GendersWithPartner =="MF",]$Duration)
sd_wee_MM= sd(mydata_withoutOutlier_duration_test[mydata_withoutOutlier_duration_test$Role=="Interviewee" & mydata_withoutOutlier_duration_test$GendersWithPartner =="MM",]$Duration)

Type = c("wer_FF", "wer_FM", "wer_MF", "wer_MM","wee_FF", "wee_FM", "wee_MF", "wee_MM" )
Values_mean=c(mean_wer_FF, mean_wer_FM, mean_wer_MF, mean_wer_MM, mean_wee_FF, mean_wee_FM, mean_wee_MF,mean_wee_MM)
Values_sd=c(sd_wer_FF, sd_wer_FM, sd_wer_MF, sd_wer_MM, sd_wee_FF,sd_wee_FM,sd_wee_MF,sd_wee_MM)

#find means,SD by Role * Gender
mean_wer_F= mean(mydata_withoutOutlier_duration_test[mydata_withoutOutlier_duration_test$Role=="Interviewer" & mydata_withoutOutlier_duration_test$Gender =="F",]$Duration)
mean_wer_M= mean(mydata_withoutOutlier_duration_test[mydata_withoutOutlier_duration_test$Role=="Interviewer" & mydata_withoutOutlier_duration_test$Gender =="M",]$Duration)
mean_wee_F= mean(mydata_withoutOutlier_duration_test[mydata_withoutOutlier_duration_test$Role=="Interviewee" & mydata_withoutOutlier_duration_test$Gender =="F",]$Duration)
mean_wee_M= mean(mydata_withoutOutlier_duration_test[mydata_withoutOutlier_duration_test$Role=="Interviewee" & mydata_withoutOutlier_duration_test$Gender =="M",]$Duration)

sd_wer_F= sd(mydata_withoutOutlier_duration_test[mydata_withoutOutlier_duration_test$Role=="Interviewer" & mydata_withoutOutlier_duration_test$Gender =="F",]$Duration)
sd_wer_M= sd(mydata_withoutOutlier_duration_test[mydata_withoutOutlier_duration_test$Role=="Interviewer" & mydata_withoutOutlier_duration_test$Gender =="M",]$Duration)
sd_wee_F= sd(mydata_withoutOutlier_duration_test[mydata_withoutOutlier_duration_test$Role=="Interviewee" & mydata_withoutOutlier_duration_test$Gender =="F",]$Duration)
sd_wee_M= sd(mydata_withoutOutlier_duration_test[mydata_withoutOutlier_duration_test$Role=="Interviewee" & mydata_withoutOutlier_duration_test$Gender =="M",]$Duration)

#find means,SD by PartnerGender * Gender
mean_F_F= mean(mydata_withoutOutlier_duration_test[mydata_withoutOutlier_duration_test$PartnerGender=="F" & mydata_withoutOutlier_duration_test$Gender =="F",]$Duration)
mean_F_M= mean(mydata_withoutOutlier_duration_test[mydata_withoutOutlier_duration_test$PartnerGender=="F" & mydata_withoutOutlier_duration_test$Gender =="M",]$Duration)
mean_M_F= mean(mydata_withoutOutlier_duration_test[mydata_withoutOutlier_duration_test$PartnerGender=="M" & mydata_withoutOutlier_duration_test$Gender =="F",]$Duration)
mean_M_M= mean(mydata_withoutOutlier_duration_test[mydata_withoutOutlier_duration_test$PartnerGender=="M" & mydata_withoutOutlier_duration_test$Gender =="M",]$Duration)



Type = c("wer_F", "wer_M", "wee_F", "wee_M" )
Values_mean=c(mean_wer_F, mean_wer_M, mean_wee_F, mean_wee_M)
Values_sd=c(sd_wer_F, sd_wer_M, sd_wee_F, sd_wee_M)




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

library("lme4")

model_empty=lmer(Duration ~ 1+ (1|InterviewerID/IntervieweeID), data= mydata_withoutOutlier_duration_test, REML=FALSE)
ss <- getME(model_empty,c("theta","fixef"))
model_empty <- update(model_empty,start=ss,control=lmerControl(optCtrl=list(maxfun=2e4)))

model_1=lmer(Duration ~ Role+ (1|InterviewerID/IntervieweeID), data= mydata_withoutOutlier_duration_test, REML=FALSE)


anova(model_empty, model_1)
summary(model_1)

plot(model_1)

model_2=lmer(Duration ~ Role * Gender + (1|InterviewerID/IntervieweeID), data= mydata_withoutOutlier_duration_test)
anova(model_1,model_2)
plot(model_2)
#model_2 is better

model_3=lmer(Duration ~ Role + Gender + (1|InterviewerID/IntervieweeID), data= mydata_withoutOutlier_duration_test)
anova(model_2,model_3)
# still model_2 is better
#now we are are sure that there is an interaction between role and gender

model_4=lmer(Duration ~ Role * Gender * PartnerGender+ (1|InterviewerID/IntervieweeID), data= mydata_withoutOutlier_duration_test, REML=FALSE)
ss <- getME(model_4,c("theta","fixef"))
model_4 <- update(model_4,start=ss,control=lmerControl(optCtrl=list(maxfun=2e4)))
anova(model_2,model_4)
plot(model_4)
#model_4 is better


model_5= lmer(Duration ~ Role + Gender * PartnerGender+ (1|InterviewerID/IntervieweeID), data= mydata_withoutOutlier_duration_test, REML=FALSE)
ss <- getME(model_5,c("theta","fixef"))
model_5 <- update(model_5,start=ss,control=lmerControl(optCtrl=list(maxfun=2e4)))
model_5 <- update(model_5,start=ss,control=lmerControl(optimizer="bobyqa",
                                                       optCtrl=list(maxfun=2e5)))


anova(model_4,model_5)

model_5_1= lmer(Duration ~ Role + Gender + PartnerGender+ Gender:PartnerGender:Role + Gender:PartnerGender + (1|InterviewerID/IntervieweeID), data= mydata_withoutOutlier_duration_test, REML=FALSE)
ss <- getME(model_5_1,c("theta","fixef"))
model_5_1 <- update(model_5_1,start=ss,control=lmerControl(optCtrl=list(maxfun=2e4)))
model_5_1 <- update(model_5_1,start=ss,control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

#model_5 throw singularity error
#anova(model_4,model_5_1)                                                  
#summary(model_5_1)
#plot(model_5_1)


model_5_2= lmer(Duration ~ Role + Gender + PartnerGender+ Gender:PartnerGender:Role + Gender:PartnerGender + (1+Gender|InterviewerID:IntervieweeID) +(1|InterviewerID), data= mydata_withoutOutlier_duration_test, REML=FALSE)
#ss <- getME(model_5_2,c("theta","fixef"))
#model_5_2 <- update(model_5_2,start=ss,control=lmerControl(optCtrl=list(maxfun=2e4)))
#model_5_2 <- update(model_5_2,start=ss,control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(model_5_2)

anova(model_4,model_5_2) 

##Plotted




model_selected=model_4

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
qqp(mydata_withoutOutlier_duration_test$Duration, "norm")



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


##non-normal data, continue with pql
PQL <- glmmPQL(Duration ~ Role + Gender + PartnerGender+ Gender:PartnerGender:Role + Gender:PartnerGender, random=(~1|InterviewerID/IntervieweeID), family = gaussian(link = "log"),
               data = mydata_withoutOutlier_duration_test, verbose = FALSE )

#Throw error simplfy model:

PQL1 <- glmmPQL(Duration ~ Role + Gender*PartnerGender+ Role:Gender + Role:PartnerGender, random=(~1|InterviewerID/IntervieweeID), family = gaussian(link = "log"),
                data = mydata_withoutOutlier_duration_test, verbose = FALSE )

summary(PQL1)

PQL2 <- glmmPQL(Duration ~ Role + Gender*PartnerGender+ Role:Gender + Role:PartnerGender, random=(~1+Gender*PartnerGender|InterviewerID/IntervieweeID), family = gaussian(link = "log"),
                data = mydata_withoutOutlier_duration_test, verbose = FALSE )



PQL3 <- glmmPQL(Duration ~ Role*Gender*PartnerGender, random=(~1+Gender*PartnerGender|InterviewerID/IntervieweeID), family = gaussian(link = "log"),
                data = mydata_withoutOutlier_duration_test, verbose = FALSE )


model_selected=PQL3
plot(model_selected)
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
duration.emm <- emmeans(model_selected, ~ Role * Gender * PartnerGender)
pairs(duration.emm, simple = "Role")

##Investgate effect of Role
emm1 = emmeans(model_selected, spec='Role')
summary(emm1)
m_tukey = contrast(emm1, method='pairwise')
summary(m_tukey)

emmip(model_selected, Role ~ Gender | PartnerGender)
emmip(model_selected, Role ~ PartnerGender | Gender)
emm_s.t <- emmeans(model_selected, pairwise ~ Role | Gender)   
emm_s.t

emm_s.t <- emmeans(model_selected, pairwise ~  Role| PartnerGender)   
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


mydata_withoutOutlier_duration_test_PG_F_G_F = mydata_withoutOutlier_duration_test[mydata_withoutOutlier_duration_test$PartnerGender=="F" & mydata_withoutOutlier_duration_test$Gender=="F",]
meansPG_F_G_F=tapply(mydata_withoutOutlier_duration_test_PG_F_G_F$Duration, mydata_withoutOutlier_duration_test_PG_F_G_F$Role,mean)
meansPG_F_G_F

mydata_withoutOutlier_duration_test_PG_F_G_M = mydata_withoutOutlier_duration_test[mydata_withoutOutlier_duration_test$PartnerGender=="F" & mydata_withoutOutlier_duration_test$Gender=="M",]
meansPG_F_G_M=tapply(mydata_withoutOutlier_duration_test_PG_F_G_M$Duration, mydata_withoutOutlier_duration_test_PG_F_G_M$Role,mean)
meansPG_F_G_M



mydata_withoutOutlier_duration_test_PG_M_G_F = mydata_withoutOutlier_duration_test[mydata_withoutOutlier_duration_test$PartnerGender=="M" & mydata_withoutOutlier_duration_test$Gender=="F",]
meansPG_M_G_F=tapply(mydata_withoutOutlier_duration_test_PG_M_G_F$Duration, mydata_withoutOutlier_duration_test_PG_M_G_F$Role,mean)
meansPG_M_G_F

mydata_withoutOutlier_duration_test_PG_M_G_M = mydata_withoutOutlier_duration_test[mydata_withoutOutlier_duration_test$PartnerGender=="M" & mydata_withoutOutlier_duration_test$Gender=="M",]
meansPG_M_G_M=tapply(mydata_withoutOutlier_duration_test_PG_M_G_M$Duration, mydata_withoutOutlier_duration_test_PG_M_G_M$Role,mean)
meansPG_M_G_M


# Create a, b, c, d variables
Duration <- c(306.6, 247.8, 328.4, 269.3, 293.8, 325.8, 280.2, 286.1 )
Role <- c('Interviewee', 'Interviewer', 'Interviewee', 'Interviewer', 'Interviewee', 'Interviewer', 'Interviewee', 'Interviewer')
PartnerGender <- c('PG:F','PG:F','PG:F','PG:F', 'PG:M', 'PG:M', 'PG:M', 'PG:M')
Gender <- c('F','F', 'M', 'M', 'F','F','M', 'M')
# Join the variables to create a data frame
meansofDuration <- data.frame(Duration,Role,PartnerGender,Gender)
summary(meansofDuration)


p <- ggplot(data = meansofDuration, mapping = aes(x = Gender, y = Duration, color = Role)) + 
  facet_wrap(~ PartnerGender) + # Tell R that we also want to split up by audience.
  geom_point(size=5) + # Use geom_jitter instead of geom_point, otherwise the points are drawn over each other
  geom_smooth(method='lm', se=FALSE) # Draw a linear regression line through the points.

p + theme(
  plot.title = element_text(color="black", size=20, face="bold.italic"),
  axis.title.x = element_text(color="black", size=20, face="bold"),
  axis.text.x = element_text(color="black", size=18, face="bold"),
  axis.title.y = element_text(color="black", size=20, face="bold"),
  axis.text.y = element_text(color="black", size=18, face="bold"),
  strip.text.x = element_text(size = 20, colour = "black"),
  legend.text=element_text(color="black", size=18, face="bold"),
  legend.title=element_text(color="black", size=18, face="bold"),
)

####################################################
####################################################

##Investgate effect of Gender
pairs(duration.emm, simple = "Gender")
emmip(model_selected, Gender ~ Role | PartnerGender)
emmip(model_selected, Gender ~ PartnerGender | Role)

emm_s.t <- emmeans(model_selected, pairwise ~ Gender | Role)   
emm_s.t

emm_s.t <- emmeans(model_selected, pairwise ~ Gender | PartnerGender)   
emm_s.t


####################################################
####################################################

##Investgate effect of PartnerGender
pairs(duration.emm, simple = "PartnerGender")
emmip(model_selected, PartnerGender ~ Role | Gender)
emmip(model_selected, PartnerGender ~ Gender | Role)

emm_s.t <- emmeans(model_selected, pairwise ~ PartnerGender | Role)   
emm_s.t


emm_s.t <- emmeans(model_selected, pairwise ~ PartnerGender | Gender)   
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
mydata_withoutOutlier_duration_test_R_WER = mydata_withoutOutlier_duration_test[which(mydata_withoutOutlier_duration_test$Role=="Interviewer" & mydata_withoutOutlier_duration_test$Gender=="F" & mydata_withoutOutlier_duration_test$PartnerGender=='M' ),]
meansR_WER=tapply(mydata_withoutOutlier_duration_test_R_WER$Duration, mydata_withoutOutlier_duration_test_R_WER$PartnerGender,mean)
sdsR_WER=tapply(mydata_withoutOutlier_duration_test_R_WER$Duration, mydata_withoutOutlier_duration_test_R_WER$PartnerGender,sd)
R_WER_F=mydata_withoutOutlier_duration_test_R_WER[which(mydata_withoutOutlier_duration_test_R_WER$PartnerGender=="F"),]
nrow_R_WER_F=nrow(R_WER_F)
R_WER_M=mydata_withoutOutlier_duration_test_R_WER[which(mydata_withoutOutlier_duration_test_R_WER$PartnerGender=="M"),]
nrow_R_WER_M=nrow(R_WER_M)
se_R_WER_F= sdsR_WER[1] / sqrt(nrow_R_WER_F)
se_R_WER_M= sdsR_WER[2] / sqrt(nrow_R_WER_M)


contrast(duration.emm, "consec", simple = "each", combine = FALSE, adjust = "mvt")


mydata_withoutOutlier_duration_test_WER_G_F_PG_F = mydata_withoutOutlier_duration_test[mydata_withoutOutlier_duration_test$Gender=='F' & mydata_withoutOutlier_duration_test$PartnerGender=='F',]
meansWER_G_F_PG_F=tapply(mydata_withoutOutlier_duration_test_WER_G_F_PG_F$Duration, mydata_withoutOutlier_duration_test_WER_G_F_PG_F$Role,mean)
meansWER_G_F_PG_F




# Create a, b, c, d variables
Duration <- c(306.6, 247.8, 328.4, 269.3, 293.8, 325.8, 280.2, 286.1 )
Role <- c('Interviewee', 'Interviewer', 'Interviewee', 'Interviewer', 'Interviewee', 'Interviewer', 'Interviewee', 'Interviewer')
PartnerGender <- c('F','F','F','F', 'M', 'M', 'M', 'M')
Gender <- c('F','F', 'M', 'M', 'F','F','M', 'M')
# Join the variables to create a data frame
meansofDuration <- data.frame(Duration,Role,PartnerGender,Gender)
summary(meansofDuration)

library(tidyr)
meansofDuration=unite(meansofDuration, GenderPartnerGender, Gender:PartnerGender, sep=" ")

p <- ggplot(data = meansofDuration, mapping = aes(x = Role, y = Duration, color = GenderPartnerGender)) + 
  facet_wrap(~ Role) + # Tell R that we also want to split up by audience.
  geom_point(size=5) + # Use geom_jitter instead of geom_point, otherwise the points are drawn over each other
  geom_smooth(method='lm', se=FALSE) +# Draw a linear regression line through the points.
  labs(color = "Gender - PartnerGender") 

p + theme(
  plot.title = element_text(color="black", size=20, face="bold.italic"),
  axis.title.x = element_text(color="black", size=20, face="bold"),
  axis.text.x = element_text(color="black", size=18, face="bold"),
  axis.title.y = element_text(color="black", size=20, face="bold"),
  axis.text.y = element_text(color="black", size=18, face="bold"),
  strip.text.x = element_text(size = 20, colour = "black"),
  legend.text=element_text(color="black", size=18, face="bold"),
  legend.title=element_text(color="black", size=18, face="bold"),
)

