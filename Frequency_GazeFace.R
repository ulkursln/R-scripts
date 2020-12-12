#	Check & fix Model Assumptions:
#http://www.bodowinter.com/tutorial/bw_LME_tutorial1.pdf 


##Read Data
mydata<-read.delim("D:\\Folders\\doktora\\TIK4\\Analizler\\analysis_for_rawData\\RData\\RData_numberOfFaceContactsExtended.txt", sep= " ", header =T)
names(mydata)
ncol(mydata) #numberofColumns
nrow(mydata)
str(mydata)
# [1] "IntervieweeID"        "InterviewerFrequency" "IntervieweeFrequency" "InterviewerGender"   
# [5] "IntervieweeGender"    "InterviewerID" 

plot(InterviewerFrequency~IntervieweeFrequency, data=mydata)
boxplot(mydata$InterviewerFrequency,horizontal = TRUE)
mydata$InterviewerFrequency[order(mydata$InterviewerFrequency,decreasing = T)[1:25]]

mydata_withoutOutlier<-mydata[which(mydata$InterviewerFrequency <58),]
boxplot(mydata_withoutOutlier$InterviewerFrequency,horizontal = TRUE)
nrow(mydata_withoutOutlier)

boxplot(mydata_withoutOutlier$IntervieweeFrequency,horizontal = TRUE)


##*****************************************************
##multivariate analysis of variance (MANOVA).

#http://www.sthda.com/english/wiki/manova-test-in-r-multivariate-analysis-of-variance

#Check Normality
#install.packages("mvnormtest")
library("mvnormtest")
mshapiro.test(t(mydata_withoutOutlier$IntervieweeFrequency))
mshapiro.test(t(mydata_withoutOutlier$InterviewerFrequency))
#normally distributed

bartlett.test(IntervieweeFrequency ~ interaction(InterviewerGender,IntervieweeGender), data=mydata_withoutOutlier)
library(car)

leveneTest(IntervieweeFrequency ~ InterviewerGender * IntervieweeGender, data=mydata_withoutOutlier)
leveneTest(InterviewerFrequency ~ InterviewerGender * IntervieweeGender, data=mydata_withoutOutlier)
#homogeniy of variances provided


res.man <- manova(cbind(IntervieweeFrequency, InterviewerFrequency) ~ IntervieweeGender * InterviewerGender, data = mydata_withoutOutlier)
summary(res.man)
# Look to see which differ
summary.aov(res.man)


##*************************************************

#Same result with manova
summary(mydata_withoutOutlier)
pairs(mydata_withoutOutlier)
#plot(Duration~Role, data=mydata[mydata$SessionID=1,])

mlm1 <- lm(cbind(InterviewerFrequency, IntervieweeFrequency) ~ InterviewerGender + IntervieweeGender , data = mydata_withoutOutlier)
summary(mlm1)

mlm2 <- lm(cbind(InterviewerFrequency, IntervieweeFrequency) ~ InterviewerGender*IntervieweeGender, data = mydata_withoutOutlier)
summary(mlm2)

anova(mlm1,mlm2)

mlm1_1 <- lm(cbind(InterviewerFrequency, IntervieweeFrequency) ~ IntervieweeGender , data = mydata_withoutOutlier)
summary(mlm1_1)

anova(mlm1,mlm1_1)
mlm1_2 <- lm(cbind(InterviewerFrequency, IntervieweeFrequency) ~ InterviewerGender:IntervieweeGender , data = mydata_withoutOutlier)
summary(mlm1_2)

anova(mlm1_1,mlm1_2)

mlm1_3 <- lm(cbind(InterviewerFrequency, IntervieweeFrequency) ~ 1 , data = mydata_withoutOutlier)
anova(mlm1_1,mlm1_3)

mlm1_4 <- lm(cbind(InterviewerFrequency, IntervieweeFrequency) ~ 1 +(1|InterviewerID), data = mydata_withoutOutlier)

anova(mlm1_3,mlm1_4)


selectedModel=mlm1_3

head(resid(selectedModel))
head(fitted(selectedModel))
coef(selectedModel)
sigma(selectedModel)
vcov(selectedModel)


library(car)
Anova(selectedModel)


################################33
#PAired T-test
#http://www.sthda.com/english/wiki/paired-samples-t-test-in-r

summary(mydata_withoutOutlier)
t.test(mydata_withoutOutlier$InterviewerFrequency,mydata_withoutOutlier$IntervieweeFrequency, mu=0, alt="two.sided", paired=T, conf.level=0.95)
sd(mydata_withoutOutlier$InterviewerFrequency)
sd(mydata_withoutOutlier$IntervieweeFrequency)
ste <- function(x) sd(x)/sqrt(length(x))
ste_interviewer=ste(mydata_withoutOutlier$InterviewerFrequency)
ste_interviewee=ste(mydata_withoutOutlier$IntervieweeFrequency)

#install.packages("ggpubr")
library("ggpubr")


role<-c("Interviewer", "Interviewer", "Interviewer", "Interviewer", "Interviewer",
        "Interviewer", "Interviewer", "Interviewer", "Interviewer", "Interviewer",
        "Interviewer", "Interviewer", "Interviewer", "Interviewer", "Interviewer",
        "Interviewer", "Interviewer", "Interviewer", "Interviewer", "Interviewer",
        "Interviewer", "Interviewer", "Interviewer"
)

frequency<-mydata_withoutOutlier$InterviewerFrequency

role2<-c("Interviewee", "Interviewee", "Interviewee", "Interviewee", "Interviewee",
         "Interviewee", "Interviewee", "Interviewee", "Interviewee", "Interviewee",
         "Interviewee", "Interviewee", "Interviewee", "Interviewee", "Interviewee",
         "Interviewee", "Interviewee", "Interviewee", "Interviewee", "Interviewee",
         "Interviewee", "Interviewee", "Interviewee"
)

frequency2<-mydata_withoutOutlier$IntervieweeFrequency

columns_formated_data <- data.frame( 
  Role = role,
  Frequency = frequency
)

columns_formated_data2 <- data.frame( 
  Role = role2,
  Frequency = frequency2
)

columns_formated_data <- rbind(columns_formated_data, columns_formated_data2)

library("dplyr")
group_by(columns_formated_data, Role) %>%
  summarise(
    count = n(),
    mean = mean(Frequency, na.rm = TRUE),
    sd = sd(Frequency, na.rm = TRUE)
  )


##install
# if(!require(devtools)) install.packages("devtools")
# devtools::install_github("kassambara/ggpubr")

# Plot weight by group and color by group
library("ggpubr")
ggboxplot(columns_formated_data, x = "Role", y = "Frequency", 
          color = "Role", palette = c("#00AFBB", "#E7B800"),
          order = c("Interviewer", "Interviewee"),
          ylab = "Frequency", xlab = "Role")


#Preleminary test to check paired t-test assumptions
# compute the difference
d <- with(columns_formated_data, 
          Frequency[Role == "Interviewee"] - Frequency[Role == "Interviewer"])
# Shapiro-Wilk normality test for the differences
shapiro.test(d) # => p-value = 0.6141

# From the output, the p-value is greater than the significance level 0.05 implying that the distribution of 
# the differences (d) are not significantly different from normal distribution. 
# In other words, we can assume the normality.

# Compute t-test
res <- t.test(Frequency ~ Role, data = columns_formated_data, paired = TRUE)
res
res$p.value
# printing the mean
res$estimate
# printing the confidence interval
res$conf.int
