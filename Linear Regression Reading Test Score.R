setwd("D:\\Study  Materials\\R Language\\DataSet")

pisaTrain=read.csv("pisa2009train.csv")
pisaTest=read.csv("pisa2009test.csv")

#How many students are there in the training set?
nrow(pisaTrain)

#Using tapply() on pisaTrain, what is the average reading test score of males?
tapply(pisaTrain$readingScore,pisaTrain$male,mean)

#Which variables are missing data in at least one observation in the training set? S
sapply(pisaTrain,function(x){sum(is.na(x))}) #gives all counts
#from above select all >0's
colnames(pisaTrain)[sapply(pisaTrain,function(x){sum(is.na(x))})>0]

#Identify the missing values and replace them with an appropriate value. 
#simce I have to fill the mssing values in each of the columns in both train and test
#i wil have to fill the nas in train and then in test, 2 times...
#to avoid this i will merge them and then fill for the merged dataset and after filling it
#split it back in the ratoi as they were earlier
df_all=rbind(pisaTrain,pisaTest)
dim(df_all)

df_all$male=as.factor(df_all$male)
df_all$preschool=as.factor(df_all$preschool)
df_all$expectBachelors=as.factor(df_all$expectBachelors)
df_all$motherHS=as.factor(df_all$motherHS)
df_all$motherBachelors=as.factor(df_all$motherBachelors)
df_all$fatherBachelors=as.factor(df_all$fatherBachelors)
df_all$fatherHS=as.factor(df_all$fatherHS)
df_all$schoolHasLibrary=as.factor(df_all$schoolHasLibrary)
df_all$publicSchool=as.factor(df_all$publicSchool)
df_all$urban=as.factor(df_all$urban)
df_all$motherWork=as.factor(df_all$motherWork)
df_all$fatherWork=as.factor(df_all$fatherWork)
df_all$selfBornUS=as.factor(df_all$selfBornUS)
df_all$motherBornUS=as.factor(df_all$motherBornUS)
df_all$fatherBornUS=as.factor(df_all$fatherBornUS)
df_all$englishAtHome=as.factor(df_all$englishAtHome)
df_all$computerForSchoolwork=as.factor(df_all$computerForSchoolwork)
df_all$read30MinsADay=as.factor(df_all$read30MinsADay)

df_all$raceeth[is.na(df_all$raceeth)]="White"
df_all$preschool[is.na(df_all$preschool)]=1
df_all$expectBachelors[is.na(df_all$expectBachelors)]=1
df_all$motherHS[is.na(df_all$motherHS)]=1
df_all$motherBachelors[is.na(df_all$motherBachelors)]=1
df_all$motherWork[is.na(df_all$motherWork)]=1
df_all$fatherHS[is.na(df_all$fatherHS)]=1
df_all$fatherBachelors[is.na(df_all$fatherBachelors)]=1
df_all$fatherWork[is.na(df_all$fatherWork)]=1
df_all$selfBornUS[is.na(df_all$selfBornUS)]=1
df_all$motherBornUS[is.na(df_all$motherBornUS)]=1
df_all$fatherBornUS[is.na(df_all$fatherBornUS)]=1
df_all$englishAtHome[is.na(df_all$englishAtHome)]=1
df_all$computerForSchoolwork[is.na(df_all$computerForSchoolwork)]=1
df_all$read30MinsADay[is.na(df_all$read30MinsADay)]=1
df_all$read30MinsADay[is.na(df_all$read30MinsADay)]=1
df_all$minutesPerWeekEnglish[is.na(df_all$minutesPerWeekEnglish)]=median(df_all$minutesPerWeekEnglish,na.rm = T)
df_all$studentsInEnglish[is.na(df_all$studentsInEnglish)]=mean(df_all$studentsInEnglish,na.rm = T)
df_all$schoolHasLibrary[is.na(df_all$schoolHasLibrary)]=1
df_all$schoolSize[is.na(df_all$schoolSize)]=median(df_all$schoolSize,na.rm = T)

sum(is.na(df_all))
summary(df_all)

#Filling missing value is a pain there is a pacage MICE which help us to do this

library(mice)
filled_data = mice(df_all,m=2,meth='pmm',seed=500)

#split back in the same ratoi as train & test
pisaTrain_filled=head(df_all,nrow(pisaTrain))
pisaTest_filled=head(df_all,nrow(pisaTest))

#To include unordered factors in a linear regression model, we define one level as the 
#"reference level" and add a binary variable for each of the remaining levels. In this way, 
#a factor with n levels is replaced by n-1 binary variables. The reference level is typically
#selected to be the most frequently occurring level in the dataset.

pisaTrain_filled$raceeth = relevel(pisaTrain_filled$raceeth, "White")
pisaTest_filled$raceeth = relevel(pisaTest_filled$raceeth, "White")

#Now, build a linear regression model (call it lmScore) using the training set to predict 
#readingScore using all the remaining variables.
Model1=lm(readingScore~.,data=pisaTrain_filled)
summary(Model1)

library(car)
vif(Model1)
#need to analyze output

Model2=lm(readingScore~grade+male+englishAtHome+computerForSchoolwork+read30MinsADay+minutesPerWeekEnglish+publicSchool+urban+schoolSize+expectBachelors+raceeth,data=pisaTrain_filled)
summary(Model2)

#What is the training-set root-mean squared error (RMSE) of lmScore?
sqrt(mean(Model2$residuals^2))

#Consider two students A and B. They have all variable values the same, except that student A 
#is in grade 11 and student B is in grade 9. What is the predicted reading score of student A 
#minus the predicted reading score of student B?
#The coefficient 29.54 on grade is the difference in reading score between two students who 
#are identical other than having a difference in grade of 1. Because A and B have a difference 
#in grade of 2, the model predicts that student A has a reading score that is 2*29.54 larger.

#What is the meaning of the coefficient associated with variable raceethAsian?
#The only difference between an Asian student and white student with otherwise identical 
#variables is that the former has raceethAsian=1 and the latter has raceethAsian=0. 
#The predicted reading score for these two students will differ by the coefficient on the 
#variable raceethAsian

#Based on the significance codes, which variables are candidates for removal from the model? 
#Select all that apply. (We'll assume that the factor variable raceeth should only be removed 
#if none of its levels are significant.)
#From summary(Model1), we can see which variables were significant at the 0.05 level. Because 
#several of the binary variables generated from the race factor variable are significant, 
#we should not remove this variable.


#Using the "predict" function and supplying the "newdata" argument, use the lmScore model to 
#predict the reading scores of students in pisaTest. 
predTest=predict(Model2,newdata=pisaTest_filled)
summary(predTest)

#What is the sum of squared errors (SSE) of lmScore on the testing set?
SSE=sum((pisaTest_filled$readingScore-predTest)^2)
SSE

#What is the root-mean squared error (RMSE) of lmScore on the testing set?
sqrt(mean((pisaTest_filled$readingScore-predTest)^2))

#What is the predicted test score used in the baseline model?
baseline_Model=mean(pisaTrain_filled$readingScore)
baseline_Model

#What is the sum of squared errors of the baseline model on the testing set?
SST=sum((pisaTest_filled$readingScore-baseline_Model)^2)
SST

#What is the test-set R-squared value of lmScore?
R2=1-(SSE/SST)
R2