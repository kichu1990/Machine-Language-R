#Suppose the coefficients of a logistic regression model with two independent variables are as 
#follows:
#B0=-1.5,B1=3.B2=0.5

#And we have an observation with the following values for the independent variables:
#x1=1,x2=5

#What is the value of the Logit for this observation?
-1.5+(3*1)+(-0.5*5)
#The Logit is just log(Odds), and looks like the linear regression equation.

#What is the value of the Odds for this observation? 
exp(-1)

#What is the value of P(y = 1) for this observation?
#P(y = 1) = 1/(1 + e^(-Logit)) 
1/(1+exp(1))

###############################################################################################
setwd("C:\\Users\\abhic6\\Documents\\R")
loans = read.csv("loans.csv")

#What proportion of the loans in the dataset were not paid in full? 
#Please input a number between 0 and 1.
prop.table(table(loans$not.fully.paid))

#Which of the following variables has at least one missing observation? Select all that apply.
sapply(loans,function(x){sum(is.na(x))})

#Which of the following is the best reason to fill in the missing values for these variables 
#instead of removing observations with missing data? 
View(loans)
str(loans)
loans$inq.last.6mths=as.factor(loans$inq.last.6mths)
loans$pub.rec =as.factor(loans$pub.rec)
loans$credit.policy =as.factor(loans$credit.policy)
loans$not.fully.paid =as.factor(loans$not.fully.paid)

boxplot(loans$days.with.cr.line)
missing = subset(loans, is.na(log.annual.inc) | is.na(days.with.cr.line) | is.na(revol.util) | 
                   is.na(inq.last.6mths) | is.na(delinq.2yrs) | is.na(pub.rec))
table(missing$not.fully.paid)

#From nrow(missing), we see that only 62 of 9578 loans have missing data; removing this small 
#number of observations would not lead to overfitting. From table(missing$not.fully.paid), 
#we see that 12 of 62 loans with missing data were not fully paid, or 19.35%. This rate is 
#similar to the 16.01% across all loans, so the form of biasing described is not an issue. 
#However, to predict risk for loans with missing data we need to fill in the missing values 
#instead of removing the observations.

library(mice)
set.seed(144)
vars.for.imputation = setdiff(names(loans), "not.fully.paid")
imputed = complete(mice(loans[vars.for.imputation]))
loans[vars.for.imputation] = imputed

setwd("D:\\Study  Materials\\R Language\\DataSet")
loans_imputed = read.csv("loans_imputed.csv")

summary(loans)
summary(loans_imputed)

loans$inq.last.6mths[is.na(loans$inq.last.6mths)]=0
loans$pub.rec[is.na(loans$pub.rec)]=0
loans$delinq.2yrs[is.na(loans$delinq.2yrs)]=0

hist(loans$int.rate)
hist(loans$log.annual.inc)
hist(loans$days.with.cr.line)
hist(loans$revol.util)
loans$int.rate[is.na(loans$int.rate)]=mean(loans$int.rate,na.rm=T)
loans$log.annual.inc[is.na(loans$log.annual.inc)]=mean(loans$log.annual.inc,na.rm=T)
loans$days.with.cr.line[is.na(loans$days.with.cr.line)]=median(loans$days.with.cr.line,na.rm=T)
loans$revol.util[is.na(loans$revol.util)]=median(loans$revol.util,na.rm=T)

#Handling Outliers
boxplot(loans$revol.bal)
Q1_revol.bal=quantile(loans$revol.bal,0.25)
Q2_revol.bal=quantile(loans$revol.bal,0.50)
Q3_revol.bal=quantile(loans$revol.bal,0.75)
IQR_revol.bal=Q3_revol.bal-Q1_revol.bal
Q3_revol.bal+(1.5*IQR_revol.bal)
loans$revol.bal=sapply(loans$revol.bal,function(x){ ifelse(x>40843.25,40843.25,x)})

boxplot(loans$days.with.cr.line)
Q1_days.with.cr.line=quantile(loans$days.with.cr.line,0.25)
Q2_days.with.cr.line=quantile(loans$days.with.cr.line,0.50)
Q3_days.with.cr.line=quantile(loans$days.with.cr.line,0.75)
IQR_days.with.cr.line=Q3_days.with.cr.line-Q1_days.with.cr.line
Q3_days.with.cr.line+(1.5*IQR_days.with.cr.line)
loans$days.with.cr.line=sapply(loans$days.with.cr.line,function(x){ ifelse(x>10094.9,10094.9 ,x)})

boxplot(loans$log.annual.inc)
Q1_log.annual.inc=quantile(loans$log.annual.inc,0.25)
Q2_log.annual.inc=quantile(loans$log.annual.inc,0.50)
Q3_log.annual.inc=quantile(loans$log.annual.inc,0.75)
IQR_log.annual.inc=Q3_log.annual.inc-Q1_log.annual.inc
Q3_log.annual.inc+(1.5*IQR_log.annual.inc)
Q1_log.annual.inc-(1.5*IQR_log.annual.inc)
loans$log.annual.inc=sapply(loans$log.annual.inc,function(x){ ifelse(x>12.38696,12.38696 ,x)})
loans$log.annual.inc=sapply(loans$log.annual.inc,function(x){ ifelse(x<9.461286,9.461286 ,x)})

boxplot(loans$int.rate)
Q1_int.rate=quantile(loans$int.rate,0.25)
Q2_int.rate=quantile(loans$int.rate,0.50)
Q3_int.rate=quantile(loans$int.rate,0.75)
IQR_int.rate=Q3_int.rate-Q1_int.rate
Q3_int.rate+(1.5*IQR_int.rate)
loans$int.rate=sapply(loans$int.rate,function(x){ ifelse(x>0.1959,0.1959,x)})

boxplot(loans$installment)
Q1_installment=quantile(loans$installment,0.25)
Q2_installment=quantile(loans$installment,0.50)
Q3_installment=quantile(loans$installment,0.75)
IQR_installment=Q3_installment-Q1_installment
Q3_installment+(1.5*IQR_installment)
loans$installment=sapply(loans$installment,function(x){ ifelse(x>836.2512,836.2512,x)})

#What best describes the process we just used to handle missing values?
#We predicted missing variable values using the available independent variables for each 
#observation.

#use the sample.split function to select the 70% of observations for the training set 
#(the dependent variable for sample.split is not.fully.paid). 

library(glmnet)
library(caTools)
set.seed(144)
spl= sample.split(loans$not.fully.paid, SplitRatio=0.70)
train_loans = subset(loans,spl==T)
test_loans  = subset(loans,spl==F)

cor(train_loans[,unlist(lapply(train_loans, is.numeric))])

Model1=glm(not.fully.paid~.,data=train_loans,family="binomial")
summary(Model1)
#AIC: 5520.8

#Checking accuracy on Train Data
res =predict(Model1,train_loans,type = "response")
table(res)
result = ifelse(res > 0.5, 1, 0)
table(result)
table(ActualValue = train_loans$not.fully.paid, PredictedValue = res >0.5))
(5580+44)/nrow(train_loans) #0.838777

#check accuracy on Test Data
test_loans$pred_test=predict(Model1,newdata=test_loans,type="response")
table(test_loans$not.fully.paid,test_loans$pred_test > 0.5)
#Accuaracy Of the Model
(2393+15)/nrow(test_loans) #0.8381483

#Building new Model based on Significance variable
Model2=glm(not.fully.paid~.-revol.util-delinq.2yrs-dti-days.with.cr.line,data=train_loans,family="binomial")
summary(Model2)
#AIC: 5521.3

#Checking accuracy on Train Data
res =predict(Model2,train_loans,type = "response")
table(res)
result = ifelse(res > 0.5, 1, 0)
table(result)
table(ActualValue = train_loans$not.fully.paid, PredictedValue = res >0.5)
(5580+43)/nrow(train_loans) #0.8386279

#check accuracy on Test Data
test_loans$pred_test=predict(Model2,newdata=test_loans,type="response")
table(test_loans$not.fully.paid,test_loans$pred_test > 0.5)
#Accuaracy Of the Model
(2396+14)/nrow(test_loans) #0.8388444

Model1$xlevels[["delinq.2yrs"]] <- union(Model1$xlevels[["delinq.2yrs"]], levels(test_loans$delinq.2yrs))

#Doing Step wise Logistic Regression and Build Model
Model3=glm(not.fully.paid~credit.policy + installment + purpose + revol.bal + log.annual.inc + fico + inq.last.6mths + 
             pub.rec,data=train_loans,family="binomial")
summary(Model3)
#AIC: 5514.3

#What is the accuracy of the logistic regression model? 
test_loans$pred_test=predict(Model1,newdata=test_loans,type="response")
View(test_loans)
table(test_loans$not.fully.paid,test_loans$pred_test > 0.5)

test_loans$pred_test_bi=sapply(test_loans$pred_test,function(x){ ifelse(x>0.5,1,0)})

accuracy=(2405+7)/nrow(test_loans)
accuracy

#What is the accuracy of the baseline model?
#baseline model of guessing every loan would be paid back in full 

table(test_loans$not.fully.paid)
2413/(2413+460)

#Use the ROCR package to compute the test set AUC.
library(ROCR)
pred = prediction(test_loans$pred_test,test_loans$not.fully.paid)
as.numeric(performance(pred, "auc")@y.values)

bivariate = glm(not.fully.paid~int.rate, data=train_loans, family="binomial")
summary(bivariate)
cor(train_loans$fico,train_loans$int.rate)
cor(train_loans$fico,train_loans$int.rate)
cor(loans$int.rate,loans$not.fully.paid)

#Make test set predictions for the bivariate model. 
#What is the highest predicted probability of a loan not being paid in full on the testing set?
pred.bivariate = predict(bivariate, newdata=test_loans, type="response")
max(pred.bivariate)

#What is the test set AUC of the bivariate model?
prediction.bivariate = prediction(pred.bivariate, test_loans$not.fully.paid)
as.numeric(performance(prediction.bivariate, "auc")@y.values)

#How much does a $10 investment with an annual interest rate of 6% pay back after 3 years, using continuous compounding of interest? 
#Hint: remember to convert the percentage to a proportion before doing the math. Enter the number of dollars, without the $ sign.
10 * exp(0.06*3)

#While the investment has value c * exp(rt) dollars after collecting interest, the investor had to pay $c for the investment. 
#What is the profit to the investor if the investment is paid back in full?
c * exp(rt) - c

#Now, consider the case where the investor made a $c investment, but it was not paid back in full. Assume, conservatively, that no money 
#was received from the borrower (often a lender will receive some but not all of the value of the loan, making this a pessimistic 
#assumption of how much is received). What is the profit to the investor in this scenario?
- c
#A person's profit is what they get minus what they paid for it. In this case, the investor gets no money but paid c dollars, yielding 
#a profit of -c dollars.

#In order to evaluate the quality of an investment strategy, we need to compute this profit for each loan in the test set. 
#For this variable, we will assume a $1 investment (aka c=1). To create the variable, we first assign to the profit for a fully paid loan,
#exp(rt)-1, to every observation, and we then replace this value with -1 in the cases where the loan was not paid in full. 
#All the loans in our dataset are 3-year loans, meaning t=3 in our calculations. Enter the following commands in your R console to create
#this new variable:
test_loans$profit = exp(test_loans$int.rate*3) - 1
test_loans$profit[test_loans$not.fully.paid == 1] = -1
#From summary(test$profit), we see the maximum profit for a $1 investment in any loan is $0.8895. 
#Therefore, the maximum profit of a $10 investment is 10 times as large, or $8.895.

#irst, use the subset() function to build a data frame called highInterest consisting of the test set loans with an interest rate of 
#at least 15%.
High_Interest_loan=subset(test_loans,int.rate>=0.15)
View(High_Interest_loan)

#What is the average profit of a $1 investment in one of these high-interest loans (do not include the $ sign in your answer)?
summary(High_Interest_loan$profit)

#What proportion of the high-interest loans were not paid back in full?
prop.table(table(High_Interest_loan$not.fully.paid))

#Use the subset() function to build a data frame called selectedLoans consisting of the high-interest loans with predicted risk not 
#exceeding the cutoff we just computed. Check to make sure you have selected 100 loans for investment.
#What is the profit of the investor, who invested $1 in each of these 100 loans (do not include the $ sign in your answer)?
cutoff = sort(High_Interest_loan$pred_test, decreasing=FALSE)[100]
selectedLoans = subset(High_Interest_loan, pred_test <= cutoff)
selectedLoans

#What is the profit of the investor, who invested $1 in each of these 100 loans 
#(do not include the $ sign in your answer)?
sum(selectedLoans$profit)

#How many of 100 selected loans were not paid back in full?
table(High_Interest_loan$not.fully.paid)
