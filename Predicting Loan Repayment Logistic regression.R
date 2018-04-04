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

setwd("D:\\Study  Materials\\R Language\\DataSet")
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
loans$delinq.2yrs =as.factor(loans$delinq.2yrs )
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

Model1$xlevels[["delinq.2yrs"]] <- union(Model1$xlevels[["delinq.2yrs"]], levels(test_loans$delinq.2yrs))


pisaTrain_filled$raceeth = relevel(pisaTrain_filled$inq.last.6mths, "White")
pisaTest_filled$raceeth = relevel(pisaTest_filled$raceeth, "White")


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
