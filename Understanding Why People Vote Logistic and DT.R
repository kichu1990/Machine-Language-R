Vote=read.csv("C:\\Users\\ABHIC6\\Downloads\\gerber.csv")
View(Vote)

#What proportion of people in this dataset voted in this election?
prop.table(table(Vote$voting))

#Which of the four "treatment groups" had the largest percentage of people who actually 
#voted (voting = 1)?
tapply(Vote$voting,Vote$civicduty,mean)
tapply(Vote$voting,Vote$hawthorne,mean)
tapply(Vote$voting,Vote$self,mean)
tapply(Vote$voting,Vote$neighbors,mean)

#Build a logistic regression model for voting using the four treatment group variables as 
#the independent variables
Model_1=glm(voting~civicduty+hawthorne+self+neighbors,data=Vote,family = binomial)
summary(Model_1)

#Using a threshold of 0.3, what is the accuracy of the logistic regression model? 
predictLog=predict(Model_1,type="response")
table(Vote$voting, predictLog > 0.3)

#accuracy
(134513+51966)/(134513+51966+100875+56730)

#Using a threshold of 0.5, what is the accuracy of the logistic regression model? 
predictLog=predict(Model_1,type="response")
table(Vote$voting, predictLog > 0.5)

#accuracy
(235388+0)/(235388+108696)

library(ROCR)
library(rpart)
library(rpart.plot)
ROCRpred = prediction(predictLog, Vote$voting)
as.numeric(performance(ROCRpred, "auc")@y.values)

CARTmodel = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=Vote)
prp(CARTmodel)

CARTmodel2 = rpart(voting ~civicduty+hawthorne+self+neighbors,data=Vote,cp=0.0)
prp(CARTmodel2)

#Using only the CART tree plot, determine what fraction (a number between 0 and 1) of 
#"Civic Duty" people voted:0.31

#Make a new tree that includes the "sex" variable, again with cp = 0.0
CARTmodel3 = rpart(voting ~civicduty+hawthorne+self+neighbors+sex+control,data=Vote,cp=0.0)
prp(CARTmodel3)

#Create a regression tree using just the "control" variable, then create another tree
#with the "control" and "sex" variables, both with cp=0.0.
CARTcontrol = rpart(voting ~ control, data=Vote, cp=0.0)
CARTsex = rpart(voting ~ control + sex, data=Vote, cp=0.0)

prp(CARTcontrol,digits=6)
abs(0.296638-0.34)

prp(CARTsex,digits=6)

#Now, using the second tree (with control and sex), determine who is affected more by NOT 
#being in the control group
#They are affected about the same (change in probability within 0.001 of each other).

#Going back to logistic regression now, create a model using "sex" and "control". 
#Interpret the coefficient for "sex":

logModel=glm(voting~ control + sex,data=Vote,family = binomial)
summary(logModel)

#Coefficient is negative, reflecting that women are less likely to vote.
