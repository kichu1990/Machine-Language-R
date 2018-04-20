State=read.csv("C:\\Users\\ABHIC6\\Downloads\\statedataSimple.csv")
View(State)
str(State)

#Create linear regression models we made in the previous homework question. First, predict Life.Exp using all of the other variables as the
#independent variables (Population, Income, Illiteracy, Murder, HS.Grad, Frost, Area ). Use the entire dataset to build the model.
Model_lm1=lm(Life.Exp~Population+Income+Illiteracy+Murder+HS.Grad+Frost+Area,data=State)
summary(Model_lm1)

#What is the adjusted R-squared of the model?
#0.6922 

#Calculate the sum of squared errors (SSE) between the predicted life expectancies using this model and the actual life expectancies:
sum(Model_lm1$residuals^2)

#Build a second linear regression model using just Population, Murder, Frost, and HS.Grad as independent variables
Model_lm2=lm(Life.Exp~Population+Illiteracy+Murder+HS.Grad+Frost,data=State)
summary(Model_lm2)

# What is the adjusted R-squared for this model?
#0.7061 

#Calculate the sum of squared errors again, using this reduced model:
sum(Model_lm2$residuals^2)

#Trying different combinations of variables in linear regression controls the complexity of the model. This is similar to trying different 
#numbers of splits in a tree, which is also controlling the complexity of the model.The second answer is incorrect because as we see here, 
#a model with fewer variables actually has a higher adjusted R-squared. If your accuracy is just as good, a model with fewer variables is 
#almost always better.The third answer is incorrect because the variables we removed have non-zero correlations with the dependent variable
#Life.Exp.
#cor(statedata$Life.Exp, statedata$Income)
#cor(statedata$Life.Exp, statedata$Illiteracy)
#cor(statedata$Life.Exp, statedata$Area)

#build a CART model 
library(rpart)
library(rpart.plot)
StateTree=rpart(Life.Exp~.,data=State)
prp(StateTree)

#Use the regression tree you just built to predict life expectancies (using the predict function)
PredictStateTree = predict(StateTree)

#calculate the sum-of-squared-errors (SSE) like you did for linear regression
SSE=sum((State$Life.Exp-PredictStateTree)^2)

#The error is higher than for the linear regression models. One reason might be that we haven't made the tree big enough. 
#Set the minbucket parameter to 5, and recreate the tree.
StateTree_New=rpart(Life.Exp~.,data=State,minbucket=5)
prp(StateTree_New)
#Area and HS.Grad Appear New in Tree

#Do you think the default minbucket parameter is smaller or larger than 5 based on the tree that was built?
#Since the tree now has more splits, it must be true that the default minbucket parameter was limiting the tree from splitting 
#more before. So the default minbucket parameter must be larger than 5

#What is the SSE of this tree?
PredictStateTree_New = predict(StateTree_New)
SSE=sum((State$Life.Exp-PredictStateTree_New)^2)

#Can we do even better? Create a tree that predicts Life.Exp using only Area, with the minbucket parameter to 1. 
#What is the SSE of this newest tree?
StateTree_New2=rpart(Life.Exp~Area,data=State,minbucket=1)
prp(StateTree_New2)
PredictStateTree = predict(StateTree_New2)
SSE=sum((State$Life.Exp-PredictStateTree)^2)
