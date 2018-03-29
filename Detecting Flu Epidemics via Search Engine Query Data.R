setwd("C:\\Users\\abhic6\\Downloads")
FluTrain=read.csv("FluTrain.csv")
View(FluTrain)

#Looking at the time period 2004-2011, which week corresponds to the highest percentage of ILI-related 
#physician visits? Select the day of the month corresponding to the start of this week.
subset(FluTrain,ILI == max(ILI))

#Which week corresponds to the highest percentage of ILI-related query fraction?
subset(FluTrain,Queries == max(Queries))

#Let us now understand the data at an aggregate level. Plot the histogram of the dependent variable, ILI. 
#What best describes the distribution of values of ILI?

hist(FluTrain$ILI)
#Most of the ILI values are small, with a relatively small number of much larger values (in statistics, 
#this sort of data is called "skew right"). 

#When handling a skewed dependent variable, it is often useful to predict the logarithm of the 
#dependent variable instead of the dependent variable itself -- this prevents the small number of unusually 
#large or small observations from having an undue influence on the sum of squared errors of predictive 
#models. In this problem, we will predict the natural log of the ILI variable, which can be computed in 
#R using the log() function.

plot(log(FluTrain$ILI),FluTrain$Queries)
#There is a positive, linear relationship between log(ILI) and Queries.

#Based on the plot we just made, it seems that a linear regression model could be a good modeling choice. 
#Based on our understanding of the data from the previous subproblem, which model best describes our 
#estimation problem?

log(ILI) = intercept + coefficient x Queries, where the coefficient is positive

#From the previous subproblem, we are predicting log(ILI) using the Queries variable. 
#From the plot in the previous subproblem, we expect the coefficient on Queries to be positive.

#What is the training set R-squared value for FluTrend1 model (the "Multiple R-squared")?

lm_ILI=lm(log(ILI)~Queries,data=FluTrain)
summary(lm_ILI)

#For a single variable linear regression model, there is a direct relationship between the R-squared and 
#the correlation between the independent and the dependent variables. What is the relationship we infer
#from our problem?

Correlation = cor(FluTrain$Queries, log(FluTrain$ILI))
Correlation^2 

setwd("C:\\Users\\abhic6\\Downloads")
FluTest=read.csv("FluTest.csv")

PredTest1=predict(lm_ILI,newdata=FluTest)
PredTest1 = exp(predict(lm_ILI, newdata=FluTest))

#What is our estimate for the percentage of ILI-related physician visits for the week of March 11, 2012?
PredTest1[which(FluTest$Week == "2012-03-11 - 2012-03-17")]  #2.187378 
FluTest$ILI[11]

#What is the relative error betweeen the estimate (our prediction) and the observed value for the week of 
#March 11, 2012? Note that the relative error is calculated as
(FluTest$ILI-PredTest1)/FluTest$ILI

SSE=sum((FluTest$ILI-PredTest1)^2)
SSE

#What is the Root Mean Square Error (RMSE) between our estimates and the actual observations for the 
#percentage of ILI-related physician visits, on the test set?
#dividing SSE by the number of observations and taking the square root:
  
RMSE=sqrt(SSE/nrow(FluTest))
RMSE