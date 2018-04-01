setwd("D:\\Study  Materials\\R Language\\DataSet")
statedata=read.csv("statedata.csv")

str(statedata)

plot(statedata$x,statedata$y)

#Using the tapply command, determine which region of the US (West, North Central, South, or 
#Northeast) has the highest average high school graduation rate of all the states in the region:
tapply(statedata$HS.Grad,statedata$state.region,mean,na.rm=T)

#make a boxplot of the murder rate by region 
boxplot(statedata$Murder ~ statedata$state.region) 
tapply(statedata$Murder,statedata$state.region,median,na.rm=T)

#You should see that there is an outlier in the Northeast region of the boxplot you just 
#generated. Which state does this correspond to?
subset(statedata,statedata$state.region=="Northeast"&statedata$Murder>10)

#Check if any NA are present
summary(statedata)
sum(is.na(statedata))

cor(subset(statedata,select = c(-state.abb,-state.name,-state.division,-state.region)))

#Build the model with all potential variables included (Population, Income, Illiteracy, 
#Murder, HS.Grad, Frost, and Area). 
Model_1=lm(Life.Exp~Population+Income+Illiteracy+Murder+HS.Grad+Frost+Area,data=statedata)
summary(Model_1)

#What is the coefficient for "Income" in your linear regression model?
#-2.180e-05

#Now plot a graph of life expectancy vs. income using the command:
plot(statedata$Income,statedata$Life.Exp)

#The model we built does not display the relationship we saw from the plot of life expectancy
#vs. income. Which of the following explanations seems the most reasonable?
#Multicollinearity

#Although income is an insignificant variable in the model, this does not mean that there is 
#no association between income and life expectancy. However, in the presence of all of the 
#other variables, income does not add statistically significant explanatory power to the model.
#This means that multicollinearity is probably the issue.

library(car)
vif(Model_2)

Model_2=lm(Life.Exp~Population+Income+Murder+HS.Grad+Frost+Area,data=statedata)
summary(Model_2)

#Removing insignificant variables changes the Multiple R-squared value of the model.
Model_3=lm(Life.Exp~Population+Murder+HS.Grad+Frost,data=statedata)
summary(Model_3)

#On the contrary, when we remove insignificant variables, the "Adjusted R-squred" will 
#frequently be better. This value accounts for the complexity of the model, and thus tends 
#to increase as insignificant variables are removed, and decrease as insignificant variables 
#are added.

#check any Outliers Exist
boxplot(statedata$Population)
boxplot(statedata$Murder)
boxplot(statedata$Frost)
boxplot(statedata$HS.Grad)

Q1_Population=quantile(statedata$Population,0.25)
Q2_Population=quantile(statedata$Population,0.50)
Q3_Population=quantile(statedata$Population,0.75)
IQR_Population=Q3_Population-Q1_Population

Toppercent=Q3_Population+(1.5*IQR_Population)
Toppercent
statedata$New_pop=sapply(statedata$Population,function(x){ ifelse(x>10802,10802,x)})

Model_4=lm(Life.Exp~Population+Murder+HS.Grad+Frost,data=statedata)
summary(Model_4)

#Which state do we predict to have the lowest life expectancy?
statedata$state.name[which.min(sort(Model_4$fitted.values))]

#Which state actually has the lowest life expectancy? 
statedata$state.name[which.min(sort(statedata$Life.Exp))]

#Which state do we predict to have the highest life expectancy?
statedata$state.name[which.max(sort(Model_4$fitted.values))]

#Which state actually has the high life expectancy? 
statedata$state.name[which.max(sort(statedata$Life.Exp))]

#For which state do we make the smallest absolute error?
statedata$state.name[which.min(sort(abs(Model_4$residuals)))]

#For which state do we make the largest absolute error?
statedata$state.name[which.max(sort(abs(Model_4$residuals)))]