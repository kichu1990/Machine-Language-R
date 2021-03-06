setwd("D:\\Study  Materials\\R Language\\DataSet")
climate_change=read.csv("climate_change.csv")

View(climate_change)

#Check if any NA Exists
sum(is.na(climate_change))

#Compute the correlations between all the variables in the dataset except Date & Month. 
cor(subset(climate_change, select = c(-Year,-Month)))

#Which of the following independent variables is N2O highly correlated with (absolute 
#correlation greater than 0.7)? 
#CO2,CH4,CFC.12,Temp

#Find out which variables are highly correlated with the Dependent Variable? 
#CO2,N2O,CFC.12,CH4

#What will be the prediction of a baseline Model? 
pred=mean(climate_change$Temp)

#SST/TSS
sum((climate_change$Temp-pred)^2)

#build a linear regression model to predict the dependent variable Temp, using CO2, N2O, 
#CFC.12, and Aerosols as independent variables
#colnames(climate_change)[colnames(climate_change)=="CFC.12"]="CFC_12"
#colnames(climate_change)[colnames(climate_change)=="CFC.11"]="CFC_11"

#plot Histogram for all Independent variable

hist(climate_change$CO2)
hist(climate_change$N2O)
hist(climate_change$Aerosols)
hist(climate_change$CFC.12)

boxplot(climate_change$CO2)
boxplot(climate_change$N2O)
boxplot(climate_change$Aerosols)
boxplot(climate_change$CFC.12)

Q1_Aerosol=quantile(climate_change$Aerosols,0.25)
Q2_Aerosol=quantile(climate_change$Aerosols,0.50)
Q3_Aerosol=quantile(climate_change$Aerosols,0.75)

IQR_Aerosol=Q3_Aerosol-Q1_Aerosol

Q1_CFC.12=quantile(climate_change$CFC.12,0.25)
Q2_CFC.12=quantile(climate_change$CFC.12,0.50)
Q3_CFC.12=quantile(climate_change$CFC.12,0.75)
IQR_CFC.12=Q3_CFC.12-Q1_CFC.12

Q1_CFC.12-(1.5*IQR_CFC.12)
Q3_Aerosol+(1.5*IQR_Aerosol)

colnames(climate_change)[colnames(climate_change)=="CFC.12"]="CFC_12"
colnames(climate_change)[colnames(climate_change)=="CFC.11"]="CFC_11"

aerosols = sapply(aerosols,function(x){x>0.0132}) = 0.0132

#Check the outlier percentage

length(which(climate_change$Aerosols > Q3_Aerosol+(1.5*IQR_Aerosol)
             | climate_change$Aerosols < Q1_Aerosol-(1.5*IQR_Aerosol)))/length(climate_change$Aerosols)

length(which(climate_change$CFC.12 > Q3_CFC.12+(1.5*IQR_CFC.12)
             | climate_change$CFC.12 < Q1_CFC.12-(1.5*IQR_CFC.12)))/length(climate_change$CFC.12)

climate_linreg=lm(Temp~CO2+N2O+Aerosols+CFC.12,data=climate_change)
summary(climate_linreg)

#Which variables are significant in the model? 
CO2,Aerosols,CFC.12,N2O

#Handling Outliers
climate_change$Aerosols=sapply(climate_change$Aerosols,function(x){ ifelse(x>0.0273,0.0273,x)})
climate_change$CFC.12=sapply(climate_change$CFC.12,function(x){ ifelse(x<370.2405,370.2405,x)})

#Split the Data to Test and Train
climate_change$rowid=runif(308)
set.seed(400)
climate_change_train=subset(climate_change,rowid<=0.7)
climate_change_test=subset(climate_change,rowid>0.7)

#build a linear regression model to predict the dependent variable 
climate_linreg=lm(Temp~CO2+N2O+Aerosols+CFC.12,data=climate_change_train)
summary(climate_linreg)

#Find the correlation between the actual & predicted values of Temp. Square this number and you should get the Multiple R2.
(cor(climate_linreg$fitted.values,climate_change_train$Temp)^2)

prediction=predict(climate_linreg,newdata=climate_change_test)

#Find the RMSE, SSE and MAPE for the above Model. 
Error=climate_change_test$Temp-prediction
SSE=sum((prediction-climate_change_test$Temp)^2)
MAPE= sum(abs(Error)/climate_change_test$Temp)/nrow(climate_change_test)
RMSE=sqrt(mean((prediction-climate_change_test$Temp)^2))
  
#Remove the insignificant variable and re run the model. What effect did it have on R2 and Adjusted R2? 
climate_linreg_New=lm(Temp~CO2+Aerosols+CFC.12,data=climate_change_train)
summary(climate_linreg_New)

prediction_New=predict(climate_linreg_New,newdata=climate_change_test)

#What effect did it have on R2 and Adjusted R2?
#Adjusted R2 increased and R2 decreased
#Observe the change closely. Does this validate the definition of R2 & Adj R2?

#Find the RMSE, SSE and MAPE for the above Model. 
Error_New=climate_change_test$Temp-prediction_New
SSE=sum((prediction_New-climate_change_test$Temp)^2)
MAPE= sum(abs(Error_New)/climate_change_test$Temp)/nrow(climate_change_test)
RMSE=sqrt(mean((prediction_New-climate_change_test$Temp)^2))
