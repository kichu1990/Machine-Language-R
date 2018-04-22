setwd("D:\\Study  Materials\\R Language\\DataSet")
Credit_Risk_Train_data=read.csv("Credit_Risk_Train_data.csv")
Credit_Risk_Test_data=read.csv("Credit_Risk_Test_data.csv")

#To use Rbind all the columnname in both the Dataset should be of same name
colnames(Credit_Risk_Test_data)[colnames(Credit_Risk_Test_data)=="outcome"]="Loan_Status"
df_all=rbind(Credit_Risk_Train_data,Credit_Risk_Test_data)
df_all$Loan_Status = sapply(df_all$Loan_Status,function(x){ ifelse(x=='Y',1,0)})
df_all$Loan_Status = as.character(df_all$Loan_Status)
df_all$Loan_Status = as.factor(df_all$Loan_Status)

#DataType Conversion
df_all$Gender=as.character(df_all$Gender)
df_all$Gender=as.factor(df_all$Gender)

df_all$Married=as.character(df_all$Married)
df_all$Married=as.factor(df_all$Married)

df_all$Self_Employed=as.character(df_all$Self_Employed)
df_all$Self_Employed=as.factor(df_all$Self_Employed)

df_all$Dependents=as.character(df_all$Dependents)
df_all$Dependents=as.factor(df_all$Dependents)

df_all$Credit_History=as.factor(df_all$Credit_History)
df_all$Loan_Status_Der=as.factor(df_all$Loan_Status_Der)

#Handling NA in the Data

df_all$Credit_History[is.na(df_all$Credit_History)]=1
df_all$LoanAmount[is.na(df_all$LoanAmount)]=median(df_all$LoanAmount,na.rm=T)
df_all$Loan_Amount_Term[is.na(df_all$Loan_Amount_Term)]=median(df_all$Loan_Amount_Term,na.rm=T)

df_all$Gender[df_all$Gender==""] = "NA"
df_all$Gender[is.na(df_all$Gender)]="Male"
df_all$Married[df_all$Married==""] = "NA"
df_all$Married[is.na(df_all$Married)]="Yes"
df_all$Dependents[df_all$Dependents==""] = "NA"
df_all$Dependents[is.na(df_all$Dependents)]=0
df_all$Self_Employed[df_all$Self_Employed==""] = "NA"
df_all$Self_Employed[is.na(df_all$Self_Employed)]="Yes"

summary(df_all)
str(df_all)

#Handling Outliers in Data
boxplot(Credit_Risk_Train_data_filled$LoanAmount)
Q1_LoanAmount=quantile(Credit_Risk_Train_data_filled$LoanAmount,0.25)
Q2_LoanAmount=quantile(Credit_Risk_Train_data_filled$LoanAmount,0.50)
Q3_LoanAmount=quantile(Credit_Risk_Train_data_filled$LoanAmount,0.75)
IQR_LoanAmount=Q3_LoanAmount-Q1_LoanAmount
Q3_LoanAmount+(1.5*IQR_LoanAmount)

Credit_Risk_Train_data_filled$LoanAmount=sapply(Credit_Risk_Train_data_filled$LoanAmount,
                                        function(x){ ifelse(x>261,261,x)})

boxplot(Credit_Risk_Train_data_filled$ApplicantIncome)
Q1_ApplicantIncome=quantile(Credit_Risk_Train_data_filled$ApplicantIncome,0.25)
Q2_ApplicantIncome=quantile(Credit_Risk_Train_data_filled$ApplicantIncome,0.50)
Q3_ApplicantIncome=quantile(Credit_Risk_Train_data_filled$ApplicantIncome,0.75)
IQR_ApplicantIncome=Q3_ApplicantIncome-Q1_ApplicantIncome
Q3_ApplicantIncome+(1.5*IQR_ApplicantIncome)

Credit_Risk_Train_data_filled$ApplicantIncome=sapply(Credit_Risk_Train_data_filled$ApplicantIncome,
                                                function(x){ ifelse(x>10172,10172,x)})

boxplot(Credit_Risk_Train_data_filled$CoapplicantIncome)
Q1_CoapplicantIncome=quantile(Credit_Risk_Train_data_filled$CoapplicantIncome,0.25)
Q2_CoapplicantIncome=quantile(Credit_Risk_Train_data_filled$CoapplicantIncome,0.50)
Q3_CoapplicantIncome=quantile(Credit_Risk_Train_data_filled$CoapplicantIncome,0.75)
IQR_CoapplicantIncome=Q3_CoapplicantIncome-Q1_CoapplicantIncome
Q3_CoapplicantIncome+(1.5*IQR_CoapplicantIncome)

Credit_Risk_Train_data_filled$CoapplicantIncome=sapply(Credit_Risk_Train_data_filled$CoapplicantIncome,
                                                     function(x){ ifelse(x>5744,5744,x)})

cor(Credit_Risk_Train_data_filled[,unlist(lapply(Credit_Risk_Train_data_filled, is.numeric))])

#split back in the same ratoi as train & test
Credit_Risk_Test_data_filled=tail(df_all,nrow(Credit_Risk_Test_data))
Credit_Risk_Train_data_filled=head(df_all,nrow(Credit_Risk_Train_data))

df = Credit_Risk_Train_data_filled[-1]
dft = Credit_Risk_Test_data_filled[-1]

# Check the relation between the categorical variables
table(Credit_Risk_Train_data_filled$Loan_Status,Credit_Risk_Train_data_filled$Gender)
chisq.test(table(Credit_Risk_Train_data_filled$Loan_Status,Credit_Risk_Train_data_filled$Gender))

table(Credit_Risk_Train_data_filled$Loan_Status,Credit_Risk_Train_data_filled$Married)
chisq.test(table(Credit_Risk_Train_data_filled$Loan_Status,Credit_Risk_Train_data_filled$Married))

table(Credit_Risk_Train_data_filled$Loan_Status,Credit_Risk_Train_data_filled$Dependents)
chisq.test(table(Credit_Risk_Train_data_filled$Loan_Status,Credit_Risk_Train_data_filled$Dependents))

table(Credit_Risk_Train_data_filled$Loan_Status,Credit_Risk_Train_data_filled$Education)
chisq.test(table(Credit_Risk_Train_data_filled$Loan_Status,Credit_Risk_Train_data_filled$Education))

table(Credit_Risk_Train_data_filled$Loan_Status,Credit_Risk_Train_data_filled$Self_Employed)
chisq.test(table(Credit_Risk_Train_data_filled$Loan_Status,Credit_Risk_Train_data_filled$Self_Employed))

table(Credit_Risk_Train_data_filled$Loan_Status,Credit_Risk_Train_data_filled$Credit_History)
chisq.test(table(Credit_Risk_Train_data_filled$Loan_Status,Credit_Risk_Train_data_filled$Credit_History))

table(Credit_Risk_Train_data_filled$Loan_Status,Credit_Risk_Train_data_filled$Property_Area)
chisq.test(table(Credit_Risk_Train_data_filled$Loan_Status,Credit_Risk_Train_data_filled$Property_Area))

table(Credit_Risk_Train_data_filled$Loan_Status,Credit_Risk_Train_data_filled$Dependents)
chisq.test(table(Credit_Risk_Train_data_filled$Loan_Status,Credit_Risk_Train_data_filled$Dependents))

#Build Logistic regression Model
Model_1=glm(Loan_Status_Der~ApplicantIncome+Credit_History+LoanAmount+CoapplicantIncome+
              Dependents+Property_Area+Loan_Amount_Term+Gender,data=df,family = binomial)
summary(Model_1)

#Build a Model with significant variables
Model_2=glm(Loan_Status_Der~Credit_History+Property_Area,data=df,family = binomial)
summary(Model_2)

#Check the accuracy on the Train Data
res =predict(Model_1, df, type = "response" )
table(res)
result = ifelse(res > 0.5, 1, 0)
table(result)
table(ActualValue = df$Loan_Status_Der, PredictedValue = res >0.5)
(84+415)/nrow(df)

#Check the accuracy on the Test Data
dft$pred_test=predict(Model_1,newdata=dft,type="response")
table(dft$Loan_Status_Der,dft$pred_test > 0.5)
#Accuaracy Of the Model
(58+289)/nrow(dft)

# Finding the threshold using ROC curve
library(ROCR) 
library(Metrics)
pr = prediction(dft$pred_test,dft$Loan_Status_Der)
perf = performance(pr,measure = "tpr",x.measure = "fpr") 
plot(perf,colorize=T)
auc.temp=performance(pr,"auc")
AUC=as.numeric(auc.temp@y.values)
AUC
resp = ifelse (resp_test > 0.2,1,0)
table(dft$Loan_Status_Der,resp)

library(caret)
confusionMatrix(table(df$Loan_Status,result))

# Stepwise Algorithm to identify Important variables using AIC 
step(Model_1)
Model_3=glm(Loan_Status_Der ~ CoapplicantIncome + Credit_History + Property_Area, data = df,family = binomial)
summary(Model_3)
#AIC: 582

# Validating the model on Training Data
resp_train = predict(Model_3,df, type = 'response')
result_train = ifelse(res > 0.5, 1, 0)
table(ActualValue = df$Loan_Status_Der, PredictedValue = resp_train >0.5)
(84+415)/nrow(df)

# Validating the model on Testing Data
resp_test = predict(Model_3,dft, type = 'response')
table(ActualValue = dft$Loan_Status_Der, PredictedValue = resp_test >0.5)
(58+289)/nrow(dft)

# Finding the threshold using ROC curve
library(ROCR) 
library(Metrics)
pr = prediction(resp_test,dft$Loan_Status_Der)
perf = performance(pr,measure = "tpr",x.measure = "fpr") 
plot(perf,colorize=T)
auc.temp=performance(pr,"auc")
AUC=as.numeric(auc.temp@y.values)
resp = ifelse (resp_test > 0.2,1,0)
table(dft$Loan_Status_Der,resp)

library(caret)
confusionMatrix(table(df$Loan_Status,result_train))
