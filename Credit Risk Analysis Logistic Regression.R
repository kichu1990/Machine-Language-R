setwd("D:\\Study  Materials\\R Language\\DataSet")
Credit_Risk_Train_data=read.csv("Credit_Risk_Train_data.csv")
Credit_Risk_Test_data=read.csv("Credit_Risk_Test_data.csv")

#To use Rbind all the columnname in both the Dataset should be of same name
colnames(Credit_Risk_Test_data)[colnames(Credit_Risk_Test_data)=="outcome"]="Loan_Status"
df_all=rbind(Credit_Risk_Train_data,Credit_Risk_Test_data)
df_all$Loan_Status_Der=sapply(df_all$Loan_Status,function(x){ ifelse(x=='Y',1,0)})

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

#split back in the same ratoi as train & test
Credit_Risk_Test_data_filled=head(df_all,nrow(Credit_Risk_Test_data))
Credit_Risk_Train_data_filled=head(df_all,nrow(Credit_Risk_Train_data))

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

Model_1=glm(Loan_Status_Der~.-Loan_Status-Loan_ID,data=Credit_Risk_Train_data_filled,family = binomial)
summary(Model_1)


Model_2=glm(Loan_Status_Der~.-Loan_Status-ApplicantIncome-Loan_Amount_Term-Loan_ID-LoanAmount-Self_Employed-Education-Dependents-Gender-CoapplicantIncome,data=Credit_Risk_Train_data_filled,family = binomial)
summary(Model_2)

library("MASS")

Credit_Risk_Test_data_filled$pred_test=predict(Model_2,newdata=Credit_Risk_Test_data_filled,type="response")
table(Credit_Risk_Test_data_filled$Loan_Status_Der,Credit_Risk_Test_data_filled$pred_test > 0.5)
Credit_Risk_Test_data_filled$pred_test_bi=sapply(Credit_Risk_Test_data_filled$pred_test,function(x){ ifelse(x>0.5,1,0)})

table(Credit_Risk_Test_data_filled$Loan_Status_Der,Credit_Risk_Test_data_filled$pred_test_bi)

print(chisq.test(Credit_Risk_Train_data_filled$Loan_Status_Der,Credit_Risk_Train_data_filled$Credit_History))
