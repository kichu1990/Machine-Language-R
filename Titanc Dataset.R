setwd("D:\\Study  Materials\\R Language\\DataSet")
Titanic=read.csv("train.csv")
View(Titanic)
#How many Rows and Columns do you have?
dim(Titanic)

#Check the datatype of each column. 
Titanic$Survived=as.factor(Titanic$Survived)
Titanic$Pclass=as.factor(Titanic$Pclass)
Titanic$Parch=as.factor(Titanic$Parch)
Titanic$SibSp=as.factor(Titanic$SibSp)
str(Titanic)
summary(Titanic)

#How many Male and Female are present?
summary(Titanic)

#Of the total Passengers, How many survived and how many did not.
table(Titanic$Survived)

#What proportion Survived?
prop.table(table(Titanic$Survived))

#How many Male Survived. (hint Table function can take >1 inputs)
table(Titanic$Survived,Titanic$Sex)

#How many Passengers do you have in each Pclass.
table(Titanic$Pclass)

#Draw histogram for the Fare paid. 
hist(Titanic$Fare)

#Find the mean Fare paid by Passengers in each Pclass. 
tapply(Titanic$Fare,Titanic$Pclass,mean)

#How many Passengers embarked the Ship and at which Station?
table(Titanic$Embarked)

#Count the number of missing values in each column? Does embarked column has any missing 
#value?? 
apply(Titanic,2,function(x){sum(is.na(x))}) 

#13)	Create a smaller dataframe with (all Male and travelling first class) and another df 
#with all female OR anybody below the Age of 20. How many rows are in each df.

df_FC_Male=subset(Titanic,Titanic$Pclass=="1"&Titanic$Sex=="male")
nrow(df_FC_Male)

df_Female=subset(Titanic,Titanic$Age<20|Titanic$Sex=="female")
nrow(df_Female)

#What is the # of Male and Female Survived with respect to each Pclass.
table(Titanic$Survived,Titanic$Pclass)

#Use grep to create a subset of all Passenger who survived. Call it df_survived. 
#Whats the number of rows?
df_Survived=grep("1",Titanic$Survived)

#Find all the columns which has the letter 'e ' in the column name. 
#How many columns do you get?
df_col_name_with_e=Titanic[, grep("e" ,colnames(Titanic))]
df_col_name_with_e

mean(Titanic$Age,Titanic$Sex=="male",na.rm=T)

#Fill the Missing Values of Age separately for the Male and Female by their Averages
sum(is.na(Titanic$Age))
tapply(Titanic$Age,Titanic$Sex,mean,na.rm=T)
Titanic$Age[is.na(Titanic$Age) & Titanic$Sex=="male"]=30.72
Titanic$Age[is.na(Titanic$Age) & Titanic$Sex=="female"]=27.91
sum(is.na(Titanic$Age))

#Create a new column with the first name of each passenger. Call this column "First name"

#to split the Name we will have to convert the nmaes to characters first. Y?
#because DataType Factors cannot be changed... All characters are by default read as
#factors when you use the read.csv function.
Titanic$Name=as.character(Titanic$Name)


strsplit(Titanic$Name[1],"[.,]")

Titanic$FirstName=sapply(Titanic$Name,function(x){strsplit(x,"[,.]")[[1]][3]})
Titanic$LastName=sapply(Titanic$Name,function(x){strsplit(x,"[,.]")[[1]][1]})
Titanic$Title=sapply(Titanic$Name,function(x){strsplit(x,"[,.]")[[1]][2]})

Titanic$FirstName=sub(" ","",Titanic$FirstName)
Titanic$LastName=sub(" ","",Titanic$FirstName)
Titanic$Title=sub(" ","",Titanic$FirstName)
