census$over50k=ifelse(census$over50k=='>50K',1,0)
census$over50k=as.factor(census$over50k)
census$workclass=as.factor(census$workclass)
census$education=as.factor(census$education)
census$maritalstatus=as.factor(census$maritalstatus)
census$occupation=as.factor(census$occupation)
census$relationship=as.factor(census$relationship)
census$sex =as.factor(census$sex )
census$race =as.factor(census$race )
census$nativecountry =as.factor(census$nativecountry )
unique(census$over50k)

sapply(census, function(x) sum(is.na(x)))

library(glmnet)
library(caTools)
set.seed(2000)
spl= sample.split(census$over50k, SplitRatio=0.60)
train_census = subset(census, spl==T)
test_census  = subset(census, spl==F)


#Next, build a logistic regression model to predict the dependent variable "over50k", 
#using all of the other variables in the dataset as independent variables. 
#Use the training set to build the model.

logistic_census=glm(over50k~.,data=train_census,family="binomial")
summary(logistic_census)
pred=predict(logistic_census,newdata=test_census,type="response")
pred_class=ifelse(pred>=0.5,1,0)
table(test_census$over50k,pred_class)

#What is the accuracy of the model on the testing set? Use a threshold of 0.5. 
#(You might see a warning message when you make predictions on the test set - you can 
#safely ignore it.)

accuracy=(9051+1888)/nrow(test_census)
accuracy

#What is the area-under-the-curve (AUC) for this model on the test set?

library("ROCR")
ROCR_cart=prediction(pred,test_census$over50k)
ROCCurve_cart=performance(ROCR_cart,"tpr","fpr")
plot(ROCCurve_cart,colorize=T)

#What is the AUC of the CART model on the test set?

auc.temp=performance(ROCR_cart,"auc")
AUC=as.numeric(auc.temp@y.values)
AUC #90.61


#Let us now build a classification tree to predict "over50k". Use the training set to 
#build the model, and all of the other variables as independent variables. Use the default 
#parameters, so don't set a value for minbucket or cp. Remember to specify method="class" 
#as an argument to rpart, since this is a classification problem. After you are done 
#building the model, plot the resulting tree.

library(rpart)
library(rpart.plot)
tree_census=rpart(over50k~.,data=train_census,method="class")
prp(tree_census)

#How many splits does the tree have in total?
#4

#Which variable does the tree split on at the first level (the very first split of the tree)?
#relationship

#Which variables does the tree split on at the second level (immediately after the first 
#split of the tree)? Select all that apply.

#capital gain, education

#What is the accuracy of the model on the testing set? Use a threshold of 0.5. 
#(You can either add the argument type="class", or generate probabilities and use a 
#threshold of 0.5 like in logistic regression.)


nrow(test_census)
pred_cart=predict(tree_census,newdata=test_census,type="class")
table(test_census$over50k,pred_cart)

accuracy=(9243+1596)/nrow(test_census)
accuracy    #84.4%


#Plot the ROC curve for the CART model you have estimated. 
library("ROCR")
pred_cart=predict(tree_census,newdata=test_census)
ROCR_cart=prediction(pred_cart[,2],test_census$over50k) 
ROCCurve_cart=performance(ROCR_cart,"tpr","fpr")
plot(ROCCurve_cart,colorize=T)

#What is the AUC of the CART model on the test set?

auc.temp=performance(ROCR_cart,"auc")
AUC=as.numeric(auc.temp@y.values)
AUC  #84.70

#Problem 3.1 - A Random Forest Model
#Before building a random forest model, we'll down-sample our training set

library("randomForest")
set.seed(1)

trainSmall_census = train_census[sample(nrow(train_census), 2000), ]

#Make sure NA not present in the Dataframe

library(dplyr)
set.seed(1)
rf_census=randomForest(over50k~.-nativecountry ,data=trainSmall_census)
rf_census
library(caret)
varImpPlot(rf_census)

accuracy=(1383+313)/nrow(trainSmall_census)
accuracy

#make predictions using this model on the entire test set. 
#What is the accuracy of the model on the test set, using a threshold of 0.5? 


#test_census$workclass =factor(test_census$workclass,levels=levels(trainSmall_census$workclass))
#test_census$education =factor(test_census$education,levels=levels(trainSmall_census$education))
#test_census$maritalstatus =factor(test_census$maritalstatus,levels=levels(trainSmall_census$maritalstatus))
#test_census$occupation =factor(test_census$occupation,levels=levels(trainSmall_census$occupation))
#test_census$relationship =factor(test_census$relationship,levels=levels(trainSmall_census$relationship))
#test_census$sex =factor(test_census$sex,levels=levels(trainSmall_census$sex))
#test_census$race =factor(test_census$race,levels=levels(trainSmall_census$race))


pred=predict(rf_census,newdata=test_census)
table(pred,test_census$over50k)
accuracy=(8871+2038)/nrow(test_census)
accuracy


#One metric that we can look at is the number of times, aggregated over all of the trees 
#in the random forest model, that a certain variable is selected for a split. To view this
#metric, run the following lines of R code (replace "MODEL" with the name of your random 
#forest model):

vu = varUsed(rf_census, count=TRUE)
vusorted = sort(vu, decreasing = FALSE, index.return = TRUE)
dotchart(vusorted$x, names(rf_census$forest$xlevels[vusorted$ix]))

#This code produces a chart that for each variable measures the number of times that 
#variable was selected for splitting (the value on the x-axis). Which of the following 
#variables is the most important in terms of the number of splits?\
#age


#Let us select the cp parameter for our CART model using k-fold cross validation, 
#with k = 10 folds. Do this by using the train function. Set the seed beforehand to 2. 
#Test cp values from 0.002 to 0.1 in 0.002 increments, by using the following command:

library(caret)
library(e1071)
cpgrid = expand.grid( .cp = seq(0.002,0.1,0.002))
numfolds=trainControl(method="cv",number=10) # cv= cross validation, 10 folds
train(over50k~.,data=train_census,method="rpart",trControl=numfolds,tuneGrid=cpgrid)

#Fit a CART model to the training data using this value of cp. 
#What is the prediction accuracy on the test set?

pred_CART=predict(tree_census,newdata=test_census,cp=0.002,type="class")
table(test_census$over50k,pred_cart_CART)
accuracy=(9243+1596)/nrow(test_census)
accuracy
       
tree_final=rpart(over50k~.,cp=0.002,data=train)
prp(tree_final)

pred_test=predict(tree_final,newdata=test,type="class")
table(test$over50k,pred_test)
accuracy=(22966+4552)/nrow(test)
#86.05%   
