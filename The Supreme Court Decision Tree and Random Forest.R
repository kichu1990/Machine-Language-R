# Read in the data
stevens=read.csv("D:\\Study  Materials\\R Language\\DataSet\\stevens.csv")
str(stevens)

# Split the data
library(caTools)
set.seed(3000)
spl   = sample.split(stevens$Reverse, SplitRatio = 0.7)
Train = subset(stevens, spl==TRUE)
Test  = subset(stevens, spl==FALSE)

# Install rpart library
install.packages("rpart")
library(rpart)
install.packages("rpart.plot")
library(rpart.plot)

# CART model
StevensTree = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, 
                    data = Train, method="class", minbucket=25)
#method=class tells rpart to Build a Classification Tress instead of Regression
prp(StevensTree)

# Make predictions
PredictCART = predict(StevensTree, newdata = Test, type = "class")
table(Test$Reverse, PredictCART)
(41+71)/(41+36+22+71)

#Baseline Model Accuracy
(22+71)/(41+36+22+71)

# ROC curve
library(ROCR)
PredictROC = predict(StevensTree, newdata = Test)
PredictROC
#For each observation in the test set,it gives two numbers which can be thought of as the probability 
#of outcome 0 and the probability of outcome 1.

pred = prediction(PredictROC[,2], Test$Reverse)
perf = performance(pred, "tpr", "fpr")
plot(perf)

as.numeric(performance(pred, "auc")@y.values)

#change the minbucket parameter to 5. Plot the tree.
StevensTree_2 = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, 
                      data = Train, method="class", minbucket=5)
prp(StevensTree_2)

#How many splits does the tree have?
#16

#change the minbucket parameter to 100. Plot the tree.
StevensTree_3 = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, 
                      data = Train, method="class", minbucket=100)
prp(StevensTree_3)

#How many splits does the tree have?
#1

# Install randomForest package
install.packages("randomForest")
library(randomForest)

# Build random forest model
StevensForest = randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, 
                             data = Train, ntree=200, nodesize=25 )

#In CART, we added the argument method="class",so that it was clear that we're doing a classification problem.
# trees can alsobe used for regression problems, which The randomForest function does not have a method argument.
#So when we want to do a classification problem,we need to make sure outcome is a factor.

# Convert outcome to factor
Train$Reverse = as.factor(Train$Reverse)
Test$Reverse = as.factor(Test$Reverse)

StevensForest = randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, 
                             data = Train, ntree=200, nodesize=25 )

# Make predictions
PredictForest = predict(StevensForest, newdata = Test)
table(Test$Reverse, PredictForest)
(40+74)/(40+37+19+74)

# set the seed to 100, and the re-build the random forest model

set.seed(100)
spl   = sample.split(stevens$Reverse, SplitRatio = 0.7)
Train = subset(stevens, spl==TRUE)
Test  = subset(stevens, spl==FALSE)

Train$Reverse = as.factor(Train$Reverse)
Test$Reverse = as.factor(Test$Reverse)

StevensForest = randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, 
                             data = Train, ntree=200, nodesize=25 )
PredictForest = predict(StevensForest, newdata = Test)
table(Test$Reverse, PredictForest)
(45+74)/(45+32+19+74)

# set the seed to 200, and the re-build the random forest model

set.seed(200)
spl   = sample.split(stevens$Reverse, SplitRatio = 0.7)
Train = subset(stevens, spl==TRUE)
Test  = subset(stevens, spl==FALSE)

Train$Reverse = as.factor(Train$Reverse)
Test$Reverse = as.factor(Test$Reverse)

StevensForest = randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, 
                             data = Train, ntree=200, nodesize=25 )
PredictForest = predict(StevensForest, newdata = Test)
table(Test$Reverse, PredictForest)2 
(45+74)/(45+32+19+74)

# Install cross-validation packages
install.packages("caret")
library(caret)
install.packages("e1071")
library(e1071)

# Define cross-validation experiment
numFolds = trainControl( method = "cv", number = 10 )
cpGrid = expand.grid( .cp = seq(0.01,0.5,0.01)) 

# Perform the cross validation
train(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, 
      data = Train, method = "rpart", trControl = numFolds, tuneGrid = cpGrid )

# Create a new CART model
StevensTreeCV = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, 
                      data = Train, method="class", cp = 0.20)

#Plot the tree that we created using cross-validation. How many splits does it have?
prp(StevensTreeCV)


# Make predictions
PredictCV = predict(StevensTreeCV, newdata = Test, type = "class")
table(Test$Reverse, PredictCV)
(59+64)/(59+18+29+64)
