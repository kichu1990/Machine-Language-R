library(tidyverse)
library(caret)
library(glmnet)
library(glmnet)

# Load the data and remove NAs
data("PimaIndiansDiabetes2", package = "mlbench")
PimaIndiansDiabetes2 = na.omit(PimaIndiansDiabetes2)
# Inspect the data
sample_n(PimaIndiansDiabetes2, 3)
# Split the data into training and test set
set.seed(123)
training.samples =PimaIndiansDiabetes2$diabetes %>% createDataPartition(p = 0.8, list = FALSE)
train.data=PimaIndiansDiabetes2[training.samples, ]
test.data=PimaIndiansDiabetes2[-training.samples, ]

#The R function model.matrix() helps to create the matrix of predictors and also automatically converts categorical 
#predictors to appropriate dummy variables, which is required for the glmnet() function.
# Dumy code categorical predictor variables
x =model.matrix(diabetes~., train.data)[,-1]
View(x)

# Convert the outcome (class) to a numerical variable
y = ifelse(train.data$diabetes == "pos", 1, 0)

glmnet(x, y, family = "binomial", alpha = 1, lambda = NULL)

#x: matrix of predictor variables
#y: the response or outcome variable, which is a binary variable.
#family: the response type. Use "binomial" for a binary outcome variable
#alpha: the elasticnet mixing parameter. Allowed values include:
#  "1": for lasso regression
#  "0": for ridge regression
#   a value between 0 and 1 (say 0.3) for elastic net regression.
#lamba: a numeric value defining the amount of shrinkage. Should be specify by analyst.
#In penalized regression, you need to specify a constant lambda to adjust the amount of the coefficient shrinkage. 
#The best lambda for your data, can be defined as the lambda that minimize the cross-validation prediction error rate. 
#This can be determined automatically using the function cv.glmnet().

library(glmnet)
# Find the best lambda using cross-validation
set.seed(123) 
cv.lasso=cv.glmnet(x, y, alpha = 1, family = "binomial",nlambda=100)
cv.lasso=cv.glmnet(x, y, alpha = 1, family = "binomial",lambda=10^seq(4,-1-1))
plot(cv.lasso)
coef(cv.lasso,cv.lasso$lambda.min)

# Fit the final model on the training data
model=glmnet(x, y, alpha = 1, family = "binomial",lambda = cv.lasso$lambda.min)

# Display regression coefficients
coef(model)

# Make predictions on the test data
x.test = model.matrix(diabetes ~., test.data)[,-1]
probabilities=model %>% predict(newx = x.test)
predicted.classes = ifelse(probabilities > 0.5, "pos", "neg")

# Model accuracy
observed.classes = test.data$diabetes
table(observed.classes,predicted.classes)
(49+11)/nrow(test.data)

mean(predicted.classes == observed.classes)

######################################################################################################################

set.seed(123) 
cv=cv.glmnet(x, y, alpha = 0)

# Display the best lambda value
cv$lambda.min

# Fit the final model on the training data
model=glmnet(x, y, alpha = 0, family = "binomial",lambda = cv$lambda.min)
coef(model)

# Make predictions on the test data
x.test = model.matrix(diabetes ~., test.data)[,-1]
probabilities=model %>% predict(newx = x.test)
predicted.classes = ifelse(probabilities > 0.5, "pos", "neg")

# Model accuracy
observed.classes = test.data$diabetes
table(observed.classes,predicted.classes)


