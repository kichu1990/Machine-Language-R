setwd("D:\\Study  Materials\\R Language\\DataSet")
test=read.csv("Test_BBA.csv")
train=read.csv("Train_BBA.csv")

View(test)

#add a column to test
test$Item_Outlet_Sales = 1

#combine the data set
combi = rbind(train,test)

summary(combi)

#impute missing values with median
hist(combi$Item_Visibility)
combi$Item_Weight[is.na(combi$Item_Weight)] <- median(combi$Item_Weight, na.rm = TRUE)

#impute 0 with median
combi$Item_Visibility = ifelse(combi$Item_Visibility == 0, median(combi$Item_Visibility), combi$Item_Visibility)

table(combi$Outlet_Size, combi$Outlet_Type)
levels(combi$Outlet_Size)[1]="Other"

my_data=subset(combi,select = -c(Item_Outlet_Sales, Item_Identifier,Outlet_Identifier))

#check available variables
colnames(my_data)

#PCA works on numeric variables
#check variable class
str(my_data)

#6 out of 9 variables are categorical in nature. We have some additional work to do now. 
#We'll convert these categorical variables into numeric using one hot encoding.
#load library
library(dummies)

#create a dummy data frame
new_my_data=dummy.data.frame(my_data, names = c("Item_Fat_Content","Item_Type",
                                                "Outlet_Establishment_Year","Outlet_Size",
                                                "Outlet_Location_Type","Outlet_Type"))
#check the data set
str(new_my_data)

#divide the new data
pca.train = new_my_data[1:nrow(train),]
pca.test = new_my_data[-(1:nrow(train)),]

#The base R function prcomp() is used to perform PCA. By default, it centers the variable to have mean equals to zero. 
#With parameter scale. = T, we normalize the variables to have standard deviation equals to 1.

#principal component analysis
prin_comp = prcomp(pca.train, scale. = T)
names(prin_comp)

#outputs the mean of variables
prin_comp$center

#outputs the standard deviation of variables
prin_comp$scale

prin_comp$rotation

#plot the resultant principal components.
biplot(prin_comp, scale = 0)

#compute standard deviation of each principal component
std_dev=prin_comp$sdev

#compute variance
pr_var=std_dev^2

#check variance of first 10 components
pr_var[1:10]

#proportion of variance explained
prop_varex=pr_var/sum(pr_var)
prop_varex[1:20]

#plot(prop_varex, xlab = "Principal Component",
plot(prop_varex, xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     type = "b")

#cumulative scree plot
plot(cumsum(prop_varex), xlab = "Principal Component",
       ylab = "Cumulative Proportion of Variance Explained",
       type = "b")

#add a training set with principal components
train.data=data.frame(Item_Outlet_Sales = train$Item_Outlet_Sales, prin_comp$x)

#we are interested in first 30 PCAs
train.data=train.data[,1:31]


#run a decision tree
install.packages("rpart")
library(rpart)
rpart.model= rpart(Item_Outlet_Sales ~ .,data = train.data, method = "anova")
rpart.model

#transform test into PCA
test.data=predict(prin_comp, newdata = pca.test)
test.data=as.data.frame(test.data)

#select the first 30 components
test.data=test.data[,1:30]

#make prediction on test data
rpart.prediction=predict(rpart.model, test.data)
