setwd("C:\\Users\\ABHIC6\\Downloads")

str(songs)
songs$Top10=as.factor(songs$Top10)

#How many observations (songs) are from the year 2010?
nrow(subset(songs,songs$year=="2010"))
table(songs$year)

#How many songs does the dataset include for which the artist name is "Michael Jackson"?
MJ=subset(songs,songs$artistname=="Michael Jackson")

#Which of these songs by Michael Jackson made it to the Top 10?
table(MJ$Top10)
subset(MJ,MJ$Top10)

#What are the values of timesignature variable that occur in our dataset?
unique(songs$timesignature)

#Which timesignature value is the most frequent among songs in our dataset?
table(songs$timesignature)

#Out of all of the songs in our dataset, the song with the highest tempo?
which.max(songs$tempo)
songs$songtitle[which.max(songs$tempo)]

#split "SongsTrain" consisting of all the observations up to and including 2009 song 
#releases, and a testing set "SongsTest", consisting of the 2010 song releases.
SongsTrain=subset(songs,songs$year=="2009")
SongsTest=subset(songs,songs$year=="2010")

#How many observations (songs) are in the training set?
nrow(SongsTest)

nonvars=c("year","songtitle","artistname","songID","artistID")
SongsTrain = SongsTrain[ , !(names(SongsTrain) %in% nonvars) ]
SongsTest = SongsTest[ , !(names(SongsTest) %in% nonvars) ]
str(SongsTrain)

#Build Logistic regression Model
Model_1=glm(Top10~.,data=SongsTrain,family = binomial)
summary(Model_1)
AIC: 291.46

res =predict(Model_1, SongsTrain, type = "response" )
table(ActualValue = SongsTrain$Top10, PredictedValue = res >0.5)
accuracy=(416+13)/nrow(SongsTrain)  #0.9171843

res_Test =predict(Model_1, SongsTest, type = "response" )
table(ActualValue = SongsTest$Top10, PredictedValue = res_Test >0.5)
accuracy=(307+20)/nrow(SongsTest)
accuracy #0.8766756

#Build Model with all Significant vriables
Model_2=glm(Top10 ~ timesignature + pitch + timbre_0_min  + timbre_1_min + timbre_3_min + timbre_3_max + 
      timbre_4_min + timbre_6_min  + timbre_10_min, family = binomial, data = SongsTrain)
summary(Model_2)
AIC: 272.46

res =predict(Model_2, SongsTrain, type = "response" )
table(ActualValue = SongsTrain$Top10, PredictedValue = res >0.5)
accuracy=(416+13)/nrow(SongsTrain)
accuracy #0.8881988

res_Test =predict(Model_2, SongsTest, type = "response" )
table(ActualValue = SongsTest$Top10, PredictedValue = res_Test >0.5)
accuracy=(305+19)/nrow(SongsTest)
accuracy #0.8686327
cor(SongsTrain[,unlist(lapply(SongsTrain, is.numeric))])

#stepwise Logistic Regression

Model_3=glm(Top10 ~ timesignature + pitch + timbre_0_min + timbre_0_max + timbre_1_min + timbre_3_min + timbre_3_max + 
      timbre_4_min + timbre_6_min + timbre_8_max + timbre_10_min, family = binomial, data = SongsTrain)
summary(Model_3)
AIC: 256.89

res =predict(Model_3, SongsTrain, type = "response" )
table(ActualValue = SongsTrain$Top10, PredictedValue = res >0.5)
accuracy=(416+19)/nrow(SongsTrain)
accuracy #0.9006211

res_Test =predict(Model_3, SongsTest, type = "response" )
table(ActualValue = SongsTest$Top10, PredictedValue = res_Test >0.5)
accuracy=(309+22)/nrow(SongsTest)
accuracy #0.8873995

library(randomForest)
bestmtry = tuneRF(SongsTrain, SongsTrain$Top10, stepFactor = 1.2, improve = 0.01, trace = TRUE , plot = T)
rf=randomForest(SongsTrain$Top10~.,data = SongsTrain, ntree=500,nodesize=2,mtry=7)
varImpPlot(rf)

Model_4=glm(Top10 ~ timbre_0_max + timbre_0_min+loudness,family = binomial, data = SongsTrain)
summary(Model_4)

res =predict(Model_4, SongsTrain, type = "response" )
table(ActualValue = SongsTrain$Top10, PredictedValue = res >0.5)
accuracy=(422+11)/nrow(SongsTrain)
accuracy #0.8964803

res_Test =predict(Model_4, SongsTest, type = "response" )
table(ActualValue = SongsTest$Top10, PredictedValue = res_Test >0.5)
accuracy=(312+5)/nrow(SongsTest)
accuracy 0.849866

#What is the correlation between the variables "loudness" and "energy" in the training set?
cor(SongsTrain$loudness,SongsTrain$energy) #0.778218

#Create Model 2, which is Model 1 without the independent variable "loudness". 
Model_1_log=glm(Top10 ~ .- loudness, data=SongsTrain, family=binomial)
summary(Model_1_log) #289.66

#Look at the summary of SongsLog2, and inspect the coefficient of the variable "energy". What do you observe?
#The coefficient estimate for energy is positive in Model 2, suggesting that songs with higher energy levels tend to be more popular. 
#However, note that the variable energy is not significant in this model.

#create Model, which should be exactly like Model 1, but without the variable "energy".
Model_2_log=glm(Top10 ~ .- energy, data=SongsTrain, family=binomial)
summary(Model_1_log) #289.46

#Looking at the output of summary(Model_2_log), we can see that loudness has a positive coefficient estimate, meaning that our model 
#predicts that songs with heavier instrumentation tend to be more popular. This is the same conclusion we got from Model 2.

#Make predictions on the test set using Model 3. What is the accuracy of Model 3 on the test set, using a threshold of 0.45? 
#(Compute the accuracy as a number between 0 and 1.)
res_Test =predict(Model_2_log, SongsTest, type = "response" )
table(ActualValue = SongsTest$Top10, PredictedValue = res_Test >0.45)
accuracy=(302+25)/nrow(SongsTest)#0.8766756

#What would the accuracy of the baseline model be on the test set? 
table(SongsTest$Top10)
314/(314+59) #0.8418231

#What is the sensitivity of Model 3 on the test set, using a threshold of 0.45?

#What is the specificity of Model 3 on the test set, using a threshold of 0.45?
