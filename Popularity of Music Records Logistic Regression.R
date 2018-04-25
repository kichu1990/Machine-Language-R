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

#Build Model with all Significant vriables
Model_2=glm(Top10 ~ timesignature + pitch + timbre_0_min  + timbre_1_min + timbre_3_min + timbre_3_max + 
      timbre_4_min + timbre_6_min  + timbre_10_min, family = binomial, data = SongsTrain)
summary(Model_2)
AIC: 272.46

cor(SongsTrain[,unlist(lapply(SongsTrain, is.numeric))])

res =predict(Model_1, SongsTrain, type = "response" )
table(ActualValue = SongsTrain$Top10, PredictedValue = res >0.5)
accuracy=(416+13)/nrow(SongsTrain)  #0.8881988

res =predict(Model_2, SongsTrain, type = "response" )
table(ActualValue = SongsTrain$Top10, PredictedValue = res >0.5)
accuracy=(417+24)/nrow(SongsTrain)
accuracy :0.9130435

res_Test =predict(Model_2, SongsTest, type = "response" )
table(ActualValue = SongsTest$Top10, PredictedValue = res_Test >0.5)
accuracy=(309+20)/nrow(SongsTest)
accuracy
