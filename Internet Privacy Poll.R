setwd("D:\\Study  Materials\\R Language\\DataSet")
IPS=read.csv("AnonymityPoll.csv")

#How many people participated in the poll?
nrow(IPS) #1002

#How many interviewees responded that they use a smartphone?
table(IPS$Smartphone) #487

#How many interviewees responded that they don't use a smartphone?
#472

#How many interviewees did not respond to the question, resulting in a missing value, or NA, 
#in the summary() output?
summary(IPS) 
sum(is.na(IPS))#43

#Which of the following are states in the Midwest census region? 
unique(IPS$State[which(IPS$Region=="Midwest")])

#Wich was the state in the South census region with the largest number of interviewees?
table(IPS$State,IPS$Region)
unique(IPS$Region)

#How many interviewees reported not having used the Internet and not having used a 
#smartphone?
table(IPS$Internet.Use,IPS$Smartphone)
#186

#How many interviewees reported having used the Internet and having used a smartphone?
#470

#How many interviewees reported having used the Internet but not having used a smartphone?
#285

#How many interviewees reported having used a smartphone but not having used the Internet?
#17

#How many interviewees have a missing value for their Internet use?
sum(is.na(IPS$Internet.Use))#1

#How many interviewees have a missing value for their smartphone use?
sum(is.na(IPS$Smartphone))#43

#Use the subset function to obtain a data frame called "limited", which is limited to 
#interviewees who reported Internet use or who reported smartphone use.
limited_IPS=subset(IPS,Smartphone==1|Internet.Use==1)

#How many interviewees are in the new data frame?
nrow(limited_IPS)

#Which variables have missing values in the limited data frame?
summary(limited_IPS)

#What is the average number of pieces of personal information on the Internet, 
#according to the Info.On.Internet variable
mean(limited_IPS$Info.On.Internet)

#How many interviewees reported a value of 0 for Info.On.Internet?
table(limited_IPS$Info.On.Internet) #105

#How many interviewees reported the maximum value of 11 for Info.On.Internet?
#8

#What proportion of interviewees who answered the Worry.About.Info question worry about 
#how much information is available about them on the Internet? 
prop.table(table(limited_IPS$Worry.About.Info))
table(limited_IPS$Worry.About.Info)
386/(386+404)

#What proportion of interviewees who answered the Anonymity.
table(limited_IPS$Anonymity.Possible)
278/(475+278)

#What proportion of interviewees who answered the Tried.
table(limited_IPS$Tried.Masking.Identity)
128/(128+656)

#What proportion of interviewees who answered the Privacy.Laws
table(limited_IPS$Privacy.Laws.Effective)
186/(186+541)

#Build a histogram of the age of interviewees. What is the best represented age group 
#in the population?
hist(limited_IPS$Age,breaks=100
plot(limited_IPS$Age,limited_IPS$Info.On.Internet)
max(table(limited_IPS$Age,limited_IPS$Info.On.Internet))

#To avoid points covering each other up, we can use the jitter() function on the values 
#we pass to the plot function. Experimenting with the command jitter(c(1, 2, 3)), 
#what appears to be the functionality of the jitter command?
jitter(c(1, 2, 3))

#jitter adds or subtracts a small amount of random noise to the values passed to it, and two runs will yield different 
#results
#Now, plot Age against Info.On.Internet with plot(jitter(limited$Age), jitter(limited$Info.On.Internet)). 
#What relationship to you observe between Age and Info.On.Internet?
plot(jitter(limited_IPS$Age), jitter(limited_IPS$Info.On.Internet))
#Older age seems moderately associated with a smaller value for Info.On.Internet

#What is the average Info.On.Internet value for smartphone users?
tapply(limited_IPS$Info.On.Internet,limited_IPS$Smartphone,summary)
#2.92

#What is the average Info.On.Internet value for non-smartphone users?
#4.368

#What proportion of smartphone users who answered the Tried.Masking.Identity question have 
#tried masking their identity when using the Internet?
tapply(limited_IPS$Tried.Masking.Identity,limited_IPS$Smartphone,table)
93/(93+390)

#What proportion of non smartphone users who answered the Tried.Masking.
#Identity question have tried masking their identity when using the Internet?
33/(248+33)
