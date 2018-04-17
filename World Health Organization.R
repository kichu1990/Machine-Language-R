WHO=read.csv("C:\\Users\\abhic6\\Downloads")
str(WHO)
summary(WHO)
WHO$Country=as.factor(WHO$Country)
WHO$Region=as.factor(WHO$Region)
WHO$FertilityRate=as.numeric(WHO$FertilityRate)
WHO$PrimarySchoolEnrollmentMale=as.numeric(WHO$PrimarySchoolEnrollmentMale)
WHO$PrimarySchoolEnrollmentFemale=as.numeric(WHO$PrimarySchoolEnrollmentFemale)
WHO$GNI=as.numeric(WHO$GNI)
WHO$LiteracyRate=as.numeric(WHO$LiteracyRate)
WHO_Europe=subset(WHO,Region=="Europe")
write.csv(WHO_Europe.csv)
summary(WHO_Europe$Under15)
plot(WHO$GNI,WHO$FertilityRate)


outliers=subset(WHO,GNI>10000&FertilityRate>2.5)
outliers[c("Country","FertilityRate","GNI")]

#What is the mean value of the "Over60" variable?        
mean(WHO$Over60)

#Which country has the smallest percentage of the population over 60?
which.min(WHO$Over60)
WHO$Country[183]

#Which country has the largest literacy rate?
summary(WHO$LiteracyRate)
which.max(WHO$LiteracyRate)
WHO$Country[44]

hist(WHO$CellularSubscribers)
boxplot(WHO$LifeExpectancy~WHO$Region,xlab="",ylab="LifeExpectancy")    
tapply(WHO$Over60,WHO$Region,mean)
tapply(WHO$LiteracyRate,WHO$Region,min) #NA are coming due to missing value
tapply(WHO$LiteracyRate,WHO$Region,min,na.rm=T)

#Which region has the lowest average child mortality rate across all countries in that region?
tapply(WHO$ChildMortality,WHO$Region,min)
