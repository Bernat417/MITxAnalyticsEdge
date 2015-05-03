#Read the WHO.csv file
WHO = read.csv("WHO.csv",stringsAsFactors=FALSE)

#Analyze the dataframe using numerical descriptive statistics
str(WHO)
summary(WHO)

#Create, analyze, save and discard the subset WHO_Europe
WHO_Europe = subset(WHO, Region == "Europe")
str(WHO_Europe)
summary(WHO_Europe)
write.csv(WHO_Europe,"WHO_Europe.csv")
rm(WHO_Europe)

#Explore the dataframe
WHO$Under15
sd(WHO$Under15)
summary(WHO$Under15)

#Country with the least percentage of population under15
WHO$Country[which.min(WHO$Under15)]

#Country with the most percentage of population under15
WHO$Country[which.max(WHO$Under15)]

#Let's analyze the relation between income and fertility rate
plot(WHO$GNI,WHO$FertilityRate)
Outliers = subset(WHO, GNI > 10000 & FertilityRate >2.5)
nrow(Outliers)
Outliers[c("Country","GNI","FertilityRate")]

#Lets make some plots
hist(WHO$CellularSubscribers)
boxplot(WHO$LifeExpectancy ~ WHO$Region, xlab="", ylab="Life Expectancy", main = "Life Expectancy by Region")

#Lets explore the data
table(WHO$Region)
tapply(WHO$Over60, WHO$Region, mean)
tapply(WHO$LiteracyRate, WHO$Region, min, na.rm = TRUE)
which.min(tapply(WHO$ChildMortality, WHO$Region, min, na.rm = TRUE))
