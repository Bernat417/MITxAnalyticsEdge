#---------------------------------------------LECTURE: WHO DATA ANALYSIS---------------------------------------------#
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

#---------------------------------------------LECTURE: USDA DATA ANALYSIS---------------------------------------------#

USDA = read.csv("USDA.csv", stringsAsFactors = FALSE)
str(USDA)
summary(USDA)
names(USDA)

USDA$Description[which.max(USDA$Cholesterol)]
USDA$Description[which.max(USDA$Sodium)]

HighSodium = subset(USDA, USDA$Sodium > 10000)
nrow(HighSodium)
HighSodium$Description
sd(USDA$Sodium, na.rm = TRUE)

max(USDA$VitaminC)
plot(USDA$Protein, USDA$TotalFat, xlab = "Protein", ylab = "Fat", main = "Protein vs Fat", col = "red")
hist(USDA$VitaminC, xlab = "Vitamin C (mg)", main = "Histogram of Vitaminc C Levels", xlim = c(0,100), breaks = 2000)
boxplot(USDA$Sugar, ylab = "Sugar (grams)", main = "Boxplot of Sugar Levels")


USDA$HighSodium = as.numeric(USDA$Sodium > mean(USDA$Sodium, na.rm = TRUE))
USDA$HighProtein = as.numeric(USDA$Protein > mean(USDA$Protein, na.rm = TRUE))
USDA$HighFat = as.numeric(USDA$TotalFat > mean(USDA$TotalFat, na.rm = TRUE))
USDA$HighCarb = as.numeric(USDA$Carbohydrate > mean(USDA$Carbohydrate, na.rm = TRUE))
names(USDA)

table(USDA$HighSodium)
table(USDA$HighSodium, USDA$HighSodium)

#Groups argument1 elements by argument2 and applies argument3
tapply(USDA$Iron, USDA$HighProtein, mean, na.rm = TRUE)
tapply(USDA$VitaminC, USDA$HighCarb, summary, na.rm = TRUE)

#---------------------------------------------ASSIGNMENT: CHICAGO CRIME ANALYSIS---------------------------------------------#

