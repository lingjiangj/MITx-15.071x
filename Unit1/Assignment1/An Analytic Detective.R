setwd("~/文档/15.071x/Unit1/Assignment1")
mvt <- read.csv("mvtWeek1.csv")

#1.1 How many rows of data (observations) are in this dataset?
#1.2 How many variables are in this dataset?
str(mvt)

#1.3 Using the "max" function, what is the maximum value of the variable "ID"?
max(mvt$ID)

#1.4 What is the minimum value of the variable "Beat"?
min(mvt$Beat)

#1.5 How many observations have value TRUE in the Arrest variable (this is the number of crimes for which an arrest was made)?
summary(mvt$Arrest)

#1.6 How many observations have a LocationDescription value of ALLEY?
nrow(subset(mvt, LocationDescription=="ALLEY"))

#2.1 In what format are the entries in the variable Date?
mvt$Date[1]

#2.2 What is the month and year of the median date in our dataset? Enter your answer as "Month Year", without the quotes. (Ex: if the answer was 2008-03-28, you would give the answer "March 2008", without the quotes.)
DateConvert <- as.Date(strptime(mvt$Date, "%m/%d/%y %H:%M"))
summary(DateConvert)

#2.3 In which month did the fewest motor vehicle thefts occur?
#2.4 On which weekday did the most motor vehicle thefts occur?
mvt$Month = months(DateConvert)
mvt$Weekday = weekdays(DateConvert)
mvt$Date = DateConvert

which.min(table(mvt$Month))
which.max(table(mvt$Weekday))

#2.5 Which month has the largest number of motor vehicle thefts for which an arrest was made?
which.max(table(subset(mvt,Arrest == TRUE,Month)))

#3.1 In general, does it look like crime increases or decreases from 2002 - 2012?
# In general, does it look like crime increases or decreases from 2005 - 2008?
#In general, does it look like crime increases or decreases from 2009 - 2011?
hist(mvt$Date, breaks=100)

#3.2 Does it look like there were more crimes for which arrests were made in the first half of the time period or the second half of the time period? (Note that the time period is from 2001 to 2012, so the middle of the time period is the beginning of 2007.)
boxplot(mvt$Date~mvt$Arrest)

#3.3 For what proportion of motor vehicle thefts in 2001 was an arrest made?
arrest_Port <- function(mvt,year){
    a <- table(subset(mvt,Year == year, Arrest))
    portion <- as.numeric(a["TRUE"])/(as.numeric(a["TRUE"])+as.numeric(a["FALSE"]))
    portion
}
arrest_Port(mvt,2001)

#3.4 For what proportion of motor vehicle thefts in 2007 was an arrest made?
arrest_Port(mvt,2007)

#3.5 For what proportion of motor vehicle thefts in 2012 was an arrest made?
arrest_Port(mvt,2012)

#4.1 Which locations are the top five locations for motor vehicle thefts, excluding the "Other" category? You should select 5 of the following options.
sort(table(mvt$LocationDescription))

#4.2 How many observations are in Top5?
Top5 <- subset(mvt,LocationDescription =="STREET"|LocationDescription == "PARKING LOT/GARAGE(NON.RESID.)"|LocationDescription == "ALLEY"|LocationDescription == "GAS STATION"|LocationDescription == "DRIVEWAY - RESIDENTIAL")
str(Top5)

#4.3 One of the locations has a much higher arrest rate than the other locations. Which is it? Please enter the text in exactly the same way as how it looks in the answer options for Problem 4.1.
Top5$LocationDescription = factor(Top5$LocationDescription)
a <- prop.table(table(Top5$LocationDescription, Top5$Arrest),margin=1)   
sort(a[,"TRUE"],decreasing = TRUE)

#4.4 On which day of the week do the most motor vehicle thefts at gas stations happen?
which.max(table(subset(Top5,LocationDescription == "GAS STATION",Weekday)))

#4.5 On which day of the week do the fewest motor vehicle thefts in residential driveways happen?
which.min(table(subset(Top5,LocationDescription == "DRIVEWAY - RESIDENTIAL",Weekday)))






