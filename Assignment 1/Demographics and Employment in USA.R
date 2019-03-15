setwd("~/文档/15.071x/Unit1/Assignment1")
CPS <- read.csv("CPSData.csv")

#1.1 How many interviewees are in the dataset?
#1.2 Among the interviewees with a value reported for the Industry variable, what is the most common industry of employment? Please enter the name exactly how you see it
summary(CPS)
str(CPS)

#1.3 Which state has the fewest interviewees?
# Which state has the largest number of interviewees?
sort(table(CPS$State))

#1.4 What proportion of interviewees are citizens of the United States?
a <- table(CPS$Citizenship)
1 - (a["Non-Citizen"]/sum(a))

#1.5 The CPS differentiates between race (with possible values American Indian, Asian, Black, Pacific Islander, White, or Multiracial) and ethnicity. 
#    A number of interviewees are of Hispanic ethnicity, as captured by the Hispanic variable. 
#    For which races are there at least 250 interviewees in the CPS dataset of Hispanic ethnicity? (Select all that apply.)
table(CPS$Race,CPS$Hispanic)[,2] >=250

#2.1 Which variables have at least one interviewee with a missing (NA) value? (Select all that apply.)
summary(CPS)

#2.2 We can see the breakdown of whether Married is missing based on the reported value of the Region variable with the function table(CPS$Region, is.na(CPS$Married)). Which is the most accurate:

#2.3 How many states had all interviewees living in a non-metropolitan area (aka they have a missing MetroAreaCode value)? 
#For this question, treat the District of Columbia as a state (even though it is not technically a state).
#How many states had all interviewees living in a metropolitan area? Again, treat the District of Columbia as a state.
colSums(table(CPS$State,is.na(CPS$MetroAreaCode)) == 0)

#2.4 Which region of the United States has the largest proportion of interviewees living in a non-metropolitan area?
which.max(prop.table(table(CPS$Region,is.na(CPS$MetroAreaCode)),margin = 1)[,"TRUE"])

#2.5 Which state has a proportion of interviewees living in a non-metropolitan area closest to 30%?
#Which state has the largest proportion of non-metropolitan interviewees, ignoring states where all interviewees were non-metropolitan?
sort(tapply(is.na(CPS$MetroAreaCode),CPS$State,mean))


#########
CountryMap <- read.csv("CountryCodes.csv")
MetroAreaMap <- read.csv("MetroAreaCodes.csv")

#3.1 How many observations (codes for metropolitan areas) are there in MetroAreaMap?
# How many observations (codes for countries) are there in CountryMap?
nrow(MetroAreaMap)
nrow(CountryMap)

#3.2 Review the new version of the CPS data frame with the summary() and str() functions. 
# What is the name of the variable that was added to the data frame by the merge() operation?
# How many interviewees have a missing value for the new metropolitan area variable? 
# Note that all of these interviewees would have been removed from the merged data frame if we did not include the all.x=TRUE parameter.
CPS = merge(CPS, MetroAreaMap, by.x="MetroAreaCode", by.y="Code", all.x=TRUE)
summary(CPS)

#3.3 Which of the following metropolitan areas has the largest number of interviewees?
table(CPS$MetroArea)

#3.4 Which metropolitan area has the highest proportion of interviewees of Hispanic ethnicity? 
which.max(tapply(CPS$Hispanic,CPS$MetroArea,mean))

#3.5 Remembering that CPS$Race == "Asian" returns a TRUE/FALSE vector of whether an interviewee is Asian, 
# determine the number of metropolitan areas in the United States from which at least 20% of interviewees are Asian.
table(tapply(CPS$Race == "Asian",CPS$MetroArea,mean) >= 0.2)

#3.6Integrating Metropolitan Area Data
which.min(tapply(CPS$Education == "No high school diploma", CPS$MetroArea, mean,na.rm=TRUE))

#4.1
CPS = merge(CPS,CountryMap,by.x="CountryOfBirthCode",by.y="Code",all.x = TRUE)
summary(CPS)

#4.2
summary(CPS$Country)

#4.3
1-table(subset(CPS$Country,CPS$MetroArea=="New York-Northern New Jersey-Long Island, NY-NJ-PA"))["United States"]/
    sum(table(subset(CPS$Country,CPS$MetroArea=="New York-Northern New Jersey-Long Island, NY-NJ-PA")))

#4.4
which.max(tapply(CPS$Country == "India",CPS$MetroArea,sum,na.rm = TRUE))
which.max(tapply(CPS$Country == "Brazil",CPS$MetroArea,sum,na.rm = TRUE))
which.max(tapply(CPS$Country == "Somalia",CPS$MetroArea,sum,na.rm = TRUE))
