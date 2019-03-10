setwd("~/文档/15.071x/Unit2/Assignment 2")
climate <- read.csv("climate_change.csv")
# 1.1/1.2
training <- subset(climate,Year <= 2006)
testing <- subset(climate,Year > 2006)
climate_lm <- lm(Temp ~  MEI + CO2 + CH4 + N2O + CFC.11 + CFC.12 + TSI + Aerosols,data = training)
summary(climate_lm)

#2.1
# Answer:All of the gas concentration variables reflect human development - N2O and CFC.11 are correlated with other variables in the data set. 

#2.2
sort(cor(training)["N2O",], decreasing = TRUE)
sort(cor(training)["CFC.11",], decreasing = TRUE)

#3
TempReg <- lm(Temp ~ MEI + TSI + Aerosols + N2O,data = training)
summary(TempReg)

#4
step_Reg <- step(climate_lm)
summary(step_Reg)

#5
TempPrediction <- predict(step_Reg, newdata = testing)
SSE = sum((TempPrediction - testing$Temp)^2)
SST = sum((mean(training$Temp)-testing$Temp)^2)
R2 = 1-SSE/SST
R2
