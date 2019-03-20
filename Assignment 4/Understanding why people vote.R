setwd("~/文档/15.071x/Unit 4/Assignment 4")
gerber <- read.csv("gerber.csv")

#Problem 1.1
prop.table(table(gerber$voting))

#Problem 1.2
civ <- as.numeric(tapply(gerber$voting,gerber$civicduty,mean)[2])
haw <- as.numeric(tapply(gerber$voting,gerber$hawthorne,mean)[2])
self <- as.numeric(tapply(gerber$voting,gerber$self,mean)[2])
nei <- as.numeric(tapply(gerber$voting,gerber$neighbors,mean)[2])
which.max(c (civ,haw,self,nei))

#Problem 1.3
voting_glm <- glm(voting~civicduty+hawthorne+self+neighbors,data = gerber,family = "binomial")
summary(voting_glm)

#Problem 1.4
voting_pred <- predict(voting_glm,type = "response")
table(gerber$voting,voting_pred > 0.3)
(134513+51966)/(134513+51966+56730+100875)

#Problem 1.5
table(gerber$voting,voting_pred >0.5)
235388/(235388+108696)

#Problem 1.6
library(ROCR)
ROCRpred <- prediction(voting_pred,gerber$voting)
as.numeric(performance(ROCRpred,"auc")@y.values)

#Problem 2.1
library(rpart)
library(rpart.plot)
CARTmodel = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber)
prp(CARTmodel)

#Problem 2.2/2.3
CARTmodel2 = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber, cp=0.0)
prp(CARTmodel2)

#Problem 2.4
CARTmodel3 = rpart(voting ~ civicduty + hawthorne + self + neighbors +sex, data=gerber, cp=0.0)
prp(CARTmodel3)

#Problem 3.1/3.2
CARTcontrol <- rpart(voting ~ control,data = gerber,cp =0.0)
CARTsex <- rpart(voting ~ control + sex,data = gerber, cp = 0.0)
prp(CARTcontrol,digits = 6)
abs(0.296638-0.34)
prp(CARTsex,digit = 6)
abs(0.334176-0.290456)
abs(0.302795-0.345818)

#Problem 3.3
LogModelSex <- glm(voting~control+sex, data = gerber,family = "binomial")
summary(LogModelSex)

#Problem 3.4
Possibilities = data.frame(sex=c(0,0,1,1),control=c(0,1,0,1))
predict(LogModelSex, newdata=Possibilities, type="response")
abs(0.2908065-0.290456)

#Problem 3.5
LogModel2 = glm(voting ~ sex + control + sex:control, data=gerber, family="binomial")
summary(LogModel2)

#Problem 3.6
predict(LogModel2, newdata=Possibilities, type="response")
abs(0.2904558-0.290456)





























