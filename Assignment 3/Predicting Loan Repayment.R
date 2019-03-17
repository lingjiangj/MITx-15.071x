setwd("~/文档/15.071x/Unit3/Assignment 3")
#Problem 1.1
loans <- read.csv("loans.csv")
str(loans)
prop.table(table(loans$not.fully.paid))

#Problem 1.2
summary(loans)

#Problem 1.3
missing <- subset(loans,is.na(log.annual.inc)|is.na(days.with.cr.line)|is.na(revol.util)|is.na(inq.last.6mths) | is.na(delinq.2yrs) | is.na(pub.rec))
nrow(missing)
table(missing$not.fully.paid)

#Problem 1.4
loansimputed=read.csv("loans_imputed.csv")
library(mice)
set.seed(144)
vars.for.imputation = setdiff(names(loans), "not.fully.paid")
imputed = complete(mice(loans[vars.for.imputation]))
loans[vars.for.imputation] = imputed
summary(loansimputed)
summary(loans)

#Problem 2.1
library(caTools)
set.seed(144)
split = sample.split(loans$not.fully.paid, 0.7)
train = subset(loans, split == TRUE)
test = subset(loans, split == FALSE)
mod <- glm(not.fully.paid ~.,data = train, family = "binomial")
summary(mod)

#Problem 2.3
test$predicted.risk = predict(mod, newdata=test, type="response")
table(test$not.fully.paid, test$predicted.risk > 0.5)
2403/2873
2413/2873

#Problem 2.4
library(ROCR)
pred <- prediction(test$predicted.risk,test$not.fully.paid)
as.numeric(performance(pred,"auc")@y.values)

#Problem 3.1
bivariate = glm(not.fully.paid~int.rate, data=train, family="binomial")
summary(bivariate)

#Problem 3.2
pred.bivariate <- predict(bivariate,newdata = test,type = "response")
summary(pred.bivariate)

#Problem 3.3
predicion.bivariate <- prediction(pred.bivariate,test$not.fully.paid)
as.numeric(performance(predicion.bivariate,"auc")@y.values)

#Problem 4.1
10*exp(0.06*3)

#Problem 5.1
test$profit = exp(test$int.rate*3) - 1
test$profit[test$not.fully.paid == 1] = -1
summary(test$profit)

#Problem 6.1
highInterest <- subset(test,int.rate >= 0.15)
summary(highInterest)
prop.table(table(highInterest$not.fully.paid))

#Problem 6.2
cutoff = sort(highInterest$predicted.risk, decreasing=FALSE)[100]
selectedLoans <- subset(highInterest,predicted.risk <= cutoff)
sum(selectedLoans$profit)
table(selectedLoans$not.fully.paid)









