setwd("~/文档/15.071x/Unit1/Assignment1")
#1.1
poll <- read.csv("AnonymityPoll.csv")
summary(poll)
str(poll)

#1.2
table(poll$Smartphone)
summary(poll$Smartphone)

#1.3
sort(table(poll$State,poll$Region)[,"Midwest"],decreasing = TRUE)
sort(table(poll$State,poll$Region)[,"South"],decreasing = TRUE)

#2.1
table(poll$Internet.Use,poll$Smartphone,dnn=c("Internet use","smart phone use"))

#2.2
summary(poll$Internet.Use)
summary(poll$Smartphone)

#2.3
limited <- subset(poll, Internet.Use == 1|Smartphone == 1)
nrow(limited)

#3.1/3.2
summary(limited)

#3.3
table(limited$Info.On.Internet)

#3.4
prop.table(table(limited$Worry.About.Info))

#3.5
prop.table(table(limited$Anonymity.Possible))

#3.6
prop.table(table(limited$Tried.Masking.Identity))

#3.7
prop.table(table(limited$Privacy.Laws.Effective))

#4.1
hist(limited$Age,limited$Info.On.Internet)

#4.2
max(table(limited$Age,limited$Info.On.Internet))

#4.3
jitter(c(1, 2, 3))

#4.4    
plot(jitter(limited$Age), jitter(limited$Info.On.Internet))

#4.5
tapply(limited$Info.On.Internet,limited$Smartphone,summary)

#4.6
a <- tapply(limited$Tried.Masking.Identity,limited$Smartphone,table)
prop.table(a$`0`)
prop.table(a$`1`)
