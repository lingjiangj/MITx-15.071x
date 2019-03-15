setwd("~/文档/15.071x/Unit1/Assignment1")
IBM <- read.csv("IBMStock.csv")
GE <- read.csv("GEStock.csv")
ProcterGamble <- read.csv("ProcterGambleStock.csv")
CocaCola <- read.csv("CocaColaStock.csv")
Boeing <- read.csv("BoeingStock.csv")

#1.1 Our five datasets all have the same number of observations. How many observations are there in each data set?
IBM$Date = as.Date(IBM$Date, "%m/%d/%y")
GE$Date = as.Date(GE$Date, "%m/%d/%y")
CocaCola$Date = as.Date(CocaCola$Date, "%m/%d/%y")
ProcterGamble$Date = as.Date(ProcterGamble$Date, "%m/%d/%y")
Boeing$Date = as.Date(Boeing$Date, "%m/%d/%y")
nrow(IBM)
nrow(GE)
nrow(CocaCola)
nrow(ProcterGamble)
nrow(Boeing)

#1.2 What is the earliest year in our datasets?
#1.3 What is the latest year in our datasets?
summary(IBM$Date)

#1.4 What is the mean stock price of IBM over this time period?
summary(IBM$StockPrice)

#1.5 What is the minimum stock price of General Electric (GE) over this time period?
summary(GE)

#1.6 What is the maximum stock price of Coca-Cola over this time period?
summary(CocaCola)

#1.7 What is the median stock price of Boeing over this time period?
summary(Boeing)

#1.8 What is the standard deviation of the stock price of Procter & Gamble over this time period?
sd(ProcterGamble$StockPrice)

#2.1 Around what year did Coca-Cola has its highest stock price in this time period?
plot(CocaCola$Date, CocaCola$StockPrice, type='l', col="red")

#2.2 In March of 2000, the technology bubble burst, and a stock market crash occurred. According to this plot, which company's stock dropped more?
#2.3 Around 1983, the stock for one of these companies (Coca-Cola or Procter and Gamble) was going up, while the other was going down. Which one was going up?
lines(ProcterGamble$Date, ProcterGamble$StockPrice)
abline(v=as.Date(c("2000-03-01")), lwd=2)
abline(v=as.Date(c("1983-03-01")), lwd=1)

#3.1 Which stock fell the most right after the technology bubble burst in March 2000?
#3.2 Which stock reaches the highest value in the time period 1995-2005?
plot(CocaCola$Date[301:432], CocaCola$StockPrice[301:432], type="l", col="red", ylim=c(0,210))
lines(ProcterGamble$Date[301:432], ProcterGamble$StockPrice[301:432], col="blue")
lines(IBM$Date[301:432], IBM$StockPrice[301:432], col="green")
lines(GE$Date[301:432],GE$StockPrice[301:432],col="purple")
lines(Boeing$Date[301:432],Boeing$StockPrice[301:432],col="orange")
abline(v=as.Date(c("2000-03-01",lwd=1)))

#3.3 In October of 1997, there was a global stock market crash that was caused by an economic crisis in Asia. Comparing September 1997 to November 1997, which companies saw a decreasing trend in their stock price? (Select all that apply.)
abline(v=as.Date(c("1997-09-01",lwd=1)))
abline(v=as.Date(c("1997-11-01",lwd=1)))

#3.4 In the last two years of this time period (2004 and 2005) which stock seems to be performing the best, in terms of increasing stock price?
abline(v=as.Date(c("2004-01-01",lwd=1)))
abline(v=as.Date(c("2005-01-01",lwd=1)))

#4.1 For IBM, compare the monthly averages to the overall average stock price. In which months has IBM historically had a higher stock price (on average)? Select all that apply.
tapply(IBM$StockPrice,months(IBM$Date),mean)

#4.2 General Electric and Coca-Cola both have their highest average stock price in the same month. Which month is this?
tapply(GE$StockPrice,months(IBM$Date),mean)
tapply(CocaCola$StockPrice,months(IBM$Date),mean)







