### Generating_Insights_Through_Data_Visualization
library(readxl)

Weekly_Gasoline_prices <- read_excel(
   "E:/R prac/Chapter 4/Lesson_05_Generating_Insights_Through_Data_Visualization/Weekly Gasoline prices.xls")
View(Weekly_Gasoline_prices)
head(Weekly_Gasoline_prices) 

plot(Weekly_Gasoline_prices ,type='l',main='Time-series',col='red', lwd=2)

install.packages("zoo")
library(zoo)

WGP = zoo(Weekly_Gasoline_prices$`Gasoline Prices  (Dollars per Gallon)`,Weekly_Gasoline_prices$Date)
head(WGP)

WGP1 = aggregate(WGP, as.yearqtr)
par(mfrow=c(2,1))


plot(Weekly_Gasoline_prices ,type='l',main='Time-series weekly',col='red', lwd=2)
plot(WGP1 ,type='l',main='Time-series Qutrly',col='green', lwd=2)

par(mfrow=c(1,1))

barplot(WGP1, ylab= 'Quaterly Prices', xlab = 'Date', col= 'blue', main='Gasoline',border='red')


############################
library(readxl)
Nations <- read_excel("E:/R prac/Chapter 4/Lesson_05_Generating_Insights_Through_Data_Visualization/Nations.xlsx")
View(Nations)

head(Nations)

Nations$Outlook=as.factor(Nations$Outlook)
levels(Nations$Outlook)
table(Nations$Outlook)

barplot(table(Nations$Outlook) ,main = 'Barplot')
barplot(table(Nations$Outlook) ,main = 'Barplot',horiz = T)
barplot(table(Nations$Outlook) ,main = 'Rating outlook', col = rainbow(3),
        xlab = 'outlook',ylab = 'Freq',border = 'green')
legend('topleft',c('Negative','Positive','Stable'),fill=rainbow(3),cex = 1)


################Frequency Distribution

library(readxl)
Soft_drink <- read_excel("E:/R prac/Chapter 4/Lesson_05_Generating_Insights_Through_Data_Visualization/Cold drink data.xlsx")
summary(Soft_drink)
head(Soft_drink)
table(Soft_drink)
Soft_drink=cbind.data.frame(
  table(Soft_drink))
Soft_drink$relative_freq=Soft_drink$Freq/sum(Soft_drink$Freq)

barplot(Soft_drink$relative_freq,names.arg = Soft_drink$Cold.Drink,col = c(1,2,3,4,5))
barplot(Soft_drink$relative_freq,names.arg = Soft_drink$Cold.Drink,col = rainbow(5))
barplot(Soft_drink$relative_freq,names.arg = Soft_drink$Cold.Drink,col = rainbow(5),horiz = T)
barplot(Soft_drink$relative_freq,names.arg = Soft_drink$Cold.Drink,col = rainbow(5))
legend('topright',legend=c('ck','dck','pep','psi','Spr'),fill =  rainbow(5),cex = .7)

pie(Soft_drink$relative_freq, labels =Soft_drink$Cold.Drink, main = 'Soft_drink' ,col =   rainbow(5))

paste0(round(Soft_drink$relative_freq*100,2),'%')

pie(Soft_drink$relative_freq, labels =paste0(round(Soft_drink$relative_freq*100,2),'%')
, main = 'Soft_drink' ,col =   rainbow(5))
legend('topright',legend=c('ck','dck','pep','psi','Spr'),fill =  rainbow(5),cex = .7)

install.packages("plotrix")
library("plotrix")
pie3D(Soft_drink$relative_freq, labels =paste0(round(Soft_drink$relative_freq*100,2),'%')
      , main = 'Soft_drink' ,col =   rainbow(5))
legend('topleft',c('ck','dck','pep','psi','Spr'),fill =  rainbow(5),cex = .5)

############## Histogram 
var =sample(50:100,10000,replace = T)
summary(var)
hist(var,xlab= 'frequency',ylab = 'variable',col = rainbow(20),border = 'green',breaks = 100)
par(mfrow=c(1,3))
hist(var,xlab= 'frequency',ylab = 'variable',col = rainbow(20),border = 'green',breaks = 100)
hist(var,xlab= 'frequency',ylab = 'variable',col = rainbow(20),border = 'green',breaks = 10)
hist(var,xlab= 'frequency',ylab = 'variable',col = rainbow(20),border = 'green',breaks = 1)






