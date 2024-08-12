setwd('E:/R prac/Chapter 4/Lesson_04_Data_Cleaning_Data_Handling')
getwd()
Data = read.csv('Salary.csv')
dim(Data)


summary(Data)
colnames(Data)


#changing column names 

colnames(Data)[1]='Names'
colnames(Data)[2:4]=c('Title','ID','Agencyname')


############### Cleaning
x=c(1,NA,2,3,4,-0.5,0.2)
x
x>2
is.na(NA)
x>2 & !is.na(x)
(x==0) | (x==2)
(x==0 | x==2) & !is.na(x) 
is.nan(0/0)
is.infinite(1/0)

head(Data)
Data_1 = Data
Data_1[1000,5]= NA
Data_1[3000,2]= NA
Data_1[4000,3]= NA

sum(is.na(Data))
sum(is.na(Data_1))
all(is.na(Data))
all(is.na(Data_1))

###Changing NA
Data_1[is.na(Data_1)]=0
sum(is.na(Data_1))
all(!is.na(Data_1))

###cleaning Na from data frame

DF= data.frame(a=c(NA,1,2),b=c('one',NA,'three'))
DF

subset(DF,!is.na(a))
subset(DF,!is.na(b))
subset(DF,complete.cases(DF))
na.omit(DF)
#######
install.packages(car)
library(car)
head(Freedman)
str(Freedman)
summary(Freedman)

median(Freedman$density)
median(Freedman$density,na.rm = T)


Freedman.good =na.omit(Freedman)
summary(Freedman.good)



Freedman_notav = Freedman[!complete.cases(Freedman),]
Freedman_notav


######## outliers replace
library(UsingR)
install.packages('UsingR')
library(UsingR)

x = babies$dwt
summary(x)
x[x==999]= NA
summary(x)
range(x, na.rm = T)


########### REmoval Of Non Unique Value
head(Data)
Data_2 = Data
dim(Data_2)

Data_3 =rbind.data.frame(Data_2,Data_2[1:500,])
dim(Data_3)

Data_4 = unique(Data_3)
dim(Data_4)


############## Data Handling
############Selection of rows and columns 

head(iris)
iris[,3]
head(iris[,3])

iris[,c(3,5)]
iris[,c(3:5)]
iris[c(2:10),c(2:5)]
iris[c('Sepal.Width','Species')]
head(iris[c('Sepal.Width','Species')])


###Creating new variables

iris$petal.ratio = iris$Petal.Length/iris$Petal.Width
iris$Sepal.ratio = iris$Sepal.Length/iris$Sepal.Width
head(iris)


###Extraction of observations
iris[iris$Petal.Width>.5 & iris$Species == 'setosa',]
subset(iris,Petal.Width>.5 & Species == 'setosa')

####summarizing the observations


install.packages('dplyr')
library(dplyr)
summary(iris)
str(iris)

summarise(iris,petal.length.mean= mean(iris$Petal.Length),sepal.length.mean =mean(iris$Sepal.Length),
          petal.length.sd= sd(iris$Petal.Length),sepal.length.sd =sd(iris$Sepal.Length),)


#### how to work with data frame

library(car)

dim(Davis)
head(Davis)
output = data.frame(matrix(nrow = dim(Davis)[1],ncol = dim(Davis)[2]))
dim(output)
colnames(output) = c('Gender','wt','ht','repwt','repht')
head(output)

output$Gender =Davis$sex
output$wt=Davis$weight
output$ht=Davis$height
output$repwt=Davis$repwt
output$repht=Davis$repht
 head(output)
 
 
#### Working with factor variables 
 


library(UsingR)
head(Cars93) 
d = Cars93[1:3,1:4]
d
str(d)

d[3,4] = NA
d[1,1]= NA
d


d[3,c(2,4)] = list('A3',40)

d
class(d$Model)
levels(d$Model)
d$Model =droplevels(d$Model)
d$Model


levels(d$Model)=c(levels(d$Model), c('A3','A4','A5'))
levels(d$Model)
d

d[3,c(2,4)] = list('A3',40)
d[4,]= list('Audi','A3','Midsize', 35)
d
d=rbind(d,list('Audi','A4','Midsize', 35))
d



####Transforming Data frames Across Long and wider format 

speed.1 = c(850,740,900,1070,930,850)
speed.2 = c(930,650,760,810,1000,1000)
speed.3 = c(800,790,760,800,880,880)
speed.4 = c(800,790,760,800,880,880)
speed.5 =c(800,790,760,800,880,880)
id = c(1,2,3,4,5,6)
Run = c('A','B','C','D','E','F')
Speed = cbind.data.frame(id ,Run,speed.1,speed.2,speed.3,speed.4,speed.5)
head(Speed)


install.packages("reshape2")
library(reshape2)

long = melt(Speed, id.vars = names(Speed)[1:2], variable.name = 'Speed')
head(long, 10)


wide = dcast(long, id+Run ~Speed)



#### Merging Data Frames 

V1 = c('The Avengers', 'Dark knight','Hunger Games', 'Skyfall', 'Hobbit')
V2 = c(885462359,654235987,523652368,236541879,985698569)
domestic = cbind.data.frame(V1,V2)
head(domestic)
colnames(domestic)= c('Names', 'Domestic')
head(domestic)

V3 = c('The Avengers', 'Dark knight','Ice age', 'Skyfall', 'Hobbit')
V4 = c(885462359,654235987,523652368,236541879,985698569)
foreign = cbind.data.frame(V3,V4)
head(foreign)
colnames(foreign)= c('name','foreign')
head(foreign)

final= merge(domestic,foreign, by.x = 'Names',by.y = 'name' ,all = T)

####determining duplicate


df <- data.frame(cats = c('a','b','a','b','c'), num1 = c(5,6,8,10,3))
df[duplicated(df[,'cats']) | duplicated(df[,'cats'],fromLast=T),]
