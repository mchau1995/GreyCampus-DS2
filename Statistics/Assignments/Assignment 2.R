## Minh Chau _ Assignment 2 _ Week 20, 21/2/2021

##1.Print "France win Croatia"
goals <- c(4,3)
country <- c("France", "Croatia")
names(goals) <- country
goals
names(goals[1])
if (goals[1] > goals[2]) {
  print(paste(names(goals[1]), "win", names(goals[2])))
}

##2.
head(mtcars)
str(mtcars)
for (i in 8 : 11){
  mtcars[i] <- as.factor(mtcars[i])
}
str(mtcars)

##Dataset2.1
install.packages(readxl)
library(readxl)
dataset21 <- read_excel("Dataset_2.1.xlsx")
head(dataset21)
str(dataset21)
dataset21$Year <- as.factor(dataset21$Year)
dataset21$ID <- as.factor(dataset21$ID)
View(dataset21)
##1.function to get percentage of NAs in each column: input the name of column 
function_1 <- function(i){
  a <- sum(is.na(i))*100/nrow(dataset21)
  print(paste(a, "%"))
}
function_1(dataset21$No.of.Cylinders.and.Rotors)  

##2.function to get percentage of NAs in each row: input the number of row
function_2 <- function(i){
  a <- sum(is.na(dataset21[i,]))*100/ncol(dataset21)
  print(paste(a, "%"))
}

function_2(27)   

##3.
windows(width = 12, height = 10)
library(dplyr)

datanum <- select_if(dataset21, is.numeric)
View(datanum)
apply(datanum, 2, summary)
for (i in 1:ncol(datanum)) {
  boxplot(datanum[i],main = dimnames(datanum[i]))
}

### Function to get summary and boxplot, input the name of column
function_3_2 <- function(i) {
  if (class(i) == "numeric"){
    boxplot(i,main = dimnames(i))
    summary(i, na.rm = TRUE)
  }
}

function_3_2(dataset21$CO)


##4. Function to get histogram: input the name of column
library(tidyverse)
datanum <- select_if(dataset21, is.numeric)
str(datanum)
draw_histogram = function(dataframe, column){
  ggplot(dataframe, aes(column)) +  geom_histogram(binwidth=0.5)
}
draw_histogram(datanum, datanum$THC)


##2.2

setwd("D:/Phan mem/R/Grey-Campus/Assignment 2")
dataset22 <- read.csv(file ="Data set - 2.2.csv", header = TRUE)
library(lubridate)

#1
d1 <- subset(dataset22, select = c(2,3,5))
d1$First.FD.Date <- mdy(d1$First.FD.Date)
format(d1$First.FD.Date,"%d/%m/%Y")
d1$Last.FD.Date <- mdy(d1$Last.FD.Date)
format(d1$Last.FD.Date,"%d/%m/%Y")
d1$FD.termination.date <- mdy(d1$FD.termination.date)
format(d1$FD.termination.date,"%d/%m/%Y")
str(d1)
## I concern when I finsish format function, I view result following right structur "01/05/2015"
##, but when I view it by str or View fucntion, it appears like this "2015-05-01". 
##Hence, I concern whether I did right or not?? 

##2
dataset22$D.O.B <- dmy(dataset22$Date.of.Birth, locale = "English")
format(dataset22$D.O.B, "19%y/%m/%d")
class(month(dataset22$D.O.B))

##3
d1$First.FD.Date -> dataset22$First.FD.Date
d1$Last.FD.Date -> dataset22$Last.FD.Date  
d1$FD.termination.date -> dataset22$FD.termination.date  

##4
View(dataset22)
dataset22$age <- year(format(dataset22$First.FD.Date,"%Y/%m/%d")) - year(format(dataset22$D.O.B, "19%y/%m/%d"))
dataset22$age 

## In this section, when I tried with the more symbol function,  
##dataset22$age <- year(dataset22$First.FD.Date)) - year(dataset22$D.O.B)), 
# it will give me the wrong value with negative result. But it works in this function. 
## I confused this part a lot.

  
