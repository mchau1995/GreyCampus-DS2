install.packages("readxl")
library(readxl)
install.packages("dplyr ")
library(dplyr)
setwd("D:/Phan mem/R/Grey-Campus/Assignmet 3")
raw_dataset <- read.csv(file ="COVID19.csv", na.string = "")

##Step 1: Keep rows containing country information and remove the rest of the rows.

dataset_1 <- slice(raw_dataset, 9:n())
dataset_1 <- subset(dataset_1, dataset_1$Country.Other != "Total:")
View(dataset_1)
##Step 2: Calculate the missing percentage of each column using a function
dataset_1
NA_per_cal <- function(x){
  (sum(is.na(x))/length(x))*100
}

NAper <- apply(dataset_1, 2, NA_per_cal)
subset(NAper, NAper>5)
dataset_2 <- subset(dataset_1, select = -(NewDeaths))

##Step 3:
df <- dataset_2
View(df)
str(df)
library(ggplot2)
library(MASS)
library(tidyr)

#1
boxplot(cbind(as.numeric(df$TotalCases),as.numeric(df$TotalDeaths),as.numeric(df$TotalRecovered)), main = "Plot of Cases",
        names = c("Cases", "Death cases", "Recovered Cases"), xlab = "Cases", ylab = "Length",
        col = c("#999999", "#E69F00", "#56B4E9"), frame = FALSE,na.rm = TRUE)

#2

df$TotalCases <- na.omit(as.numeric(gsub("," ,"",df$TotalCases)))
df$Population <- as.numeric(gsub("," ,"",df$Population))
df$Population

ggplot(df, aes(x = TotalCases, y = Population))+
  geom_point(alpha = 0.5)+
  stat_smooth(method = "lm", col = "red", se = FALSE)+
  scale_x_continuous("TotalCases",
                     limits = c(0, 30000000),
                     expand = c(0, 0))
##log value
ggplot(df, aes(x = TotalCases, y = Population))+
  geom_point(alpha = 0.6)+
  coord_fixed()+
  scale_x_log10()+
  scale_y_log10()+
  stat_smooth(method = "lm", col = "red", se = FALSE)  

#3
df$Tot.Cases.1M.pop <- as.numeric(gsub("," ,"",df$Tot.Cases.1M.pop))


ggplot(df, aes(x = Tot.Cases.1M.pop, y = Population))+
  geom_point(alpha = 0.5)+
  scale_x_continuous("TotalCases/1M",
                     limits = c(0, 150000),
                     expand = c(0, 0))+
  scale_y_continuous("Population",
                   limits = c(0, 300000000),
                   expand = c(0, 0))+
  stat_smooth(method = "lm", col = "red", se = FALSE)

##log value
ggplot(df, aes(x = Tot.Cases.1M.pop, y = Population))+
  geom_point(alpha = 0.6)+
  coord_fixed()+
  scale_x_log10()+
  scale_y_log10()+
  stat_smooth(method = "lm", col = "red", se = FALSE)

##4

##5
df$TotalDeaths <- as.numeric(gsub("," ,"",df$TotalDeaths ))
ggplot(df, aes(x = TotalCases, y = TotalDeaths ))+
  geom_point(alpha = 0.5)+
  stat_smooth(method = "lm", col = "red", se = FALSE)

##6
df$Deaths.1M.pop <- as.numeric(gsub("," ,"",df$Deaths.1M.pop))
ggplot(df, aes(x = TotalCases, y = Deaths.1M.pop ))+
  geom_point(alpha = 0.5)+
  stat_smooth(method = "lm", col = "red", se = FALSE)

ggplot(df, aes(x = TotalCases, y = Deaths.1M.pop))+
  geom_point(alpha = 0.6)+
  coord_fixed()+
  scale_x_log10()+
  scale_y_log10()+
  stat_smooth(method = "lm", col = "red", se = FALSE)

##7
ggplot(df, aes(fill=Continent, y=Tot.Cases.1M.pop, x=Continent)) + 
  geom_bar(position="dodge", stat="identity")

##8
ggplot(df, aes(fill=Continent, y=Deaths.1M.pop, x=Continent)) + 
  geom_bar(position="dodge", stat="identity")

##9
df$Tests.1M.pop <- as.numeric(gsub("," ,"",df$Tests.1M.pop))
df$Country.Other[which.max(df$Tests.1M.pop)]


##10
ggplot(df, aes(fill=Continent, y=Tests.1M.pop, x=Continent)) + 
  geom_bar(position="dodge", stat="identity")

#11
install.packages('moments')
library(moments)
df$Tests.1M.pop[df$Tests.1M.pop == ""] <- NA
na.omit(df$Tests.1M.pop)
a<- na.omit(as.numeric(gsub("," ,"",df$Tests.1M.pop)))
skewness(a, na.rm = TRUE)
## result is 3.716781, hence it is right-skewed.


           
     