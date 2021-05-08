####### Coding challenge
head(iris)
View(iris)
### Question 1: Means of Sepal.length of setosa differs from means of versicolor. Create a 
# graph to examine the difference between two groups. 
install.packages("tidyverse")
install.packages("rstatix")
install.packages("ggpubr")
library(tidyverse)
library(rstatix)
library(ggpubr)
library(dplyr)
levels(iris$Species)
#  group_by(Species)%>%
iris %>%
  filter(Species != "virginica") %>%
  group_by(Species) %>%
  ggplot(aes(x=Species, y=Sepal.Length))+
  geom_boxplot()
  
### Question 2: Please check, if all assumptions are meeting.
iris %>%
  filter(Species != "virginica") %>%
  group_by(Species) %>%
  identify_outliers(Sepal.Length)

# Normality assumptions

# Compute Shapiro wilk test by groups
iris %>%
  filter(Species != "Virginica") %>%
  group_by(Species) %>%
  shapiro_test(Sepal.Length)

# Draw a qq plot by group
iris %>%
  filter(Species != "virginica") %>%
  group_by(Species) %>%
  ggqqplot(iris, x = "Sepal.Length", facet.by = "Species")


# Check the equality of variances
iris %>% 
  filter(Species != "virginica") %>%
  levene_test(Sepal.Length ~ Species)

### Question 3: Please use appropriate test to see the difference.
iris %>%
  filter(Species != "Virginica") %>%
  t_test(Sepal.Length ~ Species, var.equal = TRUE) %>%
  add_significance()


### Question 4: There are 3 species of plants in iris data set. Please use 
# appropriate test to examine the difference in sepal length, 
levels(iris$Species)
anv = aov(Sepal.Length ~ Species, data = iris)
summary(anv)

### Question 5: Please create appropriate box plot to compare three groups
iris %>%
  ggplot(aes(x=Species,y=Sepal.Length))+
  geom_boxplot()

### Question 6: If there is any difference in Sepal.length, which groups are
# different.

# pst hoc test
TukeyHSD(anv)


#Pairwise t-test
pairwise.t.test(iris$Sepal.Length, iris$Species, p.adjust.method =  "BH")

### Question 7: Please check all assumptions required for the test.

## Conditions of one way ANOVA

## 1) Check the homogeneity of variance assumption
plot(anv,1)

## leven's test for homogeneity of variance
iris %>% levene_test(Sepal.Length ~ Species)
##2) Normality of residuals.
plot(anv,2)

# Extract the residuals
residuals <- residuals(anv)
# Run Shapiro-Wilk test
shapiro.test(residuals)
