setwd("~/Ubiqum/Project 2/Task 2 - Predicting Brand Preference")
incomplete <- read.csv("SurveyIncomplete.csv")
complete <- read.csv("Complete_Responses.csv")

library(caret)
library(dplyr)
library(ggplot2)
library(gridExtra)

summary(complete)
str(complete)

#updating data types of variables
complete$elevel <- as.ordered(complete$elevel)
complete$car <- as.factor(complete$car)
complete$brand <- as.factor(complete$brand)
complete$zipcode <- as.factor(complete$zipcode)

#visualizing data: bar charts and histograms of each variable
p1 <- ggplot(complete, aes(elevel)) +
  geom_bar(fill = "white", color = "black")
p2 <- ggplot(complete, aes(car)) +
  geom_bar(fill = "white", color = "black")
p3 <- ggplot(complete, aes(zipcode)) +
  geom_bar(fill = "white", color = "black")
p4 <- ggplot(complete, aes(brand)) +
  geom_bar(fill = "white", color = "black")
p5 <- ggplot(complete, aes(age)) +
  geom_histogram(binwidth = 5, fill = "white", color = "black") +
  scale_x_continuous(breaks = seq(20, 80, 5))
p6 <- ggplot(complete, aes(ï..salary)) +
  geom_histogram(binwidth = 10000, fill = "white", color = "black") +
  scale_x_continuous(breaks = seq(20000, 160000, 20000))
p7 <- ggplot(complete, aes(credit)) +
  geom_histogram(binwidth = 50000, fill = "white", color = "black") +
  scale_x_continuous(breaks = seq(0, 500000, 100000))
grid.arrange(p1, p2, p3, p4, p5, p6, p7, ncol=3)
#all data relatively evenly distributed

#visualizing data: boxplots of each variable
boxplot(complete$age, horizontal = TRUE, main = "Age")
boxplot(complete$credit, horizontal = TRUE, main = "Credit")
boxplot(complete$ï..salary, horizontal = TRUE, main = "Salary")
#no outliers in boxplots

#hypothesis: income and age influences brand preference
#create a boxplots to visualize hypothesis
p10 <- ggplot(complete, aes(x = brand, y = age)) +
  geom_boxplot() #age needs to be explored further
p20 <- ggplot(complete, aes(x = brand, y = ï..salary)) +
  geom_boxplot() #income seems to have effect on brand preference
p30 <- ggplot(complete, aes(x = brand, y = credit)) +
  geom_boxplot()
grid.arrange(p10, p20, p30, ncol = 3)

#create violin plots to see distribution better
p12 <- ggplot(complete, aes(x = brand, y = age)) +
  geom_violin() #age needs to be explored further
p22 <- ggplot(complete, aes(x = brand, y = ï..salary)) +
  geom_violin() #income seems to have effect on brand preference
p32 <- ggplot(complete, aes(x = brand, y = credit)) +
  geom_violin()
grid.arrange(p12, p22, p32, ncol = 3)

#plot brand preference against other categorical variables to explor further
p11 <- ggplot(complete, aes(x = brand, fill = elevel)) +
  geom_bar() +
  scale_y_continuous(breaks = seq(0, 6000, 1000))
p12 <- ggplot(complete, aes(x = brand, fill = car)) +
  geom_bar() +
  scale_y_continuous(breaks = seq(0, 6000, 1000))
p13 <- ggplot(complete, aes(x = brand, fill = zipcode)) +
  geom_bar() +
  scale_y_continuous(breaks = seq(0, 6000, 1000))
grid.arrange(p11, p12, p13, ncol = 3)

#plot histograms with brand preference filled in by color
p22 <- ggplot(complete, aes(x=ï..salary, fill=brand)) +
      geom_histogram(binwidth = 10000, color = "black") +
      scale_x_continuous(breaks = seq(20000, 160000, 20000))
p23 <- ggplot(complete, aes(x=age, fill=brand)) +
      geom_histogram(binwidth = 5, color = "black") +
      scale_x_continuous(breaks = seq(20, 80, 5))
p24 <- ggplot(complete, aes(x=credit, fill=brand)) +
      geom_histogram(binwidth = 10000, color = "black") +
      scale_x_continuous(breaks = seq(0, 500000, 50000))
grid.arrange(p22, p23, p24, ncol = 1)

#plot barcharts with brand preference filled in by color
p31 <- ggplot(complete, aes(x=elevel, fill=brand)) +
  geom_bar()
p32 <- ggplot(complete, aes(x=car, fill=brand)) +
  geom_bar()
p33 <- ggplot(complete, aes(x=zipcode, fill=brand)) +
  geom_bar()
grid.arrange(p31, p32, p33, ncol = 1)