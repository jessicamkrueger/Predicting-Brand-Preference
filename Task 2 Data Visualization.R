setwd("~/Ubiqum/Project 2/Task 2 - Predicting Brand Preference/Original Data Sets")
incomplete <- read.csv("SurveyIncomplete.csv")
complete <- read.csv("Complete_Responses.csv")

#Load necessary packages
library(caret)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(rpart.plot)

summary(complete)
str(complete)

#updating data types of variables
complete$elevel <- as.ordered(complete$elevel)
complete$car <- as.factor(complete$car)
complete$brand <- as.factor(complete$brand)
complete$zipcode <- as.factor(complete$zipcode)

#visualizing data: bar charts and histograms of each variable
p1 <- ggplot(complete, aes(elevel)) +
  geom_bar(fill = "blue", color = "black") +
  labs( x = "Education Level", y = "Count of Responses")
p2 <- ggplot(complete, aes(car)) +
  geom_bar(fill = "green", color = "black") +
  labs( x = "Make of Primary Car", y = "Count of Responses")
p3 <- ggplot(complete, aes(zipcode)) +
  geom_bar(fill = "red", color = "black") +
  labs( x = "Region Based on Zipcode", y = "Count of Responses")
p5 <- ggplot(complete, aes(age)) +
  geom_histogram(binwidth = 5, fill = "yellow", color = "black") +
  scale_x_continuous(breaks = seq(20, 80, 5)) +
  labs( x = "Age", y = "Count of Responses")
p6 <- ggplot(complete, aes(�..salary)) +
  geom_histogram(binwidth = 10000, fill = "purple", color = "black") +
  scale_x_continuous(breaks = seq(20000, 160000, 20000)) +
  labs( x = "Salary not Including Bonuses", y = "Count of Responses")
p7 <- ggplot(complete, aes(credit)) +
  geom_histogram(binwidth = 50000, fill = "orange", color = "black") +
  scale_x_continuous(breaks = seq(0, 500000, 100000)) +
  labs( x = "Amount of Credit Available", y = "Count of Responses")
grid.arrange(p1, p2, p3, p5, p6, p7, ncol=3)
#all data relatively evenly distributed

#visualizing data: boxplots of each variable
boxplot(complete$age, horizontal = TRUE, main = "Age")
boxplot(complete$credit, horizontal = TRUE, main = "Credit")
boxplot(complete$�..salary, horizontal = TRUE, main = "Salary")
#no outliers in boxplots

#hypothesis: income and age influences brand preference
#create a boxplots to visualize hypothesis
p10 <- ggplot(complete, aes(x = brand, y = age)) +
  geom_boxplot() #age needs to be explored further
p20 <- ggplot(complete, aes(x = brand, y = �..salary)) +
  geom_boxplot() #income seems to have effect on brand preference
p30 <- ggplot(complete, aes(x = brand, y = credit)) +
  geom_boxplot()
grid.arrange(p10, p20, p30, ncol = 3)

#create violin plots to see distribution better
p12 <- ggplot(complete, aes(x = brand, y = age)) +
  geom_violin() #age needs to be explored further
p22 <- ggplot(complete, aes(x = brand, y = �..salary)) +
  geom_violin() #income seems to have effect on brand preference
p32 <- ggplot(complete, aes(x = brand, y = credit)) +
  geom_violin()
grid.arrange(p12, p22, p32, ncol = 3)

#plot brand preference against other categorical variables to explor further
p11 <- ggplot(complete, aes(x = brand, fill = elevel)) +
  geom_bar() +
  scale_y_continuous(breaks = seq(0, 6000, 1000)) +
  labs(title = "Brand Preference and Education Level", x = "Brand Preference", fill = "Education Level", y = "Count of Responses") +
  scale_x_discrete(labels = c("Acer", "Sony")) +
  scale_fill_discrete(labels = c("Less than High School Degree", 
                      "High School Degree",
                      "Some College",
                      "Year College Degree",
                      "Master's, Doctoral or Professional Degree"))
p12 <- ggplot(complete, aes(x = brand, fill = car)) +
  geom_bar() +
  scale_y_continuous(breaks = seq(0, 6000, 1000)) +
  labs(title = "Brand Preference and Make of Car", x = "Brand Preference", fill = "Make of Primary Car", y = "Count of Responses") +
  scale_x_discrete(labels = c("Acer", "Sony")) +
  scale_fill_discrete(labels = c("BMW","Buick", "Cadillac", "Chevrolet", "Chrysler",
                                 "Dodge", "Ford", "Honda", "Hyundai", "Jeep", "Kia", 
                                 "Lincoln", "Mazda", "Mercedes Benz", "Mitsubishi", 
                                 "Nissan", "Ram", "Subaru", "Toyota", "None of the 
                                  above"))
p13 <- ggplot(complete, aes(x = brand, fill = zipcode)) +
  geom_bar() +
  scale_y_continuous(breaks = seq(0, 6000, 1000)) +
  labs(title = "Brand Preference and Region", x = "Brand Preference", fill = "Region Based on Zipcode", y = "Count of Responses") +
  scale_x_discrete(labels = c("Acer", "Sony")) +
  scale_fill_discrete(labels = c("New England", "Mid-Atlantic", "East North Central", 
                                 "West North Central", "South Atlantic", "East South Central",
                                 "West South Central", "Mountain", "Pacific"))
                                 
grid.arrange(p11, p12, p13, ncol = 3)


#plot histograms with brand preference filled in by color
p22 <- ggplot(complete, aes(x=�..salary, fill=brand)) +
      geom_histogram(binwidth = 10000, color = "black") +
      scale_x_continuous(breaks = seq(20000, 160000, 20000)) +
      scale_fill_discrete(labels = c("Acer", "Sony")) +
      labs(x = "Salary", y= "Count of Responses", title = "Salary and Brand Preference")
p23 <- ggplot(complete, aes(x=age, fill=brand)) +
      geom_histogram(binwidth = 5, color = "black") +
      scale_x_continuous(breaks = seq(20, 80, 5)) +
      scale_fill_discrete(labels = c("Acer", "Sony"))
p24 <- ggplot(complete, aes(x=credit, fill=brand)) +
      geom_histogram(binwidth = 10000, color = "black") +
      scale_x_continuous(breaks = seq(0, 500000, 50000)) +
      scale_fill_discrete(labels = c("Acer", "Sony")) +
      labs(x = "Available Credit", y= "Count of Responses", title = "Credit and Brand Preference")
grid.arrange(p22, p24, ncol = 1)

#plot barcharts with brand preference filled in by color
p31 <- ggplot(complete, aes(x=elevel, fill=brand)) +
  geom_bar()
p32 <- ggplot(complete, aes(x=car, fill=brand)) +
  geom_bar()
p33 <- ggplot(complete, aes(x=zipcode, fill=brand)) +
  geom_bar()
grid.arrange(p31, p32, p33, ncol = 1)

#plotting age and salary by brand preference
ggplot(complete, aes(x=age, y=�..salary, color = brand)) +
  geom_point() +
  labs(title = "Age, Salary, and Actual Brand Preference", color = "Brand Preference", x = "Age", y = "Salary") +
  scale_color_discrete(labels = c("Acer", "Sony"))

ggplot(complete, aes(x=elevel, y=�..salary, color = brand)) +
  geom_point()

#create decision tree plot of variables
dTree <- rpart(brand ~ ., data = complete, cp = .02)
rpart.plot(dTree, cex = .85)
printcp(dTree) #prints the tree metrics. We can choose Cp by analyzing the cross 
#validation error. For every split we expect the validation error to reduce, but if 
#the model suffers from overfitting the cross validation error increases or shows 
#negligible improvement We can rebuild the tree with updated cp 
#printcp(tree) shows the Training error , cross validation error and standard 
#deviation at each node.
plotcp(dTree) #plots the cp 
