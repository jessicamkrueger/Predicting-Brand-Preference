
library(caret)
library(dplyr)
library(gridExtra)
library(rpart.plot)

setwd("~/Ubiqum/Project 2/Task 2 - Predicting Brand Preference/Original Data Sets")
incomplete <- read.csv("SurveyIncomplete.csv")
complete <- read.csv("Complete_Responses.csv")

#converting data types
complete$elevel <- as.ordered(complete$elevel)
complete$car <- as.factor(complete$car)
complete$brand <- as.factor(complete$brand)
complete$zipcode <- as.factor(complete$zipcode)

#remove attributes not needed
completeModel <- select(complete, ï..salary, age, brand)

set.seed(998)

# define an 75%/25% train/test split of the dataset
inTraining <- createDataPartition(completeModel$brand, p = .75, list = FALSE)
training <- completeModel[inTraining,]
testing <- completeModel[-inTraining,]

#10 fold cross validation
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 10)

#train kNN classification model
kNN_Model <- train(brand~., data = training, method = "rf", trControl=fitControl)

#predictor variables
predictors(kNN_Model)

#make predictions
testPred_kNN <- predict(kNN_Model, testing)

#performace measurment
postResample(testPred_kNN, testing$brand)
#output
#Accuracy     Kappa 
#0.9007603 0.7886384 

#view kNN model
kNN_Model
#output
#Random Forest 

#7501 samples
#2 predictor
#2 classes: '0', '1' 

#No pre-processing
#Resampling: Cross-Validated (10 fold, repeated 10 times) 
#Summary of sample sizes: 6751, 6751, 6751, 6751, 6752, 6750, ... 
#Resampling results:
  
 # Accuracy   Kappa    
#0.9070258  0.8024847

#Tuning parameter 'mtry' was held constant at
#a value of 2

#creating new dataframe with actual brand and predicted brand
errors <- mutate(testing, predict_brand = testPred_kNN)
#plotting predictions
p100 <- ggplot(errors, aes(x= age, y = ï..salary, color = predict_brand)) +
  geom_point()
#plotting actuals
p101 <- ggplot(errors, aes(x= age, y = ï..salary, color = brand)) +
  geom_point()

#creating new column with T/F vales for prediction 
correct <- errors$predict_brand == errors$brand
errors <- mutate(errors, correct_pred. = correct)

#plotting correction predictions
p102 <- ggplot(errors, aes(x= age, y = ï..salary, color = correct_pred.)) +
  geom_point() +
  labs(title = "Random Forest Prediction Accuracy", x = "Age", y = "Salary", color = "Prediction Accuracy") +
  scale_color_manual(labels = c("Error", "Correct"), values = c("black", "green"))
grid.arrange(p100, p101, p102, ncol = 1)

#confusion matrix of random forest
confusionMatrix(kNN_Model)
#Cross-Validated (10 fold, repeated 10 times) Confusion Matrix 

#(entries are percentual average cell counts across resamples)

#Reference
#Prediction    0    1
#0 33.3  4.7
#1  4.6 57.4

#Accuracy (average) : 0.907

#change name of column in incomplete to match complete
incomplete <- rename(incomplete, ï..salary = salary)

#apply model to incomplete survey data to make predictions
brand_prediction <- predict(kNN_Model, incomplete)

#add survey predictions to incomplete dataset
com_incomplete <- mutate(incomplete, brand = brand_prediction)

#combine dataframes into one
final_data <- rbind(complete, com_incomplete)

#summarize final data to show brand preference
summary(final_data$brand)
#output
#0    1 
#5673 9327 

#plot brand preference for final report
p122 <- ggplot(final_data, aes(brand)) +
  geom_bar(fill = c("green", "blue")) +
  ggtitle("Brand Preference: Complete Surveys and Predicted Values") +
  scale_y_continuous(breaks = seq (0, 10000, 1000)) +
  scale_x_discrete(labels = c("Acer", "Sony")) +
  labs(x= "Brand Preference", y = "Count of Responses")
  

p123 <- ggplot(complete, aes(brand)) +
  geom_bar(fill = c("green", "blue")) +
  ggtitle("Brand Preference for Complete Surveys Only") +
  scale_y_continuous(breaks = seq (0, 10000, 1000)) +
  scale_x_discrete(labels = c("Acer", "Sony")) +
  labs(x= "Brand Preference", y = "Count of Responses")

p124 <- ggplot(com_incomplete, aes(brand)) +
  geom_bar(fill = c("green", "blue")) +
  ggtitle("Brand Preference for Predicted Values Only") +
  scale_y_continuous(breaks = seq (0, 10000, 1000)) +
  scale_x_discrete(labels = c("Acer", "Sony")) +
  labs(x= "Brand Preference", y = "Count of Responses")

grid.arrange(p123, p124, ncol = 2)
