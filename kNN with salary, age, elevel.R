
library(caret)
library(dplyr)

setwd("~/Ubiqum/Project 2/Task 2 - Predicting Brand Preference")
incomplete <- read.csv("SurveyIncomplete.csv")
complete <- read.csv("Complete_Responses.csv")

#converting data types
complete$elevel <- as.ordered(complete$elevel)
complete$car <- as.factor(complete$car)
complete$brand <- as.factor(complete$brand)
complete$zipcode <- as.factor(complete$zipcode)

#only salary, education level, and age included in analysis
complete2 <- select(complete, -(car:credit))

set.seed(998)

# define an 75%/25% train/test split of the dataset
inTraining <- createDataPartition(complete$brand, p = .75, list = FALSE)
training <- complete2[inTraining,]
testing <- complete2[-inTraining,]

#10 fold cross validation
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 10)

#train Stepwise Linear regression model
kNN_Model <- train(brand~., data = training, method = "knn", trControl=fitControl)

#predictor variables
predictors(kNN_Model)
#output
#1] "ï..salary" "age"       "elevel.L"  "elevel.Q"  "elevel.C"  "elevel^4" 

#make predictions
testPred_kNN <- predict(kNN_Model, testing)

#performace measurment
postResample(testPred_kNN, testing$brand)
#output
#Accuracy     Kappa 
#0.7022809 0.3645954 

#view kNN model
kNN_Model
#k-Nearest Neighbors 

#7501 samples
#3 predictor
#2 classes: '0', '1' 

#No pre-processing
#Resampling: Cross-Validated (10 fold, repeated 10 times) 
#Summary of sample sizes: 6751, 6751, 6751, 6751, 6752, 6750, ... 
#Resampling results across tuning parameters:
  
#  k  Accuracy   Kappa    
#5  0.7017715  0.3625212
#7  0.7054522  0.3702017
#9  0.7105584  0.3798446

#Accuracy was used to select the optimal model using the largest value.
#The final value used for the model was k = 9.

#confusion matrix
confusionMatrix(testPred_kNN, testing$brand)
#output
#Confusion Matrix and Statistics

#Reference
#Prediction    0    1
#0  564  363
#1  381 1191

#Accuracy : 0.7023          
#95% CI : (0.6839, 0.7202)
#No Information Rate : 0.6218          
#P-Value [Acc > NIR] : <2e-16          

#Kappa : 0.3646          
#Mcnemar's Test P-Value : 0.5331          
                                          
 #           Sensitivity : 0.5968          
  #          Specificity : 0.7664          
   #      Pos Pred Value : 0.6084          
    #     Neg Pred Value : 0.7576          
     #        Prevalence : 0.3782          
      #   Detection Rate : 0.2257          
   #Detection Prevalence : 0.3709          
     # Balanced Accuracy : 0.6816          
                                          
    #   'Positive' Class : 0 
