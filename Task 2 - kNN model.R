
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

set.seed(998)

# define an 75%/25% train/test split of the dataset
inTraining <- createDataPartition(complete$brand, p = .75, list = FALSE)
training <- complete[inTraining,]
testing <- complete[-inTraining,]

#10 fold cross validation
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 10)

#train kNN classification model
kNN_Model <- train(brand~., data = training, method = "knn", trControl=fitControl)

#predictor variables
predictors(kNN_Model)

#make predictions
testPred_kNN <- predict(kNN_Model, testing)

#performace measurment
postResample(testPred_kNN, testing$brand)
# output
#Accuracy     Kappa 
#0.6954782 0.3510223 

#view kNN model
kNN_Model

#output
#k-Nearest Neighbors 

#7501 samples
#6 predictor
#2 classes: '0', '1' 

#No pre-processing
#Resampling: Cross-Validated (10 fold, repeated 10 times) 
#Summary of sample sizes: 6751, 6751, 6751, 6751, 6752, 6750, ... 
#Resampling results across tuning parameters:
  
  #k  Accuracy   Kappa    
#5  0.6846709  0.3250716
#7  0.6898691  0.3356154
#9  0.6964686  0.3504826

#Accuracy was used to select the optimal model using the largest value.
#The final value used for the model was k = 9.

#plot predicted verses actual
plot(testPred_kNN,testing$brand)

# estimate variable importance
importance <- varImp(kNN_Model, scale=FALSE)

# summarize importance
print(importance)

# plot importance
plot(importance)
