setwd("~/Ubiqum/Project 2/Task 2 - Predicting Brand Preference")
incomplete <- read.csv("SurveyIncomplete.csv")
complete <- read.csv("Complete_Responses.csv")

summary(complete)
summary(incomplete)

install.packages("caret", dependencies = c("Depends", "Suggests"))

#caret Pipeline Example 
#dataframe = WholeYear
#Y Value = TotalSolarRad

WholeYear <- read.csv("WholeYear.csv")
set.seed(998)

# define an 75%/25% train/test split of the dataset
inTraining <- createDataPartition(WholeYear$TotalSolarRad, p = .75, list = FALSE)
training <- WholeYear[inTraining,]
testing <- WholeYear[-inTraining,]

#10 fold cross validation
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 10)

#train Stepwise Linear regression model
LMFit1 <- train(TotalSolarRad~., data = training, method = "leapSeq", trControl=fitControl)

#predictor variables
predictors(LMFit1)

#make predictions
testPredLM1 <- predict(LMFit1, testing)

#performace measurment
postResample(testPredLM1, testing$TotalSolarRad)

#plot predicted verses actual
plot(testPredLM1,testing$TotalSolarRad)

