#############Linear regression#################

#Installing the packages and libraries required
install.packages("XLConnect")
install.packages("ISLR")
library(ggplot2)
library(XLConnect)
library(caTools)
library(ISLR)
library(forecast)
library(caret)
##Import the final dataset
data <- read.csv("C:/Users/aband/Desktop/DataSciences- INFO 7390/Midterm/prud_final.csv")
## Summary of the dataset
summary(data)
data
##data not scaled and is in raw form 
str(data)

##Dividing our Dataset into Train Data & Test Data
set.seed(150)
trainingData <- sample(1:nrow(data), 0.7*nrow(data)) 
## Model training data rows and test data rows
traindata <- data[trainingData, ]  
testdata  <- data[-trainingData, ]   
##Verifying the number of rows of the training and test data
nrow(traindata)
nrow(testdata)
## Applying the correlation function to the variables
cor(data)

##Initial Multiple Linear Regression Model
lm1 <- lm(Response ~ ., data=traindata)
summary(lm1)
lm2 <- lm (Response~Product_Info_4 + Ins_Age + Ht + Wt + BMI + Family_Hist_2 + Family_Hist_4 
           + Medical_History_1 + Employment_Info_3_2 + InsuredInfo_2_2 
           +InsuredInfo_5_2 + InsuredInfo_6_2 + InsuredInfo_7_2 + Insurance_History_7_1 
           + Medical_History_4_2 + Medical_History_7_2 + Medical_History_11_1 
           + Medical_History_11_2 + Medical_History_22_2 + Medical_History_35_1 
           + Medical_History_39_1 + Medical_History_40_1 + Medical_Keyword_2_2 + 
             Medical_Keyword_3_2 + Medical_Keyword_9_2 + Medical_Keyword_15_2 + 
             Medical_Keyword_22_2 + Medical_Keyword_31_2 + Medical_Keyword_33_2    + 
             Medical_Keyword_34_2    + Medical_Keyword_38_2 + Medical_Keyword_45_2, data = traindata)
summary(lm2)
cor(lm2)

hist(lm2$response,xlab = "response",main ="Histogram of residuals")
##Normal probability plot for residuals
qqnorm(lm2$response,main="Normal Probability Plot", pch=19)
##Line on normal probability plot
qqline(lm2$response)
## The likelihood ratio test can be performed in R for both the linear models using the anova() function.
anova(lm2, lm2, test ="Chisq") 
### since the summary is better for the second model, it is considered for prediction on the test data
predict.lm <- predict(lm2, newdata = testdata)
## Sample view of the predicted model
View(as.data.frame(predict.lm))
## Determine the length of the predicted model-309
length(predict.lm)
## Provides the summary of the predicted model
summary(predict.lm)

## Using the accuracy function to determine the the various errors as shown below:
accuracy.lm <- accuracy(predict.lm, testdata$Response)
accuracy.lm
summary(accuracy.lm)
##Determing the root Mean Square error(RMSE) to evaluate the model performance
rmse.lm <- mean((Test_df$Response- predict.lm)^2)
print(rmse.lm)
rmse.lm <- mean((Train_df$Response- predict.lm)^2)
print(rmse.lm)
