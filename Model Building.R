##PARTITIONING with stratified random sampling##

##Creating training and testing data with imbalance
library(caret) #data partioning library and other machine learning tools
set.seed(1234)
trainIndex <- createDataPartition(credit_risk_df$loan_status, p = .7,
                                  list = FALSE,
                                  times = 1)
Train <- credit_risk_df[ trainIndex,]
Valid <- credit_risk_df[-trainIndex,]

write.csv(Train, 'credit_risk_dataset(Training Set).csv', row.names = FALSE)
write.csv(Valid, 'credit_risk_dataset(Testing Set).csv', row.names = FALSE)

##Creating training and testing data with BALANCED loan_status var
fiftydf <- sample_n(subset(credit_risk_df, loan_status == 0), 7108)##DIPLR function
fiftydf2 <- rbind(subset(credit_risk_df, loan_status == 1), fiftydf)

set.seed(1234)
trainIndex2 <- createDataPartition(fiftydf2$loan_status, p = .7,
                                  list = FALSE,
                                  times = 1)
Train2 <- fiftydf2[ trainIndex2,]
Valid2 <- fiftydf2[-trainIndex2,]


##Test linear regression with all vars. No split yet
TrainModel1 <- lm(log(person_income) ~. -cb_person_cred_hist_length -cb_person_default_on_file -loan_percent_income -loan_int_rate, data = Train, na.action = na.omit)
summary(TrainModel1)
par(mfrow=c(2,2))
plot(TrainModel1)
# Test for Multicollinearity 
car::vif(TrainModel1)

##Test against Valid set
ValidModel1 <- TrainModel1 <- lm(log(person_income) ~. -cb_person_cred_hist_length -cb_person_default_on_file -loan_percent_income -loan_int_rate, data = Valid, na.action = na.omit)
summary(ValidModel1)
car::vif(ValidModel1)

##Linear regression with age as ind. var
TrainModel2 <- lm(person_age ~.-cb_person_default_on_file -loan_percent_income +log(person_income) -person_income -loan_amnt, data = Train, na.action = na.omit)
summary(TrainModel2)
##RESIDUAL ANALYSIS##
par(mfrow=c(2,2))
plot(TrainModel2)
car::vif(TrainModel2)

plot(TrainModel2$residuals)
abline(0,0,col='black')
hist(TrainModel2$residuals)
summary(TrainModel2$residuals)
sd(TrainModel2$residuals)

##Test against Valid set
ValidModel2 <- lm(person_age ~.-cb_person_default_on_file -loan_percent_income +log(person_income) -person_income -loan_amnt, data = Valid, na.action = na.omit)
summary(ValidModel2)
plot(ValidModel2$residuals)
##View predictions against actual
predictions <- TrainModel2 %>% predict(Valid)
View(cbind(Valid$person_age,predictions))


##RESIDUAL ANALYSIS##
plot(ValidModel2$residuals)
abline(0,0,col='black')
hist(ValidModel2$residuals)
summary(ValidModel2$residuals)
sd(ValidModel2$residuals)
##RMSE in one line. Same value from
sqrt(mean(ValidModel2$residuals^2))

####
##First Logistic Regression
M_LOG<-glm(loan_status ~. -loan_percent_income -person_income -cb_person_default_on_file -cb_person_cred_hist_length -person_age , data = Train, family = "binomial", na.action = na.omit)
summary(M_LOG)
exp(cbind(M_LOG$coefficients, confint(M_LOG)))

##out-sample summary statistics
confusionMatrix(table(predict(M_LOG, Valid2, type="response") >= .5,
                      Valid2$loan_status == 1), positive='TRUE')

##Model 1 in-sample summary statistics
predictions<-predict(M_LOG, Train, type="response")

#converts predictions to boolean TRUE (1) or FALSE (0) based on 1/2 threshold on output probability
binpredict <- (predictions >= .5)
View(binpredict)

#build confusion matrix based on binary prediction in-sample
confusion<-table(binpredict, Train$loan_status == 1)
confusion

#display summary analysis of confusion matrix in-sample
confusionMatrix(confusion)

##If want to add predictions to validation df
predictions<-predict(M_LOG, Valid, type="response")
ValidView <- cbind(Valid, predictions)


##Loop through threshhold values and print out accuracy of each
p = .1
while (p < 1){
  cm <- confusionMatrix(table(predict(M_LOG, Valid, type="response") >= p,
                              Valid$loan_status == 1), positive='TRUE')
  overall.Neg_Pred_Value <- cm$byClass['Neg Pred Value']
  print(overall.Neg_Pred_Value)
  p <- p + .1
}


##Second logistic Regression
M_LOG2<-glm(loan_status ~. -loan_percent_income, data = Train, family = "binomial", na.action = na.omit)
summary(M_LOG2)
exp(cbind(M_LOG2$coefficients, confint(M_LOG2)))
confusionMatrix(table(predict(M_LOG2, Valid, type="response") >= .4,
                      Valid$loan_status == 1), positive='TRUE')

SensitivityTruePos <- list()
OneMinusSpecificityFalsePositive <- list()
p = .1
while (p < 1){
  cm2 <- confusionMatrix(table(predict(M_LOG2, Valid, type="response") >= p,
                              Valid$loan_status == 1), positive='TRUE')
  byclass.minTruePos <- cm2$byClass['Sensitivity']
  byclass.minFalsePos <- (1 - cm2$byClass['Specificity'])
  SensitivityTruePos <- append(SensitivityTruePos,byclass.minTruePos) 
  OneMinusSpecificityFalsePositive <- append(OneMinusSpecificityFalsePositive,byclass.minFalsePos) 
  print(byclass.minTruePos)
  print(byclass.minFalsePos)
  p <- p + .1
}
##p = .3 has Sensitivity of 0.7049892 and 1-Specificity of 0.1297857 

##Attempt at ROC plot
plot(OneMinusSpecificityFalsePositive,SensitivityTruePos)


##Addition models: CART, Random Forest, SVM

#####################
########CART#########
#####################

#rpart package implementation **Not working**
train_control <- trainControl(method="cv", number=10, savePredictions = TRUE)
M_CART <- train(loan_status ~., data = Train, trControl=train_control, tuneLength=10, method = "rpart", na.action = na.omit) #increasing tunelength increases regularization penalty
##the "cv", number = 10 refers to 10-fold cross validation on the training data
plot(M_CART) #produces plot of cross-validation results
M_CART$bestTune #returns optimal complexity parameter
confusionMatrix(predict(M_CART, Valid), Valid$loan_status, positive='1')

##WORKING CART##
##Test on Train and Valid
library(rpart)
modelcart <- rpart(loan_status ~., data = Train)
par(xpd = NA) # otherwise on some devices the text is clipped
plot(modelcart)
text(modelcart, digits = 3)
confusionMatrix(modelcart %>% predict(Valid, "class"), Valid$loan_status, positive='1')

##Test on Train2 and Valid2
modelcart2 <- rpart(loan_status ~., data = Train2)
par(xpd = NA) # otherwise on some devices the text is clipped
plot(modelcart2)
text(modelcart2, digits = 3)
confusionMatrix(modelcart2 %>% predict(Valid2, "class"), Valid2$loan_status, positive='1')