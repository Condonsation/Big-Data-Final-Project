##PARTITIONING with stratified random sampling##

##Creating training and testing data
library(caret) #data partioning library and other machine learning tools
set.seed(1234)
trainIndex <- createDataPartition(credit_risk_df$loan_status, p = .7,
                                  list = FALSE,
                                  times = 1)
Train <- credit_risk_df[ trainIndex,]
Valid <- credit_risk_df[-trainIndex,]

write.csv(Train, 'credit_risk_dataset(Training Set).csv', row.names = FALSE)
write.csv(Valid, 'credit_risk_dataset(Testing Set).csv', row.names = FALSE)

##Test linear regression with all vars. No split yet
TrainModel1 <- lm(log(person_income) ~. -cb_person_default_on_file -cb_person_cred_hist_length -loan_percent_income -loan_int_rate, data = Train, na.action = na.omit)
summary(TrainModel1)
ValidModel1 <- lm(log(person_income) ~. -cb_person_default_on_file -cb_person_cred_hist_length -loan_percent_income -loan_int_rate, data = Valid, na.action = na.omit)
summary(ValidModel1)

##Linear regression with age as ind. var
TrainModel2 <- lm(scale(person_age) ~. -cb_person_default_on_file -person_income + log(person_income) -loan_percent_income -loan_amnt, data = Train, na.action = na.omit)
summary(TrainModel2)
ValidModel2 <- lm(scale(person_age) ~. -cb_person_default_on_file, data = Valid, na.action = na.omit)
summary(ValidModel2)

####
##First Logistic Regression with all variables
M_LOG<-glm(loan_status ~. -loan_percent_income -person_income -cb_person_default_on_file -cb_person_cred_hist_length -person_age +log(person_income), data = Train, family = "binomial", na.action = na.omit)
summary(M_LOG)
exp(cbind(M_LOG$coefficients, confint(M_LOG)))

##out-sample summary statistics
confusionMatrix(table(predict(M_LOG, Valid, type="response") >= .5,
                      Valid$loan_status == 1), positive='TRUE')

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
  overall.accuracy <- cm$overall['Accuracy']
  print(overall.accuracy)
  p <- p + .1
}


##Second logistic Regression
M_LOG2<-glm(loan_status ~. -loan_percent_income, data = Train, family = "binomial", na.action = na.omit)
summary(M_LOG2)
exp(cbind(M_LOG$coefficients, confint(M_LOG2)))
confusionMatrix(table(predict(M_LOG2, Valid, type="response") >= .1,
                      Valid$loan_status == 1), positive='TRUE')

TruePos <- list()
TrueNeg <- list()
p = .1
while (p < 1){
  cm2 <- confusionMatrix(table(predict(M_LOG2, Valid, type="response") >= p,
                              Valid$loan_status == 1), positive='TRUE')
  byclass.minTruePos <- cm2$byClass['Sensitivity']
  byclass.minFalsePos <- cm2$byClass['Neg Pred Value']
  TruePos <- append(TruePos,byclass.minTruePos) 
  TrueNeg <- append(TrueNeg,byclass.minFalsePos) 
  print(byclass.minFalsePos)
  p <- p + .1
}

##Attempt at ROC plot
plot(TrueNeg,TruePos, ylim = 0:1, xlim = 0:1)




