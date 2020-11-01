
##Test linear regression with all vars. No split yet
model.omit <- lm(person_income ~., data = credit_risk_df, na.action = na.omit)
model.omit2 <- lm(log(person_income) ~person_age + cb_person_default_on_file + person_emp_length + loan_amnt + person_home_ownership + loan_status, data = credit_risk_df, na.action = na.omit)
summary(model.omit2)
model.omit3 <- lm(loan_int_rate ~loan_grade + loan_status + cb_person_default_on_file, data = credit_risk_df, na.action = na.omit)

##PARTITIONING with stratified random sampling##

##Creating training and testing data
library(caret) #data partioning library and other machine learning tools
set.seed(1234)
trainIndex <- createDataPartition(credit_risk_df$loan_status, p = .7,
                                  list = FALSE,
                                  times = 1)
Train <- credit_risk_df[ trainIndex,]
Valid <- credit_risk_df[-trainIndex,]

##First Logistic Regression with all variables
M_LOG<-glm(loan_status ~., data = Train, family = "binomial", na.action = na.omit)
summary(M_LOG)
exp(cbind(M_LOG$coefficients, confint(M_LOG)))

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
M_LOG2<-glm(loan_status ~. -cb_person_cred_hist_length -cb_person_default_on_file -person_age, data = Train, family = "binomial", na.action = na.omit)
summary(M_LOG2)
exp(cbind(M_LOG$coefficients, confint(M_LOG2)))
confusionMatrix(table(predict(M_LOG2, Valid, type="response") >= .1,
                      Valid$loan_status == 1), positive='TRUE')

p = .1
while (p < 1){
  cm2 <- confusionMatrix(table(predict(M_LOG2, Valid, type="response") >= p,
                              Valid$loan_status == 1), positive='TRUE')
  byclass.minfalsepos <- cm2$byClass['Pos Pred Value']
  print(byclass.minfalsepos)
  p <- p + .1
}


