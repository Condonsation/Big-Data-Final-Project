
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

M_LOG<-glm(loan_status ~., data = Train, family = "binomial")
summary(M_LOG)
exp(cbind(M_LOG$coefficients, confint(M_LOG)))
confusionMatrix(table(predict(M_LOG, Valid, type="response") >= 0.5,
                      Valid$loan_status == 1), positive='TRUE')