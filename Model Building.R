##PARTITIONING with stratified random sampling##

##Creating training and testing data with imbalance. 70/15/15 split
library(caret) #data partioning library and other machine learning tools
set.seed(1234)
trainIndex <- createDataPartition(credit_risk_df$loan_status, p = .7,
                                  list = FALSE,
                                  times = 1)
Train <- credit_risk_df[ trainIndex,]
TestValid <- credit_risk_df[-trainIndex,]
set.seed(1234)
trainIndex2 <- createDataPartition(TestValid$loan_status, p = .5,
                                  list = FALSE,
                                  times = 1)
Test <- TestValid[ trainIndex2,]
Valid <- TestValid[-trainIndex2,]

write.csv(Train, 'credit_risk_dataset(Training Set).csv', row.names = FALSE)
write.csv(Test, 'credit_risk_dataset(Testing Set).csv', row.names = FALSE)
write.csv(Valid, 'credit_risk_dataset(Validating Set).csv', row.names = FALSE)


##Creating training and testing data with BALANCED loan_status var for later testing if necessary
fiftydf <- sample_n(subset(credit_risk_df, loan_status == 0), 7108)##DIPLR function
fiftydf2 <- rbind(subset(credit_risk_df, loan_status == 1), fiftydf)

set.seed(1234)
trainIndex2 <- createDataPartition(fiftydf2$loan_status, p = .7,
                                  list = FALSE,
                                  times = 1)
Train2 <- fiftydf2[ trainIndex2,]
Valid2 <- fiftydf2[-trainIndex2,]

##Linear regression modeling
##Steps:
# Remove loan_percent_income
# Remove loan_int_rate
# Run
# Remove cb_person_cred_hist_length
# Run
# Remove cb_person_default_on_file
# Run
# subset data to remove C level from loan grade
# Run
# Subset data to remove Other from person_home_ownership

# first OLS model creation
TrainModel1 <- lm(log(person_income) ~.-loan_int_rate -cb_person_default_on_file -cb_person_cred_hist_length -loan_percent_income -log_person_income, data = Train, na.action = na.omit)
summary(TrainModel1)
# VIF check for multicollinearity
car::vif(TrainModel1) 
plot(TrainModel1$residuals)
abline(0,0,col='black')
hist(TrainModel1$residuals)
summary(TrainModel1$residuals)
## Run with Test set
TestModel1 <- lm(log(person_income) ~.-loan_int_rate -cb_person_default_on_file -cb_person_cred_hist_length -loan_percent_income -log_person_income, data = Test, na.action = na.omit)
summary(TestModel1)

# Test if residuals are normally distributed using jaques-Bera test
library(tseries)
jarque.bera.test(TrainModel1$residuals) #null hypothesis: data is distribution is normal. Reject Null

# second OLS model
TrainModel2 <- lm(log(person_income) ~.-loan_int_rate -cb_person_default_on_file -cb_person_cred_hist_length -loan_percent_income -log_person_income, data = subset(Train, person_home_ownership != "OTHER"), na.action = na.omit)
summary(TrainModel2)
# VIF check for multicollinearity
car::vif(TrainModel2) ##all under 10
# Check residuals
plot(TrainModel2$residuals)
abline(0,0,col='black')
hist(TrainModel2$residuals)
summary(TrainModel2$residuals)
## Run with Test set
TestModel2 <- lm(log(person_income) ~.-loan_int_rate -cb_person_default_on_file -cb_person_cred_hist_length -loan_percent_income -log_person_income, data = subset(Test, person_home_ownership != "OTHER"), na.action = na.omit)
summary(TestModel2)

# third OLS model
TrainModel3 <- lm(log(person_income) ~.-loan_int_rate -cb_person_default_on_file -cb_person_cred_hist_length -loan_percent_income -log_person_income, data = subset(Train, loan_grade != "C"), na.action = na.omit)
summary(TrainModel3)
# VIF check for multicollinearity
car::vif(TrainModel3) ##all under 10
# Check residuals
plot(TrainModel3$residuals)
abline(0,0,col='black')
hist(TrainModel3$residuals)
summary(TrainModel3$residuals)
## Run with Test set
TestModel3 <- lm(log(person_income) ~.-loan_int_rate -cb_person_default_on_file -cb_person_cred_hist_length -loan_percent_income -log_person_income, data = subset(Test, loan_grade != "C"), na.action = na.omit)
summary(TestModel3)
## Run with Validation set
ValidModel3 <- lm(log(person_income) ~.-loan_int_rate -cb_person_default_on_file -cb_person_cred_hist_length -loan_percent_income -log_person_income, data = subset(Valid, loan_grade != "C"), na.action = na.omit)
summary(ValidModel3)

# fourth OLS model
TrainModel4 <- lm(log(person_income) ~.-loan_int_rate -cb_person_default_on_file -cb_person_cred_hist_length -loan_percent_income -log_person_income, data = subset(Train, loan_grade != "C" & person_home_ownership != "OTHER"), na.action = na.omit)
summary(TrainModel4)
# VIF check for multicollinearity
car::vif(TrainModel4) ##all under 10
# Check residuals
plot(TrainModel4$residuals)
abline(0,0,col='black')
hist(TrainModel4$residuals)
summary(TrainModel4$residuals)

# Test if residuals are normally distributed using jaques-Bera test
library(tseries)
jarque.bera.test(TrainModel4$residuals) #null hypothesis: data is distribution is normal
par(mfrow=c(2,2)) # init 4 charts in 1 panel
plot(TrainModel4) ## Check to heteroskedasticity 
## Run with Test set
TestModel4 <- lm(log(person_income) ~.-loan_int_rate -cb_person_default_on_file -cb_person_cred_hist_length -loan_percent_income -log_person_income, data = subset(Test, loan_grade != "C" & person_home_ownership != "OTHER"), na.action = na.omit)
summary(TestModel4)

##RMSE in one line. Same value from
sqrt(mean(TestModel4$residuals^2))

####Classification task
##First Logistic Regression
M_LOG<-glm(loan_status ~.-person_income -cb_person_cred_hist_length -cb_person_default_on_file -person_age -loan_amnt, data = Train, family = "binomial", na.action = na.omit)
summary(M_LOG)
exp(cbind(M_LOG$coefficients, confint(M_LOG)))
##Loop through threshhold values and print out accuracy of each
p = .1
while (p < 1){
  cm <- confusionMatrix(table(predict(M_LOG, Train, type="response") >= p,
                              Train$loan_status == 1), positive='TRUE')
  overall.accuracy <- cm$overall['Accuracy']
  print(overall.accuracy)
  p <- p + .1
}
## p = .5 has the highest accuarcy

##in-sample summary statistics
confusionMatrix(table(predict(M_LOG, Train, type="response") >= .5,
                      Train$loan_status == 1), positive='TRUE')

##out-sample summary statistics for M_LOG
confusionMatrix(table(predict(M_LOG, Test, type="response") >= .5,
                      Test$loan_status == 1), positive='TRUE')

## Second logistic regression model
CR_LOG1 <- glm(loan_status ~ .-person_income, data = Train, family = "binomial")
summary(CR_LOG1)
exp(cbind(CR_LOG1$coefficients, confint(CR_LOG1)))

#in sample error#
caret::confusionMatrix(table(predict(CR_LOG1, Train, type="response") >= .5,
                             Train$loan_status == 1), positive='TRUE')
#out of sample error#
caret::confusionMatrix(table(predict(CR_LOG1, Test, type="response") >= 0.5,
                             Test$loan_status == 1), positive = 'TRUE')
##Attempt at ROC plot
# plot(OneMinusSpecificityFalsePositive,SensitivityTruePos)

##Addition models: CART and SVM

#####################
########CART#########
#####################

##WORKING CART##
##Test on Train and Valid
library(rpart)
modelcart <- rpart(loan_status ~., data = Train)
par(xpd = NA) # otherwise on some devices the text is clipped
plot(modelcart)
text(modelcart, digits = 3)
##in-sample output table
confusionMatrix(modelcart %>% predict(Train, "class"), Train$loan_status, positive='1')
##Test set out-sample output table
confusionMatrix(modelcart %>% predict(Test, "class"), Test$loan_status, positive='1')
##Validation set out-sample output table
confusionMatrix(modelcart %>% predict(Valid2, "class"), Valid2$loan_status, positive='1')


##Better graph of CART
library(rpart)
install.packages("RColorBrewer")
library(rpart.plot)
rpart.plot(modelcart, space =0, tweak = 2)

# SVM code chunk from Dan:
library(e1071)
Train$loan_status<-factor(Train$loan_status)
Test$loan_status<-factor(Test$loan_status)
TestnoNA<-na.omit(Test)
TrainnoNA<-na.omit(Train)
SVM0<-svm(loan_status~., data = Train)
##in-sample output table
confusionMatrix(predict(SVM0, TrainnoNA), TrainnoNA$loan_status, positive= '1')
##out-sample output table
confusionMatrix(predict(SVM0, TestnoNA), TestnoNA$loan_status, positive= '1')
