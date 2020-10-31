
##Test linear regression with all vars. No split yet
model.omit <- lm(person_income ~., data = credit_risk_df, na.action = na.omit)
model.omit2 <- lm(person_income ~person_age + cb_person_default_on_file + person_emp_length + loan_amnt + person_home_ownership + loan_status, data = credit_risk_df, na.action = na.omit)
summary(model.omit3)
model.omit3 <- lm(loan_int_rate ~loan_grade + loan_status + cb_person_default_on_file, data = credit_risk_df, na.action = na.omit)
