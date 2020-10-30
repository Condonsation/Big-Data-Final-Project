library(readr)
credit_risk_df <- read_csv("credit_risk_dataset (For Cleaning).csv")
View(credit_risk_dataset_For_Cleaning_)

##Take a look at data
summary(credit_risk_df)
credit_risk_df$person_home_ownership <- as.factor(credit_risk_df$person_home_ownership)
credit_risk_df$loan_intent <- as.factor(credit_risk_df$loan_intent)
credit_risk_df$loan_grade <- as.factor(credit_risk_df$loan_grade)
credit_risk_df$cb_person_default_on_file <- as.factor(credit_risk_df$cb_person_default_on_file)

# NOTES on Summary stats:
# person_age: Max = 144
# person_emp_length: Max = 123; NAs=895
# loan_int_rate: NAs = 3116
# Default Rate: 5745/(26836+5745) = 0.1763298



  