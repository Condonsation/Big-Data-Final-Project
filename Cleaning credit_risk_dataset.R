library(readr)
library(ggplot2)
library(dplyr)

credit_risk_df <- read_csv("credit_risk_dataset (For Cleaning).csv")
View(credit_risk_dataset_For_Cleaning_)

##Take a look at data
summary(credit_risk_df)
credit_risk_df$person_home_ownership <- as.factor(credit_risk_df$person_home_ownership)
credit_risk_df$loan_intent <- as.factor(credit_risk_df$loan_intent)
credit_risk_df$loan_grade <- as.factor(credit_risk_df$loan_grade)
credit_risk_df$cb_person_default_on_file <- as.factor(credit_risk_df$cb_person_default_on_file)
credit_risk_df$loan_status <- as.factor(credit_risk_df$loan_status)

glimpse(credit_risk_df)

# NOTES on Summary stats:
# person_age: Max = 144
# person_emp_length: Max = 123; NAs=895
# loan_int_rate: NAs = 3116
# Default Rate: 7108/(7108+25473) = 0.218164
# Total % NAs: (895 + 3116) / (32,581 * 12) = 0.01025905

ggplot(credit_risk_df, aes(person_age, loan_amnt ,color = loan_status)) + 
  geom_point()

ggplot(credit_risk_df, aes(person_age, loan_int_rate)) + 
  geom_point()

ggplot(credit_risk_df, aes(person_age, fill = loan_status)) + 
  geom_histogram(stat = "count") +
  facet_wrap(~loan_grade)
cor(credit_risk_df)  
