library(readr)
library(ggplot2)
library(dplyr)

credit_risk_df <- read_csv("credit_risk_dataset (For Cleaning).csv") ##df to be cleaned
View(credit_risk_dataset_For_Cleaning_)
credit_risk_dataset_Original <- read_csv("credit_risk_dataset (Original).csv") ##original df for plotting outliers

##Take a look at data
summary(credit_risk_df)
credit_risk_df$person_home_ownership <- as.factor(credit_risk_df$person_home_ownership)
credit_risk_df$loan_intent <- as.factor(credit_risk_df$loan_intent)
credit_risk_df$loan_grade <- as.factor(credit_risk_df$loan_grade)
credit_risk_df$cb_person_default_on_file <- as.factor(credit_risk_df$cb_person_default_on_file)
credit_risk_df$loan_status <- as.factor(credit_risk_df$loan_status)

##Add log(person_income) column to help normalize
credit_risk_df$log_person_income <- log(credit_risk_df$person_income)

##take a look at df
glimpse(credit_risk_df)

# NOTES on Summary stats:
# person_age: Max = 144
# person_emp_length: Max = 123; NAs=895
# loan_int_rate: NAs = 3116
# Default Rate: 7108/(7108+25473) = 0.218164
# Total % NAs: (895 + 3116) / (32,581 * 12) = 0.01025905
# Income variable should be normalized 

#######################################
###METHOD 2: Deletion when necessary###
#######################################
plot(credit_risk_df$person_age, credit_risk_df$person_income) ##plot shows unrealiztic outliers
View(subset(credit_risk_df, person_age > 120)) ##Five people over 120
credit_risk_df$person_age[credit_risk_df$person_age > 120]<-NA ##Make 5 obs > 120 years equal to NA
plot(credit_risk_df$person_age, credit_risk_df$person_emp_length) 
credit_risk_df$person_emp_length[credit_risk_df$person_emp_length > 120]<-NA ##Make 2 obs > 120 years equal to NA
write.csv(credit_risk_df, 'credit_risk_dataset (Cleaned).csv', row.names = FALSE) ##Save cleaned file as CSV

##Create visulizations of outliers
ggplot(credit_risk_dataset_Original, aes(person_age, person_emp_length)) + 
  geom_point(size=3) + xlab("Age") + ylab("Employement Length") + theme(
    axis.title.x = element_text(size = 16),
    axis.text.x = element_text(size = 14),
    axis.title.y = element_text(size = 16))

##Create visulizations of relationships
plot(credit_risk_df$person_age, log(credit_risk_df$person_income))
hist(log(credit_risk_df$person_income))

ggplot(credit_risk_df, aes(person_age, loan_amnt ,color = loan_status)) + 
  geom_point()

ggplot(credit_risk_df, aes(loan_percent_income, person_age ,color = loan_status)) + 
  geom_point()

ggplot(credit_risk_df, aes(loan_amnt, log(person_income) ,color = loan_status)) + 
  geom_point() + xlab("Loan Amount") + ylab("Income (logged)") + theme(
    axis.title.x = element_text(size = 16),
    axis.text.x = element_text(size = 14),
    axis.title.y = element_text(size = 16))

ggplot(credit_risk_df, aes(cb_person_cred_hist_length, person_age ,color = loan_status)) + 
  geom_point(size =2)

ggplot(credit_risk_df, aes(loan_percent_income, loan_amnt ,color = loan_status)) + 
  geom_point(size =2) ##Potential Multicollinearity issue

ggplot(credit_risk_df, aes(person_age, log_person_income ,color = loan_status)) + 
  geom_point(size =2)

ggplot(credit_risk_df, aes(log(person_income), loan_int_rate, color = loan_grade)) + 
  geom_point() ##Higher interest rates default more

ggplot(credit_risk_df, aes(person_age, loan_int_rate, color = loan_grade)) + 
  geom_point(size=2.5) + xlab("Income (logged)") + ylab("Interest Rate") + theme(
    axis.title.x = element_text(size = 16),
    axis.text.x = element_text(size = 14),
    axis.title.y = element_text(size = 16))##High correlation between int. rate and loan grade and default 

ggplot(credit_risk_df, aes(loan_grade, loan_int_rate, color = loan_status)) + 
  geom_point(size=4) + xlab("Loan Grade") + ylab("Interest Rate") + theme(
    axis.title.x = element_text(size = 16),
    axis.text.x = element_text(size = 14),
    axis.title.y = element_text(size = 16))

ggplot(credit_risk_df, aes(loan_int_rate, cb_person_default_on_file, color = loan_status)) + 
  geom_point() ##High correlation between int. rate and loan grade

ggplot(credit_risk_df, aes(loan_status, fill = person_home_ownership)) + 
  geom_histogram(stat = "count") ##High corr between rent and default

ggplot(credit_risk_df, aes(loan_percent_income, fill = loan_grade)) + 
  geom_histogram(stat = "count")

ggplot(credit_risk_df, aes(loan_status, fill = loan_grade)) + 
  geom_histogram(stat = "count")

ggplot(credit_risk_df, aes(person_income, fill = loan_grade)) + 
  geom_histogram(stat = "count")

ggplot(credit_risk_df, aes(person_age, fill = loan_status)) + 
  geom_histogram(stat = "count") +
  facet_wrap(~loan_grade)

ggplot(credit_risk_df, aes(person_home_ownership, fill = loan_grade)) + 
  geom_histogram(stat = "count") +
  facet_wrap(~loan_status) + xlab("Home Ownership Type") + ylab("Count") + theme(
    axis.title.x = element_text(size = 16),
    axis.text.x = element_text(size = 10),
    axis.title.y = element_text(size = 16))

ggplot(credit_risk_df, aes(loan_intent, fill = loan_grade)) + 
  geom_histogram(stat = "count") +
  facet_wrap(~loan_status) + xlab("Loan Intent") + ylab("Count") + theme(
    axis.title.x = element_text(size = 16),
    axis.text.x = element_text(size = 10),
    axis.title.y = element_text(size = 16))

ggplot(credit_risk_df, aes(loan_percent_income, fill = loan_grade)) + 
  geom_histogram(stat = "count") +
  facet_wrap(~loan_status)

##create basic correlation output to look for relationships
testcor <- na.omit(credit_risk_df)
testcor$person_home_ownership <- as.numeric(testcor$person_home_ownership)
testcor$loan_intent <- as.numeric(testcor$loan_intent)
testcor$loan_grade <- as.numeric(testcor$loan_grade)
testcor$cb_person_default_on_file <- as.numeric(testcor$cb_person_default_on_file)
testcor$loan_status <- as.numeric(testcor$loan_status)
cor(testcor)
##High correlations:
# cb_person_cred_hist_length/ Person_age = .88
# person_income/loan_amnt = .32
# loan_grade/int_rate = .93
# loan_grade/cb_person_default_on_file  = .54
# loan_amount/loan_percent_income = .58
# lona_int_rate/cb_person_default_on_file = .5
# loan_status/Loan_grade = .38
# loan_status/loan_int_rate = .34
# loan_status/%_income = .38
# %_income/loan_status = .38
# cb_person_default_on_file/loan_grade = .54

##Correlation heatmap matrix creation
library(reshape2)
cormat <- round(cor(testcor),2)
head(cormat)
melted_cormat <- melt(cormat)
head(melted_cormat)
##Tile plot of corr matrix
ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() 
##Get rid of redundant info
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}
upper_tri <- get_upper_tri(cormat)
melted_cormat <- melt(upper_tri, na.rm = TRUE)
# Heatmap
library(ggplot2)
ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()
##Reorder matrix
reorder_cormat <- function(cormat){
  # Use correlation between variables as distance
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}
# Reorder the correlation matrix
cormat <- reorder_cormat(cormat)
upper_tri <- get_upper_tri(cormat)
# Melt the correlation matrix
melted_cormat <- melt(upper_tri, na.rm = TRUE)
# Create a ggheatmap
ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()
# Print the heatmap
print(ggheatmap)
##Add correlation coefficients on the heatmap
ggheatmap + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))


