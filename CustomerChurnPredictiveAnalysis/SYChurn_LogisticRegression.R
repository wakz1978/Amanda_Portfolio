# install.packages("DescTools") 
# install.packages("reghelper")
library(DescTools)
library("ResourceSelection")
library(car)
library("reghelper") 

#------------------------------------------------------------------- 
# Import data 
# SY has run a cold calling campaign a month ago. 
# From this campaign you are provided a rich dataset that contains 
# information about a random set of existing SY clients (sample size: 4140). 
#------------------------------------------------------------------- 

# Pull data from the csv file 
telecomm_df <-read.csv("Original_A3_Model_development.csv", header=TRUE)

#------------------------------------------------------------------- 
# Get an overview of the variables 
#------------------------------------------------------------------- 
names(telecomm_df)  

#------------------------------------------------------------------- 
#  Client information 
# 1. Customer ID – identification of a customer 
# 2. Gender – customer gender (binary variable: male / female) 
# 3. Senior Citizen – whether the customer is retired (binary variable: Yes=1/ No=0) 
# 4. Partner – whether the customer is married (binary variable: Yes/ No) 
# 5. Dependents – whether the customer has dependents (binary variable: Yes/ No) 

# Details about the customer’s status with SY 
# 1. Tenure – number of months a person has been a customer of the company 
# 2. Phone Service – whether the telephone service is connected (binary variable: Yes/ No) 
# 3. Contract – type of customer contract (binary variable: Long term/ Short term) 
# 4. Paperless Billing – whether the customer uses paperless billing (binary variable: Yes/No) 
# 5. Monthly Charges – current monthly payment 
# 6. Total Charges – the total amount that the client paid for the services for the entire 
# time 
#--------------------------------------------------------------------

#---------------------------------------------------------------------
# Recoding of Dependent variable (Renew) and Predictor Variables
#---------------------------------------------------------------------

# Renew: Whether the customer renews the contract (Binary variable: Yes/ No). 
# Convert it to a number Renew = No -> 0 and Renew = Yes -> 1 
telecomm_df$Renew <-ifelse(telecomm_df$Renew=="Yes",1,0)

# Recoding of Predictor variables convert to binary 0 or 1.
telecomm_df$Gender_IsMale <-ifelse(telecomm_df$gender=="Male",1,0)
telecomm_df$Partner <-ifelse(telecomm_df$Partner=="Yes",1,0)
telecomm_df$Dependents <-ifelse(telecomm_df$Dependents=="Yes",1,0)
telecomm_df$PhoneService <-ifelse(telecomm_df$PhoneService=="Yes",1,0)
telecomm_df$Contract_LT <-ifelse(telecomm_df$Contract=="Long term",1,0)
telecomm_df$PaperlessBilling <-ifelse(telecomm_df$PaperlessBilling=="Yes",1,0)

#-------------------------------------------------------------------
# Create LastCampaign with only the columns we are interested in 
# Drop the unmeaningful "CustomerID"
# Drop "gender" column and "Contract" as recoded these columns into 
# Gender_IsMale and Contract_LT 
#-------------------------------------------------------------------
LastCampaign <- telecomm_df[, !names(telecomm_df) %in% c("customerID", "Contract", "gender")]

str(LastCampaign)

# # Identify the variables that contribute to "Renew" by correlation
# cor(LastCampaign)
# write.csv((cor(LastCampaign)[,9]),"Correlation_Coefficients.csv")

# -----------------------------------------------------------
# Logistic Regression Model Development 
#-------------------------------------------------------------
# Step 1: Define research goal
#
# 1. Identify the probability of each customer to renew in the new campaign. (Renew = 1)
# 2. Customers will be identified as "Targeted" if the probability to renew is more than 0.14, else "Untargeted"
# 3. Compare the profit from the last campaign's "untargeted" approach to the expected profit from the new campaign's "targeted" approach.
#-------------------------------------------------------------

# -----------------------------------------------------------
# Step 2: Specify the model 
# -----------------------------------------------------------

# First model - logistic regression model from LastCampaign data (glm) and include all independent variables as "." to predict dependent variable "Renew"
model1<-glm(Renew ~ .,data=LastCampaign, family=binomial)

#----------------------------------------------------------
# Step 3: Model evaluation.
# First, we do local model evaluation. We check model variable significance or not & multicollinearity issue
# Second, we do global model evaluation. We check Nagelkerke R square & accuracy 
# -----------------------------------------------------------

# Local Model Evaluation - Multicollinearity Test 
# Check for VIF over 10, this indicates multicollinearity amongst variables
vif(model1)

# Check correlation of these two variables
cor(LastCampaign$tenure, LastCampaign$TotalCharges)


# Local Model Evaluation - Significance Test  
# Check significance levels of independent variables in the model
summary(model1)

#--------------------------------------------------------------------
# Model Evaluation Model 1

# Local Model Evaluation: VIFs are greater than 10 for "tenure" and "TotalCharges" means there is multicollinearity issue in model.
# Additionally "Partner" and "Dependents" have significance level of well above 0.05 as does "Gender_IsMale" or not significant in predicting chance of renewal.
# Overall this is not a good model due to VIF and non-significant independent variables
#  To address Multicollinarity we will drop "Total Charges" as lowest significance and highest VIF, we will leave tenure as is significant.
# To Address this we will remove -  "TotalCharges", "Gender_IsMale", "Partner", "Dependents"

# Global model evaluation  - We will skip for Model 1 due to high VIFs and insignificant variables that need to be fixed first
#--------------------------------------------------------------------

# Adjust to Model 2 with new set of independent variables
model2<-glm(Renew ~ SeniorCitizen+
                PhoneService+
                PaperlessBilling+
                MonthlyCharges+
                Contract_LT+
                tenure, 
            data=LastCampaign, family=binomial)

#-----------------------------------------------------------
# Step 3: Model evaluation: 
# First, we do local model evaluation. We check model parameter significant or not & multicollinearity issue.
# Second, we do global model evaluation. We check Nagelkerke R square & accuracy 
# -----------------------------------------------------------

# Local Model Evaluation - Multicollinearity Test 
vif(model2)


# Local Model Evaluation - Significance Test  
summary(model2)

# -----------------------------------------------------------
# Global model evaluation 
# -----------------------------------------------------------

# -----------------------------------------------------------
# Nagelkerke R square
# It is always between 0 and 1. The higher the better.
# -----------------------------------------------------------
PseudoR2(model2, which = "Nagelkerke") 

# -----------------------------------------------------------
# Global model evaluation - Prediction Accuracy
# -----------------------------------------------------------
# 

#Create a Confusion Matrix to determine prediction accuracy of the model
#Cut off 
Conf(model2, cutoff = 0.14)

# Prediction accuracy 
print(paste('Prediction Accuracy Model 2:', prediction_accuracy <- (1007+1537)/ 4140))


# # ANOVA tests whether the variance in a set of data explained by the model is significantly greater than the unexplained variance.
# # Thus, we want this test to be significant (p<.05).
# # The test proceeds in a step wise manner, adding one of the independent variables
# # in each step. We are only interested in the value in the last step.
# # Here it is significant so ok.
anova(model2, test="Chisq")

# # Hosmer Lemeshow test whether the predicted values and the actual values are significantly different.
# # "Renew" identifies the observed binary, "fitted"model" the predicted.
# # The test partitions the data into groups and compares for each one
# # whether there are differences between predictions and observations.
# # "g=10" is th edefault choice for the number of groups the test uses.
# # For a good model performance we want them to be NOT different.
# # Thus, we want the Hosmer Lemeshow Test to be INSIGNIFICANT (>.05)!
# # Here it is significant, thats not a good model.
h3<-hoslem.test(LastCampaign$Renew, fitted(model2), g=10)
h3

#-----------------------------------------------------------------
# Standardization of Model Coefficients
# For comparison of affect of independent variables on dependent "Renew" to see which variable has the strongest effect.
#-----------------------------------------------------------------
model2.std <- beta(model2) 
model2.std

# Write standardized coefficients to csv file for visualization
write.csv((coef(model2.std)[, "Estimate"]),"Standarised_Coefficients.csv")

#----------------------------------------------------------------
# Odds-Ratio Calculations
# To see how the log odds "Renew" affected by 1 unit increase in variables in model, minus the intercept
#----------------------------------------------------------------
exp(coef(model2))
exp(confint(model2))

# Write odds-ratios estimates and confidence intervals to csv file for visualisation
write.csv((cbind(exp(coef(model2)), exp(confint(model2)))), "Odds_Ratio.csv")

# #-------------------------------------------------------------------------
# # New Campaign Calculation of  Target Customers at probability of Renew <.14 = Targeted
# #-------------------------------------------------------------------------


# Read in NewCampaign Data
NewCampaign <-read.csv("A3_Prediction.csv", header=TRUE)

str(NewCampaign)

#---------------------------------------------------------------------
# Re-coding of Independent variables in NewCampaign
#
# Currently, the independent variables are character format
# To run a logistic regression, we need to convert them to binary 0 or 1
# We re code them by using if else function.
# Change Categorical values into 1 or 0 for each column in NewCampaign 
#-------------------------------------------------------------------


NewCampaign$PhoneService <-ifelse(NewCampaign$PhoneService=="Yes",1,0)
NewCampaign$Contract_LT <-ifelse(NewCampaign$Contract=="Long term",1,0)
NewCampaign$PaperlessBilling <-ifelse(NewCampaign$PaperlessBilling=="Yes",1,0)

NewCampaign <- NewCampaign[, !names(NewCampaign) %in% c('customerID','gender','Partner', 'Dependents', 'TotalCharges', 'Contract')]
                           


NewCampaign$Predicted.Renew <- predict(model2, newdata = NewCampaign, type = "response")

str(NewCampaign)
#-------------------------------------------------------------------
# Threshold Calculation Targeted vs. Untargeted for New Campaign
#
# Advertising-to-sales ratio = Total advertising expenses / Sales revenue 
#
# When a threshold is 7 (cost per customer contact) / 50 (value of retaining 
# an existing customer), 0.14 means breakeven point of not churn. If the 
# predicted probability of ‘renew’ is greater than 0.14 (or predicted 
#                                                            probability of ‘churn’ is less than 0.86), then this customer will be the 
# targeted customer.  
#-------------------------------------------------------------------

print(paste('Target Renew Threshold for Future Campaign:', 7 / 50))


# Classify customers "targeted" vs. "untargeted" based on the probabilities "of "Renew"
# If probability "Renew" < 0.14, then Target = 0
# If probability "Renew" >  0.14, then Target = 1

NewCampaign$Target.Customer <- ifelse(NewCampaign$Predicted.Renew > 0.14, 1, 0)
str(NewCampaign)
write.csv(NewCampaign, "NewCampaign Data.csv")

#---------------------------------------------
# Step 5: Predictions

# Advertising Campaign Statistics Prediction This Month
#
#---------------------------------------------

# Print Number of customers who will be targeted (Probability Renew > 0.14)
num_target_customers <- sum(NewCampaign$Target.Customer == 1)

print(paste('Number of Targeted Customers New Campaign:', num_target_customers))

# Print Ratio of customers who are targeted vs untargeted new campaign
print(paste("Target Customer Ratio New Campaign:", num_target_customers/nrow(NewCampaign)))
 

# Number of targeted customer = 1118, and they are ordered from lowest 
# probability to the highest probability of churn (all will be contacted)
targeted.probabilities <- sort(NewCampaign$Predicted.Renew, decreasing = TRUE)[1:1118]

# Predicted Revenue = Probability of Renewal of Targeted Customers × Worth for retaining an existing customer
predicted.revenue <- round(targeted.probabilities * 50, 2)

# Predicted Profit = Predicted Revenue - Predicted Cost
predicted.profits <- sum(predicted.revenue) - (num_target_customers*7)
predicted.profits

#-------------------------------------------------------------------
# Advertising Campaign Statistics Last Campaign vs. New Campaign
#-------------------------------------------------------------------

last_campaign_renewals <- sum(telecomm_df$Renew == 1)
num_customers_contacted <- nrow(telecomm_df)

print(paste('Total Cost of Advertising (Last Campaign): $', num_customers_contacted* 7))

# Calculate Predicted Cost New Campaign
print(paste('Total New Campaign Cost (Number of Targeted Customers x $7): $',num_target_customers* 7))

# Acutal Profit Last Campaign
print(paste('Total Profit of Last Campaign (Revenue - Cost): $', last_campaign_renewals*50 - num_customers_contacted* 7))

# Predicted Profit for New Campaign
print(paste('Total Predicted Profit of New Campaign (Revenue - Cost): $',sum(predicted.revenue), '- $', sum(num_target_customers * 7), ' = $',predicted.profits))

# Acutal Profit per customer Last Campaign
print(paste('Total Profit Per Customer Contacted (Last Campaign) $', round((((last_campaign_renewals * 50) - (num_customers_contacted * 7))/num_customers_contacted), 2)))

# Predicted Profit per customer New Campaign
print(paste('Total Predicted Profit of New Campaign per Contact: $', round((predicted.profits / num_target_customers), 2)))


# And for those we calculate the expected profits,
# which are $14142.45
# Expected Profit/Customer Contacted is $12.65
# Remember: In the initial sample with the untargeted campaign,
# our profit amounted to $25320 only.
# Actual Profit Per Customer Contacted was $6.12

#-----------------------------------------------------------------
# WHAT IF COST WAS $6
#---------------------------------------------------------------
print(paste('Target Renew Threshold for Future Campaign:', 6 / 50))
NewCampaign$Target.Customer6 <- ifelse(NewCampaign$Predicted.Renew > 0.12, 1, 0)

# Print Number of customers who will be targeted (Probability Renew > 0.14)
num_target_customers6 <- sum(NewCampaign$Target.Customer6 == 1)

print(paste('Number of Targeted Customers New Campaign @$6:', num_target_customers6))

# Number of targeted customer = 1118, and they are ordered from lowest 
# probability to the highest probability of churn (all will be contacted)
targeted.probabilities6 <- sort(NewCampaign$Predicted.Renew, decreasing = TRUE)[1:1184]

# Predicted Revenue = Probability of Renewal of Targeted Customers × Worth for retaining an existing customer
predicted.revenue6 <- round(targeted.probabilities6 * 50, 2)

# Predicted Profit = Predicted Revenue - Predicted Cost
predicted.profits6 <- sum(predicted.revenue6) - (num_target_customers6*6)
predicted.profits6

#-----------------------------------------------------------------
# WHAT IF COST WAS $5
#---------------------------------------------------------------
print(paste('Target Renew Threshold for Future Campaign:', 5 / 50))
NewCampaign$Target.Customer5 <- ifelse(NewCampaign$Predicted.Renew > 0.10, 1, 0)

# Print Number of customers who will be targeted (Probability Renew > 0.14)
num_target_customers5 <- sum(NewCampaign$Target.Customer5 == 1)

print(paste('Number of Targeted Customers New Campaign @$5:', num_target_customers5))

# Number of targeted customer = 1118, and they are ordered from lowest 
# probability to the highest probability of churn (all will be contacted)
targeted.probabilities5 <- sort(NewCampaign$Predicted.Renew, decreasing = TRUE)[1:1243]

# Predicted Revenue = Probability of Renewal of Targeted Customers × Worth for retaining an existing customer
predicted.revenue5 <- round(targeted.probabilities5 * 50, 2)

# Predicted Profit = Predicted Revenue - Predicted Cost
predicted.profits5 <- sum(predicted.revenue5) - (num_target_customers5*5)
predicted.profits5

