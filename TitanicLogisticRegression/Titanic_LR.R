library(DescTools)
library("ResourceSelection")
library(car)
library("reghelper") 
# install.packages("pROC")
library(pROC)
#------------------------------------------------------------------- 
# Import data 
# Data Dictionary
# Variable	Definition	Key
# survival	Survival	0 = No, 1 = Yes
# pclass	Ticket class	1 = 1st, 2 = 2nd, 3 = 3rd
# sex	Sex	
# Age	Age in years	
# sibsp	# of siblings / spouses aboard the Titanic	
# parch	# of parents / children aboard the Titanic	
# ticket	Ticket number	
# fare	Passenger fare	
# cabin	Cabin number	
# embarked	Port of Embarkation	C = Cherbourg, Q = Queenstown, S = Southampton
#------------------------------------------------------------------- 

# Pull data from the csv file 
titanic_training <-read.csv("train.csv", header=TRUE)

#
names(titanic_training)

unique(titanic_training$Embarked)

table(titanic_training$Embarked, useNA = "ifany")

titanic_training$Embarked[titanic_training$Embarked == ""] <- NA

str(titanic_training)

colSums(is.na(titanic_training))

table(titanic_training$Embarked)

library('fastDummies')
#response should be between "0" and "1"
titanic_training <- dummy_cols(titanic_training, select_columns = c('Pclass','Sex'), remove_first_dummy = TRUE, remove_selected_columns = TRUE)

#response should be between "0" and "1"
titanic_training[] <-lapply(titanic_training, as.numeric)

str(titanic_training)

# Drop columns not needed in analysis as cannot be numeric
titanic_training <- titanic_training[,!names(titanic_training) %in% c('PassengerId','Cabin','Name', 'Embarked', 'Ticket')]

str(titanic_training)

model1 <- glm(Survived ~ ., data = titanic_training, family = "binomial")
vif(model1)

summary(model1)

model2 <- glm(Survived ~ Age + SibSp + Pclass_2 + Pclass_3 + Sex_male, data = titanic_training, family = "binomial")

vif(model2)

summary(model2)

PseudoR2(model2, which = "Nagelkerke") 

anova(model2, test="Chisq")

# Get model data
model_data <- model.frame(model2)

h3<-hoslem.test(model_data$Survived, fitted(model2), g = 10)
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
    
write.csv((cbind(exp(coef(model2)), exp(confint(model2)))), "Odds_Ratio.csv")


titanic_training_modeldata <- model.frame(model2)
titanic_training_modeldata$Predicted_survival <- predict(model2, type = "response")

roc(
    titanic_training_modeldata$Survived ~ titanic_training_modeldata$Predicted_survival,
    plot = TRUE,
    print.auc = TRUE,
    print.thres = "best"
)

titanic_training_modeldata$IsAlive <- ifelse(titanic_training_modeldata$Predicted_survival > 0.521, 1, 0)

# conf_matrix <- table(titanic_training_modeldata$IsAlive, titanic_training_modeldata$Survived)
# 
# conf_matrix
# accuracytraining <- message("Accuracy Training Data: ", sum(diag(conf_matrix))/sum(conf_matrix))
# message("Sensitivity Training: ", conf_matrix[2, 2]/sum(conf_matrix[ , 2]))
# message("Specificity Training: ", conf_matrix[1, 1]/sum(conf_matrix[ , 1]))
# Above same as conf
Conf(model2, cutoff = 0.521)

titanic_test <- read.csv("test.csv", header = TRUE)

str(titanic_test)

names(titanic_test)

#response should be between "0" and "1"
titanic_test <- dummy_cols(titanic_test, select_columns = c('Pclass','Sex'), remove_first_dummy = TRUE, remove_selected_columns = TRUE)

#response should be between "0" and "1"
titanic_test[] <-lapply(titanic_test, as.numeric)


str(titanic_test)

titanic_test$Predicted_Survival <- predict(model2, newdata = titanic_test, type = "response")


titanic_test$Survived <- ifelse(titanic_test$Predicted_Survival > 0.521, 1, 0)

Submission <- titanic_test[, c('PassengerId', 'Survived')]
write.csv(Submission, "Submission.csv")
