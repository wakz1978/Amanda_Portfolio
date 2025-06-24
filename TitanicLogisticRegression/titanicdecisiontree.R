# Amanda Wright 23rd June 2025 
# Titanic Survival Prediction in R with Decision Trees

# Upload libraries for decision tree
# The rpart library in R is used for building decision trees, specifically for classification and regression tasks. The name stands for Recursive Partitioning and Regression Trees.
# Here’s what it does in a nutshell:
#   - It splits your data into subsets based on the values of input variables.
# - These splits are chosen to best separate the outcome variable (either a class label or a numeric value).
# - The result is a tree-like model that’s easy to interpret and visualize.
library(rpart)
library(rpart.plot)

#Read in titanic training data
training <- read.csv("train.csv")

# Inspect dataframe 
str(training)
head(training)

# Summary statistics of training dataframe numerical columns
summary(training)

# frequency data of the columns attached
lapply(training[c("Survived", "Pclass", "Sex", "Embarked","SibSp", "Parch" )], table)

# Then find where the empty strings are
empty_counts <- sapply(training, function(x) if (is.character(x)) sum(x == "", na.rm = TRUE) else 0)

# View columns with any empty strings
empty_counts[empty_counts > 0]

# Make "" empty strings in Cabin and Embarked NA
training[c("Cabin", "Embarked")] <- lapply(training[c("Cabin", "Embarked")], function(x) ifelse(x == "", NA, x))

# Check the "" empty strings are now NA       
training[is.na(training$Cabin), ]
training[is.na(training$Embarked), ]

# Data Dictionary
# Variable	Definition	Key
# Survived	Survival	0 = No, 1 = Yes
# Pclass	Ticket class	1 = 1st, 2 = 2nd, 3 = 3rd
# Sex	Sex	
# Age	Age in years	
# SibSp	# of siblings / spouses aboard the Titanic	
# Parch	# of parents / children aboard the Titanic	
# Ticket	Ticket number	
# Fare	Passenger fare	
# Cabin	Cabin number	
# Embarked	Port of Embarkation	C = Cherbourg, Q = Queenstown, S = Southampton

# Check for missing values - there are 177 missing ages in the data
print(colSums(is.na(training)))

# Create the model - will output a decision tree based on predicting survived
# using Pclass, Sex, Age, SibSp, Parch, Fare, Embarked
model <- rpart(
  formula = Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare,
  data = training,
  method = "class"
)

# Plot the Decision Tree
rpart.plot(model)

model

#Use the model to predict the survival of the training data passengers
training$Prediction <- predict(
  object = model,
  newdata = training,
  type = "class"
)

#inspect the dataframe, should see a new column "Prediction"
head(training)

# Create a confusion matrix of the model predicted survival vs. actual survival
conf_matrix <- table(training$Prediction,training$Survived)

# Show confusion matrix and accuracy 
conf_matrix
accuracy <-  sum(diag(conf_matrix)/sum(conf_matrix))
sensitivity <- conf_matrix[2,2]/sum(conf_matrix[,2])
specificity <- conf_matrix[1,1]/sum(conf_matrix[,1])

message("Accuracy: ", accuracy)
message("Sensitivity: ", sensitivity)
message("Specificity: ", specificity)

# read in test data
test <- read.csv("test.csv")


# Use the model to predict if passengers survived, store in column "Survived"
test$Survived<- predict(
  object = model,
  newdata = test,
  type = "class"
)


# Create a dataframe of the PassengerId from the test data and the prediction
submission <- data.frame(PassengerId = test$PassengerId,
                         Survived = test$Survived)

#write file to CSV
write.csv(submission, "submission.csv", row.names = FALSE)
