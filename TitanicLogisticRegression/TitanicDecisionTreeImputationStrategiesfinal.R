# Amanda Wright 23rd June 2025 
# Titanic Survival Prediction in R with Decision Trees

# Upload libraries for decision tree
# The rpart library in R is used for building decision trees, specifically for classification and regression tasks. The name stands for Recursive Partitioning and Regression Trees.
# Here’s what it does in a nutshell:
#   - It splits your data into subsets based on the values of input variables.
# - These splits are chosen to best separate the outcome variable (either a class label or a numeric value).
# - The result is a tree-like model that’s easy to interpret and visualize.

#install.packages("rpart")
library(rpart)
library(rpart.plot)
#install.packages("pROC")  # if not already installed
library(pROC)


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

# For the missing values in Age, try a few different imputation methods
#
# None = rpart will deal with it 
#
# Median = Assign median age for NA values in Age
#
# GroupWise = - This replaces NA values in x (the Age values for that group) with the median of the non-missing values in that group based on Pclass and Sex
#
# Mice = MICE , or Multiple Imputation by Chained Equations
# offers a robust way to preserve relationships between variables and reduce bias # compared to simpler methods like mean or median imputation.
imputation_methods <- list(
    none = function(df) df,
    
    median = function(df) {
        df$Age[is.na(df$Age)] <- median(df$Age, na.rm = TRUE)
        df
    },
    
    groupwise = function(df) {
        df$Age <- ave(df$Age, df$Pclass, df$Sex, FUN = function(x) {
            x[is.na(x)] <- median(x, na.rm = TRUE)
            return(x)
        })
        df
    },
    mice = function(df) {
        library(mice)
        imp <- mice(df, m = 1, printFlag = FALSE)
        return(complete(imp, 1))
    }
)       

# Function to create a model, based on training data, evaluate accuracy and store
# as well as comparing test data outcomes and prepare submission files
create_model <- function(training, impute_method = imputation_methods){
    
    #Assign training to df so it is fresh data each time
    df <- training
    
    # Apply the selected imputation method
    df <- imputation_methods[[impute_method]](df)
    
    # Create the model - will output a decision tree based on predicting        survived
    # using Pclass, Sex, Age, SibSp, Parch, Fare
    model <- rpart(
        formula = Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare,
        data = df,
        method = "class"
    )
    
    # Plot the Decision Tree
    rpart.plot(model)
    
    #Use the model to predict the survival of the training data passengers
    df$Prediction <- predict(
        object = model,
        newdata = df,
        type = "class"
    )

    # Create confusion matrix and accuracy metrics
    conf_matrix <- table(df$Prediction,df$Survived)
    accuracy <-  sum(diag(conf_matrix)/sum(conf_matrix))
    sensitivity <- conf_matrix[2,2]/sum(conf_matrix[,2])
    specificity <- conf_matrix[1,1]/sum(conf_matrix[,1])    
    
    # read in test data
    test <- read.csv("test.csv")
    
    # Use the model to predict if passengers survived, store in column "Survived"
    test$Survived<- predict(
        object = model,
        newdata = test,
        type = "class"
    )
    
    #Create a dataframe to return with the survival predictions, method and accuracy
    evaluation_model <- data.frame(
        SurvivalPrediction = names(table(test$Survived)),
        Freq = as.vector(table(test$Survived)),
        Method = impute_method,
        Accuracy = accuracy,
        Sensitivity = conf_matrix[2,2]/sum(conf_matrix[,2]),
        Specificity = conf_matrix[1,1]/sum(conf_matrix[,1])
    )
    
    # Create a submission dataframe of the PassengerId from the test data and the prediction
    submission <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
    
    # Create a submission file name with the impute method in the name
    submission_filename <- paste("submission", impute_method, ".csv", sep = "")
    
    #write file to CSV
    write.csv(submission, submission_filename, row.names = FALSE)
    
    #Return the results dataframe
    return(evaluation_model)
}

# Create an empty results dataframe
results <- data.frame()

# For each method, create the model using the training data, and assign results
for (method_name in names(imputation_methods)){
    model_results <- create_model(training, method_name)
    results <- rbind(results, model_results)
}

#Print out results
results

# The None Strategy actually scored the best accuracy in the Kaggle Titanic Competition, as in it predicted who survived the best on the test data at .78468