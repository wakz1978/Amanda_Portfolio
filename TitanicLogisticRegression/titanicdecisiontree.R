library(rpart)
library(rpart.plot)

training <- read.csv("train.csv")

# training$IsAlive <- ifelse(training$Survived == "Survived", 1, 0)

head(training)

model <- rpart(
  formula = Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
  data = training,
  method = "class"
)

rpart.plot(model)

model

training$Prediction <- predict(
  object = model,
  newdata = training,
  type = "class"
)

head(training)

conf_matrix <- table(training$Prediction,training$Survived)

accuracy <-  sum(diag(conf_matrix)/sum(conf_matrix))
message("Accuracy: ", accuracy)
message("Sensitivity: ", conf_matrix[2,2]/sum(conf_matrix[,2]))
message("Specificity: ", conf_matrix[1,1]/sum(conf_matrix[,1]))

test <- read.csv("test.csv")


test$Prediction <- predict(
  object = model,
  newdata = test,
  type = "class"
)

test$Survived <- test$Prediction

submission <- data.frame(PassengerId = test$PassengerId,
                         Survived = test$Survived)
write.csv(submission, "submission.csv")
# conf_matrix2 <- table(test$Prediction,test$Survived)
# conf_matrix2
# 
# accuracy <-  sum(diag(conf_matrix2)/sum(conf_matrix2))
# message("Accuracy: ", accuracy)
# message("Sensitivity: ", conf_matrix2[2,2]/sum(conf_matrix2[,2]))
# message("Specificity: ", conf_matrix2[1,1]/sum(conf_matrix2[,1]))

