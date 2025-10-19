rm(list = ls())
Phish <- read.csv("PhishingData.csv")
set.seed(32722303) # Your Student ID is the random seed
L <- as.data.frame(c(1:50))
L <- L[sample(nrow(L), 10, replace = FALSE),]
Phish <- Phish[(Phish$A01 %in% L),]
PD <- Phish[sample(nrow(Phish), 2000, replace = FALSE),] # sample of 2000 rows

# Q1
# Count the number of phishing sites (label 0)
phishing_count <- sum(PD$Class== 1)

# Count the number of legitimate sites (label 1)
legitimate_count <- sum(PD$Class == 0)

# Calculate the proportion
proportion <- phishing_count / legitimate_count

# Print the proportion
print(proportion)

# Do other summary() etc
# Count the number of rows with any NA values
na_row_count <- sum(apply(PD, 1, function(row) any(is.na(row))))

# Print the result
print(na_row_count)

# over 20% of the data have NA value

# Omit rows with any NA values
PD_clean <- na.omit(PD)

# # Create a new variable that represents 0 as "phishing" and 1 as "legitimate"
# PD_clean$ClassLabel <- ifelse(PD_clean$Class == 1, "legitimate", "phishing")

# Q3

# Ensure Class is treated as a factor for classification
PD_clean$Class <- as.factor(PD_clean$Class)

set.seed(32722303) #Student ID as random seed
train.row = sample(1:nrow(PD_clean), 0.7*nrow(PD_clean))
PD_clean.train = PD_clean[train.row,]
PD_clean.test = PD_clean[-train.row,]

# Q4
#Decision Tree
# Load necessary libraries
library(tree)
library(ROCR)

# Calculate a decision tree DONE
PD_clean.tree = tree(Class ~., data = PD_clean.train)
summary(PD_clean.tree)
plot(PD_clean.tree)
text(PD_clean.tree, pretty = 0)
# do predictions as classes and draw a table
PD_clean.predtree = predict(PD_clean.tree, PD_clean.test, type = "class")
t1=table(Predicted_Class = PD_clean.predtree, Actual_Class = PD_clean.test$Class)
cat("\n#Decision Tree Confusion\n")
print(t1)
# Calculate accuracy
tree_accuracy <- sum(diag(t1)) / sum(t1)
print(paste("Accuracy:", round(tree_accuracy, 4)))

# do predictions as probabilities and draw ROC
PD_clean.pred.tree = predict(PD_clean.tree, PD_clean.test, type = "vector")
# computing a simple ROC curve (x-axis: fpr, y-axis: tpr)
# labels are actual values, predictors are probability of class
PD_cleanDpred <- prediction(PD_clean.pred.tree[,2], PD_clean.test$Class)
PD_cleanDperf <- performance(PD_cleanDpred,"tpr","fpr")
PD_cleanDAUC <- performance(PD_cleanDpred, "auc")@y.values[[1]]
plot(PD_cleanDperf)
abline(0,1)

#Naive Bayes DONE
library(e1071)
# Calculate naive bayes
PD_clean.bayes = naiveBayes(Class ~. , data = PD_clean.train)
PD_clean.predbayes = predict(PD_clean.bayes, PD_clean.test)
t2=table(Predicted_Class = PD_clean.predbayes, Actual_Class = PD_clean.test$Class)
cat("\n#NaiveBayes Confusion\n")
print(t2)
# Calculate accuracy
bayes_accuracy <- sum(diag(t2)) / sum(t2)
print(paste("Accuracy:", round(bayes_accuracy, 4)))

# outputs as confidence levels
PD_clean.pred.bayes = predict(PD_clean.bayes, PD_clean.test, type = 'raw')
PD_cleanBpred <- prediction( PD_clean.pred.bayes[,2], PD_clean.test$Class)
PD_cleanBperf <- performance(PD_cleanBpred,"tpr","fpr")
PD_cleanBAUC <- performance(PD_cleanBpred, "auc")@y.values[[1]]
plot(PD_cleanBperf, add=TRUE, col = "blueviolet")

# Bagging DONE
library(adabag)
PD_clean.bag <- bagging(Class ~. , data = PD_clean.train, mfinal=5)
PD_cleanpred.bag <- predict.bagging(PD_clean.bag, PD_clean.test)
cat("\n#Bagging Confusion\n")
print(PD_cleanpred.bag$confusion)
# Calculate accuracy
confusion_matrix <- PD_cleanpred.bag$confusion
bag_accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste("Bagging Accuracy:", round(bag_accuracy, 4)))

# JCpred.bag
PD_cleanBagpred <- prediction(PD_cleanpred.bag$prob[,2], PD_clean.test$Class)
PD_cleanBagperf <- performance(PD_cleanBagpred,"tpr","fpr")
PD_cleanBagAUC <- performance(PD_cleanBagpred, "auc")@y.values[[1]]
plot(PD_cleanBagperf, add=TRUE, col = "blue")

#Boosting DONE
PD_clean.Boost <- boosting(Class ~. , data = PD_clean.train, mfinal=10)
PD_cleanpred.boost <- predict.boosting(PD_clean.Boost, newdata=PD_clean.test)
cat("\n#Boosting Confusion\n")
print(PD_cleanpred.boost$confusion)
# Calculate accuracy
confusion_matrix <- PD_cleanpred.boost$confusion
boost_accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste("Boosting Accuracy:", round(boost_accuracy, 4)))

# JCpred.boost
PD_cleanBoostpred <- prediction( PD_cleanpred.boost$prob[,2], PD_clean.test$Class)
PD_cleanBoostperf <- performance(PD_cleanBoostpred,"tpr","fpr")
PD_cleanBoostAUC <- performance(PD_cleanBoostpred, "auc")@y.values[[1]]
plot(PD_cleanBoostperf, add=TRUE, col = "red")


# Random Forest DONE
library(randomForest)
PD_clean.rf <- randomForest(Class ~. , data = PD_clean.train, na.action = na.exclude)
PD_cleanpredrf <- predict(PD_clean.rf, PD_clean.test)
t3=table(Predicted_Class = PD_cleanpredrf, Actual_Class = PD_clean.test$Class)
cat("\n#Random Forest Confusion\n")
print(t3)
# Calculate accuracy
rf_accuracy <- sum(diag(t3)) / sum(t3)
print(paste("Bagging Accuracy:", round(rf_accuracy, 4)))


PD_cleanpred.rf <- predict(PD_clean.rf, PD_clean.test, type="prob")
# JCpred.rf
PD_cleanFpred <- prediction( PD_cleanpred.rf[,2], PD_clean.test$Class)
PD_cleanFperf <- performance(PD_cleanFpred,"tpr","fpr")
PD_cleanFAUC <- performance(PD_cleanFpred, "auc")@y.values[[1]]

plot(PD_cleanFperf, add=TRUE, col = "darkgreen")


#Attribute importance
cat("\n#Decision Tree Attribute Importance\n")
print(summary(PD_clean.tree))
cat("\n#Bagging Attribute Importance\n")
print(PD_clean.bag$importance)
cat("\n#Boosting Attribute Importance\n")
print(PD_clean.Boost$importance)
cat("\n#Random Forest Attribute Importance\n")
print(PD_clean.rf$importance)

results <- data.frame(
  Classifier = c("Decision Tree", "Naive Bayes", "Bagging", "Boosting", "Random Forest"),
  Accuracy = c(tree_accuracy, bayes_accuracy, bag_accuracy, boost_accuracy, rf_accuracy),
  AUC = c(PD_cleanDAUC, PD_cleanBAUC, PD_cleanBagAUC, PD_cleanBoostAUC, PD_cleanFAUC)
)

print(results)

# Q9

# Extract the relevant attribute from the test dataset
test_features <- PD_clean.test[, "A01", drop = FALSE]

# Set threshold value for the attribute A01
threshold_A01 <- 30  # Adjust this threshold based on experimentation

# Define a simple classifier rule based on A01
classify_website <- function(data) {
  predictions <- ifelse(data$A01 > threshold_A01, 1, 0)  # 1 for phishing, 0 for legitimate
  return(predictions)
}

# Apply the classifier to the test data
predicted_labels <- classify_website(test_features)

# Assuming PD_clean.test$Class contains the actual labels
actual_labels <- PD_clean.test$Class

# Generate predicted probabilities for the ROC curve
# Here, we assume a simplistic approach where:
# - Predictions of phishing (1) are assigned a probability of 0.9
# - Predictions of legitimate (0) are assigned a probability of 0.1
predicted_probabilities <- ifelse(predicted_labels == 1, 0.9, 0.1)

# Calculate the ROC curve
pred <- prediction(predicted_probabilities, actual_labels)
perf <- performance(pred, "tpr", "fpr")

# Plot the ROC curve
plot(perf, col = "blue", main = "ROC Curve for Simple Classifier Using A01")
abline(a = 0, b = 1, col = "red", lty = 2)

# Calculate the AUC
auc <- performance(pred, "auc")
auc_value <- as.numeric(auc@y.values)
cat("AUC:", auc_value, "\n")

# Calculate accuracy
accuracy <- mean(predicted_labels == actual_labels)
cat("Accuracy:", accuracy, "\n")

# Question 10
library(caret)
set.seed(9999)
train_control <- trainControl(method = "cv", number = 10)

# Define grid of hyperparameters to search over
tune_grid <- expand.grid(mtry = c(2, 4, 6, 8, 10))

# Train Random Forest model with cross-validation
rf_model <- train(Class ~ ., 
                  data = PD_clean.train, 
                  method = "rf",
                  trControl = train_control,
                  tuneGrid = tune_grid)

# Print cross-validation results
print(rf_model)

# Get best Random Forest model from cross-validation
best_rf_model <- rf_model$finalModel

# Evaluate model performance on test set (if available)
rf_predictions <- predict(best_rf_model, newdata = PD_clean.test)
confusion_matrix <- confusionMatrix(rf_predictions, PD_clean.test$Class)
print(confusion_matrix)
# Calculate probabilities
rf_probabilities <- predict(best_rf_model, newdata = PD_clean.test, type = "prob")

# Create prediction object
rf_pred <- prediction(rf_probabilities[, 2], PD_clean.test$Class)

# Calculate performance measures
rf_perf <- performance(rf_pred, "tpr", "fpr")
rf_auc <- performance(rf_pred, "auc")
auc_value <- rf_auc@y.values[[1]]
# Print AUC
print(paste("AUC:", auc_value))

print(t3)
rf_accuracy <- sum(diag(t3)) / sum(t3)
print(rf_accuracy)
PD_cleanpred.rf <- predict(PD_clean.rf, PD_clean.test, type="prob")
# JCpred.rf
PD_cleanFpred <- prediction( PD_cleanpred.rf[,2], PD_clean.test$Class)
PD_cleanFperf <- performance(PD_cleanFpred,"tpr","fpr")
PD_cleanFAUC <- performance(PD_cleanFpred, "auc")@y.values[[1]]
print(PD_cleanFAUC)


# Extract the importance of attributes
importance <- best_rf_model$importance

# Print the importance of attributes
print(importance)

num_trees <- best_rf_model$ntree
print(num_trees)

print(best_rf_model$forest[[1]])

# Question 11
# Load necessary libraries
library(neuralnet)
library(caret)
library(ROCR)

# Split the data into training and test sets
#set.seed(9999)  # Ensuring reproducibility
train_indices <- sample(1:nrow(PD_clean), 0.8 * nrow(PD_clean))
PD_clean.train <- PD_clean[train_indices, ]
PD_clean.test <- PD_clean[-train_indices, ]

# Normalize the feature columns in the training and test sets
selected_features <- c("A01", "A18", "A22", "A23")
PD_clean.train[selected_features] <- scale(PD_clean.train[selected_features])
PD_clean.test[selected_features] <- scale(PD_clean.test[selected_features])

# Build and train ANN model
# Train the neural network
PD_clean.nn <- neuralnet(Class ~ A01 + A18 + A22 + A23, 
                         data = PD_clean.train,
                         hidden = 3)

# Predict probabilities for class 1 (phishing)
PD_clean.pred <- compute(PD_clean.nn, PD_clean.test[selected_features])$net.result

# Convert probabilities to binary predictions based on threshold of 0.5
binary_predictions <- ifelse(PD_clean.pred > 0.5, 1, 0)

# Confusion matrix
confusion_matrix <- table(observed = PD_clean.test$Class, predicted = binary_predictions)
print(confusion_matrix)

# Convert confusion matrix to numeric matrix for calculation
confusion_matrix <- as.matrix(confusion_matrix)

# Calculate accuracy
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste("Accuracy:", round(accuracy, 4)))

# Calculate AUC
pred_obj <- ROCR::prediction(PD_clean.pred, PD_clean.test$Class)
perf <- performance(pred_obj, "tpr", "fpr")
auc <- performance(pred_obj, "auc")@y.values[[1]]
print(paste("AUC:", round(auc, 4)))

# Plot ROC curve
plot(perf, main = "ROC Curve for ANN", col = "blue")
abline(0, 1, col = "red", lty = 2)

# Question 12
# Load necessary libraries
library(e1071)
library(ROCR)

PD_clean$Class <- as.factor(PD_clean$Class)

# Split the data into training and test sets
train_indices <- sample(1:nrow(PD_clean), 0.8 * nrow(PD_clean))
PD_clean.train <- PD_clean[train_indices, ]
PD_clean.test <- PD_clean[-train_indices, ]

# Train the Support Vector Machine model
svm_model <- svm(Class ~ A01 + A18 + A22 + A23, 
                 data = PD_clean.train,
                 kernel = "radial",  # Radial basis function kernel (Gaussian)
                 gamma = 0.1,        # Kernel parameter
                 cost = 10)          # Cost parameter for regularization

# Make predictions on the test set
svm_pred <- predict(svm_model, PD_clean.test)

# Calculate accuracy
accuracy <- mean(svm_pred == PD_clean.test$Class)
print(paste("Accuracy:", round(accuracy, 4)))

# Create confusion matrix
confusion_matrix <- table(observed = PD_clean.test$Class, predicted = svm_pred)
print("Confusion Matrix:")
print(confusion_matrix)

# Create prediction object for ROC curve
pred_obj <- prediction(as.numeric(svm_pred), as.numeric(PD_clean.test$Class))

# Calculate ROC curve and AUC
perf <- performance(pred_obj, "tpr", "fpr")
auc <- performance(pred_obj, "auc")@y.values[[1]]
print(paste("AUC:", round(auc, 4)))

# Plot ROC curve
plot(perf, main = "ROC Curve for SVM", col = "blue")
abline(0, 1, col = "red", lty = 2)