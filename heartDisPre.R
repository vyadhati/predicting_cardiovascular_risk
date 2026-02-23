# Load necessary libraries
library(ggplot2)
library(gplots)
library(psych)
library(corrplot)
library(naniar)
library(caret)
library(ROCR)
library(dplyr)


# Read the CSV file
data <- read.csv("D:/Masters/POM 681/Projects/heart.csv")

# Display the first few rows of the dataframe
head(data)
str(data)
# Summarize the numerical columns
summary(data)

#Checking tht data for missing or null values
any(is.na(data))
any(is.null(data))

#Getting some visuals to check for missing or null values
colSums(is.na(data))
vis_miss(data)

# Plot target variable distribution
# Get the count of each category in the target variable
target_counts <- table(data$target)
# Create a data frame for plotting
options(repr.plot.width = 8, repr.plot.height = 6)
plot_data <- data.frame(Target = factor(names(target_counts), levels = c("0", "1")), Count = as.numeric(target_counts))

options(repr.plot.width = 8, repr.plot.height = 6)
ggplot(plot_data, aes(x = Target, y = Count, fill = Target)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Count), position = position_stack(vjust = 0.5), size = 4, color = "black") +
  labs(x = "Target", y = "Count", title = "Target Variable Distribution") +
  scale_fill_manual(values = c("salmon", "lightblue"), name = "Target") +
  theme_minimal()


# Separate categorical and continuous variables
categorical_val <- c()
continous_val <- c()

for (column in names(data)) {
  print("==============================")
  print(paste(column, ":", unique(data[[column]])))
  if (length(unique(data[[column]])) <= 10) {
    categorical_val <- c(categorical_val, column)
  } else {
    continous_val <- c(continous_val, column)
  }
}
print(categorical_val)
print(continous_val)

par(mfrow=c(3, 3))
for (i in 1:length(categorical_val)) {
  hist(data[data$target == 0, categorical_val[i]], col='blue', main=paste("Histogram of", categorical_val[i], "for Target=0"), xlab=categorical_val[i])
  hist(data[data$target == 1, categorical_val[i]], col='red', main=paste("Histogram of", categorical_val[i], "for Target=1"), xlab=categorical_val[i])
}
# Add legend outside the loop
legend("topright", legend=c("Have Heart Disease = NO", "Have Heart Disease = YES"), fill=c("blue", "red"))



# Plot histograms for continuous variables
par(mfrow=c(3, 2))
for (i in 1:length(continous_val)) {
  hist(data[data$target == 0, continous_val[i]], col='blue', main=paste("Histogram of", continous_val[i], "for Target=0"), xlab=continous_val[i])
  hist(data[data$target == 1, continous_val[i]], col='red', main=paste("Histogram of", continous_val[i], "for Target=1"), xlab=continous_val[i])
}
legend("topright", legend=c("Have Heart Disease = NO", "Have Heart Disease = YES"), fill=c("blue", "red"))

# End the par layout
par(mfrow = c(1, 1))


# Scatter plot of Age and Max Heart Rate colored by target
options(repr.plot.width = 8, repr.plot.height = 6)
plot(data$age[data$target == 1], data$thalach[data$target == 1], col='red', xlab="Age", ylab="Max Heart Rate",
     main="Heart Disease in function of Age and Max Heart Rate")
points(data$age[data$target == 0], data$thalach[data$target == 0], col='blue')
legend("topright", legend=c("Disease", "No Disease"), col=c("red", "blue"), pch=1)

# Set the plot size
options(repr.plot.width = 8, repr.plot.height = 6)
# Correlation matrix
corr_matrix <- cor(data)
corrplot(corr_matrix)

# Plot the correlation matrix
corrplot(corr_matrix, method="color", type="upper", addCoef.col="black", tl.cex=0.8,number.cex = 0.6) 

# Plot the heatmap with correlation values
# Round the correlation matrix to two decimal places
corr_matrix_rounded <- round(corr_matrix, 2)

heatmap.2(corr_matrix_rounded,
          trace="none",       # Remove trace lines
          col=rev(colorRampPalette(c("navy", "white", "firebrick3"))(100)),  # Change color palette
          dendrogram="none",  # Remove dendrogram
          key=TRUE,           # Add color key
          keysize=1.0,        # Set size of the color key
          key.title="Correlation",  # Add color key title
          symkey=FALSE,       # Show asymmetrical key
          density.info="none",  # Remove density plot
          cexRow=0.7,         # Set size of row labels
          cexCol=0.7,         # Set size of column labels
          margins=c(2, 4),  # Set margins for row and column labels
          main="Correlation Heatmap",  # Add main title
          xlab="Variables",   # Add x-axis label
          ylab="Variables",   # Add y-axis label
          cellnote=corr_matrix_rounded,  # Include correlation values as annotations
          notecol="black",    # Set color of annotations
          notecex=0.5)        # Set size of annotations


# Calculate the point-biserial correlation coefficient for each predictor variable
corr_with_target <- sapply(data[, -14], function(x) {
  cor(x, data$target)
})

# Plot the correlation with the target variable (horizontal bar plot)
options(repr.plot.width = 8, repr.plot.height = 6)
barplot(corr_with_target, 
        names.arg = names(corr_with_target),
        col =  "lightblue",
        horiz = TRUE,  # Create a horizontal bar plot
        main = "Correlation with Target Variable",
        xlab = "Correlation",
        ylab = "Predictor Variable",
        xlim = c(-1, 1),  # Set the limit for the x-axis
        las = 2,  # Rotate y-axis labels
        cex.names = 0.8,  # Adjust the size of the y-axis labels
        axes = FALSE)  # Suppress default axis labels and ticks

# Add grid lines
grid(nx = NULL, ny = 10, col = "gray", lty = "dotted")

# Add values inside the bars (closer to the bars)
text(x = corr_with_target, 
     y = 1:length(corr_with_target), 
     labels = round(corr_with_target, 2),  # Round correlation values to 2 decimal places
     pos = 4,  # Position the text inside the bars
     offset = 0.25,  # Set a smaller offset to fit closer to the bars
     cex = 0.8)  # Adjust the size of the text labels

#Coverting the taget variable to class(factor) type for better classification analysis
data$target <- as.factor(data$target)


# Scatter Plots & Correlations
#corelation between independent variable
options(repr.plot.width = 8, repr.plot.height = 6)
pairs.panels(data[,-14], # Exclude target variable
             gap=0, 
             bg=c("deepskyblue","chartreuse")[data$target],
             pch=21 # Set point character for scatter plots
)

#DATA PARTITIONING 
# Split data into training and testing sets - train (70%) & test (30%)
set.seed(123) # For reproducibility
ind <- sample(2,nrow(data), replace = T, prob = c(0.7,0.3))
#trainIndex <- createDataPartition(df$target, p = .7, list = FALSE)
train <- data[ind==1,]
test <- data[ind==2,]


###########   LOGISTIC REGRESSION   ############

# Train logistic regression model
lr_model <- glm(target ~ ., data=train, family=binomial(link = "logit"))
summary(lr_model)

# Predict on training set
train_pred <- predict(lr_model, newdata=train, type="response")
#head(train_pred)
#head(train)
#table(train_pred)

#Misclassification error - from train data 
#Convert probability to binary predictions (0 or 1)
train_pred_class <- ifelse(train_pred>0.5, 1,0)
tab1 <- table(Predicted = train_pred_class, Actual = train$target)
tab1
#Value of misclassication in training dataset 
1- sum(diag(tab1))/sum(tab1)


# Predict on testing set
test_pred <- predict(lr_model, test, type="response")

#Misclassification error - from train data 
#Convert probability to binary predictions (0 or 1)
test_pred_class <- ifelse(test_pred > 0.5, 1, 0)
tab2 <- table(Predicted = test_pred_class, Actual = test$target)
tab2
#Value of misclassication in testing dataset 
1- sum(diag(tab2))/sum(tab2)

print_score <- function(actual, predicted, train = TRUE) {
  if(train) {
    cat("Train Results:\n")
  } else {
    cat("Test Results:\n")
  }
  
  confusion <- table(actual, predicted)
  cat("Confusion Matrix:\n")
  print(confusion)
  
  accuracy <- sum(diag(confusion)) / sum(confusion)
  cat("Accuracy:", accuracy, "\n")
  
  precision <- diag(confusion) / colSums(confusion)
  cat("Precision:", precision, "\n")
  
  recall <- diag(confusion) / rowSums(confusion)
  cat("Recall:", recall, "\n")
  
  f1_score <- 2 * (precision * recall) / (precision + recall)
  cat("F1 Score:", f1_score, "\n")
}


# Print model performance
print_score(train$target, train_pred_class, train=TRUE)
print_score(test$target, test_pred_class, train=FALSE)



###########   RANDOM FOREST MODEL  ############

library(randomForest)

set.seed(222)
rf <- randomForest(target~., data=train,
                   ntree = 500,
                   mtry = 8,
                   importance = TRUE,
                   proximity = TRUE)

print(rf)
rf$mtry

# Predict & Confusion Matrix - training set
train_rf_pred <- predict(rf, train, type="response")
#head(train_pred)
#head(train)

cm_rf_train <- confusionMatrix(train_rf_pred, train$target)
print(cm_rf_train)


# Prediction & Confusion Matrix - test data
test_rf_pred <- predict(rf, test)
cm_rf_test <- confusionMatrix(test_rf_pred, test$target)
print(cm_rf_test)


#Misclassification error - from train data 
rf_tab1 <- table(Predicted = train_rf_pred, Actual = train$target)
rf_tab1
#Value of misclassication in training dataset 
1- sum(diag(rf_tab1))/sum(rf_tab1)


#Misclassification error - from test data 
rf_tab2 <- table(Predicted = test_rf_pred, Actual = test$target)
rf_tab2
#Value of misclassication in testing dataset 
1- sum(diag(rf_tab2))/sum(rf_tab2)

#Printing the Summary of model performance on training and testing data
print_score(train$target, train_rf_pred, train=TRUE)
print_score(test$target, test_rf_pred, train=FALSE)

# Error rate of Random Forest
plot(rf)

# Tune mtry
t <- tuneRF(train[,-14], train[,14],
            stepFactor = 0.5,
            plot = TRUE,
            ntreeTry = 500,
            trace = TRUE,
            improve = 0.05)

#Building the model with the tuned values
print("AFTER TUNING:")

set.seed(222)
rf_tune <- randomForest(target~., data=train,
                   ntree = 500,
                   mtry = 3,
                   importance = TRUE,
                   proximity = TRUE)

print(rf_tune)

# Predict  & Confusion Matrix - training set
train_rf_pred <- predict(rf_tune, train, type="response")
cm_rf_train <- confusionMatrix(train_rf_pred, train$target)
print(cm_rf_train)


# Prediction & Confusion Matrix - test data
test_rf_pred <- predict(rf_tune, test)
cm_rf_test <- confusionMatrix(test_rf_pred, test$target)
print(cm_rf_test)


#Misclassification error - from train data 
rf_tab1 <- table(Predicted = train_rf_pred, Actual = train$target)
rf_tab1
#Value of misclassication in training dataset 
1- sum(diag(rf_tab1))/sum(rf_tab1)
print_score(train$target, train_rf_pred, train=TRUE)

#Misclassification error - from test data 
rf_tab2 <- table(Predicted = test_rf_pred, Actual = test$target)
rf_tab2
#Value of misclassication in test dataset 
1- sum(diag(rf_tab2))/sum(rf_tab2)


#Printing the Summary of model performance on training and testing data
print_score(train$target, train_rf_pred, train=TRUE)
print_score(test$target, test_rf_pred, train=FALSE)

# Error rate of Random Forest
plot(rf_tune)

# No. of nodes for the trees
hist(treesize(rf_tune),
     main = "No. of Nodes for the Trees",
     col = "green")


# Variable Importance
varImpPlot(rf_tune)
varImpPlot(rf_tune,
           sort = T,
           n.var = 5,
           main = "Top 5 - Variable Importance")
importance(rf_tune)


# Extract Single Tree
getTree(rf_tune, 1, labelVar = TRUE)

# Multi-dimensional Scaling Plot of Proximity Matrix
# Generate a custom color palette with at least 3 colors
library(RColorBrewer)  
custom_palette <- brewer.pal(3, "Set1")
MDSplot(rf_tune, train$target, palette = custom_palette)


###########   EXTREME GRADIENT BOOSTING MODEL   ############
library(xgboost)

#Bagging Training data set
set.seed(123)
cvcontrol <- trainControl(method="repeatedcv",
                          number = 5,
                          repeats = 2,
                          allowParallel = TRUE)


#Building the gradient boosting model
set.seed(123)
xgb <- train(target ~ ., 
             data=train,
             method="xgbTree",   
             trControl=cvcontrol,
             tuneGrid = expand.grid(nrounds = 500,
                                    max_depth = 4, # effects overfitting
                                    eta = 0.28, #learning rate 
                                    gamma = 1.8,
                                    colsample_bytree = 1,
                                    min_child_weight = 1,
                                    subsample = 1))


#Variable importance 
plot(varImp(xgb))
v <- varImp(xgb)
plot(v,5)

# Predict & Confusion Matrix - training set
train_xgb_pred <- predict(xgb, train, type="raw")
cm_xgb_train <- confusionMatrix(train_xgb_pred, train$target)
print(cm_xgb_train)

#Misclassification error - from train data 
xgb_tab1 <- table(Predicted = train_xgb_pred, Actual = train$target)
xgb_tab1
#Value of misclassication in training dataset 
1- sum(diag(xgb_tab1))/sum(xgb_tab1)


# Prediction & Confusion Matrix - test data
test_xgb_pred <- predict(xgb, test, type="raw")
cm_xgb_test <- confusionMatrix(test_xgb_pred, test$target)
print(cm_xgb_test)


#Misclassification error - from test data 
xgb_tab2 <- table(Predicted = test_xgb_pred, Actual = test$target)
xgb_tab2
#Value of misclassication in test dataset 
1- sum(diag(xgb_tab2))/sum(xgb_tab2)
print_score(test$target, test_xgb_pred, train=FALSE)

#Printing the Summary of model performance on training and testing data
print_score(train$target, train_xgb_pred, train=TRUE)
print_score(test$target, test_xgb_pred, train=FALSE)


###########   EVALUATING THE 3 MODEL PERFORMANCE   ############
# Load necessary libraries
library(pROC)

options(repr.plot.width = 8, repr.plot.height = 6)

# Logistic Regression ROC
lr_probs <- predict(lr_model, test, type = "response")
lr_roc <- roc(test$target, lr_probs)
plot(lr_roc, col = "red")

# Random Forest ROC
rf_probs <- predict(rf_tune, test, type = "prob")[, 2]
rf_roc <- roc(test$target, rf_probs)
plot(rf_roc, col = "green", add = TRUE)


# Gradient Boosting ROC
# Gradient Boosting ROC
xgb_probs <- predict(xgb, test, type = "prob")[, 2]  
xgb_roc <- roc(test$target, xgb_probs)
plot(xgb_roc, col = "blue", add = TRUE)

# Add AUC values as text
text(0.8, 0.2, paste("LR AUC =", round(auc(lr_roc), 3)), adj = c(0.9, 0.1), col = "red")
text(0.6, 0.3, paste("RF AUC =", round(auc(rf_roc), 3)), adj = c(0.9, 0.1), col = "green")
text(0.4, 0.4, paste("GB AUC =", round(auc(xgb_roc), 3)), adj = c(0.9, 0.1), col = "blue")

title(main = "ROC for the Models Developed", line = 3)
# Legend
legend("bottomright", 
       legend = c("Logistic Regression", "Random Forest", "Gradient Boosting"),
       col = c("red", "green", "blue"),    # Adjust colors as per your plot
       lty = 1, cex = 0.8)


