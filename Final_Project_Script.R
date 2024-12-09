
# Final Project Script
## Luke and Sophia

library(fable)
library(forecast)
library(fpp2)
library(ggplot2)
library(fabletools)
library(datasets)
library(tidyverse)
library(fma)
library(expsmooth)
library(readr)
library(dplyr)
library(GGally)
library(caret)

ToyotaCorolla <- read_csv("ToyotaCorolla.csv")
db <- ToyotaCorolla

#EDA

# 1.
summary(db1)

# 2. Distribution plots for numerical variables
numerical_vars <- c("Age_08_04", "CC", "HP", "Quarterly_Tax", "Guarantee_Period", "Price")
numerical_vars <- c("Age_08_04", "HP", "Automatic", 
  "Doors", "Quarterly_Tax", "Mfg_Guarantee", "Guarantee_Period",
  "Airco", "Automatic_airco",
  "Sport_Model", "Tow_Bar")

for (var in numerical_vars) {
  print(ggplot(db1, aes_string(x = var)) + 
          geom_histogram(binwidth = 1, fill = "blue", color = "black", alpha = 0.7) + 
          theme_minimal() + 
          ggtitle(paste("Distribution of", var)) + 
          labs(x = var, y = "Count"))
}


outlier <- db %>% filter(Price > 28000)  # Filter rows where HP > 4000
outlier  # Display the outliers

# Calculate the mean of CC excluding outliers
mean_CC <- mean(db$CC[db$CC <= 15000], na.rm = TRUE)

# Replace outliers in the CC variable with the calculated mean
db <- db %>%
  mutate(CC = ifelse(CC > 15000, mean_CC, CC))

# Verify changes
summary(db$CC)



# Set a seed for reproducibility
set.seed(123)

# Shuffle the data
db <- db[sample(nrow(db)), ]

# Calculate the number of rows for each split
n <- nrow(db)
n_train <- floor(0.5 * n)  # 50% for training
n_valid <- floor(0.3 * n)  # 30% for validation

# Create indices for splitting
train_indices <- 1:n_train
valid_indices <- (n_train + 1):(n_train + n_valid)
test_indices <- (n_train + n_valid + 1):n

train_data <- db[train_indices, ]
valid_data <- db[valid_indices, ]
test_data <- db[test_indices, ]

cat("Training set size:", nrow(train_data), "\n")
cat("Validation set size:", nrow(valid_data), "\n")
cat("Test set size:", nrow(test_data), "\n")
colnames(db)
db1 <- db %>%
  select(Age_08_04, KM, Fuel_Type, HP, Automatic, Doors, Quarterly_Tax,
         Mfg_Guarantee, Guarantee_Period, Airco, Automatic_airco, CD_Player,
         Powered_Windows, Sport_Model, Tow_Bar, Price)


ggplot(test_data, aes(x = Price)) + 
  geom_histogram(binwidth = 10, fill = "blue", color = "black", alpha = 0.7)

## A)

# Identify character varaibles
char_vars <- db1 %>%
  select(where(is.character)) %>%
  names()

#Apply label encoding (convert charater variables to integers)
for (var in char_vars) {
  db1[[var]] <- as.integer(factor(db1[[var]]))
}

# lets make a correlation matrix
numeric_vars <- db1 %>%
  select(where(is.numeric))

correlation_matrix <- cor(numeric_vars, use = "complete.obs")  # Use only complete cases
correlation_matrix
print(correlation_matrix)

# Visualize the correlation matrix
library(reshape2)
# from chat gpt
melted_correlation <- melt(correlation_matrix)
ggplot(data = melted_correlation, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  theme_minimal() +
  labs(title = "Correlation Matrix", x = "", y = "")

## top 4 
## Age , KM, Automatic Airco, cd_player, Airco relate heavily with Price.

## B)


library(MASS)  # For stepwise regression

# Formula for the full model
full_model <- lm(Price ~ ., data = db1)

# Stepwise regression using both directions (forward and backward selection)
stepwise_model <- stepAIC(full_model, direction = "both", trace = FALSE)
summary(stepwise_model)

## the p-value is high which is good. the Adjusted R-squared is 88% which is good.
## p-value is less than 0.05 by a lot. This means our model is useful.

# Extract the predictors in the reduced model
predictors <- names(coef(stepwise_model))[-1]  # Remove the intercept
cat("Predictors in the reduced model:", predictors, "\n")

### now we look at test set.


## C)

# Fit the reduced model on training data
reduced_model <- lm(Price ~ ., data = train_data[, c("Price", predictors)])

# Make predictions on the test set
predictions <- predict(reduced_model, newdata = test_data)

# Actual values
actuals <- test_data$Price

# Calculate performance metrics
rmse <- sqrt(mean((predictions - actuals)^2))  # RMSE
mae <- mean(abs(predictions - actuals))        # MAE
r_squared <- 1 - sum((predictions - actuals)^2) / sum((mean(actuals) - actuals)^2)  # R²

# Output results
cat("Performance Metrics:\n")
cat("RMSE:", rmse, "\n")
cat("MAE:", mae, "\n")
cat("R²:", r_squared, "\n")

#using chat
##rsquared is 90% which is even better than the training (88%)
# Optional: Plot residuals
residuals <- actuals - predictions
plot(predictions, residuals, main = "Residual Plot", xlab = "Predicted Values", ylab = "Residuals")
abline(h = 0, col = "red")

### with box cox and the same variables or less variables. 

predictors <- c("Age_08_04", "HP", "Automatic", 
                "Doors", "Quarterly_Tax", "Mfg_Guarantee", "Guarantee_Period",
                "Airco", "Automatic_airco",
                "Sport_Model", "Tow_Bar")
predictors <- c("Age_08_04", "KM", "Fuel_Type", "HP", "Automatic", 
  "Doors", "Quarterly_Tax", "Mfg_Guarantee", "Guarantee_Period",
  "Airco", "Automatic_airco", "CD_Player", "Powered_Windows",
  "Sport_Model", "Tow_Bar")

predictors <- c("Age_08_04", "HP", "Quarterly_Tax",
                "Automatic_airco")

reduced_model <- lm(Price ~ ., data = train_data[, c("Price", predictors)])

# Define the predictors
predictors <- c("Age_08_04", "KM", "HP", "Automatic", 
                "Doors", "Quarterly_Tax", "Mfg_Guarantee", "Guarantee_Period",
                "Airco", "Automatic_airco", "CD_Player", "Powered_Windows",
                "Sport_Model", "Tow_Bar")

# Apply Box-Cox transformation to predictors
transformed_features <- db1  # Copy the dataset
boxcox_trans <- list()  # Store the BoxCoxTrans objects for each feature

for (col in predictors) {
  if (all(db1[[col]] > 0)) {  # Ensure positive values for Box-Cox
    bc <- BoxCoxTrans(db1[[col]])
    boxcox_trans[[col]] <- bc
    transformed_features[[col]] <- predict(bc, db1[[col]])
  } else {
    cat("Skipping Box-Cox for", col, "due to non-positive values.\n")
  }
}

# Combine transformed features with the target variable (unchanged)
transformed_db <- cbind(transformed_features[, predictors], Price = db1$Price)

# Split the transformed data into training and testing sets
set.seed(123)
train_index <- createDataPartition(transformed_db$Price, p = 0.8, list = FALSE)
train_data <- transformed_db[train_index, ]
test_data <- transformed_db[-train_index, ]

# Fit the linear model
reduced_model <- lm(Price ~ ., data = train_data)

# Make predictions
predictions <- predict(reduced_model, newdata = test_data)

# Calculate performance metrics
actuals <- test_data$Price
rmse <- sqrt(mean((predictions - actuals)^2))  # RMSE
mae <- mean(abs(predictions - actuals))        # MAE
r_squared <- 1 - sum((predictions - actuals)^2) / sum((mean(actuals) - actuals)^2)  # R²

# Output metrics
cat("Performance Metrics:\n")
cat("RMSE:", rmse, "\n")
cat("MAE:", mae, "\n")
cat("R²:", r_squared, "\n")


# Optional: Plot residuals
residuals <- actuals - predictions_original
plot(predictions_original, residuals, main = "Residual Plot After Box-Cox", xlab = "Predicted Values", ylab = "Residuals")
abline(h = 0, col = "red")
### worse results with this scaler. I shouldnt scale for this model.


# D)
library(caret)
#


# Assuming `db1` is your dataset and `Price` is the target variable
predictors <- c("Age_08_04", "KM", "HP", "Automatic", 
                "Doors", "Quarterly_Tax", "Mfg_Guarantee", "Guarantee_Period",
                "Airco", "Automatic_airco", "CD_Player", "Powered_Windows",
                "Sport_Model", "Tow_Bar")
predictors <- c("Age_08_04", "HP", "Quarterly_Tax",
                "Automatic_airco")

##### using normalize 

# Normalize the data
preproc <- preProcess(db1[, predictors], method = c("center", "scale"))
normalized_data <- predict(preproc, db1[, predictors])

# Combine normalized predictors with the target variable
normalized_db <- cbind(normalized_data, Price = db1$Price)

# Split the data into training and testing sets
set.seed(123)  # For reproducibility
train_index <- createDataPartition(normalized_db$Price, p = 0.8, list = FALSE)
train_data <- normalized_db[train_index, ]
test_data <- normalized_db[-train_index, ]


##### or use quantile transformer which according to online helps with normlaizing when outliers are present. 

# Apply quantile transformation
preproc <- preProcess(db1[, predictors], method = "YeoJohnson")  # Yeo-Johnson is a robust transform
quantile_scaled_data <- predict(preproc, db1[, predictors])

# Combine scaled predictors with the target variable
quantile_scaled_db <- cbind(quantile_scaled_data, Price = db1$Price)

# Split into training and testing sets
set.seed(123)
train_index <- createDataPartition(quantile_scaled_db$Price, p = 0.8, list = FALSE)
train_data <- quantile_scaled_db[train_index, ]
test_data <- quantile_scaled_db[-train_index, ]

# Check which one I used. 
head(train_data)



##### Perform k-NN for k = 1 to 10
results <- data.frame(k = 1:10, RMSE = NA)

for (k in 1:10) {
  # Train k-NN model
  knn_model <- train(
    Price ~ ., data = train_data,
    method = "knn",
    tuneGrid = data.frame(k = k),
    trControl = trainControl(method = "cv", number = 5)  # 5-fold cross-validation
  )
  
  # Get predictions on the test set
  predictions <- predict(knn_model, newdata = test_data)
  
  # Calculate RMSE
  rmse <- sqrt(mean((predictions - test_data$Price)^2))
  results$RMSE[k] <- rmse
}

rmse
#### Determine the best k
best_k <- results[which.min(results$RMSE), "k"]
cat("Best k chosen:", best_k, "\n")

# Step 5: Visualize RMSE for different k values
library(ggplot2)
ggplot(results, aes(x = k, y = RMSE)) +
  geom_line() +
  geom_point() +
  labs(title = "k-NN Performance", x = "k (Number of Neighbors)", y = "RMSE") +
  theme_minimal()

## the optimal k changes based on the train set. This means our data is neither overfit nor oversmooth.

# Assuming `predictions` contains the predicted values from the best k
# and `test_data$Price` contains the actual test prices.

# Create a data frame for plotting
plot_data <- data.frame(Actual = test_data$Price, Predicted = predictions)

# Plot Actual vs Predicted Values
library(ggplot2)
ggplot(plot_data, aes(x = Actual, y = Predicted)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(title = "Actual vs Predicted Prices",
       x = "Actual Prices",
       y = "Predicted Prices") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Actual values
actuals <- test_data$Price

# Calculate performance metrics
rmse <- sqrt(mean((predictions - actuals)^2))  # RMSE
mae <- mean(abs(predictions - actuals))        # MAE
r_squared <- 1 - sum((predictions - actuals)^2) / sum((mean(actuals) - actuals)^2)  # R²

# Output results
cat("Performance Metrics After Transformation:\n")
cat("RMSE:", rmse, "\n")
cat("MAE:", mae, "\n")
cat("R²:", r_squared, "\n")

# Optional: Plot residuals
residuals <- actuals - predictions
plot(predictions, residuals, main = "Residual Plot After Scaling", xlab = "Predicted Values", ylab = "Residuals")
abline(h = 0, col = "red")








## Model 3

# Load necessary libraries
library(rpart)
## install.packages("rpart.plot")
library(rpart.plot)

predictors <- c("Age_08_04", "KM", "HP", "Automatic", 
                "Doors", "Quarterly_Tax", "Mfg_Guarantee", "Guarantee_Period",
                "Airco", "Automatic_airco", "CD_Player", "Powered_Windows",
                "Sport_Model", "Tow_Bar")
predictors <- c("Age_08_04", "KM", "Quarterly_Tax",
                "Automatic_airco")

##### using normalize 

# Normalize the data
preproc <- preProcess(db1[, predictors], method = c("center", "scale"))
normalized_data <- predict(preproc, db1[, predictors])

# Combine normalized predictors with the target variable
normalized_db <- cbind(normalized_data, Price = db1$Price)

# Split the data into training and testing sets
set.seed(123)  # For reproducibility
train_index <- createDataPartition(normalized_db$Price, p = 0.8, list = FALSE)
train_data <- db1[train_index, ]
test_data <- db1[-train_index, ]
train_data <- train_data[, c(predictors, "Price")]
test_data <- test_data[, c(predictors, "Price")]
# 1. Build a constrained tree (low number of splits)
low_split_tree <- rpart(Price ~ ., data = train_data, 
                        method = "anova", 
                        control = rpart.control(maxdepth = 2))  # Low number of splits

# Visualize the constrained tree
rpart.plot(low_split_tree, main = "Low Split Tree")

# Predictions and evaluation for the constrained tree
low_split_predictions <- predict(low_split_tree, newdata = test_data)
low_split_rmse <- sqrt(mean((low_split_predictions - test_data$Price)^2))
cat("RMSE for Low Split Tree:", low_split_rmse, "\n")

# 2. Find the optimal number of splits using cross-validation
set.seed(123)
optimal_tree <- train(
  Price ~ ., data = train_data, method = "rpart",
  trControl = trainControl(method = "cv", number = 5),  # 5-fold cross-validation
  tuneGrid = data.frame(cp = seq(0.01, 0.1, by = 0.01))  # Test different complexity parameters
)

# Best complexity parameter (cp)
best_cp <- optimal_tree$bestTune$cp
cat("Optimal Complexity Parameter (cp):", best_cp, "\n")

# Train the decision tree with the optimal cp
optimal_split_tree <- rpart(Price ~ ., data = train_data, 
                            method = "anova", 
                            control = rpart.control(cp = best_cp))

# Visualize the optimal tree
rpart.plot(optimal_split_tree, main = "Optimal Split Tree")

# Predictions and evaluation for the optimal tree
optimal_split_predictions <- predict(optimal_split_tree, newdata = test_data)
optimal_split_rmse <- sqrt(mean((optimal_split_predictions - test_data$Price)^2))
cat("RMSE for Optimal Split Tree:", optimal_split_rmse, "\n")

# 3. Compare Actual vs Predicted for both models
comparison_data <- data.frame(
  Actual = test_data$Price,
  Low_Split_Predicted = low_split_predictions,
  Optimal_Split_Predicted = optimal_split_predictions
)

# Visualization of the comparison
library(ggplot2)
ggplot(comparison_data, aes(x = Actual)) +
  geom_point(aes(y = Low_Split_Predicted, color = "Low Split Tree"), alpha = 0.6) +
  geom_point(aes(y = Optimal_Split_Predicted, color = "Optimal Split Tree"), alpha = 0.6) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +
  labs(title = "Comparison of Actual vs Predicted Prices",
       x = "Actual Prices",
       y = "Predicted Prices",
       color = "Model") +
  theme_minimal()

## as you increase the amount of splits the more accurate it gets but eventually you dont want more splits bc u find an optimal number of splits. 

# Actual values
actuals <- test_data$Price

# R-squared for the Low Split Tree
ss_res_low <- sum((actuals - low_split_predictions)^2)
ss_tot <- sum((actuals - mean(actuals))^2)
r_squared_low <- 1 - (ss_res_low / ss_tot)
cat("R-squared for Low Split Tree:", r_squared_low, "\n")

# R-squared for the Optimal Split Tree
ss_res_optimal <- sum((actuals - optimal_split_predictions)^2)
r_squared_optimal <- 1 - (ss_res_optimal / ss_tot)
cat("R-squared for Optimal Split Tree:", r_squared_optimal, "\n")

# 83% is not as good as the last model which was 90% rsquared

## f) 

# Helper function to calculate RMSE and R-squared
calc_metrics <- function(actual, predicted) {
  rmse <- sqrt(mean((actual - predicted)^2))
  ss_res <- sum((actual - predicted)^2)
  ss_tot <- sum((actual - mean(actual))^2)
  r_squared <- 1 - (ss_res / ss_tot)
  list(RMSE = rmse, R_squared = r_squared)
}

# Metrics for Low Split Tree
low_split_train_preds <- predict(low_split_tree, newdata = train_data)
low_split_test_preds <- predict(low_split_tree, newdata = test_data)

low_split_train_metrics <- calc_metrics(train_data$Price, low_split_train_preds)
low_split_test_metrics <- calc_metrics(test_data$Price, low_split_test_preds)

# Metrics for Optimal Split Tree
optimal_split_train_preds <- predict(optimal_split_tree, newdata = train_data)
optimal_split_test_preds <- predict(optimal_split_tree, newdata = test_data)

optimal_split_train_metrics <- calc_metrics(train_data$Price, optimal_split_train_preds)
optimal_split_test_metrics <- calc_metrics(test_data$Price, optimal_split_test_preds)

# Print results
cat("Low Split Tree Metrics:\n")
cat("  Training - RMSE:", low_split_train_metrics$RMSE, 
    "R-squared:", low_split_train_metrics$R_squared, "\n")
cat("  Test - RMSE:", low_split_test_metrics$RMSE, 
    "R-squared:", low_split_test_metrics$R_squared, "\n\n")

cat("Optimal Split Tree Metrics:\n")
cat("  Training - RMSE:", optimal_split_train_metrics$RMSE, 
    "R-squared:", optimal_split_train_metrics$R_squared, "\n")
cat("  Test - RMSE:", optimal_split_test_metrics$RMSE, 
    "R-squared:", optimal_split_test_metrics$R_squared, "\n")

## G)

# so for looking at the optimal split tree the test RMSE was worse then the training. and the r_squared was 83% compared to the training of 86%. 
# I think its like this because of overfitting or bc the test is kinda small. 

  
## I)
# Get variable importance for the optimal decision tree
variable_importance <- optimal_split_tree$variable.importance

# Display the importance of variables
print(variable_importance)

# Create a bar plot of variable importance
importance_data <- data.frame(Variable = names(variable_importance),
                              Importance = variable_importance)

# Sort the variables by importance
importance_data <- importance_data[order(importance_data$Importance, decreasing = TRUE), ]

# Plot the variable importance
library(ggplot2)
ggplot(importance_data, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +
  labs(title = "Variable Importance for Price Prediction",
       x = "Variables",
       y = "Importance") +
  theme_minimal()

db1
# Scatter plot: Age_08_04 vs Price, colored by Price
ggplot(db1) +
  geom_point(aes(x = Age_08_04, y = Price, color = Price)) +
  labs(title = "Age_08_04 vs Price", x = "Age_08_04", y = "Price") +
  theme_minimal()

# Scatter plot: HP vs Price, colored by Price
ggplot(db1) +
  geom_point(aes(x = HP, y = Price, color = Price)) +
  labs(title = "HP vs Price", x = "HP", y = "Price") +
  theme_minimal()

# Scatter plot: Quarterly_Tax vs Price, colored by Price
ggplot(db1) +
  geom_point(aes(x = Quarterly_Tax, y = Price, color = Price)) +
  labs(title = "Quarterly Tax vs Price", x = "Quarterly Tax", y = "Price") +
  theme_minimal()

# Scatter plot: Automatic_airco vs Price, colored by Price
ggplot(db1) +
  geom_point(aes(x = Automatic_airco, y = Price, color = Price)) +
  labs(title = "Automatic Airco vs Price", x = "Automatic Airco", y = "Price") +
  theme_minimal()

