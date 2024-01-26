library(NLP)
library(dplyr)
library(Boruta)
library(MASS)
library(nortest)
library(vcd)
library(polycor)
library(plyr)
library(rcompanion)

library(ggplot2)
library(naniar)
library(UpSetR)
library(visdat)
library(VIM)


library(nnet)
library(caret)
library(randomForest)

# accuracy calculations
get_accuracy <- function(confusion_matrix) {
  accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
  return(accuracy)
}

# precision calculations
get_precision <- function(confusion_matrix) {
  confusion_matrix <- as.matrix(confusion_matrix)
  precision <- round(diag(confusion_matrix) / rowSums(confusion_matrix), 3)
  return(precision)
}


# recall calculations
get_recall <- function(confusion_matrix) {
  confusion_matrix <- as.matrix(confusion_matrix)
  recall <- round(diag(confusion_matrix) / colSums(confusion_matrix), 3)
  return(recall)
}



# f1 calculations
get_f1 <- function(confusion_matrix) {
  confusion_matrix <- as.matrix(confusion_matrix)
  recall <- diag(confusion_matrix) / colSums(confusion_matrix)
  precision <- diag(confusion_matrix) / rowSums(confusion_matrix)
  f1 <- round(2 * (precision * recall) / (precision + recall), 3)
  return(f1)
}

change_output_range <- function(output_vector) {
  #change the levels of happiness from 0-10 to 1-3
  original_vector_output <- output_vector
  numeric_vector <- as.numeric(original_vector_output)
  breaks <- c(-Inf, 4, 7, 11)
  labels <- c(1, 2, 3)
  converted_vector <- ordered(cut(numeric_vector, breaks = breaks, labels = labels, include.lowest = TRUE))
  return (converted_vector)
}


split_data <- function(data) {
  # Set the seed for reproducibility
  set.seed(123)
  idx <- sample(seq_len(nrow(data)), size = 0.8 * nrow(data))
  train <- data[train_indices, ]
  test <- data[-train_indices, ]
  splitted_data <- c(train, test)
  
  return(splitted_data)
}


#Format the data frame to represent factor levels, ordinal levels and interval nature
convert_frame_to_correct_type = function(df) {
  var_info <- read.csv("D:/dissertation/vars.csv")
  var_info <- (var_info[, 1])[1:113]
  
  for (i in 1:nrow(var_info)) {
    row_values <- var_info[i, ]
    var_name <- as.String(row_values$variable)
    
    df[[var_name]] <- 
      switch(
        var_type,
        "nominal" = {
          var_type <- as.String(row_values$type)
          var_max <-as.integer(row_values$valid_max)
          #any encoding greater than the max threshold in meta data are represented as missing in ESS
          df <- df %>% mutate( {{ var_name }} := case_when( .data[[var_name]] <= var_max ~ .data[[var_name]], TRUE ~ NA_real_ ) )
          factor(df[[var_name]])
        },
        "ordinal" = {
          var_type <- as.String(row_values$type)
          var_max <-as.integer(row_values$valid_max)
          df <- df %>% mutate( {{ var_name }} := case_when( .data[[var_name]] <= var_max ~ .data[[var_name]], TRUE ~ NA_real_ ) )
          df[[var_name]] <- ordered(df[[var_name]])
        },
        "interval" = {
          var_type <- as.String(row_values$type)
          var_max <-as.integer(row_values$valid_max)
          df <- df %>% mutate( {{ var_name }} := case_when( .data[[var_name]] <= var_max ~ .data[[var_name]], TRUE ~ NA_real_ ) )
        },
        stop("Invalid var_type specified.")
      )
  }
  return(df)
}



raw_data <- read.csv("D:/dissertation/ess10/ESS10.csv")
df <- convert_frame_to_correct_type(raw_data)


## check association strength and direction of the association with the output variable
var_info <- read.csv("D:/dissertation/vars.csv")
for (i in 2:nrow(var_info)) {
  df_init <- df
  row <- var_info[i, ]
  var_type <- as.character(row$type)
  
  result <- switch(
    var_type,
    "nominal" = {
      row <- var_info[i, ]
      var_type <- as.character(row$type)
      independant_variable <- as.character(row$variable)
      df_init <- df_init[df_init$happy <= 10, ]
      y <- df_init$happy
      contingency_table <- table(df_init[[independant_variable]], y)
      chi_square_result <- chisq.test(contingency_table)
      cramer_v_value <- assocstats(contingency_table)$cramer
      
      cat(row$desc)
      cat(chi_square_result, "\n")
      cat("Cramér's V:", cramer_v_value, "\n")
    },
    default = {
      row <- var_info[i, ]
      var_type <- as.character(row$type)
      independant_variable <- as.character(row$variable)
      df_init <- df_init[df_init$happy <= 10, ]
      x <- df_init[[independant_variable]]
      y <- df_init$happy
      cat(row$desc)
      correlation <- cor(x, y, method = "spearman")
      cat(correlation, "\n")
    }
  )
}


###
### Related plots for variables
###
ggplot(df, aes(x = factor(rlgdgr
))) +
  geom_bar(fill = "orange", color = "black", position = "stack", stat = "count", 
           aes(y = ..prop.., group = 1)) +
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +
  labs(title = "Religous Level",
       x = "Level",
       y = "Percentage")

ggplot(df, aes(x = factor(hincfel
))) +
  geom_bar(fill = "orange", color = "black", position = "stack", stat = "count", 
           aes(y = ..prop.., group = 1)) +
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +
  labs(title = "Feeling of household income",
       x = "Level",
       y = "Percentage")

ggplot(df, aes(x = factor(gndr
))) +
  geom_bar(fill = "orange", color = "black", position = "stack", stat = "count", 
           aes(y = ..prop.., group = 1)) +
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +
  labs(title = "Gender distribution",
       x = "Level",
       y = "Percentage")

ggplot(df, aes(x = factor(health
))) +
  geom_bar(fill = "orange", color = "black", position = "stack", stat = "count", 
           aes(y = ..prop.., group = 1)) +
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +
  labs(title = "Subjective general health",
       x = "Level",
       y = "Percentage")
hist(df$agea, 
     main = "Age distribution",
     xlab = "Age",
     ylab = "Frequency",
     col = "skyblue",
     border = "black")

#plot missing data
ggplot_object <- vis_miss(df, "warn_large_data" = FALSE)
plot(ggplot_object, col = c("navyblue", "red"), numbers = TRUE, sortVars = TRUE, labels = colnames(df), cex.axis = 0.7, gap = 3, ylab = c("Missing data", "Pattern"))


##
## Hypothesis test for missingness with output relation using chi square
##
contingency_table <- table(is.na(df$hinctnta), df$happy)
print(contingency_table)
chi_square_result <- chisq.test(contingency_table)
print(chi_square_result)


contingency_table <- table(is.na(df$stfedu), df$happy)
print(contingency_table)
chi_square_result <- chisq.test(contingency_table)
print(chi_square_result)


contingency_table <- table(is.na(df$hmsfmlsh), df$happy)
print(contingency_table)
chi_square_result <- chisq.test(contingency_table)
print(chi_square_result)


contingency_table <- table(is.na(df$trstep), df$happy)
print(contingency_table)
chi_square_result <- chisq.test(contingency_table)
print(chi_square_result)


contingency_table <- table(is.na(df$trstun), df$happy)
print(contingency_table)
chi_square_result <- chisq.test(contingency_table)
print(chi_square_result)


#remove missing columns
complete_data <- na.omit(df)
dim(complete_data)
table(complete_data$happy)

df_without_range_change <- na.omit(copy(df))
# Perform feature selection for df_without_range_change using Boruta
boruta_result_df_without_range_change <- Boruta(df_without_range_change$happy ~ ., data = complete_data, doTrace = 2)
boruta_selected_features_df_without_range_change <- getSelectedAttributes(df_without_range_change, withTentative = FALSE)
df_without_range_change <- df_without_range_change[, boruta_selected_features_df_without_range_change]


#change the range of happy variable
df$happy <- change_output_range(df$happy)
df_with_range_change <- na.omit(copy(df))
# Perform feature selection for df_without_range_change using Boruta
boruta_result_df_with_range_change <- Boruta(df_with_range_change$happy ~ ., data = complete_data, doTrace = 2)
boruta_selected_features_df_with_range_change <- getSelectedAttributes(df_with_range_change, withTentative = FALSE)
df_with_range_change <- df_with_range_change[, boruta_selected_features_df_with_range_change]

train_data <- split_data(df_without_range_change)[0]
test_data <- split_data(df_without_range_change)[1]

weights <- 1 / as.numeric(train_data$happy)

# Apply ordered logistic regression with class weights
model <- polr(happy ~ ., weights = weights, data = train_data, Hess = TRUE)
# Make predictions on the test set for ordered logistic regression
predicted_classes <- predict(model, newdata = test_data)
confusion_matrix <- table(predicted_classes, test_data$happy)
print(confusion_matrix)

cat("Accuracy of the model:", get_accuracy(confusion_matrix), "\n")


train_data <- split_data(df_with_range_change)[0]
test_data <- split_data(df_with_range_change)[1]

weights <- 1 / as.numeric(train_data$happy)

# Apply ordered logistic regression with class weights
model <- polr(happy ~ ., weights = weights, data = train_data, Hess = TRUE)
# Make predictions on the test set for ordered logistic regression
predicted_classes <- predict(model, newdata = test_data)
confusion_matrix <- table(predicted_classes, test_data$happy)
print(confusion_matrix)

cat("Accuracy of the model:", get_accuracy(confusion_matrix), "\n")


# Use the "rf" method for all variable types
method_list <- rep("rf", length(colnames(df)))
# Create a mice imputation object
rf_imp_model <- mice(df, method = method_list, m = 5, maxit = 20, predictorMatrix = pred_matrix)
rf_imputed_data <- complete(rf_imp_model)
pmm_imp_model <- mice(df, m = 5, maxit = 22, method = "pmm", seed = 123)
pmm_imputed_data <- complete(pmm_imp_model)


# Perform feature selection for dataset 1 using Boruta
boruta_result_rf <- Boruta(rf_imputed_data$happy ~ ., data = complete_data, doTrace = 2)
boruta_selected_features_rf <- getSelectedAttributes(boruta_result_rf, withTentative = FALSE)

plot(boruta_result_rf)
print(boruta_selected_features)
print(boruta_result_rf)

# Find the rejected features
rejected_features <- names(boruta_result_rf$finalDecision[boruta_result$finalDecision == "Rejected"])
print(rejected_features)

# Perform feature selection for dataset 1 using Boruta
boruta_result_pmm <- Boruta(pmm_imputed_data$happy ~ ., data = complete_data, doTrace = 2)
boruta_selected_features_pmm <- getSelectedAttributes(boruta_result_pmm, withTentative = FALSE)

plot(boruta_result_pmm)
print(boruta_selected_features_pmm)
print(boruta_result_pmm)

# Find the rejected features
rejected_features <- names(boruta_result_pmm$finalDecision[boruta_result_pmm$finalDecision == "Rejected"])
print(rejected_features)

# Perform feature selection for dataset 1 using Boruta
boruta_result <- Boruta(complete_data$happy ~ ., data = complete_data, doTrace = 2)
boruta_selected_features <- getSelectedAttributes(boruta_result, withTentative = FALSE)

plot(boruta_result)
print(boruta_selected_features)
print(boruta_result)

# Find the rejected features
rejected_features <- names(boruta_result$finalDecision[boruta_result$finalDecision == "Rejected"])
print(rejected_features)

# Identify features
features <- names(df)[1:ncol(df)]  # Exclude the target variable
target <- "happy"

# Handle nominal variables using model.matrix and convert to data frame
mydata_modelmatrix <- model.matrix( 
  ~ . - 1, data = df[, features, drop = FALSE])

#prepare as model matrix as some algorithms expect model matrix
model_matrix_data <- as.data.frame(mydata_modelmatrix)
model_matrix_data$happy <- factor(model_matrix_data$happy)

df$happy <- factor(df$happy)

# Create data set 1
data_set_1 <- complete_data[, boruta_selected_features]
dim(data_set_1)

data_set_2 <- rf_imputed_data[, boruta_selected_features_rf]
dim(data_set_2)

data_set_3 <- pmm_imputed_data[, boruta_selected_features_pmm]
dim(data_set_3)

train_data <- split_data(data_set_1)[0]
test_data <- split_data(data_set_1)[1]

weights <- 1 / as.numeric(train_data$happy)

# Apply ordered logistic regression with class weights
model <- polr(happy ~ ., weights = weights, data = train_data, Hess = TRUE)

# Make predictions on the test set for ordered logistic regression
predicted_classes <- predict(model, newdata = test_data)
confusion_matrix <- table(predicted_classes, test_data$happy)
print(confusion_matrix)

cat("Accuracy of the model:", get_accuracy(confusion_matrix), "\n")
cat("Class-wise Precision:", get_precision(confusion_matrix), "\n")
cat("Class-wise Recall:", get_recall(confusion_matrix), "\n")
cat("F1 values:", get_f1(confusion_matrix), "\n")

# Apply ordered probit regression with class weights
model <- polr(happy ~ ., weights = weights, data = train_data, method = "probit")

# Make predictions on the test set for ordered probit regression
predicted_classes <- predict(model, newdata = test_data)

confusion_matrix <- table(predicted_classes, test_data$happy)
print(confusion_matrix)

cat("Accuracy of the model:", get_accuracy(confusion_matrix), "\n")
cat("Class-wise Precision:", get_precision(confusion_matrix), "\n")
cat("Class-wise Recall:", get_recall(confusion_matrix), "\n")
cat("F1 values:", get_f1(confusion_matrix), "\n")

library(caret)
library(randomForest)

train_data <- split_data(data_set_1)[0]
test_data <- split_data(data_set_1)[1]

table(train_data$happy)
class_weights <- 1 / table(train_data$happy)

ctrl <- trainControl(method = "cv", number = 5)
model <- randomForest(happy ~ ., data = train_data, classwt = class_weights, ntree = 1500, mtry = sqrt(ncol(train_data)))

print(summary(model))
predictions <- predict(model, newdata = test_data)

confusion_matrix <- table(predictions, test_data$happy)
print(conf_matrix)

cat("Accuracy of the model:", get_accuracy(confusion_matrix), "\n")
cat("Class-wise Precision:", get_precision(confusion_matrix), "\n")
cat("Class-wise Recall:", get_recall(confusion_matrix), "\n")
cat("F1 values:", get_f1(confusion_matrix), "\n")

library(lightgbm)

# Sample data into training and testing sets
train_indices <- sample(seq_len(nrow(data_set_1)), size = 0.8 * nrow(data_set_1))
train_data <- data_set_1[train_indices, ]
test_data <- data_set_1[-train_indices, ]

# Extract features and target variable
train_dt <- as.matrix(train_data[, !names(train_data) %in% c("happy")])
label <- as.integer(train_data$happy)  # Convert to integer for ordinal regression

# Create LightGBM dataset
train_set <- lgb.Dataset(train_dt, label = label)

# Set class weights based on the distribution of the target variable
class_weights <- 1 / table(label)

# Set LightGBM parameters for ordinal regression with class weights
params <- list(
  objective = "regression_l2",
  metric = "custom", 
  boosting_type = "gbdt",
  num_leaves = 50,
  learning_rate = 0.05,
  feature_fraction = 0.9,
  weight = class_weights
)

# Train the LightGBM model
model <- lgb.train(params = params, data = train_set, nrounds = 500)

# Make predictions on the test set
predictions <- round(predict(model, as.matrix(test_data[, !names(test_data) %in% c("happy")])))
actual_values <- as.integer(test_data$happy)
conf_matrix <- table(predictions, actual_values)

cat("Accuracy of the model:", get_accuracy(confusion_matrix), "\n")
cat("Class-wise Precision:", get_precision(confusion_matrix), "\n")
cat("Class-wise Recall:", get_recall(confusion_matrix), "\n")
cat("F1 values:", get_f1(confusion_matrix), "\n")


###
## Data set 2
###


train_data <- split_data(data_set_2)[0]
test_data <- split_data(data_set_2)[1]

weights <- 1 / as.numeric(train_data$happy)

# Apply ordered logistic regression with class weights
model <- polr(happy ~ ., weights = weights, data = train_data, Hess = TRUE)
# Make predictions on the test set for ordered logistic regression
predicted_classes <- predict(model, newdata = test_data)
confusion_matrix <- table(predicted_classes, test_data$happy)
print(confusion_matrix)

cat("Accuracy of the model:", get_accuracy(confusion_matrix), "\n")
cat("Class-wise Precision:", get_precision(confusion_matrix), "\n")
cat("Class-wise Recall:", get_recall(confusion_matrix), "\n")
cat("F1 values:", get_f1(confusion_matrix), "\n")

# Apply ordered probit regression with class weights
model <- polr(happy ~ ., weights = weights, data = train_data, method = "probit")
# Make predictions on the test set for ordered probit regression
predicted_classes <- predict(model, newdata = test_data)
confusion_matrix <- table(predicted_classes, test_data$happy)
print(confusion_matrix)

cat("Accuracy of the model:", get_accuracy(confusion_matrix), "\n")
cat("Class-wise Precision:", get_precision(confusion_matrix), "\n")
cat("Class-wise Recall:", get_recall(confusion_matrix), "\n")
cat("F1 values:", get_f1(confusion_matrix), "\n")

library(caret)
library(randomForest)

train_data <- split_data(data_set_2)[0]
test_data <- split_data(data_set_2)[1]

table(train_data$happy)
class_weights <- 1 / table(train_data$happy)
ctrl <- trainControl(method = "cv", number = 5)
model <- randomForest(happy ~ ., data = train_data, classwt = class_weights, ntree = 1500, mtry = sqrt(ncol(train_data)))

print(summary(model))
predictions <- predict(model, newdata = test_data)
confusion_matrix <- table(predictions, test_data$happy)
print(conf_matrix)

cat("Accuracy of the model:", get_accuracy(confusion_matrix), "\n")
cat("Class-wise Precision:", get_precision(confusion_matrix), "\n")
cat("Class-wise Recall:", get_recall(confusion_matrix), "\n")
cat("F1 values:", get_f1(confusion_matrix), "\n")

library(lightgbm)

# Sample data into training and testing sets
train_data <- split_data(data_set_2)[0]
test_data <- split_data(data_set_2)[1]

# Extract features and target variable
train_dt <- as.matrix(train_data[, !names(train_data) %in% c("happy")])
label <- as.integer(train_data$happy)  # Convert to integer for ordinal regression
# Create LightGBM dataset
train_set <- lgb.Dataset(train_dt, label = label)

# Set class weights based on the distribution of the target variable
class_weights <- 1 / table(label)

# Set LightGBM parameters for ordinal regression with class weights
params <- list(
  objective = "regression_l2",
  metric = "custom", 
  boosting_type = "gbdt",
  num_leaves = 50,
  learning_rate = 0.05,
  feature_fraction = 0.9,
  weight = class_weights
)

# Train the LightGBM model
model <- lgb.train(params = params, data = train_set, nrounds = 500)
# Make predictions on the test set
predictions <- round(predict(model, as.matrix(test_data[, !names(test_data) %in% c("happy")])))
actual_values <- as.integer(test_data$happy)
conf_matrix <- table(predictions, actual_values)

cat("Accuracy of the model:", get_accuracy(confusion_matrix), "\n")
cat("Class-wise Precision:", get_precision(confusion_matrix), "\n")
cat("Class-wise Recall:", get_recall(confusion_matrix), "\n")
cat("F1 values:", get_f1(confusion_matrix), "\n")


###
## Data set 3
###


train_data <- split_data(data_set_3)[0]
test_data <- split_data(data_set_3)[1]

weights <- 1 / as.numeric(train_data$happy)

# Apply ordered logistic regression with class weights
model <- polr(happy ~ ., weights = weights, data = train_data, Hess = TRUE)
# Make predictions on the test set for ordered logistic regression
predicted_classes <- predict(model, newdata = test_data)
confusion_matrix <- table(predicted_classes, test_data$happy)
print(confusion_matrix)

cat("Accuracy of the model:", get_accuracy(confusion_matrix), "\n")
cat("Class-wise Precision:", get_precision(confusion_matrix), "\n")
cat("Class-wise Recall:", get_recall(confusion_matrix), "\n")
cat("F1 values:", get_f1(confusion_matrix), "\n")

# Apply ordered probit regression with class weights
model <- polr(happy ~ ., weights = weights, data = train_data, method = "probit")
# Make predictions on the test set for ordered probit regression
predicted_classes <- predict(model, newdata = test_data)
confusion_matrix <- table(predicted_classes, test_data$happy)
print(confusion_matrix)

cat("Accuracy of the model:", get_accuracy(confusion_matrix), "\n")
cat("Class-wise Precision:", get_precision(confusion_matrix), "\n")
cat("Class-wise Recall:", get_recall(confusion_matrix), "\n")
cat("F1 values:", get_f1(confusion_matrix), "\n")

library(caret)
library(randomForest)

train_data <- split_data(data_set_3)[0]
test_data <- split_data(data_set_3)[1]

table(train_data$happy)
class_weights <- 1 / table(train_data$happy)
ctrl <- trainControl(method = "cv", number = 5)
model <- randomForest(happy ~ ., data = train_data, classwt = class_weights, ntree = 1500, mtry = sqrt(ncol(train_data)))

print(summary(model))
predictions <- predict(model, newdata = test_data)
confusion_matrix <- table(predictions, test_data$happy)
print(conf_matrix)

cat("Accuracy of the model:", get_accuracy(confusion_matrix), "\n")
cat("Class-wise Precision:", get_precision(confusion_matrix), "\n")
cat("Class-wise Recall:", get_recall(confusion_matrix), "\n")
cat("F1 values:", get_f1(confusion_matrix), "\n")

library(lightgbm)

# Sample data into training and testing sets
train_data <- split_data(data_set_3)[0]
test_data <- split_data(data_set_3)[1]

# Extract features and target variable
train_dt <- as.matrix(train_data[, !names(train_data) %in% c("happy")])
label <- as.integer(train_data$happy)  # Convert to integer for ordinal regression
# Create LightGBM dataset
train_set <- lgb.Dataset(train_dt, label = label)

# Set class weights based on the distribution of the target variable
class_weights <- 1 / table(label)

# Set LightGBM parameters for ordinal regression with class weights
params <- list(
  objective = "regression_l2",
  metric = "custom", 
  boosting_type = "gbdt",
  num_leaves = 50,
  learning_rate = 0.05,
  feature_fraction = 0.9,
  weight = class_weights
)

# Train the LightGBM model
model <- lgb.train(params = params, data = train_set, nrounds = 500)
# Make predictions on the test set
predictions <- round(predict(model, as.matrix(test_data[, !names(test_data) %in% c("happy")])))
actual_values <- as.integer(test_data$happy)
confusion_matrix <- table(predictions, actual_values)

cat("Accuracy of the model:", get_accuracy(confusion_matrix), "\n")
cat("Class-wise Precision:", get_precision(confusion_matrix), "\n")
cat("Class-wise Recall:", get_recall(confusion_matrix), "\n")
cat("F1 values:", get_f1(confusion_matrix), "\n")



library(ordinal)
library(caret)

train_data <- split_data(data_set_2)[0]
test_data <- split_data(data_set_2)[1]

# Create a train control object for cross-validation
ctrl <- trainControl(method = "cv", number = 5)  # Adjust the number of folds as needed
hyperparameter_grid <- expand.grid(cp = seq(0.001, 0.5, by = 0.001))
# Train Ordered Probit Regression Models
models <- list()
for (i in seq_along(hyperparameter_grid$cp)) {
  model <- polr(happy ~ ., data = train_data, Hess = TRUE, method = "probit", control = clm.control(tol = hyperparameter_grid$cp[i]))
  models[[i]] <- list(model = model, cp = hyperparameter_grid$cp[i])
}
AIC_values <- sapply(models, function(model_list) AIC(model_list$model))
best_model_index <- which.min(AIC_values)
best_model <- models[[best_model_index]]$model

best_cp <- models[[best_model_index]]$cp
print(best_cp)

summary_coeff <- summary(best_model)
coefficients_table <- summary_coeff$coefficients
typeof(coefficients_table)

# Displaying the significant coefficients
options(max.print = 1000)  
tail(coefficients_table, n=100)

# Save the data frame to a CSV file
write.csv(coefficients_table, file = file_path, row.names = FALSE)
file_path <- "D:/significant_coefficients.csv"

# Set significance level
alpha <- 0.05
# Identify significant coefficients
significant_coefficients <- names(p_values[p_values < alpha])

# Display results
cat("Significant coefficients:\n")
cat(significant_coefficients, sep = "\n")

# Variable Importance
importance_values <- varImp(best_model)

# Display Variable Importance
print(importance_values)

# Extract coefficients
coefficients <- coef(model)

predicted_classes <- predict(model, newdata = test_data)

# Evaluate the model
confusion_matrix <- table(predicted_classes, test_data$happy)
print(confusion_matrix)

cat("Accuracy of the model:", get_accuracy(confusion_matrix), "\n")
cat("Class-wise Precision:", get_precision(confusion_matrix), "\n")
cat("Class-wise Recall:", get_recall(confusion_matrix), "\n")
cat("F1 values:", get_f1(confusion_matrix), "\n")


## test ordinal logistic regression with hyper parameter tuning
set.seed(123)
trainIndex <- createDataPartition(data_set_2$happy, p = 0.8, list = FALSE, times = 1)
train_data <- data_set_2[trainIndex, ]
test_data <- data_set_2[-trainIndex, ]

# Specify class weights
class_weights <- 1/table(train_data$happy)

# Define the hyperparameter grid
hyperparameter_grid <- expand.grid(size = seq(1, 10), decay = seq(0.1, 0.9, by = 0.1))

# Create a training control object for cross-validation
ctrl <- trainControl(method = "cv", number = 5, classProbs = TRUE)

# Train the ordinal logistic regression model with hyperparameter tuning
model <- train(happy ~ ., data = train_data, method = "nnet", trace = FALSE,
               trControl = ctrl, tuneGrid = hyperparameter_grid,
               MaxNWts = 10000, MaxIter = 1000, linout = TRUE, skip = TRUE)

# Print the best hyperparameter values
print(paste("Best size:", model$bestTune$size))
print(paste("Best decay:", model$bestTune$decay))

# Make predictions on the test set
predictions <- predict(model, newdata = test_data, type = "response")

# Convert predicted probabilities to predicted classes
predicted_classes <- apply(predictions, 1, which.max)

# Confusion matrix
conf_matrix <- table(predicted_classes, test_data$happy)

# Calculate performance metrics
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
precision <- diag(conf_matrix) / rowSums(conf_matrix)
recall <- diag(conf_matrix) / colSums(conf_matrix)
f1 <- 2 * (precision * recall) / (precision + recall)

print(accuracy)
print(precision)
print(recall)
print(f1)


