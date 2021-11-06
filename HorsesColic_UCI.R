# ------------------------------------------------------------------------------
# Predicting survival and need for surgical intervention in horses with colic 
# using machine learning
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# Downloading, cleaning and exploring the data
# ------------------------------------------------------------------------------

# The data will be downloaded from the UCI Machine Learning Repository. 
# The variable names and characteristics are specified in the Data Set Information.

col_names <- c("surgery", "age", "hospital_number", "rectal_temp", "pulse", 
               "respiratory_rate", "temp_of_extremeties", "peripheral_pulse", 
               "mucous_membranes", "capillary_refill_time", "pain", 
               "peristalsis", "abdominal_distension", "nasogastric_tube",
               "nasogastric_reflux", "nasogastric_reflux_pH", 
               "rectal_exam_feces", "abdomen", "packed_cell_volume", 
               "total_protein", "abdominocent_appearance", 
               "abdominocent_protein", "outcome", "surgical_lesion",
               "lesion_1", "lesion_2", "lesion_3", "cp_data")
colic.train <- read.table(url("http://archive.ics.uci.edu/ml/machine-learning-databases/horse-colic/horse-colic.data"),
                          header = FALSE, col.names = col_names)
colic.test <- read.table(url("http://archive.ics.uci.edu/ml/machine-learning-databases/horse-colic/horse-colic.test"),
                         header = FALSE, col.names = col_names)
data <- rbind(colic.train, colic.test)
str(data, vec.len = 2)

# Missing values are indicated by `?`, we will convert them to `NA`
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
library(tidyverse)
data <- data %>% mutate_all(~replace(., .=="?", NA))

# Some numeric variables are stored as characters, we will convert them to numeric values.
data <- data %>% 
  mutate_at(vars(rectal_temp, pulse, respiratory_rate,
                 nasogastric_reflux_pH, packed_cell_volume, total_protein,
                 abdominocent_protein), as.numeric)

# The remaining categorical variables will be converted to factors based on the Data Set Information.
data <- data %>% 
  mutate(surgery = factor(surgery, labels = c("yes", "no")),
         age = factor(age, labels = c("adult", "young")),
         temp_of_extremeties = factor(temp_of_extremeties, 
                                      labels = c("normal", "warm", "cool", "cold")),
         peripheral_pulse = factor(peripheral_pulse, 
                                   labels = c("normal", "increased", "reduced", "absent")),
         mucous_membranes = factor(mucous_membranes, 
                                   labels = c("normal_pink", "bright_pink", "pale_pink",
                                              "pale_cyanotic", "bright_red", "dark_cyanotic")),
         capillary_refill_time = factor(capillary_refill_time, 
                                        labels = c("<3_sec", ">3_sec", "3_sec")),
         pain = factor(pain, 
                       labels = c("alert", "depressed", "mild", "severe", "extreme")),
         peristalsis = factor(peristalsis, 
                              labels = c("hypermotile", "normal", "hypomotile", "absent")),
         abdominal_distension = factor(abdominal_distension, 
                                       labels = c("none", "slight", "moderate", "severe")),
         nasogastric_tube = factor(nasogastric_tube, 
                                   labels = c("none", "slight", "significant")),
         nasogastric_reflux = factor(nasogastric_reflux, 
                                     labels = c("none", ">1L", "<1L")),
         rectal_exam_feces = factor(rectal_exam_feces, 
                                    labels = c("normal", "increased", "decreased", "absent")),
         abdomen = factor(abdomen, 
                          labels = c("normal", "other", "firm", "small", "large")),
         abdominocent_appearance = factor(abdominocent_appearance, 
                                          labels = c("clear", "cloudy", "serosanguinous")),
         outcome = factor(outcome, 
                          labels = c("lived", "died", "euthanized")),
         surgical_lesion = factor(surgical_lesion, 
                                  labels = c("yes", "no")))

# The variable `cp_data` and the `hospital_number` will be removed from the data set
data <- data %>% select(-cp_data, -hospital_number)

# There are missing values in the data. Let's explore that further.
if(!require(naniar)) install.packages("naniar", repos = "http://cran.us.r-project.org")
library(naniar)
vis_miss(data) + theme(axis.text.x = element_text(angle = 90))
if(!require(UpSetR)) install.packages("UpSetR", repos = "http://cran.us.r-project.org")
library(UpSetR)
gg_miss_upset(data)

# Remove features that contain > 50% missing values
data <- data %>% select_if(~mean(is.na(.)) < 0.5)

# Remove observations with missing values for the outcome variable
data <- data %>% filter(!is.na(outcome) & !is.na(surgical_lesion))

# Take a look at how the different categories of the outcome are distributed in the data
data %>% group_by(outcome) %>% 
  summarize(n = n(), proportion = n()/nrow(data))

# Make a plot that shows how the different features are distributed across outcomes
data %>% select(which(sapply(.,class) == "numeric"),outcome) %>% 
  gather(key = "feature", value = "value", -outcome) %>%
  ggplot(aes(value, color = outcome)) + 
  geom_density() +
  facet_wrap(~feature, scales = "free") +
  theme(axis.title.x = element_blank())

# Convert the outcome variable into a binary variable (survived or died)
data <- data %>% 
  mutate(outcome = as.factor(ifelse(outcome == "euthanized", 
                                    "died", 
                                    levels(outcome)[outcome])))
# Again, take a look at the distribution of the outcomes
data %>% group_by(outcome) %>% 
  summarize(n = n(), proportion = n()/nrow(data))

# Make a plot that shows the distribution of the factor variables 
# and how they are splitacross outcomes
data %>% select_if(~!is.numeric(.)) %>% 
  gather(key = "feature", value = "value", -outcome) %>%
  ggplot(aes(value, fill = outcome)) + 
  geom_bar(stat = "count") +
  facet_wrap(~feature, scales = "free", nrow = 3) +
  theme(axis.text.x = element_text(angle = 30, vjust = 0.8, size = 6),
        axis.title.x = element_blank(),
        strip.text.x = element_text(size = 6))

# Remove the peripheral pulse variable
data <- data %>% select(-peripheral_pulse)

# Create a combined category (>= 3 seconds) for capillary_refill_time
data <- data %>% 
  mutate(capillary_refill_time = 
           as.factor(ifelse(capillary_refill_time %in% c("3_sec", ">3_sec"),
                            ">=3_sec",
                            levels(capillary_refill_time)[capillary_refill_time])))

# Make a plot that shows the distribution of the continuous variables for different outcomes
data %>% select(which(sapply(.,class) == "numeric"), outcome) %>% 
  gather(key = "feature", value = "value", -outcome) %>%
  ggplot(aes(value, color = outcome)) + 
  geom_density() +
  facet_wrap(~feature, scales = "free") +
  theme(axis.title.x = element_blank())

# Look at the total_protein variable
head(data$total_protein)

# Convert total_protein values entered in g/L to g/dL.
data <- data %>% 
  mutate(total_protein = ifelse(total_protein > 25, total_protein/10, total_protein))

# Again, plot the distribution of the continuous variables for different outcomes
data %>% select(which(sapply(.,class) == "numeric"), outcome) %>% 
  gather(key = "feature", value = "value", -outcome) %>%
  ggplot(aes(value, color = outcome)) + 
  geom_density() +
  facet_wrap(~feature, scales = "free") +
  theme(axis.title.x = element_blank())

# Create a data set for the first model
# (remove features that would not be available in practice)
data_survive <- data %>% select(-surgical_lesion, -lesion_1, -lesion_2, -lesion_3)

# Create a data set for the second model
# (remove features that would not be available in practice)
data_surgery <- data %>% select(-outcome, -lesion_1, -lesion_2, -lesion_3, -surgery)

# Take a look at how the outcome variable for the second model (surgery or not)
# is distributed in the data
data_surgery %>% group_by(surgical_lesion) %>% 
  summarize(n = n(), proportion = n()/nrow(data))

# Make a plot that shows how the different features are distributed for horses 
# treated surgically and conservatively
data_surgery %>%  
  select_if(~!is.numeric(.)) %>% 
  gather(key = "feature", value = "value", -surgical_lesion) %>%
  ggplot(aes(value, fill = surgical_lesion)) + 
  geom_bar(stat = "count") +
  facet_wrap(~feature, scales = "free", nrow = 3) +
  theme(axis.text.x = element_text(angle = 30, vjust = 0.8, size = 6),
        axis.title.x = element_blank(),
        strip.text.x = element_text(size = 6))

# Make a plot the shows the distribution of the continuous variables for horses
# treated surgically and conservatively
data_surgery %>% select(which(sapply(.,class) == "numeric"), surgical_lesion) %>% 
  gather(key = "feature", value = "value", -surgical_lesion) %>%
  ggplot(aes(value, color = surgical_lesion)) + 
  geom_density() +
  facet_wrap(~feature, scales = "free") +
  theme(axis.title.x = element_blank())

# ------------------------------------------------------------------------------
# Handling missing values (first model)
# ------------------------------------------------------------------------------

# Show the remaining features that contain missing values
data %>% select_if(~any(is.na(.))) %>% 
  summarise(across(everything(), ~sum(is.na(.)))) %>% t() %>% 
  as.data.frame() %>% 
  rename(missing = V1) %>%
  mutate(missing_percentage = round(missing/nrow(data)*100))

# Impute missing values
if(!require(mice)) install.packages("mice", repos = "http://cran.us.r-project.org")
library(mice)
predictormatrix <- quickpred(data_survive, exclude = NULL, mincor = 0.1)
set.seed(42, sample.kind = "Rounding") # for reproducible results
imputed <- mice(data_survive, maxit = 5, predictorMatrix = predictormatrix, printFlag = FALSE)
imp <- complete(imputed) # Get the complete data set with imputed values from first iteration

# Show the density plots for imputed data sets
densityplot(imputed)

# ------------------------------------------------------------------------------
# Split the data into train and test data sets (first model)
# ------------------------------------------------------------------------------

# Split the data into train (80%) and test (20%) set
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
library(caret)
set.seed(42, sample.kind="Rounding") # For reproducible results
test_index <- createDataPartition(y = imp$outcome, times = 1, p = 0.2, list = FALSE)


# ------------------------------------------------------------------------------
# Training the machine learning algorithms (first model)
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# Classification tree (first model)
# ------------------------------------------------------------------------------
# Train the classification tree model with all 5 imputed data sets and 
# predict the outcome in the test set by majority voting
train.control <- trainControl(method = "cv", number = 10, p = 0.8)
pred_rpart <- data.frame(matrix(0, nrow = nrow(test_index), ncol = 5))
set.seed(42, sample.kind="Rounding") # For reproducible results
for (i in 1:5){
  train <- complete(imputed, i)[-test_index,]
  test <- complete(imputed, i)[test_index,]
  model_rpart <- train(outcome ~ .,
                       method = "rpart",
                       tuneGrid = data.frame(cp = seq(0, 0.05, len = 25)),
                       trControl = train.control,
                       data = train)
  pred_rpart[,i] <- predict(model_rpart, test)
}
pred_rpart_majority <- as.factor(ifelse(rowMeans(pred_rpart == "lived") > 0.5,
                                        "lived", "died"))

# Look at the confusion matrix
cm_rpart <- confusionMatrix(pred_rpart_majority, imp$outcome[test_index])
cm_rpart

# Plot the classification tree (using the last imputed data set)
if(!require(rpart.plot)) install.packages("rpart.plot", repos = "http://cran.us.r-project.org")
library(rpart.plot)
rpart.plot(model_rpart$finalModel)

# ------------------------------------------------------------------------------
# Random forest model (first model)
# ------------------------------------------------------------------------------
# Train the random forest model for all 5 imputed data sets
pred_rf <- data.frame(matrix(0, nrow = nrow(test_index), ncol = 5))
set.seed(42, sample.kind="Rounding") # For reproducible results
for (i in 1:5){
  train <- complete(imputed, i)[-test_index,]
  test <- complete(imputed, i)[test_index,]
  model_rf <- train(outcome ~ .,
                    method = "rf",
                    tuneGrid = data.frame(mtry = seq(1,10,1)),
                    trControl = train.control,
                    data = train)
  pred_rf[,i] <- predict(model_rf, test)
}
pred_rf_majority <- as.factor(ifelse(rowMeans(pred_rf == "lived") > 0.5,
                                     "lived", "died"))
# Look at the confusion matrix
cm_rf <- confusionMatrix(pred_rf_majority, imp$outcome[test_index])
cm_rf

# Show the variable importance of the features in the random forest model
varImp(model_rf)

# ------------------------------------------------------------------------------
# Xgboost model (first model)
# ------------------------------------------------------------------------------
# Prepare the data for training the xgboost model
# Remove the information about the target variable we want to predict
data_xgb <- imp %>% select(-outcome)

# convert the column with the outcome variable to a boolean vector. 
# TRUE indicates that the horses survived, FALSE indicates that the horses died.
survived <- data$outcome == "lived"

# Convert the categorical variables into numeric variables using dummyVars
dmy <- dummyVars(" ~ .", data = data_xgb)
data_xgb <- data.frame(predict(dmy, newdata = data_xgb))

# Create a matrix
data_xgb_matrix <- data.matrix(data_xgb)

# Split the data into a training and a test set
train_data <- data_xgb_matrix[-test_index,]
train_labels <- survived[-test_index]
test_data <- data_xgb_matrix[test_index,]
test_labels <- survived[test_index]

# Convert the matrices into a DMatrix
if(!require(xgboost)) install.packages("xgboost", repos = "http://cran.us.r-project.org")
library(xgboost)
dtrain <- xgb.DMatrix(data = train_data, label= train_labels)
dtest <- xgb.DMatrix(data = test_data, label= test_labels)

# Train a basic model with all the hyperparameters at default
set.seed(42, sample.kind="Rounding") # For reproducible results
model_xgb_default <- xgboost(data = dtrain,
                             nrounds = 100,
                             objective = "binary:logistic",
                             eval_metric = "logloss",
                             verbose = F)

pred_train <- predict(model_xgb_default, dtrain)
err_train <- mean(as.numeric(pred_train > 0.5) != train_labels)
pred_test <- predict(model_xgb_default, dtest)
err_test <- mean(as.numeric(pred_test > 0.5) != test_labels)
print(paste("train-error=", err_train, "test-error=", err_test))

# Tune hyperparameters
best_param <- list() # Create an empty list for tuned hyperparameters
best_seednumber <- 1234 # Seed from best iteration
best_logloss <- Inf # Logloss from best iteration
best_logloss_index <- 0 # Index (number of boosting rounds) from best iteration
set.seed(42, sample.kind="Rounding")
for (iter in 1:100) {
  param <- list(objective = "binary:logistic",
                eval_metric = "logloss",
                max_depth = sample(3:9, 1),
                eta = runif(1, 0.01, 0.3),
                subsample = runif(1, 0.6, 1),
                colsample_bytree = runif(1, 0.6, 1), 
                min_child_weight = sample(1:40, 1)
  )
  cv.nround <-  1000
  cv.nfold <-  10 # 10-fold cross-validation
  seed.number  <-  sample.int(10000, 1) # set seed for cross-validation
  set.seed(seed.number, sample.kind="Rounding")
  negative_cases <- sum(train_labels == FALSE)
  postive_cases <- sum(train_labels == TRUE)
  mdcv <- xgb.cv(data = dtrain, params = param,  
                 nfold = cv.nfold, nrounds = cv.nround,
                 verbose = F, early_stopping_rounds = 3, 
                 scale_pos_weight = negative_cases/postive_cases,
                 maximize = FALSE)
  
  min_logloss <- min(mdcv$evaluation_log$test_logloss_mean)
  min_logloss_index <- which.min(mdcv$evaluation_log$test_logloss_mean)
  
  if (min_logloss < best_logloss) {
    best_logloss <- min_logloss
    best_logloss_index <- min_logloss_index
    best_seednumber <- seed.number
    best_param <- param
  }
}

# The best index (min_logloss_index) is the best "nround" in the model
nround <- best_logloss_index
set.seed(best_seednumber, sample.kind="Rounding")
model_xgb <- xgboost(data = dtrain, params = best_param, nround = nround, verbose = F)

# Check error in test data
pred_test <- predict(model_xgb, dtest)
err_test <- mean(as.numeric(pred_test > 0.5) != test_labels)
print(err_test)

# Train the model again with the tuned hyperparameters in all 5 imputed data sets
pred_xgb <- data.frame(matrix(0, nrow = nrow(test_index), ncol = 5))
set.seed(42, sample.kind="Rounding")
for (i in 1:5){
  data_xgb <- complete(imputed, i) %>% select(-outcome)
  survived <- complete(imputed, i)$outcome == "lived"
  dmy <- dummyVars(" ~ .", data = data_xgb)
  data_xgb <- data.frame(predict(dmy, newdata = data_xgb))
  data_xgb_matrix <- data.matrix(data_xgb)
  train_data <- data_xgb_matrix[-test_index,]
  train_labels <- survived[-test_index]
  test_data <- data_xgb_matrix[test_index,]
  test_labels <- survived[test_index]
  dtrain <- xgb.DMatrix(data = train_data, label= train_labels)
  dtest <- xgb.DMatrix(data = test_data, label= test_labels)
  negative_cases <- sum(train_labels == FALSE)
  postive_cases <- sum(train_labels == TRUE)
  model_xgb <- xgboost(data = dtrain,
                       nrounds = nround,
                       params = best_param,
                       scale_pos_weight = negative_cases/postive_cases,
                       verbose = F)
  pred_xgb[,i] <- predict(model_xgb, dtest)
}
pred_xgb_majority <- as.factor(ifelse(rowMeans(pred_xgb) > 0.5, "lived", "died"))
cm_xgb <- confusionMatrix(pred_xgb_majority, data$outcome[test_index])
cm_xgb

# Plot the model, fitted to the last imputed data set
xgb.plot.multi.trees(feature_names = names(data_xgb_matrix), model = model_xgb)

# ------------------------------------------------------------------------------
# Model evaluation (first model)
# ------------------------------------------------------------------------------
# Create a table of the results for each model
result_firstmodel <- data.frame("Model" = c("Classification Tree",
                                            "Random Forest",
                                            "XGBoost"),
                                "Accuracy" = c(cm_rpart$overall["Accuracy"],
                                               cm_rf$overall["Accuracy"],
                                               cm_xgb$overall["Accuracy"]),
                                "Sensitivity" = c(cm_rpart$byClass["Sensitivity"],
                                                  cm_rf$byClass["Sensitivity"],
                                                  cm_xgb$byClass["Sensitivity"]),
                                "Specificity" = c(cm_rpart$byClass["Specificity"],
                                                  cm_rf$byClass["Specificity"],
                                                  cm_xgb$byClass["Specificity"]))
result_firstmodel


# ------------------------------------------------------------------------------
# Handling missing values (second model)
# ------------------------------------------------------------------------------
# Impute missing values
predictormatrix <- quickpred(data_surgery, exclude = NULL, mincor = 0.1)
set.seed(42, sample.kind = "Rounding") # for reproducible results
imputed <- mice(data_surgery, maxit = 5, predictorMatrix = predictormatrix, printFlag = FALSE)
imp <- complete(imputed)

# Show density plot for imputed data sets
densityplot(imputed)

# ------------------------------------------------------------------------------
# Split the data into train and test data sets (second model)
# ------------------------------------------------------------------------------
# Split the data into train (80%) and test (20%) set
set.seed(42, sample.kind="Rounding") # For reproducible results
test_index <- createDataPartition(y = imp$surgical_lesion, times = 1, p = 0.2, list = FALSE)

# ------------------------------------------------------------------------------
# Training the machine learning algorithms (second model)
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# Classification tree (second model)
# ------------------------------------------------------------------------------
# Train the classification tree model with all 5 imputed data sets and 
# predict the outcome in the test set by majority voting
train.control <- trainControl(method = "cv", number = 10, p = 0.8)
pred_rpart <- data.frame(matrix(0, nrow = nrow(test_index), ncol = 5))
set.seed(42, sample.kind="Rounding") # For reproducible results
for (i in 1:5){
  train <- complete(imputed, i)[-test_index,]
  test <- complete(imputed, i)[test_index,]
  model_rpart <- train(surgical_lesion ~ .,
                       method = "rpart",
                       tuneGrid = data.frame(cp = seq(0, 0.05, len = 25)),
                       trControl = train.control,
                       data = train)
  pred_rpart[,i] <- predict(model_rpart, test)
}
pred_rpart_majority <- as.factor(ifelse(rowMeans(pred_rpart == "yes") > 0.5,
                                        "yes", "no"))
# Look at the confusion matrix
cm_rpart <- confusionMatrix(pred_rpart_majority, test$surgical_lesion)
cm_rpart

# Plot the classification tree (using the last imputed data set)
rpart.plot(model_rpart$finalModel)

# ------------------------------------------------------------------------------
# Random forest model (second model)
# ------------------------------------------------------------------------------
# Train the random forest model for all 5 imputed data sets
pred_rf <- data.frame(matrix(0, nrow = nrow(test), ncol = 5))
set.seed(42, sample.kind="Rounding") # For reproducible results
for (i in 1:5){
  train <- complete(imputed, i)[-test_index,]
  test <- complete(imputed, i)[test_index,]
  model_rf <- train(surgical_lesion ~ .,
                    method = "rf",
                    tuneGrid = data.frame(mtry = seq(1,10,1)),
                    trControl = train.control,
                    data = train)
  pred_rf[,i] <- predict(model_rf, test)
}
pred_rf_majority <- as.factor(ifelse(rowMeans(pred_rf == "yes") > 0.5,
                                     "yes", "no"))
# Look at the confusion matrix
cm_rf <- confusionMatrix(pred_rf_majority, test$surgical_lesion)
cm_rf

# ------------------------------------------------------------------------------
# Xgboost model (second model)
# ------------------------------------------------------------------------------
# Train the xgboost default model 
pred_xgb <- data.frame(matrix(0, nrow = nrow(test), ncol = 5))
set.seed(42, sample.kind="Rounding") # For reproducible results
for (i in 1:5){
  data_xgb <- complete(imputed, i) %>% select(-surgical_lesion)
  surgical_lesion <- complete(imputed, i)$surgical_lesion == "yes"
  dmy <- dummyVars(" ~ .", data = data_xgb)
  data_xgb <- data.frame(predict(dmy, newdata = data_xgb))
  data_xgb_matrix <- data.matrix(data_xgb)
  train_data <- data_xgb_matrix[-test_index,]
  train_labels <- surgical_lesion[-test_index]
  test_data <- data_xgb_matrix[test_index,]
  test_labels <- surgical_lesion[test_index]
  dtrain <- xgb.DMatrix(data = train_data, label= train_labels)
  dtest <- xgb.DMatrix(data = test_data, label= test_labels)
  model_xgb <- xgboost(data = dtrain,
                       nrounds = 100,
                       objective = "binary:logistic",
                       eval_metric = "logloss",
                       verbose = F)
  pred_xgb[,i] <- predict(model_xgb, dtest)
}
pred_xgb_majority <- as.factor(ifelse(rowMeans(pred_xgb) > 0.5, "yes", "no"))
confusionMatrix(pred_xgb_majority, test$surgical_lesion)

# Tune hyperparameters
best_param <- list() # Create an empty list for tuned hyperparameters
best_seednumber <- 1234 # Seed from best iteration
best_logloss <- Inf # Logloss from best iteration
best_logloss_index <- 0 # Index (number of boosting rounds) from best iteration
set.seed(42, sample.kind = "Rounding")
for (iter in 1:100) {
  param <- list(objective = "binary:logistic",
                eval_metric = "logloss",
                max_depth = sample(3:9, 1),
                eta = runif(1, 0.01, 0.3),
                subsample = runif(1, 0.6, 1),
                colsample_bytree = runif(1, 0.6, 1), 
                min_child_weight = sample(1:40, 1)
  )
  cv.nround <-  1000
  cv.nfold <-  10 # 10-fold cross-validation
  seed.number  <-  sample.int(10000, 1) # set seed for cross-validation
  set.seed(seed.number, sample.kind="Rounding")
  negative_cases <- sum(train_labels == FALSE)
  postive_cases <- sum(train_labels == TRUE)
  mdcv <- xgb.cv(data = dtrain, params = param,  
                 nfold = cv.nfold, nrounds = cv.nround,
                 verbose = F, early_stopping_rounds = 3, 
                 scale_pos_weight = negative_cases/postive_cases,
                 maximize = FALSE)
  
  min_logloss <- min(mdcv$evaluation_log$test_logloss_mean)
  min_logloss_index <- which.min(mdcv$evaluation_log$test_logloss_mean)
  
  if (min_logloss < best_logloss) {
    best_logloss <- min_logloss
    best_logloss_index <- min_logloss_index
    best_seednumber <- seed.number
    best_param <- param
  }
}

# The best index (min_logloss_index) is the best "nround" in the model
nround <- best_logloss_index
set.seed(best_seednumber, sample.kind="Rounding")
model_xgb <- xgboost(data = dtrain, params = best_param, nround = nround, verbose = F)

# Check error in test data
pred_test <- predict(model_xgb, dtest)
err_test <- mean(as.numeric(pred_test > 0.5) != test_labels)
print(err_test)

# Train the xgboost model again using the tuned hyperparameters
pred_xgb <- data.frame(matrix(0, nrow = nrow(test_index), ncol = 5))
set.seed(42, sample.kind = "Rounding")
for (i in 1:5){
  data_xgb <- complete(imputed, i) %>% select(-surgical_lesion)
  surgical_lesion <- complete(imputed, i)$surgical_lesion == "yes"
  dmy <- dummyVars(" ~ .", data = data_xgb)
  data_xgb <- data.frame(predict(dmy, newdata = data_xgb))
  data_xgb_matrix <- data.matrix(data_xgb)
  train_data <- data_xgb_matrix[-test_index,]
  train_labels <- surgical_lesion[-test_index]
  test_data <- data_xgb_matrix[test_index,]
  test_labels <- surgical_lesion[test_index]
  dtrain <- xgb.DMatrix(data = train_data, label= train_labels)
  dtest <- xgb.DMatrix(data = test_data, label= test_labels)
  negative_cases <- sum(train_labels == FALSE)
  postive_cases <- sum(train_labels == TRUE)
  model_xgb <- xgboost(data = dtrain,
                       nrounds = nround,
                       params = best_param,
                       scale_pos_weight = negative_cases/postive_cases,
                       verbose = F)
  pred_xgb[,i] <- predict(model_xgb, dtest)
}
pred_xgb_majority <- as.factor(ifelse(rowMeans(pred_xgb) > 0.5, "yes", "no"))
cm_xgb <- confusionMatrix(pred_xgb_majority, test$surgical_lesion)
cm_xgb

# ------------------------------------------------------------------------------
# Model evaluation (first model)
# ------------------------------------------------------------------------------
# Create a table of the results for each model
result_secondmodel <- data.frame("Model" = c("Classification Tree",
                                             "Random Forest",
                                             "XGBoost"),
                                 "Accuracy" = c(cm_rpart$overall["Accuracy"],
                                                cm_rf$overall["Accuracy"],
                                                cm_xgb$overall["Accuracy"]),
                                 "Sensitivity" = c(cm_rpart$byClass["Sensitivity"],
                                                   cm_rf$byClass["Sensitivity"],
                                                   cm_xgb$byClass["Sensitivity"]),
                                 "Specificity" = c(cm_rpart$byClass["Specificity"],
                                                   cm_rf$byClass["Specificity"],
                                                   cm_xgb$byClass["Specificity"]))
result_secondmodel
