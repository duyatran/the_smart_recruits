setwd("C:/Users/Duy Tran/Desktop/the_smart_recruits/the_smart_recruits")
library(xgboost)
library(Matrix)
library(ggplot2)
library(Ckmeans.1d.dp)
library(caret)
library(readr)
library(dplyr)
library(tidyr)
library(randomForest)
#Reading in training and testing data
train = read.csv("train.csv", stringsAsFactors = TRUE)
test = read.csv("test.csv", stringsAsFactors = TRUE)
train$Office_PIN = as.factor(train$Office_PIN)
test$Office_PIN = as.factor(test$Office_PIN)
#Fixing the dates column
train$Application_Receipt_Date = gsub("^([0-9]){1}\\/","0\\1\\/", train$Application_Receipt_Date)
train$Application_Receipt_Date = gsub("\\/([0-9]){1}\\/","\\/0\\1\\/", train$Application_Receipt_Date)
train$Application_Receipt_Date = as.Date(train$Application_Receipt_Date, format = "%m/%d/%Y")

train$Applicant_BirthDate = gsub("^([0-9]){1}\\/","0\\1\\/", train$Applicant_BirthDate)
train$Applicant_BirthDate = gsub("\\/([0-9]){1}\\/","\\/0\\1\\/", train$Applicant_BirthDate)
train$Applicant_BirthDate = as.Date(train$Applicant_BirthDate, format = "%m/%d/%Y")

train$Manager_DOJ = gsub("^([0-9]){1}\\/","0\\1\\/", train$Manager_DOJ)
train$Manager_DOJ = gsub("\\/([0-9]){1}\\/","\\/0\\1\\/", train$Manager_DOJ)
train$Manager_DOJ = as.Date(train$Manager_DOJ, format = "%m/%d/%Y")

train$Manager_DoB = gsub("^([0-9]){1}\\/","0\\1\\/", train$Manager_DoB)
train$Manager_DoB = gsub("\\/([0-9]){1}\\/","\\/0\\1\\/", train$Manager_DoB)
train$Manager_DoB = as.Date(train$Manager_DoB, format = "%m/%d/%Y")

train$Applicant_Gender = ifelse(train$Applicant_Gender == "F", 1, 0)
train$Manager_Gender = ifelse(train$Manager_Gender == "F", 1, 0)

# Fix blanks in Marital Status
train$Applicant_Marital_Status = as.character(train$Applicant_Marital_Status)
train$Applicant_Marital_Status[train$Applicant_Marital_Status == ""] = "Other"
train$Applicant_Marital_Status = as.factor(train$Applicant_Marital_Status)

#Establish orders for designations
train$Manager_Joining_Designation = as.character(train$Manager_Joining_Designation)
train$Manager_Joining_Designation[train$Manager_Joining_Designation == ""] = "0"
train$Manager_Joining_Designation[train$Manager_Joining_Designation == "Other"] = "0"
train$Manager_Joining_Designation = gsub("Level ","", train$Manager_Joining_Designation)
train$Manager_Joining_Designation = as.numeric(train$Manager_Joining_Designation)

train$Manager_Current_Designation = as.character(train$Manager_Current_Designation)
train$Manager_Current_Designation[train$Manager_Current_Designation == ""] = "0"
train$Manager_Current_Designation = gsub("Level ","", train$Manager_Current_Designation)
train$Manager_Current_Designation = as.numeric(train$Manager_Current_Designation)

train$promotion = as.numeric(train$Manager_Current_Designation-train$Manager_Joining_Designation)
train$Manager_Joining_Designation = as.factor(train$Manager_Joining_Designation)
train$Manager_Current_Designation = as.factor(train$Manager_Current_Designation)

#Fill in blanks or NAs
train$Applicant_Occupation[train$Applicant_Occupation == ""] = "Others"

train$Applicant_Qualification[train$Applicant_Qualification == ""] = "Others"

train$Manager_Grade[is.na(train$Manager_Grade)] = median(train$Manager_Grade, na.rm = TRUE)
train$Manager_Num_Application[is.na(train$Manager_Num_Application)] = median(train$Manager_Num_Application, na.rm = TRUE)
train$Manager_Num_Coded[is.na(train$Manager_Num_Coded)] = median(train$Manager_Num_Coded, na.rm = TRUE)
train$Manager_Business[is.na(train$Manager_Business)] = mean(train$Manager_Business, na.rm = TRUE)

train$Manager_Business2[is.na(train$Manager_Business2)] = mean(train$Manager_Business2, na.rm = TRUE)
train$Manager_Num_Products[is.na(train$Manager_Num_Products)] = median(train$Manager_Num_Products, na.rm = TRUE)
train$Manager_Num_Products2[is.na(train$Manager_Num_Products2)] = median(train$Manager_Num_Products2, na.rm = TRUE)

train$Manager_Status = as.character(train$Manager_Status)
train$Manager_Status[train$Manager_Status == ""] = "Other"
train$Manager_Status = as.factor(train$Manager_Status)


# Cleaning data for test
#Fixing the dates column
test$Application_Receipt_Date = gsub("^([0-9]){1}\\/","0\\1\\/", test$Application_Receipt_Date)
test$Application_Receipt_Date = gsub("\\/([0-9]){1}\\/","\\/0\\1\\/", test$Application_Receipt_Date)
test$Application_Receipt_Date = as.Date(test$Application_Receipt_Date, format = "%m/%d/%Y")

test$Applicant_BirthDate = gsub("^([0-9]){1}\\/","0\\1\\/", test$Applicant_BirthDate)
test$Applicant_BirthDate = gsub("\\/([0-9]){1}\\/","\\/0\\1\\/", test$Applicant_BirthDate)
test$Applicant_BirthDate = as.Date(test$Applicant_BirthDate, format = "%m/%d/%Y")

test$Manager_DOJ = gsub("^([0-9]){1}\\/","0\\1\\/", test$Manager_DOJ)
test$Manager_DOJ = gsub("\\/([0-9]){1}\\/","\\/0\\1\\/", test$Manager_DOJ)
test$Manager_DOJ = as.Date(test$Manager_DOJ, format = "%m/%d/%Y")

test$Manager_DoB = gsub("^([0-9]){1}\\/","0\\1\\/", test$Manager_DoB)
test$Manager_DoB = gsub("\\/([0-9]){1}\\/","\\/0\\1\\/", test$Manager_DoB)
test$Manager_DoB = as.Date(test$Manager_DoB, format = "%m/%d/%Y")

test$Applicant_Gender = ifelse(test$Applicant_Gender == "F", 1, 0)
test$Manager_Gender = ifelse(test$Manager_Gender == "F", 1, 0)

# Fix marital status blanks
test$Applicant_Marital_Status = as.character(test$Applicant_Marital_Status)
test$Applicant_Marital_Status[test$Applicant_Marital_Status == ""] = "Other"
test$Applicant_Marital_Status = as.factor(test$Applicant_Marital_Status)

#Establish orders for designations and add promotion variable
test$Manager_Joining_Designation = as.character(test$Manager_Joining_Designation)
test$Manager_Joining_Designation[test$Manager_Joining_Designation == ""] = "0"
test$Manager_Joining_Designation = gsub("Level ","", test$Manager_Joining_Designation)
test$Manager_Joining_Designation = as.numeric(test$Manager_Joining_Designation)

test$Manager_Current_Designation = as.character(test$Manager_Current_Designation)
test$Manager_Current_Designation[test$Manager_Current_Designation == ""] = "0"
test$Manager_Current_Designation = gsub("Level ","", test$Manager_Current_Designation)
test$Manager_Current_Designation = as.numeric(test$Manager_Current_Designation)

test$promotion = as.numeric(test$Manager_Current_Designation-test$Manager_Joining_Designation)
test$Manager_Joining_Designation = as.factor(test$Manager_Joining_Designation)
test$Manager_Current_Designation = as.factor(test$Manager_Current_Designation)

#Fill in blanks or NAs
test$Applicant_Occupation[test$Applicant_Occupation == ""] = "Others"
test$Applicant_Qualification[test$Applicant_Qualification == ""] = "Others"
test$Manager_Grade[is.na(test$Manager_Grade)] = median(test$Manager_Grade, na.rm = TRUE)
test$Manager_Num_Application[is.na(test$Manager_Num_Application)] = median(test$Manager_Num_Application, na.rm = TRUE)
test$Manager_Num_Coded[is.na(test$Manager_Num_Coded)] = median(test$Manager_Num_Coded, na.rm = TRUE)
test$Manager_Business[is.na(test$Manager_Business)] = mean(test$Manager_Business, na.rm = TRUE)
test$Manager_Business2[is.na(test$Manager_Business2)] = mean(test$Manager_Business2, na.rm = TRUE)
test$Manager_Num_Products[is.na(test$Manager_Num_Products)] = median(test$Manager_Num_Products, na.rm = TRUE)
test$Manager_Num_Products2[is.na(test$Manager_Num_Products2)] = median(test$Manager_Num_Products2, na.rm = TRUE)

test$Manager_Status = as.character(test$Manager_Status)
test$Manager_Status[test$Manager_Status == ""] = "Other"
test$Manager_Status = as.factor(test$Manager_Status)

# Equalize levels of Manager Joining Designation by changing all "7" to "6"
train$Manager_Joining_Designation[train$Manager_Joining_Designation == "7"] = "6"
levels(test$Manager_Joining_Designation) <- levels(train$Manager_Joining_Designation)

# Add missing level into train and test
levels(test$Applicant_Qualification) <- levels(train$Applicant_Qualification)

# Remove Applicant city PIN
train = train[,-4]
test = test[,-4]

# Add age
train$Applicant_Age = as.numeric(train$Application_Receipt_Date - train$Applicant_BirthDate) / 365
train$Applicant_Age[is.na(train$Applicant_Age)] = mean(train$Applicant_Age, na.rm=TRUE)

test$Applicant_Age = as.numeric(test$Application_Receipt_Date - test$Applicant_BirthDate) / 365
test$Applicant_Age[is.na(test$Applicant_Age)] = mean(test$Applicant_Age, na.rm=TRUE)

# Add manager's years of experience when application received
train$Years_Joined = as.numeric(train$Application_Receipt_Date - train$Manager_DOJ) / 365
train$Years_Joined[is.na(train$Years_Joined)] = mean(train$Years_Joined, na.rm=TRUE)
train$Manager_Age = as.numeric(train$Manager_DOJ - train$Manager_DoB) / 365
train$Manager_Age[is.na(train$Manager_Age)] = mean(train$Manager_Age, na.rm=TRUE)

test$Years_Joined = as.numeric(test$Application_Receipt_Date - test$Manager_DOJ) / 365
test$Years_Joined[is.na(test$Years_Joined)] = mean(test$Years_Joined, na.rm=TRUE)
test$Manager_Age = as.numeric(test$Manager_DOJ - test$Manager_DoB) / 365
test$Manager_Age[is.na(test$Manager_Age)] = mean(test$Manager_Age, na.rm=TRUE)

# removing all dates column
train = train[,-c(1, 2, 3, 5, 9, 15)]
test_ID = test$ID
test = test[,-c(1, 2, 3, 5, 9, 15)]

#train = train[,c(17, 18, 19, 11, 9, 12, 13, 10, 15)]
#test = test[,c(17, 18, 19, 11, 9, 12, 13, 10)]
# xgboost
seed <- 235
set.seed(seed)
#train$Business_Sourced = as.factor(train$Business_Sourced)
target = train$Business_Sourced
# cross-validation
# xgboost fitting with arbitrary parameters
xgb_params_1 = list(
  objective = "binary:logistic",                                               # binary classification
  eta = 0.01,                                                                  # learning rate
  max.depth = 3,                                                               # max tree depth
  eval_metric = "auc",                                                          # evaluation/loss metric
  gamma = 0,
  subsample=0.8,
  colsample_bytree=0.8,
  min_child_weight=1,
  scale_pos_weight=1
  
  )
#train$Business_Sourced = as.factor(train$Business_Sourced)
test$Business_Sourced = -1
for (col in colnames(train)) {
  levels(test$col) <- levels(train$col)
}

ForestModel = randomForest(Business_Sourced ~ ., data=train, nodesize=1, ntree=1000)
pred = predict(ForestModel, newdata=test)

# sparse_matrix = sparse.model.matrix(Business_Sourced~.-1, data=train)
# 
# model_cv <- xgb.cv(data=sparse_matrix, 
#                    silent = 1, 
#                    label=as.matrix(target), 
#                    params = xgb_params_1, 
#                    nfold=5, 
#                    nrounds=1000)
# 
# model <- xgboost(data=sparse_matrix,
#                  silent = 1,
#                  label=target, 
#                  params = xgb_params_1, 
#                  nround=1000,
#                  early.stop.round = 100)
# test$Business_Sourced = -1
# sparse_matrix_test = sparse.model.matrix(Business_Sourced~.-1, data=test)
# pred <- predict(model, sparse_matrix_test)


# Plot feature importance
#output_vector = train[,"Business_Sourced"] == "1"
#importance = xgb.importance(feature_names = sparse_matrix@Dimnames[[2]], model = model)
#importance_raw = xgb.importance(feature_names = sparse_matrix@Dimnames[[2]], model = model, data = sparse_matrix, label = output_vector)
#xgb.plot.importance(importance_matrix = importance_raw)
#submit = data.frame("ID" = test_ID, "Business_Sourced" = pred)
#write.csv(submit, "submit.csv", row.names = F)
