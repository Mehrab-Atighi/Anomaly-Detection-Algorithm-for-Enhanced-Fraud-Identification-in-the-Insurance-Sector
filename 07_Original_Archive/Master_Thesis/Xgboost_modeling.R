#XGBoost Modeling
# source(file = "Labaling.R")
# Labaling = Labaling(path = "carclaims.csv" , response_column_number = 33)
# attach(Labaling)
# percent1 = 0.5
# percent2 = 0.8
# percent3 = 1
### Balancing and editing Dataset
### Balancing and editing Dataset
diff = setdiff(1:dim(full_data_without_label1)[1] , I1)
I11 = sample(diff , size = c(0.1*length(I1)) , replace = FALSE)
train_df = data.frame(full_data_without_label1[c(I1,I11) , ] , yhat = Result_final_outliers$label[c(I1 , I11) ])
train_df_Balance = ovun.sample(yhat ~., data = train_df, method = "over")$data

library(xgboost)

# Set XGBoost parameters
params <- list(
  objective = "binary:logistic",   # For binary classification
  eval_metric = "logloss",         # Logarithmic Loss (Log Loss) for evaluation
  eta = 0.01,                       # Learning rate (Typical values: 0.01 - 0.3)
  max_depth = 10,                   # Maximum depth of a tree (Typical values: 3-10)
  nrounds = 100                   # Number of boosting rounds (or trees)
)


# Convert data into DMatrix format required by XGBoost
dtrain1 <- xgb.DMatrix(data = as.matrix(train_df_Balance[,-c(categorical_columns,ncol(train_df_Balance))])  , label = factor(train_df_Balance$yhat))
dtest1 <- xgb.DMatrix(data = as.matrix(full_data_without_label2[-c(categorical_columns)]), label = full_data_label2)


# Train the XGBoost model
XgbModel1 <- xgboost(params, data = dtrain1, nrounds = params$nrounds)
xgb_p_1 <- predict(XgbModel1, dtest1 , type = "class")
# Convert probabilities to binary predictions
xgb_p_1 <- factor(ifelse(xgb_p_1 > median(xgb_p_1), 1, 0))
(xgb_cm_1 <- confusionMatrix(xgb_p_1, full_data_label2, mode = "everything"))
(xgb_roc_1 = ROSE::roc.curve(factor(full_data_label2) , predicted = xgb_p_1))
xgb_roc_1[2]

xgb_p_1_1 = predict(XgbModel1, dtrain1 , type = "class")
xgb_p_1_1 <- factor(ifelse(xgb_p_1_1 > median(xgb_p_1_1), 1, 0))
(xgb_cm_1_1 = (confusionMatrix(data = xgb_p_1_1 , reference = factor(train_df_Balance$yhat) ,mode = "everything")))
(xgb_roc_1_1 = ROSE::roc.curve(factor(train_df_Balance$yhat) , predicted = xgb_p_1_1))
xgb_roc_1_1[2]



### Balancing and editing Dataset2

xgb_1_index = which(xgb_p_1 == 1)
xgb_11_index = which(xgb_p_1 == 0)
I22 = sample(xgb_11_index , 0.1*length(xgb_1_index))
I2 = xgb_1_index
Result_final_rond1 = data.frame(ID = (percent1*number_of_rows):(percent2*number_of_rows) , label = 0)
for(i in I2){
  Result_final_rond1$label[i] = 1
}

j = 0
jj = table(Result_final_rond1$label)
I2 = which(Result_final_rond1$label == 1) 
for(i in 1:nrow(Result_final_rond1)){
  if(Result_final_rond1$label[i] == 1){
    if(full_data_label2[i] == 0){
      Result_final_rond1$label[i] = 0
      j = j +1
    }
  }
}
(acc1_2 = (1 - (j / jj[2])))

train_df2 = data.frame(full_data_without_label2[c(I2,I22) , ] , yhat = Result_final_rond1$label[c(I2,I22)])
train_df2 = rbind(train_df , train_df2)
train_df_Balance2 = ovun.sample(yhat ~., data = train_df2, method = "over" )$data

### Modeling rond 2:

# Convert data into DMatrix format required by XGBoost
dtrain2 <- xgb.DMatrix(data = as.matrix(train_df_Balance2[,-c(categorical_columns,ncol(train_df_Balance2))])  , label = factor(train_df_Balance2$yhat))
dtest2 <- xgb.DMatrix(data = as.matrix(full_data_without_label3[-c(categorical_columns)]), label = full_data_label3)


# Train the XGBoost model
XgbModel2 <- xgboost(params, data = dtrain2, nrounds = params$nrounds)
xgb_p_2 <- predict(XgbModel2, dtest2 , type = "class")
# Convert probabilities to binary predictions
xgb_p_2 <- factor(ifelse(xgb_p_2 > median(xgb_p_2), 1, 0))
(xgb_cm_2 <- confusionMatrix(xgb_p_2, full_data_label3, mode = "everything"))
(xgb_roc_2 = ROSE::roc.curve(factor(full_data_label3) , predicted = xgb_p_2))
xgb_roc_2[2]

xgb_p_2_1 = predict(XgbModel2, dtrain2 , type = "class")
xgb_p_2_1 <- factor(ifelse(xgb_p_2_1 > median(xgb_p_2_1), 1, 0))
(xgb_cm_2_1 = (confusionMatrix(data = xgb_p_2_1 , reference = factor(train_df_Balance2$yhat) ,mode = "everything")))
(xgb_roc_2_1 = ROSE::roc.curve(factor(train_df_Balance2$yhat) , predicted = xgb_p_2_1))
xgb_roc_2_1[2]
### Balancing and editing Dataset3

xgb_2_index = which(xgb_p_2 == 1)
xgb_22_index = which(xgb_p_2 == 0)
I33 = sample(xgb_22_index , 0.1*length(xgb_2_index))
I3 = xgb_2_index
Result_final_rond2 = data.frame(ID = (percent2*number_of_rows):(percent3*number_of_rows) , label = 0)
for(i in I3){
  Result_final_rond2$label[i] = 1
}

j = 0
jj = table(Result_final_rond1$label)
I3 = which(Result_final_rond1$label == 1) 
for(i in 1:nrow(Result_final_rond1)){
  if(Result_final_rond1$label[i] == 1){
    if(full_data_label2[i] == 0){
      Result_final_rond1$label[i] = 0
      j = j +1
    }
  }
}
(acc1_3 = (1 - (j / jj[2])))

train_df3 = data.frame(full_data_without_label3[c(I3,I33) , ] , yhat = Result_final_rond2$label[c(I3,I33)])
train_df3 = rbind(train_df , train_df2 , train_df3)
train_df_Balance3 = ovun.sample(yhat ~., data = train_df3, method = "over" )$data


### Modeling rond 3:
# Convert data into DMatrix format required by XGBoost
dtrain3 <- xgb.DMatrix(data = as.matrix(train_df_Balance3[,-c(categorical_columns,ncol(train_df_Balance3))])  , label = factor(train_df_Balance3$yhat))
dtest3 <- xgb.DMatrix(data = as.matrix(full_data_without_label4[-c(categorical_columns)]), label = full_data_label4)


# Train the XGBoost model
XgbModel3 <- xgboost(params, data = dtrain3, nrounds = params$nrounds)
xgb_p_3 <- predict(XgbModel3, dtest3 , type = "class")
# Convert probabilities to binary predictions
xgb_p_3 <- factor(ifelse(xgb_p_3 > median(xgb_p_3), 1, 0))
(xgb_cm_3 <- confusionMatrix(xgb_p_3, full_data_label4, mode = "everything"))
(xgb_roc_3 = ROSE::roc.curve(factor(full_data_label4) , predicted = xgb_p_2))
xgb_roc_2[2]

xgb_p_3_1 = predict(XgbModel3, dtrain3 , type = "class")
xgb_p_3_1 <- factor(ifelse(xgb_p_3_1 > median(xgb_p_3_1), 1, 0))
(xgb_cm_3_1 = (confusionMatrix(data = xgb_p_3_1 , reference = factor(train_df_Balance3$yhat) ,mode = "everything")))
(xgb_roc_3_1 = ROSE::roc.curve(factor(train_df_Balance3$yhat) , predicted = xgb_p_3_1))
xgb_roc_3_1[2]

(output_xgb = rbind(xgb_test_1 = c(xgb_cm_1$byClass , xgb_cm_1$overall, xgb_roc_1[2]) , xgb_test_2 = c(xgb_cm_2$byClass , xgb_cm_2$overall, xgb_roc_2[2]) , xgb_test_3 = c(xgb_cm_3$byClass , xgb_cm_3$overall, xgb_roc_3[2]),
                    xgb_train_1 = c(xgb_cm_1_1$byClass , xgb_cm_1_1$overall, xgb_roc_1_1[2]) , xgb_train_2 = c(xgb_cm_2_1$byClass , xgb_cm_2_1$overall, xgb_roc_2_1[2]) , xgb_train_3 = c(xgb_cm_3_1$byClass , xgb_cm_3_1$overall, xgb_roc_3_1[2])))
