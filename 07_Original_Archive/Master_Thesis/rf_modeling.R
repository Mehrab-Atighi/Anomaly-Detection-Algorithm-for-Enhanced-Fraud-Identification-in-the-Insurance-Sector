#Random forest Modeling
# 
# source(file = "Labaling.R")
# Labaling = Labaling(path = "carclaims.csv" , response_column_number = 33)
# attach(Labaling)
# percent1 = 0.5
# percent2 = 0.8
# percent3 = 1
### Balancing and editing Dataset
diff = setdiff(1:dim(full_data_without_label1)[1] , I1)
I11 = sample(diff , size = c(0.1*length(I1)) , replace = FALSE)
train_df = data.frame(full_data_without_label1[c(I1,I11) , ] , yhat = Result_final_outliers$label[c(I1 , I11) ])
train_df_Balance = ovun.sample(yhat ~., data = train_df, method = "over")$data


### Modeling rond 1:
library(randomForest)

RfModel1 = randomForest(x = train_df_Balance[,-c(ncol(train_df_Balance))],
                             y = train_df_Balance$yhat,
                             ntree = 500)
rf_p_1 <- predict(RfModel1, full_data_without_label2,type = "class")
# Calculate confusion matrix
(rf_cm_1 <- confusionMatrix(rf_p_1, full_data_label2, mode = "everything"))
(rf_roc_1 = ROSE::roc.curve(factor(full_data_label2) , predicted = rf_p_1))
rf_roc_1[2]

rf_p_1_1 = predict(RfModel1, type = "class")
(rf_cm_1_1 = (confusionMatrix(data = rf_p_1_1 , reference = factor(train_df_Balance$yhat) ,mode = "everything")))
(rf_roc_1_1 = ROSE::roc.curve(factor(train_df_Balance$yhat) , predicted = rf_p_1_1))
rf_roc_1_1[2]
### Balancing and editing Dataset2

rf_1_index = which(rf_p_1 == 1)
rf_11_index = which(rf_p_1 == 0)
I22 = sample(rf_11_index , 0.1*length(rf_1_index))
I2 = rf_1_index
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

RfModel2 = randomForest(x = train_df_Balance2[,-c(ncol(train_df_Balance2))],
                        y = train_df_Balance2$yhat,
                        ntree = 500)
rf_p_2 <- predict(RfModel2, full_data_without_label3,type = "class")
# Calculate confusion matrix
(rf_cm_2 <- confusionMatrix(rf_p_2, full_data_label3, mode = "everything"))
(rf_roc_2 = ROSE::roc.curve(factor(full_data_label3) , predicted = rf_p_2))
rf_roc_2[2]


rf_p_2_1 = predict(RfModel2, type = "class")
(rf_cm_2_1 = (confusionMatrix(data = rf_p_2_1 , reference = factor(train_df_Balance2$yhat) ,mode = "everything")))
(rf_roc_2_1 = ROSE::roc.curve(factor(train_df_Balance2$yhat) , predicted = rf_p_2_1))
rf_roc_2_1[2]
### Balancing and editing Dataset3

rf_2_index = which(rf_p_2 == 1)
rf_22_index = which(rf_p_2 == 0)
I33 = sample(rf_22_index , 0.1*length(rf_2_index))
I3 = rf_2_index
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

RfModel3= randomForest(x = train_df_Balance3[,-c(ncol(train_df_Balance3))],
                        y = train_df_Balance3$yhat,
                        ntree = 500)
rf_p_3 <- predict(RfModel3, full_data_without_label4,type = "class")
# Calculate confusion matrix
(rf_cm_3 <- confusionMatrix(rf_p_3, full_data_label4, mode = "everything"))
(rf_roc_3 = ROSE::roc.curve(factor(full_data_label4) , predicted = rf_p_3))
rf_roc_3[2]

rf_p_3_1 = predict(RfModel3, type = "class")
(rf_cm_3_1 = (confusionMatrix(data = rf_p_3_1 , reference = factor(train_df_Balance3$yhat) ,mode = "everything")))
(rf_roc_3_1 = ROSE::roc.curve(factor(train_df_Balance3$yhat) , predicted = rf_p_3_1))
rf_roc_3_1[2]


(output_rf = rbind(rf_test_1 = c(rf_cm_1$byClass , rf_cm_1$overall, rf_roc_1[2]) , rf_test_2 = c(rf_cm_2$byClass , rf_cm_2$overall, rf_roc_2[2]) , rf_test_3 = c(rf_cm_3$byClass , rf_cm_3$overall, rf_roc_3[2]),
                   rf_train_1 = c(rf_cm_1_1$byClass , rf_cm_1_1$overall, rf_roc_1_1[2]) , rf_train_2 = c(rf_cm_2_1$byClass , rf_cm_2_1$overall, rf_roc_2_1[2]) , rf_train_3 = c(rf_cm_3_1$byClass , rf_cm_3_1$overall, rf_roc_3_1[2])))
