#Knn Modeling
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


preProcValues1 <- preProcess(train_df_Balance[,-c(categorical_columns,ncol(train_df_Balance))], method = c("center", "scale"))
trainTransformed1 <- cbind(predict(preProcValues1, train_df_Balance[,-c(categorical_columns,ncol(train_df_Balance))]), yhat = train_df_Balance$yhat)
testTransformed1 <- cbind(predict(preProcValues1, full_data_without_label2[,-c(categorical_columns,ncol(train_df_Balance))]) , yhat = full_data_label2)


### Modeling rond 1:
library(caret)

knnModel1 <- train(
  yhat ~ ., 
  data = trainTransformed1, 
  method = "knn", 
  trControl = trainControl(method = "cv"), 
  tuneGrid = data.frame(k = c(3:8))
)

best_model1<- knn3(
  yhat ~ .,
  data = trainTransformed1,
  k = knnModel1$bestTune$k
)

knn_p_1 <- predict(best_model1, testTransformed1,type = "class")
# Calculate confusion matrix
(knn_cm_1 <- confusionMatrix(knn_p_1, testTransformed1$yhat, mode = "everything"))
(knn_roc_1 = ROSE::roc.curve(factor(full_data_label2) , predicted = knn_p_1))
knn_roc_1[2]

knn_p_1_1 = predict(best_model1,trainTransformed1 ,  type = "class")
(knn_cm_1_1 = (confusionMatrix(data = knn_p_1_1 , reference = factor(trainTransformed1$yhat) ,mode = "everything")))
(knn_roc_1_1 = ROSE::roc.curve(factor(trainTransformed1$yhat), predicted = knn_p_1_1))
knn_roc_1_1[2]
### Balancing and editing Dataset2

knn_1_index = which(knn_p_1 == 1)
knn_11_index = which(knn_p_1 == 0)
I22 = sample(knn_11_index , 0.1*length(knn_1_index))
I2 = knn_1_index
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

preProcValues2 <- preProcess(train_df_Balance2[,-c(categorical_columns,ncol(train_df_Balance2))], method = c("center", "scale"))
trainTransformed2 <- cbind(predict(preProcValues2, train_df_Balance2[,-c(categorical_columns,ncol(train_df_Balance2))]), yhat = train_df_Balance2$yhat)
testTransformed2 <- cbind(predict(preProcValues2, full_data_without_label3[,-c(categorical_columns,ncol(train_df_Balance2))]) , yhat = full_data_label3)

### Modeling rond 2:

knnModel2 <- train(
  yhat ~ ., 
  data = trainTransformed2, 
  method = "knn", 
  trControl = trainControl(method = "cv"), 
  tuneGrid = data.frame(k = c(3:8))
)

best_model2<- knn3(
  yhat ~ .,
  data = trainTransformed2,
  k = knnModel2$bestTune$k
)

knn_p_2 <- predict(best_model2, testTransformed2,type = "class")
# Calculate confusion matrix
(knn_cm_2 <- confusionMatrix(knn_p_2, testTransformed2$yhat, mode = "everything"))
(knn_roc_2 = ROSE::roc.curve(factor(full_data_label3) , predicted = knn_p_2))
knn_roc_2[2]

knn_p_2_1 = predict(best_model1,trainTransformed2 ,  type = "class")
(knn_cm_2_1 = (confusionMatrix(data = knn_p_2_1 , reference = factor(trainTransformed2$yhat) ,mode = "everything")))
(knn_roc_2_1 = ROSE::roc.curve(factor(trainTransformed2$yhat), predicted = knn_p_2_1))
knn_roc_2_1[2]
### Balancing and editing Dataset3

knn_2_index = which(knn_p_2 == 1)
knn_22_index = which(knn_p_2 == 0)
I33 = sample(knn_22_index , 0.1*length(knn_2_index))
I3 = knn_2_index
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

preProcValues3<- preProcess(train_df_Balance3[,-c(categorical_columns,ncol(train_df_Balance3))], method = c("center", "scale"))
trainTransformed3 <- cbind(predict(preProcValues3, train_df_Balance3[,-c(categorical_columns,ncol(train_df_Balance3))]), yhat = train_df_Balance3$yhat)
testTransformed3 <- cbind(predict(preProcValues3, full_data_without_label4[,-c(categorical_columns,ncol(train_df_Balance3))]) , yhat = full_data_label4)

### Modeling rond 3:

knnModel3 <- train(
  yhat ~ ., 
  data = trainTransformed3, 
  method = "knn", 
  trControl = trainControl(method = "cv"), 
  tuneGrid = data.frame(k = c(3:8))
)

best_model3<- knn3(
  yhat ~ .,
  data = trainTransformed3,
  k = knnModel3$bestTune$k
)

knn_p_3 <- predict(best_model3, testTransformed3,type = "class")
# Calculate confusion matrix
(knn_cm_3 <- confusionMatrix(knn_p_3, testTransformed3$yhat , mode = "everything"))
(knn_roc_3 = ROSE::roc.curve(factor(full_data_label4) , predicted = knn_p_3))
knn_roc_3[2]

knn_p_3_1 = predict(best_model1,trainTransformed3 ,  type = "class")
(knn_cm_3_1 = (confusionMatrix(data = knn_p_3_1 , reference = factor(trainTransformed3$yhat) ,mode = "everything")))
(knn_roc_3_1 = ROSE::roc.curve(factor(trainTransformed3$yhat), predicted = knn_p_3_1))
knn_roc_3_1[2]


(output_knn = rbind(knn_test_1 = c(knn_cm_1$byClass , knn_cm_1$overall, knn_roc_1[2]) , knn_test_2 = c(knn_cm_2$byClass , knn_cm_2$overall, knn_roc_2[2]) , knn_test_3 = c(knn_cm_3$byClass , knn_cm_3$overall, knn_roc_3[2]),
                   knn_train_1 = c(knn_cm_1_1$byClass , knn_cm_1_1$overall, knn_roc_1_1[2]) , knn_train_2 = c(knn_cm_2_1$byClass , knn_cm_2_1$overall, knn_roc_2_1[2]) , knn_train_3 = c(knn_cm_3_1$byClass , knn_cm_3_1$overall, knn_roc_3_1[2])))
