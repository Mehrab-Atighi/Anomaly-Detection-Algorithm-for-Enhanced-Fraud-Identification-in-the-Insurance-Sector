#Multinomial logistic Modeling
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
  
  
  ### Modeling rond 1:
  library(nnet)
  library(caret)
  
  MlModel1 = multinom(yhat ~ . , data = train_df_Balance)
  ml_p_1 = predict(MlModel1, newdata = full_data_without_label2 , type = "class")
  (ml_cm_1 = (confusionMatrix(data = ml_p_1 , reference = factor(full_data_label2) ,mode = "everything")))
  (ml_roc_1 = ROSE::roc.curve(factor(full_data_label2) , predicted = ml_p_1))
  ml_roc_1[2]
  
  
  ml_p_1_1 = predict(MlModel1, type = "class")
  (ml_cm_1_1 = (confusionMatrix(data = ml_p_1_1 , reference = factor(train_df_Balance$yhat) ,mode = "everything")))
  (ml_roc_1_1 = ROSE::roc.curve(factor(train_df_Balance$yhat) , predicted = ml_p_1_1))
  ml_roc_1_1[2]
  ### Balancing and editing Dataset2
  
  ml_1_index = which(ml_p_1 == 1)
  ml_11_index = which(ml_p_1 == 0)
  I22 = sample(ml_11_index , 0.1*length(ml_1_index))
  I2 = ml_1_index
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
  
  train_df2 = data.frame(full_data_without_label2[c(I2, I22) , ] , yhat = Result_final_rond1$label[c(I2,I22)])
  train_df2 = rbind(train_df , train_df2)
  train_df_Balance2 = ovun.sample(yhat ~., data = train_df2, method = "over" )$data
  
  ### Modeling rond 2:
  
  MlModel2 = multinom(yhat ~ . , data = train_df_Balance2)
  ml_p_2 = predict(MlModel2, newdata = full_data_without_label3 , type = "class")
  (ml_cm_2 = (confusionMatrix(data = ml_p_2 , reference = factor(full_data_label3) ,mode = "everything")))
  (ml_roc_2 = ROSE::roc.curve(factor(full_data_label3) , predicted = ml_p_2))
  ml_roc_2[2]
  
  
  ml_p_2_1 = predict(MlModel2, type = "class")
  (ml_cm_2_1 = (confusionMatrix(data = ml_p_2_1 , reference = factor(train_df_Balance2$yhat) ,mode = "everything")))
  (ml_roc_2_1 = ROSE::roc.curve(factor(train_df_Balance2$yhat) , predicted = ml_p_2_1))
  ml_roc_2_1[2]
  
  
  ### Balancing and editing Dataset3
  
  ml_2_index = which(ml_p_2 == 1)
  ml_22_index = which(ml_p_2 == 0)
  I33 = sample(ml_22_index , 0.1*length(ml_2_index))
  I3 = ml_2_index
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
  
  train_df3 = data.frame(full_data_without_label3[c(I3,I33) , ] , yhat = Result_final_rond2$label[c(I3 , I33)])
  train_df3 = rbind(train_df , train_df2 , train_df3)
  train_df_Balance3 = ovun.sample(yhat ~., data = train_df3, method = "over" )$data
  
  
  ### Modeling rond 3:
  
  MlModel3 = multinom(yhat ~ . , data = train_df_Balance3)
  ml_p_3 = predict(MlModel3, newdata = full_data_without_label4 , type = "class")
  (ml_cm_3 = (confusionMatrix(data = ml_p_3 , reference = factor(full_data_label4) ,mode = "everything")))
  (ml_roc_3 = ROSE::roc.curve(factor(full_data_label4) , predicted = ml_p_3))
  ml_roc_3[2]
  
  ml_p_3_1 = predict(MlModel3, type = "class")
  (ml_cm_3_1 = (confusionMatrix(data = ml_p_3_1 , reference = factor(train_df_Balance3$yhat) ,mode = "everything")))
  (ml_roc_3_1 = ROSE::roc.curve(factor(train_df_Balance3$yhat) , predicted = ml_p_3_1))
  ml_roc_3_1[2]
  
  
  (output_ml = rbind(ml_test_1 = c(ml_cm_1$byClass , ml_cm_1$overall, ml_roc_1[2]) , ml_test_2 = c(ml_cm_2$byClass , ml_cm_2$overall, ml_roc_2[2]) , ml_test_3 = c(ml_cm_3$byClass , ml_cm_3$overall, ml_roc_3[2]),
    ml_train_1 = c(ml_cm_1_1$byClass , ml_cm_1_1$overall, ml_roc_1_1[2]) , ml_train_2 = c(ml_cm_2_1$byClass , ml_cm_2_1$overall, ml_roc_2_1[2]) , ml_train_3 = c(ml_cm_3_1$byClass , ml_cm_3_1$overall, ml_roc_3_1[2])))