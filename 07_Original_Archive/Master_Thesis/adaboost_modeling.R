#Adaboost Modeling

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
library(adabag)

AdabModel1 = boosting(yhat ~ ., data = train_df_Balance, mfinal = 100) 
# mfinal: number of boosting iterations (trees)


adab_p_1 <- predict(AdabModel1, full_data_without_label2 , type = "class")
# Calculate confusion matrix
(adab_cm_1 <- confusionMatrix(factor(adab_p_1$class), full_data_label2, mode = "everything"))
(adab_roc_1 = ROSE::roc.curve(factor(full_data_label2) , predicted = factor(adab_p_1$class)))
adab_roc_1[2]

adab_p_1_1 = predict(AdabModel1, train_df_Balance , type = "class")
(adab_cm_1_1 = (confusionMatrix(data = factor(adab_p_1_1$class) , reference = factor(train_df_Balance$yhat) ,mode = "everything")))
(adab_roc_1_1 = ROSE::roc.curve(factor(train_df_Balance$yhat) , predicted = factor(adab_p_1_1$class)))
adab_roc_1_1[2]
### Balancing and editing Dataset2

adab_1_index = which(adab_p_1$class == 1)
adab_11_index = which(adab_p_1$class == 0)
I22 = sample(adab_11_index , 0.1*length(adab_1_index))
I2 = adab_1_index
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

AdabModel2 = boosting(yhat ~ ., data = train_df_Balance2, mfinal = 100) 


adab_p_2 <- predict(AdabModel2, full_data_without_label3,type = "class")
# Calculate confusion matrix
(adab_cm_2 <- confusionMatrix(factor(adab_p_2$class), full_data_label3, mode = "everything"))
(adab_roc_2 = ROSE::roc.curve(factor(full_data_label3) , predicted = factor(adab_p_2$class)))
adab_roc_2[2]

adab_p_2_1 = predict(AdabModel2, train_df_Balance2 , type = "class")
(adab_cm_2_1 = (confusionMatrix(data = factor(adab_p_2_1$class) , reference = factor(train_df_Balance2$yhat) ,mode = "everything")))
(adab_roc_2_1 = ROSE::roc.curve(factor(train_df_Balance2$yhat) , predicted = factor(adab_p_2_1$class)))
adab_roc_2_1[2]
### Balancing and editing Dataset3

adab_2_index = which(adab_p_2$class == 1)
adab_22_index = which(adab_p_2$class == 0)
I33 = sample(adab_22_index , 0.1*length(adab_2_index))
I3 = adab_2_index
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
AdabModel3 = boosting(yhat ~ ., data = train_df_Balance3, mfinal = 100) 


adab_p_3 <- predict(AdabModel3, full_data_without_label4,type = "class")
# Calculate confusion matrix
(adab_cm_3 <- confusionMatrix(factor(adab_p_3$class), full_data_label4, mode = "everything"))
(adab_roc_3 = ROSE::roc.curve(factor(full_data_label4) , predicted = factor(adab_p_3$class)))
adab_roc_3[2]


adab_p_3_1 = predict(AdabModel3, train_df_Balance3 , type = "class")
(adab_cm_3_1 = (confusionMatrix(data = factor(adab_p_3_1$class) , reference = factor(train_df_Balance3$yhat) ,mode = "everything")))
(adab_roc_3_1 = ROSE::roc.curve(factor(train_df_Balance3$yhat) , predicted = factor(adab_p_3_1$class)))
adab_roc_3_1[2]


(output_adab = rbind(adab_test_1 = c(adab_cm_1$byClass , adab_cm_1$overall, adab_roc_1[2]) , adab_test_2 = c(adab_cm_2$byClass , adab_cm_2$overall, adab_roc_2[2]) , adab_test_3 = c(adab_cm_3$byClass , adab_cm_3$overall, adab_roc_3[2]),
                    adab_train_1 = c(adab_cm_1_1$byClass , adab_cm_1_1$overall, adab_roc_1_1[2]) , adab_train_2 = c(adab_cm_2_1$byClass , adab_cm_2_1$overall, adab_roc_2_1[2]) , adab_train_3 = c(adab_cm_3_1$byClass , adab_cm_3_1$overall, adab_roc_3_1[2])))
