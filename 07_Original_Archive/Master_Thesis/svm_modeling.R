#Support Vector Machine Modeling
# percent1 = 0.5
# percent2 = 0.8
# percent3 = 0.9 
# source(file = "Labaling.R")
# Labaling = Labaling(path = "carclaims.csv" , response_column_number = 33 
#                     ,percent1 = percent1 , percent2 = percent2 , percent3 = percent3)
# attach(Labaling)
### Balancing and editing Dataset
diff = setdiff(1:dim(full_data_without_label1)[1] , I1)
I11 = sample(diff , size = c(0.1*length(I1)) , replace = FALSE)
train_df = data.frame(full_data_without_label1[c(I1,I11) , ] , yhat = Result_final_outliers$label[c(I1 , I11) ])
train_df_Balance = ovun.sample(yhat ~., data = train_df, method = "over")$data


### Modeling rond 1:
library(e1071)

SvmModel1 = svm(formula = yhat ~ .,
               data = train_df_Balance,
               type = 'C-classification',
               kernel = 'radial')

svm_p_1 <- predict(SvmModel1, full_data_without_label2,type = "class")
# Calculate confusion matrix
(svm_cm_1 <- confusionMatrix(svm_p_1, full_data_label2, mode = "everything"))
(svm_roc_1 = ROSE::roc.curve(factor(full_data_label2) , predicted = svm_p_1))
svm_roc_1[2]

svm_p_1_1 = predict(SvmModel1, train_df_Balance , type = "class")
(svm_cm_1_1 = (confusionMatrix(data = svm_p_1_1 , reference = factor(train_df_Balance$yhat) ,mode = "everything")))
(svm_roc_1_1 = ROSE::roc.curve(factor(train_df_Balance$yhat) , predicted = svm_p_1_1))
svm_roc_1_1[2]
### Balancing and editing Dataset2

svm_1_index = which(svm_p_1 == 1)
svm_11_index = which(svm_p_1 == 0)
I22 = sample(svm_11_index , 0.1*length(svm_1_index))
I2 = svm_1_index
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

SvmModel2 = svm(formula = yhat ~ .,
                data = train_df_Balance2,
                type = 'C-classification',
                kernel = 'radial')

svm_p_2 <- predict(SvmModel2, full_data_without_label3,type = "class")
# Calculate confusion matrix
(svm_cm_2 <- confusionMatrix(svm_p_2, full_data_label3, mode = "everything"))
(svm_roc_2 = ROSE::roc.curve(factor(full_data_label3) , predicted = svm_p_2))
svm_roc_2[2]

svm_p_2_1 = predict(SvmModel2, train_df_Balance2 , type = "class")
(svm_cm_2_1 = (confusionMatrix(data = svm_p_2_1 , reference = factor(train_df_Balance2$yhat) ,mode = "everything")))
(svm_roc_2_1 = ROSE::roc.curve(factor(train_df_Balance2$yhat) , predicted = svm_p_2_1))
svm_roc_2_1[2]
### Balancing and editing Dataset3

svm_2_index = which(svm_p_2 == 1)
svm_22_index = which(svm_p_2 == 0)
I33 = sample(svm_22_index , 0.1*length(svm_2_index))
I3 = svm_2_index
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
SvmModel3 = svm(formula = yhat ~ .,
                data = train_df_Balance3,
                type = 'C-classification',
                kernel = 'radial')

svm_p_3 <- predict(SvmModel3, full_data_without_label4,type = "class")
# Calculate confusion matrix
(svm_cm_3 <- confusionMatrix(svm_p_3, full_data_label4, mode = "everything"))
(svm_roc_3 = ROSE::roc.curve(factor(full_data_label4) , predicted = svm_p_3))
svm_roc_3[2]


svm_p_3_1 = predict(SvmModel3, train_df_Balance3 , type = "class")
(svm_cm_3_1 = (confusionMatrix(data = svm_p_3_1 , reference = factor(train_df_Balance3$yhat) ,mode = "everything")))
(svm_roc_3_1 = ROSE::roc.curve(factor(train_df_Balance3$yhat) , predicted = svm_p_3_1))
svm_roc_3_1[2]


(output_svm = rbind(svm_test_1 = c(svm_cm_1$byClass , svm_cm_1$overall, svm_roc_1[2]) , svm_test_2 = c(svm_cm_2$byClass , svm_cm_2$overall, svm_roc_2[2]) , svm_test_3 = c(svm_cm_3$byClass , svm_cm_3$overall, svm_roc_3[2]),
                    svm_train_1 = c(svm_cm_1_1$byClass , svm_cm_1_1$overall, svm_roc_1_1[2]) , svm_train_2 = c(svm_cm_2_1$byClass , svm_cm_2_1$overall, svm_roc_2_1[2]) , svm_train_3 = c(svm_cm_3_1$byClass , svm_cm_3_1$overall, svm_roc_3_1[2])))
