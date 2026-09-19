
#Naive Bayes Modeling
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
library(naivebayes)

NbModel1 = naive_bayes(yhat ~ ., data = train_df_Balance)

nb_p_1 <- predict(NbModel1, full_data_without_label2,type = "class")
# Calculate confusion matrix
(nb_cm_1 <- confusionMatrix(nb_p_1, full_data_label2, mode = "everything"))
(nb_roc_1 = ROSE::roc.curve(factor(full_data_label2) , predicted =nb_p_1))
nb_roc_1[2]

nb_p_1_1 = predict(NbModel1, type = "class")
(nb_cm_1_1 = (confusionMatrix(data =nb_p_1_1 , reference = factor(train_df_Balance$yhat) ,mode = "everything")))
(nb_roc_1_1 = ROSE::roc.curve(factor(train_df_Balance$yhat) , predicted =nb_p_1_1))
nb_roc_1_1[2]
### Balancing and editing Dataset2

nb_1_index = which(nb_p_1 == 1)
nb_11_index = which(nb_p_1 == 0)
I22 = sample(nb_11_index , 0.1*length(nb_1_index))
I2 =nb_1_index
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

NbModel2 = naive_bayes(yhat ~ ., data = train_df_Balance)


nb_p_2 <- predict(NbModel2, full_data_without_label3,type = "class")
# Calculate confusion matrix
(nb_cm_2 <- confusionMatrix(nb_p_2, full_data_label3, mode = "everything"))
(nb_roc_2 = ROSE::roc.curve(factor(full_data_label3) , predicted =nb_p_2))
nb_roc_2[2]

nb_p_2_1 = predict(NbModel2, train_df_Balance2 , type = "class")
(nb_cm_2_1 = (confusionMatrix(data =nb_p_2_1 , reference = factor(train_df_Balance2$yhat) ,mode = "everything")))
(nb_roc_2_1 = ROSE::roc.curve(factor(train_df_Balance2$yhat) , predicted =nb_p_2_1))
nb_roc_2_1[2]
### Balancing and editing Dataset3

nb_2_index = which(nb_p_2 == 1)
nb_22_index = which(nb_p_2 == 0)
I33 = sample(nb_22_index , 0.1*length(nb_2_index))
I3 =nb_2_index
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
NbModel3 = naive_bayes(yhat ~ ., data = train_df_Balance)


nb_p_3 <- predict(NbModel3, full_data_without_label4,type = "class")
# Calculate confusion matrix
(nb_cm_3 <- confusionMatrix(nb_p_3, full_data_label4, mode = "everything"))
(nb_roc_3 = ROSE::roc.curve(factor(full_data_label4) , predicted =nb_p_3))
nb_roc_3[2]


nb_p_3_1 = predict(NbModel3, train_df_Balance3 , type = "class")
(nb_cm_3_1 = (confusionMatrix(data =nb_p_3_1 , reference = factor(train_df_Balance3$yhat) ,mode = "everything")))
(nb_roc_3_1 = ROSE::roc.curve(factor(train_df_Balance3$yhat) , predicted =nb_p_3_1))
nb_roc_3_1[2]


(output_nb = rbind(nb_test_1 = c(nb_cm_1$byClass ,nb_cm_1$overall,nb_roc_1[2]) ,nb_test_2 = c(nb_cm_2$byClass ,nb_cm_2$overall,nb_roc_2[2]) ,nb_test_3 = c(nb_cm_3$byClass ,nb_cm_3$overall,nb_roc_3[2]),
                   nb_train_1 = c(nb_cm_1_1$byClass ,nb_cm_1_1$overall,nb_roc_1_1[2]) ,nb_train_2 = c(nb_cm_2_1$byClass ,nb_cm_2_1$overall,nb_roc_2_1[2]) ,nb_train_3 = c(nb_cm_3_1$byClass ,nb_cm_3_1$overall,nb_roc_3_1[2])))
