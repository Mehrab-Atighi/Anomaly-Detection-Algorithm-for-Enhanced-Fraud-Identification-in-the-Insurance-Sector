S1 = Sys.time()

source(file = "Labaling.R")
path1 = "carclaims.csv"; response_column_number1 = 33 ;YesOrNo1 = TRUE
path2 = "insurance fraud claims.csv" ; response_column_number2 = 39 ;YesOrNo2 = TRUE;
path3 = "fraud_oracle.csv"; response_column_number3 = 16 ;YesOrNo3 = FALSE

# FinalModeling = function(
    models = c("ml" , "svm" , "xgb" , "rf" , "nb" , "knn" , "adab")  
                         percent1 = 0.5 ; percent2 = 0.8 ; percent3 = 0.9 
                         path = path1 ; response_column_number = response_column_number1
                         YesOrNo = YesOrNo1;
                         # ){
  percent1 = percent1 ; percent2 = percent2 ; percent3 = percent3 ; path = path ;
  response_column_number = response_column_number  ; YesOrNo = YesOrNo; models = models
  L = Labaling(path = path , response_column_number = response_column_number 
                      ,percent1 = percent1 , percent2 = percent2 , percent3 = percent3 , YesOrNo = YesOrNo)
  attach(L)
# load(file = "pre.RData")
# full_data_label1 =  outputs$full_data_label1  
# full_data_without_label1 = outputs$full_data_without_label1 
# full_data_label_nn1 =  outputs$full_data_label_nn1  
# full_data_without_label_nn1 = outputs$full_data_without_label_nn1 
# 
# full_data_label2 =  outputs$full_data_label2  
# full_data_without_label2 = outputs$full_data_without_label2 
# full_data_label_nn2 =  outputs$full_data_label_nn2  
# full_data_without_label_nn2 = outputs$full_data_without_label_nn2 
# full_data_label3 =  outputs$full_data_label3  
# full_data_without_label3 = outputs$full_data_without_label3 
# full_data_label_nn3 =  outputs$full_data_label_nn3
# full_data_without_label_nn3 = outputs$full_data_without_label_nn3 
# 
# full_data_label4 =  outputs$full_data_label4
# full_data_without_label4 = outputs$full_data_without_label4
# full_data_label_nn4 =  outputs$full_data_label_nn4
# full_data_without_label_nn4 = outputs$full_data_without_label_nn4
# numeric_columns = outputs$numeric_columns 
# categorical_columns = outputs$categorical_columns
# number_of_rows = outputs$nrw 
# train_p1_h2o = outputs$train_p1_h2o
# categorical_columns = outputs$categorical_columns
#   I1 = L$I1 
#   cm_1 = L$cm_1  
#   cm_2 = L$cm_2 
#   cm_3 = L$cm_3 
#   cm_4 = L$cm_4 
#   cm_5 = L$cm_5 
#   cm_6 = L$cm_6 
#   cm_7 = L$cm_7 
#   cm_8 = L$cm_8 
#   cm_9  = L$cm_9 
#   cm_total = L$cm_total 
#   Result_final_outliers = L$Result_final_outliers 
#   acc1_1 = L$acc1_1 
#   categorical_columns = L$categorical_columns
#   percent1 = L$percent1 
#   percent2 = L$percent2 
#   percent3 = L$percent3
  
modellen = length(models)
 
  
diff = setdiff(1:dim(full_data_without_label1)[1] , I1)
I11 = sample(diff , size = c(0.1*length(I1)) , replace = FALSE)
train_df = data.frame(full_data_without_label1[c(I1,I11) , ] , yhat = Result_final_outliers$label[c(I1 , I11) ])
train_df_Balance = ovun.sample(yhat ~., data = train_df, method = "over")$data



### Modeling rond 1:

#logistic

if(length(which(models == "ml")) == 1){

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

ml_1_index = which(ml_p_1 == 1)
ml_11_index = which(ml_p_1 == 0)
ml_I22 = sample(ml_11_index , 0.1*length(ml_1_index))
ml_I2 = ml_1_index
}else{
  ml_I2 = c()
}

#svm
if(length(which(models == "svm")) == 1){
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

svm_1_index = which(svm_p_1 == 1)
svm_11_index = which(svm_p_1 == 0)
svm_I22 = sample(svm_11_index , 0.1*length(svm_1_index))
svm_I2 = svm_1_index
}else{svm_I2 = c()}

#Xgboost
if(length(which(models == "xgb")) == 1){
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
xgb_1_index = which(xgb_p_1 == 1)
xgb_11_index = which(xgb_p_1 == 0)
xgb_I22 = sample(xgb_11_index , 0.1*length(xgb_1_index))
xgb_I2 = xgb_1_index}else{xgb_I2 = c()}

#random forest
if(length(which(models == "rf")) == 1){
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

rf_1_index = which(rf_p_1 == 1)
rf_11_index = which(rf_p_1 == 0)
rf_I22 = sample(rf_11_index , 0.1*length(rf_1_index))
rf_I2 = rf_1_index}else{rf_I2 = c()}

#naive bayes
if(length(which(models == "nb")) == 1){
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

nb_1_index = which(nb_p_1 == 1)
nb_11_index = which(nb_p_1 == 0)
nb_I22 = sample(nb_11_index , 0.1*length(nb_1_index))
nb_I2 =nb_1_index}else{nb_I2 = c()}


# Knn
if(length(which(models == "knn")) == 1){
  
preProcValues1 <- preProcess(train_df_Balance[,-c(categorical_columns,ncol(train_df_Balance))], method = c("center", "scale"))
trainTransformed1 <- cbind(predict(preProcValues1, train_df_Balance[,-c(categorical_columns,ncol(train_df_Balance))]), yhat = train_df_Balance$yhat)
testTransformed1 <- cbind(predict(preProcValues1, full_data_without_label2[,-c(categorical_columns,ncol(train_df_Balance))]) , yhat = full_data_label2)

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

knn_1_index = which(knn_p_1 == 1)
knn_11_index = which(knn_p_1 == 0)
knn_I22 = sample(knn_11_index , 0.1*length(knn_1_index))
knn_I2 = knn_1_index}else{knn_I2 = c()}

# adaboost
if(length(which(models == "adab")) == 1){
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

adab_1_index = which(adab_p_1$class == 1)
adab_11_index = which(adab_p_1$class == 0)
adab_I22 = sample(adab_11_index , 0.1*length(adab_1_index))
adab_I2 = adab_1_index}else{adab_I2 = c()}
### Balancing and editing Dataset2


qq = c(adab_I2,knn_I2 , nb_I2, rf_I2 , xgb_I2 , svm_I2 , ml_I2)

I2 = qq[which(table(qq) >= modellen/2)]
I22 = qq[which(table(qq) < modellen/2)]
I22 = sample(I22 , size = 0.1*length(I2) , replace = FALSE)

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
#multinomial logistic
if(length(which(models == "ml")) == 1){
MlModel2 = multinom(yhat ~ . , data = train_df_Balance2)
ml_p_2 = predict(MlModel2, newdata = full_data_without_label3 , type = "class")
(ml_cm_2 = (confusionMatrix(data = ml_p_2 , reference = factor(full_data_label3) ,mode = "everything")))
(ml_roc_2 = ROSE::roc.curve(factor(full_data_label3) , predicted = ml_p_2))
ml_roc_2[2]


ml_p_2_1 = predict(MlModel2, type = "class")
(ml_cm_2_1 = (confusionMatrix(data = ml_p_2_1 , reference = factor(train_df_Balance2$yhat) ,mode = "everything")))
(ml_roc_2_1 = ROSE::roc.curve(factor(train_df_Balance2$yhat) , predicted = ml_p_2_1))
ml_roc_2_1[2]

ml_2_index = which(ml_p_2 == 1)
ml_22_index = which(ml_p_2 == 0)
ml_I33 = sample(ml_22_index , 0.1*length(ml_2_index))
ml_I3 = ml_2_index
}else{ml_I3 = c()}
#svm 
if(length(which(models == "svm")) == 1){
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

svm_2_index = which(svm_p_2 == 1)
svm_22_index = which(svm_p_2 == 0)
svm_I33 = sample(svm_22_index , 0.1*length(svm_2_index))
svm_I3 = svm_2_index
}else{svm_I3 = c()}


#xgboost
if(length(which(models == "xgb")) == 1){
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
xgb_2_index = which(xgb_p_2 == 1)
xgb_22_index = which(xgb_p_2 == 0)
xgb_I33 = sample(xgb_22_index , 0.1*length(xgb_2_index))
xgb_I3 = xgb_2_index}else{xgb_I3 = c()}

#random forest 
if(length(which(models == "rf")) == 1){
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
rf_2_index = which(rf_p_2 == 1)
rf_22_index = which(rf_p_2 == 0)
rf_I33 = sample(rf_22_index , 0.1*length(rf_2_index))
rf_I3 = rf_2_index}else{rf_I3 = c()}


# naive bayes
if(length(which(models == "nb")) == 1){
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

nb_2_index = which(nb_p_2 == 1)
nb_22_index = which(nb_p_2 == 0)
nb_I33 = sample(nb_22_index , 0.1*length(nb_2_index))
nb_I3 =nb_2_index}else{nb_I3 = c()}


# knn
if(length(which(models == "knn")) == 1){
preProcValues2 <- preProcess(train_df_Balance2[,-c(categorical_columns,ncol(train_df_Balance2))], method = c("center", "scale"))
trainTransformed2 <- cbind(predict(preProcValues2, train_df_Balance2[,-c(categorical_columns,ncol(train_df_Balance2))]), yhat = train_df_Balance2$yhat)
testTransformed2 <- cbind(predict(preProcValues2, full_data_without_label3[,-c(categorical_columns,ncol(train_df_Balance2))]) , yhat = full_data_label3)

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

knn_2_index = which(knn_p_2 == 1)
knn_22_index = which(knn_p_2 == 0)
knn_I33 = sample(knn_22_index , 0.1*length(knn_2_index))
knn_I3 = knn_2_index}else{knn_I3 = c()}


# adaboost
if(length(which(models == "ml")) == 1){
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

adab_2_index = which(adab_p_2$class == 1)
adab_22_index = which(adab_p_2$class == 0)
adab_I33 = sample(adab_22_index , 0.1*length(adab_2_index))
adab_I3 = adab_2_index}else{adab_I3 = c()}

### Balancing and editing Dataset3

qq2 = c(adab_I3,knn_I3 , nb_I3, rf_I3 , xgb_I3 , svm_I3 , ml_I3)

I3 = qq2[which(table(qq2) >= modellen/2)]
I33 = qq2[which(table(qq2) < modellen/2)]
I33 = sample(I33 , size = 0.1*length(I3) , replace = FALSE)

Result_final_rond2 = data.frame(ID = (percent2*number_of_rows):(percent3*number_of_rows) , label = 0)
for(i in I3){
  Result_final_rond2$label[i] = 1
}

j = 0
jj = table(Result_final_rond2$label)
I3 = which(Result_final_rond2$label == 1) 
for(i in 1:nrow(Result_final_rond2)){
  if(Result_final_rond2$label[i] == 1){
    if(full_data_label3[i] == 0){
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
if(length(which(models == "ml")) == 1){
#multinomial logistic
MlModel3 = multinom(yhat ~ . , data = train_df_Balance3)
ml_p_3 = predict(MlModel3, newdata = full_data_without_label4 , type = "class")
(ml_cm_3 = (confusionMatrix(data = ml_p_3 , reference = factor(full_data_label4) ,mode = "everything")))
(ml_roc_3 = ROSE::roc.curve(factor(full_data_label4) , predicted = ml_p_3))
ml_roc_3[2]

ml_p_3_1 = predict(MlModel3, type = "class")
(ml_cm_3_1 = (confusionMatrix(data = ml_p_3_1 , reference = factor(train_df_Balance3$yhat) ,mode = "everything")))
(ml_roc_3_1 = ROSE::roc.curve(factor(train_df_Balance3$yhat) , predicted = ml_p_3_1))
ml_roc_3_1[2]}else{ml_cm_3 = c();ml_cm_3_1 = c();ml_roc_3 = c() ; ml_roc_3_1 = c()}


#svm 
if(length(which(models == "svm")) == 1){
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
svm_roc_3_1[2]}else{svm_cm_3 = c();svm_cm_3_1 = c();svm_roc_3 = c() ; svm_roc_3_1 = c()}


#xgboost
if(length(which(models == "xgb")) == 1){
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
xgb_roc_3_1[2]}else{xgb_cm_3 = c();xgb_cm_3_1 = c();xgb_roc_3 = c() ; xgb_roc_3_1 = c()}


#random forest
if(length(which(models == "rf")) == 1){
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
rf_roc_3_1[2]}else{rf_cm_3 = c();rf_cm_3_1 = c();rf_roc_3 = c() ; rf_roc_3_1 = c()}


#Naive Bayes
if(length(which(models == "nb")) == 1){
NbModel3 = naive_bayes(yhat ~ ., data = train_df_Balance)


nb_p_3 <- predict(NbModel3, full_data_without_label4,type = "class")
# Calculate confusion matrix
(nb_cm_3 <- confusionMatrix(nb_p_3, full_data_label4, mode = "everything"))
(nb_roc_3 = ROSE::roc.curve(factor(full_data_label4) , predicted =nb_p_3))
nb_roc_3[2]


nb_p_3_1 = predict(NbModel3, train_df_Balance3 , type = "class")
(nb_cm_3_1 = (confusionMatrix(data =nb_p_3_1 , reference = factor(train_df_Balance3$yhat) ,mode = "everything")))
(nb_roc_3_1 = ROSE::roc.curve(factor(train_df_Balance3$yhat) , predicted =nb_p_3_1))
nb_roc_3_1[2]}else{nb_cm_3 = c();nb_cm_3_1 = c();nb_roc_3 = c() ; nb_roc_3_1 = c()}

#Knn
if(length(which(models == "knn")) == 1){
preProcValues3<- preProcess(train_df_Balance3[,-c(categorical_columns,ncol(train_df_Balance3))], method = c("center", "scale"))
trainTransformed3 <- cbind(predict(preProcValues3, train_df_Balance3[,-c(categorical_columns,ncol(train_df_Balance3))]), yhat = train_df_Balance3$yhat)
testTransformed3 <- cbind(predict(preProcValues3, full_data_without_label4[,-c(categorical_columns,ncol(train_df_Balance3))]) , yhat = full_data_label4)
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
knn_roc_3_1[2]}else{knn_cm_3 = c();knn_cm_3_1 = c();knn_roc_3 = c() ; knn_roc_3_1 = c()}

#Adaboost
if(length(which(models == "adab")) == 1){
AdabModel3 = boosting(yhat ~ ., data = train_df_Balance3, mfinal = 100) 


adab_p_3 <- predict(AdabModel3, full_data_without_label4,type = "class")
# Calculate confusion matrix
(adab_cm_3 <- confusionMatrix(factor(adab_p_3$class), full_data_label4, mode = "everything"))
(adab_roc_3 = ROSE::roc.curve(factor(full_data_label4) , predicted = factor(adab_p_3$class)))
adab_roc_3[2]


adab_p_3_1 = predict(AdabModel3, train_df_Balance3 , type = "class")
(adab_cm_3_1 = (confusionMatrix(data = factor(adab_p_3_1$class) , reference = factor(train_df_Balance3$yhat) ,mode = "everything")))
(adab_roc_3_1 = ROSE::roc.curve(factor(train_df_Balance3$yhat) , predicted = factor(adab_p_3_1$class)))
adab_roc_3_1[2]}else{adab_cm_3 = c();adab_cm_3_1 = c();adab_roc_3 = c() ; adab_roc_3_1 = c()}

###Making outputs

#multinomial logistic
output_ml = rbind(ml_test_1 = c(ml_cm_1$byClass , ml_cm_1$overall, ml_roc_1[2]) , ml_test_2 = c(ml_cm_2$byClass , ml_cm_2$overall, ml_roc_2[2]) , ml_test_3 = c(ml_cm_3$byClass , ml_cm_3$overall, ml_roc_3[2]),
                   ml_train_1 = c(ml_cm_1_1$byClass , ml_cm_1_1$overall, ml_roc_1_1[2]) , ml_train_2 = c(ml_cm_2_1$byClass , ml_cm_2_1$overall, ml_roc_2_1[2]) , ml_train_3 = c(ml_cm_3_1$byClass , ml_cm_3_1$overall, ml_roc_3_1[2]))
#svm
output_svm = rbind(svm_test_1 = c(svm_cm_1$byClass , svm_cm_1$overall, svm_roc_1[2]) , svm_test_2 = c(svm_cm_2$byClass , svm_cm_2$overall, svm_roc_2[2]) , svm_test_3 = c(svm_cm_3$byClass , svm_cm_3$overall, svm_roc_3[2]),
                   svm_train_1 = c(svm_cm_1_1$byClass , svm_cm_1_1$overall, svm_roc_1_1[2]) , svm_train_2 = c(svm_cm_2_1$byClass , svm_cm_2_1$overall, svm_roc_2_1[2]) , svm_train_3 = c(svm_cm_3_1$byClass , svm_cm_3_1$overall, svm_roc_3_1[2]))
#xgboost
output_xgb = rbind(xgb_test_1 = c(xgb_cm_1$byClass , xgb_cm_1$overall, xgb_roc_1[2]) , xgb_test_2 = c(xgb_cm_2$byClass , xgb_cm_2$overall, xgb_roc_2[2]) , xgb_test_3 = c(xgb_cm_3$byClass , xgb_cm_3$overall, xgb_roc_3[2]),
                   xgb_train_1 = c(xgb_cm_1_1$byClass , xgb_cm_1_1$overall, xgb_roc_1_1[2]) , xgb_train_2 = c(xgb_cm_2_1$byClass , xgb_cm_2_1$overall, xgb_roc_2_1[2]) , xgb_train_3 = c(xgb_cm_3_1$byClass , xgb_cm_3_1$overall, xgb_roc_3_1[2]))
#random forest
output_rf = rbind(rf_test_1 = c(rf_cm_1$byClass , rf_cm_1$overall, rf_roc_1[2]) , rf_test_2 = c(rf_cm_2$byClass , rf_cm_2$overall, rf_roc_2[2]) , rf_test_3 = c(rf_cm_3$byClass , rf_cm_3$overall, rf_roc_3[2]),
                  rf_train_1 = c(rf_cm_1_1$byClass , rf_cm_1_1$overall, rf_roc_1_1[2]) , rf_train_2 = c(rf_cm_2_1$byClass , rf_cm_2_1$overall, rf_roc_2_1[2]) , rf_train_3 = c(rf_cm_3_1$byClass , rf_cm_3_1$overall, rf_roc_3_1[2]))
#naive bayes
output_nb = rbind(nb_test_1 = c(nb_cm_1$byClass ,nb_cm_1$overall,nb_roc_1[2]) ,nb_test_2 = c(nb_cm_2$byClass ,nb_cm_2$overall,nb_roc_2[2]) ,nb_test_3 = c(nb_cm_3$byClass ,nb_cm_3$overall,nb_roc_3[2]),
                  nb_train_1 = c(nb_cm_1_1$byClass ,nb_cm_1_1$overall,nb_roc_1_1[2]) ,nb_train_2 = c(nb_cm_2_1$byClass ,nb_cm_2_1$overall,nb_roc_2_1[2]) ,nb_train_3 = c(nb_cm_3_1$byClass ,nb_cm_3_1$overall,nb_roc_3_1[2]))
#knn
output_knn = rbind(knn_test_1 = c(knn_cm_1$byClass , knn_cm_1$overall, knn_roc_1[2]) , knn_test_2 = c(knn_cm_2$byClass , knn_cm_2$overall, knn_roc_2[2]) , knn_test_3 = c(knn_cm_3$byClass , knn_cm_3$overall, knn_roc_3[2]),
                   knn_train_1 = c(knn_cm_1_1$byClass , knn_cm_1_1$overall, knn_roc_1_1[2]) , knn_train_2 = c(knn_cm_2_1$byClass , knn_cm_2_1$overall, knn_roc_2_1[2]) , knn_train_3 = c(knn_cm_3_1$byClass , knn_cm_3_1$overall, knn_roc_3_1[2]))
#adab
output_adab = rbind(adab_test_1 = c(adab_cm_1$byClass , adab_cm_1$overall, adab_roc_1[2]) , adab_test_2 = c(adab_cm_2$byClass , adab_cm_2$overall, adab_roc_2[2]) , adab_test_3 = c(adab_cm_3$byClass , adab_cm_3$overall, adab_roc_3[2]),
                    adab_train_1 = c(adab_cm_1_1$byClass , adab_cm_1_1$overall, adab_roc_1_1[2]) , adab_train_2 = c(adab_cm_2_1$byClass , adab_cm_2_1$overall, adab_roc_2_1[2]) , adab_train_3 = c(adab_cm_3_1$byClass , adab_cm_3_1$overall, adab_roc_3_1[2]))


(final_outputs = rbind(svm = output_svm , ml = output_ml 
                      , adab = output_adab , knn = output_knn , nb = output_nb , 
                         rf = output_rf , xgb = output_xgb))
# return(final_outputs)
# }

# m1 = FinalModeling(models = c("ml" , "svm" , "xgb" , "rf" , "nb" , "knn" , "adab") , 
#                    percent1 = 0.5 , percent2 = 0.8 , percent3 = 0.9 ,
#                    path = "carclaims.csv" , response_column_number = 33  ,
#                    YesOrNo = TRUE)
write.csv(final_outputs , paste0("Result_of_" , path))
S2 = Sys.time()