### input dataset :
library(ROSE)
data = read.csv("carclaims.csv")
#set.seed(1)
s1 = sample(1:nrow(data))
data = data[s1,]
library(fastDummies)
data[,33] = factor(ifelse(data[,33] == "Yes" , 1 , 0))
response = data[,33]
categorical_columns = as.numeric(which(sapply(data[,-33], is.character)))
numeric_columns = as.numeric(which(sapply(data[,-33], is.numeric)))

data_new = data.frame(dummy_cols(data[,categorical_columns] , remove_selected_columns = TRUE , remove_first_dummy = TRUE) ,data[,numeric_columns],dummy_cols(data[,33]))
dimention = dim(data_new)
data_new = array(unlist(data_new), dim = c(dimention[1],dimention[2]))
for(i in 1:ncol(data_new)-2){
  data_new[,i] = as.double(data_new[,i])
}

data = data_new
#View(data)

### Check missing values:

data = na.omit(data)

### select response column (جداسازی از داده ها)

## here we should get response column number from system
# we should chek that fraud is yes? no? or 0 1 ? which one?
response_column_number = c(dim(data_new)[2] , dim(data_new)[2]-1)

full_data_label = data[,response_column_number]
full_data_without_label =data[,-c(response_column_number)]


categorical_columns = 1:(dim(full_data_without_label)[2] - length(numeric_columns)-1)
# for(i in categorical_columns){
#   full_data_without_label[,i] = as.factor(full_data_without_label[,i])
# }
numeric_columns = (max(categorical_columns)+1):(max(categorical_columns)+1 + length(numeric_columns))
#calculate gini index:
j = 1 ; gini = c()
for(i in categorical_columns){
  my_table <- table(full_data_without_label[,i])
  my_prop <- prop.table(my_table)
  gini[j] <- 1 - sum(my_prop^2)
  j = j+1
}

#library(randomForest)
# random forest method
# regressor <-randomForest(full_data_label ~ . , data = full_data_without_label[,categorical_columns] , importance=TRUE)
# varImpPlot(regressor)
# imp = importance(regressor)
# sort meanDecreaseGini 
d1 = which(gini  > quantile(gini , probs = 0.1) & (gini < quantile(gini , probs = 0.9)))  # or use mean or median?

# continouse columns :
library(caret)
NearZeroVarince = nearZeroVar(full_data_without_label[,numeric_columns] , saveMetrics = TRUE , names = TRUE)
#sort near zero variance variable
d2 = cbind(rownames(NearZeroVarince) , NearZeroVarince[,4])

#now we want to make a newdata frame with important variables: 
qq2 = d2[which(d2[,2] == "FALSE"),1]
numeric_columns2 = c(); j= 1
categorical_columns2 =categorical_columns[d1]
for(i in 1:length(NearZeroVarince$nzv)){
  if(NearZeroVarince$nzv[i] == FALSE){
    numeric_columns2[j]  = i
    j = j +1
  }
}


important_columns = union(numeric_columns2 , categorical_columns2)

### Select 20% of data for find outliers
dim_data = dim(full_data_without_label)
dd3 = round (0.5 * dim_data[1]) 
### find outliers index for categorical columns with voting between models ((1/2 + 1) ==> 1)



r1 = c()
r_total_1 = c()

## A. Boxplot method ##
for(i in numeric_columns2){
  out <- boxplot.stats(full_data_without_label[1:dd3 , i])$out
  h = length(which(full_data_without_label[1:dd3 , i] %in% c(out)))
  if(h >0 ){
    r1 <- c(r1,which(full_data_without_label[1:dd3 , i] %in% c(out)))
  }
}
r1 = unique(r1)
Result1 = data.frame(ID = 1:dd3 , label = 0)
for(i in 1:nrow(Result1)){
  if(sum(r1 == i)>0){
    Result1$label[i] = 1
  }
}
r_total_1 = c(r_total_1 , r1)
Result1$label = as.factor(Result1$label)
(cm_1 = confusionMatrix(data = Result1$label , reference = response[1:dd3] , mode = "everything"))
auc1 = roc.curve(response[1:dd3] , predicted = Result1$label)
auc1[2]
## B.Percntile ##
r2 = c()

for(i in numeric_columns2){
  lower_bound <- quantile(full_data_without_label[1:dd3,i] , 0.05)
  upper_bound <- quantile(full_data_without_label[1:dd3,i] , 0.95)
  h = length(which(full_data_without_label[1:dd3,i] < lower_bound | full_data_without_label[1:dd3,i] > upper_bound))
  if(h>0){
    r2<- c(r2,which(full_data_without_label[1:dd3,i] < lower_bound | full_data_without_label[1:dd3,i] > upper_bound))
  }
}
r2 = unique(r2)
Result2 = data.frame(ID = 1:dd3, label = 0)
for(i in 1:nrow(Result2)){
  if(sum(r2 == i)>0){
    Result2$label[i] = 1
  }
}
r_total_1 = c(r_total_1 , r2)
Result2$label = as.factor(Result2$label)
(cm_2 = confusionMatrix(data = Result2$label , reference = response[1:dd3],mode = "everything"))
auc2 = roc.curve(response[1:dd3] , predicted = Result2$label)
auc2[2]

## C.Zscore ##
r3 = c()
for(i in numeric_columns2){
  zData = scale(full_data_without_label[,i])
  h = length(which(zData > 3.29 | zData < -3.29))
  if(h>0){
    r3 = c(r3,which(zData > 3.29 | zData < -3.29))
  }
}
r3 = unique(r3)
Result3 = data.frame(ID = 1:dd3 , label = 0)
for(i in 1:nrow(Result3)){
  if(sum(r3 == i)>0){
    Result3$label[i] = 1
  }
}
r_total_1 = c(r_total_1 , r3)
Result3$label = as.factor(Result3$label)
(cm_3 = confusionMatrix(data = Result3$label , reference = response[1:dd3],mode = "everything"))
auc3 = roc.curve(response[1:dd3] , predicted = Result3$label)
auc3[2]




## D.Hampel filter ##
r4 = c()
for(i in numeric_columns2){
  lower_bound <- median(full_data_without_label[1:dd3,i]) - 3 * mad(full_data_without_label[1:dd3,i], constant = 1)
  upper_bound <- median(full_data_without_label[1:dd3,i]) + 3 * mad(full_data_without_label[1:dd3,i], constant = 1)
  h = length(which(full_data_without_label[1:dd3,i] < lower_bound | full_data_without_label[1:dd3,i] > upper_bound))
  if(h>0){
    r4 <- c(r4,which(full_data_without_label[1:dd3,i] < lower_bound | full_data_without_label[1:dd3,i] > upper_bound))
  }
  
}
r4 = unique(r4)
r_total_1 = c(r_total_1 , r4)
Result4 = data.frame(ID = 1:dd3 , label = 0)
for(i in 1:nrow(Result4)){
  if(sum(r4 == i)>0){
    Result4$label[i] = 1
  }
}
r_total_1 = c(r_total_1 , r4)
Result4$label = as.factor(Result4$label)
(cm_4 = confusionMatrix(data = Result4$label , reference = response[1:dd3],mode = "everything"))
auc4 = roc.curve(response[1:dd3] , predicted = Result4$label)
auc4[2]


## E Grubbs’s test ##
# The Grubbs test allows to detect whether the highest or lowest value in a dataset is an outlier.
# The Grubbs test detects one outlier at a time (highest or lowest value), so the null and alternative hypotheses are as follows:
# Null Hypothesis: The highest value is not an outlier
# Alternative hypothesis: The highest value is an outlier

library(tidyverse)
library(outliers)
r5 = c()
for(i in numeric_columns2){
  (test1 <- grubbs.test(full_data_without_label[1:dd3,i]))
  (test2 <- grubbs.test(full_data_without_label[1:dd3,i], opposite = TRUE))
  
  df = data.frame(x = c(test1$alternative , test2$alternative) , PValue = c(test1$p.value, test2$p.value))
  df = separate(df , col = x  ,into =  c("A" , "B" ,"C" , "D" , "E" , "F") , sep = " ")
  if(df$PValue[1] <= 0.05){
    h = length(which(full_data_without_label[1:dd3,i] == as.numeric(df$C[1])) )
    if(h > 0 ){
      r5 = c(r,which(full_data_without_label[1:dd3,i] == as.numeric(df$C[1])))
    }
  }
  if(df$PValue[2] <= 0.05){
    h =length(which(full_data_without_label[1:dd3,i] == as.numeric(df$C[2])))
    if( h > 0 ){
      r5 = c(r,which(full_data_without_label[1:dd3,i] == as.numeric(df$C[2])))
    }
  }
}
r5 = unique(r5)
r_total_1 = c(r_total_1 , r5)
Result5 = data.frame(ID = 1:dd3 , label = 0)
for(i in 1:nrow(Result5)){
  if(sum(r5 == i)>0){
    Result5$label[i] = 1
  }
}
r_total_1 = c(r_total_1 , r5)
Result5$label = as.factor(Result5$label)
(cm_5 = confusionMatrix(data = Result5$label , reference = response[1:dd3],mode = "everything"))
auc5 = roc.curve(response[1:dd3] , predicted = Result5$label)
auc5[2]

## Rosner’s test ##
library(EnvStats)
r6 = c()
for(i in numeric_columns2){
  test <- rosnerTest(full_data_without_label[,i],k = 10)
  h = length(which(test$all.stats$Outlier == TRUE))
  if(h > 0){r = c(r6,test$all.stats$Obs.Num[which(test$all.stats$Outlier == TRUE)])}
}

r6 = unique(r6)
r_total_1 = c(r_total_1 , r6)
Result6 = data.frame(ID = 1:dd3 , label = 0)
for(i in 1:nrow(Result6)){
  if(sum(r6 == i)>0){
    Result6$label[i] = 1
  }
}
r_total_1 = c(r_total_1 , r6)
Result6$label = as.factor(Result6$label)
(cm_6 = confusionMatrix(data = Result6$label , reference = response[1:dd3],mode = "everything"))
auc6 = roc.curve(response[1:dd3] , predicted = Result6$label)
auc6[2]



## LOF Method ##
library(DMwR2)
r7 = c()
outlier.scores <- lofactor(full_data_without_label[1:dd3,numeric_columns2], k=3)
outliers <- order(outlier.scores, decreasing=T)[1:(0.2*dd3)]
if(length(outliers) > 0){
  r7 = c(r7 , outliers)
}
r7 = unique(r7)
r_total_1 = c(r_total_1 , r7)
Result7 = data.frame(ID = 1:dd3 , label = 0)
for(i in 1:nrow(Result7)){
  if(sum(r7 == i)>0){
    Result7$label[i] = 1
  }
}
Result7$label = as.factor(Result7$label)
(cm_7 = confusionMatrix(data = Result7$label , reference = response[1:dd3],mode = "everything"))
auc7 = roc.curve(response[1:dd3] , predicted = Result7$label)
auc7[2]

##########
library(adamethods)

r8 <- do_knno(full_data_without_label[1:dd3, numeric_columns2], 3, (0.15*dd3))
r8 = unique(r8)

Result8 = data.frame(ID = 1:dd3 , label = 0)
for(i in 1:nrow(Result8)){
  if(sum(r8 == i)>0){
    Result8$label[i] = 1
  }
}
r_total_1 = c(r_total_1 , r8)
Result8$label = as.factor(Result8$label)
(cm_8 = confusionMatrix(data = Result8$label , reference = response[1:dd3],mode = "everything"))
auc8 = roc.curve(response[1:dd3] , predicted = Result8$label)
auc8[2]

###calculate accuracy for all second methods

#isolatiion forest 
library(h2o)
h2o.init()
data <- h2o.importFile(path ="carclaims.csv")
# data <- data[1:dd3,c(numeric_columns2 , response_column_number)]
# Split dataset giving the training dataset 75% of the data
#prostate_split <- h2o.splitFrame(data = data, ratios = 0.9)

# Create a training set from the 1st dataset in the split
#train <- prostate_split[[1]]
train <- data[1:dd3,]
resp_col = 33
# Create a testing set from the 2nd dataset in the split
# test <- prostate_split[[2]]
######test <- data[(dd3+1):15420, c(numeric_columns2, response_column_number)]
# Build an Isolation forest model
model <- h2o.isolationForest(training_frame = train[,-resp_col],
                             max_depth = 20,
                             ntrees = 50,
                             stopping_metric = "anomaly_score")
# Calculate score
score <- h2o.predict(model, train[,-resp_col])
result_pred9 <- as.data.frame(score$predict)
r9 <- ifelse(result_pred9[,1]>0.5 , 1 , 0)
# Calculate accuracy

(cm_9 = confusionMatrix(data = factor(r9) , reference = factor(as.vector(ifelse(train$FraudFound == "Yes" , 1 , 0))),mode = "everything"))
auc9 = roc.curve(factor(as.vector(ifelse(train$FraudFound == "Yes" , 1 , 0))) , predicted = factor(r9))
auc9[2]
r_total_1 = c(r_total_1 , which(r9 == 1))
# Predict the leaf node assignment
#ln_pred <- h2o.predict_leaf_node_assignment(model, test)



### find outliers index for categorical columns with voting between models ((1/2 + 1) ==> 1)
r10 = c()
library(klaR)
model = kmodes(full_data_without_label[1:dd3,categorical_columns2], 2, iter.max = 10, weighted = FALSE, fast = TRUE)
r10= factor(ifelse(model$cluster == 1 , 1 , 0))
confusionMatrix(data = r10 , reference = response[1:dd3],mode = "everything")
auc10 = roc.curve(response[1:dd3] , predicted = r10)
auc10[2]
r10 = which(r10 == 1)
r_total_1 = c(r_total_1 , r10)


# #DBScan
# r11 = c()
# library(dbscan)
# model = dbscan(full_data_without_label[1:dd3,] , eps = 0.3)
# r11 = as.factor(ifelse(model$cluster > 0 , 1 , 0))
# confusionMatrix(data = r11 , reference = response[1:dd3],mode = "everything")
# auc11 = roc.curve(response[1:dd3] , predicted = r11)
# auc11[2]
# r_total_1 = c(r_total_1 , r11)


## calculating final confusion matrix with counting model votes.
r_total_1 = c(r_total_1 , r10)
Result_final_1 = data.frame(ID = 1:dd3 , label = 0)
for(i in 1:nrow(Result_final_1)){
  if(sum(r_total_1 == i)>4){
    Result_final_1$label[i] = 1
  }
}
Result_final_1$label = as.factor(Result_final_1$label)
I = which(Result_final_1$label == 1) 
(cm_total = confusionMatrix(data = Result_final_1$label , reference = response[1:dd3],mode = "everything"))
auc_total = roc.curve(response[1:dd3] , predicted = Result_final_1$label)
auc_total[2]



#now we need to make a new dataset for supervised learning here:
j = 0
jj = table(Result_final_1$label[1:dd3])
for(i in 1:nrow(Result_final_1)){
  if(Result_final_1$label[i] == 1){
    if(full_data_label[i] == 0){
      Result_final_1$label[i] = 0
      j = j +1
    }
  }
}
(acc1_1 = (1 - (j / jj[2])))


dd4 = round(0.8 * nrow(full_data_without_label))
train_df1 = data.frame(full_data_without_label[I , important_columns] , yhat = Result_final_1$label[I])
train_df1_1 = ovun.sample(yhat ~., data = train_df1, method = "over", N = 2*nrow(train_df1))$data
test_df1 = data.frame(full_data_without_label[(dd3+1):dd4 , important_columns])

#multinomial logistic regression method:
library(nnet)
fit1 = multinom(yhat ~ . , data = train_df1_1)
pp1 = predict(fit1 , newdata = test_df1 , type = "class")
(cm_ml1 = (confusionMatrix(data = pp1 , reference = response[(dd3+1):dd4] ,mode = "everything")))
auc_ml1 = ROSE::roc.curve(response[(dd3+1):dd4] , predicted = pp1)
auc_ml1[2]
# for(i in 1:(ncol(train_df1_1)-1)){
#   train_df1_1[,i] = as.double(train_df1_1[,i])
# }
library(tensorflow)
#اینجا تفاوت اینکه دیتا بالانس شده را برای شبکه استفاده بکند یا نه مشخص میشود.
train_df1_1 = data.frame(train_df1_1[,-c(dim(train_df1_1)[2])] ,yhat =dummy_cols(train_df1_1[,c(dim(train_df1_1)[2])] ,remove_selected_columns = TRUE))
train_df1_1 = array_reshape(train_df1_1 , dim = c(dim(train_df1_1)[1] , dim(train_df1_1)[2]))
#train_df1 = data.frame(full_data_without_label[1:dd3 , important_columns] , yhat = Result_final_1$label)
dimention = dim(train_df1_1)
train_df1_1 = array(unlist(train_df1_1), dim = c(dimention[1],dimention[2]))
test_df1 = array(unlist(test_df1) , dim = c(dd4 - dd3 , (dimention[2]-2)))

#neural network method:
library(tensorflow)
library(keras)
##### Define the model #####
model <- keras_model_sequential() %>%
  layer_dense(units = 256, activation = "relu", input_shape = c(dimention[2]-2)) %>%
  layer_dense(units = 128, activation = "relu") %>%
  layer_dense(units = 64, activation = "relu") %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dense(units = 2, activation = "softmax")

##### model info #####

summary(model)

##### Compile Model #####
model %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = optimizer_rmsprop(),
  metrics = 'accuracy'
)

##### Evaluate the model #####
history1 <- model %>% fit(
  train_df1_1[,-c(dim(train_df1_1)[2]:(dim(train_df1_1)[2]-1))],
  train_df1_1[,c(dim(train_df1_1)[2]:(dim(train_df1_1)[2]-1))],
  epochs = 5,
  batch_size =256,
  shuffle = TRUE
)
history2 = model %>% evaluate(test_df1 , full_data_label[(dd3+1):dd4,])


pp3 = model$predict(test_df1)
x = apply(pp3, 1, which.min) - 1
table(x)
(cm_ml3 = (confusionMatrix(data = factor(x) , reference = response[(dd3+1):dd4] ,mode = "everything")))


#second method with balanced data

# train_df1_1 = ovun.sample(yhat ~., data = train_df1, method = "over", N = 2*nrow(train_df1))$data
# #train_df1_1 = ovun.sample(yhat ~., data = train_df1, method = "under", N = 0.1*nrow(train_df1))$data
# 
# fit1 = multinom(yhat ~ . , data = train_df1_2)
# pp1 = predict(fit1 , newdata = test_df1 , type = "class")
# (cm_ml1 = (confusionMatrix(data = pp1 , reference = factor(full_data_label[(dd3+1):dd4]) ,mode = "everything")))
# ROSE::roc.curve(factor(full_data_label[(dd3+1):dd4]) , predicted = pp1)

#third test


#now we need to make a new dataset for supervised learning here:

dd5 = round( 0.85* nrow(full_data_without_label))
Result_final_1 = data.frame(ID = 1:dd5 , label = factor(c(train_df1$yhat ,  pp1 , factor(rep(0 , (dd5 - dd4 ))))))
j = 0
jj = table(Result_final_1$label[dd3+1:dd4])
for(i in (dd3+1):dd4){
  if(Result_final_1$label[i] == 1){
    if(full_data_label[i] == 0){
      Result_final_1$label[i] = 0
      j = j+1
    }
  }
}
(acc1_2 = 1 - (j / jj[2]))

train_df2 = data.frame(full_data_without_label[1:dd4 , important_columns] , yhat = Result_final_1$label[1:dd4])
test_df2 = data.frame(full_data_without_label[(dd4+1):dd5 , important_columns] )
##dd = ovun.sample(yhat ~., data = train_df2, method = "over", N = 2*nrow(train_df1))$data

#multinomial logistic regression method:
library(nnet)
fit2 = multinom(yhat ~ . , data = train_df2)
pp2 = predict(fit2 , newdata = test_df2 , type = "class")
(cm_ml2 = (confusionMatrix(data = pp2 , reference = factor(full_data_label[(dd4+1):dd5]) ,mode = "everything")))
auc_ml2 = ROSE::roc.curve(factor(full_data_label[(dd4+1):dd5]) , predicted = pp2)
auc_ml2[2]


dd6 = round( 0.88* nrow(full_data_without_label))
Result_final_1 = data.frame(ID = 1:dd6 , label = factor(c(train_df2$yhat ,  pp2 , factor(rep(0 , (dd6 - dd5))))))
j = 0
jj = table(Result_final_1$label[dd4+1:dd5])
for(i in (dd3+1):dd4){
  if(Result_final_1$label[i] == 1){
    if(full_data_label[i] == 0){
      Result_final_1$label[i] = 0
      j = j+1
    }
  }
}
(acc1_3 = 1 - (j / jj[2]))
#svm method:
# 
# #neural network method:
# library(tensorflow)
# library(neuralnet)
# fit3 = neuralnet(train_df1$yhat ~ . , data = train_df1[,1:length(numeric_columns2)] , hidden = 3  )
# pp3 = predict(fit3 , newdata = test_df1 , type = "probs")
# (cm_ml3 = (confusionMatrix(data = pp3 , reference = test_df1$yhat ,mode = "everything")))
# #random forest method:
# library(randomForest)
# fit4 = randomForest(yhat ~ . , data = train_df1)
# pp4 = predict(fit4 , newdata = test_df1 , type = "class")
# (cm_ml4 = (confusionMatrix(data = pp4 , reference = test_df1$yhat ,mode = "everything")))
