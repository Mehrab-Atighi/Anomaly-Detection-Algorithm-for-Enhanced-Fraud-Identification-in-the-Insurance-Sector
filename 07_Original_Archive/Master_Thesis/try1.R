# 1. Descriptive statistics #

#########################################################################
library(caret)
data = read.csv("carclaims.csv")
dim(data)
data = na.omit(data)
dim(data)

#use 70% of dataset as training set and 30% as test set
sample <- sample(c(TRUE, FALSE), nrow(data), replace=TRUE, prob=c(0.7,0.3))
train  <- data[sample, -33]
test   <- data[!sample, -33]
train_label = factor(ifelse(data[sample,33] == "Yes" , 1 , 0))
test_label = factor(ifelse(data[!sample,33] == "Yes" , 1 , 0))
full_data_without_label =data[,-33]
full_data_label = factor(ifelse(data[,33] == "Yes" , 1 , 0))
numeric_col = as.numeric(which(sapply(train, is.numeric)))
#or using bottem code for find numeric cloumns
# library(dplyr)
# select_if(dat, is.numeric)


## A. Minimum and maximum ##

#apply(data_without_label[,numeric_col], 2, range)

r = c()
r_total_1 = c()

## B. Boxplot method ##
for(i in numeric_col){
  out <- boxplot.stats(full_data_without_label[,i])$out
  h = length(which(full_data_without_label[,i] %in% c(out)))
  if(h >0 ){
  r <- c(r,which(full_data_without_label[,i] %in% c(out)))
  }
}
r = unique(r)
Result1 = data.frame(ID = 1:nrow(full_data_without_label) , label = 0)
for(i in 1:nrow(Result1)){
  if(sum(which(r == i))>0){
    Result1$label[i] = 1
  }
}
r_total_1 = c(r_total_1 , r)
Result1$label = as.factor(Result1$label)
confusionMatrix(data = Result1$label , reference = full_data_label , mode = "everything")
r = c()
## C.Percntile ##
for(i in numeric_col){
  lower_bound <- quantile(full_data_without_label[,i] , 0.025)
  upper_bound <- quantile(full_data_without_label[,i] , 0.975)
  h = length(which(full_data_without_label[,i] < lower_bound | full_data_without_label[,i] > upper_bound))
  if(h>0){
  r<- c(r,which(full_data_without_label[,i] < lower_bound | full_data_without_label[,i] > upper_bound))
  }
}
r = unique(r)
Result2 = data.frame(ID = 1:nrow(full_data_without_label) , label = 0)
for(i in 1:nrow(Result2)){
  if(sum(which(r == i))>0){
    Result2$label[i] = 1
  }
}
r_total_1 = c(r_total_1 , r)
Result2$label = as.factor(Result2$label)
confusionMatrix(data = Result2$label , reference = full_data_label,mode = "everything")


## D.Zscore ##
r = c()
for(i in numeric_col){
  zData = scale(full_data_without_label[,i])
  h = length(which(zData > 3.29 | zData < -3.29))
  if(h>0){
  r = c(r,which(zData > 3.29 | zData < -3.29))
  }
}
r = unique(r)
Result3 = data.frame(ID = 1:nrow(full_data_without_label) , label = 0)
for(i in 1:nrow(Result3)){
  if(sum(which(r == i))>0){
    Result3$label[i] = 1
  }
}
r_total_1 = c(r_total_1 , r)
Result3$label = as.factor(Result3$label)
confusionMatrix(data = Result3$label , reference = full_data_label,mode = "everything")





## E.Hampel filter ##
r = c()
for(i in numeric_col){
  lower_bound <- median(full_data_without_label[,i]) - 3 * mad(full_data_without_label[,i], constant = 1)
  upper_bound <- median(full_data_without_label[,i]) + 3 * mad(full_data_without_label[,i], constant = 1)
  h = length(which(full_data_without_label[,i] < lower_bound | full_data_without_label[,i] > upper_bound))
  if(h>0){
    r  <- c(r,which(full_data_without_label[,i] < lower_bound | full_data_without_label[,i] > upper_bound))
    }

}
r = unique(r)
r_total_1 = c(r_total_1 , r)
Result4 = data.frame(ID = 1:nrow(full_data_without_label) , label = 0)
for(i in 1:nrow(Result4)){
  if(sum(which(r == i))>0){
    Result4$label[i] = 1
  }
}
r_total_1 = c(r_total_1 , r)
Result4$label = as.factor(Result4$label)
confusionMatrix(data = Result4$label , reference = full_data_label,mode = "everything")

#final result for all descriptive methods :
r_total_1 = unique(r_total_1)

Result_final_1 = data.frame(ID = 1:nrow(full_data_without_label) , label = 0)
for(i in 1:nrow(Result_final_1)){
  if(sum(which(r == i))>0){
    Result_final_1$label[i] = 1
  }
}
Result_final_1$label = as.factor(Result_final_1$label)
confusionMatrix(data = Result_final_1$label , reference = full_data_label,mode = "everything")



# 2. Statistical tests #

## F Grubbs’s test ##
# The Grubbs test allows to detect whether the highest or lowest value in a dataset is an outlier.
# The Grubbs test detects one outlier at a time (highest or lowest value), so the null and alternative hypotheses are as follows:
# Null Hypothesis: The highest value is not an outlier
# Alternative hypothesis: The highest value is an outlier

library(tidyverse)
library(outliers)
r2 = c()
r_total_2 = c()
for(i in numeric_col){
(test1 <- grubbs.test(full_data_without_label[,i]))
(test2 <- grubbs.test(full_data_without_label[,i], opposite = TRUE))

df = data.frame(x = c(test1$alternative , test2$alternative) , PValue = c(test1$p.value, test2$p.value))
df = separate(df , col = x  ,into =  c("A" , "B" ,"C" , "D" , "E" , "F") , sep = " ")
if(df$PValue[1] <= 0.05){
 h = length(which(full_data_without_label[,i] == as.numeric(df$C[1])) ) 
 if(h > 0 ){
   r2 = c(r2,which(full_data_without_label[,i] == as.numeric(df$C[1])))
 }
}
if(df$PValue[2] <= 0.05){
  h =length(which(full_data_without_label[,i] == as.numeric(df$C[2])))
  if( h > 0 ){
    r2 = c(r2,which(full_data_without_label[,i] == as.numeric(df$C[2])))
  }
}
}
r2 = unique(r2)
r_total_2 = c(r_total_2 , r2)
Result5 = data.frame(ID = 1:nrow(full_data_without_label) , label = 0)
for(i in 1:nrow(Result5)){
  if(sum(which(r == i))>0){
    Result5$label[i] = 1
  }
}
r_total_2 = c(r_total_2 , r)
Result5$label = as.factor(Result5$label)
confusionMatrix(data = Result5$label , reference = full_data_label,mode = "everything")


## Rosner’s test ##
library(EnvStats)
r2 = c()
for(i in numeric_col){
test <- rosnerTest(full_data_without_label[1:3000,i],k = 5)
h = length(which(test$all.stats$Outlier == TRUE))
if(h > 0){r2 = c(r2 ,test$all.stats$Obs.Num[which(test$all.stats$Outlier == TRUE)])}
}

r2 = unique(r2)
r_total_2 = c(r_total_2 , r2)
Result6 = data.frame(ID = 1:nrow(full_data_without_label) , label = 0)
for(i in 1:nrow(Result6)){
  if(sum(r2 == i)>0){
    Result6$label[i] = 1
  }
}
r_total_2 = c(r_total_2 , r2)
Result6$label = as.factor(Result6$label)
confusionMatrix(data = Result6$label , reference = full_data_label,mode = "everything")




## LOF Method ##
library(DMwR2)
r2 = c()
outlier.scores <- lofactor(full_data_without_label[,numeric_col], k=3)
outliers <- order(outlier.scores, decreasing=T)[1:(0.1*nrow(full_data_without_label))]
if(length(outliers) > 0){
r2 = c(r2 , outliers)
}
r2 = unique(r2)
r_total_2 = c(r_total_2 , r2)
Result7 = data.frame(ID = 1:nrow(full_data_without_label) , label = 0)
for(i in 1:nrow(Result7)){
  if(sum(which(r2 == i))>0){
    Result7$label[i] = 1
  }
}
Result7$label = as.factor(Result7$label)
confusionMatrix(data = Result7$label , reference = full_data_label,mode = "everything")


##########
library(adamethods)

r2 <- do_knno(data, 3, (0.1*nrow(full_data_without_label)))


r2 = unique(r2)

Result8 = data.frame(ID = 1:nrow(full_data_without_label) , label = 0)
for(i in 1:nrow(Result8)){
  if(sum(which(r == i))>0){
    Result8$label[i] = 1
  }
}
r_total_2 = c(r_total_2 , r)
Result8$label = as.factor(Result8$label)
confusionMatrix(data = Result8$label , reference = full_data_label,mode = "everything")
#calculate accuracy for all second methods

r_total_2 = unique(r_total_2)

Result_final_2 = data.frame(ID = 1:nrow(full_data_without_label) , label = 0)
for(i in 1:nrow(Result_final_2)){
  if(sum(which(r == i))>0){
    Result_final_2$label[i] = 1
  }
}
Result_final_2$label = as.factor(Result_final_2$label)
confusionMatrix(data = Result_final_2$label , reference = full_data_label,mode = "everything")




#isolatiion forest 
library(h2o)
h2o.init()
data <- h2o.importFile(path ="carclaims.csv")
# Split dataset giving the training dataset 75% of the data
prostate_split <- h2o.splitFrame(data = data, ratios = 0.75)

# Create a training set from the 1st dataset in the split
train <- prostate_split[[1]]

# Create a testing set from the 2nd dataset in the split
test <- prostate_split[[2]]

# Build an Isolation forest model
model <- h2o.isolationForest(training_frame = train,
                             sample_rate = 0.1,
                              max_depth = 40,
                             ntrees = 150,
                             stopping_metric = "anomaly_score",
                              contamination = 0.1)

# Calculate score
score <- h2o.predict(model, test)
result_pred <- as.data.frame(score$predict)

# Calculate accuracy



confusionMatrix(data = factor(result_pred[,1]) , reference = factor(as.vector(ifelse(test$FraudFound == "Yes" , 1 , 0)[,1])),mode = "everything")
# Predict the leaf node assignment
#ln_pred <- h2o.predict_leaf_node_assignment(model, test)




