Preprocessing = function(path , response_column_number ,
                         YesOrNo = TRUE , percent1 = percent1 ,
                         percent2 = percent2 , percent3 = percent3 ){
  
### input data csv
data = read.csv(path)
set.seed(1234)
### make a random sample of rows
s1 = sample(1:nrow(data))
data = data[s1,]

### Check and transform to 0 - 1 for response columns
if(YesOrNo == T){
  data[,response_column_number] = factor(ifelse(data[,response_column_number] == "Yes" , 1 , 0))
  data[,response_column_number] = relevel(data[,response_column_number] , ref = 1)
}else{
  data[,response_column_number] = factor(data[,response_column_number])
  data[,response_column_number] = relevel(data[,response_column_number] , ref = 1)
}

### find categorical and numeric columns
categorical_columns = as.numeric(which(sapply(data[,-response_column_number], is.character)))
numeric_columns = as.numeric(which(sapply(data[,-response_column_number], is.numeric)))
PP = 1
for(P in numeric_columns){
  if(P >= response_column_number){
    numeric_columns[PP] = numeric_columns[PP] +1 
    PP = PP+1
  }else{PP = PP+1}
}

PP = 1
for(P in categorical_columns){
  if(P >= response_column_number){
    categorical_columns[PP] = categorical_columns[PP] +1 
    PP = PP+1
  }else{PP = PP+1}
}

### make factor for categorical columns
for(i in categorical_columns){
  data[,i] = as.factor(data[,i])
}



### Feature selection for categorical columns
j = 1 ; gini = c()
for(i in categorical_columns){
  my_table <- table(data[,i])
  my_prop <- prop.table(my_table)
  gini[j] <- 1 - sum(my_prop^2)
  j = j+1
}
d1 = which( gini  > 0.1 & gini < 0.9 )
gini_important_columns_name = names(data[,categorical_columns[d1]])



### Feature selection for numeric columns
library(caret)
NearZeroVarince = nearZeroVar(data[,numeric_columns] , saveMetrics = TRUE , names = TRUE)
d2 = cbind(rownames(NearZeroVarince) , NearZeroVarince[,4])
NZV_important_columns_name = d2[which(d2[,2] == "FALSE"),1]





categorical_columns_important = c()
numeric_columns_important = c()
for(i in 1:length(gini_important_columns_name)){
  categorical_columns_important[i] = which(colnames(data) == gini_important_columns_name[i])
}
for(i in 1:length(NZV_important_columns_name)){
  numeric_columns_important[i] = which(colnames(data) == NZV_important_columns_name[i])
}
important_columns = c(numeric_columns_important , categorical_columns_important)

### make new dataset with important columns
data = data[,c(important_columns , response_column_number)]
response_column_number = length(important_columns) + 1

### find categorical and numeric columns for new dataset with important columns
categorical_columns = as.numeric(which(sapply(data[,-response_column_number], is.factor)))
numeric_columns = as.numeric(which(sapply(data[,-response_column_number], is.numeric)))

###make final dataset for h20:
library(h2o)
h2o.init()
data_h <- h2o.importFile(path = path)
nrw = nrow(data_h)
train_p1_h2o <- data_h[1:(percent1*nrw),-response_column_number]


### make final dataset for other models
full_data_label =  data[,response_column_number]
full_data_without_label = data[,-response_column_number]

for(i in 1:length(numeric_columns)){
  data[,i] = scale(data[,i])
}
#از اینجا
### make final dataset for nn models
library(fastDummies)
data_nn = data.frame(dummy_cols(data[,categorical_columns] , remove_selected_columns = TRUE , remove_first_dummy = TRUE),
                     data[,numeric_columns],dummy_cols(data[,response_column_number] , remove_selected_columns = TRUE , remove_first_dummy = TRUE))
dimention = dim(data_nn)
data_nn = array(unlist(data_nn), dim = c(dimention[1],dimention[2]))
for(i in 1:(dimention[2]-1)){
  data_nn[,i] = as.double(data_nn[,i])
}
data_nn = na.omit(data_nn)
response_columns_number = c(dimention[2] , (dimention[2]-1))

full_data_label_nn = data[,response_column_number]
full_data_without_label_nn =data_nn[,-c(response_columns_number)]
#تا اینچا


### make splits of data for other models
nrw = nrow(full_data_without_label)
full_data_label1 = full_data_label[1:(percent1*nrw)]
full_data_without_label1 = full_data_without_label[1:(percent1*nrw) , ]

full_data_label2 = full_data_label[((nrw*percent1)+1):(percent2*nrw)]
full_data_without_label2 = full_data_without_label[((percent1*nrw) +1):(percent2*nrw) , ]

full_data_label3 = full_data_label[((nrw*percent2)+1):(percent3*nrw)]
full_data_without_label3 = full_data_without_label[((percent2*nrw) +1):(percent3*nrw) , ]

full_data_label4 = full_data_label[((nrw*percent3)+1):nrw]
full_data_without_label4 = full_data_without_label[((percent3*nrw) +1):nrw , ]

#از اینجا
### make splits of data for nn models
full_data_label_nn1 = full_data_label_nn[1:(percent1*nrw)]
full_data_without_label_nn1 = full_data_without_label_nn[1:(percent1*nrw) , ]

full_data_label_nn2 = full_data_label_nn[((nrw*percent1)+1):(percent2*nrw)]
full_data_without_label_nn2 = full_data_without_label_nn[((percent1*nrw) +1):(percent2*nrw) , ]

full_data_label_nn3 = full_data_label_nn[((nrw*percent2)+1):(percent3*nrw)]
full_data_without_label_nn3 = full_data_without_label_nn[((percent2*nrw) +1):(percent3*nrw) , ]

full_data_label_nn4 = full_data_label_nn[((nrw*percent3)+1):nrw]
full_data_without_label_nn4 = full_data_without_label_nn[((percent3*nrw) +1):nrw , ]
# تا اینجا
### make function outputs
outputs = list(
full_data_label1 =  full_data_label1  ,
full_data_without_label1 = full_data_without_label1 ,
full_data_label_nn1 =  full_data_label_nn1  ,
full_data_without_label_nn1 = full_data_without_label_nn1 ,

full_data_label2 =  full_data_label2  ,
full_data_without_label2 = full_data_without_label2 ,
full_data_label_nn2 =  full_data_label_nn2  ,
full_data_without_label_nn2 = full_data_without_label_nn2 ,

full_data_label3 =  full_data_label3  ,
full_data_without_label3 = full_data_without_label3 ,
full_data_label_nn3 =  full_data_label_nn3  ,
full_data_without_label_nn3 = full_data_without_label_nn3 ,

full_data_label4 =  full_data_label4  ,
full_data_without_label4 = full_data_without_label4 ,
full_data_label_nn4 =  full_data_label_nn4  ,
full_data_without_label_nn4 = full_data_without_label_nn4 ,
numeric_columns = numeric_columns , categorical_columns = categorical_columns,
number_of_rows = nrw , 
train_p1_h2o = train_p1_h2o,
categorical_columns = categorical_columns)
save(outputs , file = "pre.RData")
return(outputs)
}

