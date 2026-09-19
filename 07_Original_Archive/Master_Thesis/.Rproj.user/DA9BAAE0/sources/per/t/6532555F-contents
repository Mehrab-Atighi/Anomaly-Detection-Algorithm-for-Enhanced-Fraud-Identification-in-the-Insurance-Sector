source(file = "Labaling.R")
# source(file = "LabelingWithEntropy.R")
# source(file = "LabalingMultiLayer.R")
set.seed(1234)
Nice = list()
number = 1
# path1 = "carclaims.csv"; response_column_number1 = 33 ;YesOrNo1 = TRUE ;vote_num1 = 6;model_num1 = 6
# path2 = "insurance fraud claims.csv" ; response_column_number2 = 39 ;YesOrNo2 = TRUE; vote_num2 = 6;model_num2 = 6
# path3 = "fraud_oracle.csv"; response_column_number3 = 16 ;YesOrNo3 = FALSE ; vote_num3 = 4;model_num3 = 5
# response_column_numbers = c(33,39,16)
# YesOrNo_vector = c(T,T,F)
percent1 = 0.5 ; percent2 = 0.8 ; percent3 = 0.9 
paths = c("carclaims.csv" , "insurance fraud claims.csv" , "fraud_oracle.csv")
Models = list()
Models[[1]] = c("ml" , "svm" , "xgb"  , "nb"  , "rf", "knn" , "adab" , "nn" ) 
Models[[2]] = c("ml" , "svm" , "xgb"  , "nb"  , "rf", "knn" , "adab" ) 
Models[[3]] = c("ml" , "svm" , "xgb"  , "nb"  , "rf", "knn" ) 
Models[[4]] = c("ml" , "svm" , "xgb"  , "nb"  , "rf"  ) 
Models[[5]] = c("ml" , "svm" , "xgb"  , "nb"   ) 
 


model_nums = list()
model_nums[[1]] = c(3:7)
model_nums[[2]] = c(3:6)
model_nums[[3]] = c(3:5)
model_nums[[4]] = c(3:4)
model_nums[[5]] = c(3:3)


for(KK in 1:5){
  models = Models[[KK]]
  for(model_num in model_nums[[KK]]){
    for(vote_num in 1:5){
      for(path in paths){
        if(path == "carclaims.csv"){
          response_column_number = 33
          YesOrNo = T
        }
        if(path == "insurance fraud claims.csv"){
          response_column_number = 39
          YesOrNo = T
        }
        if(path == "fraud_oracle.csv"){
           response_column_number = 16
           YesOrNo = F
         }
        
        
        model_names = paste0(models , collapse = "_")
    # ){
    percent1 = percent1 ; percent2 = percent2 ; percent3 = percent3
    S = c()
    S[1] = Sys.time()
    L = Labaling(path = path , response_column_number = response_column_number 
                 ,percent1 = percent1 , percent2 = percent2 , percent3 = percent3 , YesOrNo = YesOrNo , vote_num = vote_num) 
    S[2] = Sys.time()
    #LabalingMultiLayer
    #Labaling
    #Labaling2
    attach(L)
    # model_num = length(models)
    
    diff = setdiff(1:dim(full_data_without_label1)[1] , I1)
    I11 = sample(diff , size = c(0.1*length(I1)) , replace = FALSE)
    train_df_nn = data.frame(full_data_without_label_nn1[c(I1,I11),] , yhat = Result_final_outliers$label[c(I1 , I11)])
    train_df_Balance_nn_all = ovun.sample(yhat ~., data = train_df_nn, method = "over")$data
    train_df_nn_balance_label = train_df_Balance_nn_all$yhat
    train_df_Balance_nn_all = train_df_Balance_nn_all[,-ncol(train_df_Balance_nn_all)]
    train_df = data.frame(full_data_without_label1[c(I1,I11) , ] , yhat = Result_final_outliers$label[c(I1 , I11) ])
    train_df_Balance = ovun.sample(yhat ~., data = train_df, method = "over")$data
    train_df_Balance_nn_all = array(unlist(train_df_Balance_nn_all) , dim = c(dim(train_df_Balance_nn_all)[1] , dim(train_df_Balance_nn_all)[2]))
   
    S[3] = Sys.time()
    ### Modeling rond 1:
     # neural network
    if(length(which(models == "nn")) == 1){
      library(tensorflow)
      library(keras)
      dimention = dim(train_df_Balance_nn_all)
      train_df_nn_balance_label= ifelse(train_df_nn_balance_label == 0 , 0, 1)
        ##### Define the model #####
      NnModel1 <- keras_model_sequential() %>%
        layer_dense(units = 32, activation = "relu", input_shape = c(dimention[2])) %>%
        layer_dropout(rate = 0.4) %>%
        layer_dense(units = 1, activation = "sigmoid")
      ##### model info #####
      summary(NnModel1)
      ##### Compile Model #####
      NnModel1  %>% compile(
        optimizer = "adam",
        loss = "binary_crossentropy",
        metrics = c("accuracy")
      )
      ##### Evaluate the model #####
      nn_p_1_1 <- NnModel1 %>% fit(
        train_df_Balance_nn_all,
        train_df_nn_balance_label,
        epochs = 4,
        batch_size =8,
        shuffle = TRUE)
      nn_p_1_1 <- predict(NnModel1 , x = train_df_Balance_nn_all , type = "response")
      nn_p_1_1 = as.vector(ifelse(nn_p_1_1 <= 0.5 , 0 , 1))
      (nn_cm_1_1 = confusionMatrix(data = as.factor(nn_p_1_1) , reference = as.factor(train_df_nn_balance_label) ,mode = "everything"))
      (nn_roc_1_1 = ROSE::roc.curve(as.factor(train_df_nn_balance_label) , predicted = nn_p_1_1))
      nn_roc_1_1[2]
      nn_p_1 = predict(NnModel1 , full_data_without_label_nn2)
      nn_p_1 = as.vector(ifelse(nn_p_1 <= 0.5 , 0 , 1))
      (nn_cm_1 = confusionMatrix(data = as.factor(nn_p_1) , reference = as.factor(full_data_label2) ,mode = "everything"))
      (nn_roc_1 = ROSE::roc.curve(as.factor(full_data_label2) , predicted = nn_p_1))
      nn_roc_1[2]
      nn_1_index = which(nn_p_1 == 1)
      nn_11_index = which(nn_p_1 == 0)
      nn_I22 = sample(nn_11_index , 0.1*length(nn_11_index))
      nn_I2 = nn_1_index
    }else{
      nn_cm_1 = c();nn_cm_1_1 = c();nn_roc_1 = c() ; nn_roc_1_1 = c();nn_I2 = c()
    }
    S[4] = Sys.time()
    
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
      ml_I22 = sample(ml_11_index , 0.1*length(ml_11_index))
      ml_I2 = ml_1_index
    }else{
      ml_cm_1 = c();ml_cm_1_1 = c();ml_roc_1 = c() ; ml_roc_1_1 = c();ml_I2 = c()
    }
    S[5] = Sys.time()
    
    #svm
    if(length(which(models == "svm")) == 1){
      library(e1071)
      SvmModel1 = svm(formula = yhat ~ .,
                      data = train_df_Balance,
                      type = 'C-classification',
                      kernel = 'linear')
      
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
      svm_I22 = sample(svm_11_index , 0.1*length(svm_11_index))
      svm_I2 = svm_1_index
    }else{svm_cm_1 = c();svm_cm_1_1 = c();svm_roc_1 = c() ; svm_roc_1_1 = c();svm_I2 = c()}
    S[6] = Sys.time()
    
    #Xgboost
    if(length(which(models == "xgb")) == 1){
      library(xgboost)
      
      # Set XGBoost parameters
      params <- list(
        objective = "binary:logistic",   # For binary classification or "binary:logistic"
        eval_metric = "error",         # Logarithmic Loss (Log Loss) for evaluation
        eta = 0.3,                       # Learning rate (Typical values: 0.01 - 0.3)
        max_depth = 20 ,                  # Maximum depth of a tree (Typical values: 3-10)
        nrounds = 1 ,          # Number of boosting rounds (or trees)
        depth = 20
        # gamma = 10,
        # min_child_weight = 1,
        # subsample = 0.8,
        # colsample_bytree = 1,
        # lambda = 1,
        # alpha = 1
        # 
      )
      # Convert data into DMatrix format required by XGBoost
      dtrain1 <- xgb.DMatrix(data = as.matrix(train_df_Balance[,-c(categorical_columns,ncol(train_df_Balance))])  , label = ifelse(train_df_Balance$yhat==0 , 0 , 1))
      dtest1 <- xgb.DMatrix(data = as.matrix(full_data_without_label2[-c(categorical_columns)]), label = ifelse(full_data_label2==0 , 0 , 1))
      
      
      # Train the XGBoost model
      XgbModel1 <- xgboost(params, data = dtrain1, nrounds = params$nrounds)
      xgb_p_1 <- predict(XgbModel1, dtest1 , type = "class")
      # Convert probabilities to binary predictions
      xgb_p_1 <- factor(ifelse(xgb_p_1 > 0.5, 1, 0))
      (xgb_cm_1 <- confusionMatrix(xgb_p_1, full_data_label2, mode = "everything"))
      (xgb_roc_1 = ROSE::roc.curve(factor(full_data_label2) , predicted = xgb_p_1))
      xgb_roc_1[2]
      
      xgb_p_1_1 = predict(XgbModel1, dtrain1 , type = "class")
      xgb_p_1_1 <- factor(ifelse(xgb_p_1_1 > 0.5, 1, 0))
      (xgb_cm_1_1 = (confusionMatrix(data = xgb_p_1_1 , reference = factor(train_df_Balance$yhat) ,mode = "everything")))
      (xgb_roc_1_1 = ROSE::roc.curve(factor(train_df_Balance$yhat) , predicted = xgb_p_1_1))
      xgb_roc_1_1[2]
      xgb_1_index = which(xgb_p_1 == 1)
      xgb_11_index = which(xgb_p_1 == 0)
      xgb_I22 = sample(xgb_11_index , 0.1*length(xgb_11_index))
      xgb_I2 = xgb_1_index}else{xgb_cm_1 = c();xgb_cm_1_1 = c();xgb_roc_1 = c() ; xgb_roc_1_1 = c();xgb_I2 = c()}
    S[7] = Sys.time()
    
    #random forest
    if(length(which(models == "rf")) == 1){
      library(randomForest)
      
      RfModel1 = randomForest(x = train_df_Balance[,-c(ncol(train_df_Balance))],
                              y = train_df_Balance$yhat,
                              ntree = 20)
      rf_p_1 <- as.factor(as.vector(predict(RfModel1, full_data_without_label2,type = "class")))
      # Calculate confusion matrix
      (rf_cm_1 <- confusionMatrix(rf_p_1, full_data_label2, mode = "everything"))
      (rf_roc_1 = ROSE::roc.curve(factor(full_data_label2) , predicted = rf_p_1))
      rf_roc_1[2]
      
      rf_p_1_1 = as.factor(as.vector(predict(RfModel1, type = "class")))
      rf_p_1_1 = factor(ifelse(is.na(rf_p_1_1) , 1 , rf_p_1_1) - 1)
      (rf_cm_1_1 = (confusionMatrix(data = rf_p_1_1 , reference = factor(train_df_Balance$yhat) ,mode = "everything")))
      (rf_roc_1_1 = ROSE::roc.curve(factor(train_df_Balance$yhat) , predicted = rf_p_1_1))
      rf_roc_1_1[2]
      
      rf_1_index = which(rf_p_1 == 1)
      rf_11_index = which(rf_p_1 == 0)
      rf_I22 = sample(rf_11_index , 0.1*length(rf_11_index))
      rf_I2 = rf_1_index}else{rf_cm_1 = c();rf_cm_1_1 = c();rf_roc_1 = c() ; rf_roc_1_1 = c();rf_I2 = c()}
    S[8] = Sys.time()
    
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
      nb_I22 = sample(nb_11_index , 0.1*length(nb_11_index))
      nb_I2 =nb_1_index}else{nb_cm_1 = c();nb_cm_1_1 = c();nb_roc_1 = c() ; nb_roc_1_1 = c();nb_I2 = c()}
    
    S[9] = Sys.time()
    
    # Knn
    if(length(which(models == "knn")) == 1){
      
      preProcValues1 <- preProcess(train_df_Balance[,-c(categorical_columns,ncol(train_df_Balance))], method = c("center", "scale"))
      trainTransformed1 <- cbind(predict(preProcValues1, train_df_Balance[,-c(categorical_columns,ncol(train_df_Balance))]), yhat = train_df_Balance$yhat)
      testTransformed1 <- cbind(predict(preProcValues1, full_data_without_label2[,-c(categorical_columns,ncol(train_df_Balance))]) , yhat = full_data_label2)
      
      library(caret)
      knnModel1 <- caret::train(
        
        yhat ~ . , 
        data = trainTransformed1, 
        method = "knn", 
        trControl = trainControl(method = "cv"), 
        tuneGrid = data.frame(k = c(5:7))
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
      knn_I22 = sample(knn_11_index , 0.1*length(knn_11_index))
      knn_I2 = knn_1_index}else{knn_cm_1 = c();knn_cm_1_1 = c();knn_roc_1 = c() ; knn_roc_1_1 = c();knn_I2 = c()}
    S[10] = Sys.time()
    
    # adaboost
    if(length(which(models == "adab")) == 1){
      library(adabag)
      
      AdabModel1 = boosting(yhat ~ ., data = train_df_Balance, mfinal = 1) 
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
      adab_I22 = sample(adab_11_index , 0.1*length(adab_11_index))
      adab_I2 = adab_1_index}else{adab_cm_1 = c();adab_cm_1_1 = c();adab_roc_1 = c() ; adab_roc_1_1 = c();adab_I2 = c()}
    S[11] = Sys.time()
    ###################################################################
    ### Balancing and editing Dataset2
    
    
    qq = c(adab_I2,knn_I2 , nb_I2, rf_I2 , xgb_I2 , svm_I2 , ml_I2 , nn_I2)
    
    I2 = which(table(qq) >= model_num)
    I22 = which(table(qq) < model_num)
    I22 = sample(I22 , size = ifelse(round(0.1*length(I2) , 0) <1 , length(I2) , 0.1*length(I2)) , replace = FALSE)
    
    Result_final_rond1 = data.frame(ID = (percent1*number_of_rows+1):(percent2*number_of_rows) , label = 0)
    for(i in I2){
      Result_final_rond1$label[i] = 1
    }
    total1_cm = confusionMatrix(data = factor(Result_final_rond1$label) , reference = full_data_label2 , mode = "everything")
    (total1_roc = ROSE::roc.curve( full_data_label2 , factor(Result_final_rond1$label)))
    
    j = 0
    jj = table(Result_final_rond1$label)
    I2 = which(full_data_label2 == 1) 
    for(i in 1:length(full_data_label2)){
      if(Result_final_rond1$label[i] == 1){
        if(full_data_label2[i] == 0){
          Result_final_rond1$label[i] = 0
        }else{j = j+1}

      }
    }
    
    
    (acc1_2 = (1 - (j / length(I2))))
    S[12] = Sys.time()
    
    train_df2 = data.frame(full_data_without_label2[c(I2,I22) , ] , yhat = Result_final_rond1$label[c(I2 , I22)])
    train_df2 = rbind(train_df , train_df2)
    if(prop.table(table(train_df2$yhat))[1] == 0.5){
      train_df_Balance2 = train_df2
    }else{
          train_df_Balance2 = ovun.sample(yhat ~., data = train_df2, method = "over" )$data
    }
    train_df_nn2 = data.frame(full_data_without_label_nn2[c(I2,I22),] , yhat = Result_final_rond1$label[c(I2,I22)])
    train_df_nn2 = rbind(train_df_nn2 , train_df_nn)
    if(prop.table(table(train_df_nn2$yhat))[1] == 0.5){
      train_df_Balance_nn_all_2 = train_df_nn2
    }else{
      train_df_Balance_nn_all_2 = ovun.sample(yhat ~., data = train_df_nn2, method = "over")$data
    }
    train_df_nn_balance_label_2 = train_df_Balance_nn_all_2$yhat
    train_df_Balance_nn_all_2 = train_df_Balance_nn_all_2[,-ncol(train_df_Balance_nn_all_2)]
    train_df_Balance_nn_all_2 = array(unlist(train_df_Balance_nn_all_2) , dim = c(dim(train_df_Balance_nn_all_2)[1] , dim(train_df_Balance_nn_all_2)[2]))
    S[13] = Sys.time()
   
    
    ### Modeling rond 2:
    
    # neural network
    if(length(which(models == "nn")) == 1){
      dimention = dim(train_df_Balance_nn_all_2)
      train_df_nn_balance_label_2= ifelse(train_df_nn_balance_label_2 == 0 , 0, 1)
      ##### Define the model #####
      NnModel2 <- keras_model_sequential() %>%
        layer_dense(units = 32, activation = "relu", input_shape = c(dimention[2])) %>%
        layer_dropout(rate = 0.4) %>%
        # layer_dense(units = 64, activation = "relu") %>%
        # layer_dropout(rate = 0.3) %>%
        layer_dense(units = 1, activation = "sigmoid")

      ##### model info #####
      
      summary(NnModel2)
      
      ##### Compile Model #####
      NnModel2  %>% compile(
        optimizer = "adam",
        loss = "binary_crossentropy",
        metrics = c("accuracy")
      )
      
      ##### Evaluate the model #####
      nn_p_2_2 <- NnModel2 %>% fit(
        train_df_Balance_nn_all_2,
        train_df_nn_balance_label_2,
        epochs = 4,
        batch_size =8,
        shuffle = TRUE)
      nn_p_2_2 <- predict(NnModel2 , x = train_df_Balance_nn_all_2 , type = "response")
      nn_p_2_2 = as.vector(ifelse(nn_p_2_2 <= 0.5 , 0 , 1))
      (nn_cm_2_1 = (confusionMatrix(data = as.factor(nn_p_2_2) , reference = as.factor(train_df_nn_balance_label_2) ,mode = "everything")))
      (nn_roc_2_1 = ROSE::roc.curve(as.factor(train_df_nn_balance_label_2) , predicted = nn_p_2_2))
      nn_roc_2_1[2]

      
      
      #nn_p_1 <- NnModel1 %>% evaluate(full_data_without_label_nn2 , as.integer(full_data_label_nn2))
      nn_p_2 = predict(NnModel2 , full_data_without_label_nn3)
      nn_p_2 = as.vector(ifelse(nn_p_2 <= 0.5 , 0 , 1))
      (nn_cm_2 = confusionMatrix(data = as.factor(nn_p_2) , reference = as.factor(full_data_label3) ,mode = "everything"))
      (nn_roc_2 = ROSE::roc.curve(as.factor(full_data_label3) , predicted = nn_p_2))
      nn_roc_2[2]
      nn_2_index = which(nn_p_2 == 1)
      nn_22_index = which(nn_p_2 == 0)
      nn_I33 = sample(nn_22_index , 0.1*length(nn_22_index))
      nn_I3 = nn_2_index
    }else{
      nn_cm_2 = c();nn_cm_2_1 = c();nn_roc_2 = c() ; nn_roc_2_1 = c();nn_I3 = c()
    }
    
    S[14] = Sys.time()
    
    #multinomial logistic
    if(length(which(models == "ml")) == 1){
      MlModel2 = multinom(yhat ~ . , data = train_df_Balance2)
      ml_p_2 = predict(MlModel2, newdata = full_data_without_label3 , type = "class")
      (ml_cm_2 = confusionMatrix(data = ml_p_2 , reference = factor(full_data_label3) ,mode = "everything"))
      (ml_roc_2 = ROSE::roc.curve(factor(full_data_label3) , predicted = ml_p_2))
      ml_roc_2[2]
      
      
      ml_p_2_1 = predict(MlModel2, type = "class")
      (ml_cm_2_1 = (confusionMatrix(data = ml_p_2_1 , reference = factor(train_df_Balance2$yhat) ,mode = "everything")))
      (ml_roc_2_1 = ROSE::roc.curve(factor(train_df_Balance2$yhat) , predicted = ml_p_2_1))
      ml_roc_2_1[2]
      
      ml_2_index = which(ml_p_2 == 1)
      ml_22_index = which(ml_p_2 == 0)
      ml_I33 = sample(ml_22_index , 0.1*length(ml_22_index))
      ml_I3 = ml_2_index
    }else{ml_cm_2 = c();ml_cm_2_1 = c();ml_roc_2 = c() ; ml_roc_2_1 = c();ml_I3 = c()}
    S[15] = Sys.time()
    
    #svm 
    if(length(which(models == "svm")) == 1){
      SvmModel2 = svm(formula = yhat ~ .,
                      data = train_df_Balance2,
                      type = 'C-classification',
                      kernel = 'linear')
      
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
      svm_I33 = sample(svm_22_index , 0.1*length(svm_22_index))
      svm_I3 = svm_2_index
    }else{svm_cm_2 = c();svm_cm_2_1 = c();svm_roc_2 = c() ; svm_roc_2_1 = c();svm_I3 = c()}
    
    S[16] = Sys.time()
    
    #xgboost
    if(length(which(models == "xgb")) == 1){
      # Convert data into DMatrix format required by XGBoost
      dtrain2 <- xgb.DMatrix(data = as.matrix(train_df_Balance2[,-c(categorical_columns,ncol(train_df_Balance2))])  , label = ifelse(train_df_Balance2$yhat == 0 , 0 , 1))
      dtest2 <- xgb.DMatrix(data = as.matrix(full_data_without_label3[-c(categorical_columns)]), label = ifelse(full_data_label3==0  , 0 , 1))
      
      
      # Train the XGBoost model
      XgbModel2 <- xgboost(params, data = dtrain2, nrounds = params$nrounds)
      xgb_p_2 <- predict(XgbModel2, dtest2 , type = "class")
      # Convert probabilities to binary predictions
      xgb_p_2 <- factor(ifelse(xgb_p_2 > 0.5, 1, 0))
      (xgb_cm_2 <- confusionMatrix(xgb_p_2, full_data_label3, mode = "everything"))
      (xgb_roc_2 = ROSE::roc.curve(factor(full_data_label3) , predicted = xgb_p_2))
      xgb_roc_2[2]
      
      xgb_p_2_1 = predict(XgbModel2, dtrain2 , type = "class")
      xgb_p_2_1 <- factor(ifelse(xgb_p_2_1 > 0.5, 1, 0))
      (xgb_cm_2_1 = (confusionMatrix(data = xgb_p_2_1 , reference = factor(train_df_Balance2$yhat) ,mode = "everything")))
      (xgb_roc_2_1 = ROSE::roc.curve(factor(train_df_Balance2$yhat) , predicted = xgb_p_2_1))
      xgb_roc_2_1[2]
      xgb_2_index = which(xgb_p_2 == 1)
      xgb_22_index = which(xgb_p_2 == 0)
      xgb_I33 = sample(xgb_22_index , 0.1*length(xgb_22_index))
      xgb_I3 = xgb_2_index}else{xgb_cm_2 = c();xgb_cm_2_1 = c();xgb_roc_2 = c() ; xgb_roc_2_1 = c();xgb_I3 = c()}
    S[17] = Sys.time()
    
    #random forest 
    if(length(which(models == "rf")) == 1){
      RfModel2 = randomForest(x = train_df_Balance2[,-c(ncol(train_df_Balance2))],
                              y = train_df_Balance2$yhat,
                              ntree = 20)
      rf_p_2 <- as.factor(as.vector(predict(RfModel2, full_data_without_label3,type = "class")))
      # Calculate confusion matrix
      (rf_cm_2 <- confusionMatrix(rf_p_2, full_data_label3, mode = "everything"))
      (rf_roc_2 = ROSE::roc.curve(factor(full_data_label3) , predicted = rf_p_2))
      rf_roc_2[2]
      
      
      rf_p_2_1 = as.factor(as.vector(predict(RfModel2, type = "class")))
      rf_p_2_1 = factor(ifelse(is.na(rf_p_2_1) , 1 , rf_p_2_1) - 1)
      (rf_cm_2_1 = (confusionMatrix(data = rf_p_2_1 , reference = factor(train_df_Balance2$yhat) ,mode = "everything")))
      (rf_roc_2_1 = ROSE::roc.curve(factor(train_df_Balance2$yhat) , predicted = rf_p_2_1))
      rf_roc_2_1[2]
      rf_2_index = which(rf_p_2 == 1)
      rf_22_index = which(rf_p_2 == 0)
      rf_I33 = sample(rf_22_index , 0.1*length(rf_22_index))
      rf_I3 = rf_2_index
      }else{rf_cm_2 = c();rf_cm_2_1 = c();rf_roc_2 = c() ; rf_roc_2_1 = c();rf_I3 = c()}
    
    S[18] = Sys.time()
    
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
      nb_I33 = sample(nb_22_index , 0.1*length(nb_22_index))
      nb_I3 =nb_2_index}else{nb_cm_2 = c();nb_cm_2_1 = c();nb_roc_2 = c() ; nb_roc_2_1 = c();nb_I3 = c()}
    
    S[19] = Sys.time()
    # knn
    if(length(which(models == "knn")) == 1){
      preProcValues2 <- preProcess(train_df_Balance2[,-c(categorical_columns,ncol(train_df_Balance2))], method = c("center", "scale"))
      trainTransformed2 <- cbind(predict(preProcValues2, train_df_Balance2[,-c(categorical_columns,ncol(train_df_Balance2))]), yhat = train_df_Balance2$yhat)
      testTransformed2 <- cbind(predict(preProcValues2, full_data_without_label3[,-c(categorical_columns,ncol(train_df_Balance2))]) , yhat = full_data_label3)
      
      knnModel2 <- caret::train(
        yhat ~ ., 
        data = trainTransformed2, 
        method = "knn", 
        trControl = trainControl(method = "cv"), 
        tuneGrid = data.frame(k = c(5:7))
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
      knn_I33 = sample(knn_22_index , 0.1*length(knn_22_index))
      knn_I3 = knn_2_index}else{knn_cm_2 = c();knn_cm_2_1 = c();knn_roc_2 = c() ; knn_roc_2_1 = c();knn_I3 = c()}
    S[20] = Sys.time()
    # adaboost
    if(length(which(models == "ml")) == 1){
      AdabModel2 = boosting(yhat ~ ., data = train_df_Balance2, mfinal = 1) 
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
      adab_I33 = sample(adab_22_index , 0.1*length(adab_22_index))
      adab_I3 = adab_2_index}else{adab_cm_2 = c();adab_cm_2_1 = c();adab_roc_2 = c() ; adab_roc_2_1 = c();adab_I3 = c()}
    S[21] = Sys.time()
    
    ### Balancing and editing Dataset3
    
    qq2 = c(adab_I3,knn_I3 , nb_I3, rf_I3 , xgb_I3 , svm_I3 , ml_I3,nn_I3)
    
    I3 = as.numeric(which(table(qq2) >=model_num))
    I33 = as.numeric(which(table(qq2) < model_num))
    I33 = sample(I33 , size = ifelse(round(0.1*length(I3) , 0) <1 , length(I3) , 0.1*length(I3)) , replace = FALSE)
    
    Result_final_rond2 = data.frame(ID = (percent2*number_of_rows+1):(percent3*number_of_rows) , label = 0)
    for(i in I3){
      Result_final_rond2$label[i] = 1
    }
    
    total2_cm = confusionMatrix(data = factor(Result_final_rond2$label) , reference = full_data_label3 , mode = "everything")
    (total2_roc = ROSE::roc.curve( full_data_label3 , factor(Result_final_rond2$label)))
    
    
    j = 0
    jj = table(Result_final_rond2$label)
    I3 = which(full_data_label3 == 1) 
    for(i in 1:length(full_data_label3)){
      if(Result_final_rond2$label[i] == 1){
        if(full_data_label3[i] == 0){
          Result_final_rond2$label[i] = 0
        }else{j = j+1}
        
      }
    }
    
    
    (acc1_3 = (1 - (j / length(I3))))
    S[22] = Sys.time()
    
    
    train_df3 = data.frame(full_data_without_label3[c(I3,I33) , ] , yhat = Result_final_rond2$label[c(I3 , I33)])
    train_df3 = rbind(train_df , train_df2 , train_df3)
    if(prop.table(table(train_df3$yhat))[1] == 0.5){
      train_df_Balance3 = train_df3
    }else{
      train_df_Balance3 = ovun.sample(yhat ~., data = train_df3, method = "over" )$data
    }
    train_df_nn3 = data.frame(full_data_without_label_nn3[c(I3,I33),] , yhat = Result_final_rond2$label[c(I3,I33)])
    train_df_nn3 = rbind(train_df_nn3 , train_df_nn2)
    if(prop.table(table(train_df_nn3$yhat))[1] == 0.5){
      train_df_Balance_nn_all_3 = train_df_nn3
    }else{
      train_df_Balance_nn_all_3 = ovun.sample(yhat ~., data = train_df_nn3, method = "over")$data
    }
    train_df_nn_balance_label_3 = train_df_Balance_nn_all_3$yhat
    train_df_Balance_nn_all_3 = train_df_Balance_nn_all_3[,-ncol(train_df_Balance_nn_all_3)]
    train_df_Balance_nn_all_3 = array(unlist(train_df_Balance_nn_all_3) , dim = c(dim(train_df_Balance_nn_all_3)[1] , dim(train_df_Balance_nn_all_2)[2]))
    S[23] = Sys.time()
    
    
    
    ### Modeling rond 3:
    
    # neural network
    if(length(which(models == "nn")) == 1){
      dimention = dim(train_df_Balance_nn_all_3)
      train_df_nn_balance_label_3= ifelse(train_df_nn_balance_label_3 == 0 , 0, 1)
      ##### Define the model #####
      NnModel3 <- keras_model_sequential() %>%
        layer_dense(units = 32, activation = "relu", input_shape = c(dimention[2])) %>%
        layer_dropout(rate = 0.4) %>%
        # layer_dense(units = 64, activation = "relu") %>%
        # layer_dropout(rate = 0.3) %>%
        layer_dense(units = 1, activation = "sigmoid")
      
      ##### model info #####
      
      summary(NnModel3)
      
      ##### Compile Model #####
      NnModel3  %>% compile(
        optimizer = "adam",
        loss = "binary_crossentropy",
        metrics = c("accuracy")
      )
      
      ##### Evaluate the model #####
      nn_p_3_3 <- NnModel3 %>% fit(
        train_df_Balance_nn_all_3,
        train_df_nn_balance_label_3,
        epochs = 4,
        batch_size =8,
        shuffle = TRUE)
      nn_p_3_3 <- predict(NnModel3 , x = train_df_Balance_nn_all_3 , type = "response")
      nn_p_3_3 = as.vector(ifelse(nn_p_3_3 <= 0.5 , 0 , 1))
      (nn_cm_3_1 = confusionMatrix(data = as.factor(nn_p_3_3) , reference = as.factor(train_df_nn_balance_label_3) ,mode = "everything"))
      (nn_roc_3_1 = ROSE::roc.curve(as.factor(train_df_nn_balance_label_3) , predicted = nn_p_3_3))
      nn_roc_3_1[2]

      
      
      #nn_p_1 <- NnModel1 %>% evaluate(full_data_without_label_nn2 , as.integer(full_data_label_nn2))
      nn_p_3 = predict(NnModel3 , full_data_without_label_nn3)
      nn_p_3 = as.vector(ifelse(nn_p_3 <= 0.5 , 0 , 1))
      (nn_cm_3 = (confusionMatrix(data = as.factor(nn_p_3) , reference = as.factor(full_data_label4) ,mode = "everything")))
      (nn_roc_3 = ROSE::roc.curve(as.factor(full_data_label3) , predicted = nn_p_3))
      nn_roc_3[2]
      nn_3_index = which(nn_p_3 == 1)
      nn_33_index = which(nn_p_3 == 0)
      nn_I44 = sample(nn_33_index , 0.1*length(nn_33_index))
      nn_I4 = nn_3_index
    }else{
      nn_cm_3 = c();nn_cm_3_1 = c();nn_roc_3 = c() ; nn_roc_3_1 = c();nn_I4 = c()
    }
    
    S[24] = Sys.time()
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
      ml_roc_3_1[2]
      ml_3_index = which(ml_p_3 == 1)
      ml_33_index = which(ml_p_3 == 0)
      ml_I44 = sample(ml_33_index , 0.1*length(ml_33_index))
      ml_I4 = ml_3_index
    }else{ml_cm_3 = c();ml_cm_3_1 = c();ml_roc_3 = c() ; ml_roc_3_1 = c();ml_I4 = c()}
    
    S[25] = Sys.time()
    
    #svm 
    if(length(which(models == "svm")) == 1){
      SvmModel3 = svm(formula = yhat ~ .,
                      data = train_df_Balance3,
                      type = 'C-classification',
                      kernel = 'linear')
      
      svm_p_3 <- predict(SvmModel3, full_data_without_label4,type = "class")
      # Calculate confusion matrix
      (svm_cm_3 <- confusionMatrix(svm_p_3, full_data_label4, mode = "everything"))
      (svm_roc_3 = ROSE::roc.curve(factor(full_data_label4) , predicted = svm_p_3))
      svm_roc_3[2]
      
      
      svm_p_3_1 = predict(SvmModel3, train_df_Balance3 , type = "class")
      (svm_cm_3_1 = (confusionMatrix(data = svm_p_3_1 , reference = factor(train_df_Balance3$yhat) ,mode = "everything")))
      (svm_roc_3_1 = ROSE::roc.curve(factor(train_df_Balance3$yhat) , predicted = svm_p_3_1))
      svm_roc_3_1[2]
      svm_3_index = which(svm_p_3 == 1)
      svm_33_index = which(svm_p_3 == 0)
      svm_I44 = sample(svm_33_index , 0.1*length(svm_33_index))
      svm_I4 = svm_3_index
    }else{svm_cm_3 = c();svm_cm_3_1 = c();svm_roc_3 = c() ; svm_roc_3_1 = c();svm_I4 = c()}
    
    S[26] = Sys.time()
    #xgboost
    if(length(which(models == "xgb")) == 1){
      # Convert data into DMatrix format required by XGBoost
      dtrain3 <- xgb.DMatrix(data = as.matrix(train_df_Balance3[,-c(categorical_columns,ncol(train_df_Balance3))])  , label = ifelse(train_df_Balance3$yhat==0 , 0 ,1))
      dtest3 <- xgb.DMatrix(data = as.matrix(full_data_without_label4[-c(categorical_columns)]), label = ifelse(full_data_label4 == 0 , 0 , 1))
      
      
      # Train the XGBoost model
      XgbModel3 <- xgboost(params, data = dtrain3, nrounds = params$nrounds)
      xgb_p_3 <- predict(XgbModel3, dtest3 , type = "class")
      # Convert probabilities to binary predictions
      xgb_p_3 <- factor(ifelse(xgb_p_3 > 0.5, 1, 0))
      (xgb_cm_3 <- confusionMatrix(xgb_p_3, full_data_label4, mode = "everything"))
      (xgb_roc_3 = ROSE::roc.curve(factor(full_data_label4) , predicted = xgb_p_2))
      xgb_roc_2[2]
      
      xgb_p_3_1 = predict(XgbModel3, dtrain3 , type = "class")
      xgb_p_3_1 <- factor(ifelse(xgb_p_3_1 > 0.5, 1, 0))
      (xgb_cm_3_1 = (confusionMatrix(data = xgb_p_3_1 , reference = factor(train_df_Balance3$yhat) ,mode = "everything")))
      (xgb_roc_3_1 = ROSE::roc.curve(factor(train_df_Balance3$yhat) , predicted = xgb_p_3_1))
      xgb_roc_3_1[2]
      xgb_3_index = which(xgb_p_3 == 1)
      xgb_33_index = which(xgb_p_3 == 0)
      xgb_I44 = sample(xgb_33_index , 0.1*length(xgb_33_index))
      xgb_I4 = xgb_3_index
    }else{xgb_cm_3 = c();xgb_cm_3_1 = c();xgb_roc_3 = c() ; xgb_roc_3_1 = c();xgb_I4 = c()}
    S[27] = Sys.time()
    #random forest
    if(length(which(models == "rf")) == 1){
      RfModel3= randomForest(x = train_df_Balance3[,-c(ncol(train_df_Balance3))],
                             y = train_df_Balance3$yhat,
                             ntree = 20)
      rf_p_3 <- as.factor(as.vector(predict(RfModel3, full_data_without_label4,type = "class")))
      # Calculate confusion matrix
      (rf_cm_3 <- confusionMatrix(rf_p_3, full_data_label4, mode = "everything"))
      (rf_roc_3 = ROSE::roc.curve(factor(full_data_label4) , predicted = rf_p_3))
      rf_roc_3[2]
      
      rf_p_3_1 = as.factor(as.vector(predict(RfModel3, type = "class")))
      rf_p_3_1 = factor(ifelse(is.na(rf_p_3_1) , 1 , rf_p_3_1) - 1)
      (rf_cm_3_1 = (confusionMatrix(data = rf_p_3_1 , reference = factor(train_df_Balance3$yhat) ,mode = "everything")))
      (rf_roc_3_1 = ROSE::roc.curve(factor(train_df_Balance3$yhat) , predicted = rf_p_3_1))
      rf_roc_3_1[2]
      rf_3_index = which(rf_p_3 == 1)
      rf_33_index = which(rf_p_3 == 0)
      rf_I44 = sample(rf_33_index , 0.1*length(rf_33_index))
      rf_I4 = rf_3_index
    }else{rf_cm_3 = c();rf_cm_3_1 = c();rf_roc_3 = c() ; rf_roc_3_1 = c();rf_I4 = c()}
    
    S[28] = Sys.time()
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
      nb_roc_3_1[2]
      nb_3_index = which(nb_p_3 == 1)
      nb_33_index = which(nb_p_3 == 0)
      nb_I44 = sample(nb_33_index , 0.1*length(nb_33_index))
      nb_I4 =nb_3_index
    }else{nb_cm_3 = c();nb_cm_3_1 = c();nb_roc_3 = c() ; nb_roc_3_1 = c();nb_I4 = c()}
    S[29] = Sys.time()
    #Knn
    if(length(which(models == "knn")) == 1){
      preProcValues3<- preProcess(train_df_Balance3[,-c(categorical_columns,ncol(train_df_Balance3))], method = c("center", "scale"))
      trainTransformed3 <- cbind(predict(preProcValues3, train_df_Balance3[,-c(categorical_columns,ncol(train_df_Balance3))]), yhat = train_df_Balance3$yhat)
      testTransformed3 <- cbind(predict(preProcValues3, full_data_without_label4[,-c(categorical_columns,ncol(train_df_Balance3))]) , yhat = full_data_label4)
      knnModel3 <- caret::train(
        yhat ~ ., 
        data = trainTransformed3, 
        method = "knn", 
        trControl = trainControl(method = "cv"), 
        tuneGrid = data.frame(k = c(5:7))
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
      knn_3_index = which(knn_p_3 == 1)
      knn_33_index = which(knn_p_3 == 0)
      knn_I33 = sample(knn_33_index , 0.1*length(knn_33_index))
      knn_I4 = knn_3_index
    }else{knn_cm_3 = c();knn_cm_3_1 = c();knn_roc_3 = c() ; knn_roc_3_1 = c();knn_I4 = c()}
    S[30] = Sys.time()
    #Adaboost
    if(length(which(models == "adab")) == 1){
      AdabModel3 = boosting(yhat ~ ., data = train_df_Balance3, mfinal = 1) 
      
      
      adab_p_3 <- predict(AdabModel3, full_data_without_label4,type = "class")
      # Calculate confusion matrix
      (adab_cm_3 <- confusionMatrix(factor(adab_p_3$class), full_data_label4, mode = "everything"))
      (adab_roc_3 = ROSE::roc.curve(factor(full_data_label4) , predicted = factor(adab_p_3$class)))
      adab_roc_3[2]
      
      
      adab_p_3_1 = predict(AdabModel3, train_df_Balance3 , type = "class")
      (adab_cm_3_1 = (confusionMatrix(data = factor(adab_p_3_1$class) , reference = factor(train_df_Balance3$yhat) ,mode = "everything")))
      (adab_roc_3_1 = ROSE::roc.curve(factor(train_df_Balance3$yhat) , predicted = factor(adab_p_3_1$class)))
      adab_roc_3_1[2]
      adab_3_index = which(adab_p_3$class == 1)
      adab_33_index = which(adab_p_3$class == 0)
      adab_I44 = sample(adab_33_index , 0.1*length(adab_33_index))
      adab_I4 = adab_2_index
      
    }else{adab_cm_3 = c();adab_cm_3_1 = c();adab_roc_3 = c() ; adab_roc_3_1 = c();adab_I4 = c()}
    S[31] = Sys.time()
    #Last cms = 
    qq3 = c(adab_I4,knn_I4 , nb_I4, rf_I4 , xgb_I4 , svm_I4 , ml_I4 , nn_I4)
    
    I4 = which(table(qq3) >= model_num)
    I44 = which(table(qq3) < model_num)
    I44 = sample(I44 , size = ifelse(round(0.1*length(I4) , 0) <1 , length(I4) , 0.1*length(I4)) , replace = FALSE)
    
    Result_final_rond3 = data.frame(ID = (percent3*number_of_rows+1):(1*number_of_rows) , label = 0)
    for(i in I4){
      Result_final_rond3$label[i] = 1
    }
    
    total3_cm = confusionMatrix(data = factor(Result_final_rond3$label) , reference = full_data_label4, mode = "everything")
    (total3_roc = ROSE::roc.curve( full_data_label4 , factor(Result_final_rond3$label)))
    
    
    j = 0
    jj = table(Result_final_rond3$label)
    I4 = which(full_data_label4 == 1) 
    for(i in 1:length(full_data_label4)){
      if(Result_final_rond3$label[i] == 1){
        if(full_data_label4[i] == 0){
          Result_final_rond3$label[i] = 0
        }else{j = j+1}
        
      }
    }
    
    
    (acc1_4 = (1 - (j / length(I4))))
    S[32] = Sys.time()
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
    #nn 
    # nn_cm_1 = c() ; nn_cm_2 = c() ; nn_cm_3 = c() ; nn_roc_1 = c() ; nn_roc_2 = c() ; nn_roc_3 = c();
    # nn_cm_1_1 = c() ; nn_cm_2_1 = c() ; nn_cm_3_1 = c() ; nn_roc_1_1 = c() ; nn_roc_2_1 = c() ; nn_roc_3_1 = c();
    output_nn = rbind(nn_test_1 = c(nn_cm_1$byClass , nn_cm_1$overall, nn_roc_1[2]) , nn_test_2 = c(nn_cm_2$byClass , nn_cm_2$overall, nn_roc_2[2]) , nn_test_3 = c(nn_cm_3$byClass , nn_cm_3$overall, nn_roc_3[2]),
                      nn_train_1 = c(nn_cm_1_1$byClass , nn_cm_1_1$overall, nn_roc_1_1[2]) , nn_train_2 = c(nn_cm_2_1$byClass , nn_cm_2_1$overall, nn_roc_2_1[2]) , nn_train_3 = c(nn_cm_3_1$byClass , nn_cm_3_1$overall, nn_roc_3_1[2]))
    
    output_total = rbind(total1_cm = c(total1_cm$byClass , total1_cm$overall , total1_roc[2]) ,
                         total2_cm =  c(total2_cm$byClass , total2_cm$overall , total2_roc[2]),
                         total3_cm =  c(total3_cm$byClass , total3_cm$overall , total3_roc[2]))
    final_outputs = rbind(svm = output_svm , ml = output_ml 
                           , adab = output_adab , knn = output_knn , nb = output_nb , 
                           rf = output_rf , xgb = output_xgb, nn = output_nn , output_total =  output_total )
    times = c()
    j = 1
    for(i in 2:32){
      times[j] = difftime(S[i],S[i-1] , units = "m")
      j = j+1
    }
    names_times = c("Labeling ",
                    "Balancing Data1",
                    "nn modeling rond1",
                    "ml modeling rond1",
                    "svm modeling rond1",
                    "xgb modeling rond1",
                    "rf modeling rond1",
                    " nb modeling rond1",
                    "knn modeling rond1",
                    "adab modeling rond1",
                    "calculate totam cm rond1",
                    "Balancing Data2",
                    "nn modeling rond2",
                    "ml modeling rond2",
                    "svm modeling rond2",
                    "xgb modeling rond2",
                    "rf modeling rond2",
                    " nb modeling rond2",
                    "knn modeling rond2",
                    "adab modeling rond2",
                    "calculate totam cm rond2",
                    "Balancing Data3",
                    "nn modeling rond3",
                    "ml modeling rond3",
                    "svm modeling rond3",
                    "xgb modeling rond3",
                    "rf modeling rond3",
                    " nb modeling rond3",
                    "knn modeling rond3",
                    "adab modeling rond3",
                    "calculate totam cm rond3")
    # tt = c()
    # tt[1:3] = times[c()] 
    time_data = data.frame(times  , names_times , Dataset = path , modelnames = model_names , model_num = model_num , vote_num = vote_num)
    cm_outputs = cbind(final_outputs , dataset = path  , modelnames = model_names , model_num = model_num , vote_num = vote_num)
    final_result = list( cm_outputs = cm_outputs , time_output = time_data)
    # return(final_outputs)
    # }
    
    # m1 = FinalModeling(models = c("ml" , "svm" , "xgb" , "rf" , "nb" , "knn" , "adab") , 
    #                    percent1 = 0.5 , percent2 = 0.8 , percent3 = 0.9 ,
    #                    path = "carclaims.csv" , response_column_number = 33  ,
    #                    YesOrNo = TRUE)
    write.csv(cm_outputs , paste0("outputs/Model_num_",model_num, "_vote_", vote_num, "_Dataset_" , path  , "_KK_" , KK  , ".csv"))
    write.csv(time_data , paste0("outputs/TIME_Model_num_",model_num, "_vote_", vote_num, "_Dataset_" , path  , "_KK_" , KK , ".csv"))
    Nice[[number]] = cm_outputs
    number = number + 1
      }
    
    }
  }
}

# FINAL = bind_rows(Nice)
# write.csv(FINAL , paste0("outputs/FINAL.csv"))

# FinalModeling = function(75-34 "rf", "knn" , "adab" , "nn"
    # models = c("ml" , "svm" , "xgb"  , "nb"  , "rf", "knn" , "adab" ) 
    

    